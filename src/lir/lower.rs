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

use crate::ir;
use crate::ir::instructions::{
    BinOp as GirBinOp, CmpOp as GirCmpOp, Constant, Instruction, Operand, Place, Projection,
    Terminator, UnOp as GirUnOp,
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
struct FuncLowering<'a> {
    gir_func: &'a ir::Function,
    gir_types: &'a TypeRegistry,
    lir_func: LirFunction,
    /// GIR LocalId → LIR SlotId mapping.
    local_to_slot: Vec<SlotId>,
    /// GIR BlockId → LIR BlockId mapping.
    block_map: Vec<BlockId>,
    /// Struct registry reference (for FieldPtr).
    struct_reg: &'a StructRegistry,
    /// Function name → FuncId (for Call).
    func_index: &'a std::collections::HashMap<String, FuncId>,
    /// Global name → GlobalId (for GlobalRef/GlobalAssign).
    global_index: &'a std::collections::HashMap<String, GlobalId>,
    /// Module struct definitions (for field-count checking).
    module_structs: &'a [StructDef],
    /// Module globals (for type lookup).
    module_globals: &'a [LirGlobal],
    /// Synthetic externs discovered during lowering (for unknown Call targets).
    pending_externs: Vec<LirExtern>,
    /// Whether `directive overflow=wrap` is active (integer overflow wraps).
    overflow_wrap: bool,
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
            if map_monomorphized_to_runtime(&ext.name).is_some() {
                continue;
            }
            self.module.add_extern(LirExtern {
                name: ext.name.clone(),
                params: ext.params.iter().map(|t| map_gir_type_with_structs(t, &self.gir.type_registry, Some(&self.struct_reg))).collect(),
                return_type: map_gir_type_with_structs(&ext.return_type, &self.gir.type_registry, Some(&self.struct_reg)),
                is_variadic: ext.is_variadic,
            });
        }

        // Lower globals.
        for global in &self.gir.globals {
            let gid = self.module.add_global(LirGlobal {
                name: global.name.clone(),
                ty: map_gir_type_with_structs(&global.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                init: lower_global_init(&global.init, &self.func_index),
                is_const: false, // GIR doesn't distinguish const vs mut globals
            });
            self.global_index.insert(global.name.clone(), gid);
        }

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

        // Compute elem_drop_recipes for types that need compound drops.
        self.compute_all_drop_recipes();

        self.module
    }

    /// Compute drop recipes for all types that need compound element drops.
    /// A compound drop is needed when a type has Custom or Recursive drop and
    /// also has fields that need dropping (e.g., Container with Custom drop + data: Vector).
    fn compute_all_drop_recipes(&mut self) {
        let type_defs: Vec<_> = self.gir.type_registry.type_defs().iter()
            .map(|td| (td.name.clone(), td.metadata.drop_strategy.clone(), td.kind.clone()))
            .collect();
        for (name, _strategy, _kind) in &type_defs {
            let actions = self.compute_drop_actions(name);
            if !actions.is_empty() {
                self.module.elem_drop_recipes.insert(name.clone(), actions);
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

    /// Compute the drop actions for a given type.
    /// Returns empty if no compound drops are needed.
    fn compute_drop_actions(&self, type_name: &str) -> Vec<ElemDropAction> {
        use crate::ir::types::DropStrategy;
        let strategy = self.infer_drop_strategy(type_name);
        match strategy {
            DropStrategy::None => vec![],
            DropStrategy::Trivial(ref fn_name) if fn_name == "free" => {
                // Box: just free. No compound drops needed.
                vec![ElemDropAction::Call(fn_name.clone())]
            }
            DropStrategy::Trivial(ref fn_name) => {
                // Collection free (gorget_array_free, gorget_map_free).
                // Check if elements need dropping.
                let is_array_free = fn_name == "gorget_array_free";
                let is_map_free = fn_name == "gorget_map_free";
                if is_array_free || is_map_free {
                    let elem_type_name = if is_array_free {
                        type_name.strip_prefix("Vector__")
                            .or_else(|| type_name.strip_prefix("Deque__"))
                    } else {
                        type_name.strip_prefix("Dict__")
                            .or_else(|| type_name.strip_prefix("HashMap__"))
                            .and_then(|rest| rest.find("__").map(|idx| &rest[idx + 2..]))
                    };
                    if let Some(elem_name) = elem_type_name {
                        let sub_actions = self.compute_drop_actions(elem_name);
                        if sub_actions.is_empty() {
                            // Simple element type, no compound drops needed
                            vec![]
                        } else {
                            // Elements need compound drops: iterate sub-elements, then free
                            let mut actions = vec![ElemDropAction::SubElems(sub_actions)];
                            actions.push(ElemDropAction::Call(fn_name.clone()));
                            actions
                        }
                    } else {
                        vec![]
                    }
                } else {
                    vec![]
                }
            }
            DropStrategy::Custom(ref fn_name) => {
                // Custom drop + field drops.
                let mut actions = vec![ElemDropAction::Call(fn_name.clone())];
                let field_drops = self.compute_field_drop_actions(type_name);
                actions.extend(field_drops);
                actions
            }
            DropStrategy::Recursive => {
                // No custom drop function, but fields need dropping.
                self.compute_field_drop_actions(type_name)
            }
        }
    }

    /// Compute drop actions for each droppable field of a struct type.
    fn compute_field_drop_actions(&self, type_name: &str) -> Vec<ElemDropAction> {
        use crate::ir::types::DropStrategy;
        let type_def = match self.gir.type_registry.get_type_def(type_name) {
            Some(td) => td,
            None => return vec![],
        };
        let sdef = match &type_def.kind {
            crate::ir::types::TypeDefKind::Struct(s) => s,
            _ => return vec![],
        };
        let mut actions = vec![];
        for (field_idx, field) in sdef.fields.iter().enumerate() {
            let field_type_name = match self.gir.type_registry.get(field.type_id) {
                Some(GirType::Named(n)) => n.clone(),
                _ => continue,
            };
            let field_drop = self.infer_drop_strategy(&field_type_name);
            match field_drop {
                DropStrategy::None => continue,
                DropStrategy::Trivial(ref fn_name) => {
                    // Check if this field (a collection) has elements that need compound drops
                    let sub_actions = self.compute_drop_actions(&field_type_name);
                    if sub_actions.is_empty() {
                        actions.push(ElemDropAction::Field {
                            struct_name: type_name.to_string(),
                            field_idx: field_idx as u32,
                            actions: vec![ElemDropAction::Call(fn_name.clone())],
                        });
                    } else {
                        actions.push(ElemDropAction::Field {
                            struct_name: type_name.to_string(),
                            field_idx: field_idx as u32,
                            actions: sub_actions,
                        });
                    }
                }
                DropStrategy::Custom(_) | DropStrategy::Recursive => {
                    let sub_actions = self.compute_drop_actions(&field_type_name);
                    if !sub_actions.is_empty() {
                        actions.push(ElemDropAction::Field {
                            struct_name: type_name.to_string(),
                            field_idx: field_idx as u32,
                            actions: sub_actions,
                        });
                    }
                }
            }
        }
        actions
    }

    fn lower_type_defs(&mut self) {
        for def in self.gir.type_registry.type_defs() {
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

            // Guard struct types need a fixed layout: { ptr owner; ptr data; }
            if is_guard_struct_type(&def.name) {
                let fields = vec![
                    ("owner".into(), LirType::Ptr),
                    ("ptr".into(), LirType::Ptr),
                ];
                let sid = self.module.add_struct(StructDef {
                    name: def.name.clone(),
                    fields,
                });
                self.struct_reg.register(&def.name, sid);
                continue;
            }

            match &def.kind {
                gir_types::TypeDefKind::Struct(sdef) => {
                    let fields: Vec<(String, LirType)> = sdef
                        .fields
                        .iter()
                        .map(|f| {
                            (
                                f.name.clone(),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            )
                        })
                        .collect();
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields,
                    });
                    self.struct_reg.register(&def.name, sid);
                }
                gir_types::TypeDefKind::Enum(edef) => {
                    // Enums become a struct with a tag field + variant fields.
                    let mut fields: Vec<(String, LirType)> = vec![("tag".into(), LirType::I32)];
                    for variant in &edef.variants {
                        for (i, f) in variant.fields.iter().enumerate() {
                            fields.push((
                                format!("{}_{}", variant.name, i),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            ));
                        }
                    }
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields,
                    });
                    self.struct_reg.register(&def.name, sid);
                }
                gir_types::TypeDefKind::Alias(_) => {
                    // Type aliases are transparent — no LIR struct needed.
                }
            }
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
fn synthesize_struct_fields(
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

/// Split a Result's inner components: "double__Str" → ("double", "Str").
/// Handles compound types like "Vector__int64_t__Str" → ("Vector__int64_t", "Str").
fn split_result_components(rest: &str) -> Option<(&str, &str)> {
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
fn component_to_lir_type(name: &str, struct_reg: &StructRegistry) -> LirType {
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
fn collection_runtime_type(name: &str) -> Option<&'static str> {
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
fn opaque_runtime_type_name(name: &str) -> Option<&'static str> {
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
fn is_opaque_pointer_type(name: &str) -> bool {
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
fn is_guard_struct_type(name: &str) -> bool {
    name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
}

impl<'a> FuncLowering<'a> {
    fn new(
        gir_func: &'a ir::Function,
        gir_types: &'a TypeRegistry,
        struct_reg: &'a StructRegistry,
        func_index: &'a std::collections::HashMap<String, FuncId>,
        global_index: &'a std::collections::HashMap<String, GlobalId>,
        module_structs: &'a [StructDef],
        module_globals: &'a [LirGlobal],
        overflow_wrap: bool,
    ) -> Self {
        let params: Vec<LirType> = gir_func
            .params
            .iter()
            .map(|t| map_gir_type_with_structs(t, gir_types, Some(struct_reg)))
            .collect();
        let return_type = map_gir_type_with_structs(&gir_func.return_type, gir_types, Some(struct_reg));
        let mut lir_func = LirFunction::new(gir_func.name.clone(), params, return_type);
        lir_func.is_test_fn = gir_func.is_test_fn;

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
        }
    }

    fn lower(&mut self) {
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
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::ParamRef {
                        dst: param_val,
                        index: param_idx as u32,
                        ty: slot_ty,
                    });
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::SlotStore {
                        slot,
                        value: param_val,
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

    fn lower_instruction(&mut self, inst: &Instruction, bb: BlockId) {
        match inst {
            Instruction::Assign { dst, value } => {
                // Special-case: Constant::Null assigned to an enum-typed local.
                // Null represents a fieldless variant (e.g. None, Error).  Instead of
                // emitting NullPtr (which becomes memset(0) ⇒ tag=0 = wrong variant),
                // emit proper enum init with the correct tag ordinal.
                if let Operand::Constant(Constant::Null) = value {
                    // Try to materialize a proper enum null variant (tag set, payload zeroed)
                    // instead of emitting raw NullPtr which would become memcpy-from-NULL.
                    if let Some(()) = self.try_materialize_null_for_assign(dst, bb) {
                        return;
                    }
                }
                // Special-case: Option/Result source → non-Option/Result dest.
                // The GIR C backend implicitly extracts the payload field (e.g.
                // `_21 = _23.data.Some._0`). We must do the same: emit an
                // EnumFieldLoad-style extraction instead of a raw copy.
                if let Some(val) = self.try_enum_payload_extract(dst, value, bb) {
                    self.store_to_place(dst, val, bb);
                    return;
                }
                // Special-case: Box[Trait] ← Box[Concrete] trait object construction.
                // The GIR C backend wraps this as (TraitObj){.data = src, .vtable = &vtable}.
                if self.try_trait_object_construct(dst, value, bb) {
                    return;
                }
                let val = self.lower_operand(value, bb);
                self.store_to_place(dst, val, bb);
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
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None,
                        name: "gorget_array_extend".to_string(),
                        args: vec![result, r],
                    });
                    self.store_to_local(*dst, result, bb);
                } else {
                    let result = self.lir_func.next_value();
                    let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
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
                let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
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
                let to = map_gir_type_with_structs(target_type, self.gir_types, Some(self.struct_reg));

                // Check if target is Str — emit conversion call instead of invalid (Str)(val) cast.
                let is_str_target = matches!(&to, LirType::Struct(sid) if {
                    self.module_structs.get(sid.0 as usize)
                        .map_or(false, |s| s.name == "Str" || s.name == "GorgetString")
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
                        // Ptr source (const char*) → Str: wrap directly with gorget_str_from_cstr.
                        let str_ty = self.struct_reg.lookup("Str")
                            .map(LirType::Struct).unwrap_or(LirType::Ptr);
                        let cstr_result = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: Some(cstr_result),
                            name: "gorget_str_from_cstr".to_string(),
                            args: vec![val],
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
                    });
                    self.ensure_extern(conv_fn, &[if is_float { LirType::F64 } else if is_bool { LirType::Bool } else { LirType::I64 }], &LirType::Ptr);
                    // The result is a const char* (Ptr) — the SlotStore Ptr→Str path in c_lir
                    // will wrap it with gorget_str_from_literal since it's a cstr_val.
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
                let to = map_gir_type_with_structs(target_type, self.gir_types, Some(self.struct_reg));
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
            Instruction::Call { dst, func, args } => {
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
                    let emit_name = map_monomorphized_to_runtime(func)
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
                    // regex_compile (without flags) needs a NULL second arg
                    let mut lir_args = lir_args;
                    if emit_name == "gorget_regex_compile" && !func.contains("compile_with") {
                        let null_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
                        lir_args.push(null_val);
                    }
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
                        } else if arg_type.as_deref().map_or(false, |n| n == "str" || n == "Str") {
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
                        let zero_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                            dst: zero_val, ty: LirType::I64, value: 0,
                        });
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
                let mut emit_name = map_monomorphized_to_runtime(func)
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
                // regex_compile (without flags) needs a NULL second arg
                let regex_compile_needs_null = emit_name == "gorget_regex_compile"
                    && !func.contains("compile_with");

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
                // Inject NULL flags for regex_compile (not compile_with)
                let mut lir_args = lir_args;
                if regex_compile_needs_null {
                    let null_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::NullPtr { dst: null_val });
                    lir_args.push(null_val);
                }
                // gorget_regex_find/split take 3 args but GIR only passes 2 — inject default 0
                if (emit_name == "gorget_regex_find" || emit_name == "gorget_regex_split") && lir_args.len() == 2 {
                    let zero_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                        dst: zero_val, ty: LirType::I64, value: 0,
                    });
                    lir_args.push(zero_val);
                }
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
                                let tag_val = self.lir_func.next_value();
                                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                                    dst: tag_val,
                                    ty: LirType::I32,
                                    value: tag_ordinal as i64,
                                });
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
            } => {
                let mut base_val = self.lower_place_addr(base, bb);
                // Use effective type after base projections (e.g., Deref→Field chain).
                let effective_type = self.effective_place_type(base);
                // If the effective type is a pointer (e.g., closure env param),
                // load the pointer value first so FieldPtr operates on the struct, not the slot.
                if let Some(GirType::Ptr(_) | GirType::MutPtr(_)) = self.gir_types.get(effective_type) {
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
                let result = self.lir_func.next_value();
                let field_ty = self.resolve_field_type(effective_type, *field);
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: fptr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, result, bb);
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
                let is_str = base_type_name == "Str" || base_type_name == "GorgetString";
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
                    let ret_ty = map_gir_type_with_structs(&dst_gir_ty, self.gir_types, Some(self.struct_reg));
                    let str_ty = self.struct_reg.lookup("Str")
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
                    });
                    self.store_to_local(*dst, result, bb);
                } else if is_str {
                    // Str[int] → gorget_str_index(str, idx)
                    let base_val = self.lower_place_addr(base, bb);
                    let idx = self.lower_operand(index, bb);
                    let dst_gir_ty = self.gir_func.locals[dst.0 as usize].type_id;
                    let ret_ty = map_gir_type_with_structs(&dst_gir_ty, self.gir_types, Some(self.struct_reg));
                    let str_ty = self.struct_reg.lookup("Str")
                        .map(LirType::Struct).unwrap_or(LirType::Ptr);
                    self.ensure_extern("gorget_str_index", &[str_ty, LirType::I64], &ret_ty);
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(result),
                        name: "gorget_str_index".to_string(),
                        args: vec![base_val, idx],
                    });
                    self.store_to_local(*dst, result, bb);
                } else if is_array || is_dict {
                    // Vector[int] → gorget_array_get(&arr, idx)
                    // Dict[key] → gorget_map_get(&map, &key)
                    let base_val = self.lower_place_addr(base, bb);
                    let idx = self.lower_operand(index, bb);
                    let fn_name = if is_dict { "gorget_map_get" } else { "gorget_array_get" };
                    self.ensure_extern(fn_name, &[LirType::Ptr, LirType::I64], &LirType::Ptr);
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(ptr_val),
                        name: fn_name.to_string(),
                        args: vec![base_val, idx],
                    });
                    // gorget_array_get / gorget_map_get return void* pointing to the element.
                    // Dereference it to get the actual element value.
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
                    let result = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: result,
                        ty: elem_ty.clone(),
                        ptr: ptr_val,
                    });
                    self.store_to_local(*dst, result, bb);

                    // IndexLoad is a move — zero the array/dict slot to prevent
                    // double-free when the collection is dropped.
                    // Check if element type needs dropping (resource/move semantics).
                    let elem_type_name = base_type_name
                        .strip_prefix("Vector__")
                        .or_else(|| base_type_name.strip_prefix("Dict__").and_then(|r| r.rsplit_once("__").map(|(_, v)| v)))
                        .unwrap_or("");
                    let elem_needs_zero = match self.infer_drop_strategy(elem_type_name) {
                        crate::ir::types::DropStrategy::None => false,
                        _ => true,
                    };
                    if elem_needs_zero {
                        let byte_size = c_sizeof_lir_type(&elem_ty, &self.module_structs) as i64;
                        if byte_size > 0 {
                            let zero = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                                dst: zero, ty: LirType::I32, value: 0,
                            });
                            let sz = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                                dst: sz, ty: LirType::I64, value: byte_size,
                            });
                            self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                                ptr: ptr_val, byte: zero, size: sz,
                            });
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
                let tag_val = self.lir_func.next_value();
                let tag_ordinal = self.resolve_variant_ordinal(type_name, variant);
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: tag_val,
                    ty: LirType::I32,
                    value: tag_ordinal as i64,
                });
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
                    let zero = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                        dst: zero,
                        ty: LirType::I32,
                        value: 0,
                    });
                    let size = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                        dst: size,
                        ty: LirType::I64,
                        value: byte_size,
                    });
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
                let result = self.lir_func.next_value();
                let field_ty = self.resolve_enum_field_type(gir_type_id, variant, *field);
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: fptr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, result, bb);
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
                let addr = self.lower_place_addr(place, bb);
                let zero = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: zero,
                    ty: LirType::I32,
                    value: 0,
                });
                let size = self.lir_func.next_value();
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
                    map_gir_type_with_structs(&gir_type, self.gir_types, Some(self.struct_reg))
                };
                let byte_size = match &effective_ty {
                    LirType::Struct(_) => c_sizeof_lir_type(&effective_ty, &self.module_structs) as i64,
                    _ => super::types::scalar_size(&effective_ty).unwrap_or(8) as i64,
                };
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: size,
                    ty: LirType::I64,
                    value: byte_size,
                });
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
                });
            }

            Instruction::LoadThreadLocal { dst, name } => {
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: format!("__gorget_tls_{name}"),
                    args: vec![],
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PushAllocator { allocator } => {
                let alloc = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_push_allocator".into(),
                    args: vec![alloc],
                });
            }

            Instruction::PopAllocator => {
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_pop_allocator".into(),
                    args: vec![],
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
                        let val = self.lir_func.next_value();
                        // Emit InlineC with a dst, then store to slot.
                        self.lir_func.block_mut(bb).insts.push(Inst::InlineC {
                            dst: Some(val),
                            code: code.clone(),
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotStore {
                            slot,
                            value: val,
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

    fn lower_terminator(&mut self, term: &Terminator, bb: BlockId) -> Term {
        match term {
            Terminator::Return(operand) => {
                let ret_type = map_gir_type_with_structs(&self.gir_func.return_type, self.gir_types, Some(self.struct_reg));
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
    fn mark_inline_c_referenced_slots(&mut self, expr: &str, bb: BlockId) {
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
    fn lower_operand(&mut self, operand: &Operand, bb: BlockId) -> ValueId {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => self.lower_place_load(place, bb),
            Operand::Constant(c) => self.lower_constant(c, bb),
        }
    }

    /// Check if a GIR operand refers to a Str-typed local (simple, no projections).
    fn operand_is_str(&self, operand: &Operand) -> bool {
        let str_sid = self.struct_reg.lookup("Str");
        let gs_sid = self.struct_reg.lookup("GorgetString");
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                if !place.projections.is_empty() { return false; }
                let idx = place.local.0 as usize;
                if idx >= self.local_to_slot.len() { return false; }
                let slot = self.local_to_slot[idx];
                let slot_ty = &self.lir_func.slots[slot.0 as usize].ty;
                matches!(slot_ty, LirType::Struct(sid) if Some(*sid) == str_sid || Some(*sid) == gs_sid)
            }
            _ => false,
        }
    }

    /// Shared extern-call emitter used by both `Instruction::Call` (unresolved)
    /// and `Instruction::CallExtern`.  Handles sizeof synthesis for collection
    /// and concurrency constructors, and struct-return rewriting for mutex lock /
    /// rwlock read/write.
    fn emit_extern_call(
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
                        map_gir_type_with_structs(&gir_ty, self.gir_types, Some(self.struct_reg))
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
                if elem_type.as_deref() == Some("Str") || elem_type.as_deref() == Some("GorgetString") {
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
                let sz_val = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: sz_val,
                    ty: LirType::I64,
                    value: elem_sz,
                });
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
            if key_type.as_deref() == Some("Str") || key_type.as_deref() == Some("GorgetString") {
                // Use _str variant for string keys.
                let str_variant = if is_dict { "gorget_dict_new_str" } else { "gorget_map_new_str" };
                actual_emit_name = Some(str_variant.into());
                let v = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: v,
                    ty: LirType::I64,
                    value: val_sz as i64,
                });
                lir_args.push(v);
            } else {
                let k = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: k,
                    ty: LirType::I64,
                    value: key_sz as i64,
                });
                let v = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: v,
                    ty: LirType::I64,
                    value: val_sz as i64,
                });
                lir_args.push(k);
                lir_args.push(v);
            }
        }
        let emit_name = actual_emit_name.as_deref().unwrap_or(emit_name);
        // gorget_array_contains needs elem_size appended.
        if emit_name == "gorget_array_contains" && args.len() >= 2 {
            let elem_lir_ty = self.operand_lir_type(&args[1]);
            let elem_sz = lir_type_sizeof(&elem_lir_ty) as i64;
            let sz_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                dst: sz_val,
                ty: LirType::I64,
                value: elem_sz,
            });
            lir_args.push(sz_val);
        }

        // Concurrency constructors: gorget_mutex_new(size, &val),
        // gorget_shared_new(size, &val), gorget_rwlock_new(size, &val).
        // The GIR emits a single arg (the initial value). We prepend sizeof.
        if matches!(emit_name, "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new")
            && lir_args.len() == 1
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                dst: sz_val,
                ty: LirType::I64,
                value: elem_sz,
            });
            lir_args.insert(0, sz_val);
        }

        // gorget_channel_new(capacity, elem_size) — GIR passes (capacity).
        if emit_name == "gorget_channel_new" && lir_args.len() == 1 {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                dst: sz_val,
                ty: LirType::I64,
                value: elem_sz,
            });
            lir_args.push(sz_val);
        }

        // gorget_guard_set(guard, &val, sizeof) and gorget_write_guard_set
        if matches!(emit_name, "gorget_guard_set" | "gorget_write_guard_set")
            && lir_args.len() == 2
        {
            let elem_sz = concurrency_elem_size(original_name, self.module_structs).unwrap_or(8) as i64;
            let sz_val = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                dst: sz_val,
                ty: LirType::I64,
                value: elem_sz,
            });
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
            map_gir_type_with_structs(&gir_ty, self.gir_types, Some(self.struct_reg))
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

        // Pre-drop for gorget_array_set: drop the old element before overwriting.
        // The GIR backend does this inline; in LIR we emit the full drop sequence.
        if emit_name == "gorget_array_set" && lir_args.len() >= 3 {
            if let Some(elem_type) = collection_elem_type_from_name(original_name) {
                if type_needs_drop(elem_type, self.gir_types, &self.func_index) {
                    let arr_ptr = lir_args[0];
                    let idx = lir_args[1];
                    let old_ptr = self.lir_func.next_value();
                    self.ensure_extern("gorget_array_get", &[LirType::Ptr, LirType::I64], &LirType::Ptr);
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: Some(old_ptr),
                        name: "gorget_array_get".to_string(),
                        args: vec![arr_ptr, idx],
                    });
                    self.emit_drop_at_ptr(old_ptr, elem_type, bb);
                }
            }
        }

        let is_void_ret = matches!(ret_ty, LirType::Void);
        let result = if is_void_ret { None } else { dst.map(|_| self.lir_func.next_value()) };
        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
            dst: result,
            name: actual_emit_name,
            args: lir_args,
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
                                    let zero_val = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                                        dst: zero_val, ty: LirType::I32, value: 0,
                                    });
                                    let size_val = self.lir_func.next_value();
                                    self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                                        dst: size_val, ty: LirType::I64, value: byte_size,
                                    });
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
    fn lower_printf_args(&mut self, args: &[Operand], bb: BlockId) -> Vec<ValueId> {
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

                    // Str fields: 0=data (Ptr), 1=len (I64)
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
    fn lower_place_load(&mut self, place: &Place, bb: BlockId) -> ValueId {
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

    /// Elaborate a GIR Drop/DropIfAlive into LIR call sequences.
    fn lower_drop(&mut self, place: &Place, bb: BlockId) {
        use crate::ir::types::DropStrategy;

        let local_idx = place.local.0 as usize;
        let type_id = self.gir_func.locals[local_idx].type_id;

        // Look up the type name and drop strategy from the type registry.
        let (type_name, strategy) = if let Some(GirType::Named(name)) = self.gir_types.get(type_id) {
            let strat = if let Some(type_def) = self.gir_types.get_type_def(name) {
                type_def.metadata.drop_strategy.clone()
            } else {
                DropStrategy::None
            };
            (Some(name.clone()), strat)
        } else {
            (None, DropStrategy::None)
        };

        match strategy {
            DropStrategy::None => {
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
            }
            DropStrategy::Trivial(ref fn_name) if fn_name == "free" => {
                let slot = self.local_to_slot[local_idx];
                let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();

                // Check if this is a trait-object Box (struct with data+vtable)
                // vs a regular Box (raw pointer). Trait boxes need free(val.data).
                let is_trait_box = type_name.as_deref()
                    .and_then(|n| n.strip_prefix("Box__"))
                    .map(|inner| {
                        self.gir_types.get_type_def(&format!("{inner}_TraitObj")).is_some()
                    })
                    .unwrap_or(false);

                if is_trait_box {
                    // Trait box: free the .data field
                    let addr = self.lower_place_addr(place, bb);
                    // Find the struct_id for this Box type
                    if let Some(sid) = self.struct_reg.lookup(type_name.as_deref().unwrap_or("")) {
                        let data_ptr = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                            dst: data_ptr,
                            base: addr,
                            struct_id: sid,
                            field: 0, // data is field 0
                        });
                        let data_val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::Load {
                            dst: data_val,
                            ptr: data_ptr,
                            ty: LirType::Ptr,
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None,
                            name: "free".to_string(),
                            args: vec![data_val],
                        });
                    } else {
                        // Fallback: just free the whole value
                        let val = self.lir_func.next_value();
                        self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                            dst: val, slot, ty: slot_ty,
                        });
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None,
                            name: "free".to_string(),
                            args: vec![val],
                        });
                    }
                } else {
                    // Regular Box: check if inner type has a custom drop, call it first.
                    // Box__Tracked → inner = "Tracked", look up Tracked's drop strategy.
                    let inner_name = type_name.as_deref()
                        .and_then(|n| n.strip_prefix("Box__"));
                    if let Some(inner) = inner_name {
                        use crate::ir::types::DropStrategy as DS;
                        let inner_drop = self.gir_types.get_type_def(inner)
                            .map(|td| td.metadata.drop_strategy.clone())
                            .unwrap_or(DS::None);
                        let inner_drop_fn = match &inner_drop {
                            DS::Custom(fn_name) => Some(fn_name.clone()),
                            DS::Trivial(fn_name) if fn_name != "free" => Some(fn_name.clone()),
                            _ => None,
                        };
                        if let Some(drop_fn) = inner_drop_fn {
                            // Call inner drop: drop_fn(box_ptr)
                            // box_ptr IS the pointer to the inner value (Box is just a pointer)
                            let box_val = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                                dst: box_val, slot, ty: slot_ty.clone(),
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn.as_str()) {
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                    dst: None, func: fid, args: vec![box_val],
                                });
                            } else {
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None, name: drop_fn, args: vec![box_val],
                                });
                            }
                        }
                    }
                    // Then free the allocation
                    let val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                        dst: val, slot, ty: slot_ty,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None,
                        name: "free".to_string(),
                        args: vec![val],
                    });
                }
            }
            DropStrategy::Trivial(ref fn_name) => {
                // Trivial drop: single free/cleanup call. fn_name(&place)
                let addr = self.lower_place_addr(place, bb);

                // For collection types (Vector, Set, Dict), emit a special extern call
                // that tells the backend to generate element-level drops before freeing.
                // The name encodes the element drop function:
                //   __gorget_array_drop_elems__ElemDrop (for Vector)
                //   __gorget_map_drop_vals__ValDrop (for Dict)
                let is_array_free = fn_name == "gorget_array_free";
                let is_map_free = fn_name == "gorget_map_free";
                if is_array_free || is_map_free {
                    let elem_type_name = type_name.as_deref().and_then(|tn| {
                        if is_array_free {
                            tn.strip_prefix("Vector__").or_else(|| tn.strip_prefix("Deque__"))
                        } else {
                            tn.strip_prefix("Dict__").or_else(|| tn.strip_prefix("HashMap__"))
                                .and_then(|rest| {
                                    rest.find("__").map(|idx| &rest[idx + 2..])
                                })
                        }
                    });

                    if let Some(elem_name) = elem_type_name {
                        use crate::ir::types::DropStrategy as DS;
                        let elem_drop = self.gir_types.get_type_def(elem_name)
                            .map(|td| td.metadata.drop_strategy.clone())
                            .unwrap_or(DS::None);

                        // Check if this element type needs compound drops (Custom with
                        // droppable fields, Recursive, or nested collection).
                        let needs_recipe = self.elem_needs_compound_drop(elem_name);

                        if needs_recipe {
                            // Use recipe-based drop: the backend will look up the recipe
                            // and generate nested for-loops and field accesses.
                            let tag = if is_array_free {
                                format!("__gorget_array_drop_recipe__{elem_name}")
                            } else {
                                format!("__gorget_map_drop_recipe__{elem_name}")
                            };
                            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                dst: None,
                                name: tag,
                                args: vec![addr],
                            });
                        } else {
                            let elem_drop_fn = match &elem_drop {
                                DS::Trivial(fn_name) => Some(fn_name.clone()),
                                DS::Custom(fn_name) => Some(fn_name.clone()),
                                DS::Recursive => {
                                    let name = format!("{elem_name}__drop");
                                    if self.func_index.contains_key(name.as_str()) {
                                        Some(name)
                                    } else {
                                        None
                                    }
                                }
                                DS::None => None,
                            };
                            if let Some(drop_fn) = elem_drop_fn {
                                let tag = if is_array_free {
                                    format!("__gorget_array_drop_elems__{drop_fn}")
                                } else {
                                    format!("__gorget_map_drop_vals__{drop_fn}")
                                };
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None,
                                    name: tag,
                                    args: vec![addr],
                                });
                            }
                        }
                    }
                }

                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: None, func: fid, args: vec![addr],
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![addr],
                    });
                }
            }
            DropStrategy::Custom(ref fn_name) => {
                // Custom drop: call user drop, then drop fields recursively.
                // Guard with memcmp zero check — if the struct was moved (zeroed),
                // skip the drop entirely.  Mirrors GIR backend emit_drop_code.
                let addr = self.lower_place_addr(place, bb);
                let byte_size = self.compute_place_byte_size(place);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: format!("__gorget_drop_if_alive_open__{byte_size}"),
                    args: vec![addr],
                });
                // Re-compute addr after the guard since we emitted new instructions.
                let addr2 = self.lower_place_addr(place, bb);
                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: None, func: fid, args: vec![addr2],
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![addr2],
                    });
                }
                self.lower_field_drops(place, &type_name, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_drop_if_alive_close".to_string(),
                    args: vec![],
                });
            }
            DropStrategy::Recursive => {
                // Guard with memcmp zero check for recursive drops too.
                let addr = self.lower_place_addr(place, bb);
                let byte_size = self.compute_place_byte_size(place);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: format!("__gorget_drop_if_alive_open__{byte_size}"),
                    args: vec![addr],
                });
                self.lower_field_drops(place, &type_name, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_drop_if_alive_close".to_string(),
                    args: vec![],
                });
            }
        }
    }

    /// Compute the byte size of a place's type for memcmp zero checks.
    fn compute_place_byte_size(&self, place: &Place) -> usize {
        let local_idx = place.local.0 as usize;
        let type_id = self.gir_func.locals[local_idx].type_id;
        let lir_ty = map_gir_type_with_structs(&type_id, self.gir_types, Some(self.struct_reg));
        match &lir_ty {
            LirType::Struct(_) => c_sizeof_lir_type(&lir_ty, &self.module_structs),
            _ => super::types::scalar_size(&lir_ty).unwrap_or(8) as usize,
        }
    }

    /// Emit the full drop sequence for a type at a given pointer address.
    /// Used for pre-drops (e.g., dropping old element before `gorget_array_set`).
    /// The `type_name` is the GIR type name (e.g., "Container", "Vector__Container").
    fn emit_drop_at_ptr(&mut self, ptr: ValueId, type_name: &str, bb: BlockId) {
        use crate::ir::types::DropStrategy;
        let type_def = match self.gir_types.get_type_def(type_name) {
            Some(td) => td,
            None => return,
        };
        let strategy = type_def.metadata.drop_strategy.clone();
        match strategy {
            DropStrategy::None => {}
            DropStrategy::Trivial(ref fn_name) if fn_name == "free" => {
                // Box-like: free the pointer
                let val = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: val, ptr, ty: LirType::Ptr,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None, name: "free".to_string(), args: vec![val],
                });
            }
            DropStrategy::Trivial(ref fn_name) => {
                // Collection free: may need element-level drops first.
                let is_array_free = fn_name == "gorget_array_free";
                let is_map_free = fn_name == "gorget_map_free";
                if is_array_free || is_map_free {
                    let elem_type_name = if is_array_free {
                        type_name.strip_prefix("Vector__").or_else(|| type_name.strip_prefix("Deque__"))
                    } else {
                        type_name.strip_prefix("Dict__").or_else(|| type_name.strip_prefix("HashMap__"))
                            .and_then(|rest| rest.find("__").map(|idx| &rest[idx + 2..]))
                    };
                    if let Some(elem_name) = elem_type_name {
                        let needs_recipe = self.elem_needs_compound_drop(elem_name);
                        if needs_recipe {
                            let tag = if is_array_free {
                                format!("__gorget_array_drop_recipe__{elem_name}")
                            } else {
                                format!("__gorget_map_drop_recipe__{elem_name}")
                            };
                            self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                dst: None, name: tag, args: vec![ptr],
                            });
                        } else {
                            use crate::ir::types::DropStrategy as DS;
                            let elem_drop = self.gir_types.get_type_def(elem_name)
                                .map(|td| td.metadata.drop_strategy.clone())
                                .unwrap_or(DS::None);
                            let elem_drop_fn = match &elem_drop {
                                DS::Trivial(f) => Some(f.clone()),
                                DS::Custom(f) => Some(f.clone()),
                                DS::Recursive => {
                                    let name = format!("{elem_name}__drop");
                                    if self.func_index.contains_key(name.as_str()) { Some(name) } else { None }
                                }
                                DS::None => None,
                            };
                            if let Some(drop_fn) = elem_drop_fn {
                                let tag = if is_array_free {
                                    format!("__gorget_array_drop_elems__{drop_fn}")
                                } else {
                                    format!("__gorget_map_drop_vals__{drop_fn}")
                                };
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None, name: tag, args: vec![ptr],
                                });
                            }
                        }
                    }
                }
                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: None, func: fid, args: vec![ptr],
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![ptr],
                    });
                }
            }
            DropStrategy::Custom(ref fn_name) => {
                if let Some(&fid) = self.func_index.get(fn_name.as_str()) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: None, func: fid, args: vec![ptr],
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: None, name: fn_name.clone(), args: vec![ptr],
                    });
                }
                // Also drop fields recursively.
                self.emit_field_drops_at_ptr(ptr, type_name, bb);
            }
            DropStrategy::Recursive => {
                self.emit_field_drops_at_ptr(ptr, type_name, bb);
            }
        }
    }

    /// Emit field-by-field drops for a struct at a given pointer.
    fn emit_field_drops_at_ptr(&mut self, base_ptr: ValueId, type_name: &str, bb: BlockId) {
        use crate::ir::types::DropStrategy;
        let type_def = match self.gir_types.get_type_def(type_name) {
            Some(td) => td,
            None => return,
        };
        if let crate::ir::types::TypeDefKind::Struct(ref sdef) = type_def.kind {
            let struct_id = self.struct_reg.lookup(type_name).unwrap_or(StructId(0));
            let fields: Vec<_> = sdef.fields.iter().enumerate().map(|(i, f)| {
                let field_type_name = match self.gir_types.get(f.type_id) {
                    Some(GirType::Named(n)) => Some(n.clone()),
                    _ => None,
                };
                let drop_fn = field_type_name.as_ref().and_then(|n| {
                    self.gir_types.get_type_def(n).map(|td| td.metadata.drop_strategy.clone())
                }).unwrap_or(DropStrategy::None);
                let fn_name = match &drop_fn {
                    DropStrategy::Trivial(f) | DropStrategy::Custom(f) => Some(f.clone()),
                    DropStrategy::Recursive => field_type_name.as_ref().map(|n| format!("{n}__drop")),
                    DropStrategy::None => None,
                };
                (i as u32, fn_name)
            }).collect();
            for (field_idx, drop_fn_name) in fields {
                if let Some(drop_fn) = drop_fn_name {
                    let field_ptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: field_ptr, base: base_ptr, struct_id, field: field_idx,
                    });
                    if let Some(&fid) = self.func_index.get(drop_fn.as_str()) {
                        self.lir_func.block_mut(bb).insts.push(Inst::Call {
                            dst: None, func: fid, args: vec![field_ptr],
                        });
                    } else {
                        self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                            dst: None, name: drop_fn, args: vec![field_ptr],
                        });
                    }
                }
            }
        }
    }

    /// Emit field-by-field drops for a struct value (used by Recursive and Custom strategies).
    fn lower_field_drops(&mut self, place: &Place, type_name: &Option<String>, bb: BlockId) {
        use crate::ir::types::DropStrategy;
        if let Some(type_name) = type_name {
            if let Some(type_def) = self.gir_types.get_type_def(type_name) {
                if let crate::ir::types::TypeDefKind::Struct(ref sdef) = type_def.kind {
                    let base_addr = self.lower_place_addr(place, bb);
                    let struct_id = self.struct_reg.lookup(type_name).unwrap_or(StructId(0));
                    for (field_idx, field) in sdef.fields.iter().enumerate() {
                        let field_type_name = match self.gir_types.get(field.type_id) {
                            Some(GirType::Named(n)) => Some(n.clone()),
                            _ => None,
                        };
                        let field_drop = field_type_name.as_ref().and_then(|n| {
                            self.gir_types.get_type_def(n).map(|td| td.metadata.drop_strategy.clone())
                        }).unwrap_or(DropStrategy::None);
                        let drop_fn = match &field_drop {
                            DropStrategy::Trivial(fn_name) | DropStrategy::Custom(fn_name) => Some(fn_name.clone()),
                            DropStrategy::Recursive => {
                                field_type_name.as_ref().map(|n| format!("{n}__drop"))
                            }
                            DropStrategy::None => None,
                        };
                        if let Some(drop_fn_name) = drop_fn {
                            let field_ptr = self.lir_func.next_value();
                            self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                                dst: field_ptr,
                                base: base_addr,
                                struct_id,
                                field: field_idx as u32,
                            });
                            if let Some(&fid) = self.func_index.get(drop_fn_name.as_str()) {
                                self.lir_func.block_mut(bb).insts.push(Inst::Call {
                                    dst: None, func: fid, args: vec![field_ptr],
                                });
                            } else {
                                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                                    dst: None, name: drop_fn_name, args: vec![field_ptr],
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    /// Infer drop strategy for a type, using name-based fallback for collection types.
    fn infer_drop_strategy(&self, type_name: &str) -> crate::ir::types::DropStrategy {
        use crate::ir::types::DropStrategy;
        if let Some(td) = self.gir_types.get_type_def(type_name) {
            return td.metadata.drop_strategy.clone();
        }
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

    /// Check if an element type needs compound drops (recipe-based).
    /// True when the type has Custom drop with droppable fields, Recursive drop
    /// (with or without explicit __drop fn), or is a collection with droppable elements.
    fn elem_needs_compound_drop(&self, type_name: &str) -> bool {
        use crate::ir::types::DropStrategy;
        let strategy = self.infer_drop_strategy(type_name);
        match &strategy {
            DropStrategy::None => false,
            DropStrategy::Trivial(fn_name) => {
                // Collection with droppable elements that themselves need compound drops?
                let is_collection_free = fn_name == "gorget_array_free" || fn_name == "gorget_map_free";
                if !is_collection_free {
                    return false;
                }
                // Check if inner elements need compound drops
                let is_array = fn_name == "gorget_array_free";
                let elem_name = if is_array {
                    type_name.strip_prefix("Vector__").or_else(|| type_name.strip_prefix("Deque__"))
                } else {
                    type_name.strip_prefix("Dict__").or_else(|| type_name.strip_prefix("HashMap__"))
                        .and_then(|rest| rest.find("__").map(|idx| &rest[idx + 2..]))
                };
                elem_name.map_or(false, |en| self.elem_needs_compound_drop(en))
            }
            DropStrategy::Custom(_) => {
                // Check if the type has any droppable fields
                self.type_has_droppable_fields(type_name)
            }
            DropStrategy::Recursive => {
                // Recursive always needs compound drop (field-by-field)
                true
            }
        }
    }

    /// Check if a struct type has any fields with non-None drop strategy.
    fn type_has_droppable_fields(&self, type_name: &str) -> bool {
        use crate::ir::types::DropStrategy;
        let type_def = match self.gir_types.get_type_def(type_name) {
            Some(td) => td,
            None => return false,
        };
        let sdef = match &type_def.kind {
            crate::ir::types::TypeDefKind::Struct(s) => s,
            _ => return false,
        };
        for field in &sdef.fields {
            let field_type_name = match self.gir_types.get(field.type_id) {
                Some(GirType::Named(n)) => n.clone(),
                _ => continue,
            };
            let field_drop = self.infer_drop_strategy(&field_type_name);
            if !matches!(field_drop, DropStrategy::None) {
                return true;
            }
        }
        false
    }

    /// Get the address of a GIR place.
    fn lower_place_addr(&mut self, place: &Place, bb: BlockId) -> ValueId {
        let slot = self.local_to_slot[place.local.0 as usize];
        let mut addr = self.lir_func.next_value();
        self.lir_func
            .block_mut(bb)
            .insts
            .push(Inst::SlotAddr { dst: addr, slot });

        // Track the current GIR type through each projection step.
        let mut current_gir_type = self.gir_func.locals[place.local.0 as usize].type_id;

        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    let struct_id = self.resolve_struct_id_for_field(current_gir_type, *field, self.module_structs);
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: next,
                        base: addr,
                        struct_id,
                        field: *field,
                    });
                    addr = next;
                    // Update type to the field's type for subsequent projections.
                    current_gir_type = self.resolve_field_gir_type_id(current_gir_type, *field);
                }
                Projection::Index(idx_local) => {
                    let idx_slot = self.local_to_slot[idx_local.0 as usize];
                    let idx = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                        dst: idx,
                        slot: idx_slot,
                        ty: LirType::I64,
                    });
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                        dst: next,
                        base: addr,
                        index: idx,
                        elem_size: 8,
                    });
                    addr = next;
                }
                Projection::Deref => {
                    // Load the pointer from addr, then use that as the new addr.
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: ptr_val,
                        ptr: addr,
                        ty: LirType::Ptr,
                    });
                    addr = ptr_val;
                    // Update type to the pointee type.
                    current_gir_type = self.resolve_deref_gir_type_id(current_gir_type);
                }
            }
        }

        addr
    }

    /// Lower a GIR constant to a LIR value.
    fn lower_constant(&mut self, c: &Constant, bb: BlockId) -> ValueId {
        let dst = self.lir_func.next_value();
        let inst = match c {
            Constant::Bool(v) => Inst::BoolConst { dst, value: *v },
            Constant::I8(v) => Inst::IConst { dst, ty: LirType::I8, value: *v as i64 },
            Constant::I16(v) => Inst::IConst { dst, ty: LirType::I16, value: *v as i64 },
            Constant::I32(v) => Inst::IConst { dst, ty: LirType::I32, value: *v as i64 },
            Constant::I64(v) => Inst::IConst { dst, ty: LirType::I64, value: *v },
            Constant::U8(v) => Inst::IConst { dst, ty: LirType::U8, value: *v as i64 },
            Constant::U16(v) => Inst::IConst { dst, ty: LirType::U16, value: *v as i64 },
            Constant::U32(v) => Inst::IConst { dst, ty: LirType::U32, value: *v as i64 },
            Constant::U64(v) => Inst::IConst { dst, ty: LirType::U64, value: *v as i64 },
            Constant::F32(v) => Inst::FConst { dst, ty: LirType::F32, bits: (*v as f64).to_bits() },
            Constant::F64(v) => Inst::FConst { dst, ty: LirType::F64, bits: v.to_bits() },
            Constant::Str(s) => Inst::StrLit { dst, value: s.clone() },
            Constant::Null => Inst::NullPtr { dst },
            Constant::Unit => Inst::IConst { dst, ty: LirType::I32, value: 0 }, // unit = zero
            Constant::SizeOf(type_id) => {
                let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
                let size = c_sizeof_lir_type(&ty, self.module_structs);
                Inst::IConst { dst, ty: LirType::I64, value: size as i64 }
            }
            Constant::FuncRef(name) => {
                if let Some(fid) = self.func_index.get(name) {
                    Inst::FuncAddr { dst, func: *fid }
                } else {
                    // Unknown function — emit as a string for now.
                    Inst::IConst { dst, ty: LirType::I64, value: 0 }
                }
            }
            Constant::GlobalRef(name) => {
                if let Some(&gid) = self.global_index.get(name) {
                    // Load the global's value: take address, then load.
                    let addr = self.lir_func.next_value();
                    let global_ty = self.module_globals[gid.0 as usize].ty.clone();
                    self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr { dst: addr, global: gid });
                    Inst::Load { dst, ptr: addr, ty: global_ty }
                } else {
                    Inst::NullPtr { dst }
                }
            }
            Constant::GlobalRefPtr(name) => {
                if let Some(&gid) = self.global_index.get(name) {
                    Inst::GlobalAddr { dst, global: gid }
                } else {
                    Inst::NullPtr { dst }
                }
            }
        };
        self.lir_func.block_mut(bb).insts.push(inst);
        dst
    }

    // ── Store helpers ───────────────────────────────────────────────────────

    /// Derive the LIR type of a GIR operand.
    /// Get the GIR type name for an operand (for type-aware dispatch).
    fn operand_gir_type_name(&self, operand: &Operand) -> Option<String> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let idx = place.local.0 as usize;
                if idx < self.gir_func.locals.len() {
                    let gir_ty = self.gir_func.locals[idx].type_id;
                    match self.gir_types.get(gir_ty) {
                        Some(GirType::Named(name)) => Some(name.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn operand_lir_type(&self, operand: &Operand) -> LirType {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let idx = place.local.0 as usize;
                if idx < self.gir_func.locals.len() {
                    let gir_ty = self.gir_func.locals[idx].type_id;
                    map_gir_type_with_structs(&gir_ty, self.gir_types, Some(self.struct_reg))
                } else {
                    LirType::Ptr
                }
            }
            Operand::Constant(c) => match c {
                Constant::I8(_) | Constant::I16(_) | Constant::I32(_) | Constant::I64(_)
                | Constant::U8(_) | Constant::U16(_) | Constant::U32(_) | Constant::U64(_)
                | Constant::SizeOf(_) => LirType::I64,
                Constant::F32(_) | Constant::F64(_) => LirType::F64,
                Constant::Bool(_) => LirType::Bool,
                Constant::Str(_) | Constant::Null | Constant::FuncRef(_) | Constant::GlobalRef(_) | Constant::GlobalRefPtr(_) => LirType::Ptr,
                Constant::Unit => LirType::Void,
            },
        }
    }

    /// Ensure a synthetic extern declaration exists for an unknown function.
    /// If the extern already exists from a previous call site, merge parameter types
    /// by preferring more specific types (e.g., Struct over Ptr).
    fn ensure_extern(&mut self, name: &str, arg_types: &[LirType], ret_ty: &LirType) {
        // For known runtime functions, use canonical signatures instead of call-site inference.
        if let Some((canon_params, canon_ret)) = runtime_extern_sig(name, self.struct_reg) {
            if let Some(existing) = self.pending_externs.iter_mut().find(|e| e.name == name) {
                existing.params = canon_params;
                existing.return_type = canon_ret;
            } else {
                self.pending_externs.push(LirExtern {
                    name: name.to_string(),
                    params: canon_params,
                    return_type: canon_ret,
                    is_variadic: false,
                });
            }
            return;
        }

        // Detect newtype constructors: if the function name matches a struct name,
        // the return type should be that struct (not i64 or i32 from GIR's extern decl).
        let actual_ret = if let Some(sid) = self.struct_reg.lookup(name) {
            LirType::Struct(sid)
        } else {
            ret_ty.clone()
        };

        if let Some(existing) = self.pending_externs.iter_mut().find(|e| e.name == name) {
            // Merge param types: prefer aggregate/specific types over Ptr.
            for (i, new_ty) in arg_types.iter().enumerate() {
                if i < existing.params.len() {
                    if matches!(existing.params[i], LirType::Ptr) && !matches!(new_ty, LirType::Ptr) {
                        existing.params[i] = new_ty.clone();
                    }
                }
            }
            // Also update return type if existing is I64 and new is more specific.
            if matches!(existing.return_type, LirType::I64 | LirType::I32) && !matches!(actual_ret, LirType::I64 | LirType::I32) {
                existing.return_type = actual_ret;
            }
            return;
        }
        self.pending_externs.push(LirExtern {
            name: name.to_string(),
            params: arg_types.to_vec(),
            return_type: actual_ret,
            is_variadic: false,
        });
    }

    fn store_to_local(&mut self, local: ir::types::LocalId, value: ValueId, bb: BlockId) {
        let slot = self.local_to_slot[local.0 as usize];
        self.lir_func
            .block_mut(bb)
            .insts
            .push(Inst::SlotStore { slot, value });
    }

    fn store_to_place(&mut self, place: &Place, value: ValueId, bb: BlockId) {
        if place.projections.is_empty() {
            self.store_to_local(place.local, value, bb);
        } else {
            let addr = self.lower_place_addr(place, bb);
            self.lir_func
                .block_mut(bb)
                .insts
                .push(Inst::Store { ptr: addr, value });
        }
    }

    // ── Type resolution helpers ─────────────────────────────────────────────

    fn resolve_struct_id(&self, gir_type_id: GirTypeId) -> StructId {
        let gir_type = self.gir_types.get(gir_type_id);
        match gir_type {
            Some(GirType::Named(name)) => {
                if let Some(sid) = self.struct_reg.lookup(&name) {
                    return sid;
                }
            }
            // Unwrap pointer/ref types to find the inner Named type (e.g. &Color → Color).
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                return self.resolve_struct_id(*inner);
            }
            _ => {}
        }
        StructId(0) // fallback
    }

    /// Resolve struct ID with field-count safety: if the resolved struct has
    /// fewer fields than the field index, try a wider compatible type.
    /// Handles Str→GorgetString promotion (GIR uses GorgetString fields on Str locals).
    fn resolve_struct_id_for_field(&self, gir_type_id: GirTypeId, field: u32, structs: &[StructDef]) -> StructId {
        let sid = self.resolve_struct_id(gir_type_id);
        let struct_def = &structs[sid.0 as usize];
        if (field as usize) < struct_def.fields.len() {
            return sid;
        }
        // Str (2 fields) with field >= 2 → GorgetString (3 fields).
        if struct_def.name == "Str" {
            if let Some(gs_sid) = self.struct_reg.lookup("GorgetString") {
                return gs_sid;
            }
        }
        sid
    }

    fn resolve_type_name(&self, gir_type_id: GirTypeId) -> String {
        let gir_type = self.gir_types.get(gir_type_id);
        // Unwrap Ptr/MutPtr to find the inner Named type.
        let inner = match gir_type {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => self.gir_types.get(*inner),
            other => other,
        };
        if let Some(GirType::Named(name)) = inner {
            name.clone()
        } else {
            String::new()
        }
    }

    fn resolve_field_type(&self, gir_type_id: GirTypeId, field: u32) -> LirType {
        let gir_type = self.gir_types.get(gir_type_id);
        // Unwrap Ptr/MutPtr to find the inner Named type.
        let inner_type = match gir_type {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => self.gir_types.get(*inner),
            other => other,
        };
        if let Some(GirType::Named(name)) = inner_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return map_gir_type_with_structs(&f.type_id, self.gir_types, Some(self.struct_reg));
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    /// Return the GIR TypeId of a struct field (for tracking types through projection chains).
    fn resolve_field_gir_type_id(&self, gir_type_id: GirTypeId, field: u32) -> GirTypeId {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return f.type_id;
                    }
                }
            }
        }
        gir_type_id // fallback: keep same type
    }

    /// Resolve the pointee type for a Deref projection.
    fn resolve_deref_gir_type_id(&self, gir_type_id: GirTypeId) -> GirTypeId {
        match self.gir_types.get(gir_type_id) {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
            Some(GirType::Named(name)) if name.starts_with("Box__") => {
                // Box types are Named("Box__X") — the inner type is encoded in the name.
                if let Some(type_def) = self.gir_types.get_type_def(name.as_str()) {
                    if let crate::ir::types::TypeDefKind::Struct(ref s) = type_def.kind {
                        if let Some(f) = s.fields.first() {
                            return f.type_id;
                        }
                    }
                }
                gir_type_id // fallback — resolve_place_type has name-based fallback
            }
            _ => gir_type_id, // fallback
        }
    }

    /// Compute the effective GIR type after following all projections in a place.
    fn effective_place_type(&self, place: &Place) -> GirTypeId {
        let mut ty = self.gir_func.locals[place.local.0 as usize].type_id;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    ty = self.resolve_field_gir_type_id(ty, *field);
                }
                Projection::Deref => {
                    ty = self.resolve_deref_gir_type_id(ty);
                }
                Projection::Index(_) => {
                    // Element type — keep as-is for now (array element type tracking TBD)
                }
            }
        }
        ty
    }

    fn resolve_enum_field_type(
        &self,
        gir_type_id: GirTypeId,
        variant_name: &str,
        field: u32,
    ) -> LirType {
        // Unwrap Ptr/MutPtr to get to the Named enum type.
        let mut tid = gir_type_id;
        loop {
            match self.gir_types.get(tid) {
                Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) => tid = *inner,
                _ => break,
            }
        }
        if let Some(GirType::Named(name)) = self.gir_types.get(tid) {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                    for v in &edef.variants {
                        if v.name == variant_name {
                            if let Some(f) = v.fields.get(field as usize) {
                                return map_gir_type_with_structs(&f.type_id, self.gir_types, Some(self.struct_reg));
                            }
                        }
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    fn resolve_variant_ordinal(&self, type_name: &str, variant_name: &str) -> usize {
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                for (i, v) in edef.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return i;
                    }
                }
            }
        }
        0
    }

    fn resolve_variant_field_offset(&self, type_name: &str, variant_name: &str) -> usize {
        // Field offset = 1 (tag) + sum of field counts of preceding variants.
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                let mut offset = 1; // tag field
                for v in &edef.variants {
                    if v.name == variant_name {
                        return offset;
                    }
                    offset += v.fields.len();
                }
            }
        }
        1
    }

    /// Get the GIR type IDs for a specific variant's fields.
    fn resolve_variant_field_types(&self, type_name: &str, variant_name: &str) -> Vec<Option<GirTypeId>> {
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                for v in &edef.variants {
                    if v.name == variant_name {
                        return v.fields.iter().map(|f| Some(f.type_id)).collect();
                    }
                }
            }
        }
        vec![]
    }

    /// Materialize a properly tagged null-variant enum for an Assign { dst, Null }.
    /// Handles both simple locals (`dst.projections.is_empty()`) and projected
    /// field assignments (`local.field[i] = Null`).
    fn try_materialize_null_for_assign(&mut self, dst: &Place, bb: BlockId) -> Option<()> {
        let local_idx = dst.local.0 as usize;
        if local_idx >= self.gir_func.locals.len() { return None; }

        // Resolve the target type through projections.
        let gir_ty = if dst.projections.is_empty() {
            self.gir_func.locals[local_idx].type_id
        } else {
            self.resolve_projected_gir_type(dst)?
        };

        let (struct_id, tag_ordinal) = self.find_enum_null_variant(gir_ty)?;

        if dst.projections.is_empty() {
            // Simple local: write tag into the local's slot.
            let slot = self.local_to_slot[local_idx];
            let base = self.lir_func.next_value();
            self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot });
            self.emit_enum_tag_store(base, struct_id, tag_ordinal, bb);
        } else {
            // Projected field: compute the field address, then write tag there.
            let base = self.lower_place_addr(dst, bb);
            self.emit_enum_tag_store(base, struct_id, tag_ordinal, bb);
        }
        Some(())
    }

    /// When a GIR Assign copies from an Option/Result-typed source to a
    /// non-Option/Result destination, the GIR C backend implicitly extracts
    /// the payload (e.g. `_21 = _23.data.Some._0`).  We replicate this by
    /// emitting FieldPtr(field=1) + Load on the source enum struct.
    fn try_enum_payload_extract(
        &mut self,
        dst: &Place,
        value: &Operand,
        bb: BlockId,
    ) -> Option<ValueId> {
        // Only applies to Copy/Move of a simple local (no projections on source).
        let src_local = match value {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return None,
        };

        let src_idx = src_local.0 as usize;
        let dst_idx = dst.local.0 as usize;
        if src_idx >= self.gir_func.locals.len() || dst_idx >= self.gir_func.locals.len() {
            return None;
        }

        let src_type_id = self.gir_func.locals[src_idx].type_id;
        let dst_type_id = self.gir_func.locals[dst_idx].type_id;

        // Check: source is Option__* or Result__*, destination is NOT.
        let src_name = match self.gir_types.get(src_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return None,
        };
        let is_option = src_name.starts_with("Option__");
        let is_result = src_name.starts_with("Result__");
        if !is_option && !is_result {
            return None;
        }

        // Destination must not be the same enum type.
        let dst_is_same = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => *n == src_name,
            _ => false,
        };
        if dst_is_same {
            return None;
        }

        // Also skip if destination is another Option/Result.
        let dst_is_enum = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => n.starts_with("Option__") || n.starts_with("Result__"),
            _ => false,
        };
        if dst_is_enum {
            return None;
        }

        // Extract the payload: field 1 for Option (Some_0), field 1 for Result (Ok_0).
        let struct_id = self.resolve_struct_id(src_type_id);
        let payload_field: u32 = 1;

        let src_slot = self.local_to_slot[src_idx];
        let base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot: src_slot });

        let fptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: fptr,
            base,
            struct_id,
            field: payload_field,
        });

        let field_ty = self.resolve_enum_field_type(src_type_id, if is_option { "Some" } else { "Ok" }, 0);
        let result = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Load {
            dst: result,
            ptr: fptr,
            ty: field_ty,
        });

        Some(result)
    }

    /// Detect `Box[Trait] ← Box[Concrete]` assignments and construct the trait object
    /// by setting field 0 (data) = src value and field 1 (vtable) = &Trait_for_Concrete_vtable.
    fn try_trait_object_construct(
        &mut self,
        dst: &Place,
        value: &Operand,
        bb: BlockId,
    ) -> bool {
        let src_local = match value {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return false,
        };
        let src_idx = src_local.0 as usize;
        let dst_idx = dst.local.0 as usize;
        if src_idx >= self.gir_func.locals.len() || dst_idx >= self.gir_func.locals.len() {
            return false;
        }
        let dst_type_id = self.gir_func.locals[dst_idx].type_id;
        let src_type_id = self.gir_func.locals[src_idx].type_id;
        let dst_name = match self.gir_types.get(dst_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return false,
        };
        let src_name = match self.gir_types.get(src_type_id) {
            Some(GirType::Named(n)) => n.clone(),
            _ => return false,
        };
        // Both must be Box__ types with different inner types.
        if !dst_name.starts_with("Box__") || !src_name.starts_with("Box__") {
            return false;
        }
        let dst_inner = &dst_name[5..];
        let src_inner = &src_name[5..];
        if dst_inner == src_inner {
            return false;
        }
        // Check that a VTable type exists for the trait (dst_inner is the trait name).
        let vtable_type = format!("{dst_inner}_VTable");
        if self.gir_types.get_type_def(&vtable_type).is_none() {
            return false;
        }
        // Find the trait object struct (e.g. Describer_TraitObj).
        let trait_obj_type = format!("{dst_inner}_TraitObj");
        let trait_obj_sid = match self.struct_reg.lookup(&trait_obj_type) {
            Some(sid) => sid,
            None => return false,
        };
        // Find the vtable global (e.g. Describer_for_Widget_vtable).
        let vtable_global_name = format!("{dst_inner}_for_{src_inner}_vtable");
        let vtable_gid = match self.global_index.get(&vtable_global_name) {
            Some(&gid) => gid,
            None => return false,
        };

        // Construct the trait object:
        // field 0 (data) = src value (cast to void*)
        // field 1 (vtable) = &vtable_global
        let dst_slot = self.local_to_slot[dst_idx];
        let dst_base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: dst_base,
            slot: dst_slot,
        });

        // Load src value (Box__Concrete = void*).
        // Box types are represented as LirType::Struct in LIR but are actually void*
        // at runtime. lower_operand returns the slot address for aggregates, so we
        // need to explicitly load the pointer value from the slot.
        let src_slot = self.local_to_slot[src_idx];
        let src_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
            dst: src_addr,
            slot: src_slot,
        });
        let src_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::Load {
            dst: src_val,
            ptr: src_addr,
            ty: LirType::Ptr,
        });

        // Store data pointer (field 0).
        let data_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: data_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: data_ptr,
            value: src_val,
        });

        // Store vtable pointer (field 1).
        let vtable_addr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::GlobalAddr {
            dst: vtable_addr,
            global: vtable_gid,
        });
        let vtable_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: vtable_ptr,
            base: dst_base,
            struct_id: trait_obj_sid,
            field: 1,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: vtable_ptr,
            value: vtable_addr,
        });

        true
    }

    /// Emit instructions to set the tag field of an enum at `base` address.
    fn emit_enum_tag_store(&mut self, base: ValueId, struct_id: StructId, tag_ordinal: usize, bb: BlockId) {
        let tag_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
            dst: tag_val,
            ty: LirType::I32,
            value: tag_ordinal as i64,
        });
        let tag_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: tag_ptr, base, struct_id, field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: tag_ptr, value: tag_val,
        });
    }

    /// Resolve the GIR type of a Place by walking projections.
    fn resolve_projected_gir_type(&self, place: &Place) -> Option<GirTypeId> {
        let mut current_type = self.gir_func.locals[place.local.0 as usize].type_id;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            match &def.kind {
                                gir_types::TypeDefKind::Struct(sdef) => {
                                    if let Some(f) = sdef.fields.get(*field as usize) {
                                        current_type = f.type_id;
                                        continue;
                                    }
                                }
                                gir_types::TypeDefKind::Enum(edef) => {
                                    // Field 0 = tag, field 1+ = variant payloads
                                    // The payload fields are numbered across variants.
                                    let mut fi = 0u32;
                                    for v in &edef.variants {
                                        for vf in &v.fields {
                                            fi += 1; // tag takes field 0
                                            if fi == *field {
                                                current_type = vf.type_id;
                                                break;
                                            }
                                        }
                                    }
                                    continue;
                                }
                                _ => {}
                            }
                        }
                    }
                    return None;
                }
                Projection::Deref | Projection::Index(_) => {
                    return None; // Conservative: can't resolve through deref/index.
                }
            }
        }
        Some(current_type)
    }

    /// If `gir_ty` is an enum, find the first variant with no fields (the "null" variant,
    /// e.g. None for Option, Error for Result).  Returns `(StructId, tag_ordinal)`.
    fn find_enum_null_variant(&self, gir_ty: GirTypeId) -> Option<(StructId, usize)> {
        let gir_type = self.gir_types.get(gir_ty)?;
        if let GirType::Named(name) = gir_type {
            let def = self.gir_types.get_type_def(&name)?;
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                let struct_id = self.struct_reg.lookup(&name)?;
                for (i, v) in edef.variants.iter().enumerate() {
                    if v.fields.is_empty() {
                        return Some((struct_id, i));
                    }
                }
            }
        }
        None
    }

    /// For a collection method call like `Vector__Option__int64_t__push`,
    /// when a `Constant::Null` arg is passed as the element, create a properly
    /// tagged enum slot on the stack and return its address.
    /// Returns `None` if we can't determine the element type.
    fn materialize_null_enum_for_collection_arg(&mut self, func_name: &str, bb: BlockId) -> Option<ValueId> {
        // Extract the element type name from monomorphized call names.
        // Patterns: Vector__ELEM__push, Vector__ELEM__set, Set__ELEM__add,
        //           Heap__ELEM__push, gorget_channel_send, etc.
        let elem_type_name = Self::extract_elem_type_from_method_name(func_name)?;

        // Look up the struct and find the null variant (first fieldless variant).
        let struct_id = self.struct_reg.lookup(&elem_type_name)?;
        let lir_struct = self.module_structs.get(struct_id.0 as usize)?;

        // Find the null variant tag by looking up the GIR type def.
        let tag_ordinal = self.find_null_variant_tag_by_name(&elem_type_name)?;

        // Create a temporary slot of the enum type.
        let slot = self.lir_func.add_slot(LirType::Struct(struct_id), None);
        let base = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr { dst: base, slot });

        // Zero-init the slot first (memset 0).
        let zero = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
            dst: zero, ty: LirType::I32, value: 0,
        });
        // Set the tag field to the null variant ordinal.
        let tag_val = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::IConst {
            dst: tag_val, ty: LirType::I32, value: tag_ordinal as i64,
        });
        let tag_ptr = self.lir_func.next_value();
        self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
            dst: tag_ptr, base, struct_id, field: 0,
        });
        self.lir_func.block_mut(bb).insts.push(Inst::Store {
            ptr: tag_ptr, value: tag_val,
        });
        Some(base)
    }

    /// Find the null variant tag ordinal by struct name.
    fn find_null_variant_tag_by_name(&self, name: &str) -> Option<usize> {
        let def = self.gir_types.get_type_def(name)?;
        if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
            for (i, v) in edef.variants.iter().enumerate() {
                if v.fields.is_empty() {
                    return Some(i);
                }
            }
        }
        None
    }

    /// Extract element type name from a monomorphized collection method name.
    /// E.g., "Vector__Option__int64_t__push" → "Option__int64_t"
    fn extract_elem_type_from_method_name(func_name: &str) -> Option<String> {
        // Collection prefixes and their method suffixes
        let prefixes = ["Vector__", "Set__", "Heap__", "HashSet__", "Deque__"];
        let suffixes = ["__push", "__add", "__set", "__contains", "__remove",
                        "__insert", "__index_of", "__binary_search"];
        for prefix in &prefixes {
            if let Some(rest) = func_name.strip_prefix(prefix) {
                for suffix in &suffixes {
                    if let Some(elem) = rest.strip_suffix(suffix) {
                        if !elem.is_empty() {
                            return Some(elem.to_string());
                        }
                    }
                }
            }
        }
        None
    }

    fn resolve_place_type(&self, place: &Place) -> LirType {
        let local_type = self.gir_func.locals[place.local.0 as usize].type_id;
        if place.projections.is_empty() {
            return map_gir_type_with_structs(&local_type, self.gir_types, Some(self.struct_reg));
        }

        // Walk projections to determine final type.
        let mut current_type = local_type;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                                if let Some(f) = sdef.fields.get(*field as usize) {
                                    current_type = f.type_id;
                                    continue;
                                }
                            }
                        }
                    }
                    return LirType::I64; // fallback
                }
                Projection::Deref => {
                    let resolved = self.resolve_deref_gir_type_id(current_type);
                    if resolved == current_type {
                        // resolve_deref_gir_type_id couldn't resolve — try Box name parsing
                        if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                            if let Some(inner) = name.strip_prefix("Box__") {
                                return match inner {
                                    "int64_t" => LirType::I64,
                                    "int32_t" => LirType::I32,
                                    "int16_t" => LirType::I16,
                                    "int8_t" => LirType::I8,
                                    "uint8_t" => LirType::U8,
                                    "double" => LirType::F64,
                                    "float" => LirType::F32,
                                    "bool" => LirType::Bool,
                                    "Str" => LirType::Struct(
                                        self.struct_reg.lookup("Str").unwrap_or(StructId(0))
                                    ),
                                    _ => {
                                        // Named inner type — look up as struct
                                        if let Some(sid) = self.struct_reg.lookup(inner) {
                                            LirType::Struct(sid)
                                        } else {
                                            LirType::I64
                                        }
                                    }
                                };
                            }
                        }
                    }
                    current_type = resolved;
                }
                Projection::Index(_) => {
                    return LirType::I64; // default element type
                }
            }
        }

        map_gir_type_with_structs(&current_type, self.gir_types, Some(self.struct_reg))
    }
}

/// Replace `%lld` with `%.*s` at positions where the arg is a Str.
/// `is_str` is indexed from arg[1] onward (arg[0] is the format string).
fn fix_printf_str_format(fmt: &str, is_str: &[bool]) -> String {
    let mut result = String::with_capacity(fmt.len() + 8);
    let mut arg_idx = 0usize;
    let bytes = fmt.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 1 < bytes.len() {
            if bytes[i + 1] == b'%' {
                result.push_str("%%");
                i += 2;
                continue;
            }
            // Check if this is %lld
            if i + 4 <= bytes.len() && &bytes[i..i+4] == b"%lld" {
                if arg_idx < is_str.len() && is_str[arg_idx] {
                    result.push_str("%.*s");
                } else {
                    result.push_str("%lld");
                }
                arg_idx += 1;
                i += 4;
                continue;
            }
            // Other format specifiers: scan past them
            let start = i;
            i += 1;
            while i < bytes.len() && !bytes[i].is_ascii_alphabetic() && bytes[i] != b'%' {
                i += 1;
            }
            if i < bytes.len() && bytes[i].is_ascii_alphabetic() {
                i += 1;
            }
            result.push_str(&fmt[start..i]);
            arg_idx += 1;
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
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
            GirType::Ptr(_) | GirType::MutPtr(_) => LirType::Ptr,
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

fn lower_binop(dst: ValueId, op: GirBinOp, lhs: ValueId, rhs: ValueId, ty: LirType, overflow_wrap: bool) -> Inst {
    let default_overflow = if overflow_wrap { Overflow::Wrap } else { Overflow::Trap };
    match op {
        GirBinOp::Add => Inst::Add { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Sub => Inst::Sub { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Mul => Inst::Mul { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Div => Inst::Div { dst, ty, lhs, rhs },
        GirBinOp::Rem => Inst::Rem { dst, ty, lhs, rhs },
        GirBinOp::Mod => Inst::Mod { dst, ty, lhs, rhs },
        GirBinOp::Pow => {
            // Pow doesn't have a direct LIR instruction. Emit as CallExtern to pow().
            // For now, emit as Mul (placeholder).
            Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Trap }
        }
        GirBinOp::BitAnd => Inst::BitAnd { dst, ty, lhs, rhs },
        GirBinOp::BitOr => Inst::BitOr { dst, ty, lhs, rhs },
        GirBinOp::BitXor => Inst::BitXor { dst, ty, lhs, rhs },
        GirBinOp::Shl => Inst::Shl { dst, ty, lhs, rhs },
        GirBinOp::Shr => Inst::Shr { dst, ty, lhs, rhs },
        GirBinOp::AddWrap => Inst::Add { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::SubWrap => Inst::Sub { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::MulWrap => Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
    }
}

fn lower_unop(dst: ValueId, op: GirUnOp, operand: ValueId, ty: LirType) -> Inst {
    match op {
        GirUnOp::Neg => Inst::Neg { dst, ty, operand },
        GirUnOp::Not => Inst::Not { dst, operand },
        GirUnOp::BitNot => Inst::BitNot { dst, ty, operand },
    }
}

fn map_cmp_op(op: GirCmpOp) -> CmpOp {
    match op {
        GirCmpOp::Eq => CmpOp::Eq,
        GirCmpOp::Ne => CmpOp::Ne,
        GirCmpOp::Lt => CmpOp::Lt,
        GirCmpOp::Le => CmpOp::Le,
        GirCmpOp::Gt => CmpOp::Gt,
        GirCmpOp::Ge => CmpOp::Ge,
    }
}

/// Return canonical (params, return_type) for known Gorget runtime functions.
/// This prevents call-site inference from producing wrong parameter types
/// (e.g. GorgetString instead of Str for gorget_str_* functions).
fn runtime_extern_sig(name: &str, sr: &StructRegistry) -> Option<(Vec<LirType>, LirType)> {
    let str_ty = || sr.lookup("Str").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let gs_ty = || sr.lookup("GorgetString").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let arr_ty = || sr.lookup("GorgetArray").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let s = str_ty;
    let g = gs_ty;

    match name {
        // String concatenation and conversion
        "gorget_str_cat" => Some((vec![s(), s()], g())),
        "gorget_str_eq" => Some((vec![s(), s()], LirType::Bool)),
        "gorget_str_cmp" => Some((vec![s(), s()], LirType::I64)),
        "gorget_str_from_cstr" => Some((vec![LirType::Ptr], s())),
        "gorget_str_to_cstr" => Some((vec![s()], LirType::Ptr)),
        "gorget_str_empty" => Some((vec![], s())),
        "gorget_str_index" => Some((vec![s(), LirType::I64], s())),
        "gorget_str_slice" => Some((vec![s(), LirType::I64, LirType::I64], s())),
        "gorget_str_byte_slice" => Some((vec![s(), LirType::I64, LirType::I64], s())),
        "gorget_str_char_at" => Some((vec![s(), LirType::I64], s())),
        "gorget_str_codepoint_at" => Some((vec![s(), LirType::I64], s())),
        "gorget_utf8_codepoint_len_at" => Some((vec![s(), LirType::I64], LirType::I64)),
        "gorget_str_byte_at" => Some((vec![s(), LirType::I64], LirType::U8)),
        "gorget_str_byte_len" => Some((vec![s()], LirType::I64)),
        "gorget_str_codepoint_count" => Some((vec![s()], LirType::I64)),
        "gorget_str_is_empty" => Some((vec![s()], LirType::Bool)),
        "gorget_str_contains" => Some((vec![s(), s()], LirType::Bool)),
        "gorget_str_starts_with" => Some((vec![s(), s()], LirType::Bool)),
        "gorget_str_ends_with" => Some((vec![s(), s()], LirType::Bool)),
        "gorget_str_find" => Some((vec![s(), s()], LirType::I64)),
        "gorget_str_index_of" => Some((vec![s(), s()], LirType::I64)),
        "gorget_str_count" => Some((vec![s(), s()], LirType::I64)),
        "gorget_str_trim" | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws" => Some((vec![s()], s())),
        "gorget_str_strip" | "gorget_str_lstrip" | "gorget_str_rstrip" => Some((vec![s(), s()], s())),
        "gorget_str_removeprefix" | "gorget_str_removesuffix" => Some((vec![s(), s()], s())),
        "gorget_str_to_upper" | "gorget_str_to_lower" => Some((vec![s()], g())),
        "gorget_str_replace" => Some((vec![s(), s(), s()], g())),
        "gorget_str_repeat" => Some((vec![s(), LirType::I64], g())),
        "gorget_str_pad_left" | "gorget_str_pad_right" => Some((vec![s(), LirType::I64, s()], g())),
        "gorget_str_is_alpha" | "gorget_str_is_digit" | "gorget_str_is_alphanumeric"
        | "gorget_str_is_whitespace" | "gorget_str_is_upper" | "gorget_str_is_lower"
        | "gorget_str_is_hex_digit" | "gorget_str_is_ascii" | "gorget_str_has_null" => {
            Some((vec![s()], LirType::Bool))
        }
        "gorget_str_split" => Some((vec![s(), s()], arr_ty())),
        "gorget_str_join" => Some((vec![s(), arr_ty()], g())),
        "gorget_str_bytes" | "gorget_str_codepoints" | "gorget_str_chars" => {
            Some((vec![s()], arr_ty()))
        }
        // GorgetString methods
        "gorget_string_new" => Some((vec![LirType::Ptr], g())),
        "gorget_string_from_str" => Some((vec![s()], g())),
        "gorget_string_clone" => Some((vec![LirType::Ptr], g())),
        "gorget_string_free" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_string_eq" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Bool)),
        "gorget_string_cstr" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_string_concat" => Some((vec![LirType::Ptr, LirType::Ptr], g())),
        "gorget_string_append" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_str_str" => Some((vec![s(), s()], s())),
        // (gorget_str_slice handled above, from Str__substring → gorget_str_slice mapping)
        "gorget_str_from_literal" => Some((vec![LirType::Ptr, LirType::I64], s())),
        "gorget_str_from_int" | "gorget_str_from_float" | "gorget_str_from_bool" => {
            Some((vec![LirType::I64], s()))
        }

        // Collection methods
        "gorget_array_new" => Some((vec![LirType::I64], arr_ty())),
        "gorget_array_with_capacity" => Some((vec![LirType::I64, LirType::I64], arr_ty())),
        "gorget_array_push" | "gorget_array_set" | "gorget_array_insert" => {
            Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void))
        }
        "gorget_array_get" | "gorget_array_pop" | "gorget_array_first" | "gorget_array_last" => {
            Some((vec![LirType::Ptr, LirType::I64], LirType::Ptr))
        }
        "gorget_array_safe_pop" => {
            Some((vec![LirType::Ptr], LirType::Ptr))
        }
        "gorget_array_remove" => Some((vec![LirType::Ptr, LirType::I64], LirType::Void)),
        "gorget_array_remove_opt" => Some((vec![LirType::Ptr, LirType::I64], LirType::Ptr)),
        "gorget_array_len" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_array_contains" => Some((vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Bool)),
        "gorget_array_is_empty" => Some((vec![LirType::Ptr], LirType::Bool)),
        "gorget_array_index_of" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::I64)),
        "gorget_array_binary_search" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::I64)),
        "gorget_array_clear" | "gorget_array_free" | "gorget_array_reverse"
        | "gorget_array_dedup" | "gorget_array_extend" | "gorget_array_reserve" => {
            Some((vec![LirType::Ptr], LirType::Void))
        }
        "gorget_array_clone" | "gorget_array_slice" => Some((vec![LirType::Ptr], arr_ty())),
        // Map methods (unordered)
        "gorget_map_new" => Some((vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))))),
        "gorget_map_new_str" => Some((vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))))),
        // Dict methods (ordered — only new differs; put/get/etc. use gorget_map_*)
        "gorget_dict_new" => Some((vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))))),
        "gorget_dict_new_str" => Some((vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))))),
        "gorget_map_put" => Some((vec![LirType::Ptr, LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_map_get" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Ptr)),
        "gorget_map_remove" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Bool)),
        "gorget_map_contains" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Bool)),
        "gorget_map_len" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_map_is_empty" => Some((vec![LirType::Ptr], LirType::Bool)),
        "gorget_map_clear" | "gorget_map_free" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_map_clone" => Some((vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))))),
        "gorget_map_keys" | "gorget_map_values" | "gorget_map_items" => Some((vec![LirType::Ptr], arr_ty())),
        // Set methods
        "gorget_set_new" | "gorget_ordered_set_new" => Some((vec![LirType::I64], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))))),
        "gorget_set_new_str" | "gorget_ordered_set_new_str" => Some((vec![], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))))),
        "gorget_set_add" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_set_contains" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Bool)),
        "gorget_set_remove" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Bool)),
        "gorget_set_len" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_set_is_empty" => Some((vec![LirType::Ptr], LirType::Bool)),
        "gorget_set_clear" | "gorget_set_free" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_set_clone" => Some((vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))))),
        "gorget_set_to_array" => Some((vec![LirType::Ptr], arr_ty())),
        // Heap methods
        "gorget_heap_new" => Some((vec![LirType::I64], LirType::Ptr)),
        "gorget_heap_push" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_heap_pop" | "gorget_heap_peek" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_heap_len" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_heap_free" => Some((vec![LirType::Ptr], LirType::Void)),

        // Mutex / Guard methods
        "gorget_mutex_new" => Some((vec![LirType::I64, LirType::Ptr], LirType::Ptr)),
        "gorget_mutex_lock" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_mutex_lock_to" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_mutex_free" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_guard_release" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_guard_get" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_guard_set" => Some((vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void)),
        "gorget_guard_get_ptr" => Some((vec![LirType::Ptr], LirType::Ptr)),

        // Shared methods
        "gorget_shared_new" => Some((vec![LirType::I64, LirType::Ptr], LirType::Ptr)),
        "gorget_shared_clone" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_shared_drop" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_shared_get" | "gorget_shared_get_ptr" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_shared_strong_count" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_shared_downgrade" => Some((vec![LirType::Ptr], LirType::Ptr)),

        // Weak methods
        "gorget_weak_clone" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_weak_drop" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_weak_upgrade" => Some((vec![LirType::Ptr], LirType::I64)),

        // Channel methods
        "gorget_channel_new" => Some((vec![LirType::I64, LirType::I64], LirType::Ptr)),
        "gorget_channel_send" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_channel_recv" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_channel_close" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_channel_len" | "gorget_channel_capacity" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_channel_is_closed" => Some((vec![LirType::Ptr], LirType::Bool)),
        "gorget_channel_retain" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_channel_release" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_channel_free" => Some((vec![LirType::Ptr], LirType::Void)),

        // RWLock / ReadGuard / WriteGuard methods
        "gorget_rwlock_new" => Some((vec![LirType::I64, LirType::Ptr], LirType::Ptr)),
        "gorget_rwlock_read" | "gorget_rwlock_write" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_rwlock_read_to" | "gorget_rwlock_write_to" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_rwlock_free" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_read_guard_get" | "gorget_read_guard_get_ptr" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_read_guard_release" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_write_guard_get" | "gorget_write_guard_get_ptr" => Some((vec![LirType::Ptr], LirType::Ptr)),
        "gorget_write_guard_set" => Some((vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void)),
        "gorget_write_guard_release" => Some((vec![LirType::Ptr], LirType::Void)),

        // Allocator push/pop stubs
        "__gorget_push_allocator" => Some((vec![LirType::Ptr], LirType::Void)),
        "__gorget_pop_allocator" => Some((vec![], LirType::Void)),

        // chr/ord
        "gorget_char_chr" => Some((vec![LirType::I64], s())),
        "gorget_str_ord" => Some((vec![s()], LirType::I64)),
        // Conversion helpers
        "gorget_int_to_str" => Some((vec![LirType::I64], LirType::Ptr)),
        "gorget_float_to_str" => Some((vec![LirType::F64], LirType::Ptr)),
        "gorget_bool_to_str" => {
            Some((vec![LirType::Bool], LirType::Ptr))
        }
        "gorget_codepoint_to_utf8" => Some((vec![LirType::I64], LirType::Ptr)),
        "gorget_int_to_float" => Some((vec![LirType::I64], LirType::F64)),
        // I/O
        "gorget_read_file" => Some((vec![LirType::Ptr], g())),
        "gorget_write_file" | "gorget_append_file" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        "gorget_file_exists" | "gorget_is_dir" => Some((vec![LirType::Ptr], LirType::Bool)),
        // Math (integer)
        "gorget_abs" => Some((vec![LirType::I64], LirType::I64)),
        "gorget_min" | "gorget_max" => Some((vec![LirType::I64, LirType::I64], LirType::I64)),
        // Math (float)
        "gorget_fabs" => Some((vec![LirType::F64], LirType::F64)),
        "gorget_fmin" | "gorget_fmax" => Some((vec![LirType::F64, LirType::F64], LirType::F64)),
        "gorget_sqrt" | "gorget_floor" | "gorget_ceil" | "gorget_round"
        | "gorget_log" | "gorget_log2" | "gorget_log10"
        | "gorget_sin" | "gorget_cos" | "gorget_tan"
        | "gorget_asin" | "gorget_acos" | "gorget_atan" => {
            Some((vec![LirType::F64], LirType::F64))
        }
        "gorget_pow" | "gorget_atan2" => Some((vec![LirType::F64, LirType::F64], LirType::F64)),
        // Random
        "gorget_rand" => Some((vec![], LirType::I64)),
        "gorget_rand_range" => Some((vec![LirType::I64, LirType::I64], LirType::I64)),
        "gorget_seed" => Some((vec![LirType::I64], LirType::Void)),
        // Time
        "gorget_time" | "gorget_time_ms" => Some((vec![], LirType::I64)),
        "gorget_sleep_ms" | "gorget_reactor_sleep_ms" => Some((vec![LirType::I64], LirType::Void)),
        "gorget_format_time" => Some((vec![LirType::I64, LirType::Ptr], LirType::Ptr)),
        "gorget_parse_time" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::I64)),

        // Barrier
        "gorget_barrier_new" => Some((vec![LirType::I64], LirType::Ptr)),
        "gorget_barrier_wait" | "gorget_barrier_free" => Some((vec![LirType::Ptr], LirType::Void)),
        // CondVar
        "gorget_condvar_new" => Some((vec![], LirType::Ptr)),
        "gorget_condvar_notify_one" | "gorget_condvar_notify_all" | "gorget_condvar_free" => {
            Some((vec![LirType::Ptr], LirType::Void))
        }
        "gorget_condvar_wait_guard" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Void)),
        // AtomicInt
        "gorget_atomic_int_new" => Some((vec![LirType::I64], LirType::Ptr)),
        "gorget_atomic_int_load" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_atomic_int_store" => Some((vec![LirType::Ptr, LirType::I64], LirType::Void)),
        "gorget_atomic_int_add" | "gorget_atomic_int_sub" => Some((vec![LirType::Ptr, LirType::I64], LirType::I64)),
        "gorget_atomic_int_compare_exchange" => Some((vec![LirType::Ptr, LirType::I64, LirType::I64], LirType::Bool)),
        "gorget_atomic_int_free" => Some((vec![LirType::Ptr], LirType::Void)),
        // AtomicBool
        "gorget_atomic_bool_new" => Some((vec![LirType::Bool], LirType::Ptr)),
        "gorget_atomic_bool_load" => Some((vec![LirType::Ptr], LirType::Bool)),
        "gorget_atomic_bool_store" => Some((vec![LirType::Ptr, LirType::Bool], LirType::Void)),
        "gorget_atomic_bool_swap" => Some((vec![LirType::Ptr, LirType::Bool], LirType::Bool)),
        "gorget_atomic_bool_compare_exchange" => Some((vec![LirType::Ptr, LirType::Bool, LirType::Bool], LirType::Bool)),
        "gorget_atomic_bool_free" => Some((vec![LirType::Ptr], LirType::Void)),
        // Process
        "gorget_process_spawn" => Some((vec![LirType::Ptr, LirType::Ptr], LirType::Ptr)),
        "gorget_process_wait" | "gorget_process_pid" => Some((vec![LirType::Ptr], LirType::I64)),
        "gorget_process_kill" | "gorget_process_close_stdin" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_process_write_stdin" => Some((vec![LirType::Ptr, s()], LirType::Void)),
        "gorget_process_read_stdout" | "gorget_process_read_stderr" => Some((vec![LirType::Ptr], g())),

        // Panic / abort functions (void return)
        "gorget_panic" => Some((vec![LirType::Ptr], LirType::Void)),
        "gorget_assert_fail" => Some((vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void)),
        "gorget_overflow_add" | "gorget_overflow_sub" | "gorget_overflow_mul" => {
            Some((vec![], LirType::Void))
        }
        _ => None,
    }
}

/// Map monomorphized GIR function names to their C runtime equivalents.
/// E.g., `Vector__Str__push` → `gorget_array_push`,
///       `Dict__Str__int64_t__put` → `gorget_map_put`,
///       `GorgetString__to_upper` → `gorget_str_to_upper`.
/// Returns true if `s` is a known C type name (indicating the "method" part of a
/// monomorphized name is actually a type parameter, not a method name).
fn is_type_name(s: &str) -> bool {
    matches!(s, "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "Str" | "GorgetString"
        | "GorgetArray" | "GorgetMap" | "GorgetSet" | "void"
        | "T" | "U" | "V")
}

/// Returns true if the GIR function name refers to a collection or concurrency
/// method whose first argument (self) should be passed by pointer (GlobalAddr)
/// rather than by value (GlobalAddr+Load). These are mutating methods on
/// Vector, Dict, Set, HashMap, HashSet, Heap, Mutex, RWLock, etc.
fn is_self_by_ptr_method(name: &str) -> bool {
    // Collections and guards store their data inline (as struct values), so passing
    // by pointer (GlobalAddr without Load) gives a pointer to the struct — correct
    // for mutating methods.
    //
    // Mutex and RWLock are already POINTER types (GorgetMutex*, GorgetRWLock*),
    // so the global holds a pointer value. Passing by value (GlobalAddr+Load) gives
    // the pointer itself, which is what the runtime functions expect. Do NOT include
    // Mutex__ or RWLock__ here — they should be passed by value.
    //
    // Guard/ReadGuard/WriteGuard ARE structs (gorget_guard_t etc.), so they need
    // by-pointer passing for their mutating methods (get, set, drop/release).
    name.starts_with("Vector__")
        || name.starts_with("GorgetArray__")
        || name.starts_with("Dict__")
        || name.starts_with("HashMap__")
        || name.starts_with("GorgetMap__")
        || name.starts_with("Set__")
        || name.starts_with("HashSet__")
        || name.starts_with("GorgetSet__")
        || name.starts_with("Heap__")
        || name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
        || name.starts_with("GorgetString__")
        || name.starts_with("Str__")
        || name.starts_with("Deque__")
}

fn map_monomorphized_to_runtime(name: &str) -> Option<String> {
    // Vector__T__method → gorget_array_method
    // GorgetArray__method → gorget_array_method  (non-generic array calls)
    // Higher-order methods (filter, map, fold, any, all, each, reduce, flat_map, find, find_index)
    // are NOT runtime functions — they are generated inline by the c_lir backend.
    // Keep them as their original monomorphized names so the backend can detect and generate them.
    if name.starts_with("Vector__") || name.starts_with("GorgetArray__") {
        let method = name.rsplit("__").next()?;
        // Guard: if the "method" is actually a type name (int64_t, double, etc.),
        // this is a constructor call like Vector__int64_t(cap), not a method call.
        // Keep the original name — the c_lir backend handles these constructors specially.
        if is_type_name(method) {
            return None;
        }
        match method {
            "filter" | "map" | "flat_map" | "fold" | "reduce" | "any" | "all"
            | "each" | "find" | "find_index" | "sorted" | "sort" | "unique" | "count" => return None,
            // Vector.get() returns Option[T] — use safe (non-panicking) get.
            "get" => return Some("gorget_array_safe_get".into()),
            // Vector.pop() returns Option[T] — use safe (non-panicking) pop.
            "pop" => return Some("gorget_array_safe_pop".into()),
            _ => return Some(format!("gorget_array_{method}")),
        }
    }
    // Dict__K__V__method → gorget_dict_new for "new", gorget_map_* for all others
    // HashMap__K__V__method / GorgetMap__method → gorget_map_method (unordered)
    // Higher-order methods (filter, fold, each, any, all, map) and non-runtime methods
    // (update, get_or, get_or_put) keep their monomorphized names for inline codegen.
    if name.starts_with("Dict__") || name.starts_with("HashMap__") || name.starts_with("GorgetMap__") {
        let method = name.rsplit("__").next()?;
        match method {
            "filter" | "fold" | "each" | "any" | "all" | "map"
            | "update" | "get_or" | "get_or_put" => return None,
            // Dict.new() needs gorget_dict_new (ordered); all other methods use gorget_map_*
            "new" if name.starts_with("Dict__") => return Some("gorget_dict_new".into()),
            "has" => return Some("gorget_map_contains".into()),
            "set" => return Some("gorget_map_put".into()),
            _ => return Some(format!("gorget_map_{method}")),
        }
    }
    // Set__T__method → gorget_set_method
    // GorgetSet__method → gorget_set_method
    // Higher-order methods and non-runtime set operations keep monomorphized names.
    if name.starts_with("Set__") || name.starts_with("HashSet__") || name.starts_with("GorgetSet__") {
        let method = name.rsplit("__").next()?;
        match method {
            "filter" | "fold" | "each" | "any" | "all" | "map"
            | "is_subset" | "is_superset"
            | "union" | "intersection" | "difference" | "symmetric_difference" => return None,
            "has" => return Some("gorget_set_contains".into()),
            // Set.new() needs gorget_ordered_set_new (ordered); HashSet uses unordered.
            "new" if name.starts_with("Set__") => return Some("gorget_ordered_set_new".into()),
            "new_str" if name.starts_with("Set__") => return Some("gorget_ordered_set_new_str".into()),
            _ => return Some(format!("gorget_set_{method}")),
        }
    }
    // GorgetString__method → gorget_str_method (for string methods)
    if name.starts_with("GorgetString__") {
        let method = name.strip_prefix("GorgetString__")?;
        return Some(format!("gorget_str_{method}"));
    }
    // Str__method → gorget_str_method (with fixups for name mismatches)
    if name.starts_with("Str__") {
        let method = name.strip_prefix("Str__")?;
        let mapped = format!("gorget_str_{method}");
        // Fixup: these GIR method names don't match runtime function names.
        return Some(match mapped.as_str() {
            "gorget_str_substring" => "gorget_str_slice".into(),
            _ => mapped,
        });
    }
    // Option/Result helpers are handled inline by the c_lir backend — don't map them.
    // Heap__T__method → gorget_heap_method
    if name.starts_with("Heap__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_heap_{method}"));
    }
    // Mutex__T__method → gorget_mutex_method  (new/lock/free)
    // Guard__T__method → gorget_guard_method  (get/set/drop/get_ptr/release)
    if name.starts_with("Mutex__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_mutex_{method}"));
    }
    if name.starts_with("Guard__") {
        let method = name.rsplit("__").next()?;
        // Guard__T__drop → gorget_guard_release (RAII drop = release the mutex)
        if method == "drop" {
            return Some("gorget_guard_release".into());
        }
        return Some(format!("gorget_guard_{method}"));
    }
    // Shared__T and Weak__T methods are NOT mapped — they have different calling
    // conventions (monomorphized wrappers pass/return typed values, runtime uses void*).
    // Inline wrappers are emitted by the c_lir backend.
    if name.starts_with("Shared__") || name.starts_with("Weak__") {
        return None;
    }
    // Channel__T methods are NOT mapped — they have different calling conventions
    // (monomorphized wrappers pass values, runtime uses void*). Inline wrappers
    // are emitted by the c_lir backend.
    if name.starts_with("Channel__") {
        return None;
    }
    // RWLock__T__method → gorget_rwlock_method  (new/read/write/free)
    if name.starts_with("RWLock__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_rwlock_{method}"));
    }
    // ReadGuard__T__method → gorget_read_guard_method  (get/get_ptr/drop)
    if name.starts_with("ReadGuard__") {
        let method = name.rsplit("__").next()?;
        if method == "drop" {
            return Some("gorget_read_guard_release".into());
        }
        return Some(format!("gorget_read_guard_{method}"));
    }
    // WriteGuard__T__method → gorget_write_guard_method  (get/set/get_ptr/drop)
    if name.starts_with("WriteGuard__") {
        let method = name.rsplit("__").next()?;
        if method == "drop" {
            return Some("gorget_write_guard_release".into());
        }
        return Some(format!("gorget_write_guard_{method}"));
    }
    // Bare stdlib helpers → gorget_ prefixed runtime functions.
    // Delegates to the shared map_stdlib_name() in crate::backend.
    if let Some(mapped) = crate::backend::map_stdlib_name(name) {
        return Some(mapped.to_string());
    }
    // SDL wildcard fallback for any sdl_ function not explicitly listed.
    if name.starts_with("sdl_") {
        return Some(format!("gorget_{name}"));
    }
    None
}

/// Extract the element type name from a monomorphized collection method name.
/// E.g., `Vector__Container__set` → "Container".
/// E.g., `Vector__Vector__Container__set` → "Vector__Container".
fn collection_elem_type_from_name(original_name: &str) -> Option<&str> {
    let rest = original_name.strip_prefix("Vector__")
        .or_else(|| original_name.strip_prefix("Set__"))
        .or_else(|| original_name.strip_prefix("HashSet__"))
        .or_else(|| original_name.strip_prefix("Heap__"))?;

    // Strip method suffix: find rightmost `__` where suffix is all-lowercase
    if let Some(pos) = rest.rfind("__") {
        let suffix = &rest[pos + 2..];
        if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_lowercase() || c == '_') {
            let elem = &rest[..pos];
            if !elem.is_empty() {
                return Some(elem);
            }
        }
    }
    // No method suffix found — return whole rest
    if !rest.is_empty() { Some(rest) } else { None }
}

/// Check if a GIR type needs dropping.
fn type_needs_drop(
    type_name: &str,
    registry: &TypeRegistry,
    func_index: &std::collections::HashMap<String, FuncId>,
) -> bool {
    use crate::ir::types::DropStrategy;
    if let Some(type_def) = registry.get_type_def(type_name) {
        match &type_def.metadata.drop_strategy {
            DropStrategy::None => false,
            DropStrategy::Trivial(_) | DropStrategy::Custom(_) => true,
            DropStrategy::Recursive => {
                let name = format!("{type_name}__drop");
                func_index.contains_key(name.as_str())
                    || matches!(type_def.kind, crate::ir::types::TypeDefKind::Struct(_))
            }
        }
    } else {
        false
    }
}

/// Extract the element sizeof from a monomorphized collection constructor name.
/// E.g., `Vector__int64_t__new` → sizeof(int64_t) = 8.
/// Returns the size in bytes, or None if the name doesn't match.
fn elem_size_from_monomorphized(name: &str, structs: &[StructDef]) -> Option<usize> {
    // Extract the type portion between the collection prefix and the method name.
    let type_str = if let Some(rest) = name.strip_prefix("Vector__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Set__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("HashSet__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Heap__") {
        rest.strip_suffix("__new")?
    } else {
        // Dict/HashMap constructors are handled by dict_elem_sizes_from_monomorphized.
        return None;
    };
    Some(c_sizeof_with_structs(type_str, structs))
}

/// Extract the inner-type sizeof from a monomorphized concurrency constructor or
/// guard set call. Works for Mutex__T__new, Shared__T__new, RWLock__T__new,
/// Channel__T__new, Guard__T__set, WriteGuard__T__set.
fn concurrency_elem_size(name: &str, structs: &[StructDef]) -> Option<usize> {
    // Try each prefix; the type sits between the prefix and the __method suffix.
    for prefix in &["Mutex__", "Shared__", "RWLock__", "Channel__", "Guard__", "WriteGuard__"] {
        if let Some(rest) = name.strip_prefix(prefix) {
            // Find the last `__method` segment.
            if let Some(idx) = rest.rfind("__") {
                let type_str = &rest[..idx];
                return Some(c_sizeof_with_structs(type_str, structs));
            }
        }
    }
    None
}

/// Extract key and value sizeof from a monomorphized Dict constructor name.
/// E.g., `Dict__Str__int64_t__new` → (sizeof(Str), sizeof(int64_t)) = (16, 8).
fn dict_elem_sizes_from_monomorphized(name: &str, structs: &[StructDef]) -> (usize, usize) {
    // Dict__K__V__new or HashMap__K__V__new
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"));
    if let Some(types) = rest {
        // Split on `__` to find key and value type names.
        // For simple types: "Str__int64_t" → key=Str, val=int64_t
        // For complex types: "int64_t__Str" → key=int64_t, val=Str
        // Heuristic: try splitting at each `__` boundary and pick the first valid split.
        if let Some(idx) = types.find("__") {
            let key = &types[..idx];
            let val = &types[idx + 2..];
            return (c_sizeof_with_structs(key, structs), c_sizeof_with_structs(val, structs));
        }
    }
    (8, 8) // fallback
}

/// Extract the key type name from a monomorphized Dict/HashMap name.
/// E.g., `Dict__Str__int64_t__new` → Some("Str").
fn dict_key_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    let idx = rest.find("__")?;
    Some(rest[..idx].to_string())
}

/// Extract the element type from a monomorphized Set/HashSet name.
/// E.g., `Set__Str__new` → Some("Str").
fn set_elem_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Set__")
        .or_else(|| name.strip_prefix("HashSet__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    Some(rest.to_string())
}

/// Return the sizeof of an LIR type in bytes (best-effort for 64-bit targets).
fn lir_type_sizeof(ty: &LirType) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 | LirType::F32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 => 8,
        LirType::Ptr => 8,
        LirType::Struct(_) => 8, // conservative; struct sizeof varies
        LirType::Void => 0,
    }
}

/// Map a GIR C type name to its sizeof in bytes.
/// `structs` is used to compute sizes of user-defined struct types.
fn c_sizeof_with_structs(type_name: &str, structs: &[StructDef]) -> usize {
    match type_name {
        "int64_t" | "uint64_t" | "double" => 8,
        "int32_t" | "uint32_t" | "float" => 4,
        "int16_t" | "uint16_t" => 2,
        "int8_t" | "uint8_t" | "bool" => 1,
        // Str is a (data_ptr, len) pair — 16 bytes on 64-bit
        "Str" => 16,
        _ => {
            // Runtime collection structs: GorgetArray = {data, len, cap, elem_size, alloc} = 40 bytes
            if type_name.starts_with("Vector__") || type_name == "GorgetArray" {
                return 40;
            }
            // GorgetMap = 13 fields × 8 = 104 bytes (keys, values, states, count, cap, key_size, val_size, alloc, order, order_len, tombstones, hash_fn, eq_fn)
            if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") || type_name == "GorgetMap" {
                return 104;
            }
            // GorgetSet aliases GorgetMap (same struct)
            if type_name.starts_with("Set__") || type_name.starts_with("HashSet__") || type_name == "GorgetSet" {
                return 104;
            }
            // GorgetString = {data, len, cap, alloc} = 32 bytes
            if type_name == "GorgetString" || type_name == "String" {
                return 32;
            }
            // GorgetClosure / Callable = {fn_ptr, env} = 16 bytes
            if type_name == "GorgetClosure" || type_name.starts_with("Callable__") || type_name.starts_with("Callable_") {
                return 16;
            }
            // Task__T = { void* __task; void (*__drop)(void*); } = 16 bytes
            if type_name.starts_with("Task__") {
                return 16;
            }
            // Tuple__T1__T2__... — sum of field sizes with 8-byte alignment per field
            if let Some(rest) = type_name.strip_prefix("Tuple__") {
                return c_sizeof_tuple_fields(rest, structs);
            }
            // Option__T — tag(4) + padding(4) + payload
            if let Some(inner) = type_name.strip_prefix("Option__") {
                let payload = c_sizeof_with_structs(inner, structs);
                // struct { int32_t tag; <pad to 8>; T payload; }
                return 8 + std::cmp::max(payload, 8);
            }
            // User-defined struct — look up in LIR struct definitions.
            if let Some(sd) = structs.iter().find(|s| s.name == type_name) {
                return c_sizeof_struct_from_fields(&sd.fields, structs);
            }
            // Pointer/opaque types default to 8
            8
        }
    }
}

/// Compute the size of a struct from its LIR field definitions.
fn c_sizeof_struct_from_fields(fields: &[(String, LirType)], structs: &[StructDef]) -> usize {
    let mut total = 0usize;
    for (_name, ty) in fields {
        let field_sz = c_sizeof_lir_type(ty, structs);
        let align = std::cmp::min(field_sz, 8);
        if align > 0 {
            total = (total + align - 1) / align * align;
        }
        total += field_sz;
    }
    // Align total to 8 bytes.
    let align = 8;
    total = (total + align - 1) / align * align;
    total
}

/// Compute sizeof for an LirType.
fn c_sizeof_lir_type(ty: &LirType, structs: &[StructDef]) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr => 8,
        LirType::F32 => 4,
        LirType::Struct(sid) => {
            if let Some(sd) = structs.get(sid.0 as usize) {
                // For runtime structs whose LIR field list omits hidden C fields
                // (e.g. the `alloc` pointer in GorgetArray/GorgetMap), use the
                // authoritative hardcoded size rather than counting LIR fields.
                let runtime_size = match sd.name.as_str() {
                    // GorgetArray: {data, len, cap, elem_size, alloc} = 5 × 8 = 40
                    "GorgetArray" => Some(40usize),
                    // GorgetMap / GorgetSet: 13 fields × 8 = 104
                    "GorgetMap" | "GorgetSet" => Some(104usize),
                    // GorgetString: {data, len, cap, alloc} = 4 × 8 = 32
                    "GorgetString" => Some(32usize),
                    // Str: {data, len} = 2 × 8 = 16
                    "Str" => Some(16usize),
                    _ if sd.name.starts_with("Task__") => Some(16usize),
                    _ => None,
                };
                if let Some(sz) = runtime_size {
                    return sz;
                }
                c_sizeof_struct_from_fields(&sd.fields, structs)
            } else {
                8
            }
        }
        LirType::Void => 0,
    }
}

/// Compute the size of a tuple from its mangled field types.
/// `Tuple__int64_t__Str` → fields are [int64_t, Str] → 8 + 16 = 24.
/// Fields are split on `__` but multi-word types like `int64_t` contain `_`
/// (not `__`), so we split on `__` and rejoin single-underscore segments.
fn c_sizeof_tuple_fields(fields_str: &str, structs: &[StructDef]) -> usize {
    let mut total = 0usize;
    // Split on __ delimiter.  Type names use single _ (int64_t, uint8_t).
    for part in fields_str.split("__") {
        if part.is_empty() { continue; }
        let field_sz = c_sizeof_with_structs(part, structs);
        // Align each field to its natural alignment (max 8).
        let align = std::cmp::min(field_sz, 8);
        if align > 0 {
            total = (total + align - 1) / align * align;
        }
        total += field_sz;
    }
    // Align total to 8 bytes (struct padding).
    let align = 8;
    total = (total + align - 1) / align * align;
    total
}

fn lower_global_init(init: &ir::GlobalInit, func_index: &std::collections::HashMap<String, FuncId>) -> LirGlobalInit {
    match init {
        ir::GlobalInit::Zeroed => LirGlobalInit::Zeroed,
        ir::GlobalInit::Bytes(b) => LirGlobalInit::Bytes(b.clone()),
        ir::GlobalInit::FnRef(name) => {
            if let Some(fid) = func_index.get(name) {
                LirGlobalInit::FuncAddr(*fid)
            } else {
                LirGlobalInit::Zeroed
            }
        }
        ir::GlobalInit::Struct { fields, .. } => {
            LirGlobalInit::Struct {
                struct_id: StructId(0), // placeholder
                fields: fields.iter().map(|(_, f)| lower_global_init(f, func_index)).collect(),
            }
        }
        ir::GlobalInit::RuntimeCall(expr) => {
            // Try to parse the expression as a numeric constant.
            if let Ok(v) = expr.parse::<i64>() {
                LirGlobalInit::Bytes(v.to_le_bytes().to_vec())
            } else if let Ok(v) = expr.parse::<f64>() {
                LirGlobalInit::Bytes(v.to_le_bytes().to_vec())
            } else {
                // Complex runtime call — remap function names to runtime equivalents.
                let mut remapped = expr.clone();
                if let Some(paren_pos) = remapped.find('(') {
                    let func_name = &remapped[..paren_pos];
                    if let Some(mapped) = map_monomorphized_to_runtime(func_name) {
                        // Concurrency constructors need sizeof + address-of injected:
                        // Mutex__T__new(val) → gorget_mutex_new(sizeof(T), &(T){val})
                        if matches!(mapped.as_str(), "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new") {
                            let args_str = &remapped[paren_pos + 1..remapped.len() - 1]; // strip parens
                            let elem_size = concurrency_elem_size(func_name, &[]).unwrap_or(8);
                            // Use a compound literal with the element type for proper alignment
                            let elem_type = func_name
                                .strip_prefix("Mutex__").or_else(|| func_name.strip_prefix("Shared__"))
                                .or_else(|| func_name.strip_prefix("RWLock__"))
                                .and_then(|r| r.rsplit_once("__").map(|(t, _)| t))
                                .unwrap_or("int64_t");
                            remapped = format!("{mapped}(sizeof({elem_type}), &({elem_type}){{{args_str}}})");
                        } else {
                            remapped = format!("{mapped}{}", &remapped[paren_pos..]);
                        }
                    }
                }
                LirGlobalInit::RuntimeCall(remapped)
            }
        }
    }
}

/// Top-level entry point: lower a GIR module to LIR.
pub fn lower_module(gir: &ir::Module) -> LirModule {
    let ctx = LoweringContext::new(gir);
    ctx.lower()
}

/// Convert a GIR TypeId to its C type name (for spawn metadata).
fn gir_type_to_c(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    use gir_types::*;
    if type_id == BOOL_TYPE { return "bool".into(); }
    if type_id == I8_TYPE { return "int8_t".into(); }
    if type_id == I16_TYPE { return "int16_t".into(); }
    if type_id == I32_TYPE { return "int32_t".into(); }
    if type_id == I64_TYPE { return "int64_t".into(); }
    if type_id == U8_TYPE { return "uint8_t".into(); }
    if type_id == U16_TYPE { return "uint16_t".into(); }
    if type_id == U32_TYPE { return "uint32_t".into(); }
    if type_id == U64_TYPE { return "uint64_t".into(); }
    if type_id == F32_TYPE { return "float".into(); }
    if type_id == F64_TYPE { return "double".into(); }
    if type_id == UNIT_TYPE { return "void".into(); }
    if let Some(gir_type) = registry.get(type_id) {
        match gir_type {
            GirType::Ptr(inner) if *inner == U8_TYPE => "const char*".into(),
            GirType::Ptr(inner) => format!("const {}*", gir_type_to_c(*inner, registry)),
            GirType::MutPtr(inner) => format!("{}*", gir_type_to_c(*inner, registry)),
            GirType::Named(name) => {
                // Map collection instantiations to runtime struct names.
                if let Some(rt) = collection_runtime_type(name) {
                    rt.into()
                } else if is_opaque_pointer_type(name) {
                    // Opaque types are lowered to Ptr (void*) in LIR.
                    "void*".into()
                } else if let Some(rt) = opaque_runtime_type_name(name) {
                    rt.into()
                } else {
                    name.clone()
                }
            }
            GirType::FnPtr { .. } => "void*".into(),
            _ => format!("int64_t"), // fallback
        }
    } else {
        "int64_t".into()
    }
}

/// Convert a GIR TypeId to C type for spawn context fields.
/// Callable params (FnPtr) become void*; void becomes void*.
fn spawn_param_c_type(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    if matches!(registry.get(type_id), Some(GirType::FnPtr { .. })) {
        return "void*".into();
    }
    let c = gir_type_to_c(type_id, registry);
    if c == "void" { "void*".into() } else { c }
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
                    Instruction::Assign {
                        dst: Place::local(LocalId(1)),
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
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(1)),
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
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(2)),
                        value: Operand::Constant(Constant::I64(10)),
                    }],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                BasicBlock {
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(2)),
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
                instructions: vec![Instruction::Assign {
                    dst: Place::local(LocalId(1)),
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
