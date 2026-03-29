use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::instructions::Operand;
use crate::ir::types::*;
use crate::parser::ast::{Expr, Ownership, PrimitiveType, Type};
use crate::semantic::AnalysisResult;
use crate::span::Spanned;

use super::closures::ClosureLowering;
use super::drops::DropElaborator;
use super::types::TypeMapper;

use crate::ir::types::BlockId;

/// How a function parameter is passed at the C ABI level.
/// Single source of truth — replaces scattered re-derivation in lower_call_arg
/// and format_args_with_coercion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamABI {
    /// Passed by value (copy types, small primitives).
    ByValue,
    /// Passed by const pointer (`const T*` in C). Bare resource-type params.
    ByPtr,
    /// Passed by mutable pointer (`T*` in C). `&` (MutableBorrow) or `!` (Move) on resource types.
    ByMutPtr,
}

/// Metadata for a shared variable's hidden local and wrapper.
#[derive(Debug, Clone, Copy)]
pub struct SharedLocalInfo {
    /// The hidden local that holds the actual wrapper (Mutex/Shared/Atomic/RwLock).
    pub hidden_local: LocalId,
    /// The inner value type (T in Mutex[T], Shared[T], etc.).
    pub inner_type: TypeId,
    /// The wrapper type (e.g., Mutex__int64_t, Shared__Str).
    pub wrapper_type: TypeId,
    /// Which locking/access protocol to use.
    pub kind: SharedLocalKind,
    /// The original `shared` annotation from the AST (`Auto`, `Atomic`, etc.).
    pub ast_shared: crate::parser::ast::SharedKind,
}

/// The kind of wrapper used for a shared variable's hidden local.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedLocalKind {
    /// Mutex[T] — lock-based read/write (single lock for all access)
    Mutex,
    /// Shared[T] — ARC-only, read via `.get()` (no locking)
    SharedArc,
    /// AtomicInt or AtomicBool — lock-free atomic ops
    Atomic,
    /// RWLock[T] — reader-writer lock (concurrent reads, exclusive writes)
    RwLock,
}

/// Information about a loop for break/continue targeting.
pub struct LoopInfo {
    pub header_bb: BlockId,  // target for continue
    pub exit_bb: BlockId,    // target for break
}

/// State for generic monomorphization within a function body.
#[derive(Default)]
pub struct GenericState {
    /// Type name substitutions for generic monomorphization.
    /// Maps template type names (e.g., "Container__T") to monomorphized names
    /// (e.g., "Container__int64_t") during generic function body lowering.
    pub type_name_subs: FxHashMap<String, String>,
    /// Raw generic param → concrete mangled fragment subs (e.g., "T" → "int64_t").
    /// Used by resolve_type_name for on-the-fly substitution of names not in the
    /// pre-computed type_name_subs map.
    pub generic_fragment_subs: Vec<(String, String)>,
    /// Generic type parameter → concrete TypeId substitutions.
    /// Maps bare type parameters (e.g., "T") to their concrete TypeIds (e.g., I64_TYPE)
    /// during generic function body lowering.
    pub generic_type_params: FxHashMap<String, TypeId>,
}

/// State for spawn/concurrency tracking during lowering.
#[derive(Default)]
pub struct SpawnState {
    /// Spawn wrapper functions accumulated during lowering; emitted into the module after
    /// all regular functions. Each wrapper reconstructs the closure struct from flat args
    /// and calls the corresponding __Closure_N__call function.
    pub wrapper_fns: Vec<crate::ir::Function>,
    /// Task variable LocalId → spawned fn_name (e.g., local for `t1` → "produce").
    /// Set by Spawn lowering, consumed by Await lowering.
    pub result_locals: FxHashMap<LocalId, String>,
    /// Set during Spawn lowering; consumed by lower_var_decl to register result_locals.
    pub pending_fn: Option<String>,
    /// Accumulated set of all spawned fn names (NOT cleared between functions).
    pub fn_names: FxHashMap<String, bool>,
    /// Subset of fn_names that should run on the blocking pool instead of M:N executor.
    pub blocking_fn_names: rustc_hash::FxHashSet<String>,
    /// Accumulated set of thread-spawned fn names: fn_name → return TypeId.
    /// NOT cleared between functions. Used to emit thread spawn/join helpers.
    pub thread_fns: FxHashMap<String, TypeId>,
    /// Task TypeId → spawned fn_name. Enables await dispatch for tasks stored
    /// in collections (where result_locals doesn't have an entry because the
    /// task local has projections like vector indexing).
    pub task_type_fns: FxHashMap<TypeId, Vec<String>>,
    /// Scheduler backend for `spawn`.
    pub scheduler_mode: crate::ir::SchedulerMode,
}

impl SpawnState {
    /// Register a Task TypeId → spawned fn_name mapping for await dispatch
    /// on tasks stored in collections (where result_locals doesn't apply).
    pub fn register_task_type_fn(&mut self, task_type: TypeId, fn_name: String) {
        self.task_type_fns.entry(task_type).or_default().push(fn_name);
    }
}

/// State for `shared` variable tracking during lowering.
#[derive(Default)]
pub struct SharedVarState {
    /// Shared variable facade locals → SharedLocalInfo.
    /// The facade local has the user-visible inner type T; the hidden local holds the
    /// actual Mutex[T], Shared[T], or AtomicInt/AtomicBool. The `kind` determines which
    /// ops to emit for transparent read/write access.
    pub locals: FxHashMap<LocalId, SharedLocalInfo>,
    /// When true, shared variable reads return the raw wrapper local instead of auto-locking/getting.
    /// Set during spawn arg lowering so shared vars are passed as Mutex/Shared pointers to spawned tasks.
    pub pass_raw: bool,
    /// Function AST bodies indexed by function name. Populated during pre-scan.
    /// Used by async shared token generation to re-lower function bodies with shared params.
    pub fn_ast_bodies: FxHashMap<String, crate::parser::ast::FunctionDef>,
    /// Deferred shared-async variant requests. Recorded at spawn sites, processed after
    /// all functions are lowered (so the source GIR function is available to transform).
    pub pending_variants: Vec<crate::ir::transforms::shared_async::PendingSharedVariant>,
}

/// Tracks lowering state within a function.
pub struct LoweringContext<'a> {
    pub analysis: &'a AnalysisResult,
    pub type_mapper: TypeMapper,
    /// Owned during lowering — taken from Module, returned after.
    pub type_registry: TypeRegistry,
    /// Closure lowering state.
    pub closures: ClosureLowering,
    /// Drop elaboration state.
    pub drops: DropElaborator,
    /// name → (LocalId, GIR TypeId) for variables in the current function.
    locals: FxHashMap<String, (LocalId, TypeId)>,
    /// Function signatures: name → (param GIR TypeIds, return GIR TypeId).
    pub fn_sigs: FxHashMap<String, (Vec<TypeId>, TypeId)>,
    /// Enum variant → (enum_type_name, variant_name) mapping.
    pub enum_variants: FxHashMap<String, (String, String)>,
    /// Struct field info: (type_name, field_name) → (field_index, field_type_id).
    pub struct_fields: FxHashMap<(String, String), (u32, TypeId)>,
    /// Closure info: struct_name → (call_fn_name, struct_type_id, by-value captures with field indices).
    /// Each capture entry is (name, type_id, struct_field_index).
    pub closure_info: FxHashMap<String, (String, TypeId, Vec<(String, TypeId, u32)>)>,
    /// Stack of active loops for break/continue targeting.
    loop_stack: Vec<LoopInfo>,
    /// Generic monomorphization state.
    pub generics: GenericState,
    /// Spawn/concurrency tracking.
    pub spawn: SpawnState,
    /// Shared variable tracking.
    pub shared: SharedVarState,
    /// Module-level constants: name → Constant value (for imports like PI, E, etc.)
    pub module_constants: FxHashMap<String, crate::ir::instructions::Constant>,
    /// Whether `directive strip-asserts` is active (asserts become no-ops).
    pub strip_asserts: bool,
    /// Whether snapshot capture mode is active (`--snapshot save`).
    pub snapshot_mode: bool,
    /// Whether `directive overflow wrap` is active (integer overflow wraps).
    pub overflow_wrap: bool,
    /// Whether `directive explicit-clone` is active (auto-clone is an error, not a warning).
    pub explicit_clone: bool,
    /// LocalIds that are mutable capture pointers (need deref on read/write in closure bodies).
    /// Tracks `&` (MutableBorrow) and `!` (Move) struct params, which are MutPtr in GIR.
    pub mut_capture_locals: FxHashMap<LocalId, TypeId>,
    /// LocalIds that hold borrowed Ptr references — bare-borrow resource params
    /// and collection borrowing reads. These are NOT auto-dereferenced on access;
    /// they stay as Ptr(T) throughout the callee body. The LIR backend uses
    /// SlotLoad instead of SlotAddr for these locals. Not registered for drop.
    pub ref_locals: FxHashSet<LocalId>,
    /// LocalIds that are named variables (vs anonymous temps from expressions).
    /// Used to distinguish variable-to-variable assignment (needs clone) from
    /// temp-to-variable (needs move-zero).
    pub named_locals: FxHashSet<LocalId>,
    /// Extern binding: Gorget name → C symbol name (e.g., "llabs_wrapper" → "llabs").
    pub extern_bindings: FxHashMap<String, String>,
    /// Default parameter values: fn_name → Vec<(param_index, default_expr)>.
    pub fn_defaults: FxHashMap<String, Vec<(usize, crate::parser::ast::Expr)>>,
    /// Function parameter names: fn_name → Vec<param_name> (in declaration order).
    pub fn_param_names: FxHashMap<String, Vec<String>>,
    /// Function parameter ownerships: fn_name → Vec<Ownership> (in declaration order).
    /// Used by token wrapper generation to determine lock type per shared arg.
    pub fn_param_ownerships: FxHashMap<String, Vec<crate::parser::ast::Ownership>>,
    /// Unified parameter ABI: fn_name → Vec<ParamABI> (in declaration order).
    /// Single source of truth for how each parameter is passed at the C ABI level.
    pub fn_param_abis: FxHashMap<String, Vec<ParamABI>>,
    /// If current function uses `throws`, the Result TypeId for wrapping return/throw.
    pub current_throws_result_type: Option<TypeId>,
    /// Target type hint for the current expression being lowered.
    /// Set by VarDecl/Assign handlers so enum variant constructors (Some, None, Ok, Error)
    /// can pick the correctly-monomorphized type.
    pub expected_type: Option<TypeId>,
    /// Closure parameter type hints for higher-order collection methods.
    /// Set before lowering closure arguments to filter/map/fold/etc. so that
    /// untyped closure params get the correct element type instead of I64_TYPE.
    pub closure_param_type_hints: Vec<TypeId>,
    /// Callable parameter return types: LocalId → return TypeId.
    /// Populated during function setup for parameters with Callable/function types.
    pub callable_return_types: FxHashMap<LocalId, TypeId>,
    /// Module-level global variable names (from StaticDecl items).
    /// Used by Expr::Identifier lowering to emit Constant::GlobalRef instead of I64(0).
    pub global_names: rustc_hash::FxHashSet<String>,
    /// Module-level global variable type names: var_name → AST type name (e.g. "AtomicInt").
    /// Used by infer_type_name_from_operand_full to dispatch methods on globals.
    pub global_type_names: FxHashMap<String, String>,
    /// Set of equip method names that are GIR-lowered (not extern/C-runtime).
    /// Used by lower_method_call to decide whether to pass resource-type args by pointer.
    pub gir_equip_methods: rustc_hash::FxHashSet<String>,
    /// Active `with shared_var:` auto-refresh bindings.
    /// Maps the with-binding local → the shared facade local it mirrors.
    /// After each await, the shared var is re-read into the binding local.
    pub with_shared_refresh: Vec<(LocalId, LocalId)>,
    /// Accumulated `on error:` cleanup blocks. Emitted in LIFO order on error paths.
    pub on_error_blocks: Vec<crate::parser::ast::Block>,
    /// Accumulated `assert return` postcondition expressions.
    /// Checked at every `return` site before the value is returned.
    pub postconditions: Vec<(crate::span::Spanned<crate::parser::ast::Expr>, Option<crate::span::Spanned<crate::parser::ast::Expr>>)>,
    /// Parameters upgraded from Borrow to Move in generic functions that return them directly.
    /// The return path must zero the source through the pointer to prevent caller double-free.
    pub move_override_params: std::collections::HashSet<String>,
    /// Methods whose -1 sentinel return should be wrapped into Option[int].
    /// Populated during registration for stdlib collection/string `find`/`index_of` methods.
    /// User-defined methods default to NOT being in this set.
    pub sentinel_to_option_methods: rustc_hash::FxHashSet<String>,
    /// Maps temp locals from field_load → (source_field_place, field_type).
    /// Used by VarDecl/Assign to emit MoveZero after extracting resource-type fields.
    pub field_load_origins: FxHashMap<LocalId, (crate::ir::instructions::Place, TypeId)>,
    /// TupleInit element origins: tuple_local → Vec<element_local_ids>.
    /// Used by the return path to MoveZero element locals when returning a tuple.
    pub tuple_element_locals: FxHashMap<LocalId, Vec<LocalId>>,
    /// Accumulated implicit clone warnings during lowering.
    pub implicit_clone_warnings: Vec<crate::ir::ImplicitCloneWarning>,
    /// Maps monomorphized method name → C runtime function name.
    /// Populated from BuiltinTypeProtocol declarations during module setup.
    /// Used by the LIR backend to replace `map_monomorphized_to_runtime()`.
    pub runtime_callees: FxHashMap<String, String>,
    /// CoW: variable names that are reassigned in the current function body.
    /// Pre-scanned before lowering. Locals in this set skip CoW aliasing.
    pub cow_reassigned_names: rustc_hash::FxHashSet<String>,
    /// CoW: alias_local → source_local. The alias holds a Ptr(T) that borrows source's data.
    /// When either side is mutated, the alias is severed by cloning.
    pub cow_alias_sources: FxHashMap<LocalId, LocalId>,
    /// CoW reverse: source_local → {alias_locals}. For severing when source is mutated.
    pub cow_alias_targets: FxHashMap<LocalId, rustc_hash::FxHashSet<LocalId>>,
    /// CoW: collection_local → [ref_locals pointing into it].
    /// When the collection is mutated, these refs must be cloned out first.
    /// CoW Phase 1c: bare Ptr params (borrow from caller). On mutation, these
    /// are cloned to owned copies so the caller's data is not modified.
    pub cow_ptr_params: rustc_hash::FxHashSet<LocalId>,
    pub cow_collection_refs: FxHashMap<LocalId, Vec<LocalId>>,
    /// Phase 1f: name → use count in the function body. Names with count=1 are
    /// single-use (dead after their one use) → auto-move at push/constructor.
    pub name_use_counts: rustc_hash::FxHashMap<String, u32>,
}


impl<'a> LoweringContext<'a> {
    pub fn new(analysis: &'a AnalysisResult, type_mapper: TypeMapper, type_registry: TypeRegistry) -> Self {
        Self {
            analysis,
            type_mapper,
            type_registry,
            closures: ClosureLowering::new(),
            drops: DropElaborator::new(),
            locals: FxHashMap::default(),
            fn_sigs: FxHashMap::default(),
            enum_variants: FxHashMap::default(),
            struct_fields: FxHashMap::default(),
            closure_info: FxHashMap::default(),
            loop_stack: Vec::new(),
            generics: GenericState::default(),
            spawn: SpawnState::default(),
            shared: SharedVarState::default(),
            module_constants: FxHashMap::default(),
            strip_asserts: false,
            snapshot_mode: false,
            overflow_wrap: false,
            explicit_clone: false,
            mut_capture_locals: FxHashMap::default(),
            ref_locals: FxHashSet::default(),
            named_locals: FxHashSet::default(),
            extern_bindings: FxHashMap::default(),
            fn_defaults: FxHashMap::default(),
            fn_param_names: FxHashMap::default(),
            fn_param_ownerships: FxHashMap::default(),
            fn_param_abis: FxHashMap::default(),
            current_throws_result_type: None,
            expected_type: None,
            closure_param_type_hints: Vec::new(),
            callable_return_types: FxHashMap::default(),
            global_names: rustc_hash::FxHashSet::default(),
            global_type_names: FxHashMap::default(),
            gir_equip_methods: rustc_hash::FxHashSet::default(),
            with_shared_refresh: Vec::new(),
            on_error_blocks: Vec::new(),
            postconditions: Vec::new(),
            move_override_params: std::collections::HashSet::new(),
            sentinel_to_option_methods: rustc_hash::FxHashSet::default(),
            field_load_origins: FxHashMap::default(),
            tuple_element_locals: FxHashMap::default(),
            implicit_clone_warnings: Vec::new(),
            runtime_callees: FxHashMap::default(),
            cow_reassigned_names: rustc_hash::FxHashSet::default(),
            cow_alias_sources: FxHashMap::default(),
            cow_alias_targets: FxHashMap::default(),
            cow_collection_refs: FxHashMap::default(),
            cow_ptr_params: rustc_hash::FxHashSet::default(),
            name_use_counts: rustc_hash::FxHashMap::default(),
        }
    }

    /// Populate fn_sigs and runtime_callees from the BuiltinTypeProtocol declarations.
    ///
    /// Scans ALL registered named types and matches them against the protocol table.
    /// For each match, instantiates method signatures with the type's concrete type args
    /// and inserts them into fn_sigs and runtime_callees.
    ///
    /// Called once during module setup, after all types have been registered.
    pub fn register_builtin_method_sigs(&mut self) {
        use crate::ir::lowering::builtins::{self, LookupCtx, BuiltinTypeArgs};
        use crate::ir::types::GirType;

        // Helper: resolve a C type name fragment to a TypeId.
        // Handles both primitives (int64_t, bool, double) and named types (GorgetStringView, Point).
        let resolve_type_name = |name: &str, mapper: &super::types::TypeMapper| -> TypeId {
            match name {
                "bool" => BOOL_TYPE,
                "double" | "float64_t" => F64_TYPE,
                "float" => crate::ir::types::F32_TYPE,
                "int64_t" | "int" => I64_TYPE,
                "int32_t" => crate::ir::types::I32_TYPE,
                "int16_t" => crate::ir::types::I16_TYPE,
                "int8_t" => crate::ir::types::I8_TYPE,
                "uint64_t" => crate::ir::types::U64_TYPE,
                "uint32_t" => crate::ir::types::U32_TYPE,
                "uint16_t" => crate::ir::types::U16_TYPE,
                "uint8_t" => crate::ir::types::U8_TYPE,
                "void" => UNIT_TYPE,
                "GorgetStringView" | "Str" => mapper.string_view_type,
                "GorgetString" => mapper.owned_string_type,
                other => mapper.lookup_named(other).unwrap_or(I64_TYPE),
            }
        };

        // Collect (mangled_name, type_id, protocol) for all builtin types.
        // We collect first to avoid borrow conflicts on self.
        let mut entries: Vec<(String, TypeId, &'static builtins::BuiltinTypeProtocol)> = Vec::new();
        for (mangled_name, &type_id) in &self.type_mapper.named_types {
            if let Some(protocol) = builtins::protocol_for_mangled_name(mangled_name) {
                entries.push((mangled_name.clone(), type_id, protocol));
            }
        }

        for (mangled_name, type_id, protocol) in &entries {
            // Extract type args from the mangled name.
            // Convention: Vector__int64_t → elem = resolve("int64_t")
            //             Dict__int64_t__Str → key = resolve("int64_t"), val = resolve("Str")
            let suffix = mangled_name.strip_prefix(protocol.base_name)
                .and_then(|s| s.strip_prefix("__"))
                .unwrap_or("");

            let (elem, key, val, elem_name_str, val_name_str) = if protocol.type_arity == 2 {
                // Two type args: K__V — split at first __ separator
                if let Some(pos) = suffix.find("__") {
                    let key_name = &suffix[..pos];
                    let val_name = &suffix[pos + 2..];
                    let key = resolve_type_name(key_name, &self.type_mapper);
                    let val = resolve_type_name(val_name, &self.type_mapper);
                    (key, key, val, key_name.to_string(), val_name.to_string())
                } else {
                    (I64_TYPE, I64_TYPE, I64_TYPE, "int64_t".to_string(), "int64_t".to_string())
                }
            } else if !suffix.is_empty() {
                // Single type arg: T
                let elem = resolve_type_name(suffix, &self.type_mapper);
                (elem, elem, elem, suffix.to_string(), suffix.to_string())
            } else {
                (I64_TYPE, I64_TYPE, I64_TYPE, "int64_t".to_string(), "int64_t".to_string())
            };

            let type_args = BuiltinTypeArgs {
                elem,
                key,
                val,
                self_type: *type_id,
                self_name: mangled_name.clone(),
            };

            let type_registry = &self.type_registry;
            let lookup_ctx = LookupCtx {
                lookup_type_by_name: &|name: &str| self.type_mapper.lookup_named(name),
                string_view_type: self.type_mapper.string_view_type,
                owned_string_type: self.type_mapper.owned_string_type,
                is_resource: &|tid| type_registry.is_resource_type(tid),
                ensure_option: &|name: &str, _inner: TypeId| {
                    // At startup, Options should already be registered; just look up.
                    self.type_mapper.lookup_named(name).unwrap_or(I64_TYPE)
                },
                elem_name: elem_name_str.clone(),
                val_name: val_name_str.clone(),
            };

            // Collect method entries first (to avoid borrow conflicts with type_registry)
            let method_entries: Vec<_> = protocol.methods.iter().map(|method| {
                let fn_key = format!("{mangled_name}__{}", method.name);
                let method_params = (method.params)(&type_args);
                let ret = (method.return_type)(&type_args, &lookup_ctx);
                (fn_key, method_params, ret, method.runtime_callee)
            }).collect();

            for (fn_key, method_params, ret, runtime_callee) in method_entries {
                // Build full params: self pointer + method params
                let self_ptr_type = self.type_registry.insert(
                    GirType::MutPtr(*type_id)
                );
                let mut params = vec![self_ptr_type];
                params.extend(method_params);

                // Only insert if not already present (equip-defined methods take precedence)
                if !self.fn_sigs.contains_key(&fn_key) {
                    self.fn_sigs.insert(fn_key.clone(), (params, ret));
                }

                // Runtime callee mapping (for LIR backend)
                if let Some(callee) = runtime_callee {
                    self.runtime_callees.insert(fn_key, callee.to_string());
                }
            }
        }
    }

    /// Populate only the runtime_callees table from the protocol (not fn_sigs).
    /// Called at startup; fn_sigs is populated on-the-fly by resolve_builtin_method_return_type.
    pub fn register_builtin_runtime_callees(&mut self) {
        use crate::ir::lowering::builtins;

        for (mangled_name, &_type_id) in &self.type_mapper.named_types.clone() {
            if let Some(protocol) = builtins::protocol_for_mangled_name(mangled_name) {
                for method in protocol.methods {
                    if let Some(callee) = method.runtime_callee {
                        let fn_key = format!("{mangled_name}__{}", method.name);
                        self.runtime_callees.entry(fn_key).or_insert_with(|| callee.to_string());
                    }
                }
            }
        }
    }

    /// Resolve a builtin method's return type on-the-fly from the protocol table.
    /// Used as a fallback when fn_sigs doesn't have an entry (late-registered types).
    /// Also populates fn_sigs and runtime_callees for future lookups.
    pub fn resolve_builtin_method_return_type(&mut self, type_name: &str, method_name: &str) -> Option<TypeId> {
        use crate::ir::lowering::builtins::{self, LookupCtx, BuiltinTypeArgs};

        let protocol = builtins::protocol_for_mangled_name(type_name)?;

        // Find the method in the protocol
        let method = protocol.methods.iter().find(|m| m.name == method_name)?;

        // Extract type args
        let suffix = type_name.strip_prefix(protocol.base_name)
            .and_then(|s| s.strip_prefix("__"))
            .unwrap_or("");

        let resolve = |name: &str| -> TypeId {
            match name {
                "bool" => BOOL_TYPE,
                "double" | "float64_t" => crate::ir::types::F64_TYPE,
                "int64_t" | "int" => I64_TYPE,
                "int32_t" => crate::ir::types::I32_TYPE,
                "int8_t" => crate::ir::types::I8_TYPE,
                "uint8_t" => crate::ir::types::U8_TYPE,
                "uint16_t" => crate::ir::types::U16_TYPE,
                "uint32_t" => crate::ir::types::U32_TYPE,
                "uint64_t" => crate::ir::types::U64_TYPE,
                "void" => UNIT_TYPE,
                "GorgetStringView" | "Str" => self.type_mapper.string_view_type,
                "GorgetString" => self.type_mapper.owned_string_type,
                other => self.type_mapper.lookup_named(other).unwrap_or(I64_TYPE),
            }
        };

        let self_type = self.type_mapper.lookup_named(type_name).unwrap_or(I64_TYPE);

        let (elem, key, val, elem_name_str, val_name_str) = if protocol.type_arity == 2 {
            if let Some(pos) = suffix.find("__") {
                let key_name = &suffix[..pos];
                let val_name = &suffix[pos + 2..];
                let k = resolve(key_name);
                let v = resolve(val_name);
                (k, k, v, key_name.to_string(), val_name.to_string())
            } else {
                (I64_TYPE, I64_TYPE, I64_TYPE, "int64_t".to_string(), "int64_t".to_string())
            }
        } else if !suffix.is_empty() {
            let e = resolve(suffix);
            (e, e, e, suffix.to_string(), suffix.to_string())
        } else {
            (I64_TYPE, I64_TYPE, I64_TYPE, "int64_t".to_string(), "int64_t".to_string())
        };

        let type_args = BuiltinTypeArgs {
            elem, key, val,
            self_type,
            self_name: type_name.to_string(),
        };

        let type_registry = &self.type_registry;
        let type_mapper = &self.type_mapper;
        let lookup_ctx = LookupCtx {
            lookup_type_by_name: &|name: &str| self.type_mapper.lookup_named(name),
            string_view_type: self.type_mapper.string_view_type,
            owned_string_type: self.type_mapper.owned_string_type,
            is_resource: &|tid| type_registry.is_resource_type(tid),
            ensure_option: &|name: &str, inner: TypeId| {
                // On-the-fly: look up or register the Option type
                if let Some(tid) = type_mapper.lookup_named(name) {
                    return tid;
                }
                // Option not registered yet — return I64_TYPE as fallback.
                // The Option type will be registered by the override logic in
                // lower_method_call if needed.
                I64_TYPE
            },
            elem_name: elem_name_str.clone(),
            val_name: val_name_str.clone(),
        };

        let ret = (method.return_type)(&type_args, &lookup_ctx);

        // Populate fn_sigs for future lookups
        let fn_key = format!("{type_name}__{method_name}");
        if !self.fn_sigs.contains_key(&fn_key) {
            let method_params = (method.params)(&type_args);
            let self_ptr_type = self.type_registry.insert(
                crate::ir::types::GirType::MutPtr(self_type)
            );
            let mut params = vec![self_ptr_type];
            params.extend(method_params);
            self.fn_sigs.insert(fn_key.clone(), (params, ret));
        }

        // Populate runtime_callees
        if let Some(callee) = method.runtime_callee {
            self.runtime_callees.entry(fn_key).or_insert_with(|| callee.to_string());
        }

        Some(ret)
    }

    /// Emit an implicit clone warning for a resource type being auto-cloned.
    pub fn warn_implicit_clone(
        &mut self,
        span: crate::span::Span,
        type_id: TypeId,
        reason: crate::ir::ImplicitCloneReason,
    ) {
        let type_name = self.type_registry.type_name(type_id)
            .map(|n| demangle_type_name(&n))
            .unwrap_or_else(|| "unknown".to_string());
        let is_error = self.explicit_clone;
        self.implicit_clone_warnings.push(crate::ir::ImplicitCloneWarning {
            span,
            type_name,
            reason,
            is_error,
        });
    }

}

/// Convert an internal mangled type name to user-friendly Gorget syntax.
/// e.g., `Vector__int64_t` → `Vector[int]`, `Dict__GorgetString__int64_t` → `Dict[String, int]`
fn demangle_type_name(name: &str) -> String {
    // Map C type names back to Gorget names
    fn c_to_gorget(s: &str) -> &str {
        match s {
            "int64_t" => "int",
            "double" => "float",
            "bool" => "bool",
            "int32_t" => "int32",
            "int16_t" => "int16",
            "int8_t" => "int8",
            "uint64_t" => "uint",
            "uint32_t" => "uint32",
            "uint16_t" => "uint16",
            "uint8_t" => "uint8",
            "float" => "float32",
            "GorgetString" => "String",
            "GorgetStringView" => "String",
            _ => s,
        }
    }

    // Vector__int64_t → Vector[int]
    if let Some(elem) = name.strip_prefix("Vector__") {
        return format!("Vector[{}]", c_to_gorget(elem));
    }
    // Dict__K__V → Dict[K, V]
    if let Some(rest) = name.strip_prefix("Dict__") {
        if let Some(sep) = rest.find("__") {
            let key = &rest[..sep];
            let val = &rest[sep + 2..];
            return format!("Dict[{}, {}]", c_to_gorget(key), c_to_gorget(val));
        }
    }
    // Set__T → Set[T]
    if let Some(elem) = name.strip_prefix("Set__") {
        return format!("Set[{}]", c_to_gorget(elem));
    }
    // GorgetString → String
    if name == "GorgetString" { return "String".to_string(); }

    // Fallback: replace __ with generic brackets for other types
    if let Some(sep) = name.find("__") {
        let base = &name[..sep];
        let arg = &name[sep + 2..];
        return format!("{}[{}]", base, c_to_gorget(arg));
    }

    name.to_string()
}

impl<'a> LoweringContext<'a> {
    /// Resolve a type name, applying any active substitutions.
    pub fn resolve_type_name(&self, name: &str) -> String {
        if let Some(resolved) = self.generics.type_name_subs.get(name) {
            return resolved.clone();
        }
        // On-the-fly fragment substitution for names like "Vector__T" → "Vector__int64_t"
        // that weren't in the pre-computed map (because the template type was never registered).
        if !self.generics.generic_fragment_subs.is_empty() {
            let mut result = name.to_string();
            let mut changed = false;
            for (param, concrete) in &self.generics.generic_fragment_subs {
                let pattern_end = format!("__{param}");
                let pattern_mid = format!("__{param}__");
                if result.ends_with(&pattern_end) {
                    let prefix = &result[..result.len() - pattern_end.len()];
                    result = format!("{prefix}__{concrete}");
                    changed = true;
                } else if result.contains(&pattern_mid) {
                    result = result.replace(&pattern_mid, &format!("__{concrete}__"));
                    changed = true;
                }
            }
            if changed {
                return result;
            }
        }
        name.to_string()
    }

    /// Map an AST type to a GIR TypeId, applying any active type name substitutions.
    /// Use this instead of `type_mapper.map_ast_type()` when inside generic body lowering.
    pub fn map_type_with_subs(&self, ty: &crate::parser::ast::Type) -> TypeId {
        use crate::parser::ast::Type;
        // Check bare type parameter substitution (e.g., T → int64_t)
        if let Type::Named { name, generic_args } = ty {
            if generic_args.is_empty() {
                if let Some(&id) = self.generics.generic_type_params.get(name.node.as_str()) {
                    return id;
                }
            } else if !self.generics.type_name_subs.is_empty() || !self.generics.generic_fragment_subs.is_empty() {
                // For generic named types, check if the mangled name needs substitution.
                // resolve_type_name handles both type_name_subs (pre-computed) and
                // generic_fragment_subs (on-the-fly), e.g. "Vector__T" → "Vector__int64_t".
                let mangled = super::types::mangle_generic_name(&name.node, generic_args);
                let resolved = self.resolve_type_name(&mangled);
                if let Some(&id) = self.type_mapper.named_types.get(&resolved) {
                    return id;
                }
            }
        }
        self.type_mapper.map_ast_type(ty)
    }

    /// Register a variable in the current function scope.
    pub fn register_local(&mut self, name: &str, local_id: LocalId, type_id: TypeId) {
        self.locals.insert(name.to_string(), (local_id, type_id));
        self.named_locals.insert(local_id);
    }

    /// Phase 1f: check if a named variable is single-use (dead after its one use).
    /// Single-use variables can be auto-moved at push/constructor instead of cloned.
    /// Conservative: if the name wasn't found in the pre-scan (count=0), assume multi-use.
    pub fn is_single_use(&self, name: &str) -> bool {
        matches!(self.name_use_counts.get(name), Some(1))
    }

    /// Check if a local is a named variable (vs an anonymous temp).
    pub fn is_named_local(&self, local: LocalId) -> bool {
        self.named_locals.contains(&local)
    }

    /// Create a local AND register it for drop if its type needs dropping.
    /// Use this instead of `builder.add_local()` for temps that might be resource types.
    /// The `needs_drop()` check inside `register_local` automatically skips primitives
    /// and Ptr types — zero overhead for trivial types.
    pub fn add_local_tracked(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        type_id: crate::ir::types::TypeId,
    ) -> crate::ir::types::LocalId {
        let local = builder.add_local(type_id, None);
        self.drops.register_local(local, type_id, &self.type_registry);
        local
    }

    /// Call a function and auto-register the result for drop if it needs dropping.
    /// Uses `needs_drop` which covers Trivial, Custom, Recursive, and collection types.
    /// Safe because Move semantics zero the source when temps are consumed by assignment.
    pub fn call_tracked(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: crate::ir::types::TypeId,
    ) -> crate::ir::types::LocalId {
        let func_name: String = func.into();
        let local = builder.call(&func_name, args, return_type);
        if self.type_registry.needs_drop(return_type) {
            self.drops.register_local(local, return_type, &self.type_registry);
        }
        // User-defined functions that return StringView actually return owned
        // data at runtime (IR clones/materializes on return). Upgrade to
        // GorgetString and register for drop. Only for GIR-lowered equip
        // methods — runtime builtins may genuinely return views.
        else if return_type == self.type_mapper.string_view_type
            && self.gir_equip_methods.contains(func_name.as_str())
        {
            let owned = self.type_mapper.owned_string_type;
            builder.locals[local.0 as usize].type_id = owned;
            self.drops.register_local(local, owned, &self.type_registry);
        }
        local
    }

    /// Call an extern function and auto-register the result for drop.
    pub fn call_extern_tracked(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: crate::ir::types::TypeId,
    ) -> crate::ir::types::LocalId {
        let local = builder.call_extern(func, args, return_type);
        if self.type_registry.needs_drop(return_type) {
            self.drops.register_local(local, return_type, &self.type_registry);
        }
        local
    }

    /// Look up a variable by name.
    pub fn lookup_local(&self, name: &str) -> Option<(LocalId, TypeId)> {
        self.locals.get(name).copied()
    }

    /// Reset locals for the next function.
    pub fn clear_locals(&mut self) {
        self.locals.clear();
        self.mut_capture_locals.clear();
        self.ref_locals.clear();
        self.named_locals.clear();
        self.spawn.result_locals.clear();
        self.spawn.pending_fn = None;
        self.shared.locals.clear();
        self.with_shared_refresh.clear();
        self.postconditions.clear();
        self.move_override_params.clear();
        self.cow_reassigned_names.clear();
        self.cow_alias_sources.clear();
        self.cow_alias_targets.clear();
        self.cow_collection_refs.clear();
        self.cow_ptr_params.clear();
    }

    /// Clone the locals map for save/restore around nested scopes (if, while, for, match, etc.).
    pub fn save_locals(&self) -> FxHashMap<String, (LocalId, TypeId)> {
        self.locals.clone()
    }

    /// Take the locals map, leaving it empty. Used for save/restore during async variant generation.
    pub fn take_locals(&mut self) -> FxHashMap<String, (LocalId, TypeId)> {
        std::mem::take(&mut self.locals)
    }

    /// Restore a previously saved locals map.
    pub fn restore_locals(&mut self, locals: FxHashMap<String, (LocalId, TypeId)>) {
        self.locals = locals;
    }

    /// Iterate over all locals (for type inference).
    pub fn locals_iter(&self) -> impl Iterator<Item = (&String, &(LocalId, TypeId))> {
        self.locals.iter()
    }

    /// Resolve the GIR type for a variable declaration.
    /// Uses the explicit type if given, otherwise infers from the value expression for `auto`.
    pub fn resolve_var_type(
        &self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
    ) -> TypeId {
        match &type_.node {
            Type::Inferred => self.infer_type_from_expr(&value.node),
            other => self.map_type_with_subs(other),
        }
    }

    /// Adjust a GIR string type based on provenance analysis.
    /// After str→StringType parser unification, all string annotations produce
    /// `owned_string_type` in the IR. But provenance may have downgraded the
    /// semantic type_id to `string_id` (view). This method checks the semantic
    /// type_id and returns `str_type` if provenance determined the binding is a view.
    pub fn provenance_adjusted_string_type(
        &self,
        gir_type: TypeId,
        name: &str,
        span: crate::span::Span,
    ) -> TypeId {
        if gir_type != self.type_mapper.owned_string_type {
            return gir_type;
        }
        // Look up the provenance-adjusted type_id from semantic analysis
        if let Some(def_id) = self.analysis.scopes.lookup_def_by_span(name, span) {
            let def = self.analysis.scopes.get_def(def_id);
            if let Some(sem_tid) = def.type_id {
                if sem_tid == self.analysis.types.string_id {
                    return self.type_mapper.string_view_type;
                }
            }
        }
        gir_type
    }

    /// Extract the Ok type from a Result TypeId, if it is a Result type.
    pub fn unwrap_result_ok_type(&self, result_type: TypeId) -> Option<TypeId> {
        let name = self.type_registry.type_name(result_type)?;
        let td = self.type_registry.get_type_def(&name)?;
        if td.metadata.enum_category != Some(EnumCategory::Result) && !name.starts_with("Result__") {
            return None;
        }
        if let TypeDefKind::Enum(e) = &td.kind {
            e.variants.iter().find(|v| v.name == "Ok")
                .and_then(|v| v.fields.first().map(|f| f.type_id))
        } else { None }
    }

    /// Infer a GIR type from a literal expression (for `auto` declarations).
    fn infer_type_from_expr(&self, expr: &Expr) -> TypeId {
        match expr {
            Expr::IntLiteral(_) => I64_TYPE,
            Expr::FloatLiteral(_) => F64_TYPE,
            Expr::BoolLiteral(_) => BOOL_TYPE,
            Expr::StringLiteral(_) => self.type_mapper.string_view_type,
            Expr::BinaryOp { left, op, .. } => {
                use crate::parser::ast::BinaryOp;
                match op {
                    BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt
                    | BinaryOp::LtEq | BinaryOp::GtEq | BinaryOp::And | BinaryOp::Or => {
                        BOOL_TYPE
                    }
                    _ => self.infer_type_from_expr(&left.node),
                }
            }
            Expr::UnaryOp { operand, .. } => self.infer_type_from_expr(&operand.node),
            Expr::Call { callee, .. } => {
                // Look up the function return type
                if let Expr::Identifier(name) = &callee.node {
                    if let Some((_, ret_ty)) = self.fn_sigs.get(name.as_str()) {
                        let ret_ty = *ret_ty;
                        // In a propagation context, auto-unwrap Result to its Ok type
                        if self.current_throws_result_type.is_some() {
                            if let Some(ok_ty) = self.unwrap_result_ok_type(ret_ty) {
                                return ok_ty;
                            }
                        }
                        return ret_ty;
                    }
                    // Check if it's an enum variant constructor
                    if let Some((enum_name, _)) = self.enum_variants.get(name.as_str()) {
                        if let Some(&type_id) = self.type_mapper.named_types.get(enum_name.as_str()) {
                            return type_id;
                        }
                    }
                }
                I64_TYPE // fallback
            }
            Expr::Identifier(name) => {
                if let Some((_, ty)) = self.lookup_local(name) {
                    return ty;
                }
                I64_TYPE // fallback
            }
            Expr::StructLiteral { name, .. } => {
                if let Some(&type_id) = self.type_mapper.named_types.get(name.node.as_str()) {
                    return type_id;
                }
                UNIT_TYPE
            }
            Expr::FieldAccess { object, .. } => {
                // Try to infer from the object type
                self.infer_type_from_expr(&object.node)
            }
            _ => I64_TYPE, // conservative default
        }
    }

    /// Resolve type for a const variable (same as regular var for Phase 1).
    pub fn resolve_const_type(
        &self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
    ) -> TypeId {
        match &type_.node {
            Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Int64) => I64_TYPE,
            _ => self.resolve_var_type(type_, value),
        }
    }

    /// Resolve an identifier to an enum variant: returns (enum_type_name, variant_name).
    pub fn resolve_enum_variant(&self, name: &str) -> Option<(String, String)> {
        self.enum_variants.get(name).cloned()
    }

    /// Register a pointer type and return its TypeId.
    pub fn register_ptr_type(&mut self, pointee: TypeId) -> TypeId {
        self.type_registry.insert(GirType::Ptr(pointee))
    }

    /// Register a mutable pointer type and return its TypeId.
    pub fn register_mut_ptr_type(&mut self, pointee: TypeId) -> TypeId {
        self.type_registry.insert(GirType::MutPtr(pointee))
    }

    /// Resolve a parameter's GIR type with explicit reference semantics.
    ///
    /// Move-type params are always passed by pointer:
    /// - Borrow (bare) → Ptr (const T*) — read-only, callee cannot mutate
    /// - MutableBorrow (&) → MutPtr (T*) — mutable
    /// - Move (!) → MutPtr (T*) — mutable, callee drops pointee
    ///
    /// Copy-type structs pass by value (natural immutability via copy).
    /// Primitives pass by value (except & which becomes MutPtr for out-params).
    pub fn resolve_param_type(&mut self, base_type: TypeId, ownership: Ownership) -> TypeId {
        let is_move = self.type_registry.is_resource_type(base_type);
        match ownership {
            Ownership::MutableBorrow => self.register_mut_ptr_type(base_type),
            Ownership::Move if is_move => self.register_mut_ptr_type(base_type),
            Ownership::Borrow if is_move => self.register_ptr_type(base_type),
            _ => base_type,
        }
    }

    /// Compute the ABI for a single parameter from its base type and ownership.
    pub fn compute_param_abi(&self, base_type: TypeId, ownership: Ownership) -> ParamABI {
        let is_move = self.type_registry.is_resource_type(base_type);
        match ownership {
            Ownership::MutableBorrow => ParamABI::ByMutPtr,
            Ownership::Move if is_move => ParamABI::ByMutPtr,
            Ownership::Borrow if is_move => ParamABI::ByPtr,
            _ => ParamABI::ByValue,
        }
    }

    /// Whether this is a bare Borrow param of a Move type (read-only const pointer).
    pub fn is_ref_param(&self, base_type: TypeId, ownership: Ownership) -> bool {
        matches!(ownership, Ownership::Borrow) && self.type_registry.is_resource_type(base_type)
    }

    /// Whether this param results in a MutPtr (mutable pointer).
    /// True for MutableBorrow (&) and Move (!) on Move types.
    pub fn is_mut_ref_param(&self, base_type: TypeId, ownership: Ownership) -> bool {
        matches!(ownership, Ownership::MutableBorrow)
            || (matches!(ownership, Ownership::Move) && self.type_registry.is_resource_type(base_type))
    }

    /// Whether this param ownership + type combination results in pass-by-pointer.
    /// Resource types and &-annotated params are passed by pointer.
    pub fn is_passed_by_ptr(&self, base_type: TypeId, ownership: Ownership) -> bool {
        self.is_ref_param(base_type, ownership) || self.is_mut_ref_param(base_type, ownership)
    }

    /// Return the clone function name for deep-cloning a resource type.
    /// Used for Ptr(T) → T auto-clone and named-variable clone.
    /// Returns None for trivial types and GorgetString (uses provenance).
    pub fn clone_fn_for_ptr(&self, inner_type: TypeId) -> Option<String> {
        use crate::ir::types::{GirType, DropStrategy};
        if let Some(GirType::Named(name)) = self.type_registry.get(inner_type) {
            // Metadata-based check: clone_fn set during type registration from protocol table.
            if let Some(type_def) = self.type_registry.get_type_def(name) {
                if let Some(ref clone_fn) = type_def.metadata.clone_fn {
                    return Some(clone_fn.clone());
                }
            }

            // GorgetString → gorget_string_clone_to_owned (always produces owned copy).
            if name == "GorgetString" || name == "GorgetStringView" {
                return Some("gorget_string_clone_to_owned".to_string());
            }

            // Fallback for types without metadata.clone_fn (migration safety):
            // Collections that might not have TypeDefs with clone_fn yet.
            if name.starts_with("Vector__") || name.starts_with("Deque__") || name == "GorgetArray" {
                return Some("gorget_array_clone".to_string());
            }
            if name.starts_with("Dict__") || name.starts_with("HashMap__") || name == "GorgetMap" {
                return Some("gorget_map_clone".to_string());
            }
            if name.starts_with("Set__") || name.starts_with("HashSet__") || name == "GorgetSet" {
                return Some("gorget_set_clone".to_string());
            }

            // User structs with Recursive or Custom drop → generated {Name}__clone.
            if let Some(type_def) = self.type_registry.get_type_def(name) {
                if matches!(type_def.metadata.drop_strategy,
                    DropStrategy::Recursive | DropStrategy::Custom(_))
                {
                    return Some(format!("{name}__clone"));
                }
                // User enums with cloneable variant payloads → generated {Name}__clone.
                if let crate::ir::types::TypeDefKind::Enum(ref edef) = type_def.kind {
                    if !self.type_registry.is_option_or_result(name) && !name.starts_with("Option__") && !name.starts_with("Result__") {
                        let has_cloneable_payload = edef.variants.iter().any(|v| {
                            v.fields.iter().any(|f| self.type_registry.is_resource_type(f.type_id))
                        });
                        if has_cloneable_payload {
                            return Some(format!("{name}__clone"));
                        }
                    }
                }
            }
        }
        None
    }

    /// If an operand is Ptr(T), deep-clone it to produce an owned T.
    /// Used at Ptr→T boundaries: function args, enum constructors, collection push, etc.
    /// Returns the cloned operand (owned T), or the original if not Ptr.
    pub fn auto_clone_if_ptr(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
    ) -> Operand {
        if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
            if place.projections.is_empty() {
                let local_type = builder.locals[place.local.0 as usize].type_id;
                if let Some(inner) = self.pointee_type(local_type) {
                    if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                        let cloned = builder.call(
                            &clone_fn,
                            vec![crate::ir::builder::FunctionBuilder::copy(place.local)],
                            inner,
                        );
                        self.drops.register_local(cloned, inner, &self.type_registry);
                        return crate::ir::builder::FunctionBuilder::copy(cloned);
                    }
                }
            }
        }
        operand
    }

    // ── Copy-on-Write alias management ────────────────────────────────

    /// Register a CoW alias: `alias_local` is a Ptr(T) borrowing from `source_local`.
    /// Resolves transitively: if source is itself an alias, points to the root.
    pub fn cow_register_alias(&mut self, alias_local: LocalId, source_local: LocalId) {
        // Resolve to root source (transitively)
        let root = self.cow_resolve_root(source_local);
        self.cow_alias_sources.insert(alias_local, root);
        self.cow_alias_targets.entry(root).or_default().insert(alias_local);
    }

    /// Resolve a local to its root source (follow alias chain).
    fn cow_resolve_root(&self, local: LocalId) -> LocalId {
        let mut current = local;
        while let Some(&source) = self.cow_alias_sources.get(&current) {
            if source == current { break; }
            current = source;
        }
        current
    }

    /// Check if a local is a CoW alias of something else.
    pub fn cow_is_alias(&self, local: LocalId) -> bool {
        self.cow_alias_sources.contains_key(&local)
    }

    /// Check if a local has CoW aliases pointing to it (is a source).
    pub fn cow_has_aliases(&self, local: LocalId) -> bool {
        self.cow_alias_targets.get(&local).map_or(false, |s| !s.is_empty())
    }

    /// Before mutating `local`, sever all CoW alias relationships:
    /// - If `local` is an alias → clone source into local (local becomes owned).
    /// - If `local` is a source → clone into each alias (aliases become owned).
    /// - If `local` is a collection with refs → clone each ref out.
    pub fn cow_before_mutation(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
    ) {
        // Phase 1c: bare Ptr params — clone to owned before mutation
        // so the caller's data is not modified.
        if self.cow_ptr_params.remove(&local) {
            self.cow_materialize_alias(builder, local, local);
            // After materialization, fall through to check other CoW relationships
            // (the new owned local might itself be a source).
        }

        // Early exit: if local has no CoW relationships, nothing to do.
        if !self.cow_alias_sources.contains_key(&local)
            && !self.cow_alias_targets.contains_key(&local)
            && !self.cow_collection_refs.contains_key(&local)
        {
            return;
        }

        // Case 1: local is an alias of something else → clone source into local
        if let Some(source) = self.cow_alias_sources.remove(&local) {
            // Remove from reverse map
            if let Some(targets) = self.cow_alias_targets.get_mut(&source) {
                targets.remove(&local);
            }
            // Clone: get the source's inner type, call clone, store in local
            self.cow_materialize_alias(builder, local, source);
        }

        // Case 2: local is a source with aliases → clone into each alias
        if let Some(aliases) = self.cow_alias_targets.remove(&local) {
            for alias in aliases {
                self.cow_alias_sources.remove(&alias);
                self.cow_materialize_alias(builder, alias, local);
            }
        }

        // Case 3: local is a collection with refs into it → clone each ref
        if let Some(refs) = self.cow_collection_refs.remove(&local) {
            for ref_local in refs {
                // Only sever if the ref is still live (not already moved/reassigned)
                if self.ref_locals.contains(&ref_local) {
                    self.cow_materialize_collection_ref(builder, ref_local);
                }
            }
        }
    }

    /// Sever all aliases that point to `source_local` as their root.
    /// Used when `source_local` is about to be reassigned (aliases keep old value).
    pub fn cow_sever_all_aliases_from(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        source_local: LocalId,
    ) {
        if let Some(aliases) = self.cow_alias_targets.remove(&source_local) {
            for alias in aliases {
                self.cow_alias_sources.remove(&alias);
                self.cow_materialize_alias(builder, alias, source_local);
            }
        }
    }

    /// Materialize an alias: clone the source's data into the alias local.
    /// Changes the alias from Ptr(T) to owned T, registers for drop.
    fn cow_materialize_alias(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        alias_local: LocalId,
        _source_local: LocalId,
    ) {
        let alias_type = builder.locals[alias_local.0 as usize].type_id;
        // Only materialize if alias is actually a Ptr(T)
        let inner_type = match self.pointee_type(alias_type) {
            Some(inner) => inner,
            None => return, // Not a Ptr — nothing to materialize
        };
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            // The alias local already holds a Ptr(T) to the source data.
            // Clone it to produce an owned T.
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(alias_local)], inner_type);
            // Create a NEW local for the owned copy (can't reuse alias_local because
            // ref_locals is static per-function in the LIR — changing type mid-function breaks).
            let name_hint = builder.locals[alias_local.0 as usize].name_hint.clone();
            let owned_local = builder.add_local(inner_type,
                name_hint.as_deref());
            builder.assign(crate::ir::instructions::Place::local(owned_local),
                          crate::ir::builder::FunctionBuilder::copy(cloned));
            // Register the owned local for drop
            self.drops.register_local(owned_local, inner_type, &self.type_registry);
            // Update context: redirect the variable name to the new owned local
            if let Some(ref hint) = builder.locals[alias_local.0 as usize].name_hint {
                let name = hint.clone();
                self.register_local(&name, owned_local, inner_type);
                self.named_locals.insert(owned_local);
            }
            // The old alias_local stays as Ptr in ref_locals (dead — no more references).
        }
    }

    /// Materialize a collection ref: clone the pointed-to element into an owned local.
    fn cow_materialize_collection_ref(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        ref_local: LocalId,
    ) {
        let ref_type = builder.locals[ref_local.0 as usize].type_id;
        let inner_type = self.pointee_type(ref_type).unwrap_or(ref_type);
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            // The ref_local already holds a Ptr — use it directly as the clone arg
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(ref_local)], inner_type);
            // Update type, store clone, register for drop
            builder.locals[ref_local.0 as usize].type_id = inner_type;
            builder.assign(crate::ir::instructions::Place::local(ref_local),
                          crate::ir::builder::FunctionBuilder::copy(cloned));
            self.ref_locals.remove(&ref_local);
            self.drops.register_local(ref_local, inner_type, &self.type_registry);
        }
    }

    /// If the given local came from a resource-type field load, emit MoveZero for the
    /// source field to prevent double-free. Call this whenever a field_load temp is
    /// consumed (via assignment, function call, push, etc.).
    pub fn emit_field_origin_zero(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
    ) {
        if let Some((field_place, _)) = self.field_load_origins.remove(&local) {
            builder.move_zero(field_place);
        }
    }

    /// Populate the struct_fields cache from the TypeRegistry.
    /// Call this after all types have been registered.
    pub fn populate_struct_fields(&mut self) {
        for type_def in self.type_registry.type_defs() {
            if let TypeDefKind::Struct(ref s) = type_def.kind {
                for (i, field) in s.fields.iter().enumerate() {
                    self.struct_fields.insert(
                        (type_def.name.clone(), field.name.clone()),
                        (i as u32, field.type_id),
                    );
                }
            }
        }
    }

    /// Look up field info from the cached struct_fields: returns (field_index, field_type_id).
    pub fn lookup_field(&self, type_name: &str, field_name: &str) -> Option<(u32, TypeId)> {
        self.struct_fields.get(&(type_name.to_string(), field_name.to_string())).copied()
    }

    /// Get the type name for a GIR TypeId from the named_types cache.
    pub fn type_name_for_id(&self, type_id: TypeId) -> Option<&str> {
        self.type_mapper.named_types.iter()
            .find_map(|(name, &id)| if id == type_id { Some(name.as_str()) } else { None })
    }

    /// Get the C type name for a GIR TypeId, including primitive types.
    /// Unlike `type_name_for_id`, this also handles int, float, bool, str, etc.
    /// Falls back to named_types cache for user-defined types.
    pub fn c_type_name_for_id(&self, type_id: TypeId) -> String {
        use crate::ir::types::*;
        match type_id {
            BOOL_TYPE => "bool".to_string(),
            I8_TYPE => "int8_t".to_string(),
            I16_TYPE => "int16_t".to_string(),
            I32_TYPE => "int32_t".to_string(),
            I64_TYPE => "int64_t".to_string(),
            U8_TYPE => "uint8_t".to_string(),
            U16_TYPE => "uint16_t".to_string(),
            U32_TYPE => "uint32_t".to_string(),
            U64_TYPE => "uint64_t".to_string(),
            F32_TYPE => "float".to_string(),
            F64_TYPE => "double".to_string(),
            _ => self.type_name_for_id(type_id)
                .unwrap_or("int64_t")
                .to_string(),
        }
    }

    /// Register closure info for call dispatch.
    pub fn register_closure_info(
        &mut self,
        struct_name: String,
        call_fn_name: String,
        struct_type_id: TypeId,
        captures: Vec<(String, TypeId, u32)>,
    ) {
        self.closure_info.insert(struct_name, (call_fn_name, struct_type_id, captures));
    }

    /// Look up closure info by struct name.
    pub fn lookup_closure_info(&self, struct_name: &str) -> Option<(&str, TypeId, &[(String, TypeId, u32)])> {
        self.closure_info.get(struct_name).map(|(name, tid, caps)| (name.as_str(), *tid, caps.as_slice()))
    }

    // ---- Loop stack for break/continue ----

    /// Push a loop onto the stack (called when entering a while/for/loop).
    pub fn push_loop(&mut self, header_bb: BlockId, exit_bb: BlockId) {
        self.loop_stack.push(LoopInfo { header_bb, exit_bb });
    }

    /// Pop the current loop off the stack.
    pub fn pop_loop(&mut self) {
        self.loop_stack.pop();
    }

    /// Get the current (innermost) loop info for break/continue.
    pub fn current_loop(&self) -> Option<&LoopInfo> {
        self.loop_stack.last()
    }

    // ---- Enum variant tag resolution ----

    /// Resolve the tag index for an enum variant.
    pub fn resolve_variant_tag(&self, type_name: &str, variant_name: &str) -> Option<i64> {
        if let Some(type_def) = self.type_registry.get_type_def(type_name) {
            if let TypeDefKind::Enum(ref e) = type_def.kind {
                for (i, v) in e.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return Some(i as i64);
                    }
                }
            }
        }
        None
    }

    /// Resolve a pointer type to its pointee: Ptr(T) or MutPtr(T) → Some(T), else None.
    pub fn pointee_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.type_registry.get(type_id)? {
            GirType::Ptr(inner) | GirType::MutPtr(inner) => Some(*inner),
            _ => None,
        }
    }

    /// Resolve the inner type of a Box[T] (Named "Box__X" type) or a raw Ptr/MutPtr.
    /// Box types are stored as GirType::Named("Box__X") with a TypeDef having a single "_0" field.
    /// This is used for `*box_var` dereferencing where pointee_type() would return None.
    pub fn deref_inner_type(&self, type_id: TypeId) -> Option<TypeId> {
        // First try raw pointer types (Ptr/MutPtr)
        if let Some(inner) = self.pointee_type(type_id) {
            return Some(inner);
        }
        // Then try Named Box types: TypeDef with a single "_0" field
        if let Some(GirType::Named(name)) = self.type_registry.get(type_id) {
            if name.starts_with("Box__") {
                if let Some(type_def) = self.type_registry.get_type_def(name.as_str()) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        if let Some(f) = s.fields.first() {
                            if f.name == "_0" {
                                return Some(f.type_id);
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Look up a TypeId for a named type in the registry.
    pub fn lookup_type_by_name(&self, name: &str) -> Option<TypeId> {
        for i in 0..self.type_registry.len() {
            let tid = TypeId(i as u32);
            if let Some(GirType::Named(n)) = self.type_registry.get(tid) {
                if n == name { return Some(tid); }
            }
        }
        None
    }

    /// Auto-register an Option[T] type if it doesn't exist yet.
    /// Used when Vector.get() is called and Option[T] wasn't pre-registered.
    pub fn ensure_option_type_registered(&mut self, option_name: &str, inner_type: TypeId) {
        use super::types::make_option_type_def;
        self.type_mapper.get_or_register(option_name, &mut self.type_registry, |n| {
            make_option_type_def(n, inner_type)
        });
    }
}

/// Known blocking call names at AST level — these are yield points where the
/// shared-variable token is released. Must stay in sync with
/// `src/ir/lowering/exprs/mod.rs::BLOCKING_CALL_NAMES`.
const BLOCKING_AST_CALLS: &[&str] = &[
    "sleep", "read_file", "write_file", "append_file",
    "readdir", "http_get", "http_post", "http_put", "http_delete",
];

/// Scan an AST expression for any yield points (`.await()` or blocking calls).
pub fn expr_has_await(expr: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::Expr;
    match expr {
        Expr::Await { .. } => true,
        Expr::Call { callee, args, .. } => {
            // Check if callee is a known blocking function
            if let Expr::Identifier(name) = &callee.node {
                if BLOCKING_AST_CALLS.contains(&name.as_str()) {
                    return true;
                }
            }
            expr_has_await(&callee.node) || args.iter().any(|a| expr_has_await(&a.node.value.node))
        }
        Expr::MethodCall { receiver, args, .. } => {
            expr_has_await(&receiver.node) || args.iter().any(|a| expr_has_await(&a.node.value.node))
        }
        Expr::BinaryOp { left, right, .. } => {
            expr_has_await(&left.node) || expr_has_await(&right.node)
        }
        Expr::UnaryOp { operand, .. } => expr_has_await(&operand.node),
        Expr::Block(block) => block.stmts.iter().any(|s| stmt_has_await(&s.node)),
        Expr::If { condition, then_branch, else_branch, .. } => {
            expr_has_await(&condition.node)
            || expr_has_await(&then_branch.node)
            || else_branch.as_ref().map_or(false, |eb| expr_has_await(&eb.node))
        }
        Expr::Spawn { expr } => expr_has_await(&expr.node),
        Expr::TupleLiteral(elems) => elems.iter().any(|e| expr_has_await(&e.node)),
        Expr::Index { object, index, .. } => {
            expr_has_await(&object.node) || expr_has_await(&index.node)
        }
        Expr::FieldAccess { object, .. } => expr_has_await(&object.node),
        Expr::Closure { body, .. } => expr_has_await(&body.node),
        _ => false,
    }
}

/// Scan an AST statement for any `.await()` calls.
pub fn stmt_has_await(stmt: &crate::parser::ast::Stmt) -> bool {
    use crate::parser::ast::Stmt;
    match stmt {
        Stmt::Expr(e) => expr_has_await(&e.node),
        Stmt::VarDecl { value, .. } => expr_has_await(&value.node),
        Stmt::Assign { value, .. } => expr_has_await(&value.node),
        Stmt::CompoundAssign { value, .. } => expr_has_await(&value.node),
        Stmt::Return(Some(e)) => expr_has_await(&e.node),
        Stmt::If { condition, then_body, else_body, .. } => {
            expr_has_await(&condition.node)
            || then_body.stmts.iter().any(|s| stmt_has_await(&s.node))
            || else_body.as_ref().map_or(false, |eb| eb.stmts.iter().any(|s| stmt_has_await(&s.node)))
        }
        Stmt::While { condition, body, .. } => {
            expr_has_await(&condition.node)
            || body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        Stmt::For { iterable, body, .. } => {
            expr_has_await(&iterable.node)
            || body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        Stmt::Match { scrutinee, arms, .. } => {
            expr_has_await(&scrutinee.node)
            || arms.iter().any(|item| {
                if let Some(arm) = item.arm() {
                    expr_has_await(&arm.body.node)
                } else {
                    false
                }
            })
        }
        Stmt::With { body, .. } => {
            body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        _ => false,
    }
}
