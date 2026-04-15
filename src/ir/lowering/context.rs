use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::instructions::Operand;
use crate::ir::types::*;
use crate::parser::ast::{Expr, Ownership, PrimitiveType, Stmt, Type};
use crate::semantic::AnalysisResult;
use crate::span::Spanned;

use super::closures::ClosureLowering;
use super::drops::DropElaborator;
use super::types::TypeMapper;

use crate::ir::types::BlockId;

/// Unified ownership state for a GIR local variable.
/// Replaces the scattered `ref_locals`, `owned_locals`, `cow_alias_sources`,
/// Identity of a collection for CowBorrow provenance tracking.
/// Tracks which collection a borrowed element came from, so that
/// mutation of that collection triggers materialization of the borrow.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CollectionId {
    /// Direct named local variable (e.g., `entries` in `entries.get(i)`).
    Local(LocalId),
    /// Field access path (e.g., "self.data" in `self.data.get(i)`).
    FieldPath(String),
}

/// `cow_alias_targets`, `cow_ptr_params`, and `cow_collection_refs` maps.
#[derive(Debug, Clone, PartialEq)]
pub enum LocalOwnershipState {
    /// Owns its data. Registered for drop. Created by function calls, constructors,
    /// operators, or CoW materialization.
    Owned,
    /// CoW alias: Ptr(T) borrowing from source. Clone-on-mutation severs the alias.
    Alias { source: LocalId },
    /// Element reference: Ptr borrowed from a collection element (IndexLoad result).
    CollectionRef { collection: CollectionId },
    /// Bare Ptr param: borrows from caller. Clone-on-mutation to avoid modifying caller's data.
    BareParam,
    /// Generic Ptr reference: field loads, match pattern extracts, MutableBorrow params, etc.
    /// Not registered for drop. LIR uses SlotLoad instead of SlotAddr.
    Ref,
    /// CoW borrow: a zero-cost Ptr(T) borrow where cloning is deferred to
    /// ownership boundaries (struct init, push, return, move, mutation).
    /// Currently set by `.get().unwrap()` on collection elements. Future
    /// candidates: BareParam, CollectionRef, Alias could all unify under this.
    CowBorrow,
    /// String view: a cap=0 Str whose .data borrows from `source`'s buffer.
    /// Created by view-returning methods (slice, trim, char_at, etc.).
    /// Auto-materialized (cloned to owned) before source mutation.
    ViewOf { source: LocalId },
}

impl LocalOwnershipState {
    /// Whether this state represents a borrowed Ptr reference (not owned).
    pub fn is_ref(&self) -> bool {
        !matches!(self, LocalOwnershipState::Owned)
    }
}

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
    /// LocalId upper bound before loop body. Locals with id >= this value
    /// were created inside the loop body (fresh each iteration).
    pub pre_loop_local_count: u32,
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

/// Per-function transient state that resets between function boundaries.
///
/// Extracted from `LoweringContext` to prevent per-function state from leaking
/// across function boundaries during monomorphization. All fields reset to their
/// defaults between functions via `LoweringContext::clear_locals()`.
#[derive(Default)]
pub struct FunctionState {
    /// name → (LocalId, GIR TypeId) for variables in the current function.
    pub locals: FxHashMap<String, (LocalId, TypeId)>,
    /// Stack of active loops for break/continue targeting.
    pub loop_stack: Vec<LoopInfo>,
    /// LocalIds that are mutable capture pointers (need deref on read/write in closure bodies).
    /// Tracks `&` (MutableBorrow) and `!` (Move) struct params, which are MutPtr in GIR.
    pub mut_capture_locals: FxHashMap<LocalId, TypeId>,
    /// Unified ownership state for tracked locals. Replaces the former `ref_locals`,
    /// `owned_locals`, `cow_alias_sources`, `cow_alias_targets`, `cow_ptr_params`,
    /// and `cow_collection_refs` maps. Most locals are untracked (not in this map);
    /// only locals with ownership significance have entries.
    pub local_ownership: FxHashMap<LocalId, LocalOwnershipState>,
    /// LocalIds that are named variables (vs anonymous temps from expressions).
    /// Used to distinguish variable-to-variable assignment (needs clone) from
    /// temp-to-variable (needs move-zero).
    pub named_locals: FxHashSet<LocalId>,
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
    /// Name of the function currently being lowered (for tracking consumed params).
    pub current_fn_name: String,
    /// True when the current method has `!self` (consuming self). Field loads
    /// from self use MoveZeroSource for resource fields instead of Ptr borrows.
    pub consuming_self: bool,
    /// Maps temp locals from field_load → (source_field_place, field_type).
    /// Used by VarDecl/Assign to emit MoveZero after extracting resource-type fields.
    pub field_load_origins: FxHashMap<LocalId, (crate::ir::instructions::Place, TypeId)>,
    /// TupleInit element origins: tuple_local → Vec<element_local_ids>.
    /// Used by the return path to MoveZero element locals when returning a tuple.
    pub tuple_element_locals: FxHashMap<LocalId, Vec<LocalId>>,
    /// CoW borrow provenance: maps a CowBorrow local to the collection it
    /// borrows from. Propagated through .get() → Option → .unwrap() chain.
    /// Used by VarDecl to set CollectionRef with the correct source.
    pub cow_borrow_sources: FxHashMap<LocalId, CollectionId>,
    /// CoW: variable names that are reassigned in the current function body.
    /// Pre-scanned before lowering. Locals in this set skip CoW aliasing.
    pub cow_reassigned_names: rustc_hash::FxHashSet<String>,
    /// Flow-sensitive CoW: for each statement span.start, the set of names
    /// reassigned or !-moved on any forward path from that point.
    pub cow_reassigned_after: FxHashMap<usize, rustc_hash::FxHashSet<String>>,
    /// Phase 1f: name → use count in the function body. Names with count=1 are
    /// single-use (dead after their one use) → auto-move at push/constructor.
    pub name_use_counts: rustc_hash::FxHashMap<String, u32>,
    /// Full-function liveness analysis result. Contains span positions of
    /// identifier uses that are the last use on all reachable paths.
    pub liveness: super::liveness::LivenessResult,
    /// Locals from Move-argument lowering that need MoveZero AFTER the call.
    /// Populated by lower_call_arg when a Move param borrows a local, drained by
    /// the call lowering site after emitting the Call instruction.
    pub pending_move_zeros: Vec<LocalId>,
    /// Locals whose string heap data is a fresh allocation — not shared with any
    /// other variable. Set for function/extern call results returning the owned
    /// string type directly (gorget_str_cat, gorget_string_format, user functions).
    /// NOT set for struct inits, field loads, or pattern extracts (these may share
    /// string data with the source). Used by the return path to skip redundant clones.
    pub fresh_string_locals: rustc_hash::FxHashSet<LocalId>,
    /// Locals that have been borrowed-from via the string Borrow assignment
    /// path (`String b = a` unregisters `a`, shallow-copies to `b`).
    /// A local in this set has at least one other local sharing its heap data.
    /// Used by the return path: if the returned named local is NOT in this set,
    /// its string data is not shared → safe to move without cloning.
    pub string_borrow_sources: rustc_hash::FxHashSet<LocalId>,
    /// When true, pattern extraction of string fields skips cloning because
    /// the scrutinee is dead and BOTH the scrutinee copy AND the original
    /// variable will be MoveZeroed after extraction. Set by lower_match_stmt.
    pub scrutinee_clone_elision: bool,
    /// Set when a for-loop uses `index_load_borrow` for string elements.
    /// If false, return materialization can be skipped (no views to materialize).
    pub has_string_borrows: bool,
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
    /// Per-function transient state. Reset between function boundaries.
    pub func_state: FunctionState,
    /// Function signatures: name → (param GIR TypeIds, return GIR TypeId).
    pub fn_sigs: FxHashMap<String, (Vec<TypeId>, TypeId)>,
    /// Enum variant → (enum_type_name, variant_name) mapping.
    pub enum_variants: FxHashMap<String, (String, String)>,
    /// Struct field info: (type_name, field_name) → (field_index, field_type_id).
    pub struct_fields: FxHashMap<(String, String), (u32, TypeId)>,
    /// Closure info: struct_name → (call_fn_name, struct_type_id, by-value captures with field indices).
    /// Each capture entry is (name, type_id, struct_field_index).
    pub closure_info: FxHashMap<String, (String, TypeId, Vec<(String, TypeId, u32)>)>,
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
    /// Extern ABI marshalling kinds: fn_name → Vec<AbiKind>.
    /// Populated from FunctionDef.param_abis for Declaration-body functions.
    pub fn_extern_abi_kinds: FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
    /// Functions that are yield points (async or blocking qualifiers).
    pub yield_point_fns: rustc_hash::FxHashSet<String>,
    /// Per-function return ABI kind.
    pub fn_return_abis: rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
    /// Module-level global variable names (from StaticDecl items).
    /// Used by Expr::Identifier lowering to emit Constant::GlobalRef instead of I64(0).
    pub global_names: rustc_hash::FxHashSet<String>,
    /// Module-level global variable type names: var_name → AST type name (e.g. "AtomicInt").
    /// Used by infer_type_name_from_operand_full to dispatch methods on globals.
    pub global_type_names: FxHashMap<String, String>,
    /// Set of equip method names that are GIR-lowered (not extern/C-runtime).
    /// Used by lower_method_call to decide whether to pass resource-type args by pointer.
    pub gir_equip_methods: rustc_hash::FxHashSet<String>,
    /// Functions with FunctionBody::Extern — their call results are always owned.
    /// A C function cannot return a view into Gorget-managed memory.
    pub extern_body_fns: rustc_hash::FxHashSet<String>,
    /// Equip methods detected as trivial getters (body is `return self.field[idx]`
    /// or `return self.field`). These return Ptr(T) instead of cloning — the caller
    /// receives a CowBorrow with collection provenance.
    pub trivial_getter_methods: rustc_hash::FxHashSet<String>,
    /// Methods whose -1 sentinel return should be wrapped into Option[int].
    /// Populated during registration for stdlib collection/string `find`/`index_of` methods.
    /// User-defined methods default to NOT being in this set.
    pub sentinel_to_option_methods: rustc_hash::FxHashSet<String>,
    /// Accumulated implicit clone warnings during lowering.
    pub implicit_clone_warnings: Vec<crate::ir::ImplicitCloneWarning>,
    /// Suggestions to pass arguments with `!` (move) for last-use optimization.
    pub move_suggestions: Vec<crate::ir::MoveSuggestion>,
    /// Functions that clone a bare-param at a return/ownership boundary.
    /// Maps fn_name → set of param names that are cloned.
    /// Populated during callee lowering, queried at caller call sites.
    pub fn_consumed_params: FxHashMap<String, rustc_hash::FxHashSet<String>>,
    /// Maps monomorphized method name → C runtime function name.
    /// Populated from BuiltinTypeProtocol declarations during module setup.
    /// Used by the LIR backend to replace `map_monomorphized_to_runtime()`.
    pub runtime_callees: FxHashMap<String, String>,
    /// Maps callee span start → mangled function name for cross-module calls.
    /// Built from resolution_map + module_fn_manglings so that call lowering
    /// uses the correct target when multiple modules define the same bare name.
    pub call_resolved_names: FxHashMap<usize, String>,
}


impl<'a> LoweringContext<'a> {
    pub fn new(analysis: &'a AnalysisResult, type_mapper: TypeMapper, type_registry: TypeRegistry) -> Self {
        Self {
            analysis,
            type_mapper,
            type_registry,
            closures: ClosureLowering::new(),
            drops: DropElaborator::new(),
            func_state: FunctionState::default(),
            fn_sigs: FxHashMap::default(),
            enum_variants: FxHashMap::default(),
            struct_fields: FxHashMap::default(),
            closure_info: FxHashMap::default(),
            generics: GenericState::default(),
            spawn: SpawnState::default(),
            shared: SharedVarState::default(),
            module_constants: FxHashMap::default(),
            strip_asserts: false,
            snapshot_mode: false,
            overflow_wrap: false,
            extern_bindings: FxHashMap::default(),
            fn_defaults: FxHashMap::default(),
            fn_param_names: FxHashMap::default(),
            fn_param_ownerships: FxHashMap::default(),
            fn_param_abis: FxHashMap::default(),
            fn_extern_abi_kinds: FxHashMap::default(),
            yield_point_fns: rustc_hash::FxHashSet::default(),
            fn_return_abis: rustc_hash::FxHashMap::default(),
            global_names: rustc_hash::FxHashSet::default(),
            global_type_names: FxHashMap::default(),
            gir_equip_methods: rustc_hash::FxHashSet::default(),
            extern_body_fns: rustc_hash::FxHashSet::default(),
            trivial_getter_methods: rustc_hash::FxHashSet::default(),
            sentinel_to_option_methods: rustc_hash::FxHashSet::default(),
            implicit_clone_warnings: Vec::new(),
            move_suggestions: Vec::new(),
            fn_consumed_params: FxHashMap::default(),
            runtime_callees: FxHashMap::default(),
            call_resolved_names: FxHashMap::default(),
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
        // Handles both primitives (int64_t, bool, double) and named types (GorgetString, Point).
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
                "GorgetString" | "Str" => mapper.owned_string_type,
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

            // Resolve through Ptr — string borrow params are Ptr(GorgetString)
            // but method return types should use the base type.
            let resolved_self = self.pointee_type(*type_id).unwrap_or(*type_id);
            let type_args = BuiltinTypeArgs {
                elem,
                key,
                val,
                self_type: resolved_self,
                self_name: mangled_name.clone(),
            };

            let type_registry = &self.type_registry;
            let lookup_ctx = LookupCtx {
                lookup_type_by_name: &|name: &str| self.type_mapper.lookup_named(name),
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
                "GorgetString" | "Str" => self.type_mapper.owned_string_type,
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
            owned_string_type: self.type_mapper.owned_string_type,
            is_resource: &|tid| type_registry.is_resource_type(tid),
            ensure_option: &|name: &str, _inner: TypeId| {
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

    /// Check if a builtin method returns a view (cap=0 Str borrowing from receiver).
    pub fn builtin_returns_view(&self, type_name: &str, method_name: &str) -> bool {
        use crate::ir::lowering::builtins;
        if let Some(protocol) = builtins::protocol_for_mangled_name(type_name) {
            protocol.methods.iter()
                .find(|m| m.name == method_name)
                .map_or(false, |m| m.returns_view)
        } else {
            false
        }
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
        self.implicit_clone_warnings.push(crate::ir::ImplicitCloneWarning {
            span,
            type_name,
            reason,
        });
    }

    /// Record that the current function clones a bare-param at an ownership boundary.
    /// Called alongside warn_implicit_clone when the clone source is a Ptr(T) param.
    /// Used by call-site analysis to suggest `!arg` for last-use arguments.
    pub fn record_param_cloned(
        &mut self,
        builder: &crate::ir::builder::FunctionBuilder,
        local: LocalId,
    ) {
        if !self.is_bare_param(local) { return; }
        if let Some(name) = builder.local_name(local).map(|s| s.to_string()) {
            let fn_name = self.func_state.current_fn_name.clone();
            if !fn_name.is_empty() {
                self.fn_consumed_params
                    .entry(fn_name)
                    .or_default()
                    .insert(name);
            }
        }
    }

}

/// Convert an internal mangled type name to user-friendly Gorget syntax.
/// e.g., `Vector__int64_t` → `Vector[int]`, `Dict__GorgetString__int64_t` → `Dict[String, int]`
pub(crate) fn demangle_type_name(name: &str) -> String {
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
        self.func_state.locals.insert(name.to_string(), (local_id, type_id));
        self.func_state.named_locals.insert(local_id);
    }

    /// Phase 1f: check if a named variable is dead after the current statement.
    /// Dead variables can be auto-moved at push/constructor instead of cloned.
    /// Uses liveness analysis (reverse walk with branch union).
    pub fn is_single_use(&self, name: &str) -> bool {
        // Fallback for call sites that don't have span info.
        matches!(self.func_state.name_use_counts.get(name), Some(1))
    }

    /// Phase 1f: check if a specific use of a variable (at the given span) is
    /// the last use on all reachable execution paths. If yes, the value can be
    /// moved instead of cloned. Uses full-function liveness analysis.
    pub fn is_last_use_at(&self, _name: &str, span: crate::span::Span) -> bool {
        if self.func_state.liveness.last_use_spans.is_empty() {
            return false; // No liveness data → conservative (don't move)
        }
        self.func_state.liveness.last_use_spans.contains(&span.start)
    }

    /// Check if a local is a named variable (vs an anonymous temp).
    pub fn is_named_local(&self, local: LocalId) -> bool {
        self.func_state.named_locals.contains(&local)
    }

    /// Flow-sensitive CoW check: is `name` reassigned or !-moved on any forward
    /// path from the statement at `stmt_span_start`?
    pub fn is_cow_unsafe_at(&self, name: &str, stmt_span_start: usize) -> bool {
        self.func_state.cow_reassigned_after
            .get(&stmt_span_start)
            .map_or(false, |set| set.contains(name))
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
        // Function call results own their data — safe to Move on return.
        self.set_owned(local);
        // Mark as fresh for user-defined function calls (not in fn_sigs — these
        // have the return clone path ensuring independence) AND for builtin method
        // calls whose runtime callee provably allocates fresh buffers (replace,
        // upper, lower, repeat, pad, join, etc.).
        if return_type == self.type_mapper.owned_string_type {
            let is_user_fn = !self.fn_sigs.contains_key(func_name.as_str());
            let is_fresh_builtin = self.runtime_callees.get(func_name.as_str())
                .map_or(false, |rt| is_fresh_allocating_extern(rt));
            if is_user_fn || is_fresh_builtin {
                self.func_state.fresh_string_locals.insert(local);
            }
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
        let func_name: String = func.into();
        let local = builder.call_extern(&func_name, args, return_type);
        if self.type_registry.needs_drop(return_type) {
            self.drops.register_local(local, return_type, &self.type_registry);
        }
        self.set_owned(local);
        // Mark fresh for extern string functions that provably allocate new buffers.
        // Most runtime string functions return views (Str), but these return owned
        // GorgetString with independent heap data:
        if return_type == self.type_mapper.owned_string_type
            && is_fresh_allocating_extern(&func_name)
        {
            self.func_state.fresh_string_locals.insert(local);
        }
        local
    }

    /// Centralized ownership-aware enum init. Clones borrow-param and multi-use
    /// resource-type args before init, unregisters consumed args from drops.
    /// ALL enum_init call sites with non-empty args should use this.
    pub fn emit_enum_init_owned(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        enum_name: &str,
        variant_name: &str,
        type_id: TypeId,
        mut args: Vec<Operand>,
    ) -> LocalId {
        // Snapshot original locals before cloning — we need to know which
        // args were replaced by clones vs consumed directly.
        let originals: Vec<Option<LocalId>> = args.iter().map(|op| {
            if let Operand::Copy(place) = op {
                if place.projections.is_empty() { return Some(place.local); }
            }
            None
        }).collect();

        // Clone resource args that can't be moved into the enum variant.
        self.clone_resource_args_for_init(builder, &mut args, None);
        let dst = builder.enum_init(enum_name, variant_name, type_id, args.clone());
        self.set_owned(dst);

        // Transfer ownership: consumed args must not be double-freed at scope exit.
        // - Cloned args: the clone temp is consumed; unregister it. The original
        //   stays tracked — it was cloned, not consumed.
        // - Non-cloned args: consumed directly; unregister from drop tracking.
        //   The enum now owns the data.
        for (i, op) in args.iter().enumerate() {
            if let Operand::Copy(place) = op {
                if place.projections.is_empty() {
                    let was_cloned = originals.get(i)
                        .and_then(|o| *o)
                        .map_or(false, |orig| orig != place.local);
                    if was_cloned {
                        // Clone temp consumed by the enum — unregister it.
                        // The original local is NOT unregistered — it was
                        // cloned, not consumed, and still needs its scope-exit drop.
                        self.drops.unregister(place.local);
                    } else {
                        // Original local consumed directly — unregister.
                        self.drops.unregister(place.local);
                    }
                }
            }
        }
        dst
    }

    /// Clone resource-type args that can't be stored by move into a struct/enum.
    /// Shared by both struct literal init and enum variant init paths.
    /// Clones: Ptr(resource), non-owned string views, borrow param resources.
    pub fn clone_resource_args_for_init(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        args: &mut Vec<Operand>,
        span: Option<crate::span::Span>,
    ) {
        for op in args.iter_mut() {
            if let Operand::Copy(place) = op {
                if place.projections.is_empty() {
                    let local = place.local;
                    let local_type = builder.local_type(local);

                    // Ptr(resource) — always clone (borrows from someone else's storage)
                    if let Some(inner) = self.pointee_type(local_type) {
                        if self.type_registry.is_resource_type(inner) {
                            if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                                if let Some(s) = span {
                                    self.warn_implicit_clone(s, inner, crate::ir::ImplicitCloneReason::ConsumingArg);
                                }
                                let cloned = builder.call(&clone_fn,
                                    vec![crate::ir::builder::FunctionBuilder::copy(local)], inner);
                                self.drops.register_local(cloned, inner, &self.type_registry);
                                *op = crate::ir::builder::FunctionBuilder::copy(cloned);
                            }
                            continue;
                        }
                    }

                    if self.type_registry.is_resource_type(local_type) {
                        // Already owned — skip
                        if self.is_owned_local(local) && !self.is_named_local(local) {
                            continue;
                        }
                        // Non-owned string views — always clone
                        let is_non_owned_string = self.is_string_type(local_type)
                            && !self.is_owned_local(local);
                        // Borrow params — always clone
                        let is_borrow_param = matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::BareParam));
                        if is_non_owned_string || is_borrow_param {
                            if let Some(clone_fn) = self.clone_fn_for_ptr(local_type) {
                                if let Some(s) = span {
                                    self.warn_implicit_clone(s, local_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                                }
                                let ptr_type = self.register_ptr_type(local_type);
                                let ptr = builder.add_local(ptr_type, None);
                                builder.emit_borrow(ptr, crate::ir::instructions::Place::local(local));
                                let cloned = builder.call(&clone_fn,
                                    vec![crate::ir::builder::FunctionBuilder::copy(ptr)], local_type);
                                self.drops.register_local(cloned, local_type, &self.type_registry);
                                *op = crate::ir::builder::FunctionBuilder::copy(cloned);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Look up a variable by name.
    pub fn lookup_local(&self, name: &str) -> Option<(LocalId, TypeId)> {
        self.func_state.locals.get(name).copied()
    }

    /// Reset all per-function transient state for the next function.
    pub fn clear_locals(&mut self) {
        self.func_state = FunctionState::default();
        // Also reset per-function subfields on module-wide structs:
        self.spawn.result_locals.clear();
        self.spawn.pending_fn = None;
        self.shared.locals.clear();
    }

    /// Clone the locals map for save/restore around nested scopes (if, while, for, match, etc.).
    pub fn save_locals(&self) -> FxHashMap<String, (LocalId, TypeId)> {
        self.func_state.locals.clone()
    }

    /// Take the locals map, leaving it empty. Used for save/restore during async variant generation.
    pub fn take_locals(&mut self) -> FxHashMap<String, (LocalId, TypeId)> {
        std::mem::take(&mut self.func_state.locals)
    }

    /// Restore a previously saved locals map.
    pub fn restore_locals(&mut self, locals: FxHashMap<String, (LocalId, TypeId)>) {
        self.func_state.locals = locals;
    }

    /// Iterate over all locals (for type inference).
    pub fn locals_iter(&self) -> impl Iterator<Item = (&String, &(LocalId, TypeId))> {
        self.func_state.locals.iter()
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
            Expr::StringLiteral(_) => self.type_mapper.owned_string_type,
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
                        if self.func_state.current_throws_result_type.is_some() {
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

    /// Atomic MoveZero + mark_moved. Transfers ownership of a local: zeros the
    /// source and marks it as moved so scope-exit DropIfAlive is a no-op.
    pub fn move_zero_and_mark(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
    ) {
        builder.move_zero(crate::ir::instructions::Place::local(local));
        self.drops.mark_moved(local);
    }

    /// Check if a type is a string type, resolving through Ptr.
    pub fn is_string_type(&self, type_id: TypeId) -> bool {
        let resolved = self.pointee_type(type_id).unwrap_or(type_id);
        resolved == self.type_mapper.owned_string_type
            || resolved == self.type_mapper.owned_string_type
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
            if name == "GorgetString" {
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
                // Enums with cloneable variant payloads → generated {Name}__clone.
                // Includes Option/Result with resource payloads (e.g., Option[String]).
                if let crate::ir::types::TypeDefKind::Enum(ref edef) = type_def.kind {
                    let has_cloneable_payload = edef.variants.iter().any(|v| {
                        v.fields.iter().any(|f| self.type_registry.is_resource_type(f.type_id))
                    });
                    if has_cloneable_payload {
                        return Some(format!("{name}__clone"));
                    }
                }
            }
        }
        None
    }

    /// Ensure an operand is independently owned before crossing an ownership boundary
    /// (return, struct init, enum init, push, closure capture, Move param).
    ///
    /// Rule: clone if the local is any kind of borrow — Ptr(T), Ref/CowBorrow/
    /// CollectionRef/BareParam/Alias ownership state, a bare param, or a resource-
    /// type local that is NOT drop-tracked (for-loop string var borrowing from the
    /// outer collection, etc.). Owned drop-tracked locals and untracked non-resource
    /// locals (call result temps, primitives) are pass-through.
    ///
    /// Shape:
    ///   - Ptr(T) borrow  → clone through the pointer via `clone_fn_for_ptr(T)`
    ///   - by-value T     → clone the value via the same `clone_fn_for_ptr(T)`
    ///                      (string/array/map/set runtime clone fns accept a
    ///                      pointer to the value; C backend auto-addresses it)
    ///
    /// Returns the (possibly cloned) operand. Call sites should use the returned
    /// operand instead of the original.
    pub fn ensure_owned_at_boundary(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        span: crate::span::Span,
        reason: crate::ir::ImplicitCloneReason,
    ) -> Operand {
        let local = match &operand {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return operand,
        };
        let local_type = builder.local_type(local);

        // Case 1: Ptr(T) → clone inner.
        // Cannot move through Ptr: the callee doesn't know if the caller still
        // needs the argument. Record the param as consumed so the caller can
        // suggest `!` at last-use call sites.
        if let Some(inner) = self.pointee_type(local_type) {
            if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                self.record_param_cloned(builder, local);
                self.warn_implicit_clone(span, inner, reason);
                let cloned = builder.call(
                    &clone_fn,
                    vec![crate::ir::builder::FunctionBuilder::copy(local)],
                    inner,
                );
                self.drops.register_local(cloned, inner, &self.type_registry);
                self.set_owned(cloned);
                return crate::ir::builder::FunctionBuilder::copy(cloned);
            }
            return operand;
        }

        // Case 2: by-value resource type (GorgetString, GorgetArray, etc.)
        if !self.type_registry.is_resource_type(local_type) {
            return operand;
        }

        // Decide whether this local is a borrow that needs materializing.
        // - Tracked ref state (Ref, CowBorrow, CollectionRef, BareParam, Alias)
        // - Bare params (caller owns the data)
        //
        // Note: we intentionally DON'T treat "not drop-registered" as a proxy for
        // borrow. Several lowering paths emit correctly-independent locals without
        // explicit drop registration (LIR string assign auto-clones, `builder.call`
        // results for fresh allocating externs, etc.). Only explicit ref-state
        // flags represent aliasing relationships that require materialization.
        let ownership_is_ref = self.func_state.local_ownership.get(&local)
            .map_or(false, |s| s.is_ref());
        let is_borrow = ownership_is_ref
            || self.is_bare_param(local)
            || self.is_ref_local(local)
            || self.is_cow_borrow(local);
        if !is_borrow {
            return operand;
        }

        // Case 2b: last-use bare-param borrow → move instead of clone.
        // Only safe for bare params (not ref-locals or CoW borrows, which
        // may genuinely alias another live variable).
        if self.is_bare_param(local)
            && !self.is_ref_local(local)
            && !self.is_cow_borrow(local)
            && self.drops.is_registered(local)
        {
            let param_name = builder.local_name(local).map(|s| s.to_string());
            let is_last = param_name.as_ref()
                .map_or(false, |n| self.is_last_use_at(n, span));
            if is_last {
                self.drops.unregister(local);
                return operand;
            }
        }

        if let Some(clone_fn) = self.clone_fn_for_ptr(local_type) {
            self.warn_implicit_clone(span, local_type, reason);
            let cloned = builder.call(
                &clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(local)],
                local_type,
            );
            self.drops.register_local(cloned, local_type, &self.type_registry);
            self.set_owned(cloned);
            return crate::ir::builder::FunctionBuilder::copy(cloned);
        }
        operand
    }

    /// Clone an operand at a consuming-position boundary if the caller may still
    /// need the source after this site. Used at collection consuming-method args
    /// (push / put / set / etc.) and index-assign sugar (`v[i]=x`, `d[k]=v`).
    ///
    /// Rule:
    ///   1. Ptr(T) borrow → clone through the pointer (always, regardless of last-use).
    ///   2. By-value resource:
    ///      - Not an identifier expression → temp, treat as last-use, NO clone
    ///        (caller will MoveZero after the call).
    ///      - Not a named local → same (rare — fall back to temp path).
    ///      - A borrow (bare param, ref/cow state) → clone.
    ///      - Non-last-use named local → clone (source still live).
    ///      - Last-use drop-tracked owned named local → NO clone (caller MoveZeros).
    ///
    /// Returns the (possibly cloned) operand. If a clone was emitted, the cloned
    /// temp is drop-tracked and set as Owned so the caller can MoveZero it after
    /// the consuming call (the usual post-call clean-up path).
    pub fn ensure_owned_at_consuming_arg(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        arg_expr: &crate::span::Spanned<crate::parser::ast::Expr>,
        reason: crate::ir::ImplicitCloneReason,
    ) -> Operand {
        use crate::parser::ast::Expr;
        let local = match &operand {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return operand,
        };
        let arg_type = builder.local_type(local);

        // Case 1: Ptr(T) — always clone to materialize.
        if let Some(inner) = self.pointee_type(arg_type) {
            if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                self.warn_implicit_clone(arg_expr.span, inner, reason);
                let cloned = builder.call(
                    &clone_fn,
                    vec![crate::ir::builder::FunctionBuilder::copy(local)],
                    inner,
                );
                // Register for drops so mark_moved works in pre_call_clone_temps
                self.drops.register_local(cloned, inner, &self.type_registry);
                return crate::ir::builder::FunctionBuilder::copy(cloned);
            }
            return operand;
        }

        // Case 2: by-value resource.
        if !self.type_registry.is_resource_type(arg_type) {
            return operand;
        }
        // Determine if a clone is needed.  Two sub-cases:
        //   (a) Named identifier arg — check last-use + borrow state.
        //   (b) Non-identifier / non-named-local — expression temp, always
        //       last-use by construction (the temp was just created).
        let needs_clone = if let Expr::Identifier(ref name) = arg_expr.node {
            if self.is_named_local(local) {
                let is_borrow = !self.drops.is_registered(local)
                    || self.is_bare_param(local)
                    || self.is_ref_local(local)
                    || self.is_cow_borrow(local);
                is_borrow || !self.is_last_use_at(name, arg_expr.span)
            } else {
                // Identifier AST but local isn't "named" (e.g., intermediate
                // local from method-chain lowering).  Resolve from AST name
                // to check last-use on the original variable.
                let result = if let Some((src_local, _)) = self.lookup_local(name) {
                    let src_type = builder.local_type(src_local);
                    // Resolve through Ptr: the named variable may be Ptr(T)
                    // from .get().unwrap() while the lowered operand is the
                    // cloned T value.
                    let inner = self.pointee_type(src_type).unwrap_or(src_type);
                    if self.type_registry.is_resource_type(inner) {
                        let is_borrow = !self.drops.is_registered(src_local)
                            || self.is_bare_param(src_local)
                            || self.is_ref_local(src_local)
                            || self.is_cow_borrow(src_local);
                        is_borrow || !self.is_last_use_at(name, arg_expr.span)
                    } else { false }
                } else { false };
                result
            }
        } else {
            // Expression temp — always last-use, no clone needed.
            false
        };
        if !needs_clone { return operand; }
        if let Some(clone_fn) = self.clone_fn_for_ptr(arg_type) {
            self.warn_implicit_clone(arg_expr.span, arg_type, reason);
            let cloned = builder.call(
                &clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(local)],
                arg_type,
            );
            return crate::ir::builder::FunctionBuilder::copy(cloned);
        }
        operand
    }

    /// If an operand is Ptr(T), deep-clone it to produce an owned T.
    /// Used at Ptr→T boundaries: function args, enum constructors, collection push, etc.
    /// Returns the cloned operand (owned T), or the original if not Ptr.
    pub fn auto_clone_if_ptr(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        span: crate::span::Span,
    ) -> Operand {
        if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
            if place.projections.is_empty() {
                let local_type = builder.local_type(place.local);
                if let Some(inner) = self.pointee_type(local_type) {
                    // String Ptr params: read-through without clone.
                    // Strings are immutable — no clone needed on access.
                    // Cloning only at ownership boundaries (CoW materialization).
                    if self.type_mapper.is_string_type(inner) {
                        return operand;
                    }
                    if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                        self.warn_implicit_clone(span, inner, crate::ir::ImplicitCloneReason::CallArg);
                        let cloned = builder.call(
                            &clone_fn,
                            vec![crate::ir::builder::FunctionBuilder::copy(place.local)],
                            inner,
                        );
                        self.drops.register_local(cloned, inner, &self.type_registry);
                        self.set_owned(cloned);
                        return crate::ir::builder::FunctionBuilder::copy(cloned);
                    }
                }
            }
        }
        operand
    }

    // ── Unified ownership state helpers ──────────────────────────────

    /// Check if a local is tracked as a borrowed Ptr reference.
    pub fn is_ref_local(&self, local: LocalId) -> bool {
        self.func_state.local_ownership.get(&local).map_or(false, |s| s.is_ref())
    }

    /// Check if a local is tracked as definitely owning its data.
    pub fn is_owned_local(&self, local: LocalId) -> bool {
        matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::Owned))
    }

    /// Mark a local as owning its data. Overwrites any previous state.
    pub fn set_owned(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local, LocalOwnershipState::Owned);
    }

    /// Check if a local's string data is a fresh allocation not shared with any
    /// other variable. True only for direct function/extern call results that
    /// return the owned string type.
    pub fn is_fresh_string(&self, local: LocalId) -> bool {
        self.func_state.fresh_string_locals.contains(&local)
    }

    /// Check if a local has been borrowed-from via string Borrow assignment.
    /// If true, another local shares its heap data → clone needed on return.
    pub fn has_string_borrowers(&self, local: LocalId) -> bool {
        self.func_state.string_borrow_sources.contains(&local)
    }

    /// Mark a local as a generic Ptr reference. Only sets if not already tracked
    /// with a more specific state (BareParam, Alias, CollectionRef).
    pub fn set_ref(&mut self, local: LocalId) {
        self.func_state.local_ownership.entry(local).or_insert(LocalOwnershipState::Ref);
    }

    /// Check if a local is a bare Ptr param borrowing from the caller.
    pub fn is_bare_param(&self, local: LocalId) -> bool {
        matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::BareParam))
    }

    /// Mark a local as a bare Ptr param borrowing from the caller.
    pub fn set_bare_param(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local, LocalOwnershipState::BareParam);
    }

    /// Mark a local as a CoW borrow (deferred clone). Uses insert to
    /// override any prior state (e.g., Owned from call_extern_tracked).
    pub fn set_cow_borrow(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local, LocalOwnershipState::CowBorrow);
    }

    /// Check if a local is a CoW borrow (deferred clone).
    pub fn is_cow_borrow(&self, local: LocalId) -> bool {
        matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::CowBorrow))
    }

    /// Mark a local as a string view borrowing from `source`'s buffer.
    pub fn set_view_of(&mut self, local: LocalId, source: LocalId) {
        self.func_state.local_ownership.insert(local, LocalOwnershipState::ViewOf { source });
    }

    /// Find all locals that are views of `source`.
    pub fn views_of_source(&self, source: LocalId) -> Vec<LocalId> {
        self.func_state.local_ownership.iter()
            .filter_map(|(local, state)| {
                if matches!(state, LocalOwnershipState::ViewOf { source: s } if *s == source) {
                    Some(*local)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Record the source collection for a CowBorrow local.
    pub fn set_cow_borrow_source(&mut self, local: LocalId, collection: CollectionId) {
        self.func_state.cow_borrow_sources.insert(local, collection);
    }

    /// Look up the source collection for a CowBorrow local.
    pub fn cow_borrow_source(&self, local: LocalId) -> Option<&CollectionId> {
        self.func_state.cow_borrow_sources.get(&local)
    }

    /// Mark a local as a collection element reference.
    pub fn set_collection_ref(&mut self, local: LocalId, collection: CollectionId) {
        self.func_state.local_ownership.insert(local, LocalOwnershipState::CollectionRef { collection });
    }

    /// Derive the set of ref locals for GIR function output.
    /// Collects all locals with any ref state (Ref, BareParam, Alias, CollectionRef).
    /// Flush ownership state from the lowering side map onto the builder's Local structs.
    /// Called after lowering a function body, before `builder.build()`.
    pub fn flush_ownership_to_locals(&self, builder: &mut crate::ir::builder::FunctionBuilder) {
        for (&local_id, state) in &self.func_state.local_ownership {
            let idx = local_id.0 as usize;
            if idx < builder.locals.len() {
                builder.locals[idx].ownership = match state {
                    LocalOwnershipState::Owned => crate::ir::OwnershipState::Owned,
                    // Borrows from caller or field loads — never dropped
                    LocalOwnershipState::BareParam | LocalOwnershipState::Ref => {
                        crate::ir::OwnershipState::Ref
                    }
                    // CoW borrows that may have been materialized on some paths
                    LocalOwnershipState::Alias { .. }
                    | LocalOwnershipState::CollectionRef { .. }
                    | LocalOwnershipState::CowBorrow
                    | LocalOwnershipState::ViewOf { .. } => {
                        crate::ir::OwnershipState::MaybeBorrowed
                    }
                };
            }
        }
    }

    // ── Copy-on-Write alias management ────────────────────────────────

    /// Register a CoW alias: `alias_local` is a Ptr(T) borrowing from `source_local`.
    /// Resolves transitively: if source is itself an alias, points to the root.
    pub fn cow_register_alias(&mut self, alias_local: LocalId, source_local: LocalId) {
        let root = self.cow_resolve_root(source_local);
        self.func_state.local_ownership.insert(alias_local, LocalOwnershipState::Alias { source: root });
    }

    /// Resolve a local to its root source (follow alias chain).
    fn cow_resolve_root(&self, local: LocalId) -> LocalId {
        let mut current = local;
        while let Some(LocalOwnershipState::Alias { source }) = self.func_state.local_ownership.get(&current) {
            if *source == current { break; }
            current = *source;
        }
        current
    }

    /// Check if a local is a CoW alias of something else.
    pub fn cow_is_alias(&self, local: LocalId) -> bool {
        matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::Alias { .. }))
    }

    /// Check if a local has CoW aliases pointing to it (is a source).
    pub fn cow_has_aliases(&self, local: LocalId) -> bool {
        self.func_state.local_ownership.values().any(|s| matches!(s, LocalOwnershipState::Alias { source } if *source == local))
    }

    /// Collect all aliases pointing to `source`. Derived query — O(n) scan.
    fn cow_aliases_of(&self, source: LocalId) -> Vec<LocalId> {
        self.func_state.local_ownership.iter()
            .filter_map(|(&id, s)| match s {
                LocalOwnershipState::Alias { source: s } if *s == source => Some(id),
                _ => None,
            })
            .collect()
    }

    /// Check if a collection has any element refs pointing into it.
    pub fn cow_has_collection_refs(&self, collection: LocalId) -> bool {
        let target = CollectionId::Local(collection);
        self.func_state.local_ownership.values().any(|s| matches!(s, LocalOwnershipState::CollectionRef { collection: c } if *c == target))
    }

    /// Collect all collection refs pointing to a `CollectionId`. Derived query — O(n) scan.
    fn cow_collection_refs_for_id(&self, target: &CollectionId) -> Vec<LocalId> {
        self.func_state.local_ownership.iter()
            .filter_map(|(&id, s)| match s {
                LocalOwnershipState::CollectionRef { collection: c } if c == target => Some(id),
                _ => None,
            })
            .collect()
    }

    /// Collect all collection refs pointing to a direct local.
    pub fn cow_collection_refs_for(&self, collection: LocalId) -> Vec<LocalId> {
        self.cow_collection_refs_for_id(&CollectionId::Local(collection))
    }

    /// Before mutating `local`, sever all CoW alias relationships:
    /// - If `local` is a BareParam → clone to owned before mutation.
    /// - If `local` is an alias → clone source into local (local becomes owned).
    /// - If `local` is a source → clone into each alias (aliases become owned).
    /// - If `local` is a collection with refs → clone each ref out.
    pub fn cow_before_mutation(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
        span: crate::span::Span,
    ) {
        // Phase 1c: bare Ptr params — clone to owned before mutation
        if matches!(self.func_state.local_ownership.get(&local), Some(LocalOwnershipState::BareParam)) {
            self.func_state.local_ownership.remove(&local);
            self.cow_materialize_alias(builder, local, local, span);
        }

        // Case 1: local is an alias of something else → clone source into local
        if let Some(LocalOwnershipState::Alias { source }) = self.func_state.local_ownership.get(&local).cloned() {
            self.func_state.local_ownership.remove(&local);
            self.cow_materialize_alias(builder, local, source, span);
        }

        // Case 2: local is a source with aliases → clone into each alias
        let aliases = self.cow_aliases_of(local);
        if !aliases.is_empty() {
            for alias in aliases {
                self.func_state.local_ownership.remove(&alias);
                self.cow_materialize_alias(builder, alias, local, span);
            }
        }

        // Case 3: local is a collection with refs into it → clone each ref
        let refs = self.cow_collection_refs_for(local);
        if !refs.is_empty() {
            for ref_local in refs {
                // Only sever if the ref is still live (not already moved/reassigned)
                if self.is_ref_local(ref_local) {
                    self.cow_materialize_collection_ref(builder, ref_local, span);
                }
            }
        }

        // Case 4: local is a string with live views → materialize each view
        // before the source is mutated (push/append/clear/reassign).
        let views = self.views_of_source(local);
        for view_local in views {
            self.func_state.local_ownership.remove(&view_local);
            self.cow_materialize_view(builder, view_local, span);
        }
    }

    /// Before mutating a field-accessed collection (e.g., `self.data.push(x)`),
    /// materialize all CollectionRefs that borrow from that field path.
    pub fn cow_before_field_mutation(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        field_path: &str,
        span: crate::span::Span,
    ) {
        let target = CollectionId::FieldPath(field_path.to_string());
        let refs = self.cow_collection_refs_for_id(&target);
        for ref_local in refs {
            if self.is_ref_local(ref_local) {
                self.cow_materialize_collection_ref(builder, ref_local, span);
            }
        }
    }

    /// Sever all aliases that point to `source_local` as their root.
    /// Used when `source_local` is about to be reassigned (aliases keep old value).
    pub fn cow_sever_all_aliases_from(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        source_local: LocalId,
        span: crate::span::Span,
    ) {
        let aliases = self.cow_aliases_of(source_local);
        for alias in aliases {
            self.func_state.local_ownership.remove(&alias);
            self.cow_materialize_alias(builder, alias, source_local, span);
        }
        // Clean up other CoW tracking for the reassigned source — it's about
        // to get a new value, so stale entries would cause incorrect clones.
        if matches!(self.func_state.local_ownership.get(&source_local), Some(LocalOwnershipState::BareParam)) {
            self.func_state.local_ownership.remove(&source_local);
        }
        // Remove collection refs pointing to this source
        let refs = self.cow_collection_refs_for(source_local);
        for r in refs {
            self.func_state.local_ownership.remove(&r);
        }
        // Materialize string views borrowing from this source
        let views = self.views_of_source(source_local);
        for view_local in views {
            self.func_state.local_ownership.remove(&view_local);
            self.cow_materialize_view(builder, view_local, span);
        }
    }

    /// Materialize an alias: clone the source's data into the alias local.
    /// Changes the alias from Ptr(T) to owned T, registers for drop.
    /// Materialize a string view: clone the view into an owned Str so the
    /// source can be safely mutated. The view local gets a fresh owned buffer.
    fn cow_materialize_view(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        view_local: LocalId,
        span: crate::span::Span,
    ) {
        // First, materialize any transitive views (views of THIS view)
        let sub_views = self.views_of_source(view_local);
        for sub in sub_views {
            self.func_state.local_ownership.remove(&sub);
            self.cow_materialize_view(builder, sub, span);
        }

        let view_type = builder.local_type(view_local);
        if let Some(clone_fn) = self.clone_fn_for_ptr(view_type) {
            self.warn_implicit_clone(span, view_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(view_local)], view_type);
            let name_hint = builder.local_name(view_local).map(|s| s.to_string());
            let owned_local = builder.add_local(view_type, name_hint.as_deref());
            builder.assign(crate::ir::instructions::Place::local(owned_local),
                          crate::ir::builder::FunctionBuilder::copy(cloned));
            self.drops.register_local(owned_local, view_type, &self.type_registry);
            self.set_owned(owned_local);
            if let Some(ref hint) = builder.local_name(view_local).map(|s| s.to_string()) {
                let name = hint.clone();
                self.register_local(&name, owned_local, view_type);
                self.func_state.named_locals.insert(owned_local);
            }
        }
    }

    fn cow_materialize_alias(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        alias_local: LocalId,
        _source_local: LocalId,
        span: crate::span::Span,
    ) {
        let alias_type = builder.local_type(alias_local);
        let inner_type = match self.pointee_type(alias_type) {
            Some(inner) => inner,
            None => return,
        };
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            self.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(alias_local)], inner_type);
            let name_hint = builder.local_name(alias_local).map(|s| s.to_string());
            let owned_local = builder.add_local(inner_type,
                name_hint.as_deref());
            builder.assign(crate::ir::instructions::Place::local(owned_local),
                          crate::ir::builder::FunctionBuilder::copy(cloned));
            self.drops.register_local(owned_local, inner_type, &self.type_registry);
            self.set_owned(owned_local);
            if let Some(ref hint) = builder.local_name(alias_local).map(|s| s.to_string()) {
                let name = hint.clone();
                self.register_local(&name, owned_local, inner_type);
                self.func_state.named_locals.insert(owned_local);
            }
        }
    }

    /// Materialize a collection ref: clone the pointed-to element into an owned local.
    pub fn cow_materialize_collection_ref(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        ref_local: LocalId,
        span: crate::span::Span,
    ) {
        let ref_type = builder.local_type(ref_local);
        let inner_type = self.pointee_type(ref_type).unwrap_or(ref_type);
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            self.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(ref_local)], inner_type);
            let name_hint = builder.local_name(ref_local).map(|s| s.to_string());
            let owned_local = builder.add_local(inner_type,
                name_hint.as_deref());
            builder.assign(crate::ir::instructions::Place::local(owned_local),
                          crate::ir::builder::FunctionBuilder::copy(cloned));
            self.drops.register_local(owned_local, inner_type, &self.type_registry);
            self.set_owned(owned_local);
            if let Some(ref hint) = builder.local_name(ref_local).map(|s| s.to_string()) {
                let name = hint.clone();
                self.register_local(&name, owned_local, inner_type);
                self.func_state.named_locals.insert(owned_local);
            }
            // The old ref_local is now dead
            self.func_state.local_ownership.remove(&ref_local);
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
        if let Some((field_place, _)) = self.func_state.field_load_origins.remove(&local) {
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
    /// `pre_loop_local_count` is `builder.locals.len() as u32` at loop entry —
    /// locals with id >= this are loop-body locals (fresh each iteration).
    pub fn push_loop(&mut self, header_bb: BlockId, exit_bb: BlockId, pre_loop_local_count: u32) {
        self.func_state.loop_stack.push(LoopInfo { header_bb, exit_bb, pre_loop_local_count });
    }

    /// Check if a local was created inside the current innermost loop body.
    pub fn is_loop_body_local(&self, local: LocalId) -> bool {
        self.current_loop()
            .map_or(false, |info| local.0 >= info.pre_loop_local_count)
    }

    /// Pop the current loop off the stack.
    pub fn pop_loop(&mut self) {
        self.func_state.loop_stack.pop();
    }

    /// Get the current (innermost) loop info for break/continue.
    pub fn current_loop(&self) -> Option<&LoopInfo> {
        self.func_state.loop_stack.last()
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
    /// If the inner type is droppable, immediately upgrades to Resource+Recursive
    /// so that drop elaboration sees the correct semantics during lowering.
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

#[allow(dead_code)]
/// Count how many times a variable name is used within a single statement.
/// Used by is_single_use to prevent MoveZero of variables used more than once
/// in the same expression (e.g., `v.push(v.len())` uses `v` twice).
fn count_name_uses_in_stmt(stmt: &Stmt, name: &str) -> u32 {
    let mut count = 0u32;
    match stmt {
        Stmt::VarDecl { value, .. } => count_name_in_expr(&value.node, name, &mut count),
        Stmt::Assign { target, value, .. } => {
            count_name_in_expr(&target.node, name, &mut count);
            count_name_in_expr(&value.node, name, &mut count);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            count_name_in_expr(&target.node, name, &mut count);
            count_name_in_expr(&value.node, name, &mut count);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) => {
            count_name_in_expr(&expr.node, name, &mut count);
        }
        _ => {}
    }
    count
}

#[allow(dead_code)]
fn count_name_in_expr(expr: &Expr, name: &str, count: &mut u32) {
    match expr {
        Expr::Identifier(n) if n == name => { *count += 1; }
        Expr::Call { callee, args, .. } => {
            count_name_in_expr(&callee.node, name, count);
            for a in args { count_name_in_expr(&a.node.value.node, name, count); }
        }
        Expr::MethodCall { receiver, args, .. } => {
            count_name_in_expr(&receiver.node, name, count);
            for a in args { count_name_in_expr(&a.node.value.node, name, count); }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            count_name_in_expr(&object.node, name, count);
        }
        Expr::Index { object, index, .. } => {
            count_name_in_expr(&object.node, name, count);
            count_name_in_expr(&index.node, name, count);
        }
        Expr::BinaryOp { left, right, .. } => {
            count_name_in_expr(&left.node, name, count);
            count_name_in_expr(&right.node, name, count);
        }
        Expr::UnaryOp { operand, .. } | Expr::Move { expr: operand } => {
            count_name_in_expr(&operand.node, name, count);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            count_name_in_expr(&condition.node, name, count);
            count_name_in_expr(&then_branch.node, name, count);
            for (c, b) in elif_branches { count_name_in_expr(&c.node, name, count); count_name_in_expr(&b.node, name, count); }
            if let Some(b) = else_branch { count_name_in_expr(&b.node, name, count); }
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            count_name_in_expr(&body.node, name, count);
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            count_name_in_expr(&scrutinee.node, name, count);
            for arm in arms { count_name_in_expr(&arm.body.node, name, count); }
            if let Some(b) = else_arm { count_name_in_expr(&b.node, name, count); }
        }
        _ => {}
    }
}

/// Extern string functions that provably allocate fresh buffers.
/// Their results are independent of any input — safe to skip the
/// self-referential reassignment clone guard.
fn is_fresh_allocating_extern(name: &str) -> bool {
    matches!(name,
        "gorget_str_to_upper" | "gorget_str_to_lower"
        | "gorget_str_replace" | "gorget_str_repeat"
        | "gorget_str_pad_left" | "gorget_str_pad_right"
        | "gorget_str_join"
        | "gorget_str_cat"
        | "gorget_string_format"
        | "gorget_int_to_str" | "gorget_float_to_str" | "gorget_bool_to_str"
    )
}
