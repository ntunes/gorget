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
    /// Generic type parameter → full substituted AST type. Complements
    /// `generic_type_params` (which stores GIR TypeIds) for cases where we
    /// need the AST shape: closure-typed params (`F f` with F → `int(int)`)
    /// can only be recognised as Callable/Function from the AST, since the
    /// immutable TypeMapper collapses Function types to UNIT_TYPE at the GIR
    /// boundary.
    pub generic_param_ast_types: FxHashMap<String, crate::parser::ast::Type>,
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
    /// Unified ownership state for tracked locals. Replaces the former `ref_locals`,
    /// `owned_locals`, `cow_alias_sources`, `cow_alias_targets`, `cow_ptr_params`,
    /// `cow_collection_refs`, and the legacy 7-variant LocalOwnershipState
    /// map (deleted in Phase D3-full). Most locals are untracked (not in
    /// this map); only locals with ownership significance have entries.
    /// See `docs/internals/unified-resource-model.md` §6.
    pub local_ownership: FxHashMap<LocalId, crate::ir::LocalOwnership>,
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
    /// Locals corresponding to params upgraded from Borrow to Move in generic
    /// functions that return them directly. The return path zeroes the source
    /// through the pointer to prevent caller double-free.
    pub move_override_params: rustc_hash::FxHashSet<LocalId>,
    /// Name of the function currently being lowered (for tracking consumed params).
    pub current_fn_name: String,
    /// True when the current method has `!self` (consuming self). Field loads
    /// from self use MoveZeroSource for resource fields instead of Ptr borrows.
    pub consuming_self: bool,
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
    ///
    /// **Phase D4 retirement attempt (2026-05-04, reverted).** The doc maps this
    /// sidecar to `LocalOwnershipState::ViewOf { source }`. Probe outcome:
    /// genuine gating — ViewOf flushes to `OwnershipState::MaybeBorrowed`,
    /// which the LIR backend's `lower_place_addr` treats as a Ptr ABI
    /// (`SlotLoad → void*` instead of `SlotAddr → Str*`). Tagging Branch A's
    /// value-type LHS (a 32-byte GorgetString slot holding a shallow copy of
    /// the source's `{data, cap, len, alloc}`) as ViewOf produces a slot/local
    /// type mismatch in C codegen ("incompatible types when assigning to type
    /// 'void *' from type 'Str'"). The structural difference: ViewOf models
    /// cap=0 byte-slice views (a Str whose data field points into another
    /// buffer), whereas this sidecar tracks value-aliasing — a full struct
    /// copy that shares the heap region with the source. Both answer "if I
    /// return X, must I clone?" but model different invariants. Retirement
    /// requires either: (a) `flush_ownership_to_locals` leaving ViewOf as
    /// Owned for value-typed Str locals, or (b) a separate
    /// `LocalOwnershipState::SharedHeap { other }` variant that flushes to
    /// Owned but propagates the same return-path signal. See TODO entry on
    /// CoW materialization for related work.
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

/// Snapshot of lowering state taken at branch entry, restored at branch exit.
/// Carries BOTH the name→local map and local_ownership so that CoW materialization
/// that runs inside one branch (rebinding a name, removing an ownership flag)
/// does not leak into sibling branches or post-join code.
///
/// `local_id_boundary`: any local whose ID is ≥ this was created after the snapshot
/// — its ownership state is kept as-is on restore (branch-local locals survive).
///
/// `local_types_at_save`: map of LocalId → declared type at save time. On restore,
/// if a local's `builder.locals[i].type_id` has been CHANGED during the scope
/// (e.g. `assigns.rs`'s in-place CoW upgrade flipping Ptr(T)→T), that local is
/// treated as permanently upgraded — its ownership state is *not* reverted.
/// This prevents inconsistent (ownership=CollectionRef, type=T) states that
/// break LIR codegen.
#[derive(Clone)]
pub struct SavedScope {
    locals: FxHashMap<String, (LocalId, TypeId)>,
    local_ownership: FxHashMap<LocalId, crate::ir::LocalOwnership>,
    local_id_boundary: u32,
    local_types_at_save: FxHashMap<LocalId, TypeId>,
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
                // Apply BOTH internal and trailing occurrences in the same pass —
                // a name like `last_iter__T__VectorIter__T` has T in both positions
                // and an if/else between them leaves one behind.
                let pattern_mid = format!("__{param}__");
                if result.contains(&pattern_mid) {
                    result = result.replace(&pattern_mid, &format!("__{concrete}__"));
                    changed = true;
                }
                let pattern_end = format!("__{param}");
                if result.ends_with(&pattern_end) {
                    let prefix = &result[..result.len() - pattern_end.len()];
                    result = format!("{prefix}__{concrete}");
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

    /// Phase D4: First-class liveness query for the lower_var_decl typed
    /// match. Returns true if the source operand's underlying local is
    /// referenced AFTER `stmt_span`. For unnamed temps, returns false
    /// (temps die at their last use, which is the current statement).
    /// For non-place operands (constants), returns false. For named
    /// locals, returns `!is_last_use_at(name, stmt_span)` — conservative
    /// when liveness data is missing.
    ///
    /// Used by `lower_var_decl` to decide between Move (source dead) and
    /// Borrow / Clone (source alive). Without this query, the function
    /// fell back to four sidecar predicates (named_local, cow_unsafe_at,
    /// drops.is_registered, needs_drop) that proxied liveness imprecisely.
    pub fn source_live_past(
        &self,
        operand: &crate::ir::instructions::Operand,
        stmt_span: crate::span::Span,
        builder: &crate::ir::builder::FunctionBuilder,
    ) -> bool {
        use crate::ir::instructions::Operand;
        let place = match operand {
            Operand::Copy(p) | Operand::Move(p) => p,
            _ => return false,
        };
        if !place.projections.is_empty() {
            return false; // Field/index access — conservative.
        }
        let local_idx = place.local.0 as usize;
        if local_idx >= builder.locals.len() { return false; }
        // Named locals: use liveness data via name lookup. Unnamed temps:
        // not in liveness map → live=false (single-use by SSA-like
        // construction).
        if let Some(name) = builder.locals[local_idx].name_hint.as_deref() {
            if self.func_state.named_locals.contains(&place.local) {
                return !self.is_last_use_at(name, stmt_span);
            }
        }
        false
    }

    /// Phase D4: returns the source operand's local ownership state, or
    /// None for constants / non-place operands. The companion to
    /// `source_live_past` for the typed match in `lower_var_decl`.
    pub fn source_ownership(
        &self,
        operand: &crate::ir::instructions::Operand,
        builder: &crate::ir::builder::FunctionBuilder,
    ) -> Option<crate::ir::LocalOwnership> {
        use crate::ir::instructions::Operand;
        let place = match operand {
            Operand::Copy(p) | Operand::Move(p) => p,
            _ => return None,
        };
        if !place.projections.is_empty() { return None; }
        let local_idx = place.local.0 as usize;
        if local_idx >= builder.locals.len() { return None; }
        // Prefer the live func_state map (lowering-time canonical state)
        // over builder.locals[].ownership which only reflects flushed
        // post-lowering state.
        if let Some(o) = self.func_state.local_ownership.get(&place.local) {
            return Some(o.clone());
        }
        Some(builder.locals[local_idx].ownership.clone())
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

    /// Flow-sensitive source-mutation check: is the collection at `source_path`
    /// (either a local name like `"x"` or a field path like `"self.data"`)
    /// mutated on any forward path from `stmt_span_start`? Used to decide at
    /// var_decl / CoW-borrow sites whether a borrow of an element from this
    /// collection is safe to keep, or must be eagerly materialized.
    ///
    /// Treats ANY reassignment/`!`-move of an ancestor path as a mutation — e.g.
    /// `self.data = new_vec` invalidates borrows of `self.data.get(i)`, and
    /// `self = other` invalidates borrows of `self.data.get(i)` too. A bare
    /// collection name is safe ONLY if neither the name itself nor any of its
    /// prefixes (which don't exist for bare locals) is reassigned later.
    pub fn is_source_mut_unsafe_at(&self, source_path: &str, stmt_span_start: usize) -> bool {
        let set = match self.func_state.cow_reassigned_after.get(&stmt_span_start) {
            Some(s) => s,
            None => return false,
        };
        // Direct mutation marker for the exact path.
        if set.contains(&format!("@mut:{}", source_path)) {
            return true;
        }
        // Name reassignment for a bare local path or the root of a field path
        // (e.g. `x = ...` invalidates borrows of `x.foo`; `self = ...` invalidates
        //  borrows of `self.data`).
        let root = source_path.split('.').next().unwrap_or(source_path);
        if set.contains(root) {
            return true;
        }
        // Ancestor mutations invalidate the borrow: `helper(&self)` (records
        // `@mut:self`) must invalidate borrows of `self.data`; `self.data = new`
        // (records `@mut:self.data`) must invalidate borrows of `self.data.items`.
        // Walk every prefix of the path and check for @mut:{prefix}.
        let parts: Vec<&str> = source_path.split('.').collect();
        let mut prefix = String::new();
        for (i, part) in parts.iter().enumerate() {
            if i == 0 {
                prefix.push_str(part);
            } else {
                prefix.push('.');
                prefix.push_str(part);
            }
            // Skip the full path — already checked above.
            if i == parts.len() - 1 {
                break;
            }
            if set.contains(&format!("@mut:{}", prefix)) {
                return true;
            }
        }
        false
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
                .map_or(false, |rt| runtime_returns_fresh(rt));
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
        // GorgetString with independent heap data. Driven by the typed
        // `RuntimeSig.returns_fresh` flag — see `runtime_returns_fresh` below.
        if return_type == self.type_mapper.owned_string_type
            && runtime_returns_fresh(&func_name)
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
                        let is_borrow_param = self.is_bare_param(local);
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

    /// Clone the name→local map AND local_ownership for save/restore around nested
    /// scopes (if, while, for, match, etc.). See `SavedScope` for semantics.
    pub fn save_locals(&self, builder: &crate::ir::builder::FunctionBuilder) -> SavedScope {
        // Snapshot per-local declared types so restore can detect in-place type
        // flips (e.g. assigns.rs's CoW upgrade Ptr(T)→T) and skip reverting
        // ownership for those locals.
        let local_types_at_save: FxHashMap<LocalId, TypeId> = self.func_state.local_ownership
            .keys()
            .chain(self.func_state.locals.values().map(|(l, _)| l))
            .copied()
            .map(|lid| (lid, builder.local_type(lid)))
            .collect();
        SavedScope {
            locals: self.func_state.locals.clone(),
            local_ownership: self.func_state.local_ownership.clone(),
            local_id_boundary: builder.locals.len() as u32,
            local_types_at_save,
        }
    }

    /// Take the locals map, leaving it empty. Used for save/restore during async variant generation.
    pub fn take_locals(&mut self) -> FxHashMap<String, (LocalId, TypeId)> {
        std::mem::take(&mut self.func_state.locals)
    }

    /// Restore a previously saved scope: name→local bindings come back fully;
    /// ownership state is restored for pre-existing locals whose declared type
    /// hasn't changed, and preserved for locals whose type was upgraded in the
    /// branch (e.g. Ptr(T)→T CoW upgrade) or locals created after save.
    ///
    /// *Targeted fix*: for locals created inside the scope (lid ≥ boundary),
    /// clear any `CollectionRef` or `CowBorrow` ownership state — those states
    /// register the local with `cow_before_field_mutation` / `cow_before_mutation`,
    /// and a later mutation of the (still-live) source collection would re-
    /// materialise the now-out-of-scope local, reading dead slot memory.
    /// Other ownership states (Owned, Ref, Alias, BareParam, ViewOf) are kept
    /// — they're either pure metadata or reference aliasing that's already
    /// severed by runtime CoW on mutation of the aliased source.
    pub fn restore_locals(&mut self, builder: &crate::ir::builder::FunctionBuilder, saved: SavedScope) {
        self.func_state.locals = saved.locals;
        let boundary = saved.local_id_boundary;
        let saved_types = &saved.local_types_at_save;
        let mut restored = saved.local_ownership.clone();
        for (lid, state) in &self.func_state.local_ownership {
            let post_save = lid.0 >= boundary;
            let type_flipped = saved_types.get(lid)
                .map_or(false, |orig| *orig != builder.local_type(*lid));
            if post_save {
                // Branch-local collection-element / view borrows: drop at
                // scope exit so cow_before_field_mutation doesn't issue a
                // materialise-read on a dead slot. (Equivalent to the
                // pre-D3-full filter that dropped CollectionRef/CowBorrow.)
                let keep = !matches!(state,
                    crate::ir::LocalOwnership::Borrowed {
                        origin: crate::ir::BorrowOrigin::CollectionElement(_)
                              | crate::ir::BorrowOrigin::FieldPath(_)
                              | crate::ir::BorrowOrigin::CowBorrowPending,
                        ..
                    } | crate::ir::LocalOwnership::View { .. }
                );
                if keep {
                    restored.insert(*lid, state.clone());
                }
            } else if type_flipped {
                restored.insert(*lid, state.clone());
            }
        }
        let flipped: Vec<LocalId> = saved_types.iter()
            .filter(|(lid, orig)| lid.0 < boundary && **orig != builder.local_type(**lid))
            .filter(|(lid, _)| !self.func_state.local_ownership.contains_key(lid))
            .map(|(lid, _)| *lid)
            .collect();
        for lid in flipped {
            restored.remove(&lid);
        }
        self.func_state.local_ownership = restored;
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
            Expr::StringLiteral(_, _) => self.type_mapper.owned_string_type,
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
    /// Returns None for trivial types.
    pub fn clone_fn_for_ptr(&self, inner_type: TypeId) -> Option<String> {
        use crate::ir::types::{GirType, DropStrategy};
        if let Some(GirType::Named(name)) = self.type_registry.get(inner_type) {
            // Metadata-based: clone_fn populated at registration from
            // BuiltinTypeProtocol (or hand-set for the runtime-named singletons
            // GorgetString / GorgetArray / GorgetMap / GorgetSet). Covers
            // every Vector/Deque/Dict/HashMap/Set/HashSet instantiation +
            // GorgetString and the runtime-named collection types.
            if let Some(type_def) = self.type_registry.get_type_def(name) {
                if let Some(ref clone_fn) = type_def.metadata.clone_fn {
                    return Some(clone_fn.clone());
                }
            }

            // Callable/MutCallable/ConsumeCallable/GorgetClosure don't have
            // TypeDef registration today (Callable values lower to GirType::FnPtr
            // at locals; the Named form only appears via collection-elem
            // resolve_inner_type fallback). Closures own a heap-alloc'd env via
            // __gorget_closure_env_alloc; `.clone()` on a borrow must produce
            // an independently-owned closure with its own env — shallow memcpy
            // would leave both the slot and the clone aliasing the same heap
            // region and UAF when the source drops. Will be retired when
            // Callable types get TypeDef registration.
            if name.starts_with("Callable__")
                || name.starts_with("MutCallable__")
                || name.starts_with("ConsumeCallable__")
                || name == "GorgetClosure"
            {
                return Some("gorget_closure_clone_to_owned".to_string());
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
    /// Auto-deref `Ref[T] → T` at return-value boundaries for primitives and
    /// value types. Applies when the operand is `Copy(place)` / `Move(place)`
    /// of a bare `Ptr(T)` local and the enclosing return type is bare `T`
    /// (not Ptr). Used by expression-body functions and equip methods after
    /// `ensure_owned_at_boundary` has run.
    ///
    /// Returns the updated operand (may be a newly-introduced tmp holding the
    /// loaded value) or the original operand if no deref is needed.
    pub fn auto_deref_at_return(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        ret_type: TypeId,
    ) -> Operand {
        use crate::ir::types::GirType;
        use crate::ir::instructions::{Place, Projection};
        if matches!(self.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
            return operand;
        }
        let place = match &operand {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.clone(),
            _ => return operand,
        };
        let src_type = builder.local_type(place.local);
        let inner = match self.type_registry.get(src_type) {
            Some(GirType::Ptr(inner)) => *inner,
            _ => return operand,
        };
        if self.type_registry.is_resource_type(inner) {
            return operand;
        }
        let tmp = builder.add_local(inner, None);
        builder.assign(
            Place::local(tmp),
            Operand::Copy(Place {
                local: place.local,
                projections: vec![Projection::Deref],
            }),
        );
        crate::ir::builder::FunctionBuilder::copy(tmp)
    }

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
        //
        // NOTE: auto-deref for non-resource pointees (Ref[T] → T) does NOT
        // live here because this function doesn't know the target slot's type.
        // Struct field init calls this per-field — when both source and target
        // are `Ref[T]`, we want pass-through, not deref. The Ref[T] → T deref
        // is handled at the specific sites that know their target is bare T:
        // VarDecl, return statement, expression-body function return, call args
        // (via `auto_clone_if_ptr`).
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
        // - Tracked ref state (Ref, CowBorrow, CollectionRef, BareParam, Alias, ViewOf)
        // - Bare params (caller owns the data)
        //
        // Note: we intentionally DON'T treat "not drop-registered" as a proxy for
        // borrow. Several lowering paths emit correctly-independent locals without
        // explicit drop registration (LIR string assign auto-clones, `builder.call`
        // results for fresh allocating externs, etc.). Only explicit ref-state
        // flags represent aliasing relationships that require materialization.
        // Phase D: is_ref_local already covers all non-Owned variants;
        // is_bare_param and is_cow_borrow are subsets of it. Keep the
        // explicit bare_param + cow_borrow checks since both are still
        // semantically meaningful at this site (they fire even for
        // shapes is_ref_local might miss in legacy edge cases).
        let is_borrow = self.is_ref_local(local)
            || self.is_bare_param(local)
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
                    // Ptr to a non-resource, non-string value type — e.g. reading a
                    // `Ref[int]` (from `v.get()` / a `Ref[T]` field) where the callee
                    // expects `int`. Deref to load the pointee value. Primitives are
                    // scalars; simple user value structs are Copy-semantics, so a
                    // by-value load is just a memcpy at the backend.
                    if !self.type_registry.is_resource_type(inner) {
                        let deref_place = crate::ir::instructions::Place {
                            local: place.local,
                            projections: vec![crate::ir::instructions::Projection::Deref],
                        };
                        let tmp = builder.add_local(inner, None);
                        builder.assign(
                            crate::ir::instructions::Place::local(tmp),
                            Operand::Copy(deref_place),
                        );
                        return crate::ir::builder::FunctionBuilder::copy(tmp);
                    }
                }
            }
        }
        operand
    }

    // ── Unified ownership state helpers ──────────────────────────────

    /// Check if a local is tracked as a borrowed Ptr reference.
    /// Phase D: reads `local_ownership`. Returns true iff the local has
    /// a v2 entry that isn't `Owned`.
    pub fn is_ref_local(&self, local: LocalId) -> bool {
        self.func_state.local_ownership.get(&local).map_or(false, |s| s.is_ref())
    }

    /// Check if a local is tracked as definitely owning its data.
    /// Phase D: reads `local_ownership`.
    pub fn is_owned_local(&self, local: LocalId) -> bool {
        matches!(self.func_state.local_ownership.get(&local), Some(crate::ir::LocalOwnership::Owned))
    }

    /// Mark a local as owning its data. Overwrites any previous state.
    pub fn set_owned(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local, crate::ir::LocalOwnership::Owned);
    }

    /// Drop ownership tracking for a local.
    pub fn unset_ownership(&mut self, local: LocalId) {
        self.func_state.local_ownership.remove(&local);
    }

    /// Check if a local's string data is a fresh allocation not shared with any
    /// other variable. True only for direct function/extern call results that
    /// return the owned string type.
    pub fn is_fresh_string(&self, local: LocalId) -> bool {
        self.func_state.fresh_string_locals.contains(&local)
    }

    /// Check if a local has been borrowed-from via string Borrow assignment.
    /// If true, another local shares its heap data → clone needed on return.
    /// Phase D4 widening (2026-05-05): OR with the typed SharedHeap walk so
    /// the typed channel is strictly superset of the legacy sidecar during
    /// transition. The sidecar half is dropped in Phase 3.
    pub fn has_string_borrowers(&self, local: LocalId) -> bool {
        if self.func_state.string_borrow_sources.contains(&local) {
            return true;
        }
        use crate::ir::LocalOwnership;
        self.func_state.local_ownership.values().any(|state| matches!(
            state,
            LocalOwnership::SharedHeap { source } if *source == local
        ))
    }

    /// Mark a local as the source of a string Borrow assignment (`String b = a`).
    /// The target shares the source's heap data, so subsequent uses of the
    /// source — particularly `return source` — must clone.
    pub fn mark_string_borrow_source(&mut self, local: LocalId) {
        self.func_state.string_borrow_sources.insert(local);
    }

    /// Mark a local as a value-aliasing shallow copy of `source` (the
    /// `String b = a` shape). Phase D4 additive: typed companion to
    /// `mark_string_borrow_source(source)` — both are populated in
    /// transition. SharedHeap flushes to a Value-typed slot (same as
    /// Owned) but participates in `views_of_source(source)` so source
    /// mutation triggers materialisation through the same path that
    /// cap=0 byte-slice views use.
    pub fn set_shared_heap(&mut self, local: LocalId, source: LocalId) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::SharedHeap { source }
        );
    }

    /// Reset all callable-return-type tracking. Called at function-boundary
    /// entry; per-function transient state.
    pub fn callable_return_types_clear(&mut self) {
        self.func_state.callable_return_types.clear();
    }

    /// Record the return type of a callable-typed local. Reads back via
    /// `callable_return_type` at call sites for `cb(...)` return-type
    /// inference when `cb` binds a closure / function reference.
    pub fn set_callable_return_type(&mut self, local: LocalId, ret_type: TypeId) {
        self.func_state.callable_return_types.insert(local, ret_type);
    }

    /// Look up the recorded return type for a callable-typed local.
    pub fn callable_return_type(&self, local: LocalId) -> Option<TypeId> {
        self.func_state.callable_return_types.get(&local).copied()
    }

    /// Mark a local as a generic Ptr reference. Only sets if not already tracked
    /// with a more specific origin (set_bare_param / set_param_borrow_unique /
    /// set_field_borrow / set_collection_ref / set_view_of / cow_register_alias).
    /// The Alias(self) placeholder marks "borrowed but origin unknown to this
    /// layer" — the legacy fallback case from field loads / pattern extracts
    /// that don't have a more specific setter.
    pub fn set_ref(&mut self, local: LocalId) {
        self.func_state.local_ownership.entry(local).or_insert(
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Alias(local),
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// Check if a local is a bare Ptr param borrowing from the caller.
    /// Phase D: v2 representation is Borrowed { Param(self), Shared } —
    /// the self-referential Param(local) where local == this is the
    /// signature set_bare_param writes. Mutability::Unique would mean
    /// set_param_borrow_unique (a `&` param), which is not bare.
    pub fn is_bare_param(&self, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin, Mutability};
        matches!(self.func_state.local_ownership.get(&local),
            Some(LocalOwnership::Borrowed { origin: BorrowOrigin::Param(p), mutability: Mutability::Shared })
                if *p == local
        )
    }

    /// Mark a local as a bare Ptr param borrowing from the caller.
    pub fn set_bare_param(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Param(local),
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// A `&` (MutableBorrow) param on a resource type. Origin is the
    /// param itself; mutability is Unique. Replaces a generic `set_ref`
    /// call for this specific class.
    pub fn set_param_borrow_unique(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Param(local),
                mutability: crate::ir::Mutability::Unique,
            }
        );
    }

    /// Tag a `!`-sigil resource parameter so the LIR drop lowering knows the
    /// callee owns the pointee (not just borrows through the MutPtr). The flag
    /// is read by `lir/lower/drops.rs::lower_drop` to bypass the
    /// `is_pure_borrow_for` Nop and emit the deref-aware drop sequence. The
    /// drop accountant pairs this with `register_owning_param`, which emits
    /// `DropIfAlive { *local }` so the drop-flag dataflow controls whether
    /// the drop fires (suppressed if the body transferred ownership onward).
    ///
    /// Mirrors `set_param_borrow_unique` for read-site routing — both `&` and
    /// `!` resource params share the same `Borrowed { Param(self), Unique }`
    /// ownership shape and `BorrowedPtr` slot kind. The `is_owning_param`
    /// flag is the single typed bit distinguishing them at the GIR/LIR
    /// boundary.
    pub fn set_owning_param(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].is_owning_param = true;
        }
    }

    /// Whether `local` is a unique-borrow param-shape pointer:
    /// `Borrowed { origin: Param(self), mutability: Unique }`.
    /// Set by `set_param_borrow_unique` for `&` and `!` params (and
    /// closure mut-captures, which share the same shape). Read sites
    /// auto-deref through the MutPtr local.
    pub fn is_param_borrow_unique(&self, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin, Mutability};
        matches!(self.func_state.local_ownership.get(&local),
            Some(LocalOwnership::Borrowed { origin: BorrowOrigin::Param(p), mutability: Mutability::Unique })
                if *p == local
        )
    }

    /// A Ptr-typed local that's a borrow of a struct field (or enum
    /// variant payload field). `base` is the struct/scrutinee local;
    /// `field` is the field/variant-payload index.
    pub fn set_field_borrow(&mut self, local: LocalId, base: LocalId, field: u32) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Field { base, field },
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// Tag `local` as the source for element `index` of tuple temp `tuple`.
    /// Recorded at `Inst::TupleInit` emission so the return path can
    /// MoveZero element sources when the tuple is returned. Replaces the
    /// `tuple_element_locals` sidecar — see unified-resource-model.md §6.3.
    pub fn set_tuple_element_borrow(&mut self, local: LocalId, tuple: LocalId, index: u32) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::TupleElement { tuple, index },
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// Walk `func.locals` and yield each local tagged as a TupleElement of
    /// the given `tuple` temp. Replaces the legacy `tuple_element_locals`
    /// sidecar lookup. Yields the source local id alongside its index;
    /// callers iterate without ordering guarantees because the return-path
    /// MoveZero is order-insensitive.
    pub fn tuple_element_sources(&self, tuple: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.iter()
            .filter_map(|(local, state)| match state {
                LocalOwnership::Borrowed {
                    origin: BorrowOrigin::TupleElement { tuple: t, .. }, ..
                } if *t == tuple => Some(*local),
                _ => None,
            })
            .collect()
    }

    /// Mark a local as a CoW borrow (deferred clone). The placeholder
    /// `CowBorrowPending` origin distinguishes this from generic
    /// `set_ref`'s `Alias(self)` so `is_cow_borrow` can match. A
    /// subsequent `set_cow_borrow_source` upgrades to CollectionElement
    /// / FieldPath origin once the source collection is known.
    pub fn set_cow_borrow(&mut self, local: LocalId) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::CowBorrowPending,
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// Check if a local is a CoW borrow (deferred clone).
    /// Phase D: matches v2 Borrowed origin variants that all map to legacy
    /// CowBorrow / CollectionRef:
    ///   - CowBorrowPending: set_cow_borrow without source
    ///   - CollectionElement(_): direct collection-local borrow
    ///   - FieldPath(_): collection borrow through a field path
    /// The set_collection_ref path also produces CollectionElement /
    /// FieldPath origins, so this predicate effectively answers "is
    /// this any flavor of collection-element borrow." Same answer as
    /// legacy `is_cow_borrow` returned in practice — callers use it to
    /// gate clone-on-mutation and similar decisions, all of which apply
    /// to set_collection_ref-tagged locals identically.
    pub fn is_cow_borrow(&self, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        matches!(self.func_state.local_ownership.get(&local),
            Some(LocalOwnership::Borrowed {
                origin: BorrowOrigin::CowBorrowPending
                      | BorrowOrigin::CollectionElement(_)
                      | BorrowOrigin::FieldPath(_),
                ..
            })
        )
    }

    /// Mark a local as a string view borrowing from `source`'s buffer.
    pub fn set_view_of(&mut self, local: LocalId, source: LocalId) {
        self.func_state.local_ownership.insert(local,
            crate::ir::LocalOwnership::View {
                source: crate::ir::BorrowOrigin::RuntimeView(source),
            }
        );
    }

    /// Find all locals that are views of `source`. Phase D: reads v2.
    /// View-only — SharedHeap targets use `shared_heap_aliases_of_source`.
    /// View entries materialise via `cow_materialize_view` (cap=0 byte
    /// slice → cloned to owned buffer). SharedHeap entries are
    /// independent 32-byte slots whose heap was already deep-cloned at
    /// the `gorget_string_copy_cow` boundary; running them through
    /// `cow_materialize_view` would emit a redundant clone-to-owned and
    /// shift slot indices in self-host driver compilation.
    pub fn views_of_source(&self, source: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.iter()
            .filter_map(|(local, state)| {
                if matches!(state, LocalOwnership::View { source: BorrowOrigin::RuntimeView(s) } if *s == source) {
                    Some(*local)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find all locals that are SharedHeap value-aliases of `source`.
    /// Sibling to `views_of_source` but for the value-aliasing shape
    /// (`String b = a` → `b` is a 32-byte struct copy whose heap data
    /// the runtime CoW path shares with `a`). Source-mutation
    /// invalidation drops the SharedHeap tag (downgrades to plain
    /// drop-tracking via `unset_ownership`) — it does NOT call
    /// `cow_materialize_view`, because the heap is already deep-owned
    /// at the slot.
    pub fn shared_heap_aliases_of_source(&self, source: LocalId) -> Vec<LocalId> {
        use crate::ir::LocalOwnership;
        self.func_state.local_ownership.iter()
            .filter_map(|(local, state)| {
                if matches!(state, LocalOwnership::SharedHeap { source: s } if *s == source) {
                    Some(*local)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Record the source collection for a CowBorrow local.
    /// Does NOT upgrade the v2 entry past CowBorrowPending — the v2
    /// origin distinction (CowBorrowPending vs CollectionElement /
    /// FieldPath) is what lets is_cow_borrow disambiguate legacy
    /// CowBorrow from CollectionRef. Upgrading would broaden what
    /// cow_collection_refs_for_id matches, breaking the legacy
    /// invariant where cow_borrow locals weren't picked up by field
    /// mutation passes. The cow_borrow_sources sidecar still carries
    /// the source for cow_borrow_source() lookups.
    pub fn set_cow_borrow_source(&mut self, local: LocalId, collection: CollectionId) {
        self.func_state.cow_borrow_sources.insert(local, collection);
    }

    /// Look up the source collection for a CowBorrow local.
    pub fn cow_borrow_source(&self, local: LocalId) -> Option<&CollectionId> {
        self.func_state.cow_borrow_sources.get(&local)
    }

    /// Look up the source collection of a local marked as a CollectionRef.
    /// Phase D: reads v2 (Borrowed { CollectionElement | FieldPath }).
    pub fn collection_ref_source(&self, local: LocalId) -> Option<CollectionId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        match self.func_state.local_ownership.get(&local) {
            Some(LocalOwnership::Borrowed {
                origin: BorrowOrigin::CollectionElement(c), ..
            }) => Some(CollectionId::Local(*c)),
            Some(LocalOwnership::Borrowed {
                origin: BorrowOrigin::FieldPath(p), ..
            }) => Some(CollectionId::FieldPath(p.clone())),
            _ => None,
        }
    }

    /// Mark a local as a collection element reference.
    pub fn set_collection_ref(&mut self, local: LocalId, collection: CollectionId) {
        let v2 = match collection {
            CollectionId::Local(coll_local) => crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::CollectionElement(coll_local),
                mutability: crate::ir::Mutability::Shared,
            },
            CollectionId::FieldPath(path) => crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::FieldPath(path),
                mutability: crate::ir::Mutability::Shared,
            },
        };
        self.func_state.local_ownership.insert(local, v2);
    }

    /// Persist per-local ownership onto the GIR `Local` structs at the
    /// GIR/LIR boundary. D6: `Local.ownership` carries the rich
    /// `LocalOwnership` directly — no collapse to a 3-variant shape.
    /// LIR consumers read the typed enum (origin, mutability, view source)
    /// for drop, SlotLoad routing, and CoW materialisation decisions.
    pub fn flush_ownership_to_locals(&self, builder: &mut crate::ir::builder::FunctionBuilder) {
        for (&local_id, state) in &self.func_state.local_ownership {
            let idx = local_id.0 as usize;
            if idx >= builder.locals.len() { continue; }
            builder.locals[idx].ownership = state.clone();
        }
        // §6.8 Stage 3: derive `slot_kind` from (type, ownership) for
        // every local. Walk all locals (not just the ones with explicit
        // ownership entries above) so default-Owned Ptr locals get
        // OwnedPtr correctly. The mapping mirrors LIR's current
        // `is_ref()` behavior:
        //
        //   type=Ptr/MutPtr AND ownership.is_ref()  → BorrowedPtr
        //   type=Ptr/MutPtr AND !is_ref             → OwnedPtr
        //   type=non-Ptr                            → Value
        //
        // The empirical audit (1100-fixture sweep) showed zero locals in
        // the "non-Ptr type with borrow ownership" combination, so we
        // don't handle a fourth case. If one ever appears, the assert at
        // Stage 4's LIR sites will surface it.
        for local in builder.locals.iter_mut() {
            let is_ptr = matches!(self.type_registry.get(local.type_id),
                Some(crate::ir::types::GirType::Ptr(_) | crate::ir::types::GirType::MutPtr(_)));
            local.slot_kind = if is_ptr {
                if local.ownership.is_ref() {
                    crate::ir::SlotKind::BorrowedPtr
                } else {
                    crate::ir::SlotKind::OwnedPtr
                }
            } else {
                crate::ir::SlotKind::Value
            };
        }
    }

    // ── Copy-on-Write alias management ────────────────────────────────

    /// Register a CoW alias: `alias_local` is a Ptr(T) borrowing from `source_local`.
    /// Resolves transitively: if source is itself an alias, points to the root.
    pub fn cow_register_alias(&mut self, alias_local: LocalId, source_local: LocalId) {
        let root = self.cow_resolve_root(source_local);
        self.func_state.local_ownership.insert(alias_local,
            crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Alias(root),
                mutability: crate::ir::Mutability::Shared,
            }
        );
    }

    /// Resolve a local to its root source (follow alias chain).
    /// Phase D: walks v2 `Borrowed { Alias(s), .. }` chain. Self-loops
    /// (source == current — produced by set_ref placeholders) terminate
    /// resolution at the local itself, matching the legacy semantics
    /// where set_ref-marked locals weren't real aliases.
    fn cow_resolve_root(&self, local: LocalId) -> LocalId {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let mut current = local;
        while let Some(LocalOwnership::Borrowed {
            origin: BorrowOrigin::Alias(source), ..
        }) = self.func_state.local_ownership.get(&current) {
            if *source == current { break; }
            current = *source;
        }
        current
    }

    /// Check if a local is a CoW alias of something else.
    /// Phase D: a true alias has v2 = Borrowed { Alias(s), .. } with s != self
    /// (the self-loop form is the set_ref placeholder, not a real alias).
    pub fn cow_is_alias(&self, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        matches!(self.func_state.local_ownership.get(&local),
            Some(LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(s), .. }) if *s != local
        )
    }

    /// Check if a local has CoW aliases pointing to it (is a source).
    /// Phase D: scans v2 for Alias entries pointing at `local`, excluding
    /// self-loop placeholders.
    pub fn cow_has_aliases(&self, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.iter().any(|(other, s)|
            matches!(s, LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(src), .. }
                       if *src == local && *other != local)
        )
    }

    /// Collect all aliases pointing to `source`. Derived query — O(n) scan.
    /// Phase D: scans v2, excludes self-loop placeholders.
    fn cow_aliases_of(&self, source: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.iter()
            .filter_map(|(&id, s)| match s {
                LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(src), .. }
                    if *src == source && id != source => Some(id),
                _ => None,
            })
            .collect()
    }

    /// Check if a collection has any element refs pointing into it.
    /// Phase D: scans v2 for Borrowed { CollectionElement(`collection`), .. }.
    pub fn cow_has_collection_refs(&self, collection: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.values().any(|s|
            matches!(s, LocalOwnership::Borrowed {
                origin: BorrowOrigin::CollectionElement(c), ..
            } if *c == collection)
        )
    }

    /// Collect all collection refs pointing to a `CollectionId`. Derived query — O(n) scan.
    /// Phase D: scans v2 for Borrowed { CollectionElement | FieldPath }
    /// matching the target.
    fn cow_collection_refs_for_id(&self, target: &CollectionId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        self.func_state.local_ownership.iter()
            .filter_map(|(&id, s)| {
                let matches = match (s, target) {
                    (LocalOwnership::Borrowed {
                        origin: BorrowOrigin::CollectionElement(c), ..
                    }, CollectionId::Local(t)) => *c == *t,
                    (LocalOwnership::Borrowed {
                        origin: BorrowOrigin::FieldPath(p), ..
                    }, CollectionId::FieldPath(t)) => p == t,
                    _ => false,
                };
                if matches { Some(id) } else { None }
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
        if self.is_bare_param(local) {
            self.unset_ownership(local);
            self.cow_materialize_alias(builder, local, local, span);
        }

        // Case 1: local is an alias of something else → clone source into local.
        // Phase D: read alias source from v2 (Borrowed { Alias(s), .. } with
        // s != local — self-loops are set_ref placeholders, not real aliases).
        let alias_source: Option<LocalId> = {
            use crate::ir::{LocalOwnership, BorrowOrigin};
            match self.func_state.local_ownership.get(&local) {
                Some(LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(s), .. }) if *s != local => Some(*s),
                _ => None,
            }
        };
        if let Some(source) = alias_source {
            self.unset_ownership(local);
            self.cow_materialize_alias(builder, local, source, span);
        }

        // Case 2: local is a source with aliases → clone into each alias
        let aliases = self.cow_aliases_of(local);
        if !aliases.is_empty() {
            for alias in aliases {
                self.unset_ownership(alias);
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
            self.unset_ownership(view_local);
            self.cow_materialize_view(builder, view_local, span);
        }

        // Case 5: local has SharedHeap value-aliases → drop their tag.
        // The aliases are independent 32-byte struct slots whose heap
        // was already deep-cloned at the `gorget_string_copy_cow`
        // boundary, so no IR-level materialise is needed — only the
        // typed-state invalidation so source-mutation isn't blocked by
        // a stale alias tag pointing at a re-used slot.
        let shared_aliases = self.shared_heap_aliases_of_source(local);
        for alias_local in shared_aliases {
            self.unset_ownership(alias_local);
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
            self.unset_ownership(alias);
            self.cow_materialize_alias(builder, alias, source_local, span);
        }
        // Clean up other CoW tracking for the reassigned source — it's about
        // to get a new value, so stale entries would cause incorrect clones.
        if self.is_bare_param(source_local) {
            self.unset_ownership(source_local);
        }
        // Remove collection refs pointing to this source
        let refs = self.cow_collection_refs_for(source_local);
        for r in refs {
            self.unset_ownership(r);
        }
        // Materialize string views borrowing from this source
        let views = self.views_of_source(source_local);
        for view_local in views {
            self.unset_ownership(view_local);
            self.cow_materialize_view(builder, view_local, span);
        }
        // Drop SharedHeap value-aliases — heap already deep-cloned at the
        // shallow-copy boundary; no IR-level materialise needed.
        let shared_aliases = self.shared_heap_aliases_of_source(source_local);
        for alias_local in shared_aliases {
            self.unset_ownership(alias_local);
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
            self.unset_ownership(sub);
            self.cow_materialize_view(builder, sub, span);
        }

        let view_type = builder.local_type(view_local);
        if let Some(clone_fn) = self.clone_fn_for_ptr(view_type) {
            self.warn_implicit_clone(span, view_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(view_local)], view_type);
            let name_hint = builder.local_name(view_local).map(|s| s.to_string());
            let owned_local = builder.add_local(view_type, name_hint.as_deref());
            // Phase C: cloned is a fresh owned local dead at this single
            // use — Move transfers ownership into owned_local. Plain
            // assign (Copy mode) would alias the clone, leaking the
            // original. Mirrors the sibling cow_materialize_alias below.
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                crate::ir::instructions::Place::local(owned_local),
                crate::ir::builder::FunctionBuilder::copy(cloned),
            );
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
            // Phase C: cloned is a fresh owned local that's dead at this single
            // use — Move mode transfers ownership into owned_local. Copy mode
            // would alias the clone, leaking the original.
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                crate::ir::instructions::Place::local(owned_local),
                crate::ir::builder::FunctionBuilder::copy(cloned),
            );
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
        // Resource pointees: clone the pointee to an independent owned copy.
        // Non-resource pointees (primitives / Copy value structs): deref the
        // pointer to capture the current value at this snapshot — subsequent
        // source mutations can't affect the owned int/float/struct copy.
        let has_clone = self.clone_fn_for_ptr(inner_type).is_some();
        if !has_clone && self.type_registry.is_resource_type(inner_type) {
            return;
        }
        self.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::CoWMaterialization);
        let name_hint = builder.local_name(ref_local).map(|s| s.to_string());
        let owned_local = builder.add_local(inner_type, name_hint.as_deref());
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            let cloned = builder.call(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(ref_local)], inner_type);
            // Phase C: cloned is fresh + dead — Move into owned_local.
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                crate::ir::instructions::Place::local(owned_local),
                crate::ir::builder::FunctionBuilder::copy(cloned),
            );
            self.drops.register_local(owned_local, inner_type, &self.type_registry);
            self.set_owned(owned_local);
        } else {
            // Deref the Ref pointer to load the pointee value.
            builder.assign(
                crate::ir::instructions::Place::local(owned_local),
                Operand::Copy(crate::ir::instructions::Place {
                    local: ref_local,
                    projections: vec![crate::ir::instructions::Projection::Deref],
                }),
            );
            self.set_owned(owned_local);
        }
        if let Some(ref hint) = builder.local_name(ref_local).map(|s| s.to_string()) {
            let name = hint.clone();
            self.register_local(&name, owned_local, inner_type);
            self.func_state.named_locals.insert(owned_local);
        }
        // The old ref_local is now dead
        self.unset_ownership(ref_local);
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

    /// Phase A — auto-register a collection-family TypeDef + Named TypeId
    /// using the BuiltinTypeProtocol metadata. Idempotent: returns the
    /// existing TypeId if already registered. Used by the late-registration
    /// sites (dict literal lowering, items()/zip()/etc. method return-type
    /// inference) that synthesize `Vector__T` / `Dict__K_V` / `Set__T` /
    /// `HashMap__K_V` / `HashSet__T` names without going through
    /// register_collection_alias or map_ast_type_mut. Without metadata,
    /// downstream consumers (`collection_runtime_type`,
    /// `is_collection_type_name`, `is_resource_type`, ...) that read
    /// `metadata.collection_kind` would see no entry and fall through
    /// to a Ptr.
    ///
    /// Pass the canonical mangled name (e.g. "Vector__Tuple__int64_t__int64_t");
    /// the protocol base ("Vector") is detected from the leading prefix.
    pub fn ensure_collection_type(&mut self, name: &str) -> TypeId {
        if let Some(tid) = self.type_mapper.lookup_named(name) {
            return tid;
        }
        let base = if name.starts_with("Vector__") { "Vector" }
            else if name.starts_with("Deque__") { "Deque" }
            else if name.starts_with("Dict__") { "Dict" }
            else if name.starts_with("HashMap__") { "HashMap" }
            else if name.starts_with("Set__") { "Set" }
            else if name.starts_with("HashSet__") { "HashSet" }
            else {
                // Not a known collection family — register as a bare Named
                // and let downstream callers handle it.
                let tid = self.type_registry.insert(GirType::Named(name.to_string()));
                self.type_mapper.register_named(name.to_string(), tid);
                return tid;
            };
        if let Some(protocol) = super::builtins::lookup_protocol(base) {
            if !self.type_registry.has_type_def(name) {
                let drop_strat = match protocol.drop_fn {
                    Some(f) => crate::ir::types::DropStrategy::Trivial(f.to_string()),
                    None => crate::ir::types::DropStrategy::None,
                };
                self.type_registry.add_type_def(crate::ir::types::TypeDef {
                    name: name.to_string(),
                    kind: crate::ir::types::TypeDefKind::Struct(crate::ir::types::StructDef { fields: vec![] }),
                    metadata: crate::ir::types::TypeMetadata {
                        size: None,
                        align: None,
                        copy_semantics: protocol.copy_semantics,
                        drop_strategy: drop_strat,
                        clone_fn: protocol.clone_fn.map(String::from),
                        clone_inplace_fn: protocol.clone_inplace_fn.map(String::from),
                        materialize_fn: protocol.materialize_fn.map(String::from),
                        collection_kind: protocol.collection_kind,
                        enum_category: None,
                    },
                });
            }
        }
        let tid = self.type_registry.insert(GirType::Named(name.to_string()));
        self.type_mapper.register_named(name.to_string(), tid);
        tid
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
        Expr::Spawn { expr, .. } => expr_has_await(&expr.node),
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

/// True iff the runtime fn `name` always returns a fresh, independently
/// heap-allocated buffer (no aliasing into any input).
///
/// The source of truth is `RuntimeSig.returns_fresh` in `src/lir/runtime.rs`.
/// Replaces the previous `is_fresh_allocating_extern` matches!() name list.
///
/// Special case: `gorget_string_format` is not in the typed RuntimeFn registry
/// (it's a variadic-shaped formatter emitted only via the f-string lowering
/// path) — its result is tagged at the emission site directly. See
/// `src/ir/lowering/exprs/mod.rs` for that direct write.
fn runtime_returns_fresh(name: &str) -> bool {
    crate::lir::runtime::RuntimeFn::from_c_name(name)
        .map(|f| f.signature().returns_fresh)
        .unwrap_or(false)
}
