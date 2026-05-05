pub mod abi;
pub mod types;
pub mod instructions;
pub mod builder;
pub mod lowering;
pub mod printer;
pub mod transforms;
pub mod validate;

use instructions::{Instruction, Terminator};
use types::{LocalId, TypeId, TypeRegistry};
use crate::span::Span;

/// Compile-time selectable scheduler backend for `spawn`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SchedulerMode {
    /// M:N thread pool + work-stealing (default).
    #[default]
    Pool,
    /// 1:1 OS thread per spawn.
    Thread,
    /// Synchronous on caller thread.
    Inline,
    /// N:1 cooperative event loop.
    Single,
}

/// Metadata for a single test function, used by the C backend to generate the test runner.
#[derive(Debug, Clone)]
pub struct TestFnInfo {
    /// GIR function name (e.g. `__test_0`).
    pub fn_name: String,
    /// Human-readable test name (e.g. `"addition works"`).
    pub display_name: String,
    /// True when `@should_panic` attribute is present — panic = PASS, no panic = FAIL.
    pub should_panic: bool,
    /// Expected panic message substring (from `@should_panic("msg")`).
    pub expected_panic_msg: Option<String>,
    /// True when `@skip` attribute is present — test is reported but not executed.
    pub skipped: bool,
    /// Skip reason (from `@skip("reason")`).
    pub skip_reason: Option<String>,
    /// Per-test timeout in milliseconds (from `@timeout(5000)` or global `--timeout`).
    pub timeout_ms: Option<u64>,
}

/// Metadata for a single benchmark function.
#[derive(Debug, Clone)]
pub struct BenchFnInfo {
    /// GIR function name (e.g. `__bench_0`).
    pub fn_name: String,
    /// Human-readable benchmark name (e.g. `"vector sort"`).
    pub display_name: String,
}

/// Backend-specific runtime feature flags and type lists.
///
/// Separated from `Module` to keep the core IR struct focused on
/// types, functions, globals, and externs.  Everything here is
/// populated during lowering and consumed by the C backend / sim.
#[derive(Debug, Clone, Default)]
pub struct RuntimeFeatures {
    // ── Concurrency ────────────────────────────────────────────────
    /// True if any async function was detected; causes the C backend to emit async runtime.
    pub has_async: bool,
    /// True if any `spawn` expression was found; emits executor runtime.
    pub has_spawn: bool,
    /// Scheduler backend for `spawn` (pool, thread, inline, single).
    pub scheduler_mode: SchedulerMode,
    /// Whether any TaskGroup was used (triggers TaskGroup runtime emission).
    pub has_task_group: bool,
    /// Whether the blocking thread pool is needed (auto-offloaded blocking calls or spawn_blocking).
    pub has_blocking_pool: bool,
    /// Whether any std.sync types are used (AtomicInt, AtomicBool, Barrier, RWLock).
    pub has_sync: bool,
    /// Channel element C type names (e.g., ["int64_t"] for Channel[int]).
    pub channel_types: Vec<String>,
    /// Channel element types that need recv_timeout wrapper (subset of channel_types).
    pub channel_recv_timeout_types: Vec<String>,
    /// Shared[T] inner C type names (e.g., ["int64_t"] for Shared[int]).
    pub shared_types: Vec<String>,
    /// Weak[T] inner C type names (e.g., ["int64_t"] for Weak[int]).
    pub weak_types: Vec<String>,
    /// Mutex[T] inner C type names (e.g., ["int64_t"] for Mutex[int]).
    pub mutex_types: Vec<String>,
    /// RWLock[T] inner C type names (e.g., ["int64_t"] for RWLock[int]).
    pub rwlock_types: Vec<String>,
    /// Spawned functions: (fn_name, [(param_name, param_type)], return_type).
    pub spawned_fns: Vec<(String, Vec<(String, TypeId)>, TypeId)>,
    /// Functions that should run on the blocking pool instead of the M:N executor.
    pub blocking_fn_names: rustc_hash::FxHashSet<String>,

    // ── Threads / processes ────────────────────────────────────────
    /// Whether std.thread is used.
    pub has_thread: bool,
    /// Whether std.process Process type (fork+exec) is used.
    pub has_process: bool,
    /// Thread[T] return C type names (e.g., ["int64_t"] for Thread[int]).
    pub thread_types: Vec<String>,
    /// Thread-spawned functions: (fn_name, return_type).
    pub thread_spawned_fns: Vec<(String, TypeId)>,

    // ── Test runner ────────────────────────────────────────────────
    /// Test functions registered for the test runner.
    pub test_fns: Vec<TestFnInfo>,
    /// True when lowered in test mode (gg test).
    pub is_test_module: bool,
    /// True when a `suite setup:` block was lowered.
    pub has_suite_setup: bool,
    /// True when a `suite teardown:` block was lowered.
    pub has_suite_teardown: bool,
    /// Benchmark functions registered for the bench runner.
    pub bench_fns: Vec<BenchFnInfo>,

    // ── Codegen hints ──────────────────────────────────────────────
    /// When true, arithmetic wraps on overflow instead of aborting.
    pub overflow_wrap: bool,
    /// When set, emit trace instrumentation and write events to this file path.
    pub trace_filename: Option<String>,

    // ── Hot reload ─────────────────────────────────────────────────
    /// When true, this module uses `directive hot-reload`.
    pub hot_reload: bool,
    /// Name of the hot-reload State struct (derived from `init()` return type).
    pub hot_reload_state_type: Option<String>,
    /// FNV-1a hash of the State struct's field layout (for change detection).
    pub hot_reload_state_hash: u64,
    /// True when a `reload()` function exists in the module.
    pub hot_reload_has_reload_fn: bool,
}

/// Warning emitted when the compiler auto-clones a resource type.
/// Makes hidden allocations visible — future `--show-clones` will use this.
#[derive(Debug, Clone)]
pub struct ImplicitCloneWarning {
    /// Source span of the expression that triggers the clone.
    pub span: crate::span::Span,
    /// Human-readable type name being cloned.
    pub type_name: String,
    /// What triggered the clone.
    pub reason: ImplicitCloneReason,
}

/// Suggestion to pass an argument with `!` (move) instead of by borrow,
/// because the argument is the last use of the variable and the callee
/// would otherwise deep-clone it.
#[derive(Debug, Clone)]
pub struct MoveSuggestion {
    /// Span of the argument expression at the call site.
    pub span: crate::span::Span,
    /// Name of the variable being passed.
    pub name: String,
    /// Human-readable type name.
    pub type_name: String,
}

/// Why the compiler inserted an implicit clone.
#[derive(Debug, Clone)]
pub enum ImplicitCloneReason {
    /// `Type x = borrowed_ref` — explicit type receives a borrow
    VarDeclFromBorrow,
    /// `x = named_var` — named variable assigned to another named variable
    NamedToNamed,
    /// `return borrowed_ref` — return type mismatch (Ptr → owned)
    ReturnFromBorrow,
    /// `f(!borrowed_ref)` — move parameter receives a borrow
    MoveParamFromBorrow,
    /// `MyStruct{field: borrowed_ref}` — struct field receives a borrow
    StructFieldFromBorrow,
    /// CoW materialization: collection mutation forced clone of a borrowed element
    CoWMaterialization,
    /// Closure captures a borrowed reference that must be independently owned
    ClosureCapture,
    /// Match/case pattern extracts a resource-type field from a scrutinee
    PatternExtraction,
    /// Argument to a consuming operation (push, field store, enum variant init)
    ConsumingArg,
    /// Borrowed reference passed as argument to a function call
    CallArg,
}

impl std::fmt::Display for ImplicitCloneReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VarDeclFromBorrow => write!(f, "use `auto` for zero-cost borrow, or `.clone()` for explicit copy"),
            Self::NamedToNamed => write!(f, "use `!x` for move, or `.clone()` for explicit copy"),
            Self::ReturnFromBorrow => write!(f, "use `.clone()` for explicit copy"),
            Self::MoveParamFromBorrow => write!(f, "use `.clone()` for explicit copy"),
            Self::StructFieldFromBorrow => write!(f, "use `.clone()` for explicit copy"),
            Self::CoWMaterialization => write!(f, "mutation forced clone of borrowed element"),
            Self::ClosureCapture => write!(f, "closure captures borrowed reference"),
            Self::PatternExtraction => write!(f, "pattern extracts resource field from borrowed scrutinee"),
            Self::ConsumingArg => write!(f, "consuming operation requires owned data"),
            Self::CallArg => write!(f, "borrowed reference cloned at call boundary"),
        }
    }
}

/// A complete GIR module.
#[derive(Debug, Clone)]
pub struct Module {
    pub type_registry: TypeRegistry,
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
    pub externs: Vec<ExternDecl>,
    /// Original .gg filename (for backtrace display).
    pub source_filename: Option<String>,
    /// Concatenated source text (for source-line display in errors).
    pub source_code: Option<String>,
    /// Backend-specific runtime feature flags and type lists.
    pub runtime: RuntimeFeatures,
    /// Single source of truth for how each function's params are passed (by value, const ptr, mut ptr).
    pub fn_param_abis: rustc_hash::FxHashMap<String, Vec<crate::ir::lowering::context::ParamABI>>,
    /// Inferred function purity: name → Purity level.
    pub fn_purity: crate::semantic::purity::PurityByName,
    /// Implicit clone warnings emitted during lowering.
    pub implicit_clone_warnings: Vec<ImplicitCloneWarning>,
    /// Suggestions to use `!arg` (move) for last-use arguments.
    pub move_suggestions: Vec<MoveSuggestion>,
    /// Maps monomorphized method name → C runtime function name.
    /// Populated from BuiltinTypeProtocol declarations. Used by LIR backend
    /// to replace `map_monomorphized_to_runtime()`.
    pub runtime_callees: rustc_hash::FxHashMap<String, String>,
    /// Per-function extern ABI kinds: fn_name → Vec<AbiKind>.
    /// Populated from FunctionDef.param_abis for Declaration-body functions.
    pub fn_extern_abi_kinds: rustc_hash::FxHashMap<String, Vec<abi::AbiKind>>,
    /// Functions that are yield points (async or blocking). The shared_async
    /// transform uses this to release/reacquire mutex locks around calls.
    /// Populated from is_async and is_blocking qualifiers during pre-scan.
    pub yield_point_fns: rustc_hash::FxHashSet<String>,
    /// Per-function return ABI kind: fn_name → AbiKind.
    /// Populated from extern block ABI string + return type during pre-scan.
    pub fn_return_abis: rustc_hash::FxHashMap<String, abi::AbiKind>,
}

impl Module {
    /// Create an empty module with pre-allocated primitive types.
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            externs: Vec::new(),
            source_filename: None,
            source_code: None,
            runtime: RuntimeFeatures::default(),
            fn_param_abis: rustc_hash::FxHashMap::default(),
            fn_purity: rustc_hash::FxHashMap::default(),
            implicit_clone_warnings: Vec::new(),
            move_suggestions: Vec::new(),
            runtime_callees: rustc_hash::FxHashMap::default(),
            fn_extern_abi_kinds: rustc_hash::FxHashMap::default(),
            yield_point_fns: rustc_hash::FxHashSet::default(),
            fn_return_abis: rustc_hash::FxHashMap::default(),
        }
    }

    /// Look up a function by name.
    pub fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f| f.name == name)
    }

    /// Look up an extern declaration by name.
    pub fn find_extern(&self, name: &str) -> Option<&ExternDecl> {
        self.externs.iter().find(|e| e.name == name)
    }

    /// Check if a function or extern with the given name exists.
    pub fn has_callable(&self, name: &str) -> bool {
        self.find_function(name).is_some() || self.find_extern(name).is_some()
    }
}

/// A GIR function.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    /// `_0` = return place, `_1.._N` = params, rest = user/temps.
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    /// True for test functions (test "...") — enables cleanup stack registration
    /// for droppable locals so they're cleaned up on panic/longjmp.
    pub is_test_fn: bool,
    /// Human-readable Gorget function name for trace output (e.g. "add", "Point.distance").
    /// None for compiler-generated functions (closures, vtable methods, etc.).
    pub display_name: Option<String>,
    /// Byte-span of the function definition in source (for backtrace display).
    pub def_span: Option<Span>,
    /// `with` auto-refresh pairs: `(binding_local, param_local)`.
    /// After yield points in spawned functions, the binding local should be
    /// re-read from the (re-derived) param facade. Populated by AST lowering
    /// when `with shared_param:` is used on a shared parameter.
    pub with_refresh_pairs: Vec<(LocalId, LocalId)>,
    /// Inner spawn calls that pass a param through as a shared arg.
    /// Each entry: `(spawn_callee_name, vec of (call_arg_index, param_index))`.
    /// Used by the `shared_async` transform to rewrite inner spawns to pass
    /// the `Shared[Mutex[T]]` wrapper instead of the facade value.
    pub inner_shared_spawns: Vec<InnerSharedSpawn>,
}

/// Metadata for an inner spawn call inside a function that may need rewriting
/// when the enclosing function becomes a shared-async variant.
#[derive(Debug, Clone)]
pub struct InnerSharedSpawn {
    /// The callee being spawned (e.g., "modifier").
    pub callee_name: String,
    /// Callee param types (from fn_sigs).
    pub callee_param_types: Vec<TypeId>,
    /// Return type of the inner callee.
    pub callee_return_type: TypeId,
    /// Which args reference which params: (call_arg_index, param_index).
    /// param_index is 0-based into the enclosing function's params.
    pub shared_arg_mappings: Vec<(usize, usize)>,
    /// Whether the callee has internal yield points (determines sync vs async variant).
    pub callee_has_awaits: bool,
    /// Param ownership info for the callee (for determining mutability).
    pub callee_param_ownerships: Vec<crate::parser::ast::Ownership>,
}

// ── Phase D: typed local ownership and borrow provenance ─────────────
// See docs/internals/unified-resource-model.md §6 for the design.
//
// `LocalOwnership` + `BorrowOrigin` + `Mutability` replaced the 7-variant
// `LocalOwnershipState` (deleted in D3-full) and the 3-variant
// `OwnershipState` (deleted in D6). They are now the single typed shape
// carried on `Local.ownership` through the GIR/LIR boundary.

/// Single typed ownership state for a local. Carries borrow provenance
/// (via [`BorrowOrigin`]) and mutability inline rather than scattering
/// them across sidecar maps. Source of truth at the GIR/LIR boundary.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum LocalOwnership {
    /// Owns its data. Registered for drop at scope exit.
    #[default]
    Owned,
    /// Borrowed — does NOT drop. Carries provenance (which root the
    /// borrow points into) and mutability (shared vs unique).
    Borrowed { origin: BorrowOrigin, mutability: Mutability },
    /// Runtime view: a value that's structurally a borrow at runtime
    /// (cap=0 sentinel for strings today; broader notion under Phase B
    /// were it not deferred). Drop is a no-op until the value is
    /// materialized (cloned to owned). Source mutation triggers the
    /// materialize.
    View { source: BorrowOrigin },
    /// Value-aliasing shallow copy: the local IS its own owned slot at
    /// runtime (32-byte GorgetString struct, NOT a Ptr) but its heap
    /// data is shared with `source` (`String b = a` shape). Flushes to
    /// the same slot kind as Owned (so SlotKind/ABI routing keeps the
    /// value layout intact); however the local participates in
    /// `views_of_source(source)` queries so source mutation triggers
    /// materialization (clone the shared heap into a fresh buffer) —
    /// exactly like a View, but without changing the runtime shape.
    /// Phase D4 retirement attempt #2 (additive, 2026-05-05): kept
    /// alongside the legacy `string_borrow_sources` sidecar to allow
    /// progressive migration without destabilising self_host_bootstrap.
    SharedHeap { source: LocalId },
    /// Started borrowed, may have been materialized on some paths.
    /// Conditional drop guard via the existing memcmp-zero mechanism.
    /// Today's `OwnershipState::MaybeBorrowed` — kept until Phase C
    /// makes it unreachable.
    MaybeOwned,
}

impl LocalOwnership {
    /// Whether this state represents a borrowed Ptr reference (not owned).
    /// Used by LIR-level SlotLoad routing: anything not Owned is a Ptr at
    /// runtime. Returns true for Borrowed, View, and MaybeOwned.
    /// SharedHeap returns false: it IS owned at runtime (32-byte value
    /// struct in its own slot) — only the heap data behind it is shared.
    pub fn is_ref(&self) -> bool {
        !matches!(self, LocalOwnership::Owned | LocalOwnership::SharedHeap { .. })
    }

    /// Whether this is a "pure" borrow with no chance of being materialized.
    /// Such locals must NOT be dropped — the owner is elsewhere on the stack.
    /// Equivalent to legacy `OwnershipState::Ref`. The flush-time predicate:
    /// self-rooted Borrowed (param-self / alias-self / Field /
    /// CowBorrowPending) — these have no external source that could trigger
    /// CoW materialisation, so they stay borrowed for their entire lifetime.
    pub fn is_pure_borrow(&self) -> bool {
        match self {
            LocalOwnership::Borrowed { origin, .. } => match origin {
                BorrowOrigin::Field { .. } | BorrowOrigin::CowBorrowPending => true,
                BorrowOrigin::Param(_) | BorrowOrigin::Alias(_) => {
                    // Self-rooted (placeholder) → pure borrow. External-rooted
                    // (`Param(other_local)`, `Alias(other_local)`) is the
                    // MaybeBorrowed case and may have been materialized.
                    // The flush-time check uses the local's own LocalId for
                    // the comparison, so this method needs that context —
                    // see is_pure_borrow_for() for the precise predicate.
                    false
                }
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether this is a "pure" borrow given the local's own id, used to
    /// detect self-rooted Param / Alias placeholders. Locals whose origin
    /// resolves to themselves are sentinels meaning "borrowed but no
    /// external source tracked here" — never materialized, never dropped.
    pub fn is_pure_borrow_for(&self, self_id: LocalId) -> bool {
        match self {
            LocalOwnership::Borrowed { origin, .. } => match origin {
                BorrowOrigin::Field { .. } | BorrowOrigin::CowBorrowPending => true,
                BorrowOrigin::Param(p) => *p == self_id,
                BorrowOrigin::Alias(a) => *a == self_id,
                _ => false,
            },
            _ => false,
        }
    }
}

/// Where a borrow points. Carried inside `LocalOwnership::Borrowed`
/// and `LocalOwnership::View`. Replaces the per-shape source fields
/// scattered across `LocalOwnershipState` (Alias.source, CollectionRef.collection,
/// ViewOf.source) and the sidecar maps (cow_borrow_sources, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BorrowOrigin {
    /// Param N of the enclosing function. Const if Shared, mutable if Unique.
    Param(LocalId),
    /// Element borrowed from a collection. Mutation of the collection
    /// triggers materialisation (today's `LocalOwnershipState::CollectionRef`).
    /// `LocalId` here is the collection's own local; field-path forms of
    /// CollectionId are folded to their root local.
    CollectionElement(LocalId),
    /// Field of a struct local. Mutation of the struct (or assignment to
    /// the field) triggers materialisation.
    Field { base: LocalId, field: u32 },
    /// Alias of another local — propagate origin transitively to root via
    /// the resolution helper (today's `cow_resolve_root`).
    Alias(LocalId),
    /// Fresh runtime view (e.g. `s.trim()`, `s[1..3]`) borrowing from
    /// `source`'s buffer. Today's `LocalOwnershipState::ViewOf`.
    RuntimeView(LocalId),
    /// Field-path borrow: a collection element borrowed from a path
    /// like `self.data` or `cfg.items`. Path is the dotted-string form
    /// (e.g. "self.data"). Carries a String because the path may
    /// traverse multiple struct layers — a single LocalId can't
    /// represent the chain. Mutation of any prefix of the path
    /// triggers materialisation. Mirrors the legacy
    /// `CollectionId::FieldPath(String)` shape.
    FieldPath(String),
    /// Pending CoW borrow: set_cow_borrow was called without a known
    /// source. A subsequent set_cow_borrow_source upgrades the entry
    /// to CollectionElement / FieldPath. Distinct from set_ref's
    /// Alias(self) placeholder so is_cow_borrow can disambiguate.
    /// Should never persist past D6 once eager source propagation
    /// lands (set_cow_borrow gains a source-known-at-call-time
    /// signature).
    CowBorrowPending,
    /// Element source for a tuple temp. Recorded at `Inst::TupleInit`
    /// emission time so the return path can MoveZero the original
    /// element locals when a tuple is returned: the tuple struct
    /// shallow-copies element values, and without zeroing the sources
    /// both the returned tuple and the surviving locals own the same
    /// heap data. `tuple` is the tuple temp's local; `index` is the
    /// 0-based position of this local inside that tuple. Replaces the
    /// legacy `tuple_element_locals: FxHashMap<LocalId, Vec<LocalId>>`
    /// sidecar — see unified-resource-model.md §6.3.
    TupleElement { tuple: LocalId, index: u32 },
}

/// Mutability of a borrow. Today this distinguishes `&` / `!` mutable
/// captures (Unique) from default const-Ptr borrows (Shared).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Shared,
    Unique,
}

/// How a local's storage slot is laid out and accessed at the LIR layer.
///
/// `lower_place_addr` and the downstream LIR readers (`insts.rs:786`,
/// `LoadRef`, `IndexLoad`) make different routing decisions per kind.
/// See `unified-resource-model.md` §6.8.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SlotKind {
    /// Slot holds the value directly (size = sizeof(type)). Address-of is
    /// `&slot`. Reads project starting from the value type. Stores write
    /// the bytes at `&slot`.
    #[default]
    Value,
    /// Slot holds a pointer that this local OWNS. Type is `Ptr(T)`/`MutPtr(T)`.
    /// `lower_place_addr` returns `&slot` (a pointer-to-pointer); downstream
    /// must `Load` to materialize the pointer value before projecting.
    /// Created via `borrow_mut`, `Option[Ref[T]]::unwrap`, etc.
    OwnedPtr,
    /// Slot holds a pointer that's a non-owning view. Type is `Ptr(T)`/`MutPtr(T)`.
    /// `lower_place_addr` returns the pointer VALUE directly via `SlotLoad`.
    /// Downstream skips the deref-Load. Drop is a no-op (the source owns).
    /// Created via the borrow setters: `set_ref`, `set_bare_param`,
    /// `set_param_borrow_unique`, `set_field_borrow`, `set_collection_ref`,
    /// `set_view_of`, `set_cow_borrow`, `cow_register_alias`.
    BorrowedPtr,
}

/// A local variable slot.
#[derive(Debug, Clone)]
pub struct Local {
    pub type_id: TypeId,
    pub name_hint: Option<String>,
    /// Ownership / borrow state. Source of truth at the GIR/LIR boundary
    /// (D6: lifted from `func_state.local_ownership` directly onto Local).
    /// LIR consumers read the rich enum to decide drop, SlotLoad routing,
    /// and CoW materialisation.
    pub ownership: LocalOwnership,
    /// Slot layout/access kind. §6.8: written by GIR borrow setters and
    /// `add_local(ptr_type, ...)` paths; read by LIR slot-routing sites.
    /// Will eventually subsume the slot-routing semantics that `is_ref()`
    /// currently bundles into ownership.
    pub slot_kind: SlotKind,
    /// True for `!`-sigil resource parameters: the caller transferred
    /// ownership of the underlying value, but the slot itself holds a
    /// pointer (MutPtr) for ABI uniformity. The callee owns the data
    /// behind the pointer and MUST drop it at function exit unless it
    /// transfers ownership onward (via inner `consume`/`push`/`put`/etc.,
    /// which emit a `MoveZero` on the param slot).
    ///
    /// Distinguishes `!` from `&` resource params: both have
    /// `Borrowed { Param(self), Unique }` ownership and `BorrowedPtr`
    /// slot kind for read-site routing, but only `!` owns its pointee
    /// and needs the exit drop. Read by `lower_drop` (LIR) to bypass the
    /// `is_pure_borrow_for` Nop and emit the deref-aware drop sequence.
    pub is_owning_param: bool,
}

/// A basic block.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
    /// Parallel source span for each instruction (None for compiler-generated instructions).
    pub span_map: Vec<Option<Span>>,
    /// Source span of the terminator instruction.
    pub terminator_span: Option<Span>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            terminator: None,
            span_map: Vec::new(),
            terminator_span: None,
        }
    }
}

/// A global constant or variable.
#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub type_id: TypeId,
    pub init: GlobalInit,
}

#[derive(Debug, Clone)]
pub enum GlobalInit {
    Zeroed,
    Struct {
        type_name: String,
        fields: Vec<(String, GlobalInit)>,
    },
    FnRef(String),
    Bytes(Vec<u8>),
    /// Runtime-evaluated extern call. Args are typed so backends don't
    /// reverse-engineer C syntax. Used for module-level globals that
    /// need heap allocation or runtime state (`AtomicInt(0)` →
    /// `gorget_atomic_int_new(0)`, `Dict[K, V]()` →
    /// `gorget_dict_new(sizeof(K), sizeof(V))`, `static String s = "x"`
    /// → `gorget_str_from_literal("x", 1)`, etc.). Both backends
    /// emit the call at the start of `main` (or the C-side ctor).
    Extern {
        name: String,
        args: Vec<GlobalInitArg>,
    },
}

/// A single runtime-init argument — typed so backends don't C-syntax-parse
/// it back out of a string. `Sizeof` and `StrLit` carry the C type / raw
/// text respectively because at IR-lowering time the LIR struct registry
/// isn't yet populated for monomorphized types; backends look up by name.
#[derive(Debug, Clone)]
pub enum GlobalInitArg {
    /// Plain integer literal — sizes, counts, AtomicInt initial value.
    Int(i64),
    /// Plain float literal — passed by value to runtime ctors.
    Float(f64),
    /// Bool literal — encoded as 0/1 by both backends.
    Bool(bool),
    /// `sizeof(c_type)` — backends emit `sizeof(c_type)` (C) or the
    /// concrete byte count (LLVM).
    Sizeof(String),
    /// String literal — used by `gorget_str_from_literal(s, len)`.
    /// Stored raw (unescaped); backends apply target-appropriate
    /// escaping. Empty string == zero-length literal.
    StrLit(String),
    /// `&(c_type){value}` — address of a stack-allocated `c_type`
    /// initialized to `value`. Generated by the LIR translator when
    /// remapping `Mutex__T__new(v)` to `gorget_mutex_new(sizeof(T),
    /// &(T){v})`. The LLVM backend emits an alloca + store and passes
    /// the alloca's address; the C backend emits the C compound-literal
    /// expression verbatim.
    AddrOfInline {
        c_type: String,
        value: Box<GlobalInitArg>,
    },
}

/// An extern function declaration.
#[derive(Debug, Clone)]
pub struct ExternDecl {
    pub name: String,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub is_variadic: bool,
    /// Per-parameter ABI marshalling kind. Empty = all Auto.
    pub param_abis: Vec<abi::AbiKind>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::{I32_TYPE, I64_TYPE, UNIT_TYPE};

    #[test]
    fn empty_module() {
        let module = Module::new();
        assert!(module.functions.is_empty());
        assert!(module.globals.is_empty());
        assert!(module.externs.is_empty());
        assert_eq!(module.type_registry.len(), 12); // primitives
    }

    #[test]
    fn module_with_function() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![Local {
                type_id: I32_TYPE,
                name_hint: None,
                ownership: LocalOwnership::default(),
                slot_kind: SlotKind::default(),
                is_owning_param: false,
            }],
            blocks: vec![BasicBlock::new()],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
        });
        assert_eq!(module.functions.len(), 1);
        let f = module.find_function("main").unwrap();
        assert_eq!(f.name, "main");
        assert_eq!(f.return_type, I32_TYPE);
    }

    #[test]
    fn module_with_global() {
        let mut module = Module::new();
        module.globals.push(Global {
            name: "counter".into(),
            type_id: I64_TYPE,
            init: GlobalInit::Zeroed,
        });
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "counter");

        module.externs.push(ExternDecl {
            name: "printf".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            is_variadic: true,
            param_abis: vec![],
        });
        assert!(module.has_callable("printf"));
        assert!(!module.has_callable("missing"));
    }
}
