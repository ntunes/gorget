pub mod abi;
pub mod types;
pub mod instructions;
pub mod builder;
pub mod lowering;
pub mod printer;
pub mod transforms;
pub mod validate;
pub mod liveness;
pub mod tag_ownership;

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
/// populated during lowering and consumed by the C backend.
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
    /// Thread-spawned functions: (fn_name, return_type, payload type name as
    /// baked into the `Thread__{name}` call-site symbols ("void" for unit),
    /// stack_size bytes; 0 = OS default).
    pub thread_spawned_fns: Vec<(String, TypeId, String, i64)>,

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
/// Drives the unified `--clones[=MODE]` diagnostic. `sites` consumes
/// (span, type_name, reason); `verbose` adds `size_bytes` + `runtime_fn`;
/// `stats` (future per-id wiring) joins on `id` to compute the real
/// (size × frequency) perf cost per clone site.
#[derive(Debug, Clone)]
pub struct ImplicitCloneWarning {
    /// Stable identifier for this clone site. Monotonically allocated at
    /// emission time; deterministic within a build. Future runtime
    /// instrumentation will index a per-site counter array by this id.
    pub id: crate::ir::types::CloneId,
    /// Source span of the expression that triggers the clone.
    pub span: crate::span::Span,
    /// Human-readable type name being cloned.
    pub type_name: String,
    /// What triggered the clone.
    pub reason: ImplicitCloneReason,
    /// Approximate byte cost of the clone (handle size for resource types;
    /// 0 when not computable at warning-emit time). Resource-type handles
    /// are 24–128 bytes; the payload they own is reflected in `runtime_fn`
    /// rather than measured here, because element/recursive counts are not
    /// known statically.
    pub size_bytes: usize,
    /// Runtime function the compiler will call to perform the clone
    /// (`gorget_array_clone`, `gorget_map_clone`, `<UserStruct>__clone`,
    /// …). Empty when the boundary clone elides to a value copy or the
    /// dispatch is ambiguous at this site. Sourced from
    /// `LoweringContext::clone_fn_for_ptr` — the same typed metadata the
    /// lowering uses to emit the call; no name-matching at consumers.
    pub runtime_fn: String,
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
///
/// G3 note: this is the `MaterializeReason` carrier. It is `Copy` so it can
/// ride as a typed field on the clone-emitting `Instruction::Call` (see
/// `builder.call_clone`) without allocation, naming WHICH ownership boundary
/// demanded the clone. Every variant names a distinct boundary kind; there is
/// no `Other` catch-all.
///
/// Scope today: the reason is a GIR-only fact. It does NOT survive GIR→LIR —
/// `Instruction::Call` is destructured with `..` at `lir/lower/insts.rs`
/// (LIR's `Inst::Call` has no reason field), so backends never see it. LIR
/// survival (so the planner can read a directive at the layer it costs
/// against) is the planner follow-up, not this foundation. The Core-#9
/// exemption for this landing rests on that non-survival: nothing observable
/// changes.
///
/// `NeedsClassification` is the TRANSITIONAL burn-down marker only — a clone
/// site not yet migrated to pass its real reason. The strict validator
/// (`GG_VALIDATE_CLONE_REASONS=strict`) fails on it; it is deleted when the
/// burn-down hits zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// Result of an `extern borrowed T f(...)` call — the FFI returned a
    /// non-owning alias. The compiler clones at the call boundary so the
    /// caller's slot survives subsequent FFI state mutations that may
    /// invalidate the borrowed buffer.
    BorrowedExternReturn,
    /// CoW materialization hoisted to a loop PRE-HEADER (loop-carried
    /// bare-param). Distinct from at-site `CoWMaterialization` so the planner
    /// can cost per-iteration vs once-per-loop. Emitted only by the
    /// loop-pre-header materialize path (`materialize_loop_carried_bare_params`
    /// → `cow_before_mutation`); at-site CoW keeps `CoWMaterialization`.
    LoopPreHeaderMaterialize,
    /// CoW materialization hoisted to a SCOPE PRE-HEADER (a bare param a
    /// non-loop scope — an `if`/elif/else branch (bodies + conditions), `with`,
    /// `unsafe`, named-scope, `match` arms (bodies + guards), `select` recv
    /// arms — mutates, hoisted once before the scope dispatch so the post-merge
    /// read sees the private copy without a phi). Distinct from
    /// `LoopPreHeaderMaterialize` (per-loop hoist; also covers loop-`else`
    /// bodies) and at-site `CoWMaterialization` so the planner can cost the
    /// once-per-scope hoist. Emitted only by the scope pre-header materialize
    /// path (`materialize_scope_carried_bare_params` → `cow_before_mutation`).
    BranchPreHeaderMaterialize,
    /// User wrote `.clone()` explicitly. The clone is a user directive, not a
    /// compiler-inserted materialization; still a clone the validator must see.
    /// Tags the INSTRUCTION only — it does NOT mint an `ImplicitCloneWarning`
    /// or a Clone-Report row (the report's "N implicit clone(s)" count and the
    /// zero-clone ratchet tests depend on explicit clones staying out).
    ExplicitUserClone,
    /// TRANSITIONAL burn-down marker: a clone site not yet threaded with its
    /// real reason. `GG_VALIDATE_CLONE_REASONS=strict` fails on any of these.
    /// Delete this variant when the census hits zero.
    NeedsClassification,
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
            Self::BorrowedExternReturn => write!(f, "extern returns borrowed alias — cloned to caller-owned"),
            Self::LoopPreHeaderMaterialize => write!(f, "loop-carried materialize hoisted to pre-header"),
            Self::BranchPreHeaderMaterialize => write!(f, "branch-carried materialize hoisted to pre-branch"),
            Self::ExplicitUserClone => write!(f, "explicit `.clone()`"),
            Self::NeedsClassification => write!(f, "UNCLASSIFIED clone (burn-down marker)"),
        }
    }
}

/// Typed metadata carried from builtin protocol registration to LIR lowering.
/// Replaces the `is_self_by_ptr_method` name-list at the LIR call site.
#[derive(Debug, Clone)]
pub struct RuntimeCalleeInfo {
    /// C runtime function name (e.g. `gorget_array_push`).
    pub name: String,
    /// True when `self_conv` is `Borrow | MutBorrow` — the method receives
    /// self as a pointer.  False for `ByValue` (concurrency handles) and
    /// `Static` (no receiver).  Drives the GlobalRef→GlobalAddr decision in
    /// LIR arg lowering.
    pub self_by_ptr: bool,
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
    /// Place-resolver fall-through histogram rows when `--resolvers` was armed:
    /// `(resolver, shape, reason, count)`. Worklist generator only (Core #13) —
    /// emptiness is not soundness. Empty when the instrument is off.
    pub resolver_miss_hist: Vec<(String, String, String, u64)>,
    /// Per-site fall-through log when `--resolvers=sites` was armed.
    pub resolver_miss_sites: Vec<crate::ir::lowering::ResolverMissRecord>,
    /// Suggestions to use `!arg` (move) for last-use arguments.
    pub move_suggestions: Vec<MoveSuggestion>,
    /// Maps monomorphized method name → runtime callee metadata.
    /// Populated from BuiltinTypeProtocol declarations; `self_by_ptr` is set
    /// from `SelfConvention` so LIR lowering never re-derives it by name.
    pub runtime_callees: rustc_hash::FxHashMap<String, RuntimeCalleeInfo>,
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
    /// Per-sub-pass cumulative wall-clock time for the GIR lowering phase,
    /// surfaced through `gg profile` so the next dominant hot spot inside
    /// `gir_lower` is visible without instrumenting individual sites.
    /// Populated only by `lower_module`; unused by codegen/optimization.
    pub gir_lower_pass_times: std::collections::HashMap<&'static str, std::time::Duration>,
    /// Set of extern function names whose return value is `borrowed` —
    /// a non-owned pointer (e.g. SDL_GetError's internal buffer) that the
    /// caller must clone before treating as owned. Populated from
    /// `FunctionDef.returns_borrowed` for both the Gorget-side name and
    /// the bound C symbol (so call-site lookups find it via either).
    ///
    /// **Consumer wired** at `src/ir/lowering/exprs/calls.rs:1401` —
    /// call sites that return a borrowed value have a clone inserted
    /// automatically.
    pub fn_returns_borrowed: rustc_hash::FxHashSet<String>,
    /// Tier 2c (snag #23 class) — typed registry of shallow-copy
    /// heap-allocating consumer extern names.
    ///
    /// Populated at the writer site every time the GIR lowering emits a
    /// `__gorget_box_alloc_<T>` call (Box.new and `Box(value)` ctor).
    /// `Box.new` shallow-copies its argument's interior pointers into a
    /// fresh heap slot, so the source slot must be `MoveZero`'d before
    /// any subsequent Drop fires (see snag #23 / commit `4ebefe44`).
    ///
    /// Read by [`crate::ir::validate::validate_drop_pre_rebind`] to
    /// recognise heap-allocating consumers without name matching. Adding
    /// a new shallow-copy heap-allocating consumer at any future writer
    /// site is a single `module.heap_alloc_consumer_externs.insert(...)`
    /// call and the validator picks it up automatically.
    ///
    /// **Not in scope:** deep-clone consumers
    /// (`gorget_string_clone_to_owned`, `gorget_array_clone`,
    /// `gorget_map_clone`, `gorget_set_clone`) — those return a fresh
    /// independent value, source's storage is untouched, and a later
    /// Drop of source is correct. The set excludes them by construction:
    /// only writers that produce the snag #23 shape (shallow-copy alias
    /// of source interior into the freshly-allocated destination) insert.
    pub heap_alloc_consumer_externs: rustc_hash::FxHashSet<String>,
    /// Typed registry of consume-shape extern function names (collection
    /// mutators: `Vector__T__push`, `Dict__K__V__put`, `Set__T__add`,
    /// `Channel__T__send`, etc.). Populated at module finalization from
    /// `LoweringContext::fn_param_ownerships` — any registered fn with at
    /// least one `Ownership::Move` param is consume-shape by definition.
    ///
    /// Read by [`crate::ir::validate::validate_consume_sites`] to recognise
    /// mangled collection-mutator calls (which is_runtime_collection_mutator's
    /// name allowlist misses, because that allowlist matches the post-mono
    /// runtime symbol — `gorget_map_put` — not the IR-stage mangled name
    /// `Dict__K__V__put`). The dict-literal-resource-value double-free fix
    /// (commit `077f756e`) exposed this gap: Tier 2a's classifier was looking
    /// for the runtime symbol but the IR call uses the mangled mono name, so
    /// the validator missed the consume-without-MoveZero violation.
    ///
    /// Per CLAUDE.md "No name matching" / structural-guards Tier 3a: this
    /// registry is the typed-metadata bridge between the IR-stage mangled
    /// name and the runtime's consume contract. Writers populate it at
    /// `register_collection_method_sigs` registration time, and the validator
    /// reads it as the source of truth.
    pub consume_externs: rustc_hash::FxHashSet<String>,
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
            resolver_miss_hist: Vec::new(),
            resolver_miss_sites: Vec::new(),
            move_suggestions: Vec::new(),
            runtime_callees: rustc_hash::FxHashMap::default(),
            fn_extern_abi_kinds: rustc_hash::FxHashMap::default(),
            yield_point_fns: rustc_hash::FxHashSet::default(),
            fn_return_abis: rustc_hash::FxHashMap::default(),
            gir_lower_pass_times: std::collections::HashMap::new(),
            fn_returns_borrowed: rustc_hash::FxHashSet::default(),
            heap_alloc_consumer_externs: rustc_hash::FxHashSet::default(),
            consume_externs: rustc_hash::FxHashSet::default(),
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
    /// Cross-frame fault propagation (error-model.md §11, Inc-2.1a): true iff
    /// this function PARTICIPATES — its LAST param is a SYNTHESIZED trailing
    /// `MutPtr<i32>` fault-slot that is NOT part of its callable type. Direct
    /// callers pass the slot; a value-position / closure-adapter invocation
    /// (2-arg callable ABI) must pass `NULL` for it (the callee's fault arm then
    /// panics inline — indirect propagation is deferred to 2.3b). Typed flag,
    /// set at the source in `lower_function`; read by the first-class adapter
    /// generation so it doesn't forward a phantom slot arg. (devbook/24 rule 2.)
    pub participates_in_fault: bool,
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
// See docs/devbook/13-ownership-in-ir.md (Phase D, local axis) for the design.
//
// `LocalOwnership` + `BorrowOrigin` + `Mutability` replaced the 7-variant
// `LocalOwnershipState` (deleted in D3-full) and the 3-variant
// `OwnershipState` (deleted in D6). They are now the single typed shape
// carried on `Local.ownership` through the GIR/LIR boundary.

/// Single typed ownership state for a local. Carries borrow provenance
/// (via [`BorrowOrigin`]) and mutability inline rather than scattering
/// them across sidecar maps. Source of truth at the GIR/LIR boundary.
///
/// **Variant ordering note (Phase D4.5 step 5a):** `Untracked` is the
/// `#[default]` so newly-allocated `Local`s start as "no ownership
/// decision yet recorded". This preserves the legacy FxHashMap absence
/// semantics — readers like `is_owned_local` only return `true` when a
/// setter explicitly wrote `Owned`/`FreshOwned`/`SharedHeap`. Without
/// this distinction, retiring `func_state.local_ownership` would flip
/// every untracked local to `Owned` (the previous default), silently
/// registering drops on non-resource and not-yet-decided values.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum LocalOwnership {
    /// No ownership decision recorded yet. Equivalent to absence from
    /// the legacy `func_state.local_ownership: FxHashMap` — readers
    /// must NOT treat this as `Owned`. Set as the `#[default]` so new
    /// `Local`s start untracked; setters move the local to a concrete
    /// state (`Owned`, `Borrowed { .. }`, `View { .. }`, etc.) when
    /// the lowering point has enough info to decide.
    #[default]
    Untracked,
    /// Owns its data. Registered for drop at scope exit. Heap data may be
    /// shared with another local (e.g., via Move from a non-fresh source,
    /// via value-aliasing). For the strictly stronger "no aliasing" case
    /// see `FreshOwned`.
    Owned,
    /// Owns its data AND the heap allocation is provably fresh — no other
    /// local shares the same buffer. Sibling of `Owned`; strictly stronger.
    /// Set when a runtime callee returns a freshly allocated GorgetString
    /// (gorget_str_cat, gorget_string_format, gorget_str_to_upper, …) or
    /// when a user-defined function's return path guaranteed independence.
    /// Used by the return-clone elision and the self-referential reassign
    /// guard, both of which are sound when aliasing is excluded.
    FreshOwned,
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
    /// the same slot kind as Owned so SlotKind/ABI routing keeps the
    /// value layout intact; the local participates in
    /// `shared_heap_aliases_of_source(source)` so source mutation can
    /// invalidate the tag, and in `has_string_borrowers(source)` so
    /// return paths know to clone. Sole source of truth for the
    /// `String b = a` shape — the legacy `string_borrow_sources` sidecar
    /// was retired 2026-05-05 (Phase D4 attempt #3 Phase 3).
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
    /// `Untracked` returns false (legacy FxHashMap-absence semantic:
    /// `local_ownership.get(id).map_or(false, ..)`).
    pub fn is_ref(&self) -> bool {
        !matches!(self,
            LocalOwnership::Untracked
            | LocalOwnership::Owned
            | LocalOwnership::FreshOwned
            | LocalOwnership::SharedHeap { .. }
        )
    }

    /// Whether this state owns its data. Returns true for `Owned`,
    /// `FreshOwned`, and `SharedHeap` — all three carry their slot at
    /// runtime; `FreshOwned` adds the no-aliasing axis on top of `Owned`,
    /// `SharedHeap` adds source-provenance for value-aliasing.
    pub fn is_owned(&self) -> bool {
        matches!(self,
            LocalOwnership::Owned
            | LocalOwnership::FreshOwned
            | LocalOwnership::SharedHeap { .. }
        )
    }

    /// Whether the heap data is provably fresh — no other local shares the
    /// same buffer. Strictly stronger than `is_owned()`. The return-clone
    /// elision relies on this predicate.
    pub fn is_fresh(&self) -> bool {
        matches!(self, LocalOwnership::FreshOwned)
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
    /// T-A: when this local is the untracked value-temp that `Expr::Identifier`
    /// lowering produced by auto-deref'ing a bare `!` owning resource param,
    /// this is that source param's `LocalId`. Lets a downstream ctor/boundary
    /// consuming position MOVE the value (zeroing the param slot) instead of
    /// defensively cloning the untracked temp. `None` for every other local.
    pub deref_of_owning_param: Option<LocalId>,
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
    /// Address of the per-concrete-type `Box__<inner>__drop` wrapper — the
    /// trait-object vtable's `__drop` slot. Carries the mangled concrete
    /// inner type name as TYPED metadata (set once at `emit_vtable_globals`);
    /// backends spell the wrapper symbol at their emit boundary. Distinct
    /// from `FnRef` because the wrapper is backend-synthesized (no GIR
    /// function exists for it, so a `FnRef` would lower to `Zeroed`).
    BoxDropRef(String),
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
    /// R34 Track A: a const-foldable global collection emitted as STATIC C
    /// data — a `cap = 0` `GorgetArray` view over a file-scope compound-literal
    /// backing buffer (static storage duration, C11 §6.5.2.5p5). Replaces the
    /// imperative `__gg_static_init_<name>()` builder (a startup sequence of
    /// `gorget_array_new` + N× `gorget_array_push`) — no runtime constructor,
    /// no startup allocation, the data lives in `.rodata`/`.data`.
    ///
    /// RECURSIVE: each element is a full `GlobalInit` — scalar `Bytes`, string
    /// view (`Extern gorget_str_from_literal`, cap=0 into `.rodata`), a nested
    /// struct/enum (`Struct`), or a further nested `StaticArrayView`. This is
    /// what lets the RUNTIME_FNS / RESOURCES / BUILTIN_* tables (Vector-of-
    /// struct-of-{string,enum,nested-Vector,Option}) lower to pure data.
    ///
    /// `elem_type_name` is the mangled element type name (`int64_t`,
    /// `RuntimeParam`, `MatchKind`, `ResourceEntry`, …) that the LIR layer
    /// resolves to the element `LirType`; the backends spell the C / LLVM
    /// element type from that typed handle (never from a name substring).
    StaticArrayView {
        elem_type_name: String,
        elems: Vec<GlobalInit>,
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
    /// `extern borrowed T f(...)` — the return value is a non-owned pointer
    /// (e.g. SDL_GetError's internal buffer). The IR/LIR layer is expected
    /// to insert a clone at the ownership boundary so the caller gets an
    /// owned value. Currently parser/AST-side only; the auto-clone consumer
    /// in call lowering is a TODO. See TODO.md for follow-on work.
    pub returns_borrowed: bool,
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
                deref_of_owning_param: None,
            }],
            blocks: vec![BasicBlock::new()],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            participates_in_fault: false,
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
            returns_borrowed: false,
        });
        assert!(module.has_callable("printf"));
        assert!(!module.has_callable("missing"));
    }
}
