use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::instructions::{Constant, Operand};
use crate::ir::types::*;
use crate::parser::ast::{Expr, Ownership, PrimitiveType, Stmt, Type};
use crate::semantic::AnalysisResult;
use crate::span::Spanned;

use super::closures::ClosureLowering;
use super::drops::DropElaborator;
use super::types::TypeMapper;

use crate::ir::types::BlockId;

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

/// What producing an owned value out of a `Ptr(T)` / `MutPtr(T)` requires.
/// Read only through [`LoweringContext::ptr_materialization_kind`] — see that
/// method for why this is one accessor and not three call-site policies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PtrMaterialization {
    /// Deep-clone through the pointer with this clone fn.
    Clone(String),
    /// Load the pointee by value — primitives and plain value structs.
    DerefLoad,
    /// No owning representation exists for this pointee; the pointer stands.
    PassThrough,
}

/// What a destination slot knows about its own type at a materialization site.
///
/// The distinction is DECLARED-vs-INFERRED, not "does a type exist". Almost
/// every collection destination can produce *a* type; the ones that produce it
/// by stripping a mangled name (with a fallback) do not actually know their
/// slot, and treating that guess as declared mis-sizes the slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotType {
    /// The slot's DECLARED type: a struct field's declared type, a return
    /// signature, a `Ptr`/`MutPtr` target.
    Known(TypeId),
    /// The slot does not exist yet — it is MINTED from the materialized
    /// operand. Always materializes.
    FromOperand,
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
    /// Accumulated set of thread-spawned fn names: fn_name → (return TypeId,
    /// payload type NAME as baked into the `Thread__{name}` symbols at the
    /// spawn/join call sites ("void" for unit), stack_size bytes).
    /// NOT cleared between functions. Used to emit thread spawn/join helpers.
    /// The name is resolved ONCE here at the intrinsic (the same string the
    /// call-site symbols use) and written through to the LIR emit so the
    /// helper definitions can never drift from the call sites (layering
    /// rule 4, resolve once / write through).
    /// `stack_size` of 0 = OS default (plain pthread_create, byte-identical to the
    /// pre-stack-size wrapper); non-zero = a pthread_attr-sized wrapper. One size per fn.
    pub thread_fns: FxHashMap<String, (TypeId, String, i64)>,
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
        // Dedup: the only consumers of `task_type_fns` are the await-dispatch
        // `fns.len() == 1` gates (methods.rs / exprs/mod.rs). N spawns of the
        // SAME fn must collapse to len 1 so the named `__gorget_await_<fn>` path
        // resolves; without dedup, N same-type spawns push N entries and the
        // gate falls through, silently dropping the await. Nothing relies on the
        // Vec reflecting spawn count — only DISTINCT producer fns matter here.
        let v = self.task_type_fns.entry(task_type).or_default();
        if !v.contains(&fn_name) {
            v.push(fn_name);
        }
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
/// defaults between functions via `LoweringContext::begin_function_body_reset()`,
/// which only `functions::begin_function_body` may call.
#[derive(Default)]
pub struct FunctionState {
    /// name → (LocalId, GIR TypeId) for variables in the current function.
    pub locals: FxHashMap<String, (LocalId, TypeId)>,
    /// Stack of active loops for break/continue targeting.
    pub loop_stack: Vec<LoopInfo>,
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
    /// Callable parameter *argument* types: LocalId → Vec of the callable's
    /// declared param TypeIds (plain inner — no MutPtr wrap; the sigil-side of
    /// this axis lives in the parallel `callable_param_ownerships` sidecar).
    /// Read by the indirect-call arg-emit loops in `exprs/calls.rs` so
    /// `lower_call_arg`'s `callee_param_type` and `callee_param_ownership`
    /// derivations see the SAME shape they see for a direct call — the
    /// write-site fix for the both-lane SIGSEGV class Track B1 closes.
    /// Empty vec for a callable with zero params. Uniform on LOCAL declarations
    /// and PARAM binding (populated at the same four `set_callable_return_type`
    /// sites in `functions.rs` plus the one in `stmts/mod.rs`).
    pub callable_param_types: FxHashMap<LocalId, Vec<TypeId>>,
    /// Parallel to `callable_param_types` — the callable's declared param
    /// `Ownership` per index (`MutableBorrow` for `&`, `Move` for `!`,
    /// `Borrow` otherwise). Wired via `fn_param_ownerships` under the synthetic
    /// call name (`__callable_N` / `__gorget_closure_call_N`) so
    /// `lower_call_arg` picks the same pointer-vs-value forwarding it uses on
    /// direct calls.
    pub callable_param_ownerships: FxHashMap<LocalId, Vec<crate::parser::ast::Ownership>>,
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
    /// Phase D4: keyed by `LocalId` (typed) — replaces the legacy `HashSet<String>`
    /// name-based shape. See `docs/devbook/13-ownership-in-ir.md` (Phase D, §6.6).
    pub move_override_params: FxHashSet<LocalId>,
    /// Name of the function currently being lowered (for tracking consumed params).
    pub current_fn_name: String,
    /// True when the current method has `!self` (consuming self). Field loads
    /// from self use MoveZeroSource for resource fields instead of Ptr borrows.
    pub consuming_self: bool,
    /// CoW borrow provenance: maps a CowBorrow local to the collection it
    /// borrows from. Propagated through .get() → Option → .unwrap() chain.
    /// Used by VarDecl to set CollectionRef with the correct source.
    cow_borrow_sources: FxHashMap<LocalId, CollectionId>,
    /// CoW: variable names that are reassigned in the current function body.
    /// Pre-scanned before lowering. Locals in this set skip CoW aliasing.
    pub cow_reassigned_names: rustc_hash::FxHashSet<String>,
    /// Names that are the target of an assignment textually INSIDE a loop body
    /// (`for`/`while`/`loop`, recursively). Pre-scanned before lowering. Read by
    /// the owning-`!`-param call-arg fast-path (`lower_call_arg`): a loop-carried
    /// accumulator (`x = f(!x)`) must NOT take the pointer-forward + whole-slot
    /// MoveZero fast-path — that marks the reused pointer slot dead and the
    /// back-edge reassignment `_x.* = …` then reads it, tripping the GIR
    /// "read after MoveZero" validator. Such moves fall through to the
    /// temp-materialize path (correct for the reassigned shape).
    pub loop_reassigned_names: rustc_hash::FxHashSet<String>,
    /// Flow-sensitive CoW: for each statement span.start, the set of names
    /// reassigned or !-moved on any forward path from that point.
    ///
    /// Perf note (2026-05-18): set value type is `Rc<str>` (not `String`)
    /// because `cow_after_block` clones the live "future" set at every
    /// statement boundary (`result.insert(stmt.span.start, future.clone())`)
    /// and at every branch (`saved = future.clone()` etc). With `String`,
    /// each clone reallocates every entry — for the 695-function
    /// self-host lowerer that scaled to ~7 ms. With `Rc<str>` the clone
    /// is refcount-bump-per-entry, no allocation. Synthesised
    /// `"@mut:path"` markers and AST-borrowed names both round-trip through
    /// the same `Rc<str>` cell.
    pub cow_reassigned_after: FxHashMap<usize, rustc_hash::FxHashSet<std::rc::Rc<str>>>,
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
    /// Owning temporaries materialized as borrow-arguments (bare resource arg
    /// passed to a by-pointer param). The callee only borrows them, so the
    /// caller owns them and must drop them once the call expression completes
    /// (temporary lifetime). Populated by lower_call_arg, drained as DropIfAlive
    /// by the call lowering site right after emitting the Call instruction.
    pub pending_temp_drops: Vec<LocalId>,
    /// When true, pattern extraction of string fields skips cloning because
    /// the scrutinee is dead and BOTH the scrutinee copy AND the original
    /// variable will be MoveZeroed after extraction. Set by lower_match_stmt.
    pub scrutinee_clone_elision: bool,
    /// Set when a for-loop uses `index_load_borrow` for string elements.
    /// If false, return materialization can be skipped (no views to materialize).
    pub has_string_borrows: bool,
    /// One-shot suppression for `lower_expr`'s end-of-expression auto-prop hook.
    /// Set by sites that need the raw `Result[T, E]` operand (match scrutinee with
    /// `Ok`/`Error` arm patterns, rethrow inner, catch inner). Consumed (reset
    /// to `false`) inside `lower_expr` before lowering, so nested sub-expressions
    /// auto-prop normally. See `maybe_auto_propagate` for the routing.
    pub suppress_auto_prop: bool,
    /// Lazy loop-carried CoW materialize state.
    /// Maps a lazy-CoW element-borrow value local (`s`, a pre-loop String value
    /// slot holding a shallow borrow) → its `materialized?` flag local (`s_mat`,
    /// a pre-loop bool slot, init false). Both locals are allocated BEFORE the
    /// loop (lid < the loop's save_locals boundary) so they survive
    /// restore_locals and become loop-carried (LIR-SSA phis them at the header).
    /// The mutation-site materialize (`cow_materialize_view_lazy_in_place`)
    /// reads this to emit a flag-guarded IN-PLACE clone (clone once, from the
    /// still-valid borrow, write into `s`'s own slot) instead of a fresh-local
    /// rebind that wouldn't survive the loop boundary.
    ///
    /// An entry MUST survive in-place materialization: `restore_locals` can
    /// resurrect the `Borrowed{CollectionElement}` tag after a branch/loop
    /// boundary, and the persistent entry + runtime flag keep re-emitted
    /// guards correct (at most one runtime clone). Only a WRITE to the local
    /// (`lower_assign` / `lower_compound_assign`) removes the entry.
    pub cow_lazy_mat_flag: FxHashMap<LocalId, LocalId>,
    /// Memoized `is`-scrutinee locals, keyed by the `Expr::Is` node's span
    /// start. When an `Expr::Is` is lowered as a boolean VALUE (the tag test in
    /// an `if`/`elif`/`while` condition, an expr-`if`, or an `and`-chain), the
    /// scrutinee is evaluated exactly once into `scrut_local`; that local +
    /// type are recorded here. `emit_is_bindings` (which runs LATER, in the
    /// then/body block, to bind the pattern payload) READS this entry and
    /// reuses `scrut_local` INSTEAD of re-lowering the scrutinee expression.
    ///
    /// Re-lowering re-evaluates the scrutinee — for a side-effecting scrutinee
    /// (a mutating `&self` method returning `Option`, e.g. `if scopes.define(…)
    /// is Some(id):`) that means the method is CALLED TWICE, binding the payload
    /// from the second call and mis-observing state (`resolve.gg`
    /// `define_pattern_bindings` bind-to-local workaround). Sharing the single
    /// scrutinee local mirrors how `match` lowers its scrutinee once. Keyed by
    /// span-start because the value-lowering and the binding-lowering are
    /// dispatched from different call sites over the SAME `Spanned<Expr>` node
    /// (unique per source location).
    ///
    /// The entry is READ, never removed (`emit_is_bindings` uses
    /// `.get(..).copied()`). Read-not-remove is LOAD-BEARING: an `and`-chain
    /// binds its LEFT operand in TWO dominated blocks — `lower_short_circuit`'s
    /// rhs block (so the binding is in scope while evaluating the right operand)
    /// AND the outer then/body block (for the branch body) — and BOTH must reuse
    /// the single scrutinee evaluation. Removing on the first read would force
    /// the second binding site to re-lower, re-invoking the scrutinee and
    /// re-introducing the double-eval on the left operand. Leaving stale entries
    /// is harmless: spans are unique per source location, the value-lowering
    /// block dominates every binding site (so the local is always valid to
    /// read), and the whole map is cleared en masse per-function via `Default`
    /// (`begin_function_body_reset`) — so a stale entry can never be reused across
    /// functions.
    pub is_scrut_memo: FxHashMap<usize, (LocalId, TypeId)>,
}

/// WHERE a materialize directive fires relative to the scope structure — the
/// planner's position axis (devbook/11 § "Materialization points"; devbook/24
/// rule 1: the fact rides a typed field, never a name/shape heuristic). Keyed
/// by the span the consumer applies it at, so per-position costing (once-per-
/// loop vs once-per-branch vs per-mutation) stays honest.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MaterializePosition {
    /// Materialize immediately before the mutation at this span (at-site CoW —
    /// the assign/index/compound/method-receiver/`&`-formation positions).
    AtSite { mutation: crate::span::Span },
    /// Hoisted once to a loop pre-header keyed by the loop's anchor span.
    LoopPreHeader { anchor: crate::span::Span },
    /// Hoisted once to a conditional-scope pre-header (dominating point) keyed
    /// by the scope's anchor span.
    BranchPreHeader { anchor: crate::span::Span },
}

/// One materialize directive: WHICH root to break the alias on, WHY (the
/// `ImplicitCloneReason` cost tag stamped on the emitted clone), and at WHICH
/// position. The per-function `MaterializePlan` is the table of these — the
/// explicit form of what today is split between the ambient `cow_reason`
/// (why) and the scattered at-site `cow_before_mutation` calls (where). The
/// self-host lane already carries the equivalent table (`cow_scope_muts`, the
/// flat "anchor@name" scope-mutation set — devbook/11 § "planner consumer #1").
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MaterializeDirective {
    pub root: LocalId,
    pub reason: crate::ir::ImplicitCloneReason,
    pub position: MaterializePosition,
}

impl MaterializeDirective {
    /// The user-source span this directive applies its clone at (drives the
    /// CloneId minted for `--clones` attribution — the same span the pre-fix
    /// at-site call passed).
    pub fn span(&self) -> crate::span::Span {
        match self.position {
            MaterializePosition::AtSite { mutation } => mutation,
            MaterializePosition::LoopPreHeader { anchor }
            | MaterializePosition::BranchPreHeader { anchor } => anchor,
        }
    }
}

/// The explicit per-function materialization plan (planner campaign round 3).
/// A directive table that grows client-by-client as at-site `cow_before_mutation`
/// call classes are converted to plan lookups (the `ratchet_b_materialize_site_count`
/// convergence meter DECREASES with each conversion). Today it records every
/// converted materialize for observability/costing and is applied through the
/// SINGLE reason-stamping funnel (`cow_before_mutation_planned`); a
/// future planner pass reads the table to CHOOSE a strategy per boundary (hoist,
/// elide-by-liveness) rather than materializing unconditionally at each site.
#[derive(Debug, Default)]
pub struct MaterializePlan {
    /// Directives recorded this function (append-order). Observability/costing
    /// today; the future planner's working set.
    pub directives: Vec<MaterializeDirective>,
}

impl MaterializePlan {
    pub fn clear(&mut self) {
        self.directives.clear();
    }
    pub fn record(&mut self, directive: MaterializeDirective) {
        self.directives.push(directive);
    }
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
    /// Extern binding: Gorget name → C symbol name (e.g., "llabs_wrapper" → "llabs").
    pub extern_bindings: FxHashMap<String, String>,
    /// Default parameter values: fn_name → Vec<(param_index, default_expr)>.
    pub fn_defaults: FxHashMap<String, Vec<(usize, crate::parser::ast::Expr)>>,
    /// Function parameter names: fn_name → Vec<param_name> (in declaration order).
    pub fn_param_names: FxHashMap<String, Vec<String>>,
    /// Function parameter ownerships: fn_name → Vec<Ownership> (in declaration order).
    /// Used by token wrapper generation to determine lock type per shared arg.
    pub fn_param_ownerships: FxHashMap<String, Vec<crate::parser::ast::Ownership>>,
    /// Tier 2a strengthening: typed registry of consume-shape extern fn
    /// names (collection mutators emitting `Dict__K__V__put` etc.). Writer
    /// sites register here at call emission; the registry transfers to
    /// `Module::consume_externs` at finalization. See `Module::consume_externs`
    /// for the rationale (catches the `is_runtime_collection_mutator`
    /// name-allowlist gap that lets the validator miss mangled-name
    /// collection-mutator calls).
    pub consume_externs: rustc_hash::FxHashSet<String>,
    /// Unified parameter ABI: fn_name → Vec<ParamABI> (in declaration order).
    /// Single source of truth for how each parameter is passed at the C ABI level.
    pub fn_param_abis: FxHashMap<String, Vec<ParamABI>>,
    /// Extern ABI marshalling kinds: fn_name → Vec<AbiKind>.
    /// Populated from FunctionDef.param_abis for Declaration-body functions.
    pub fn_extern_abi_kinds: FxHashMap<String, Vec<crate::ir::abi::AbiKind>>,
    /// Functions that are yield points (async or blocking qualifiers).
    pub yield_point_fns: rustc_hash::FxHashSet<String>,
    /// Functions declared `noreturn` (extern functions like `exit`/`abort`).
    /// Lowered by emitting `unreachable` after the call so the basic block
    /// terminates correctly; this lets divergent match-expression arms
    /// compose with the surrounding result type.
    pub noreturn_fns: rustc_hash::FxHashSet<String>,
    /// Per-function return ABI kind.
    pub fn_return_abis: rustc_hash::FxHashMap<String, crate::ir::abi::AbiKind>,
    /// Extern functions declared `borrowed T f(...)` — their return value is
    /// a non-owned pointer (the FFI buffer's lifetime is not Gorget-managed).
    /// Callers must clone at the ownership boundary; the IR layer is
    /// expected to auto-insert that clone. Populated for both the Gorget
    /// name and the bound C symbol.
    pub fn_returns_borrowed: rustc_hash::FxHashSet<String>,
    /// Module-level global variable names (from StaticDecl items).
    /// Used by Expr::Identifier lowering to emit Constant::GlobalRef instead of I64(0).
    pub global_names: rustc_hash::FxHashSet<String>,
    /// Module-level global variable type names: var_name → AST type name (e.g. "AtomicInt").
    /// Used by infer_type_name_from_operand_full to dispatch methods on globals.
    pub global_type_names: FxHashMap<String, String>,
    /// Names of module-level `String FOO = "literal"` globals whose backing
    /// `GorgetString` is a cap=0 rodata view (no heap allocation). The C/LLVM
    /// emitters recognize the same `gorget_str_from_literal(StrLit, Int)` ctor
    /// shape and emit a static `{ .data="...", .cap=0, .len=N, .alloc=NULL }`
    /// initializer in place of a runtime call. Tracked here so the GIR clone-
    /// on-access path (`clone_resource_global_ref`) can elide the clone call —
    /// the global never aliases drop-tracked memory, so there's nothing to
    /// double-free and nothing to deep-copy.
    pub string_literal_view_globals: rustc_hash::FxHashSet<String>,
    /// R34 Track A: `DefId`s of module-level statics that are DIRECTLY mutated
    /// somewhere in the program (mutating-method receiver, assign/compound-
    /// assign target root, `&STATIC` mut-borrow, `!STATIC` move — including
    /// index/field projections). Populated by `scan_mutated_statics` in a
    /// whole-program pre-pass that COMPLETES BEFORE the static-lowering loop
    /// (statics lower before functions, so a same-loop population would be
    /// empty when consumed → a mutated static wrongly emitted as an immutable
    /// `.rodata` view → UB). `lower_static_decl` refuses the const-view
    /// optimization for any static whose `DefId` is in this set. Keyed by the
    /// original static `DefId` — sound across module boundaries because imports
    /// rebind to the original definition's `DefId` (not a fresh alias id).
    pub mutated_static_defs: rustc_hash::FxHashSet<crate::semantic::ids::DefId>,
    /// Set of equip method names that are GIR-lowered (not extern/C-runtime).
    /// Used by lower_method_call to decide whether to pass resource-type args by pointer.
    pub gir_equip_methods: rustc_hash::FxHashSet<String>,
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
    /// Monotonic allocator for `CloneId` — bumped once per `warn_implicit_clone`
    /// call so every warning gets a stable per-build identifier. The next-id
    /// scheme is intentionally simple (u32 counter, no recycling); deterministic
    /// within a build.
    pub next_clone_id: u32,
    /// `--clones=stats`: emit a `__gorget_clone_site_hit(<CloneId>)` counter
    /// bump immediately before each implicit clone call, giving per-site
    /// runtime attribution (joined offline with the `--clones=verbose` static
    /// table). False (the default) emits nothing — zero cost when off.
    pub clone_stats: bool,
    /// `--resolvers` place-resolver fall-through bookkeeping (worklist only;
    /// Core #13). Default-off; single `enabled` check before any string work.
    pub resolver_diag: super::resolver_diag::ResolverDiag,
    /// G3: the `MaterializeReason` the CoW materialize helpers
    /// (`cow_materialize_alias`/`_view`/`_collection_ref`) stamp on the clone
    /// `Call` they emit. Defaults to `CoWMaterialization` (at-site CoW). The
    /// loop-pre-header entry (`cow_before_mutation_loop_preheader`) temporarily
    /// raises it to `LoopPreHeaderMaterialize` (save/restore) so the planner can
    /// cost once-per-loop hoists distinctly from per-iteration at-site clones.
    /// A scoped ambient value rather than a param threaded through the ~30-site
    /// CoW web — the reason is caller-CONTEXT ("we're in a loop pre-header"),
    /// not a property of the materialized local, and it is GIR-only (dropped at
    /// LIR), so a mis-scope can only mis-count the census, never miscompile.
    pub cow_reason: crate::ir::ImplicitCloneReason,
    /// The explicit per-function materialization plan (planner campaign round
    /// 3). Records every materialize routed through the plan-apply funnel; the
    /// convergence meter (`ratchet_b_materialize_site_count`) drops as at-site
    /// `cow_before_mutation` classes migrate here. Reset per function via
    /// `functions::begin_function_body` (the universal per-function-body entry
    /// every lowering path funnels through) — so this genuinely IS per-function,
    /// not module-wide accumulation.
    pub materialize_plan: MaterializePlan,
    /// Maps monomorphized method name → runtime callee metadata.
    /// Populated from BuiltinTypeProtocol declarations during module setup.
    /// Used by the LIR backend to replace `map_monomorphized_to_runtime()`.
    pub runtime_callees: FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
    /// Maps callee span start → mangled function name for cross-module calls.
    /// Built from resolution_map + module_fn_manglings so that call lowering
    /// uses the correct target when multiple modules define the same bare name.
    pub call_resolved_names: FxHashMap<usize, String>,
    /// Tier 2c (snag #23 class) — typed registry of shallow-copy
    /// heap-allocating consumer extern names emitted by this lowering.
    ///
    /// Populated at the writer site every time a Box.new / `Box(value)`
    /// ctor emits a `__gorget_box_alloc_<T>` extern call. Threaded onto
    /// `Module.heap_alloc_consumer_externs` at the end of lowering for
    /// the validator (`validate_drop_pre_rebind`) to read structurally.
    /// Replaces the prior `callee.starts_with("__gorget_box_alloc_")`
    /// name match per CLAUDE.md "No name matching".
    pub heap_alloc_consumer_externs: rustc_hash::FxHashSet<String>,
    /// Per-sub-pass timing accumulators for `lower_function`. Surfaces in
    /// `gg profile` as `lower_function::<name>` sub-passes folded into the
    /// `gir_lower_pass_times` map by `lower_module`. Mirrors the pattern
    /// from commit `3dfc9916` (per-pass timing in `lower_module`), one
    /// layer deeper. Sub-pass names: `setup` (return type/params/locals
    /// registration + drop-scope push), `prescan` (cow_unsafe / name use
    /// counts / liveness), `body` (the actual statement+expression
    /// lowering and tail return emission), `finalize` (ownership flush +
    /// builder.build + module.functions push).
    pub lower_fn_sub_times: std::collections::HashMap<&'static str, std::time::Duration>,
    /// Cumulative wall-time spent inside nested `lower_stmt` calls. Used by
    /// the per-statement-kind instrumentation in `lower_stmt` to compute
    /// EXCLUSIVE (self) time per kind: `exclusive = elapsed - (post - pre)`
    /// where `pre`/`post` snapshot this counter around the dispatch. Each
    /// `lower_stmt` adds its own total `elapsed` here so the parent call
    /// subtracts it out. Resets implicitly with the context lifetime
    /// (no cross-function bleed because every parent eventually finishes
    /// and the counter is read as a delta, not absolute).
    pub stmt_nested_dur: std::time::Duration,
    /// Synthetic zero-arg `__gg_static_init_<name>()` functions built during
    /// static-decl lowering for collection-literal static initializers
    /// (Bug B). Accumulated here, then lowered through the normal
    /// `lower_function` path in the non-generic function loop (after
    /// monomorph collection). Each function's body is the load-bearing
    /// `<T> __r = <RHS>; return __r` shape — see `lower_static_decl`.
    pub synthetic_static_init_fns: Vec<crate::parser::ast::FunctionDef>,
}

/// Snapshot of lowering state taken at branch entry, restored at branch exit.
/// Carries the name→local map and a per-local ownership snapshot so that
/// CoW materialization that runs inside one branch (rebinding a name,
/// removing an ownership flag) does not leak into sibling branches or
/// post-join code.
///
/// `local_id_boundary`: any local whose ID is ≥ this was created after the snapshot
/// — its ownership state is kept as-is on restore (branch-local locals survive,
/// modulo the CollectionElement/FieldPath/View filter applied in restore_locals).
///
/// `pre_save_ownership[i]`: ownership state of local `i` at save time, for
/// `i in 0..local_id_boundary`. Phase D4.5 step 5c: replaces the legacy
/// `FxHashMap<LocalId, LocalOwnership>` snapshot with a dense `Vec` —
/// `Local.ownership` is now the live source of truth, so a positional
/// snapshot mirrors it perfectly.
///
/// `local_types_at_save`: per-local declared type at save time, parallel
/// to `pre_save_ownership`. On restore, if a local's
/// `builder.locals[i].type_id` has been CHANGED during the scope
/// (e.g. `assigns.rs`'s in-place CoW upgrade flipping Ptr(T)→T), that local is
/// treated as permanently upgraded — its ownership state is *not* reverted.
/// This prevents inconsistent (ownership=CollectionRef, type=T) states that
/// break LIR codegen.
#[derive(Clone)]
pub struct SavedScope {
    locals: FxHashMap<String, (LocalId, TypeId)>,
    pre_save_ownership: Vec<crate::ir::LocalOwnership>,
    pre_save_types: Vec<TypeId>,
    local_id_boundary: u32,
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
            extern_bindings: FxHashMap::default(),
            fn_defaults: FxHashMap::default(),
            fn_param_names: FxHashMap::default(),
            fn_param_ownerships: FxHashMap::default(),
            consume_externs: rustc_hash::FxHashSet::default(),
            fn_param_abis: FxHashMap::default(),
            fn_extern_abi_kinds: FxHashMap::default(),
            yield_point_fns: rustc_hash::FxHashSet::default(),
            noreturn_fns: {
                // `gorget_panic` is the hardcoded C-symbol for `panic(msg)`
                // (lowered at `stmts/mod.rs`'s `call_extern("gorget_panic",
                // …)`). Pre-existing TODO recommended either declaring
                // `panic` in stdlib as `extern noreturn` (layering-correct,
                // multi-step) or registering the C symbol directly here.
                // Going with option (b) since the hardcoded lowering site
                // already knows to call `gorget_panic`; the noreturn flag
                // just needs to propagate to call-site terminator emission
                // so `panic()` in match-as-expression / `??` RHS / catch
                // recovery positions is treated as a divergent expression
                // (Never-typed) rather than a void call. Pairs with the
                // typecheck change that returns `never_id` for `panic`.
                let mut s = rustc_hash::FxHashSet::default();
                s.insert(String::from("gorget_panic"));
                s
            },
            fn_return_abis: rustc_hash::FxHashMap::default(),
            fn_returns_borrowed: rustc_hash::FxHashSet::default(),
            global_names: rustc_hash::FxHashSet::default(),
            global_type_names: FxHashMap::default(),
            string_literal_view_globals: rustc_hash::FxHashSet::default(),
            mutated_static_defs: rustc_hash::FxHashSet::default(),
            gir_equip_methods: rustc_hash::FxHashSet::default(),
            trivial_getter_methods: rustc_hash::FxHashSet::default(),
            sentinel_to_option_methods: rustc_hash::FxHashSet::default(),
            implicit_clone_warnings: Vec::new(),
            next_clone_id: 0,
            clone_stats: false,
            resolver_diag: super::resolver_diag::ResolverDiag::default(),
            cow_reason: crate::ir::ImplicitCloneReason::CoWMaterialization,
            materialize_plan: MaterializePlan::default(),
            runtime_callees: FxHashMap::default(),
            call_resolved_names: FxHashMap::default(),
            heap_alloc_consumer_externs: rustc_hash::FxHashSet::default(),
            lower_fn_sub_times: std::collections::HashMap::new(),
            stmt_nested_dur: std::time::Duration::ZERO,
            synthetic_static_init_fns: Vec::new(),
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
        for (mangled_name, &type_id) in self.type_mapper.iter_named() {
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

            let lookup_ctx = LookupCtx {
                lookup_type_by_name: &|name: &str| self.type_mapper.lookup_named(name),
                owned_string_type: self.type_mapper.owned_string_type,
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
                let method_params = (method.params)(&type_args, &lookup_ctx);
                let ret = (method.return_type)(&type_args, &lookup_ctx);
                (fn_key, method_params, ret, method.runtime_callee, method.self_conv)
            }).collect();

            for (fn_key, method_params, ret, runtime_callee, self_conv) in method_entries {
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
                    use crate::ir::lowering::builtins::SelfConvention;
                    self.runtime_callees.insert(fn_key, crate::ir::RuntimeCalleeInfo {
                        name: callee.to_string(),
                        self_by_ptr: matches!(self_conv, SelfConvention::Borrow | SelfConvention::MutBorrow),
                    });
                }
            }
        }
    }

    /// Populate only the runtime_callees table from the protocol (not fn_sigs).
    /// Called at startup; fn_sigs is populated on-the-fly by resolve_builtin_method_return_type.
    pub fn register_builtin_runtime_callees(&mut self) {
        use crate::ir::lowering::builtins::{self, SelfConvention};

        for (mangled_name, &_type_id) in &self.type_mapper.named_snapshot() {
            if let Some(protocol) = builtins::protocol_for_mangled_name(mangled_name) {
                for method in protocol.methods {
                    if let Some(callee) = method.runtime_callee {
                        let fn_key = format!("{mangled_name}__{}", method.name);
                        self.runtime_callees.entry(fn_key).or_insert_with(|| crate::ir::RuntimeCalleeInfo {
                            name: callee.to_string(),
                            self_by_ptr: matches!(method.self_conv, SelfConvention::Borrow | SelfConvention::MutBorrow),
                        });
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

        let type_mapper = &self.type_mapper;
        let lookup_ctx = LookupCtx {
            lookup_type_by_name: &|name: &str| self.type_mapper.lookup_named(name),
            owned_string_type: self.type_mapper.owned_string_type,
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

        // Populate fn_sigs for future lookups. The self-param's pointer
        // kind reflects the method's `self_conv`: an immutable borrow
        // (`Borrow`) registers as `Ptr(T)`, a mutable borrow
        // (`MutBorrow`) as `MutPtr(T)`, by-value (`ByValue`) as the
        // type itself, and `Static` has no self param. This matters
        // because `lower_method_call`'s `needs_mut` check reads back the
        // first-param's pointer kind to decide whether to call
        // `cow_before_mutation` — which materializes (clones + rebinds
        // the variable name) the receiver before the call. Treating
        // every method as MutPtr conservatively triggered that
        // materialization on non-mutating methods (e.g. `substring`,
        // `slice`), and the rebind then leaked across control-flow
        // merges, causing later reads of the same name to come from a
        // local that was only initialized in one branch.
        let fn_key = format!("{type_name}__{method_name}");
        if !self.fn_sigs.contains_key(&fn_key) {
            let method_params = (method.params)(&type_args, &lookup_ctx);
            use crate::ir::lowering::builtins::SelfConvention;
            let mut params: Vec<TypeId> = match method.self_conv {
                SelfConvention::Borrow => vec![
                    self.type_registry.insert(crate::ir::types::GirType::Ptr(self_type)),
                ],
                SelfConvention::MutBorrow => vec![
                    self.type_registry.insert(crate::ir::types::GirType::MutPtr(self_type)),
                ],
                SelfConvention::ByValue => vec![self_type],
                SelfConvention::Static => vec![],
            };
            params.extend(method_params);
            self.fn_sigs.insert(fn_key.clone(), (params, ret));
        }

        // Populate runtime_callees
        if let Some(callee) = method.runtime_callee {
            use crate::ir::lowering::builtins::SelfConvention;
            self.runtime_callees.entry(fn_key).or_insert_with(|| crate::ir::RuntimeCalleeInfo {
                name: callee.to_string(),
                self_by_ptr: matches!(method.self_conv, SelfConvention::Borrow | SelfConvention::MutBorrow),
            });
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

    /// Typed Option/Result combinator kind for `type_name::method_name`, if any.
    ///
    /// Reads [`BuiltinMethodDecl::combinator_kind`] from the protocol table —
    /// the single source of truth. Never reconstructs meaning from the method
    /// name string (layering rule 2 / Core #2). `None` for non-combinators and
    /// for Vector/Dict/Set HOFs that share names like `map`/`flat_map`.
    pub fn builtin_combinator_kind(
        &self,
        type_name: &str,
        method_name: &str,
    ) -> Option<crate::ir::lowering::builtins::CombinatorKind> {
        use crate::ir::lowering::builtins;
        builtins::protocol_for_mangled_name(type_name)
            .and_then(|p| p.methods.iter().find(|m| m.name == method_name))
            .and_then(|m| m.combinator_kind)
    }

    /// G3: true when the builtin method `type_name::method_name` is a `.clone()`
    /// (deep clone dispatched to `gorget_array_clone`/`gorget_map_clone`/
    /// `gorget_set_clone`). Mirrors [`Self::builtin_returns_view`] — reads the
    /// typed `BuiltinMethodDecl::is_clone` accessor from the protocol table (the
    /// source of truth), never the resolved runtime symbol. The generic-dispatch
    /// clone Call is tagged `ExplicitUserClone` when this is true.
    pub fn builtin_method_is_clone(&self, type_name: &str, method_name: &str) -> bool {
        use crate::ir::lowering::builtins;
        if let Some(protocol) = builtins::protocol_for_mangled_name(type_name) {
            protocol.methods.iter()
                .find(|m| m.name == method_name)
                .map_or(false, |m| m.is_clone())
        } else {
            false
        }
    }

    /// Emit an implicit clone warning for a resource type being auto-cloned.
    ///
    /// The clone-emit site already knows the type being cloned; this is the
    /// one chokepoint where the four diagnostic fields are populated:
    /// `id` from a monotonic counter, `type_name` demangled from the registry,
    /// `runtime_fn` from `clone_fn_for_ptr` (same typed metadata the lowering
    /// uses to emit the call — no name-matching), and `size_bytes` from a
    /// handle-size table keyed on the runtime function (which already encodes
    /// the type's storage category).
    pub fn warn_implicit_clone(
        &mut self,
        span: crate::span::Span,
        type_id: TypeId,
        reason: crate::ir::ImplicitCloneReason,
    ) -> crate::ir::types::CloneId {
        let type_name = self.type_registry.type_name(type_id)
            .map(|n| demangle_type_name(&n))
            .unwrap_or_else(|| "unknown".to_string());
        // The clone-emit dispatch routes through `clone_fn_for_ptr` — call it
        // here so the diagnostic carries the *same* runtime function name the
        // lowering actually emits. Strips the leading `&` indirection when the
        // type_id is itself a Ptr/MutPtr (the warning is conceptually about
        // the pointee being cloned).
        let inner = match self.type_registry.get(type_id) {
            Some(crate::ir::types::GirType::Ptr(t)) => *t,
            Some(crate::ir::types::GirType::MutPtr(t)) => *t,
            _ => type_id,
        };
        let runtime_fn = self.clone_fn_for_ptr(inner).unwrap_or_default();
        let size_bytes = clone_handle_size_for_runtime_fn(&runtime_fn);
        let id = crate::ir::types::CloneId(self.next_clone_id);
        self.next_clone_id = self.next_clone_id.wrapping_add(1);
        self.implicit_clone_warnings.push(crate::ir::ImplicitCloneWarning {
            id,
            span,
            type_name,
            reason,
            size_bytes,
            runtime_fn,
        });
        id
    }

    /// Per-site runtime attribution for `--clones=stats`: emit a
    /// `__gorget_clone_site_hit(<id>)` counter bump into the current block,
    /// immediately before the clone call the caller is about to emit. The
    /// runtime definition (counter table + atexit `[clone-site]` report) is
    /// emitted by the C backend when `LirModule::clone_stats` is set. When
    /// `clone_stats` is off this emits nothing — the instrumented and
    /// uninstrumented builds are byte-identical.
    pub fn emit_clone_site_hit(
        &self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        id: crate::ir::types::CloneId,
    ) {
        if !self.clone_stats { return; }
        builder.call_extern_void(
            "__gorget_clone_site_hit",
            vec![Operand::Constant(Constant::I64(id.0 as i64))],
        );
    }

    /// Place-resolver fall-through chokepoint (`--resolvers`). Worklist only —
    /// never changes resolve results. No-op when the instrument is off (the
    /// shape walk is skipped entirely).
    #[inline]
    pub fn resolver_miss(
        &mut self,
        id: super::resolver_diag::ResolverId,
        expr: Option<&Expr>,
        reason: super::resolver_diag::MissReason,
        span: Option<crate::span::Span>,
    ) {
        if !self.resolver_diag.enabled() {
            return;
        }
        let shape = match expr {
            Some(e) => super::resolver_diag::expr_shape_chain(
                e,
                super::resolver_diag::SHAPE_MAX_DEPTH,
            ),
            None => "LoweredOperand".to_string(),
        };
        self.resolver_diag.bump(id, shape, reason, span);
    }

    /// Like [`Self::resolver_miss`] but tags a pre-built shape (G4 Operand path
    /// when the AST object was already threaded as a shape string).
    #[inline]
    pub fn resolver_miss_shape(
        &mut self,
        id: super::resolver_diag::ResolverId,
        shape: String,
        reason: super::resolver_diag::MissReason,
        span: Option<crate::span::Span>,
    ) {
        if !self.resolver_diag.enabled() {
            return;
        }
        self.resolver_diag.bump(id, shape, reason, span);
    }

    /// Core-invariant #4 producer helper — the ONE way a straight-line
    /// implicit-clone site mints its diagnostic AND its runtime attribution.
    /// Pairs `warn_implicit_clone` (CloneId mint + static Clone-Report row)
    /// with `emit_clone_site_hit` (the `--clones=stats` per-site counter bump,
    /// emitted into the current block immediately before the clone call the
    /// caller is about to emit). Returns the minted `CloneId`.
    ///
    /// Only the three CONDITIONAL clone sites may call the two halves
    /// separately — their clone executes inside a branch, so the hit must be
    /// emitted INSIDE that branch (counting actual clones, not guard
    /// evaluations): the lazy-string materialization guard and the
    /// Ptr-vs-value deref arm (both in this file) and the Option[Ref] payload
    /// lift (`try_lift_option_ref`, `stmts/mod.rs`). Those three are
    /// allowlisted in `tests/lints.rs::clone_warn_hit_pairing`; any other
    /// bare `warn_implicit_clone` call fails that lint.
    pub fn warn_clone_and_hit(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        span: crate::span::Span,
        type_id: TypeId,
        reason: crate::ir::ImplicitCloneReason,
    ) -> crate::ir::types::CloneId {
        let id = self.warn_implicit_clone(span, type_id, reason);
        self.emit_clone_site_hit(builder, id);
        id
    }

    /// G3 producer chokepoint: emit a compiler-inserted clone as ONE call.
    /// Folds the diagnostic (`warn_clone_and_hit` — CloneId mint +
    /// `--clones=stats` runtime hit + the static Clone-Report row) with the
    /// tagged clone call (`builder.call_clone`, whose emitted
    /// `Instruction::Call` carries the typed `reason` so the clone-reason
    /// validator identifies it without name-matching the callee).
    ///
    /// Use at every STRAIGHT-LINE clone site where the warn and the call are
    /// adjacent and unconditional and share one `type_id` (the pointee being
    /// cloned == the clone call's return type). Sites that split warn/call
    /// across a branch (the lazy-string guard, the Ptr-vs-value deref arm,
    /// `try_lift_option_ref`), that interleave drop/ownership bookkeeping
    /// between warn and call, or that warn only under `if let Some(span)`
    /// keep their `warn_clone_and_hit`/`warn_implicit_clone` where it is and
    /// call `builder.call_clone(&fn, args, ty, reason)` directly. The
    /// explicit-`.clone()` paths (which must NOT warn) also call
    /// `builder.call_clone` directly, with `ExplicitUserClone`.
    ///
    /// The internal `warn_clone_and_hit` spelling is uncounted by
    /// `tests/lints.rs::clone_warn_hit_pairing` (which counts the bare
    /// `.warn_implicit_clone(` / `.emit_clone_site_hit(` markers), so folding
    /// a straight-line site into `emit_clone` leaves that allowlist balanced.
    pub fn emit_clone(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        clone_fn: &str,
        args: Vec<Operand>,
        span: crate::span::Span,
        type_id: TypeId,
        reason: crate::ir::ImplicitCloneReason,
    ) -> crate::ir::types::LocalId {
        self.warn_clone_and_hit(builder, span, type_id, reason);
        builder.call_clone(clone_fn, args, type_id, reason)
    }

}

/// Approximate handle size in bytes for the runtime clone function, used by
/// the `--clones=verbose` diagnostic to give users a sense of how much each
/// site copies. These are the cloned *handles* — the payload is reflected
/// in the runtime function name rather than measured here (a `gorget_array_clone`
/// can be cheap for an empty vector or expensive for a vector of strings;
/// the dynamic cost shows up in the `stats` mode's per-id counter, future work).
///
/// Sizes track the typedefs in `src/backend/c/c_runtime.rs` (see lines ~234ff).
/// The map is keyed on runtime fn name rather than type because the runtime fn
/// already encodes the storage category (Array vs Map vs String, etc.); per
/// the layering doctrine, we read the typed dispatch metadata, not pattern-match
/// on type_name.
pub(crate) fn clone_handle_size_for_runtime_fn(runtime_fn: &str) -> usize {
    match runtime_fn {
        // GorgetArray = {data, cap, len, elem_size, alloc, elem_drop, elem_clone, elem_materialize} = 8 ptrs
        "gorget_array_clone" => 64,
        // GorgetMap (= GorgetSet) = 18 fields, mostly 8-byte ptrs/sizes
        "gorget_map_clone" | "gorget_set_clone" => 144,
        // GorgetString = {data, cap, len, alloc} = 32
        "gorget_string_clone_to_owned" | "gorget_string_cow_materialize" => 32,
        // GorgetClosure = {fn, env} = 16
        "gorget_closure_clone" => 16,
        // Box, Shared, Weak, Channel — small ref-counted handle; let runtime_fn empty fall through to 0.
        "" => 0,
        // User struct/enum `__clone` — handle size is unknown without a TypeRegistry lookup;
        // fall back to 0 (caller's verbose output renders this as a dash).
        _ => 0,
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

    // vector-only-by-design: this is the diagnostic pretty-printer that maps
    // mangled C-form names back to Gorget-form for error messages. Deque
    // does not currently have a Gorget-form pretty-print arm here; adding
    // it is a separate diagnostic-parity task (a `Deque[T]` printer arm)
    // and does NOT belong to this Vector__ site's semantic dispatch. If
    // the diagnostic parity is added later, the arm gets `.or_else` here
    // and this comment can be removed.
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
                //
                // Phase A residual #1, sub-TODO 1b: respect the Callable family
                // local-form invariant. `Callable[T(P)]` at a local declaration
                // must always lower to `GirType::FnPtr` for closure-call dispatch
                // (`__gorget_closure_call_<N>`) at calls.rs:912; the Named form
                // `Callable__GorgetClosure` is reserved for in-collection
                // positions. `map_ast_type_mut` honors this via an explicit skip
                // of the `named_types` lookup at types.rs:149; the same skip
                // belongs here so that an eagerly-registered `Callable__…` Named
                // TypeDef doesn't leak into a local's gir_type via the subs path
                // (whose direct `named_types.get` lookup bypasses the special
                // case). Without this, the local binds to Named, the dispatch
                // at calls.rs falls past both the `UNIT_TYPE` and `FnPtr`
                // branches, and a regular `Call { func: "h" }` is emitted —
                // failing the `@h` undefined-function validator at calls.rs:880.
                let base = name.node.as_str();
                if matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
                    return self.type_mapper.map_ast_type(ty); // None → UNIT_TYPE; caller falls back to map_ast_type_mut → FnPtr
                }
                let mangled = super::types::mangle_generic_name(&name.node, generic_args);
                let resolved = self.resolve_type_name(&mangled);
                if let Some(id) = self.type_mapper.lookup_named(&resolved) {
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
    pub fn is_last_use_at(&self, name: &str, span: crate::span::Span) -> bool {
        if self.func_state.liveness.last_use_spans.is_empty() {
            return false; // No liveness data → conservative (don't move)
        }
        // The NAME is checked, not just the position. Until 2026-08-27 this
        // ignored its name argument and asked only "is some variable's last use
        // at this position", trusting all ~16 call sites to pair a name with
        // that name's own span. A caller that passed an ENCLOSING span got a
        // conservative `false` (harmless); a caller that paired a name with a
        // DIFFERENT variable's last-use position would have got a wrong `true`
        // — a move where a clone was required. Nothing enforced the pairing,
        // so the guarantee is now taken rather than assumed.
        self.func_state
            .liveness
            .last_use_spans
            .get(&span.start)
            .is_some_and(|recorded| recorded == name)
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
        // Phase D4.5 step 5b.4: read directly from `Local.ownership` —
        // setters dual-write the typed field at every call site, so
        // it's the canonical lowering-time state.
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
        // Hot path: every CoW-borrow site at var-decl + method-receiver lowering
        // calls this. The previous shape did `format!("@mut:{}", path)` per call
        // PLUS per-prefix in the ancestor loop — ~3 allocations per call on a
        // typical 2-segment path like `self.data`. The set is `FxHashSet<Rc<str>>`
        // which accepts `&str` lookups via `Borrow<str>`; reuse a single
        // `String` buffer for all markers.
        const PREFIX: &str = "@mut:";
        let mut marker = String::with_capacity(PREFIX.len() + source_path.len());
        marker.push_str(PREFIX);
        marker.push_str(source_path);
        if set.contains(marker.as_str()) {
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
        // Walk every STRICT prefix of the path and check for @mut:{prefix}.
        // Buffer is reused: truncate back to PREFIX and rebuild incrementally.
        marker.truncate(PREFIX.len());
        let mut parts = source_path.split('.').peekable();
        while let Some(part) = parts.next() {
            // Skip the full path — already checked above (the last iteration
            // would equal `marker == "@mut:<full path>"`).
            if parts.peek().is_none() {
                break;
            }
            if marker.len() > PREFIX.len() {
                marker.push('.');
            }
            marker.push_str(part);
            if set.contains(marker.as_str()) {
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
        self.call_tracked_impl(builder, func, args, return_type, None)
    }

    /// Emit an **indirect-dispatch** call and register its result for drop at
    /// the value's birth (Core #3), exactly as [`Self::call_tracked`] does for
    /// a statically-resolved one.
    ///
    /// **THE CLASS this chokepoint owns** — every call the lowering emits
    /// whose *callee is selected at run time*, plus the sibling arms of the
    /// helpers that emit them:
    ///
    /// * a closure environment's call thunk — `__Closure_N__call`,
    ///   `__gorget_closure_call_N`;
    /// * a `Callable[T]` parameter slot — `__callable_N`;
    /// * a trait-object vtable slot — `Box__Trait__method`;
    /// * the `FuncRef` arm of [`call_closure_in_adapter`], which shares the
    ///   two above inside one helper (helper-scoped, not dispatch-scoped: the
    ///   defect is *"a write site mints a droppable owned result and never
    ///   registers it"*, and a statically-named callee exhibits it too —
    ///   `5372d443` fixed exactly such a member of this class by
    ///   register-at-birth).
    ///
    /// The result of such a call is a freshly materialized owned value, no
    /// different from a direct call's. The direct-call paths have always
    /// routed through `call_tracked`; these arms spelled `builder.call`
    /// instead, so nothing ever registered the result and it leaked — once per
    /// call, i.e. **unbounded inside a loop**.
    ///
    /// ENFORCING GUARD (Core #6/#14 — this comment is not on its own):
    /// `indirect_dispatch_results_registered_at_birth` in `tests/lints.rs`
    /// pins the per-file census of ALL FOUR raw dst-producing spellings
    /// (`builder.call` / `call_clone` / `call_extern` / `call_extern_into`)
    /// under `src/ir/lowering/`, so a NEW arm written the old way fails the
    /// lint even though it is nowhere near this function — whichever of the
    /// four it happens to spell.
    pub fn call_indirect_tracked(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: crate::ir::types::TypeId,
    ) -> crate::ir::types::LocalId {
        self.call_tracked_impl(builder, func, args, return_type, None)
    }

    /// G3: `call_tracked` for a CLONE call — identical drop-registration +
    /// ownership bookkeeping, but the emitted `Instruction::Call` carries the
    /// typed `reason` so the clone-reason validator sees it (the clone
    /// emitters that route through `call_tracked` — explicit `.clone()`
    /// dispatch on user structs/strings and the f-string struct-interpolation
    /// deep clone — need the tag, not a bare `call_tracked`). Tags the
    /// INSTRUCTION only: it does NOT mint an `ImplicitCloneWarning` (these
    /// sites never warned, and `ExplicitUserClone` must stay out of the
    /// Clone-Report per the "N implicit clone(s)" contract).
    pub fn call_tracked_clone(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: crate::ir::types::TypeId,
        reason: crate::ir::ImplicitCloneReason,
    ) -> crate::ir::types::LocalId {
        self.call_tracked_impl(builder, func, args, return_type, Some(reason))
    }

    /// Shared body of `call_tracked` / `call_tracked_clone`: emit the call
    /// (tagged with `reason` when `Some`, via `builder.call_clone`; a plain
    /// `builder.call` otherwise), then register for drop + set ownership.
    fn call_tracked_impl(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        func: impl Into<String>,
        args: Vec<Operand>,
        return_type: crate::ir::types::TypeId,
        reason: Option<crate::ir::ImplicitCloneReason>,
    ) -> crate::ir::types::LocalId {
        let func_name: String = func.into();
        let local = match reason {
            Some(r) => builder.call_clone(&func_name, args, return_type, r),
            None => builder.call(&func_name, args, return_type),
        };
        if self.type_registry.needs_drop(return_type) {
            self.drops.register_local(local, return_type, &self.type_registry);
        }
        // Function call results own their data — safe to Move on return.
        self.set_owned(builder, local);
        // Mark as fresh for user-defined function calls (not in fn_sigs — these
        // have the return clone path ensuring independence) AND for builtin method
        // calls whose runtime callee provably allocates fresh buffers (replace,
        // upper, lower, repeat, pad, join, etc.).
        // Phase D4: typed-only signal — sidecar writer retired.
        if return_type == self.type_mapper.owned_string_type {
            let is_user_fn = !self.fn_sigs.contains_key(func_name.as_str());
            let is_fresh_builtin = self.runtime_callees.get(func_name.as_str())
                .map_or(false, |info| runtime_returns_fresh(&info.name));
            if is_user_fn || is_fresh_builtin {
                self.set_owned_fresh(builder, local);
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
        self.set_owned(builder, local);
        // Mark fresh for extern string functions that provably allocate new buffers.
        // Most runtime string functions return views (Str), but these return owned
        // GorgetString with independent heap data. Driven by the typed
        // `RuntimeSig.returns_fresh` flag (see `runtime_returns_fresh` below).
        // Phase D4: typed-only signal via `set_owned_fresh` — sidecar
        // writer retired in commit b0a962e8.
        if return_type == self.type_mapper.owned_string_type
            && runtime_returns_fresh(&func_name)
        {
            self.set_owned_fresh(builder, local);
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
        arg_spans: Option<Vec<Option<crate::span::Span>>>,
    ) -> LocalId {
        // Round XIX Track N2 Class B (cell E chokepoint): pack
        // `Box[Concrete] → Box[Trait]` into each variant field that expects a
        // trait-box. Core #4 — one producer for all Some/Ok/Error/user-enum
        // arms that route through this chokepoint. Same IR adapter the
        // smart-ptr ctors use; LIR `try_trait_object_construct` remains the
        // decision site.
        {
            let field_type_names: Vec<Option<String>> = self
                .type_registry
                .get_type_def(enum_name)
                .and_then(|td| {
                    if let crate::ir::types::TypeDefKind::Enum(ref edef) = td.kind {
                        edef.variants
                            .iter()
                            .find(|v| v.name == variant_name)
                            .map(|v| {
                                v.fields
                                    .iter()
                                    .map(|f| match self.type_registry.get(f.type_id) {
                                        Some(crate::ir::types::GirType::Named(n)) => {
                                            Some(n.clone())
                                        }
                                        _ => None,
                                    })
                                    .collect()
                            })
                    } else {
                        None
                    }
                })
                .unwrap_or_default();
            for (i, op) in args.iter_mut().enumerate() {
                if let Some(Some(name)) = field_type_names.get(i) {
                    *op = crate::ir::lowering::exprs::pack_trait_object_for_smart_ptr_ctor(
                        self, builder, std::mem::replace(op, Operand::Constant(Constant::Unit)), name,
                    );
                }
            }
        }

        // Snapshot original locals before cloning — we need to know which
        // args were replaced by clones vs consumed directly.
        let originals: Vec<Option<LocalId>> = args.iter().map(|op| {
            if let Operand::Copy(place) = op {
                if place.projections.is_empty() { return Some(place.local); }
            }
            None
        }).collect();

        // Clone resource args that can't be moved into the enum variant.
        // Pass per-arg spans so that owned named locals at their last use can
        // be moved (no clone) rather than unconditionally cloned.
        self.clone_resource_args_for_init(builder, &mut args, arg_spans.as_deref());
        let dst = builder.enum_init(enum_name, variant_name, type_id, args.clone());
        self.set_owned(builder, dst);

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
    ///
    /// Tier 2a Phase 2A: every cloned temp is tagged `FreshOwned` — the
    /// clone's result is a fresh heap allocation that doesn't alias any
    /// other slot. Without the tag the consume-site validator sees
    /// `Untracked` even though the lowering correctly produced a clone.
    /// Mirrors `set_owned_fresh` at every other clone-then-consume site
    /// (e.g. `lower_var_decl_assign_mode`'s `emit_clone_to_owned`).
    pub fn clone_resource_args_for_init(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        args: &mut Vec<Operand>,
        arg_spans: Option<&[Option<crate::span::Span>]>,
    ) {
        for (idx, op) in args.iter_mut().enumerate() {
            if let Operand::Copy(place) = op {
                if place.projections.is_empty() {
                    let local = place.local;
                    let local_type = builder.local_type(local);
                    let maybe_span = arg_spans.and_then(|spans| spans.get(idx)).and_then(|s| *s);

                    // Ptr(resource) — always clone (borrows from someone else's storage)
                    if let Some(inner) = self.pointee_type(local_type) {
                        if self.type_registry.is_resource_type(inner) {
                            if let Some(clone_fn) = self.clone_fn_for_ptr(inner) {
                                if let Some(s) = maybe_span {
                                    self.warn_clone_and_hit(builder, s, inner, crate::ir::ImplicitCloneReason::ConsumingArg);
                                }
                                let cloned = builder.call_clone(&clone_fn,
                                    vec![crate::ir::builder::FunctionBuilder::copy(local)], inner,
                                    crate::ir::ImplicitCloneReason::ConsumingArg);
                                self.drops.register_local(cloned, inner, &self.type_registry);
                                self.set_owned_fresh(builder, cloned);
                                *op = crate::ir::builder::FunctionBuilder::copy(cloned);
                            }
                            continue;
                        }
                    }

                    if self.type_registry.is_resource_type(local_type) {
                        // Already owned unnamed temp — dead by construction, skip (move).
                        if self.is_owned_local(builder, local) && !self.is_named_local(local) {
                            continue;
                        }
                        // Non-owned string views — always clone
                        let is_non_owned_string = self.is_string_type(local_type)
                            && !self.is_owned_local(builder, local);
                        // Borrow params (bare Ptr param) — always clone
                        let is_borrow_param = self.is_bare_param(builder, local);
                        // Untracked locals have unknown ownership — clone conservatively.
                        let is_untracked = matches!(
                            builder.locals.get(local.0 as usize)
                                .map(|l| &l.ownership),
                            Some(crate::ir::LocalOwnership::Untracked)
                        );
                        // Named owned locals: check last-use to decide move vs clone.
                        // If this is the last use (span-confirmed), the value can be
                        // moved into the enum/struct — no clone needed. The post-init
                        // drop-transfer logic (was_cloned=false) unregisters the original
                        // from scope-exit drops, so the enum owns the data exclusively.
                        // Conservative fallback (no span, or not last-use): clone.
                        if self.is_named_local(local) && !is_non_owned_string && !is_borrow_param && !is_untracked {
                            let ownership = builder.locals.get(local.0 as usize)
                                .map(|l| l.ownership.clone())
                                .unwrap_or(crate::ir::LocalOwnership::Untracked);
                            if ownership.is_owned() {
                                if let Some(span) = maybe_span {
                                    if let Some(name) = builder.locals.get(local.0 as usize)
                                        .and_then(|l| l.name_hint.as_deref())
                                    {
                                        if self.is_last_use_at(name, span) {
                                            // Last-use owned named local: move (no clone).
                                            // Source dies here; enum takes ownership.
                                            continue;
                                        }
                                    }
                                }
                                // No span, or not last-use: fall through to clone below.
                            }
                        }
                        let needs_clone = is_non_owned_string
                            || is_borrow_param
                            || self.is_named_local(local)
                            || is_untracked;
                        if needs_clone {
                            // T-A: owning `!` param deref-temp at its single-use last
                            // use MOVES into the enum variant instead of cloning. All
                            // 3 by-value ctor clone sites route through this shared
                            // helper (`maybe_move_owning_param_ctor_temp`) — sibling
                            // site 1 of 3; a 4th site must call it too (Core #4).
                            let move_span = maybe_span.unwrap_or(crate::span::Span { start: 0, end: 0 });
                            if let Some(moved) = self.maybe_move_owning_param_ctor_temp(builder, op, move_span) {
                                *op = moved;
                                continue;
                            }
                            if let Some(clone_fn) = self.clone_fn_for_ptr(local_type) {
                                if let Some(s) = maybe_span {
                                    self.warn_clone_and_hit(builder, s, local_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                                }
                                let ptr_type = self.register_ptr_type(local_type);
                                let ptr = builder.add_local(ptr_type, None);
                                builder.emit_borrow(ptr, crate::ir::instructions::Place::local(local));
                                let cloned = builder.call_clone(&clone_fn,
                                    vec![crate::ir::builder::FunctionBuilder::copy(ptr)], local_type,
                                    crate::ir::ImplicitCloneReason::ConsumingArg);
                                self.drops.register_local(cloned, local_type, &self.type_registry);
                                self.set_owned_fresh(builder, cloned);
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
    ///
    /// ⚠ THE RAW HALF ONLY. Resetting is half of what a body-lowering path owes:
    /// the other half is the five per-function prescans the CoW / auto-move
    /// machinery reads back out of `func_state`. Call
    /// `functions::begin_function_body(ctx, body)` — which does both as ONE
    /// operation — and never this directly. It must keep exactly ONE caller
    /// (that function); the `function_body_prescans_are_centralised` lint in
    /// `tests/lints.rs` pins the count.
    pub(crate) fn begin_function_body_reset(&mut self) {
        self.func_state = FunctionState::default();
        // Also reset per-function subfields on module-wide structs:
        self.spawn.result_locals.clear();
        self.spawn.pending_fn = None;
        self.shared.locals.clear();
        // Planner round 3: the `MaterializePlan` is per-function transient state
        // (directives recorded this function's lowering), so it resets here — in
        // the raw half of the SINGLE universal per-function-body entry every
        // body-lowering path funnels through (`lower_function` /
        // `lower_equip_method` / `lower_generic_function` /
        // `lower_equip_method_with_subs` — which also covers
        // `lower_method_instance`, since it delegates — PLUS the closure
        // (`emit_closure_call_function`), both trait paths, and the four
        // module-loop body paths). One source of truth (devbook/24 r3) rather
        // than a clear scattered across every entry point; closures are drained
        // in a dedicated post-pass (mod.rs P2.4), never mid-enclosing-function,
        // so this never wipes an in-progress plan.
        self.materialize_plan.clear();
    }

    /// Clone the name→local map AND ownership snapshot for save/restore around
    /// nested scopes (if, while, for, match, etc.). See `SavedScope` for semantics.
    /// Phase D4.5 step 5c: snapshot is a dense `Vec` over `builder.locals`,
    /// not a sparse FxHashMap — `Local.ownership` is the live source of truth.
    pub fn save_locals(&self, builder: &crate::ir::builder::FunctionBuilder) -> SavedScope {
        let n = builder.locals.len();
        let pre_save_ownership: Vec<crate::ir::LocalOwnership> = builder.locals.iter()
            .map(|l| l.ownership.clone())
            .collect();
        let pre_save_types: Vec<TypeId> = builder.locals.iter()
            .map(|l| l.type_id)
            .collect();
        SavedScope {
            locals: self.func_state.locals.clone(),
            pre_save_ownership,
            pre_save_types,
            local_id_boundary: n as u32,
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
    /// Other ownership states (Owned, Ref, BareParam, ViewOf) are kept
    /// — they're either pure metadata or reference aliasing that's already
    /// severed by runtime CoW on mutation of the aliased source. `Alias`,
    /// though, is dropped for branch-local locals (matcluster #4): an alias
    /// BOUND in a never-taken branch (`if cond: Vector v5 = v0`) leaves v5's
    /// alias slot NULL on the not-taken path, so a LATER `cow_aliases_of(v0)`
    /// at a merge-point mutation would blind-clone the NULL alias
    /// (`gorget_array_clone(NULL)` → SIGSEGV, both backends). Resetting a
    /// branch-local alias to unowned at scope exit makes `cow_aliases_of` skip
    /// it — no clone of a maybe-uninitialized alias.
    ///
    /// Phase D4.5 step 5c: writes through `builder.locals[i].ownership` —
    /// the FxHashMap snapshot is gone; the typed field IS the live store.
    pub fn restore_locals(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, saved: SavedScope) {
        self.func_state.locals = saved.locals;
        let boundary = saved.local_id_boundary as usize;
        for idx in 0..builder.locals.len() {
            if idx < boundary {
                // Pre-existing local. If type flipped during the scope,
                // keep the current ownership (the in-place type flip
                // implies the ownership was deliberately upgraded);
                // otherwise revert to the saved snapshot.
                let type_flipped = builder.locals[idx].type_id != saved.pre_save_types[idx];
                if !type_flipped {
                    builder.locals[idx].ownership = saved.pre_save_ownership[idx].clone();
                }
            } else {
                // Post-save (branch-local). Drop CollectionElement /
                // FieldPath / CowBorrowPending / Alias / View states so
                // cow_before_field_mutation / cow_aliases_of don't issue a
                // materialise-read (or clone) on a dead slot once we leave the
                // scope. matcluster #4: `Alias(_)` is included — a dead-branch
                // alias bind (`if cond: Vector v5 = v0`) NULLs v5's alias slot
                // on the not-taken path; without this reset a merge-point
                // `v0[i] = x` fires `cow_aliases_of(v0)` and clones the NULL
                // alias (`gorget_array_clone(NULL)` → SIGSEGV, both backends).
                let drop_state = matches!(&builder.locals[idx].ownership,
                    crate::ir::LocalOwnership::Borrowed {
                        origin: crate::ir::BorrowOrigin::CollectionElement(_)
                              | crate::ir::BorrowOrigin::FieldPath(_)
                              | crate::ir::BorrowOrigin::CowBorrowPending
                              | crate::ir::BorrowOrigin::Alias(_),
                        ..
                    } | crate::ir::LocalOwnership::View { .. }
                );
                if drop_state {
                    builder.locals[idx].ownership = crate::ir::LocalOwnership::default();
                }
            }
        }
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
        // Read typed `enum_category` (Phase A) — the dead `name.starts_with`
        // fallback is no longer needed: every Result registration sets it.
        if td.metadata.enum_category != Some(EnumCategory::Result) {
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
                        if let Some(type_id) = self.type_mapper.lookup_named(enum_name.as_str()) {
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
                if let Some(type_id) = self.type_mapper.lookup_named(name.node.as_str()) {
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
    ///
    /// This is the colliding read site: `enum_variants` is a flat
    /// `variant_name -> (enum, variant)` map populated last-write-wins, so when
    /// two enums declare the same variant name (e.g. `Type.TArray` from `ast.gg`
    /// and `CRuntimeType.TArray` from `compiler/data/schema.gg`) the second
    /// registration shadows the first and every `TArray(...)` constructor
    /// resolves to the wrong enum — regardless of the expected type the
    /// typechecker already determined. Prefer `resolve_enum_variant_typed` at
    /// any constructor call site that has an `expected_type` in hand.
    pub fn resolve_enum_variant(&self, name: &str) -> Option<(String, String)> {
        self.enum_variants.get(name).cloned()
    }

    /// Type-aware variant resolution (SSOT — devbook/24 rules 2+4): when
    /// `expected_type` is an enum that *declares* a variant named `name`, return
    /// THAT enum's `(name, variant)` pair, bypassing the last-write-wins
    /// `enum_variants` collision. The typechecker already resolved the
    /// constructor's type into `func_state.expected_type`; honouring it here
    /// means the GIR `EnumInit` carries the correct enum name (and the LIR
    /// struct-id derived from it is correct), instead of a name reconstructed
    /// from a colliding flat map.
    ///
    /// Stricter than the pattern/match side (`stmts/patterns.rs`): it gates on
    /// actual variant membership before preferring the expected type, then falls
    /// back to the flat map only when the expected type does not disambiguate
    /// (no expected type, not an enum, or it does not own this variant).
    pub fn resolve_enum_variant_typed(
        &self,
        name: &str,
        expected_type: Option<TypeId>,
    ) -> Option<(String, String)> {
        if let Some(et) = expected_type {
            // Deref Ptr/MutPtr like the pattern side does, so an expected
            // `&Enum` / `!Enum` still disambiguates.
            let et = match self.type_registry.get(et) {
                Some(GirType::Ptr(i) | GirType::MutPtr(i)) => *i,
                _ => et,
            };
            if let Some(enum_name) = self.type_registry.type_name(et) {
                // Confirm THIS enum actually declares `name` before preferring it.
                let declares = self
                    .type_registry
                    .get_type_def(&enum_name)
                    .and_then(|td| match &td.kind {
                        TypeDefKind::Enum(ed) => Some(ed),
                        _ => None,
                    })
                    .is_some_and(|ed| ed.variants.iter().any(|v| v.name == name));
                if declares {
                    return Some((enum_name, name.to_string()));
                }
            }
        }
        self.resolve_enum_variant(name)
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

    /// Assign `operand` into an EXISTING `dst` local under `mode`, deciding
    /// per the CoW consume-site rule whether the source may be MOVED or must
    /// be CLONED, and pairing a Move-mode assign with the required MoveZero +
    /// mark_moved on the source.
    ///
    /// **The question this helper asks is `owns ∧ dead`, not `is_moved`.**
    /// `is_moved` only answers "has some earlier site already taken this
    /// slot" — it says nothing about whether the source is still READ
    /// downstream. A staging site that moves an owned local which is live
    /// past the staging point produces
    ///
    /// ```text
    /// [Mv] _dst = copy _src
    ///      move_zero _src
    ///      ... copy _src ...     // a real later read
    /// ```
    ///
    /// — and the `move_zero` is elided by the backend whenever drop-tracking
    /// proves it unobservable, so `_dst` and `_src` end up aliasing one heap
    /// buffer: double-free at scope exit, or a use-after-free if the survivor
    /// reallocs. `caller_src_span` is what makes the liveness half answerable
    /// (`source_live_past`); pass the span of the SOURCE EXPRESSION. `None`
    /// means "no liveness information at this call site" and keeps the plain
    /// move-follow-through behaviour.
    ///
    /// The Move-follow-through half is the same pattern `lower_var_decl` uses
    /// at `src/ir/lowering/stmts/mod.rs:1189-1198` (Pattern::Binding) and
    /// `:1236-1245` (Pattern::Tuple).
    ///
    /// The read-side validator for the missing move-follow-through is
    /// `validate_move_follow_through` (`src/ir/validate.rs`); the read-side
    /// validator for the live-source half is the
    /// `StagingMoveIntoOwnedSlot` consume-site class. When this helper
    /// answers correctly neither trips (Core #1: fix at the write site).
    pub fn assign_with_move_follow_through(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        dst: LocalId,
        operand: Operand,
        mode: crate::ir::instructions::AssignMode,
        caller_src_span: Option<crate::span::Span>,
    ) {
        use crate::ir::instructions::{AssignMode, Place};
        if mode == AssignMode::Move {
            if let Some(span) = caller_src_span {
                if let Some(cloned) = self.clone_live_staging_source(builder, dst, &operand, span) {
                    builder.assign_mode(
                        AssignMode::Move,
                        Place::local(dst),
                        crate::ir::builder::FunctionBuilder::copy(cloned),
                    );
                    return;
                }
            }
        }
        builder.assign_mode(mode, Place::local(dst), operand.clone());
        if mode == AssignMode::Move {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                if place.projections.is_empty()
                    && place.local != dst
                    && !self.drops.is_moved(place.local)
                {
                    self.move_zero_and_mark(builder, place.local);
                }
            }
        }
    }

    /// The `dead` half of the consume rule, answered once for every staging
    /// site: if the source of a Move-mode staging assign is still LIVE past
    /// `span`, the move is illegal and the boundary owes a clone. Returns the
    /// fresh clone's local, or `None` when the move is legal (source dead, or
    /// not a bare owned place, or the type has no clone fn).
    ///
    /// Kept beside [`assign_with_move_follow_through`] rather than inlined at
    /// each staging site: the four `Result`-family staging sites
    /// (`emit_result_auto_propagate`, `lower_rethrow_expr`, and both assigns
    /// in `lower_catch_expr`) all asked `is_moved` independently and all got
    /// the same wrong answer (Core #4 — fix the class at the producer).
    fn clone_live_staging_source(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        dst: LocalId,
        operand: &Operand,
        span: crate::span::Span,
    ) -> Option<LocalId> {
        let place = match operand {
            Operand::Copy(p) | Operand::Move(p) => p,
            Operand::Constant(_) => return None,
        };
        if !place.projections.is_empty() || place.local == dst {
            return None;
        }
        // Already taken by an earlier site — there is nothing left to alias.
        if self.drops.is_moved(place.local) {
            return None;
        }
        if !self.source_live_past(operand, span, builder) {
            return None;
        }
        let src_type = builder.local_type(place.local);
        let inner = self.pointee_type(src_type).unwrap_or(src_type);
        let clone_fn = self.clone_fn_for_ptr(inner)?;
        let reason = crate::ir::ImplicitCloneReason::ConsumingArg;
        self.warn_clone_and_hit(builder, span, inner, reason);
        Some(builder.call_clone(
            &clone_fn,
            vec![crate::ir::builder::FunctionBuilder::copy(place.local)],
            inner,
            reason,
        ))
    }

    /// Materialize `operand` into a FRESH addressable local of `inner_type`,
    /// with the same Move-follow-through guard as
    /// [`assign_with_move_follow_through`] (of which this is a thin wrapper
    /// that also allocates the destination).
    ///
    /// Mirrors the self-host's `op_consume` chokepoint pattern (see
    /// `tests/fixtures/self_host_lowerer/lower.gg:2625` — the SH's uniform
    /// ownership-boundary helper that picks OpMove/OpClone/OpCopy per source
    /// and drop-tracking state, with the Move follow-through baked in).
    ///
    /// Use at ownership-crossing boundaries that need a pointer-shaped
    /// destination — Mutex/RwLock/Shared constructors take `&init` for a
    /// sizeof-driven memcpy, so the initializer must live in an addressable
    /// slot before the ctor call. The three resource arms of
    /// `lower_shared_var_decl` all match this shape.
    ///
    /// Caller supplies `mode` — in `lower_shared_var_decl` this is the local
    /// closure `resource_assign_mode(ctx, inner_type)` (Copy for scalar
    /// inners; Move for resource/needs-drop inners).
    pub fn materialize_addressable(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        inner_type: TypeId,
        mode: crate::ir::instructions::AssignMode,
    ) -> LocalId {
        let tmp = builder.add_local(inner_type, None);
        self.assign_with_move_follow_through(builder, tmp, operand, mode, None);
        tmp
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
    ///
    /// Thin wrapper over [`TypeRegistry::clone_fn_name_for_def`]: that
    /// method is the single source of truth for the resolver shape (see
    /// the doc-comment there). Hosting it on the registry lets the
    /// validator (`preceded_by_clone`) reach the same answer without
    /// pulling in `LoweringContext`.
    pub fn clone_fn_for_ptr(&self, inner_type: TypeId) -> Option<String> {
        use crate::ir::types::GirType;
        if let Some(GirType::Named(name)) = self.type_registry.get(inner_type) {
            if let Some(td) = self.type_registry.get_type_def(name) {
                return self.type_registry.clone_fn_name_for_def(td);
            }
        }
        // A closure VALUE has the same runtime representation and the same
        // clone function as the `Callable__GorgetClosure` named type
        // (`builtins.rs:1109/1124/1139/1154` declare
        // `clone_fn: Some("gorget_closure_clone_to_owned")` for all four
        // spellings), but at a LOCAL position it is a `GirType::FnPtr` with no
        // `TypeDef` behind it — so the `Named` lookup above cannot see it.
        // Without this arm the `.clone()` lowering emits
        // `gorget_closure_clone_to_owned` with the wrong argument type
        // ("incompatible type for argument 1"); with it, `Ptr(FnPtr)` also
        // materializes by clone rather than by a 16-byte `DerefLoad` of the
        // env handle. `todo/t0936`.
        if matches!(self.type_registry.get(inner_type), Some(GirType::FnPtr { .. })) {
            return Some("gorget_closure_clone_to_owned".to_string());
        }
        None
    }

    /// The ONE policy for turning a `Ptr(T)` / `MutPtr(T)` operand into a value
    /// an owning destination can hold. Layering rule 3 (one source of truth per
    /// axis): every `Ptr`→owned decision reads THIS accessor. Three sites used
    /// to answer it independently and disagreed — `ensure_owned_at_boundary`
    /// and `ensure_owned_at_consuming_arg` had no deref arm at all (so a
    /// `Ptr(int)` sailed through as a raw address), while `auto_clone_if_ptr`
    /// had one. A caller that re-derives this from `clone_fn_for_ptr` /
    /// `is_resource_type` at its own site is re-opening that divergence.
    ///
    /// Polarity note: the discriminator is the POINTEE, and it covers `Ptr` and
    /// `MutPtr` alike — `pointee_type` already unifies them, while every gate
    /// that hand-rolled the test was `Ptr`-only (the asymmetry recorded as a
    /// double-free at `stmts/mod.rs`'s owning-param carve-out).
    pub(crate) fn ptr_materialization_kind(&self, pointee: TypeId) -> PtrMaterialization {
        if let Some(clone_fn) = self.clone_fn_for_ptr(pointee) {
            return PtrMaterialization::Clone(clone_fn);
        }
        if !self.type_registry.is_resource_type(pointee) {
            // Primitives and plain value structs are Copy-semantics: a by-value
            // load of the pointee is a memcpy at the backend and yields a fully
            // independent owned value.
            return PtrMaterialization::DerefLoad;
        }
        // A resource with no clone fn has no owning representation to produce
        // (`Box[Trait]` at a literal element position is the live example).
        // Passing the pointer through preserves today's behaviour rather than
        // inventing an ownership transfer the type cannot express.
        PtrMaterialization::PassThrough
    }

    /// Emit a by-value load of `Ptr(inner)` into a fresh local. Only valid when
    /// [`Self::ptr_materialization_kind`] answered [`PtrMaterialization::DerefLoad`].
    fn emit_deref_load(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
        inner: TypeId,
    ) -> Operand {
        let deref_place = crate::ir::instructions::Place {
            local,
            projections: vec![crate::ir::instructions::Projection::Deref],
        };
        let tmp = builder.add_local(inner, None);
        builder.assign(
            crate::ir::instructions::Place::local(tmp),
            Operand::Copy(deref_place),
        );
        crate::ir::builder::FunctionBuilder::copy(tmp)
    }

    /// Materialize `operand` for a destination slot AND report the type of the
    /// value produced — in ONE call.
    ///
    /// This inverts the dependency that made the container-literal mint unsound.
    /// The literal arms used to infer their element type from the operand and
    /// materialize it *afterwards*, so a borrow-typed source minted a
    /// `Vector[Ptr(T)]`: 8-byte slots with no element drop/clone/materialize
    /// vtable, into which the (correctly emitted) clone was then stored and
    /// freed. Because the arms now obtain the operand and its type from this
    /// single call, "minted slot type == materialized operand type" holds BY
    /// CONSTRUCTION rather than by two predicates agreeing.
    ///
    /// `slot` says what the destination already knows about itself:
    /// * [`SlotType::Known`] — the slot's DECLARED type (a struct field's
    ///   declared type, a return signature, a `Ptr`/`MutPtr` target). A
    ///   pointer-typed slot is left holding a pointer; anything else
    ///   materializes and the DECLARED type is reported, so a widening
    ///   destination (`Vector[Box[Trait]]` from a `Box[Concrete]` element)
    ///   still gets its declared slot width and its packing adapter.
    /// * [`SlotType::FromOperand`] — the slot does not exist yet; it is minted
    ///   from what this call produces. This is the container-literal case and
    ///   it ALWAYS materializes.
    ///
    /// ⚠ Callers pass `Known` only for a DECLARED type. Where the "destination
    /// type" is itself INFERRED — `infer_collection_element_type` recovers it by
    /// stripping a mangled name and falls back to `I64` — the destination does
    /// not actually know its slot, and passing that guess as `Known` mis-sizes
    /// the slot exactly as the defect this call exists to close. Those sites
    /// pass `FromOperand`.
    pub fn materialize_for_slot(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        arg_expr: &crate::span::Spanned<crate::parser::ast::Expr>,
        slot: SlotType,
        reason: crate::ir::ImplicitCloneReason,
    ) -> (Operand, TypeId) {
        if let SlotType::Known(target) = slot {
            // A pointer-typed destination holds a pointer: materializing into
            // it would store a value where the slot expects an address. This is
            // the gate the six hand-rolled `if !matches!(.., GirType::Ptr(_))`
            // tests were each re-deriving, now asked once.
            if self.pointee_type(target).is_some() {
                return (operand, target);
            }
        }
        let owned = self.materialize_owned_operand(builder, operand, arg_expr, reason);
        let produced =
            crate::ir::lowering::exprs::infer_operand_type_full(self, &owned, builder);
        match slot {
            SlotType::Known(target) => (owned, target),
            SlotType::FromOperand => (owned, produced),
        }
    }

    /// Materialize an operand into an owned value for a non-pointer destination.
    /// Shared body of [`Self::materialize_for_slot`]: the consuming-position
    /// clone-if-live / move-if-dead decision, followed by the `Ptr`→owned policy
    /// from [`Self::ptr_materialization_kind`] for anything still pointer-typed.
    fn materialize_owned_operand(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        arg_expr: &crate::span::Spanned<crate::parser::ast::Expr>,
        reason: crate::ir::ImplicitCloneReason,
    ) -> Operand {
        let owned = self.ensure_owned_at_consuming_arg(builder, operand, arg_expr, reason);
        // `ensure_owned_at_consuming_arg` clones through a pointer whenever the
        // pointee has a clone fn. What it cannot do is DEREF, because it does
        // not know whether its caller's slot is a `Ref[T]` (pass the pointer)
        // or a bare `T` (load the value). Here the slot is known non-pointer,
        // so the load is the right answer and this is the only place that can
        // say so.
        let local = match &owned {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return owned,
        };
        let Some(inner) = self.pointee_type(builder.local_type(local)) else {
            return owned;
        };
        match self.ptr_materialization_kind(inner) {
            PtrMaterialization::DerefLoad => self.emit_deref_load(builder, local, inner),
            // `Clone` was already discharged by `ensure_owned_at_consuming_arg`
            // above; reaching it here would mean the clone did not fire, and
            // passing the pointer on is the pre-existing behaviour.
            PtrMaterialization::Clone(_) | PtrMaterialization::PassThrough => owned,
        }
    }

    /// Return the borrow-view function name for a type whose runtime supports
    /// drop-safe cap=0 views — the typed eligibility read for the lazy
    /// loop-carried CoW bind (`emit_lazy_loopcarried_borrow`). Phase 1:
    /// String only (`gorget_string_borrow_view`); collections cannot join
    /// until their frees are view-aware (`gorget_array_free` runs `elem_drop`
    /// whenever `data != NULL` regardless of cap — a cap=0 array view would
    /// double-drop every element; Dict/Set similar; user structs have no view
    /// discriminator). The metadata axis is `TypeMetadata.borrow_view_fn`
    /// (sibling of `clone_fn`/`materialize_fn`), set once at type
    /// registration — devbook/24 rules 2-3: typed metadata, one source of
    /// truth, no name matching.
    pub fn borrow_view_fn_for(&self, inner_type: TypeId) -> Option<String> {
        use crate::ir::types::GirType;
        if let Some(GirType::Named(name)) = self.type_registry.get(inner_type) {
            if let Some(td) = self.type_registry.get_type_def(name) {
                return td.metadata.borrow_view_fn.clone();
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

    /// T-A (gorget-arena snag #1 ctor extension): the owning-`!`-param carve-out
    /// for the struct/enum **ctor field-init** boundary — the 8th consuming
    /// category snag #1 left cloning. `operand` here is the untracked **deref
    /// temp** that `Expr::Identifier` lowering produced for a bare `!` param
    /// (the param linkage lives on the typed `Local.deref_of_owning_param`
    /// field, one source of truth — no sidecar map). If that temp came from a
    /// non-string owning `!` param whose **single-use last use** this is, MOVE
    /// it instead of the by-value defensive clone: `set_owned` the temp and
    /// `move_zero_and_mark` the param slot (suppressing its exit-drop) so the
    /// recipient ctor becomes the sole owner of the heap buffer. Returns the
    /// (already-Copy) operand on a hit; `None` when not move-eligible (caller
    /// then clones through `clone_fn_for_ptr`). Mirrors snag #1's consuming-arg
    /// carve-out; SAME TWO LANDMINES, both preserved:
    ///   (1) Strings clone via a different path and must NOT be move-zeroed here
    ///       (double-free) — excluded via `is_string_type`;
    ///   (2) `is_single_use` rejects a param reassigned in a loop (`lhs = f(lhs)`),
    ///       where zeroing the reused slot trips the GIR "read after MoveZero"
    ///       validator.
    ///
    /// Centralizes the move decision for all 3 by-value ctor clone sites
    /// (enum-init `clone_resource_args_for_init`, struct-boundary
    /// `ensure_owned_at_boundary` Case 2, user-literal `clone_multi_use_resource_args`)
    /// so a 4th such site added later is forced through the shared helper (Core #4).
    pub(crate) fn maybe_move_owning_param_ctor_temp(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: &Operand,
        span: crate::span::Span,
    ) -> Option<Operand> {
        let temp = match operand {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return None,
        };
        // Was this temp a deref of an owning `!` param? (typed provenance on Local)
        let param = builder.locals.get(temp.0 as usize)?.deref_of_owning_param?;
        let inner = builder.local_type(temp);
        // Strings clone via a different path — excluded (move-zeroing double-frees).
        if self.is_string_type(inner) || !self.type_registry.is_resource_type(inner) {
            return None;
        }
        let param_name = builder.local_name(param)?.to_string();
        // Single-use last use on all paths — otherwise the param is still live.
        if !self.is_last_use_at(&param_name, span) || !self.is_single_use(&param_name) {
            return None;
        }
        // MOVE: the temp already holds the pointee bytes (identifier-lowering
        // deref'd `*param` into it). Own it and zero the param slot so the single
        // exit-drop accountant does not re-drop the transferred buffer.
        self.set_owned(builder, temp);
        self.move_zero_and_mark(builder, param);
        Some(operand.clone())
    }

    pub fn ensure_owned_at_boundary(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        span: crate::span::Span,
        reason: crate::ir::ImplicitCloneReason,
    ) -> Operand {
        // Case 0: `Constant::GlobalRef(name)` for a resource-typed module global.
        //
        // A `String DT_LOCAL = "literal"` global lowers to a heap-allocated
        // `GorgetString` in the program's startup block. Reading it by name
        // (`return DT_LOCAL`, `String s = DT_LOCAL`, …) lowers in LIR to
        // `GlobalAddr` + `Load`, i.e. a shallow byte-copy of the global's
        // struct. That copy aliases the global's heap buffer. If the consumer
        // treats the value as owned (return slot, var binding, struct field
        // init, etc.), the subsequent scope-exit drop frees the global's
        // buffer — and the next read of the global re-frees the same buffer
        // → double-free.
        //
        // The fix: at an ownership boundary, clone the global through its
        // pointer (`GlobalRefPtr`) so the boundary receives a fresh owned
        // allocation independent of the global.
        //
        // For positions that legitimately need a borrow (call args by &/bare
        // pointer, `&GLOBAL` syntax), the `GlobalRef → GlobalRefPtr` rewrite
        // in those code paths short-circuits before this helper runs.
        if let Operand::Constant(Constant::GlobalRef(name)) = &operand {
            // `String FOO = "literal"` globals are cap=0 rodata views. The
            // shallow byte-copy that LIR's GlobalAddr+Load produces aliases
            // immortal `.rodata`, so the consumer's drop is a no-op and no
            // clone is needed. See `lower_static_decl` / `clone_resource_global_ref`.
            if self.string_literal_view_globals.contains(name) {
                return operand;
            }
            let global_type = self.global_type_names.get(name).cloned()
                .and_then(|tn| crate::ir::lowering::exprs::lookup_global_type(self, &tn));
            if let Some(global_ty) = global_type {
                if self.type_registry.is_resource_type(global_ty) {
                    if let Some(clone_fn) = self.clone_fn_for_ptr(global_ty) {
                        self.warn_clone_and_hit(builder, span, global_ty, reason);
                        // Pass &GLOBAL (GlobalRefPtr) to the clone fn — matches
                        // the `gorget_string_clone_to_owned(const GorgetString*)`
                        // / `gorget_array_clone(const GorgetArray*)` etc. ABIs.
                        let cloned = builder.call_clone(
                            &clone_fn,
                            vec![Operand::Constant(Constant::GlobalRefPtr(name.clone()))],
                            global_ty,
                            reason,
                        );
                        self.drops.register_local(cloned, global_ty, &self.type_registry);
                        self.set_owned_fresh(builder, cloned);
                        return crate::ir::builder::FunctionBuilder::copy(cloned);
                    }
                }
            }
            return operand;
        }
        let local = match &operand {
            Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.local,
            _ => return operand,
        };
        let local_type = builder.local_type(local);

        // Case 1: Ptr(T) → clone inner.
        // Cannot move through Ptr: the callee doesn't know if the caller still
        // needs the argument.
        //
        // NOTE: auto-deref for non-resource pointees (Ref[T] → T) does NOT
        // live here because this function doesn't know the target slot's type.
        // Struct field init calls this per-field — when both source and target
        // are `Ref[T]`, we want pass-through, not deref. The Ref[T] → T deref
        // is handled at the sites that know their target is a bare `T`:
        // VarDecl, return statement, expression-body function return, call args
        // (via `auto_clone_if_ptr`), and container-literal element slots (via
        // `materialize_for_slot`, which asks the slot first and only then
        // reaches the shared `DerefLoad` arm).
        if let Some(inner) = self.pointee_type(local_type) {
            if let PtrMaterialization::Clone(clone_fn) = self.ptr_materialization_kind(inner) {
                self.warn_clone_and_hit(builder, span, inner, reason);
                let cloned = builder.call_clone(
                    &clone_fn,
                    vec![crate::ir::builder::FunctionBuilder::copy(local)],
                    inner,
                    reason,
                );
                self.drops.register_local(cloned, inner, &self.type_registry);
                self.set_owned(builder, cloned);
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
        let is_borrow = self.is_ref_local(builder, local)
            || self.is_bare_param(builder, local)
            || self.is_cow_borrow(builder, local);
        // Untracked resource locals have unknown ownership — clone conservatively.
        // The validator requires Owned ownership at consume sites; Untracked always
        // fires unless preceded by a clone. (Tier 2a Phase 2B: close the gap.)
        let is_untracked_resource = matches!(
            builder.locals.get(local.0 as usize).map(|l| &l.ownership),
            Some(crate::ir::LocalOwnership::Untracked)
        ) && self.type_registry.is_resource_type(local_type);
        if !is_borrow && !is_untracked_resource {
            return operand;
        }

        // (A former "Case 2b" sat here: a *last-use* bare-param borrow was
        // supposed to MOVE instead of cloning. It was DEAD CODE — its guard
        // `is_bare_param(l) && !is_ref_local(l)` is unsatisfiable, because
        // `is_bare_param` matches `Borrowed { Param(self), Shared }` and
        // `is_ref_local` is `ownership.is_ref()`, which is true for EVERY
        // `Borrowed` variant. Deleted rather than repaired: had it ever fired
        // it would have `drops.unregister`'d the param and handed the caller's
        // own buffer back across the boundary — manufacturing exactly the
        // return-borrow double-free class this chokepoint exists to prevent.
        // It also contradicted the ratified contract: the move-eligible shapes
        // are `!arg`, expression temps, and named locals at last use — "not
        // from `.get()`, a view-returning method, or a parameter — those bind
        // borrows" (CLAUDE.md, "Ownership at Consuming Positions"). A bare
        // param binds a borrow; the caller keeps ownership, so ONE clone here
        // is the hand-written count, not a charter breach.)

        // T-A: owning `!` resource param deref-temp at its single-use last use
        // MOVES into the struct/enum ctor field instead of the defensive clone
        // (sibling site 2 of 3 — shared `maybe_move_owning_param_ctor_temp`).
        if let Some(moved) = self.maybe_move_owning_param_ctor_temp(builder, &operand, span) {
            return moved;
        }
        if let Some(clone_fn) = self.clone_fn_for_ptr(local_type) {
            self.warn_clone_and_hit(builder, span, local_type, reason);
            let cloned = builder.call_clone(
                &clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(local)],
                local_type,
                reason,
            );
            self.drops.register_local(cloned, local_type, &self.type_registry);
            self.set_owned(builder, cloned);
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
        //
        // The `DerefLoad` answer is deliberately NOT taken here: this helper
        // does not know its caller's slot, and a `Ref[T]` destination must keep
        // the pointer. The sites that DO know reach it through
        // `materialize_for_slot`, which asks the slot before materializing.
        if let Some(inner) = self.pointee_type(arg_type) {
            if let PtrMaterialization::Clone(clone_fn) = self.ptr_materialization_kind(inner) {
                self.warn_clone_and_hit(builder, arg_expr.span, inner, reason);
                let cloned = builder.call_clone(
                    &clone_fn,
                    vec![crate::ir::builder::FunctionBuilder::copy(local)],
                    inner,
                    reason,
                );
                // Register for drops so mark_moved works in pre_call_clone_temps
                self.drops.register_local(cloned, inner, &self.type_registry);
                return crate::ir::builder::FunctionBuilder::copy(cloned);
            }
            return operand;
        }

        // Case 2: by-value resource OR refcount handle. Refcount handles
        // (Shared/Weak/Channel) are NOT `is_resource_type` (thin-pointer,
        // Trivial copy) but still need clone-if-live — their `clone_fn` is a
        // by-value incref, which the by-value `clone_fn_for_ptr` call at the
        // tail emits correctly (SCOUT-PROTO #1b, Defect B).
        if !self.type_registry.is_resource_type(arg_type)
            && !self.type_registry.is_refcount_clone_type(arg_type)
        {
            return operand;
        }
        // Determine if a clone is needed.  Two sub-cases:
        //   (a) Named identifier arg — check last-use + borrow state.
        //   (b) Non-identifier / non-named-local — expression temp, always
        //       last-use by construction (the temp was just created).
        // Centralized owning-`!`-param carve-out: when a non-string owning `!`
        // resource param is forwarded into a consuming position at its
        // single-use last use, it MOVES (not clones). Record the PARAM slot so
        // the single exit-drop accountant is suppressed uniformly at every
        // shared-helper caller site (push/put/set/insert, index-set,
        // dict-assign, field-assign, Box ctor/.new) via one `move_zero`.
        let mut owning_param_move_src: Option<LocalId> = None;
        let needs_clone = if let Expr::Identifier(ref name) = arg_expr.node {
            if self.is_named_local(local) {
                let is_borrow = !self.drops.is_registered(local)
                    || self.is_bare_param(builder, local)
                    || self.is_ref_local(builder, local)
                    || self.is_cow_borrow(builder, local);
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
                        // Owning `!` resource param (non-string) at its
                        // single-use last use → MOVE. `is_single_use` guards
                        // against a param reassigned in a loop (`lhs = f(lhs)`),
                        // where move-zeroing the reused slot would trip the GIR
                        // "read after MoveZero" validator. Strings clone via a
                        // different path and must NOT be move-zeroed here.
                        let is_owning_param = (src_local.0 as usize) < builder.locals.len()
                            && builder.locals[src_local.0 as usize].is_owning_param
                            && !self.type_mapper.is_string_type(inner);
                        if is_owning_param
                            && self.is_last_use_at(name, arg_expr.span)
                            && self.is_single_use(name)
                        {
                            owning_param_move_src = Some(src_local);
                            false
                        } else {
                            let is_borrow = !self.drops.is_registered(src_local)
                                || self.is_bare_param(builder, src_local)
                                || self.is_ref_local(builder, src_local)
                                || self.is_cow_borrow(builder, src_local);
                            is_borrow || !self.is_last_use_at(name, arg_expr.span)
                        }
                    } else { false }
                } else { false };
                result
            }
        } else {
            // Expression temp: last-use by construction. Track B (Round XXXII
            // MEMORY SAFETY / ONE OWNERSHIP BOUNDARY) restored the View-tag
            // check here so a View-tagged temp (Guard.get and family) clones
            // at the boundary instead of memcpying as a shallow alias into
            // the consumer — fixes the `guard_get_into_dict_put_double_free`
            // class.
            //
            // ⚠ NARROWED at round-close 2026-08-06: the identifier arm's
            // 4-clause predicate (`!drops.is_registered`, `is_bare_param`,
            // `is_ref_local`, `is_cow_borrow`) FALSE-POSITIVES on fresh owned
            // temps that just haven't been drop-registered yet (vector
            // literals, method-call returns, ctor calls) — reverted at
            // round-close because it caused runtime double-frees on
            // `collection_bare_none_value` and over-cloning on a dozen other
            // fixtures. The two clauses that actually catch View-alias temps
            // are `is_ref_local` (typed `LocalOwnership::View`) and
            // `is_cow_borrow` (typed CoW-borrow marker) — both read from
            // ONE source of truth in typed metadata, no drop-accountant
            // dependency. `!drops.is_registered`/`is_bare_param` remain in
            // the identifier arm above where a named local has known origin;
            // for a temp they add no signal beyond ownership-typed state.
            self.is_ref_local(builder, local) || self.is_cow_borrow(builder, local)
        };
        if !needs_clone {
            if let Some(src) = owning_param_move_src {
                // Move: the deref temp now owns the value; move-zero the param
                // slot so the exit-drop accountant does not re-drop it.
                self.set_owned(builder, local);
                self.move_zero_and_mark(builder, src);
            }
            return operand;
        }
        if let Some(clone_fn) = self.clone_fn_for_ptr(arg_type) {
            self.warn_clone_and_hit(builder, arg_expr.span, arg_type, reason);
            let cloned = builder.call_clone(
                &clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(local)],
                arg_type,
                reason,
            );
            // Deliberately NOT `drops.register_local`'d (unlike Case 1's
            // Ptr→T materialization above). EVERY caller of this helper is a
            // CONSUMING position — push/put/set/insert/send, container literal,
            // ctor field-init, index-set, var-decl RHS — that immediately takes
            // ownership of the returned value (moves/memcpys it into the
            // collection slot / struct field / destination local, whose OWN
            // drop then balances it). Registering this transient clone as a
            // standalone droppable would double-drop the very allocation the
            // consumer now owns. For a refcount handle the same holds: the
            // clone is a by-value incref and the destination's field/elem drop
            // provides the single matching decref (verified ASan-clean for the
            // Shared/Weak ctor + array/dict-literal consuming shapes). Case 1
            // registers only because a materialized Ptr→T is a fresh STANDALONE
            // value whose `mark_moved` state `pre_call_clone_temps` tracks.
            //
            // Class-retiring guard (Core #6, Round MEMORY SAFETY / ONE
            // OWNERSHIP BOUNDARY Track B): the returned local MUST differ
            // from the input — a borrow that reached needs_clone=true but
            // came out with the same local id is exactly the view-into-
            // consumer escape hatch this fix retired. `call_clone` mints a
            // fresh local, so this fires only if a future refactor
            // short-circuits the clone.
            debug_assert_ne!(
                cloned, local,
                "ensure_owned_at_consuming_arg: needs_clone was true \
                 but the returned operand shares the input's local id — \
                 the borrow source would escape to the callee without \
                 materialization. Round MEMORY SAFETY / ONE OWNERSHIP \
                 BOUNDARY Track B fix contract; see \
                 tests/security/guard_get_into_dict_put_double_free.gg \
                 and tests/lints.rs::view_producer_into_consuming_cell_has_coverage."
            );
            return crate::ir::builder::FunctionBuilder::copy(cloned);
        }
        // Fall-through: `needs_clone` was true (borrow-detection predicate
        // fired) but the type has no `clone_fn_for_ptr`. This is the pre-
        // Track-B behavior for shapes where no clone_fn exists (e.g.
        // `Box[Trait]` at a vector-literal consuming position, verified
        // 2026-08-06 at round-close by `box_trait_vector_lit`). Preserve
        // that behavior — return the operand unchanged; the pre-Track-B
        // path handled these without a clone and did not silently miscompile
        // on them either. Track B's specific target (View-tagged Guard temps)
        // reaches the clone path above; this fall-through is for the
        // long-tail shapes where the else-arm's `!drops.is_registered`
        // predicate fires on a not-yet-classified local.
        //
        // ⚠ An earlier debug_assert here (v1 output-review fold) tripped on
        // legitimate corpus tests. Filed as a follow-up: audit whether the
        // else-arm predicate is too broad (`!drops.is_registered` catches
        // not-yet-registered as well as actual borrows); ideal fix is to
        // narrow the predicate to actual-borrow-only shapes, retiring the
        // debug_assert with it.
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
                    //
                    // ⚠ This carve-out is a CALL-ARG policy (CoW's
                    // default-borrow at a plain call), NOT part of the
                    // `Ptr`→owned answer, so it sits ABOVE the shared accessor
                    // rather than inside it. Deleting it does NOT change any
                    // test's output — `ptr_materialization_kind` answers
                    // `Clone` for `String` — it changes how much the program
                    // ALLOCATES, which no pass/fail gate can see. Measured on
                    // `tests/fixtures/cli_basic.gg` with `--clones=stats`:
                    // `string_clone` 37 -> 43 and `total_allocs` 64 -> 70 with
                    // this branch removed. It is live, not vestigial: a direct
                    // `gg build` census over the first 300 fixtures records
                    // 1980 calls to this function, 241 reaching this `Ptr`
                    // branch, and 123 taking this early return.
                    //
                    // ⚠ Instrument note for whoever revisits this: the
                    // integration harness SWALLOWS `gg`'s stderr, so counting
                    // an `eprintln!` probe from `cargo test` reports zero for
                    // every branch and reads as "dead code". Count it by
                    // invoking `gg build` directly, and use a positive control
                    // — this branch was twice concluded dead from a suite-level
                    // zero that measured nothing at all.
                    //
                    // It IS a type-identity test standing in for typed
                    // metadata; replacing it with a `TypeMetadata` flag is
                    // filed, not done here, because the replacement is a
                    // behaviour-preserving refactor and this is a
                    // memory-safety fix.
                    if self.type_mapper.is_string_type(inner) {
                        return operand;
                    }
                    match self.ptr_materialization_kind(inner) {
                        PtrMaterialization::Clone(clone_fn) => {
                            self.warn_clone_and_hit(builder, span, inner, crate::ir::ImplicitCloneReason::CallArg);
                            let cloned = builder.call_clone(
                                &clone_fn,
                                vec![crate::ir::builder::FunctionBuilder::copy(place.local)],
                                inner,
                                crate::ir::ImplicitCloneReason::CallArg,
                            );
                            self.drops.register_local(cloned, inner, &self.type_registry);
                            self.set_owned(builder, cloned);
                            return crate::ir::builder::FunctionBuilder::copy(cloned);
                        }
                        // Ptr to a non-resource, non-string value type — e.g. reading a
                        // `Ref[int]` (from `v.get()` / a `Ref[T]` field) where the callee
                        // expects `int`. Deref to load the pointee value. Primitives are
                        // scalars; simple user value structs are Copy-semantics, so a
                        // by-value load is just a memcpy at the backend.
                        PtrMaterialization::DerefLoad => {
                            let local = place.local;
                            return self.emit_deref_load(builder, local, inner);
                        }
                        PtrMaterialization::PassThrough => {}
                    }
                }
            }
        }
        operand
    }

    // ── Unified ownership state helpers ──────────────────────────────

    /// Check if a local is tracked as a borrowed Ptr reference.
    /// Phase D4.5 step 5b: reads `Local.ownership` (the typed field on
    /// the GIR `Local`). `Untracked` returns `false`, mirroring the
    /// legacy FxHashMap-absent semantic.
    pub fn is_ref_local(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        builder.locals[idx].ownership.is_ref()
    }

    /// Check if a local is tracked as definitely owning its data.
    /// Phase D4.5 step 5b: reads `Local.ownership`. Both `Owned` and
    /// `FreshOwned` own their data — fresh is the strictly-stronger
    /// sub-axis. `Untracked` returns `false`, mirroring the legacy
    /// FxHashMap-absent semantic.
    pub fn is_owned_local(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        builder.locals[idx].ownership.is_owned()
    }

    /// Mark a local as owning its data. Overwrites any previous state.
    /// Phase D4.5 step 5d: writes through to `builder.locals[id].ownership`.
    /// The legacy `func_state.local_ownership: FxHashMap` was retired —
    /// the typed field on `Local` is the sole live store.
    pub fn set_owned(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Owned;
        }
    }

    /// Mark a local as owning a freshly-allocated buffer (no aliasing).
    /// Strictly stronger than `set_owned`. The return-clone elision and
    /// self-referential reassign guard rely on this stronger fact.
    pub fn set_owned_fresh(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::FreshOwned;
        }
    }

    /// Drop ownership tracking for a local. Resets `builder.locals[id].ownership`
    /// to the default (`Untracked`), preserving the legacy FxHashMap
    /// "absence == no entry" semantic.
    pub fn unset_ownership(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::default();
        }
    }

    /// Untrack EVERY transient element/field-path borrow handle minted in the
    /// local range `[start, end)` — the handles `lower_expr` creates while
    /// lowering a PROJECTED store-target object (`v[i].field = x`,
    /// `m[i][j][k] = x`, `s.grid[i][j].field = x`).
    ///
    /// Each such handle is TRANSIENT: it exists only to name the store LOCATION
    /// and is DEAD once the assign statement completes — NOT a live borrow that
    /// outlives the statement. `lower_index_access` tags EVERY index-load in a
    /// multi-level chain as a live `CollectionElement`/`FieldPath` borrow into
    /// its base (`m[i]` → h1 into `m`; `m[i][j]` → h2 into h1). Leaving ANY of
    /// them CoW-tracked lets a later same-collection mutation (`m.push()` in a
    /// loop) hit `cow_before_mutation` Case 3 ("clone each ref into the
    /// collection"); once the G1 projected-root materialize has replaced the
    /// collection with a private owned copy, the first push reallocates that
    /// copy's buffer and the cloned handle dangles → heap-use-after-free (both
    /// backends). Resetting only the OUTERMOST operand (the pre-a84e66bb fix)
    /// closed depth-1 but left the INTERMEDIATE handles dangling at depth >= 2;
    /// this closes the whole class (Core #4) by resetting the ENTIRE chain.
    ///
    /// The callers pass the range spanning the ENTIRE assign statement — the
    /// store-target OBJECT chain AND the RHS value / index expression — because
    /// an RHS/index element-ref into the SAME collection the store
    /// root-materialized (`v[0].name = v[1].name`, `m[0][0] = m[1][0]`) is minted
    /// into the private copy too and dangles identically. They untrack only AFTER
    /// the store's `ensure_owned_at_consuming_arg` (or `clone_ptr_rhs_if_needed`)
    /// has cloned the stored value, so every element/field-path handle left in
    /// range is a dead READ ref, safe to reset in BOTH same- and
    /// different-collection cases.
    ///
    /// Range-safe: the projected-store branch always lowers a NON-identifier
    /// object, so every element/field-path handle minted in `[start, end)` is an
    /// anonymous transient index-load dst (`add_local(_, None)` — no name hint).
    /// The `local_name(..).is_none()` guard additionally spares any named binding
    /// in range as defense in depth (a named binding always carries a name hint —
    /// e.g. a `T r = coll.get(i)` element bind the surrounding code still reads).
    /// Mirrors `restore_locals`, which drops the identical states for scope-local
    /// handles. Store-neutral (the store uses the Place, not the ownership tag).
    pub fn untrack_transient_element_refs_in_range(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        start: usize,
        end: usize,
    ) {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let end = end.min(builder.locals.len());
        for idx in start..end {
            let local = crate::ir::types::LocalId(idx as u32);
            let is_transient = matches!(
                &builder.locals[idx].ownership,
                LocalOwnership::Borrowed {
                    origin: BorrowOrigin::CollectionElement(_)
                          | BorrowOrigin::FieldPath(_)
                          | BorrowOrigin::CowBorrowPending,
                    ..
                }
            );
            // Skip any handle that carries a name hint — an anonymous transient
            // (the shape lower_index_access mints) never does.
            if is_transient && builder.local_name(local).is_none() {
                builder.locals[idx].ownership = LocalOwnership::default();
            }
        }
    }

    /// Check if a local's string data is a fresh allocation not shared with any
    /// other variable. True only for direct function/extern call results that
    /// return the owned string type. Phase D4.5 step 5b.2: reads
    /// `Local.ownership.is_fresh()` directly.
    pub fn is_fresh_string(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        builder.locals[idx].ownership.is_fresh()
    }

    /// Check if a local has been borrowed-from via the `String b = a`
    /// shallow-copy path. If true, another local shares its heap data
    /// → clone needed on return. Phase D4.5 step 5b.3: iterates
    /// `builder.locals` directly through the typed `Local.ownership`
    /// field (the legacy FxHashMap was retired in step 5d).
    pub fn has_string_borrowers(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::LocalOwnership;
        builder.locals.iter().any(|l| matches!(
            &l.ownership,
            LocalOwnership::SharedHeap { source } if *source == local
        ))
    }

    /// Mark a local as a value-aliasing shallow copy of `source` (the
    /// `String b = a` shape). SharedHeap flushes to a Value-typed slot
    /// (same as Owned) — SlotKind/ABI routing keeps the 32-byte struct
    /// layout intact — but participates in
    /// `shared_heap_aliases_of_source(source)` so source mutation can
    /// invalidate the alias tag, and in `has_string_borrowers(source)`
    /// so return paths know to clone.
    pub fn set_shared_heap(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, source: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::SharedHeap { source };
        }
    }

    /// Reset all callable-return-type tracking. Called at function-boundary
    /// entry; per-function transient state.
    pub fn callable_return_types_clear(&mut self) {
        self.func_state.callable_return_types.clear();
        self.func_state.callable_param_types.clear();
        self.func_state.callable_param_ownerships.clear();
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

    /// Record the ARGUMENT types + ownerships of a callable-typed local. Plain
    /// inner TypeIds (no MutPtr wrap) — matches the direct-call `fn_sigs` shape.
    /// The `Ownership` per index (`MutableBorrow` for `&`, `Move` for `!`,
    /// `Borrow` otherwise) is the second sidecar the indirect-call arg-emit
    /// loops feed to `lower_call_arg` as if they were a direct call.
    pub fn set_callable_param_types(
        &mut self,
        local: LocalId,
        param_types: Vec<TypeId>,
        param_ownerships: Vec<crate::parser::ast::Ownership>,
    ) {
        self.func_state.callable_param_types.insert(local, param_types);
        self.func_state.callable_param_ownerships.insert(local, param_ownerships);
    }

    /// Look up the recorded ARGUMENT types for a callable-typed local.
    pub fn callable_param_types(&self, local: LocalId) -> Option<&[TypeId]> {
        self.func_state.callable_param_types.get(&local).map(|v| v.as_slice())
    }

    /// Look up the recorded ARGUMENT ownerships for a callable-typed local.
    pub fn callable_param_ownerships(&self, local: LocalId) -> Option<&[crate::parser::ast::Ownership]> {
        self.func_state.callable_param_ownerships.get(&local).map(|v| v.as_slice())
    }

    /// Record ALL THREE callable sidecars for a callable-typed local at ONE
    /// call. Layering-discipline chokepoint (Core #4/#6): the three sidecars
    /// (return type, param types, param ownerships) are one logical fact — a
    /// call site that populates only some is a parallel-population defect the
    /// indirect-call arg-emit path would silently mis-route on. Every
    /// function-registration path (`src/ir/lowering/functions.rs` — 4 sites)
    /// and the `lower_var_decl` Binding arm (`src/ir/lowering/stmts/mod.rs`)
    /// call this instead of writing the three sidecars in parallel.
    pub fn set_callable_sig(
        &mut self,
        local: LocalId,
        ret_type: TypeId,
        param_types: Vec<TypeId>,
        param_ownerships: Vec<crate::parser::ast::Ownership>,
    ) {
        self.set_callable_return_type(local, ret_type);
        self.set_callable_param_types(local, param_types, param_ownerships);
    }

    /// Mark a local as a generic Ptr reference. Only sets if not already tracked
    /// with a more specific origin (set_bare_param / set_param_borrow_unique /
    /// set_field_borrow / set_collection_ref / set_view_of / cow_register_alias).
    /// The Alias(self) placeholder marks "borrowed but origin unknown to this
    /// layer" — the legacy fallback case from field loads / pattern extracts
    /// that don't have a more specific setter.
    pub fn set_ref(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return; }
        // Only set if currently Untracked (matches the legacy
        // `entry(local).or_insert(...)` no-op semantic for tracked locals).
        if matches!(builder.locals[idx].ownership, crate::ir::LocalOwnership::Untracked) {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Alias(local),
                mutability: crate::ir::Mutability::Shared,
            };
        }
    }

    /// Check if a local is a bare Ptr param borrowing from the caller.
    /// Phase D4.5 step 5b.2: reads `Local.ownership` directly. v2
    /// representation is Borrowed { Param(self), Shared } — the self-
    /// referential Param(local) where local == this is the signature
    /// set_bare_param writes. Mutability::Unique would mean
    /// set_param_borrow_unique (a `&` param), which is not bare.
    pub fn is_bare_param(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin, Mutability};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        matches!(&builder.locals[idx].ownership,
            LocalOwnership::Borrowed { origin: BorrowOrigin::Param(p), mutability: Mutability::Shared }
                if *p == local
        )
    }

    /// Mark a local as a bare Ptr param borrowing from the caller.
    pub fn set_bare_param(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Param(local),
                mutability: crate::ir::Mutability::Shared,
            };
        }
    }

    /// A `&` (MutableBorrow) param on a resource type. Origin is the
    /// param itself; mutability is Unique. Replaces a generic `set_ref`
    /// call for this specific class.
    pub fn set_param_borrow_unique(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Param(local),
                mutability: crate::ir::Mutability::Unique,
            };
        }
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
    /// auto-deref through the MutPtr local. Phase D4.5 step 5b.2:
    /// reads `Local.ownership` directly.
    pub fn is_param_borrow_unique(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin, Mutability};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        matches!(&builder.locals[idx].ownership,
            LocalOwnership::Borrowed { origin: BorrowOrigin::Param(p), mutability: Mutability::Unique }
                if *p == local
        )
    }

    /// A Ptr-typed local that's a borrow of a struct field (or enum
    /// variant payload field). `base` is the struct/scrutinee local;
    /// `field` is the field/variant-payload index.
    pub fn set_field_borrow(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, base: LocalId, field: u32) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Field { base, field },
                mutability: crate::ir::Mutability::Shared,
            };
        }
    }

    /// Tag a Ptr-typed field-load result with the RIGHT borrow origin
    /// (devbook/11 "CoW default: borrow everywhere"). If the base is itself a
    /// CoW collection-element borrow (`coll.get(i).unwrap().field`, for-element
    /// `x.field`), the field borrows out of the SAME collection — the element's
    /// memory is owned by the collection, so the severance/materialization unit
    /// is the collection, not the (unnamed, statement-scoped) element temp.
    /// Propagate the collection provenance so the var-decl default-borrow
    /// branch (`stmts/mod.rs` CollectionRef propagation) and
    /// `cow_before_mutation` see through the chain; a plain `Field { base }`
    /// tag here LOSES that provenance (base is an unnamed temp no mutation
    /// tracking can route back to) and forces an eager VarDeclFromBorrow clone
    /// per read — the round-33 DEEP-1 top-1 clone site. Non-element bases keep
    /// the plain Field origin.
    pub fn set_field_or_elem_borrow(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, base: LocalId, field: u32) {
        if self.is_cow_borrow(builder, base) {
            let source = self.cow_borrow_source(base).cloned()
                .or_else(|| self.collection_ref_source(builder, base));
            self.set_cow_borrow(builder, local);
            if let Some(coll) = source {
                self.set_cow_borrow_source(builder, local, coll);
            }
        } else {
            self.set_field_borrow(builder, local, base, field);
        }
    }

    /// If `local` is a `Borrowed { origin: Field { base, field }, .. }`,
    /// return its `(base, field)` tuple. Used at the VarDecl boundary
    /// (Site #1) to propagate Field origin onto typed bindings.
    pub fn field_borrow_origin(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> Option<(LocalId, u32)> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return None; }
        match &builder.locals[idx].ownership {
            LocalOwnership::Borrowed {
                origin: BorrowOrigin::Field { base, field }, ..
            } => Some((*base, *field)),
            _ => None,
        }
    }

    /// Tag `local` as the source for element `index` of tuple temp `tuple`.
    /// Recorded at `Inst::TupleInit` emission so the return path can
    /// MoveZero element sources when the tuple is returned. Replaces the
    /// `tuple_element_locals` sidecar — see `docs/devbook/13-ownership-in-ir.md` (BorrowOrigin).
    ///
    /// Self-host snag #6: do NOT clobber an existing Owned / FreshOwned /
    /// SharedHeap state. If the element local owned its data at TupleInit
    /// time (e.g. `Some((!k, !v))` where `!v` produces a Move-assigned
    /// temp tagged Owned), overwriting to Borrowed makes the consume-site
    /// validator read `Borrowed` at the very TupleInit that's consuming
    /// it — false positive. Layering discipline rule 1: this writer must
    /// not drop the Owned invariant the upstream lowering established.
    /// The TupleElement origin is only needed to track later return-path
    /// MoveZero of borrow-sourced elements; Owned/FreshOwned elements are
    /// handled instead by the unconditional `drops.unregister(local)` side
    /// effect below (scope-exit won't double-drop them after the tuple
    /// takes their data).
    pub fn set_tuple_element_borrow(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, tuple: LocalId, index: u32) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            let preserve = matches!(
                builder.locals[idx].ownership,
                crate::ir::LocalOwnership::Owned
                | crate::ir::LocalOwnership::FreshOwned
                | crate::ir::LocalOwnership::SharedHeap { .. }
            );
            if !preserve {
                builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                    origin: crate::ir::BorrowOrigin::TupleElement { tuple, index },
                    mutability: crate::ir::Mutability::Shared,
                };
            }
        }
        // The element's drop responsibility transfers to the tuple: unregister the
        // elem_local from the drops tracker so that scope-exit doesn't drop it a
        // second time after the tuple has already taken ownership.  The return-path
        // reader (stmts/mod.rs `tuple_element_sources`) still emits a MoveZero for
        // droppable elem locals when the tuple is being *returned*, which zeroes the
        // source slot so no aliased data survives; that MoveZero is safe whether or
        // not the local is registered. For the non-return case (tuple passed to a
        // function) the callee drops the tuple's contents — no additional caller-side
        // drop is needed.
        self.drops.unregister(local);
    }

    /// Walk `func.locals` and yield each local tagged as a TupleElement of
    /// the given `tuple` temp. Phase D4.5 step 5b.3: reads the typed
    /// `Local.ownership` field directly. Yields the source local id;
    /// callers iterate without ordering guarantees because the
    /// return-path MoveZero is order-insensitive.
    pub fn tuple_element_sources(&self, builder: &crate::ir::builder::FunctionBuilder, tuple: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| match &l.ownership {
                LocalOwnership::Borrowed {
                    origin: BorrowOrigin::TupleElement { tuple: t, .. }, ..
                } if *t == tuple => Some(LocalId(idx as u32)),
                _ => None,
            })
            .collect()
    }

    /// Mark a local as a CoW borrow (deferred clone). The placeholder
    /// `CowBorrowPending` origin distinguishes this from generic
    /// `set_ref`'s `Alias(self)` so `is_cow_borrow` can match. A
    /// subsequent `set_cow_borrow_source` upgrades to CollectionElement
    /// / FieldPath origin once the source collection is known.
    pub fn set_cow_borrow(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::CowBorrowPending,
                mutability: crate::ir::Mutability::Shared,
            };
        }
    }

    /// Check if a local is a CoW borrow (deferred clone).
    /// Phase D4.5 step 5b.2: reads `Local.ownership` directly. Matches
    /// v2 Borrowed origin variants that all map to legacy
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
    pub fn is_cow_borrow(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        matches!(&builder.locals[idx].ownership,
            LocalOwnership::Borrowed {
                origin: BorrowOrigin::CowBorrowPending
                      | BorrowOrigin::CollectionElement(_)
                      | BorrowOrigin::FieldPath(_),
                ..
            }
        )
    }

    /// Mark a local as a string view borrowing from `source`'s buffer.
    pub fn set_view_of(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, source: LocalId) {
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::View {
                source: crate::ir::BorrowOrigin::RuntimeView(source),
            };
        }
    }

    /// Find all locals that are views of `source`. Phase D: reads v2.
    /// View-only — SharedHeap targets use `shared_heap_aliases_of_source`.
    /// View entries materialise via `cow_materialize_view` (cap=0 byte
    /// slice → cloned to owned buffer). SharedHeap entries are
    /// independent 32-byte slots whose heap was already deep-cloned at
    /// the `gorget_string_copy_cow` boundary; running them through
    /// `cow_materialize_view` would emit a redundant clone-to-owned and
    /// shift slot indices in self-host driver compilation.
    pub fn views_of_source(&self, builder: &crate::ir::builder::FunctionBuilder, source: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| {
                if matches!(&l.ownership, LocalOwnership::View { source: BorrowOrigin::RuntimeView(s) } if *s == source) {
                    Some(LocalId(idx as u32))
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
    pub fn shared_heap_aliases_of_source(&self, builder: &crate::ir::builder::FunctionBuilder, source: LocalId) -> Vec<LocalId> {
        use crate::ir::LocalOwnership;
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| {
                if matches!(&l.ownership, LocalOwnership::SharedHeap { source: s } if *s == source) {
                    Some(LocalId(idx as u32))
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
    ///
    /// The stored `CollectionId::Local` names the STORAGE OWNER, never the
    /// spelling the caller happened to reach the collection through — see
    /// `resolve_collection_identity`. Callers pass whichever local they have
    /// in hand; the normalization is this producer's job.
    pub fn set_cow_borrow_source(&mut self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId, collection: CollectionId) {
        let collection = self.resolve_collection_identity(builder, collection);
        self.func_state.cow_borrow_sources.insert(local, collection);
    }

    /// Normalize a collection identity to the STORAGE OWNER before it is
    /// stored as provenance.
    ///
    /// `Vector[String] alias = v` binds a second NAME to one collection;
    /// `alias` is tagged `Borrowed { Alias(v) }`. A borrow taken through
    /// `alias` (`alias.get(0).unwrap()`) borrows out of **`v`'s** buffer, so
    /// the provenance that survives into the mutation passes must say `v`.
    /// Storing the spelling instead makes every downstream identity
    /// comparison miss: the `source_mut_unsafe` name query
    /// (`stmts/mod.rs`, keyed on `builder.local_name`) never sees the
    /// prescan's `v` mutations, so the lazy rescue is never emitted, and
    /// `cow_collection_refs_for_id`'s `CollectionElement(c) == t` equality
    /// never matches the mutated root, so the sever walk skips the view.
    /// The result is that WHICH NAME the view is spelled through decides
    /// whether the program reads freed memory.
    ///
    /// This is the single resolution point for that identity (Layering
    /// rule 4, "resolve once, write through"): both producers normalize
    /// here, so no consumer has to re-derive it or is allowed to disagree.
    /// `FieldPath` identities are structural paths, not locals, and pass
    /// through untouched. `cow_resolve_root` is idempotent, so re-storing an
    /// already-resolved identity is a no-op.
    fn resolve_collection_identity(
        &self,
        builder: &crate::ir::builder::FunctionBuilder,
        collection: CollectionId,
    ) -> CollectionId {
        match collection {
            CollectionId::Local(l) => CollectionId::Local(self.cow_resolve_root(builder, l)),
            CollectionId::FieldPath(_) => collection,
        }
    }

    /// Look up the source collection for a CowBorrow local.
    pub fn cow_borrow_source(&self, local: LocalId) -> Option<&CollectionId> {
        self.func_state.cow_borrow_sources.get(&local)
    }

    /// Look up the source collection of a local marked as a CollectionRef.
    /// Phase D4.5 step 5b.2: reads `Local.ownership` directly.
    pub fn collection_ref_source(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> Option<CollectionId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return None; }
        match &builder.locals[idx].ownership {
            LocalOwnership::Borrowed {
                origin: BorrowOrigin::CollectionElement(c), ..
            } => Some(CollectionId::Local(*c)),
            LocalOwnership::Borrowed {
                origin: BorrowOrigin::FieldPath(p), ..
            } => Some(CollectionId::FieldPath(p.clone())),
            _ => None,
        }
    }

    /// Mark a local as a collection element reference.
    ///
    /// Like `set_cow_borrow_source`, the recorded `CollectionElement` names
    /// the STORAGE OWNER, not the spelling — `resolve_collection_identity`.
    pub fn set_collection_ref(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, local: LocalId, collection: CollectionId) {
        let collection = self.resolve_collection_identity(builder, collection);
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = match collection {
                CollectionId::Local(coll_local) => crate::ir::LocalOwnership::Borrowed {
                    origin: crate::ir::BorrowOrigin::CollectionElement(coll_local),
                    mutability: crate::ir::Mutability::Shared,
                },
                CollectionId::FieldPath(path) => crate::ir::LocalOwnership::Borrowed {
                    origin: crate::ir::BorrowOrigin::FieldPath(path),
                    mutability: crate::ir::Mutability::Shared,
                },
            };
        }
    }

    /// Emit the lazy loop-carried element borrow — the default lowering for a
    /// String bind from a CoW element borrow whose source collection is
    /// mutated on a forward path. `local_id` is the declared String-typed var
    /// (`s`); `operand` is the `Ptr(String)` element borrow
    /// (`coll.get(i).unwrap()`); `collection` is the source. Instead of an
    /// eager clone at the bind (the pre-lazy lowering: clone in the bind block
    /// before the loop header), this materializes `s` as a pre-loop String
    /// VALUE slot holding a SHALLOW borrow (cap=0 view) + allocates a pre-loop
    /// `s_mat=false` flag, and records the pair in `cow_lazy_mat_flag`. The
    /// deep clone is deferred to the mutation site
    /// (`cow_materialize_view_lazy_in_place`, dispatched from Case 3 of
    /// `cow_before_mutation` and the W3a-W3d lazy-source read hooks via
    /// `materialize_lazy_source_if_needed`), flag-guarded so it fires at most
    /// once from the still-valid borrow. Dead mutation path → 0 clones. Both
    /// new slots are pre-loop locals (created here, before the loop's
    /// save_locals) so they survive restore_locals and become loop-carried
    /// (LIR-SSA phis them at the header) — the rebind-survival problem the
    /// a12333a0 attempt and the cow_borrow_outlives_push fixture comment
    /// describe. Doc: devbook/11 "Lazy loop-carried materialization".
    pub fn emit_lazy_loopcarried_borrow(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        name: &str,
        local_id: LocalId,
        inner_string_type: TypeId,
        _ptr_type: TypeId,
        operand: Operand,
        collection: CollectionId,
        span: crate::span::Span,
    ) {
        use crate::ir::instructions::{AssignMode, Place, Projection};
        // Typed eligibility read (devbook/24: no name-matching) — the caller
        // gated on this same accessor, so the unwrap is the contract.
        let borrow_view_fn = self
            .borrow_view_fn_for(inner_string_type)
            .expect("emit_lazy_loopcarried_borrow: caller checked borrow_view_fn eligibility");
        // ABI normalization: when the element borrow carries a trailing Deref
        // projection (`*(Str*)p` — a 32-byte VALUE), passing it where the
        // callee expects `const Str*` mis-types the C call (the self-host
        // driver's `map_runtime_name`/`resolve_sizeof_c_type` bind shapes).
        // The pre-deref pointer is exactly the argument the callee wants —
        // strip the Deref and pass the pointer place.
        let operand = match operand {
            Operand::Copy(ref p) | Operand::Move(ref p)
                if p.projections.last() == Some(&Projection::Deref) =>
            {
                let mut np = p.clone();
                np.projections.pop();
                Operand::Copy(np)
            }
            other => other,
        };
        // 1) Shallow borrow of the element into `s`'s value slot. The runtime
        //    `gorget_string_borrow_view` is a 32-byte struct copy with cap
        //    FORCED to 0 (drop-safe view) — NO heap alloc, NO clone. Valid as
        //    long as `coll` isn't reallocated; the mutation-site materialize
        //    severs that dependency.
        let borrowed = builder.call(
            &borrow_view_fn,
            vec![operand],
            inner_string_type,
        );
        // Retype `s` to the String VALUE type (was Ptr) and write the borrow in.
        builder.locals[local_id.0 as usize].type_id = inner_string_type;
        builder.assign_mode(
            AssignMode::Move,
            Place::local(local_id),
            crate::ir::builder::FunctionBuilder::copy(borrowed),
        );
        self.register_local(name, local_id, inner_string_type);
        self.func_state.named_locals.insert(local_id);
        // Tag as a CollectionElement borrow so `cow_before_mutation` Case 3
        // (`cow_collection_refs_for`) finds it when `coll` is mutated. The
        // lazy-flag side-map routes it to the in-place materialize there.
        self.set_collection_ref(builder, local_id, collection);
        // `s` carries a (possibly heap-owning after materialize) String — it
        // MUST be drop-tracked. The runtime free is cap-driven: an
        // unmaterialized cap=0 view frees nothing, a materialized cap>0 owned
        // copy frees its buffer. Sound in both loop-carried branches.
        // update-not-reregister: the VarDecl path may have already registered
        // this local (a plain `register_local` here emitted TWO exit-block
        // frees — benign for String's zeroing free, still wrong).
        self.drops.update_or_register_type(local_id, inner_string_type, &self.type_registry);

        // 2) Pre-loop `s_mat = false` flag. Created HERE (before the loop) so it
        //    is a loop-carried local (lid < the loop's save_locals boundary).
        let flag = builder.add_local(crate::ir::types::BOOL_TYPE, Some("__cow_mat"));
        builder.assign(
            Place::local(flag),
            crate::ir::builder::FunctionBuilder::const_bool(false),
        );
        self.func_state.cow_lazy_mat_flag.insert(local_id, flag);
        let _ = span;
    }

    /// Flag-guarded IN-PLACE materialize of a lazy loop-carried element
    /// borrow. Emits:
    ///
    ///     if !s_mat:
    ///         s = clone_to_owned(&s)   // in-place: overwrite s's OWN slot
    ///         s_mat = true
    ///
    /// `s` is a String VALUE slot already holding the shallow borrow; the clone
    /// reads `s`'s current (still-valid on the first mutating iteration) value
    /// and writes the deep-owned copy back into the SAME slot. No fresh local,
    /// no name rebind — so it survives the enclosing loop's restore_locals and
    /// the post-loop read of `s` (which resolves to this same loop-carried slot)
    /// is correct in BOTH the materialized and never-materialized branches.
    /// The flag guard makes the clone fire at most once.
    ///
    /// Multi-mutation-site soundness: `restore_locals` reverts per-branch
    /// ownership, so each branch-arm mutation site re-finds the tag and
    /// emits its own guard (two guard callsites, first dynamically dead →
    /// exactly one runtime clone). Same-straight-line later sites are
    /// covered by dominance: the first guard's `cont_bb` dominates them, so
    /// the flag is true and their guards are runtime no-ops.
    pub fn cow_materialize_view_lazy_in_place(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        s_local: LocalId,
        flag_local: LocalId,
        span: crate::span::Span,
    ) {
        use crate::ir::instructions::{AssignMode, Place};
        use crate::ir::types::BOOL_TYPE;
        let s_type = builder.local_type(s_local);
        let clone_fn = match self.clone_fn_for_ptr(s_type) {
            Some(f) => f,
            None => return,
        };
        // CONDITIONAL clone site: bare `warn_implicit_clone` (not the
        // `warn_clone_and_hit` helper) because the hit must be emitted inside
        // mat_bb below. Allowlisted in tests/lints.rs::clone_warn_hit_pairing.
        let cid = self.warn_implicit_clone(span, s_type, crate::ir::ImplicitCloneReason::CoWMaterialization);

        // if !s_mat goto mat_bb else cont_bb
        let mat_bb = builder.new_block();
        let cont_bb = builder.new_block();
        let not_mat = builder.un_op(
            crate::ir::instructions::UnOp::Not,
            BOOL_TYPE,
            crate::ir::builder::FunctionBuilder::copy(flag_local),
        );
        builder.branch(
            crate::ir::builder::FunctionBuilder::copy(not_mat),
            mat_bb,
            cont_bb,
        );

        // mat_bb: s = clone_to_owned(&s); s_mat = true; goto cont_bb
        builder.switch_to(mat_bb);
        // Attribution: the clone only executes on the unmaterialized branch,
        // so the site-hit lives inside mat_bb (counts actual clones, not
        // guard evaluations).
        self.emit_clone_site_hit(builder, cid);
        // The clone fn takes `const GorgetString*` — pass `s` by value; the C
        // emit takes its address (same shape as every other clone_to_owned
        // callsite: `gorget_string_clone_to_owned(&__vN)`).
        let cloned = builder.call_clone(
            &clone_fn,
            vec![crate::ir::builder::FunctionBuilder::copy(s_local)],
            s_type,
            crate::ir::ImplicitCloneReason::CoWMaterialization,
        );
        builder.assign_mode(
            AssignMode::Move,
            Place::local(s_local),
            crate::ir::builder::FunctionBuilder::copy(cloned),
        );
        builder.assign(
            Place::local(flag_local),
            crate::ir::builder::FunctionBuilder::const_bool(true),
        );
        builder.jump(cont_bb);

        // continue
        builder.switch_to(cont_bb);
        // `s` is now Owned (on the materialized path) — keep drop-tracking; the
        // cap-driven free handles the unmaterialized branch.
        self.set_owned(builder, s_local);
    }

    /// Shared lazy-source READ hook: if `operand` is a projection-free
    /// Copy/Move of a local present in `cow_lazy_mat_flag`, emit the
    /// flag-guarded in-place materialize on it (no-op otherwise).
    ///
    /// A read that captures a lazy view's VALUE or ADDRESS into another
    /// binding loses provenance to the source collection — Case 3 of
    /// `cow_before_mutation` can then no longer materialize the captured
    /// copy. Materializing the SOURCE first means the captured
    /// bytes/pointer target the local's own owned buffer. FOUR call sites —
    /// the complete view-producer read set (devbook/11 "view-producer
    /// enumeration"; grep `gorget_str_view_region` across ALL of src/ and
    /// walk each hit to its GIR producer before adding a sibling):
    ///   - W3a `lower_var_decl` trailing-assign entry (alias / move-steal
    ///     binds: `String x = s`),
    ///   - W3b `returns_view` method receivers, BEFORE the call captures the
    ///     header (`s.substring(..)` as temp or named bind),
    ///   - W3c `lower_index_access` place-arm (`s[i]` / `s[a..b]` — never
    ///     consult `returns_view`, carry NO View tag),
    ///   - W3d `lower_for_string` source (`for c in s:` — synthetic
    ///     `gorget_str_codepoint_at` views).
    /// The map entry survives the materialize (see `cow_lazy_mat_flag` doc);
    /// the runtime flag keeps the clone at-most-once.
    pub fn materialize_lazy_source_if_needed(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: &Operand,
        span: crate::span::Span,
    ) {
        if let Operand::Copy(p) | Operand::Move(p) = operand {
            if p.projections.is_empty() {
                if let Some(&flag) = self.func_state.cow_lazy_mat_flag.get(&p.local) {
                    self.cow_materialize_view_lazy_in_place(builder, p.local, flag, span);
                }
            }
        }
    }

    /// Derive `slot_kind` for every local at the GIR/LIR boundary.
    /// Phase D4.5 step 5d: `Local.ownership` is the sole live store; the
    /// legacy `func_state.local_ownership: FxHashMap` was retired. This
    /// pass derives `slot_kind` per (type, ownership). The empirical
    /// audit showed zero "non-Ptr type with borrow ownership" combos,
    /// so the mapping is total over the three cases.
    ///
    /// Historical name: `flush_ownership_to_locals`. The "flush" is now
    /// purely a `slot_kind` derivation; the ownership half is written
    /// directly at every setter call site.
    pub fn flush_ownership_to_locals(&self, builder: &mut crate::ir::builder::FunctionBuilder) {
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
    pub fn cow_register_alias(&mut self, builder: &mut crate::ir::builder::FunctionBuilder, alias_local: LocalId, source_local: LocalId) {
        let root = self.cow_resolve_root(builder, source_local);
        let idx = alias_local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Alias(root),
                mutability: crate::ir::Mutability::Shared,
            };
        }
    }

    /// Resolve a local to its root source (follow alias chain).
    /// Phase D4.5 step 5b.3: walks `Local.ownership` directly. Self-loops
    /// (source == current — produced by set_ref placeholders) terminate
    /// resolution at the local itself, matching the legacy semantics
    /// where set_ref-marked locals weren't real aliases.
    fn cow_resolve_root(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> LocalId {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let mut current = local;
        loop {
            let idx = current.0 as usize;
            if idx >= builder.locals.len() { break; }
            match &builder.locals[idx].ownership {
                LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(source), .. } => {
                    if *source == current { break; }
                    current = *source;
                }
                _ => break,
            }
        }
        current
    }

    /// Check if a local is a CoW alias of something else.
    /// Phase D4.5 step 5b.2: reads `Local.ownership` directly. A true
    /// alias has v2 = Borrowed { Alias(s), .. } with s != self
    /// (the self-loop form is the set_ref placeholder, not a real alias).
    pub fn cow_is_alias(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        let idx = local.0 as usize;
        if idx >= builder.locals.len() { return false; }
        matches!(&builder.locals[idx].ownership,
            LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(s), .. } if *s != local
        )
    }

    /// Check if a local has CoW aliases pointing to it (is a source).
    /// Phase D4.5 step 5b.3: scans `Local.ownership` for Alias entries
    /// pointing at `local`, excluding self-loop placeholders.
    pub fn cow_has_aliases(&self, builder: &crate::ir::builder::FunctionBuilder, local: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate().any(|(idx, l)|
            matches!(&l.ownership, LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(src), .. }
                       if *src == local && LocalId(idx as u32) != local)
        )
    }

    /// Collect all aliases pointing to `source`. Derived query — O(n) scan.
    /// Phase D4.5 step 5b.3: scans `Local.ownership` directly.
    fn cow_aliases_of(&self, builder: &crate::ir::builder::FunctionBuilder, source: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| match &l.ownership {
                LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(src), .. }
                    if *src == source && LocalId(idx as u32) != source => Some(LocalId(idx as u32)),
                _ => None,
            })
            .collect()
    }

    /// Check if a collection has any element refs pointing into it.
    /// Phase D4.5 step 5b.3: scans `Local.ownership` for Borrowed
    /// { CollectionElement(`collection`), .. }.
    pub fn cow_has_collection_refs(&self, builder: &crate::ir::builder::FunctionBuilder, collection: LocalId) -> bool {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().any(|l|
            matches!(&l.ownership, LocalOwnership::Borrowed {
                origin: BorrowOrigin::CollectionElement(c), ..
            } if *c == collection)
        )
    }

    /// Collect all collection refs pointing to a `CollectionId`. Derived query — O(n) scan.
    /// Phase D4.5 step 5b.3: scans `Local.ownership` directly.
    fn cow_collection_refs_for_id(&self, builder: &crate::ir::builder::FunctionBuilder, target: &CollectionId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| {
                let matches = match (&l.ownership, target) {
                    (LocalOwnership::Borrowed {
                        origin: BorrowOrigin::CollectionElement(c), ..
                    }, CollectionId::Local(t)) => *c == *t,
                    (LocalOwnership::Borrowed {
                        origin: BorrowOrigin::FieldPath(p), ..
                    }, CollectionId::FieldPath(t)) => p == t,
                    _ => false,
                };
                if matches { Some(LocalId(idx as u32)) } else { None }
            })
            .collect()
    }

    /// Collect all collection refs pointing to a direct local.
    pub fn cow_collection_refs_for(&self, builder: &crate::ir::builder::FunctionBuilder, collection: LocalId) -> Vec<LocalId> {
        self.cow_collection_refs_for_id(builder, &CollectionId::Local(collection))
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
        if self.is_bare_param(builder, local) {
            self.unset_ownership(builder, local);
            self.cow_materialize_alias(builder, local, local, span);
        }

        // Case 1: local is an alias of something else → clone source into local.
        // Phase D4.5 step 5b.4: read alias source from `Local.ownership`
        // (Borrowed { Alias(s), .. } with s != local — self-loops are
        // set_ref placeholders, not real aliases).
        let alias_source: Option<LocalId> = {
            use crate::ir::{LocalOwnership, BorrowOrigin};
            let idx = local.0 as usize;
            if idx >= builder.locals.len() { None }
            else {
                match &builder.locals[idx].ownership {
                    LocalOwnership::Borrowed { origin: BorrowOrigin::Alias(s), .. } if *s != local => Some(*s),
                    _ => None,
                }
            }
        };
        if let Some(source) = alias_source {
            self.unset_ownership(builder, local);
            self.cow_materialize_alias(builder, local, source, span);
        }

        // Case 1b: local is ITSELF a CollectionElement / FieldPath borrow being
        // mutated in place (e.g. `Vector[int] x = coll.get(0).unwrap(); x.bump(99)`).
        // Mutating `x` directly aliases into the collection's buffer; value
        // semantics requires materialising `x` into an independent owned copy
        // first, so the mutation lands in `x`'s own buffer (and the source
        // collection is left untouched). Case 3 only severs when the COLLECTION
        // is mutated — this is the symmetric "the element ref is the thing being
        // mutated" case, which was previously unhandled (Case 1 matches only
        // `Alias`). `cow_materialize_collection_ref` clones the pointee and
        // rebinds the name; the mutating-method caller re-resolves the receiver
        // afterwards (exprs/methods.rs).
        let is_element_borrow = {
            use crate::ir::{LocalOwnership, BorrowOrigin};
            let idx = local.0 as usize;
            idx < builder.locals.len()
                && matches!(
                    &builder.locals[idx].ownership,
                    LocalOwnership::Borrowed {
                        origin: BorrowOrigin::CollectionElement(_) | BorrowOrigin::FieldPath(_),
                        ..
                    }
                )
        };
        if is_element_borrow {
            self.cow_materialize_collection_ref(builder, local, span);
        }

        // Case 2: local is a source with aliases → clone into each alias
        let aliases = self.cow_aliases_of(builder, local);
        if !aliases.is_empty() {
            for alias in aliases {
                self.unset_ownership(builder, alias);
                self.cow_materialize_alias(builder, alias, local, span);
            }
        }

        // Case 3: local is a collection with refs into it → clone each ref
        let refs = self.cow_collection_refs_for(builder, local);
        if !refs.is_empty() {
            for ref_local in refs {
                // A lazy loop-carried element borrow routes to the
                // flag-guarded IN-PLACE materialize instead of the legacy
                // fresh-local rebind (which wouldn't survive the enclosing
                // loop's restore_locals). Detection: the ref appears in
                // `cow_lazy_mat_flag`. The flag + slot are pre-loop locals so
                // the materialize is loop-carried.
                //
                // ORDER (deliberate): the lazy route runs BEFORE the
                // `is_ref_local` liveness check the legacy arm uses. The
                // legacy check skips refs whose Ptr binding was already
                // moved/reassigned; a lazy local is a VALUE slot whose map
                // entry is removed at every write site
                // (`lower_assign`/`lower_compound_assign`), so map membership
                // IS the liveness signal — a present entry means the slot
                // still holds the (possibly already-materialized) element
                // borrow, and the flag guard makes a re-emitted materialize a
                // runtime no-op.
                if let Some(&flag) = self.func_state.cow_lazy_mat_flag.get(&ref_local) {
                    self.cow_materialize_view_lazy_in_place(builder, ref_local, flag, span);
                    continue;
                }
                // Skip ANONYMOUS ELEMENT BORROWS — the sibling of Case 6's
                // named-locals filter, and required for the same reason it
                // states there ("duplicate clones of stale Ptr values").
                //
                // The predicate is the ABSENCE OF A NAME HINT rather than
                // `is_named_local`, because the name hint is exactly what
                // `cow_materialize_collection_ref` rebinds through: no hint,
                // no landing site for the clone. (It is also the typed field
                // on `Local`, not a `func_state` proxy.)
                //
                // `cow_materialize_collection_ref` rebinds the element by its
                // NAME HINT. An anonymous Ptr temp — the `v[0]` index-load
                // result, whose value the var-decl has ALREADY eagerly cloned
                // into the user's variable — has no name to rebind, so the
                // emitted clone lands in a throwaway local that is dropped
                // unread. It is not merely wasted work: it clones FROM the
                // element pointer, and on the second iteration of a
                // reallocating loop that pointer has already been freed, so
                // the "rescue" is itself the use-after-free.
                //
                // The filter is on Ptr-typed refs specifically, NOT on every
                // anonymous ref. A SLICE (`v[a:b]`) is tagged through the same
                // producer but is a collection VALUE, not an element borrow —
                // it is anonymous, it IS read (the var-decl assign consumes
                // it), and it needs the clone to reach its owned slot. Only
                // the Ptr shape is a borrow with nothing to rebind.
                let ref_slot = &builder.locals[ref_local.0 as usize];
                let ref_is_ptr_temp = ref_slot.name_hint.is_none()
                    && matches!(
                        self.type_registry.get(ref_slot.type_id),
                        Some(crate::ir::types::GirType::Ptr(_) | crate::ir::types::GirType::MutPtr(_))
                    );
                if ref_is_ptr_temp { continue; }
                // Only sever if the ref is still live (not already moved/reassigned)
                if self.is_ref_local(builder, ref_local) {
                    self.cow_materialize_collection_ref(builder, ref_local, span);
                }
            }
        }

        // Case 4: local is a string with live views → materialize each view
        // before the source is mutated (push/append/clear/reassign).
        let views = self.views_of_source(builder, local);
        for view_local in views {
            self.unset_ownership(builder, view_local);
            self.cow_materialize_view(builder, view_local, span);
        }

        // Case 5: local has SharedHeap value-aliases → drop their tag.
        // The aliases are independent 32-byte struct slots whose heap
        // was already deep-cloned at the `gorget_string_copy_cow`
        // boundary, so no IR-level materialise is needed — only the
        // typed-state invalidation so source-mutation isn't blocked by
        // a stale alias tag pointing at a re-used slot.
        let shared_aliases = self.shared_heap_aliases_of_source(builder, local);
        for alias_local in shared_aliases {
            self.unset_ownership(builder, alias_local);
        }

        // Case 6: local is a struct with live NAMED field borrows pointing into it →
        // materialize each. Triggered when the struct is reassigned or moved.
        // Without this, `String path = imp.module_path; imp = NewImport()` leaves
        // path dangling. Mirrors Case 3 (collection refs) for the field-borrow
        // shape used by deferred String materialization site #1.
        //
        // Filter to NAMED locals: ephemeral field-load temps from
        // expression lowering (e.g., the temp behind `obj.field` inside
        // `f(obj.field, ...)`) carry the same Field origin tag but are
        // dead immediately after the expression finishes. Materialising
        // them produces duplicate clones of stale Ptr values.
        let field_borrows = self.field_borrows_of(builder, local);
        for fb_local in field_borrows {
            if !self.is_named_local(fb_local) { continue; }
            self.unset_ownership(builder, fb_local);
            self.cow_materialize_alias(builder, fb_local, local, span);
        }
    }

    /// G3: `cow_before_mutation` run from a LOOP PRE-HEADER (a bare param the
    /// loop body mutates, hoisted once before the loop). Identical materialize
    /// behavior; the only difference is that every clone emitted stamps
    /// `LoopPreHeaderMaterialize` instead of at-site `CoWMaterialization`, so
    /// the planner can cost the once-per-loop hoist distinctly. Routes through
    /// the plan (`apply_materialize_directive`) with a `LoopPreHeader` directive
    /// keyed by the loop's pre-header span, so the table's position coverage is
    /// honest (loop hoists ARE recorded as `LoopPreHeader`, not just AtSite).
    pub fn cow_before_mutation_loop_preheader(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
        span: crate::span::Span,
    ) {
        self.apply_materialize_directive(
            builder,
            MaterializeDirective {
                root: local,
                reason: crate::ir::ImplicitCloneReason::LoopPreHeaderMaterialize,
                position: MaterializePosition::LoopPreHeader { anchor: span },
            },
        );
    }

    /// The reason-stamping materialize primitive under the plan-apply funnel
    /// (planner round 3). Runs `cow_before_mutation` with the directive's `reason`
    /// stamped on every clone emitted — at-site (`CoWMaterialization`), loop
    /// pre-header (`LoopPreHeaderMaterialize`), or branch pre-header
    /// (`BranchPreHeaderMaterialize`) — so the planner can cost each position
    /// distinctly. Identical materialize behavior across positions; only the
    /// `reason` differs. Save/restore of the scoped `cow_reason` (see the field
    /// doc for why this is caller-context ambient rather than a threaded param).
    /// This owns the SINGLE `.cow_before_mutation(` call (the ratchet's
    /// convergence meter endpoint); EVERY materialize — at-site and both
    /// pre-header positions — reaches it exclusively through
    /// `apply_materialize_directive`, so no client mints a new call site.
    pub fn cow_before_mutation_planned(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
        reason: crate::ir::ImplicitCloneReason,
        span: crate::span::Span,
    ) {
        let prev = self.cow_reason;
        self.cow_reason = reason;
        self.cow_before_mutation(builder, local, span);
        self.cow_reason = prev;
    }

    /// THE plan-apply funnel (planner campaign round 3). Executes ONE
    /// `MaterializeDirective` — records it in the per-function plan (for
    /// costing/observability + the future planner's working set) and applies it
    /// through the SINGLE reason-stamping materialize funnel
    /// (`cow_before_mutation_planned`, which owns the lone
    /// `.cow_before_mutation(` call). Converting an at-site
    /// `ctx.cow_before_mutation(...)` class to `ctx.apply_materialize_directive`
    /// removes that direct call from the `ratchet_b_materialize_site_count`
    /// convergence meter — the campaign's proof that a class migrated behind the
    /// plan. `apply_materialize_directive` is NOT a `.cow_before_mutation(`
    /// textual call, so the ratchet counts only the funnel's single call. This is
    /// the SINGLE entry every plan client routes through — the at-site class
    /// (`plan_materialize_at_site`) AND both pre-header consumers
    /// (`cow_before_mutation_loop_preheader` → `LoopPreHeader`,
    /// `materialize_scope_carried_bare_params` → `BranchPreHeader`) — so all three
    /// `MaterializePosition` variants are genuinely constructed and recorded.
    pub fn apply_materialize_directive(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        directive: MaterializeDirective,
    ) {
        self.materialize_plan.record(directive);
        self.cow_before_mutation_planned(
            builder,
            directive.root,
            directive.reason,
            directive.span(),
        );
    }

    /// Convenience: record + apply an AT-SITE CoW materialize of `root` at the
    /// mutation `span` through the plan (reason `CoWMaterialization`). The
    /// planner round-3 first-client entry for the assign-target-root class
    /// (`s.field = x` / `d[k] = x` / `xs[i] OP= x`); mirrors the self-host
    /// lane's shared `cow_materialize_projected_root` funnel.
    pub fn plan_materialize_at_site(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        root: LocalId,
        span: crate::span::Span,
    ) {
        self.apply_materialize_directive(
            builder,
            MaterializeDirective {
                root,
                reason: crate::ir::ImplicitCloneReason::CoWMaterialization,
                position: MaterializePosition::AtSite { mutation: span },
            },
        );
    }

    /// Find every local borrowing some field of `base`. Phase D4.5 step
    /// 5b.3: scans `Local.ownership` for `Borrowed { Field { base, .. }, .. }`
    /// matching the target.
    fn field_borrows_of(&self, builder: &crate::ir::builder::FunctionBuilder, base: LocalId) -> Vec<LocalId> {
        use crate::ir::{LocalOwnership, BorrowOrigin};
        builder.locals.iter().enumerate()
            .filter_map(|(idx, l)| match &l.ownership {
                LocalOwnership::Borrowed {
                    origin: BorrowOrigin::Field { base: b, .. }, ..
                } if *b == base => Some(LocalId(idx as u32)),
                _ => None,
            })
            .collect()
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
        let refs = self.cow_collection_refs_for_id(builder, &target);
        for ref_local in refs {
            if self.is_ref_local(builder, ref_local) {
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
        let aliases = self.cow_aliases_of(builder, source_local);
        for alias in aliases {
            self.unset_ownership(builder, alias);
            self.cow_materialize_alias(builder, alias, source_local, span);
        }
        // Clean up other CoW tracking for the reassigned source — it's about
        // to get a new value, so stale entries would cause incorrect clones.
        if self.is_bare_param(builder, source_local) {
            self.unset_ownership(builder, source_local);
        }
        // Remove collection refs pointing to this source.
        // Lazy loop-carried refs MATERIALIZE first (sibling of Case 3 in
        // `cow_before_mutation` — this sever runs BEFORE that dispatch when
        // the reassigned collection also has Alias-aliases, so without the
        // routing here the lazy view would never materialize and would
        // dangle once the old buffer is dropped). Then unset as before; the
        // flag-map entry stays (the slot still holds the now-owned value;
        // a later W3 read hook's guarded clone is a runtime no-op).
        let refs = self.cow_collection_refs_for(builder, source_local);
        for r in refs {
            self.unset_ownership(builder, r);
            // (unset first so the materialize's trailing `set_owned` is the
            // final state — the slot really does own its buffer afterwards.)
            if let Some(&flag) = self.func_state.cow_lazy_mat_flag.get(&r) {
                self.cow_materialize_view_lazy_in_place(builder, r, flag, span);
            }
        }
        // Materialize string views borrowing from this source
        let views = self.views_of_source(builder, source_local);
        for view_local in views {
            self.unset_ownership(builder, view_local);
            self.cow_materialize_view(builder, view_local, span);
        }
        // Drop SharedHeap value-aliases — heap already deep-cloned at the
        // shallow-copy boundary; no IR-level materialise needed.
        let shared_aliases = self.shared_heap_aliases_of_source(builder, source_local);
        for alias_local in shared_aliases {
            self.unset_ownership(builder, alias_local);
        }

        // Materialize field borrows pointing at this source (deferred-string
        // materialization: a Ptr-typed field-load like `String x = imp.field`
        // is propagated as a Field borrow rather than eager-cloned, so source
        // reassignment must clone the bytes back into the borrower).
        // Filter to NAMED locals — see cow_before_mutation Case 6 rationale.
        let fbs = self.field_borrows_of(builder, source_local);
        for fb in fbs {
            if !self.is_named_local(fb) { continue; }
            self.unset_ownership(builder, fb);
            self.cow_materialize_alias(builder, fb, source_local, span);
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
        let sub_views = self.views_of_source(builder, view_local);
        for sub in sub_views {
            self.unset_ownership(builder, sub);
            self.cow_materialize_view(builder, sub, span);
        }

        let view_type = builder.local_type(view_local);
        if let Some(clone_fn) = self.clone_fn_for_ptr(view_type) {
            let reason = self.cow_reason;
            self.warn_clone_and_hit(builder, span, view_type, reason);
            let cloned = builder.call_clone(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(view_local)], view_type,
                reason);
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
            self.set_owned(builder, owned_local);
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
            let reason = self.cow_reason;
            self.warn_clone_and_hit(builder, span, inner_type, reason);
            let cloned = builder.call_clone(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(alias_local)], inner_type,
                reason);
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
            self.set_owned(builder, owned_local);
            if let Some(ref hint) = builder.local_name(alias_local).map(|s| s.to_string()) {
                let name = hint.clone();
                self.register_local(&name, owned_local, inner_type);
                self.func_state.named_locals.insert(owned_local);
            }
        } else if !self.type_registry.is_resource_type(inner_type) {
            // 2E scout: non-resource pointee (a pure-value struct behind a
            // bare Ptr param — plain-`self` on a value struct is the
            // canonical shape). No clone fn exists and none is needed:
            // deref-copy the pointee into a fresh owned local and rebind, so
            // the write lands on the private copy (mirrors
            // cow_materialize_collection_ref's deref arm; nothing to
            // drop-register — no resource fields, by transitivity of
            // resource-ness).
            //
            // T1.4 (wave-2 executor decision): this arm is DELIBERATELY silent —
            // no `warn_implicit_clone`. Unlike the clone-fn arm above (which
            // routes through `warn_clone_and_hit`), a pure-value struct memcpy
            // is not a runtime clone: no alloc, no drop, no `runtime_fn`, so it
            // never bumps `[clone-stats]`. Minting a Clone-Report row here would
            // create a site that reads "0 hits" forever — exactly the
            // silent-under-attribution the `clone_warn_hit_pairing` lint exists
            // to forbid (context.rs is budgeted 3 bare warns / 3 hits, all
            // conditional). The sibling `cow_materialize_collection_ref` mints
            // ONE CloneId because it is a CONDITIONAL site (its own resource arm
            // may clone); this else-if is unconditionally value-only, so there
            // is nothing to attribute.
            let name_hint = builder.local_name(alias_local).map(|s| s.to_string());
            let owned_local = builder.add_local(inner_type, name_hint.as_deref());
            builder.assign(
                crate::ir::instructions::Place::local(owned_local),
                Operand::Copy(crate::ir::instructions::Place {
                    local: alias_local,
                    projections: vec![crate::ir::instructions::Projection::Deref],
                }),
            );
            self.set_owned(builder, owned_local);
            if let Some(ref hint) = name_hint {
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
        // CONDITIONAL clone site: bare `warn_implicit_clone` (not the
        // `warn_clone_and_hit` helper) because only the clone-fn arm below
        // actually clones. Allowlisted in tests/lints.rs::clone_warn_hit_pairing.
        let reason = self.cow_reason;
        let cid = self.warn_implicit_clone(span, inner_type, reason);
        let name_hint = builder.local_name(ref_local).map(|s| s.to_string());
        let owned_local = builder.add_local(inner_type, name_hint.as_deref());
        if let Some(clone_fn) = self.clone_fn_for_ptr(inner_type) {
            // Attribution: hit only on the real clone path — the deref arm
            // below is a value copy, not a runtime clone.
            self.emit_clone_site_hit(builder, cid);
            let cloned = builder.call_clone(&clone_fn,
                vec![crate::ir::builder::FunctionBuilder::copy(ref_local)], inner_type,
                reason);
            // Phase C: cloned is fresh + dead — Move into owned_local.
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                crate::ir::instructions::Place::local(owned_local),
                crate::ir::builder::FunctionBuilder::copy(cloned),
            );
            self.drops.register_local(owned_local, inner_type, &self.type_registry);
            self.set_owned(builder, owned_local);
        } else {
            // Deref the Ref pointer to load the pointee value.
            builder.assign(
                crate::ir::instructions::Place::local(owned_local),
                Operand::Copy(crate::ir::instructions::Place {
                    local: ref_local,
                    projections: vec![crate::ir::instructions::Projection::Deref],
                }),
            );
            self.set_owned(builder, owned_local);
        }
        if let Some(ref hint) = builder.local_name(ref_local).map(|s| s.to_string()) {
            let name = hint.clone();
            self.register_local(&name, owned_local, inner_type);
            self.func_state.named_locals.insert(owned_local);
        }
        // The old ref_local is now dead
        self.unset_ownership(builder, ref_local);
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
        self.type_mapper.iter_named()
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
        // Then try Named Box types: TypeDef with a single "_0" field.
        // Reads the typed `metadata.is_box` flag (set at every Box-TypeDef
        // registration path) rather than name-prefix matching.
        if self.type_registry.is_box(type_id) {
            if let Some(GirType::Named(name)) = self.type_registry.get(type_id) {
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
    ///
    /// **Phase 2 widening migration prerequisite (2026-05-07).** Before this
    /// commit, the doc above was aspirational: registration injected the
    /// TypeDef via `get_or_register` but never ran the upgrade scan. Late-
    /// registered Options (`Option[Box[T]]`, `Option[Vector[T]]`, etc. coming
    /// from a `.get()` deep inside a function) had `copy_semantics=Default`
    /// and `drop_strategy=None`, so `needs_drop` / `is_resource_or_contains_resource`
    /// returned false for them — racing with the module-level
    /// `upgrade_types_from_fields` pass that already ran at module start.
    /// The fix: probe the inner type's drop status at registration time and
    /// upgrade the freshly-registered Option immediately. The module-level
    /// upgrade scan is still authoritative for transitive cases (struct A
    /// containing Option[B] where B was upgraded after A), but
    /// late-registered Options now have correct first-order metadata.
    /// Writer-boundary coercion: rewrite a `Constant::Null` RHS into a
    /// properly tagged `Option[T]::None` enum-init when the destination is
    /// Option/Result-typed. Returns the operand unchanged in all other
    /// shapes.
    ///
    /// **Invariant** (Snag #32 family — "None-literal materialisation at
    /// writer boundaries"). At every Assign / field-store / index-store /
    /// deref-store site whose dst-type is a tagged enum wrapper
    /// (`Option__T` / `Result__T__E`), a `Constant::Null` RHS must be
    /// rewritten to an `enum_init <T> None []` before emission. Without
    /// this, the C backend memsets the 40-byte struct to zero, which
    /// (because `Some=0 / None=1` in the current discriminator layout)
    /// produces a *Some(empty payload)* zombie — silently dropping the
    /// user's `field = None` write.
    ///
    /// Three boundaries used this fallback before today:
    /// - `Option[T] x = None` (VarDecl) — caught by the Assign handler's
    ///   pre-existing rewrite path.
    /// - `f(None)` (call arg) — caught by `materialise_none_for_expected_type`
    ///   on `Expr::NoneLiteral` after the Snag #29b runtime fix.
    /// - `r.field = None`, `arr[i] = None`, `*box = None` — Snag #32. Plugged
    ///   by routing every writer-side `lower_expr(value)` in `stmts/assigns.rs`
    ///   through this helper.
    ///
    /// The companion module-exit validator
    /// `validate_no_null_assign_to_option_slot` is a defence-in-depth ratchet
    /// that fatal-panics if a future writer site forgets to route through
    /// here.
    pub fn coerce_null_to_option_none(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        operand: Operand,
        target_type: TypeId,
    ) -> Operand {
        use crate::ir::instructions::Constant;
        if !matches!(operand, Operand::Constant(Constant::Null)) {
            return operand;
        }
        let name = match self.type_registry.type_name(target_type) {
            Some(n) => n.to_string(),
            None => return operand,
        };
        if !name.starts_with("Option__") || name.starts_with("Option__Ref__") {
            return operand;
        }
        let inner = self.type_registry
            .get_type_def(&name)
            .and_then(|td| match &td.kind {
                crate::ir::types::TypeDefKind::Enum(e) => {
                    e.variants.iter().find(|v| v.name == "Some")
                        .and_then(|v| v.fields.first().map(|f| f.type_id))
                }
                _ => None,
            });
        let Some(inner_type) = inner else { return operand; };
        self.ensure_option_type_registered(&name, inner_type);
        let dst = builder.enum_init(&name, "None", target_type, vec![]);
        crate::ir::builder::FunctionBuilder::copy(dst)
    }

    pub fn ensure_option_type_registered(&mut self, option_name: &str, inner_type: TypeId) {
        use super::types::make_option_type_def;
        use crate::ir::types::GirType;
        // Coherence-at-construction (Tier 1c, `docs/devbook/25-structural-guards.md`):
        // make_option_type_def now reads the inner type's drop status from
        // the registry and writes Recursive + Resource into the wrapper's
        // metadata directly, so the post-hoc fix-up that lived here is no
        // longer needed. Bypassing get_or_register keeps its closure
        // signature unchanged; the body mirrors get_or_register's caching.
        if self.type_mapper.lookup_named(option_name).is_some() { return; }
        let td = make_option_type_def(option_name, inner_type, &self.type_registry);
        self.type_registry.add_type_def(td);
        let type_id = self.type_registry.insert(GirType::Named(option_name.to_string()));
        self.type_mapper.register_named(option_name.to_string(), type_id);
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
                        borrow_view_fn: protocol.borrow_view_fn.map(String::from),
                        collection_kind: protocol.collection_kind,
                        enum_category: None,
                        c_runtime_alias: protocol.c_runtime_alias.map(String::from),
                        is_closure_env: false,
                        is_box: false,
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
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            expr_has_await(&condition.node)
            || expr_has_await(&then_branch.node)
            || elif_branches.iter().any(|(c, b)| {
                expr_has_await(&c.node) || expr_has_await(&b.node)
            })
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
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            expr_has_await(&condition.node)
            || then_body.stmts.iter().any(|s| stmt_has_await(&s.node))
            || elif_branches.iter().any(|(c, b)| {
                expr_has_await(&c.node)
                    || b.stmts.iter().any(|s| stmt_has_await(&s.node))
            })
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
        Expr::UnaryOp { operand, .. }
        | Expr::Move { expr: operand }
        | Expr::Propagate { expr: operand } => {
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
