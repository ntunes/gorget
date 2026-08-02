use rustc_hash::{FxHashMap, FxHashSet};
use std::time::{Duration, Instant};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, ScopeId, TypeId};
use super::resolve::{FunctionInfo, ResolutionMap};
use super::scope::ScopeTable;
use super::types::{self, ResolvedType, TypeTable};

// ─── Fallible State (Option/Result tracking) ──────────────

/// Whether an Option/Result variable has been guard-checked.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FallibleState {
    /// Not yet checked with is_some/is_ok/match.
    Unchecked,
    /// Checked via guard (is_some, is_ok, match, etc.).
    Checked,
}

// ─── Variable State ────────────────────────────────────────

/// Tracks the ownership state of a variable.
#[derive(Debug, Clone)]
pub(super) enum VarState {
    /// Variable is available for use.
    Live,
    /// Ownership was transferred; cannot use.
    Moved { moved_at: Span },
}

/// Snapshot of all variable states (for branching).
pub(super) type StateSnapshot = FxHashMap<DefId, VarState>;

/// Dead-write lint (`DeadBareParamWrite`): per-bare-resource-param tracking.
///
/// Deliberately NOT threaded through `BranchState` (a documented deviation
/// from devbook 10's thread-every-axis rule): reads/writes are recorded in
/// walk order across all branches (union semantics), which can only SUPPRESS
/// a warning — a sibling-branch read counts as a read-after-write even when
/// no path executes both. That is conservative in the anti-false-positive
/// direction, the right lean for a warning; per-branch precision would only
/// add true positives at the cost of threading a new axis through every
/// save/restore/merge site.
#[derive(Debug, Clone)]
pub(super) struct DeadWriteInfo {
    /// Parameter name (for the warning message).
    pub(super) name: String,
    /// The parameter's declaration site (secondary label on the warning).
    pub(super) param_span: Span,
    /// Clock of the most recent genuine read (not a write-target read).
    pub(super) last_read: Option<u32>,
    /// Clock + span of the most recent mutation through the param.
    pub(super) last_write: Option<(u32, Span)>,
    /// Loop ids enclosing the most recent write.
    pub(super) write_loops: Vec<u32>,
    /// Loop ids enclosing any genuine read (loop-carried read-after-write:
    /// a read anywhere inside a loop that also contains a write dynamically
    /// follows the write on iterations 2+, so it suppresses).
    pub(super) read_loops: FxHashSet<u32>,
}

// ─── Capture Set Tracking ─────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BorrowCaptureMode {
    Read,
    Mutable,
}

#[derive(Debug, Clone)]
pub(super) struct CaptureEntry {
    pub(super) def_id: DefId,
    pub(super) name: String,
    pub(super) mode: BorrowCaptureMode,
    /// True if the captured variable has a non-Static borrow origin
    /// (str param, &T, reference derived from local, etc.)
    pub(super) has_borrowed_origin: bool,
}

#[derive(Debug, Clone, Default)]
pub(super) struct CaptureSet {
    pub(super) captures: Vec<CaptureEntry>,
}


// ─── Borrow Origin ────────────────────────────────────────

/// Tracks where a reference-typed value originated from.
/// Used for lifetime inference: prevents returning references to locals
/// and using references after their source is moved.
#[derive(Debug, Clone)]
pub(super) enum BorrowOrigin {
    /// String literal or global constant — always valid.
    Static,
    /// Function parameter — valid in caller's scope.
    Param { #[allow(dead_code)] param_index: usize, def_id: DefId },
    /// Local variable — scope-limited, can't escape the function.
    Local(DefId),
    /// Match binding — owns data extracted from the scrutinee.
    /// `binding_def_id` is this binding's DefId (for move/invalidation tracking).
    /// `scrutinee_origin` is the matched expression's origin (for lifetime/return checks).
    /// `is_ref` is true when the binding has a reference type (str, &T, etc.),
    /// meaning it borrows from the scrutinee rather than owning the data.
    MatchBinding { binding_def_id: DefId, scrutinee_origin: Box<BorrowOrigin>, is_ref: bool },
    /// Call result — inherits origins from callee's `return_borrows_from` args.
    CallResult(Vec<BorrowOrigin>),
    /// Heap-allocated owned value (f-string, etc.) — local lifetime.
    /// Like Local for escape analysis (contains_local() = true) but has no DefId.
    Owned,
    /// Conservative fallback — treated as potentially local in Phase 1,
    /// refined to CallResult in Phase 2+ when callee info is available.
    Unknown,
}

impl BorrowOrigin {
    /// Returns true if this origin (or any nested origin) references a local.
    /// Unknown is treated conservatively — it might be local.
    fn contains_local(&self) -> bool {
        match self {
            BorrowOrigin::Local(_) | BorrowOrigin::Owned => true,
            BorrowOrigin::Unknown => true,
            // Only reference-type bindings borrow from the scrutinee.
            // Non-reference bindings own their extracted data independently.
            BorrowOrigin::MatchBinding { scrutinee_origin, is_ref, .. } => {
                if *is_ref { scrutinee_origin.contains_local() } else { false }
            }
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.contains_local()),
            _ => false,
        }
    }

    /// Returns true if this origin (or any nested origin) references the given DefId.
    fn references_def(&self, target: DefId) -> bool {
        match self {
            BorrowOrigin::Local(def_id) | BorrowOrigin::Param { def_id, .. } => *def_id == target,
            // Only check the binding itself, NOT the scrutinee — sibling bindings are independent
            BorrowOrigin::MatchBinding { binding_def_id, .. } => *binding_def_id == target,
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.references_def(target)),
            BorrowOrigin::Static | BorrowOrigin::Owned | BorrowOrigin::Unknown => false,
        }
    }

    /// Collect the names of all locals referenced by this origin.
    fn local_names(&self, scopes: &ScopeTable) -> Vec<String> {
        match self {
            BorrowOrigin::Local(def_id) => vec![scopes.get_def(*def_id).name.clone()],
            BorrowOrigin::MatchBinding { binding_def_id, .. } => vec![scopes.get_def(*binding_def_id).name.clone()],
            BorrowOrigin::Owned => vec!["<f-string>".to_string()],
            BorrowOrigin::Unknown => vec!["<unresolved origin>".to_string()],
            BorrowOrigin::CallResult(origins) => {
                origins.iter().flat_map(|o| o.local_names(scopes)).collect()
            }
            _ => vec![],
        }
    }

    /// Returns true if this origin (or any nested origin) is Unknown.
    fn contains_unknown(&self) -> bool {
        match self {
            BorrowOrigin::Unknown => true,
            BorrowOrigin::MatchBinding { scrutinee_origin, is_ref, .. } => {
                if *is_ref { scrutinee_origin.contains_unknown() } else { false }
            }
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.contains_unknown()),
            _ => false,
        }
    }

    /// Collect all source DefIds referenced by this origin.
    fn source_def_ids(&self) -> Vec<DefId> {
        match self {
            BorrowOrigin::Param { def_id, .. } | BorrowOrigin::Local(def_id) => vec![*def_id],
            BorrowOrigin::MatchBinding { binding_def_id, .. } => vec![*binding_def_id],
            BorrowOrigin::CallResult(origins) => {
                origins.iter().flat_map(|o| o.source_def_ids()).collect()
            }
            BorrowOrigin::Static | BorrowOrigin::Owned | BorrowOrigin::Unknown => vec![],
        }
    }
}

/// Snapshot of origin tracking state (for branching).
pub(super) type OriginSnapshot = FxHashMap<DefId, BorrowOrigin>;

/// Info about a local derived from a shared variable, used for stale-condition tracking.
#[derive(Debug, Clone)]
pub(super) struct SharedDerivedInfo {
    pub(super) local_name: String,
    pub(super) shared_name: String,
    /// Where the local was derived from the shared variable (VarDecl or assignment span).
    pub(super) derivation_span: Span,
    /// The await point that made this entry stale (set when promoted from shared_derived to stale).
    pub(super) await_span: Option<Span>,
}

/// Combined snapshot of variable states and origin tracking for branching.
/// Used by save/restore/merge_branch_state to handle all branching state atomically.
pub(super) struct BranchState {
    pub(super) var_states: StateSnapshot,
    pub(super) origins: OriginSnapshot,
    pub(super) invalidated: FxHashSet<DefId>,
    pub(super) reassignment_invalidated: FxHashMap<DefId, (String, Span)>,
    pub(super) await_invalidated: FxHashSet<DefId>,
    /// Variables currently captured mutably by live closures.
    pub(super) mut_captured_vars: FxHashMap<DefId, Vec<(String, DefId, Span)>>,
    /// Reverse map: closure DefId → captured variable DefIds.
    pub(super) mut_capture_owners: FxHashMap<DefId, Vec<DefId>>,
    /// Locals derived from shared variables (not yet stale).
    pub(super) shared_derived: FxHashMap<DefId, SharedDerivedInfo>,
    /// Locals derived from shared that became stale after await.
    pub(super) stale_shared_derived: FxHashMap<DefId, SharedDerivedInfo>,
    /// Whether this branch always diverges (return/break/continue/throw).
    pub(super) diverges: bool,
    /// Fallible (Option/Result) variable guard states.
    pub(super) fallible_states: FxHashMap<DefId, FallibleState>,
    /// Implicit CoW borrows: variable DefId → (source-root DefId, field path).
    /// Populated when a variable is bound from `.get().unwrap()` or `vec[i]`
    /// on a collection with resource-type elements.
    /// Field path lets the mutation-time invalidation check prune disjoint
    /// sibling-field mutations: `gpu.shader_cache.get(i)` borrows from the
    /// path `gpu.shader_cache`, so `gpu.deform_face_indices.push(...)` doesn't
    /// invalidate it.
    pub(super) index_borrow_sources: FxHashMap<DefId, IndexBorrowSource>,
    /// Live Mutex guards (Mutex DefId → (Guard DefId, name, lock span)).
    /// Branched independently so that locks acquired in one arm don't leak
    /// into the other arm (e.g. `if cond: lock_m() else: lock_m()`).
    pub(super) live_guards: FxHashMap<DefId, (DefId, String, Span)>,
}

/// Where an implicit CoW borrow points: the root binding plus the field
/// path projected from it. An empty path means the borrow is from the
/// root binding itself (e.g. `let x = vec.get(0)`); a non-empty path
/// means the borrow is from a sub-collection (e.g. `let x =
/// gpu.shader_cache.get(0)` records `gpu` + `["shader_cache"]`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct IndexBorrowSource {
    pub(super) root: DefId,
    pub(super) field_path: Vec<String>,
}

// ─── Blocking Call Detection ───────────────────────────────
// These AST-level names correspond to functions that release the shared-variable
// token (blocking I/O, sleep).  Must stay in sync with
// `src/ir/lowering/exprs/mod.rs::BLOCKING_CALL_NAMES` and the broader list in
// `src/ir/transforms/shared_async.rs::BLOCKING_STDLIB_CALLS`.

pub(super) const BLOCKING_CALL_NAMES: &[&str] = &[
    "sleep", "read_file", "write_file", "append_file",
    "readdir", "http_get", "http_post", "http_put", "http_delete",
];

// NOTE: `is_blocking_call_name` and `expr_contains_yield_point` are now
// purity-aware methods on BorrowChecker. BLOCKING_CALL_NAMES above is still
// used by `is_yield_point_call` for backwards compatibility.

mod type_utils;
use type_utils::*;
mod origins;
mod helpers;
mod check_expr;
mod check_stmt;

// ─── Borrow Checker ────────────────────────────────────────

pub(super) struct BorrowChecker<'a> {
    pub(super) scopes: &'a ScopeTable,
    pub(super) types: &'a TypeTable,
    pub(super) resolution_map: &'a ResolutionMap,
    pub(super) function_info: &'a FxHashMap<DefId, FunctionInfo>,
    pub(super) function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
    pub(super) errors: Vec<SemanticError>,
    /// Variable state: DefId -> current state.
    pub(super) var_states: FxHashMap<DefId, VarState>,
    /// Nesting depth inside loops (for move-in-loop detection).
    pub(super) loop_depth: usize,
    /// Stack of DefId sets: variables declared in each loop nesting level.
    /// Variables declared within a loop body are re-created each iteration
    /// and can safely be moved. Only variables from OUTSIDE the innermost
    /// loop are rejected.
    pub(super) loop_local_defs: Vec<FxHashSet<DefId>>,
    /// While walking the RHS of `x = ...`, this holds `Some(x's DefId)`.
    /// Lets `check_move` waive the move-in-loop guard for the
    /// `x = f(!x, …)` left-fold pattern: the move IS followed by an
    /// immediate rebind of the same name in the same statement, so the
    /// next iteration sees a fresh value, not a moved one.
    pub(super) assignment_rebind_target: Option<DefId>,
    /// DefIds of collections currently being iterated over by for-loops.
    /// Mutating these collections inside the loop body is an error (iterator invalidation).
    pub(super) for_loop_iterables: FxHashSet<DefId>,
    /// Nesting depth inside arena `with` blocks (for escape detection).
    pub(super) arena_depth: usize,
    /// Variables declared while arena_depth > 0 that hold non-Copy types.
    /// These must not escape the arena scope.
    pub(super) arena_scoped_vars: FxHashSet<DefId>,
    /// Expression type map from the type checker (for lifetime tracking).
    pub(super) expr_types: &'a FxHashMap<Span, TypeId>,
    /// Current function's body scope (for scope-aware variable lookup).
    pub(super) current_fn_scope: Option<ScopeId>,

    // ── Struct borrowing state (Phase 4) ──
    /// Structs that contain reference-type fields (directly or transitively).
    pub(super) ref_type_structs: FxHashSet<DefId>,
    /// Per-struct field flags: true if that field's type is a reference type.
    pub(super) struct_field_ref_flags: FxHashMap<DefId, Vec<bool>>,
    /// Per-struct field flags: true if that field's type is a `MutRef[T]`
    /// (or sigil `T &`) — i.e. an exclusive borrow. Subset of
    /// `struct_field_ref_flags`. Used to enforce MutRef exclusivity at
    /// construction time.
    pub(super) struct_field_mut_ref_flags: FxHashMap<DefId, Vec<bool>>,
    /// Per-struct field NAMES in declaration order (index-aligned with
    /// `DefInfo.field_types`). Covers ALL structs. Used by the arena-escape
    /// lvalue-type resolver to recover a named field's value type for a
    /// `struct.field += …` / `struct.field = …` compound/assign target.
    pub(super) struct_field_names: FxHashMap<DefId, Vec<String>>,

    // ── Lifetime inference state ──
    /// Origin of each reference-typed variable.
    pub(super) var_origins: FxHashMap<DefId, BorrowOrigin>,
    /// DefIds whose data has been moved (invalidated).
    pub(super) invalidated_origins: FxHashSet<DefId>,
    /// Current function's return type (if it's a reference type).
    pub(super) current_return_type_id: Option<TypeId>,
    /// Depth of Item::Module nesting. When > 0, we're checking an imported module's
    /// code. DanglingReturn checks are skipped because (a) imported code is validated
    /// by its own project's borrow checker, and (b) cross-module origin tracking has
    /// limitations — built-in methods (unwrap, get, etc.) aren't in method_resolutions,
    /// so origin tracking falls back to conservative Local origins, causing false positives.
    pub(super) imported_module_depth: usize,
    /// Current function's param (DefId, param_index) pairs.
    pub(super) current_param_def_ids: Vec<(DefId, usize)>,

    // ── Implicit CoW borrow tracking ──
    /// Variable DefId → source collection DefId. Populated when a variable
    /// is bound from `.get().unwrap()` or `vec[i]` on a collection with
    /// resource-type elements. Used to detect MutationWhileBorrowed for
    /// implicit CoW borrows (not just explicit `T &` references).
    pub(super) index_borrow_sources: FxHashMap<DefId, IndexBorrowSource>,

    // ── Borrow dependency export (for drop ordering) ──
    /// Per-local borrow sources: borrower DefId → Vec<source DefId>.
    /// Exported to AnalysisResult so the drop elaborator can order drops correctly.
    pub(super) borrow_deps: FxHashMap<DefId, Vec<DefId>>,

    // ── Method resolution (Phase 7) ──
    /// Method span start → DefId (from typechecker, for origin/temporary tracking).
    pub(super) method_resolutions: &'a FxHashMap<usize, super::MethodResolution>,

    // ── Reassignment invalidation (Phase 11) ──
    /// Variables whose borrow source was reassigned, making their reference stale.
    /// Maps the dependent variable's DefId → (source_name, reassignment_span).
    pub(super) reassignment_invalidated: FxHashMap<DefId, (String, Span)>,

    /// Whether the current execution path has unconditionally diverged
    /// (return, break, continue, throw). Used to exclude diverging branches
    /// from state merges so that moves in early-return paths don't poison
    /// the post-branch state.
    pub(super) diverged: bool,

    /// Whether we are inside a return expression (for allowing implicit
    /// constructor moves of non-loop-local vars — return exits the function).
    pub(super) in_return_expr: bool,

    /// Whether the current function is `async`.
    pub(super) current_function_is_async: bool,
    /// Whether the current function has `throws`.
    pub(super) current_function_throws: bool,
    /// Variables with non-static borrow origins that were Live before an `await`.
    /// Using these after the await triggers BorrowAcrossAwait.
    pub(super) await_invalidated: FxHashSet<DefId>,

    /// Capture sets for closure-typed variables, keyed by the variable's DefId.
    pub(super) closure_capture_sets: FxHashMap<DefId, CaptureSet>,
    /// Variables (struct/array) that contain closures capturing local variables.
    /// When returned, these would cause use-after-free.
    pub(super) vars_containing_closures: FxHashSet<DefId>,
    /// Temporarily holds the capture set computed for the most recent closure
    /// expression, picked up by VarDecl to associate it with the variable's DefId.
    pub(super) pending_capture_set: Option<CaptureSet>,

    /// Variables currently captured mutably by live closures.
    /// Maps variable DefId → Vec of (closure variable name, closure DefId, span of closure decl).
    /// Multiple closures may capture the same variable; the Vec tracks all of them.
    /// While any entry is present, reading or writing the variable directly is an error.
    pub(super) mut_captured_vars: FxHashMap<DefId, Vec<(String, DefId, Span)>>,
    /// Reverse map: closure DefId → list of captured variable DefIds registered in
    /// mut_captured_vars.  Used to clean up mut_captured_vars when the closure is moved
    /// or goes out of scope.
    pub(super) mut_capture_owners: FxHashMap<DefId, Vec<DefId>>,
    /// DefIds of variables declared with `shared` — maps to (SharedKind, name, span).
    pub(super) shared_var_defs: FxHashMap<DefId, (crate::parser::ast::SharedKind, String, Span)>,
    /// Locals derived from a shared binding (read before any await).
    pub(super) shared_derived: FxHashMap<DefId, SharedDerivedInfo>,
    /// Locals derived from shared that have become stale (an await occurred
    /// between the read and the current point). Using these in branch conditions
    /// triggers a warning.
    pub(super) stale_shared_derived: FxHashMap<DefId, SharedDerivedInfo>,
    /// Shared variables that have been written to (assigned or compound-assigned).
    pub(super) shared_written: FxHashSet<DefId>,
    /// Shared variables that have been passed as spawn arguments.
    pub(super) shared_spawned: FxHashSet<DefId>,
    /// Output: CFA decisions populated during analysis.
    pub(super) shared_out: FxHashMap<DefId, super::SharedStrategy>,
    /// Stale-condition warnings collected during analysis (§3.4).
    pub(super) stale_warnings: Vec<super::errors::SemanticWarning>,
    /// DefIds of `with` bindings that track shared variables (auto-refresh).
    /// These are exempt from stale-condition warnings — the compiler guarantees
    /// they are refreshed after every await point.
    pub(super) with_shared_tracked: FxHashSet<DefId>,
    /// Stack of (shared_name, condition_span, kind) for enclosing branches/loops
    /// whose condition/iterable references a `with`-tracked shared variable.
    /// Yield points inside these trigger check-then-act or iterator invalidation warnings.
    pub(super) with_guarded_conditions: Vec<(Vec<String>, Span, WithGuardKind)>,
    /// Depth of `with` blocks (for detecting spawn inside `with`).
    pub(super) with_depth: usize,

    /// Inferred function purity (for yield-point detection in `with` blocks).
    pub(super) fn_purity: &'a super::purity::PurityByName,

    /// Tracks local variable declarations for unused-variable detection.
    /// Maps DefId → (name, span, is_used). Reset per function.
    pub(super) local_var_usage: FxHashMap<DefId, (String, Span, bool)>,

    /// Tracks whether Option/Result variables have been guard-checked.
    /// Maps DefId → FallibleState. Reset per function.
    pub(super) fallible_states: FxHashMap<DefId, FallibleState>,

    /// Tracks which variables have been reassigned (for const promotion warnings).
    pub(super) var_reassigned: FxHashSet<DefId>,

    /// Tracks which `&` (MutableBorrow) parameters have been actually mutated.
    pub(super) mut_param_mutated: FxHashSet<DefId>,
    /// `&` parameters in the current function: (DefId, name, span).
    pub(super) current_mut_params: Vec<(DefId, String, Span)>,
    /// BARE (Borrow) parameters of the current function: (DefId, name).
    /// Diagnostic mirror of `current_mut_params` for the
    /// `GG_REPORT_BARE_MUTATED` oracle (see `check_function`'s report block).
    pub(super) current_bare_params: Vec<(DefId, String)>,
    /// Bare (Borrow) params the mutation classifier marked as mutated —
    /// same marking channels as `mut_param_mutated`, recorded for the
    /// `GG_REPORT_BARE_MUTATED` oracle. A bare param on this list is one the
    /// corrected NeedlessMutableBorrow lint would KEEP `&` if it were `&` —
    /// i.e. baring it was (or would be) wrong.
    pub(super) bare_param_mutated: FxHashSet<DefId>,
    /// Dead-write lint: bare (Borrow) resource params of the current
    /// function. DefId → tracking info. Reset per function.
    pub(super) deadwrite_params: FxHashMap<DefId, DeadWriteInfo>,
    /// Bare-Res-param roots that appeared as a bare-borrow arg in a SELF-
    /// RECURSIVE call site inside this function's body. Populated at the free-
    /// call and method-call arms in `check_expr`; consumed at end-of-
    /// `check_function` by the `W_RecursiveBareParamMaterialize` emit block,
    /// intersected with `bare_param_mutated` (an existing set) — the diagnostic
    /// fires only for params both mutated in the body AND recursed bare. Reset
    /// per function (inline in `check_function`, alongside the other per-fn
    /// sets — NOT in `reset_per_function_state`, which is a different set).
    pub(super) bare_param_recursed_bare: FxHashSet<DefId>,
    /// The DefId of the function currently being checked. Set at the top of
    /// `check_function` via `scopes.lookup_def_by_span(&func.name.node,
    /// func.name.span)` (the identity-preserving lookup that also succeeds for
    /// equip methods — plain `scopes.lookup` returns the wrong def for equip
    /// methods because the equip-block scope is not pushed at `check_function`
    /// entry). Used by the self-recursion classifier:
    /// `resolve_callee_def_id(callee) == self.current_fn_def_id` is the
    /// self-recursive-call predicate at both call arms. Cleared per function.
    pub(super) current_fn_def_id: Option<DefId>,
    /// Monotonic event clock for read/write ordering within a function walk.
    pub(super) deadwrite_clock: u32,
    /// Stack of unique loop ids currently being walked (For/While/Loop bodies).
    pub(super) deadwrite_loop_stack: Vec<u32>,
    /// Allocator for loop ids.
    pub(super) deadwrite_next_loop_id: u32,
    /// When Some(def_id): the walker is inside the target/receiver of a
    /// mutation of that def — Identifier reads of it are part of the write,
    /// not a use of the materialized copy, so they are not recorded.
    pub(super) deadwrite_write_root: Option<DefId>,
    /// Span start of the expression of the current `Stmt::Expr`, if any.
    /// A mutating method call at exactly this span is in statement position
    /// (result discarded → pure write); anywhere else its result is consumed
    /// (a read of the copy).
    pub(super) deadwrite_stmt_expr_start: Option<usize>,
    /// Whether to emit CouldBeConst warnings (opt-in via `--warn-const`).
    pub(super) warn_const: bool,
    /// True when checking the value expression of a destructuring VarDecl
    /// (Pattern::Tuple). Destructuring implicitly moves the value, so
    /// `MoveWithoutOperator` is suppressed.
    pub(super) in_destructuring_bind: bool,

    /// Tracks live `Guard` bindings to detect double-lock deadlocks.
    /// Maps Mutex/RwLock DefId → (Guard DefId, guard name, lock span). A
    /// second `m.lock()` while the prior guard is still in scope would
    /// deadlock at runtime (locks are non-reentrant). Entries are removed
    /// on block exit (see `check_block`).
    pub(super) live_guards: FxHashMap<DefId, (DefId, String, Span)>,

    /// Round XXV Track D §D-3 — set to `true` at the option-D intercepts in
    /// `check_stmt::VarDecl` / `Stmt::Assign` when the RHS is a COMPOUND-
    /// shape borrow-bind (an `if` / `match` / `do` / block whose tail is a
    /// `&`-of-a-place, per `type_utils::expr_is_borrow_bind`). D10(a) at
    /// the typechecker owns the authoritative `E_LocalBorrowBind` on the
    /// SAME syntax, so the safety-pass mirror-walker `E_AmpInOperandPosition`
    /// on the branch/arm/tail `&`s would be a DUPLICATE. The
    /// `check_expr::Expr::MutableBorrow` arm (statement position) + the
    /// f-string-interp `Expr::MutableBorrow` arm inside
    /// `check_interpolation_expr` SKIP their emit when this flag is set.
    /// RESET to `false` after each statement so it never leaks to siblings.
    pub(super) suppress_amp_in_operand_position: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum WithGuardKind {
    BranchCondition,
    Iteration,
}

/// Consume context for `arena_backed_source` — distinguishes a value that is
/// BOUND/returned (a plain string literal stays a static view → safe) from
/// one INGESTED into an owning collection (a literal materializes an owned
/// heap copy through the arena allocator → escapes).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum EscapeCtx {
    /// Assign-to-outer / return: literal binds as a static view.
    Bind,
    /// Element push/put/insert/add/send into an owning collection.
    Ingest,
}

impl<'a> BorrowChecker<'a> {
    fn new(
        scopes: &'a ScopeTable,
        types: &'a TypeTable,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
        function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
        expr_types: &'a FxHashMap<Span, TypeId>,
        method_resolutions: &'a FxHashMap<usize, super::MethodResolution>,
        ref_type_structs: FxHashSet<DefId>,
        struct_field_ref_flags: FxHashMap<DefId, Vec<bool>>,
        struct_field_mut_ref_flags: FxHashMap<DefId, Vec<bool>>,
        struct_field_names: FxHashMap<DefId, Vec<String>>,
        fn_purity: &'a super::purity::PurityByName,
    ) -> Self {
        Self {
            scopes,
            types,
            resolution_map,
            function_info,
            function_body_scopes,
            errors: Vec::new(),
            var_states: FxHashMap::default(),
            loop_depth: 0,
            loop_local_defs: Vec::new(),
            assignment_rebind_target: None,
            for_loop_iterables: FxHashSet::default(),
            arena_depth: 0,
            arena_scoped_vars: FxHashSet::default(),
            expr_types: expr_types,
            current_fn_scope: None,
            method_resolutions,
            ref_type_structs,
            struct_field_ref_flags,
            struct_field_mut_ref_flags,
            struct_field_names,
            var_origins: FxHashMap::default(),
            invalidated_origins: FxHashSet::default(),
            index_borrow_sources: FxHashMap::default(),
            borrow_deps: FxHashMap::default(),
            current_return_type_id: None,
            imported_module_depth: 0,
            current_param_def_ids: Vec::new(),
            reassignment_invalidated: FxHashMap::default(),
            diverged: false,
            in_return_expr: false,
            current_function_is_async: false,
            current_function_throws: false,
            await_invalidated: FxHashSet::default(),
            closure_capture_sets: FxHashMap::default(),
            vars_containing_closures: FxHashSet::default(),
            pending_capture_set: None,
            mut_captured_vars: FxHashMap::default(),
            mut_capture_owners: FxHashMap::default(),
            shared_var_defs: FxHashMap::default(),
            shared_derived: FxHashMap::default(),
            stale_shared_derived: FxHashMap::default(),
            shared_written: FxHashSet::default(),
            shared_spawned: FxHashSet::default(),
            shared_out: FxHashMap::default(),
            stale_warnings: Vec::new(),
            with_shared_tracked: FxHashSet::default(),
            with_guarded_conditions: Vec::new(),
            with_depth: 0,
            fn_purity,
            local_var_usage: FxHashMap::default(),
            fallible_states: FxHashMap::default(),
            var_reassigned: FxHashSet::default(),
            mut_param_mutated: FxHashSet::default(),
            current_mut_params: Vec::new(),
            current_bare_params: Vec::new(),
            bare_param_mutated: FxHashSet::default(),
            deadwrite_params: FxHashMap::default(),
            bare_param_recursed_bare: FxHashSet::default(),
            current_fn_def_id: None,
            deadwrite_clock: 0,
            deadwrite_loop_stack: Vec::new(),
            deadwrite_next_loop_id: 0,
            deadwrite_write_root: None,
            deadwrite_stmt_expr_start: None,
            warn_const: false,
            in_destructuring_bind: false,
            live_guards: FxHashMap::default(),
            suppress_amp_in_operand_position: false,
        }
    }

    fn error(&mut self, kind: SemanticErrorKind, span: Span) {
        self.errors.push(SemanticError { kind, span });
    }

    /// Returns the type name ("Option" or "Result") if the TypeId is an Option/Result type.
    fn is_option_or_result_type(&self, type_id: TypeId) -> Option<String> {
        if let ResolvedType::Generic(def_id, _) = self.types.get(type_id) {
            let name = self.scopes.get_def(*def_id).name.clone();
            if name == "Option" || name == "Result" {
                return Some(name);
            }
        }
        None
    }

}



mod return_borrows;
use return_borrows::compute_all_return_borrows;

// ─── Pass 5b: Full Borrow Check ──────────────────────────────

/// Run borrow checking on the entire module.
pub fn check_module(
    module: &Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &FxHashMap<(String, usize), ScopeId>,
    expr_types: &FxHashMap<Span, TypeId>,
    method_resolutions: &FxHashMap<usize, super::MethodResolution>,
    errors: &mut Vec<SemanticError>,
    warn_const: bool,
) -> (FxHashMap<DefId, super::SharedStrategy>, Vec<super::errors::SemanticWarning>, super::purity::PurityByName, FxHashMap<DefId, Vec<DefId>>, FxHashMap<&'static str, Duration>) {
    let mut pt: FxHashMap<&'static str, Duration> = FxHashMap::default();
    macro_rules! time { ($name:expr, $e:expr) => {{
        let t = Instant::now();
        let r = $e;
        *pt.entry($name).or_default() += t.elapsed();
        r
    }} }
    // Phase 4: compute which structs have reference-type fields
    let ref_type_structs = time!("compute_ref_type_structs", compute_ref_type_structs(module, scopes));
    let struct_field_ref_flags = time!("compute_struct_field_ref_flags", compute_struct_field_ref_flags(module, scopes, &ref_type_structs));
    let struct_field_mut_ref_flags = time!("compute_struct_field_mut_ref_flags", compute_struct_field_mut_ref_flags(module, scopes, &ref_type_structs));
    let struct_field_names = time!("compute_struct_field_names", compute_struct_field_names(module, scopes));

    // Pass 5a: compute return_borrows_from for each function
    time!("compute_all_return_borrows", compute_all_return_borrows(module, scopes, types, resolution_map, function_info, &ref_type_structs));

    // Pass 5b½: Purity inference — lightweight AST walk (moved before borrow check
    // so purity info is available for yield-point detection in `with` blocks).
    let purity_by_name = time!("infer_purity", infer_purity(module, scopes, resolution_map));

    // Pass 5b: full borrow check with origin tracking
    let mut checker = BorrowChecker::new(
        scopes, types, resolution_map, function_info, function_body_scopes,
        expr_types,
        method_resolutions, ref_type_structs, struct_field_ref_flags,
        struct_field_mut_ref_flags,
        struct_field_names,
        &purity_by_name,
    );
    checker.warn_const = warn_const;

    time!("check_items_recursive", check_items_recursive(&mut checker, &module.items));

    // Final CFA pass: assign default strategies for shared vars not yet decided.
    // - If a shared var is spawned AND locally written → upgrade to ArcMutex
    //   (main thread writes + spawned thread reads = data race without mutex)
    // - If a shared var is spawned but never locally written → keep cfa_at_spawn's decision
    // - If a shared var is never spawned → ArcMutex if written, ArcOnly if read-only
    let mut warnings = Vec::new();
    for (&def_id, (_, name, span)) in &checker.shared_var_defs {
        let entry = checker.shared_out.entry(def_id).or_insert_with(|| {
            // Not spawned at all — decide based on local writes
            if checker.shared_written.contains(&def_id) {
                super::SharedStrategy::ArcMutex
            } else {
                super::SharedStrategy::ArcOnly
            }
        });
        // If spawned as ArcOnly but locally written, upgrade to ArcMutex
        if *entry == super::SharedStrategy::ArcOnly && checker.shared_written.contains(&def_id) {
            *entry = super::SharedStrategy::ArcMutex;
        }
        // Warn if a shared var never crosses a concurrency boundary
        if !checker.shared_spawned.contains(&def_id) {
            warnings.push(super::errors::SemanticWarning {
                kind: super::errors::SemanticWarningKind::UnnecessaryShared {
                    name: name.clone(),
                },
                span: *span,
            });
        }
    }

    // Phase 4: Unused import detection
    time!("unused_imports", {
        let mut used_def_ids: FxHashSet<DefId> = resolution_map.values().copied().collect();
        collect_used_type_def_ids(&module.items, scopes, &mut used_def_ids);
        let mut imported_defs: Vec<(DefId, String, Span)> = Vec::new();
        collect_imported_defs(&module.items, scopes, &mut imported_defs);
        for (def_id, name, span) in &imported_defs {
            if !used_def_ids.contains(def_id) && !name.starts_with('_') {
                warnings.push(super::errors::SemanticWarning {
                    kind: super::errors::SemanticWarningKind::UnusedImport {
                        name: name.clone(),
                    },
                    span: *span,
                });
            }
        }
    });

    // Phase 8: Private-in-public signature detection
    time!("check_private_in_public", check_private_in_public(&module.items, scopes, &mut checker.errors));

    warnings.extend(checker.stale_warnings);
    errors.extend(checker.errors);
    let shared_out = checker.shared_out;
    let borrow_deps = checker.borrow_deps;

    (shared_out, warnings, purity_by_name, borrow_deps, pt)
}

fn check_items_recursive(checker: &mut BorrowChecker, items: &[Spanned<Item>]) {
    for item in items {
        match &item.node {
            Item::Module { items: inner, .. } => {
                checker.imported_module_depth += 1;
                check_items_recursive(checker, inner);
                checker.imported_module_depth -= 1;
            }
            Item::Function(f) => {
                checker.check_function(f);
            }
            Item::Equip(impl_block) => {
                for method in &impl_block.items {
                    checker.check_function(&method.node);
                }
            }
            Item::Test(t) => {
                checker.reset_per_function_state();
                checker.check_block(&t.body);
            }
            Item::Bench(b) => {
                checker.reset_per_function_state();
                checker.check_block(&b.body);
            }
            Item::SuiteSetup(s) => {
                checker.reset_per_function_state();
                checker.check_block(&s.body);
            }
            Item::SuiteTeardown(s) => {
                checker.reset_per_function_state();
                checker.check_block(&s.body);
            }
            _ => {}
        }
    }
}

mod validation;
use validation::{check_private_in_public, collect_imported_defs, collect_used_type_def_ids, infer_purity};

#[cfg(test)]
mod tests;
