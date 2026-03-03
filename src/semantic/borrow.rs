use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{ArenaEscapeKind, SemanticError, SemanticErrorKind};
use super::ids::{DefId, ScopeId, TypeId};
use super::resolve::{FunctionInfo, ResolutionMap};
use super::scope::{DefKind, ScopeTable};
use super::types::{self, ResolvedType, TypeTable};

// ─── Variable State ────────────────────────────────────────

/// Tracks the ownership state of a variable.
#[derive(Debug, Clone)]
enum VarState {
    /// Variable is available for use.
    Live,
    /// Ownership was transferred; cannot use.
    Moved { moved_at: Span },
}

/// Snapshot of all variable states (for branching).
type StateSnapshot = FxHashMap<DefId, VarState>;

// ─── Borrow Origin ────────────────────────────────────────

/// Tracks where a reference-typed value originated from.
/// Used for lifetime inference: prevents returning references to locals
/// and using references after their source is moved.
#[derive(Debug, Clone)]
enum BorrowOrigin {
    /// String literal or global constant — always valid.
    Static,
    /// Function parameter — valid in caller's scope.
    Param { #[allow(dead_code)] param_index: usize, def_id: DefId },
    /// Local variable — scope-limited, can't escape the function.
    Local(DefId),
    /// Call result — inherits origins from callee's `return_borrows_from` args.
    CallResult(Vec<BorrowOrigin>),
    /// Conservative fallback — treated as potentially local in Phase 1,
    /// refined to CallResult in Phase 2+ when callee info is available.
    Unknown,
}

impl BorrowOrigin {
    /// Returns true if this origin (or any nested origin) references a local.
    /// Unknown is treated conservatively — it might be local.
    fn contains_local(&self) -> bool {
        match self {
            BorrowOrigin::Local(_) => true,
            BorrowOrigin::Unknown => true,
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.contains_local()),
            _ => false,
        }
    }

    /// Returns true if this origin (or any nested origin) references the given DefId.
    fn references_def(&self, target: DefId) -> bool {
        match self {
            BorrowOrigin::Local(def_id) | BorrowOrigin::Param { def_id, .. } => *def_id == target,
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.references_def(target)),
            BorrowOrigin::Static | BorrowOrigin::Unknown => false,
        }
    }

    /// Collect the names of all locals referenced by this origin.
    fn local_names(&self, scopes: &ScopeTable) -> Vec<String> {
        match self {
            BorrowOrigin::Local(def_id) => vec![scopes.get_def(*def_id).name.clone()],
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
            BorrowOrigin::CallResult(origins) => origins.iter().any(|o| o.contains_unknown()),
            _ => false,
        }
    }

    /// Collect all source DefIds referenced by this origin.
    fn source_def_ids(&self) -> Vec<DefId> {
        match self {
            BorrowOrigin::Param { def_id, .. } | BorrowOrigin::Local(def_id) => vec![*def_id],
            BorrowOrigin::CallResult(origins) => {
                origins.iter().flat_map(|o| o.source_def_ids()).collect()
            }
            BorrowOrigin::Static | BorrowOrigin::Unknown => vec![],
        }
    }
}

/// Active outlives constraint from a call site with `where a outlives b`.
/// Tracks the source DefIds for each group so we can detect violations when
/// the "longer" group's source is invalidated while the "shorter" group's
/// source is still alive.
#[derive(Debug, Clone)]
struct ActiveOutlives {
    longer_group: String,
    shorter_group: String,
    longer_source_def_ids: Vec<DefId>,
    shorter_source_def_ids: Vec<DefId>,
    _call_span: Span,
}

/// Snapshot of origin tracking state (for branching).
type OriginSnapshot = FxHashMap<DefId, BorrowOrigin>;

/// Combined snapshot of variable states and origin tracking for branching.
/// Used by save/restore/merge_branch_state to handle all branching state atomically.
struct BranchState {
    var_states: StateSnapshot,
    origins: OriginSnapshot,
    invalidated: FxHashSet<DefId>,
    reassignment_invalidated: FxHashMap<DefId, (String, Span)>,
    await_invalidated: FxHashSet<DefId>,
    /// Whether this branch always diverges (return/break/continue/throw).
    diverges: bool,
}

// ─── Copy Type Detection ───────────────────────────────────

/// Returns true if a type is Copy (trivially copyable, no `!` needed).
///
/// `str` is Copy — an immutable view (`const char*`) that never owns memory.
/// `String` (PrimitiveType::StringType) is non-Copy — it owns a heap buffer
/// (GorgetString struct) and must be moved with `!`.
fn is_copy_type(type_id: TypeId, types: &TypeTable, scopes: &ScopeTable) -> bool {
    match types.get(type_id) {
        ResolvedType::Primitive(prim) => {
            use PrimitiveType::*;
            matches!(
                prim,
                Int | Int8
                    | Int16
                    | Int32
                    | Int64
                    | Uint
                    | Uint8
                    | Uint16
                    | Uint32
                    | Uint64
                    | Float
                    | Float32
                    | Float64
                    | Bool
                    | Char
                    | Str
                    | CStr
            )
        }
        ResolvedType::Void | ResolvedType::Never | ResolvedType::Error => true,
        ResolvedType::Tuple(elems) => {
            let elems = elems.clone();
            elems.iter().all(|e| is_copy_type(*e, types, scopes))
        }
        ResolvedType::Generic(def_id, _) => {
            // Channel[T], Shared[T], Weak[T], and Mutex[T] are Copy — they're opaque pointers.
            // Guard[T] and TaskGroup are NOT Copy — they hold exclusive resources.
            matches!(scopes.get_def(*def_id).name.as_str(), "Channel" | "Shared" | "Weak" | "Mutex")
        }
        ResolvedType::Defined(def_id) => {
            // Arena/TrackingAllocator/PoolAllocator/TlsfAllocator/FixedBufferAllocator/FallbackAllocator are Copy — they're pointers
            matches!(scopes.get_def(*def_id).name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator")
        }
        // Everything else is non-Copy (String, structs, enums, etc.)
        _ => false,
    }
}

// ─── Reference-Type Struct Detection ──────────────────────

/// Check if an AST Type refers to a reference type: `str`, `Slice`, or a named
/// type whose DefId is in `ref_structs`.
fn is_ast_type_ref(ty: &Type, scopes: &ScopeTable, ref_structs: &FxHashSet<DefId>) -> bool {
    match ty {
        Type::Primitive(PrimitiveType::Str) => true,
        Type::Slice { .. } => true,
        Type::Named { name, .. } => {
            // Search from module scope (scope 0) since struct defs are module-level.
            // scopes.current may be at a nested scope after prior passes.
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &name.node) {
                ref_structs.contains(&def_id)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Scan the module's struct definitions and compute which structs contain
/// reference-type fields (directly or transitively). Returns their DefIds.
fn compute_ref_type_structs(module: &Module, scopes: &ScopeTable) -> FxHashSet<DefId> {
    // Collect all struct defs with their DefId and fields
    let mut struct_infos: Vec<(DefId, &[Spanned<FieldDef>])> = Vec::new();
    for item in &module.items {
        if let Item::Struct(s) = &item.node {
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &s.name.node) {
                struct_infos.push((def_id, &s.fields));
            }
        }
    }

    // Fixpoint iteration: keep adding structs until stable
    let mut ref_structs = FxHashSet::default();
    loop {
        let prev_len = ref_structs.len();
        for (def_id, fields) in &struct_infos {
            if ref_structs.contains(def_id) {
                continue;
            }
            for field in *fields {
                if is_ast_type_ref(&field.node.type_.node, scopes, &ref_structs) {
                    ref_structs.insert(*def_id);
                    break;
                }
            }
        }
        if ref_structs.len() == prev_len {
            break;
        }
    }
    ref_structs
}

/// Build a per-struct map of which field indices are reference types.
/// Used to select which struct literal args contribute to the borrow origin.
fn compute_struct_field_ref_flags(
    module: &Module,
    scopes: &ScopeTable,
    ref_type_structs: &FxHashSet<DefId>,
) -> FxHashMap<DefId, Vec<bool>> {
    let mut result = FxHashMap::default();
    for item in &module.items {
        if let Item::Struct(s) = &item.node {
            if let Some(def_id) = scopes.lookup_from_scope(ScopeId(0), &s.name.node) {
                if ref_type_structs.contains(&def_id) {
                    let flags: Vec<bool> = s.fields.iter()
                        .map(|f| is_ast_type_ref(&f.node.type_.node, scopes, ref_type_structs))
                        .collect();
                    result.insert(def_id, flags);
                }
            }
        }
    }
    result
}

// ─── Borrow Checker ────────────────────────────────────────

struct BorrowChecker<'a> {
    scopes: &'a ScopeTable,
    types: &'a TypeTable,
    resolution_map: &'a ResolutionMap,
    function_info: &'a FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
    errors: Vec<SemanticError>,
    /// Variable state: DefId -> current state.
    var_states: FxHashMap<DefId, VarState>,
    /// Nesting depth inside loops (for move-in-loop detection).
    loop_depth: usize,
    /// Stack of DefId sets: variables declared in each loop nesting level.
    /// Variables declared within a loop body are re-created each iteration
    /// and can safely be moved. Only variables from OUTSIDE the innermost
    /// loop are rejected.
    loop_local_defs: Vec<FxHashSet<DefId>>,
    /// Nesting depth inside arena `with` blocks (for escape detection).
    arena_depth: usize,
    /// Variables declared while arena_depth > 0 that hold non-Copy types.
    /// These must not escape the arena scope.
    arena_scoped_vars: FxHashSet<DefId>,
    /// Whether the file has `directive immutable-by-default`.
    immutable_by_default: bool,
    /// Expression type map from the type checker (for lifetime tracking).
    _expr_types: &'a FxHashMap<Span, TypeId>,
    /// Current function's body scope (for scope-aware variable lookup).
    current_fn_scope: Option<ScopeId>,

    // ── Struct borrowing state (Phase 4) ──
    /// Structs that contain reference-type fields (directly or transitively).
    ref_type_structs: FxHashSet<DefId>,
    /// Per-struct field flags: true if that field's type is a reference type.
    struct_field_ref_flags: FxHashMap<DefId, Vec<bool>>,

    // ── Lifetime inference state ──
    /// Origin of each reference-typed variable.
    var_origins: FxHashMap<DefId, BorrowOrigin>,
    /// DefIds whose data has been moved (invalidated).
    invalidated_origins: FxHashSet<DefId>,
    /// Current function's return type (if it's a reference type).
    current_return_type_id: Option<TypeId>,
    /// Current function's param (DefId, param_index) pairs.
    current_param_def_ids: Vec<(DefId, usize)>,

    // ── Method resolution (Phase 7) ──
    /// Method span start → DefId (from typechecker, for origin/temporary tracking).
    method_resolutions: &'a FxHashMap<usize, DefId>,

    // ── Outlives constraints (Phase 5) ──
    /// Active `where a outlives b` constraints from call sites.
    active_outlives: Vec<ActiveOutlives>,

    // ── Reassignment invalidation (Phase 11) ──
    /// Variables whose borrow source was reassigned, making their reference stale.
    /// Maps the dependent variable's DefId → (source_name, reassignment_span).
    reassignment_invalidated: FxHashMap<DefId, (String, Span)>,

    /// Whether the current execution path has unconditionally diverged
    /// (return, break, continue, throw). Used to exclude diverging branches
    /// from state merges so that moves in early-return paths don't poison
    /// the post-branch state.
    diverged: bool,

    /// Whether we are inside a return expression (for allowing implicit
    /// constructor moves of non-loop-local vars — return exits the function).
    in_return_expr: bool,

    /// Whether the current function is `async`.
    current_function_is_async: bool,
    /// Variables with non-static borrow origins that were Live before an `await`.
    /// Using these after the await triggers BorrowAcrossAwait.
    await_invalidated: FxHashSet<DefId>,
}

impl<'a> BorrowChecker<'a> {
    fn new(
        scopes: &'a ScopeTable,
        types: &'a TypeTable,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
        function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
        immutable_by_default: bool,
        expr_types: &'a FxHashMap<Span, TypeId>,
        method_resolutions: &'a FxHashMap<usize, DefId>,
        ref_type_structs: FxHashSet<DefId>,
        struct_field_ref_flags: FxHashMap<DefId, Vec<bool>>,
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
            arena_depth: 0,
            arena_scoped_vars: FxHashSet::default(),
            immutable_by_default,
            _expr_types: expr_types,
            current_fn_scope: None,
            method_resolutions,
            ref_type_structs,
            struct_field_ref_flags,
            var_origins: FxHashMap::default(),
            invalidated_origins: FxHashSet::default(),
            current_return_type_id: None,
            current_param_def_ids: Vec::new(),
            active_outlives: Vec::new(),
            reassignment_invalidated: FxHashMap::default(),
            diverged: false,
            in_return_expr: false,
            current_function_is_async: false,
            await_invalidated: FxHashSet::default(),
        }
    }

    fn error(&mut self, kind: SemanticErrorKind, span: Span) {
        self.errors.push(SemanticError { kind, span });
    }

    /// Mark a variable as Live (e.g., on declaration or reassignment).
    fn mark_live(&mut self, def_id: DefId) {
        self.var_states.insert(def_id, VarState::Live);
        // Track variables declared inside loops so we can allow safe
        // per-iteration moves (variable is re-created each iteration).
        if let Some(local_set) = self.loop_local_defs.last_mut() {
            local_set.insert(def_id);
        }
    }

    /// Check that a variable is usable (Live). Error if Moved.
    /// Also checks if a reference-typed variable's source has been invalidated.
    fn check_use(&mut self, def_id: DefId, span: Span) {
        if let Some(VarState::Moved { moved_at }) = self.var_states.get(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(
                SemanticErrorKind::UseAfterMove {
                    name,
                    moved_at: *moved_at,
                },
                span,
            );
            return;
        }

        // Phase 11: Check if this variable's borrow source was reassigned.
        if let Some((source_name, reassigned_at)) = self.reassignment_invalidated.get(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(
                SemanticErrorKind::UseAfterSourceMoved {
                    name,
                    source_name: source_name.clone(),
                    moved_at: *reassigned_at,
                },
                span,
            );
            return;
        }

        // Lifetime check: if this variable has a reference type, check that its
        // source hasn't been moved/invalidated.
        if let Some(origin) = self.var_origins.get(&def_id).cloned() {
            for &invalidated_id in &self.invalidated_origins {
                if origin.references_def(invalidated_id) {
                    let name = self.scopes.get_def(def_id).name.clone();
                    let source_name = self.scopes.get_def(invalidated_id).name.clone();
                    // Find the span where the source was moved
                    let moved_at = if let Some(VarState::Moved { moved_at }) = self.var_states.get(&invalidated_id) {
                        *moved_at
                    } else {
                        span
                    };
                    self.error(
                        SemanticErrorKind::UseAfterSourceMoved {
                            name,
                            source_name,
                            moved_at,
                        },
                        span,
                    );
                    return;
                }
            }
        }

        // Async: check if variable was alive before an await suspension point.
        if self.await_invalidated.contains(&def_id) {
            let name = self.scopes.get_def(def_id).name.clone();
            self.error(SemanticErrorKind::BorrowAcrossAwait { name }, span);
            self.await_invalidated.remove(&def_id); // prevent duplicate errors
            return;
        }
    }

    // ─── Origin Tracking ──────────────────────────────────

    /// Merge multiple origins into a single origin.
    fn merge_origins(origins: Vec<BorrowOrigin>) -> BorrowOrigin {
        match origins.len() {
            0 => BorrowOrigin::Static,
            1 => origins.into_iter().next().unwrap(),
            _ => BorrowOrigin::CallResult(origins),
        }
    }

    /// Compute the borrow origin of an expression.
    fn compute_expr_origin(&self, expr: &Spanned<Expr>) -> BorrowOrigin {
        match &expr.node {
            // String literals are always valid (static storage).
            Expr::StringLiteral(_) => BorrowOrigin::Static,

            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(def_id);

                    // Is it a parameter?
                    if def.is_param {
                        if let Some((_, idx)) = self.current_param_def_ids.iter().find(|(id, _)| *id == def_id) {
                            return BorrowOrigin::Param { param_index: *idx, def_id };
                        }
                    }

                    // Is it a local variable with a known origin?
                    if let Some(origin) = self.var_origins.get(&def_id) {
                        return origin.clone();
                    }

                    // Only mark as Local if this variable owns data (non-reference, non-callable type).
                    // Reference-type locals that aren't in var_origins (e.g. pattern bindings
                    // from match arms) are views into existing data — not new local sources.
                    // Callable-type locals get their capture origins stored in var_origins;
                    // if absent, the closure captures no ref-type data and is safe to return.
                    if def.kind == DefKind::Variable {
                        if let Some(type_id) = def.type_id {
                            if !types::is_reference_type(type_id, self.types, &self.ref_type_structs)
                                && !types::is_callable_type(type_id, self.types)
                            {
                                return BorrowOrigin::Local(def_id);
                            }
                        } else {
                            // No type info — conservative: treat as local
                            return BorrowOrigin::Local(def_id);
                        }
                    }
                }
                BorrowOrigin::Unknown
            }

            // Field access propagates from the object
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.compute_expr_origin(object)
            }

            // Index/slice propagates from the container
            Expr::Index { object, .. } => self.compute_expr_origin(object),

            // Call/method call: use callee's return_borrows_from if available (Phase 2+)
            Expr::Call { callee, args, .. } => {
                self.compute_call_origin(callee, args)
            }
            Expr::MethodCall { receiver, method, args, .. } => {
                self.compute_method_call_origin(receiver, method, args)
            }

            // If/Match expressions: union of branch origins (conservative)
            Expr::If { then_branch, elif_branches, else_branch, .. } => {
                let mut origins = vec![self.compute_expr_origin(then_branch)];
                for (_, body) in elif_branches {
                    origins.push(self.compute_expr_origin(body));
                }
                if let Some(else_br) = else_branch {
                    origins.push(self.compute_expr_origin(else_br));
                }
                Self::merge_origins(origins)
            }

            Expr::Match { arms, else_arm, .. } => {
                let mut origins: Vec<_> = arms.iter()
                    .map(|arm| self.compute_expr_origin(&arm.body))
                    .collect();
                if let Some(else_arm) = else_arm {
                    origins.push(self.compute_expr_origin(else_arm));
                }
                Self::merge_origins(origins)
            }

            Expr::Block(block) | Expr::Do { body: block } => {
                // Origin comes from the last expression statement
                if let Some(last) = block.stmts.last() {
                    if let Stmt::Expr(e) = &last.node {
                        return self.compute_expr_origin(e);
                    }
                }
                BorrowOrigin::Unknown
            }

            // Struct literal: origin is the union of all reference-type field args
            Expr::StructLiteral { name, args, .. } => {
                if let Some(def_id) = self.resolution_map.get(&name.span.start).copied()
                    .or_else(|| self.scopes.lookup(&name.node))
                {
                    if let Some(ref_flags) = self.struct_field_ref_flags.get(&def_id) {
                        let origins: Vec<BorrowOrigin> = args.iter()
                            .zip(ref_flags.iter())
                            .filter(|(_, is_ref)| **is_ref)
                            .map(|(arg, _)| self.compute_expr_origin(arg))
                            .collect();
                        return Self::merge_origins(origins);
                    }
                }
                BorrowOrigin::Unknown
            }

            // Closure: origin is the union of captured ref-type variables' origins
            Expr::Closure { params, body, .. } => {
                let param_names: FxHashSet<&str> = params.iter()
                    .map(|p| p.node.name.node.as_str()).collect();
                let captured_origins = self.collect_captured_ref_origins(body, &param_names);
                Self::merge_origins(captured_origins)
            }

            // Transparent wrappers: propagate inner origin
            Expr::Move { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::Try { expr: inner }
            | Expr::TryCapture { expr: inner }
            | Expr::As { expr: inner, .. } => {
                self.compute_expr_origin(inner)
            }

            // NilCoalescing: either branch could provide the result
            Expr::NilCoalescing { lhs, rhs } => {
                let origins = vec![
                    self.compute_expr_origin(lhs),
                    self.compute_expr_origin(rhs),
                ];
                Self::merge_origins(origins)
            }

            // Collection literals: propagate element origins
            Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
                let origins: Vec<_> = elems.iter()
                    .map(|e| self.compute_expr_origin(e))
                    .collect();
                Self::merge_origins(origins)
            }

            Expr::DictLiteral(pairs) => {
                let origins: Vec<_> = pairs.iter()
                    .flat_map(|(k, v)| [self.compute_expr_origin(k), self.compute_expr_origin(v)])
                    .collect();
                Self::merge_origins(origins)
            }

            // Self in equip methods: param 0
            Expr::SelfExpr => {
                if let Some(&(def_id, idx)) = self.current_param_def_ids.first() {
                    BorrowOrigin::Param { param_index: idx, def_id }
                } else {
                    BorrowOrigin::Unknown
                }
            }

            // Value-type literals: always produce new owned values, never references
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::CharLiteral(_) => BorrowOrigin::Static,

            // Binary/unary ops produce new values (arithmetic, comparison, logical)
            Expr::BinaryOp { .. } | Expr::UnaryOp { .. } => BorrowOrigin::Static,

            // Range produces a new value
            Expr::Range { .. } => BorrowOrigin::Static,

            // `is` check produces a bool
            Expr::Is { .. } => BorrowOrigin::Static,

            // Comprehensions produce new collections
            Expr::ListComprehension { .. }
            | Expr::DictComprehension { .. }
            | Expr::SetComprehension { .. } => BorrowOrigin::Static,

            // Optional chain propagates from object
            Expr::OptionalChain { object, .. } => self.compute_expr_origin(object),

            // Mutable borrow propagates from inner
            Expr::MutableBorrow { expr: inner } => self.compute_expr_origin(inner),

            // Await/spawn propagate from inner expression
            Expr::Await { expr: inner } | Expr::Spawn { expr: inner } => {
                self.compute_expr_origin(inner)
            }

            // Path expressions (e.g., Module.name): resolve like identifiers
            Expr::Path { .. } => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    if let Some(origin) = self.var_origins.get(&def_id) {
                        return origin.clone();
                    }
                    let def = self.scopes.get_def(def_id);
                    if def.kind == DefKind::Variable {
                        return BorrowOrigin::Local(def_id);
                    }
                }
                // Qualified path to a constant/static — treat as static
                BorrowOrigin::Static
            }

            // Everything else: unknown (conservative)
            _ => BorrowOrigin::Unknown,
        }
    }

    /// Compute the origin of a function call result using callee's `return_borrows_from`.
    /// Struct constructor calls are handled by `compute_expr_origin()` for `StructLiteral`
    /// (struct calls are rewritten to StructLiteral by the post-resolution rewrite pass).
    fn compute_call_origin(&self, callee: &Spanned<Expr>, args: &[Spanned<CallArg>]) -> BorrowOrigin {
        let callee_def_id = self.resolve_callee_def_id(callee);
        if let Some(def_id) = callee_def_id {
            if let Some(info) = self.function_info.get(&def_id) {
                if !info.return_borrows_from.is_empty() {
                    let origins: Vec<BorrowOrigin> = info.return_borrows_from.iter()
                        .filter_map(|&idx| args.get(idx).map(|a| self.compute_expr_origin(&a.node.value)))
                        .collect();
                    return if origins.len() == 1 {
                        origins.into_iter().next().unwrap()
                    } else {
                        BorrowOrigin::CallResult(origins)
                    };
                }
                if info.return_origin_is_static {
                    return BorrowOrigin::Static;
                }
                // If callee returns an owned (non-reference, non-callable) type,
                // the call produces fresh data — always Static.
                if let Some(ret_tid) = info.return_type_id {
                    if !types::is_reference_type(ret_tid, self.types, &self.ref_type_structs)
                        && !types::is_callable_type(ret_tid, self.types)
                    {
                        return BorrowOrigin::Static;
                    }
                }
            }

            // Callable variable calls: `h(req)` where `h: Callable[T(...)]`.
            // We can't inspect h's body, so propagate conservatively from h's own
            // captured origin (analogous to method receiver) plus all arg origins.
            let def = self.scopes.get_def(def_id);
            if def.kind == DefKind::Variable {
                let callable_origin = self.compute_expr_origin(callee);
                let mut origins = vec![callable_origin];
                origins.extend(args.iter().map(|a| self.compute_expr_origin(&a.node.value)));
                return Self::merge_origins(origins);
            }

            // Enum variant constructors: `Ok(x)`, `Error("msg")`, `Some(y)`, etc.
            // The variant transparently wraps its arguments; propagate their origins.
            if def.kind == DefKind::Variant {
                let origins: Vec<BorrowOrigin> = args.iter()
                    .map(|a| self.compute_expr_origin(&a.node.value))
                    .collect();
                return Self::merge_origins(origins);
            }
        }
        BorrowOrigin::Unknown
    }

    /// Compute the origin of a method call result using `return_borrows_from` data.
    fn compute_method_call_origin(&self, receiver: &Spanned<Expr>, method: &Spanned<String>, args: &[Spanned<CallArg>]) -> BorrowOrigin {
        let method_def_id_opt = self.method_resolutions.get(&method.span.start);
        if let Some(&def_id) = method_def_id_opt {
            let info_opt = self.function_info.get(&def_id);
            if let Some(info) = info_opt {
                if !info.return_borrows_from.is_empty() {
                    // Methods have self as param 0: index 0 = receiver, N>0 = args[N-1]
                    let origins: Vec<BorrowOrigin> = info.return_borrows_from.iter()
                        .filter_map(|&idx| {
                            if idx == 0 {
                                Some(self.compute_expr_origin(receiver))
                            } else {
                                args.get(idx - 1).map(|a| self.compute_expr_origin(&a.node.value))
                            }
                        })
                        .collect();
                    return Self::merge_origins(origins);
                }
                if info.return_origin_is_static {
                    return BorrowOrigin::Static;
                }
                // If callee returns an owned (non-reference, non-callable) type,
                // the call produces fresh data — always Static.
                if let Some(ret_tid) = info.return_type_id {
                    if !types::is_reference_type(ret_tid, self.types, &self.ref_type_structs)
                        && !types::is_callable_type(ret_tid, self.types)
                    {
                        return BorrowOrigin::Static;
                    }
                }
            }
        }
        // If the receiver is a struct/enum/newtype TYPE NAME (not a value instance),
        // any unresolved static factory method constructs a fresh value — always Static.
        // This covers patterns like `HttpServerResponse.not_found()` where the struct
        // definition's type_id is None so method_resolutions was never populated.
        if let Expr::Identifier(_) = &receiver.node {
            if let Some(def_id) = self.resolve_callee_def_id(receiver) {
                let def = self.scopes.get_def(def_id);
                if matches!(def.kind, DefKind::Struct | DefKind::Enum | DefKind::Newtype) {
                    return BorrowOrigin::Static;
                }
            }
        }
        // Fallback: conservatively propagate receiver origin
        self.compute_expr_origin(receiver)
    }

    /// Record outlives constraints for a call to a function with `where a outlives b` bounds.
    /// Maps group names to actual argument origins via `param_live_groups`.
    fn record_call_outlives(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<CallArg>],
        call_span: Span,
    ) {
        let callee_def_id = match self.resolve_callee_def_id(callee) {
            Some(id) => id,
            None => return,
        };
        let info = match self.function_info.get(&callee_def_id) {
            Some(i) => i,
            None => return,
        };
        if info.outlives_bounds.is_empty() {
            return;
        }

        // Build group name → argument origin mapping
        let mut group_origins: FxHashMap<String, Vec<DefId>> = FxHashMap::default();
        for (i, group) in info.param_live_groups.iter().enumerate() {
            if let Some(group_name) = group {
                if let Some(arg) = args.get(i) {
                    let origin = self.compute_expr_origin(&arg.node.value);
                    group_origins
                        .entry(group_name.clone())
                        .or_default()
                        .extend(origin.source_def_ids());
                }
            }
        }

        // Create ActiveOutlives entries for each bound
        for (longer, shorter) in &info.outlives_bounds {
            let longer_ids = group_origins.get(longer).cloned().unwrap_or_default();
            let shorter_ids = group_origins.get(shorter).cloned().unwrap_or_default();
            if !longer_ids.is_empty() || !shorter_ids.is_empty() {
                self.active_outlives.push(ActiveOutlives {
                    longer_group: longer.clone(),
                    shorter_group: shorter.clone(),
                    longer_source_def_ids: longer_ids,
                    shorter_source_def_ids: shorter_ids,
                    _call_span: call_span,
                });
            }
        }
    }

    /// Check outlives constraints when a variable is moved.
    /// For `where a outlives b`: if the moved DefId is a source for group `a` ("longer"),
    /// and any source for group `b` ("shorter") is not yet invalidated, that's a violation.
    fn check_outlives_on_move(&mut self, moved_def_id: DefId, span: Span) {
        let mut violations = Vec::new();

        for constraint in &self.active_outlives {
            // Check if the moved variable is a source for the "longer" group
            if constraint.longer_source_def_ids.contains(&moved_def_id) {
                // Check if any "shorter" group source is still alive
                for &shorter_id in &constraint.shorter_source_def_ids {
                    if !self.invalidated_origins.contains(&shorter_id) {
                        let longer_name = self.scopes.get_def(moved_def_id).name.clone();
                        let shorter_name = self.scopes.get_def(shorter_id).name.clone();
                        violations.push((
                            constraint.longer_group.clone(),
                            constraint.shorter_group.clone(),
                            longer_name,
                            shorter_name,
                            span,
                        ));
                    }
                }
            }
        }

        for (longer_group, shorter_group, longer_source, shorter_source, span) in violations {
            self.error(
                SemanticErrorKind::OutlivesViolation {
                    longer_group,
                    shorter_group,
                    longer_source,
                    shorter_source,
                },
                span,
            );
        }
    }

    /// Move a variable: mark as Moved. Error if already moved or inside a loop.
    fn check_move(&mut self, def_id: DefId, span: Span) {
        let name = self.scopes.get_def(def_id).name.clone();

        // Check if already moved
        if let Some(VarState::Moved { moved_at }) = self.var_states.get(&def_id) {
            self.error(
                SemanticErrorKind::DoubleMove {
                    name: name.clone(),
                    first_move: *moved_at,
                },
                span,
            );
            return;
        }

        // Check if inside a loop — but allow moves of variables declared within
        // the innermost loop body (they are re-created each iteration).
        if self.loop_depth > 0 {
            let is_loop_local = self.loop_local_defs.last()
                .map_or(false, |set| set.contains(&def_id));
            if !is_loop_local {
                self.error(SemanticErrorKind::MoveInLoop { name }, span);
                return;
            }
        }

        self.var_states.insert(def_id, VarState::Moved { moved_at: span });
        // Invalidate this def for lifetime tracking — any reference-typed variable
        // whose origin chain includes this DefId becomes dangling.
        self.invalidated_origins.insert(def_id);

        // Phase 5: Check outlives constraints — if we're moving a "longer" group's source,
        // check that all "shorter" group sources are already invalidated.
        self.check_outlives_on_move(def_id, span);
    }


    // ─── Branch State (combined VarState + Origin) ───────

    /// Save combined branch state (var states + origin tracking).
    fn save_branch_state(&self) -> BranchState {
        BranchState {
            var_states: self.var_states.clone(),
            origins: self.var_origins.clone(),
            invalidated: self.invalidated_origins.clone(),
            reassignment_invalidated: self.reassignment_invalidated.clone(),
            await_invalidated: self.await_invalidated.clone(),
            diverges: self.diverged,
        }
    }

    /// Restore combined branch state.
    fn restore_branch_state(&mut self, state: &BranchState) {
        self.var_states = state.var_states.clone();
        self.var_origins = state.origins.clone();
        self.invalidated_origins = state.invalidated.clone();
        self.reassignment_invalidated = state.reassignment_invalidated.clone();
        self.await_invalidated = state.await_invalidated.clone();
        self.diverged = state.diverges;
    }

    /// Merge multiple branch states: union var states (moved in either = moved),
    /// union origins, union invalidated sets.
    /// Branches that diverge (return/break/continue/throw) are excluded from
    /// the merge because their state never reaches the join point. If ALL
    /// branches diverge, the merged state is marked diverged.
    fn merge_branch_states(&mut self, states: &[BranchState]) {
        if states.is_empty() {
            return;
        }

        // Filter to branches that actually reach the join point.
        let live: Vec<&BranchState> = states.iter().filter(|s| !s.diverges).collect();

        if live.is_empty() {
            // Every branch diverges — keep first state, mark diverged.
            self.var_states = states[0].var_states.clone();
            self.var_origins = states[0].origins.clone();
            self.invalidated_origins = states[0].invalidated.clone();
            self.reassignment_invalidated = states[0].reassignment_invalidated.clone();
            self.await_invalidated = states[0].await_invalidated.clone();
            self.diverged = true;
            return;
        }

        let mut merged_vars = live[0].var_states.clone();
        let mut merged_origins = live[0].origins.clone();
        let mut merged_invalidated = live[0].invalidated.clone();
        let mut merged_reassignment_invalidated = live[0].reassignment_invalidated.clone();
        let mut merged_await_invalidated = live[0].await_invalidated.clone();

        for state in &live[1..] {
            // Merge var states: moved in either = moved
            for (def_id, b_state) in &state.var_states {
                match (merged_vars.get(def_id), b_state) {
                    (Some(VarState::Moved { .. }), _) => {}
                    (_, VarState::Moved { moved_at }) => {
                        merged_vars.insert(*def_id, VarState::Moved { moved_at: *moved_at });
                    }
                    _ => {}
                }
            }
            // Merge origins: union (keep both)
            for (def_id, origin) in &state.origins {
                merged_origins.entry(*def_id).or_insert_with(|| origin.clone());
            }
            // Merge invalidated: union (conservative)
            merged_invalidated.extend(&state.invalidated);
            // Merge reassignment_invalidated: union (conservative)
            for (def_id, info) in &state.reassignment_invalidated {
                merged_reassignment_invalidated.entry(*def_id).or_insert_with(|| info.clone());
            }
            // Merge await_invalidated: union (conservative — if any branch has await, use after merge is suspect)
            merged_await_invalidated.extend(&state.await_invalidated);
        }

        self.var_states = merged_vars;
        self.var_origins = merged_origins;
        self.invalidated_origins = merged_invalidated;
        self.reassignment_invalidated = merged_reassignment_invalidated;
        self.await_invalidated = merged_await_invalidated;
        self.diverged = false;
    }

    /// Check if a DefId refers to a ConsumeCallable-typed variable.
    fn is_consume_callable_var(&self, def_id: DefId) -> bool {
        let def = self.scopes.get_def(def_id);
        if def.kind != DefKind::Variable {
            return false;
        }
        if let Some(type_id) = def.type_id {
            match self.types.get(type_id) {
                ResolvedType::ConsumeCallableTrait(_) => true,
                ResolvedType::BoxedCallable { kind, .. }
                    if *kind == super::types::ClosureKind::ConsumeCallable =>
                {
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    }

    // ─── Expression Walking ────────────────────────────────

    fn check_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            // Literals — no ownership concerns
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::CharLiteral(_)
            | Expr::StringLiteral(_)
            | Expr::NoneLiteral
            | Expr::SelfExpr
            | Expr::It => {}

            Expr::Identifier(_) => {
                // Check that the variable is still live
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let kind = self.scopes.get_def(def_id).kind;
                    if kind == DefKind::Variable {
                        self.check_use(def_id, expr.span);
                    }
                }
            }

            Expr::Path { segments } => {
                if let Some(first) = segments.first() {
                    if let Some(&def_id) = self.resolution_map.get(&first.span.start) {
                        let kind = self.scopes.get_def(def_id).kind;
                        if kind == DefKind::Variable {
                            self.check_use(def_id, first.span);
                        }
                    }
                }
            }

            Expr::Move { expr: inner } => {
                // The `!` operator: move the value
                if let Expr::Identifier(_) = &inner.node {
                    if let Some(&def_id) = self.resolution_map.get(&inner.span.start) {
                        let kind = self.scopes.get_def(def_id).kind;
                        if kind == DefKind::Variable {
                            self.check_move(def_id, expr.span);
                        }
                    }
                } else {
                    // Move of a complex expression — just recurse
                    self.check_expr(inner);
                }
            }

            Expr::MutableBorrow { expr: inner } => {
                // The `&` operator: mutable borrow
                // Check that the inner expression is still usable
                self.check_expr(inner);
            }

            Expr::Deref { expr: inner } => {
                self.check_expr(inner);
            }

            Expr::UnaryOp { operand, .. } => {
                self.check_expr(operand);
            }

            Expr::BinaryOp { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }

            Expr::Call { callee, args, .. } => {
                // If callee is a ConsumeCallable variable, the call consumes it
                let consumed = if let Expr::Identifier(_) = &callee.node {
                    if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                        if self.is_consume_callable_var(def_id) {
                            self.check_move(def_id, expr.span);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if !consumed {
                    self.check_expr(callee);
                }
                self.check_call_ownership(callee, args);
                self.check_call_aliasing(args);

                // Determine if callee is a constructor (variant/newtype) —
                // their args implicitly consume non-Copy values.
                let is_constructor = self.resolve_callee_def_id(callee)
                    .map(|id| {
                        let kind = self.scopes.get_def(id).kind;
                        matches!(kind, DefKind::Variant | DefKind::Newtype)
                    })
                    .unwrap_or(false);

                for arg in args {
                    match arg.node.ownership {
                        Ownership::Move => {
                            // Argument passed with `!` — check the move
                            if let Expr::Identifier(_) = &arg.node.value.node {
                                if let Some(&def_id) =
                                    self.resolution_map.get(&arg.node.value.span.start)
                                {
                                    let kind = self.scopes.get_def(def_id).kind;
                                    if kind == DefKind::Variable {
                                        self.check_move(def_id, arg.span);
                                    }
                                }
                            } else {
                                self.check_expr(&arg.node.value);
                            }
                        }
                        Ownership::MutableBorrow | Ownership::Borrow => {
                            // For constructor calls, bare non-Copy identifier
                            // args are implicitly consumed (moved into fields).
                            if is_constructor {
                                if let Expr::Identifier(_) = &arg.node.value.node {
                                    if let Some(&var_def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                                        let def = self.scopes.get_def(var_def_id);
                                        if def.kind == DefKind::Variable && !def.is_param {
                                            if let Some(type_id) = def.type_id {
                                                if !is_copy_type(type_id, self.types, self.scopes) {
                                                    // Skip implicit move check only when
                                                    // we are in a return expression — return
                                                    // exits the function so the move happens
                                                    // at most once even inside a loop.
                                                    let skip_implicit_move = self.loop_depth > 0
                                                        && !self.loop_local_defs.last()
                                                            .map_or(false, |s| s.contains(&var_def_id))
                                                        && self.in_return_expr;
                                                    if !skip_implicit_move {
                                                        self.check_move(var_def_id, arg.span);
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            self.check_expr(&arg.node.value);
                        }
                    }
                }

                // Phase 5: Record outlives constraints from this call site
                self.record_call_outlives(callee, args, expr.span);
            }

            Expr::MethodCall {
                receiver, args, ..
            } => {
                self.check_expr(receiver);
                self.check_call_aliasing(args);
                // Detect qualified enum variant constructors: EnumName.VariantName(args)
                // When the receiver resolves to an Enum def, treat args as implicitly consumed.
                let is_enum_constructor = if let Expr::Identifier(_) = &receiver.node {
                    self.resolution_map.get(&receiver.span.start)
                        .map(|&def_id| self.scopes.get_def(def_id).kind == DefKind::Enum)
                        .unwrap_or(false)
                } else {
                    false
                };
                for arg in args {
                    match arg.node.ownership {
                        Ownership::Move => {
                            if let Expr::Identifier(_) = &arg.node.value.node {
                                if let Some(&def_id) =
                                    self.resolution_map.get(&arg.node.value.span.start)
                                {
                                    let kind = self.scopes.get_def(def_id).kind;
                                    if kind == DefKind::Variable {
                                        self.check_move(def_id, arg.span);
                                    }
                                }
                            } else {
                                self.check_expr(&arg.node.value);
                            }
                        }
                        Ownership::MutableBorrow | Ownership::Borrow => {
                            // For qualified enum variant constructors, bare non-Copy
                            // identifier args are implicitly consumed (moved into fields).
                            if is_enum_constructor {
                                if let Expr::Identifier(_) = &arg.node.value.node {
                                    if let Some(&var_def_id) = self.resolution_map.get(&arg.node.value.span.start) {
                                        let def = self.scopes.get_def(var_def_id);
                                        if def.kind == DefKind::Variable && !def.is_param {
                                            if let Some(type_id) = def.type_id {
                                                if !is_copy_type(type_id, self.types, self.scopes) {
                                                    let skip_implicit_move = self.loop_depth > 0
                                                        && !self.loop_local_defs.last()
                                                            .map_or(false, |s| s.contains(&var_def_id))
                                                        && self.in_return_expr;
                                                    if !skip_implicit_move {
                                                        self.check_move(var_def_id, arg.span);
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            self.check_expr(&arg.node.value);
                        }
                    }
                }
            }

            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.check_expr(object);
            }

            Expr::Index { object, index } => {
                self.check_expr(object);
                self.check_expr(index);
            }

            Expr::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.check_expr(s);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
            }

            Expr::OptionalChain { object, .. } => {
                self.check_expr(object);
            }

            Expr::NilCoalescing { lhs, rhs } => {
                self.check_expr(lhs);
                self.check_expr(rhs);
            }

            Expr::Await { expr: inner } => {
                self.check_expr(inner);
                if self.current_function_is_async {
                    let to_invalidate: Vec<DefId> = self.var_origins.iter()
                        .filter(|(_, origin)| !matches!(origin, BorrowOrigin::Static))
                        .filter_map(|(def_id, _)| {
                            // Not in var_states = never moved = implicitly live (e.g. params).
                            if !matches!(self.var_states.get(def_id), Some(VarState::Moved { .. })) {
                                Some(*def_id)
                            } else {
                                None
                            }
                        })
                        .collect();
                    for def_id in to_invalidate {
                        self.await_invalidated.insert(def_id);
                    }
                }
            }

            Expr::Try { expr: inner }
            | Expr::Spawn { expr: inner }
            | Expr::TryCapture { expr: inner } => {
                self.check_expr(inner);
            }

            Expr::If {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                self.check_expr(condition);

                let before = self.save_branch_state();
                self.check_expr(then_branch);
                let mut branch_states = vec![self.save_branch_state()];

                for (cond, body) in elif_branches {
                    self.restore_branch_state(&before);
                    self.check_expr(cond);
                    self.check_expr(body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_br) = else_branch {
                    self.restore_branch_state(&before);
                    self.check_expr(else_br);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                let scrutinee_origin = self.compute_expr_origin(scrutinee);
                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_branch_state(&before);
                    self.mark_pattern_origins(&arm.pattern, &scrutinee_origin);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_branch_state(&before);
                    self.check_expr(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Expr::Block(block) => {
                self.check_block(block);
            }

            Expr::Do { body } => {
                self.check_block(body);
            }

            Expr::Closure { body, .. } => {
                // A closure definition must not leak state into the enclosing
                // scope.  Moves/borrows inside the body execute when the
                // closure is *called*, not when it is *defined*.  Snapshot the
                // enclosing state, check the body in isolation, then restore.
                let saved = self.save_branch_state();
                let saved_loop_depth = self.loop_depth;
                let saved_loop_locals = std::mem::take(&mut self.loop_local_defs);
                let saved_in_return = self.in_return_expr;
                let saved_is_async = self.current_function_is_async;
                let saved_arena_depth = self.arena_depth;
                self.loop_depth = 0;
                self.arena_depth = 0;
                self.in_return_expr = false;
                self.current_function_is_async = false; // closures are not async (async closures deferred)
                self.check_expr(body);
                self.restore_branch_state(&saved);
                self.loop_depth = saved_loop_depth;
                self.loop_local_defs = saved_loop_locals;
                self.in_return_expr = saved_in_return;
                self.current_function_is_async = saved_is_async;
                self.arena_depth = saved_arena_depth;
            }

            Expr::ImplicitClosure { body } => {
                let saved = self.save_branch_state();
                let saved_loop_depth = self.loop_depth;
                let saved_loop_locals = std::mem::take(&mut self.loop_local_defs);
                let saved_in_return = self.in_return_expr;
                let saved_is_async = self.current_function_is_async;
                let saved_arena_depth = self.arena_depth;
                self.loop_depth = 0;
                self.arena_depth = 0;
                self.in_return_expr = false;
                self.current_function_is_async = false;
                self.check_expr(body);
                self.restore_branch_state(&saved);
                self.loop_depth = saved_loop_depth;
                self.loop_local_defs = saved_loop_locals;
                self.in_return_expr = saved_in_return;
                self.current_function_is_async = saved_is_async;
                self.arena_depth = saved_arena_depth;
            }

            Expr::ListComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::DictComprehension {
                key,
                value,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(key);
                self.check_expr(value);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::SetComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
            }

            Expr::ArrayLiteral(elements) | Expr::TupleLiteral(elements) => {
                for elem in elements {
                    self.check_expr(elem);
                }
            }

            Expr::DictLiteral(pairs) => {
                for (k, v) in pairs {
                    self.check_expr(k);
                    self.check_expr(v);
                }
            }

            Expr::StructLiteral { args, .. } => {
                // Struct fields own their data — non-Copy identifier args
                // are implicitly consumed (moved into the struct).
                for arg in args {
                    if let Expr::Identifier(_) = &arg.node {
                        if let Some(&var_def_id) = self.resolution_map.get(&arg.span.start) {
                            let def = self.scopes.get_def(var_def_id);
                            if def.kind == DefKind::Variable && !def.is_param {
                                if let Some(type_id) = def.type_id {
                                    if !is_copy_type(type_id, self.types, self.scopes) {
                                        let skip_implicit_move = self.loop_depth > 0
                                            && !self.loop_local_defs.last()
                                                .map_or(false, |s| s.contains(&var_def_id))
                                            && self.in_return_expr;
                                        if !skip_implicit_move {
                                            self.check_move(var_def_id, arg.span);
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    self.check_expr(arg);
                }
            }

            Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
                self.check_expr(inner);
            }

            Expr::DotShorthand { args, .. } => {
                for arg in args {
                    self.check_expr(&arg.node.value);
                }
            }
        }
    }

    // ─── Statement Walking ─────────────────────────────────

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                pattern, value, type_, ..
            } => {
                // Check the value expression
                self.check_expr(value);

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                self.check_value_needs_move(value);

                // Mark new bindings as Live
                self.mark_pattern_live_spanned(pattern);

                // Track non-Copy variables declared inside arena scope
                if self.arena_depth > 0 {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span)
                            .or_else(|| self.find_def_by_name(name))
                        {
                            if let Some(type_id) = self.scopes.get_def(def_id).type_id {
                                if !is_copy_type(type_id, self.types, self.scopes) {
                                    self.arena_scoped_vars.insert(def_id);
                                }
                            }
                        }
                    }
                }

                // Track `alloc=` arena propagation: if the value is a Call with
                // alloc=<arena_scoped_var>, mark the new variable as arena-scoped.
                if let Pattern::Binding(name) = &pattern.node {
                    if let Expr::Call { args, .. } = &value.node {
                        for arg in args {
                            if arg.node.name.as_ref().map_or(false, |n| n.node == "alloc") {
                                if let Expr::Identifier(alloc_name) = &arg.node.value.node {
                                    let alloc_def = self.scopes.lookup_def_by_span(alloc_name, arg.node.value.span)
                                        .or_else(|| self.find_def_by_name(alloc_name));
                                    if let Some(alloc_did) = alloc_def {
                                        if self.arena_scoped_vars.contains(&alloc_did)
                                            || (self.arena_depth > 0 && self.is_allocator_type(alloc_did))
                                        {
                                            if let Some(var_did) = self.scopes.lookup_def_by_span(name, pattern.span)
                                                .or_else(|| self.find_def_by_name(name))
                                            {
                                                self.arena_scoped_vars.insert(var_did);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Track origin for reference-typed variables
                if let Pattern::Binding(name) = &pattern.node {
                    let def_id_opt = self.scopes.lookup_def_by_span(name, pattern.span)
                        .or_else(|| self.find_def_by_name(name));
                    if let Some(def_id) = def_id_opt {
                        let is_ref = self.is_var_reference_type(def_id, Some(type_));
                        if is_ref {
                            let origin = self.compute_expr_origin(value);
                            self.var_origins.insert(def_id, origin);

                            // Phase 6: Detect binding a ref type to a temporary
                            if self.is_temporary_borrow(value) {
                                let callee_name = match &value.node {
                                    Expr::Call { callee, .. } => Self::extract_callee_name(callee),
                                    Expr::MethodCall { receiver, method, .. } => {
                                        format!("{}.{}", Self::extract_receiver_name(receiver), method.node)
                                    }
                                    _ => "<unknown>".to_string(),
                                };
                                self.error(
                                    SemanticErrorKind::TemporaryBorrow {
                                        name: name.clone(),
                                        callee: callee_name,
                                        temp_at: None,
                                    },
                                    value.span,
                                );
                            }
                        }

                        // Phase 9+10: Track origin for callable-typed variables.
                        // Covers both direct closures and callable values from function calls.
                        if !self.var_origins.contains_key(&def_id) {
                            let is_callable = self.scopes.get_def(def_id).type_id
                                .map_or(false, |tid| types::is_callable_type(tid, self.types));
                            // Also check expression form for `auto` vars where type_id may not be set
                            if is_callable || matches!(&value.node, Expr::Closure { .. }) {
                                let origin = self.compute_expr_origin(value);
                                self.var_origins.insert(def_id, origin);
                            }
                        }
                    }
                }
            }

            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }

            Stmt::Assign { target, value } => {
                self.check_expr(value);

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                self.check_value_needs_move(value);

                // Arena escape check: cannot assign arena-scoped value to outer variable
                if self.arena_depth > 0 {
                    if let Expr::Identifier(target_name) = &target.node {
                        if let Some(&target_def_id) = self.resolution_map.get(&target.span.start) {
                            let rhs_def_id = match &value.node {
                                Expr::Identifier(_) => self.resolution_map.get(&value.span.start).copied(),
                                Expr::Move { expr: inner } => {
                                    if let Expr::Identifier(_) = &inner.node {
                                        self.resolution_map.get(&inner.span.start).copied()
                                    } else { None }
                                }
                                _ => None,
                            };
                            if let Some(rhs_id) = rhs_def_id {
                                if self.arena_scoped_vars.contains(&rhs_id)
                                    && !self.arena_scoped_vars.contains(&target_def_id)
                                {
                                    self.error(
                                        SemanticErrorKind::ArenaEscape {
                                            name: self.scopes.get_def(rhs_id).name.clone(),
                                            kind: ArenaEscapeKind::AssignOuter {
                                                target: target_name.clone(),
                                            },
                                        },
                                        value.span,
                                    );
                                }
                            }
                        }
                    }
                }

                // Check immutability/const constraints on identifier targets
                match &target.node {
                    Expr::Identifier(_) => {
                        if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                            let def = self.scopes.get_def(def_id);
                            if def.kind == DefKind::Const {
                                self.error(
                                    SemanticErrorKind::AssignmentToConst { name: def.name.clone() },
                                    target.span,
                                );
                            } else if self.immutable_by_default
                                && !def.is_mutable
                                && def.kind == DefKind::Variable
                            {
                                self.error(
                                    SemanticErrorKind::AssignmentToImmutable { name: def.name.clone() },
                                    target.span,
                                );
                            }
                            // Reassignment revives a moved variable
                            self.mark_live(def_id);
                            // Also un-invalidate: if this variable was moved and
                            // reassigned, it's no longer a dangling source.
                            self.invalidated_origins.remove(&def_id);

                            // Phase 11: If the dependent variable itself is reassigned,
                            // clear its stale-borrow entry — it now has a fresh value.
                            self.reassignment_invalidated.remove(&def_id);

                            // Async: reassignment after await clears suspension-point invalidation.
                            self.await_invalidated.remove(&def_id);

                            // Phase 11: Reassignment invalidation.
                            // When a non-Copy owning variable is reassigned, all existing
                            // references borrowing from the old value become dangling.
                            let is_copy = self.scopes.get_def(def_id).type_id
                                .map_or(false, |tid| is_copy_type(tid, self.types, self.scopes));
                            if !is_copy {
                                let source_name = self.scopes.get_def(def_id).name.clone();
                                let dependents: Vec<DefId> = self.var_origins.iter()
                                    .filter(|(vid, origin)| **vid != def_id && origin.references_def(def_id))
                                    .map(|(vid, _)| *vid)
                                    .collect();
                                for dep_id in dependents {
                                    self.reassignment_invalidated.insert(dep_id, (source_name.clone(), value.span));
                                }
                            }

                            // Update origin tracking for reference-typed variables
                            if self.var_origins.contains_key(&def_id) {
                                let origin = self.compute_expr_origin(value);
                                self.var_origins.insert(def_id, origin);
                            } else {
                                // Phase 10: Insert fresh origin for callable-typed reassignments
                                let is_callable = self.scopes.get_def(def_id).type_id
                                    .map_or(false, |tid| types::is_callable_type(tid, self.types));
                                if is_callable || matches!(&value.node, Expr::Closure { .. }) {
                                    let origin = self.compute_expr_origin(value);
                                    self.var_origins.insert(def_id, origin);
                                }
                            }
                        }
                    }
                    // For field/index assignments, check the base object
                    _ => {
                        self.check_expr(target);
                    }
                }
            }

            Stmt::CompoundAssign { target, value, .. } => {
                // Check immutability/const constraints on identifier targets
                if let Expr::Identifier(_) = &target.node {
                    if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                        let def = self.scopes.get_def(def_id);
                        if def.kind == DefKind::Const {
                            self.error(
                                SemanticErrorKind::AssignmentToConst { name: def.name.clone() },
                                target.span,
                            );
                        } else if self.immutable_by_default
                            && !def.is_mutable
                            && def.kind == DefKind::Variable
                        {
                            self.error(
                                SemanticErrorKind::AssignmentToImmutable { name: def.name.clone() },
                                target.span,
                            );
                        }
                    }
                }
                self.check_expr(target);
                self.check_expr(value);
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let saved_in_return = self.in_return_expr;
                    self.in_return_expr = true;
                    self.check_expr(expr);
                    self.in_return_expr = saved_in_return;

                    // Arena escape check: cannot return arena-scoped values
                    if self.arena_depth > 0 {
                        if let Expr::Identifier(name) = &expr.node {
                            if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                                if self.arena_scoped_vars.contains(&def_id) {
                                    self.error(
                                        SemanticErrorKind::ArenaEscape {
                                            name: name.clone(),
                                            kind: ArenaEscapeKind::Return,
                                        },
                                        expr.span,
                                    );
                                }
                            }
                        }
                    }

                    // Lifetime check: if the function returns a reference or callable type,
                    // verify the return expression doesn't reference local data.
                    if let Some(ret_type_id) = self.current_return_type_id {
                        if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
                            || types::is_callable_type(ret_type_id, self.types)
                        {
                            let origin = self.compute_expr_origin(expr);
                            if origin.contains_local() {
                                let return_name = match &expr.node {
                                    Expr::Identifier(n) => n.clone(),
                                    _ => "<expression>".to_string(),
                                };
                                if origin.contains_unknown() && !matches!(&origin, BorrowOrigin::Local(_)) {
                                    self.error(
                                        SemanticErrorKind::UnresolvedBorrowOrigin {
                                            name: return_name,
                                        },
                                        expr.span,
                                    );
                                } else {
                                    let local_names = origin.local_names(self.scopes);
                                    let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                                    let local_declared_at = origin.source_def_ids().first()
                                        .map(|&did| self.scopes.get_def(did).span);
                                    self.error(
                                        SemanticErrorKind::DanglingReturn {
                                            name: return_name,
                                            local_name,
                                            local_declared_at,
                                        },
                                        expr.span,
                                    );
                                }
                            }
                        }
                    }
                }
                self.diverged = true;
            }

            Stmt::Throw(expr) => {
                self.check_expr(expr);
                self.diverged = true;
            }

            Stmt::Break(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr);
                }
                self.diverged = true;
            }

            Stmt::Continue => {
                self.diverged = true;
            }

            Stmt::Pass => {}

            Stmt::For {
                iterable,
                body,
                else_body,
                ..
            } => {
                self.check_expr(iterable);
                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
                let after_body = self.save_branch_state();
                if let Some(else_body) = else_body {
                    // for-else: else only runs if body never does (0 iterations)
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    let after_else = self.save_branch_state();
                    self.merge_branch_states(&[after_body, after_else]);
                } else {
                    // Body may execute 0+ times: merge pre-loop with post-body
                    self.merge_branch_states(&[before, after_body]);
                }
            }

            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                self.check_expr(condition);
                // Mark borrow origins for all `is` pattern bindings (including compound conditions)
                self.mark_compound_is_origins(&condition.node);
                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
                let after_body = self.save_branch_state();
                if let Some(else_body) = else_body {
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    let after_else = self.save_branch_state();
                    self.merge_branch_states(&[after_body, after_else]);
                } else {
                    self.merge_branch_states(&[before, after_body]);
                }
            }

            Stmt::Loop { body } => {
                let before = self.save_branch_state();
                let saved_in_return = self.in_return_expr;
                self.in_return_expr = false;
                self.loop_depth += 1;
                self.loop_local_defs.push(FxHashSet::default());
                self.check_block(body);
                self.loop_local_defs.pop();
                self.loop_depth -= 1;
                self.in_return_expr = saved_in_return;
                let after_body = self.save_branch_state();
                // Infinite loop, but break can exit: merge pre+post for break paths
                self.merge_branch_states(&[before, after_body]);
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                self.check_expr(condition);
                // Mark borrow origins for all `is` pattern bindings (including compound conditions)
                self.mark_compound_is_origins(&condition.node);

                let before = self.save_branch_state();
                self.check_block(then_body);
                let mut branch_states = vec![self.save_branch_state()];

                for (cond, body) in elif_branches {
                    self.restore_branch_state(&before);
                    self.check_expr(cond);
                    self.mark_compound_is_origins(&cond.node);
                    self.check_block(body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_body) = else_body {
                    self.restore_branch_state(&before);
                    self.check_block(else_body);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                let scrutinee_origin = self.compute_expr_origin(scrutinee);
                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_branch_state(&before);
                    self.mark_pattern_origins(&arm.pattern, &scrutinee_origin);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_branch_state(&before);
                    self.check_block(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::Select { arms, else_arm } => {
                let before = self.save_branch_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_branch_state(&before);
                    match &arm.op {
                        SelectOp::Recv { channel, .. } => {
                            self.check_expr(channel);
                        }
                        SelectOp::Send { channel, value } => {
                            self.check_expr(channel);
                            self.check_expr(value);
                        }
                    }
                    self.check_block(&arm.body);
                    branch_states.push(self.save_branch_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_branch_state(&before);
                    self.check_block(else_arm);
                    branch_states.push(self.save_branch_state());
                } else {
                    branch_states.push(before);
                }

                self.merge_branch_states(&branch_states);
            }

            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.check_expr(&binding.expr);
                }

                // Detect if any binding is an allocator type (Arena or TrackingAllocator)
                let is_arena_with = bindings.iter().any(|b| {
                    self.scopes.lookup_def_by_span(&b.name.node, b.name.span)
                        .map_or(false, |did| self.is_allocator_type(did))
                });

                if is_arena_with {
                    self.arena_depth += 1;
                }
                self.check_block(body);
                if is_arena_with {
                    self.arena_depth -= 1;
                }
            }

            Stmt::Unsafe { body } => {
                self.check_block(body);
            }

            Stmt::Assert { condition, message } => {
                self.check_expr(condition);
                if let Some(msg) = message {
                    self.check_expr(msg);
                }
            }

            Stmt::Item(_) => {}

            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                // Conditions are meta expressions not evaluated by borrow checker.
                // Walk bodies conservatively (all branches are potentially live).
                self.check_block(then_body);
                for (_, body) in elif_branches {
                    self.check_block(body);
                }
                if let Some(eb) = else_body {
                    self.check_block(eb);
                }
            }

            Stmt::MetaFor { body, .. } => {
                // Range is a meta expression: skip; just check the body.
                self.check_block(body);
            }
        }
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    /// Check if a value expression is a bare identifier of a non-Copy type (needs `!`).
    fn check_value_needs_move(&mut self, value: &Spanned<Expr>) {
        if let Expr::Identifier(_) = &value.node {
            if let Some(&def_id) = self.resolution_map.get(&value.span.start) {
                let def = self.scopes.get_def(def_id);
                // Only check local variables, not functions/types/imports.
                // Skip parameters — they're borrowed from the caller, so
                // re-binding them is just copying a pointer, not transferring ownership.
                if def.kind == DefKind::Variable && !def.is_param {
                    if let Some(type_id) = def.type_id {
                        if !is_copy_type(type_id, self.types, self.scopes) {
                            self.error(
                                SemanticErrorKind::MoveWithoutOperator {
                                    name: def.name.clone(),
                                },
                                value.span,
                            );
                        }
                    }
                }
            }
        }
    }

    /// Walk compound `And` chains in conditions, marking borrow origins for each
    /// `is` sub-expression's pattern bindings.
    fn mark_compound_is_origins(&mut self, expr: &Expr) {
        match expr {
            Expr::Is { expr: scrutinee, negated: false, pattern, .. } => {
                let origin = self.compute_expr_origin(scrutinee);
                self.mark_pattern_origins(pattern, &origin);
            }
            Expr::BinaryOp { left, op: BinaryOp::And, right } => {
                self.mark_compound_is_origins(&left.node);
                self.mark_compound_is_origins(&right.node);
            }
            _ => {}
        }
    }

    /// Walk a pattern from a match arm, mark bindings as Live, and assign
    /// origins derived from the scrutinee to any reference-type bindings.
    /// Uses span-based DefId lookup to avoid name collisions between arms.
    fn mark_pattern_origins(&mut self, pattern: &Spanned<Pattern>, scrutinee_origin: &BorrowOrigin) {
        match &pattern.node {
            Pattern::DotShorthand { fields, .. } => {
                for field in fields {
                    self.mark_pattern_origins(field, scrutinee_origin);
                }
                return;
            }
            Pattern::Binding(name) => {
                // Use span-based lookup to find the correct DefId for this exact binding.
                // Name-based lookup (find_def_by_name) can return the wrong DefId when
                // multiple match arms bind the same name.
                let def_id = self.scopes.lookup_def_by_span(name, pattern.span)
                    .or_else(|| self.find_def_by_name(name));
                if let Some(def_id) = def_id {
                    self.mark_live(def_id);
                    // Only assign origin to reference-type or callable-type bindings
                    if let Some(type_id) = self.scopes.get_def(def_id).type_id {
                        if types::is_reference_type(type_id, self.types, &self.ref_type_structs)
                            || types::is_callable_type(type_id, self.types)
                        {
                            self.var_origins.insert(def_id, scrutinee_origin.clone());
                        }
                    }
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_origins(field, scrutinee_origin);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_origins(elem, scrutinee_origin);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_origins(first, scrutinee_origin);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Mark all bindings in a pattern as Live.
    fn mark_pattern_live_spanned(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.node {
            Pattern::DotShorthand { fields, .. } => {
                for field in fields {
                    self.mark_pattern_live_spanned(field);
                }
                return;
            }
            Pattern::Binding(name) => {
                // Use span-based lookup to find the exact DefId for this binding,
                // avoiding confusion when multiple variables share the same name
                // in different scopes within the same function.
                if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span) {
                    self.mark_live(def_id);
                } else if let Some(def_id) = self.find_def_by_name(name) {
                    // Fallback for cases where span doesn't match exactly
                    self.mark_live(def_id);
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_live_spanned(field);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_live_spanned(elem);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_live_spanned(first);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Find the DefId for a variable name by looking it up in the scope table.
    fn find_def_by_name(&self, name: &str) -> Option<DefId> {
        if let Some(scope_id) = self.current_fn_scope {
            self.scopes.lookup_within_function(scope_id, name)
        } else {
            self.scopes.lookup(name)
        }
    }

    /// Resolve a Call callee expression to its DefId (if it's a simple identifier).
    fn resolve_callee_def_id(&self, callee: &Spanned<Expr>) -> Option<DefId> {
        match &callee.node {
            Expr::Identifier(_) => self.resolution_map.get(&callee.span.start).copied(),
            Expr::Path { segments } => {
                segments.first().and_then(|s| self.resolution_map.get(&s.span.start).copied())
            }
            _ => None,
        }
    }

    /// Check that call-site ownership annotations match the parameter declarations.
    fn check_call_ownership(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<CallArg>],
    ) {
        let def_id = match self.resolve_callee_def_id(callee) {
            Some(id) => id,
            None => return,
        };

        // Skip constructors (structs, enum variants) — they don't have FunctionInfo
        let kind = self.scopes.get_def(def_id).kind;
        if matches!(kind, DefKind::Struct | DefKind::Variant) {
            return;
        }

        let info = match self.function_info.get(&def_id) {
            Some(info) => info,
            None => return, // builtins, extern, etc.
        };

        for (i, arg) in args.iter().enumerate() {
            if i >= info.param_ownerships.len() {
                break; // varargs or mismatched count (caught by type checker)
            }

            let expected = info.param_ownerships[i];
            let found = arg.node.ownership;

            if expected != found {
                let param_name = info.param_names[i].clone();
                let expected_str = match expected {
                    Ownership::Borrow => "borrow (bare)",
                    Ownership::MutableBorrow => "mutable borrow (& or mutable)",
                    Ownership::Move => "consume (! or consuming)",
                };
                let found_str = match found {
                    Ownership::Borrow => "borrow (bare)",
                    Ownership::MutableBorrow => "mutable borrow (& or mutable)",
                    Ownership::Move => "consume (! or consuming)",
                };
                self.error(
                    SemanticErrorKind::OwnershipMismatch {
                        param_name,
                        expected: expected_str.to_string(),
                        found: found_str.to_string(),
                    },
                    arg.span,
                );
            }
        }
    }

    /// Detect aliasing conflicts within a single call's arguments.
    /// e.g., f(&x, &x) — double mutable borrow
    /// e.g., f(x, &x) — immutable read + mutable borrow
    /// e.g., f(&x, !x) — mutable borrow + move
    fn check_call_aliasing(&mut self, args: &[Spanned<CallArg>]) {
        // Collect (DefId, Ownership, span) for identifier arguments
        let mut arg_vars: Vec<(DefId, Ownership, Span)> = Vec::new();

        for arg in args {
            let (inner_expr, ownership) = match arg.node.ownership {
                Ownership::Move => (&arg.node.value, Ownership::Move),
                Ownership::MutableBorrow => (&arg.node.value, Ownership::MutableBorrow),
                Ownership::Borrow => (&arg.node.value, Ownership::Borrow),
            };

            if let Expr::Identifier(_) = &inner_expr.node {
                if let Some(&def_id) = self.resolution_map.get(&inner_expr.span.start) {
                    let kind = self.scopes.get_def(def_id).kind;
                    if kind == DefKind::Variable {
                        arg_vars.push((def_id, ownership, arg.span));
                    }
                }
            }
        }

        // Check pairs for conflicts
        for i in 0..arg_vars.len() {
            for j in (i + 1)..arg_vars.len() {
                let (id_a, own_a, span_a) = &arg_vars[i];
                let (id_b, own_b, _span_b) = &arg_vars[j];

                if id_a != id_b {
                    continue;
                }

                let name = self.scopes.get_def(*id_a).name.clone();

                let conflict = match (own_a, own_b) {
                    // Double mutable borrow
                    (Ownership::MutableBorrow, Ownership::MutableBorrow) => {
                        Some("cannot borrow mutably more than once in the same call")
                    }
                    // Mutable borrow + move (either order)
                    (Ownership::MutableBorrow, Ownership::Move)
                    | (Ownership::Move, Ownership::MutableBorrow) => {
                        Some("cannot borrow and move the same variable in a call")
                    }
                    // Borrow (bare read) + mutable borrow
                    (Ownership::Borrow, Ownership::MutableBorrow)
                    | (Ownership::MutableBorrow, Ownership::Borrow) => {
                        Some("cannot use bare and mutable borrow of the same variable in a call")
                    }
                    _ => None,
                };

                if let Some(detail) = conflict {
                    self.error(
                        SemanticErrorKind::BorrowConflict {
                            name,
                            detail: detail.to_string(),
                        },
                        *span_a,
                    );
                }
            }
        }
    }

    /// Collect origins of captured reference-type variables in a closure body.
    /// Uses the ExprVisitor trait for exhaustive variant coverage.
    fn collect_captured_ref_origins(
        &self,
        expr: &Spanned<Expr>,
        param_names: &FxHashSet<&str>,
    ) -> Vec<BorrowOrigin> {
        use crate::parser::visitor::ExprVisitor;
        let mut collector = CapturedRefOriginCollector {
            resolution_map: self.resolution_map,
            scopes: self.scopes,
            types: self.types,
            ref_type_structs: &self.ref_type_structs,
            var_origins: &self.var_origins,
            param_names,
            origins: Vec::new(),
        };
        collector.visit_expr(expr);
        collector.origins
    }

    /// Check if a function returns a temporary (non-reference owning type) with
    /// no `return_borrows_from`. Used by both Call and MethodCall detection.
    fn is_temporary_from_function(&self, def_id: DefId) -> bool {
        let Some(info) = self.function_info.get(&def_id) else { return false };
        // Only check user-defined functions with bodies (not declarations/externs)
        if !info.has_body {
            return false;
        }
        let Some(ret_type_id) = info.return_type_id else { return false };
        // Skip generic functions — return type might be a type param that resolves
        // to a reference type at instantiation site
        if !info.generic_param_names.is_empty() {
            return false;
        }
        // Owning return type + no return_borrows_from = temporary
        !types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
            && info.return_borrows_from.is_empty()
    }

    /// Check if a call/method-call expression returns a temporary.
    /// Binding a reference type to such a value is an error
    /// because the temporary will be dropped immediately.
    fn is_temporary_borrow(&self, value: &Spanned<Expr>) -> bool {
        match &value.node {
            Expr::Call { callee, .. } => {
                let Some(def_id) = self.resolve_callee_def_id(callee) else { return false };
                self.is_temporary_from_function(def_id)
            }
            Expr::MethodCall { method, .. } => {
                let Some(&def_id) = self.method_resolutions.get(&method.span.start) else { return false };
                self.is_temporary_from_function(def_id)
            }
            _ => false,
        }
    }

    /// Extract the callee name from a Call expression (for error messages).
    fn extract_callee_name(callee: &Spanned<Expr>) -> String {
        match &callee.node {
            Expr::Identifier(name) => name.clone(),
            Expr::Path { segments } => {
                segments.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join(".")
            }
            _ => "<expression>".to_string(),
        }
    }

    /// Extract a readable name from a method receiver expression (for error messages).
    fn extract_receiver_name(receiver: &Spanned<Expr>) -> String {
        match &receiver.node {
            Expr::Identifier(name) => name.clone(),
            Expr::FieldAccess { object, field } => {
                format!("{}.{}", Self::extract_receiver_name(object), field.node)
            }
            _ => "<expr>".to_string(),
        }
    }

    /// Check if a DefId refers to any allocator type (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, or FallbackAllocator).
    fn is_allocator_type(&self, def_id: DefId) -> bool {
        self.scopes.get_def(def_id).type_id.map_or(false, |tid| {
            matches!(self.types.get(tid), ResolvedType::Defined(d)
                if matches!(self.scopes.get_def(*d).name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator"))
        })
    }

    /// Check if a variable is a reference type, using its type annotation or resolved type.
    fn is_var_reference_type(&self, def_id: DefId, type_annotation: Option<&Spanned<Type>>) -> bool {
        // Try the type annotation first (works even before type checking)
        if let Some(ann) = type_annotation {
            return is_ast_type_ref(&ann.node, self.scopes, &self.ref_type_structs);
        }
        // Fall back to resolved type
        let def = self.scopes.get_def(def_id);
        if let Some(type_id) = def.type_id {
            return types::is_reference_type(type_id, self.types, &self.ref_type_structs);
        }
        false
    }

    fn check_function(&mut self, func: &FunctionDef) {
        // Reset state for each function
        self.var_states.clear();
        self.loop_depth = 0;
        self.loop_local_defs.clear();
        self.arena_depth = 0;
        self.arena_scoped_vars.clear();
        self.diverged = false;
        self.var_origins.clear();
        self.invalidated_origins.clear();
        self.current_param_def_ids.clear();
        self.active_outlives.clear();
        self.current_function_is_async = func.qualifiers.is_async;
        self.await_invalidated.clear();

        // Set scope-aware lookup context for this function
        self.current_fn_scope = self.function_body_scopes
            .get(&(func.name.node.clone(), func.name.span.start))
            .copied();
        if self.current_fn_scope.is_none() {
            // Module-level or equip function — fall back to global lookup.
        }

        // Set up return type for lifetime checking
        if let Some(def_id) = self.scopes.lookup(&func.name.node) {
            if let Some(info) = self.function_info.get(&def_id) {
                self.current_return_type_id = info.return_type_id;
            }
        } else {
            self.current_return_type_id = None;
        }

        // Set up param origins for lifetime tracking
        for (i, param) in func.params.iter().enumerate() {
            if let Some(def_id) = self.find_def_by_name(&param.node.name.node) {
                self.current_param_def_ids.push((def_id, i));

                // If this param is a reference type, track its origin as Param
                let is_ref = is_ast_type_ref(&param.node.type_.node, self.scopes, &self.ref_type_structs);
                if is_ref {
                    self.var_origins.insert(def_id, BorrowOrigin::Param { param_index: i, def_id });
                }
            }
        }

        match &func.body {
            FunctionBody::Block(block) => {
                self.check_block(block);
            }
            FunctionBody::Expression(expr) => {
                // Expression-body functions: also validate dangling returns
                if let Some(ret_type_id) = self.current_return_type_id {
                    if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs)
                        || types::is_callable_type(ret_type_id, self.types)
                    {
                        let origin = self.compute_expr_origin(expr);
                        if origin.contains_local() {
                            let return_name = match &expr.node {
                                Expr::Identifier(n) => n.clone(),
                                _ => "<expression>".to_string(),
                            };
                            if origin.contains_unknown() && !matches!(&origin, BorrowOrigin::Local(_)) {
                                self.error(
                                    SemanticErrorKind::UnresolvedBorrowOrigin {
                                        name: return_name,
                                    },
                                    expr.span,
                                );
                            } else {
                                let local_names = origin.local_names(self.scopes);
                                let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                                let local_declared_at = origin.source_def_ids().first()
                                    .map(|&did| self.scopes.get_def(did).span);
                                self.error(
                                    SemanticErrorKind::DanglingReturn {
                                        name: return_name,
                                        local_name,
                                        local_declared_at,
                                    },
                                    expr.span,
                                );
                            }
                        }
                    }
                }
                self.check_expr(expr);
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }
}

// ─── Pass 5a: Compute return_borrows_from ─────────────────

/// Compute `return_borrows_from` for each function by analyzing its body.
/// This is a lightweight pre-pass before the main borrow check.
fn compute_all_return_borrows(
    module: &Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    ref_type_structs: &FxHashSet<DefId>,
) {
    for item in &module.items {
        match &item.node {
            Item::Function(f) => {
                compute_function_return_borrows(f, scopes, types, resolution_map, function_info, ref_type_structs);
            }
            Item::Equip(impl_block) => {
                for method in &impl_block.items {
                    compute_function_return_borrows(&method.node, scopes, types, resolution_map, function_info, ref_type_structs);
                }
            }
            _ => {}
        }
    }
}

fn compute_function_return_borrows(
    func: &FunctionDef,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    ref_type_structs: &FxHashSet<DefId>,
) {
    let def_id = match scopes.lookup(&func.name.node) {
        Some(id) => id,
        None => return,
    };

    let ret_type_id = match function_info.get(&def_id).and_then(|fi| fi.return_type_id) {
        Some(id) => id,
        None => return,
    };

    // Only relevant if the return type is a reference or callable type
    if !types::is_reference_type(ret_type_id, types, ref_type_structs)
        && !types::is_callable_type(ret_type_id, types)
    {
        return;
    }

    // Phase 3: Check for explicit `live` annotations first (they override body analysis).
    // Phase 5: If named groups are present (live(a), live(b)), fall through to body analysis
    // to determine precisely which groups flow to the return — named groups enable more
    // precise tracking than bare `live`.
    {
        let info = match function_info.get(&def_id) {
            Some(i) => i,
            None => return,
        };
        let live_indices: Vec<usize> = info.param_is_live.iter()
            .enumerate()
            .filter(|(_, is_live)| **is_live)
            .map(|(i, _)| i)
            .collect();
        let has_named_groups = info.param_live_groups.iter().any(|g| g.is_some());
        if !live_indices.is_empty() && !has_named_groups {
            // Bare `live` (no group names): all live params → return_borrows_from
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_borrows_from = live_indices;
            }
            return;
        }
        // Named groups: fall through to body analysis for precision
    }

    // Phase 2: Body analysis — trace return expressions back to params
    // Build a map from param names to their indices for this function
    let param_name_to_idx: FxHashMap<String, usize> = {
        let info = match function_info.get(&def_id) {
            Some(i) => i,
            None => return,
        };
        info.param_names.iter().enumerate()
            .map(|(i, name)| (name.clone(), i))
            .collect()
    };

    let mut borrows_from = FxHashSet::default();

    // Build local alias map for Block bodies (expression bodies have no locals)
    let local_aliases = match &func.body {
        FunctionBody::Block(block) => build_local_alias_map(block, &param_name_to_idx, &*function_info, resolution_map, scopes),
        _ => LocalAliasMap::default(),
    };

    // Shared reborrow for trace functions (compute_function_return_borrows holds &mut)
    let fi_ref: &FxHashMap<DefId, FunctionInfo> = &*function_info;

    match &func.body {
        FunctionBody::Expression(expr) => {
            trace_expr_to_params(expr, &param_name_to_idx, &local_aliases, fi_ref, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Block(block) => {
            trace_block_returns_to_params(block, &param_name_to_idx, &local_aliases, fi_ref, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // No body — apply elision rules
            let info = match function_info.get(&def_id) {
                Some(i) => i,
                None => return,
            };
            let ref_param_indices: Vec<usize> = info.param_type_ids.iter()
                .enumerate()
                .filter(|(_, tid)| tid.map_or(false, |id|
                    types::is_reference_type(id, types, ref_type_structs)
                    || types::is_callable_type(id, types)))
                .map(|(i, _)| i)
                .collect();
            if ref_param_indices.len() == 1 {
                borrows_from.insert(ref_param_indices[0]);
            } else if !info.param_names.is_empty() && info.param_names[0] == "self" {
                borrows_from.insert(0);
            } else if ref_param_indices.is_empty() {
                // No reference-type params → return can't borrow from any param.
                // Mark as static so callers don't get Unknown origin.
                if let Some(fi) = function_info.get_mut(&def_id) {
                    fi.return_origin_is_static = true;
                }
                return;
            }
        }
    }

    if !borrows_from.is_empty() {
        let mut result: Vec<usize> = borrows_from.into_iter().collect();
        result.sort();
        if let Some(fi) = function_info.get_mut(&def_id) {
            fi.return_borrows_from = result;
        }
    } else {
        // Elision fallback for functions with bodies that didn't trace to any param
        let info = match function_info.get(&def_id) {
            Some(i) => i,
            None => return,
        };
        let ref_param_indices: Vec<usize> = info.param_type_ids.iter()
            .enumerate()
            .filter(|(_, tid)| tid.map_or(false, |id|
                types::is_reference_type(id, types, ref_type_structs)
                || types::is_callable_type(id, types)))
            .map(|(i, _)| i)
            .collect();
        if ref_param_indices.len() == 1 {
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_borrows_from = ref_param_indices;
            }
        } else if !info.param_names.is_empty() && info.param_names[0] == "self" {
            // Method with &self — borrows from self
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_borrows_from = vec![0];
            }
        }

        // If function has a body and return_borrows_from is still empty after
        // body analysis + elision, the return is provably static.
        let info = function_info.get(&def_id).unwrap();
        if info.has_body && info.return_borrows_from.is_empty() {
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_origin_is_static = true;
            }
        }
    }
}

// ─── Visitor: Captured Reference Origin Collector ─────────────

/// Walks a closure body collecting origins of captured reference-type variables.
/// Skips nested closures (own capture scope) and closure parameters.
struct CapturedRefOriginCollector<'a> {
    resolution_map: &'a ResolutionMap,
    scopes: &'a ScopeTable,
    types: &'a TypeTable,
    ref_type_structs: &'a FxHashSet<DefId>,
    var_origins: &'a FxHashMap<DefId, BorrowOrigin>,
    param_names: &'a FxHashSet<&'a str>,
    origins: Vec<BorrowOrigin>,
}

impl crate::parser::visitor::ExprVisitor for CapturedRefOriginCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                if self.param_names.contains(name.as_str()) {
                    return;
                }
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(def_id);
                    if def.kind == DefKind::Variable {
                        if let Some(type_id) = def.type_id {
                            if types::is_reference_type(type_id, self.types, self.ref_type_structs)
                                || types::is_callable_type(type_id, self.types)
                            {
                                if let Some(origin) = self.var_origins.get(&def_id) {
                                    self.origins.push(origin.clone());
                                }
                            }
                        }
                    }
                }
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Default walk handles all other variants exhaustively
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    // visit_stmt and visit_block: use default walk_stmt/walk_block.
    // This covers Stmt::With, Assert, and all other statement variants
    // that the previous manual walker missed with its `_ => {}` catch-all.
}

// ─── Visitor: Closure Body Param Tracer ──────────────────────

/// Walks a closure body to find references to enclosing function parameters.
/// Skips nested closures (they have their own capture scope).
struct ClosureBodyParamTracer<'a> {
    outer_params: &'a FxHashMap<String, usize>,
    outer_aliases: &'a LocalAliasMap,
    closure_params: &'a FxHashSet<&'a str>,
    result: &'a mut FxHashSet<usize>,
}

impl crate::parser::visitor::ExprVisitor for ClosureBodyParamTracer<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                if !self.closure_params.contains(name.as_str()) {
                    if let Some(&idx) = self.outer_params.get(name) {
                        self.result.insert(idx);
                    } else if let Some(indices) = self.outer_aliases.get(name) {
                        self.result.extend(indices);
                    }
                }
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Default walk handles all other variants exhaustively
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    // visit_stmt and visit_block: use default walk_stmt/walk_block.
    // This covers all statement variants exhaustively, fixing coverage gaps
    // in the previous manual trace_closure_body_stmts (which missed Assign,
    // CompoundAssign, For, While, Loop, Match, With, Unsafe, Assert, etc.).
}

// ─── Local Alias Map ─────────────────────────────────────────

/// Maps local variable names to the set of param indices their values may originate from.
/// Over-approximates via union: assignments in different branches are merged.
type LocalAliasMap = FxHashMap<String, FxHashSet<usize>>;

/// Build a map from local variable names to the param indices they may alias.
/// Walks all statements in the function body before return-tracing begins.
fn build_local_alias_map(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) -> LocalAliasMap {
    let mut aliases = LocalAliasMap::default();
    build_aliases_from_block(block, param_names, &mut aliases, function_info, resolution_map, scopes);
    aliases
}

fn build_aliases_from_block(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    aliases: &mut LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) {
    for stmt in &block.stmts {
        build_aliases_from_stmt(&stmt.node, param_names, aliases, function_info, resolution_map, scopes);
    }
}

fn build_aliases_from_stmt(
    stmt: &Stmt,
    param_names: &FxHashMap<String, usize>,
    aliases: &mut LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) {
    match stmt {
        Stmt::VarDecl { pattern, value, .. } => {
            if let Pattern::Binding(name) = &pattern.node {
                let indices = collect_param_indices(&value.node, param_names, aliases, function_info, resolution_map, scopes);
                if !indices.is_empty() {
                    aliases.entry(name.clone()).or_default().extend(indices);
                }
            }
        }
        Stmt::Assign { target, value } => {
            if let Expr::Identifier(name) = &target.node {
                // Skip params — they already have direct entries
                if !param_names.contains_key(name) {
                    let indices = collect_param_indices(&value.node, param_names, aliases, function_info, resolution_map, scopes);
                    if !indices.is_empty() {
                        // Union with existing (conservative for reassignment)
                        aliases.entry(name.clone()).or_default().extend(indices);
                    }
                }
            }
        }
        // Recurse into control flow — over-approximate by unioning all branches
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            build_aliases_from_block(then_body, param_names, aliases, function_info, resolution_map, scopes);
            for (_, body) in elif_branches {
                build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
            }
            if let Some(else_body) = else_body {
                build_aliases_from_block(else_body, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms {
                if let Expr::Block(block) = &arm.body.node {
                    build_aliases_from_block(block, param_names, aliases, function_info, resolution_map, scopes);
                }
            }
            if let Some(else_arm) = else_arm {
                build_aliases_from_block(else_arm, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                build_aliases_from_block(&arm.body, param_names, aliases, function_info, resolution_map, scopes);
            }
            if let Some(else_arm) = else_arm {
                build_aliases_from_block(else_arm, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body } => {
            build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
        }
        Stmt::With { body, .. } | Stmt::Unsafe { body } => {
            build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
        }
        _ => {}
    }
}

/// Trace an expression to the set of param indices it may originate from.
/// Consults both `param_names` (direct params) and `aliases` (local variables).
fn collect_param_indices(
    expr: &Expr,
    param_names: &FxHashMap<String, usize>,
    aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) -> FxHashSet<usize> {
    let mut result = FxHashSet::default();
    match expr {
        Expr::Identifier(name) => {
            if let Some(&idx) = param_names.get(name) {
                result.insert(idx);
            } else if let Some(indices) = aliases.get(name) {
                result.extend(indices);
            }
        }
        Expr::SelfExpr => {
            if param_names.contains_key("self") {
                result.insert(0);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            result.extend(collect_param_indices(&object.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Index { object, .. } => {
            result.extend(collect_param_indices(&object.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Call { callee, args, .. } => {
            // Resolve callee and look up return_borrows_from
            let callee_def_id = match &callee.node {
                Expr::Identifier(_) => resolution_map.get(&callee.span.start).copied(),
                Expr::Path { segments } => segments.first().and_then(|s| resolution_map.get(&s.span.start).copied()),
                _ => None,
            };
            if let Some(def_id) = callee_def_id {
                if let Some(info) = function_info.get(&def_id) {
                    if !info.return_borrows_from.is_empty() {
                        for &idx in &info.return_borrows_from {
                            if let Some(arg) = args.get(idx) {
                                result.extend(collect_param_indices(&arg.node.value.node, param_names, aliases, function_info, resolution_map, scopes));
                            }
                        }
                        return result;
                    }
                }
            }
            // Callee not resolved or no return_borrows_from — no info
        }
        Expr::If { then_branch, elif_branches, else_branch, .. } => {
            result.extend(collect_param_indices(&then_branch.node, param_names, aliases, function_info, resolution_map, scopes));
            for (_, body) in elif_branches {
                result.extend(collect_param_indices(&body.node, param_names, aliases, function_info, resolution_map, scopes));
            }
            if let Some(else_br) = else_branch {
                result.extend(collect_param_indices(&else_br.node, param_names, aliases, function_info, resolution_map, scopes));
            }
        }
        Expr::NilCoalescing { lhs, rhs } => {
            result.extend(collect_param_indices(&lhs.node, param_names, aliases, function_info, resolution_map, scopes));
            result.extend(collect_param_indices(&rhs.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Move { expr: inner } | Expr::Deref { expr: inner } | Expr::Try { expr: inner } => {
            result.extend(collect_param_indices(&inner.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        _ => {}
    }
    result
}

/// Trace a return expression back through variable assignments to find which params flow to it.
fn trace_expr_to_params(
    expr: &Spanned<Expr>,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match &expr.node {
        Expr::Identifier(name) => {
            if let Some(&idx) = param_names.get(name) {
                result.insert(idx);
            } else if let Some(indices) = local_aliases.get(name) {
                result.extend(indices);
            }
        }

        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            trace_expr_to_params(object, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Index { object, .. } => {
            trace_expr_to_params(object, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::If { then_branch, elif_branches, else_branch, .. } => {
            trace_expr_to_params(then_branch, param_names, local_aliases, function_info, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_expr_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_br) = else_branch {
                trace_expr_to_params(else_br, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }

        Expr::Block(block) | Expr::Do { body: block } => {
            if let Some(last) = block.stmts.last() {
                if let Stmt::Expr(e) = &last.node {
                    trace_expr_to_params(e, param_names, local_aliases, function_info, resolution_map, scopes, result);
                }
            }
        }

        Expr::SelfExpr => {
            if param_names.contains_key("self") {
                result.insert(0);
            }
        }

        Expr::StructLiteral { args, .. } => {
            for arg in args {
                trace_expr_to_params(arg, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }

        Expr::Call { callee, args, .. } => {
            // Resolve callee and trace through its return_borrows_from
            let callee_def_id = match &callee.node {
                Expr::Identifier(_) => resolution_map.get(&callee.span.start).copied(),
                Expr::Path { segments } => segments.first().and_then(|s| resolution_map.get(&s.span.start).copied()),
                _ => None,
            };
            if let Some(def_id) = callee_def_id {
                if let Some(info) = function_info.get(&def_id) {
                    if !info.return_borrows_from.is_empty() {
                        for &idx in &info.return_borrows_from {
                            if let Some(arg) = args.get(idx) {
                                trace_expr_to_params(&arg.node.value, param_names, local_aliases, function_info, resolution_map, scopes, result);
                            }
                        }
                    }
                }
            }
        }

        Expr::NilCoalescing { lhs, rhs } => {
            trace_expr_to_params(lhs, param_names, local_aliases, function_info, resolution_map, scopes, result);
            trace_expr_to_params(rhs, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Move { expr: inner } | Expr::Deref { expr: inner } | Expr::Try { expr: inner } => {
            trace_expr_to_params(inner, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Closure { body, params, .. } => {
            use crate::parser::visitor::ExprVisitor;
            let closure_param_names: FxHashSet<&str> = params.iter()
                .map(|p| p.node.name.node.as_str())
                .collect();
            let mut tracer = ClosureBodyParamTracer {
                outer_params: param_names,
                outer_aliases: local_aliases,
                closure_params: &closure_param_names,
                result,
            };
            tracer.visit_expr(body);
        }

        _ => {}
    }
}

/// Walk a block looking for Return statements and trace them to params.
fn trace_block_returns_to_params(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    for stmt in &block.stmts {
        trace_stmt_returns_to_params(&stmt.node, param_names, local_aliases, function_info, resolution_map, scopes, result);
    }
}

fn trace_stmt_returns_to_params(
    stmt: &Stmt,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match stmt {
        Stmt::Return(Some(expr)) => {
            trace_expr_to_params(expr, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            trace_block_returns_to_params(then_body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_body) = else_body {
                trace_block_returns_to_params(else_body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms {
                if let Expr::Block(block) = &arm.body.node {
                    trace_block_returns_to_params(block, param_names, local_aliases, function_info, resolution_map, scopes, result);
                }
            }
            if let Some(else_arm) = else_arm {
                trace_block_returns_to_params(else_arm, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                trace_block_returns_to_params(&arm.body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_arm) = else_arm {
                trace_block_returns_to_params(else_arm, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body } => {
            trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        Stmt::With { body, .. } | Stmt::Unsafe { body } => {
            trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        _ => {}
    }
}

// ─── Pass 5b: Full Borrow Check ──────────────────────────────

/// Run borrow checking on the entire module.
pub fn check_module(
    module: &Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    function_body_scopes: &FxHashMap<(String, usize), ScopeId>,
    immutable_by_default: bool,
    expr_types: &FxHashMap<Span, TypeId>,
    method_resolutions: &FxHashMap<usize, DefId>,
    errors: &mut Vec<SemanticError>,
) {
    // Phase 4: compute which structs have reference-type fields
    let ref_type_structs = compute_ref_type_structs(module, scopes);
    let struct_field_ref_flags = compute_struct_field_ref_flags(module, scopes, &ref_type_structs);

    // Pass 5a: compute return_borrows_from for each function
    compute_all_return_borrows(module, scopes, types, resolution_map, function_info, &ref_type_structs);

    // Pass 5b: full borrow check with origin tracking
    let mut checker = BorrowChecker::new(
        scopes, types, resolution_map, function_info, function_body_scopes,
        immutable_by_default, expr_types,
        method_resolutions, ref_type_structs, struct_field_ref_flags,
    );

    for item in &module.items {
        match &item.node {
            Item::Function(f) => {
                checker.check_function(f);
            }
            Item::Equip(impl_block) => {
                for method in &impl_block.items {
                    checker.check_function(&method.node);
                }
            }
            Item::Test(t) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.loop_local_defs.clear();
                checker.arena_depth = 0;
                checker.arena_scoped_vars.clear();
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
                checker.await_invalidated.clear();
                for binding in &t.with_bindings {
                    checker.check_expr(&binding.expr);
                }
                checker.check_block(&t.body);
            }
            Item::SuiteSetup(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.loop_local_defs.clear();
                checker.arena_depth = 0;
                checker.arena_scoped_vars.clear();
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
                checker.await_invalidated.clear();
                checker.check_block(&s.body);
            }
            Item::SuiteTeardown(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.loop_local_defs.clear();
                checker.arena_depth = 0;
                checker.arena_scoped_vars.clear();
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
                checker.await_invalidated.clear();
                checker.check_block(&s.body);
            }
            _ => {}
        }
    }

    errors.extend(checker.errors);
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::semantic;
    use super::*;

    fn check(source: &str) -> Vec<SemanticError> {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze(&mut module, &[]);
        result.errors
    }

    fn has_error(errors: &[SemanticError], pred: impl Fn(&SemanticErrorKind) -> bool) -> bool {
        errors.iter().any(|e| pred(&e.kind))
    }

    #[test]
    fn use_after_move() {
        let source = "\
void main():
    String s1 = \"hello\"
    String s2 = !s1
    print(s1)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s1")),
            "expected UseAfterMove for s1, got: {:?}", errors
        );
    }

    #[test]
    fn double_move() {
        let source = "\
void main():
    String s = \"hello\"
    String a = !s
    String b = !s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { name, .. } if name == "s")),
            "expected DoubleMove for s, got: {:?}", errors
        );
    }

    #[test]
    fn move_in_loop() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    for i in 0..3:
        consume(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected MoveInLoop for s, got: {:?}", errors
        );
    }

    #[test]
    fn copy_types_ok() {
        let source = "\
void main():
    int a = 5
    int b = a
    int c = a
    print(\"{b}\")
    print(\"{c}\")
";
        let errors = check(source);
        // int is Copy — no errors expected from borrow checker
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::MoveWithoutOperator { .. }
                    | SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected borrow errors for Copy types: {:?}", errors
        );
    }

    #[test]
    fn move_then_new_decl_ok() {
        // After moving s, declaring a new s in the same scope is fine
        // (The old s is gone, but the new one is a fresh variable)
        let source = "\
void main():
    String s1 = \"hello\"
    String s2 = !s1
    int x = 5
    int y = x
";
        let errors = check(source);
        // No borrow errors: s1 moved once (valid), x is Copy
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
                    | SemanticErrorKind::DoubleMove { .. }
            )),
            "unexpected borrow errors: {:?}", errors
        );
    }

    #[test]
    fn reassignment_revives() {
        let source = "\
void main():
    String s = \"hello\"
    String t = !s
    s = \"world\"
    print(s)
";
        let errors = check(source);
        // After moving s and reassigning it, s is live again — no errors
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected UseAfterMove after reassignment: {:?}", errors
        );
    }

    // ── Ownership mismatch tests ──

    #[test]
    fn ownership_mismatch_move_param_bare_call() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    consume(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "s")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_mismatch_borrow_param_move_call() {
        let source = "\
void read_it(String &s):
    pass

void main():
    String s = \"hello\"
    read_it(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "s")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_mismatch_bare_param_mut_call() {
        let source = "\
void look(int x):
    pass

void main():
    int x = 5
    look(&x)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "x")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_match_move_ok() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    consume(!s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { .. })),
            "unexpected OwnershipMismatch: {:?}", errors
        );
    }

    #[test]
    fn ownership_match_borrow_ok() {
        let source = "\
void read_it(String &s):
    pass

void main():
    String s = \"hello\"
    read_it(&s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { .. })),
            "unexpected OwnershipMismatch: {:?}", errors
        );
    }

    // ── Aliasing conflict tests ──

    #[test]
    fn aliasing_double_mut_borrow() {
        let source = "\
void both(String &a, String &b):
    pass

void main():
    String s = \"hello\"
    both(&s, &s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for double &, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_borrow_and_mut_borrow() {
        let source = "\
void mixed(String a, String &b):
    pass

void main():
    String s = \"hello\"
    mixed(s, &s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for bare + &, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_mut_borrow_and_move() {
        let source = "\
void danger(String &a, String !b):
    pass

void main():
    String s = \"hello\"
    danger(&s, !s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for & + !, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_double_bare_ok() {
        let source = "\
void both(int a, int b):
    pass

void main():
    int x = 5
    both(x, x)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { .. })),
            "unexpected BorrowConflict for double bare: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_double_call() {
        let source = "\
int apply_once(ConsumeCallable[int(int)] f, int x):
    return f(x)

void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    int r1 = f(5)
    int r2 = f(10)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { name, .. } if name == "f")),
            "expected DoubleMove for f, got: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_single_call_ok() {
        let source = "\
void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    int r = f(5)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DoubleMove { .. }
                    | SemanticErrorKind::MoveInLoop { .. }
                    | SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected borrow errors for single ConsumeCallable call: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_loop_error() {
        let source = "\
void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    for i in 0..3:
        int r = f(i)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "f")),
            "expected MoveInLoop for f, got: {:?}", errors
        );
    }

    #[test]
    fn if_else_branch_merging() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    if true:
        consume(!s)
    else:
        pass
    print(s)
";
        let errors = check(source);
        // s is moved in one branch but not the other — conservative: treat as moved
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s")),
            "expected UseAfterMove after conditional move, got: {:?}", errors
        );
    }

    // ── Lifetime inference tests ──

    #[test]
    fn return_str_literal_ok() {
        let source = "\
str f() = \"hello\"
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for string literal: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_param_ok() {
        let source = "\
str f(str s) = s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for param forwarding: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_local_string() {
        let source = "\
str f():
    String s = \"hi\"
    return s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { name, local_name, .. }
                if name == "s" && local_name == "s")),
            "expected DanglingReturn for local String, got: {:?}", errors
        );
    }

    #[test]
    fn use_str_after_string_moved() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hi\"
    str v = s
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for v after s moved, got: {:?}", errors
        );
    }

    #[test]
    fn cross_function_borrow_ok() {
        let source = "\
str id(str s) = s

void main():
    print(id(\"hi\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for cross-function borrow: {:?}", errors
        );
    }

    #[test]
    fn cross_function_chain() {
        let source = "\
str f(str s) = s

str g(str s) = f(s)

void main():
    print(g(\"hello\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for chained cross-function borrow: {:?}", errors
        );
    }

    #[test]
    fn cross_function_dangling() {
        let source = "\
str id(str s) = s

void consume(String !s):
    pass

void main():
    String s = \"hi\"
    str v = id(s)
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved through cross-function call, got: {:?}", errors
        );
    }

    #[test]
    fn live_param_explicit() {
        let source = "\
str first(live str a, str b) = a
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn with live annotation: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_expression_body_local() {
        // Expression-body function returning a local String → dangling
        let source = "\
str bad():
    String s = \"hello\"
    return s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "expected DanglingReturn for expression-body returning local: {:?}", errors
        );
    }

    #[test]
    fn str_from_param_through_local_ok() {
        let source = "\
str f(str s):
    str local = s
    return local
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for param forwarded through local: {:?}", errors
        );
    }

    #[test]
    fn str_view_reassigned_ok() {
        let source = "\
void main():
    String s = \"hello\"
    str v = s
    v = \"world\"
    String t = !s
    print(v)
";
        let errors = check(source);
        // v was reassigned to a literal before s was moved — no error
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved after reassignment: {:?}", errors
        );
    }

    // ── Struct borrowing tests (Phase 4) ──

    #[test]
    fn struct_str_field_auto() {
        // Struct with a str field assigned from param — no error
        let source = "\
struct View:
    str name

void main():
    str s = \"hello\"
    View v = View(s)
    print(v.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for struct with str field: {:?}", errors
        );
    }

    #[test]
    fn struct_outlives_source() {
        // Struct borrows from moved local → UseAfterSourceMoved
        let source = "\
struct View:
    str name

void consume(String !s):
    pass

void main():
    String s = \"hello\"
    View v = View(s)
    consume(!s)
    print(v.name)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for struct outliving its source, got: {:?}", errors
        );
    }

    #[test]
    fn struct_from_literal_ok() {
        // Struct with str field from string literal → no error (Static origin)
        let source = "\
struct View:
    str name

void main():
    View v = View(\"hello\")
    print(v.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for struct with literal str field: {:?}", errors
        );
    }

    #[test]
    fn struct_no_ref_fields_unaffected() {
        // Struct without reference-type fields — no borrow tracking
        let source = "\
struct Point:
    float x
    float y

void main():
    Point p = Point(1.0, 2.0)
    print(\"{p.x}\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for non-ref struct: {:?}", errors
        );
    }

    #[test]
    fn struct_transitive_borrow() {
        // Struct containing another struct with a ref field — transitive
        let source = "\
struct Inner:
    str name

struct Outer:
    Inner inner

void consume(String !s):
    pass

void main():
    String s = \"hello\"
    Inner i = Inner(s)
    Outer o = Outer(i)
    consume(!s)
    print(o.inner.name)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "expected UseAfterSourceMoved for transitive struct borrow, got: {:?}", errors
        );
    }

    #[test]
    fn struct_mixed_fields() {
        // Struct with both ref and non-ref fields — only ref field tracked
        let source = "\
struct Tagged:
    str label
    int count

void consume(String !s):
    pass

void main():
    String s = \"hello\"
    Tagged t = Tagged(s, 42)
    consume(!s)
    print(t.label)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "t" && source_name == "s")),
            "expected UseAfterSourceMoved for struct with mixed fields, got: {:?}", errors
        );
    }

    // ── Phase 5: Named borrow groups + outlives ──

    #[test]
    fn named_groups_basic_ok() {
        let source = "\
str pick(live(a) str x, live(b) str y) where a outlives b:
    return x

void main():
    String s1 = \"hello\"
    String s2 = \"world\"
    str r = pick(s1, s2)
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OutlivesViolation { .. })),
            "expected no outlives violation, got: {:?}", errors
        );
    }

    #[test]
    fn named_groups_shorter_moved_ok() {
        // Moving the "shorter" group's source is fine — only "longer" must outlive "shorter"
        let source = "\
str pick(live(a) str x, live(b) str y) where a outlives b:
    return x

void main():
    String s1 = \"hello\"
    String s2 = \"world\"
    str r = pick(s1, s2)
    String moved = !s2
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OutlivesViolation { .. })),
            "expected no outlives violation when shorter group moved, got: {:?}", errors
        );
    }

    #[test]
    fn named_groups_outlives_violation() {
        // Moving the "longer" group's source while the "shorter" group's source is alive
        let source = "\
str pick(live(a) str x, live(b) str y) where a outlives b:
    return x

void main():
    String s1 = \"hello\"
    String s2 = \"world\"
    str r = pick(s1, s2)
    String moved = !s1
    print(s2)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OutlivesViolation { longer_group, .. }
                if longer_group == "a")),
            "expected OutlivesViolation for group a, got: {:?}", errors
        );
    }

    #[test]
    fn bare_live_still_works() {
        // Bare `live` (no group name) still works for backwards compat
        let source = "\
str view(live str s):
    return s

void main():
    String s = \"hello\"
    str v = view(s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OutlivesViolation { .. }
                | SemanticErrorKind::UseAfterSourceMoved { .. }
                | SemanticErrorKind::DanglingReturn { .. })),
            "expected no errors for bare live, got: {:?}", errors
        );
    }

    // ── Phase 6: Branch origin merging ──

    #[test]
    fn branch_origin_merging_if_one_moves() {
        // Move source in one branch only → use ref after merge → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    if true:
        consume(!s)
    else:
        pass
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved after branch origin merge, got: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_both_move() {
        // Move source in both branches → use ref after → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    if true:
        consume(!s)
    else:
        consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved when moved in both branches, got: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_neither_moves() {
        // No moves in any branch → no error
        let source = "\
void main():
    String s = \"hello\"
    str v = s
    if true:
        pass
    else:
        pass
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved when neither branch moves: {:?}", errors
        );
    }

    // ── Phase 6: Pattern binding origins ──

    // ── Phase 6: Closure capture origin tracking ──

    #[test]
    fn closure_capture_source_moved() {
        // Closure captures ref-type var, source moved → UseAfterSourceMoved on call
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    auto f = (): print(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "f" && source_name == "s")),
            "expected UseAfterSourceMoved for f after s moved, got: {:?}", errors
        );
    }

    #[test]
    fn closure_return_captures_local() {
        // Returning a closure that captures a local reference → DanglingReturn
        let source = "\
Callable[void()] bad():
    String local = \"hello\"
    str v = local
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "expected DanglingReturn for closure capturing local, got: {:?}", errors
        );
    }

    #[test]
    fn closure_return_captures_param_ok() {
        // Returning a closure that captures a param → no error
        let source = "\
Callable[void()] ok(str v):
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for closure capturing param: {:?}", errors
        );
    }

    #[test]
    fn closure_return_literal_ok() {
        // Returning a closure that captures a literal str → no error
        let source = "\
Callable[void()] ok():
    str v = \"hello\"
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for closure capturing literal: {:?}", errors
        );
    }

    #[test]
    fn closure_no_ref_captures_ok() {
        // Closure with no ref-type captures → no false positive
        let source = "\
void main():
    int x = 42
    auto f = (): print(x)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for closure with no ref captures: {:?}", errors
        );
    }

    #[test]
    fn closure_capture_literal_ok() {
        // Closure capturing str from literal → Static origin
        let source = "\
void main():
    str v = \"hello\"
    auto f = (): print(v)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for closure capturing literal: {:?}", errors
        );
    }

    // ── Closure body scope isolation ──

    #[test]
    fn closure_body_move_does_not_leak_to_enclosing_scope() {
        // A move inside a closure body should not mark the variable as Moved
        // in the enclosing scope — the closure body executes on call, not at
        // definition.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    auto f = (): consume(!s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. })),
            "closure body move should not leak to enclosing scope: {:?}", errors
        );
    }

    #[test]
    fn closure_body_move_still_detected_inside_body() {
        // A double-move inside the closure body should still be caught.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    auto f = ():
        consume(!s)
        consume(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. }
                | SemanticErrorKind::DoubleMove { .. })),
            "double move inside closure body should be caught: {:?}", errors
        );
    }

    #[test]
    fn closure_in_loop_body_no_false_move_in_loop() {
        // A move inside a closure body should not trigger MoveInLoop even if
        // the closure is defined inside a loop.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    for i in 0..3:
        auto f = (): consume(!s)
        print(\"ok\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "move inside closure body should not trigger MoveInLoop: {:?}", errors
        );
    }

    #[test]
    fn closure_definition_preserves_enclosing_origins() {
        // A closure that captures a ref-type variable should not change the
        // variable's origin in the enclosing scope.
        let source = "\
void main():
    String owner = \"hello\"
    str v = owner
    auto f = (): print(v)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterMove { .. }
                | SemanticErrorKind::UseAfterSourceMoved { .. })),
            "closure definition should not alter enclosing origins: {:?}", errors
        );
    }

    // ── Phase 6: Temporary borrow detection ──

    #[test]
    fn temporary_borrow_str_from_string_call() {
        // str v = make_string() where make_string returns String → TemporaryBorrow
        let source = "\
String make_string():
    return \"hello\"

void main():
    str v = make_string()
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { name, callee, .. }
                if name == "v" && callee == "make_string")),
            "expected TemporaryBorrow for str from String call, got: {:?}", errors
        );
    }

    #[test]
    fn no_temporary_borrow_str_from_str_call() {
        // str v = get_str() where get_str returns str → no error (returns ref type)
        let source = "\
str get_str() = \"hello\"

void main():
    str v = get_str()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow for str from str call: {:?}", errors
        );
    }

    #[test]
    fn no_temporary_borrow_owning_to_owning() {
        // String s = make_string() → no error (owning to owning)
        let source = "\
String make_string():
    return \"hello\"

void main():
    String s = make_string()
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow for owning-to-owning: {:?}", errors
        );
    }

    #[test]
    fn match_pattern_binding_source_moved() {
        // str view's source moved before use in match → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    consume(!s)
    int x = 1
    match x:
        case 1:
            print(v)
        case 2:
            pass
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for pattern binding, got: {:?}", errors
        );
    }

    #[test]
    fn match_pattern_binding_literal_ok() {
        // Scrutinee from literal → no error
        let source = "\
void main():
    str v = \"hello\"
    int x = 1
    match x:
        case 1:
            print(v)
        case 2:
            pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved for literal pattern binding: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_match() {
        // Move source in one match arm → use ref after → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    int x = 1
    match x:
        case 1:
            consume(!s)
        case 2:
            pass
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved after match branch origin merge, got: {:?}", errors
        );
    }

    #[test]
    fn method_origin_borrows_from_self() {
        // Method returns str borrowing from self.field — no error when receiver is alive
        let source = "\
struct Holder:
    String name

equip Holder:
    str get_name(self):
        return self.name

void main():
    Holder h = Holder(\"hello\")
    str v = h.get_name()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                | SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected error for method borrowing from alive receiver: {:?}", errors
        );
    }

    #[test]
    fn method_origin_use_after_source_moved() {
        // Take str from method, move receiver, use str → UseAfterSourceMoved
        let source = "\
struct Holder:
    String name

equip Holder:
    str get_name(self):
        return self.name

void consume(Holder !h):
    pass

void main():
    Holder h = Holder(\"hello\")
    str v = h.get_name()
    consume(!h)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "h")),
            "expected UseAfterSourceMoved for v after h moved, got: {:?}", errors
        );
    }

    #[test]
    fn method_temporary_borrow() {
        // str v = b.build() where build() returns String → TemporaryBorrow
        let source = "\
struct Builder:
    String data

equip Builder:
    String build(self):
        return !self.data

void main():
    Builder b = Builder(\"hello\")
    str v = b.build()
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { name, callee, .. }
                if name == "v" && callee == "b.build")),
            "expected TemporaryBorrow for method returning String, got: {:?}", errors
        );
    }

    #[test]
    fn loop_origin_merging_use_after_move() {
        // Move source inside while body, use ref after loop → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    int i = 0
    while i < 3:
        consume(!s)
        i = i + 1
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                    if name == "v" && source_name == "s"))
            || has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected UseAfterSourceMoved or MoveInLoop after loop origin merge, got: {:?}", errors
        );
    }

    #[test]
    fn loop_origin_no_move_ok() {
        // No move in loop → no error
        let source = "\
void main():
    String s = \"hello\"
    str v = s
    int i = 0
    while i < 3:
        print(v)
        i = i + 1
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                | SemanticErrorKind::MoveInLoop { .. })),
            "unexpected error for loop with no move: {:?}", errors
        );
    }

    // ── Phase 8: Origin completeness sweep ──

    #[test]
    fn match_expr_origin_use_after_move() {
        // Match expression result borrows from source → move source → use result → error
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = match 1:
        case 1: s
        case 2: s
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for match expr origin, got: {:?}", errors
        );
    }

    #[test]
    fn try_expr_origin_propagation() {
        // str v = get_result()? borrows from param → move param source → use v → error
        let source = "\
str get_view(str s):
    return s

void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = get_view(s)
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for try expr origin propagation, got: {:?}", errors
        );
    }

    // ── Phase 10: Cross-function callable lifetime tracking ──

    #[test]
    fn cross_function_closure_source_moved() {
        // Closure from function call borrows param → source moved → use closure → error
        let source = "\
Callable[void()] make_printer(str v):
    return (): print(v)

void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    auto f = make_printer(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "f" && source_name == "s")),
            "expected UseAfterSourceMoved for cross-function closure, got: {:?}", errors
        );
    }

    #[test]
    fn cross_function_closure_ok() {
        // Closure from function call — source not moved → no error
        let source = "\
Callable[void()] make_printer(str v):
    return (): print(v)

void main():
    String s = \"hello\"
    str v = s
    auto f = make_printer(v)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "expected no UseAfterSourceMoved, got: {:?}", errors
        );
    }

    #[test]
    fn closure_reassignment_tracks_origin() {
        // Reassigning a closure variable updates origin → source moved → error
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    str v = s
    auto f = (): print(\"\")
    f = (): print(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "f" && source_name == "s")),
            "expected UseAfterSourceMoved for closure reassignment, got: {:?}", errors
        );
    }

    #[test]
    fn closure_pattern_binding_origin() {
        // Match-binding a callable-type value propagates origin → source moved → error
        let source = "\
void consume(String !s):
    pass

Callable[void()] make_printer(str v):
    return (): print(v)

void main():
    String s = \"hello\"
    str v = s
    auto c = make_printer(v)
    int x = 1
    match x:
        case n:
            consume(!s)
            c()
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "c" && source_name == "s")),
            "expected UseAfterSourceMoved for callable after source moved, got: {:?}", errors
        );
    }

    // ── Phase 11: Reassignment invalidation ──

    #[test]
    fn reassignment_invalidates_borrow() {
        // Reassigning a non-Copy owner invalidates borrows from the old value
        let source = "\
void main():
    String s = \"hello\"
    str v = s
    s = \"world\"
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "expected UseAfterSourceMoved for v after s reassigned, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_new_borrow_ok() {
        // After reassignment, new borrows from the variable are fine
        let source = "\
void main():
    String s = \"hello\"
    s = \"world\"
    str v = s
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved error, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_reborrow_ok() {
        // If the dependent variable is itself reassigned, the stale entry is cleared
        let source = "\
void main():
    String s = \"hello\"
    str v = s
    s = \"world\"
    v = s
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved error after reborrow, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_copy_type_ok() {
        // Copy types (str) don't destroy the old value on reassignment
        let source = "\
void main():
    str s = \"hello\"
    str v = s
    s = \"world\"
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved for Copy type, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_transitive() {
        // Transitive through a function call: w borrows from v which borrows from s
        let source = "\
str identity(live str x):
    return x

void main():
    String s = \"hello\"
    str v = s
    str w = identity(v)
    s = \"world\"
    print(w)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "w" && source_name == "s")),
            "expected UseAfterSourceMoved for w (transitive) after s reassigned, got: {:?}", errors
        );
    }

    // ── Constructor implicit move tests ──

    #[test]
    fn struct_constructor_implicit_move() {
        let source = "\
struct Wrapper:
    String value

void main():
    String s = \"hello\"
    Wrapper w = Wrapper(s)
    print(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s")),
            "expected UseAfterMove for s after implicit move into struct constructor, got: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_copy_args_ok() {
        let source = "\
struct Point:
    int x
    int y

void main():
    int a = 1
    int b = 2
    Point p = Point(a, b)
    print(\"{a}\")
    print(\"{b}\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
                    | SemanticErrorKind::DoubleMove { .. }
            )),
            "unexpected move errors for Copy-type constructor args: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_double_move() {
        let source = "\
struct Pair:
    String a
    String b

void main():
    String s = \"hello\"
    Pair p = Pair(s, s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { name, .. } if name == "s")),
            "expected DoubleMove for s passed twice to struct constructor, got: {:?}", errors
        );
    }

    #[test]
    fn variant_constructor_implicit_move() {
        let source = "\
enum Container:
    Holding(String)
    Empty

void main():
    String s = \"hello\"
    Container c = Container.Holding(s)
    print(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s")),
            "expected UseAfterMove for s after implicit move into variant constructor, got: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_param_not_moved() {
        // Parameters are borrowed from the caller — passing them to a
        // constructor should NOT be treated as a move.
        let source = "\
struct Wrapper:
    String value

void wrap(String s):
    Wrapper w = Wrapper(s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
                    | SemanticErrorKind::DoubleMove { .. }
            )),
            "unexpected move errors for param passed to constructor: {:?}", errors
        );
    }

    #[test]
    fn constructor_move_in_loop_not_return() {
        // `auto w = Wrapper.Value(s)` in a loop should error — s is consumed every iteration
        let source = "\
enum Wrapper:
    Value(String)

void main():
    String s = \"hello\"
    for i in 0..3:
        auto w = Wrapper.Value(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected MoveInLoop for s in non-return constructor, got: {:?}", errors
        );
    }

    #[test]
    fn constructor_move_in_loop_return() {
        // `return Wrapper.Value(label)` in a loop should be fine — return exits the function
        let source = "\
enum Wrapper:
    Value(String)

Wrapper find(Vector[String] items):
    for item in items:
        String label = item
        return Wrapper.Value(label)
    return Wrapper.Value(\"default\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "unexpected MoveInLoop for return-position constructor: {:?}", errors
        );
    }

    #[test]
    fn struct_literal_move_in_loop_not_return() {
        // Struct constructor consuming non-loop-local var in a loop should error.
        // Parser rewrites `Container(s)` to StructLiteral for struct types.
        let source = "\
struct Container:
    String value

void main():
    String s = \"hello\"
    for i in 0..3:
        auto b = Container(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected MoveInLoop for s in struct literal, got: {:?}", errors
        );
    }

    #[test]
    fn nested_constructor_in_return() {
        // `return Outer.Wrap(Inner.Val(s))` — nested constructors in return should be fine
        let source = "\
enum Inner:
    Val(String)

enum Outer:
    Wrap(Inner)

Outer find(Vector[String] items):
    for item in items:
        String s = item
        return Outer.Wrap(Inner.Val(s))
    return Outer.Wrap(Inner.Val(\"default\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "unexpected MoveInLoop for nested return-position constructor: {:?}", errors
        );
    }

    // ─── Pass 5a: Local Variable Alias Tracing ───────────────

    #[test]
    fn return_through_local_two_ref_params() {
        // `str pick(str a, str b)` with local alias — Pass 5a should trace `result` back to `a`
        let source = "\
str pick(str a, str b):
    str result = a
    return result

void main():
    str x = \"hello\"
    str y = \"world\"
    str r = pick(x, y)
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for return through local alias: {:?}", errors
        );
    }

    #[test]
    fn return_through_local_use_after_move() {
        // Return goes through local → Pass 5a traces to `a` → moving source invalidates result
        let source = "\
str id(str x):
    return x

str pick(str a, str b):
    str result = a
    return result

void main():
    String s = \"hello\"
    str r = pick(s, \"world\")
    String s2 = !s
    print(r)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, .. } if name == "r")),
            "expected UseAfterSourceMoved for r after source s was moved, got: {:?}", errors
        );
    }

    #[test]
    fn return_through_local_branch_union() {
        // result assigned from a or b depending on branch — both should flow
        let source = "\
str pick(str a, str b, bool flag):
    str result = a
    if flag:
        result = b
    return result

void main():
    str x = \"hello\"
    str y = \"world\"
    str r = pick(x, y, true)
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for branch union alias: {:?}", errors
        );
    }

    #[test]
    fn return_through_transitive_alias() {
        // `str x = a; str y = x; return y` — transitive chain should resolve
        let source = "\
str chain(str a, str b):
    str x = a
    str y = x
    return y

void main():
    str s = \"hello\"
    str r = chain(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for transitive alias chain: {:?}", errors
        );
    }

    #[test]
    fn return_through_call() {
        // `return id(a)` where id has return_borrows_from = [0] — trace through call
        let source = "\
str id(str x):
    return x

str wrapper(str a, str b):
    return id(a)

void main():
    str s = \"hello\"
    str r = wrapper(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for return through call: {:?}", errors
        );
    }

    #[test]
    fn return_local_assigned_from_call() {
        // `str result = id(a); return result` — alias from call result
        let source = "\
str id(str x):
    return x

str wrapper(str a, str b):
    str result = id(a)
    return result

void main():
    str s = \"hello\"
    str r = wrapper(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for local assigned from call: {:?}", errors
        );
    }

    #[test]
    fn unknown_origin_return_rejected() {
        // Bodyless function returning str with multiple ref params and no live annotation.
        // Multiple ref params = elision can't choose → origin is Unknown.
        // Returning its result should be rejected.
        let source = "\
str get_data(str a, str b)

str wrapper(str x, str y):
    str s = get_data(x, y)
    return s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UnresolvedBorrowOrigin { .. })),
            "expected UnresolvedBorrowOrigin, got: {:?}", errors
        );
    }

    #[test]
    fn static_origin_return_ok() {
        // Function with body returning a string literal — origin is Static.
        let source = "\
str greet():
    return \"hello\"

str wrapper():
    str s = greet()
    return s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "unexpected error for static return: {:?}", errors
        );
    }

    #[test]
    fn unknown_closure_capture_rejected() {
        // Closure captures str from unresolvable call, returned — should error.
        let source = "\
str get_data(str a, str b)

Callable[str()] wrapper(str x, str y):
    str s = get_data(x, y)
    return (): s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "expected error for closure capturing unknown origin, got: {:?}", errors
        );
    }

    #[test]
    fn merge_unknown_with_static() {
        // If/else with Static and Unknown branches — should error on return.
        let source = "\
str get_data(str a, str b)

str pick(bool cond, str x, str y):
    if cond:
        return \"hello\"
    return get_data(x, y)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "expected UnresolvedBorrowOrigin for merged unknown+static, got: {:?}", errors
        );
    }

    #[test]
    fn owned_return_from_bodyless_fn_ok() {
        // Bodyless function returning an owned type — caller should be fine
        // returning coerced result, since owned data is always Static.
        let source = "\
String make_string()

str wrapper():
    return make_string()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "unexpected error for owned return coercion: {:?}", errors
        );
    }

    // ─── Async/Await Borrow-Across-Await Tests ──────────────

    #[test]
    fn borrow_across_await_param_rejected() {
        // str param used after await → BorrowAcrossAwait
        let source = "\
async int do_work():
    return 1

async void process(str name):
    do_work().await()
    print(name)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { name } if name == "name")),
            "expected BorrowAcrossAwait for name, got: {:?}", errors
        );
    }

    #[test]
    fn borrow_across_await_local_rejected() {
        // Local str borrowing from param, used after await → error
        let source = "\
str get_slice(str input):
    return input

async int do_work():
    return 1

async void process(str data):
    str s = get_slice(data)
    do_work().await()
    print(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "expected BorrowAcrossAwait for s or data, got: {:?}", errors
        );
    }

    #[test]
    fn owned_across_await_ok() {
        // int (Copy) used after await → no error
        let source = "\
async int do_work():
    return 1

async int compute():
    int x = 42
    do_work().await()
    return x
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for Copy type: {:?}", errors
        );
    }

    #[test]
    fn static_str_across_await_ok() {
        // str from literal used after await → no error (Static origin)
        let source = "\
async int do_work():
    return 1

async void greet():
    str msg = \"hello\"
    do_work().await()
    print(msg)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for static str: {:?}", errors
        );
    }

    #[test]
    fn borrow_used_before_await_only_ok() {
        // str param used before await, not after → no error
        let source = "\
async int do_work():
    return 1

async void process(str name):
    print(name)
    do_work().await()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for use-before-await: {:?}", errors
        );
    }

    #[test]
    fn reassigned_after_await_ok() {
        // str param reassigned after await, then used → no error
        let source = "\
async int do_work():
    return 1

async void process(str name):
    do_work().await()
    name = \"fresh\"
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for reassigned-after-await: {:?}", errors
        );
    }

    #[test]
    fn borrow_across_await_in_branch() {
        // await in one if branch, use after merge → error (conservative)
        let source = "\
async int do_work():
    return 1

async void process(str name, bool cond):
    if cond:
        do_work().await()
    print(name)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { name } if name == "name")),
            "expected BorrowAcrossAwait for branch-await, got: {:?}", errors
        );
    }

    #[test]
    fn spawn_is_not_suspension_point() {
        // str param used after spawn → no error (spawn doesn't suspend)
        let source = "\
async int do_work():
    return 1

async void process(str name):
    auto task = spawn do_work()
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for spawn: {:?}", errors
        );
    }
}
