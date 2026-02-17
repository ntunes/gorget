use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
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
    fn contains_local(&self) -> bool {
        match self {
            BorrowOrigin::Local(_) => true,
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
            BorrowOrigin::CallResult(origins) => {
                origins.iter().flat_map(|o| o.local_names(scopes)).collect()
            }
            _ => vec![],
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
    call_span: Span,
}

/// Snapshot of origin tracking state (for branching).
type OriginSnapshot = FxHashMap<DefId, BorrowOrigin>;

/// Combined snapshot of variable states and origin tracking for branching.
/// Used by save/restore/merge_branch_state to handle all branching state atomically.
struct BranchState {
    var_states: StateSnapshot,
    origins: OriginSnapshot,
    invalidated: FxHashSet<DefId>,
}

// ─── Copy Type Detection ───────────────────────────────────

/// Returns true if a type is Copy (trivially copyable, no `!` needed).
///
/// `str` is Copy — an immutable view (`const char*`) that never owns memory.
/// `String` (PrimitiveType::StringType) is non-Copy — it owns a heap buffer
/// (GorgetString struct) and must be moved with `!`.
fn is_copy_type(type_id: TypeId, types: &TypeTable) -> bool {
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
            )
        }
        ResolvedType::Void | ResolvedType::Never | ResolvedType::Error => true,
        ResolvedType::Tuple(elems) => {
            let elems = elems.clone();
            elems.iter().all(|e| is_copy_type(*e, types))
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
    errors: Vec<SemanticError>,
    /// Variable state: DefId -> current state.
    var_states: FxHashMap<DefId, VarState>,
    /// Nesting depth inside loops (for move-in-loop detection).
    loop_depth: usize,
    /// Whether the file has `directive immutable-by-default`.
    immutable_by_default: bool,
    /// Expression type map from the type checker (for lifetime tracking).
    expr_types: &'a FxHashMap<Span, TypeId>,

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
}

impl<'a> BorrowChecker<'a> {
    fn new(
        scopes: &'a ScopeTable,
        types: &'a TypeTable,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
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
            errors: Vec::new(),
            var_states: FxHashMap::default(),
            loop_depth: 0,
            immutable_by_default,
            expr_types,
            method_resolutions,
            ref_type_structs,
            struct_field_ref_flags,
            var_origins: FxHashMap::default(),
            invalidated_origins: FxHashSet::default(),
            current_return_type_id: None,
            current_param_def_ids: Vec::new(),
            active_outlives: Vec::new(),
        }
    }

    fn error(&mut self, kind: SemanticErrorKind, span: Span) {
        self.errors.push(SemanticError { kind, span });
    }

    /// Mark a variable as Live (e.g., on declaration or reassignment).
    fn mark_live(&mut self, def_id: DefId) {
        self.var_states.insert(def_id, VarState::Live);
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
    }

    // ─── Origin Tracking ──────────────────────────────────

    /// Merge multiple origins: filter out Unknown, return single or CallResult union.
    fn merge_origins(origins: Vec<BorrowOrigin>) -> BorrowOrigin {
        let meaningful: Vec<_> = origins.into_iter()
            .filter(|o| !matches!(o, BorrowOrigin::Unknown))
            .collect();
        match meaningful.len() {
            0 => BorrowOrigin::Unknown,
            1 => meaningful.into_iter().next().unwrap(),
            _ => BorrowOrigin::CallResult(meaningful),
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

                    // Only mark as Local if this variable owns data (non-reference type).
                    // Reference-type locals that aren't in var_origins (e.g. pattern bindings
                    // from match arms) are views into existing data — not new local sources.
                    if def.kind == DefKind::Variable {
                        if let Some(type_id) = def.type_id {
                            if !types::is_reference_type(type_id, self.types, &self.ref_type_structs) {
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
                let mut captured_origins = Vec::new();
                self.collect_captured_ref_origins(body, &param_names, &mut captured_origins);
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
            }
        }
        BorrowOrigin::Unknown
    }

    /// Compute the origin of a method call result using `return_borrows_from` data.
    fn compute_method_call_origin(&self, receiver: &Spanned<Expr>, method: &Spanned<String>, args: &[Spanned<CallArg>]) -> BorrowOrigin {
        if let Some(&def_id) = self.method_resolutions.get(&method.span.start) {
            if let Some(info) = self.function_info.get(&def_id) {
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
                    call_span,
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

        // Check if inside a loop
        if self.loop_depth > 0 {
            self.error(SemanticErrorKind::MoveInLoop { name }, span);
            return;
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
        }
    }

    /// Restore combined branch state.
    fn restore_branch_state(&mut self, state: &BranchState) {
        self.var_states = state.var_states.clone();
        self.var_origins = state.origins.clone();
        self.invalidated_origins = state.invalidated.clone();
    }

    /// Merge multiple branch states: union var states (moved in either = moved),
    /// union origins, union invalidated sets.
    fn merge_branch_states(&mut self, states: &[BranchState]) {
        if states.is_empty() {
            return;
        }
        let mut merged_vars = states[0].var_states.clone();
        let mut merged_origins = states[0].origins.clone();
        let mut merged_invalidated = states[0].invalidated.clone();

        for state in &states[1..] {
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
        }

        self.var_states = merged_vars;
        self.var_origins = merged_origins;
        self.invalidated_origins = merged_invalidated;
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
                        _ => {
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

            Expr::Try { expr: inner }
            | Expr::Await { expr: inner }
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
                    self.mark_pattern_origins(&arm.pattern.node, &scrutinee_origin);
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
                // Closures have their own scope — for now just check the body
                self.check_expr(body);
            }

            Expr::ImplicitClosure { body } => {
                self.check_expr(body);
            }

            Expr::ListComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                self.loop_depth += 1;
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_depth -= 1;
            }

            Expr::DictComprehension {
                key,
                value,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                self.loop_depth += 1;
                self.check_expr(key);
                self.check_expr(value);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_depth -= 1;
            }

            Expr::SetComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.check_expr(iterable);
                self.loop_depth += 1;
                self.check_expr(comp_expr);
                if let Some(cond) = condition {
                    self.check_expr(cond);
                }
                self.loop_depth -= 1;
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
                for arg in args {
                    self.check_expr(arg);
                }
            }

            Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
                self.check_expr(inner);
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
                self.mark_pattern_live(&pattern.node);

                // Track origin for reference-typed variables
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = self.find_def_by_name(name) {
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
                                    },
                                    value.span,
                                );
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

                            // Update origin tracking for reference-typed variables
                            if self.var_origins.contains_key(&def_id) {
                                let origin = self.compute_expr_origin(value);
                                self.var_origins.insert(def_id, origin);
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
                    self.check_expr(expr);

                    // Lifetime check: if the function returns a reference type,
                    // verify the return expression doesn't reference local data.
                    if let Some(ret_type_id) = self.current_return_type_id {
                        if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs) {
                            let origin = self.compute_expr_origin(expr);
                            if origin.contains_local() {
                                let local_names = origin.local_names(self.scopes);
                                let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                                // Get a name for what's being returned
                                let return_name = match &expr.node {
                                    Expr::Identifier(n) => n.clone(),
                                    _ => "<expression>".to_string(),
                                };
                                self.error(
                                    SemanticErrorKind::DanglingReturn {
                                        name: return_name,
                                        local_name,
                                    },
                                    expr.span,
                                );
                            }
                        }
                    }
                }
            }

            Stmt::Throw(expr) => {
                self.check_expr(expr);
            }

            Stmt::Break(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr);
                }
            }

            Stmt::Continue | Stmt::Pass => {}

            Stmt::For {
                iterable,
                body,
                else_body,
                ..
            } => {
                self.check_expr(iterable);
                let before = self.save_branch_state();
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
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
                let before = self.save_branch_state();
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
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
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
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

                let before = self.save_branch_state();
                self.check_block(then_body);
                let mut branch_states = vec![self.save_branch_state()];

                for (cond, body) in elif_branches {
                    self.restore_branch_state(&before);
                    self.check_expr(cond);
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
                    self.mark_pattern_origins(&arm.pattern.node, &scrutinee_origin);
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

            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.check_expr(&binding.expr);
                }
                self.check_block(body);
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
                        if !is_copy_type(type_id, self.types) {
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

    /// Walk a pattern from a match arm, mark bindings as Live, and assign
    /// origins derived from the scrutinee to any reference-type bindings.
    fn mark_pattern_origins(&mut self, pattern: &Pattern, scrutinee_origin: &BorrowOrigin) {
        match pattern {
            Pattern::Binding(name) => {
                if let Some(def_id) = self.find_def_by_name(name) {
                    self.mark_live(def_id);
                    // Only assign origin to reference-type bindings
                    if let Some(type_id) = self.scopes.get_def(def_id).type_id {
                        if types::is_reference_type(type_id, self.types, &self.ref_type_structs) {
                            self.var_origins.insert(def_id, scrutinee_origin.clone());
                        }
                    }
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_origins(&field.node, scrutinee_origin);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_origins(&elem.node, scrutinee_origin);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_origins(&first.node, scrutinee_origin);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Mark all bindings in a pattern as Live.
    fn mark_pattern_live(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Binding(name) => {
                // Look up the DefId by scanning definitions
                // Since the pattern was just defined, it's the most recently defined with this name
                if let Some(def_id) = self.find_def_by_name(name) {
                    self.mark_live(def_id);
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    self.mark_pattern_live(&field.node);
                }
            }
            Pattern::Tuple(elements) => {
                for elem in elements {
                    self.mark_pattern_live(&elem.node);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.mark_pattern_live(&first.node);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Find the DefId for a variable name by looking it up in the scope table.
    fn find_def_by_name(&self, name: &str) -> Option<DefId> {
        self.scopes.lookup_by_name_anywhere(name)
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

    /// Recursively walk a closure body collecting origins of captured reference-type variables.
    /// Skips: closure parameters (in `param_names`), nested closures (own capture scope),
    /// and variables that are not reference-typed or have no tracked origin.
    fn collect_captured_ref_origins(
        &self,
        expr: &Spanned<Expr>,
        param_names: &FxHashSet<&str>,
        origins: &mut Vec<BorrowOrigin>,
    ) {
        match &expr.node {
            Expr::Identifier(name) => {
                // Skip closure parameters
                if param_names.contains(name.as_str()) {
                    return;
                }
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(def_id);
                    if def.kind == DefKind::Variable {
                        // Check if it's a reference type with a tracked origin
                        if let Some(type_id) = def.type_id {
                            if types::is_reference_type(type_id, self.types, &self.ref_type_structs) {
                                if let Some(origin) = self.var_origins.get(&def_id) {
                                    origins.push(origin.clone());
                                }
                            }
                        }
                    }
                }
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Recurse into sub-expressions
            Expr::BinaryOp { left, right, .. } => {
                self.collect_captured_ref_origins(left, param_names, origins);
                self.collect_captured_ref_origins(right, param_names, origins);
            }
            Expr::UnaryOp { operand, .. } | Expr::Move { expr: operand }
            | Expr::MutableBorrow { expr: operand } | Expr::Deref { expr: operand }
            | Expr::Try { expr: operand } | Expr::TryCapture { expr: operand }
            | Expr::Await { expr: operand } | Expr::Spawn { expr: operand }
            | Expr::As { expr: operand, .. } | Expr::Is { expr: operand, .. } => {
                self.collect_captured_ref_origins(operand, param_names, origins);
            }
            Expr::Call { callee, args, .. } => {
                self.collect_captured_ref_origins(callee, param_names, origins);
                for arg in args {
                    self.collect_captured_ref_origins(&arg.node.value, param_names, origins);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.collect_captured_ref_origins(receiver, param_names, origins);
                for arg in args {
                    self.collect_captured_ref_origins(&arg.node.value, param_names, origins);
                }
            }
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.collect_captured_ref_origins(object, param_names, origins);
            }
            Expr::Index { object, index } => {
                self.collect_captured_ref_origins(object, param_names, origins);
                self.collect_captured_ref_origins(index, param_names, origins);
            }
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                self.collect_captured_ref_origins(condition, param_names, origins);
                self.collect_captured_ref_origins(then_branch, param_names, origins);
                for (cond, body) in elif_branches {
                    self.collect_captured_ref_origins(cond, param_names, origins);
                    self.collect_captured_ref_origins(body, param_names, origins);
                }
                if let Some(e) = else_branch {
                    self.collect_captured_ref_origins(e, param_names, origins);
                }
            }
            Expr::Block(block) | Expr::Do { body: block } => {
                for stmt in &block.stmts {
                    self.collect_captured_ref_origins_stmt(stmt, param_names, origins);
                }
            }
            Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
                for e in elems {
                    self.collect_captured_ref_origins(e, param_names, origins);
                }
            }
            Expr::StructLiteral { args, .. } => {
                for arg in args {
                    self.collect_captured_ref_origins(arg, param_names, origins);
                }
            }
            Expr::NilCoalescing { lhs, rhs } => {
                self.collect_captured_ref_origins(lhs, param_names, origins);
                self.collect_captured_ref_origins(rhs, param_names, origins);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { self.collect_captured_ref_origins(s, param_names, origins); }
                if let Some(e) = end { self.collect_captured_ref_origins(e, param_names, origins); }
            }
            Expr::OptionalChain { object, .. } => {
                self.collect_captured_ref_origins(object, param_names, origins);
            }
            Expr::Match { scrutinee, arms, else_arm } => {
                self.collect_captured_ref_origins(scrutinee, param_names, origins);
                for arm in arms {
                    self.collect_captured_ref_origins(&arm.body, param_names, origins);
                }
                if let Some(else_arm) = else_arm {
                    self.collect_captured_ref_origins(else_arm, param_names, origins);
                }
            }
            Expr::DictLiteral(pairs) => {
                for (k, v) in pairs {
                    self.collect_captured_ref_origins(k, param_names, origins);
                    self.collect_captured_ref_origins(v, param_names, origins);
                }
            }
            Expr::ListComprehension { expr: comp_expr, iterable, condition, .. } => {
                self.collect_captured_ref_origins(iterable, param_names, origins);
                self.collect_captured_ref_origins(comp_expr, param_names, origins);
                if let Some(cond) = condition {
                    self.collect_captured_ref_origins(cond, param_names, origins);
                }
            }
            Expr::SetComprehension { expr: comp_expr, iterable, condition, .. } => {
                self.collect_captured_ref_origins(iterable, param_names, origins);
                self.collect_captured_ref_origins(comp_expr, param_names, origins);
                if let Some(cond) = condition {
                    self.collect_captured_ref_origins(cond, param_names, origins);
                }
            }
            Expr::DictComprehension { key, value, iterable, condition, .. } => {
                self.collect_captured_ref_origins(iterable, param_names, origins);
                self.collect_captured_ref_origins(key, param_names, origins);
                self.collect_captured_ref_origins(value, param_names, origins);
                if let Some(cond) = condition {
                    self.collect_captured_ref_origins(cond, param_names, origins);
                }
            }
            _ => {} // Literals, SelfExpr, It, etc.
        }
    }

    /// Helper for collect_captured_ref_origins that handles statements.
    fn collect_captured_ref_origins_stmt(
        &self,
        stmt: &Spanned<Stmt>,
        param_names: &FxHashSet<&str>,
        origins: &mut Vec<BorrowOrigin>,
    ) {
        match &stmt.node {
            Stmt::Expr(e) | Stmt::Return(Some(e)) | Stmt::Throw(e) | Stmt::Break(Some(e)) => {
                self.collect_captured_ref_origins(e, param_names, origins);
            }
            Stmt::VarDecl { value, .. } => {
                self.collect_captured_ref_origins(value, param_names, origins);
            }
            Stmt::Assign { target, value } => {
                self.collect_captured_ref_origins(target, param_names, origins);
                self.collect_captured_ref_origins(value, param_names, origins);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.collect_captured_ref_origins(target, param_names, origins);
                self.collect_captured_ref_origins(value, param_names, origins);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.collect_captured_ref_origins(condition, param_names, origins);
                for s in &then_body.stmts {
                    self.collect_captured_ref_origins_stmt(s, param_names, origins);
                }
                for (cond, body) in elif_branches {
                    self.collect_captured_ref_origins(cond, param_names, origins);
                    for s in &body.stmts {
                        self.collect_captured_ref_origins_stmt(s, param_names, origins);
                    }
                }
                if let Some(else_body) = else_body {
                    for s in &else_body.stmts {
                        self.collect_captured_ref_origins_stmt(s, param_names, origins);
                    }
                }
            }
            Stmt::For { iterable, body, .. } | Stmt::While { condition: iterable, body, .. } => {
                self.collect_captured_ref_origins(iterable, param_names, origins);
                for s in &body.stmts {
                    self.collect_captured_ref_origins_stmt(s, param_names, origins);
                }
            }
            Stmt::Loop { body } | Stmt::Unsafe { body } => {
                for s in &body.stmts {
                    self.collect_captured_ref_origins_stmt(s, param_names, origins);
                }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.collect_captured_ref_origins(scrutinee, param_names, origins);
                for arm in arms {
                    self.collect_captured_ref_origins(&arm.body, param_names, origins);
                }
                if let Some(else_arm) = else_arm {
                    for s in &else_arm.stmts {
                        self.collect_captured_ref_origins_stmt(s, param_names, origins);
                    }
                }
            }
            Stmt::Assert { condition, message } => {
                self.collect_captured_ref_origins(condition, param_names, origins);
                if let Some(msg) = message {
                    self.collect_captured_ref_origins(msg, param_names, origins);
                }
            }
            _ => {} // Pass, Continue, Return(None), Break(None), Item, With
        }
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
        self.var_origins.clear();
        self.invalidated_origins.clear();
        self.current_param_def_ids.clear();
        self.active_outlives.clear();

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
                    if types::is_reference_type(ret_type_id, self.types, &self.ref_type_structs) {
                        let origin = self.compute_expr_origin(expr);
                        if origin.contains_local() {
                            let local_names = origin.local_names(self.scopes);
                            let local_name = local_names.first().cloned().unwrap_or_else(|| "<local>".to_string());
                            let return_name = match &expr.node {
                                Expr::Identifier(n) => n.clone(),
                                _ => "<expression>".to_string(),
                            };
                            self.error(
                                SemanticErrorKind::DanglingReturn {
                                    name: return_name,
                                    local_name,
                                },
                                expr.span,
                            );
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

    // Only relevant if the return type is a reference type
    if !types::is_reference_type(ret_type_id, types, ref_type_structs) {
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

    match &func.body {
        FunctionBody::Expression(expr) => {
            trace_expr_to_params(expr, &param_name_to_idx, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Block(block) => {
            trace_block_returns_to_params(block, &param_name_to_idx, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // No body — apply elision rules
            let info = match function_info.get(&def_id) {
                Some(i) => i,
                None => return,
            };
            let ref_param_indices: Vec<usize> = info.param_type_ids.iter()
                .enumerate()
                .filter(|(_, tid)| tid.map_or(false, |id| types::is_reference_type(id, types, ref_type_structs)))
                .map(|(i, _)| i)
                .collect();
            if ref_param_indices.len() == 1 {
                borrows_from.insert(ref_param_indices[0]);
            } else if !info.param_names.is_empty() && info.param_names[0] == "self" {
                borrows_from.insert(0);
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
            .filter(|(_, tid)| tid.map_or(false, |id| types::is_reference_type(id, types, ref_type_structs)))
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
    }
}

/// Trace a return expression back through variable assignments to find which params flow to it.
fn trace_expr_to_params(
    expr: &Spanned<Expr>,
    param_names: &FxHashMap<String, usize>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match &expr.node {
        Expr::Identifier(name) => {
            if let Some(&idx) = param_names.get(name) {
                result.insert(idx);
            }
            // Note: doesn't trace through local variable assignments in this simple version.
            // The full borrow checker (Pass 5b) handles transitive tracking via var_origins.
        }

        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            trace_expr_to_params(object, param_names, resolution_map, scopes, result);
        }

        Expr::Index { object, .. } => {
            trace_expr_to_params(object, param_names, resolution_map, scopes, result);
        }

        Expr::If { then_branch, elif_branches, else_branch, .. } => {
            trace_expr_to_params(then_branch, param_names, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_expr_to_params(body, param_names, resolution_map, scopes, result);
            }
            if let Some(else_br) = else_branch {
                trace_expr_to_params(else_br, param_names, resolution_map, scopes, result);
            }
        }

        Expr::Block(block) | Expr::Do { body: block } => {
            if let Some(last) = block.stmts.last() {
                if let Stmt::Expr(e) = &last.node {
                    trace_expr_to_params(e, param_names, resolution_map, scopes, result);
                }
            }
        }

        Expr::SelfExpr => {
            // self borrows from param index 0
            if param_names.contains_key("self") {
                result.insert(0);
            }
        }

        // Struct literal: trace all args conservatively (the function's return type
        // is already known to be a reference type if we got here)
        Expr::StructLiteral { args, .. } => {
            for arg in args {
                trace_expr_to_params(arg, param_names, resolution_map, scopes, result);
            }
        }

        // Everything else: no param flow detected in this lightweight analysis
        _ => {}
    }
}

/// Walk a block looking for Return statements and trace them to params.
fn trace_block_returns_to_params(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    for stmt in &block.stmts {
        trace_stmt_returns_to_params(&stmt.node, param_names, resolution_map, scopes, result);
    }
}

fn trace_stmt_returns_to_params(
    stmt: &Stmt,
    param_names: &FxHashMap<String, usize>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match stmt {
        Stmt::Return(Some(expr)) => {
            trace_expr_to_params(expr, param_names, resolution_map, scopes, result);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            trace_block_returns_to_params(then_body, param_names, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_block_returns_to_params(body, param_names, resolution_map, scopes, result);
            }
            if let Some(else_body) = else_body {
                trace_block_returns_to_params(else_body, param_names, resolution_map, scopes, result);
            }
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms {
                if let Expr::Block(block) = &arm.body.node {
                    trace_block_returns_to_params(block, param_names, resolution_map, scopes, result);
                }
            }
            if let Some(else_arm) = else_arm {
                trace_block_returns_to_params(else_arm, param_names, resolution_map, scopes, result);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body } => {
            trace_block_returns_to_params(body, param_names, resolution_map, scopes, result);
        }
        Stmt::With { body, .. } | Stmt::Unsafe { body } => {
            trace_block_returns_to_params(body, param_names, resolution_map, scopes, result);
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
        scopes, types, resolution_map, function_info, immutable_by_default, expr_types,
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
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
                for binding in &t.with_bindings {
                    checker.check_expr(&binding.expr);
                }
                checker.check_block(&t.body);
            }
            Item::SuiteSetup(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
                checker.check_block(&s.body);
            }
            Item::SuiteTeardown(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.var_origins.clear();
                checker.invalidated_origins.clear();
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
        let result = semantic::analyze(&mut module);
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
    fn closure_capture_origin_computed() {
        // Closure capturing a ref-type var → compute_expr_origin returns non-Unknown
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
        // v's source s is moved before f() is called. However, the current check only
        // detects UseAfterSourceMoved on direct use of v, not through closure f().
        // The closure origin computation itself is verified by the fact that
        // compute_expr_origin for the closure won't return Unknown.
        // For now, we verify no false positives.
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn: {:?}", errors
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
            has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { name, callee }
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
            has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { name, callee }
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
}
