use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::resolve::{FunctionInfo, ResolutionMap};
use super::scope::{DefKind, ScopeTable};
use super::types::{ResolvedType, TypeTable};

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

// ─── Copy Type Detection ───────────────────────────────────

/// Returns true if a type is Copy (trivially copyable, no `!` needed).
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
}

impl<'a> BorrowChecker<'a> {
    fn new(
        scopes: &'a ScopeTable,
        types: &'a TypeTable,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
        immutable_by_default: bool,
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
    }

    fn save_state(&self) -> StateSnapshot {
        self.var_states.clone()
    }

    fn restore_state(&mut self, state: StateSnapshot) {
        self.var_states = state;
    }

    /// Merge two branch states conservatively: if moved in either, treat as moved.
    fn merge_states(&mut self, a: &StateSnapshot, b: &StateSnapshot) {
        let mut merged = a.clone();
        for (def_id, b_state) in b {
            match (merged.get(def_id), b_state) {
                // If moved in either branch, it's moved (use the first move span encountered)
                (Some(VarState::Moved { .. }), _) => {} // already moved in a
                (_, VarState::Moved { moved_at }) => {
                    merged.insert(*def_id, VarState::Moved { moved_at: *moved_at });
                }
                _ => {} // both Live, stays Live
            }
        }
        self.var_states = merged;
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
                self.check_expr(callee);
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

                let before = self.save_state();
                self.check_expr(then_branch);
                let after_then = self.save_state();

                // Start each elif from the pre-if state
                let mut branch_states = vec![after_then];
                for (cond, body) in elif_branches {
                    self.restore_state(before.clone());
                    self.check_expr(cond);
                    self.check_expr(body);
                    branch_states.push(self.save_state());
                }

                if let Some(else_br) = else_branch {
                    self.restore_state(before.clone());
                    self.check_expr(else_br);
                    branch_states.push(self.save_state());
                } else {
                    // No else branch — the "before" state is a possible outcome
                    branch_states.push(before.clone());
                }

                // Merge all branch states
                let mut result = branch_states[0].clone();
                for state in &branch_states[1..] {
                    self.restore_state(result.clone());
                    self.merge_states(&result, state);
                    result = self.save_state();
                }
                self.restore_state(result);
            }

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                let before = self.save_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_state(before.clone());
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    branch_states.push(self.save_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_state(before.clone());
                    self.check_expr(else_arm);
                    branch_states.push(self.save_state());
                } else {
                    branch_states.push(before.clone());
                }

                let mut result = branch_states[0].clone();
                for state in &branch_states[1..] {
                    self.restore_state(result.clone());
                    self.merge_states(&result, state);
                    result = self.save_state();
                }
                self.restore_state(result);
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
                pattern, value, ..
            } => {
                // Check the value expression
                self.check_expr(value);

                // Check: if value is a bare identifier of non-Copy type, needs `!`
                self.check_value_needs_move(value);

                // Mark new bindings as Live
                self.mark_pattern_live(&pattern.node);
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
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                }
            }

            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                self.check_expr(condition);
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                }
            }

            Stmt::Loop { body } => {
                self.loop_depth += 1;
                self.check_block(body);
                self.loop_depth -= 1;
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                self.check_expr(condition);

                let before = self.save_state();
                self.check_block(then_body);
                let after_then = self.save_state();

                let mut branch_states = vec![after_then];
                for (cond, body) in elif_branches {
                    self.restore_state(before.clone());
                    self.check_expr(cond);
                    self.check_block(body);
                    branch_states.push(self.save_state());
                }

                if let Some(else_body) = else_body {
                    self.restore_state(before.clone());
                    self.check_block(else_body);
                    branch_states.push(self.save_state());
                } else {
                    branch_states.push(before.clone());
                }

                let mut result = branch_states[0].clone();
                for state in &branch_states[1..] {
                    self.restore_state(result.clone());
                    self.merge_states(&result, state);
                    result = self.save_state();
                }
                self.restore_state(result);
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.check_expr(scrutinee);
                let before = self.save_state();
                let mut branch_states = Vec::new();

                for arm in arms {
                    self.restore_state(before.clone());
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    branch_states.push(self.save_state());
                }

                if let Some(else_arm) = else_arm {
                    self.restore_state(before.clone());
                    self.check_block(else_arm);
                    branch_states.push(self.save_state());
                } else {
                    branch_states.push(before.clone());
                }

                let mut result = branch_states[0].clone();
                for state in &branch_states[1..] {
                    self.restore_state(result.clone());
                    self.merge_states(&result, state);
                    result = self.save_state();
                }
                self.restore_state(result);
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
                // Only check local variables, not functions/types/imports
                if def.kind == DefKind::Variable {
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
                    Ownership::Move => "move (! or moving)",
                };
                let found_str = match found {
                    Ownership::Borrow => "borrow (bare)",
                    Ownership::MutableBorrow => "mutable borrow (& or mutable)",
                    Ownership::Move => "move (! or moving)",
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

    fn check_function(&mut self, func: &FunctionDef) {
        // Reset state for each function
        self.var_states.clear();
        self.loop_depth = 0;

        match &func.body {
            FunctionBody::Block(block) => {
                self.check_block(block);
            }
            FunctionBody::Expression(expr) => {
                self.check_expr(expr);
            }
            FunctionBody::Declaration => {}
        }
    }
}

/// Run borrow checking on the entire module.
pub fn check_module(
    module: &Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    immutable_by_default: bool,
    errors: &mut Vec<SemanticError>,
) {
    let mut checker = BorrowChecker::new(scopes, types, resolution_map, function_info, immutable_by_default);

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
                for binding in &t.with_bindings {
                    checker.check_expr(&binding.expr);
                }
                checker.check_block(&t.body);
            }
            Item::SuiteSetup(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
                checker.check_block(&s.body);
            }
            Item::SuiteTeardown(s) => {
                checker.var_states.clear();
                checker.loop_depth = 0;
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
}
