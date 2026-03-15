use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, ScopeId, TypeId};
use super::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use super::scope::{DefKind, ScopeKind, ScopeTable};
use super::traits::TraitRegistry;
use super::types::{self, ResolvedType, TypeTable};

/// Return the valid (min, max) range for an integer primitive type.
fn int_range(prim: &PrimitiveType) -> Option<(i128, i128)> {
    match prim {
        PrimitiveType::Int8 => Some((-128, 127)),
        PrimitiveType::Int16 => Some((-32768, 32767)),
        PrimitiveType::Int32 => Some((-2_147_483_648, 2_147_483_647)),
        PrimitiveType::Int | PrimitiveType::Int64 => Some((-9_223_372_036_854_775_808, 9_223_372_036_854_775_807)),
        PrimitiveType::Uint8 => Some((0, 255)),
        PrimitiveType::Uint16 => Some((0, 65535)),
        PrimitiveType::Uint32 => Some((0, 4_294_967_295)),
        PrimitiveType::Uint | PrimitiveType::Uint64 => Some((0, 18_446_744_073_709_551_615)),
        _ => None, // float, bool, char, string — not integer types
    }
}

/// Returns true if this primitive is any integer type (signed or unsigned).
fn is_integer_type(prim: &PrimitiveType) -> bool {
    int_range(prim).is_some()
}

// ══════════════════════════════════════════════════════════════
// Closure kind classification
// ══════════════════════════════════════════════════════════════

use super::types::ClosureKind;
use crate::parser::visitor::{self, ExprVisitor};
use std::collections::HashSet;

/// Classify a closure's kind based on its `is_move` flag and whether the body
/// mutates captured (non-local) variables.
pub fn classify_closure_kind(
    is_move: bool,
    params: &[Spanned<ClosureParam>],
    body: &Spanned<Expr>,
) -> ClosureKind {
    if is_move {
        return ClosureKind::ConsumeCallable;
    }
    let param_names: HashSet<String> = params.iter()
        .map(|p| p.node.name.node.clone())
        .collect();
    let mut detector = CapturedMutationDetector { locals: param_names, found: false };
    detector.visit_expr(body);
    if detector.found {
        ClosureKind::MutCallable
    } else {
        ClosureKind::Callable
    }
}

/// Extract the root identifier from a (possibly nested) lvalue expression.
fn root_identifier(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Identifier(name) => Some(name.as_str()),
        Expr::FieldAccess { object, .. } => root_identifier(&object.node),
        Expr::Index { object, .. } => root_identifier(&object.node),
        Expr::Deref { expr, .. } => root_identifier(&expr.node),
        _ => None,
    }
}

/// Returns true if `target` assigns to a non-local (captured) variable.
fn is_capture_mutation(target: &Expr, locals: &HashSet<String>) -> bool {
    if let Some(name) = root_identifier(target) {
        !locals.contains(name)
    } else {
        false
    }
}

/// Collect binding names from a pattern into the locals set.
fn collect_pattern_bindings(pattern: &Pattern, locals: &mut HashSet<String>) {
    match pattern {
        Pattern::Binding(name) => { locals.insert(name.clone()); }
        Pattern::Tuple(pats) => {
            for p in pats { collect_pattern_bindings(&p.node, locals); }
        }
        Pattern::Constructor { fields, .. } => {
            for f in fields { collect_pattern_bindings(&f.node, locals); }
        }
        Pattern::Or(pats) => {
            for p in pats { collect_pattern_bindings(&p.node, locals); }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        Pattern::DotShorthand { fields, .. } => {
            for f in fields { collect_pattern_bindings(&f.node, locals); }
        }
    }
}

/// Detects mutations to captured (non-local) variables in a closure body.
/// Does NOT recurse into nested closures (they have their own capture scope).
/// Uses `found` flag for short-circuit termination.
struct CapturedMutationDetector {
    locals: HashSet<String>,
    found: bool,
}

impl ExprVisitor for CapturedMutationDetector {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        if self.found { return; }
        match &expr.node {
            // Nested closures have their own capture scope — skip
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            _ => visitor::walk_expr(self, expr),
        }
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
        if self.found { return; }
        match &stmt.node {
            Stmt::Assign { target, value } => {
                if is_capture_mutation(&target.node, &self.locals) {
                    self.found = true;
                    return;
                }
                self.visit_expr(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                if is_capture_mutation(&target.node, &self.locals) {
                    self.found = true;
                    return;
                }
                self.visit_expr(value);
            }
            Stmt::VarDecl { pattern, value, .. } => {
                collect_pattern_bindings(&pattern.node, &mut self.locals);
                self.visit_expr(value);
            }
            Stmt::For { pattern, .. } => {
                collect_pattern_bindings(&pattern.node, &mut self.locals);
                visitor::walk_stmt(self, stmt);
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.visit_expr(scrutinee);
                for arm in arms.iter().filter_map(|i| i.arm()) {
                    if self.found { return; }
                    let saved = self.locals.clone();
                    collect_pattern_bindings(&arm.pattern.node, &mut self.locals);
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr(&arm.body);
                    self.locals = saved;
                }
                if let Some(eb) = else_arm {
                    self.visit_block(eb);
                }
            }
            Stmt::Select { arms, else_arm } => {
                for arm in arms {
                    if self.found { return; }
                    match &arm.op {
                        SelectOp::Recv { channel, name, .. } => {
                            self.visit_expr(channel);
                            let saved = self.locals.clone();
                            self.locals.insert(name.node.clone());
                            self.visit_block(&arm.body);
                            self.locals = saved;
                        }
                        SelectOp::Send { channel, value } => {
                            self.visit_expr(channel);
                            self.visit_expr(value);
                            self.visit_block(&arm.body);
                        }
                    }
                }
                if let Some(eb) = else_arm {
                    self.visit_block(eb);
                }
            }
            Stmt::With { bindings, body } => {
                for b in bindings {
                    self.locals.insert(b.name.node.clone());
                }
                for b in bindings {
                    if self.found { return; }
                    self.visit_expr(&b.expr);
                }
                self.visit_block(body);
            }
            _ => visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_block(&mut self, block: &Block) {
        let saved = self.locals.clone();
        for stmt in &block.stmts {
            if self.found { break; }
            self.visit_stmt(stmt);
        }
        self.locals = saved;
    }
}

/// Type checker with bidirectional inference.
struct TypeChecker<'a> {
    scopes: &'a mut ScopeTable,
    types: &'a mut TypeTable,
    traits: &'a TraitRegistry,
    resolution_map: &'a ResolutionMap,
    function_info: &'a FxHashMap<DefId, FunctionInfo>,
    enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
    struct_fields: &'a FxHashMap<DefId, StructFieldInfo>,
    errors: Vec<SemanticError>,
    /// Substitution map: type variable ID -> resolved type ID.
    substitutions: FxHashMap<u32, TypeId>,
    next_type_var: u32,
    /// The return type of the current function being checked.
    current_return_type: Option<TypeId>,
    /// Whether the current function has `throws`.
    current_function_throws: bool,
    /// Whether the current function is `async`.
    current_function_is_async: bool,
    /// Type variable for implicit `it` parameter inside ImplicitClosure.
    implicit_it_type: Option<TypeId>,
    /// Map from expression span to its inferred TypeId (used by codegen for Result-based `?`).
    expr_types: FxHashMap<Span, TypeId>,
    /// Map from method call span start → DefId of resolved method (for borrow checker).
    method_resolutions: FxHashMap<usize, DefId>,
    /// The self type of the current equip block (if any).
    current_self_type: Option<TypeId>,
    /// Declared type hint for integer literal coercion (e.g., uint8 x = 5).
    decl_type_hint: Option<TypeId>,
    /// Maps (function_name, span_start) → body scope id (for scope-aware lookups).
    function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
    /// Current function's body scope (for scope-aware variable lookup).
    current_fn_scope: Option<ScopeId>,
    /// Current function's trait bounds: (param_name, [trait_name, ...]).
    /// Used for trait bound propagation: when a generic param `T` with bound `Numeric`
    /// is passed to a callee requiring `Numeric`, the bound is satisfied transitively.
    current_trait_bounds: Vec<(String, Vec<String>)>,
    /// Nesting depth inside loops (for break/continue validation).
    loop_depth: usize,
}

impl<'a> TypeChecker<'a> {
    fn new(
        scopes: &'a mut ScopeTable,
        types: &'a mut TypeTable,
        traits: &'a TraitRegistry,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
        enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
        struct_fields: &'a FxHashMap<DefId, StructFieldInfo>,
        function_body_scopes: &'a FxHashMap<(String, usize), ScopeId>,
    ) -> Self {
        Self {
            scopes,
            types,
            traits,
            resolution_map,
            function_info,
            enum_variants,
            struct_fields,
            errors: Vec::new(),
            substitutions: FxHashMap::default(),
            next_type_var: 0,
            current_return_type: None,
            current_function_throws: false,
            current_function_is_async: false,
            implicit_it_type: None,
            expr_types: FxHashMap::default(),
            method_resolutions: FxHashMap::default(),
            current_self_type: None,
            decl_type_hint: None,
            function_body_scopes,
            current_fn_scope: None,
            current_trait_bounds: Vec::new(),
            loop_depth: 0,
        }
    }

    fn fresh_type_var(&mut self) -> TypeId {
        let var_id = self.next_type_var;
        self.next_type_var += 1;
        self.types.fresh_var(var_id)
    }

    fn error(&mut self, kind: SemanticErrorKind, span: Span) {
        self.errors.push(SemanticError { kind, span });
    }

    /// Look up a definition in the resolution map, guarding against cross-module
    /// span collisions by verifying the resolved name matches the expected name.
    fn resolve_name(&self, span_start: usize, expected_name: &str) -> Option<DefId> {
        self.resolution_map
            .get(&span_start)
            .copied()
            .filter(|&def_id| self.scopes.get_def(def_id).name == expected_name)
            .or_else(|| {
                if let Some(scope_id) = self.current_fn_scope {
                    self.scopes.lookup_within_function(scope_id, expected_name)
                } else {
                    self.scopes.lookup(expected_name)
                }
            })
    }

    /// Resolve a type variable to its substitution, following chains.
    fn resolve_type(&self, id: TypeId) -> TypeId {
        match self.types.get(id) {
            ResolvedType::Var(var_id) => {
                if let Some(&sub) = self.substitutions.get(var_id) {
                    self.resolve_type(sub)
                } else {
                    id
                }
            }
            _ => id,
        }
    }

    /// Deeply resolve a type: follow Var chains and also resolve inner types
    /// of composite types like Function. Returns a new TypeId if any inner
    /// types changed, or the original if already fully resolved.
    fn resolve_type_deep(&mut self, id: TypeId) -> TypeId {
        let base = self.resolve_type(id);
        match self.types.get(base).clone() {
            ResolvedType::Function { params, return_type, param_ownerships } => {
                let new_params: Vec<TypeId> = params.iter()
                    .map(|&p| self.resolve_type_deep(p))
                    .collect();
                let new_ret = self.resolve_type_deep(return_type);
                if new_params == params && new_ret == return_type {
                    base
                } else {
                    self.types.insert(ResolvedType::Function {
                        params: new_params,
                        return_type: new_ret,
                        param_ownerships: param_ownerships.clone(),
                    })
                }
            }
            ResolvedType::CallableTrait(inner) => {
                let new_inner = self.resolve_type_deep(inner);
                if new_inner == inner { base } else { self.types.insert(ResolvedType::CallableTrait(new_inner)) }
            }
            ResolvedType::MutCallableTrait(inner) => {
                let new_inner = self.resolve_type_deep(inner);
                if new_inner == inner { base } else { self.types.insert(ResolvedType::MutCallableTrait(new_inner)) }
            }
            ResolvedType::ConsumeCallableTrait(inner) => {
                let new_inner = self.resolve_type_deep(inner);
                if new_inner == inner { base } else { self.types.insert(ResolvedType::ConsumeCallableTrait(new_inner)) }
            }
            ResolvedType::BoxedCallable { kind, inner } => {
                let new_inner = self.resolve_type_deep(inner);
                if new_inner == inner { base } else { self.types.insert(ResolvedType::BoxedCallable { kind, inner: new_inner }) }
            }
            _ => base,
        }
    }

    /// Return a human-readable name for a resolved type.
    /// Uses the definition name for `Defined`/`Generic` types instead of the
    /// unhelpful `"<defined>"` from `TypeTable::display`.
    fn describe_resolved_type(&self, type_id: TypeId) -> String {
        match self.types.get(type_id) {
            ResolvedType::Defined(def_id) => self.scopes.get_def(*def_id).name.clone(),
            ResolvedType::Generic(def_id, args) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                if args.is_empty() {
                    name
                } else {
                    let arg_strs: Vec<_> =
                        args.iter().map(|a| self.describe_resolved_type(*a)).collect();
                    format!("{}[{}]", name, arg_strs.join(", "))
                }
            }
            ResolvedType::TraitObject(def_id) => {
                format!("trait {}", self.scopes.get_def(*def_id).name)
            }
            _ => self.types.display(type_id),
        }
    }

    /// Unify two types, binding type variables as needed.
    fn unify(&mut self, a: TypeId, b: TypeId, span: Span) -> TypeId {
        let a = self.resolve_type(a);
        let b = self.resolve_type(b);

        if a == b {
            return a;
        }

        let error_id = self.types.error_id;
        let never_id = self.types.never_id;

        // Error type unifies with anything
        if a == error_id || b == error_id {
            return error_id;
        }

        // Never type unifies with anything (diverging expressions)
        if a == never_id {
            return b;
        }
        if b == never_id {
            return a;
        }

        let a_ty = self.types.get(a).clone();
        let b_ty = self.types.get(b).clone();

        match (&a_ty, &b_ty) {
            (ResolvedType::Var(var_id), _) => {
                self.substitutions.insert(*var_id, b);
                b
            }
            (_, ResolvedType::Var(var_id)) => {
                self.substitutions.insert(*var_id, a);
                a
            }
            // Structural unification for compound types with fresh TypeIds
            (
                ResolvedType::Generic(def_a, args_a),
                ResolvedType::Generic(def_b, args_b),
            ) if def_a == def_b && args_a.len() == args_b.len() => {
                let pairs: Vec<_> = args_a.iter().copied().zip(args_b.iter().copied()).collect();
                for (arg_a, arg_b) in pairs {
                    self.unify(arg_a, arg_b, span);
                }
                a
            }
            (ResolvedType::Tuple(a_elems), ResolvedType::Tuple(b_elems))
                if a_elems.len() == b_elems.len() =>
            {
                let pairs: Vec<_> =
                    a_elems.iter().copied().zip(b_elems.iter().copied()).collect();
                for (ea, eb) in pairs {
                    self.unify(ea, eb, span);
                }
                a
            }
            (ResolvedType::Array(a_elem, a_size), ResolvedType::Array(b_elem, b_size))
                if a_size == b_size =>
            {
                self.unify(*a_elem, *b_elem, span);
                a
            }
            (ResolvedType::Slice(a_elem), ResolvedType::Slice(b_elem)) => {
                self.unify(*a_elem, *b_elem, span);
                a
            }
            (
                ResolvedType::Function {
                    params: a_params,
                    return_type: a_ret,
                    ..
                },
                ResolvedType::Function {
                    params: b_params,
                    return_type: b_ret,
                    ..
                },
            ) if a_params.len() == b_params.len() => {
                let pairs: Vec<_> = a_params
                    .iter()
                    .copied()
                    .zip(b_params.iter().copied())
                    .collect();
                for (pa, pb) in pairs {
                    self.unify(pa, pb, span);
                }
                self.unify(*a_ret, *b_ret, span);
                a
            }
            // Same-kind callable traits: unify inner function types
            (ResolvedType::CallableTrait(a_inner), ResolvedType::CallableTrait(b_inner))
            | (ResolvedType::MutCallableTrait(a_inner), ResolvedType::MutCallableTrait(b_inner))
            | (ResolvedType::ConsumeCallableTrait(a_inner), ResolvedType::ConsumeCallableTrait(b_inner)) => {
                let (a_inner, b_inner) = (*a_inner, *b_inner);
                self.unify(a_inner, b_inner, span);
                a
            }
            // Callable hierarchy coercion: Callable → MutCallable → ConsumeCallable (upward OK)
            (ResolvedType::MutCallableTrait(a_inner), ResolvedType::CallableTrait(b_inner)) => {
                let (a_inner, b_inner) = (*a_inner, *b_inner);
                self.unify(a_inner, b_inner, span);
                a // MutCallable accepts Callable
            }
            (ResolvedType::ConsumeCallableTrait(a_inner), ResolvedType::CallableTrait(b_inner))
            | (ResolvedType::ConsumeCallableTrait(a_inner), ResolvedType::MutCallableTrait(b_inner)) => {
                let (a_inner, b_inner) = (*a_inner, *b_inner);
                self.unify(a_inner, b_inner, span);
                a // ConsumeCallable accepts Callable and MutCallable
            }
            // BoxedCallable: same kind, unify inner types
            (ResolvedType::BoxedCallable { kind: a_kind, inner: a_inner },
             ResolvedType::BoxedCallable { kind: b_kind, inner: b_inner })
                if a_kind == b_kind => {
                let (a_inner, b_inner) = (*a_inner, *b_inner);
                self.unify(a_inner, b_inner, span);
                a
            }
            // BoxedCallable ↔ CallableTrait: auto-boxing
            (ResolvedType::BoxedCallable { inner: a_inner, .. },
             ResolvedType::CallableTrait(b_inner))
            | (ResolvedType::BoxedCallable { inner: a_inner, .. },
               ResolvedType::MutCallableTrait(b_inner))
            | (ResolvedType::BoxedCallable { inner: a_inner, .. },
               ResolvedType::ConsumeCallableTrait(b_inner)) => {
                let (a_inner, b_inner) = (*a_inner, *b_inner);
                self.unify(a_inner, b_inner, span);
                a
            }
            // BoxedCallable ↔ Function: auto-boxing from function type
            (ResolvedType::BoxedCallable { inner: a_inner, .. },
             ResolvedType::Function { .. }) => {
                let a_inner = *a_inner;
                self.unify(a_inner, b, span);
                a
            }
            // Callable ↔ Function: auto-coerce function pointer to callable (any variant)
            (ResolvedType::CallableTrait(inner), ResolvedType::Function { .. })
            | (ResolvedType::MutCallableTrait(inner), ResolvedType::Function { .. })
            | (ResolvedType::ConsumeCallableTrait(inner), ResolvedType::Function { .. }) => {
                let inner = *inner;
                self.unify(inner, b, span);
                a
            }
            (ResolvedType::Function { .. }, ResolvedType::CallableTrait(inner))
            | (ResolvedType::Function { .. }, ResolvedType::MutCallableTrait(inner))
            | (ResolvedType::Function { .. }, ResolvedType::ConsumeCallableTrait(inner)) => {
                let inner = *inner;
                self.unify(a, inner, span);
                b
            }
            // Allow implicit widening between integer types (matches C codegen behavior)
            (ResolvedType::Primitive(a_prim), ResolvedType::Primitive(b_prim))
                if is_integer_type(a_prim) && is_integer_type(b_prim) =>
            {
                a // accept the expected (lhs) type
            }
            // String→str coercion: String auto-coerces to str (owned → view)
            (ResolvedType::Primitive(PrimitiveType::Str), ResolvedType::Primitive(PrimitiveType::StringType))
            | (ResolvedType::Primitive(PrimitiveType::StringType), ResolvedType::Primitive(PrimitiveType::Str)) => {
                a // accept the expected (lhs) type
            }
            // cstr ↔ str coercion (both are const char* in S0)
            (ResolvedType::Primitive(PrimitiveType::Str), ResolvedType::Primitive(PrimitiveType::CStr))
            | (ResolvedType::Primitive(PrimitiveType::CStr), ResolvedType::Primitive(PrimitiveType::Str)) => {
                a
            }
            // cstr ↔ String coercion
            (ResolvedType::Primitive(PrimitiveType::CStr), ResolvedType::Primitive(PrimitiveType::StringType))
            | (ResolvedType::Primitive(PrimitiveType::StringType), ResolvedType::Primitive(PrimitiveType::CStr)) => {
                a
            }
            // Mutex[T]/Shared[T] ↔ T coercion for shared variables:
            // A shared variable has type T but may be passed where Mutex[T] or Shared[T]
            // is expected (e.g., spawned functions that receive the raw wrapper).
            (ResolvedType::Generic(def_id, args), _) if args.len() == 1 => {
                let name = &self.scopes.get_def(*def_id).name;
                let is_shared_wrapper = name == "Mutex" || name == "Shared" || name == "RWLock";
                if is_shared_wrapper {
                    self.unify(args[0], b, span);
                    a
                } else {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: self.describe_resolved_type(a),
                            found: self.describe_resolved_type(b),
                        },
                        span,
                    );
                    a
                }
            }
            (_, ResolvedType::Generic(def_id, args)) if args.len() == 1 => {
                let name = &self.scopes.get_def(*def_id).name;
                let is_shared_wrapper = name == "Mutex" || name == "Shared" || name == "RWLock";
                if is_shared_wrapper {
                    self.unify(a, args[0], span);
                    b
                } else {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: self.describe_resolved_type(a),
                            found: self.describe_resolved_type(b),
                        },
                        span,
                    );
                    a
                }
            }
            // AtomicInt ↔ int, AtomicBool ↔ bool coercion for shared(atomic) variables
            (ResolvedType::Defined(def_id), _) => {
                let name = &self.scopes.get_def(*def_id).name;
                if (name == "AtomicInt" && matches!(b_ty, ResolvedType::Primitive(PrimitiveType::Int)))
                    || (name == "AtomicBool" && matches!(b_ty, ResolvedType::Primitive(PrimitiveType::Bool)))
                {
                    a
                } else {
                    if a_ty != b_ty {
                        self.error(
                            SemanticErrorKind::TypeMismatch {
                                expected: self.describe_resolved_type(a),
                                found: self.describe_resolved_type(b),
                            },
                            span,
                        );
                    }
                    a
                }
            }
            (_, ResolvedType::Defined(def_id)) => {
                let name = &self.scopes.get_def(*def_id).name;
                if (name == "AtomicInt" && matches!(a_ty, ResolvedType::Primitive(PrimitiveType::Int)))
                    || (name == "AtomicBool" && matches!(a_ty, ResolvedType::Primitive(PrimitiveType::Bool)))
                {
                    b
                } else {
                    if a_ty != b_ty {
                        self.error(
                            SemanticErrorKind::TypeMismatch {
                                expected: self.describe_resolved_type(a),
                                found: self.describe_resolved_type(b),
                            },
                            span,
                        );
                    }
                    a
                }
            }
            _ => {
                if a_ty != b_ty {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: self.describe_resolved_type(a),
                            found: self.describe_resolved_type(b),
                        },
                        span,
                    );
                }
                a
            }
        }
    }

    // ─── Expression Inference ──────────────────────────────

    fn infer_expr(&mut self, expr: &Spanned<Expr>) -> TypeId {
        match &expr.node {
            Expr::IntLiteral(n) => {
                if let Some(hint_id) = self.decl_type_hint {
                    if let ResolvedType::Primitive(prim) = self.types.get(hint_id).clone() {
                        if let Some((min, max)) = int_range(&prim) {
                            let val = *n as i128;
                            if val < min || val > max {
                                self.error(
                                    SemanticErrorKind::ValueOutOfRange {
                                        value: val,
                                        type_name: format!("{prim:?}").to_lowercase(),
                                        min,
                                        max,
                                    },
                                    expr.span,
                                );
                            }
                            return hint_id; // coerce literal to declared type
                        }
                    }
                }
                self.types.int_id
            }
            Expr::FloatLiteral(_) => self.types.float_id,
            Expr::BoolLiteral(_) => self.types.bool_id,
            Expr::StringLiteral(s) => {
                use crate::lexer::token::StringSegment;
                for seg in &s.segments {
                    if let StringSegment::Interpolation(var_name, _) = seg {
                        let def_id_opt = if let Some(scope_id) = self.current_fn_scope {
                            self.scopes.lookup_within_function(scope_id, var_name)
                        } else {
                            self.scopes.lookup(var_name)
                        };
                        if let Some(def_id) = def_id_opt {
                            let def = self.scopes.get_def(def_id);
                            if let Some(type_id) = def.type_id {
                                match self.types.get(type_id) {
                                    ResolvedType::Primitive(_) | ResolvedType::Void => {}
                                    ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                                        let def = self.scopes.get_def(*def_id);
                                        if def.kind != DefKind::GenericParam {
                                            let type_name = &def.name;
                                            if !self.traits.has_trait_impl_by_name(type_name, "Displayable") {
                                                self.error(
                                                    SemanticErrorKind::NonPrintableInterpolation {
                                                        var_name: var_name.clone(),
                                                        type_name: self.describe_resolved_type(type_id),
                                                    },
                                                    expr.span.clone(),
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        self.error(
                                            SemanticErrorKind::NonPrintableInterpolation {
                                                var_name: var_name.clone(),
                                                type_name: self.describe_resolved_type(type_id),
                                            },
                                            expr.span.clone(),
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
                if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_, _))) {
                    self.types.owned_string_id
                } else if s.kind == crate::lexer::token::StringKind::CStr {
                    self.types.cstr_id
                } else {
                    self.types.string_id
                }
            }
            Expr::NoneLiteral => {
                // None is Option[?T] — for now return error type
                self.types.error_id
            }

            Expr::Identifier(name) => {
                if let Some(def_id) = self.resolve_name(expr.span.start, name) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(type_id) = def.type_id {
                        type_id
                    } else {
                        // Type not yet assigned (will be set during type checking)
                        self.types.error_id
                    }
                } else {
                    // Unresolved — may be a builtin
                    self.types.error_id
                }
            }

            Expr::SelfExpr => {
                self.current_self_type.unwrap_or(self.types.error_id)
            }

            Expr::It => {
                // Implicit closure parameter — use type from enclosing ImplicitClosure
                self.implicit_it_type.unwrap_or(self.types.error_id)
            }

            Expr::Path { segments } => {
                if let Some(first) = segments.first() {
                    if let Some(def_id) = self.resolve_name(first.span.start, &first.node) {
                        let def = self.scopes.get_def(def_id);
                        match def.kind {
                            DefKind::Enum => {
                                // Could be an enum variant access: Option.None
                                self.types.defined_id(def_id)
                            }
                            _ => def.type_id.unwrap_or(self.types.error_id),
                        }
                    } else {
                        self.types.error_id
                    }
                } else {
                    self.types.error_id
                }
            }

            Expr::UnaryOp { op, operand } => {
                match op {
                    UnaryOp::Neg => {
                        // Special case: -IntLiteral with a type hint — check the negated value
                        if let Expr::IntLiteral(n) = &operand.node {
                            if let Some(hint_id) = self.decl_type_hint {
                                if let ResolvedType::Primitive(prim) = self.types.get(hint_id).clone() {
                                    if let Some((min, max)) = int_range(&prim) {
                                        let val = -(*n as i128);
                                        if val < min || val > max {
                                            self.error(
                                                SemanticErrorKind::ValueOutOfRange {
                                                    value: val,
                                                    type_name: format!("{prim:?}").to_lowercase(),
                                                    min,
                                                    max,
                                                },
                                                expr.span,
                                            );
                                        }
                                        return hint_id;
                                    }
                                }
                            }
                        }
                        let operand_type = self.infer_expr(operand);
                        operand_type
                    }
                    UnaryOp::Not => {
                        self.infer_expr(operand);
                        self.types.bool_id
                    }
                    UnaryOp::BitNot => {
                        let operand_type = self.infer_expr(operand);
                        operand_type
                    }
                }
            }

            Expr::BinaryOp { left, op, right } => {
                let left_type = self.infer_expr(left);
                let right_type = self.infer_expr(right);

                match op {
                    // Comparison operators return bool
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::LtEq
                    | BinaryOp::GtEq => {
                        self.unify(left_type, right_type, expr.span);
                        self.types.bool_id
                    }
                    // Logical operators require bool
                    BinaryOp::And | BinaryOp::Or => {
                        self.unify(left_type, self.types.bool_id, left.span);
                        self.unify(right_type, self.types.bool_id, right.span);
                        self.types.bool_id
                    }
                    // `in` returns bool
                    BinaryOp::In => self.types.bool_id,
                    // Arithmetic operators — result is same type
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem
                    | BinaryOp::Mod | BinaryOp::AddWrap | BinaryOp::SubWrap | BinaryOp::MulWrap => {
                        self.unify(left_type, right_type, expr.span)
                    }
                    // Bitwise operators — result is same type
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor
                    | BinaryOp::Shl | BinaryOp::Shr => {
                        self.unify(left_type, right_type, expr.span)
                    }
                }
            }

            Expr::Call { callee, generic_args, args, .. } => {
                let callee_type = self.infer_expr(callee);
                let resolved = self.resolve_type(callee_type);

                // Validate `alloc=` named arg on builtin constructors
                if let Expr::Identifier(cname) = &callee.node {
                    let is_builtin_ctor = matches!(cname.as_str(),
                        "Vector" | "Dict" | "HashMap"
                        | "Set" | "HashSet" | "Channel" | "String" | "Arena" | "PoolAllocator" | "TlsfAllocator"
                        | "FixedBufferAllocator" | "FallbackAllocator"
                    );
                    if is_builtin_ctor {
                        for arg in args {
                            if let Some(ref name) = arg.node.name {
                                if name.node != "alloc" {
                                    self.error(
                                        SemanticErrorKind::UnknownNamedArg { name: name.node.clone() },
                                        arg.span,
                                    );
                                } else {
                                    // Validate the alloc= value type is an allocator
                                    let alloc_type = self.infer_expr(&arg.node.value);
                                    let alloc_resolved = self.resolve_type(alloc_type);
                                    let is_alloc = match self.types.get(alloc_resolved) {
                                        ResolvedType::Defined(def_id) => {
                                            matches!(self.scopes.get_def(*def_id).name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator")
                                        }
                                        _ => false,
                                    };
                                    if !is_alloc {
                                        self.error(
                                            SemanticErrorKind::TypeMismatch {
                                                expected: "allocator type (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, or FallbackAllocator)".to_string(),
                                                found: format!("{:?}", self.types.get(alloc_resolved)),
                                            },
                                            arg.node.value.span,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                // Check where-clause trait bounds for generic calls
                if let Some(type_args) = generic_args {
                    if let Expr::Identifier(cname) = &callee.node {
                        if let Some(def_id) = self.resolve_name(callee.span.start, cname) {
                            self.check_trait_bounds(def_id, type_args, expr.span);
                        }
                    }
                }

                // Try to look up FunctionInfo for named args / default params
                let func_info = if let Expr::Identifier(cname) = &callee.node {
                    self.resolve_name(callee.span.start, cname)
                        .and_then(|def_id| self.function_info.get(&def_id))
                } else {
                    None
                };

                // Check if callee is a function
                match self.types.get(resolved).clone() {
                    ResolvedType::Function {
                        params,
                        return_type,
                        ..
                    } => {
                        let has_named = args.iter().any(|a| a.node.name.is_some());
                        let has_defaults = func_info.map_or(false, |fi| fi.param_defaults.iter().any(|d| d.is_some()));

                        if (has_named || has_defaults) && func_info.is_some() {
                            // Full named-arg / default-param validation
                            let fi = func_info.unwrap();
                            self.check_named_args_and_defaults(args, &params, fi, expr.span);
                        } else {
                            // Simple positional check (original behavior)
                            if args.len() != params.len() {
                                self.error(
                                    SemanticErrorKind::WrongArgCount {
                                        expected: params.len(),
                                        found: args.len(),
                                    },
                                    expr.span,
                                );
                            }
                            for (arg, &param_type) in args.iter().zip(params.iter()) {
                                let prev_hint = self.decl_type_hint;
                                self.decl_type_hint = Some(param_type);
                                let arg_type = self.infer_expr(&arg.node.value);
                                self.decl_type_hint = prev_hint;
                                self.unify(param_type, arg_type, arg.span);
                                self.validate_closure_arg_kind(param_type, &arg.node.value);
                            }
                        }
                        return_type
                    }
                    ResolvedType::CallableTrait(inner)
                    | ResolvedType::MutCallableTrait(inner)
                    | ResolvedType::ConsumeCallableTrait(inner)
                    | ResolvedType::BoxedCallable { inner, .. } => {
                        // Callable[sig]-typed callable — extract Function from inner
                        if let ResolvedType::Function { params, return_type, .. } = self.types.get(inner).clone() {
                            if args.len() != params.len() {
                                self.error(
                                    SemanticErrorKind::WrongArgCount {
                                        expected: params.len(),
                                        found: args.len(),
                                    },
                                    expr.span,
                                );
                            }
                            for (arg, &param_type) in args.iter().zip(params.iter()) {
                                let prev_hint = self.decl_type_hint;
                                self.decl_type_hint = Some(param_type);
                                let arg_type = self.infer_expr(&arg.node.value);
                                self.decl_type_hint = prev_hint;
                                self.unify(param_type, arg_type, arg.span);
                            }
                            return_type
                        } else {
                            for arg in args {
                                self.infer_expr(&arg.node.value);
                            }
                            self.types.error_id
                        }
                    }
                    ResolvedType::Error => {
                        // Check if callee is a struct/newtype constructor
                        if let Expr::Identifier(cname) = &callee.node {
                            if let Some(def_id) = self.resolve_name(callee.span.start, cname) {
                                let def = self.scopes.get_def(def_id);
                                let def_kind = def.kind;
                                let def_name = def.name.clone();
                                match def_kind {
                                    DefKind::Struct | DefKind::Newtype => {
                                        let mut arg_types = Vec::new();
                                        for arg in args {
                                            arg_types.push(self.infer_expr(&arg.node.value));
                                        }
                                        // For generic constructors like Pair[int, float](...),
                                        // resolve type args and return Generic; for non-generic
                                        // like Vec2(...), return Defined.
                                        if let Some(type_args) = generic_args {
                                            let resolved_args: Vec<TypeId> = type_args.iter().map(|ta| {
                                                match super::types::ast_type_to_resolved(
                                                    &ta.node, ta.span, self.scopes, self.types,
                                                ) {
                                                    Ok(tid) => tid,
                                                    Err(_) => self.types.error_id,
                                                }
                                            }).collect();
                                            return self.types.intern_generic(def_id, resolved_args);
                                        }
                                        // Box(value) → Box[T] where T is inferred from the argument
                                        if def_name == "Box" && arg_types.len() == 1 {
                                            return self.types.intern_generic(def_id, arg_types);
                                        }
                                        return self.types.defined_id(def_id);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        // Don't cascade — just infer arg types
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        // Known void-returning builtins: return void instead of error
                        if let Expr::Identifier(cname) = &callee.node {
                            if matches!(cname.as_str(), "print" | "assert" | "panic") {
                                return self.types.void_id;
                            }
                        }
                        self.types.error_id
                    }
                    ResolvedType::Defined(def_id) => {
                        // Could be a struct constructor or enum variant
                        let def = self.scopes.get_def(def_id);
                        let def_kind = def.kind;
                        let def_name = def.name.clone();
                        match def_kind {
                            DefKind::Struct | DefKind::Variant | DefKind::Newtype => {
                                // Infer argument types
                                let mut arg_types = Vec::new();
                                for arg in args {
                                    arg_types.push(self.infer_expr(&arg.node.value));
                                }
                                // Box.new(value) → Box[T] where T is inferred from the argument
                                if def_name == "Box" && arg_types.len() == 1 {
                                    return self.types.intern_generic(def_id, arg_types);
                                }
                                self.types.defined_id(def_id)
                            }
                            _ => {
                                for arg in args {
                                    self.infer_expr(&arg.node.value);
                                }
                                self.types.error_id
                            }
                        }
                    }
                    _ => {
                        // Not a function type — could still be a constructor call
                        // Check if the callee is an identifier resolving to a struct/enum
                        if let Expr::Identifier(cname) = &callee.node {
                            if let Some(def_id) = self.resolve_name(callee.span.start, cname) {
                                let def = self.scopes.get_def(def_id);
                                match def.kind {
                                    DefKind::Struct | DefKind::Variant | DefKind::Newtype => {
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
                                        return self.types.defined_id(def_id);
                                    }
                                    DefKind::Function => {
                                        // Function without resolved type — just infer args
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
                                        return self.types.error_id;
                                    }
                                    DefKind::Variable | DefKind::Const | DefKind::Static => {
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
                                        self.error(
                                            SemanticErrorKind::NotAFunction { name: cname.clone() },
                                            expr.span,
                                        );
                                        return self.types.error_id;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        self.types.error_id
                    }
                }
            }

            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                // Static method calls on type names: int.parse(), float.default()
                if let Expr::Identifier(name) = &receiver.node {
                    if let Some(ret) = self.resolve_static_method_type(name, &method.node, args, expr.span) {
                        return ret;
                    }
                }

                let receiver_type = self.infer_expr(receiver);
                let resolved_receiver = self.resolve_type(receiver_type);

                // Try to resolve method via trait registry
                if let Some((def_id, sig)) =
                    self.traits.resolve_method(resolved_receiver, &method.node)
                {
                    self.method_resolutions.insert(method.span.start, *def_id);
                    let sig = sig.clone();
                    // Check argument count
                    if args.len() != sig.params.len() {
                        self.error(
                            SemanticErrorKind::WrongArgCount {
                                expected: sig.params.len(),
                                found: args.len(),
                            },
                            expr.span,
                        );
                    }
                    for (arg, &param_type) in args.iter().zip(sig.params.iter()) {
                        let arg_type = self.infer_expr(&arg.node.value);
                        self.unify(param_type, arg_type, arg.span);
                    }
                    sig.return_type
                } else {
                    // Check for closure-returning Option/Result methods (map, and_then, or_else)
                    if let Some(ret_type) = self.infer_closure_method_type(resolved_receiver, &method.node, args) {
                        self.expr_types.insert(expr.span, ret_type);
                        ret_type
                    } else {
                        // Method not found — check built-in type methods
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        if let Some(ret_type) = self.builtin_method_type(resolved_receiver, &method.node) {
                            self.expr_types.insert(expr.span, ret_type);
                            ret_type
                        } else {
                            // TODO: Emit NoMethodFound here once method resolution
                            // covers all paths (equip, builtin, runtime). Currently
                            // too many false positives from imported module methods
                            // and runtime-only methods.
                            self.types.error_id
                        }
                    }
                }
            }

            Expr::FieldAccess { object, field } => {
                let object_type = self.infer_expr(object);
                let resolved = self.resolve_type(object_type);
                // Check if the field exists on the resolved type.
                // Only check Defined (non-generic) structs to avoid false positives
                // on wrapper/guard types like ReadGuard[T] that proxy field access.
                if let ResolvedType::Defined(did) = self.types.get(resolved).clone() {
                    if let Some(sfi) = self.struct_fields.get(&did) {
                        if !sfi.fields.iter().any(|(name, _)| name == &field.node) {
                            let type_name = self.describe_resolved_type(resolved);
                            self.error(
                                SemanticErrorKind::NoFieldFound {
                                    field: field.node.clone(),
                                    type_: type_name,
                                },
                                expr.span,
                            );
                        }
                    }
                }
                self.types.error_id
            }

            Expr::TupleFieldAccess { object, .. } => {
                let object_type = self.infer_expr(object);
                let resolved = self.resolve_type(object_type);
                match self.types.get(resolved).clone() {
                    ResolvedType::Tuple(_elems) => {
                        // Would check index bounds here
                        self.types.error_id
                    }
                    _ => self.types.error_id,
                }
            }

            Expr::Index { object, index } => {
                let object_type = self.infer_expr(object);
                let index_type = self.infer_expr(index);
                let resolved_obj = self.resolve_type(object_type);
                // str[int] → str (codepoint view), str[Range] → str (codepoint range)
                if resolved_obj == self.types.string_id {
                    if matches!(&index.node, Expr::Range { .. }) {
                        // Range bounds already inferred recursively
                        self.types.string_id
                    } else {
                        self.unify(index_type, self.types.int_id, expr.span);
                        self.types.string_id
                    }
                } else {
                    // Check for Vector[T] indexing/slicing
                    let vec_info = if let ResolvedType::Generic(def_id, args) = self.types.get(resolved_obj) {
                        let name = self.scopes.get_def(*def_id).name.clone();
                        if matches!(name.as_str(), "Vector") {
                            Some(args.first().copied().unwrap_or(self.types.int_id))
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    if let Some(elem_tid) = vec_info {
                        if matches!(&index.node, Expr::Range { .. }) {
                            resolved_obj
                        } else {
                            self.unify(index_type, self.types.int_id, expr.span);
                            elem_tid
                        }
                    } else {
                        // Check for Dict[K,V] / HashMap[K,V] indexing
                        let map_info = if let ResolvedType::Generic(def_id, args) = self.types.get(resolved_obj) {
                            let name = self.scopes.get_def(*def_id).name.clone();
                            if matches!(name.as_str(), "Dict" | "HashMap") && args.len() >= 2 {
                                Some((args[0], args[1]))
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        if let Some((key_tid, val_tid)) = map_info {
                            self.unify(index_type, key_tid, index.span);
                            val_tid
                        } else {
                            self.types.error_id
                        }
                    }
                }
            }

            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.infer_expr(start);
                }
                if let Some(end) = end {
                    self.infer_expr(end);
                }
                self.types.error_id // Range type
            }

            Expr::OptionalChain { object, .. } => {
                self.infer_expr(object);
                self.types.error_id
            }

            Expr::DefaultOp { lhs, rhs } => {
                let _lhs_type = self.infer_expr(lhs);
                let rhs_type = self.infer_expr(rhs);
                rhs_type // unwrapped type
            }

            Expr::Move { expr: inner }
            | Expr::MutableBorrow { expr: inner } => {
                self.infer_expr(inner) // ownership modifiers don't change the type
            }

            Expr::Deref { expr: inner } => {
                let inner_type = self.infer_expr(inner);
                let resolved = self.resolve_type(inner_type);
                // *expr unwraps Box[T] → T
                if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                    if self.scopes.get_def(def_id).name == "Box" && args.len() == 1 {
                        return args[0];
                    }
                }
                inner_type
            }

            Expr::Await { expr: inner } => {
                if let Expr::Await { .. } = &inner.node {
                    self.error(SemanticErrorKind::DoubleAwait, expr.span);
                }
                let inner_type = self.infer_expr(inner);
                if !self.current_function_is_async {
                    self.error(SemanticErrorKind::AwaitOutsideAsync, expr.span);
                }
                let resolved = self.resolve_type(inner_type);
                let future_or_task_type = if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                    let name = self.scopes.get_def(def_id).name.clone();
                    if (name == "Future" || name == "Task") && args.len() == 1 {
                        Some((resolved, args))
                    } else {
                        None
                    }
                } else {
                    // Try Future first, then Task
                    self.try_resolve_call_generic_type(inner, "Future", 1)
                        .or_else(|| self.try_resolve_call_generic_type(inner, "Task", 1))
                };
                if let Some((type_id, args)) = future_or_task_type {
                    self.expr_types.insert(inner.span, type_id);
                    args[0]
                } else {
                    if inner_type != self.types.error_id {
                        self.error(SemanticErrorKind::AwaitNonFuture, expr.span);
                    }
                    self.types.error_id
                }
            }

            Expr::Spawn { expr: inner } => {
                let inner_type = self.infer_expr(inner);
                let resolved = self.resolve_type(inner_type);
                let future_type = if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                    if self.scopes.get_def(def_id).name == "Future" && args.len() == 1 {
                        Some((resolved, args))
                    } else {
                        None
                    }
                } else {
                    self.try_resolve_call_generic_type(inner, "Future", 1)
                };
                if let Some((type_id, args)) = future_type {
                    self.expr_types.insert(inner.span, type_id);
                    let task_def_id = self.scopes.lookup("Task").expect("Task not registered");
                    self.types.intern_generic(task_def_id, vec![args[0]])
                } else {
                    // Allow spawning closure/function calls that return non-Future types.
                    // The backend wraps these in an async context automatically.
                    let is_call = matches!(&inner.node, Expr::Call { .. });
                    if is_call && inner_type != self.types.error_id {
                        let task_def_id = self.scopes.lookup("Task").expect("Task not registered");
                        self.types.intern_generic(task_def_id, vec![inner_type])
                    } else {
                        if inner_type != self.types.error_id {
                            self.error(SemanticErrorKind::SpawnNonFuture, expr.span);
                        }
                        self.types.error_id
                    }
                }
            }

            Expr::SpawnBlocking { expr: inner } => {
                let inner_type = self.infer_expr(inner);
                // spawn blocking works with ANY function call — not required to be async.
                // If it's a Future[T], unwrap to T; otherwise use the return type directly.
                let resolved = self.resolve_type(inner_type);
                let result_type = if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                    if self.scopes.get_def(def_id).name == "Future" && args.len() == 1 {
                        args[0]
                    } else {
                        inner_type
                    }
                } else {
                    inner_type
                };
                let task_def_id = self.scopes.lookup("Task").expect("Task not registered");
                self.types.intern_generic(task_def_id, vec![result_type])
            }

            Expr::RawCapture { expr: inner } => {
                self.infer_expr(inner);
                self.types.error_id // Result[T, E]
            }

            Expr::If {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                let cond_type = self.infer_expr(condition);
                self.unify(cond_type, self.types.bool_id, condition.span);

                let then_type = self.infer_expr(then_branch);

                for (cond, body) in elif_branches {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                    let bt = self.infer_expr(body);
                    self.unify(then_type, bt, body.span);
                }

                if let Some(else_branch) = else_branch {
                    let else_type = self.infer_expr(else_branch);
                    self.unify(then_type, else_type, else_branch.span);
                }

                then_type
            }

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                let scrutinee_type = self.infer_expr(scrutinee);
                let mut result_type = self.fresh_type_var();

                for arm in arms {
                    self.assign_pattern_types(&arm.pattern, scrutinee_type);
                    let arm_type = self.infer_expr(&arm.body);
                    result_type = self.unify(result_type, arm_type, arm.body.span);
                }

                if let Some(else_arm) = else_arm {
                    let else_type = self.infer_expr(else_arm);
                    result_type = self.unify(result_type, else_type, else_arm.span);
                }

                let match_items: Vec<MatchItem> = arms.iter().cloned().map(MatchItem::Arm).collect();
                self.check_match_exhaustiveness(scrutinee_type, &match_items, else_arm.is_some(), expr.span);

                result_type
            }

            Expr::Block(block) => self.check_block(block),

            Expr::Do { body } => self.check_block(body),

            Expr::Closure { params, body, .. } => {
                // Infer closure type from params and body.
                // Write resolved param types back to DefInfos so that
                // references to the params inside the body can find them.
                let mut param_types = Vec::new();
                for param in params {
                    if let Some(ty) = &param.node.type_ {
                        let tid = super::types::ast_type_to_resolved(
                            &ty.node, ty.span, self.scopes, self.types,
                        )
                        .unwrap_or(self.types.error_id);
                        param_types.push(tid);
                        if let Some(def_id) = self.scopes.lookup_def_by_span(
                            &param.node.name.node,
                            param.node.name.span,
                        ) {
                            self.scopes.get_def_mut(def_id).type_id = Some(tid);
                        }
                    } else {
                        let tid = self.fresh_type_var();
                        param_types.push(tid);
                        if let Some(def_id) = self.scopes.lookup_def_by_span(
                            &param.node.name.node,
                            param.node.name.span,
                        ) {
                            self.scopes.get_def_mut(def_id).type_id = Some(tid);
                        }
                    }
                }
                let body_type = self.infer_expr(body);
                self.types.insert(ResolvedType::Function {
                    param_ownerships: vec![crate::parser::ast::Ownership::Borrow; param_types.len()],
                    params: param_types,
                    return_type: body_type,
                })
            }

            Expr::ImplicitClosure { body } => {
                let param_type = self.fresh_type_var();
                let prev_it_type = self.implicit_it_type.replace(param_type);
                let body_type = self.infer_expr(body);
                self.implicit_it_type = prev_it_type;
                self.types.insert(ResolvedType::Function {
                    params: vec![param_type],
                    return_type: body_type,
                    param_ownerships: vec![crate::parser::ast::Ownership::Borrow],
                })
            }

            Expr::ListComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.infer_expr(iterable);
                let elem_type = self.infer_expr(comp_expr);
                if let Some(cond) = condition {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                }
                let _ = elem_type;
                self.types.error_id // Vector[elem_type]
            }

            Expr::DictComprehension {
                key,
                value,
                iterable,
                condition,
                ..
            } => {
                self.infer_expr(iterable);
                self.infer_expr(key);
                self.infer_expr(value);
                if let Some(cond) = condition {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                }
                self.types.error_id // Dict[K, V]
            }

            Expr::SetComprehension {
                expr: comp_expr,
                iterable,
                condition,
                ..
            } => {
                self.infer_expr(iterable);
                self.infer_expr(comp_expr);
                if let Some(cond) = condition {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                }
                self.types.error_id // Set[T]
            }

            Expr::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return self.types.error_id;
                }
                let first_type = self.infer_expr(&elements[0]);
                for elem in &elements[1..] {
                    let et = self.infer_expr(elem);
                    self.unify(first_type, et, elem.span);
                }
                self.types.insert(ResolvedType::Array(first_type, elements.len()))
            }

            Expr::TupleLiteral(elements) => {
                let elem_types: Vec<TypeId> =
                    elements.iter().map(|e| self.infer_expr(e)).collect();
                self.types.insert(ResolvedType::Tuple(elem_types))
            }

            Expr::DictLiteral(pairs) => {
                if pairs.is_empty() {
                    // Empty dict literal — try to infer from declaration hint
                    if let Some(hint_id) = self.decl_type_hint {
                        return hint_id;
                    }
                    return self.types.error_id;
                }
                let key_type = self.infer_expr(&pairs[0].0);
                let val_type = self.infer_expr(&pairs[0].1);
                for (k, v) in &pairs[1..] {
                    let kt = self.infer_expr(k);
                    let vt = self.infer_expr(v);
                    self.unify(key_type, kt, k.span);
                    self.unify(val_type, vt, v.span);
                }
                // Build Dict[K, V] type
                if let Some(dict_def_id) = self.scopes.lookup("Dict") {
                    self.types.intern_generic(dict_def_id, vec![key_type, val_type])
                } else {
                    self.types.error_id
                }
            }

            Expr::StructLiteral { name, generic_args, args } => {
                // Resolve struct type
                if let Some(def_id) = self.resolve_name(name.span.start, &name.node) {
                    let def = self.scopes.get_def(def_id);
                    if def.kind != DefKind::Struct {
                        self.error(
                            SemanticErrorKind::NotAStruct {
                                name: name.node.clone(),
                            },
                            name.span,
                        );
                    }
                    for arg in args {
                        self.infer_expr(arg);
                    }
                    // If generic args present, build Generic type
                    if let Some(ga) = generic_args {
                        let type_ids: Vec<TypeId> = ga.iter().filter_map(|t| {
                            super::types::ast_type_to_resolved(
                                &t.node, t.span, self.scopes, self.types,
                            ).ok()
                        }).collect();
                        if !type_ids.is_empty() {
                            return self.types.intern_generic(def_id, type_ids);
                        }
                    }
                    self.types.defined_id(def_id)
                } else {
                    for arg in args {
                        self.infer_expr(arg);
                    }
                    self.types.error_id
                }
            }

            Expr::As { expr: inner, type_ } => {
                self.infer_expr(inner);
                super::types::ast_type_to_resolved(
                    &type_.node,
                    type_.span,
                    self.scopes,
                    self.types,
                )
                .unwrap_or(self.types.error_id)
            }

            Expr::Is { expr: inner, .. } => {
                self.infer_expr(inner);
                self.types.bool_id
            }

            Expr::DotShorthand { variant, args } => {
                if let Some(hint_id) = self.decl_type_hint {
                    let resolved = self.resolve_type(hint_id);
                    let is_enum = match self.types.get(resolved).clone() {
                        ResolvedType::Defined(def_id) => {
                            self.scopes.get_def(def_id).kind == DefKind::Enum
                        }
                        ResolvedType::Generic(def_id, _) => {
                            self.scopes.get_def(def_id).kind == DefKind::Enum
                        }
                        _ => false,
                    };
                    if is_enum {
                        let field_types = self.resolve_variant_field_types(hint_id, &variant.node);
                        let prev = self.decl_type_hint;
                        for (i, arg) in args.iter().enumerate() {
                            self.decl_type_hint = field_types.get(i).copied();
                            self.infer_expr(&arg.node.value);
                            self.decl_type_hint = prev;
                        }
                        return hint_id;
                    }
                }
                // No type context — error
                self.error(
                    SemanticErrorKind::TypeMismatch {
                        expected: "enum type context for dot-shorthand".into(),
                        found: format!(".{}", variant.node),
                    },
                    variant.span,
                );
                for arg in args {
                    self.infer_expr(&arg.node.value);
                }
                self.types.error_id
            }
            Expr::MetaOpInfix { left, right, .. } => {
                // Type-check both operands; the result type mirrors the left operand.
                // The operator is unknown at template-checking time; treat as arithmetic.
                let t = self.infer_expr(left);
                self.infer_expr(right);
                t
            }
            Expr::MetaOpToken(_) => {
                // Meta op tokens have no runtime value.
                self.types.void_id
            }
            Expr::Rethrow { expr: inner, transform, .. } => {
                let inner_type = self.infer_expr(inner);
                self.infer_expr(transform);
                if !self.current_function_throws {
                    self.error(SemanticErrorKind::RethrowInNonThrowingFunction, expr.span);
                }
                inner_type
            }
            Expr::Catch { expr: inner, recovery, .. } => {
                let inner_type = self.infer_expr(inner);
                self.infer_expr(recovery);
                inner_type
            }
        }
    }

    // ─── Statement Checking ────────────────────────────────

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                type_, pattern, value, shared, ..
            } => {
                // Resolve declared type first so we can set the hint for literal coercion
                let declared_type = match &type_.node {
                    Type::Inferred => None,
                    _ => super::types::ast_type_to_resolved(
                        &type_.node,
                        type_.span,
                        self.scopes,
                        self.types,
                    ).ok(),
                };

                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = declared_type;
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;

                let resolved_type = match &type_.node {
                    Type::Inferred => {
                        // auto — infer from value
                        let resolved = self.resolve_type(value_type);
                        if resolved == self.types.error_id {
                            None
                        } else {
                            Some(resolved)
                        }
                    }
                    _ => {
                        if let Some(declared_type) = declared_type {
                            // Allow assigning array literals to collection types
                            // (e.g. Vector[int] v = [1, 2, 3])
                            if !self.is_collection_assignment(declared_type, value_type)
                                && !self.is_auto_propagation_compatible(declared_type, value_type)
                            {
                                self.unify(declared_type, value_type, value.span);
                            }
                            self.validate_closure_arg_kind(declared_type, value);
                            Some(declared_type)
                        } else {
                            None
                        }
                    }
                };

                // Write the resolved type back to the pattern binding's DefInfo
                if let Some(type_id) = resolved_type {
                    if let Pattern::Binding(name) = &pattern.node {
                        // Use span-based lookup to avoid cross-module name collisions
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span) {
                            self.scopes.get_def_mut(def_id).type_id = Some(type_id);
                        }
                    }
                    // Validate shared(atomic) is only used with int or bool
                    if *shared == crate::parser::ast::SharedKind::Atomic {
                        let resolved = self.types.get(type_id);
                        let is_atomic_compatible = matches!(
                            resolved,
                            super::types::ResolvedType::Primitive(PrimitiveType::Int)
                            | super::types::ResolvedType::Primitive(PrimitiveType::Bool)
                        );
                        if !is_atomic_compatible {
                            self.error(
                                SemanticErrorKind::TypeMismatch {
                                    expected: "int or bool (shared(atomic) only supports scalar types)".to_string(),
                                    found: self.describe_resolved_type(type_id),
                                },
                                pattern.span,
                            );
                        }
                    }
                }
            }

            Stmt::Expr(expr) => {
                self.infer_expr(expr);
            }

            Stmt::Assign { target, value } => {
                let target_type = self.infer_expr(target);
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = Some(target_type);
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;
                if !self.is_auto_propagation_compatible(target_type, value_type) {
                    self.unify(target_type, value_type, value.span);
                }
            }

            Stmt::CompoundAssign { target, value, .. } => {
                let target_type = self.infer_expr(target);
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = Some(target_type);
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;
                self.unify(target_type, value_type, value.span);
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let prev_hint = self.decl_type_hint;
                    self.decl_type_hint = self.current_return_type;
                    let expr_type = self.infer_expr(expr);
                    self.decl_type_hint = prev_hint;
                    if let Some(ret_type) = self.current_return_type {
                        self.unify(ret_type, expr_type, expr.span);
                    }
                }
            }

            Stmt::Throw(expr) => {
                self.infer_expr(expr);
                if !self.current_function_throws {
                    self.error(SemanticErrorKind::ThrowInNonThrowingFunction, stmt.span);
                }
            }

            Stmt::Break(expr) => {
                if let Some(expr) = expr {
                    self.infer_expr(expr);
                }
                if self.loop_depth == 0 {
                    self.error(SemanticErrorKind::BreakOutsideLoop, stmt.span);
                }
            }

            Stmt::Assert { condition, message } => {
                self.infer_expr(condition);
                if let Some(msg) = message {
                    self.infer_expr(msg);
                }
            }
            Stmt::AssertReturn { condition, message } => {
                self.infer_expr(condition);
                if let Some(msg) = message {
                    self.infer_expr(msg);
                }
            }

            Stmt::Snapshot { value, .. } => {
                self.infer_expr(value);
            }

            Stmt::Continue => {
                if self.loop_depth == 0 {
                    self.error(SemanticErrorKind::BreakOutsideLoop, stmt.span);
                }
            }
            Stmt::Pass => {}

            Stmt::For {
                pattern, iterable, body, else_body, ..
            } => {
                // Detect .enumerate() and unwrap to inner iterable
                let (inner_iterable, is_enumerate) = if let Expr::MethodCall {
                    receiver, method, args, ..
                } = &iterable.node {
                    if method.node == "enumerate" && args.is_empty() {
                        (receiver.as_ref() as &Spanned<Expr>, true)
                    } else {
                        (iterable, false)
                    }
                } else {
                    (iterable, false)
                };

                let iter_type = self.infer_expr(inner_iterable);
                let resolved_iter = self.resolve_type(iter_type);

                // Determine the element type from the iterable
                let elem_type = if resolved_iter == self.types.string_id {
                    Some(self.types.string_id)
                } else {
                    match self.types.get(resolved_iter).clone() {
                        ResolvedType::Generic(def_id, args) => {
                            let name = self.scopes.get_def(def_id).name.clone();
                            match name.as_str() {
                                "Vector" | "Set" | "HashSet" => {
                                    args.first().copied()
                                }
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                };

                if is_enumerate {
                    // Assign types to enumerate tuple pattern: (idx, elem)
                    if let Pattern::Tuple(elems) = &pattern.node {
                        if let Some(Pattern::Binding(idx_name)) = elems.first().map(|e| &e.node) {
                            if let Some(def_id) = self.scopes.lookup_def_by_span(idx_name, elems[0].span) {
                                self.scopes.get_def_mut(def_id).type_id = Some(self.types.int_id);
                            }
                        }
                        if let Some(elem_tid) = elem_type {
                            if elems.len() >= 2 {
                                if let Pattern::Binding(elem_name) = &elems[1].node {
                                    if let Some(def_id) = self.scopes.lookup_def_by_span(elem_name, elems[1].span) {
                                        self.scopes.get_def_mut(def_id).type_id = Some(elem_tid);
                                    }
                                }
                            }
                        }
                    }
                } else if let Some(elem_tid) = elem_type {
                    // Assign element type to simple binding
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span) {
                            self.scopes.get_def_mut(def_id).type_id = Some(elem_tid);
                        }
                    }
                }

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
                let cond_type = self.infer_expr(condition);
                self.unify(cond_type, self.types.bool_id, condition.span);
                // Assign types to all `is` pattern bindings (including compound conditions)
                self.assign_compound_is_types(condition);
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
                let cond_type = self.infer_expr(condition);
                self.unify(cond_type, self.types.bool_id, condition.span);
                // Assign types to all `is` pattern bindings (including compound conditions)
                self.assign_compound_is_types(condition);
                self.check_block(then_body);

                for (cond, body) in elif_branches {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                    self.assign_compound_is_types(cond);
                    self.check_block(body);
                }

                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                }
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                let scrutinee_type = self.infer_expr(scrutinee);
                for arm in arms.iter().filter_map(|i| i.arm()) {
                    self.assign_pattern_types(&arm.pattern, scrutinee_type);
                    if let Some(guard) = &arm.guard {
                        let gt = self.infer_expr(guard);
                        self.unify(gt, self.types.bool_id, guard.span);
                    }
                    self.infer_expr(&arm.body);
                }
                if let Some(else_arm) = else_arm {
                    self.check_block(else_arm);
                }
                self.check_match_exhaustiveness(scrutinee_type, arms, else_arm.is_some(), stmt.span);
            }

            Stmt::Select { arms, else_arm } => {
                if !self.current_function_is_async {
                    self.error(SemanticErrorKind::SelectOutsideAsync, stmt.span);
                }
                for arm in arms {
                    match &arm.op {
                        SelectOp::Recv { type_, name, channel } => {
                            self.infer_expr(channel);
                            // Resolve the declared type and assign to the recv variable
                            if let Ok(type_id) = super::types::ast_type_to_resolved(
                                &type_.node, type_.span, self.scopes, self.types,
                            ) {
                                if let Some(def_id) = self.scopes.lookup_def_by_span(&name.node, name.span) {
                                    self.scopes.get_def_mut(def_id).type_id = Some(type_id);
                                }
                            }
                            self.check_block(&arm.body);
                        }
                        SelectOp::Send { channel, value } => {
                            self.infer_expr(channel);
                            self.infer_expr(value);
                            self.check_block(&arm.body);
                        }
                    }
                }
                if let Some(else_arm) = else_arm {
                    self.check_block(else_arm);
                }
            }

            Stmt::With { bindings, body } => {
                for binding in bindings {
                    let expr_type = self.infer_expr(&binding.expr);
                    // Assign the inferred type to the binding variable
                    let resolved = self.resolve_type(expr_type);
                    if resolved != self.types.error_id {
                        if let Some(def_id) = self.scopes.lookup_def_by_span(&binding.name.node, binding.name.span) {
                            self.scopes.get_def_mut(def_id).type_id = Some(resolved);
                        }
                    }
                }
                self.check_block(body);
            }

            Stmt::Unsafe { body } => {
                self.check_block(body);
            }

            Stmt::NamedScope { body, .. } => {
                self.check_block(body);
            }

            Stmt::Item(_) => {
                // Nested items are checked at the top level
            }

            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                // Conditions are meta expressions (typename(T) etc.) resolved only at
                // monomorphization time — skip infer_expr on them.
                self.check_block(then_body);
                for (_, body) in elif_branches {
                    self.check_block(body);
                }
                if let Some(eb) = else_body {
                    self.check_block(eb);
                }
            }

            Stmt::MetaFor { body, .. } => {
                // Range is a meta expression: skip infer_expr; just check the body.
                self.check_block(body);
            }

            Stmt::MetaMatch { arms, else_arm, .. } => {
                // Scrutinee and case exprs are meta expressions resolved only at
                // monomorphization time — skip infer_expr on them; check bodies only.
                for (_, body) in arms {
                    self.check_block(body);
                }
                if let Some(eb) = else_arm {
                    self.check_block(eb);
                }
            }

            Stmt::MetaWhile { body, .. } => {
                // Condition is a meta expression — skip infer_expr on it; check body only.
                self.check_block(body);
            }

            Stmt::MetaConst { .. } => {
                // Entirely a meta expression — evaluated at monomorphization time; skip.
            }

            Stmt::MetaLog { .. } => {
                // Compile-time diagnostic — removed before GIR lowering; skip.
            }
            Stmt::OnError { body } => {
                if !self.current_function_throws {
                    self.error(SemanticErrorKind::OnErrorInNonThrowingFunction, stmt.span);
                }
                self.check_block(body);
            }
        }
    }

    /// Check that a match on an enum type covers all variants.
    fn check_match_exhaustiveness(
        &mut self,
        scrutinee_type: TypeId,
        arms: &[MatchItem],
        has_else: bool,
        span: Span,
    ) {
        if has_else {
            return;
        }
        // MetaFor items expand at monomorphization time; we can't check exhaustiveness
        // statically if any are present — the expanded arms may cover all variants.
        if arms.iter().any(|i| matches!(i, MatchItem::MetaFor { .. })) {
            return;
        }

        // Resolve the scrutinee type and check if it's an enum.
        let resolved = self.resolve_type(scrutinee_type);
        let enum_def_id = match self.types.get(resolved) {
            ResolvedType::Defined(def_id) => *def_id,
            ResolvedType::Generic(def_id, _) => *def_id,
            _ => return,
        };
        if self.scopes.get_def(enum_def_id).kind != DefKind::Enum {
            return;
        }

        let variant_info = match self.enum_variants.get(&enum_def_id) {
            Some(info) => info,
            None => return,
        };
        let all_variants: Vec<&str> = variant_info.variants.iter().map(|(n, _)| n.as_str()).collect();

        // Collect covered variant names from unguarded arms.
        let mut has_catchall = false;
        let mut covered = rustc_hash::FxHashSet::default();
        for arm in arms.iter().filter_map(|i| i.arm()) {
            if arm.guard.is_some() {
                continue; // guarded arms don't guarantee coverage
            }
            self.collect_covered_variants(&arm.pattern.node, &all_variants, &mut covered, &mut has_catchall);
            if has_catchall {
                return;
            }
        }

        let missing: Vec<String> = all_variants
            .iter()
            .filter(|v| !covered.contains(**v))
            .map(|v| v.to_string())
            .collect();
        if !missing.is_empty() {
            self.error(SemanticErrorKind::NonExhaustiveMatch { missing_variants: missing }, span);
        }
    }

    /// Recursively collect which enum variants a pattern covers.
    fn collect_covered_variants<'p>(
        &self,
        pattern: &Pattern,
        all_variants: &[&str],
        covered: &mut rustc_hash::FxHashSet<String>,
        has_catchall: &mut bool,
    ) {
        match pattern {
            Pattern::Wildcard | Pattern::Rest => {
                *has_catchall = true;
            }
            Pattern::Binding(name) => {
                if all_variants.contains(&name.as_str()) {
                    covered.insert(name.clone());
                } else {
                    // It's a variable binding — acts as a catch-all.
                    *has_catchall = true;
                }
            }
            Pattern::Constructor { path, .. } => {
                if let Some(last) = path.last() {
                    covered.insert(last.node.clone());
                }
            }
            Pattern::Or(alts) => {
                for alt in alts {
                    self.collect_covered_variants(&alt.node, all_variants, covered, has_catchall);
                    if *has_catchall {
                        return;
                    }
                }
            }
            Pattern::Literal(lit) => {
                if matches!(lit.node, Expr::NoneLiteral) {
                    covered.insert("None".to_string());
                }
                // Other literals don't cover enum variants.
            }
            Pattern::Tuple(_) => {
                // Tuples don't cover enum variants.
            }
            Pattern::DotShorthand { variant, .. } => {
                covered.insert(variant.node.clone());
            }
        }
    }

    /// Assign type_ids to pattern-bound variables based on the scrutinee type.
    /// Called from match handlers so that destructured bindings (e.g. `case Error(e):`)
    /// get proper types for string interpolation and other uses.
    fn assign_pattern_types(&mut self, pattern: &Spanned<Pattern>, scrutinee_type: TypeId) {
        match &pattern.node {
            Pattern::Binding(name) => {
                // Skip if the name is a known variant (unit variant, not a real binding)
                let is_variant = self.enum_variants.values().any(|info|
                    info.variants.iter().any(|(vn, _)| vn == name)
                );
                if !is_variant {
                    // Use span-based lookup to avoid cross-module name collisions
                    if let Some(def_id) = self.scopes.lookup_def_by_span(name, pattern.span) {
                        self.scopes.get_def_mut(def_id).type_id = Some(scrutinee_type);
                    }
                }
            }
            Pattern::Constructor { path, fields } => {
                let variant_name = path.last().map(|s| s.node.as_str()).unwrap_or("");
                let field_types = self.resolve_variant_field_types(scrutinee_type, variant_name);
                for (i, field_pat) in fields.iter().enumerate() {
                    if let Some(&field_tid) = field_types.get(i) {
                        self.assign_pattern_types(field_pat, field_tid);
                    }
                }
            }
            Pattern::Tuple(elements) => {
                let resolved = self.resolve_type(scrutinee_type);
                if let ResolvedType::Tuple(field_tids) = self.types.get(resolved).clone() {
                    for (i, elem) in elements.iter().enumerate() {
                        if let Some(&tid) = field_tids.get(i) {
                            self.assign_pattern_types(elem, tid);
                        }
                    }
                }
            }
            Pattern::Or(alts) => {
                for alt in alts {
                    self.assign_pattern_types(alt, scrutinee_type);
                }
            }
            Pattern::DotShorthand { variant, fields } => {
                let field_types = self.resolve_variant_field_types(scrutinee_type, &variant.node);
                for (i, field_pat) in fields.iter().enumerate() {
                    if let Some(&field_tid) = field_types.get(i) {
                        self.assign_pattern_types(field_pat, field_tid);
                    }
                }
            }
            _ => {} // Wildcard, Literal, Rest — no bindings
        }
    }

    /// Walk compound `And` chains in conditions, assigning pattern types for each
    /// `is` sub-expression. Handles `a is Some(x) and b is Ok(y) and guard`.
    fn assign_compound_is_types(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Is { expr: scrutinee, negated: false, pattern, .. } => {
                let scrut_type = self.infer_expr(scrutinee);
                self.assign_pattern_types(pattern, scrut_type);
            }
            Expr::BinaryOp { left, op: BinaryOp::And, right } => {
                self.assign_compound_is_types(left);
                self.assign_compound_is_types(right);
            }
            _ => {}
        }
    }

    /// Resolve the field types for a particular variant given the scrutinee type.
    fn resolve_variant_field_types(&mut self, scrutinee_type: TypeId, variant_name: &str) -> Vec<TypeId> {
        let resolved = self.resolve_type(scrutinee_type);
        match self.types.get(resolved).clone() {
            ResolvedType::Generic(def_id, args) => {
                let name = self.scopes.get_def(def_id).name.clone();
                // Built-in generic enums
                match name.as_str() {
                    "Option" if !args.is_empty() => match variant_name {
                        "Some" => vec![args[0]],
                        _ => vec![],
                    },
                    "Result" if args.len() >= 2 => match variant_name {
                        "Ok" => vec![args[0]],
                        "Error" => vec![args[1]],
                        _ => vec![],
                    },
                    _ => {
                        // User-defined generic enum: substitute type params
                        self.resolve_user_enum_field_types(def_id, &args, variant_name)
                    }
                }
            }
            ResolvedType::Defined(def_id) => {
                // Non-generic user-defined enum
                self.resolve_user_enum_field_types(def_id, &[], variant_name)
            }
            _ => vec![],
        }
    }

    /// Look up variant field types from EnumVariantInfo and resolve AST types to TypeIds.
    /// For generic enums, builds a substitution map from param names → actual TypeIds.
    fn resolve_user_enum_field_types(&mut self, enum_def_id: DefId, type_args: &[TypeId], variant_name: &str) -> Vec<TypeId> {
        // Look up the EnumVariantInfo
        let info = match self.enum_variants.get(&enum_def_id) {
            Some(info) => info.clone(),
            None => return vec![],
        };

        // Find the variant's AST field types
        let ast_field_types = match info.variant_field_types.iter().find(|(vn, _)| vn == variant_name) {
            Some((_, types)) => types.clone(),
            None => return vec![],
        };

        // Build substitution map: generic param name → actual TypeId
        let subst: FxHashMap<String, TypeId> = info.generic_param_names.iter()
            .zip(type_args.iter())
            .map(|(name, &tid)| (name.clone(), tid))
            .collect();

        // Resolve each AST field type
        ast_field_types.iter().map(|ast_ty| {
            self.resolve_ast_type_with_subst(&ast_ty.node, ast_ty.span, &subst)
        }).collect()
    }

    /// Resolve an AST type to a TypeId, applying generic substitutions.
    fn resolve_ast_type_with_subst(&mut self, ast_ty: &Type, span: Span, subst: &FxHashMap<String, TypeId>) -> TypeId {
        // Check if the type is a named type that matches a substitution
        if let Type::Named { name, generic_args } = ast_ty {
            if generic_args.is_empty() {
                if let Some(&tid) = subst.get(&name.node) {
                    return tid;
                }
            }
        }
        // Fall back to normal resolution
        types::ast_type_to_resolved(ast_ty, span, self.scopes, self.types)
            .unwrap_or(self.types.error_id)
    }

    fn check_block(&mut self, block: &Block) -> TypeId {
        let mut last_type = self.types.void_id;
        let last_idx = block.stmts.len().saturating_sub(1);
        for (i, stmt) in block.stmts.iter().enumerate() {
            self.check_stmt(stmt);
            // The "value" of a block is its last expression statement,
            // or a tail if/match with branches that end in expressions.
            if let Stmt::Expr(expr) = &stmt.node {
                last_type = self.infer_expr(expr);
            } else if i == last_idx {
                last_type = self.infer_stmt_tail_type(&stmt.node);
            }
        }
        last_type
    }

    /// Infer the type produced by a statement in tail position of a block.
    /// Returns void for statements that don't produce values.
    fn infer_stmt_tail_type(&mut self, stmt: &Stmt) -> TypeId {
        match stmt {
            Stmt::If { then_body, else_body, .. } => {
                // Only value-producing if there's an else branch
                if else_body.is_some() {
                    // Type comes from the tail expression of the then branch
                    if let Some(tail) = then_body.stmts.last() {
                        if let Stmt::Expr(expr) = &tail.node {
                            return self.infer_expr(expr);
                        }
                        return self.infer_stmt_tail_type(&tail.node);
                    }
                }
                self.types.void_id
            }
            Stmt::Match { arms, .. } => {
                // Type comes from the first arm's body expression
                if let Some(first_arm) = arms.iter().find_map(|i| i.arm()) {
                    return self.infer_expr(&first_arm.body);
                }
                self.types.void_id
            }
            _ => self.types.void_id,
        }
    }

    /// Check if this is an assignment from an array/comprehension to a
    /// collection type (e.g. `Vector[int] v = [1, 2, 3]`), which should
    /// be allowed without type unification.
    fn is_collection_assignment(&self, declared: TypeId, value: TypeId) -> bool {
        let declared_resolved = self.resolve_type(declared);
        let value_resolved = self.resolve_type(value);
        if let ResolvedType::Generic(def_id, _) = self.types.get(declared_resolved) {
            let name = &self.scopes.get_def(*def_id).name;
            if matches!(name.as_str(), "Vector" | "Dict" | "HashMap" | "Set" | "HashSet") {
                // Allow any value type (array literal, comprehension, constructor call)
                return matches!(self.types.get(value_resolved),
                    ResolvedType::Array(_, _) | ResolvedType::Error
                );
            }
        }
        false
    }

    /// Check if auto-propagation allows assigning a `Result[T, E]` value to a `T`-typed
    /// destination. Requires the current function to be a propagation context (has `throws`
    /// or returns `Result`).
    fn is_auto_propagation_compatible(&self, declared: TypeId, value: TypeId) -> bool {
        // Check if value type is Result[T, E]
        let value_resolved = self.resolve_type(value);
        let (ok_type, _err_type) = if let ResolvedType::Generic(def_id, ref args) = self.types.get(value_resolved).clone() {
            let name = self.scopes.get_def(def_id).name.clone();
            if name == "Result" && args.len() == 2 {
                (args[0], args[1])
            } else {
                return false;
            }
        } else {
            return false;
        };

        // Check if declared type matches the Ok type
        let declared_resolved = self.resolve_type(declared);
        let ok_resolved = self.resolve_type(ok_type);
        if declared_resolved != ok_resolved {
            // Also accept if declared resolves to error_id (inference)
            if declared_resolved != self.types.error_id {
                return false;
            }
        }

        // Check if current function can propagate errors
        if self.current_function_throws {
            return true;
        }
        // Check if return type is Result
        if let Some(ret_type) = self.current_return_type {
            let ret_resolved = self.resolve_type(ret_type);
            if let ResolvedType::Generic(ret_def_id, _) = self.types.get(ret_resolved) {
                let ret_name = &self.scopes.get_def(*ret_def_id).name;
                if ret_name == "Result" {
                    return true;
                }
            }
        }

        false
    }

    /// Infer the return type of closure-taking methods like .map(), .and_then(), .or_else()
    /// on Option[T] and Result[T,E]. Returns None if this isn't such a method.
    fn infer_closure_method_type(
        &mut self,
        receiver_type: TypeId,
        method: &str,
        args: &[Spanned<CallArg>],
    ) -> Option<TypeId> {
        let (type_name, type_args, def_id) = match self.types.get(receiver_type) {
            ResolvedType::Generic(def_id, args) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                let args = args.clone();
                let def_id = *def_id;
                (name, args, def_id)
            }
            ResolvedType::Defined(def_id) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                let def_id = *def_id;
                (name, vec![], def_id)
            }
            _ => return None,
        };

        // Check for Iterator adapter methods on any type implementing Iterator[T]
        if matches!(method, "filter" | "map" | "fold" | "collect") {
            if let Some(ret) = self.try_iterator_adapter_type(&type_name, method, args) {
                return Some(ret);
            }
        }

        match (type_name.as_str(), method) {
            ("Option", "map") => {
                // (T) -> U, returns Option[U]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                Some(self.types.intern_generic(def_id, vec![u_type]))
            }
            ("Option", "and_then") => {
                // (T) -> Option[U], returns Option[U] directly
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                Some(ret_type)
            }
            ("Option", "or_else") => {
                // () -> Option[T], returns Option[T]
                let _ = args.first().map(|a| self.infer_expr(&a.node.value));
                Some(receiver_type)
            }
            ("Result", "map") => {
                // (T) -> U, returns Result[U, E]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                let e_type = type_args.get(1).copied()?;
                Some(self.types.intern_generic(def_id, vec![u_type, e_type]))
            }
            ("Result", "and_then") => {
                // (T) -> Result[U, E], returns Result[U, E] directly
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                Some(ret_type)
            }
            ("Result", "or_else") => {
                // (E) -> Result[T, F], returns Result[T, F] directly
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                Some(ret_type)
            }
            ("Result", "map_err") => {
                // (E) -> F, returns Result[T, F]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let f_type = self.extract_fn_return_type(closure_type)?;
                let t_type = type_args.first().copied()?;
                Some(self.types.intern_generic(def_id, vec![t_type, f_type]))
            }

            // --- Vector higher-order methods ---
            ("Vector", "filter") => {
                // (T) -> bool, returns Vector[T]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(receiver_type)
            }
            ("Vector", "map") => {
                // (T) -> U, returns Vector[U]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                Some(self.types.intern_generic(def_id, vec![u_type]))
            }
            ("Vector", "fold") => {
                // args: initial_value, closure (U, T) -> U — returns U
                let init_type = self.infer_expr(&args.first()?.node.value);
                let _ = args.get(1).map(|a| self.infer_expr(&a.node.value));
                Some(init_type)
            }
            ("Vector", "reduce") => {
                // (T, T) -> T, returns T
                let _ = self.infer_expr(&args.first()?.node.value);
                let elem_type = type_args.first().copied()?;
                Some(elem_type)
            }

            // --- Dict higher-order methods ---
            ("Dict" | "HashMap", "filter") => {
                // (K, V) -> bool, returns Dict[K,V]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(receiver_type)
            }
            ("Dict" | "HashMap", "fold") => {
                // args: initial_value, closure (U, K, V) -> U — returns U
                let init_type = self.infer_expr(&args.first()?.node.value);
                let _ = args.get(1).map(|a| self.infer_expr(&a.node.value));
                Some(init_type)
            }

            // --- Set higher-order methods ---
            ("Set" | "HashSet", "filter") => {
                // (T) -> bool, returns Set[T]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(receiver_type)
            }
            ("Set" | "HashSet", "fold") => {
                // args: initial_value, closure (U, T) -> U — returns U
                let init_type = self.infer_expr(&args.first()?.node.value);
                let _ = args.get(1).map(|a| self.infer_expr(&a.node.value));
                Some(init_type)
            }

            _ => None,
        }
    }

    /// Get the element TypeId for a type that implements Iterator[T].
    /// Looks up the Iterator impl in the trait registry, converts the AST type arg to a resolved TypeId.
    fn get_iterator_elem_type(&mut self, type_name: &str) -> Option<TypeId> {
        // Find the Iterator impl and clone the AST type arg to release the borrow
        let ast_type = self.traits.impls.iter()
            .find(|i| i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator"))
            .and_then(|i| i.trait_generic_args.first().cloned())?;
        // Convert AST type to resolved TypeId
        types::ast_type_to_resolved(&ast_type, Span { start: 0, end: 0 }, self.scopes, self.types).ok()
    }

    /// Infer the return type of Iterator adapter methods (collect, filter, map, fold).
    fn try_iterator_adapter_type(
        &mut self,
        type_name: &str,
        method: &str,
        args: &[Spanned<CallArg>],
    ) -> Option<TypeId> {
        // Check if this type implements Iterator[T]
        let elem_type = self.get_iterator_elem_type(type_name)?;

        // Look up the Vector def_id for wrapping results
        let vector_def_id = self.scopes.lookup("Vector")?;

        match method {
            "collect" => {
                // () -> Vector[T]
                Some(self.types.intern_generic(vector_def_id, vec![elem_type]))
            }
            "filter" => {
                // (T) -> bool, returns Vector[T]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(self.types.intern_generic(vector_def_id, vec![elem_type]))
            }
            "map" => {
                // (T) -> U, returns Vector[U]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                Some(self.types.intern_generic(vector_def_id, vec![u_type]))
            }
            "fold" => {
                // args: initial_value, closure (U, T) -> U — returns U
                let init_type = self.infer_expr(&args.first()?.node.value);
                let _ = args.get(1).map(|a| self.infer_expr(&a.node.value));
                Some(init_type)
            }
            _ => None,
        }
    }

    /// Validate a call with named arguments and/or default parameters.
    /// Checks: no positional after named, no unknown names, no duplicates,
    /// all required params are satisfied. Also type-checks args (including defaults).
    fn check_named_args_and_defaults(
        &mut self,
        args: &[Spanned<CallArg>],
        param_types: &[TypeId],
        func_info: &FunctionInfo,
        call_span: Span,
    ) {
        let param_names = &func_info.param_names;
        let param_defaults = &func_info.param_defaults;

        // Track which params have been satisfied
        let mut satisfied = vec![false; param_names.len()];
        let mut seen_named = false;

        // First pass: validate structure and map args to param slots
        for (i, arg) in args.iter().enumerate() {
            if let Some(ref name) = arg.node.name {
                seen_named = true;
                // Check name matches a parameter
                if let Some(pos) = param_names.iter().position(|pn| pn == &name.node) {
                    if satisfied[pos] {
                        self.error(
                            SemanticErrorKind::DuplicateNamedArg { name: name.node.clone() },
                            arg.span,
                        );
                    }
                    satisfied[pos] = true;
                    // Type-check this arg against the correct param
                    let prev_hint = self.decl_type_hint;
                    if pos < param_types.len() {
                        self.decl_type_hint = Some(param_types[pos]);
                    }
                    let arg_type = self.infer_expr(&arg.node.value);
                    self.decl_type_hint = prev_hint;
                    if pos < param_types.len() {
                        self.unify(param_types[pos], arg_type, arg.span);
                        self.validate_closure_arg_kind(param_types[pos], &arg.node.value);
                    }
                } else {
                    self.error(
                        SemanticErrorKind::UnknownNamedArg { name: name.node.clone() },
                        arg.span,
                    );
                    // Still infer the arg to avoid cascading errors
                    self.infer_expr(&arg.node.value);
                }
            } else {
                // Positional arg
                if seen_named {
                    self.error(SemanticErrorKind::PositionalAfterNamed, arg.span);
                }
                if i < param_names.len() {
                    satisfied[i] = true;
                    let prev_hint = self.decl_type_hint;
                    if i < param_types.len() {
                        self.decl_type_hint = Some(param_types[i]);
                    }
                    let arg_type = self.infer_expr(&arg.node.value);
                    self.decl_type_hint = prev_hint;
                    if i < param_types.len() {
                        self.unify(param_types[i], arg_type, arg.span);
                        self.validate_closure_arg_kind(param_types[i], &arg.node.value);
                    }
                } else {
                    // Extra positional arg beyond param count
                    self.infer_expr(&arg.node.value);
                }
            }
        }

        // Check that all params without defaults are satisfied
        for (i, sat) in satisfied.iter().enumerate() {
            if !sat {
                if i < param_defaults.len() && param_defaults[i].is_some() {
                    // Has a default — OK, type-check the default expr
                    if let Some(ref default_expr) = param_defaults[i] {
                        let default_type = self.infer_expr(default_expr);
                        if i < param_types.len() {
                            self.unify(param_types[i], default_type, default_expr.span);
                        }
                    }
                } else {
                    self.error(
                        SemanticErrorKind::MissingRequiredArg {
                            name: param_names[i].clone(),
                        },
                        call_span,
                    );
                }
            }
        }

        // Too many positional args
        if args.len() > param_names.len() && !args.iter().any(|a| a.node.name.is_some()) {
            self.error(
                SemanticErrorKind::WrongArgCount {
                    expected: param_names.len(),
                    found: args.len(),
                },
                call_span,
            );
        }
    }

    /// Try to determine if a Call expression returns a generic type with the given name
    /// by looking up the callee's FunctionInfo.
    fn try_resolve_call_generic_type(
        &self, expr: &Spanned<Expr>, type_name: &str, expected_args: usize,
    ) -> Option<(TypeId, Vec<TypeId>)> {
        if let Expr::Call { callee, .. } = &expr.node {
            if let Expr::Identifier(cname) = &callee.node {
                if let Some(def_id) = self.resolve_name(callee.span.start, cname) {
                    if let Some(info) = self.function_info.get(&def_id) {
                        if let Some(ret_type_id) = info.return_type_id {
                            let resolved = self.resolve_type(ret_type_id);
                            if let ResolvedType::Generic(d, args) = self.types.get(resolved).clone() {
                                if self.scopes.get_def(d).name == type_name && args.len() == expected_args {
                                    return Some((resolved, args));
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Extract the return type from a Function type.
    fn extract_fn_return_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.types.get(type_id) {
            ResolvedType::Function { return_type, .. } => Some(*return_type),
            _ => None,
        }
    }

    /// Resolve static method calls on type names (e.g. `int.parse("42")`, `float.default()`).
    /// Returns the return TypeId if this is a known static method, None to fall through.
    fn resolve_static_method_type(
        &mut self,
        type_name: &str,
        method: &str,
        args: &[Spanned<CallArg>],
        span: Span,
    ) -> Option<TypeId> {
        let prim = match type_name {
            "int" => Some(PrimitiveType::Int),
            "int8" => Some(PrimitiveType::Int8),
            "int16" => Some(PrimitiveType::Int16),
            "int32" => Some(PrimitiveType::Int32),
            "int64" => Some(PrimitiveType::Int64),
            "uint" => Some(PrimitiveType::Uint),
            "uint8" => Some(PrimitiveType::Uint8),
            "uint16" => Some(PrimitiveType::Uint16),
            "uint32" => Some(PrimitiveType::Uint32),
            "uint64" => Some(PrimitiveType::Uint64),
            "float" => Some(PrimitiveType::Float),
            "float32" => Some(PrimitiveType::Float32),
            "float64" => Some(PrimitiveType::Float64),
            "bool" => Some(PrimitiveType::Bool),
            "str" => Some(PrimitiveType::Str),
            _ => None,
        };
        let prim = prim?;
        let prim_tid = self.types.primitive_id(prim);

        match method {
            "parse" => {
                // Only int and float types support parse
                match prim {
                    PrimitiveType::Int | PrimitiveType::Int8 | PrimitiveType::Int16
                    | PrimitiveType::Int32 | PrimitiveType::Int64
                    | PrimitiveType::Uint | PrimitiveType::Uint8 | PrimitiveType::Uint16
                    | PrimitiveType::Uint32 | PrimitiveType::Uint64
                    | PrimitiveType::Float | PrimitiveType::Float32 | PrimitiveType::Float64 => {}
                    _ => return None,
                }
                // Infer argument types
                for arg in args {
                    self.infer_expr(&arg.node.value);
                }
                // Return Option[T]
                if let Some(option_def_id) = self.scopes.lookup("Option") {
                    let ret = self.types.intern_generic(option_def_id, vec![prim_tid]);
                    self.expr_types.insert(span, ret);
                    Some(ret)
                } else {
                    Some(prim_tid)
                }
            }
            "default" | "one" => {
                // default() supported on all primitives; one() only on numeric types
                if method == "one" {
                    match prim {
                        PrimitiveType::Int | PrimitiveType::Int8 | PrimitiveType::Int16
                        | PrimitiveType::Int32 | PrimitiveType::Int64
                        | PrimitiveType::Uint | PrimitiveType::Uint8 | PrimitiveType::Uint16
                        | PrimitiveType::Uint32 | PrimitiveType::Uint64
                        | PrimitiveType::Float | PrimitiveType::Float32 | PrimitiveType::Float64 => {}
                        _ => return None,
                    }
                }
                for arg in args {
                    self.infer_expr(&arg.node.value);
                }
                self.expr_types.insert(span, prim_tid);
                Some(prim_tid)
            }
            _ => None,
        }
    }

    /// Check if a method call is on a known built-in type, returning
    /// the return TypeId if so.
    fn builtin_method_type(&mut self, receiver_type: TypeId, method: &str) -> Option<TypeId> {
        // Determine the base type name and generic type args (if any)
        let (type_name, type_args) = match self.types.get(receiver_type) {
            ResolvedType::Generic(def_id, args) => {
                (self.scopes.get_def(*def_id).name.clone(), args.clone())
            }
            ResolvedType::Defined(def_id) => {
                (self.scopes.get_def(*def_id).name.clone(), vec![])
            }
            ResolvedType::Primitive(PrimitiveType::Str | PrimitiveType::StringType | PrimitiveType::CStr) => {
                ("str".to_string(), vec![])
            }
            ResolvedType::Primitive(PrimitiveType::Uint8) => {
                ("uint8".to_string(), vec![])
            }
            ResolvedType::Primitive(
                PrimitiveType::Int | PrimitiveType::Int8 | PrimitiveType::Int16 |
                PrimitiveType::Int32 | PrimitiveType::Int64 |
                PrimitiveType::Uint | PrimitiveType::Uint16 |
                PrimitiveType::Uint32 | PrimitiveType::Uint64 |
                PrimitiveType::Float | PrimitiveType::Float32 | PrimitiveType::Float64 |
                PrimitiveType::Bool
            ) => {
                if method == "hash" { return Some(self.types.int_id); }
                return None;
            }
            _ => return None,
        };

        // Helper: get element type T from Vector[T], fallback to int
        let elem_type = || type_args.first().copied().unwrap_or(self.types.int_id);
        // Helper: get value type V from Dict[K,V], fallback to int
        let val_type = || type_args.get(1).copied().unwrap_or(self.types.int_id);

        match type_name.as_str() {
            "Vector" => match method {
                "push" => Some(self.types.void_id),
                "pop" | "remove" | "get" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "set" => Some(self.types.void_id),
                "len" => Some(self.types.int_id),
                "index_of" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "clear" | "reserve" | "sort" | "reverse" | "insert" | "extend" => Some(self.types.void_id),
                "is_empty" | "contains" | "any" | "all" => Some(self.types.bool_id),
                "sorted" | "reversed" | "unique" | "slice" | "enumerate" => Some(receiver_type),
                "first" | "last" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "binary_search" => Some(self.types.int_id),
                _ => None,
            },
            "Dict" | "HashMap" => match method {
                "put" | "update" | "set" => Some(self.types.void_id),
                "get" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![val_type()]))
                    } else {
                        Some(val_type())
                    }
                }
                "get_or" | "get_or_put" => Some(val_type()),
                "contains" | "has" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                "keys" => {
                    // Return Vector[K]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![elem_type()]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "values" => {
                    // Return Vector[V]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![val_type()]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "items" => {
                    // Return Vector[(K,V)]
                    let tuple_tid = self.types.insert(ResolvedType::Tuple(vec![elem_type(), val_type()]));
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![tuple_tid]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                _ => None,
            },
            "Set" | "HashSet" => match method {
                "add" => Some(self.types.void_id),
                "contains" | "is_subset" | "is_superset" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                "union" | "intersection" | "difference" => Some(receiver_type),
                _ => None,
            },
            "uint8" => match method {
                "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace"
                | "is_upper" | "is_lower" | "is_hex_digit" | "is_ascii"
                    => Some(self.types.bool_id),
                "to_upper" | "to_lower" => Some(self.types.primitive_id(PrimitiveType::Uint8)),
                _ => None,
            },
            "str" | "String" => match method {
                "len" | "hash" | "count" | "byte_len" => Some(self.types.int_id),
                "index_of" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "contains" | "starts_with" | "ends_with" | "is_empty" => Some(self.types.bool_id),
                // View returns — no allocation, return str (Str)
                "trim" | "strip" | "lstrip" | "rstrip" | "removeprefix" | "removesuffix" | "byte_slice"
                | "substring"
                    => Some(self.types.string_id),
                // Allocating returns — return String (GorgetString)
                "to_upper" | "to_lower" | "replace" | "repeat" | "join" | "pad_left" | "pad_right"
                    => Some(self.types.owned_string_id),
                "enumerate" => Some(receiver_type),
                "byte_at" => Some(self.types.primitive_id(PrimitiveType::Uint8)),
                // char_at: deprecated compat alias — returns str (1-byte view, byte-indexed)
                "char_at" => Some(self.types.string_id),
                "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace"
                | "is_upper" | "is_lower" | "is_hex_digit" | "is_ascii"
                    => Some(self.types.bool_id),
                "split" => {
                    // Return Vector[str]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![self.types.string_id]))
                    } else {
                        Some(self.types.string_id) // fallback
                    }
                }
                "bytes" => {
                    // Return Vector[uint8]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        let uint8_tid = self.types.insert(ResolvedType::Primitive(PrimitiveType::Uint8));
                        Some(self.types.intern_generic(vec_def_id, vec![uint8_tid]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "codepoints" => {
                    // Return Vector[int]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "chars" => {
                    // Return Vector[str]
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![self.types.string_id]))
                    } else {
                        Some(self.types.string_id)
                    }
                }
                _ => None,
            },
            "Option" => match method {
                "unwrap" | "unwrap_or" | "expect" | "unwrap_or_else" => Some(elem_type()),
                "is_some" | "is_none" => Some(self.types.bool_id),
                "map" | "and_then" | "or_else" | "or" | "filter" => Some(receiver_type),
                "flatten" => {
                    let inner = elem_type();
                    // For Option[Option[U]], elem_type() is Option[U] — return it directly
                    if let ResolvedType::Generic(inner_def, _) = self.types.get(inner) {
                        if self.scopes.get_def(*inner_def).name == "Option" {
                            return Some(inner);
                        }
                    }
                    // Not a nested Option — gracefully return receiver_type
                    Some(receiver_type)
                }
                _ => None,
            },
            "Result" => match method {
                "unwrap" | "unwrap_or" | "expect" | "unwrap_or_else" => Some(elem_type()),
                "unwrap_error" => Some(val_type()),
                "is_ok" | "is_error" => Some(self.types.bool_id),
                "map" | "and_then" | "or_else" | "or" | "map_err" => Some(receiver_type),
                _ => None,
            },
            "Box" => match method {
                "get" => Some(elem_type()),
                "set" => Some(self.types.void_id),
                _ => None,
            },
            "Shared" => match method {
                // clone() returns another Shared[T] (same type as receiver)
                "clone" => Some(receiver_type),
                // get() returns the inner T
                "get" => Some(elem_type()),
                // strong_count() returns the number of active strong refs
                "strong_count" => Some(self.types.int_id),
                // downgrade() returns Weak[T]
                "downgrade" => {
                    if let Some(weak_def_id) = self.scopes.lookup("Weak") {
                        Some(self.types.intern_generic(weak_def_id, vec![elem_type()]))
                    } else {
                        Some(receiver_type)
                    }
                }
                // Shared[Vector[T]] element access: at(i) → T, set_at(i, val) → void, slen() → int
                "at" => {
                    // elem_type() is Vector[T]; we need T (the inner element type of the Vector)
                    let inner_vec = elem_type();
                    if let ResolvedType::Generic(_, inner_args) = self.types.get(inner_vec).clone() {
                        inner_args.first().copied()
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "set_at" => Some(self.types.void_id),
                "slen" => Some(self.types.int_id),
                _ => None,
            },
            "Weak" => match method {
                // clone() returns another Weak[T]
                "clone" => Some(receiver_type),
                // upgrade() returns Option[Shared[T]]
                "upgrade" => {
                    let shared_type = if let Some(shared_def_id) = self.scopes.lookup("Shared") {
                        self.types.intern_generic(shared_def_id, vec![elem_type()])
                    } else {
                        receiver_type
                    };
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![shared_type]))
                    } else {
                        Some(shared_type)
                    }
                }
                _ => None,
            },
            "Mutex" => match method {
                // lock() returns Guard[T]; the async wrapper is handled in codegen
                "lock" => {
                    if let Some(guard_def_id) = self.scopes.lookup("Guard") {
                        Some(self.types.intern_generic(guard_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                _ => None,
            },
            "Guard" => match method {
                // get() returns a copy of the inner T
                "get" => Some(elem_type()),
                // set(val) updates the inner T, returns void
                "set" => Some(self.types.void_id),
                _ => None,
            },
            "TaskGroup" => match method {
                // spawn(future) starts a child task, returns void
                "spawn" => Some(self.types.void_id),
                // join() returns void (blocks until all children complete)
                "join" => Some(self.types.void_id),
                _ => None,
            },
            "File" => match method {
                "read_all" => {
                    // Returns Result[String, str]
                    if let Some(result_def_id) = self.scopes.lookup("Result") {
                        Some(self.types.intern_generic(
                            result_def_id,
                            vec![self.types.owned_string_id, self.types.string_id],
                        ))
                    } else {
                        Some(self.types.owned_string_id)
                    }
                }
                "write" => Some(self.types.void_id),
                "close" => Some(self.types.void_id),
                _ => None,
            },
            "Socket" | "TlsSocket" => match method {
                "read_line" => {
                    // Returns Result[String, str]
                    if let Some(result_def_id) = self.scopes.lookup("Result") {
                        Some(self.types.intern_generic(
                            result_def_id,
                            vec![self.types.owned_string_id, self.types.string_id],
                        ))
                    } else {
                        Some(self.types.owned_string_id)
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    /// Check where-clause trait bounds for a generic function call.
    /// `callee_def_id` is the DefId of the called function,
    /// `generic_args` are the explicit type arguments at the call site.
    fn check_trait_bounds(
        &mut self,
        callee_def_id: DefId,
        generic_args: &[Spanned<Type>],
        span: Span,
    ) {
        let info = match self.function_info.get(&callee_def_id) {
            Some(info) => info.clone(),
            None => return,
        };
        if info.trait_bounds.is_empty() {
            return;
        }

        // Build mapping: generic param name → concrete type name
        let mut param_to_type: FxHashMap<&str, String> = FxHashMap::default();
        for (i, param_name) in info.generic_param_names.iter().enumerate() {
            if let Some(type_arg) = generic_args.get(i) {
                if let Some(name) = ast_type_to_gorget_name(&type_arg.node) {
                    param_to_type.insert(param_name, name);
                }
            }
        }

        // Check each bound
        for (param_name, required_traits) in &info.trait_bounds {
            if let Some(concrete_type) = param_to_type.get(param_name.as_str()) {
                for trait_name in required_traits {
                    if self.traits.has_trait_impl_by_name(concrete_type, trait_name) {
                        continue;
                    }
                    // Transitive bound propagation: if the type arg is a generic param
                    // of the current function with matching (or super-) trait bounds,
                    // the bound is satisfied transitively.
                    let satisfied_by_outer_bound = self.current_trait_bounds.iter().any(|(p, bounds)| {
                        p == concrete_type && bounds.iter().any(|b| self.traits.trait_satisfies(b, trait_name))
                    });
                    if !satisfied_by_outer_bound {
                        self.error(
                            SemanticErrorKind::UnsatisfiedTraitBound {
                                type_name: concrete_type.clone(),
                                trait_name: trait_name.clone(),
                                param_name: param_name.clone(),
                            },
                            span,
                        );
                    }
                }
            }
        }
    }

    /// Pre-register a function's signature (return type + param types) on its DefInfo
    /// so that callers can infer the function's type during type checking.
    /// Skips generic functions since their type params aren't in scope at module level.
    fn register_function_signature(&mut self, func: &FunctionDef) {
        // Skip generic functions — type params not in scope at module level
        if func.generic_params.is_some() {
            return;
        }

        let def_id = match self.scopes.lookup(&func.name.node) {
            Some(id) => id,
            // Equip method defs live in child scopes — fall back to span lookup
            None => match self.scopes.lookup_def_by_span(&func.name.node, func.name.span) {
                Some(id) => id,
                None => return,
            },
        };

        // Only process Function defs
        if self.scopes.get_def(def_id).kind != DefKind::Function {
            return;
        }

        // Resolve return type
        let return_type = super::types::ast_type_to_resolved(
            &func.return_type.node,
            func.return_type.span,
            self.scopes,
            self.types,
        )
        .unwrap_or(self.types.void_id);

        // Resolve parameter types
        let mut param_types = Vec::new();
        for param in &func.params {
            let type_id = super::types::ast_type_to_resolved(
                &param.node.type_.node,
                param.node.type_.span,
                self.scopes,
                self.types,
            )
            .unwrap_or(self.types.error_id);
            // Self-typed params: use the equip target type instead of error_id
            let type_id = if type_id == self.types.error_id {
                if matches!(&param.node.type_.node, crate::parser::ast::Type::SelfType) {
                    self.current_self_type.unwrap_or(type_id)
                } else {
                    type_id
                }
            } else {
                type_id
            };
            param_types.push(type_id);
        }

        // Async functions expose Future[T] as their return type at call sites
        let return_type = if func.qualifiers.is_async {
            let future_def_id = self.scopes.lookup("Future").expect("Future not registered");
            self.types.intern_generic(future_def_id, vec![return_type])
        } else {
            return_type
        };

        // Create the Function type and set it on the DefInfo
        let func_type = self.types.insert(ResolvedType::Function {
            param_ownerships: vec![crate::parser::ast::Ownership::Borrow; param_types.len()],
            params: param_types,
            return_type,
        });
        self.scopes.get_def_mut(def_id).type_id = Some(func_type);
    }

    fn check_function(&mut self, func: &FunctionDef) {
        // Set scope-aware lookup context for this function
        self.current_fn_scope = self.function_body_scopes
            .get(&(func.name.node.clone(), func.name.span.start))
            .copied();

        // Resolve return type
        let return_type = super::types::ast_type_to_resolved(
            &func.return_type.node,
            func.return_type.span,
            self.scopes,
            self.types,
        )
        .unwrap_or(self.types.void_id);

        self.current_return_type = Some(return_type);
        self.current_function_throws = func.throws.is_some();
        self.current_function_is_async = func.qualifiers.is_async;
        self.loop_depth = 0;

        // main() can only throw int (the process exit code)
        if func.name.node == "main" {
            if let Some(ref throws_type) = func.throws {
                let is_int = matches!(&throws_type.node, crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int));
                if !is_int {
                    self.error(SemanticErrorKind::MainThrowsNonInt, throws_type.span);
                }
            }
        }

        // Set trait bounds for the current function (enables transitive bound propagation)
        if let Some(def_id) = self.scopes.lookup(&func.name.node) {
            if let Some(info) = self.function_info.get(&def_id) {
                self.current_trait_bounds = info.trait_bounds.clone();
            }
        }

        // Resolve parameter types and write to DefInfo
        for param in &func.params {
            if let Ok(type_id) = super::types::ast_type_to_resolved(
                &param.node.type_.node,
                param.node.type_.span,
                self.scopes,
                self.types,
            ) {
                // Self-typed params: use the equip target type instead of error_id
                let type_id = if type_id == self.types.error_id {
                    if matches!(&param.node.type_.node, crate::parser::ast::Type::SelfType) {
                        self.current_self_type.unwrap_or(type_id)
                    } else {
                        type_id
                    }
                } else {
                    type_id
                };
                if let Some(def_id) = self.scopes.lookup_def_by_span(
                    &param.node.name.node,
                    param.node.name.span,
                ) {
                    self.scopes.get_def_mut(def_id).type_id = Some(type_id);
                }
            }
        }

        match &func.body {
            FunctionBody::Block(block) => {
                self.check_block(block);
            }
            FunctionBody::Expression(expr) => {
                let expr_type = self.infer_expr(expr);
                self.unify(return_type, expr_type, expr.span);
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        self.current_return_type = None;
        self.current_function_throws = false;
        self.current_fn_scope = None;
        self.current_trait_bounds = Vec::new();
    }

    /// If `arg_expr` is a closure and `param_type` is a Callable variant,
    /// classify the closure's kind and validate compatibility.
    fn validate_closure_arg_kind(&mut self, param_type: TypeId, arg_expr: &Spanned<Expr>) {
        if let Expr::Closure { is_move, params: closure_params, body, .. } = &arg_expr.node {
            let resolved = self.resolve_type(param_type);
            let expected = match self.types.get(resolved) {
                ResolvedType::CallableTrait(_) => ClosureKind::Callable,
                ResolvedType::MutCallableTrait(_) => ClosureKind::MutCallable,
                ResolvedType::ConsumeCallableTrait(_) => ClosureKind::ConsumeCallable,
                ResolvedType::BoxedCallable { kind, .. } => *kind,
                _ => return,
            };
            let actual = classify_closure_kind(*is_move, closure_params, body);
            if !actual.is_compatible_with(expected) {
                self.error(
                    SemanticErrorKind::ClosureKindMismatch {
                        expected: expected.name().to_string(),
                        found: actual.name().to_string(),
                    },
                    arg_expr.span,
                );
            }
        }
    }
}

/// Map an AST `Type` to its Gorget-level type name for trait bound checking.
fn ast_type_to_gorget_name(ty: &Type) -> Option<String> {
    match ty {
        Type::Named { name, .. } => Some(name.node.clone()),
        Type::Primitive(p) => {
            let s = match p {
                PrimitiveType::Int => "int",
                PrimitiveType::Float => "float",
                PrimitiveType::Bool => "bool",
                PrimitiveType::Str | PrimitiveType::StringType => "str",
                PrimitiveType::Void => "void",
                _ => return None,
            };
            Some(s.to_string())
        }
        _ => None,
    }
}

/// Run type checking on the entire module.
/// Returns (expr_types, method_resolutions):
/// - expr_types: span → inferred TypeId (for Result-based `?` codegen)
/// - method_resolutions: method span start → DefId (for borrow checker origin tracking)
pub fn check_module(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    traits: &TraitRegistry,
    resolution_map: &ResolutionMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    enum_variants: &FxHashMap<DefId, EnumVariantInfo>,
    struct_fields: &FxHashMap<DefId, StructFieldInfo>,
    function_body_scopes: &FxHashMap<(String, usize), ScopeId>,
    errors: &mut Vec<SemanticError>,
) -> (FxHashMap<Span, TypeId>, FxHashMap<usize, DefId>) {
    let mut checker = TypeChecker::new(scopes, types, traits, resolution_map, function_info, enum_variants, struct_fields, function_body_scopes);

    // Pre-pass: register function signatures so callers can infer return types.
    // This must run before body checking so that e.g. `auto x = imported_fn()`
    // can resolve the function's type.
    register_signatures_recursive(&mut checker, &module.items);

    check_items_recursive_tc(&mut checker, &module.items);

    // Resolve type variables in DefInfos so codegen sees concrete types.
    // Uses deep resolution to handle composite types like Function([Var, Var], Var).
    for i in 0..checker.scopes.def_count() {
        let def_id = DefId(i as u32);
        if let Some(tid) = checker.scopes.get_def(def_id).type_id {
            let resolved = checker.resolve_type_deep(tid);
            if resolved != tid {
                checker.scopes.get_def_mut(def_id).type_id = Some(resolved);
            }
        }
    }

    errors.extend(checker.errors);
    (checker.expr_types, checker.method_resolutions)
}

/// Recursively register function signatures, descending into `Item::Module` wrappers
/// so that imported module code has type information available.
fn register_signatures_recursive(checker: &mut TypeChecker, items: &[Spanned<Item>]) {
    for item in items {
        match &item.node {
            Item::Module { items: inner, .. } => {
                register_signatures_recursive(checker, inner);
            }
            Item::Function(f) => {
                checker.register_function_signature(f);
            }
            Item::Equip(impl_block) => {
                let has_generics = impl_block.generic_params.is_some();
                if let Some(generics) = &impl_block.generic_params {
                    checker.scopes.push_scope(ScopeKind::EquipBlock { self_type: None });
                    for param in &generics.node.params {
                        if let GenericParam::Type { name, .. } = &param.node {
                            let _ = checker.scopes.define(
                                name.node.clone(), DefKind::GenericParam, name.span,
                            );
                        }
                    }
                }
                checker.current_self_type = types::ast_type_to_resolved(
                    &impl_block.type_.node,
                    impl_block.type_.span,
                    checker.scopes,
                    checker.types,
                ).ok();
                for method in &impl_block.items {
                    checker.register_function_signature(&method.node);
                }
                checker.current_self_type = None;
                if has_generics {
                    checker.scopes.pop_scope();
                }
            }
            _ => {}
        }
    }
}

/// Recursively type-check items, descending into `Item::Module` wrappers
/// so that imported module code populates `expr_types` and `method_resolutions`.
fn check_items_recursive_tc(checker: &mut TypeChecker, items: &[Spanned<Item>]) {
    for item in items {
        match &item.node {
            Item::Module { items: inner, .. } => {
                // Type-check imported module code to populate expr_types/method_resolutions
                // but discard any type errors — library code may have false positives
                // in a foreign scope context.
                let error_count = checker.errors.len();
                check_items_recursive_tc(checker, inner);
                checker.errors.truncate(error_count);
            }
            Item::Function(f) => {
                checker.check_function(f);
            }
            Item::Equip(impl_block) => {
                let has_generics = impl_block.generic_params.is_some();
                if let Some(generics) = &impl_block.generic_params {
                    checker.scopes.push_scope(ScopeKind::EquipBlock { self_type: None });
                    for param in &generics.node.params {
                        if let GenericParam::Type { name, .. } = &param.node {
                            let _ = checker.scopes.define(
                                name.node.clone(), DefKind::GenericParam, name.span,
                            );
                        }
                    }
                }
                checker.current_self_type = types::ast_type_to_resolved(
                    &impl_block.type_.node,
                    impl_block.type_.span,
                    checker.scopes,
                    checker.types,
                ).ok();
                for method in &impl_block.items {
                    checker.check_function(&method.node);
                }
                checker.current_self_type = None;
                if has_generics {
                    checker.scopes.pop_scope();
                }
            }
            Item::ConstDecl(c) => {
                checker.infer_expr(&c.value);
            }
            Item::StaticDecl(s) => {
                checker.infer_expr(&s.value);
            }
            Item::Test(t) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                checker.check_block(&t.body);
                checker.current_return_type = None;
            }
            Item::Bench(b) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                checker.check_block(&b.body);
                checker.current_return_type = None;
            }
            Item::SuiteSetup(s) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                checker.check_block(&s.body);
                checker.current_return_type = None;
            }
            Item::SuiteTeardown(s) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                checker.check_block(&s.body);
                checker.current_return_type = None;
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::semantic;

    fn check(source: &str) -> Vec<super::SemanticError> {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze(&mut module, &[]);
        result.errors
    }

    #[test]
    fn simple_function_no_errors() {
        let errors = check("int add(int a, int b) = a + b\n");
        // May have some unresolved type errors but shouldn't panic
        let _ = errors;
    }

    #[test]
    fn auto_inference() {
        let errors = check("void main():\n    auto x = 5\n    auto s = \"hello\"\n");
        // Should not produce inference errors for simple cases
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::CannotInferType
            )),
            "unexpected CannotInferType error"
        );
    }

    #[test]
    fn bool_condition_check() {
        // This should work — condition is bool
        let errors = check("void main():\n    if true:\n        pass\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::TypeMismatch { .. }
            )),
            "unexpected TypeMismatch: {:?}",
            errors
        );
    }

    #[test]
    fn expression_body_type_check() {
        let errors = check("int double(int x) = x * 2\n");
        // int * int should unify fine
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::TypeMismatch { .. }
            )),
            "unexpected errors: {:?}",
            errors
        );
    }

    #[test]
    fn array_literal_types() {
        let errors = check("void main():\n    auto nums = [1, 2, 3]\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::TypeMismatch { .. }
            )),
            "unexpected errors: {:?}",
            errors
        );
    }

    #[test]
    fn interpolation_struct_rejected() {
        let errors = check(
            "struct Foo:\n    int x\nvoid main():\n    Foo f = Foo(1)\n    print(f\"{f}\")\n",
        );
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonPrintableInterpolation { .. }
            )),
            "expected NonPrintableInterpolation error, got: {:?}",
            errors
        );
    }

    #[test]
    fn interpolation_enum_rejected() {
        let errors = check(
            "enum Color:\n    Red()\n    Blue()\nvoid main():\n    Color c = Red()\n    print(f\"{c}\")\n",
        );
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonPrintableInterpolation { .. }
            )),
            "expected NonPrintableInterpolation error, got: {:?}",
            errors
        );
    }

    #[test]
    fn interpolation_primitives_ok() {
        let errors = check("void main():\n    int x = 42\n    print(f\"{x}\")\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonPrintableInterpolation { .. }
            )),
            "unexpected NonPrintableInterpolation error: {:?}",
            errors
        );
    }

    #[test]
    fn trait_bound_satisfied() {
        let source = "\
trait Printable:
    str show(self)

struct Num:
    int val

equip Num with Printable:
    str show(self):
        return \"num\"

T echo[Printable T](T x):
    return x

void main():
    Num n = Num(42)
    Num m = echo[Num](n)
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnsatisfiedTraitBound { .. }
            )),
            "unexpected UnsatisfiedTraitBound error: {:?}",
            errors
        );
    }

    #[test]
    fn trait_bound_unsatisfied() {
        let source = "\
trait Printable:
    str show(self)

struct Point:
    int x

T echo[Printable T](T x):
    return x

void main():
    Point p = Point(1)
    Point q = echo[Point](p)
";
        let errors = check(source);
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnsatisfiedTraitBound {
                    type_name, trait_name, ..
                } if type_name == "Point" && trait_name == "Printable"
            )),
            "expected UnsatisfiedTraitBound error for Point/Printable, got: {:?}",
            errors
        );
    }

    #[test]
    fn trait_bound_multiple_traits() {
        let source = "\
trait A:
    void a(self)

trait B:
    void b(self)

struct Foo:
    int x

equip Foo with A:
    void a(self):
        pass

T need_ab[A & B T](T x):
    return x

void main():
    Foo f = Foo(1)
    Foo g = need_ab[Foo](f)
";
        let errors = check(source);
        // Foo implements A but not B
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnsatisfiedTraitBound {
                    type_name, trait_name, ..
                } if type_name == "Foo" && trait_name == "B"
            )),
            "expected UnsatisfiedTraitBound for Foo/B, got: {:?}",
            errors
        );
        // Should NOT have error for trait A
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnsatisfiedTraitBound {
                    trait_name, ..
                } if trait_name == "A"
            )),
            "unexpected UnsatisfiedTraitBound for A: {:?}",
            errors
        );
    }

    #[test]
    fn trait_bound_no_where_clause_no_regression() {
        let source = "\
T identity[T](T x) = x

void main():
    int y = identity[int](42)
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnsatisfiedTraitBound { .. }
            )),
            "unexpected UnsatisfiedTraitBound: {:?}",
            errors
        );
    }

    // ── Match exhaustiveness tests ──

    #[test]
    fn match_exhaustive_all_variants_covered() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        case Green():
            pass
        case Blue():
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_non_exhaustive_missing_variant() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        case Green():
            pass
";
        let errors = check(source);
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { missing_variants }
                    if missing_variants == &["Blue"]
            )),
            "expected NonExhaustiveMatch with Blue, got: {:?}",
            errors
        );
    }

    #[test]
    fn match_exhaustive_with_else() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        else:
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_exhaustive_with_wildcard() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        case _:
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_exhaustive_with_binding_catchall() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        case other:
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_guarded_arm_not_exhaustive() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red():
            pass
        case Green():
            pass
        case Blue() if false:
            pass
";
        let errors = check(source);
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { missing_variants }
                    if missing_variants == &["Blue"]
            )),
            "expected NonExhaustiveMatch with Blue (guarded arm), got: {:?}",
            errors
        );
    }

    #[test]
    fn match_or_pattern_covers_multiple() {
        let source = "\
enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    match c:
        case Red() | Green():
            pass
        case Blue():
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_constructor_covers_variant() {
        let source = "\
enum Shape:
    Circle(float)
    Rect(float, float)

void main():
    Shape s = Circle(1.0)
    match s:
        case Circle(r):
            pass
        case Rect(w, h):
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error: {:?}",
            errors
        );
    }

    #[test]
    fn match_int_no_exhaustiveness_check() {
        let source = "\
void main():
    int x = 5
    match x:
        case 1:
            pass
        case 2:
            pass
";
        let errors = check(source);
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::NonExhaustiveMatch { .. }
            )),
            "unexpected NonExhaustiveMatch error for int: {:?}",
            errors
        );
    }

    #[test]
    fn unknown_directive_error() {
        let errors = check("directive foo-bar\nvoid main():\n    pass\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnknownDirective { name } if name == "foo-bar"
            )),
            "expected UnknownDirective error, got: {:?}",
            errors
        );
    }

    #[test]
    fn valid_directives_no_error() {
        let errors = check("directive strip-asserts\ndirective overflow=wrap\nvoid main():\n    pass\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::UnknownDirective { .. }
            )),
            "unexpected UnknownDirective error: {:?}",
            errors
        );
    }

    #[test]
    fn int_literal_range_uint8_overflow() {
        let errors = check("void main():\n    uint8 x = 256\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::ValueOutOfRange { value: 256, .. }
            )),
            "expected ValueOutOfRange for uint8 = 256, got: {:?}",
            errors
        );
    }

    #[test]
    fn int_literal_range_int8_overflow() {
        let errors = check("void main():\n    int8 x = 128\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::ValueOutOfRange { value: 128, .. }
            )),
            "expected ValueOutOfRange for int8 = 128, got: {:?}",
            errors
        );
    }

    #[test]
    fn int_literal_range_int8_neg_overflow() {
        let errors = check("void main():\n    int8 x = -129\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::ValueOutOfRange { value: -129, .. }
            )),
            "expected ValueOutOfRange for int8 = -129, got: {:?}",
            errors
        );
    }

    #[test]
    fn int_literal_range_uint8_negative() {
        let errors = check("void main():\n    uint8 x = -1\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::ValueOutOfRange { value: -1, .. }
            )),
            "expected ValueOutOfRange for uint8 = -1, got: {:?}",
            errors
        );
    }

    #[test]
    fn int_literal_range_valid_no_error() {
        let errors = check("void main():\n    uint8 a = 255\n    int8 b = -128\n    uint16 c = 0\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::ValueOutOfRange { .. }
            )),
            "unexpected ValueOutOfRange error: {:?}",
            errors
        );
    }

    // ── Async/Await tests ──

    #[test]
    fn async_fn_returns_future() {
        // An async function should type-check without errors
        let errors = check("async int fetch():\n    return 42\n");
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::TypeMismatch { .. }
            )),
            "unexpected TypeMismatch for async function: {:?}",
            errors
        );
    }

    #[test]
    fn await_extracts_type() {
        // await on an async call inside an async function should work
        let errors = check(
            "async int fetch():\n    return 1\nasync int caller():\n    return fetch().await()\n"
        );
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::AwaitNonFuture
                | super::SemanticErrorKind::AwaitOutsideAsync
            )),
            "unexpected async/await error: {:?}",
            errors
        );
    }

    #[test]
    fn await_outside_async_rejected() {
        let errors = check(
            "async int fetch():\n    return 1\nint caller():\n    return fetch().await()\n"
        );
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::AwaitOutsideAsync
            )),
            "expected AwaitOutsideAsync error, got: {:?}",
            errors
        );
    }

    #[test]
    fn double_await_rejected() {
        let errors = check(
            "async int fetch():\n    return 1\nasync void caller():\n    int x = await fetch().await()\n"
        );
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::DoubleAwait
            )),
            "expected DoubleAwait error, got: {:?}",
            errors
        );
    }

    #[test]
    fn await_non_future_rejected() {
        let errors = check("async int caller():\n    return 42.await()\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::AwaitNonFuture
            )),
            "expected AwaitNonFuture error, got: {:?}",
            errors
        );
    }

    #[test]
    fn spawn_returns_task() {
        // spawn on an async call should produce Task[T] — no type errors expected
        let errors = check(
            "async int fetch():\n    return 1\nasync void caller():\n    auto t = spawn fetch()\n"
        );
        assert!(
            !errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::SpawnNonFuture
            )),
            "unexpected SpawnNonFuture error: {:?}",
            errors
        );
    }

    #[test]
    fn spawn_non_future_rejected() {
        let errors = check("async void caller():\n    auto t = spawn 42\n");
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                super::SemanticErrorKind::SpawnNonFuture
            )),
            "expected SpawnNonFuture error, got: {:?}",
            errors
        );
    }
}
