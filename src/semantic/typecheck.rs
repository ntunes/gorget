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
/// Walk an AST `Type` and return true if any `Type::Named { name, [] }`
/// nested anywhere inside has the given name. Used by shape-2 inference
/// to detect when a method-level generic appears only in the return type
/// (e.g. the `U` in `Vector[U] map[U, F](self, F f)`).
/// Structurally match `template` against `concrete` and record bindings
/// for each bare `Named { name, [] }` in `template` whose name is listed
/// in `generic_params`. Handles the typical shape used in equip blocks:
/// template `VectorIter[T]` paired with concrete `VectorIter[int]` binds
/// `T → int`. Also handles nested shapes like `TakeIter[VectorIter[T], T]`
/// vs `TakeIter[VectorIter[int], int]`.
fn bind_template_generics(
    template: &Type,
    concrete: &Type,
    generic_params: &[String],
    bindings: &mut FxHashMap<String, Type>,
) {
    // Bare generic-param name at this position: bind directly.
    if let Type::Named { name, generic_args } = template {
        if generic_args.is_empty()
            && generic_params.iter().any(|p| p == &name.node)
        {
            bindings.entry(name.node.clone()).or_insert_with(|| concrete.clone());
            return;
        }
    }
    // Otherwise recurse through matching shapes.
    match (template, concrete) {
        (
            Type::Named { name: t_name, generic_args: t_args },
            Type::Named { name: c_name, generic_args: c_args },
        ) if t_name.node == c_name.node && t_args.len() == c_args.len() => {
            for (t, c) in t_args.iter().zip(c_args.iter()) {
                bind_template_generics(&t.node, &c.node, generic_params, bindings);
            }
        }
        (Type::Tuple(t_elems), Type::Tuple(c_elems)) if t_elems.len() == c_elems.len() => {
            for (t, c) in t_elems.iter().zip(c_elems.iter()) {
                bind_template_generics(&t.node, &c.node, generic_params, bindings);
            }
        }
        (Type::Ref(t), Type::Ref(c))
        | (Type::Owned(t), Type::Owned(c))
        | (Type::Pointer(t), Type::Pointer(c)) => {
            bind_template_generics(&t.node, &c.node, generic_params, bindings);
        }
        _ => {}
    }
}

fn type_mentions_name(ty: &Type, target: &str) -> bool {
    match ty {
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() && name.node == target {
                return true;
            }
            generic_args.iter().any(|a| type_mentions_name(&a.node, target))
        }
        Type::Tuple(elems) => elems.iter().any(|e| type_mentions_name(&e.node, target)),
        Type::Array { element, .. } | Type::Slice { element } => {
            type_mentions_name(&element.node, target)
        }
        Type::Function { return_type, params, .. } => {
            type_mentions_name(&return_type.node, target)
                || params.iter().any(|p| type_mentions_name(&p.node, target))
        }
        Type::Ref(inner) | Type::Owned(inner) | Type::Pointer(inner) => {
            type_mentions_name(&inner.node, target)
        }
        Type::Primitive(_) | Type::SelfType | Type::Inferred => false,
    }
}

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

/// Returns (bit_width, is_signed) for integer primitive types.
fn int_width_signed(prim: &PrimitiveType) -> Option<(u8, bool)> {
    match prim {
        PrimitiveType::Int8 => Some((8, true)),
        PrimitiveType::Int16 => Some((16, true)),
        PrimitiveType::Int32 => Some((32, true)),
        PrimitiveType::Int | PrimitiveType::Int64 => Some((64, true)),
        PrimitiveType::Uint8 => Some((8, false)),
        PrimitiveType::Uint16 => Some((16, false)),
        PrimitiveType::Uint32 => Some((32, false)),
        PrimitiveType::Uint | PrimitiveType::Uint64 => Some((64, false)),
        _ => None,
    }
}

/// Returns true if `from` can safely widen to `to` without data loss.
/// Safe widening: smaller signed → larger signed, smaller unsigned → larger unsigned,
/// and smaller unsigned → strictly larger signed (unsigned always fits).
fn is_safe_integer_widening(from: &PrimitiveType, to: &PrimitiveType) -> bool {
    match (int_width_signed(from), int_width_signed(to)) {
        (Some((from_bits, from_signed)), Some((to_bits, to_signed))) => {
            if from_bits == to_bits && from_signed == to_signed {
                return true; // same type (e.g. Int and Int64)
            }
            if from_bits >= to_bits && !(from_bits == to_bits && from_signed == to_signed) {
                return false; // narrowing is never safe
            }
            // from_bits < to_bits
            match (from_signed, to_signed) {
                (true, true) => true,    // signed → larger signed
                (false, false) => true,  // unsigned → larger unsigned
                (false, true) => true,   // unsigned → larger signed (always fits)
                (true, false) => false,  // signed → unsigned (may lose sign)
            }
        }
        _ => false,
    }
}

/// Format a PrimitiveType as its Gorget source name.
fn describe_primitive(prim: &PrimitiveType) -> String {
    match prim {
        PrimitiveType::Int => "int".to_string(),
        PrimitiveType::Int8 => "int8".to_string(),
        PrimitiveType::Int16 => "int16".to_string(),
        PrimitiveType::Int32 => "int32".to_string(),
        PrimitiveType::Int64 => "int64".to_string(),
        PrimitiveType::Uint => "uint".to_string(),
        PrimitiveType::Uint8 => "uint8".to_string(),
        PrimitiveType::Uint16 => "uint16".to_string(),
        PrimitiveType::Uint32 => "uint32".to_string(),
        PrimitiveType::Uint64 => "uint64".to_string(),
        PrimitiveType::Float32 => "float32".to_string(),
        PrimitiveType::Float64 => "float64".to_string(),
        PrimitiveType::Float => "float".to_string(),
        PrimitiveType::Bool => "bool".to_string(),
        PrimitiveType::StringType => "String".to_string(),
        PrimitiveType::CStr => "cstr".to_string(),
        PrimitiveType::Void => "void".to_string(),
    }
}

/// Occurs check: returns true if type variable `var_id` appears anywhere in `type_id`.
fn occurs_in(
    var_id: u32,
    type_id: TypeId,
    types: &super::types::TypeTable,
    substitutions: &FxHashMap<u32, TypeId>,
) -> bool {
    match types.get(type_id) {
        ResolvedType::Var(v) => {
            if *v == var_id {
                return true;
            }
            if let Some(&sub) = substitutions.get(v) {
                occurs_in(var_id, sub, types, substitutions)
            } else {
                false
            }
        }
        ResolvedType::Generic(_, args) => {
            args.iter().any(|&a| occurs_in(var_id, a, types, substitutions))
        }
        ResolvedType::Tuple(elems) => {
            elems.iter().any(|&e| occurs_in(var_id, e, types, substitutions))
        }
        ResolvedType::Array(elem, _) | ResolvedType::Slice(elem) => {
            occurs_in(var_id, *elem, types, substitutions)
        }
        ResolvedType::Function { params, return_type, .. } => {
            params.iter().any(|&p| occurs_in(var_id, p, types, substitutions))
                || occurs_in(var_id, *return_type, types, substitutions)
        }
        ResolvedType::CallableTrait(inner)
        | ResolvedType::MutCallableTrait(inner)
        | ResolvedType::ConsumeCallableTrait(inner) => {
            occurs_in(var_id, *inner, types, substitutions)
        }
        ResolvedType::BoxedCallable { inner, .. } => {
            occurs_in(var_id, *inner, types, substitutions)
        }
        _ => false, // Primitive, Defined, TraitObject, Error, Void, Never
    }
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
    /// Generic type parameter bounds for structs/enums (from resolve).
    struct_generic_bounds: &'a FxHashMap<DefId, (Vec<String>, Vec<(String, Vec<String>)>)>,
    /// Side-table of inferred method-level generic args, keyed on the
    /// MethodCall expression's `span.start`. Populated whenever the typecheck
    /// MethodCall arm successfully infers all method-level generic params
    /// from the call's arg types. A post-typecheck rewriter copies these
    /// into `MethodCall.generic_args` so the IR-lowering / generic-collector
    /// path picks them up via the same code path as explicit `[T1, T2]` args.
    inferred_method_targs: FxHashMap<usize, Vec<Type>>,
    /// Side-table of inferred type-args for *generic free-function* calls,
    /// keyed on the callee Identifier's span start. Populated when a generic
    /// function is invoked without explicit `[T, ...]` args and the unifier
    /// successfully binds every type-param via the args + LHS context. Pass
    /// 4.5 patches `Expr::Call.generic_args` from this map so IR-lowering's
    /// monomorphisation can pick the mangled symbol.
    inferred_call_targs: FxHashMap<usize, Vec<Type>>,
    /// Side-table of inference *failures*, keyed on `method.span.start`.
    /// Records (unresolved_param_name, reason) for each call-site where
    /// inference was attempted (method-level generic + no explicit args)
    /// but couldn't resolve all generic params. Read at the NoMethodFound
    /// emission site to swap the generic error for a typed
    /// `MethodGenericInferenceFailed` that points at the specific
    /// unresolved param. See `docs/internals/method-level-inference.md`
    /// risk #3 for the design.
    inference_failures: FxHashMap<usize, (String, String)>,
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
        struct_generic_bounds: &'a FxHashMap<DefId, (Vec<String>, Vec<(String, Vec<String>)>)>,
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
            struct_generic_bounds,
            inferred_method_targs: FxHashMap::default(),
            inferred_call_targs: FxHashMap::default(),
            inference_failures: FxHashMap::default(),
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
        self.resolve_type_inner(id, 0)
    }

    fn resolve_type_inner(&self, id: TypeId, depth: u32) -> TypeId {
        if depth > 100 {
            return self.types.error_id;
        }
        match self.types.get(id) {
            ResolvedType::Var(var_id) => {
                if let Some(&sub) = self.substitutions.get(var_id) {
                    self.resolve_type_inner(sub, depth + 1)
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

    /// Extract `param_ownerships` from a type hint, unwrapping through
    /// Callable/MutCallable/ConsumeCallable/BoxedCallable wrappers.
    fn extract_function_ownerships(&self, hint: Option<TypeId>) -> Option<Vec<crate::parser::ast::Ownership>> {
        let hint = hint?;
        let resolved = self.resolve_type(hint);
        match self.types.get(resolved) {
            ResolvedType::Function { param_ownerships, .. } => Some(param_ownerships.clone()),
            ResolvedType::CallableTrait(inner)
            | ResolvedType::MutCallableTrait(inner)
            | ResolvedType::ConsumeCallableTrait(inner)
            | ResolvedType::BoxedCallable { inner, .. } => {
                match self.types.get(*inner) {
                    ResolvedType::Function { param_ownerships, .. } => Some(param_ownerships.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn extract_fn_return_type_from_hint(&self, hint: Option<TypeId>) -> Option<TypeId> {
        let hint = hint?;
        let resolved = self.resolve_type(hint);
        match self.types.get(resolved) {
            ResolvedType::Function { return_type, .. } => Some(*return_type),
            ResolvedType::CallableTrait(inner)
            | ResolvedType::MutCallableTrait(inner)
            | ResolvedType::ConsumeCallableTrait(inner)
            | ResolvedType::BoxedCallable { inner, .. } => {
                match self.types.get(*inner) {
                    ResolvedType::Function { return_type, .. } => Some(*return_type),
                    _ => None,
                }
            }
            _ => None,
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

    /// Check if a resolved type is an enum (for cast validation).
    fn is_enum_type(&self, type_id: TypeId) -> bool {
        match self.types.get(type_id) {
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                self.enum_variants.contains_key(def_id)
            }
            _ => false,
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
                if occurs_in(*var_id, b, self.types, &self.substitutions) {
                    return error_id;
                }
                self.substitutions.insert(*var_id, b);
                b
            }
            (_, ResolvedType::Var(var_id)) => {
                if occurs_in(*var_id, a, self.types, &self.substitutions) {
                    return error_id;
                }
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
                    param_ownerships: a_own,
                },
                ResolvedType::Function {
                    params: b_params,
                    return_type: b_ret,
                    param_ownerships: b_own,
                },
            ) if a_params.len() == b_params.len() => {
                // Check ownership compatibility when both sides carry ownership info
                if !a_own.is_empty() && !b_own.is_empty()
                    && a_own.len() == b_own.len()
                    && a_own != b_own
                {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: self.describe_resolved_type(a),
                            found: self.describe_resolved_type(b),
                        },
                        span,
                    );
                    return self.types.error_id;
                }
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
            // Integer coercion: only allow safe widening (no data loss).
            // Narrowing or signed↔unsigned conversions require explicit `as`.
            (ResolvedType::Primitive(a_prim), ResolvedType::Primitive(b_prim))
                if is_integer_type(a_prim) && is_integer_type(b_prim) =>
            {
                // a = expected, b = found/actual. Check if found safely widens to expected.
                if is_safe_integer_widening(b_prim, a_prim) {
                    a
                } else {
                    self.error(
                        SemanticErrorKind::UnsafeIntegerConversion {
                            from: describe_primitive(b_prim),
                            to: describe_primitive(a_prim),
                        },
                        span,
                    );
                    a
                }
            }
            // cstr ↔ String coercion
            (ResolvedType::Primitive(PrimitiveType::CStr), ResolvedType::Primitive(PrimitiveType::StringType))
            | (ResolvedType::Primitive(PrimitiveType::StringType), ResolvedType::Primitive(PrimitiveType::CStr)) => {
                a
            }
            // Auto-deref coercion: Ref(T) ↔ T — borrowed reference auto-dereferences
            (ResolvedType::Ref(inner), _) => {
                self.unify(*inner, b, span);
                a // keep the expected (lhs) type
            }
            (_, ResolvedType::Ref(inner)) => {
                self.unify(a, *inner, span);
                a
            }
            // Owned(T) ↔ T — owned annotation is transparent for unification
            (ResolvedType::Owned(inner), _) => {
                self.unify(*inner, b, span);
                a
            }
            (_, ResolvedType::Owned(inner)) => {
                self.unify(a, *inner, span);
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
            Expr::StringLiteral(s, interp_exprs) => {
                use crate::lexer::token::StringSegment;
                // Typecheck each pre-parsed interpolation expression so method
                // calls inside `f"{...}"` get their generic args inferred and
                // mangled symbols recorded — same pipeline as any other
                // expression. Errors fired here are *suppressed*: the prior
                // lowering path re-parsed and lowered segments without
                // typecheck pre-flight, so polymorphic stdlib calls like
                // `abs(-2.5)` (which trip the unifier the same way the
                // bound-to-local form does) used to compile via the
                // IR-lowering dispatch path. Suppressing errors keeps existing
                // fixtures green while still threading inferred targs into
                // `inferred_method_targs` for Pass 4.5 sync. The simple
                // identifier path below remains for the Displayable
                // diagnostic on bare-name segments.
                let saved_err_len = self.errors.len();
                for interp in interp_exprs {
                    let _ = self.infer_expr(interp);
                }
                self.errors.truncate(saved_err_len);
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
                // If there's a type hint and it's Option[T], use it directly.
                if let Some(hint) = self.decl_type_hint {
                    let resolved = self.resolve_type(hint);
                    let is_option = match self.types.get(resolved) {
                        ResolvedType::Generic(def_id, args) if args.len() == 1 => {
                            self.scopes.get_def(*def_id).name == "Option"
                        }
                        _ => false,
                    };
                    if is_option {
                        return hint;
                    }
                }
                // No usable hint — create Option[?T] with a fresh type variable.
                if let Some(option_def_id) = self.scopes.lookup("Option") {
                    let var_id = self.next_type_var;
                    self.next_type_var += 1;
                    let fresh = self.types.fresh_var(var_id);
                    self.types.intern_generic(option_def_id, vec![fresh])
                } else {
                    self.types.error_id
                }
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
                                if name.node == "cap" {
                                    // cap= valid on collection constructors — validated at GIR lowering
                                    self.infer_expr(&arg.node.value);
                                } else if name.node != "alloc" {
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
                        // Fresh-instantiate any generic-param `Defined` references so
                        // unification binds them per call site (rather than treating
                        // T/U/E as concrete types that conflict across calls). The
                        // signature was registered with `Defined(generic_param_def_id)`
                        // placeholders; here we replace each unique generic-param
                        // DefId with a fresh `Var`, sharing the same fresh var across
                        // all positions so `Result[T, E] → Result[U, E]` correctly
                        // links the two `E`s.
                        let mut subst: FxHashMap<DefId, TypeId> = FxHashMap::default();
                        let params: Vec<TypeId> = params.iter()
                            .map(|&t| self.instantiate_generic_params(t, &mut subst))
                            .collect();
                        let return_type = self.instantiate_generic_params(return_type, &mut subst);
                        let was_generic_call = !subst.is_empty();

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
                        // If the callee was a generic free function and no
                        // explicit `[T, ...]` args were supplied, record the
                        // inferred type-args (resolved from the per-call subst)
                        // so Pass 4.5 can patch `Call.generic_args`. Without
                        // this, IR-lowering's monomorphisation has no
                        // mangled symbol to dispatch to and link-fails.
                        let already_has = generic_args.as_ref()
                            .map(|gs| !gs.is_empty())
                            .unwrap_or(false);
                        if was_generic_call && !already_has {
                            if let Expr::Identifier(cname) = &callee.node {
                                if let Some(callee_def_id) = self.resolve_name(callee.span.start, cname) {
                                    if let Some(info) = self.function_info.get(&callee_def_id).cloned() {
                                        let mut ast_targs: Vec<Type> = Vec::with_capacity(info.generic_param_names.len());
                                        let mut all_resolved = true;
                                        for param_name in &info.generic_param_names {
                                            // Find this param's DefId in the per-call subst
                                            // (subst is keyed by GenericParam DefId; the
                                            // scratch DefIds were created in
                                            // register_function_signature and persist via
                                            // the Function type's `Defined(def_id)` slots).
                                            let matching: Option<DefId> = subst.keys()
                                                .copied()
                                                .find(|d| self.scopes.get_def(*d).name == *param_name);
                                            let Some(d) = matching else {
                                                all_resolved = false;
                                                break;
                                            };
                                            let fresh_var = subst[&d];
                                            let resolved_tid = self.resolve_type_deep(fresh_var);
                                            // Bail if any param is still an unbound Var or Error.
                                            match self.types.get(resolved_tid) {
                                                ResolvedType::Var(_) | ResolvedType::Error => {
                                                    all_resolved = false;
                                                    break;
                                                }
                                                _ => {}
                                            }
                                            let Some(ast) = self.typeid_to_ast_type(resolved_tid) else {
                                                all_resolved = false;
                                                break;
                                            };
                                            ast_targs.push(ast);
                                        }
                                        if all_resolved && ast_targs.len() == info.generic_param_names.len() {
                                            self.inferred_call_targs.insert(callee.span.start, ast_targs);
                                        }
                                    }
                                }
                            }
                        }
                        // `noreturn` extern functions never return — the call's
                        // type is `Never`, so it composes with any expected
                        // type via `unify` (e.g., as a divergent match arm).
                        if func_info.map_or(false, |fi| fi.is_noreturn) {
                            self.types.never_id
                        } else {
                            return_type
                        }
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
                generic_args,
            } => {
                // Static method calls on type names: int.parse(), float.default()
                if let Expr::Identifier(name) = &receiver.node {
                    if let Some(ret) = self.resolve_static_method_type(name, &method.node, args, expr.span) {
                        return ret;
                    }
                }

                let receiver_type = self.infer_expr(receiver);
                let resolved_receiver = self.resolve_type(receiver_type);

                // Method-level generic inference (Phase 2c — runs before
                // the dispatch fork because user-space wrappers like
                // `equip [T] Vector[T]: void each[F](...)` may live in a
                // generic-template equip block whose registered self_type
                // doesn't match the concrete receiver's TypeId. The
                // TypeId-keyed `resolve_method` then misses the impl,
                // dispatch falls through to `builtin_method_type`, and
                // inference would never run if it were gated on the
                // success of `resolve_method`. The shape lookup falls
                // back to the receiver's BASE NAME so it catches both
                // direct-TypeId and generic-template impls.
                let needs_inference = generic_args.as_ref()
                    .map(|gs| gs.is_empty())
                    .unwrap_or(true);
                if needs_inference {
                    let shape_clone = self.traits
                        .resolve_method_shape(resolved_receiver, &method.node)
                        .or_else(|| {
                            let base_name = match self.types.get(resolved_receiver) {
                                ResolvedType::Generic(def_id, _)
                                | ResolvedType::Defined(def_id) => {
                                    Some(self.scopes.get_def(*def_id).name.clone())
                                }
                                _ => None,
                            }?;
                            self.traits.resolve_method_shape_by_name(&base_name, &method.node)
                        })
                        .cloned();
                    if let Some(shape) = shape_clone {
                        // Pre-infer arg types for inference. These are
                        // re-used by the typed dispatch path below; the
                        // untyped paths re-infer them, which is wasteful
                        // but harmless (idempotent + cached via expr_types).
                        let mut arg_types: Vec<TypeId> = Vec::with_capacity(args.len());
                        for arg in args.iter() {
                            arg_types.push(self.infer_expr(&arg.node.value));
                        }
                        if let Some(inferred) = self.try_infer_method_targs(&shape, &arg_types, method.span.start) {
                            self.inferred_method_targs.insert(method.span.start, inferred);
                        }
                    }
                }

                // Try to resolve method via trait registry
                if let Some((def_id, sig)) =
                    self.traits.resolve_method(resolved_receiver, &method.node)
                {
                    self.method_resolutions.insert(method.span.start, *def_id);
                    let stored_def_id = *def_id;
                    let mut sig = sig.clone();
                    // Trait-default substitution: when resolve_method falls
                    // through to the trait default-body fallback, the
                    // returned sig references `Self` and the trait's own
                    // generic `T` as placeholders (both erased to error_id
                    // at registry-build time since Self/trait-T are out of
                    // scope). Rebuild the sig against the concrete receiver
                    // so adapter constructors returning `TakeIter[Self, T]`
                    // (etc.) resolve to the concrete iterator type and
                    // subsequent chained calls dispatch correctly.
                    if self.traits.traits.contains_key(&stored_def_id) {
                        if let Some(substituted) = self.substitute_default_method_sig(
                            stored_def_id, &method.node, resolved_receiver,
                        ) {
                            sig = substituted;
                        }
                    }
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
                    // Record the method call's own type so downstream consumers
                    // (generic method-instance discovery, borrow checker) can
                    // resolve chained call receivers back to concrete types.
                    self.expr_types.insert(expr.span, sig.return_type);
                    sig.return_type
                } else {
                    // Name-based trait-default fallback FIRST — for generic-
                    // template impls (`equip [T] VectorIter[T]:`) whose
                    // impl TypeId doesn't match the concrete receiver, the
                    // trait default for `take`/`filter`/`map`/etc. is only
                    // reachable by name. Run it before
                    // `infer_closure_method_type` so the trait's default
                    // wins over the hardcoded `try_iterator_adapter_type`
                    // shortcut that would otherwise erase the concrete
                    // adapter return shape to `Vector[error]`.
                    let base_name = match self.types.get(resolved_receiver) {
                        ResolvedType::Generic(def_id, _) | ResolvedType::Defined(def_id) => {
                            Some(self.scopes.get_def(*def_id).name.clone())
                        }
                        _ => None,
                    };
                    let default_hit = base_name.as_ref().and_then(|name| {
                        let (def_id, sig) = self.traits
                            .resolve_method_by_name(name, &method.node)?;
                        // Only the trait-default case needs early
                        // routing — inherent-impl hits by name carry
                        // their own concrete sig and should continue to
                        // flow through the existing fallback chain (so
                        // any downstream handling stays intact).
                        if self.traits.traits.contains_key(def_id) {
                            Some((*def_id, sig.clone()))
                        } else {
                            None
                        }
                    });
                    if let Some((def_id, mut sig)) = default_hit {
                        self.method_resolutions.insert(method.span.start, def_id);
                        if let Some(substituted) = self.substitute_default_method_sig(
                            def_id, &method.node, resolved_receiver,
                        ) {
                            sig = substituted;
                        }
                        for (arg, &param_type) in args.iter().zip(sig.params.iter()) {
                            let arg_type = self.infer_expr(&arg.node.value);
                            self.unify(param_type, arg_type, arg.span);
                        }
                        let ret = sig.return_type;
                        self.expr_types.insert(expr.span, ret);
                        return ret;
                    }

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
                            // Name-based fallback for cross-module equip methods
                            // where TypeId doesn't match.
                            if let Some(ref name) = base_name {
                                if let Some((_def_id, sig)) = self.traits.resolve_method_by_name(name, &method.node) {
                                    let ret = sig.return_type;
                                    self.expr_types.insert(expr.span, ret);
                                    ret
                                } else {
                                    // Only emit NoMethodFound for types with inherent-only
                                    // equip blocks (no trait impls, no via delegation).
                                    // Types with trait impls may have default or via-forwarded
                                    // methods that aren't in the equip's methods map.
                                    // Stdlib/runtime types without equip blocks have methods
                                    // only in the C backend, so we'd produce false positives.
                                    //
                                    // Auto-derivable methods (clone/debug/display/hash) are
                                    // intrinsic — every type has them, and they may be synthesized
                                    // at IR-lowering time without appearing in any registered
                                    // equip block. Exempt them from the check.
                                    let is_auto_derivable = matches!(
                                        method.node.as_str(),
                                        "clone" | "debug" | "display" | "hash"
                                    );
                                    let has_inherent_only = self.traits.has_inherent_only_impls(name);
                                    if has_inherent_only && !is_auto_derivable {
                                        // If inference was attempted at this
                                        // call site and failed, emit the
                                        // typed MethodGenericInferenceFailed
                                        // instead of the generic
                                        // NoMethodFound — points the user at
                                        // the specific unresolved generic +
                                        // suggests the explicit-args fix.
                                        // See `docs/internals/method-level-
                                        // inference.md` risk #3.
                                        if let Some((unresolved, reason)) =
                                            self.inference_failures.get(&method.span.start).cloned()
                                        {
                                            self.error(
                                                SemanticErrorKind::MethodGenericInferenceFailed {
                                                    method: method.node.clone(),
                                                    type_: self.describe_resolved_type(resolved_receiver),
                                                    unresolved,
                                                    reason,
                                                },
                                                expr.span,
                                            );
                                        } else {
                                            self.error(
                                                SemanticErrorKind::NoMethodFound {
                                                    method: method.node.clone(),
                                                    type_: self.describe_resolved_type(resolved_receiver),
                                                },
                                                expr.span,
                                            );
                                        }
                                    }
                                    self.types.error_id
                                }
                            } else {
                                self.types.error_id
                            }
                        }
                    }
                }
            }

            Expr::FieldAccess { object, field } => {
                let object_type = self.infer_expr(object);
                let resolved = self.resolve_type(object_type);
                // Check if the field exists on the resolved type AND
                // return the field's actual type. Returning error_id
                // here lets bogus calls slip through downstream — e.g.
                // `match st.lex_token: case TkKeyword(kw): f(kw)` would
                // type kw as <error> because the scrutinee's type was
                // lost, and `<error>` silently accepts any concrete
                // parameter type. Look up the field's TypeId via the
                // struct's `field_types` (populated in
                // populate_def_field_types).
                if let ResolvedType::Defined(did) = self.types.get(resolved).clone() {
                    if let Some(sfi) = self.struct_fields.get(&did) {
                        if let Some(field_idx) = sfi.fields.iter().position(|(name, _)| name == &field.node) {
                            // Field exists; return its type from DefInfo.field_types
                            if let Some(field_tids) = &self.scopes.get_def(did).field_types {
                                if let Some(&tid) = field_tids.get(field_idx) {
                                    return tid;
                                }
                            }
                        } else {
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

            Expr::TupleFieldAccess { object, index } => {
                let object_type = self.infer_expr(object);
                let resolved = self.resolve_type(object_type);
                match self.types.get(resolved).clone() {
                    ResolvedType::Tuple(elems) => {
                        if *index < elems.len() {
                            elems[*index]
                        } else {
                            self.error(
                                SemanticErrorKind::TupleIndexOutOfBounds {
                                    index: *index,
                                    len: elems.len(),
                                },
                                expr.span,
                            );
                            self.types.error_id
                        }
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

            Expr::Spawn { expr: inner, .. } => {
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

            Expr::SpawnBlocking { expr: inner, .. } => {
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

            Expr::Block(block) => {
                // A block used as an expression takes its value from the
                // tail. If the tail is a divergent statement (return / throw
                // / break / continue), the block has type Never — `unify`
                // treats Never as compatible with anything, so a match arm
                // whose body ends in `return` composes correctly with the
                // match's overall value type. Without this special-case,
                // `check_block` would return void for the divergent tail
                // and the surrounding match-expression would fail to type.
                // (Done here at the value-position site, not in
                // `check_block` itself, because `check_block` is also called
                // for function bodies — making it return Never for any
                // body ending in `return` would mislead generic
                // monomorphization for returning trait methods.)
                let last_is_divergent = block.stmts.last().map_or(false, |s| matches!(
                    &s.node,
                    Stmt::Return(_) | Stmt::Throw(_) | Stmt::Break(_) | Stmt::Continue
                ));
                let block_ty = self.check_block(block);
                if last_is_divergent {
                    self.types.never_id
                } else {
                    block_ty
                }
            }

            Expr::Do { body } => self.check_block(body),

            Expr::Closure { params, body, .. } => {
                // Infer closure type from params and body.
                // Write resolved param types back to DefInfos so that
                // references to the params inside the body can find them.
                let expected_ownerships = self.extract_function_ownerships(self.decl_type_hint);
                let mut param_types = Vec::new();
                let mut param_ownerships = Vec::new();
                for (i, param) in params.iter().enumerate() {
                    // Use explicit ownership from closure param, or inherit from
                    // expected type hint (e.g. Callable[int(&T)]) for untyped params.
                    let own = if param.node.ownership != crate::parser::ast::Ownership::Borrow {
                        param.node.ownership
                    } else if let Some(expected) = expected_ownerships.as_ref().and_then(|v| v.get(i)) {
                        *expected
                    } else {
                        crate::parser::ast::Ownership::Borrow
                    };
                    param_ownerships.push(own);
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

                // Set up closure return type tracking so `return` statements
                // inside the closure body unify against the closure's own
                // return type, not the enclosing function's.
                let closure_ret_var = self.fresh_type_var();
                if let Some(expected_ret) = self.extract_fn_return_type_from_hint(self.decl_type_hint) {
                    self.unify(closure_ret_var, expected_ret, expr.span);
                }
                let saved_return_type = self.current_return_type;
                self.current_return_type = Some(closure_ret_var);

                let body_type = self.infer_expr(body);

                self.current_return_type = saved_return_type;

                // Determine the closure's return type: use the body's type for
                // expression bodies / tail expressions, or the type collected
                // from `return` statements for block bodies.
                //
                // Skip body_type when it's Never — that happens when the body
                // is an `Expr::Block` whose last statement is `Stmt::Return`
                // (the parser wraps closure expression-bodies in an explicit
                // `Return` when destructure-desugar produces a synthetic
                // block). The Stmt::Return handler already unified
                // closure_ret_var with the returned expression's type, so the
                // resolved closure_ret_var carries the correct return type;
                // body_type=Never would otherwise specialize the closure as
                // returning Never (e.g. `Closure[Never(int, (int, int))]`)
                // and break monomorphization for tuple-destructured closures
                // (closure_tuple_destructure regression, 2026-05-06).
                let return_type = if body_type != self.types.void_id
                    && body_type != self.types.never_id
                {
                    body_type
                } else {
                    let resolved = self.resolve_type(closure_ret_var);
                    if matches!(self.types.get(resolved), ResolvedType::Var(_)) {
                        if body_type == self.types.never_id {
                            self.types.never_id
                        } else {
                            self.types.void_id
                        }
                    } else {
                        resolved
                    }
                };

                self.types.insert(ResolvedType::Function {
                    param_ownerships,
                    params: param_types,
                    return_type,
                })
            }

            Expr::ImplicitClosure { body } => {
                let param_type = self.fresh_type_var();
                let prev_it_type = self.implicit_it_type.replace(param_type);
                let body_type = self.infer_expr(body);
                self.implicit_it_type = prev_it_type;
                let ownership = self.extract_function_ownerships(self.decl_type_hint)
                    .and_then(|v| v.into_iter().next())
                    .unwrap_or(crate::parser::ast::Ownership::Borrow);
                self.types.insert(ResolvedType::Function {
                    params: vec![param_type],
                    return_type: body_type,
                    param_ownerships: vec![ownership],
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
                // Propagate the declared K/V hints into each pair's inference
                // so nested collection literals in key/value positions can
                // be coerced (e.g. `Dict[K, Vector[T]] d = {"a": [1, 2, 3]}`
                // — without this, `[1, 2, 3]` infers as `T[3]` and the outer
                // Dict literal types as `Dict[K, T[3]]`, failing to unify
                // with the declared `Dict[K, Vector[T]]`). Mirrors how
                // `Vector[T] v = [1, 2, 3]` works at the bare-init level.
                let (key_hint, val_hint) = self.decl_type_hint
                    .and_then(|hint| {
                        let resolved = self.resolve_type(hint);
                        if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                            let name = &self.scopes.get_def(def_id).name;
                            if matches!(name.as_str(), "Dict" | "HashMap") && args.len() == 2 {
                                return Some((Some(args[0]), Some(args[1])));
                            }
                        }
                        None
                    })
                    .unwrap_or((None, None));
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = key_hint;
                let key_type = self.infer_expr(&pairs[0].0);
                self.decl_type_hint = val_hint;
                let val_type = self.infer_expr(&pairs[0].1);
                for (k, v) in &pairs[1..] {
                    self.decl_type_hint = key_hint;
                    let kt = self.infer_expr(k);
                    self.decl_type_hint = val_hint;
                    let vt = self.infer_expr(v);
                    self.unify(key_type, kt, k.span);
                    self.unify(val_type, vt, v.span);
                }
                self.decl_type_hint = prev_hint;
                // Build Dict[K, V] type. Use the declared K/V from the
                // hint when available — the per-pair inferences may have
                // been coerced (e.g. `int[3]` accepted under `Vector[int]`
                // hint via `is_collection_assignment`), so reading them
                // back as the literal's type would still produce the
                // un-coerced shape. Falling back to the hint K/V keeps
                // the literal's type aligned with what the decl-site
                // checker will then unify against.
                let final_key = key_hint.unwrap_or(key_type);
                let final_val = val_hint.unwrap_or(val_type);
                if let Some(dict_def_id) = self.scopes.lookup("Dict") {
                    let dict_type = self.types.intern_generic(dict_def_id, vec![final_key, final_val]);
                    self.check_struct_type_bounds(dict_type, expr.span);
                    dict_type
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
                    } else if let Some(sfi) = self.struct_fields.get(&def_id) {
                        // Skip opaque structs (0 user-visible fields) that may accept
                        // constructor arguments via special type-checker handling.
                        if !sfi.fields.is_empty() && args.len() != sfi.fields.len() {
                            self.error(
                                SemanticErrorKind::WrongFieldCount {
                                    type_: name.node.clone(),
                                    expected: sfi.fields.len(),
                                    found: args.len(),
                                },
                                name.span,
                            );
                        }
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
                            let generic_type = self.types.intern_generic(def_id, type_ids);
                            self.check_struct_type_bounds(generic_type, name.span);
                            return generic_type;
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
                let source_type = self.infer_expr(inner);
                let target_type = super::types::ast_type_to_resolved(
                    &type_.node,
                    type_.span,
                    self.scopes,
                    self.types,
                )
                .unwrap_or(self.types.error_id);
                // Validate that the cast is between compatible types.
                // Allow: numeric↔numeric, bool↔int, enum→int.
                // Reject: struct→primitive, collection→primitive, etc.
                let src = self.resolve_type(source_type);
                let tgt = self.resolve_type(target_type);
                // Unwrap Ref/Owned for castability check (auto-deref)
                let src_inner = match self.types.get(src) {
                    ResolvedType::Ref(inner) | ResolvedType::Owned(inner) => self.resolve_type(*inner),
                    _ => src,
                };
                let src_castable = matches!(
                    self.types.get(src_inner),
                    ResolvedType::Primitive(_) | ResolvedType::Error | ResolvedType::Void
                ) || self.is_enum_type(src_inner);
                let tgt_castable = matches!(
                    self.types.get(tgt),
                    ResolvedType::Primitive(_) | ResolvedType::Error | ResolvedType::Void
                );
                if !src_castable && tgt_castable && tgt != self.types.error_id {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: format!("castable type for `as {}`", self.describe_resolved_type(tgt)),
                            found: self.describe_resolved_type(src),
                        },
                        expr.span,
                    );
                }
                target_type
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

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Option<TypeId> {
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

                // Check generic type parameter trait bounds (e.g. Dict[K: Hashable, V])
                if let Some(dt) = declared_type {
                    self.check_struct_type_bounds(dt, type_.span);
                }

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
                                && !self.is_result_capture_compatible(declared_type, value_type)
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
                return Some(self.infer_expr(expr));
            }

            Stmt::Assign { target, value } => {
                let target_type = self.infer_expr(target);
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = Some(target_type);
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;
                if !self.is_auto_propagation_compatible(target_type, value_type)
                    && !self.is_result_capture_compatible(target_type, value_type)
                {
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
                if self.current_return_type.is_none() {
                    self.error(SemanticErrorKind::ReturnOutsideFunction, stmt.span);
                }
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
                    self.error(SemanticErrorKind::ContinueOutsideLoop, stmt.span);
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
                let then_type = self.check_block(then_body);

                for (cond, body) in elif_branches {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
                    self.assign_compound_is_types(cond);
                    self.check_block(body);
                }

                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                    // If-with-else in tail position produces the then-branch type.
                    // Return it so check_block doesn't re-infer via infer_stmt_tail_type.
                    return Some(then_type);
                }
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                let scrutinee_type = self.infer_expr(scrutinee);
                let mut first_arm_type = None;
                for arm in arms.iter().filter_map(|i| i.arm()) {
                    self.assign_pattern_types(&arm.pattern, scrutinee_type);
                    if let Some(guard) = &arm.guard {
                        let gt = self.infer_expr(guard);
                        self.unify(gt, self.types.bool_id, guard.span);
                    }
                    let arm_type = self.infer_expr(&arm.body);
                    if first_arm_type.is_none() {
                        first_arm_type = Some(arm_type);
                    }
                }
                if let Some(else_arm) = else_arm {
                    self.check_block(else_arm);
                }
                self.check_match_exhaustiveness(scrutinee_type, arms, else_arm.is_some(), stmt.span);
                // Match in tail position produces the first arm's type.
                // Return it so check_block doesn't re-infer via infer_stmt_tail_type.
                if let Some(ty) = first_arm_type {
                    return Some(ty);
                }
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
        None
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

    /// Walk a TypeId tree and replace each `Defined(def_id)` whose def is a
    /// `GenericParam` with a fresh `Var` per unique DefId, sharing the same
    /// fresh var across all occurrences of the same param so the unifier links
    /// them. Used at generic function call sites to instantiate the signature
    /// with fresh inference variables.
    fn instantiate_generic_params(
        &mut self,
        type_id: TypeId,
        subst: &mut FxHashMap<DefId, TypeId>,
    ) -> TypeId {
        match self.types.get(type_id).clone() {
            ResolvedType::Defined(def_id) => {
                if self.scopes.get_def(def_id).kind == DefKind::GenericParam {
                    *subst.entry(def_id).or_insert_with(|| self.fresh_type_var())
                } else {
                    type_id
                }
            }
            ResolvedType::Generic(def_id, args) => {
                let new_args: Vec<TypeId> = args.iter()
                    .map(|&a| self.instantiate_generic_params(a, subst))
                    .collect();
                if new_args == args {
                    type_id
                } else {
                    self.types.intern_generic(def_id, new_args)
                }
            }
            ResolvedType::Tuple(elems) => {
                let new_elems: Vec<TypeId> = elems.iter()
                    .map(|&e| self.instantiate_generic_params(e, subst))
                    .collect();
                if new_elems == elems {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Tuple(new_elems))
                }
            }
            ResolvedType::Array(elem, size) => {
                let new_elem = self.instantiate_generic_params(elem, subst);
                if new_elem == elem {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Array(new_elem, size))
                }
            }
            ResolvedType::Slice(elem) => {
                let new_elem = self.instantiate_generic_params(elem, subst);
                if new_elem == elem {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Slice(new_elem))
                }
            }
            ResolvedType::Function { params, param_ownerships, return_type } => {
                let new_params: Vec<TypeId> = params.iter()
                    .map(|&p| self.instantiate_generic_params(p, subst))
                    .collect();
                let new_return = self.instantiate_generic_params(return_type, subst);
                if new_params == params && new_return == return_type {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Function {
                        params: new_params,
                        param_ownerships,
                        return_type: new_return,
                    })
                }
            }
            ResolvedType::Ref(inner) => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Ref(new_inner))
                }
            }
            ResolvedType::Owned(inner) => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::Owned(new_inner))
                }
            }
            ResolvedType::CallableTrait(inner) => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::CallableTrait(new_inner))
                }
            }
            ResolvedType::MutCallableTrait(inner) => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::MutCallableTrait(new_inner))
                }
            }
            ResolvedType::ConsumeCallableTrait(inner) => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::ConsumeCallableTrait(new_inner))
                }
            }
            ResolvedType::BoxedCallable { kind, inner } => {
                let new_inner = self.instantiate_generic_params(inner, subst);
                if new_inner == inner {
                    type_id
                } else {
                    self.types.insert(ResolvedType::BoxedCallable { kind, inner: new_inner })
                }
            }
            // Var, Primitive, TraitObject, Error, Void, Never — leaf, no substitution needed.
            _ => type_id,
        }
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
            let stmt_type = self.check_stmt(stmt);
            // The "value" of a block is its last expression statement,
            // or a tail if/match with branches that end in expressions.
            if let Some(ty) = stmt_type {
                last_type = ty;
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

    /// When `resolve_method`/`resolve_method_by_name` returns a sig owned by
    /// a trait default body, the stored sig has `Self` and the trait's own
    /// generic `T` erased to `error_id` (they were out of scope at
    /// registry-build time). Rebuild the sig by substituting both against
    /// the concrete receiver so adapter constructors like `TakeIter[Self,
    /// T] take(self, int n)` resolve to the concrete iterator type.
    ///
    /// Returns `None` if any prerequisite is missing (no AST default sig,
    /// no matching impl, receiver can't be projected back to AST). Callers
    /// fall through to the unsubstituted sig in that case.
    fn substitute_default_method_sig(
        &mut self,
        trait_def_id: DefId,
        method: &str,
        receiver_type_id: TypeId,
    ) -> Option<super::traits::FunctionSig> {
        let default_sig = self.traits.traits.get(&trait_def_id)
            .and_then(|t| t.default_method_sigs.get(method))
            .cloned()?;
        let trait_generic_params = self.traits.traits.get(&trait_def_id)
            .map(|t| t.trait_generic_params.clone())
            .unwrap_or_default();

        // Receiver's base name — used to find the matching impl when
        // TypeId doesn't match (generic template impls are registered
        // with a template TypeId that differs from the concrete receiver).
        let resolved_receiver = self.resolve_type(receiver_type_id);
        let receiver_name = match self.types.get(resolved_receiver) {
            ResolvedType::Generic(def_id, _) | ResolvedType::Defined(def_id) => {
                Some(self.scopes.get_def(*def_id).name.clone())
            }
            _ => None,
        };

        // Prefer an exact-TypeId impl (for fully-concrete impls); fall
        // back to name-based matching (generic template impls).
        let impl_info = self.traits.impls.iter().find(|i| {
            i.trait_ == Some(trait_def_id) && i.self_type == resolved_receiver
        }).or_else(|| {
            let name = receiver_name.as_deref()?;
            self.traits.impls.iter().find(|i| {
                i.trait_ == Some(trait_def_id) && i.self_type_name == name
            })
        })?;
        let impl_self_type_ast = impl_info.self_type_ast.clone();
        let impl_generic_params = impl_info.impl_generic_params.clone();
        let trait_generic_args_ast: Vec<Type> = impl_info.trait_generic_args.clone();

        // Step 1: project the receiver's concrete TypeId back to AST so we
        // can use it as the value of `Self` and pair it positionally
        // against the impl's self_type template to bind impl locals.
        let receiver_ast = self.typeid_to_ast_type(resolved_receiver)?;

        // Step 2: bind impl-local generic params (`[T]` etc.) from the
        // receiver by structurally matching impl.self_type_ast vs
        // receiver_ast.
        let mut impl_bindings: FxHashMap<String, Type> = FxHashMap::default();
        bind_template_generics(
            &impl_self_type_ast,
            &receiver_ast,
            &impl_generic_params,
            &mut impl_bindings,
        );

        // Step 3: compute trait generic bindings. `trait_generic_args_ast[i]`
        // describes how the impl supplies the i-th trait generic; substitute
        // impl-local bindings into it, then pair with
        // `trait_generic_params[i]` by position.
        let mut full_bindings: FxHashMap<String, Type> = FxHashMap::default();
        full_bindings.insert("Self".to_string(), receiver_ast);
        for (tparam, targ) in trait_generic_params.iter().zip(trait_generic_args_ast.iter()) {
            let substituted = super::traits::substitute_ast_type(targ, &impl_bindings);
            full_bindings.insert(tparam.clone(), substituted);
        }

        // Step 4: substitute default sig's return + params, then resolve
        // the AST back to TypeIds. Method-level generic placeholders stay
        // unsubstituted — they'll bind at the call's inference step.
        let substituted_return = super::traits::substitute_ast_type(
            &default_sig.return_type, &full_bindings,
        );
        let return_type_id = types::ast_type_to_resolved(
            &substituted_return, Span { start: 0, end: 0 }, self.scopes, self.types,
        ).unwrap_or(self.types.error_id);

        let mut param_type_ids = Vec::with_capacity(default_sig.param_types.len());
        for p in &default_sig.param_types {
            let substituted = super::traits::substitute_ast_type(p, &full_bindings);
            let id = types::ast_type_to_resolved(
                &substituted, Span { start: 0, end: 0 }, self.scopes, self.types,
            ).unwrap_or(self.types.error_id);
            param_type_ids.push(id);
        }

        Some(super::traits::FunctionSig {
            params: param_type_ids,
            return_type: return_type_id,
            has_self: default_sig.has_self,
            self_ownership: default_sig.self_ownership,
        })
    }

    /// Check if a throws/Result-returning call can be captured as a Result value.
    /// Allows `Result[T, E] r = throwing_call()` where `throwing_call` returns `T`
    /// (with throws E) or `Result[T, E]`.
    fn is_result_capture_compatible(&self, declared: TypeId, value: TypeId) -> bool {
        // Check if declared type is Result[T, E]
        let declared_resolved = self.resolve_type(declared);
        let ok_type = if let ResolvedType::Generic(def_id, ref args) = self.types.get(declared_resolved).clone() {
            let name = self.scopes.get_def(def_id).name.clone();
            if name == "Result" && args.len() == 2 {
                args[0]
            } else {
                return false;
            }
        } else {
            return false;
        };

        // Check if value type matches the Ok type of the Result
        let value_resolved = self.resolve_type(value);
        let ok_resolved = self.resolve_type(ok_type);
        if value_resolved == ok_resolved {
            return true;
        }
        // Also accept if value resolves to error_id (inference)
        if value_resolved == self.types.error_id {
            return true;
        }
        false
    }

    /// Convert a resolved TypeId back to an AST `Type`. Used by method-
    /// generic inference to materialise inferred bindings as AST types
    /// suitable for `MethodCall.generic_args`. Returns None for shapes the
    /// downstream consumers (mangling, generic collector) don't accept,
    /// in which case the caller skips inference for that call.
    fn typeid_to_ast_type(&self, type_id: TypeId) -> Option<Type> {
        let resolved = self.resolve_type(type_id);
        let dummy = Span { start: 0, end: 0 };
        match self.types.get(resolved) {
            ResolvedType::Primitive(prim) => Some(Type::Primitive(*prim)),
            ResolvedType::Void => Some(Type::Primitive(PrimitiveType::Void)),
            ResolvedType::Defined(def_id) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                Some(Type::Named {
                    name: Spanned { node: name, span: dummy },
                    generic_args: vec![],
                })
            }
            ResolvedType::Generic(def_id, args) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                let args = args.clone();
                let mut new_args = Vec::with_capacity(args.len());
                for a in args {
                    new_args.push(Spanned {
                        node: self.typeid_to_ast_type(a)?,
                        span: dummy,
                    });
                }
                Some(Type::Named {
                    name: Spanned { node: name, span: dummy },
                    generic_args: new_args,
                })
            }
            ResolvedType::Tuple(elems) => {
                let elems = elems.clone();
                let mut new_elems = Vec::with_capacity(elems.len());
                for e in elems {
                    new_elems.push(Spanned {
                        node: self.typeid_to_ast_type(e)?,
                        span: dummy,
                    });
                }
                Some(Type::Tuple(new_elems))
            }
            ResolvedType::Function { params, param_ownerships, return_type } => {
                let params = params.clone();
                let param_ownerships = param_ownerships.clone();
                let return_type = *return_type;
                let mut new_params = Vec::with_capacity(params.len());
                for p in params {
                    new_params.push(Spanned {
                        node: self.typeid_to_ast_type(p)?,
                        span: dummy,
                    });
                }
                Some(Type::Function {
                    return_type: Box::new(Spanned {
                        node: self.typeid_to_ast_type(return_type)?,
                        span: dummy,
                    }),
                    params: new_params,
                    param_ownerships,
                })
            }
            ResolvedType::Ref(inner) => {
                let inner = *inner;
                Some(Type::Ref(Box::new(Spanned {
                    node: self.typeid_to_ast_type(inner)?,
                    span: dummy,
                })))
            }
            ResolvedType::Owned(inner) => {
                let inner = *inner;
                Some(Type::Owned(Box::new(Spanned {
                    node: self.typeid_to_ast_type(inner)?,
                    span: dummy,
                })))
            }
            // Trait objects, callable wrappers, and inference variables
            // are intentionally not convertible — they shouldn't appear as
            // an inferred method-generic binding in well-typed code, and
            // failing the conversion makes the caller skip inference
            // gracefully (falls back to the existing dispatch).
            _ => None,
        }
    }

    /// Try to infer method-level generic args for a method call.
    ///
    /// Shape 1 (predicate): when a method-level generic `G` appears
    /// directly as the type of a non-callable param, bind `G = arg.type`.
    /// e.g. `bool any[F](&self, F pred)` called as `v.iter().any(is_even)`
    /// binds `F = bool(int)`.
    ///
    /// Shape 3 (fold): `A fold[A, F](self, A init, F f)` is subsumed by
    /// shape-1 — both `A` (in `init`'s slot) and `F` (in `f`'s slot)
    /// appear directly as named-slot params and bind in one pass.
    ///
    /// Shape 2 (map — structural): when a method-level generic `U`
    /// appears only in the return type (not in any param slot) AND one
    /// param is a callable `F` bound to a Function type, bind `U =
    /// F.return_type`. e.g. `Vector[U] map[U, F](self, F f)` called
    /// as `v.iter().map(double)` where `double: int(int)` binds
    /// `F = int(int)` then `U = int`.
    ///
    /// Returns `Some(Vec<Type>)` with one AST type per generic param if
    /// every generic resolves; `None` otherwise (caller falls back to
    /// existing dispatch).
    fn try_infer_method_targs(
        &mut self,
        shape: &super::traits::MethodSigShape,
        arg_types: &[TypeId],
        method_span_start: usize,
    ) -> Option<Vec<Type>> {
        if shape.param_types.len() != arg_types.len() {
            // Arg-count mismatch is a separate error (`WrongArgCount`)
            // that the dispatch fork already emits — don't double-report
            // here. Let the existing path own it.
            return None;
        }
        let mut bindings: FxHashMap<String, TypeId> = FxHashMap::default();
        let generic_set: std::collections::HashSet<&str> =
            shape.generic_params.iter().map(|s| s.as_str()).collect();

        // Shape 1: bind G when it appears as `Type::Named { name: G, [] }`
        // directly as a param's type (non-callable, non-composite slot).
        for (param_ty, &arg_ty) in shape.param_types.iter().zip(arg_types.iter()) {
            if let Type::Named { name, generic_args } = param_ty {
                if generic_args.is_empty() && generic_set.contains(name.node.as_str()) {
                    let resolved = self.resolve_type(arg_ty);
                    if resolved == self.types.error_id {
                        // Argument failed to type — skip inference cleanly.
                        // Don't record a typed-inference-failure either; the
                        // arg-typing failure produces its own error.
                        return None;
                    }
                    bindings.entry(name.node.clone()).or_insert(resolved);
                }
            }
        }

        // Shape 2 (map — structural): for each unbound generic `G` that
        // appears in the return type, if exactly one already-bound param
        // resolves to a Function, bind `G = that function's return type`.
        // Handles the `Vector[U] map[U, F](self, F f)` pattern where U
        // is never a direct sig-param slot; the body constraint
        // `U = F.return_type` is materialised here instead.
        for g in &shape.generic_params {
            if bindings.contains_key(g) {
                continue;
            }
            if !type_mentions_name(&shape.return_type, g) {
                continue;
            }
            let mut fn_ret_type: Option<TypeId> = None;
            for &arg_ty in arg_types {
                let resolved = self.resolve_type(arg_ty);
                if let ResolvedType::Function { return_type, .. } = self.types.get(resolved) {
                    let rt = *return_type;
                    if rt == self.types.error_id {
                        continue;
                    }
                    if let Some(prev) = fn_ret_type {
                        if prev != rt {
                            // Multiple Function args with different
                            // return types — can't pick one
                            // unambiguously. Let fallback handle it.
                            fn_ret_type = None;
                            break;
                        }
                    } else {
                        fn_ret_type = Some(rt);
                    }
                }
            }
            if let Some(rt) = fn_ret_type {
                bindings.insert(g.clone(), rt);
            }
        }

        // Materialise bindings in declaration order. If any param is
        // unbound at this point, inference failed — record the
        // unresolved param + reason so the dispatch fork can swap a
        // generic NoMethodFound for a typed
        // MethodGenericInferenceFailed if no fallback handles the call.
        let mut out = Vec::with_capacity(shape.generic_params.len());
        for name in &shape.generic_params {
            let Some(tid) = bindings.get(name).copied() else {
                let reason = if type_mentions_name(&shape.return_type, name) {
                    "no callable arg's return type matches its slot in the sig (shape-2 ambiguous or absent)".to_string()
                } else {
                    "no arg's type matches its named-slot position in the sig (shape-1 absent)".to_string()
                };
                self.inference_failures.insert(method_span_start, (name.clone(), reason));
                return None;
            };
            let Some(ast) = self.typeid_to_ast_type(tid) else {
                self.inference_failures.insert(
                    method_span_start,
                    (name.clone(), format!(
                        "bound type for `{name}` couldn't be projected back to AST (likely a trait object or unbound type variable)"
                    )),
                );
                return None;
            };
            out.push(ast);
        }
        Some(out)
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
            "str" | "String" => Some(PrimitiveType::StringType),
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
            ResolvedType::Primitive(PrimitiveType::StringType | PrimitiveType::CStr) => {
                ("String".to_string(), vec![])
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
                if method == "debug" || method == "display" { return Some(self.types.owned_string_id); }
                // .mod(divisor) → Euclidean modulo, returns same type as receiver
                if method == "mod" { return Some(receiver_type); }
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
                // Borrowing methods: get/first/last return Option[T &]
                "get" | "first" | "last" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        let ref_elem = self.types.insert(ResolvedType::Ref(elem_type()));
                        Some(self.types.intern_generic(option_def_id, vec![ref_elem]))
                    } else {
                        Some(elem_type())
                    }
                }
                // Consuming methods: pop/remove return Option[T !]
                "pop" | "remove" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        let owned_elem = self.types.insert(ResolvedType::Owned(elem_type()));
                        Some(self.types.intern_generic(option_def_id, vec![owned_elem]))
                    } else {
                        Some(elem_type())
                    }
                }
                "set" => Some(self.types.void_id),
                "len" | "capacity" => Some(self.types.int_id),
                "index_of" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "clear" | "reserve" | "sort" | "sort_by" | "sort_by_key" | "reverse" | "insert" | "extend" => Some(self.types.void_id),
                "is_empty" | "contains" | "any" | "all" => Some(self.types.bool_id),
                "sorted" | "sorted_by" | "sorted_by_key" | "reversed" | "unique" | "slice" | "enumerate" => Some(receiver_type),
                "windows" | "chunks" => {
                    // Returns Vector[Vector[T]] — eager materialization.
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        let inner = self.types.intern_generic(vec_def_id, vec![elem_type()]);
                        Some(self.types.intern_generic(vec_def_id, vec![inner]))
                    } else {
                        Some(receiver_type)
                    }
                }
                "binary_search" => Some(self.types.int_id),
                _ => None,
            },
            "Dict" | "HashMap" => match method {
                "put" | "set" | "update" => Some(self.types.void_id),
                "get" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        let ref_val = self.types.insert(ResolvedType::Ref(val_type()));
                        Some(self.types.intern_generic(option_def_id, vec![ref_val]))
                    } else {
                        Some(val_type())
                    }
                }
                "get_or" | "get_or_put" => Some(val_type()),
                "contains" | "has" | "has_key" | "contains_key" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => {
                    // Dict.remove(key) → Option[V !] — returns removed value, None if absent.
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        let owned_val = self.types.insert(ResolvedType::Owned(val_type()));
                        Some(self.types.intern_generic(option_def_id, vec![owned_val]))
                    } else {
                        Some(val_type())
                    }
                }
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
                "contains" | "is_subset" | "is_superset" | "is_disjoint" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                "union" | "intersection" | "difference" | "symmetric_difference" => Some(receiver_type),
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
                "find" => {
                    // String.find(pattern, from=0, reverse=false) → Option[int]
                    // Unified search primitive. index_of() is a POLA alias.
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
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
                "trim" | "strip" | "lstrip" | "rstrip" | "trim_left" | "trim_right"
                | "removeprefix" | "removesuffix" | "byte_slice" | "substring"
                    => Some(self.types.string_id),
                // Allocating returns — return String (GorgetString)
                "to_upper" | "to_lower" | "replace" | "repeat" | "join" | "pad_left" | "pad_right"
                | "debug" | "display"
                    => Some(self.types.owned_string_id),
                "enumerate" => Some(receiver_type),
                "byte_at" => Some(self.types.primitive_id(PrimitiveType::Uint8)),
                // char_at: deprecated compat alias — returns str (1-byte view, byte-indexed)
                "char_at" => Some(self.types.string_id),
                "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace"
                | "is_upper" | "is_lower" | "is_hex_digit" | "is_ascii"
                    => Some(self.types.bool_id),
                // Mutation methods (String builder)
                "push" | "push_char" | "push_line" | "clear" => Some(self.types.void_id),
                "capacity" => Some(self.types.int_id),
                "split" | "lines" => {
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
            "Arena" => match method {
                "bytes_used" => Some(self.types.int_id),
                "reset" => Some(self.types.void_id),
                "checkpoint" => {
                    self.scopes.lookup("ArenaCheckpoint")
                        .map(|did| self.types.defined_id(did))
                        .or(Some(self.types.int_id))
                }
                _ => None,
            },
            "ArenaCheckpoint" => match method {
                "mark" => Some(self.types.void_id),
                _ => None,
            },
            "TlsfAllocator" | "FixedBufferAllocator" => match method {
                "bytes_used" => Some(self.types.int_id),
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

    /// Check generic type parameter trait bounds for struct/enum instantiations.
    /// Given a resolved TypeId, if it's a Generic type with bounds registered in
    /// struct_generic_bounds, verify each concrete type arg satisfies the required traits.
    fn check_struct_type_bounds(&mut self, type_id: TypeId, span: Span) {
        let (def_id, args) = match self.types.get(type_id).clone() {
            ResolvedType::Generic(def_id, args) => (def_id, args),
            _ => return,
        };
        let (param_names, bounds) = match self.struct_generic_bounds.get(&def_id) {
            Some(info) => info,
            None => return,
        };
        if bounds.is_empty() {
            return;
        }
        let bounds = bounds.clone();
        let param_names = param_names.clone();
        for (param_name, required_traits) in &bounds {
            let idx = match param_names.iter().position(|n| n == param_name) {
                Some(i) => i,
                None => continue,
            };
            let arg_type_id = match args.get(idx) {
                Some(&tid) => tid,
                None => continue,
            };
            // Skip type variables (unresolved generics) — bounds will be checked
            // when the outer generic is itself instantiated with concrete types.
            if matches!(self.types.get(arg_type_id), ResolvedType::Var(_)) {
                continue;
            }
            let concrete_type = self.describe_resolved_type(arg_type_id);
            for trait_name in required_traits {
                if self.traits.has_trait_impl_by_name(&concrete_type, trait_name) {
                    continue;
                }
                // Transitive bound propagation from the enclosing generic function
                let satisfied_by_outer_bound = self.current_trait_bounds.iter().any(|(p, tb)| {
                    p == &concrete_type && tb.iter().any(|b| self.traits.trait_satisfies(b, trait_name))
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

    /// Pre-register a function's signature (return type + param types) on its DefInfo
    /// so that callers can infer the function's type during type checking.
    /// Skips generic functions since their type params aren't in scope at module level.
    fn register_function_signature(&mut self, func: &FunctionDef) {
        // For generic functions, the type-param names (T, U, E, ...) live in the
        // function's body scope. We push a scratch scope mirroring those names to
        // GenericParam DefIds so `ast_type_to_resolved` can resolve them while
        // building the signature. The actual function body's references will use
        // the function's body-scope DefIds, which are SEPARATE from these
        // scratch ones — so per-call instantiation must operate by NAME not
        // by DefId. See `infer_generic_function_call`.
        let has_generics = func.generic_params.is_some();
        if has_generics {
            self.scopes.push_scope(super::scope::ScopeKind::Function);
            if let Some(generics) = &func.generic_params {
                for param in &generics.node.params {
                    if let crate::parser::ast::GenericParam::Type { name, .. } = &param.node {
                        let _ = self.scopes.define(
                            name.node.clone(),
                            DefKind::GenericParam,
                            name.span,
                        );
                    }
                }
            }
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
        let param_ownerships = self.function_info.get(&def_id)
            .map(|info| info.param_ownerships.clone())
            .unwrap_or_else(|| vec![crate::parser::ast::Ownership::Borrow; param_types.len()]);
        let func_type = self.types.insert(ResolvedType::Function {
            param_ownerships,
            params: param_types,
            return_type,
        });
        self.scopes.get_def_mut(def_id).type_id = Some(func_type);

        if has_generics {
            self.scopes.pop_scope();
        }
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
                PrimitiveType::StringType => "String",
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
    struct_generic_bounds: &FxHashMap<DefId, (Vec<String>, Vec<(String, Vec<String>)>)>,
    errors: &mut Vec<SemanticError>,
) -> (FxHashMap<Span, TypeId>, FxHashMap<usize, DefId>, FxHashMap<usize, Vec<Type>>, FxHashMap<usize, Vec<Type>>) {
    let mut checker = TypeChecker::new(scopes, types, traits, resolution_map, function_info, enum_variants, struct_fields, function_body_scopes, struct_generic_bounds);

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
    (checker.expr_types, checker.method_resolutions, checker.inferred_method_targs, checker.inferred_call_targs)
}

/// Walk the module AST and patch every `MethodCall` whose `span.start` is a
/// key in `inferred` to set `generic_args = Some(<types>)`. Skips calls that
/// already have explicit args (those are user-supplied and authoritative).
///
/// Called as Pass 4.5 from `semantic::analyze` after typecheck. The downstream
/// IR-lowering / generic-collector path reads `MethodCall.generic_args`
/// uniformly — by syncing the typecheck-inferred bindings into the AST,
/// per-call-site monomorphisation works the same whether the user wrote
/// `[T1, T2]` explicitly or relied on inference.
pub fn apply_inferred_method_targs(
    module: &mut Module,
    inferred: &FxHashMap<usize, Vec<Type>>,
) {
    fn walk_items(items: &mut [Spanned<Item>], inferred: &FxHashMap<usize, Vec<Type>>) {
        for item in items {
            match &mut item.node {
                Item::Module { items: inner, .. } => walk_items(inner, inferred),
                Item::Function(f) => walk_function(f, inferred),
                Item::Equip(eq) => {
                    for m in &mut eq.items {
                        walk_function(&mut m.node, inferred);
                    }
                }
                Item::Trait(td) => {
                    for ti in &mut td.items {
                        if let TraitItem::Method(m) = &mut ti.node {
                            walk_function(m, inferred);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    fn walk_function(f: &mut FunctionDef, inferred: &FxHashMap<usize, Vec<Type>>) {
        match &mut f.body {
            FunctionBody::Block(b) => walk_block(b, inferred),
            FunctionBody::Expression(e) => walk_expr(e, inferred),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }
    fn walk_block(b: &mut Block, inferred: &FxHashMap<usize, Vec<Type>>) {
        for stmt in &mut b.stmts {
            walk_stmt(&mut stmt.node, inferred);
        }
    }
    fn walk_stmt(s: &mut Stmt, inferred: &FxHashMap<usize, Vec<Type>>) {
        match s {
            Stmt::Expr(e) | Stmt::Throw(e) => walk_expr(e, inferred),
            Stmt::Return(Some(e)) | Stmt::Break(Some(e)) => walk_expr(e, inferred),
            Stmt::VarDecl { value, .. } => walk_expr(value, inferred),
            Stmt::Assign { target, value } => {
                walk_expr(target, inferred);
                walk_expr(value, inferred);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                walk_expr(target, inferred);
                walk_expr(value, inferred);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                walk_expr(condition, inferred);
                walk_block(then_body, inferred);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond, inferred);
                    walk_block(body, inferred);
                }
                if let Some(eb) = else_body {
                    walk_block(eb, inferred);
                }
            }
            Stmt::While { condition, body, else_body } => {
                walk_expr(condition, inferred);
                walk_block(body, inferred);
                if let Some(eb) = else_body { walk_block(eb, inferred); }
            }
            Stmt::For { iterable, body, else_body, .. } => {
                walk_expr(iterable, inferred);
                walk_block(body, inferred);
                if let Some(eb) = else_body { walk_block(eb, inferred); }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                walk_expr(scrutinee, inferred);
                for item in arms {
                    if let crate::parser::ast::MatchItem::Arm(arm) = item {
                        walk_expr(&mut arm.body, inferred);
                        if let Some(g) = &mut arm.guard {
                            walk_expr(g, inferred);
                        }
                    }
                }
                if let Some(b) = else_arm {
                    walk_block(b, inferred);
                }
            }
            Stmt::With { bindings, body } => {
                for binding in bindings {
                    walk_expr(&mut binding.expr, inferred);
                }
                walk_block(body, inferred);
            }
            Stmt::Loop { body }
            | Stmt::Unsafe { body }
            | Stmt::NamedScope { body, .. }
            | Stmt::OnError { body } => walk_block(body, inferred),
            Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
                walk_expr(condition, inferred);
                if let Some(m) = message { walk_expr(m, inferred); }
            }
            _ => {}
        }
    }
    fn walk_expr(e: &mut Spanned<Expr>, inferred: &FxHashMap<usize, Vec<Type>>) {
        match &mut e.node {
            Expr::MethodCall { receiver, generic_args, args, method } => {
                walk_expr(receiver, inferred);
                for arg in args.iter_mut() {
                    walk_expr(&mut arg.node.value, inferred);
                }
                let already_has = generic_args.as_ref()
                    .map(|gs| !gs.is_empty())
                    .unwrap_or(false);
                if !already_has {
                    if let Some(types) = inferred.get(&method.span.start) {
                        let dummy = Span { start: 0, end: 0 };
                        let spanned: Vec<Spanned<Type>> = types.iter()
                            .map(|t| Spanned { node: t.clone(), span: dummy })
                            .collect();
                        *generic_args = Some(spanned);
                    }
                }
            }
            Expr::Call { callee, args, .. } => {
                walk_expr(callee, inferred);
                for arg in args.iter_mut() {
                    walk_expr(&mut arg.node.value, inferred);
                }
            }
            Expr::StructLiteral { args, .. } => {
                for arg in args.iter_mut() {
                    walk_expr(arg, inferred);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                walk_expr(left, inferred);
                walk_expr(right, inferred);
            }
            Expr::UnaryOp { operand, .. } => walk_expr(operand, inferred),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                walk_expr(object, inferred);
            }
            Expr::Index { object, index } => {
                walk_expr(object, inferred);
                walk_expr(index, inferred);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                walk_expr(condition, inferred);
                walk_expr(then_branch, inferred);
                if let Some(eb) = else_branch {
                    walk_expr(eb, inferred);
                }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s, inferred); }
                if let Some(en) = end { walk_expr(en, inferred); }
            }
            Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner, inferred),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs, inferred);
                walk_expr(rhs, inferred);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                walk_expr(body, inferred);
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                for e in elems.iter_mut() { walk_expr(e, inferred); }
            }
            Expr::Block(b) => walk_block(b, inferred),
            Expr::StringLiteral(_, interp_exprs) => {
                for ie in interp_exprs.iter_mut() {
                    walk_expr(ie, inferred);
                }
            }
            _ => {}
        }
    }
    walk_items(&mut module.items, inferred);
}

/// Walk the module AST and patch every `Expr::Call` whose callee Identifier's
/// `span.start` is a key in `inferred` to set `generic_args = Some(<types>)`.
/// Skips calls that already have explicit args. Mirrors
/// `apply_inferred_method_targs` but for generic *free-function* calls.
pub fn apply_inferred_call_targs(
    module: &mut Module,
    inferred: &FxHashMap<usize, Vec<Type>>,
) {
    fn walk_items(items: &mut [Spanned<Item>], inferred: &FxHashMap<usize, Vec<Type>>) {
        for item in items {
            match &mut item.node {
                Item::Module { items: inner, .. } => walk_items(inner, inferred),
                Item::Function(f) => walk_function(f, inferred),
                Item::Equip(eq) => {
                    for m in &mut eq.items {
                        walk_function(&mut m.node, inferred);
                    }
                }
                Item::Trait(td) => {
                    for ti in &mut td.items {
                        if let TraitItem::Method(m) = &mut ti.node {
                            walk_function(m, inferred);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    fn walk_function(f: &mut FunctionDef, inferred: &FxHashMap<usize, Vec<Type>>) {
        match &mut f.body {
            FunctionBody::Block(b) => walk_block(b, inferred),
            FunctionBody::Expression(e) => walk_expr(e, inferred),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }
    fn walk_block(b: &mut Block, inferred: &FxHashMap<usize, Vec<Type>>) {
        for stmt in &mut b.stmts {
            walk_stmt(&mut stmt.node, inferred);
        }
    }
    fn walk_stmt(s: &mut Stmt, inferred: &FxHashMap<usize, Vec<Type>>) {
        match s {
            Stmt::Expr(e) | Stmt::Throw(e) => walk_expr(e, inferred),
            Stmt::Return(Some(e)) | Stmt::Break(Some(e)) => walk_expr(e, inferred),
            Stmt::VarDecl { value, .. } => walk_expr(value, inferred),
            Stmt::Assign { target, value } => {
                walk_expr(target, inferred);
                walk_expr(value, inferred);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                walk_expr(target, inferred);
                walk_expr(value, inferred);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                walk_expr(condition, inferred);
                walk_block(then_body, inferred);
                for (c, b) in elif_branches { walk_expr(c, inferred); walk_block(b, inferred); }
                if let Some(eb) = else_body { walk_block(eb, inferred); }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                walk_expr(scrutinee, inferred);
                for item in arms {
                    if let Some(arm) = item.arm_mut() {
                        if let Some(g) = arm.guard.as_mut() { walk_expr(g, inferred); }
                        walk_expr(&mut arm.body, inferred);
                    }
                }
                if let Some(eb) = else_arm { walk_block(eb, inferred); }
            }
            Stmt::For { iterable, body, else_body, .. } => {
                walk_expr(iterable, inferred);
                walk_block(body, inferred);
                if let Some(eb) = else_body { walk_block(eb, inferred); }
            }
            Stmt::While { condition, body, else_body } => {
                walk_expr(condition, inferred);
                walk_block(body, inferred);
                if let Some(eb) = else_body { walk_block(eb, inferred); }
            }
            Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
                walk_block(body, inferred);
            }
            _ => {}
        }
    }
    fn walk_expr(e: &mut Spanned<Expr>, inferred: &FxHashMap<usize, Vec<Type>>) {
        match &mut e.node {
            Expr::Call { callee, generic_args, args, .. } => {
                walk_expr(callee, inferred);
                for arg in args.iter_mut() {
                    walk_expr(&mut arg.node.value, inferred);
                }
                let already_has = generic_args.as_ref()
                    .map(|gs| !gs.is_empty())
                    .unwrap_or(false);
                if !already_has {
                    if let Some(types) = inferred.get(&callee.span.start) {
                        let dummy = Span { start: 0, end: 0 };
                        let spanned: Vec<Spanned<Type>> = types.iter()
                            .map(|t| Spanned { node: t.clone(), span: dummy })
                            .collect();
                        *generic_args = Some(spanned);
                    }
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                walk_expr(receiver, inferred);
                for arg in args.iter_mut() { walk_expr(&mut arg.node.value, inferred); }
            }
            Expr::StructLiteral { args, .. } => {
                for arg in args.iter_mut() { walk_expr(arg, inferred); }
            }
            Expr::BinaryOp { left, right, .. } => {
                walk_expr(left, inferred);
                walk_expr(right, inferred);
            }
            Expr::UnaryOp { operand, .. } => walk_expr(operand, inferred),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                walk_expr(object, inferred);
            }
            Expr::Index { object, index } => {
                walk_expr(object, inferred);
                walk_expr(index, inferred);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                walk_expr(condition, inferred);
                walk_expr(then_branch, inferred);
                if let Some(eb) = else_branch { walk_expr(eb, inferred); }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s, inferred); }
                if let Some(en) = end { walk_expr(en, inferred); }
            }
            Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner, inferred),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs, inferred);
                walk_expr(rhs, inferred);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                walk_expr(body, inferred);
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                for e in elems.iter_mut() { walk_expr(e, inferred); }
            }
            Expr::Block(b) => walk_block(b, inferred),
            Expr::StringLiteral(_, interp_exprs) => {
                for ie in interp_exprs.iter_mut() { walk_expr(ie, inferred); }
            }
            _ => {}
        }
    }
    walk_items(&mut module.items, inferred);
}

/// AST post-pass: rewrite `Set[T] x = expr.collect()` → `Set[T] x =
/// expr.to_set()` so that IR lowering dispatches the Set-targeted
/// trait default (see `Iterator[T]::to_set(&self)`) instead of the
/// Vector-targeted `.collect()`. Mirrors `apply_inferred_method_targs`
/// — run after typecheck, before IR lowering. Applies recursively to
/// all VarDecls (including inside function bodies, nested blocks,
/// and trait/equip method items).
///
/// Only rewrites when the RHS of the VarDecl is a MethodCall whose
/// method name is `collect`. Other call shapes (free-fn calls, inline
/// struct literals, etc.) stay untouched — the user explicitly picked
/// a different constructor.
///
/// Dict is a follow-up: `Dict[K, V] d = pairs.collect()` needs tuple
/// destructuring (trait's `T = (K, V)`) that the current signature
/// shape doesn't accommodate.
pub fn apply_collect_target_rewrites(module: &mut Module) {
    fn walk_items(items: &mut [Spanned<Item>]) {
        for item in items {
            match &mut item.node {
                Item::Module { items: inner, .. } => walk_items(inner),
                Item::Function(f) => walk_function(f),
                Item::Equip(eq) => {
                    for m in &mut eq.items {
                        walk_function(&mut m.node);
                    }
                }
                Item::Trait(td) => {
                    for ti in &mut td.items {
                        if let TraitItem::Method(m) = &mut ti.node {
                            walk_function(m);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    fn walk_function(f: &mut FunctionDef) {
        match &mut f.body {
            FunctionBody::Block(b) => walk_block(b),
            FunctionBody::Expression(e) => walk_expr(e),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }
    fn walk_block(b: &mut Block) {
        for stmt in &mut b.stmts {
            walk_stmt(&mut stmt.node);
        }
    }
    fn walk_stmt(s: &mut Stmt) {
        match s {
            Stmt::VarDecl { type_, value, .. } => {
                // If the declared type is `Set[T]`, rewrite an inner
                // `.collect()` call to `.to_set()`. If it's
                // `Dict[K, V]`, rewrite to `.to_dict[K, V]()` (type
                // args lifted from the LHS). Check AST directly — no
                // typecheck-resolved TypeId needed.
                if let Expr::MethodCall { method, generic_args, .. } = &mut value.node {
                    if method.node == "collect" {
                        if is_set_type(&type_.node) {
                            method.node = "to_set".to_string();
                        } else if let Some(kv) = dict_kv_args(&type_.node) {
                            method.node = "to_dict".to_string();
                            *generic_args = Some(kv);
                        }
                    }
                }
                walk_expr(value);
            }
            Stmt::Expr(e) | Stmt::Throw(e) => walk_expr(e),
            Stmt::Return(Some(e)) | Stmt::Break(Some(e)) => walk_expr(e),
            Stmt::Assign { target, value } | Stmt::CompoundAssign { target, value, .. } => {
                walk_expr(target);
                walk_expr(value);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                walk_expr(condition);
                walk_block(then_body);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond);
                    walk_block(body);
                }
                if let Some(eb) = else_body { walk_block(eb); }
            }
            Stmt::While { condition, body, else_body } => {
                walk_expr(condition);
                walk_block(body);
                if let Some(eb) = else_body { walk_block(eb); }
            }
            Stmt::For { iterable, body, else_body, .. } => {
                walk_expr(iterable);
                walk_block(body);
                if let Some(eb) = else_body { walk_block(eb); }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                walk_expr(scrutinee);
                for item in arms {
                    if let crate::parser::ast::MatchItem::Arm(arm) = item {
                        walk_expr(&mut arm.body);
                        if let Some(g) = &mut arm.guard { walk_expr(g); }
                    }
                }
                if let Some(b) = else_arm { walk_block(b); }
            }
            Stmt::With { bindings, body } => {
                for binding in bindings { walk_expr(&mut binding.expr); }
                walk_block(body);
            }
            Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
                walk_block(body);
            }
            _ => {}
        }
    }
    fn walk_expr(e: &mut Spanned<Expr>) {
        match &mut e.node {
            Expr::MethodCall { receiver, args, .. } => {
                walk_expr(receiver);
                for a in args { walk_expr(&mut a.node.value); }
            }
            Expr::Call { callee, args, .. } => {
                walk_expr(callee);
                for a in args { walk_expr(&mut a.node.value); }
            }
            Expr::StructLiteral { args, .. } => {
                for a in args { walk_expr(a); }
            }
            Expr::BinaryOp { left, right, .. } => {
                walk_expr(left);
                walk_expr(right);
            }
            Expr::UnaryOp { operand, .. } => walk_expr(operand),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                walk_expr(object);
            }
            Expr::Index { object, index } => {
                walk_expr(object);
                walk_expr(index);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                walk_expr(condition);
                walk_expr(then_branch);
                if let Some(eb) = else_branch { walk_expr(eb); }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s); }
                if let Some(e) = end { walk_expr(e); }
            }
            Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs);
                walk_expr(rhs);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => walk_expr(body),
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                for e in elems { walk_expr(e); }
            }
            Expr::Block(b) => walk_block(b),
            Expr::StringLiteral(_, interp_exprs) => {
                for ie in interp_exprs.iter_mut() { walk_expr(ie); }
            }
            _ => {}
        }
    }
    fn is_set_type(ty: &Type) -> bool {
        matches!(ty, Type::Named { name, generic_args }
            if (name.node == "Set" || name.node == "HashSet")
                && generic_args.len() == 1)
    }
    /// If `ty` is `Dict[K, V]` / `HashMap[K, V]`, return its generic
    /// args as a `Vec<Spanned<Type>>` ready to splice into a
    /// `MethodCall::generic_args` slot (lifting K and V from the
    /// LHS declared type onto the `.to_dict[K, V]()` call).
    fn dict_kv_args(ty: &Type) -> Option<Vec<Spanned<Type>>> {
        if let Type::Named { name, generic_args } = ty {
            if (name.node == "Dict" || name.node == "HashMap")
                && generic_args.len() == 2
            {
                return Some(generic_args.clone());
            }
        }
        None
    }
    walk_items(&mut module.items);
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
            Item::ExternBlock(ext) => {
                for func in &ext.items {
                    checker.register_function_signature(&func.node);
                }
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
                let value_ty = checker.infer_expr(&c.value);
                // Set DefInfo.type_id so format_types_canonical surfaces
                // the const's type. Without this, top-level constants like
                // `const float PI = 3.14...` don't appear in TYPE output.
                if let Some(def_id) = checker.scopes.lookup_def_by_span(&c.name.node, c.name.span) {
                    let declared = types::ast_type_to_resolved(
                        &c.type_.node, c.type_.span, checker.scopes, checker.types,
                    ).unwrap_or(value_ty);
                    checker.scopes.get_def_mut(def_id).type_id = Some(declared);
                }
            }
            Item::StaticDecl(s) => {
                let value_ty = checker.infer_expr(&s.value);
                // Same as ConstDecl above: surface the static's type so
                // top-level decls like `public static File stdin = ...`
                // appear in TYPE output.
                if let Some(def_id) = checker.scopes.lookup_def_by_span(&s.name.node, s.name.span) {
                    let declared = types::ast_type_to_resolved(
                        &s.type_.node, s.type_.span, checker.scopes, checker.types,
                    ).unwrap_or(value_ty);
                    checker.scopes.get_def_mut(def_id).type_id = Some(declared);
                }
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
        let errors = check("int add(int a, int b): a + b\n");
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
        let errors = check("int double(int x): x * 2\n");
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
T identity[T](T x): x

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
