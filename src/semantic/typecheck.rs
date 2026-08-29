use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{FallibleMarkReason, SemanticError, SemanticErrorKind};
use super::ids::{DefId, ScopeId, TypeId};
use super::MethodResolution;
use super::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use super::scope::{DefKind, DerefWrapperKind, ScopeKind, ScopeTable};
use super::traits::TraitRegistry;
use super::types::{self, ResolvedType, TypeTable};

/// Collect the generic-param names an `equip` block brings into scope for its
/// method bodies. Two sources:
///   1. The explicit `[T]` prefix (`equip [T] X[T]:` / `equip [K,V] X[K,V]:`).
///   2. The TARGET-IMPLICIT generics of `equip X[T]:` — the bare `Named { _, [] }`
///      args of the target type. These flow from the struct's own decl and are
///      NOT registered as scope defs, so a `T`-typed local in a method silently
///      resolves to `error_id` today. The unknown-type VarDecl check consults
///      this list so it doesn't flag such legit generics as undefined.
/// Collecting concrete-named args too (e.g. `equip X[SomeType]:`) is harmless:
/// those names resolve normally and would never be an unknown-type error anyway.
fn equip_generic_names(impl_block: &EquipBlock) -> Vec<String> {
    let mut names = Vec::new();
    if let Some(generics) = &impl_block.generic_params {
        for param in &generics.node.params {
            if let GenericParam::Type { name, .. } = &param.node {
                names.push(name.node.clone());
            }
        }
    }
    collect_bare_named_args(&impl_block.type_.node, &mut names);
    names
}

/// Reject module-level `const` decls whose initializer is NOT a compile-time
/// constant, by MIRRORING the lowering const-fold loop EXACTLY (`ir::lowering`
/// builds `module_constants` via a single-pass source-order fold over top-level
/// ConstDecl/MetaConst/MetaIf items using `eval_const_expr`). A ConstDecl whose
/// `eval_const_expr` returns `None` is one lowering CANNOT fold — it would
/// otherwise substitute a zero placeholder at every use site (a zeroed enum tag
/// reads as the ordinal-0 variant: `const Option[int] G = None` matched `Some`),
/// a silent miscompile. Driving the rejection off the REAL folder (one source of
/// truth, Core #1/#3 — NOT an AST-shape shadow) closes the whole class at once:
/// enum/struct/None constructors, non-const identifier refs (fn/static/var),
/// forward const-refs (not yet registered in single-pass order), and string
/// concatenation. Foldable forms still pass (literals, prior-const refs, numeric
/// arithmetic). The user reaches for `static` (runtime-initialized global).
fn check_module_const_foldability(checker: &mut TypeChecker, items: &[Spanned<Item>]) {
    use crate::ir::instructions::Constant;
    use crate::ir::lowering::eval_const_expr;
    // Same accumulator + source order as the lowering const-scan, so a ConstDecl
    // errors here IFF lowering would fail to fold it (= would silently miscompile).
    let mut known: rustc_hash::FxHashMap<String, Constant> = rustc_hash::FxHashMap::default();
    for item in items {
        match &item.node {
            Item::ConstDecl(c) => {
                // D26: reject fallible arithmetic in a const initializer BEFORE
                // eval_const_expr — the operator produces `Result[T, ArithError]`
                // which is not a foldable Constant. Recursive walk covers nested
                // shapes like `const int c = 5 + (1 +! 2)`.
                if let Some(bad_op) = find_fallible_arith(&c.value.node) {
                    checker.error(
                        SemanticErrorKind::FallibleOpInConst {
                            op: op_glyph_str(bad_op).to_string(),
                        },
                        c.value.span,
                    );
                    continue;
                }
                match eval_const_expr(&c.value.node, &known) {
                    Some(val) => {
                        known.insert(c.name.node.clone(), val);
                    }
                    None => checker.error(
                        SemanticErrorKind::NonConstantConstInitializer {
                            name: c.name.node.clone(),
                        },
                        c.value.span,
                    ),
                }
            }
            Item::MetaConst(mc) => {
                if let Some(bad_op) = find_fallible_arith(&mc.value.node) {
                    checker.error(
                        SemanticErrorKind::FallibleOpInConst {
                            op: op_glyph_str(bad_op).to_string(),
                        },
                        mc.value.span,
                    );
                    continue;
                }
                if let Some(val) = eval_const_expr(&mc.value.node, &known) {
                    known.insert(mc.name.node.clone(), val);
                }
            }
            Item::MetaIf(meta_if) => {
                if let Some(bad_op) = find_fallible_arith(&meta_if.condition.node) {
                    checker.error(
                        SemanticErrorKind::FallibleOpInConst {
                            op: op_glyph_str(bad_op).to_string(),
                        },
                        meta_if.condition.span,
                    );
                    continue;
                }
                let active = matches!(
                    eval_const_expr(&meta_if.condition.node, &known),
                    Some(Constant::Bool(true))
                );
                if active {
                    for sub in &meta_if.then_items {
                        if let Item::MetaConst(mc) = &sub.node {
                            if let Some(bad_op) = find_fallible_arith(&mc.value.node) {
                                checker.error(
                                    SemanticErrorKind::FallibleOpInConst {
                                        op: op_glyph_str(bad_op).to_string(),
                                    },
                                    mc.value.span,
                                );
                                continue;
                            }
                            if let Some(val) = eval_const_expr(&mc.value.node, &known) {
                                known.insert(mc.name.node.clone(), val);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// D26 (Round XXXIII Batch C1): recursively walk an expression AST looking for
/// a fallible-arithmetic binary op (`+!`, `-!`, `*!`, `/!`, `%!`, `<<!`, `>>!`).
/// Returns the offending `BinaryOp` on the first hit, so the caller can render
/// its glyph in the diagnostic. Const contexts can't hold `Result[T, ArithError]`
/// so any occurrence rejects; the walk covers nested shapes like
/// `5 + (1 +! 2)` (the fallible op sits inside a plain binop node).
fn find_fallible_arith(expr: &Expr) -> Option<BinaryOp> {
    match expr {
        Expr::BinaryOp { op, left, right } => {
            if op.is_fallible_arith() {
                return Some(*op);
            }
            find_fallible_arith(&left.node).or_else(|| find_fallible_arith(&right.node))
        }
        Expr::UnaryOp { operand, .. } => find_fallible_arith(&operand.node),
        _ => None,
    }
}

/// D26 (Round XXXIII Batch C1): plain-name glyph for a fallible-arithmetic
/// BinaryOp, used by `E_FallibleOpInConst` (const-context reject) and any
/// other site that needs the printed spelling without a `TypeChecker` in scope.
fn op_glyph_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::AddFallible => "+!",
        BinaryOp::SubFallible => "-!",
        BinaryOp::MulFallible => "*!",
        BinaryOp::DivFallible => "/!",
        BinaryOp::RemFallible => "%!",
        BinaryOp::ShlFallible => "<<!",
        BinaryOp::ShrFallible => ">>!",
        _ => "<binop>",
    }
}

/// Push every bare `Named { name, [] }` arg name found in the generic-arg
/// positions of `ty` (recursing through nested generics like `Pair[K, V]`).
fn collect_bare_named_args(ty: &Type, names: &mut Vec<String>) {
    if let Type::Named { generic_args, .. } = ty {
        for arg in generic_args {
            if let Type::Named { name, generic_args: inner } = &arg.node {
                if inner.is_empty() {
                    names.push(name.node.clone());
                } else {
                    collect_bare_named_args(&arg.node, names);
                }
            }
        }
    }
}

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

/// Round XXIII Track α: exhaustive enumeration of the 3 closure-returning
/// Option/Result combinator cells whose closure return must be unified
/// against a specific receiver payload axis. See
/// `TypeChecker::unify_closure_ret_axis` for the per-cell rule table,
/// exclusions, and the arm-count class-guard in `tests/lints.rs`.
///
/// Explicitly OUT-OF-CLASS (do NOT extend to these — the exclusion is
/// load-bearing, see the helper's doc-comment):
/// - `.map` / `.map_err` — scalar-returning closures, outer type
///   reconstructed from the scalar (no axis to unify).
/// - `Result.flat_map` — deliberately unregistered in `builtins.rs`
///   (assertion at ~:1425 forbids it).
/// - `Option.and_then` / `Option.flat_map` — legitimate cross-type
///   map (`T → Option[U]` where `U ≠ T` is the intended shape).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosureCombinatorCell {
    /// `Result[T, E].or_else((E) -> Result[T', E'])` — unify T' == T (Ok-axis).
    /// The Error axis IS the recovery axis; E' ≠ E is legitimate.
    ResultOrElse,
    /// `Result[T, E].and_then((T) -> Result[U, E'])` — unify E' == E (Err-axis).
    /// The Ok axis IS the mapped axis; U ≠ T is legitimate.
    ResultAndThen,
    /// `Option[T].or_else(() -> Option[T'])` — unify T' == T (Some-axis).
    /// Option has one payload; the recovery closure must produce the same T.
    OptionOrElse,
}

/// RV-A: whether a builtin wrapper's inner type (`Box[T]` → `T`) carries the
/// accessed field. Drives the 3-way field-access diagnostic — see
/// `TypeChecker::wrapper_inner_field_status`.
enum InnerFieldStatus {
    /// The inner is a known struct that HAS the field. `inner_name` is the
    /// inner struct's name (for the `E_DerefCoercionUnimplemented` message).
    Present { inner_name: String },
    /// The inner is a known struct WITHOUT the field, or a primitive (which
    /// has no named fields) — the field is definitely absent.
    Absent,
    /// The inner could not be resolved to a concrete field list (a bare
    /// wrapper with no type arg, a generic-param / opaque inner). Cannot prove
    /// present or absent — callers must not over-reject on this.
    Unknown,
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
    /// Errors that must survive the imported-module truncate at
    /// `check_items_recursive_tc`. Used for hard, concrete-vs-concrete
    /// type mismatches at call-argument sites in imported user modules
    /// — see gorget-js snag #2 (silent `to_uint32(float_arg)` in a
    /// 6000-line eval.gg, hidden by the blanket truncate). Merged into
    /// `errors` at end-of-check.
    hard_errors: Vec<SemanticError>,
    /// Substitution map: type variable ID -> resolved type ID.
    substitutions: FxHashMap<u32, TypeId>,
    next_type_var: u32,
    /// The return type of the current function being checked.
    current_return_type: Option<TypeId>,
    /// Whether the current function has `throws`.
    current_function_throws: bool,
    /// The resolved error TypeId the current function declares via `throws E`
    /// (the CALLER's error type), or `None` for a non-throws function. Snag
    /// #11: needed at the auto-propagation chokepoints to gate cross-error
    /// propagation — `current_function_throws` only records the boolean.
    current_fn_throws_type_id: Option<TypeId>,
    /// Whether the current function is `async`.
    current_function_is_async: bool,
    /// Type variable for implicit `it` parameter inside ImplicitClosure.
    implicit_it_type: Option<TypeId>,
    /// Map from expression span to its inferred TypeId (used by codegen for Result-based `?`).
    expr_types: FxHashMap<Span, TypeId>,
    /// Spans of statement-position / match-arm expressions whose inferred type
    /// is `Never` (panic, noreturn extern calls). Recorded during checking so
    /// the definite-return analysis can consult divergence without re-running
    /// inference.
    diverging_exprs: rustc_hash::FxHashSet<Span>,
    /// Spans of `match` statements/expressions (the span passed to
    /// `check_match_exhaustiveness`) whose arm set is known to cover every
    /// possible scrutinee value WITHOUT an `else` arm (exhaustive enum match
    /// or an unguarded catch-all pattern).
    /// Consulted by the definite-return analysis.
    exhaustive_matches: rustc_hash::FxHashSet<Span>,
    /// Map from method call span start → `MethodResolution` (D36: extended
    /// value type carries the resolved DefId + optional auto-deref wrapper
    /// kind, replacing the earlier parallel sidecar per Layering rule 3).
    method_resolutions: FxHashMap<usize, MethodResolution>,
    /// Snag #11: for each cross-error-type auto-propagation site that resolves
    /// to a `From[CalleeE]` impl on the caller's `CallerE`, the resolved
    /// `From::from` method DefId, keyed by the producing call expression's
    /// span. The lowering reads this to emit the `From` conversion on the
    /// error value before re-wrapping it in the caller's `Result`. Empty when
    /// every propagation is same-error-type (the byte-identical fast path).
    from_conversions: FxHashMap<Span, DefId>,
    /// The self type of the current equip block (if any).
    current_self_type: Option<TypeId>,
    /// Generic-param names in scope for the current equip block. Populated from
    /// BOTH the explicit `[T]` prefix (`equip [T] X[T]:`) AND the TARGET-IMPLICIT
    /// generics of `equip X[T]:` (where `T` flows from the struct's own decl and
    /// is NOT registered as a scope def — it silently resolves to `error_id`).
    /// The unknown-type check at the VarDecl site consults this so a `T`-typed
    /// local inside such a method isn't mistaken for an undefined type. Empty
    /// outside an equip block.
    current_equip_generics: Vec<String>,
    /// Declared type hint for integer literal coercion (e.g., uint8 x = 5).
    decl_type_hint: Option<TypeId>,
    /// One-shot flag, mirror of IR-lowering's `func_state.suppress_auto_prop`
    /// (`src/ir/lowering/exprs/mod.rs:78`). When set, the next throws-fn call
    /// inference (the Result-peel at `infer_expr` for a `Call`/`MethodCall`)
    /// keeps the raw `Result[T, E]` instead of peeling to `Ok(T)`. Set at the
    /// sites that genuinely want the whole Result — match scrutinee with
    /// Ok/Error/Some/None arms, `catch` inner, `rethrow` inner — mirroring the
    /// lowering suppress set. Consumed (reset to false) at the start of every
    /// `infer_expr`, so it only affects the immediately-enclosing producer,
    /// not nested sub-expressions. (No `==`/`!=` suppress is needed: the peel
    /// is gated to *throws*-fn calls, and an explicit `Result`-returning fn —
    /// the only `==` operand whose raw-vs-peeled compare differs — is never
    /// peeled at the producer, so `make(1) == make(1)` is unchanged.)
    suppress_auto_prop: bool,
    /// D29 one-shot, mirror of `suppress_auto_prop`: set by the `Expr::Propagate`
    /// (`expr!`) arm while inferring its inner call, so the fallible-call
    /// produce site (throws chokepoint / kind-2 detection) can verify the
    /// mandatory `!` is present. Consumed (reset) at the next `infer_expr`
    /// entry, so it is seen only at the immediately-enclosed producer, not at a
    /// nested sub-expression.
    fallible_call_marked: bool,
    /// stage-1b #2 one-shot: set by the `Expr::Call` / `Expr::MethodCall` arms
    /// just before inferring the callee / static receiver, so the `Expr::Identifier`
    /// arm permits a bare type name THERE (a constructor callee / static-method
    /// receiver) while rejecting it in every genuine value position (`match
    /// Direction:`, `Point p = Point`). Consumed (reset) at the next `infer_expr`
    /// entry so it never leaks to nested sub-expressions.
    type_name_position_ok: bool,
    /// D29 R3 (lying marks): set by the fallible-call produce sites
    /// (`resolve_throws_call_type` / `resolve_kind2_call_type`) when they see a
    /// marked call — proof the `!` was CONSUMED by a genuine fallible call. The
    /// `Expr::Propagate` arm verifies it after inferring its inner: a mark that
    /// no chokepoint consumed (`5!`, `pure(3)!`, `r!` on a Result local, the
    /// second mark of `f()!!`) is an error — an unverified mark is a lie the
    /// visibility doctrine cannot afford (and the migration's checker-as-oracle
    /// would silently bless).
    fallible_mark_consumed: bool,
    /// D29 R4 (tail discards): true while checking a block whose TAIL value is
    /// dropped — a function block body (block tails are never implicit returns;
    /// explicit `return` is required) or a loop body (the per-iteration value is
    /// discarded — the silent-Error-drop-per-iteration shape). `check_block`
    /// runs the bare-fallible-discard check on the tail statement exactly when
    /// this is set. Expression blocks (`Expr::Block`/`Expr::Do`) clear it (their
    /// tail IS consumed as the expression's value); nested statement blocks
    /// (if/match branches, `with`/`unsafe` bodies) inherit it.
    tail_value_dropped: bool,
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
    /// unresolved param. See `docs/devbook/09-type-checking.md`
    /// (method-level generic inference) risk #3 for the design.
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
            hard_errors: Vec::new(),
            substitutions: FxHashMap::default(),
            next_type_var: 0,
            current_return_type: None,
            current_function_throws: false,
            current_fn_throws_type_id: None,
            current_function_is_async: false,
            implicit_it_type: None,
            expr_types: FxHashMap::default(),
            diverging_exprs: rustc_hash::FxHashSet::default(),
            exhaustive_matches: rustc_hash::FxHashSet::default(),
            method_resolutions: FxHashMap::default(),
            from_conversions: FxHashMap::default(),
            current_self_type: None,
            current_equip_generics: Vec::new(),
            decl_type_hint: None,
            suppress_auto_prop: false,
            fallible_call_marked: false,
            type_name_position_ok: false,
            fallible_mark_consumed: false,
            tail_value_dropped: false,
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

    /// Returns true when a TypeId is fully concrete: no Vars (resolved or
    /// otherwise), no error_id, no never_id. Used to gate `hard_type_mismatch`
    /// emission — non-concrete types may still be unified successfully or
    /// represent foreign-scope inference holes, so we only mark a mismatch
    /// "hard" when both sides are firmly known.
    fn is_fully_concrete(&self, t: TypeId) -> bool {
        let r = self.resolve_type(t);
        if r == self.types.error_id || r == self.types.never_id {
            return false;
        }
        match self.types.get(r) {
            ResolvedType::Var(_) => false,
            ResolvedType::Generic(_, args) => {
                let args = args.clone();
                args.iter().all(|&a| self.is_fully_concrete(a))
            }
            ResolvedType::Tuple(elems) => {
                let elems = elems.clone();
                elems.iter().all(|&e| self.is_fully_concrete(e))
            }
            ResolvedType::Array(elem, _)
            | ResolvedType::Slice(elem)
            | ResolvedType::Ref(elem)
            | ResolvedType::Owned(elem) => {
                let elem = *elem;
                self.is_fully_concrete(elem)
            }
            _ => true,
        }
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
        // Resolve through the substitution map so diagnostics show the
        // bound type, not the fresh-Var placeholder. Composite types
        // (Generic args, etc.) recurse through this same function so
        // each leaf gets resolved.
        let type_id = self.resolve_type(type_id);
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

    /// RV-A: resolve a builtin wrapper's inner type (`Box[T]` → `T`) and
    /// classify whether `field` exists on it. Keys the 3-way field-access
    /// diagnostic (guard accept vs. Box deref-coercion-unimplemented vs.
    /// absent). Only ever called for a def already flagged
    /// `deref_wrapper_kind = Some(_)`, so this is pure inner-type resolution —
    /// no name-matching.
    fn wrapper_inner_field_status(
        &self,
        wrapper_rt: &ResolvedType,
        field: &str,
    ) -> InnerFieldStatus {
        let targs = match wrapper_rt {
            ResolvedType::Generic(_, targs) => targs,
            // A bare wrapper with no type arg — inner is unknown.
            _ => return InnerFieldStatus::Unknown,
        };
        let Some(&inner) = targs.first() else {
            return InnerFieldStatus::Unknown;
        };
        let inner_r = self.resolve_type(inner);
        match self.types.get(inner_r).clone() {
            ResolvedType::Defined(idid) | ResolvedType::Generic(idid, _) => {
                match self.struct_fields.get(&idid) {
                    Some(sfi) => {
                        if sfi.fields.iter().any(|(n, _)| n.as_str() == field) {
                            InnerFieldStatus::Present {
                                inner_name: self.scopes.get_def(idid).name.clone(),
                            }
                        } else {
                            InnerFieldStatus::Absent
                        }
                    }
                    // Inner is a type with no user field list (generic param,
                    // enum, opaque builtin) — can't prove present or absent.
                    None => InnerFieldStatus::Unknown,
                }
            }
            // A primitive inner (`Box[int]`) has no named fields → absent.
            ResolvedType::Primitive(_) => InnerFieldStatus::Absent,
            _ => InnerFieldStatus::Unknown,
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

    /// Type a `catch` recovery / `fault-catch` handler against its expected
    /// slot type — mirror of the canonical THREE-carve-out unify contract
    /// every "value into expected slot" site uses (VarDecl `:4848-4850`,
    /// Assign, free-fn arg, method arg, Return — all identical). Do NOT
    /// diverge from this set: the whole point of the helper is to route the
    /// recovery slot through the SAME "expected vs actual" contract as its
    /// siblings so literal shapes (`[]`, `None`) coerce here the way they
    /// coerce there, and divergent recoveries (`return`, `throw`, `panic`)
    /// pass via `unify`'s Never rule at `:985-990`.
    ///
    /// Track A · Core #10 (lower-or-reject / silent-fallthrough): before
    /// this helper the `Expr::Catch` arm called `infer_expr(recovery)` and
    /// DISCARDED the result, so the outer VarDecl unified a fabricated OK
    /// type against itself and any wrong-typed recovery reached codegen
    /// (silent heap-ptr-as-int64 on same-layout mismatches). Every arm of
    /// that class now routes here.
    /// `tests/lints.rs::recovery_arms_route_through_check_recovery_type`
    /// pins the invariant.
    fn check_recovery_type(&mut self, recovery: &Spanned<Expr>, expected: TypeId) {
        let prev_hint = self.decl_type_hint;
        self.decl_type_hint = Some(expected);
        let actual = self.infer_expr(recovery);
        self.decl_type_hint = prev_hint;
        if !self.is_collection_assignment(expected, actual)
            && !self.auto_prop_skips_unify(expected, actual, recovery.span)
            && !self.is_result_capture_compatible(expected, actual)
        {
            self.unify(expected, actual, recovery.span);
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
        // One-shot consume, mirror of IR-lowering's `lower_expr` entry
        // (`src/ir/lowering/exprs/mod.rs:78`). Captured here and read at the
        // throws-fn-call produce site (the Result-peel); reset so nested
        // sub-expressions auto-prop normally.
        let suppress_auto_prop = std::mem::replace(&mut self.suppress_auto_prop, false);
        // D29 one-shot: same discipline for the `!` mark — captured here, read
        // at the fallible-call produce site, reset so nested args don't inherit.
        let fallible_call_marked = std::mem::replace(&mut self.fallible_call_marked, false);
        // stage-1b #2 one-shot: whether THIS expr sits in a position where a bare
        // type name is legitimate (a call callee / static-method receiver). Set by
        // the Call/MethodCall arms just before inferring the callee/receiver;
        // consumed here so it never leaks to nested sub-expressions.
        let type_name_position_ok = std::mem::replace(&mut self.type_name_position_ok, false);
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
                // The old behavior was `self.errors.truncate(saved_err_len)` —
                // discarding ALL interpolation errors to swallow the
                // polymorphic-unify noise a bound-to-local `abs(-2.5)` would
                // otherwise trip. But that also SILENTLY SWALLOWED genuine
                // method-existence errors inside `f"{...}"` — e.g. `f"{s.str()}"`
                // or `f"{s.bogus()}"` would pass typecheck even though the same
                // call rejects outside an f-string (round-31 primitive-method
                // reject, #1). Instead of truncating, split off the
                // interpolation errors and RETAIN only the kinds that mean "the
                // method / unwrap genuinely doesn't exist" (never the
                // TypeMismatch unify-noise the truncation was there for). This
                // keeps `f"{abs(x)}"` green while surfacing `f"{s.str()}"`.
                let interp_errs = self.errors.split_off(saved_err_len);
                self.errors.extend(interp_errs.into_iter().filter(|e| {
                    // ⚠ ARM COUNT PINNED at tests/lints.rs::interp_error_retention_arms_count.
                    // If you add a new gate that emits a semantic error the user
                    // should see inside `f"{...}"`, ADD IT TO THIS WHITELIST or
                    // the error is silently swallowed and the defect ships
                    // (Round XXIX Track A residual: NotIndexable / NotIndexableMut
                    // missing → `print(f"{p[5]}")` silent-accept + OOB on both
                    // backends until owner filing `17a3e342`. Sibling widening
                    // for name/arg/field-resolution family closed by owner
                    // filing `97cd5c01` same-round: `E_UndefinedName`,
                    // `E_WrongArgCount`, `E_NotAFunction` + obvious siblings.)
                    //
                    // The whitelist DELIBERATELY EXCLUDES `TypeMismatch` — that's
                    // the polymorphic-unify noise the truncation was originally
                    // there to swallow (per the paragraph above).
                    matches!(
                        e.kind,
                        SemanticErrorKind::NoMethodFound { .. }
                            | SemanticErrorKind::MethodGenericInferenceFailed { .. }
                            | SemanticErrorKind::UnwrapOnNonOptional { .. }
                            | SemanticErrorKind::NotIndexable { .. }
                            | SemanticErrorKind::NotIndexableMut { .. }
                            // Round XXIX close sibling chip (owner filing 97cd5c01) —
                            // name / arg / field-resolution family. All represent
                            // "the identifier/field/function/arg genuinely doesn't
                            // exist or the shape is wrong", never the unify-noise
                            // TypeMismatch was the original swallow-reason for.
                            | SemanticErrorKind::UndefinedName { .. }
                            | SemanticErrorKind::WrongArgCount { .. }
                            | SemanticErrorKind::NotAFunction { .. }
                            | SemanticErrorKind::NoFieldFound { .. }
                            | SemanticErrorKind::MissingRequiredArg { .. }
                            | SemanticErrorKind::UnknownNamedArg { .. }
                            | SemanticErrorKind::NotAType { .. }
                            | SemanticErrorKind::NotAStruct { .. }
                            | SemanticErrorKind::TupleIndexOutOfBounds { .. }
                            | SemanticErrorKind::EnumerateOnNonIterator { .. }
                            // Round XXXIX Track E: `??` RHS-type reject (Option B
                            // chain-friendly). A genuine "the RHS is the wrong
                            // carrier/inner-type" reject — never the unify-noise
                            // TypeMismatch was the original swallow-reason for.
                            | SemanticErrorKind::DefaultOpRhsTypeMismatch { .. }
                    )
                }));
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
                    // stage-1b #2: a type-defining name used as a VALUE (not as a
                    // constructor callee / static-method receiver — those set
                    // `type_name_position_ok`) is rejected. `match Direction:`,
                    // `Point p = Point` type-checked clean then SIGSEGV'd at
                    // runtime (a type name is not a value). Struct ctors are
                    // rewritten to StructLiteral pre-typecheck so they never
                    // reach here; Variant is EXCLUDED because a bare variant is
                    // a legitimate first-class constructor value (`xs.map(Some)`).
                    if !type_name_position_ok
                        && matches!(
                            def.kind,
                            DefKind::Struct
                                | DefKind::Enum
                                | DefKind::Newtype
                                | DefKind::Trait
                                | DefKind::TypeAlias
                        )
                    {
                        let kind_word = match def.kind {
                            DefKind::Struct => "struct",
                            DefKind::Enum => "enum",
                            DefKind::Newtype => "type",
                            DefKind::Trait => "trait",
                            DefKind::TypeAlias => "type alias",
                            _ => "type",
                        };
                        self.error(
                            SemanticErrorKind::TypeInValuePosition {
                                name: name.clone(),
                                kind: kind_word.to_string(),
                            },
                            expr.span,
                        );
                        return self.types.error_id;
                    }
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

            // BEHAVIOUR-PRESERVING forward of the placeholder identifier this
            // node replaced: `__return__` resolved to nothing, so this arm's
            // predecessor returned `error_id` and the postcondition condition
            // went UNTYPED (`assert return >= "hello"` in an `int` function
            // checks clean). Typing it to the enclosing function's return type
            // would be an accept→reject change; the gap is filed
            // (`tests/fixtures/known_gaps/assert_return_condition_untyped.gg`
            // + its `#[ignore]`d test) and closed on its own round.
            Expr::ReturnValue => self.types.error_id,

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

                // Thread the LHS type as a hint for the RHS literal so byte
                // literals (`b'X'`, lexed as Token::IntLiteral) coerce to
                // the LHS's sized integer type at the `IntLiteral` typing
                // path (line ~1006). Same mechanism that lets `byte x = 5`
                // coerce the literal at VarDecl. Only triggers when the
                // LHS is fully concrete (resolved primitive) and the RHS
                // is a bare integer literal (with optional unary minus) —
                // anything else uses the normal independent-inference
                // path. Without this, BinaryOp drops the LHS context and
                // unify(uint8, int) fails because int → uint8 isn't safe
                // widening.
                let rhs_hint = if matches!(
                    op,
                    BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt
                    | BinaryOp::LtEq | BinaryOp::GtEq
                    | BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul
                    | BinaryOp::Div | BinaryOp::Rem | BinaryOp::Mod
                    | BinaryOp::AddWrap | BinaryOp::SubWrap | BinaryOp::MulWrap
                    | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor
                    | BinaryOp::Shl | BinaryOp::Shr
                ) {
                    let rhs_is_int_literal = matches!(
                        &right.node,
                        Expr::IntLiteral(_)
                    ) || matches!(
                        &right.node,
                        Expr::UnaryOp { op: UnaryOp::Neg, operand }
                            if matches!(operand.node, Expr::IntLiteral(_))
                    );
                    if rhs_is_int_literal {
                        let lhs_resolved = self.resolve_type(left_type);
                        // Peel Ref/Owned wrappers (auto-deref) before the integer-primitive
                        // gate, mirroring the cast-castability peel at lines ~2667. An inline
                        // `Vector[uint8].get(i).unwrap()` resolves to Ref(uint8); thread the
                        // peeled-inner Primitive typeid as the hint (the IntLiteral consumer
                        // requires a Primitive hint — a Ref hint would no-op).
                        let lhs_inner = match self.types.get(lhs_resolved) {
                            ResolvedType::Ref(inner) | ResolvedType::Owned(inner) => {
                                self.resolve_type(*inner)
                            }
                            _ => lhs_resolved,
                        };
                        if matches!(
                            self.types.get(lhs_inner),
                            ResolvedType::Primitive(p) if is_integer_type(p)
                        ) {
                            Some(lhs_inner)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };
                let right_type = if let Some(hint) = rhs_hint {
                    let prev_hint = self.decl_type_hint;
                    self.decl_type_hint = Some(hint);
                    let t = self.infer_expr(right);
                    self.decl_type_hint = prev_hint;
                    t
                } else {
                    self.infer_expr(right)
                };

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
                        let result = self.unify(left_type, right_type, expr.span);
                        // Same support gate as CompoundAssign — closes
                        // `s - "x"` / `m - r` accept-then-broken-C hole.
                        self.check_operator_supported(
                            left_type,
                            *op,
                            /*compound=*/ false,
                            expr.span,
                        );
                        result
                    }
                    // D28 `**` — amendment R3 no-type-switching. Both operands
                    // must be the same numeric type (int×int → int, float×float
                    // → float). Mixed or non-numeric operands REJECT with
                    // `E_TypeMismatchInPow`. The negative-exp + overflow trap
                    // semantics are enforced at runtime by the checked helper
                    // (see `src/backend/c/runtime/runtime_math.c`).
                    BinaryOp::Pow => {
                        let l_resolved = self.resolve_type(left_type);
                        let r_resolved = self.resolve_type(right_type);
                        let l_is_int = matches!(
                            self.types.get(l_resolved),
                            ResolvedType::Primitive(p) if is_integer_type(p)
                        );
                        let r_is_int = matches!(
                            self.types.get(r_resolved),
                            ResolvedType::Primitive(p) if is_integer_type(p)
                        );
                        let l_is_float = matches!(
                            self.types.get(l_resolved),
                            ResolvedType::Primitive(
                                PrimitiveType::Float
                                | PrimitiveType::Float32
                                | PrimitiveType::Float64
                            )
                        );
                        let r_is_float = matches!(
                            self.types.get(r_resolved),
                            ResolvedType::Primitive(
                                PrimitiveType::Float
                                | PrimitiveType::Float32
                                | PrimitiveType::Float64
                            )
                        );
                        // Cascade guard: any Error / Var / Never in operand
                        // skips the reject to avoid noisy cascading messages.
                        let has_error = matches!(
                            self.types.get(l_resolved),
                            ResolvedType::Error | ResolvedType::Var(_) | ResolvedType::Never
                        ) || matches!(
                            self.types.get(r_resolved),
                            ResolvedType::Error | ResolvedType::Var(_) | ResolvedType::Never
                        );
                        let both_int = l_is_int && r_is_int;
                        let both_float = l_is_float && r_is_float;
                        if !has_error && !(both_int || both_float) {
                            let l_desc = self.describe_resolved_type(l_resolved);
                            let r_desc = self.describe_resolved_type(r_resolved);
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::TypeMismatchInPow {
                                    left: l_desc,
                                    right: r_desc,
                                },
                                span: expr.span,
                            });
                            return self.types.error_id;
                        }
                        // Unify to nail down the result type; on both-int or
                        // both-float this is the same type as the operands.
                        self.unify(left_type, right_type, expr.span)
                    }
                    // Bitwise operators — result is same type (integer only)
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor
                    | BinaryOp::Shl | BinaryOp::Shr => {
                        let result = self.unify(left_type, right_type, expr.span);
                        self.check_operator_supported(
                            left_type,
                            *op,
                            /*compound=*/ false,
                            expr.span,
                        );
                        result
                    }
                    // D26 fallible arithmetic (Round XXXIII Batch C1). The `+!`
                    // / `-!` / `*!` / `/!` / `%!` / `<<!` / `>>!` operators
                    // are integer-only and produce `Result[T, ArithError]`.
                    // Auto-propagation is D29 (D29 disposition table via
                    // `resolve_throws_call_type`), with the `!` glyph counting
                    // as the fallible mark. Non-integer operands reject with
                    // `E_FallibleArithmeticOnNonInt`. See docs/language-reference.md.
                    _ if op.is_fallible_arith() => {
                        self.check_fallible_arith_binop(left_type, right_type, *op, expr.span)
                    }
                    _ => unreachable!(
                        "op {:?} should have matched an earlier arm — new BinaryOp variant missing a match arm?",
                        op
                    ),
                }
            }

            Expr::Call { callee, generic_args, args, .. } => {
                // A bare type-name callee (variant ctor `Some(x)`, or a static-
                // method receiver resolved elsewhere) is a legitimate type name
                // in expression position — suppress the value-position reject for
                // this immediate callee infer (one-shot, consumed at infer entry).
                self.type_name_position_ok = true;
                let callee_type = self.infer_expr(callee);
                let resolved = self.resolve_type(callee_type);
                // Track B3 prototype: record the callee's type so the safety
                // pass's indirect-call ownership check can consult it for
                // non-identifier callees (Vec[Callable[..]]'s `arr[0]`,
                // struct field callable `h.f`, an IIFE closure literal).
                self.expr_types.insert(callee.span, callee_type);

                // Round XXII Track δ: bare `None()` — mirror the IR lowerer at
                // `src/ir/lowering/exprs/mod.rs:263` where `Call { callee:
                // NoneLiteral }` delegates to the bare `NoneLiteral` arm's
                // `materialise_none_for_expected_type`. Without a check-time
                // parallel, the callee typed as `Option[?T]`, fell through the
                // wildcard at :2410, and returned `error_id` at :2469, which
                // then unified silently with any dest type (see
                // `unify` fast-path at :953). That was a silent-wrong-output
                // CLASS: `int a = None()` bound 0, `Some(3).map((int x): None())`
                // printed `Some: 1`, `Result[int,int]` `Ok(3).map((int x): None())`
                // printed `Ok: 0` — both backends agreeing on the wrong answer
                // (Core #8). Reads the typed `Expr::NoneLiteral` variant
                // discriminator, not a name (Core #2). The infer_expr(callee)
                // call above already produced the correct `Option[T]` via the
                // NoneLiteral arm's `decl_type_hint` logic (:1375-1398), so we
                // just return callee_type; the surrounding unify then rejects
                // the non-Option destination cleanly. Extra args (`None(1)`,
                // `None(x, y)`) reject as WrongArgCount — Core #10 lower-or-
                // reject: don't silently drop the call.
                if matches!(callee.node, Expr::NoneLiteral) {
                    if !args.is_empty() {
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        self.error(
                            SemanticErrorKind::WrongArgCount {
                                expected: 0,
                                found: args.len(),
                            },
                            expr.span,
                        );
                    }
                    return callee_type;
                }

                // Track R (owner Q1 2026-07-28): NonDerefContainer[BareTrait]
                // reject at CALL-EXPRESSION generic-arg position (7th user-facing
                // surface — Track P covered the 6 annotation-shaped surfaces via
                // `ast_type_to_resolved`). Constructor calls whose callee is a
                // builtin container name (Mutex / RWLock / Shared / Weak — the
                // `DefKind::Import` placeholder registered in
                // `resolve.rs::collect_top_level`, with `deref_wrapper_kind`
                // seeded to `NonDerefContainer` in the same pass) NEVER route
                // their generic-args through `ast_type_to_resolved` at this
                // position: the callee's `Identifier` types as `error_id`
                // (Import has no `type_id`), and the resulting
                // `ResolvedType::Error` branch below has no arm for
                // `DefKind::Import`, so `generic_args` is silently dropped and
                // the whole call types as `error_id`. Without Track P (which
                // fires on the ANNOTATION only), `Mutex[Speaker](Robot(...))`
                // in expression position (no var-decl annotation to catch it
                // upstream) reached C-emit and fabricated `gorget_guard_greet`
                // / `int64_t__greet` — Track M classes 1 and 3. Reads the
                // typed metadata (`deref_wrapper_kind`), not the container
                // name (layering rule 2). Emitted BEFORE `ast_type_to_resolved`
                // is called on the annotation (via the enclosing var-decl /
                // fn-param / field), so on `Mutex[Speaker] m = Mutex[Speaker](...)`
                // the annotation reject fires FIRST at the LHS span and the
                // call-site reject at the RHS span is suppressed by the
                // no-double-report gate below.
                if let Expr::Identifier(cname) = &callee.node {
                    if let Some(type_args) = generic_args.as_ref() {
                        if type_args.len() == 1 {
                            if let Some(def_id) = self.resolve_name(callee.span.start, cname) {
                                let def = self.scopes.get_def(def_id);
                                if def.deref_wrapper_kind
                                    == Some(DerefWrapperKind::NonDerefContainer)
                                {
                                    if let Ok(inner_tid) = super::types::ast_type_to_resolved(
                                        &type_args[0].node,
                                        type_args[0].span,
                                        self.scopes,
                                        self.types,
                                    ) {
                                        if let Some(trait_name) = super::types::trait_name_of_inner(
                                            inner_tid, self.scopes, self.types,
                                        ) {
                                            // Suppress double-report when a
                                            // surrounding annotation would emit
                                            // the same class at the LHS span
                                            // (`Mutex[Speaker] m = Mutex[Speaker](...)`
                                            // : Track P already fires at the
                                            // annotation before infer_expr walks
                                            // the RHS). We test the errors buffer
                                            // for a matching NonDerefContainerBareTrait
                                            // entry.
                                            let already = self.errors.iter().any(|e| matches!(
                                                &e.kind,
                                                SemanticErrorKind::NonDerefContainerBareTrait {
                                                    container: c, trait_: t,
                                                } if c == &def.name && t == &trait_name
                                            ));
                                            if !already {
                                                self.error(
                                                    SemanticErrorKind::NonDerefContainerBareTrait {
                                                        container: def.name.clone(),
                                                        trait_: trait_name,
                                                    },
                                                    callee.span,
                                                );
                                            }
                                            for arg in args {
                                                self.infer_expr(&arg.node.value);
                                            }
                                            return self.types.error_id;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Unlowered builtin "cast call" names (Chain C item 6+):
                // the resolver's `is_builtin` accepts these so `gg check`
                // passed, but they have NO lowering — the emitted C gets a
                // raw extern call (`undefined reference to 'int8'`). Only
                // `int` / `float` / `bool` have real cast lowerings
                // (`src/lir/lower/insts.rs`); `str(x)` is rejected by
                // OWNER DECISION (2026-06-10) — a free `str()` would be a
                // third conversion way beside f-strings / `.display()`,
                // against the one-obvious-way target. Gate the CLASS at
                // check time with a teaching error; a user-DEFINED fn of
                // the same name resolves and is not gated.
                if let Expr::Identifier(cname) = &callee.node {
                    if matches!(
                        cname.as_str(),
                        "str" | "byte"
                            | "int8" | "int16" | "int32" | "int64"
                            | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
                            | "float32" | "float64"
                    ) && self.resolve_name(callee.span.start, cname).is_none()
                    {
                        self.error(
                            SemanticErrorKind::UnloweredBuiltinCall {
                                name: cname.clone(),
                            },
                            expr.span,
                        );
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        return self.types.error_id;
                    }
                }

                // Validate `alloc=` named arg on builtin constructors
                if let Expr::Identifier(cname) = &callee.node {
                    let is_builtin_ctor = matches!(cname.as_str(),
                        "Vector" | "Dict" | "HashMap"
                        | "Set" | "HashSet" | "Channel" | "String" | "Arena" | "PoolAllocator" | "TlsfAllocator"
                        | "FixedBufferAllocator" | "FallbackAllocator" | "TrackingAllocator"
                    );
                    // `cap=` exists only where the ctor has a capacity
                    // param: the collection reserve, String/Channel
                    // buffers, and the single-capacity allocators.
                    // PoolAllocator(block_size, initial_count) /
                    // FallbackAllocator(primary, secondary) /
                    // TrackingAllocator() have no capacity axis — `cap=`
                    // there is an unknown name, same as on a user fn
                    // without that param (round-33).
                    let accepts_cap = matches!(cname.as_str(),
                        "Vector" | "Dict" | "HashMap" | "Set" | "HashSet"
                        | "Channel" | "String" | "Arena" | "TlsfAllocator"
                        | "FixedBufferAllocator"
                    );
                    if is_builtin_ctor {
                        // Duplicate named args used to be SILENTLY
                        // first-wins (`Vector[int](cap=4, cap=8)` reserved
                        // 4) — reject like the user-fn named-arg path
                        // (check_named_args_and_defaults) does (round-33).
                        let mut seen_cap = false;
                        let mut seen_alloc = false;
                        for arg in args {
                            if let Some(ref name) = arg.node.name {
                                if name.node == "cap" && accepts_cap {
                                    if seen_cap {
                                        self.error(
                                            SemanticErrorKind::DuplicateNamedArg { name: name.node.clone() },
                                            arg.span,
                                        );
                                    }
                                    seen_cap = true;
                                    // `cap=` takes an integer capacity (any int
                                    // width) — round-33. Before this reject the
                                    // value was "type-inferred and deferred to
                                    // lowering", where a non-int cap either
                                    // ICE'd the backend (`String(cap=true)`:
                                    // emit_types.rs GorgetString-ABI panic in
                                    // debug; llc i1-vs-GorgetString under
                                    // --backend=llvm), died as an
                                    // unintelligible cc error
                                    // (`Vector[int](cap="x")`: incompatible
                                    // arg 2 of `*__reserve`), or — worst —
                                    // silently wrong-accepted
                                    // (`Vector[int](cap=true)` reserved 1 via
                                    // C implicit conversion while llc rejected
                                    // the same program; `String(cap="x")`
                                    // treated the cap as CONTENT). Same
                                    // reject shape + `is_integer_type`
                                    // predicate as the positional `String(x)`
                                    // arm below (Core #4 — one predicate, no
                                    // parallel list).
                                    let cap_type = self.infer_expr(&arg.node.value);
                                    let cap_resolved = self.resolve_type(cap_type);
                                    let (cap_inner, cap_ok) = self.int_capacity_check(cap_resolved);
                                    if !cap_ok {
                                        self.error(
                                            SemanticErrorKind::TypeMismatch {
                                                expected: "cap= takes an integer capacity (any int width), e.g. cap=64".to_string(),
                                                found: self.describe_resolved_type(cap_inner),
                                            },
                                            arg.node.value.span,
                                        );
                                    }
                                } else if name.node != "alloc" {
                                    self.error(
                                        SemanticErrorKind::UnknownNamedArg { name: name.node.clone() },
                                        arg.span,
                                    );
                                } else {
                                    if seen_alloc {
                                        self.error(
                                            SemanticErrorKind::DuplicateNamedArg { name: name.node.clone() },
                                            arg.span,
                                        );
                                    }
                                    seen_alloc = true;
                                    // Validate the alloc= value type is an allocator
                                    let alloc_type = self.infer_expr(&arg.node.value);
                                    let alloc_resolved = self.resolve_type(alloc_type);
                                    if !self.is_allocator_arg_type(alloc_resolved) {
                                        self.error(
                                            SemanticErrorKind::TypeMismatch {
                                                expected: "allocator type (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, or FallbackAllocator)".to_string(),
                                                found: self.describe_resolved_type(alloc_resolved),
                                            },
                                            arg.node.value.span,
                                        );
                                    }
                                }
                            }
                        }
                        // `String(x)` positional 1-arg ctor accepts exactly two
                        // shapes: String(<int>) — pre-allocate n bytes capacity
                        // (any int width) — and String(<String>) — content
                        // (string/char literals, f-strings, identity). Anything
                        // else (bool/float/struct/...) used to fall through GIR
                        // lowering into `gorget_string_from_str(<non-string>)`
                        // and die at the C/LLVM toolchain with an
                        // unintelligible internal error (cc "incompatible type
                        // for argument 1"; a debug-only emit_types.rs ICE) — a
                        // language-level reject belongs here instead (Core #8).
                        // Named-arg forms (`String(cap=16)`, `String(alloc=a)`)
                        // are exempt: they're validated by the loop above, so
                        // gate on `name.is_none()`. The owner-approved
                        // cast-via-construction RFC (TODO.md) will later turn
                        // `String(T)` into a display conversion; until then the
                        // hint points at f-strings.
                        // A String ctor takes at most ONE content/capacity source:
                        // a single positional arg (content or capacity) OR cap=,
                        // optionally combined with alloc=. Multi-source shapes
                        // (`String("a", "b")`, `String("a", cap=4)`) used to slip
                        // through (only the 1-arg form was validated) and fall
                        // past the GIR String intercept into a call to an
                        // undefined `String` symbol — an unintelligible cc/llc
                        // error. Reject at check time instead (Core #8).
                        if cname == "String" {
                            let positional_count = args.iter().filter(|a| a.node.name.is_none()).count();
                            // Distinct sources: duplicate cap= is already
                            // a DuplicateNamedArg above — don't cascade.
                            let cap_count = usize::from(args.iter().any(|a| {
                                a.node.name.as_ref().map_or(false, |n| n.node == "cap")
                            }));
                            if positional_count + cap_count > 1 {
                                self.error(
                                    SemanticErrorKind::TypeMismatch {
                                        expected: "a single content or capacity argument — String(s), String(n), or String(cap=n), optionally with alloc=".to_string(),
                                        found: format!("{} content/capacity arguments", positional_count + cap_count),
                                    },
                                    expr.span,
                                );
                            }
                        }
                        if cname == "String" && args.len() == 1 && args[0].node.name.is_none() {
                            let arg_type = self.infer_expr(&args[0].node.value);
                            let resolved = self.resolve_type(arg_type);
                            // Unwrap &/! wrappers (`String(!s)` moves a String in).
                            let inner = match self.types.get(resolved) {
                                ResolvedType::Ref(t) | ResolvedType::Owned(t) => self.resolve_type(*t),
                                _ => resolved,
                            };
                            let ok = match self.types.get(inner) {
                                ResolvedType::Primitive(p) => {
                                    is_integer_type(p)
                                        || matches!(p, PrimitiveType::StringType | PrimitiveType::CStr)
                                }
                                // Error: already diagnosed — don't cascade.
                                // Never: diverging arg, unreachable anyway.
                                // Var: unbound inference variable — can't
                                // classify; never false-positive on it.
                                ResolvedType::Error | ResolvedType::Never | ResolvedType::Var(_) => true,
                                _ => false,
                            };
                            if !ok {
                                self.error(
                                    SemanticErrorKind::TypeMismatch {
                                        expected: "String(n) with an integer capacity or String(s) with String content — to convert a value to text, use an f-string: f\"{x}\"".to_string(),
                                        found: self.describe_resolved_type(inner),
                                    },
                                    args[0].node.value.span,
                                );
                            }
                        }
                        // Allocator-ctor / Channel capacity-axis policy
                        // (round-33, same Core #8 rationale as the String
                        // shapes above): these used to be arity-gated,
                        // named-arg-BLIND intercepts in GIR lowering, so
                        // every off-shape call — `Arena()`,
                        // `Arena(cap=64, alloc=a)`, `PoolAllocator(cap=8)`,
                        // `Arena("x")` — fell through to an undefined
                        // symbol / incompatible-arg cc/llc/ld error, or
                        // worse, wrong-accepted (`Arena(alloc=a)` passed
                        // the allocator STRUCT as the byte capacity).
                        // Reject off-shapes at check time; GIR lowering
                        // (exprs/calls.rs `alloc_ctor`) then only sees the
                        // accepted ones.
                        let positional_count = args.iter().filter(|a| a.node.name.is_none()).count();
                        // DISTINCT capacity sources: duplicate cap= is one
                        // source, already rejected as DuplicateNamedArg
                        // above — don't cascade a second multi-source
                        // error on top of it.
                        let cap_count = usize::from(args.iter().any(|a| {
                            a.node.name.as_ref().map_or(false, |n| n.node == "cap")
                        }));
                        match cname.as_str() {
                            // Single int capacity (positional or cap=).
                            // Omitting it is fine where the runtime
                            // defines the default — Arena 4096; TLSF
                            // 65536, documented §15.3 (the design doc's
                            // flagship spelling is `with Arena() as
                            // pool:`); Channel 0 = rendezvous — but NOT
                            // for FixedBufferAllocator: a 0-byte buffer
                            // returns NULL on every alloc (garbage-at-
                            // first-use), and §15.3 documents its
                            // capacity as required.
                            "Arena" | "TlsfAllocator" | "FixedBufferAllocator" | "Channel" => {
                                if cname == "FixedBufferAllocator" && positional_count + cap_count == 0 {
                                    self.error(
                                        SemanticErrorKind::WrongArgCount { expected: 1, found: 0 },
                                        expr.span,
                                    );
                                } else if positional_count + cap_count > 1 {
                                    self.error(
                                        SemanticErrorKind::TypeMismatch {
                                            expected: format!("a single capacity argument — {cname}(n) or {cname}(cap=n), optionally with alloc="),
                                            found: format!("{} capacity arguments", positional_count + cap_count),
                                        },
                                        expr.span,
                                    );
                                } else if positional_count == 1 {
                                    // Same integer predicate as the cap= arm.
                                    let pos = args.iter().find(|a| a.node.name.is_none()).unwrap();
                                    let pos_type = self.infer_expr(&pos.node.value);
                                    let pos_resolved = self.resolve_type(pos_type);
                                    let (pos_inner, pos_ok) = self.int_capacity_check(pos_resolved);
                                    if !pos_ok {
                                        self.error(
                                            SemanticErrorKind::TypeMismatch {
                                                expected: format!("an integer capacity (any int width), e.g. {cname}(64)"),
                                                found: self.describe_resolved_type(pos_inner),
                                            },
                                            pos.node.value.span,
                                        );
                                    }
                                }
                            }
                            // Fixed 2-positional signatures; no capacity axis.
                            "PoolAllocator" | "FallbackAllocator" => {
                                if positional_count != 2 {
                                    self.error(
                                        SemanticErrorKind::WrongArgCount { expected: 2, found: positional_count },
                                        expr.span,
                                    );
                                } else if cname == "PoolAllocator" {
                                    // PoolAllocator(block_size, initial_count):
                                    // both ints — same predicate as cap=.
                                    for pos in args.iter().filter(|a| a.node.name.is_none()) {
                                        let pos_type = self.infer_expr(&pos.node.value);
                                        let pos_resolved = self.resolve_type(pos_type);
                                        let (pos_inner, pos_ok) = self.int_capacity_check(pos_resolved);
                                        if !pos_ok {
                                            self.error(
                                                SemanticErrorKind::TypeMismatch {
                                                    expected: "an integer — PoolAllocator(block_size, initial_count)".to_string(),
                                                    found: self.describe_resolved_type(pos_inner),
                                                },
                                                pos.node.value.span,
                                            );
                                        }
                                    }
                                } else {
                                    // FallbackAllocator(primary, secondary):
                                    // both allocators — same predicate as alloc=.
                                    for pos in args.iter().filter(|a| a.node.name.is_none()) {
                                        let pos_type = self.infer_expr(&pos.node.value);
                                        let pos_resolved = self.resolve_type(pos_type);
                                        if !self.is_allocator_arg_type(pos_resolved) {
                                            self.error(
                                                SemanticErrorKind::TypeMismatch {
                                                    expected: "allocator type (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, or FallbackAllocator)".to_string(),
                                                    found: self.describe_resolved_type(pos_resolved),
                                                },
                                                pos.node.value.span,
                                            );
                                        }
                                    }
                                }
                            }
                            // Wraps the active (or alloc=-given) allocator;
                            // takes nothing else.
                            "TrackingAllocator" => {
                                if positional_count != 0 {
                                    self.error(
                                        SemanticErrorKind::WrongArgCount { expected: 0, found: positional_count },
                                        expr.span,
                                    );
                                }
                            }
                            _ => {}
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
                            let fi_param_names = fi.param_names.clone();
                            let fi_param_defaults = fi.param_defaults.clone();
                            self.check_named_args_and_defaults(
                                args, &params, &fi_param_names, &fi_param_defaults, expr.span,
                            );
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
                                // Snag #35: a throws-call result (Result[T, E])
                                // passed where the param is `T` is fine when
                                // the enclosing function can propagate, AND a
                                // throws-call result passed where the param is
                                // `Result[T, E]` is the capture form. Same
                                // policy as `Stmt::VarDecl` at line ~2520.
                                // Without this, the new throws-returns-Result
                                // typing rule (Call inference above) would
                                // false-positive at every auto-prop call-arg
                                // site (`f(throws_call())` from inside a
                                // throws function).
                                if !self.auto_prop_skips_unify(param_type, arg_type, arg.node.value.span)
                                    && !self.is_result_capture_compatible(param_type, arg_type)
                                {
                                    // Gorget-js snag #2: when unify produces a
                                    // TypeMismatch AND both param and arg types
                                    // are fully concrete (no Vars, no error_id,
                                    // no Never), mark the resulting mismatch as
                                    // "hard" so it survives the imported-module
                                    // truncate in `check_items_recursive_tc`.
                                    // This is the boundary where a user passes
                                    // a wrong-typed value into an imported
                                    // function; the truncate exists to swallow
                                    // library-foreign-scope false positives
                                    // (auto-prop holes, unresolved generic Vs),
                                    // but a concrete-vs-concrete call-arg
                                    // mismatch survived through `unify`'s full
                                    // coercion ladder (cstr↔String, Ref↔T,
                                    // Owned↔T) is never a foreign-scope
                                    // artifact.
                                    let pre_err_count = self.errors.len();
                                    self.unify(param_type, arg_type, arg.span);
                                    if self.errors.len() > pre_err_count
                                        && self.is_fully_concrete(param_type)
                                        && self.is_fully_concrete(arg_type)
                                    {
                                        // Mirror the just-pushed error into
                                        // hard_errors so the truncate path
                                        // re-appends it. The same error stays
                                        // in `errors` for the main display
                                        // pipeline.
                                        if let Some(err) = self.errors.last().cloned() {
                                            self.hard_errors.push(err);
                                        }
                                    }
                                }
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
                        } else if let Some(err_ty) = func_info.and_then(|fi| fi.throws_type_id) {
                            // Snag #35: throws functions return `Result[T, E]`
                            // at the call boundary. Snag #35-followup
                            // (Snag #B/C, gorget-js): the per-position carve-out
                            // for auto-propagation was incomplete — a throwing
                            // call in an un-carved consumer position (binop
                            // operand, match-arm tail, if-expr branch,
                            // list-literal element, method-call arg,
                            // struct-ctor field arg) failed to type-check even
                            // though IR-lowering auto-props it correctly.
                            //
                            // Fix: CENTRALIZE the peel here at the producer,
                            // mirroring IR-lowering's centralized
                            // `maybe_auto_propagate` hook (`lower_expr` entry,
                            // commit 90d09414). In a *propagating* context
                            // (`throws` or returns `Result`) the call peels to
                            // its `Ok(T)` type BY DEFAULT, so every consumer
                            // position just sees `T` and unifies normally —
                            // no position has to opt in. The raw `Result[T, E]`
                            // is kept (the peel is SUPPRESSED) only where a
                            // whole-Result value is genuinely wanted, exactly
                            // mirroring the lowering suppress set:
                            //   - destination type is `Result[..]`  (the
                            //     `Result[T,E] r = f()` capture form; mirrors
                            //     lowering's `expected_type is Result`);
                            //   - match scrutinee with Ok/Error/Some/None arms,
                            //     `catch` inner, `rethrow` inner — flagged via
                            //     the one-shot `suppress_auto_prop` (mirror of
                            //     lowering's `func_state.suppress_auto_prop`).
                            //     (No `==`/`!=` suppress: the peel is gated to
                            //     *throws*-fn calls, so an explicit `Result`-
                            //     returning fn — the only operand whose
                            //     raw-vs-peeled compare differs — is never
                            //     peeled, leaving `make(1) == make(1)` unchanged.)
                            // In a NON-propagating context the call cannot
                            // propagate, so the error is unhandled — D23 emits
                            // `E_UnhandledThrows` at the producer (below) rather
                            // than letting the raw `Result[T, E]` leak into a
                            // downstream `unify`. Shared with the method path via
                            // `resolve_throws_call_type` (fix the class, not the
                            // instance).
                            self.resolve_throws_call_type(
                                return_type,
                                err_ty,
                                suppress_auto_prop,
                                fallible_call_marked,
                                /*mark_is_operator_inherent=*/ false,
                                expr.span,
                            )
                        } else if self.type_is_result(return_type) {
                            // D29 kind-2: a non-throws free fn whose declared
                            // return is `Result[T,E]` is a fallible call under
                            // the one-mark rule. Unmarked = a legal Result value
                            // flow (bare-discard caught at statement position);
                            // marked = peel + activate.
                            self.resolve_kind2_call_type(
                                return_type,
                                suppress_auto_prop,
                                fallible_call_marked,
                                expr.span,
                            )
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
                                    DefKind::Variant => {
                                        let hint = self.decl_type_hint;
                                        return self.infer_variant_constructor(
                                            def_id, args, hint, expr.span,
                                        );
                                    }
                                    _ => {}
                                }
                            }
                        }
                        // Don't cascade — just infer arg types
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        // Known void-returning builtins: return void instead of error.
                        // `panic` is special — it never returns; type it as Never
                        // so it's compatible with any expected type, particularly
                        // when used as a match-arm or `??` RHS. Pre-existing TODO
                        // (gorget-js round 6 / item 5 from the 2026-05-13 critique)
                        // recommended this as option (b); option (a) (declare
                        // `panic` in stdlib as `extern noreturn`) is the layering-
                        // discipline answer but requires removing the hardcoded
                        // `gorget_panic` lowering at `stmts/mod.rs`.
                        if let Expr::Identifier(cname) = &callee.node {
                            if cname.as_str() == "panic" {
                                return self.types.never_id;
                            }
                            if matches!(cname.as_str(), "print" | "assert") {
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
                            DefKind::Struct | DefKind::Newtype => {
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
                            DefKind::Variant => {
                                let hint = self.decl_type_hint;
                                self.infer_variant_constructor(
                                    def_id, args, hint, expr.span,
                                )
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
                                    DefKind::Struct | DefKind::Newtype => {
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
                                        return self.types.defined_id(def_id);
                                    }
                                    DefKind::Variant => {
                                        let hint = self.decl_type_hint;
                                        return self.infer_variant_constructor(
                                            def_id, args, hint, expr.span,
                                        );
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
                            } else if let Some(variant_def_id) =
                                self.find_ambiguous_variant_by_name(cname)
                            {
                                // The resolver suppressed `undefined name` for this
                                // bare identifier because at least one enum has a
                                // variant by this name (the loader's pre-merge
                                // qualifier dropped it as ambiguous). Recover the
                                // intended variant via `decl_type_hint` — the call
                                // arg's expected enum type — exactly as the pattern
                                // path uses the scrutinee type. Mirrors the
                                // architecture documented at
                                // `build_variant_map_from_all` in `src/loader.rs`.
                                let hint = self.decl_type_hint;
                                return self.infer_variant_constructor(
                                    variant_def_id, args, hint, expr.span,
                                );
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
                // Invariant #8 (complete the class): a positional argument may
                // not follow a named argument. The free-fn `ECall` path already
                // rejects this (`check_named_args_and_defaults`); method calls
                // carry the same `Spanned<CallArg>` arg list and must reject it
                // too. The check is purely structural over the explicit arg list
                // (the receiver is `self.receiver`, NOT part of `args`), so it is
                // dispatch-independent and runs once here before any dispatch
                // fork — no off-by-one against the receiver.
                {
                    let mut seen_named = false;
                    for arg in args.iter() {
                        if arg.node.name.is_some() {
                            seen_named = true;
                        } else if seen_named {
                            self.error(SemanticErrorKind::PositionalAfterNamed, arg.span);
                        }
                    }
                }

                // Static method calls on type names: int.parse(), float.default()
                if let Expr::Identifier(name) = &receiver.node {
                    if let Some(ret) = self.resolve_static_method_type(name, &method.node, args, expr.span) {
                        return ret;
                    }
                }

                // Qualified enum-variant constructor: `Color.Red()`,
                // `Maybe.Just(42)`. Receiver is an identifier resolving to
                // an enum DefId; method name matches a variant of that
                // enum. Route through the unified variant-inference helper
                // so the call types as `Generic(parent_enum, [...args])`
                // rather than `error_id` (the historical fall-through).
                // Mirrors the IR-lowering check at `methods.rs:222-237`.
                if let Expr::Identifier(rname) = &receiver.node {
                    if let Some(rec_def_id) = self.resolve_name(receiver.span.start, rname) {
                        if self.scopes.get_def(rec_def_id).kind == DefKind::Enum {
                            let variant_def_id = self
                                .enum_variants
                                .get(&rec_def_id)
                                .and_then(|info| {
                                    info.variants
                                        .iter()
                                        .find(|(n, _)| n == &method.node)
                                        .map(|(_, vid)| *vid)
                                });
                            if let Some(vdid) = variant_def_id {
                                let hint = self.decl_type_hint;
                                let ret = self.infer_variant_constructor(
                                    vdid, args, hint, expr.span,
                                );
                                self.expr_types.insert(expr.span, ret);
                                return ret;
                            }
                        }
                    }
                }

                // A static-method receiver spelled as a bare type name
                // (`Point.origin()`, `int.parse(...)`) is a legitimate type name
                // in expression position — suppress the value-position reject for
                // this immediate receiver infer (one-shot, consumed at infer entry).
                self.type_name_position_ok = true;
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
                    self.method_resolutions.insert(method.span.start, MethodResolution::direct(*def_id));
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
                    // Argument count / default-fill check. Equip methods may
                    // carry trailing default params just like free functions —
                    // route those (and any named-arg call) through the shared
                    // `check_named_args_and_defaults` so `p.add(5)` for
                    // `int add(self, int a, int b = 2)` type-checks instead of
                    // being rejected as WrongArgCount.
                    //
                    // ALIGNMENT QUIRK: `sig.params` (from resolve_method)
                    // EXCLUDES `self`, but the method's `FunctionInfo`
                    // param_names/param_defaults INCLUDE `self` at index 0
                    // (the parser injects a synthetic self param). Strip the
                    // self slot so both views align with `sig.params`. A static
                    // equip method (no `self`) is NOT stripped — its
                    // `FunctionInfo` already excludes self, matching `sig.params`.
                    let method_info_strip = self.function_info.get(&stored_def_id).map(|fi| {
                        let skip = if fi.param_names.first().map(|n| n == "self").unwrap_or(false) {
                            1
                        } else {
                            0
                        };
                        let pn: Vec<String> = fi.param_names.iter().skip(skip).cloned().collect();
                        let pd: Vec<Option<Spanned<Expr>>> =
                            fi.param_defaults.iter().skip(skip).cloned().collect();
                        (pn, pd)
                    });
                    let has_named = args.iter().any(|a| a.node.name.is_some());
                    let has_defaults = method_info_strip
                        .as_ref()
                        .map_or(false, |(_, pd)| pd.iter().any(|d| d.is_some()));
                    if (has_named || has_defaults) && method_info_strip.is_some() {
                        let (pn, pd) = method_info_strip.unwrap();
                        self.check_named_args_and_defaults(
                            args, &sig.params, &pn, &pd, expr.span,
                        );
                    } else {
                        // Simple positional check (original behavior).
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
                    }
                    // Record the method call's own type so downstream consumers
                    // (generic method-instance discovery, borrow checker) can
                    // resolve chained call receivers back to concrete types.
                    // D23: route a `throws` method through the producer helper
                    // (was: bare `sig.return_type` → the silent miscompile).
                    let ret = self.resolve_throws_method_ret(
                        stored_def_id,
                        &method.node,
                        resolved_receiver,
                        sig.return_type,
                        suppress_auto_prop,
                        fallible_call_marked,
                        expr.span,
                    );
                    self.expr_types.insert(expr.span, ret);
                    ret
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
                        self.method_resolutions.insert(method.span.start, MethodResolution::direct(def_id));
                        if let Some(substituted) = self.substitute_default_method_sig(
                            def_id, &method.node, resolved_receiver,
                        ) {
                            sig = substituted;
                        }
                        for (arg, &param_type) in args.iter().zip(sig.params.iter()) {
                            let arg_type = self.infer_expr(&arg.node.value);
                            self.unify(param_type, arg_type, arg.span);
                        }
                        // D23: route through the producer helper. `def_id` here
                        // is the TRAIT def_id (resolve_method_by_name returns
                        // `trait_info.def_id` for a default) — the helper reads
                        // the default's throws from the trait's
                        // `DefaultMethodSig` (keyed by method name).
                        let ret = self.resolve_throws_method_ret(
                            def_id,
                            &method.node,
                            resolved_receiver,
                            sig.return_type,
                            suppress_auto_prop,
                            fallible_call_marked,
                            expr.span,
                        );
                        self.expr_types.insert(expr.span, ret);
                        return ret;
                    }

                    // Round XXVI Track A — reject the 5 wrong-receiver one-sided
                    // combinators (Result.{flat_map, filter, flatten} +
                    // Option.{map_err, unwrap_error}) BEFORE dispatch. Class-fix
                    // (Core #4) at the chokepoint; mirrors XXV Track B's ggdef
                    // receiver-gate. Class-guard is the per-arm marker (5 total)
                    // in the reject fn — see its doc-comment.
                    self.reject_wrong_receiver_combinator(resolved_receiver, &method.node, expr.span);

                    // Check for closure-returning Option/Result methods (map, and_then, or_else)
                    if let Some(ret_type) = self.infer_closure_method_type(resolved_receiver, &method.node, args) {
                        // D29 kind-2: a builtin combinator whose declared return
                        // is `Result[T,E]` (`r.and_then(f)`, `r.map(f)`, …) is a
                        // kind-2 call like any other — unmarked it is a legal
                        // Result VALUE flow (consumed by the chain), marked it
                        // peels + activates. NOT a carve-out: the uniform rule,
                        // extended to the builtin-method typing path (otherwise
                        // a marked combinator's `!` is never consumed → the R3
                        // lying-mark check would false-positive on a legal shape).
                        let ret_type = if self.type_is_result(ret_type) {
                            self.resolve_kind2_call_type(
                                ret_type,
                                suppress_auto_prop,
                                fallible_call_marked,
                                expr.span,
                            )
                        } else {
                            ret_type
                        };
                        self.expr_types.insert(expr.span, ret_type);
                        ret_type
                    } else {
                        // Method not found — check built-in type methods.
                        // For builtin collection mutators (push/insert/put/set)
                        // the element/value type is NOT a sig param — it comes
                        // from the receiver's generic args. Thread it in as a
                        // decl_type_hint so dot-shorthand enum ctors
                        // (`segments.push(.ArrayLen)`) infer their enum context.
                        let arg_hints =
                            self.builtin_mutator_arg_hints(resolved_receiver, &method.node);
                        let prev_hint = self.decl_type_hint;
                        for (i, arg) in args.iter().enumerate() {
                            self.decl_type_hint =
                                arg_hints.get(i).copied().flatten();
                            self.infer_expr(&arg.node.value);
                        }
                        self.decl_type_hint = prev_hint;
                        if let Some(ret_type) = self.builtin_method_type(resolved_receiver, &method.node) {
                            self.expr_types.insert(expr.span, ret_type);
                            ret_type
                        } else if matches!(method.node.as_str(), "unwrap" | "expect" | "unwrap_or")
                            && !self.is_option_or_result_receiver(resolved_receiver)
                            && !matches!(self.types.get(resolved_receiver), ResolvedType::Var(_))
                            && resolved_receiver != self.types.error_id
                        {
                            // Phase 1 (Brief A): `unwrap`/`expect`/`unwrap_or` on a
                            // non-Option/Result receiver. We only reach here when the
                            // method resolved through NO avenue above (trait registry,
                            // trait default, closure-Option/Result, builtin protocol),
                            // so a user-defined `unwrap` via equip is already handled.
                            // `builtin_method_type` returns the inner type for genuine
                            // Option/Result receivers, so those never land here either.
                            // Without this, the call defaulted to `error_id` and the IR
                            // lowering silently turned it into a no-op (returning the
                            // receiver unchanged). The `Var`/`error_id` guards avoid
                            // false positives on receivers whose type inference is still
                            // incomplete (we'd produce a spurious error on valid code).
                            self.error(
                                SemanticErrorKind::UnwrapOnNonOptional {
                                    method: method.node.clone(),
                                    type_: self.describe_resolved_type(resolved_receiver),
                                },
                                expr.span,
                            );
                            self.types.error_id
                        } else {
                            // Name-based fallback for cross-module equip methods
                            // where TypeId doesn't match.
                            if let Some(ref name) = base_name {
                                if let Some((def_id, sig)) = self.traits.resolve_method_by_name(name, &method.node) {
                                    // D23: for a concrete cross-module equip
                                    // method `def_id` is the equip-method def_id
                                    // (a `function_info` key with throws) — route
                                    // through the producer helper. (For a trait-
                                    // default it is the trait def_id, and the
                                    // helper reads throws from the trait's
                                    // `DefaultMethodSig`.)
                                    let (def_id, ret_ty) = (*def_id, sig.return_type);
                                    let ret = self.resolve_throws_method_ret(
                                        def_id,
                                        &method.node,
                                        resolved_receiver,
                                        ret_ty,
                                        suppress_auto_prop,
                                        fallible_call_marked,
                                        expr.span,
                                    );
                                    self.expr_types.insert(expr.span, ret);
                                    ret
                                } else {
                                    // Emit NoMethodFound when EITHER the type is a builtin
                                    // protocol type and the method is not in its protocol,
                                    // OR the type has inherent-only equip blocks (no via-field
                                    // delegation). Round XXIX Track B widened `has_inherent_only_impls`
                                    // to treat every `BuiltinTypeProtocol` base name as
                                    // authoritative — `BuiltinTypeProtocol` at
                                    // `src/ir/lowering/builtins.rs` is the single source of
                                    // truth, and the `builtin_oracle_covers_every_protocol_method`
                                    // ratchet at `tests/lints.rs` (Core #6) enforces that
                                    // every protocol method is covered by the oracle
                                    // (`builtin_method_type` or `infer_closure_method_type`)
                                    // OR by an equip block registered in `impls_by_name`.
                                    // Via-forwarded methods still bypass the reject correctly
                                    // through the `via_field` check.
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
                                    // TRACK E2 SCOUT PROTOTYPE — D36 (2026-07-27, decisions.md):
                                    // when the wrapper is `GuardAccept` or `DerefTarget` and the
                                    // method exists on the INNER type, resolve on the inner and
                                    // record the auto-deref decision for the lowering. Per D36:
                                    //   - Read face (bare `self`): accept for Guard + ReadGuard +
                                    //     WriteGuard + Box.
                                    //   - Write face (`&self`): accept for Guard + WriteGuard + Box;
                                    //     REJECT ReadGuard (mutex read-only invariant).
                                    //   - Consuming face (`!self`): accept for Box only.
                                    //   - `NonDerefContainer` (Shared/Weak/Mutex/RWLock) → REJECT.
                                    // The extracted inner TypeId + method resolution + face check
                                    // reads a typed `DerefWrapperKind` at the SSoT (no name-match).
                                    let container_did = match self.types.get(resolved_receiver) {
                                        ResolvedType::Defined(did) | ResolvedType::Generic(did, _) => *did,
                                        other => {
                                            debug_assert!(false, "MethodCall auto-deref container-did read: base_name resolved but ResolvedType is {other:?}, expected Defined/Generic");
                                            return self.types.error_id;
                                        }
                                    };
                                    let container_kind = self.scopes.get_def(container_did).deref_wrapper_kind;
                                    // Try auto-deref for GuardAccept / DerefTarget.
                                    if !is_auto_derivable {
                                        if let Some(wrapper_kind) = container_kind {
                                            if matches!(wrapper_kind,
                                                DerefWrapperKind::GuardAccept | DerefWrapperKind::DerefTarget)
                                            {
                                                // Extract the inner TypeId from the wrapper's generic args.
                                                let inner_tid = match self.types.get(resolved_receiver) {
                                                    ResolvedType::Generic(_, targs) => targs.first().copied(),
                                                    _ => None,
                                                };
                                                if let Some(inner_tid) = inner_tid {
                                                    let inner_resolved = self.resolve_type(inner_tid);
                                                    if let Some((def_id, sig)) =
                                                        self.traits.resolve_method(inner_resolved, &method.node)
                                                    {
                                                        let stored_def_id = *def_id;
                                                        let mut sig = sig.clone();
                                                        // D36 per-face split — check the SELF FACE
                                                        // of the resolved method against the wrapper.
                                                        // Read face: any wrapper (Guard/ReadGuard/
                                                        // WriteGuard/Box) accepts.
                                                        // Write face: ReadGuard REJECTS.
                                                        // Consuming face: only Box accepts.
                                                        let self_ownership = self
                                                            .function_info
                                                            .get(&stored_def_id)
                                                            .and_then(|fi| fi.param_ownerships.first().copied());
                                                        let container_name = self.scopes.get_def(container_did).name.clone();
                                                        // Parser mapping (src/parser/mod.rs:1878-1895):
                                                        //   bare `self`  => Ownership::Borrow        — READ face
                                                        //   `&self`      => Ownership::MutableBorrow — WRITE face
                                                        //   `!self`      => Ownership::Move         — CONSUMING face
                                                        // D36 face split:
                                                        //   Read      → any wrapper accepts.
                                                        //   Write     → ReadGuard rejects.
                                                        //   Consuming → only Box accepts.
                                                        let face_reject: Option<SemanticErrorKind> = match self_ownership {
                                                            Some(crate::parser::ast::Ownership::MutableBorrow) => {
                                                                if container_name == "ReadGuard" {
                                                                    Some(SemanticErrorKind::AutoDerefWriteThroughReadGuard {
                                                                        method: method.node.clone(),
                                                                        wrapper: container_name.clone(),
                                                                    })
                                                                } else { None }
                                                            }
                                                            Some(crate::parser::ast::Ownership::Move) => {
                                                                if matches!(wrapper_kind, DerefWrapperKind::GuardAccept) {
                                                                    Some(SemanticErrorKind::AutoDerefConsumingThroughGuard {
                                                                        method: method.node.clone(),
                                                                        wrapper: container_name.clone(),
                                                                    })
                                                                } else { None }
                                                            }
                                                            _ => None,
                                                        };
                                                        if let Some(err) = face_reject {
                                                            self.error(err, expr.span);
                                                            return self.types.error_id;
                                                        }
                                                        // Substitute default-body sigs (if any).
                                                        if self.traits.traits.contains_key(&stored_def_id) {
                                                            if let Some(substituted) = self.substitute_default_method_sig(
                                                                stored_def_id, &method.node, inner_resolved,
                                                            ) {
                                                                sig = substituted;
                                                            }
                                                        }
                                                        // D36: record the resolution with the
                                                        // auto-deref marker in the SSoT record.
                                                        self.method_resolutions.insert(
                                                            method.span.start,
                                                            MethodResolution {
                                                                def_id: Some(stored_def_id),
                                                                auto_deref: Some(wrapper_kind),
                                                            },
                                                        );
                                                        // Simple positional arg unification.
                                                        // (Named-arg / default-fill support is a follow-up.)
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
                                                        let ret = self.resolve_throws_method_ret(
                                                            stored_def_id,
                                                            &method.node,
                                                            inner_resolved,
                                                            sig.return_type,
                                                            suppress_auto_prop,
                                                            fallible_call_marked,
                                                            expr.span,
                                                        );
                                                        self.expr_types.insert(expr.span, ret);
                                                        return ret;
                                                    }
                                                    // No user-equipped method — probe builtin
                                                    // methods on the inner (Vector.push / Vector.len /
                                                    // Dict.get / String.len / …). Face split for
                                                    // builtins uses `is_mutating_builtin_method`
                                                    // as the write-face proxy (no FunctionInfo).
                                                    if let Some(ret_type) = self
                                                        .builtin_method_type(inner_resolved, &method.node)
                                                    {
                                                        let is_write =
                                                            crate::ir::lowering::builtins::is_mutating_builtin_method(
                                                                method.node.as_str(),
                                                            );
                                                        let container_name = self
                                                            .scopes
                                                            .get_def(container_did)
                                                            .name
                                                            .clone();
                                                        if is_write && container_name == "ReadGuard" {
                                                            self.error(
                                                                SemanticErrorKind::AutoDerefWriteThroughReadGuard {
                                                                    method: method.node.clone(),
                                                                    wrapper: container_name,
                                                                },
                                                                expr.span,
                                                            );
                                                            return self.types.error_id;
                                                        }
                                                        // Consuming face for builtins is not a
                                                        // shape the builtin registry expresses;
                                                        // the write/read split above is the whole
                                                        // discriminator for builtins under D36.
                                                        // Record the auto-deref marker with
                                                        // `def_id: None` — builtins have no
                                                        // user FunctionInfo; the borrow checker
                                                        // skips these entries.
                                                        self.method_resolutions.insert(
                                                            method.span.start,
                                                            MethodResolution {
                                                                def_id: None,
                                                                auto_deref: Some(wrapper_kind),
                                                            },
                                                        );
                                                        // Infer args using the same hint mechanism
                                                        // as the outer builtin path (so container
                                                        // literals infer their enum context).
                                                        let arg_hints = self
                                                            .builtin_mutator_arg_hints(
                                                                inner_resolved,
                                                                &method.node,
                                                            );
                                                        let prev_hint = self.decl_type_hint;
                                                        for (i, arg) in args.iter().enumerate() {
                                                            self.decl_type_hint =
                                                                arg_hints.get(i).copied().flatten();
                                                            self.infer_expr(&arg.node.value);
                                                        }
                                                        self.decl_type_hint = prev_hint;
                                                        self.expr_types.insert(expr.span, ret_type);
                                                        return ret_type;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // TRACK G (2026-07-28): `Box[Trait].method()` — a boxed
                                    // TRAIT OBJECT dispatched via vtable at runtime. The E2 auto-
                                    // deref block above tried `traits.resolve_method(inner)` and
                                    // `builtin_method_type(inner)`; both miss because the trait's
                                    // methods live in `traits.traits[trait_def_id].methods`, not on
                                    // any impl indexed by the trait's own TypeId. Pre-E1 this cell
                                    // fell through the terminal reject (returning `error_id`) and the
                                    // IR-side `Box[Trait]` vtable dispatch (`ir/lowering/exprs/
                                    // methods.rs:1627-1682`, keyed by the `Box__Shape` type name +
                                    // `Shape_VTable` lookup) produced a correct call. E1 promoted the
                                    // fallthrough to a reject and broke `examples/shapes` at check.
                                    // Fix: recognise `Box[Trait]` here — look up the trait's own
                                    // declared method sig by NAME across `traits.traits.values()`
                                    // (name-lookup because the inner def can be a `DefKind::Import`
                                    // placeholder that doesn't key `traits.traits` directly), unify
                                    // args against the sig, and return the sig's return type.
                                    //
                                    // TRACK N1 (2026-07-28): widened to also cover
                                    // `Guard[Box[Trait]].method()` (and its ReadGuard/WriteGuard
                                    // siblings) under D36 read/write face uniformity. When the
                                    // container is a guard wrapper (`GuardAccept`) OR Box
                                    // (`DerefTarget`), and the inner shape is either a
                                    // `TraitObject`, a bare-trait `Defined/Generic`, or a
                                    // cross-module `Generic(Box, [Import(trait)])`, look up the
                                    // trait's method sig by name and dispatch.
                                    //
                                    // For `GuardAccept` we set `auto_deref = Some(GuardAccept)`
                                    // so IR-lowering projects the receiver through
                                    // `emit_guard_get_ptr` (`ir/lowering/exprs/methods.rs:546-570`);
                                    // the resulting `MutPtr(Box__Trait)` then falls into the
                                    // existing Box[Trait] vtable dispatch at
                                    // `ir/lowering/exprs/methods.rs:1694-1749`.
                                    //
                                    // For `DerefTarget` we KEEP `auto_deref = None`:
                                    // Track G's rationale — the IR vtable-dispatch path keys on
                                    // the `Box__Trait` type name, NOT on the marker, and
                                    // routing through `Box__T__get_ptr` would look up a
                                    // nonexistent `Trait__method`.
                                    //
                                    // TODO (D36 face rule): the Track-G/N1 dispatch block does
                                    // NOT enforce D36's face split (ReadGuard rejects `&self`
                                    // methods; GuardAccept rejects `!self` methods — mirrored
                                    // by the E2 block at `typecheck.rs:2822-2844` for the
                                    // concrete-inner case). Fixture 2b is read-face only so it
                                    // doesn't trip. See TODO.md "Semantics / reference-grade
                                    // rejection" bucket.
                                    if matches!(
                                        container_kind,
                                        Some(DerefWrapperKind::DerefTarget)
                                            | Some(DerefWrapperKind::GuardAccept)
                                    ) {
                                        if let ResolvedType::Generic(_, targs) =
                                            self.types.get(resolved_receiver).clone()
                                        {
                                            if let Some(inner_tid) = targs.first().copied() {
                                                let inner_resolved = self.resolve_type(inner_tid);
                                                // Extract the trait def_id + name from the inner
                                                // type. Three same-file shapes:
                                                //   * `Defined(d)` / `Generic(d, _)` — bare-trait
                                                //     inner (e.g. `Guard[Speaker]`).
                                                //   * `TraitObject(d)` — Box[Trait] inner
                                                //     normalised by `types.rs:424` (same-file
                                                //     `Guard[Box[Speaker]]` collapses to
                                                //     `Generic(Guard, [TraitObject(Speaker)])`).
                                                let inner_ty_owned =
                                                    self.types.get(inner_resolved).clone();
                                                let mut inner_name_opt = match &inner_ty_owned {
                                                    ResolvedType::Defined(d)
                                                    | ResolvedType::Generic(d, _)
                                                    | ResolvedType::TraitObject(d) => {
                                                        Some(self.scopes.get_def(*d).name.clone())
                                                    }
                                                    _ => None,
                                                };
                                                // Cross-module `Guard[Box[Trait]]`: the inner is
                                                // `Generic(Box, [Import(trait)])` — the
                                                // `types.rs:428` `DefKind::Trait` carve-out
                                                // doesn't fire on `Import` placeholders, so we
                                                // never collapse to `TraitObject`. Peel one Box
                                                // layer when the outer name misses the trait
                                                // registry AND the outer def carries the typed
                                                // `DerefWrapperKind::DerefTarget` marker (the
                                                // SSoT is-Box discriminator — never a name-match,
                                                // per Layering rule 2).
                                                let outer_hit_probe = inner_name_opt
                                                    .as_ref()
                                                    .and_then(|n| {
                                                        self.traits
                                                            .traits
                                                            .values()
                                                            .find(|t| &t.name == n)
                                                    });
                                                if outer_hit_probe.is_none() {
                                                    if let ResolvedType::Generic(
                                                        outer_did,
                                                        box_targs,
                                                    ) = &inner_ty_owned
                                                    {
                                                        let outer_is_box = self
                                                            .scopes
                                                            .get_def(*outer_did)
                                                            .deref_wrapper_kind
                                                            == Some(DerefWrapperKind::DerefTarget);
                                                        if outer_is_box {
                                                            if let Some(&box_inner_tid) =
                                                                box_targs.first()
                                                            {
                                                                let box_inner_resolved =
                                                                    self.resolve_type(
                                                                        box_inner_tid,
                                                                    );
                                                                inner_name_opt = match self
                                                                    .types
                                                                    .get(box_inner_resolved)
                                                                {
                                                                    ResolvedType::Defined(d)
                                                                    | ResolvedType::Generic(
                                                                        d,
                                                                        _,
                                                                    )
                                                                    | ResolvedType::TraitObject(
                                                                        d,
                                                                    ) => Some(
                                                                        self.scopes
                                                                            .get_def(*d)
                                                                            .name
                                                                            .clone(),
                                                                    ),
                                                                    _ => None,
                                                                };
                                                            }
                                                        }
                                                    }
                                                }
                                                if let Some(inner_name) = inner_name_opt {
                                                    // Find the trait by name (import-follow-safe).
                                                    let trait_hit = self.traits.traits.values()
                                                        .find(|t| t.name == inner_name)
                                                        .and_then(|t| t.methods.get(&method.node)
                                                            .map(|sig| (t.def_id, sig.clone())));
                                                    if let Some((trait_def_id, sig)) = trait_hit {
                                                        // For GuardAccept: mark auto_deref so IR
                                                        // projects the receiver through
                                                        // `emit_guard_get_ptr`; DerefTarget stays
                                                        // `None` (see comment above).
                                                        let auto_deref = if matches!(
                                                            container_kind,
                                                            Some(DerefWrapperKind::GuardAccept)
                                                        ) {
                                                            Some(DerefWrapperKind::GuardAccept)
                                                        } else {
                                                            None
                                                        };
                                                        self.method_resolutions.insert(
                                                            method.span.start,
                                                            MethodResolution {
                                                                def_id: Some(trait_def_id),
                                                                auto_deref,
                                                            },
                                                        );
                                                        if args.len() != sig.params.len() {
                                                            self.error(
                                                                SemanticErrorKind::WrongArgCount {
                                                                    expected: sig.params.len(),
                                                                    found: args.len(),
                                                                },
                                                                expr.span,
                                                            );
                                                        }
                                                        for (arg, &param_type) in
                                                            args.iter().zip(sig.params.iter())
                                                        {
                                                            let arg_type =
                                                                self.infer_expr(&arg.node.value);
                                                            self.unify(param_type, arg_type, arg.span);
                                                        }
                                                        self.expr_types
                                                            .insert(expr.span, sig.return_type);
                                                        return sig.return_type;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    let is_wrapper_reject = matches!(
                                        container_kind,
                                        Some(DerefWrapperKind::NonDerefContainer)
                                        | Some(DerefWrapperKind::GuardAccept)
                                        | Some(DerefWrapperKind::DerefTarget)
                                    );
                                    if (has_inherent_only || is_wrapper_reject) && !is_auto_derivable {
                                        // If inference was attempted at this
                                        // call site and failed, emit the
                                        // typed MethodGenericInferenceFailed
                                        // instead of the generic
                                        // NoMethodFound — points the user at
                                        // the specific unresolved generic +
                                        // suggests the explicit-args fix.
                                        // See `docs/devbook/09-type-checking.md`
                                        // (method-level generic inference) risk #3.
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
                                // base_name is None → the receiver is a
                                // primitive (String/int/float/bool/char/...),
                                // not a Defined/Generic user type. If the
                                // method resolved through NO avenue above
                                // (builtin protocol, trait registry/default,
                                // closure-Option/Result) AND the receiver is a
                                // concrete primitive, the method genuinely does
                                // not exist — reject cleanly instead of silently
                                // yielding error_id (which let the LIR
                                // `gorget_str_{method}` name-concat fallback
                                // invent a bogus runtime symbol → ugly C error
                                // or silent miscompile). round-31.
                                //
                                // Auto-derivable methods (clone/debug/display/
                                // hash) are intrinsic — every type has them, and
                                // they may be synthesized at IR-lowering time
                                // without appearing in `builtin_method_type`.
                                // Exempt them here exactly as the
                                // `base_name.is_some()` path does above
                                // (`:2256-2263`). LAYERING NOTE (Core #1/#3):
                                // `builtin_method_type` (this file) and the IR
                                // `GORGET_STRING_VIEW` protocol
                                // (ir/lowering/builtins.rs) are two parallel
                                // method lists that can drift — the reject here
                                // consults only the former, so any String method
                                // present in the IR protocol but absent from
                                // `builtin_method_type` (slice/upper/lower/ord)
                                // must be mirrored into the oracle. The stronger
                                // single-oracle fix has this reject consult the
                                // IR protocol directly; deferred.
                                let is_auto_derivable = matches!(
                                    method.node.as_str(),
                                    "clone" | "debug" | "display" | "hash"
                                );
                                if !is_auto_derivable
                                    && matches!(
                                        self.types.get(resolved_receiver),
                                        ResolvedType::Primitive(_)
                                    )
                                {
                                    self.error(
                                        SemanticErrorKind::NoMethodFound {
                                            method: method.node.clone(),
                                            type_: self.describe_resolved_type(resolved_receiver),
                                        },
                                        expr.span,
                                    );
                                }
                                self.types.error_id
                            }
                        }
                    }
                }
            }

            Expr::FieldAccess { object, field } => {
                // `Type.member` is a legitimate type reference: a bare qualified
                // enum-variant value (`E.A`, `Color.Red`) or a static member.
                // Propagate the type-name-OK position to the object (left spine)
                // so the value-position reject does not fire on the type name.
                self.type_name_position_ok = true;
                let object_type = self.infer_expr(object);
                let mut resolved = self.resolve_type(object_type);
                // Peel `Ref` wrappers: collection reads (`v.get(i)`, `v[i]`)
                // and borrowed values produce element REFERENCES (CoW
                // zero-cost views), so `coll.get(i).unwrap().field` sees the
                // referent's struct. Mirrors the peel in
                // `check_match_exhaustiveness`; without it the field types
                // as `error_id` and a match on it is wrongly deemed
                // non-exhaustive (definite-return false positive).
                let mut peel_depth = 0;
                while let ResolvedType::Ref(inner) = self.types.get(resolved) {
                    resolved = self.resolve_type(*inner);
                    peel_depth += 1;
                    if peel_depth > 8 {
                        break;
                    }
                }
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
                        }
                        // A missing field on a concrete user struct is reported
                        // uniformly at the definitely-absent fallthrough below.
                    }
                }
                // Field access on a CONCRETE instantiation of a generic struct
                // (`Pair[int,String] p; p.tag`). The struct's `field_types`
                // were resolved at module scope in `populate_def_field_types`,
                // where the struct's own generic params (A/B) are NOT in scope,
                // so generic-param fields resolve to `error_id` there while
                // CONCRETE fields (`int tag`, `Kind kind`) resolve correctly.
                // Return the concrete field type so a genuine mismatch
                // (`String x = p.tag`) is REJECTED rather than silently accepted
                // (Core #8), and so a `match` on a concrete field read off a
                // generic-struct element (`v.get(i).unwrap().kind`) types its
                // scrutinee (fixing the snag-13 definite-return false-positive
                // for the generic case). Generic-param fields (whose resolved
                // type contains `Error`) still fall through to `error_id` and
                // are typed later by monomorphization; the full generic-arg
                // substitution that types those precisely is the follow-up
                // (Strategy 2B — see TODO.md).
                //
                // Note: unlike the `Defined` path above, a missing field is
                // NOT reported here. Several generic types auto-deref to an
                // inner type for field access (`Box[T]`, `ReadGuard[T]` /
                // `WriteGuard[T]` / `Guard[T]`, `Weak`/`Shared`), so a field
                // absent from the outer struct is legitimately resolved through
                // the deref target downstream; emitting `NoFieldFound` here
                // would false-reject those. Reporting genuinely-missing fields
                // on generic structs needs deref-awareness and is part of the
                // full follow-up.
                if let ResolvedType::Generic(did, targs) = self.types.get(resolved).clone() {
                    if let Some(sfi) = self.struct_fields.get(&did).cloned() {
                        if let Some(field_idx) =
                            sfi.fields.iter().position(|(name, _)| name == &field.node)
                        {
                            if let Some(ast_ty) = sfi.field_ast_types.get(field_idx) {
                                // Strategy 2B: substitute the struct's generic
                                // params (`A`/`B`) with the receiver's concrete
                                // type args (`int`/`String`) and resolve the
                                // field's AST type against that map — mirrors the
                                // enum path (`resolve_user_enum_field_types`).
                                // Concrete fields (`int tag`, `Kind kind`) have no
                                // param name in the subst, so they resolve to
                                // their real type (subsumes the R36-D session-1
                                // DefInfo.field_types read); a generic-param field
                                // (`A first` → int) now resolves precisely too, so
                                // a mismatch (`String bad = p.first`) is REJECTED
                                // and a `match g.kind` on a genparam field types
                                // its scrutinee instead of false-rejecting.
                                let subst: FxHashMap<String, TypeId> = sfi
                                    .generic_param_names
                                    .iter()
                                    .cloned()
                                    .zip(targs.iter().cloned())
                                    .collect();
                                let tid = self.resolve_ast_type_with_subst(
                                    &ast_ty.node,
                                    ast_ty.span,
                                    &subst,
                                );
                                // Only trust a fully-concrete field type: a field
                                // whose type still contains `Error` after subst
                                // (unbound param, or a nested name like
                                // `Vector[A]` the shallow subst can't reach) falls
                                // through to `error_id` and is typed later by
                                // monomorphization — preserving the auto-deref
                                // guard behavior (`Box[T]`, `ReadGuard[T]`, etc.).
                                if !super::traits::type_contains_error(self.types, tid) {
                                    return tid;
                                }
                            }
                        }
                    }
                }
                // ── Definitely-absent field → REJECT (Core #8) ──────────────
                // The field resolved neither above (concrete struct field,
                // generic-struct concrete field) nor is it a valid auto-deref
                // access. Returning `error_id` here silently accepts the bogus
                // access and `error_id` then unifies with ANY downstream
                // parameter type, so the C backend emits uncompilable /
                // miscompiled code (e.g. `margs.value` on a `Vector[CallArg]`
                // stored an `int 0` into a `GorgetArray` slot →
                // "incompatible types … GorgetArray from int32_t"). Report
                // NoFieldFound for types that definitely have no such field,
                // suppressing the report for still-inferring (`Var`) /
                // already-errored types (no cascade).
                //
                // RV-A 3-way disposition (decisions.md 2026-07-16 STAGING
                // RULING + SCOPE CLARIFICATION; the brief's diagnostic table).
                // The wrapper split is keyed on the TYPED `deref_wrapper_kind`
                // seeded at registration — never a name-match here:
                //   • NonDerefContainer (Shared/Weak/Mutex/RWLock): accessed
                //     via an explicit method, never deref → always NoFieldFound.
                //   • GuardAccept (Guard/ReadGuard/WriteGuard): auto-deref →
                //     present-on-inner ACCEPTS, absent rejects. ⚠ "ACCEPTS" is
                //     this pass's disposition only — the old "auto-deref that
                //     WORKS today" wording was measured false at the WRITE faces
                //     (see the note on `DerefWrapperKind::GuardAccept`).
                //   • DerefTarget (Box, §9.4's sole target): present-on-inner is
                //     E_DerefCoercionUnimplemented (backend not built); absent /
                //     primitive inner is NoFieldFound (the §9.4 message would lie).
                let resolved_rt = self.types.get(resolved).clone();
                enum FieldDisp {
                    Accept,
                    NoField,
                    DerefUnimpl { field: String, inner: String, wrapper: String },
                }
                let disp = match &resolved_rt {
                    // Primitives (int, String, bool, float, …) have no named
                    // fields; `.foo` on them is always invalid.
                    ResolvedType::Primitive(_) => FieldDisp::NoField,
                    ResolvedType::Defined(did) | ResolvedType::Generic(did, _) => {
                        match self.scopes.get_def(*did).deref_wrapper_kind {
                            Some(DerefWrapperKind::NonDerefContainer) => FieldDisp::NoField,
                            Some(DerefWrapperKind::GuardAccept) => {
                                match self.wrapper_inner_field_status(&resolved_rt, &field.node) {
                                    InnerFieldStatus::Present { .. }
                                    | InnerFieldStatus::Unknown => FieldDisp::Accept,
                                    InnerFieldStatus::Absent => FieldDisp::NoField,
                                }
                            }
                            Some(DerefWrapperKind::DerefTarget) => {
                                match self.wrapper_inner_field_status(&resolved_rt, &field.node) {
                                    InnerFieldStatus::Present { inner_name } => {
                                        FieldDisp::DerefUnimpl {
                                            field: field.node.clone(),
                                            inner: inner_name,
                                            wrapper: self.scopes.get_def(*did).name.clone(),
                                        }
                                    }
                                    InnerFieldStatus::Absent => FieldDisp::NoField,
                                    // Cannot prove the inner's fields (generic
                                    // param / opaque inner): don't over-reject.
                                    InnerFieldStatus::Unknown => FieldDisp::Accept,
                                }
                            }
                            None => {
                                if let Some(sfi) = self.struct_fields.get(did) {
                                    // Known field list: absent iff not present. A
                                    // present-but-untyped field (generic-param
                                    // typed late by monomorphization) reaches
                                    // here and must NOT be rejected.
                                    if sfi.fields.iter().any(|(n, _)| n == &field.node) {
                                        FieldDisp::Accept
                                    } else {
                                        FieldDisp::NoField
                                    }
                                } else {
                                    // A builtin generic/opaque type with no user
                                    // field list (`Vector`, `Dict`, `Set`,
                                    // `HashMap`, an enum, …) — genuinely absent.
                                    FieldDisp::NoField
                                }
                            }
                        }
                    }
                    // Named fields on tuples: only the underscore alias
                    // `._0`/`._1`/… is legal (language-reference §4.2 / §7.8);
                    // bare `.0` is `TupleFieldAccess`. Any other name is absent.
                    ResolvedType::Tuple(elems) => {
                        let name = field.node.as_str();
                        if let Some(rest) = name.strip_prefix('_') {
                            if !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit()) {
                                if let Ok(idx) = rest.parse::<usize>() {
                                    if let Some(&elem_tid) = elems.get(idx) {
                                        return elem_tid;
                                    }
                                }
                            }
                        }
                        FieldDisp::NoField
                    }
                    _ => FieldDisp::Accept,
                };
                match disp {
                    FieldDisp::Accept => {}
                    FieldDisp::NoField => {
                        let type_name = self.describe_resolved_type(resolved);
                        self.error(
                            SemanticErrorKind::NoFieldFound {
                                field: field.node.clone(),
                                type_: type_name,
                            },
                            expr.span,
                        );
                    }
                    FieldDisp::DerefUnimpl { field, inner, wrapper } => {
                        self.error(
                            SemanticErrorKind::DerefCoercionUnimplemented { field, inner, wrapper },
                            expr.span,
                        );
                    }
                }
                self.types.error_id
            }

            Expr::TupleFieldAccess { object, index } => {
                let object_type = self.infer_expr(object);
                let mut resolved = self.resolve_type(object_type);
                // Peel `Ref` wrappers, as in `FieldAccess`: a tuple read from
                // a collection element (`coll.get(i).unwrap().0`) is a view.
                let mut peel_depth = 0;
                while let ResolvedType::Ref(inner) = self.types.get(resolved) {
                    resolved = self.resolve_type(*inner);
                    peel_depth += 1;
                    if peel_depth > 8 {
                        break;
                    }
                }
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
                // Round XXIX Track A: `[]` is a SINGLE-SOURCE-OF-TRUTH gate on
                // the `Index[K,V]` trait (Layering rule 3). Prior to this
                // rewrite two parallel decision paths (hardcoded kind gate at
                // `src/ir/lowering/exprs/mod.rs:2984` and the trait dispatch
                // at `src/semantic/traits.rs:754`) left non-indexable receivers
                // — plain structs, Set, Tuple, Option, primitive int, enum —
                // falling through to an unchecked raw offset READ at both
                // backends. The trait registry query below (`has_trait_impl_by_name`)
                // is the sole semantic gate; builtins (Vector/Deque/Dict/HashMap/
                // String) satisfy `Index` intrinsically via the pattern in
                // `traits.rs::is_indexable_intrinsic` (same shape as
                // `is_numeric_primitive` / `is_hashable_primitive`).
                //
                // A generic type application in a type-ref chain
                // (`SparseSet[Health].new()` — the Index is the receiver of a
                // static method call) must let BOTH its object AND its type-arg
                // ("index" slot) be type names. Propagate ONLY when THIS Index is
                // itself in a type-name-OK position; a standalone `Type[args]` in
                // value position still rejects.
                self.type_name_position_ok = type_name_position_ok;
                let object_type = self.infer_expr(object);
                self.type_name_position_ok = type_name_position_ok;
                let index_type = self.infer_expr(index);
                let resolved_obj = self.resolve_type(object_type);

                // Silence cascading errors on inference variables / prior
                // errors / divergent (`throw`/`return`) — matches the
                // `UnwrapOnNonOptional` / `DefaultOp` suppression pattern.
                if matches!(
                    self.types.get(resolved_obj),
                    ResolvedType::Var(_) | ResolvedType::Error | ResolvedType::Never
                ) {
                    return self.types.error_id;
                }

                let is_range_index = matches!(&index.node, Expr::Range { .. });

                // Preserve the container-slice branches for the two receivers
                // where `x[a..b]` returns the CONTAINER type (not the element):
                // `v[a..b]` on Vector and `s[a..b]` on String. All other
                // Index-implementing receivers fall through to the scalar-K
                // unify below; a `Range` index against a scalar K trips
                // `E_TypeMismatch`, which is the correct signal.
                let obj_name = self.type_key_for_trait_lookup(resolved_obj);
                if is_range_index {
                    if resolved_obj == self.types.string_id {
                        return self.types.string_id;
                    }
                    if obj_name.as_deref() == Some("Vector") {
                        return resolved_obj;
                    }
                }

                // Anonymous types with no registry key (Tuple, closure,
                // function type) — reject by describe. `type_key_for_trait_lookup`
                // returns `None` for these.
                let Some(type_name) = obj_name else {
                    let type_ = self.describe_resolved_type(resolved_obj);
                    self.error(SemanticErrorKind::NotIndexable { type_ }, expr.span);
                    return self.types.error_id;
                };

                // SINGLE SEMANTIC GATE: `[]` requires an `Index[K,V]` impl.
                // Vector/Deque/Dict/HashMap/String satisfy intrinsically
                // (`is_indexable_intrinsic`); user types via `equip T with
                // Index[K,V]`. Set/HashSet/Tuple/plain struct/enum/primitive
                // int all fail here — the ungated-`[]` memory-unsafety class.
                if !self.traits.has_trait_impl_by_name(&type_name, "Index") {
                    self.error(
                        SemanticErrorKind::NotIndexable { type_: type_name.clone() },
                        expr.span,
                    );
                    return self.types.error_id;
                }

                // Impl exists. Snag #49 family: a `throws`-fn call returning
                // `Result[K, E]` in index position should auto-propagate to
                // the index type (`str[int]`, `Vector[T][int]`, `Dict[K,V][K]`)
                // when the enclosing function can propagate. Skip the strict
                // unify in that case so IR-lowering's centralized auto-prop
                // hook handles the unwrap (Snag #11: the index-position
                // auto-prop now routes through the shared E-checked gate).
                //
                // Compute (K, V) — SINGLE dispatch site on `type_name`. The
                // arm-count lint (`tests/lints.rs::index_arm_type_name_gates_count`)
                // pins this to exactly one `.as_str()` occurrence so a
                // future ad-hoc `if type_name == "Foo"` gate trips the count
                // (Core #6). Element extraction is uniform: Generic-shape
                // (Vector/Deque/Dict/HashMap) reads `args[0]` / `args[1]`;
                // Array-shape (`ResolvedType::Array(elem, _)`) reads the
                // fixed-size elem; String is a leaf.
                let generic_args: Option<Vec<TypeId>> =
                    if let ResolvedType::Generic(_, args) = self.types.get(resolved_obj) {
                        Some(args.clone())
                    } else {
                        None
                    };
                let array_elem: Option<TypeId> =
                    if let ResolvedType::Array(elem, _) = self.types.get(resolved_obj) {
                        Some(*elem)
                    } else {
                        None
                    };
                let (key_tid, val_tid) = match type_name.as_str() {
                    "Vector" | "Deque" | "Array" => {
                        let elem = generic_args
                            .as_ref()
                            .and_then(|a| a.first().copied())
                            .or(array_elem)
                            .unwrap_or(self.types.error_id);
                        (self.types.int_id, elem)
                    }
                    "Dict" | "HashMap" => {
                        let a = generic_args.as_ref();
                        let k = a
                            .and_then(|a| a.first().copied())
                            .unwrap_or(self.types.error_id);
                        let v = a
                            .and_then(|a| a.get(1).copied())
                            .unwrap_or(self.types.error_id);
                        (k, v)
                    }
                    "String" => (self.types.int_id, self.types.string_id),
                    _ => self
                        .user_index_key_value(&type_name, expr.span)
                        .unwrap_or((self.types.error_id, self.types.error_id)),
                };
                if !self.auto_prop_skips_unify(key_tid, index_type, index.span) {
                    self.unify(index_type, key_tid, index.span);
                }
                val_tid
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
                let lhs_type = self.infer_expr(lhs);
                let rhs_type = self.infer_expr(rhs);
                // `lhs ?? rhs` unwraps an `Option`/`Result` LHS (first variant
                // `Some`/`Ok`), substituting `rhs` on `None`/`Error`. A LHS that
                // is neither carrier is ill-formed: the type checker used to
                // discard `lhs_type` and return `rhs_type` (a silent no-op), and
                // the IR lowering then assumed an enum LHS and fell back to
                // `("Some", lhs_type)` — emitting C that reinterprets the LHS
                // bits as an enum (e.g. `'void *' from 'int64_t'`), which
                // crashes/exits-1 at runtime. Reject it as a clean type error.
                // Sibling of the `UnwrapOnNonOptional`/`DerefNonBox` guard class
                // (AGENTS.md Core invariant #8 — reject UB, don't miscompile).
                // `??` accepts BOTH Option and Result (verified by running
                // `Result ?? x`), so reuse the SAME `is_option_or_result_receiver`
                // helper `unwrap` uses. SUPPRESS for an LHS whose inference is
                // still incomplete (`Var`) or already errored/divergent
                // (`error_id`/`never_id`) — mirrors the `UnwrapOnNonOptional`
                // suppression (`:2216-2218`) so we never false-positive on
                // valid or divergent (`throw`/`return`/`panic`) code.
                let resolved_lhs = self.resolve_type(lhs_type);
                if !self.is_option_or_result_receiver(lhs_type)
                    && !matches!(self.types.get(resolved_lhs), ResolvedType::Var(_))
                    && resolved_lhs != self.types.error_id
                    && resolved_lhs != self.types.never_id
                {
                    self.error(
                        SemanticErrorKind::DefaultOpNonOptional {
                            type_: self.describe_resolved_type(lhs_type),
                        },
                        expr.span,
                    );
                    return self.types.error_id;
                }
                // Round XXXIX Track E (owner Option B chain-friendly + LAZY
                // ratification 2026-08-09): RHS may be inner `T` (unwrapping
                // form), the SAME carrier shape (peel-outer for `a ?? b ??
                // default`), or divergent. Anything else silently size-truncated
                // in the IR-lowering else-branch — a Core #1 write-site defect
                // upstream of the SIGSEGV on `Option[int] ?? Option[int] ??
                // int` (repro at `known_gaps/default_op_left_nested_chain_segv.gg`,
                // graduated same round to `default_op_option_rhs_accepted.gg`).
                //
                // Layering rule 3 (one source of truth): the typechecker writes
                // the CANONICAL result type into `expr_types`; the IR lowering
                // reads it and picks the shape switch (`Expr::DefaultOp` arm at
                // `src/ir/lowering/exprs/mod.rs`). No independent re-inference
                // in the lowering.
                let resolved_rhs = self.resolve_type(rhs_type);
                let rhs_suppressed = matches!(self.types.get(resolved_rhs), ResolvedType::Var(_))
                    || resolved_rhs == self.types.error_id
                    || resolved_rhs == self.types.never_id;
                let inner_t = self.default_op_inner_type(resolved_lhs);
                // Pin the LHS's canonical semantic type on its span so the IR
                // lowering can decide the shape switch without independent
                // re-inference. TypeIds are interned; equality with the outer
                // expr's canonical is the discriminator.
                self.expr_types.insert(lhs.span, resolved_lhs);
                if rhs_suppressed {
                    // Var / error / divergent — carve-out. Result type is the
                    // inner `T` (or fall back to the LHS carrier if inner
                    // extraction failed, e.g. for a still-inference LHS).
                    let canonical = inner_t.unwrap_or(resolved_lhs);
                    self.expr_types.insert(expr.span, canonical);
                    return canonical;
                }
                let carrier_match = self.default_op_rhs_matches_carrier(resolved_lhs, resolved_rhs);
                let inner_match = inner_t
                    .map(|t| self.resolve_type(t) == resolved_rhs)
                    .unwrap_or(false);
                if !carrier_match && !inner_match {
                    let expected = match inner_t {
                        Some(t) => format!(
                            "`{}` (unwrapped) or `{}` (matching left)",
                            self.describe_resolved_type(t),
                            self.describe_resolved_type(resolved_lhs),
                        ),
                        None => format!(
                            "`{}` (matching left)",
                            self.describe_resolved_type(resolved_lhs),
                        ),
                    };
                    self.error(
                        SemanticErrorKind::DefaultOpRhsTypeMismatch {
                            expected,
                            actual: self.describe_resolved_type(rhs_type),
                        },
                        expr.span,
                    );
                    return self.types.error_id;
                }
                let canonical = if carrier_match {
                    resolved_lhs
                } else {
                    inner_t.unwrap_or(rhs_type)
                };
                self.expr_types.insert(expr.span, canonical);
                canonical
            }

            Expr::Move { expr: inner }
            | Expr::MutableBorrow { expr: inner } => {
                self.infer_expr(inner) // ownership modifiers don't change the type
            }

            // D29: `expr!` propagation. The mark is recorded via the one-shot
            // `fallible_call_marked` (mirror of `suppress_auto_prop`) so the
            // immediately-enclosed fallible call verifies the mark is present.
            // TRANSPARENCY (two-layer, scout Finding 5): forward the captured
            // `suppress_auto_prop` one-shot into the inner call — a `!` between a
            // disposition (`catch`/`rethrow`) and its call must NOT eat the
            // suppress signal, or the call auto-props to `T` and the disposition
            // can no longer read the raw Result. Mirror on the lowering side.
            //
            // R3 (lying marks): the mark must be VERIFIED CONSUMED. The fallible
            // chokepoints set `fallible_mark_consumed` when they see the mark; a
            // `!` whose inner is NOT a fallible call (`5!`, `pure(3)!`, `r!` on a
            // Result local, the outer mark of `f()!!` — "no second mark" pin)
            // never consumes it → error. Save/restore so a nested inner mark's
            // consumption can't satisfy the OUTER mark's check.
            Expr::Propagate { expr: inner } => {
                let prev_consumed = std::mem::replace(&mut self.fallible_mark_consumed, false);
                self.fallible_call_marked = true;
                self.suppress_auto_prop = suppress_auto_prop;
                let inner_type = self.infer_expr(inner);
                let consumed =
                    std::mem::replace(&mut self.fallible_mark_consumed, prev_consumed);
                // Skip the lying-mark error when the inner inference already
                // failed (error_id) — the chokepoint may legitimately not have
                // run, and the real error is already reported.
                if !consumed && self.resolve_type(inner_type) != self.types.error_id {
                    self.error(
                        SemanticErrorKind::MissingFallibleMark {
                            throws_type: String::new(),
                            reason: FallibleMarkReason::MarkOnInfallible,
                        },
                        expr.span,
                    );
                }
                inner_type
            }

            Expr::Deref { expr: inner } => {
                let inner_type = self.infer_expr(inner);
                // Peel any ownership wrappers (`&box`, `!box`) before checking
                // for the smart-pointer shape, so `*(&b)` / `*(!b)` still unwrap.
                let mut resolved = self.resolve_type(inner_type);
                while let ResolvedType::Ref(t) | ResolvedType::Owned(t) =
                    self.types.get(resolved).clone()
                {
                    resolved = self.resolve_type(t);
                }
                // *expr unwraps Box[T] → T (the only smart-pointer type `*` is
                // valid on — see docs/language-reference.md §7.4: `*` operand is
                // "Pointer/smart ptr", and Box is the lone such type in the
                // language; deref-coercion §9 / Box[T] §«Box methods»).
                if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
                    if self.scopes.get_def(def_id).name == "Box" && args.len() == 1 {
                        return args[0];
                    }
                }
                // Any OTHER concretely-resolved type is not deref-able. Without
                // an error here the type checker returned `inner_type` unchanged
                // (a silent no-op) and the IR lowering emitted a garbage pointer
                // dereference (`*(int64_t*)(*(void**)&value)`) that segfaults at
                // runtime. Mirror the `unwrap`-on-non-Option guard above
                // (`UnwrapOnNonOptional`): suppress the error for types whose
                // inference is still incomplete (`Var`) or already errored, to
                // avoid spurious diagnostics on otherwise-valid code.
                if !matches!(self.types.get(resolved), ResolvedType::Var(_) | ResolvedType::Error)
                    && resolved != self.types.error_id
                {
                    self.error(
                        SemanticErrorKind::DerefNonBox {
                            type_: self.describe_resolved_type(inner_type),
                        },
                        expr.span,
                    );
                    return self.types.error_id;
                }
                inner_type
            }

            Expr::Await { expr: inner, .. } => {
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
                // Clear the inherited `decl_type_hint` so a throws/Result call
                // scrutinee is not read as an explicit-`Result` capture position
                // (mirror of lowering's `expected_type.take()` at
                // `patterns.rs:389`). D29 (2026-07-17 amendment): a kind-1 throws
                // call scrutinee is NO LONGER auto-suppressed to a raw Result —
                // `match f()!:` peels to `T` (mark required; Ok/Error arms then
                // fail to match `T` — bind to a Result first). A kind-2 call and
                // a plain Result LOCAL already carry `Result` as a value, so they
                // match Ok/Error without any suppression.
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = None;
                let scrutinee_type = self.infer_expr(scrutinee);
                self.decl_type_hint = prev_hint;
                // Record the scrutinee's typed shape so downstream passes
                // (e.g. `lint:suggest_throws`) can recognize Result[T, E]
                // scrutinees without re-running inference.
                self.expr_types.insert(scrutinee.span, scrutinee_type);
                // D29: `match f()!:` peels to `T` — reject `Ok`/`Error` arms
                // (they can no longer inspect the whole Result; capture first).
                self.check_result_arms_against_scrutinee(
                    scrutinee_type,
                    arms.iter().map(|a| &a.pattern.node),
                    scrutinee.span,
                );
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
                    Stmt::Return(_) | Stmt::Throw(_) | Stmt::Break | Stmt::Continue
                ));
                // Expression block: the tail IS the block's value (consumed).
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, false);
                let block_ty = self.check_block(block);
                self.tail_value_dropped = prev_dropped;
                if last_is_divergent {
                    self.types.never_id
                } else {
                    block_ty
                }
            }

            Expr::Do { body, .. } => {
                // Expression block: the tail IS the do-value (consumed).
                // Divergent tail (`return` / `throw` / `break` / `continue`)
                // types as Never — same shape as `Expr::Block` above, and
                // for the same reason: a value-position Do-block whose tail
                // diverges must unify with anything (`unify`'s Never rule at
                // :985-990), otherwise a multi-line `catch (_): … return`
                // recovery false-mismatches against the OK type. Symmetry
                // with `Expr::Block` was not enforced before Track A
                // surfaced it; the two site kinds ARE the same class.
                let last_is_divergent = body.stmts.last().map_or(false, |s| matches!(
                    &s.node,
                    Stmt::Return(_) | Stmt::Throw(_) | Stmt::Break | Stmt::Continue
                ));
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, false);
                let ty = self.check_block(body);
                self.tail_value_dropped = prev_dropped;
                if last_is_divergent {
                    self.types.never_id
                } else {
                    ty
                }
            }

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
                // Snag #44 (2026-05-13): the closure body is a separate fn
                // at the LIR level — its return type is independent of the
                // enclosing fn's. A `throw E(...)` inside the closure body
                // can't write to the enclosing fn's Result return slot (no
                // longjmp at C). Save/restore `current_function_throws` so
                // a throw inside a closure body in a `throws E` fn surfaces
                // as a proper "ThrowInNonThrowingFunction" diagnostic
                // rather than silently passing typecheck and crashing at
                // C-emit (`gorget_throw(struct_value, …)` type mismatch).
                // Closures that should be allowed to throw need either
                // explicit `throws` syntax on the closure (not yet
                // supported) or the enclosing fn's `?? throw E(...)` form,
                // which lowers as a divergent expression in the calling
                // frame rather than inside the closure body.
                let saved_throws = self.current_function_throws;
                self.current_function_throws = false;
                // Snag #11: the closure body is a separate fn — clear the
                // enclosing caller-E so a propagation inside the closure isn't
                // gated against the wrong error type. (`current_return_type`
                // is already set to the closure's return var above.)
                let saved_throws_tid = self.current_fn_throws_type_id;
                self.current_fn_throws_type_id = None;

                let body_type = self.infer_expr(body);

                self.current_function_throws = saved_throws;
                self.current_fn_throws_type_id = saved_throws_tid;
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

            Expr::ArrayLiteral(elements, _) => {
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
                // Propagate declared per-element types from a Tuple
                // decl_type_hint so nested collection literals coerce
                // correctly. Mirrors the DictLiteral fix: without this,
                // `(Vector[int], int) p = ([1, 2, 3], 42)` fails typecheck
                // because the first element types as `int[3]` regardless of
                // the declared `Vector[int]`. With this, each element infers
                // under its declared expected-type.
                let elem_hints: Option<Vec<TypeId>> = self.decl_type_hint
                    .and_then(|hint| {
                        let resolved = self.resolve_type(hint);
                        if let ResolvedType::Tuple(types) = self.types.get(resolved).clone() {
                            if types.len() == elements.len() {
                                return Some(types);
                            }
                        }
                        None
                    });
                let prev_hint = self.decl_type_hint;
                let elem_types: Vec<TypeId> = elements.iter().enumerate().map(|(i, e)| {
                    self.decl_type_hint = elem_hints.as_ref().map(|hs| hs[i]);
                    self.infer_expr(e)
                }).collect();
                self.decl_type_hint = prev_hint;
                // Use the declared element types when available so the
                // tuple's recorded type aligns with what the var-decl
                // checker will unify against (analogous to DictLiteral's
                // final-K/V hint fallback). For elements without a hint,
                // fall through to the inferred type.
                let final_types: Vec<TypeId> = match elem_hints {
                    Some(hints) => hints,
                    None => elem_types,
                };
                self.types.insert(ResolvedType::Tuple(final_types))
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
            Expr::Rethrow { expr: inner, error_binding, transform } => {
                // `rethrow` operates on the raw `Result[T, E]` (its error
                // path transforms the `E`), so suppress the throws-fn-call
                // auto-prop peel on the inner — mirror of lowering's
                // `suppress_auto_prop = true` at `exprs/mod.rs:2952`.
                self.suppress_auto_prop = true;
                let inner_type = self.infer_expr(inner);
                // Snag #37: bind the error-payload local's type. The
                // resolver registers `name` in scope; here we set its
                // `type_id` from the user-declared `Type` in the binding,
                // so references to `name` in the transform resolve to a
                // proper type instead of `<error>`.
                if let Some((ty, name)) = error_binding {
                    if let Some(def_id) = self.scopes.lookup_def_by_span(&name.node, name.span) {
                        if let Ok(tid) = super::types::ast_type_to_resolved(
                            &ty.node, ty.span, self.scopes, self.types,
                        ) {
                            self.scopes.get_def_mut(def_id).type_id = Some(tid);
                        }
                    }
                }
                self.infer_expr(transform);
                if !self.current_function_throws {
                    self.error(SemanticErrorKind::RethrowInNonThrowingFunction, expr.span);
                }
                // Snag #35 / #11: a `rethrow` resolves the inner Result itself —
                // its Ok path yields `T`, its Error path transforms `E` and
                // re-throws — so the EXPRESSION's value type is `T`, not
                // `Result[T, E]` (mirrors `catch` below). Returning the raw
                // Result made consumer positions (`int v = call() rethrow …`)
                // mis-classify it as an auto-propagation of the INNER error type
                // and trip the snag #11 cross-error gate, even though the
                // rethrow already converts the error. Peel to `T`.
                let resolved = self.resolve_type(inner_type);
                if let ResolvedType::Generic(def_id, ref args) = self.types.get(resolved).clone() {
                    if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" {
                        return args[0];
                    }
                }
                inner_type
            }
            Expr::Catch { expr: inner, error_binding, recovery } => {
                // `catch` resolves the raw `Result[T, E]` itself (reads the
                // `E` slot for the error binding, the `T` slot for the
                // expression's value), so suppress the throws-fn-call
                // auto-prop peel on the inner — mirror of lowering's
                // `suppress_auto_prop = true` at `exprs/mod.rs:3093`.
                self.suppress_auto_prop = true;
                let inner_type = self.infer_expr(inner);
                // Snag #37: bind the error-payload local's type. The error
                // type comes from the throws-Result's `E` slot. The
                // resolver registers `name` in scope; here we set its
                // `type_id` so references to `name` in the recovery
                // expression resolve to a proper type.
                let resolved = self.resolve_type(inner_type);
                let err_ty = if let ResolvedType::Generic(def_id, ref args) =
                    self.types.get(resolved).clone()
                {
                    if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" {
                        Some(args[1])
                    } else { None }
                } else { None };
                if let Some(err_ty) = err_ty {
                    if let Some(def_id) = self.scopes.lookup_def_by_span(
                        &error_binding.node, error_binding.span,
                    ) {
                        self.scopes.get_def_mut(def_id).type_id = Some(err_ty);
                    }
                }
                // Snag #35: throws calls now type as `Result[T, E]` at the
                // call site. `catch` resolves the Result, so the
                // expression's type is the OK type, not the Result.
                let ok_ty = if let ResolvedType::Generic(def_id, ref args) =
                    self.types.get(resolved).clone()
                {
                    if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" {
                        Some(args[0])
                    } else { None }
                } else { None };
                // Recovery-type check (Track A, Core #10): route the recovery
                // through the canonical three-carve-out unify contract that
                // VarDecl / Assign / arg-pass / return sites use, so an
                // ill-typed recovery is rejected at the writer site. Also
                // installs the OK type as `decl_type_hint` so literals like
                // `[]` / `None` coerce here just as they do at those sites.
                // Non-Result inner (already an error path — inner_type is
                // typically `error_id` here) routes through the same helper
                // with `inner_type` as the expected slot; `unify`'s
                // error_id-passes-through rule at `:980-982` makes that a
                // no-op-safe check while still keeping the recovery's OWN
                // diagnostics live and, critically, keeping this arm free of
                // the bare-`infer_expr` shape the class-retiring lint watches.
                let expected = ok_ty.unwrap_or(inner_type);
                self.check_recovery_type(recovery, expected);
                if let Some(t) = ok_ty { t } else { inner_type }
            }
        }
    }

    // ─── Statement Checking ────────────────────────────────

    // `expr_is_borrow_bind` + `block_tail_is_borrow_bind` were lifted to
    // `src/semantic/type_utils.rs` (Round XXV Track D §D-3, path (a)) so
    // both the typechecker (authoritative D10(a) rejector) and the
    // borrow-checker (mirror-walker suppressor) read the same predicate.
    // Call sites here go through
    // `crate::semantic::type_utils::expr_is_borrow_bind` — 3 sites at
    // `Stmt::VarDecl`, `Stmt::Assign`, `Item::StaticDecl` retained; the
    // borrow-checker adds a fourth (mirror-walker suppression flag set
    // from `check_stmt.rs`).

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Option<TypeId> {
        match &stmt.node {
            Stmt::VarDecl {
                type_, pattern, value, shared, ..
            } => {
                // D10(a): local `&`-binds are rejected — both forms (the
                // `T &name = ..` decl-sigil form is a parse error; this is
                // the `name = &expr` init form). Emit and continue checking
                // so downstream type output stays intact (one clean error,
                // no cascade).
                if crate::semantic::type_utils::expr_is_borrow_bind(&value.node) {
                    self.error(SemanticErrorKind::LocalBorrowBind, value.span);
                }
                // Resolve declared type first so we can set the hint for literal coercion
                let declared_type = match &type_.node {
                    Type::Inferred => None,
                    _ => {
                        // By the typecheck pass every DEFINED type is in scope —
                        // cross-module (resolve fixup done) AND the enclosing fn's
                        // generic params (free fns via `current_fn_scope`, equip
                        // blocks via the equip-generics list). So a `Type::Named`
                        // still unknown after all those checks is genuinely
                        // undefined: surface it instead of letting
                        // `ast_type_to_resolved` degrade it to `error_id`
                        // (→ silently defaulted to unit downstream).
                        // See docs/devbook/09-type-checking.md, "Unknown type names".
                        if let Some((name_node, suggestion)) =
                            super::types::unknown_named_type(
                                &type_.node,
                                self.scopes,
                                self.current_fn_scope,
                            )
                        {
                            // Suppress for an equip block's target-implicit
                            // generic params (`equip X[T]:` — `T` is never a
                            // scope def, so it can't be found by lookup).
                            if !self.current_equip_generics.contains(&name_node.node) {
                                self.error(
                                    SemanticErrorKind::UndefinedName {
                                        name: name_node.node.clone(),
                                        suggestion,
                                    },
                                    name_node.span,
                                );
                            }
                        }
                        // Track P: propagate Err (NonDerefContainer[BareTrait]) — the
                        // legacy `.ok()` silently dropped it. Other error kinds from
                        // this call are already reported ahead of time (e.g.
                        // `unknown_named_type` above); the new NonDerefContainer[Trait]
                        // reject is the one that would otherwise vanish here.
                        match super::types::ast_type_to_resolved(
                            &type_.node,
                            type_.span,
                            self.scopes,
                            self.types,
                        ) {
                            Ok(tid) => Some(tid),
                            Err(e) => {
                                self.error(e.kind, e.span);
                                None
                            }
                        }
                    }
                };

                // Check generic type parameter trait bounds (e.g. Dict[K: Hashable, V])
                if let Some(dt) = declared_type {
                    self.check_struct_type_bounds(dt, type_.span);
                }

                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = declared_type;
                // D29 (2026-07-17 amendment): an `auto`/inferred destination does
                // NOT capture a fallible call — `auto r = f()` types as `T` and
                // requires the mark (`auto r = f()!`). Only an EXPLICITLY
                // `Result[T,E]`-annotated destination captures unmarked (that is
                // `dest_is_result` at the chokepoint, driven by `declared_type`).
                // So there is no longer an auto-capture suppress here; the
                // pre-amendment `auto r = throws_call()` idiom now diagnoses
                // E_MissingFallibleMark (the migration rewrites it to an explicit
                // `Result[T,E] r = f()` capture where a whole Result is wanted).
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
                                && !self.auto_prop_skips_unify(declared_type, value_type, value.span)
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
                let ty = self.infer_expr(expr);
                if self.resolve_type(ty) == self.types.never_id {
                    self.diverging_exprs.insert(expr.span);
                }
                return Some(ty);
            }

            Stmt::Assign { target, value } => {
                // D10(a): `name = &expr` re-binds a mutable borrow to a
                // name — same class as the VarDecl-init form, same
                // rejection (see `type_utils::expr_is_borrow_bind`).
                if crate::semantic::type_utils::expr_is_borrow_bind(&value.node) {
                    self.error(SemanticErrorKind::LocalBorrowBind, value.span);
                }
                self.check_assign_target_lvalue(target);
                self.check_string_index_assign(target);
                self.check_index_mut_assign(target);
                let target_type = self.infer_expr(target);
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = Some(target_type);
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;
                // `is_collection_assignment`: mirror the `Stmt::VarDecl`
                // arm so `v = [1, 2, 3]` (where `v: Vector[int]`) accepts
                // a bare collection literal, exactly as the VarDecl-init
                // form does.
                if !self.is_collection_assignment(target_type, value_type)
                    && !self.auto_prop_skips_unify(target_type, value_type, value.span)
                    && !self.is_result_capture_compatible(target_type, value_type)
                {
                    self.unify(target_type, value_type, value.span);
                }
            }

            Stmt::CompoundAssign { target, value, op } => {
                self.check_assign_target_lvalue(target);
                self.check_string_index_assign(target);
                self.check_index_mut_assign(target);
                let target_type = self.infer_expr(target);
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = Some(target_type);
                let value_type = self.infer_expr(value);
                self.decl_type_hint = prev_hint;
                self.unify(target_type, value_type, value.span);
                // Reject ops with no builtin/overload for the LHS type so
                // accepted programs never reach the resource-moves ICE
                // (`s.name -= "x"`) or broken C (identifier/binary forms).
                self.check_operator_supported(target_type, *op, /*compound=*/ true, stmt.span);
            }

            Stmt::Return(expr) => {
                if self.current_return_type.is_none() {
                    self.error(SemanticErrorKind::ReturnOutsideFunction, stmt.span);
                }
                if expr.is_none() {
                    // Bare `return;` is only valid when the function returns
                    // void — including `void throws E`, whose RAW declared
                    // return type (`current_return_type`) is void (the `E`
                    // lives separately in `func.throws`). In a non-void
                    // function (`int`, `int throws E`, …) a bare return
                    // previously slipped through unchecked and silently
                    // lowered to a zero-initialized value (e.g. `Ok(0)` for
                    // `T throws E`); reject it as a missing return value,
                    // symmetric with a non-void non-throwing function.
                    if let Some(ret_type) = self.current_return_type {
                        if self.resolve_type(ret_type) != self.types.void_id {
                            self.error(
                                SemanticErrorKind::TypeMismatch {
                                    expected: self.describe_resolved_type(ret_type),
                                    found: "()".to_string(),
                                },
                                stmt.span,
                            );
                        }
                    }
                }
                if let Some(expr) = expr {
                    // Snag #36 + D23 §5.1: `return throws_fn(...)` auto-propagates
                    // / captures just like `T x = throws_fn(...)`. Shared with the
                    // expression-body tail via `check_return_value` — the guards
                    // (auto-prop skip, collection-literal, whole-`Result` capture)
                    // and the return-type hint live in one place so the two return
                    // forms can't drift.
                    self.check_return_value(self.current_return_type, expr);
                }
            }

            Stmt::Throw(expr) => {
                self.infer_expr(expr);
                if !self.current_function_throws {
                    self.error(SemanticErrorKind::ThrowInNonThrowingFunction, stmt.span);
                }
            }

            Stmt::Break => {
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
                                "Vector" => args.first().copied(),
                                // Round XXVII Track D: Set/HashSet impl
                                // `Iterable[T]` (they expose `.iter()`), NOT
                                // `Iterator[T]`. `.enumerate()` is an
                                // Iterator adapter (`lib/std/iter.gg:307/814`,
                                // `docs/book/05-collections.md:198-207`).
                                // Pre-D27 the enumerate scaffold at
                                // `for_loops.rs:lower_for_enumerate` read a
                                // Vector-shaped `iter.Field(2)` against the
                                // hash-table layout — silent zero output on
                                // both C+LLVM (Core #10 lower-or-reject). The
                                // reject fires ONLY in the `is_enumerate`
                                // path; a plain `for x in s:` still lowers
                                // through the collection-iter protocol.
                                // Round XXIX Track C: extend the reject to
                                // Dict/HashMap. `d.enumerate()` builds a
                                // no-`Iterator[T]`-receiver `EnumerateIter`
                                // wrapper and SIGSEGVs at runtime (Dict/
                                // HashMap impl `Iterable`, not `Iterator`).
                                // Same class as Set/HashSet — the fix-it
                                // advice `.iter().enumerate()` stays the
                                // reference-grade spelling on both lanes.
                                "Set" | "HashSet" | "Dict" | "HashMap" if is_enumerate => {
                                    self.error(
                                        SemanticErrorKind::EnumerateOnNonIterator {
                                            type_: name.clone(),
                                        },
                                        iterable.span,
                                    );
                                    args.first().copied()
                                }
                                "Set" | "HashSet" | "Dict" | "HashMap" => args.first().copied(),
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
                // R4: a loop body's per-iteration tail value is DROPPED — a bare
                // fallible call there silently drops its Error every iteration.
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, true);
                self.check_block(body);
                self.tail_value_dropped = prev_dropped;
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
                // Snag #49 family: a `throws`-fn call returning `Result[bool, E]`
                // in an if/while condition position should auto-propagate to
                // `bool` (mirrors the call-arg / return / VarDecl gates) when
                // the enclosing function can propagate. Skip the strict unify
                // in that case so IR-lowering's centralized auto-prop hook
                // handles the unwrap.
                if !self.auto_prop_skips_unify(self.types.bool_id, cond_type, condition.span) {
                    self.unify(cond_type, self.types.bool_id, condition.span);
                }
                // Assign types to all `is` pattern bindings (including compound conditions)
                self.assign_compound_is_types(condition);
                self.loop_depth += 1;
                // R4: a loop body's per-iteration tail value is DROPPED — a bare
                // fallible call there silently drops its Error every iteration.
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, true);
                self.check_block(body);
                self.tail_value_dropped = prev_dropped;
                self.loop_depth -= 1;
                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                }
            }

            Stmt::Loop { body } => {
                self.loop_depth += 1;
                // R4: loop-body tail value dropped per iteration (see While/For).
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, true);
                self.check_block(body);
                self.tail_value_dropped = prev_dropped;
                self.loop_depth -= 1;
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                let cond_type = self.infer_expr(condition);
                // Snag #49 family: see `Stmt::While` above.
                if !self.auto_prop_skips_unify(self.types.bool_id, cond_type, condition.span) {
                    self.unify(cond_type, self.types.bool_id, condition.span);
                }
                // Assign types to all `is` pattern bindings (including compound conditions)
                self.assign_compound_is_types(condition);
                let then_type = self.check_block(then_body);

                for (cond, body) in elif_branches {
                    let ct = self.infer_expr(cond);
                    if !self.auto_prop_skips_unify(self.types.bool_id, ct, cond.span) {
                        self.unify(ct, self.types.bool_id, cond.span);
                    }
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
                // D29 (2026-07-17 amendment): no auto-suppress for a throws/
                // Result call scrutinee — `match f()!:` peels to `T` (see the
                // `Expr::Match` arm). Clear the hint so the scrutinee is not read
                // as an explicit-`Result` capture position.
                let prev_hint = self.decl_type_hint;
                self.decl_type_hint = None;
                let scrutinee_type = self.infer_expr(scrutinee);
                self.decl_type_hint = prev_hint;
                // Record the scrutinee's typed shape so downstream passes
                // (e.g. `lint:suggest_throws`) can recognize Result[T, E]
                // scrutinees without re-running inference.
                self.expr_types.insert(scrutinee.span, scrutinee_type);
                // D29: `match f()!:` peels to `T` — reject `Ok`/`Error` arms.
                self.check_result_arms_against_scrutinee(
                    scrutinee_type,
                    arms.iter().filter_map(|i| i.arm()).map(|a| &a.pattern.node),
                    scrutinee.span,
                );
                let mut first_arm_type = None;
                for arm in arms.iter().filter_map(|i| i.arm()) {
                    self.assign_pattern_types(&arm.pattern, scrutinee_type);
                    if let Some(guard) = &arm.guard {
                        let gt = self.infer_expr(guard);
                        self.unify(gt, self.types.bool_id, guard.span);
                    }
                    let arm_type = self.infer_expr(&arm.body);
                    if self.resolve_type(arm_type) == self.types.never_id {
                        self.diverging_exprs.insert(arm.body.span);
                    }
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
            // Assume covered for the definite-return analysis (pre-expansion
            // we can't know; treating as covered avoids false positives).
            self.exhaustive_matches.insert(span);
            return;
        }

        // Resolve the scrutinee type and check if it's an enum. Peel `Ref`
        // wrappers: collection reads (`v.get(i)`, `v[i]`) and borrowed
        // params produce element REFERENCES (CoW zero-cost reads), and a
        // match on a reference matches the referent's variants.
        let mut resolved = self.resolve_type(scrutinee_type);
        let mut peel_depth = 0;
        while let ResolvedType::Ref(inner) = self.types.get(resolved) {
            resolved = self.resolve_type(*inner);
            peel_depth += 1;
            if peel_depth > 8 {
                break;
            }
        }
        let enum_def_id = match self.types.get(resolved) {
            ResolvedType::Defined(def_id) => *def_id,
            ResolvedType::Generic(def_id, _) => *def_id,
            _ => {
                // Non-enum scrutinee (int, String, bool, tuple, …):
                // exhaustiveness is not enforced, but an unguarded catch-all
                // arm still covers every value — record it for the
                // definite-return analysis. A bool scrutinee is also covered
                // by unguarded literal `true` + `false` arms.
                if arms.iter().filter_map(|i| i.arm()).any(|arm| {
                    arm.guard.is_none() && pattern_is_catchall_syntactic(&arm.pattern.node)
                }) || (resolved == self.types.bool_id && bool_arms_cover(arms)) {
                    self.exhaustive_matches.insert(span);
                }
                return;
            }
        };
        if self.scopes.get_def(enum_def_id).kind != DefKind::Enum {
            if arms.iter().filter_map(|i| i.arm()).any(|arm| {
                arm.guard.is_none() && pattern_is_catchall_syntactic(&arm.pattern.node)
            }) {
                self.exhaustive_matches.insert(span);
            }
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
                self.exhaustive_matches.insert(span);
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
        } else {
            self.exhaustive_matches.insert(span);
        }
    }

    // ── Definite-return analysis ─────────────────────────────────────────
    //
    // A syntactic "terminating statement" analysis in the spirit of the Go
    // spec (§ Terminating statements) / Java's "can complete normally"
    // (JLS 14.21): a non-void function body must not be able to fall off
    // its end. Runs after `check_block` in `check_function`, reading only
    // facts already computed during checking (`diverging_exprs` for
    // Never-typed calls, `exhaustive_matches` for else-less matches).
    //
    // Deliberately syntactic-conservative: condition VALUES are not
    // evaluated, so a dead `else: break` under `while true` defeats the
    // infinite-loop rule and rejects — matching Go's terminating-statement
    // rule and the existing dead-`if false: break` behavior.
    //
    // Known false-NEGATIVE-only limitation: meta constructs (`meta if` /
    // `meta for` / `meta match` / `meta while`, and match arms behind
    // `MetaFor`) expand at monomorphization and are assumed terminating
    // pre-expansion — a fall-off inside an expanded body is not caught
    // here. Never a false positive.

    /// Does control definitely NOT fall through this block to whatever
    /// follows it? True when ANY statement in the list terminates (control
    /// never proceeds past a terminating statement, so later statements are
    /// unreachable and irrelevant).
    fn block_terminates(&self, block: &Block) -> bool {
        block.stmts.iter().any(|s| self.stmt_terminates(s))
    }

    /// Terminating-statement classification. Conservative: `false` means
    /// "control may proceed past this statement".
    fn stmt_terminates(&self, stmt: &Spanned<Stmt>) -> bool {
        match &stmt.node {
            // Direct control transfer out of the fall-through path.
            Stmt::Return(_) | Stmt::Throw(_) | Stmt::Break | Stmt::Continue => true,

            // Never-typed expression statements (panic, noreturn externs),
            // or block expressions whose block terminates.
            Stmt::Expr(e) => self.expr_diverges(e),

            // if: requires an else, and every branch must terminate.
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                else_body.as_ref().is_some_and(|eb| {
                    self.block_terminates(then_body)
                        && elif_branches.iter().all(|(_, b)| self.block_terminates(b))
                        && self.block_terminates(eb)
                })
            }

            // match: every arm must diverge, and the value space must be
            // covered (an else arm that terminates, or a match recorded
            // exhaustive by `check_match_exhaustiveness`).
            Stmt::Match { arms, else_arm, .. } => {
                let arms_terminate = arms.iter().all(|item| match item {
                    MatchItem::Arm(arm) => self.expr_diverges(&arm.body),
                    // Pre-expansion meta arms: assume terminating (checked
                    // only post-monomorphization; avoids false positives).
                    MatchItem::MetaFor { .. } => true,
                });
                arms_terminate
                    && match else_arm {
                        Some(eb) => self.block_terminates(eb),
                        None => self.exhaustive_matches.contains(&stmt.span),
                    }
            }

            // loop: terminates iff no break can exit it (either it loops
            // forever or exits via return/throw).
            Stmt::Loop { body } => !block_has_loop_break(body),

            // while: a literal-`true` condition with no break is an
            // infinite loop (Java-style constant-condition rule). A
            // conditional while with a loop-else and no break always runs
            // the else on exit, so a terminating else terminates the whole
            // statement. Any break defeats both.
            Stmt::While { condition, body, else_body } => {
                if block_has_loop_break(body) {
                    return false;
                }
                if matches!(condition.node, Expr::BoolLiteral(true)) {
                    return true;
                }
                else_body.as_ref().is_some_and(|eb| self.block_terminates(eb))
            }

            // for: may iterate zero times, so only a terminating loop-else
            // (with no break) terminates the statement.
            Stmt::For { body, else_body, .. } => {
                !block_has_loop_break(body)
                    && else_body.as_ref().is_some_and(|eb| self.block_terminates(eb))
            }

            // Transparent block wrappers.
            Stmt::With { body, .. }
            | Stmt::NamedScope { body, .. } => self.block_terminates(body),

            // select: waits until some arm fires; if every arm body
            // terminates (and the else, when present, does too), control
            // never falls through.
            Stmt::Select { arms, else_arm } => {
                arms.iter().all(|a| self.block_terminates(&a.body))
                    && else_arm.as_ref().map_or(true, |eb| self.block_terminates(eb))
            }

            // Compile-time conditionals expand at monomorphization — assume
            // terminating pre-expansion (avoids false positives; the
            // expanded body is not re-typechecked).
            Stmt::MetaIf { .. }
            | Stmt::MetaFor { .. }
            | Stmt::MetaMatch { .. }
            | Stmt::MetaWhile { .. } => true,

            // Everything else falls through.
            _ => false,
        }
    }

    /// Does this expression (statement-position or match-arm body) diverge?
    fn expr_diverges(&self, expr: &Spanned<Expr>) -> bool {
        if self.diverging_exprs.contains(&expr.span) {
            return true; // typed Never during inference (panic / noreturn)
        }
        match &expr.node {
            Expr::Block(b) => self.block_terminates(b),
            Expr::Do { body, .. } => self.block_terminates(body),
            _ => false,
        }
    }

    /// The definite-return check for a non-void block-bodied function.
    fn check_definite_return(&mut self, func: &FunctionDef, block: &Block, return_type: TypeId) {
        // SYNTACTIC void gate: a generic return type (`T`) may not resolve
        // at decl time (generic bodies are fully typed only at
        // monomorphization), but a non-void-declared function still must
        // return on every path.
        if matches!(
            func.return_type.node,
            Type::Primitive(PrimitiveType::Void)
        ) {
            return;
        }
        if self.block_terminates(block) {
            return;
        }
        let rt = self.resolve_type(return_type);
        let return_type_name = if rt == self.types.error_id || rt == self.types.void_id {
            // Unresolved at decl time (generic param, undefined name):
            // fall back to the AST spelling.
            ast_type_to_gorget_name(&func.return_type.node)
                .unwrap_or_else(|| "a value".to_string())
        } else {
            self.describe_resolved_type(rt)
        };
        self.error(
            SemanticErrorKind::MissingReturn {
                function: func.name.node.clone(),
                return_type: return_type_name,
            },
            func.name.span,
        );
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
            Pattern::Constructor { path, fields, .. } => {
                let variant_name = path.last().map(|s| s.node.as_str()).unwrap_or("");
                let field_types = self.resolve_variant_field_types(scrutinee_type, variant_name);
                // Arity gate: when we know the payload/field list, wrong arity is a type error
                // (was silent — struct patterns then mis-bound via enum tag offset).
                if !field_types.is_empty() && field_types.len() != fields.len() {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: format!(
                                "pattern with {} field(s) for `{variant_name}`",
                                field_types.len()
                            ),
                            found: format!("{} field(s)", fields.len()),
                        },
                        pattern.span,
                    );
                }
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
            Pattern::DotShorthand { variant, fields, .. } => {
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
    /// Also handles **struct** constructor patterns (`case Point(x, y):`) by
    /// matching the type name to `variant_name` and returning positional field
    /// types (with arity left to the caller via the returned vec length).
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
                // Struct constructor pattern: `match p: case Point(x, y):`
                // when scrutinee is a struct named Point. Field TypeIds live on
                // DefInfo.field_types (populated in populate_def_field_types);
                // StructFieldInfo.fields is only (name, span).
                if let Some(sfi) = self.struct_fields.get(&def_id) {
                    let def_name = self.scopes.get_def(def_id).name.clone();
                    if def_name == variant_name {
                        if let Some(field_tids) = &self.scopes.get_def(def_id).field_types {
                            if field_tids.len() == sfi.fields.len() {
                                return field_tids.clone();
                            }
                        }
                        // Fallback: resolve field AST types at pattern site.
                        return sfi
                            .field_ast_types
                            .iter()
                            .filter_map(|ast_ty| {
                                super::types::ast_type_to_resolved(
                                    &ast_ty.node,
                                    ast_ty.span,
                                    self.scopes,
                                    self.types,
                                )
                                .ok()
                            })
                            .collect();
                    }
                }
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
                // D29 kind-2 bare-discard: an expression statement whose value
                // is an un-consumed fallible `Result` silently drops the Error
                // case — exactly what D29 kills. A NON-tail statement's value is
                // always dropped; the TAIL is dropped too when the enclosing
                // context discards the block value (`tail_value_dropped`: a
                // function block body or a loop body — R4's two shapes) and
                // consumed when the block is an expression (`Expr::Block`/`Do`,
                // where the normal unification governs). (Kind-1 unmarked calls
                // already errored at the call site and type as `error`, not
                // `Result`, so they never reach here.)
                if i != last_idx || self.tail_value_dropped {
                    if let Stmt::Expr(expr) = &stmt.node {
                        self.check_bare_fallible_discard(expr, ty);
                    }
                }
            } else if i == last_idx {
                last_type = self.infer_stmt_tail_type(&stmt.node);
            }
        }
        last_type
    }

    /// D29: flag a bare (unmarked) fallible CALL used as a discarded expression
    /// statement — `parse(s)` on its own line drops the `Result` outcome. Mark
    /// it (`parse(s)!`) to propagate, or handle it. A `Propagate`-wrapped call
    /// has already activated its channel (types as `T` or errored), and a plain
    /// value / local is not a fallible call, so neither reaches this branch.
    fn check_bare_fallible_discard(&mut self, expr: &Spanned<Expr>, ty: TypeId) {
        if !matches!(&expr.node, Expr::Call { .. } | Expr::MethodCall { .. }) {
            return;
        }
        let resolved = self.resolve_type(ty);
        if let ResolvedType::Generic(def_id, args) = self.types.get(resolved).clone() {
            if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" {
                self.emit_missing_fallible_mark(args[1], FallibleMarkReason::Bare, expr.span);
            }
        }
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
    /// Chain C item 6: reject `s[i] = x` / `s[i] += x` (incl. range index)
    /// when the indexed OBJECT is a String. `s[i]` is a documented
    /// read-only codepoint view (language-reference SStrings); the
    /// lowering has no String index-setter, so these compiled as SILENT
    /// NO-OPS. Vector/Dict/user types with setters are untouched — the
    /// gate keys on the object's String-ness only. The object is inferred
    /// here (before the normal target inference); for an index-assign
    /// target that is itself erroneous this can re-report an inner error,
    /// which is acceptable at a hard-error site.
    /// Reject a non-lvalue assignment / compound-assignment target at CHECK time
    /// (Core #10 lower-or-reject). The parser accepts any expression as a target,
    /// and the lowerer only handles the assignable PLACE forms — a variable,
    /// field, tuple field, index, or dereference. Anything else (`5 += 1`,
    /// `foo() += 1`, `(a + b) = x`) formerly reached lowering and SILENTLY
    /// DROPPED the write (plain `=`) or ICE'd (compound `OP=`). Rejecting here
    /// makes the lowerer's catch-all genuinely unreachable for accepted code.
    /// Allowlist = exactly the lvalue shapes `lower_assign` /
    /// `lower_compound_assign` dispatch on.
    fn check_assign_target_lvalue(&mut self, target: &Spanned<Expr>) {
        let is_lvalue = matches!(
            &target.node,
            Expr::Identifier(_)
                | Expr::FieldAccess { .. }
                | Expr::TupleFieldAccess { .. }
                | Expr::Index { .. }
                | Expr::Deref { .. }
        );
        if !is_lvalue {
            self.error(SemanticErrorKind::InvalidAssignTarget, target.span);
        }
    }

    fn check_string_index_assign(&mut self, target: &Spanned<Expr>) {
        if let Expr::Index { object, .. } = &target.node {
            let obj_type = self.infer_expr(object);
            let resolved = self.resolve_type(obj_type);
            if matches!(
                self.types.get(resolved),
                ResolvedType::Primitive(PrimitiveType::StringType)
            ) {
                self.error(SemanticErrorKind::StringIndexAssign, target.span);
            }
        }
    }

    /// Round XXIX Track A: an assign-target `x[k] = v` requires the
    /// receiver to implement `IndexMut[K,V]`. Fires **only** when `Index`
    /// is present but `IndexMut` is missing (read-only user impls like
    /// `equip Grid with Index[int, int]`) so we get the more specific
    /// `E_NotIndexableMut` message. A receiver that lacks `Index`
    /// altogether (`Pair`, `Set[int]`, `Tuple`, plain enum, primitive
    /// `int`) is handled by the read-side `E_NotIndexable` reject in the
    /// `Expr::Index` arm — that message is more actionable.
    ///
    /// String is skipped here — `check_string_index_assign` already
    /// emits the more specific `E_StringIndexAssign`.
    fn check_index_mut_assign(&mut self, target: &Spanned<Expr>) {
        let Expr::Index { object, .. } = &target.node else {
            return;
        };
        let obj_type = self.infer_expr(object);
        let resolved = self.resolve_type(obj_type);
        // Silence cascade — matches the `Expr::Index` arm's suppression.
        if matches!(
            self.types.get(resolved),
            ResolvedType::Var(_) | ResolvedType::Error | ResolvedType::Never
        ) {
            return;
        }
        let Some(type_name) = self.type_key_for_trait_lookup(resolved) else {
            return; // read-side NotIndexable will fire on anonymous type
        };
        if type_name == "String" {
            return; // E_StringIndexAssign already covers this
        }
        // Only surface NotIndexableMut when the read side accepts (Index
        // present) but the write side does not (IndexMut missing).
        if self.traits.has_trait_impl_by_name(&type_name, "Index")
            && !self.traits.has_trait_impl_by_name(&type_name, "IndexMut")
        {
            self.error(
                SemanticErrorKind::NotIndexableMut { type_: type_name },
                target.span,
            );
        }
    }

    /// Round XXIX Track A: resolve `(K, V)` from a user `equip T with
    /// Index[K, V]` impl. Concrete impls (`equip Grid with Index[int,
    /// int]`) resolve exactly; a generic impl (`equip [T] S[T] with
    /// Index[int, T]`) resolves the free parameter to `error_id` (empty
    /// substitution), matching the pre-fix behavior for user impls
    /// (error_id unifies with anything — safe, not more permissive).
    /// Full impl-generic substitution is orthogonal to the safety fix
    /// and lives in the method-dispatch path
    /// (`substitute_default_method_sig` at :7199+).
    fn user_index_key_value(&mut self, type_name: &str, span: Span) -> Option<(TypeId, TypeId)> {
        let args: Vec<Type> = self
            .traits
            .trait_generic_args_by_name(type_name, "Index")
            .to_vec();
        if args.len() < 2 {
            return None;
        }
        let subst: FxHashMap<String, TypeId> = FxHashMap::default();
        let k_tid = self.resolve_ast_type_with_subst(&args[0], span, &subst);
        let v_tid = self.resolve_ast_type_with_subst(&args[1], span, &subst);
        Some((k_tid, v_tid))
    }

    /// Bare def / primitive name used by the trait registry's
    /// `impls_by_name` / `has_trait_impl_by_name` keys. Peels Ref/Owned so a
    /// borrowed `Money` still resolves to `"Money"`. Uses the def name alone
    /// for generics (`Vector`, not `Vector[int]`) — full describe strings
    /// miss the registry and would false-reject every overloaded generic.
    fn type_key_for_trait_lookup(&self, type_id: TypeId) -> Option<String> {
        let type_id = self.resolve_type(type_id);
        let type_id = match self.types.get(type_id) {
            ResolvedType::Ref(inner) | ResolvedType::Owned(inner) => self.resolve_type(*inner),
            _ => type_id,
        };
        match self.types.get(type_id) {
            ResolvedType::Primitive(p) => Some(describe_primitive(p)),
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                Some(self.scopes.get_def(*def_id).name.clone())
            }
            // Fixed-size arrays participate in binary `+` concat (LIR
            // CollectionKind::Array) — key them as a synthetic "Array" so the
            // binary-Add special case below can match without a full describe.
            ResolvedType::Array(_, _) => Some("Array".to_string()),
            // Error / Never / unbound Vars: don't gate (cascade / inference).
            ResolvedType::Error | ResolvedType::Never | ResolvedType::Var(_) => None,
            _ => Some(self.describe_resolved_type(type_id)),
        }
    }

    /// Trait name + method name for an overloadable arithmetic op, if any.
    /// Wrap / bitwise ops have no trait equip path (integer-only builtins).
    fn op_trait_and_method(op: BinaryOp) -> Option<(&'static str, &'static str)> {
        match op {
            BinaryOp::Add => Some(("Add", "add")),
            BinaryOp::Sub => Some(("Sub", "sub")),
            BinaryOp::Mul => Some(("Mul", "mul")),
            BinaryOp::Div => Some(("Div", "div")),
            BinaryOp::Rem => Some(("Rem", "rem")),
            BinaryOp::Mod => Some(("Mod", "mod")),
            _ => None,
        }
    }

    /// Spelling of `op` for diagnostics — compound forms use `+=` etc.
    fn op_display(op: BinaryOp, compound: bool) -> &'static str {
        if compound {
            match op {
                BinaryOp::Add => "+=",
                BinaryOp::Sub => "-=",
                BinaryOp::Mul => "*=",
                BinaryOp::Pow => "**=",
                BinaryOp::Div => "/=",
                BinaryOp::Rem => "%=",
                BinaryOp::AddWrap => "+%=",
                BinaryOp::SubWrap => "-%=",
                BinaryOp::MulWrap => "*%=",
                BinaryOp::BitAnd => "&=",
                BinaryOp::BitOr => "|=",
                BinaryOp::BitXor => "^=",
                BinaryOp::Shl => "<<=",
                BinaryOp::Shr => ">>=",
                // No compound form for Mod / comparisons / logicals.
                BinaryOp::Mod => "mod=",
                BinaryOp::Eq => "==",
                BinaryOp::Neq => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Gt => ">",
                BinaryOp::LtEq => "<=",
                BinaryOp::GtEq => ">=",
                BinaryOp::And => "and",
                BinaryOp::Or => "or",
                BinaryOp::In => "in",
                // D26 compound-fallible-assign forms are v1-EXCLUDED, but the
                // diagnostic spelling still names the compound glyph so
                // op_display remains total (no `_` fall-through per Core #10).
                BinaryOp::AddFallible => "+!=",
                BinaryOp::SubFallible => "-!=",
                BinaryOp::MulFallible => "*!=",
                BinaryOp::DivFallible => "/!=",
                BinaryOp::RemFallible => "%!=",
                BinaryOp::ShlFallible => "<<!=",
                BinaryOp::ShrFallible => ">>!=",
            }
        } else {
            match op {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Pow => "**",
                BinaryOp::Div => "/",
                BinaryOp::Rem => "%",
                BinaryOp::Mod => "mod",
                BinaryOp::AddWrap => "+%",
                BinaryOp::SubWrap => "-%",
                BinaryOp::MulWrap => "*%",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitOr => "|",
                BinaryOp::BitXor => "^",
                BinaryOp::Shl => "<<",
                BinaryOp::Shr => ">>",
                BinaryOp::Eq => "==",
                BinaryOp::Neq => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Gt => ">",
                BinaryOp::LtEq => "<=",
                BinaryOp::GtEq => ">=",
                BinaryOp::And => "and",
                BinaryOp::Or => "or",
                BinaryOp::In => "in",
                // D26 fallible arithmetic — the `!` suffix IS the fallible mark.
                BinaryOp::AddFallible => "+!",
                BinaryOp::SubFallible => "-!",
                BinaryOp::MulFallible => "*!",
                BinaryOp::DivFallible => "/!",
                BinaryOp::RemFallible => "%!",
                BinaryOp::ShlFallible => "<<!",
                BinaryOp::ShrFallible => ">>!",
            }
        }
    }

    /// Whether `op` is supported on `ty` (numeric / String concat / Vector
    /// binary+compound concat / trait equip / inherent method). `compound`
    /// is reserved for future op-specific distinctions; Array-family `+` /
    /// `+=` share the same support (book desugar: `+=` ≡ `x = x + rhs`).
    fn operator_supported_for_type(&self, ty: TypeId, op: BinaryOp, compound: bool) -> bool {
        // `compound` currently unused for the Array-family path (binary and
        // compound Add are both allowed). Kept on the signature so call sites
        // stay uniform and future op-specific gates can re-use it without a
        // signature churn. Silence unused-param warning without renaming.
        let _ = compound;
        let Some(type_key) = self.type_key_for_trait_lookup(ty) else {
            // Error/Never/Var — don't cascade.
            return true;
        };

        // Wrap / bitwise / shifts: integer numeric only (not float, not traits).
        let integer_only = matches!(
            op,
            BinaryOp::AddWrap
                | BinaryOp::SubWrap
                | BinaryOp::MulWrap
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr
        );
        if integer_only {
            return matches!(
                type_key.as_str(),
                "int" | "int8" | "int16" | "int32" | "int64"
                    | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
            );
        }

        // Arithmetic family.
        if let Some((trait_name, method)) = Self::op_trait_and_method(op) {
            // Numeric primitives intrinsically satisfy Add/Sub/… via the
            // trait registry (has_trait_impl_by_name numeric intrinsic).
            if self.traits.has_trait_impl_by_name(&type_key, trait_name) {
                return true;
            }
            // Inherent equip method without the trait (rare but allowed).
            if self.traits.has_method_for_type(&type_key, method) {
                return true;
            }
            // Builtin String concatenation — only `+` / `+=`. String is NOT
            // intrinsically Add (concat is a special-case, not a trait equip).
            if matches!(op, BinaryOp::Add) && type_key == "String" {
                return true;
            }
            // Builtin Vector/Deque/Array `+` / `+=` concat (LIR
            // CollectionKind::Array — clone lhs then extend with rhs).
            // Compound `v += w` desugars to the same bin_op rebind as
            // `v = v + w` (see assigns.rs Identifier + place-RMW arms).
            // Non-Add ops (Sub/Mul/…) stay rejected — no Array-family
            // overload for those.
            if matches!(op, BinaryOp::Add)
                && matches!(type_key.as_str(), "Vector" | "Deque" | "Array")
            {
                return true;
            }
            return false;
        }

        // Non-arithmetic ops gated elsewhere (comparisons / logicals / `in`).
        true
    }

    /// Emit `E_UnsupportedOperator` when `op` is not defined for `ty`.
    fn check_operator_supported(
        &mut self,
        ty: TypeId,
        op: BinaryOp,
        compound: bool,
        span: Span,
    ) {
        if self.operator_supported_for_type(ty, op, compound) {
            return;
        }
        let type_name = self
            .type_key_for_trait_lookup(ty)
            .unwrap_or_else(|| self.describe_resolved_type(ty));
        // Prefer the human-facing describe for non-generic display when the
        // bare key is a primitive/user def (same string); for Vector keep
        // the bare key so the message says `Vector` not `Vector[int]`.
        let display_name = match self.types.get(self.resolve_type(ty)) {
            ResolvedType::Generic(_, _) if type_name == "Vector" || type_name == "Deque" => {
                type_name.clone()
            }
            ResolvedType::Primitive(_) | ResolvedType::Defined(_) | ResolvedType::Generic(_, _) => {
                // describe_resolved_type for Defined/Primitive matches the key;
                // for other generics (e.g. user `Box[T]`) show full form.
                if matches!(type_name.as_str(), "Vector" | "Deque" | "Array") {
                    type_name.clone()
                } else {
                    // For user generics like Maybe[int], show full describe;
                    // bare key is still what trait lookup used.
                    let full = self.describe_resolved_type(ty);
                    if full.starts_with(&type_name) {
                        // Prefer bare def for equip guidance (`Money`, not a
                        // longer form). Full form only when it differs usefully.
                        type_name.clone()
                    } else {
                        type_name.clone()
                    }
                }
            }
            _ => type_name.clone(),
        };
        self.error(
            SemanticErrorKind::UnsupportedOperator {
                op: Self::op_display(op, compound).to_string(),
                type_name: display_name,
            },
            span,
        );
    }

    /// D26 (Round XXXIII Batch C1): type-rule for the seven fallible arithmetic
    /// operators (`+!` / `-!` / `*!` / `/!` / `%!` / `<<!` / `>>!`). Integer-only
    /// in v1; result is `Result[T, ArithError]` with D29-auto-propagation.
    ///
    /// The `!` glyph on the operator itself counts as the fallible mark (`marked=true`
    /// on `resolve_throws_call_type`), mirroring `f()!`'s postfix mark — so the same
    /// disposition table decides bare / capture / propagate / unhandled.
    ///
    /// Non-integer operands reject with `E_FallibleArithmeticOnNonInt` (unifies
    /// float/String/user-type operand cases under one diagnostic). Both operands
    /// must be integer; a mismatched-kind pair inherits the caller `unify`'s
    /// diagnostic separately.
    fn check_fallible_arith_binop(
        &mut self,
        left_type: TypeId,
        right_type: TypeId,
        op: BinaryOp,
        span: Span,
    ) -> TypeId {
        // Unify the two operand types first (both must be the same integer type).
        let operand_type = self.unify(left_type, right_type, span);
        let resolved_operand = self.resolve_type(operand_type);
        // Reject non-integer operands (float, String, user-defined types).
        let is_int = matches!(
            self.types.get(resolved_operand),
            ResolvedType::Primitive(p) if is_integer_type(p)
        );
        if !is_int
            // Don't cascade if unify already produced Error (upstream diagnostic).
            && !matches!(self.types.get(resolved_operand), ResolvedType::Error)
        {
            self.error(
                SemanticErrorKind::FallibleArithmeticOnNonInt {
                    op: Self::op_display(op, /*compound=*/ false).to_string(),
                    found: self.describe_resolved_type(resolved_operand),
                },
                span,
            );
            return self.types.error_id;
        }
        // Look up ArithError. Prelude-registered at `resolve.rs`; if lookup
        // fails (test with prelude disabled), fall back to operand_type so we
        // don't cascade error IDs. Uses the interned `defined_id` helper so
        // repeated lookups return the same TypeId — critical for the
        // `auto_prop_error_gate`'s TypeId-equality fast-path (a fresh
        // `types.insert` each call would false-positive `E_UnconvertibleErrorPropagation`
        // when propagating `ArithError` into a caller declared `throws ArithError`).
        let err_ty = match self.scopes.lookup("ArithError") {
            Some(def_id) => self.types.defined_id(def_id),
            None => return operand_type,
        };
        // D26 shift-fallible Route B guard (Core #10, Core #8): the check-lane
        // types `<<!` / `>>!` as `Result[T, ArithError]` uniformly, but the
        // lowering's `lower_fallible_arith_binop` only handles the 5 arith
        // fallible ops — shift-fallible falls through to the plain `Shl`/`Shr`
        // trap-on-oob path (an int result). At a Route-B (Result-capture)
        // destination that mismatch is a silent type-confusion miscompile:
        // the emitted C stores an int in a `Result[int, ArithError]` slot,
        // the caller's match reads garbage bytes as the discriminant, and
        // the program SIGSEGVs. Reject here at check-time until the shift-
        // fallible lowering follow-up lands. Route A (throws-propagate /
        // auto-infer) still works — the base-op trap-on-oob path in a
        // throws context is a sound minimum for now (pins the same shape the
        // `c1_d26_shift_shl_ok`/`c1_d26_shift_shr_ok` fixtures exercise).
        let is_shift_fallible = matches!(op, BinaryOp::ShlFallible | BinaryOp::ShrFallible);
        let dest_is_result_capture = self
            .decl_type_hint
            .map_or(false, |h| self.type_is_result(h));
        if is_shift_fallible && dest_is_result_capture {
            self.error(
                SemanticErrorKind::ShiftFallibleRouteBNotYetImplemented {
                    op: Self::op_display(op, /*compound=*/ false).to_string(),
                },
                span,
            );
            return self.types.error_id;
        }
        // Route through D29 disposition table. The `!` glyph IS the mark, and
        // for D26 fallible-arith it is INHERENT to the operator (no un-marked
        // variant exists) — so the capture-position redundant-mark reject is
        // skipped (`Result[int, ArithError] r = a +! b` is legal).
        self.resolve_throws_call_type(
            operand_type,
            err_ty,
            /*suppress_auto_prop=*/ false,
            /*marked=*/ true,
            /*mark_is_operator_inherent=*/ true,
            span,
        )
    }

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

    /// Find the parent enum DefId for a given variant DefId. Scans the
    /// `enum_variants` side table; returns `None` for non-variant DefIds.
    /// Path A from the Snag #36 mixed-arm discussion: variant constructors
    /// (`Ok(x)`, `Some(x)`, `Color.Red()`, etc.) should type as the parent
    /// enum's instantiated `Result[T, E]` / `Option[T]` / `Color` — not as
    /// `Defined(variant_def_id)` (the variant's own DefId) or `error_id`.
    fn find_variant_parent_enum(&self, variant_def_id: DefId) -> Option<(DefId, EnumVariantInfo)> {
        self.enum_variants
            .iter()
            .find(|(_, info)| info.variants.iter().any(|(_, vid)| *vid == variant_def_id))
            .map(|(eid, info)| (*eid, info.clone()))
    }

    /// Locate the intended `DefKind::Variant` DefId for a bare identifier that
    /// the resolver left unresolved because the loader's pre-merge variant
    /// qualifier (see `build_variant_map_from_all` in `src/loader.rs`) dropped
    /// it as ambiguous across multiple enums.
    ///
    /// Disambiguation strategy, in order:
    ///
    ///   1. If `decl_type_hint` resolves to an enum DefId, pick the variant of
    ///      that enum whose name matches — this is the constructor-call
    ///      expected-type path, the analogue of the pattern path's
    ///      `ctx.type_registry.type_name(scrut_type)`.
    ///   2. Otherwise (no hint, or hint isn't an enum), fall back to the
    ///      single matching variant when only one enum carries this name.
    ///      Returns `None` if multiple enums match and no hint disambiguates
    ///      — the caller silently propagates `error_id` so subsequent
    ///      unification flags the genuine type mismatch.
    fn find_ambiguous_variant_by_name(&self, name: &str) -> Option<DefId> {
        // Collect all (parent_enum_def_id, variant_def_id) sightings.
        let candidates: Vec<(DefId, DefId)> = self
            .enum_variants
            .iter()
            .flat_map(|(eid, info)| {
                info.variants
                    .iter()
                    .filter(|(vn, _)| vn == name)
                    .map(move |(_, vid)| (*eid, *vid))
            })
            .collect();
        if candidates.is_empty() {
            return None;
        }
        // Step 1: try expected-type-driven disambiguation.
        if let Some(hint) = self.decl_type_hint {
            let resolved = self.resolve_type(hint);
            let hint_enum = match self.types.get(resolved) {
                ResolvedType::Defined(eid) | ResolvedType::Generic(eid, _) => Some(*eid),
                _ => None,
            };
            if let Some(hint_eid) = hint_enum {
                for (eid, vid) in &candidates {
                    if *eid == hint_eid {
                        return Some(*vid);
                    }
                }
            }
        }
        // Step 2: only return a candidate when exactly one enum defines this
        // variant — anything else without a hint is genuinely ambiguous and
        // we let the typechecker produce a downstream mismatch rather than
        // guessing.
        if candidates.len() == 1 {
            return Some(candidates[0].1);
        }
        None
    }

    /// Infer the type of a variant constructor call. Returns the parent
    /// enum's `Generic(parent_def_id, [type_args])` (or `Defined(parent)` for
    /// non-generic user enums) with type-args bound from the resolved
    /// argument types and `expected_type`; unbound positions become fresh
    /// type vars so subsequent unification can pin them.
    ///
    /// Handles ALL variant constructors uniformly:
    ///   - Prelude variants `Ok`/`Error`/`Some`/`None` (parent built-in
    ///     enums whose `EnumVariantInfo` carries empty `variant_field_types`
    ///     and `generic_param_names` — params/fields are hardcoded here).
    ///   - User-defined enums (generic and non-generic) using
    ///     `EnumVariantInfo`'s recorded AST field types.
    ///
    /// Crucially, args are inferred *inside* this helper with the
    /// per-arg `decl_type_hint` set to the variant's resolved field
    /// type. Nested variant calls (`Some(Some(100))` against destination
    /// `Option[Option[int]]`) need that hint to recurse correctly — the
    /// inner `Some` must see `Option[int]` as its expected type, not the
    /// outer `Option[Option[int]]`. Mirrors the IR-lowering's
    /// per-arg expected_type override at `methods.rs:201-221`.
    ///
    /// Replaces the previous behaviour where variant calls returned
    /// `Defined(variant_def_id)` or `error_id`, both of which silently
    /// absorbed mismatches via `unify`'s error-type absorption.
    fn infer_variant_constructor(
        &mut self,
        variant_def_id: DefId,
        args: &[Spanned<CallArg>],
        expected_type: Option<TypeId>,
        call_span: Span,
    ) -> TypeId {
        let parent = self.find_variant_parent_enum(variant_def_id);
        let (parent_enum_def_id, info) = match parent {
            Some(x) => x,
            None => {
                // Unknown variant: infer args without hint and fall back.
                for arg in args {
                    self.infer_expr(&arg.node.value);
                }
                return self.types.defined_id(variant_def_id);
            }
        };

        let parent_name = self.scopes.get_def(parent_enum_def_id).name.clone();
        let variant_name = info
            .variants
            .iter()
            .find(|(_, vid)| *vid == variant_def_id)
            .map(|(n, _)| n.clone())
            .unwrap_or_default();

        // Determine parent's generic param names (hardcode for built-ins;
        // user enums get them from `EnumVariantInfo`).
        let generic_params: Vec<String> = match parent_name.as_str() {
            "Option" => vec!["T".to_string()],
            "Result" => vec!["T".to_string(), "E".to_string()],
            _ => info.generic_param_names.clone(),
        };

        // Non-generic user enum: `Color.Red()` types as `Defined(Color)`.
        // Still infer args (so side-effects/usage of inner expressions are
        // walked) AND unify each arg's inferred type against the variant's
        // declared field type — the symmetric counterpart of the
        // substitution-driven path below, minus the substitution step.
        // Skipping this check was gorget-js snag #8: a typechecker hole
        // that silently accepted `Outer.StrV(int_x)` where the variant
        // expected `String`, then handed garbage bytes to the runtime.
        if generic_params.is_empty() {
            let field_ast_types = info
                .variant_field_types
                .iter()
                .find(|(n, _)| *n == variant_name)
                .map(|(_, ts)| ts.clone())
                .unwrap_or_default();
            let empty_subst: FxHashMap<String, TypeId> = FxHashMap::default();
            let field_type_ids: Vec<TypeId> = field_ast_types
                .iter()
                .map(|ast_field| {
                    self.resolve_ast_type_with_subst(
                        &ast_field.node,
                        ast_field.span,
                        &empty_subst,
                    )
                })
                .collect();
            let prev_hint = self.decl_type_hint;
            for (i, arg) in args.iter().enumerate() {
                let field_tid = field_type_ids.get(i).copied();
                self.decl_type_hint = field_tid;
                let arg_ty = self.infer_expr(&arg.node.value);
                self.decl_type_hint = prev_hint;
                if let Some(ftid) = field_tid {
                    // Snag #46 ↔ #8 reconciliation: a `throws`-fn call in
                    // constructor-arg position returns `Result[T, E]`, but
                    // the enclosing throws-context can auto-propagate the
                    // error and unwrap to `T` (this is the language-level
                    // throws-sugar that snag #46 originally fixed at the
                    // IR-lowering boundary). The snag #8 strict check
                    // (this branch) must respect the same carve-out the
                    // call-arg / var-decl / return sites already use —
                    // otherwise it false-positives at every auto-prop
                    // constructor-arg site. Mirror the existing pattern
                    // at line ~1361: skip `unify` when auto-prop or
                    // Result-capture is satisfied.
                    if !self.is_collection_assignment(ftid, arg_ty)
                        && !self.auto_prop_skips_unify(ftid, arg_ty, arg.span)
                        && !self.is_result_capture_compatible(ftid, arg_ty)
                    {
                        self.unify(ftid, arg_ty, arg.span);
                    }
                }
            }
            let _ = call_span;
            return self.types.defined_id(parent_enum_def_id);
        }

        // Seed bindings: param_index → optional TypeId. Pre-fill from
        // `expected_type` when it's `Generic(parent_enum, [...args])` —
        // this lets `Result[int, String] r = Ok(99)` bind `E=String`
        // before the arg-type refinement step.
        let mut bindings: Vec<Option<TypeId>> = vec![None; generic_params.len()];
        if let Some(et) = expected_type {
            let resolved = self.resolve_type(et);
            if let ResolvedType::Generic(eid, args_g) = self.types.get(resolved).clone() {
                if eid == parent_enum_def_id && args_g.len() == generic_params.len() {
                    for (i, &arg) in args_g.iter().enumerate() {
                        bindings[i] = Some(arg);
                    }
                }
            }
        }

        // Compute each variant field's expected TypeId — derived from
        // the current bindings — so args can be inferred with a precise
        // `decl_type_hint`. For built-in Option/Result the field type
        // *is* a parent generic param (Some(T), Ok(T), Error(E)); for
        // user enums it comes from the variant's AST field types under
        // the name→TypeId substitution.
        //
        // Promote any None binding to a fresh type-var first so nested
        // T positions share a single TypeId across all field-type
        // resolutions and across subsequent arg unification.
        for b in bindings.iter_mut() {
            if b.is_none() {
                *b = Some(self.fresh_type_var());
            }
        }
        let subst: FxHashMap<String, TypeId> = generic_params
            .iter()
            .zip(bindings.iter())
            .map(|(n, b)| (n.clone(), b.expect("populated above")))
            .collect();

        let field_type_ids: Vec<Option<TypeId>> = match parent_name.as_str() {
            "Option" => match variant_name.as_str() {
                "Some" => vec![Some(subst["T"])],
                _ => vec![],
            },
            "Result" => match variant_name.as_str() {
                "Ok" => vec![Some(subst["T"])],
                "Error" => vec![Some(subst["E"])],
                _ => vec![],
            },
            _ => {
                let field_ast_types = info
                    .variant_field_types
                    .iter()
                    .find(|(n, _)| *n == variant_name)
                    .map(|(_, ts)| ts.clone())
                    .unwrap_or_default();
                field_ast_types
                    .iter()
                    .map(|ast_field| {
                        Some(self.resolve_ast_type_with_subst(
                            &ast_field.node,
                            ast_field.span,
                            &subst,
                        ))
                    })
                    .collect()
            }
        };

        // Infer each arg with its expected field type as decl_type_hint,
        // then unify the resolved arg type with the field type so any
        // fresh type vars get bound. Mirrors the VarDecl carve-out for
        // collection-typed destinations: `Some([1, 2, 3])` against an
        // expected field type of `Vector[int]` shouldn't require
        // `int[3] == Vector[int]` (the IR-lowering handles the array→
        // Vector coercion at the construction site, same as for
        // `Vector[int] v = [1, 2, 3]`).
        let prev_hint = self.decl_type_hint;
        for (i, arg) in args.iter().enumerate() {
            let field_tid = field_type_ids.get(i).and_then(|f| *f);
            self.decl_type_hint = field_tid;
            let arg_ty = self.infer_expr(&arg.node.value);
            self.decl_type_hint = prev_hint;
            if let Some(ftid) = field_tid {
                // Snag #46 ↔ #8 reconciliation: see analog at the
                // non-generic branch above. A `throws`-fn call in a
                // generic variant's payload position
                // (`Some(throws_call())`, user-generic `Outer.A(call())`)
                // returns `Result[T, E]`; auto-prop in a propagating
                // context unwraps it to `T`. Skip `unify` when auto-prop
                // or Result-capture covers the apparent mismatch.
                if !self.is_collection_assignment(ftid, arg_ty)
                    && !self.auto_prop_skips_unify(ftid, arg_ty, arg.span)
                    && !self.is_result_capture_compatible(ftid, arg_ty)
                {
                    self.unify(ftid, arg_ty, arg.span);
                }
            }
        }
        // Span-anchor unification for the call as a whole — keeps
        // diagnostics pointing at the call expression itself when an
        // unrelated bound (e.g. expected_type pre-seed) contradicts the
        // computed shape.
        let _ = call_span;

        // Any remaining unbound position became a fresh type-var above;
        // expose the (possibly-bound-via-unify) TypeIds in the final
        // Generic.
        let final_args: Vec<TypeId> = bindings
            .into_iter()
            .map(|b| b.expect("populated above"))
            .collect();

        self.types.intern_generic(parent_enum_def_id, final_args)
    }

    /// Whether the current function can auto-propagate an error: it has
    /// `throws` OR returns `Result[T, E]`. This is the type-check mirror of
    /// IR-lowering's `should_auto_propagate` propagating-context check
    /// (`src/ir/lowering/exprs/mod.rs:2874-2882`). Factored out so the
    /// centralized throws-fn-call peel (the producer-side Result→T inversion)
    /// and the legacy `is_auto_propagation_compatible` consumer guards share
    /// one source of truth.
    /// D23 (throws totality): resolve the observable type of a `throws` call at
    /// a consuming position. A `throws` call is an expression of type `T` in
    /// EVERY position; its `Result[T, E]` desugar is never observable. This is
    /// the single PRODUCER chokepoint shared by the free-fn (`Expr::Call`) and
    /// method (`Expr::MethodCall`) paths — "fix the class, not the instance"
    /// (CLAUDE.md). Three disjoint outcomes:
    ///   - legit whole-`Result` positions (`Result[T,E]` capture per §10.3,
    ///     match-scrutinee-with-`Result`-arms, `catch`/`rethrow` inner) keep the
    ///     raw `Result[T, E]` — UNCHANGED;
    ///   - a propagating context (enclosing fn is `throws`/`Result`-returning)
    ///     peels to `T` (Route A), gating the callee-`E` against the caller-`E`;
    ///   - anywhere else the error is UNHANDLED → emit `E_UnhandledThrows` and
    ///     return `error_id`, which unifies with anything so the downstream
    ///     `unify` stays silent (collapses the 1-2-error cascade to ONE clean
    ///     diagnostic instead of leaking `found `Result[` / silently swallowing
    ///     / silently miscompiling).
    ///
    /// PRESERVES the `match self.scopes.lookup("Result") { … None => return_type }`
    /// fallback so a build where `Result` is out of scope does not regress.
    /// D29 kind-1 chokepoint (`throws E` callee). `marked` = the call carried a
    /// postfix `!` (an `Expr::Propagate` wrapper). Enforces the ratified /
    /// amended (`decisions.md` 2026-07-17) fallible-mark discipline:
    ///
    /// | position                            | unmarked          | marked                |
    /// |-------------------------------------|-------------------|-----------------------|
    /// | `catch`/`rethrow` inner (suppress)  | E_MissingFallibleMark | activate, keep Result |
    /// | explicit `Result[T,E]` dest (capture)| capture (legal)  | E_MissingFallibleMark (redundant) |
    /// | propagating context                 | E_MissingFallibleMark | peel to `T` (Route A) |
    /// | non-propagating, no disposition     | E_MissingFallibleMark | E_UnhandledThrows     |
    ///
    /// `mark_is_operator_inherent` (D26): when true, the mark is fused INTO the
    /// operator itself (`+! -! *!` etc — no un-marked variant of the operator
    /// exists), not an optional postfix. Skips the RedundantOnCapture reject at
    /// (2): `Result[int, ArithError] r = a +! b` is the canonical D26 capture
    /// spelling, since plain `+` produces `int`, not `Result[int, ArithError]`.
    /// Fallible-fn-calls pass `false` (the `!` is optional on capture); the
    /// D26 fallible-arith check-arm passes `true`.
    fn resolve_throws_call_type(
        &mut self,
        return_type: TypeId,
        err_ty: TypeId,
        suppress_auto_prop: bool,
        marked: bool,
        mark_is_operator_inherent: bool,
        span: Span,
    ) -> TypeId {
        if marked {
            // R3: the `!` reached a genuine fallible call — the mark is real.
            self.fallible_mark_consumed = true;
        }
        match self.scopes.lookup("Result") {
            Some(result_def_id) => {
                let raw_result = self
                    .types
                    .intern_generic(result_def_id, vec![return_type, err_ty]);
                let dest_is_result = self
                    .decl_type_hint
                    .map_or(false, |h| self.type_is_result(h));
                // (1) Disposition inner — `catch`/`rethrow` set `suppress_auto_prop`
                //     and read the whole raw Result. The mark is MANDATORY there
                //     (`f()! catch …`); an unmarked `f() catch …` is the bare-call
                //     error. Checked FIRST so a disposition wins over an ambient
                //     `Result` hint.
                if suppress_auto_prop {
                    if !marked {
                        self.emit_missing_fallible_mark(err_ty, FallibleMarkReason::Bare, span);
                    }
                    return raw_result;
                }
                // (2) Explicit `Result[T,E]` capture position. LEGAL UNMARKED (the
                //     annotation carries the visibility — 2026-07-17 amendment);
                //     marking it too is the redundant-mark error (remove the `!`).
                //     D26 exception: when the mark IS the operator (`+!` etc), the
                //     capture spelling is `Result[T, ArithError] r = a +! b` — no
                //     un-marked variant exists, so the redundant-mark reject does
                //     not apply. The raw Result flows through as the expr type.
                if dest_is_result {
                    if marked && !mark_is_operator_inherent {
                        self.emit_missing_fallible_mark(
                            err_ty,
                            FallibleMarkReason::RedundantOnCapture,
                            span,
                        );
                    }
                    return raw_result;
                }
                // (3) No capture, no disposition: the mark is mandatory. A bare
                //     fallible call is always illegal (both a propagating and a
                //     non-propagating context — bare is bare).
                if !marked {
                    self.emit_missing_fallible_mark(err_ty, FallibleMarkReason::Bare, span);
                    return self.types.error_id;
                }
                // (4) Marked: activate the error channel.
                if self.current_fn_can_propagate() {
                    // Route A: the producer-peel fires. Gate the callee-E
                    // against the caller-E before discarding it.
                    self.auto_prop_error_gate(err_ty, span);
                    return_type
                } else {
                    // Marked but cannot propagate here and no disposition
                    // attached → E_UnhandledThrows (message flipped for D29).
                    self.error(
                        SemanticErrorKind::UnhandledThrows {
                            throws_type: self.describe_resolved_type(err_ty),
                        },
                        span,
                    );
                    self.types.error_id
                }
            }
            None => return_type,
        }
    }

    /// D29: emit `E_MissingFallibleMark` with the callee error type rendered for
    /// the teaching message (never the `Result[…]` desugar — D23 contract).
    fn emit_missing_fallible_mark(
        &mut self,
        err_ty: TypeId,
        reason: FallibleMarkReason,
        span: Span,
    ) {
        // Migration instrument (env-gated, no user-facing behavior): the checker
        // IS the mark-insertion oracle — every Bare site's span END is exactly
        // the byte after the call's `)`, where the mechanical `!` goes. The
        // corpus migrator (`gg fmt`-adjacent tooling) reads these lines.
        if reason == FallibleMarkReason::Bare
            && std::env::var_os("GG_D29_MIGRATE").is_some()
        {
            eprintln!("[d29-mark] {} {}", span.start, span.end);
        }
        self.error(
            SemanticErrorKind::MissingFallibleMark {
                throws_type: self.describe_resolved_type(err_ty),
                reason,
            },
            span,
        );
    }

    /// Method-path adapter for `resolve_throws_call_type` (D23). Resolves the
    /// callee method's `throws E` and routes a throws method call through the
    /// shared producer helper; a non-throws method keeps its bare `return_type`.
    /// Called at EACH throws-carrying method-return site (the primary,
    /// trait-default, and cross-module-equip dispatch paths) so the totality
    /// gate is uniform — the `method_throws_return_sites` arm-count lint pins
    /// the site count so a new method-return site can't silently reintroduce
    /// the pre-D23 hole (a `throws` method typed as bare `T` → silent
    /// miscompile-to-garbage, the measured `int x = 1 + s.risky()`).
    ///
    /// Two throws sources, because trait methods and equip methods register
    /// through different passes:
    ///   1. CONCRETE equip/extern methods — `throws_type_id` is in
    ///      `function_info` (`resolve.rs:745`/`812`), keyed by the equip-method
    ///      def_id. Reachable at the PRIMARY site (`resolve_method` returns the
    ///      equip-method def_id) and the CROSS-MODULE site
    ///      (`resolve_method_by_name` likewise).
    ///   2. TRAIT-DEFAULT methods — BOTH `resolve_method` and
    ///      `resolve_method_by_name` return the *trait* def_id
    ///      (`traits.rs:199`/`297`), which is NOT a `function_info` key, so (1)
    ///      yields None. The default's throws clause is carried (as AST, for
    ///      call-site `Self`/`T` substitution) in the trait's `DefaultMethodSig`
    ///      — resolve it here. Without this a trait-default that *itself* throws
    ///      would slip the gate → silent miscompile-to-garbage (measured, and
    ///      pinned by `d23_unhandled_method_traitdefault.gg`).
    fn resolve_throws_method_ret(
        &mut self,
        method_def_id: DefId,
        method_name: &str,
        receiver_type_id: TypeId,
        return_type: TypeId,
        suppress_auto_prop: bool,
        marked: bool,
        span: Span,
    ) -> TypeId {
        // (1) Concrete equip/extern method.
        let mut err_ty = self
            .function_info
            .get(&method_def_id)
            .and_then(|fi| fi.throws_type_id);
        // (2) Trait-default method: `method_def_id` is the trait def_id here.
        if err_ty.is_none() {
            if let Some(throws_ast) = self
                .traits
                .traits
                .get(&method_def_id)
                .and_then(|ti| ti.default_method_sigs.get(method_name))
                .and_then(|ds| ds.throws_ast.clone())
            {
                // Substitute `Self`/trait generic params against the concrete
                // receiver BEFORE resolving — the throws clause rides the SAME
                // bindings as the default sig's return/param types
                // (`default_sig_bindings`; one substitution mechanism per
                // axis). Without it, `throws E` in `trait Risky[E]` resolved
                // whatever `E` names in the CALLER's scope: a colliding
                // top-level `struct E` mis-typed the error (spurious
                // E_UnconvertibleErrorPropagation vs. the equip's real
                // binding), and a non-colliding name resolved to `error_id`
                // (diagnostics rendered "throws `<error>`").
                let substituted = match self
                    .default_sig_bindings(method_def_id, receiver_type_id)
                {
                    Some(bindings) => {
                        super::traits::substitute_ast_type(&throws_ast, &bindings)
                    }
                    // No impl / unprojectable receiver — fall back to the raw
                    // AST (concrete throws types still resolve; trait-param
                    // ones degrade to error_id below, never to non-throws).
                    None => throws_ast,
                };
                // D23 totality: a resolution FAILURE must never read as
                // "non-throws" (the pre-D23 silent-miscompile hole). Map Err
                // to error_id — the unhandled-position arm still fires, and
                // the propagation gate's is_unsettled skip keeps genuinely
                // unresolvable generic contexts permissive.
                err_ty = Some(
                    super::types::ast_type_to_resolved(
                        &substituted,
                        span,
                        self.scopes,
                        self.types,
                    )
                    .unwrap_or(self.types.error_id),
                );
            }
        }
        match err_ty {
            Some(e) => {
                self.resolve_throws_call_type(return_type, e, suppress_auto_prop, marked, /*mark_is_operator_inherent=*/ false, span)
            }
            // Kind-2: a non-throws method whose DECLARED return is `Result[T,E]`
            // is fallible too (D29 one-mark-for-both-kinds). NO combinator
            // carve-out (the 2026-07-17 amendment dissolves it): a combinator
            // like `r.and_then(f)` is just a kind-2 call — unmarked it is a legal
            // Result VALUE flow (consumed by the chain), marked it activates the
            // channel, discarded bare it is the same E_MissingFallibleMark. No
            // receiver-type predicate is needed.
            None if self.type_is_result(return_type) => {
                self.resolve_kind2_call_type(return_type, suppress_auto_prop, marked, span)
            }
            None => return_type,
        }
    }

    /// D29 kind-2 site (non-throws callee whose declared return is `Result[T,E]`).
    /// Unmarked = a legal Result VALUE flow (its bare-discard is caught at
    /// statement position, not here). Marked = peel to `T` + activate the error
    /// channel, exactly like a kind-1 throws call.
    fn resolve_kind2_call_type(
        &mut self,
        result_return_type: TypeId,
        suppress_auto_prop: bool,
        marked: bool,
        span: Span,
    ) -> TypeId {
        if marked {
            // R3: the `!` reached a genuine fallible call — the mark is real.
            self.fallible_mark_consumed = true;
        }
        // Extract T and E from the declared `Result[T, E]` return.
        let resolved = self.resolve_type(result_return_type);
        let (t, e) = match self.types.get(resolved).clone() {
            ResolvedType::Generic(def_id, args)
                if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" =>
            {
                (args[0], args[1])
            }
            // Not actually a `Result[T,E]` (shouldn't happen — guarded by the
            // caller's `type_is_result`); return as-is.
            _ => return result_return_type,
        };
        // (1) Disposition inner — `catch`/`rethrow` set `suppress_auto_prop` and
        //     read the whole raw Result. The mark is MANDATORY here for BOTH
        //     kinds (LOG 2026-07-17 amendment: "`!` marks error-channel
        //     ACTIVATION — the three control-flow dispositions — on BOTH call
        //     kinds"): unmarked `parse(s) catch …` is the bare-call error. A
        //     `catch` on a Result LOCAL is not a call and never reaches this
        //     chokepoint — it stays legal. Checked FIRST, mirroring kind-1.
        if suppress_auto_prop {
            if !marked {
                self.emit_missing_fallible_mark(e, FallibleMarkReason::Bare, span);
            }
            return result_return_type;
        }
        if !marked {
            // Legal value flow: bind / match / pass / chain / receiver. The
            // bare-DISCARD case (a statement whose value is this un-consumed
            // Result) is enforced separately at statement position.
            return result_return_type;
        }
        // Marked: activate the channel — same discipline as a throws call.
        let dest_is_result = self
            .decl_type_hint
            .map_or(false, |h| self.type_is_result(h));
        if dest_is_result {
            // `Result[T,E] r = parse()!` — the mark is redundant (capture is the
            // unmarked spelling). Remove the `!`.
            self.emit_missing_fallible_mark(e, FallibleMarkReason::RedundantOnCapture, span);
            return result_return_type;
        }
        if self.current_fn_can_propagate() {
            self.auto_prop_error_gate(e, span);
            t
        } else {
            self.error(
                SemanticErrorKind::UnhandledThrows {
                    throws_type: self.describe_resolved_type(e),
                },
                span,
            );
            self.types.error_id
        }
    }

    /// Shared return-value check for BOTH `Stmt::Return` (block body) and an
    /// expression-body function tail. Reference §5.1: an expression body is
    /// "equivalent to a block body with `return`", so the two must type the
    /// tail identically — folding them here kills the sibling-drift class (the
    /// expr-body arm historically skipped the return-type hint + the auto-prop/
    /// capture guards, so a `throws` tail leaked instead of peeling/capturing).
    ///
    /// Sets the declared return type as the inference hint (so a throws call
    /// peels/captures against it and a bare collection/`Result` literal resolves
    /// against it), then unifies UNLESS the value is an auto-propagated throws
    /// result, a collection-literal assignment, or a legitimate whole-`Result`
    /// capture (§10.3). Returns the inferred tail type (callers use it, e.g. for
    /// the expr-body noreturn check). `return_type` is `Option` because a bare
    /// `return` in a context with no declared return type has none — matching
    /// the pre-fold `Stmt::Return` behavior (infer with no hint, skip unify).
    fn check_return_value(
        &mut self,
        return_type: Option<TypeId>,
        expr: &Spanned<Expr>,
    ) -> TypeId {
        let prev_hint = self.decl_type_hint;
        self.decl_type_hint = return_type;
        let expr_type = self.infer_expr(expr);
        self.decl_type_hint = prev_hint;
        if let Some(ret_type) = return_type {
            if !self.is_collection_assignment(ret_type, expr_type)
                && !self.auto_prop_skips_unify(ret_type, expr_type, expr.span)
                && !self.is_result_capture_compatible(ret_type, expr_type)
            {
                self.unify(ret_type, expr_type, expr.span);
            }
        }
        expr_type
    }

    fn current_fn_can_propagate(&self) -> bool {
        if self.current_function_throws {
            return true;
        }
        if let Some(ret_type) = self.current_return_type {
            let ret_resolved = self.resolve_type(ret_type);
            if let ResolvedType::Generic(ret_def_id, _) = self.types.get(ret_resolved) {
                if self.scopes.get_def(*ret_def_id).name == "Result" {
                    return true;
                }
            }
        }
        false
    }

    /// True when `type_id` resolves to a `Result[T, E]` enum.
    fn type_is_result(&self, type_id: TypeId) -> bool {
        let resolved = self.resolve_type(type_id);
        if let ResolvedType::Generic(def_id, args) = self.types.get(resolved) {
            return args.len() == 2 && self.scopes.get_def(*def_id).name == "Result";
        }
        false
    }

    /// True when a match arm pattern discriminates a `Result` variant
    /// (`Ok` / `Error`, optionally `Result.`-qualified). Used by the D29
    /// marked-match check (`match f()!:` peels to `T`; `Ok`/`Error` arms then
    /// cannot match — capture the Result first).
    fn pattern_discriminates_result(pat: &Pattern) -> bool {
        if let Pattern::Constructor { path, .. } = pat {
            let head = path.first().map(|s| s.node.as_str());
            let second = path.get(1).map(|s| s.node.as_str());
            matches!(head, Some("Ok" | "Error"))
                || (matches!(head, Some("Result")) && matches!(second, Some("Ok" | "Error")))
        } else {
            false
        }
    }

    /// D29: reject `Ok`/`Error` arms over a scrutinee that is NOT a `Result` —
    /// the `match f()!:` peel case (the mark peeled the Result to `T`, so the
    /// arms can no longer inspect it). Only fires on a concrete, resolved,
    /// non-error scrutinee to avoid cascading on an already-failed inference.
    fn check_result_arms_against_scrutinee<'p>(
        &mut self,
        scrutinee_type: TypeId,
        patterns: impl Iterator<Item = &'p Pattern>,
        span: Span,
    ) {
        let resolved = self.resolve_type(scrutinee_type);
        // Skip unresolved / error scrutinees (a prior error already reported).
        if resolved == self.types.error_id
            || matches!(self.types.get(resolved), ResolvedType::Var(_))
        {
            return;
        }
        if self.type_is_result(scrutinee_type) {
            return; // matching a real Result — fine.
        }
        let mut ps = patterns;
        if ps.any(Self::pattern_discriminates_result) {
            self.error(
                SemanticErrorKind::MissingFallibleMark {
                    throws_type: String::new(),
                    reason: FallibleMarkReason::ResultArmsOnPeeled,
                },
                span,
            );
        }
    }

    /// Check if auto-propagation allows assigning a `Result[T, E]` value to a `T`-typed
    /// destination. Requires the current function to be a propagation context (has `throws`
    /// or returns `Result`).
    ///
    /// NOTE: with the centralized throws-fn-call peel at the producer (the
    /// Snag #35-followup inversion above), a *throws-fn* call is already peeled
    /// to `T` in a propagating context, so this guard's Result-value branch is
    /// dead for those calls. It stays live for *explicit* `Result`-returning
    /// fn calls (not peeled at the producer), which still auto-prop at the
    /// consumer positions via this predicate.
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

    /// The error type (`E`) the CURRENT function propagates into: the resolved
    /// `throws E` type for a throws function, or the `E` of its `Result[T, E]`
    /// return type for a non-throws Result-returning function. `None` when the
    /// current function is not a propagating context. (Snag #11.)
    fn current_caller_error_type(&self) -> Option<TypeId> {
        if let Some(tid) = self.current_fn_throws_type_id {
            return Some(tid);
        }
        // Non-throws Result-returning caller: caller-E is args[1] of the
        // declared `Result[T, E]` return type.
        if let Some(ret_type) = self.current_return_type {
            let ret_resolved = self.resolve_type(ret_type);
            if let ResolvedType::Generic(ret_def_id, ref args) =
                self.types.get(ret_resolved).clone()
            {
                if args.len() == 2 && self.scopes.get_def(ret_def_id).name == "Result" {
                    return Some(args[1]);
                }
            }
        }
        None
    }

    /// Look up an `equip CallerE with From[CalleeE]:` impl and return its
    /// `from` method DefId. The semantic `TraitRegistry` keys trait impls on
    /// the resolved `(trait_def_id, self_type, [trait_args])` triple, so this
    /// is a direct lookup — no name reconstruction (devbook/24). (Snag #11.)
    fn lookup_from_conversion(&self, caller_err: TypeId, callee_err: TypeId) -> Option<DefId> {
        let from_def_id = self.scopes.lookup("From")?;
        // Resolve both error types to the same canonical form the registry
        // used at impl-registration time (`ast_type_to_resolved`).
        let caller_resolved = self.resolve_type(caller_err);
        let callee_resolved = self.resolve_type(callee_err);
        let idx = *self.traits.trait_impls.get(&(
            from_def_id,
            caller_resolved,
            vec![callee_resolved],
        ))?;
        self.traits.impls[idx]
            .methods
            .get("from")
            .map(|(def_id, _)| *def_id)
    }

    /// Snag #11 — the shared error-type gate at BOTH auto-propagation
    /// chokepoints (Route B's consumer guards here; Route A's producer-peel
    /// calls `auto_prop_error_gate` directly). Drop-in replacement for the old
    /// `is_auto_propagation_compatible` predicate: returns `true` exactly when
    /// the caller should SKIP `unify` (the value auto-propagates), but ALSO,
    /// when the propagation crosses error types, either records a `From`
    /// conversion or EMITS the teaching error.
    ///
    /// `prop_span` is the producing call expression's span — the key into
    /// `from_conversions` (it must be the same span the lowering's
    /// `lower_expr` hook reads). When this returns `true` for the reject case
    /// the error is already emitted; the caller skips `unify` so it does not
    /// also surface a misleading `expected T, found Result` mismatch.
    fn auto_prop_skips_unify(
        &mut self,
        declared: TypeId,
        value: TypeId,
        prop_span: Span,
    ) -> bool {
        // First, is this an auto-propagation position at all? (Same predicate
        // as before: value is `Result[T, E]`, ok-type matches declared, and
        // the current fn can propagate.) If not, the caller unifies as usual.
        if !self.is_auto_propagation_compatible(declared, value) {
            return false;
        }
        // It IS a propagation position. Pull the callee-E and gate it against
        // the caller-E. The OK-type already matched, so without this gate the
        // error types could silently diverge → the snag #11 miscompile.
        let value_resolved = self.resolve_type(value);
        let callee_err = match self.types.get(value_resolved).clone() {
            ResolvedType::Generic(def_id, ref args)
                if args.len() == 2 && self.scopes.get_def(def_id).name == "Result" =>
            {
                args[1]
            }
            _ => return true, // not a Result; predicate already vetted it
        };
        self.auto_prop_error_gate(callee_err, prop_span);
        true
    }

    /// The error-type half of the gate, factored out so Route A (producer-peel)
    /// and Route B (consumer guards) share ONE decision. Given the callee's
    /// error type and the producing call span, compare against the caller-E:
    ///   - caller not a propagating context → nothing to do (shouldn't happen
    ///     at a gated site, but harmless);
    ///   - same error type → no metadata, byte-identical fast path;
    ///   - different + `From[callee]` on caller → record the conversion DefId;
    ///   - different + no `From` → emit `UnconvertibleErrorPropagation`.
    /// (Snag #11.)
    fn auto_prop_error_gate(&mut self, callee_err: TypeId, prop_span: Span) {
        let Some(caller_err) = self.current_caller_error_type() else {
            return;
        };
        let caller_resolved = self.resolve_type(caller_err);
        let callee_resolved = self.resolve_type(callee_err);
        // Same error type — the fast path. Must stay metadata-free so the
        // lowering emits byte-identical C (the gate is a true no-op here).
        if caller_resolved == callee_resolved {
            return;
        }
        // An unresolved type variable on either side means inference hasn't
        // pinned the error type yet (generic propagation); don't gate — a
        // later, more-resolved position will. Likewise skip the error
        // sentinel (a prior error already reported).
        let is_unsettled = |checker: &Self, tid: TypeId| {
            matches!(checker.types.get(tid), ResolvedType::Var(_)) || tid == checker.types.error_id
        };
        if is_unsettled(self, caller_resolved) || is_unsettled(self, callee_resolved) {
            return;
        }
        // Different concrete error types: require a `From[callee]` on caller.
        if let Some(from_def_id) = self.lookup_from_conversion(caller_resolved, callee_resolved) {
            self.from_conversions.insert(prop_span, from_def_id);
        } else {
            let caller_name = self.describe_resolved_type(caller_resolved);
            let callee_name = self.describe_resolved_type(callee_resolved);
            self.error(
                SemanticErrorKind::UnconvertibleErrorPropagation {
                    caller_err: caller_name,
                    callee_err: callee_name,
                },
                prop_span,
            );
        }
    }

    /// Compute the `Self`/trait-generic-param AST bindings of a trait against
    /// a concrete receiver (the ONE substitution mechanism for everything a
    /// trait-default sig carries — return type, param types, AND the `throws`
    /// clause all resolve through these same bindings; one source of truth
    /// per axis, docs/devbook/24 rule 3).
    ///
    /// Returns `None` if any prerequisite is missing (no matching impl, or
    /// the receiver can't be projected back to AST).
    fn default_sig_bindings(
        &mut self,
        trait_def_id: DefId,
        receiver_type_id: TypeId,
    ) -> Option<FxHashMap<String, Type>> {
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
        Some(full_bindings)
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
        let full_bindings = self.default_sig_bindings(trait_def_id, receiver_type_id)?;

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

    /// Round XXVI Track A + Round XXVIII Track A — reject the 9 wrong-receiver
    /// one-sided combinator + tag-check cells BEFORE dispatch (Core #4
    /// chokepoint, mirror of ggdef XXV Track B at
    /// `spec/ggdef/src/elaborate/mod.rs:2551-2574`). Rust silently accepted
    /// these then crashed at C-compile (combinators) or emitted silently-wrong
    /// output (tag-checks — Option/Result share the two-variant discriminant
    /// layout so `.is_some()` on Result etc. "happens to work" for the wrong
    /// reason — Core #8 both-wrong).
    ///
    /// The 9 cells are ratified per `docs/language-reference.md:3861-3891`
    /// (Option/Result method tables); each is EITHER Option-only OR
    /// Result-only:
    ///   - (Result, flat_map)      — Option-only (`Result` uses `and_then`)
    ///   - (Result, filter)        — Option-only (no Error-side predicate)
    ///   - (Result, flatten)       — Option-only (no `Result[Result[T,E],E]`)
    ///   - (Option, map_err)       — Result-only (no Error axis to map)
    ///   - (Option, unwrap_error)  — Result-only (no Error payload to unwrap)
    ///   - (Result, is_some)       — Option-only (Result uses `is_ok/is_error`)
    ///   - (Result, is_none)       — Option-only (Result uses `is_ok/is_error`)
    ///   - (Option, is_ok)         — Result-only (Option uses `is_some/is_none`)
    ///   - (Option, is_error)      — Result-only (Option uses `is_some/is_none`)
    ///
    /// Covers BOTH combinator (Result/Option HOF-taking methods) and tag-check
    /// (`.is_*()` inspectors) shapes — rename out of scope for XXVIII Track A.
    ///
    /// Emits the existing `SemanticErrorKind::NoMethodFound` (whose Display
    /// prints "no method `X` found on type `Y`" tagged `E_NoMethodFound`);
    /// the "outside phase-0 subset (Option-only)"/"(Result-only)" hint is
    /// folded into the `type_` slot since the kind has no separate hint
    /// field.
    ///
    /// Call site is at `:2799` — AFTER trait-registry resolution
    /// (`:2652`) and default-fallback (`:2755-2796`) so a hypothetical
    /// user `equip Result: fn flat_map(...)` shadows the reject (correct
    /// semantics: user override beats built-in class-fix). No such user
    /// override exists in the current corpus (grep-verified 2026-08-02).
    ///
    /// Class-guard: `tests/lints.rs::reject_wrong_receiver_combinator_arms_count`
    /// pins the arm count (EXPECTED=9) via a distinctive per-arm marker
    /// (see the arm comments below — the marker string is spelled ONLY
    /// on the 9 arms, so this doc-line and the call-site comment at
    /// `:2799` deliberately paraphrase it to avoid inflating the count).
    /// A new one-sided combinator MUST land in ggdef's `elaborate_method`
    /// (production receiver-gate — not a lint) AND here AND in the SH
    /// chokepoint at `tests/fixtures/self_host_typechecker/typecheck.gg`
    /// (Core #9 all-lanes semantic change). ggdef LAG for tag-checks is
    /// filed as a separate follow-up: the elaborate arm-picker currently
    /// blanket-rejects `.is_some()/.is_none()/.is_ok()/.is_error()` for any
    /// receiver, so the wrong-cell case is already blocked, but the
    /// right-cell case is not yet subset-accepted (Core #13 subset-expansion).
    fn reject_wrong_receiver_combinator(
        &mut self,
        receiver_type: TypeId,
        method: &str,
        span: Span,
    ) {
        let resolved = self.resolve_type(receiver_type);
        let base_name = match self.types.get(resolved) {
            ResolvedType::Generic(def_id, _) | ResolvedType::Defined(def_id) => {
                self.scopes.get_def(*def_id).name.clone()
            }
            _ => return,
        };
        let hint = match (base_name.as_str(), method) {
            ("Result", "flat_map")     => "Option-only", // R26A_ARM_MARKER
            ("Result", "filter")       => "Option-only", // R26A_ARM_MARKER
            ("Result", "flatten")      => "Option-only", // R26A_ARM_MARKER
            ("Option", "map_err")      => "Result-only", // R26A_ARM_MARKER
            ("Option", "unwrap_error") => "Result-only", // R26A_ARM_MARKER
            // Round XXVIII Track A — tag-check cells. Option/Result share
            // the two-variant discriminant layout so `.is_some()` on Result
            // (etc.) silently "worked" for the wrong reason (Core #8
            // both-wrong: silent-accept + silent-wrong-output).
            ("Result", "is_some")      => "Option-only", // R26A_ARM_MARKER
            ("Result", "is_none")      => "Option-only", // R26A_ARM_MARKER
            ("Option", "is_ok")        => "Result-only", // R26A_ARM_MARKER
            ("Option", "is_error")     => "Result-only", // R26A_ARM_MARKER
            _ => return,
        };
        let type_desc = self.describe_resolved_type(resolved);
        self.error(
            SemanticErrorKind::NoMethodFound {
                method: method.to_string(),
                type_: format!(
                    "{type_desc} \u{2014} `.{method}()` is outside the phase-0 subset ({hint})"
                ),
            },
            span,
        );
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
            ("Option", "and_then") | ("Option", "flat_map") => {
                // (T) -> Option[U], returns Option[U] directly.
                // Legitimate cross-type map (T → Option[U] where U ≠ T is
                // intended); NOT in `unify_closure_ret_axis`'s enumeration.
                // Round XXIX Track B added `flat_map` (alias of `and_then`
                // per Option protocol at src/ir/lowering/builtins.rs:896
                // — combinator_kind::FlatMap). The gate widening at
                // has_inherent_only_impls would silently reject
                // cross-type `.flat_map()` calls without this LIVE arm
                // (the oracle DEAD arm returns receiver_type, which is
                // wrong for cross-type Option[T] → Option[U]).
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                Some(ret_type)
            }
            ("Option", "or_else") => {
                // () -> Option[T'], must return Option[T]. Track α write-site
                // fix: previously discarded the closure return entirely and
                // returned `Some(receiver_type)`, silently allowing an
                // Option[Money] closure return under an Option[int] receiver
                // — the mis-typed payload then leaked at runtime. Now the
                // closure's declared return type flows through, and the
                // shared `unify_closure_ret_axis` helper enforces T' == T.
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                let arg_span = args.first().map(|a| a.span).unwrap_or_else(Span::dummy);
                self.unify_closure_ret_axis(
                    ClosureCombinatorCell::OptionOrElse,
                    receiver_type,
                    &type_args,
                    ret_type,
                    arg_span,
                );
                Some(ret_type)
            }
            ("Result", "map") => {
                // (T) -> U, returns Result[U, E]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                let e_type = type_args.get(1).copied()?;
                Some(self.types.intern_generic(def_id, vec![u_type, e_type]))
            }
            ("Result", "and_then") => {
                // (T) -> Result[U, E'], returns Result[U, E'] directly.
                // Track α unify enforcement: E' must equal receiver's E
                // (Error-axis is a passthrough for and_then; only the Ok
                // axis is legitimately mapped). Without the unify a
                // closure returning `Result[U, int]` under a receiver
                // `Result[T, Money]` mis-sized the never-taken Error
                // slot and leaked heap Money.
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                let arg_span = args.first().map(|a| a.span).unwrap_or_else(Span::dummy);
                self.unify_closure_ret_axis(
                    ClosureCombinatorCell::ResultAndThen,
                    receiver_type,
                    &type_args,
                    ret_type,
                    arg_span,
                );
                Some(ret_type)
            }
            ("Result", "or_else") => {
                // (E) -> Result[T', F], returns Result[T', F] directly.
                // Track α write-site fix (Core #1 / Core #4): the lowerer
                // used to fall through to `_ => recv_type` at `result_local`
                // birth, mis-sizing the branch memcpys → stack-buffer-
                // overflow READ of size 80 at the merge (both Ok- and
                // Err-axis cross-type shapes triggered it). Enforcing
                // T' == T here rejects the ill-typed Ok-axis at check time
                // (recovery is the Error axis only); the lowerer's new
                // `or_else` arm at methods.rs:3779 sizes the result from
                // the closure's declared return so E' ≠ E is safe.
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                let arg_span = args.first().map(|a| a.span).unwrap_or_else(Span::dummy);
                self.unify_closure_ret_axis(
                    ClosureCombinatorCell::ResultOrElse,
                    receiver_type,
                    &type_args,
                    ret_type,
                    arg_span,
                );
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
            ("Vector", "flat_map") => {
                // (T) -> Vector[U], returns Vector[U] directly (flattens).
                // Round XXIX Track B added — Vector protocol has flat_map
                // at src/ir/lowering/builtins.rs:404 with ret_self, but
                // cross-type usage `Vector[String] ys = xs.flat_map((int
                // x): [f"{x}"])` needs the closure return type (Vector[U])
                // to flow through, not receiver_type. Mirrors Vector.map's
                // LIVE cross-type elaboration; the oracle DEAD arm above
                // returns receiver_type (wrong for cross-type) but is
                // shadowed by this LIVE path.
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let ret_type = self.extract_fn_return_type(closure_type)?;
                Some(ret_type)
            }
            ("Vector", "zip") => {
                // Vector[T].zip(Vector[U]) -> Vector[Tuple[T, U]] per Rust
                // convention. Round XXIX Track B added — Vector protocol at
                // src/ir/lowering/builtins.rs:425 says ret_self (Vector[T])
                // erasing the U-axis; that's protocol-vs-convention
                // mismatch filed as follow-up. This LIVE arm elaborates
                // the correct cross-type shape from the arg's element type.
                let arg_type = self.infer_expr(&args.first()?.node.value);
                // Extract Vector[U]'s U from the arg.
                let u_type = match self.types.get(arg_type) {
                    ResolvedType::Generic(_, inner_args) => inner_args.first().copied(),
                    _ => None,
                }?;
                let t_type = type_args.first().copied()?;
                let tuple_tid = self.types.insert(ResolvedType::Tuple(vec![t_type, u_type]));
                Some(self.types.intern_generic(def_id, vec![tuple_tid]))
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

    /// Shared integer-capacity predicate for builtin-ctor capacity args —
    /// the `cap=` arm and every positional-capacity arm (String(n),
    /// Arena/TlsfAllocator/FixedBufferAllocator/Channel positional,
    /// PoolAllocator's two ints) classify through THIS one predicate
    /// (Core #4: one predicate, no parallel list). Unwraps &/! (an int
    /// borrowed through a `&` param still names a valid capacity) and
    /// returns `(inner, ok)` so the caller can render `inner` in the
    /// diagnostic. Error: already diagnosed — don't cascade. Never:
    /// diverging arg, unreachable anyway. Var: unbound inference
    /// variable — can't classify; never false-positive.
    fn int_capacity_check(&mut self, resolved: TypeId) -> (TypeId, bool) {
        let inner = match self.types.get(resolved) {
            ResolvedType::Ref(t) | ResolvedType::Owned(t) => self.resolve_type(*t),
            _ => resolved,
        };
        let ok = match self.types.get(inner) {
            ResolvedType::Primitive(p) => is_integer_type(p),
            ResolvedType::Error | ResolvedType::Never | ResolvedType::Var(_) => true,
            _ => false,
        };
        (inner, ok)
    }

    /// Shared is-an-allocator predicate for builtin-ctor allocator args —
    /// the `alloc=` arm and FallbackAllocator's two positionals classify
    /// through this one predicate. Same Error/Never/Var no-cascade
    /// exemption as `int_capacity_check`.
    fn is_allocator_arg_type(&mut self, resolved: TypeId) -> bool {
        match self.types.get(resolved) {
            ResolvedType::Defined(def_id) => {
                matches!(
                    self.scopes.get_def(*def_id).name.as_str(),
                    "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator"
                        | "FixedBufferAllocator" | "FallbackAllocator"
                )
            }
            ResolvedType::Error | ResolvedType::Never | ResolvedType::Var(_) => true,
            _ => false,
        }
    }

    /// Validate a call with named arguments and/or default parameters.
    /// Checks: no positional after named, no unknown names, no duplicates,
    /// all required params are satisfied. Also type-checks args (including defaults).
    /// Validate named args + fill-in defaults against a parameter list.
    ///
    /// `param_names` / `param_defaults` are passed as slices (not a
    /// `&FunctionInfo`) so both call sites can supply the right view:
    /// the free-function ECall path passes the whole `FunctionInfo`
    /// slices, while the EMethodCall path passes a `self`-STRIPPED view
    /// (the method's `FunctionInfo` includes `self` at index 0, but the
    /// resolved `param_types` from `resolve_method` exclude it — they
    /// must align). See the EMethodCall WrongArgCount site.
    fn check_named_args_and_defaults(
        &mut self,
        args: &[Spanned<CallArg>],
        param_types: &[TypeId],
        param_names: &[String],
        param_defaults: &[Option<Spanned<Expr>>],
        call_span: Span,
    ) {
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
                        // Snag #35: skip unify for auto-prop / capture-compatible throws-call args.
                        if !self.auto_prop_skips_unify(param_types[pos], arg_type, arg.node.value.span)
                            && !self.is_result_capture_compatible(param_types[pos], arg_type)
                        {
                            self.unify(param_types[pos], arg_type, arg.span);
                        }
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
                        // Snag #35: skip unify for auto-prop / capture-compatible throws-call args.
                        if !self.auto_prop_skips_unify(param_types[i], arg_type, arg.node.value.span)
                            && !self.is_result_capture_compatible(param_types[i], arg_type)
                        {
                            self.unify(param_types[i], arg_type, arg.span);
                        }
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

    /// Round XXIII Track α (Core #4 class-fix, Core #2 typed metadata):
    /// closure-returning Option/Result combinators must unify a specific
    /// payload axis between the closure's declared return and the receiver.
    /// This helper is the SINGLE PRODUCER for that axis check across the
    /// 3 unify-eligible cells enumerated by `ClosureCombinatorCell`:
    ///
    /// | Cell               | Receiver          | Closure return         | Axis      |
    /// |--------------------|-------------------|------------------------|-----------|
    /// | `Result.or_else`   | `Result[T, E]`    | `Result[T', E']`       | Ok  (T'==T) |
    /// | `Result.and_then`  | `Result[T, E]`    | `Result[U, E']`        | Err (E'==E) |
    /// | `Option.or_else`   | `Option[T]`       | `Option[T']`           | Some (T'==T) |
    ///
    /// Cells intentionally EXCLUDED (Core #12 axis rationale, in the
    /// enumeration's doc-comment for the reviewer):
    /// - `.map` / `.map_err` — closures return SCALARS, not nested
    ///   Option/Result; the outer type is reconstructed from the scalar
    ///   return. No axis to unify (Core #15e Q3: enumeration is TOTAL over
    ///   the class of closures returning Option[…]/Result[…]).
    /// - `Result.flat_map` — deliberately UNREGISTERED in
    ///   `src/ir/lowering/builtins.rs::RESULT` (assertion at ~:1425
    ///   forbids it). DO NOT register: registering would invalidate the
    ///   assertion and reintroduce a runtime dispatch path with no
    ///   fixture coverage.
    /// - `Option.and_then` / `Option.flat_map` — legitimate cross-type
    ///   map: `T → Option[U]` where `U ≠ T` is the intended shape (that
    ///   IS the map). No unify.
    ///
    /// The exhaustive enumeration is ratcheted by
    /// `tests/lints.rs::unify_closure_ret_axis_class_enumeration` — a
    /// class-guard that mirrors the `container_literal_arms_count`
    /// precedent (Core #6). If a new closure-returning combinator gets
    /// added to `builtins.rs`, the lint fires and forces the author
    /// through this helper or documents the exemption alongside its
    /// sibling exclusions above.
    ///
    /// Emits `E_TypeMismatch` at `span` on axis mismatch. Silently
    /// accepts fresh Vars, error types, and shape-mismatches (the outer
    /// caller has already emitted a shape error).
    fn unify_closure_ret_axis(
        &mut self,
        cell: ClosureCombinatorCell,
        receiver_type: TypeId,
        receiver_type_args: &[TypeId],
        closure_ret_type: TypeId,
        span: Span,
    ) {
        // Skip when the closure return didn't resolve to a concrete
        // Option/Result — the outer caller's shape-mismatch path (or an
        // upstream inference failure) will surface any real error, and
        // we don't want a cascading TypeMismatch on top of a Var/error.
        let closure_resolved = self.resolve_type(closure_ret_type);
        if closure_resolved == self.types.error_id
            || matches!(self.types.get(closure_resolved), ResolvedType::Var(_))
        {
            return;
        }
        let ResolvedType::Generic(_, closure_args) = self.types.get(closure_resolved) else {
            return;
        };
        let closure_args = closure_args.clone();
        let (recv_payload, closure_payload) = match cell {
            ClosureCombinatorCell::ResultOrElse => {
                // Ok-unify: T' == T (index 0), E' free.
                (receiver_type_args.first().copied(), closure_args.first().copied())
            }
            ClosureCombinatorCell::ResultAndThen => {
                // Err-unify: E' == E (index 1), U free.
                (receiver_type_args.get(1).copied(), closure_args.get(1).copied())
            }
            ClosureCombinatorCell::OptionOrElse => {
                // Some-unify: T' == T (index 0), Option has one payload.
                (receiver_type_args.first().copied(), closure_args.first().copied())
            }
        };
        let (Some(recv_payload), Some(closure_payload)) = (recv_payload, closure_payload) else {
            return;
        };
        // Only report on fully-concrete payloads — otherwise a Var can
        // still be unified against the receiver's payload downstream, and
        // an eager report would cascade.
        if !self.is_fully_concrete(recv_payload) || !self.is_fully_concrete(closure_payload) {
            return;
        }
        let recv_resolved = self.resolve_type(recv_payload);
        let closure_payload_resolved = self.resolve_type(closure_payload);
        if recv_resolved == closure_payload_resolved {
            return;
        }
        // Mismatch: describe using the FULL receiver / closure-return
        // types so the diagnostic points the reader at the shape they
        // wrote, not just an anonymous payload TypeId.
        self.error(
            SemanticErrorKind::TypeMismatch {
                expected: self.describe_resolved_type(receiver_type),
                found: self.describe_resolved_type(closure_resolved),
            },
            span,
        );
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

    /// For builtin collection mutators whose element/value type comes from
    /// the receiver's generic args (not from a sig param), return the
    /// per-argument type hint so dot-shorthand enum constructors in the
    /// argument position can infer their enum context. Returns an empty
    /// Vec for any receiver/method that doesn't carry such a hint.
    fn builtin_mutator_arg_hints(
        &self,
        receiver_type: TypeId,
        method: &str,
    ) -> Vec<Option<TypeId>> {
        let (type_name, type_args) = match self.types.get(receiver_type) {
            ResolvedType::Generic(def_id, args) => {
                (self.scopes.get_def(*def_id).name.clone(), args.clone())
            }
            _ => return Vec::new(),
        };
        let elem = type_args.first().copied();
        let val = type_args.get(1).copied();
        match (type_name.as_str(), method) {
            // Vector[T].push(T) / .insert(idx, T)
            ("Vector", "push") => vec![elem],
            ("Vector", "insert") => vec![None, elem],
            // Set[T].insert(T) / .contains(T) / .remove(T)
            ("Set", "insert") | ("Set", "contains") | ("Set", "remove") => vec![elem],
            // Dict[K, V].put(K, V) / .insert(K, V) / .set(K, V)
            ("Dict", "put") | ("Dict", "insert") | ("Dict", "set") => vec![elem, val],
            _ => Vec::new(),
        }
    }

    /// True when `receiver_type` resolves to the prelude `Option` or `Result`
    /// enum (the only receivers for which `unwrap`/`expect`/`unwrap_or` exist).
    /// Detected via the resolved def NAME at the semantic layer — NOT the
    /// IR-layer `enum_category`, which is populated only during `lower_module`
    /// after semantic analysis and is unreachable from here.
    fn is_option_or_result_receiver(&self, receiver_type: TypeId) -> bool {
        let resolved = self.resolve_type(receiver_type);
        match self.types.get(resolved) {
            ResolvedType::Generic(def_id, _) | ResolvedType::Defined(def_id) => {
                let name = &self.scopes.get_def(*def_id).name;
                name == "Option" || name == "Result"
            }
            _ => false,
        }
    }

    /// Round XXXIX Track E: extract the inner `T` of an `Option[T]` /
    /// `Result[T,E]` carrier — variant 0's payload type. Returns `None` for a
    /// non-carrier type or a carrier whose T is unrecoverable. Used by the
    /// `Expr::DefaultOp` arm to (a) validate an unwrap-form RHS and (b) label
    /// the canonical type in `expr_types` so the IR lowering can pick the
    /// right shape (Layering rule 3 — one source of truth). Reads type args
    /// directly rather than going through `resolve_user_enum_field_types`
    /// because for the prelude Option/Result the shape is fixed.
    fn default_op_inner_type(&self, carrier_type: TypeId) -> Option<TypeId> {
        let resolved = self.resolve_type(carrier_type);
        match self.types.get(resolved) {
            ResolvedType::Generic(def_id, args) => {
                let name = &self.scopes.get_def(*def_id).name;
                if name == "Option" || name == "Result" {
                    args.first().copied()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Round XXXIX Track E: true iff `rhs_type` matches the LHS carrier shape
    /// (`Option[T] × Option[T]` with T equal, or `Result[T,E] × Result[T,E]`
    /// with T AND E equal). Used by the `Expr::DefaultOp` arm to accept
    /// `Option[T] ?? Option[T]` (Option B, ratified 2026-08-09) — the RHS
    /// peels one carrier layer for chain-friendly `a ?? b ?? default`. For
    /// Result, E-must-match (a mismatched E would size-truncate the else-branch
    /// store on the E variant — reference-grade reject).
    ///
    /// Structural equality by TypeId after top-level resolve. Types are
    /// interned via `intern_generic`, so identical shapes share IDs. Nested
    /// Vars would produce a false-negative here; the fall-through then emits
    /// `E_DefaultOpRhsTypeMismatch`, which is safe (the user rewrites the
    /// annotation) — not a soundness issue.
    fn default_op_rhs_matches_carrier(&self, lhs_type: TypeId, rhs_type: TypeId) -> bool {
        let lhs = self.resolve_type(lhs_type);
        let rhs = self.resolve_type(rhs_type);
        match (self.types.get(lhs), self.types.get(rhs)) {
            (
                ResolvedType::Generic(l_def, l_args),
                ResolvedType::Generic(r_def, r_args),
            ) => {
                if l_def != r_def {
                    return false;
                }
                let name = &self.scopes.get_def(*l_def).name;
                if name != "Option" && name != "Result" {
                    return false;
                }
                if l_args.len() != r_args.len() {
                    return false;
                }
                l_args
                    .iter()
                    .zip(r_args.iter())
                    .all(|(&la, &ra)| self.resolve_type(la) == self.resolve_type(ra))
            }
            _ => false,
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
            // Deque reuses VECTOR.methods at src/ir/lowering/builtins.rs:441 —
            // the oracle mirrors that by grouping Vector and Deque under one
            // arm. Round XXIX Track B added Deque here (Deque had ZERO oracle
            // coverage; every method silent-accepted through the reject site).
            // Cross-type HOFs (map/flat_map with U ≠ T) on Deque are typed
            // per-protocol (ret_self); infer_closure_method_type has NO Deque
            // arm today — Deque cross-type HOF elaboration is filed as a
            // follow-up (categorized TODO under Deque parity).
            "Vector" | "Deque" => match method {
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
                // Round XXIX Track B — full protocol coverage (see
                // src/ir/lowering/builtins.rs:335-427 VECTOR.methods).
                // swap_remove: protocol ret_void — Rust convention returns T
                //   (filed follow-up); protocol wins for this track.
                // swap/fill/clear/extend: void per protocol.
                // clone: receiver_type (ret_self).
                // count: int.
                // find_index: protocol ret_int — Rust convention Option[int]
                //   (filed follow-up); protocol wins for this track.
                // zip: protocol ret_self — cross-type erased (filed
                //   follow-up); protocol wins for this track.
                // find: Option[T] — DEAD defense (equip block wraps this on
                //   Vector via lib/std/iter.gg:425; Deque has no equip so
                //   this arm is LIVE for Deque.find).
                // filter/map/fold/reduce/flat_map: DEAD defense for Vector
                //   (infer_closure_method_type LIVE); LIVE for Deque —
                //   cross-type elaboration filed as follow-up.
                // each/for_each: void — Vector has these via equip block
                //   (lib/std/iter.gg:413), Deque has NO equip so this arm
                //   is LIVE for Deque.each/for_each (deque_hof_each.gg,
                //   deque_each_untyped_closure_str.gg).
                "each" | "for_each" => Some(self.types.void_id),
                "swap_remove" | "swap" | "fill" => Some(self.types.void_id),
                "clone" => Some(receiver_type),
                "count" => Some(self.types.int_id),
                // find_index: `equip [T] Vector[T]:` at lib/std/iter.gg:428
                // declares Option[int] (matches Rust convention). Protocol
                // at builtins.rs:423 says ret_int (return -1 for miss) — a
                // protocol-vs-equip disagreement. This oracle arm matches
                // the equip contract so Vector/Deque callers agree.
                "find_index" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                // zip: handled in infer_closure_method_type as LIVE
                // cross-type Vector[Tuple[T, U]] elaboration. DEAD here
                // matches protocol shape (Vector[T]) as defense.
                "zip" => Some(receiver_type),
                "find" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "filter" | "map" | "flat_map" => Some(receiver_type),
                "fold" | "reduce" => Some(self.types.int_id),
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
                // D39 Phase A.3: Dict.swap_remove(key) mirrors Dict.remove's
                // return shape per DD#6 (each type's own `remove` shape wins;
                // Dict returns Option[V !], so swap_remove does too). O(1)
                // opt-in order-destroying variant.
                "swap_remove" => {
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
                // Round XXIX Track B — full protocol coverage (see
                // src/ir/lowering/builtins.rs:444-501 DICT.methods).
                // clone: receiver_type.
                // filter/fold: DEAD defense (infer_closure_method_type LIVE
                //   for Dict.filter/Dict.fold).
                // each/any/all: LIVE (no equip block on Dict for these — the
                //   lib/std/iter.gg equip [K, V] Dict[K, V] block deliberately
                //   skips them per that file's comment at line 851-858).
                "clone" => Some(receiver_type),
                "filter" => Some(receiver_type),
                "fold" => Some(self.types.int_id),
                "each" => Some(self.types.void_id),
                "any" | "all" => Some(self.types.bool_id),
                _ => None,
            },
            "Set" | "HashSet" => match method {
                "add" => Some(self.types.void_id),
                "contains" | "is_subset" | "is_superset" | "is_disjoint" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                // D39 Phase A.3: Set.swap_remove(elem) → bool — mirrors Set's
                // own `remove` shape per DD#6 (was the elem present?). O(1)
                // opt-in, order-destroying.
                "swap_remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                "union" | "intersection" | "difference" | "symmetric_difference" => Some(receiver_type),
                // Round XXIX Track B — full protocol coverage (see
                // src/ir/lowering/builtins.rs:519-567 SET.methods).
                // insert: void (alias of add per SET.methods:533).
                // has: bool (alias of contains per SET.methods:536).
                // clone: receiver_type.
                // items: Vector[T] — MIRRORS Dict.values/Dict.keys pattern
                //   above. items() is the book-mandated ordinal-Set-access
                //   path (docs/book/05-collections.md:344 —
                //   `s.items()[0]`). Writing `Some(receiver_type)` would
                //   type items() as Set[T] → E_NotIndexable on `[0]`,
                //   reversing book semantics.
                // filter/fold: DEAD defense (infer_closure_method_type LIVE).
                // each/any/all: DEAD defense (Set equip block in
                //   lib/std/iter.gg:818 wraps them; resolve_method wins).
                "insert" => Some(self.types.void_id),
                "has" => Some(self.types.bool_id),
                "clone" => Some(receiver_type),
                "items" => {
                    if let Some(vec_def_id) = self.scopes.lookup("Vector") {
                        Some(self.types.intern_generic(vec_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "filter" => Some(receiver_type),
                "fold" => Some(self.types.int_id),
                "each" => Some(self.types.void_id),
                "any" | "all" => Some(self.types.bool_id),
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
                "len" | "hash" | "count" | "byte_len" | "ord" => Some(self.types.int_id),
                "index_of" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![self.types.int_id]))
                    } else {
                        Some(self.types.int_id)
                    }
                }
                "contains" | "starts_with" | "ends_with" | "is_empty" => Some(self.types.bool_id),
                // View returns — no allocation, return str (Str).
                // `slice` mirrors `substring` — an IR-protocol view op
                // (GORGET_STRING_VIEW) absent from this oracle until round-31.
                "trim" | "strip" | "lstrip" | "rstrip" | "trim_left" | "trim_right"
                | "removeprefix" | "removesuffix" | "byte_slice" | "substring" | "slice"
                    => Some(self.types.string_id),
                // Allocating returns — return String (GorgetString).
                // `upper`/`lower` are IR-protocol aliases of to_upper/to_lower;
                // `clone` is the auto-derivable owned-copy — both absent from
                // this oracle until round-31 (see the reject-site LAYERING NOTE).
                "to_upper" | "to_lower" | "upper" | "lower" | "clone"
                | "replace" | "repeat" | "join" | "pad_left" | "pad_right"
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
                // Round XXIX Track B — `flat_map` was missing (Option protocol
                // has it at src/ir/lowering/builtins.rs:896 with
                // combinator_kind: FlatMap; the widened gate at
                // src/semantic/traits.rs:has_inherent_only_impls would
                // reject `.flat_map()` calls without this arm). Cross-type
                // LIVE elaboration is handled by infer_closure_method_type
                // (Option.flat_map arm added below mirroring Option.and_then);
                // this arm is DEAD defense — protocol shape is ret_self.
                "map" | "and_then" | "or_else" | "or" | "filter" | "flat_map" => Some(receiver_type),
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
            // Track N3: RWLock[T].read()/.write() — mirror Mutex.lock's shape.
            // Pre-fix, fall-through to `None` had the call-site infer `error_id`;
            // `describe_resolved_type` then printed `<error>` in the wrapper, and
            // the `<error>` inner propagated through chained `.get()`/`.set()`
            // as a silent-wrong-output miscompile (chained-read printed 0;
            // chained-write dropped the mutation and deadlocked the follow-up
            // read because the WriteGuard[<error>] never released its lock).
            // The builtin protocol enumerates only `read` and `write`
            // (src/ir/lowering/builtins.rs RWLOCK), so the arm is complete.
            "RWLock" => match method {
                "read" => {
                    if let Some(read_guard_def_id) = self.scopes.lookup("ReadGuard") {
                        Some(self.types.intern_generic(read_guard_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "write" => {
                    if let Some(write_guard_def_id) = self.scopes.lookup("WriteGuard") {
                        Some(self.types.intern_generic(write_guard_def_id, vec![elem_type()]))
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
            "ReadGuard" => match method {
                "get" => Some(elem_type()),
                _ => None,
            },
            "WriteGuard" => match method {
                "get" => Some(elem_type()),
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
            // Round XXIX Track B — pre-registered protocol types (Channel/
            // Heap/Thread/atomics/sync) whose equip blocks live in
            // `lib/std/*.gg` but load ONLY when the user explicitly
            // imports the sub-module. Many fixtures use e.g.
            // `from std.async import Channel` — Channel is a compiler
            // pre-registered type (see src/semantic/resolve.rs:21) so it
            // resolves without loading std.channel, meaning the equip
            // block's method registrations are absent. The widened
            // `has_inherent_only_impls` (via `lookup_protocol`) turns
            // those into E_NoMethodFound at check time; without oracle
            // arms here, well-established fixtures regress.
            "Channel" => match method {
                "send" | "close" => Some(self.types.void_id),
                "recv" => Some(elem_type()),
                "poll_recv" => Some(self.types.bool_id),
                "recv_timeout" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "len" | "capacity" => Some(self.types.int_id),
                "is_closed" => Some(self.types.bool_id),
                _ => None,
            },
            "Heap" => match method {
                "push" => Some(self.types.void_id),
                // Heap.pop/peek return Option[T] per lib/std/heap.gg:33,38 equip
                // block (Rust convention; the BuiltinTypeProtocol at
                // src/ir/lowering/builtins.rs:803-804 says ret_elem, a
                // protocol-vs-equip mismatch analogous to Vector.find_index).
                "pop" | "peek" => {
                    if let Some(option_def_id) = self.scopes.lookup("Option") {
                        Some(self.types.intern_generic(option_def_id, vec![elem_type()]))
                    } else {
                        Some(elem_type())
                    }
                }
                "len" => Some(self.types.int_id),
                "is_empty" => Some(self.types.bool_id),
                _ => None,
            },
            "Thread" => match method {
                // Thread[T].join returns T per the equip block at
                // `lib/std/thread.gg:7` (`T join(!self)`) — the
                // BuiltinTypeProtocol at src/ir/lowering/builtins.rs:784
                // says ret_void, a protocol-vs-equip mismatch filed as
                // follow-up. Equip wins for correctness.
                "join" => Some(elem_type()),
                "id" => Some(self.types.int_id),
                _ => None,
            },
            "AtomicInt" => match method {
                "load" | "add" | "sub" => Some(self.types.int_id),
                "store" => Some(self.types.void_id),
                "compare_exchange" => Some(self.types.bool_id),
                _ => None,
            },
            "AtomicBool" => match method {
                "load" | "swap" | "compare_exchange" => Some(self.types.bool_id),
                "store" => Some(self.types.void_id),
                _ => None,
            },
            "Barrier" => match method {
                "wait" => Some(self.types.void_id),
                _ => None,
            },
            "WaitGroup" => match method {
                "add" | "done" | "wait" => Some(self.types.void_id),
                _ => None,
            },
            "Semaphore" => match method {
                "acquire" | "release" => Some(self.types.void_id),
                "try_acquire" => Some(self.types.bool_id),
                _ => None,
            },
            "OnceFlag" => match method {
                "do_once" | "is_done" => Some(self.types.bool_id),
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

        // Look up the def by (name, span) — unambiguous. The previous
        // implementation tried `scopes.lookup(name)` first and fell back to
        // span lookup for equip methods, but `lookup` walks the parent
        // chain by name and can find a SAME-NAMED def from another scope
        // (e.g. a free function or extern) when the equip block isn't
        // currently on the scope stack. The wrong def would then have its
        // `type_id` overwritten with the equip method's `(Self, args) ->
        // ret` signature, silently corrupting callers of the unrelated
        // free function (gorget-arena: `equip VFS: bool file_exists(self,
        // String)` clobbered `std.fs::file_exists(cstr) -> bool` whenever
        // both were in scope).
        let def_id = match self.scopes.lookup_def_by_span(&func.name.node, func.name.span) {
            Some(id) => id,
            None => return,
        };

        // Only process Function defs
        if self.scopes.get_def(def_id).kind != DefKind::Function {
            return;
        }

        // Resolve return type. Track P: propagate NonDerefContainer[Trait]
        // Err on function return / param declarations.
        let return_type = match super::types::ast_type_to_resolved(
            &func.return_type.node,
            func.return_type.span,
            self.scopes,
            self.types,
        ) {
            Ok(tid) => tid,
            Err(e) => {
                self.error(e.kind, e.span);
                self.types.void_id
            }
        };

        // Resolve parameter types
        let mut param_types = Vec::new();
        for param in &func.params {
            let type_id = match super::types::ast_type_to_resolved(
                &param.node.type_.node,
                param.node.type_.span,
                self.scopes,
                self.types,
            ) {
                Ok(tid) => tid,
                Err(e) => {
                    self.error(e.kind, e.span);
                    self.types.error_id
                }
            };
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
        // D29/A31: a bare `!` signature (`int f()!:`) is the reserved inferred-
        // error-set spelling. It parses (the grammar locks now) but is teaching-
        // rejected until A31 lands — steer the user to `throws E`.
        if let crate::parser::ast::ThrowsSpec::Inferred(bang_span) = &func.throws {
            self.error(SemanticErrorKind::InferredThrowsUnsupported, *bang_span);
        }
        // R5: default-param expressions are definition-site values evaluated in a
        // non-propagating context — a bare fallible call is E_MissingFallibleMark
        // (same discipline as a statement discard). Saved/restored so a default
        // cannot pollute the body walk's fallible_call_marked one-shot.
        {
            let saved_marked = self.fallible_call_marked;
            let saved_throws = self.current_function_throws;
            self.current_function_throws = false;
            self.fallible_call_marked = false;
            for p in &func.params {
                if let Some(def_expr) = &p.node.default {
                    let _ = self.infer_expr(def_expr);
                }
            }
            self.current_function_throws = saved_throws;
            self.fallible_call_marked = saved_marked;
        }
        self.current_function_throws = func.throws.declares_throws();
        // Snag #11: resolve the CALLER's error type (`throws E`) so the
        // auto-propagation chokepoints can gate cross-error propagation. The
        // boolean `current_function_throws` is not enough — we need the
        // resolved TypeId of E to compare against the callee's error type.
        // D26 auto-infer is transparent here: the pre-`collect_top_level`
        // rewrite pass mutated `func.throws` to `Explicit(ArithError)`, so
        // `explicit_type()` returns Some as if the user had written it.
        self.current_fn_throws_type_id = func.throws.explicit_type().and_then(|t| {
            super::types::ast_type_to_resolved(&t.node, t.span, self.scopes, self.types).ok()
        });
        self.current_function_is_async = func.qualifiers.is_async;
        self.loop_depth = 0;

        // main() can only throw int (the process exit code)
        if func.name.node == "main" {
            if let Some(throws_type) = func.throws.explicit_type() {
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

        // `noreturn` + `throws` is a contradiction at the declaration: a
        // `throw` RETURNS control to the caller via the error channel, but
        // callers type a noreturn call as `Never` and the IR emits
        // `unreachable` right after it. (Extern bodies included — the
        // combination is a lie regardless of where the body lives.)
        if func.qualifiers.is_noreturn {
            if let Some(throws) = func.throws.explicit_type() {
                self.error(
                    SemanticErrorKind::NoreturnWithThrows {
                        function: func.name.node.clone(),
                    },
                    throws.span,
                );
            }
        }

        match &func.body {
            FunctionBody::Block(block) => {
                // R4: a function BLOCK body's tail is never an implicit return
                // (explicit `return` required) — its value is dropped, so a bare
                // fallible call at the tail of a void fn is a silent discard.
                let prev_dropped = std::mem::replace(&mut self.tail_value_dropped, true);
                self.check_block(block);
                self.tail_value_dropped = prev_dropped;
                if func.qualifiers.is_noreturn {
                    // A noreturn body must DIVERGE: terminate on every path
                    // AND contain no `return` at all (`block_terminates`
                    // counts `return` as terminating — here that is exactly
                    // the lie: callers run into `unreachable` after the call).
                    if !self.block_terminates(block) || block_contains_return(block) {
                        self.error(
                            SemanticErrorKind::NoreturnBodyReturns {
                                function: func.name.node.clone(),
                            },
                            func.name.span,
                        );
                    }
                } else {
                    // Definite-return analysis: a non-void function must not
                    // be able to fall off the end of its body.
                    self.check_definite_return(func, block, return_type);
                }
            }
            FunctionBody::Expression(expr) => {
                // D23 §5.1: an expression body is equivalent to a block body
                // with `return`, so type the tail through the SAME path as
                // `Stmt::Return` — set the declared return type as the hint and
                // apply the auto-prop / collection / whole-`Result`-capture
                // guards. Widening: an expr-body `throws` tail now peels
                // (propagating context) / captures (`Result`-typed return) /
                // rejects with `E_UnhandledThrows` exactly as the block-body
                // form does, instead of the old unconditional `unify` that leaked.
                let expr_type = self.check_return_value(Some(return_type), expr);
                // Expression-bodied noreturn: the body must itself diverge
                // (type `Never`) — `noreturn void e(): print(1)` is the same
                // lie-path as a block body that falls off the end.
                if func.qualifiers.is_noreturn
                    && self.resolve_type(expr_type) != self.types.never_id
                {
                    self.error(
                        SemanticErrorKind::NoreturnBodyReturns {
                            function: func.name.node.clone(),
                        },
                        func.name.span,
                    );
                }
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        self.current_return_type = None;
        self.current_function_throws = false;
        self.current_fn_throws_type_id = None;
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

/// Do the unguarded arms cover both `true` and `false` literals? (Bool
/// scrutinee exhaustiveness for the definite-return analysis.)
fn bool_arms_cover(arms: &[MatchItem]) -> bool {
    let mut saw = [false, false];
    for arm in arms.iter().filter_map(|i| i.arm()) {
        if arm.guard.is_some() {
            continue;
        }
        collect_bool_literals(&arm.pattern.node, &mut saw);
    }
    saw[0] && saw[1]
}

fn collect_bool_literals(pattern: &Pattern, saw: &mut [bool; 2]) {
    match pattern {
        Pattern::Literal(e) => {
            if let Expr::BoolLiteral(b) = &e.node {
                saw[usize::from(*b)] = true;
            }
        }
        Pattern::Or(alts) => {
            for p in alts {
                collect_bool_literals(&p.node, saw);
            }
        }
        _ => {}
    }
}

/// Is this pattern a syntactic catch-all (matches any value of any type)?
/// Used by the definite-return analysis for NON-enum scrutinees, where any
/// binding acts as a catch-all. (Enum scrutinees go through the richer
/// `collect_covered_variants`, which also knows variant-name bindings.)
fn pattern_is_catchall_syntactic(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Rest | Pattern::Binding(_) => true,
        Pattern::Or(alts) => alts.iter().any(|p| pattern_is_catchall_syntactic(&p.node)),
        Pattern::Tuple(elems) => elems.iter().all(|p| pattern_is_catchall_syntactic(&p.node)),
        _ => false,
    }
}

/// Does this loop body contain a `break` that exits THIS loop? Recurses into
/// nested non-loop constructs; stops at nested loops (their `break`s bind to
/// the inner loop). Closures cannot `break` an enclosing loop, so expression
/// recursion is limited to block-shaped expressions.
fn block_has_loop_break(block: &Block) -> bool {
    block.stmts.iter().any(|s| stmt_has_loop_break(&s.node))
}

fn stmt_has_loop_break(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Break => true,
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            block_has_loop_break(then_body)
                || elif_branches.iter().any(|(_, b)| block_has_loop_break(b))
                || else_body.as_ref().is_some_and(block_has_loop_break)
        }
        Stmt::Match { arms, else_arm, .. } => {
            arms.iter().any(|item| match item {
                MatchItem::Arm(arm) => expr_has_loop_break(&arm.body.node),
                MatchItem::MetaFor { arm_template, .. } => {
                    expr_has_loop_break(&arm_template.body.node)
                }
            }) || else_arm.as_ref().is_some_and(block_has_loop_break)
        }
        Stmt::With { body, .. }
        | Stmt::NamedScope { body, .. } => block_has_loop_break(body),
        Stmt::Select { arms, else_arm } => {
            arms.iter().any(|a| block_has_loop_break(&a.body))
                || else_arm.as_ref().is_some_and(block_has_loop_break)
        }
        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            block_has_loop_break(then_body)
                || elif_branches.iter().any(|(_, b)| block_has_loop_break(b))
                || else_body.as_ref().is_some_and(block_has_loop_break)
        }
        Stmt::MetaFor { body, .. } => block_has_loop_break(body),
        Stmt::MetaMatch { arms, else_arm, .. } => {
            arms.iter().any(|(_, b)| block_has_loop_break(b))
                || else_arm.as_ref().is_some_and(block_has_loop_break)
        }
        Stmt::MetaWhile { body, .. } => block_has_loop_break(body),
        Stmt::Expr(e) => expr_has_loop_break(&e.node),
        // A nested loop's BODY captures its own breaks (opaque here), but
        // its `else` clause is NOT part of the loop for break-binding: a
        // `break` inside a loop's `else` exits the ENCLOSING loop (§6.12).
        // (`loop` has no else clause.)
        Stmt::While { else_body, .. } | Stmt::For { else_body, .. } => {
            else_body.as_ref().is_some_and(block_has_loop_break)
        }
        // Everything else can't contain a statement-level break that binds
        // to the enclosing loop.
        _ => false,
    }
}

fn expr_has_loop_break(expr: &Expr) -> bool {
    match expr {
        Expr::Block(b) => block_has_loop_break(b),
        Expr::Do { body, .. } => block_has_loop_break(body),
        _ => false,
    }
}

/// Does this `noreturn`-function body contain a `return` that belongs to
/// the FUNCTION? Unlike `break`, a `return` inside a nested loop still
/// returns from the function, so the walk recurses into every statement
/// body. Expression recursion is limited to block-shaped expressions
/// (`Expr::Block` / `Expr::Do`, mirroring `expr_has_loop_break`) and
/// NEVER enters `Expr::Closure` / `Expr::ImplicitClosure` — a `return`
/// inside a closure returns from the closure, not the enclosing function.
fn block_contains_return(block: &Block) -> bool {
    block.stmts.iter().any(|s| stmt_contains_return(&s.node))
}

fn stmt_contains_return(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(_) => true,
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            block_contains_return(then_body)
                || elif_branches.iter().any(|(_, b)| block_contains_return(b))
                || else_body.as_ref().is_some_and(block_contains_return)
        }
        Stmt::While { body, else_body, .. } | Stmt::For { body, else_body, .. } => {
            block_contains_return(body)
                || else_body.as_ref().is_some_and(block_contains_return)
        }
        Stmt::Loop { body } => block_contains_return(body),
        Stmt::Match { arms, else_arm, .. } => {
            arms.iter().any(|item| match item {
                MatchItem::Arm(arm) => expr_contains_return(&arm.body.node),
                MatchItem::MetaFor { arm_template, .. } => {
                    expr_contains_return(&arm_template.body.node)
                }
            }) || else_arm.as_ref().is_some_and(block_contains_return)
        }
        Stmt::With { body, .. }
        | Stmt::NamedScope { body, .. } => block_contains_return(body),
        Stmt::Select { arms, else_arm } => {
            arms.iter().any(|a| block_contains_return(&a.body))
                || else_arm.as_ref().is_some_and(block_contains_return)
        }
        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            block_contains_return(then_body)
                || elif_branches.iter().any(|(_, b)| block_contains_return(b))
                || else_body.as_ref().is_some_and(block_contains_return)
        }
        Stmt::MetaFor { body, .. } => block_contains_return(body),
        Stmt::MetaMatch { arms, else_arm, .. } => {
            arms.iter().any(|(_, b)| block_contains_return(b))
                || else_arm.as_ref().is_some_and(block_contains_return)
        }
        Stmt::MetaWhile { body, .. } => block_contains_return(body),
        Stmt::Expr(e) => expr_contains_return(&e.node),
        _ => false,
    }
}

fn expr_contains_return(expr: &Expr) -> bool {
    match expr {
        Expr::Block(b) => block_contains_return(b),
        Expr::Do { body, .. } => block_contains_return(body),
        // NEVER `Expr::Closure` / `Expr::ImplicitClosure`: their `return`
        // binds to the closure body, not the enclosing function.
        _ => false,
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
/// - method_resolutions: method span start → `MethodResolution` (D36:
///   extended value carries the resolved DefId + optional auto-deref
///   wrapper kind for borrow-checker origin tracking + IR lowering)
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
) -> (FxHashMap<Span, TypeId>, FxHashMap<usize, super::MethodResolution>, FxHashMap<usize, Vec<Type>>, FxHashMap<usize, Vec<Type>>, FxHashMap<Span, DefId>) {
    let mut checker = TypeChecker::new(scopes, types, traits, resolution_map, function_info, enum_variants, struct_fields, function_body_scopes, struct_generic_bounds);

    // Pre-pass: register function signatures so callers can infer return types.
    // This must run before body checking so that e.g. `auto x = imported_fn()`
    // can resolve the function's type.
    register_signatures_recursive(&mut checker, &module.items);

    check_items_recursive_tc(&mut checker, &module.items);

    // Reject module-level `const`s whose initializer is not a compile-time
    // constant (drives off the real `eval_const_expr`, mirroring the lowering
    // const-fold loop — one source of truth, no AST shadow).
    check_module_const_foldability(&mut checker, &module.items);

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
    (checker.expr_types, checker.method_resolutions, checker.inferred_method_targs, checker.inferred_call_targs, checker.from_conversions)
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
            Stmt::Return(Some(e)) => walk_expr(e, inferred),
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
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                walk_expr(condition, inferred);
                walk_expr(then_branch, inferred);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond, inferred);
                    walk_expr(body, inferred);
                }
                if let Some(eb) = else_branch {
                    walk_expr(eb, inferred);
                }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s, inferred); }
                if let Some(en) = end { walk_expr(en, inferred); }
            }
            Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner, inferred),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs, inferred);
                walk_expr(rhs, inferred);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                walk_expr(body, inferred);
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
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
            Stmt::Return(Some(e)) => walk_expr(e, inferred),
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
            Stmt::Loop { body } | Stmt::NamedScope { body, .. } => {
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
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                walk_expr(condition, inferred);
                walk_expr(then_branch, inferred);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond, inferred);
                    walk_expr(body, inferred);
                }
                if let Some(eb) = else_branch { walk_expr(eb, inferred); }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s, inferred); }
                if let Some(en) = end { walk_expr(en, inferred); }
            }
            Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner, inferred),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs, inferred);
                walk_expr(rhs, inferred);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                walk_expr(body, inferred);
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
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
    use std::collections::HashMap;

    // One source of truth for the rewrite's expected-type knowledge: a
    // free-function signature map (fn name → param declared types) so the
    // call-argument position can recover a param's type from the AST without a
    // typecheck-resolved TypeId. The Set/Dict generic args travel intact as
    // AST `Type`s — the very thing the swap needs — so the SAME decision
    // (`rewrite_collect`) serves EVERY position: VarDecl RHS, return,
    // assignment, and call-arg. Earlier this rewrite fired only at VarDecl,
    // leaving `return it.collect()` / `x = it.collect()` / `f(it.collect())`
    // into a Set/Dict as a type error; threading the expected type closes that
    // gap in all positions.
    fn collect_sigs(items: &[Spanned<Item>], sigs: &mut HashMap<String, Vec<Type>>) {
        for item in items {
            match &item.node {
                Item::Module { items: inner, .. } => collect_sigs(inner, sigs),
                Item::Function(f) => {
                    let ptys = f.params.iter().map(|p| p.node.type_.node.clone()).collect();
                    sigs.insert(f.name.node.clone(), ptys);
                }
                _ => {}
            }
        }
    }
    let mut sigs: HashMap<String, Vec<Type>> = HashMap::new();
    collect_sigs(&module.items, &mut sigs);

    // The single decision: if `value` is a top-level `.collect()` and the
    // expected type is `Set[T]` / `HashSet[T]` (→ `to_set`) or `Dict[K, V]` /
    // `HashMap[K, V]` (→ `to_dict[K, V]`, lifting K,V), swap the method. Reads
    // the expected type's generic args directly (typed metadata, no
    // name-matching of the receiver).
    fn rewrite_collect(value: &mut Spanned<Expr>, expected: &Type) {
        if let Expr::MethodCall { method, generic_args, .. } = &mut value.node {
            if method.node == "collect" {
                if is_set_type(expected) {
                    method.node = "to_set".to_string();
                } else if let Some(kv) = dict_kv_args(expected) {
                    method.node = "to_dict".to_string();
                    *generic_args = Some(kv);
                }
            }
        }
    }

    fn walk_items(items: &mut [Spanned<Item>], sigs: &HashMap<String, Vec<Type>>) {
        for item in items {
            match &mut item.node {
                Item::Module { items: inner, .. } => walk_items(inner, sigs),
                Item::Function(f) => walk_function(f, sigs),
                Item::Equip(eq) => {
                    for m in &mut eq.items {
                        walk_function(&mut m.node, sigs);
                    }
                }
                Item::Trait(td) => {
                    for ti in &mut td.items {
                        if let TraitItem::Method(m) = &mut ti.node {
                            walk_function(m, sigs);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    fn walk_function(f: &mut FunctionDef, sigs: &HashMap<String, Vec<Type>>) {
        let ret_ty = f.return_type.node.clone();
        match &mut f.body {
            FunctionBody::Block(b) => walk_block(b, &ret_ty, sigs),
            FunctionBody::Expression(e) => {
                // An expression-body fn's value IS its return — expect the
                // declared return type.
                rewrite_collect(e, &ret_ty);
                walk_expr(e, sigs);
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }
    fn walk_block(b: &mut Block, ret_ty: &Type, sigs: &HashMap<String, Vec<Type>>) {
        // Track declared types of in-scope locals so an `Assign` to a typed
        // local recovers its expected type.
        let mut locals: HashMap<String, Type> = HashMap::new();
        for stmt in &mut b.stmts {
            walk_stmt(&mut stmt.node, ret_ty, &mut locals, sigs);
        }
    }
    fn walk_stmt(
        s: &mut Stmt,
        ret_ty: &Type,
        locals: &mut HashMap<String, Type>,
        sigs: &HashMap<String, Vec<Type>>,
    ) {
        match s {
            Stmt::VarDecl { type_, pattern, value, .. } => {
                rewrite_collect(value, &type_.node);
                if let Pattern::Binding(name) = &pattern.node {
                    locals.insert(name.clone(), type_.node.clone());
                }
                walk_expr(value, sigs);
            }
            Stmt::Return(Some(e)) => {
                rewrite_collect(e, ret_ty);
                walk_expr(e, sigs);
            }
            Stmt::Assign { target, value } => {
                // Assignment to a bare local whose declared type we tracked is
                // the local's type; otherwise no expectation (descend only).
                if let Expr::Identifier(name) = &target.node {
                    if let Some(ty) = locals.get(name) {
                        rewrite_collect(value, &ty.clone());
                    }
                }
                walk_expr(target, sigs);
                walk_expr(value, sigs);
            }
            Stmt::Expr(e) | Stmt::Throw(e) => walk_expr(e, sigs),
            Stmt::CompoundAssign { target, value, .. } => {
                walk_expr(target, sigs);
                walk_expr(value, sigs);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                walk_expr(condition, sigs);
                walk_block(then_body, ret_ty, sigs);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond, sigs);
                    walk_block(body, ret_ty, sigs);
                }
                if let Some(eb) = else_body { walk_block(eb, ret_ty, sigs); }
            }
            Stmt::While { condition, body, else_body } => {
                walk_expr(condition, sigs);
                walk_block(body, ret_ty, sigs);
                if let Some(eb) = else_body { walk_block(eb, ret_ty, sigs); }
            }
            Stmt::For { iterable, body, else_body, .. } => {
                walk_expr(iterable, sigs);
                walk_block(body, ret_ty, sigs);
                if let Some(eb) = else_body { walk_block(eb, ret_ty, sigs); }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                walk_expr(scrutinee, sigs);
                for item in arms {
                    if let crate::parser::ast::MatchItem::Arm(arm) = item {
                        walk_expr(&mut arm.body, sigs);
                        if let Some(g) = &mut arm.guard { walk_expr(g, sigs); }
                    }
                }
                if let Some(b) = else_arm { walk_block(b, ret_ty, sigs); }
            }
            Stmt::With { bindings, body } => {
                for binding in bindings { walk_expr(&mut binding.expr, sigs); }
                walk_block(body, ret_ty, sigs);
            }
            Stmt::Loop { body } | Stmt::NamedScope { body, .. } => {
                walk_block(body, ret_ty, sigs);
            }
            _ => {}
        }
    }
    fn walk_expr(e: &mut Spanned<Expr>, sigs: &HashMap<String, Vec<Type>>) {
        match &mut e.node {
            Expr::MethodCall { receiver, args, .. } => {
                walk_expr(receiver, sigs);
                for a in args { walk_expr(&mut a.node.value, sigs); }
            }
            Expr::Call { callee, args, .. } => {
                // Free-call args: each arg's expected type is the callee's
                // declared param type when the callee is a known free function.
                let callee_sig = if let Expr::Identifier(name) = &callee.node {
                    sigs.get(name).cloned()
                } else {
                    None
                };
                walk_expr(callee, sigs);
                for (i, a) in args.iter_mut().enumerate() {
                    if let Some(ptys) = &callee_sig {
                        if let Some(pty) = ptys.get(i) {
                            rewrite_collect(&mut a.node.value, pty);
                        }
                    }
                    walk_expr(&mut a.node.value, sigs);
                }
            }
            Expr::StructLiteral { args, .. } => {
                for a in args { walk_expr(a, sigs); }
            }
            Expr::BinaryOp { left, right, .. } => {
                walk_expr(left, sigs);
                walk_expr(right, sigs);
            }
            Expr::UnaryOp { operand, .. } => walk_expr(operand, sigs),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                walk_expr(object, sigs);
            }
            Expr::Index { object, index } => {
                walk_expr(object, sigs);
                walk_expr(index, sigs);
            }
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                walk_expr(condition, sigs);
                walk_expr(then_branch, sigs);
                for (cond, body) in elif_branches.iter_mut() {
                    walk_expr(cond, sigs);
                    walk_expr(body, sigs);
                }
                if let Some(eb) = else_branch { walk_expr(eb, sigs); }
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { walk_expr(s, sigs); }
                if let Some(e) = end { walk_expr(e, sigs); }
            }
            Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::MutableBorrow { expr: inner }
            | Expr::OptionalChain { object: inner, .. } => walk_expr(inner, sigs),
            Expr::DefaultOp { lhs, rhs } => {
                walk_expr(lhs, sigs);
                walk_expr(rhs, sigs);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => walk_expr(body, sigs),
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
                for e in elems { walk_expr(e, sigs); }
            }
            Expr::Block(b) => {
                // A bare nested block carries no return expectation of its own;
                // an `Inferred` sentinel never matches the Set/Dict arms.
                walk_block(b, &Type::Inferred, sigs);
            }
            Expr::StringLiteral(_, interp_exprs) => {
                for ie in interp_exprs.iter_mut() { walk_expr(ie, sigs); }
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
    walk_items(&mut module.items, &sigs);
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
                // Push an EquipBlock scope unconditionally — mirrors `resolve.rs:539`'s
                // symmetric shape. The generic params (if any) are defined inside this scope.
                // Today the inverted span-first lookup in `register_function_signature` (commit
                // 27230b43, 2026-05-17) makes the scope-during-registration irrelevant for the
                // signature-write path, but the resolve-pass / typecheck-pass asymmetry was a
                // latent footgun: any future code reading `scopes.current()` or
                // `scopes.lookup_within_function(scopes.current_fn_scope(), ...)` inside
                // `register_function_signature` would silently see the root scope for
                // non-generic equip methods. Push unconditionally to close that gap.
                checker.scopes.push_scope(ScopeKind::EquipBlock { self_type: None });
                if let Some(generics) = &impl_block.generic_params {
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
                checker.scopes.pop_scope();
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
                // Type-check imported module code. Errors from imported modules
                // are real (non-exhaustive matches, body type errors) and must
                // surface — the language spec REQUIRES exhaustive match, so a
                // bug in a library is still a bug. (Previously these were
                // truncated away, silently accepting miscompiles in imports.)
                check_items_recursive_tc(checker, inner);
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
                checker.current_equip_generics = equip_generic_names(impl_block);
                for method in &impl_block.items {
                    checker.check_function(&method.node);
                }
                checker.current_self_type = None;
                checker.current_equip_generics = Vec::new();
                if has_generics {
                    checker.scopes.pop_scope();
                }
            }
            Item::ConstDecl(c) => {
                let value_ty = checker.infer_expr(&c.value);
                // Non-foldable const initializers are rejected module-wide by
                // `check_module_const_foldability` (driven off the real
                // `eval_const_expr`, mirroring the lowering const-fold loop) —
                // not a per-item AST shadow.
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
                // D10(a): a module-level `static G = &BASE` is the same
                // named-`&`-bind class as the local form — rejected (see
                // `type_utils::expr_is_borrow_bind`).
                if crate::semantic::type_utils::expr_is_borrow_bind(&s.value.node) {
                    checker.error(SemanticErrorKind::LocalBorrowBind, s.value.span);
                }
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
                // R4: test/bench/suite bodies drop their tail value (fn-body-like).
                checker.tail_value_dropped = true;
                checker.check_block(&t.body);
                checker.current_return_type = None;
            }
            Item::Bench(b) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                // R4: test/bench/suite bodies drop their tail value (fn-body-like).
                checker.tail_value_dropped = true;
                checker.check_block(&b.body);
                checker.current_return_type = None;
            }
            Item::SuiteSetup(s) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                // R4: test/bench/suite bodies drop their tail value (fn-body-like).
                checker.tail_value_dropped = true;
                checker.check_block(&s.body);
                checker.current_return_type = None;
            }
            Item::SuiteTeardown(s) => {
                checker.current_return_type = Some(checker.types.void_id);
                checker.current_function_throws = false;
                // R4: test/bench/suite bodies drop their tail value (fn-body-like).
                checker.tail_value_dropped = true;
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
        let errors = check("directive strip-asserts\nvoid main():\n    pass\n");
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
