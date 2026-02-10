use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::{DefId, TypeId};
use super::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap};
use super::scope::{DefKind, ScopeTable};
use super::traits::TraitRegistry;
use super::types::{ResolvedType, TypeTable};

/// Type checker with bidirectional inference.
struct TypeChecker<'a> {
    scopes: &'a mut ScopeTable,
    types: &'a mut TypeTable,
    traits: &'a TraitRegistry,
    resolution_map: &'a ResolutionMap,
    function_info: &'a FxHashMap<DefId, FunctionInfo>,
    enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
    errors: Vec<SemanticError>,
    /// Substitution map: type variable ID -> resolved type ID.
    substitutions: FxHashMap<u32, TypeId>,
    next_type_var: u32,
    /// The return type of the current function being checked.
    current_return_type: Option<TypeId>,
    /// Whether the current function has `throws`.
    current_function_throws: bool,
    /// Type variable for implicit `it` parameter inside ImplicitClosure.
    implicit_it_type: Option<TypeId>,
}

impl<'a> TypeChecker<'a> {
    fn new(
        scopes: &'a mut ScopeTable,
        types: &'a mut TypeTable,
        traits: &'a TraitRegistry,
        resolution_map: &'a ResolutionMap,
        function_info: &'a FxHashMap<DefId, FunctionInfo>,
        enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
    ) -> Self {
        Self {
            scopes,
            types,
            traits,
            resolution_map,
            function_info,
            enum_variants,
            errors: Vec::new(),
            substitutions: FxHashMap::default(),
            next_type_var: 0,
            current_return_type: None,
            current_function_throws: false,
            implicit_it_type: None,
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

    /// Return a human-readable name for a resolved type.
    /// Uses the definition name for `Defined`/`Generic` types instead of the
    /// unhelpful `"<defined>"` from `TypeTable::display`.
    fn describe_resolved_type(&self, type_id: TypeId) -> String {
        match self.types.get(type_id) {
            ResolvedType::Defined(def_id) => self.scopes.get_def(*def_id).name.clone(),
            ResolvedType::Generic(def_id, _) => self.scopes.get_def(*def_id).name.clone(),
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
            _ => {
                if a_ty != b_ty {
                    self.error(
                        SemanticErrorKind::TypeMismatch {
                            expected: self.types.display(a),
                            found: self.types.display(b),
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
            Expr::IntLiteral(_) => {
                // Default to int; could be constrained by context
                self.types.int_id
            }
            Expr::FloatLiteral(_) => self.types.float_id,
            Expr::BoolLiteral(_) => self.types.bool_id,
            Expr::CharLiteral(_) => self.types.char_id,
            Expr::StringLiteral(s) => {
                use crate::lexer::token::StringSegment;
                for seg in &s.segments {
                    if let StringSegment::Interpolation(var_name) = seg {
                        if let Some(def_id) = self.scopes.lookup_by_name_anywhere(var_name) {
                            let def = self.scopes.get_def(def_id);
                            if let Some(type_id) = def.type_id {
                                match self.types.get(type_id) {
                                    ResolvedType::Primitive(_) | ResolvedType::Void => {}
                                    ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                                        let type_name = &self.scopes.get_def(*def_id).name;
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
                self.types.string_id
            }
            Expr::NoneLiteral => {
                // None is Option[?T] — for now return error type
                self.types.error_id
            }

            Expr::Identifier(_) => {
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
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
                // Type of self depends on enclosing impl block
                self.types.error_id
            }

            Expr::It => {
                // Implicit closure parameter — use type from enclosing ImplicitClosure
                self.implicit_it_type.unwrap_or(self.types.error_id)
            }

            Expr::Path { segments } => {
                if let Some(first) = segments.first() {
                    if let Some(&def_id) = self.resolution_map.get(&first.span.start) {
                        let def = self.scopes.get_def(def_id);
                        match def.kind {
                            DefKind::Enum => {
                                // Could be an enum variant access: Option.None
                                self.types.insert(ResolvedType::Defined(def_id))
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
                let operand_type = self.infer_expr(operand);
                match op {
                    UnaryOp::Neg => operand_type, // same numeric type
                    UnaryOp::Not => self.types.bool_id,
                    UnaryOp::Deref => operand_type, // deref is ownership, checked later
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
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                        self.unify(left_type, right_type, expr.span)
                    }
                }
            }

            Expr::Call { callee, generic_args, args, .. } => {
                let callee_type = self.infer_expr(callee);
                let resolved = self.resolve_type(callee_type);

                // Check where-clause trait bounds for generic calls
                if let Some(type_args) = generic_args {
                    if let Expr::Identifier(_) = &callee.node {
                        if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                            self.check_trait_bounds(def_id, type_args, expr.span);
                        }
                    }
                }

                // Try to look up FunctionInfo for named args / default params
                let func_info = if let Expr::Identifier(_) = &callee.node {
                    self.resolution_map.get(&callee.span.start)
                        .and_then(|def_id| self.function_info.get(def_id))
                } else {
                    None
                };

                // Check if callee is a function
                match self.types.get(resolved).clone() {
                    ResolvedType::Function {
                        params,
                        return_type,
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
                                let arg_type = self.infer_expr(&arg.node.value);
                                self.unify(param_type, arg_type, arg.span);
                            }
                        }
                        return_type
                    }
                    ResolvedType::Error => {
                        // Check if callee is a struct/newtype constructor
                        if let Expr::Identifier(_) = &callee.node {
                            if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
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
                                            return self.types.insert(ResolvedType::Generic(def_id, resolved_args));
                                        }
                                        // Box(value) → Box[T] where T is inferred from the argument
                                        if def_name == "Box" && arg_types.len() == 1 {
                                            return self.types.insert(ResolvedType::Generic(def_id, arg_types));
                                        }
                                        return self.types.insert(ResolvedType::Defined(def_id));
                                    }
                                    _ => {}
                                }
                            }
                        }
                        // Don't cascade — just infer arg types
                        for arg in args {
                            self.infer_expr(&arg.node.value);
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
                                    return self.types.insert(ResolvedType::Generic(def_id, arg_types));
                                }
                                self.types.insert(ResolvedType::Defined(def_id))
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
                        if let Expr::Identifier(_) = &callee.node {
                            if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                                let def = self.scopes.get_def(def_id);
                                match def.kind {
                                    DefKind::Struct | DefKind::Variant | DefKind::Newtype => {
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
                                        return self.types.insert(ResolvedType::Defined(def_id));
                                    }
                                    DefKind::Function => {
                                        // Function without resolved type — just infer args
                                        for arg in args {
                                            self.infer_expr(&arg.node.value);
                                        }
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
                let receiver_type = self.infer_expr(receiver);
                let resolved_receiver = self.resolve_type(receiver_type);

                // Try to resolve method via trait registry
                if let Some((_def_id, sig)) =
                    self.traits.resolve_method(resolved_receiver, &method.node)
                {
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
                        ret_type
                    } else {
                        // Method not found — check built-in type methods
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        if let Some(ret_type) = self.builtin_method_type(resolved_receiver, &method.node) {
                            ret_type
                        } else {
                            self.types.error_id
                        }
                    }
                }
            }

            Expr::FieldAccess { object, field: _ } => {
                let object_type = self.infer_expr(object);
                let _ = self.resolve_type(object_type);
                // Field resolution requires knowing the struct definition.
                // For now, return error type — full field checking requires struct info.
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
                let _object_type = self.infer_expr(object);
                let _index_type = self.infer_expr(index);
                self.types.error_id // element type requires more info
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

            Expr::NilCoalescing { lhs, rhs } => {
                let _lhs_type = self.infer_expr(lhs);
                let rhs_type = self.infer_expr(rhs);
                rhs_type // unwrapped type
            }

            Expr::Try { expr: inner } => {
                self.infer_expr(inner);
                self.types.error_id // unwrapped Result type
            }

            Expr::Move { expr: inner }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Deref { expr: inner } => {
                self.infer_expr(inner) // ownership modifiers don't change the type
            }

            Expr::Await { expr: inner } => {
                self.infer_expr(inner);
                self.types.error_id // unwrapped Future[T] -> T
            }

            Expr::Spawn { expr: inner } => {
                self.infer_expr(inner);
                self.types.error_id // Task[T]
            }

            Expr::TryCapture { expr: inner } => {
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
                    let arm_type = self.infer_expr(&arm.body);
                    result_type = self.unify(result_type, arm_type, arm.body.span);
                }

                if let Some(else_arm) = else_arm {
                    let else_type = self.infer_expr(else_arm);
                    result_type = self.unify(result_type, else_type, else_arm.span);
                }

                self.check_match_exhaustiveness(scrutinee_type, arms, else_arm.is_some(), expr.span);

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
                        param_types.push(self.fresh_type_var());
                    }
                }
                let body_type = self.infer_expr(body);
                self.types.insert(ResolvedType::Function {
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

            Expr::StructLiteral { name, args } => {
                // Resolve struct type
                if let Some(&def_id) = self.resolution_map.get(&name.span.start) {
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
                    self.types.insert(ResolvedType::Defined(def_id))
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
        }
    }

    // ─── Statement Checking ────────────────────────────────

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                type_, pattern, value, ..
            } => {
                let value_type = self.infer_expr(value);

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
                        // Explicit type — check value matches
                        if let Ok(declared_type) = super::types::ast_type_to_resolved(
                            &type_.node,
                            type_.span,
                            self.scopes,
                            self.types,
                        ) {
                            // Allow assigning array literals to collection types
                            // (e.g. Vector[int] v = [1, 2, 3])
                            if !self.is_collection_assignment(declared_type, value_type) {
                                self.unify(declared_type, value_type, value.span);
                            }
                            Some(declared_type)
                        } else {
                            None
                        }
                    }
                };

                // Write the resolved type back to the pattern binding's DefInfo
                if let Some(type_id) = resolved_type {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                            self.scopes.get_def_mut(def_id).type_id = Some(type_id);
                        }
                    }
                }
            }

            Stmt::Expr(expr) => {
                self.infer_expr(expr);
            }

            Stmt::Assign { target, value } => {
                let target_type = self.infer_expr(target);
                let value_type = self.infer_expr(value);
                self.unify(target_type, value_type, value.span);
            }

            Stmt::CompoundAssign { target, value, .. } => {
                let target_type = self.infer_expr(target);
                let value_type = self.infer_expr(value);
                self.unify(target_type, value_type, value.span);
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let expr_type = self.infer_expr(expr);
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
            }

            Stmt::Assert { condition, message } => {
                self.infer_expr(condition);
                if let Some(msg) = message {
                    self.infer_expr(msg);
                }
            }

            Stmt::Continue | Stmt::Pass => {}

            Stmt::For {
                iterable, body, else_body, ..
            } => {
                self.infer_expr(iterable);
                self.check_block(body);
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
                self.check_block(body);
                if let Some(else_body) = else_body {
                    self.check_block(else_body);
                }
            }

            Stmt::Loop { body } => {
                self.check_block(body);
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                let cond_type = self.infer_expr(condition);
                self.unify(cond_type, self.types.bool_id, condition.span);
                self.check_block(then_body);

                for (cond, body) in elif_branches {
                    let ct = self.infer_expr(cond);
                    self.unify(ct, self.types.bool_id, cond.span);
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
                for arm in arms {
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

            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.infer_expr(&binding.expr);
                }
                self.check_block(body);
            }

            Stmt::Unsafe { body } => {
                self.check_block(body);
            }

            Stmt::Item(_) => {
                // Nested items are checked at the top level
            }
        }
    }

    /// Check that a match on an enum type covers all variants.
    fn check_match_exhaustiveness(
        &mut self,
        scrutinee_type: TypeId,
        arms: &[MatchArm],
        has_else: bool,
        span: Span,
    ) {
        if has_else {
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
        for arm in arms {
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
        }
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
                if let Some(first_arm) = arms.first() {
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
            if matches!(name.as_str(), "Vector" | "List" | "Array" | "Dict" | "HashMap" | "Map" | "Set" | "HashSet") {
                // Allow any value type (array literal, comprehension, constructor call)
                return matches!(self.types.get(value_resolved),
                    ResolvedType::Array(_, _) | ResolvedType::Error
                );
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
            _ => return None,
        };

        match (type_name.as_str(), method) {
            ("Option", "map") => {
                // (T) -> U, returns Option[U]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                Some(self.types.insert(ResolvedType::Generic(def_id, vec![u_type])))
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
                Some(self.types.insert(ResolvedType::Generic(def_id, vec![u_type, e_type])))
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

            // --- Vector higher-order methods ---
            ("Vector" | "List" | "Array", "filter") => {
                // (T) -> bool, returns Vector[T]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(receiver_type)
            }
            ("Vector" | "List" | "Array", "map") => {
                // (T) -> U, returns Vector[U]
                let closure_type = self.infer_expr(&args.first()?.node.value);
                let u_type = self.extract_fn_return_type(closure_type)?;
                Some(self.types.insert(ResolvedType::Generic(def_id, vec![u_type])))
            }
            ("Vector" | "List" | "Array", "fold") => {
                // args: initial_value, closure (U, T) -> U — returns U
                let init_type = self.infer_expr(&args.first()?.node.value);
                let _ = args.get(1).map(|a| self.infer_expr(&a.node.value));
                Some(init_type)
            }
            ("Vector" | "List" | "Array", "reduce") => {
                // (T, T) -> T, returns T
                let _ = self.infer_expr(&args.first()?.node.value);
                let elem_type = type_args.first().copied()?;
                Some(elem_type)
            }

            // --- Dict higher-order methods ---
            ("Dict" | "HashMap" | "Map", "filter") => {
                // (K, V) -> bool, returns Dict[K,V]
                let _ = self.infer_expr(&args.first()?.node.value);
                Some(receiver_type)
            }
            ("Dict" | "HashMap" | "Map", "fold") => {
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
                    let arg_type = self.infer_expr(&arg.node.value);
                    if pos < param_types.len() {
                        self.unify(param_types[pos], arg_type, arg.span);
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
                    let arg_type = self.infer_expr(&arg.node.value);
                    if i < param_types.len() {
                        self.unify(param_types[i], arg_type, arg.span);
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

    /// Extract the return type from a Function type.
    fn extract_fn_return_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.types.get(type_id) {
            ResolvedType::Function { return_type, .. } => Some(*return_type),
            _ => None,
        }
    }

    /// Check if a method call is on a known built-in type, returning
    /// the return TypeId if so.
    fn builtin_method_type(&self, receiver_type: TypeId, method: &str) -> Option<TypeId> {
        // Determine the base type name and generic type args (if any)
        let (type_name, type_args) = match self.types.get(receiver_type) {
            ResolvedType::Generic(def_id, args) => {
                (self.scopes.get_def(*def_id).name.clone(), args.clone())
            }
            ResolvedType::Defined(def_id) => {
                (self.scopes.get_def(*def_id).name.clone(), vec![])
            }
            ResolvedType::Primitive(PrimitiveType::Str | PrimitiveType::StringType) => {
                ("str".to_string(), vec![])
            }
            _ => return None,
        };

        // Helper: get element type T from Vector[T], fallback to int
        let elem_type = || type_args.first().copied().unwrap_or(self.types.int_id);
        // Helper: get value type V from Dict[K,V], fallback to int
        let val_type = || type_args.get(1).copied().unwrap_or(self.types.int_id);

        match type_name.as_str() {
            "Vector" | "List" | "Array" => match method {
                "push" => Some(self.types.void_id),
                "pop" | "get" | "remove" => Some(elem_type()),
                "set" => Some(self.types.void_id),
                "len" => Some(self.types.int_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                _ => None,
            },
            "Dict" | "HashMap" | "Map" => match method {
                "put" => Some(self.types.void_id),
                "get" => Some(val_type()),
                "contains" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                _ => None,
            },
            "Set" | "HashSet" => match method {
                "add" => Some(self.types.void_id),
                "contains" => Some(self.types.bool_id),
                "len" => Some(self.types.int_id),
                "remove" => Some(self.types.bool_id),
                "clear" => Some(self.types.void_id),
                "is_empty" => Some(self.types.bool_id),
                _ => None,
            },
            "str" | "String" => match method {
                "len" => Some(self.types.int_id),
                _ => None,
            },
            "Option" => match method {
                "unwrap" | "unwrap_or" => Some(elem_type()),
                "is_some" | "is_none" => Some(self.types.bool_id),
                _ => None,
            },
            "Result" => match method {
                "unwrap" | "unwrap_or" => Some(elem_type()),
                "is_ok" | "is_err" => Some(self.types.bool_id),
                _ => None,
            },
            "Box" => match method {
                "get" => Some(elem_type()),
                "set" => Some(self.types.void_id),
                _ => None,
            },
            "File" => match method {
                "read_all" => Some(self.types.string_id),
                "write" => Some(self.types.void_id),
                "close" => Some(self.types.void_id),
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
        if info.where_bounds.is_empty() {
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
        for (param_name, required_traits) in &info.where_bounds {
            if let Some(concrete_type) = param_to_type.get(param_name.as_str()) {
                for trait_name in required_traits {
                    if !self.traits.has_trait_impl_by_name(concrete_type, trait_name) {
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

    fn check_function(&mut self, func: &FunctionDef) {
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

        // Resolve parameter types and write to DefInfo
        for param in &func.params {
            if let Ok(type_id) = super::types::ast_type_to_resolved(
                &param.node.type_.node,
                param.node.type_.span,
                self.scopes,
                self.types,
            ) {
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
            FunctionBody::Declaration => {}
        }

        self.current_return_type = None;
        self.current_function_throws = false;
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
                PrimitiveType::Char => "char",
                PrimitiveType::Void => "void",
                _ => return None,
            };
            Some(s.to_string())
        }
        _ => None,
    }
}

/// Run type checking on the entire module.
pub fn check_module(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &mut TypeTable,
    traits: &TraitRegistry,
    resolution_map: &ResolutionMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    enum_variants: &FxHashMap<DefId, EnumVariantInfo>,
    errors: &mut Vec<SemanticError>,
) {
    let mut checker = TypeChecker::new(scopes, types, traits, resolution_map, function_info, enum_variants);

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
            Item::ConstDecl(c) => {
                checker.infer_expr(&c.value);
            }
            Item::StaticDecl(s) => {
                checker.infer_expr(&s.value);
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

    fn check(source: &str) -> Vec<super::SemanticError> {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze(&module);
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
            "struct Foo:\n    int x\nvoid main():\n    Foo f = Foo(1)\n    print(\"{f}\")\n",
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
            "enum Color:\n    Red()\n    Blue()\nvoid main():\n    Color c = Red()\n    print(\"{c}\")\n",
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
        let errors = check("void main():\n    int x = 42\n    print(\"{x}\")\n");
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

T echo[T](T x) where T is Printable:
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

T echo[T](T x) where T is Printable:
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

T need_ab[T](T x) where T is A + B:
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
}
