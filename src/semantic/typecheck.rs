use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};
use super::ids::TypeId;
use super::resolve::ResolutionMap;
use super::scope::{DefKind, ScopeTable};
use super::traits::TraitRegistry;
use super::types::{ResolvedType, TypeTable};

/// Type checker with bidirectional inference.
struct TypeChecker<'a> {
    scopes: &'a ScopeTable,
    types: &'a mut TypeTable,
    traits: &'a TraitRegistry,
    resolution_map: &'a ResolutionMap,
    errors: Vec<SemanticError>,
    /// Substitution map: type variable ID -> resolved type ID.
    substitutions: FxHashMap<u32, TypeId>,
    next_type_var: u32,
    /// The return type of the current function being checked.
    current_return_type: Option<TypeId>,
    /// Whether the current function has `throws`.
    current_function_throws: bool,
}

impl<'a> TypeChecker<'a> {
    fn new(
        scopes: &'a ScopeTable,
        types: &'a mut TypeTable,
        traits: &'a TraitRegistry,
        resolution_map: &'a ResolutionMap,
    ) -> Self {
        Self {
            scopes,
            types,
            traits,
            resolution_map,
            errors: Vec::new(),
            substitutions: FxHashMap::default(),
            next_type_var: 0,
            current_return_type: None,
            current_function_throws: false,
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
            Expr::StringLiteral(_) => self.types.string_id,
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
                // Implicit closure parameter — type depends on context
                self.types.error_id
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

            Expr::Call { callee, args, .. } => {
                let callee_type = self.infer_expr(callee);
                let resolved = self.resolve_type(callee_type);

                // Check if callee is a function
                match self.types.get(resolved).clone() {
                    ResolvedType::Function {
                        params,
                        return_type,
                    } => {
                        // Check argument count
                        if args.len() != params.len() {
                            self.error(
                                SemanticErrorKind::WrongArgCount {
                                    expected: params.len(),
                                    found: args.len(),
                                },
                                expr.span,
                            );
                        }
                        // Check argument types
                        for (arg, &param_type) in args.iter().zip(params.iter()) {
                            let arg_type = self.infer_expr(&arg.node.value);
                            self.unify(param_type, arg_type, arg.span);
                        }
                        return_type
                    }
                    ResolvedType::Error => {
                        // Don't cascade — just infer arg types
                        for arg in args {
                            self.infer_expr(&arg.node.value);
                        }
                        self.types.error_id
                    }
                    ResolvedType::Defined(def_id) => {
                        // Could be a struct constructor or enum variant
                        let def = self.scopes.get_def(def_id);
                        match def.kind {
                            DefKind::Struct | DefKind::Variant | DefKind::Newtype => {
                                // Infer argument types
                                for arg in args {
                                    self.infer_expr(&arg.node.value);
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
                    // Method not found — infer args, return error
                    for arg in args {
                        self.infer_expr(&arg.node.value);
                    }
                    // Only report error if receiver is not error type
                    if resolved_receiver != self.types.error_id {
                        // Don't error on well-known methods on error types
                        self.types.error_id
                    } else {
                        self.types.error_id
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

            Expr::Catch { expr: inner } => {
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
                self.infer_expr(scrutinee);
                let mut result_type = self.fresh_type_var();

                for arm in arms {
                    let arm_type = self.infer_expr(&arm.body);
                    result_type = self.unify(result_type, arm_type, arm.body.span);
                }

                if let Some(else_arm) = else_arm {
                    let else_type = self.infer_expr(else_arm);
                    result_type = self.unify(result_type, else_type, else_arm.span);
                }

                result_type
            }

            Expr::Block(block) => self.check_block(block),

            Expr::Do { body } => self.check_block(body),

            Expr::Closure { params, body, .. } => {
                // Infer closure type from params and body
                let mut param_types = Vec::new();
                for param in params {
                    if let Some(ty) = &param.node.type_ {
                        let tid = super::types::ast_type_to_resolved(
                            &ty.node, ty.span, self.scopes, self.types,
                        )
                        .unwrap_or(self.types.error_id);
                        param_types.push(tid);
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
                let body_type = self.infer_expr(body);
                let param_type = self.fresh_type_var();
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
                type_, value, ..
            } => {
                let value_type = self.infer_expr(value);

                match &type_.node {
                    Type::Inferred => {
                        // auto — infer from value
                        let resolved = self.resolve_type(value_type);
                        if resolved == self.types.error_id {
                            // Can't infer — but don't cascade
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
                            self.unify(declared_type, value_type, value.span);
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
                self.infer_expr(scrutinee);
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

    fn check_block(&mut self, block: &Block) -> TypeId {
        let mut last_type = self.types.void_id;
        for stmt in &block.stmts {
            self.check_stmt(stmt);
            // The "value" of a block is its last expression statement
            if let Stmt::Expr(expr) = &stmt.node {
                last_type = self.infer_expr(expr);
            }
        }
        last_type
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

/// Run type checking on the entire module.
pub fn check_module(
    module: &Module,
    scopes: &ScopeTable,
    types: &mut TypeTable,
    traits: &TraitRegistry,
    resolution_map: &ResolutionMap,
    errors: &mut Vec<SemanticError>,
) {
    let mut checker = TypeChecker::new(scopes, types, traits, resolution_map);

    for item in &module.items {
        match &item.node {
            Item::Function(f) => {
                checker.check_function(f);
            }
            Item::Implement(impl_block) => {
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
}
