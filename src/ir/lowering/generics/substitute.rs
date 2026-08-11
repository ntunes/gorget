//! AST type substitution and builtin enum injection for generic monomorphization.

use rustc_hash::FxHashMap;

use crate::parser::ast::{self, Expr, Stmt, Type};
use crate::span::Spanned;

/// Public entry point for type substitution (used by functions.rs, traits.rs).
pub fn substitute_type_pub(ty: &Type, subs: &[(String, Type)]) -> Type {
    substitute_type(ty, subs)
}

/// Merge a generic method's type-parameter bindings into an existing
/// substitution list so the METHOD-level bindings SHADOW any equip-level
/// (receiver-struct) binding of the same name.
///
/// Both an adapter struct and a trait-default method can name their closure
/// generic `F` (e.g. `equip [Iter, T, F] FilterIter[...]`'s predicate `F` — a
/// `bool(T)` closure — and the trait-default `map[U, F]`'s map closure `F` — a
/// `U(T)` closure). Because `substitute_type` takes the FIRST match in the flat
/// list, a naive append (equip-scope entries first) lets the outer `F` win,
/// mis-typing the inner one — which typed a map result `bool` and byte-
/// truncated the real `int64_t` at the `Some_0` store. This helper removes any
/// colliding equip-scope entry before pushing each method-scope binding, so the
/// innermost scope wins (correct lexical shadowing).
///
/// One shared path for every equip+method sub-merge so a new call site can't
/// reappear un-shadowed (the retain is a no-op unless a name collides). Call it
/// AFTER computing any equipped-type / `Self` substitution from the equip-only
/// subs, so the receiver's own closure param is preserved.
pub fn merge_method_subs(
    subs: &mut Vec<(String, Type)>,
    method_generic_params: Option<&Spanned<ast::GenericParams>>,
    method_type_args: &[Spanned<Type>],
) {
    let Some(gp) = method_generic_params else {
        return;
    };
    for (param, arg) in gp.node.params.iter().zip(method_type_args.iter()) {
        let name = match &param.node {
            ast::GenericParam::Type { name: s, .. } => s.node.clone(),
            ast::GenericParam::Const { name, .. } => name.node.clone(),
        };
        subs.retain(|(n, _)| n != &name);
        subs.push((name, arg.node.clone()));
    }
}

/// Public entry point for whole-function-body substitution.
///
/// Used by the default-trait-method lowering path (functions.rs) so a body
/// like `return TakeIter[Self, T](self, n)` has every `Self` and `T`
/// rewritten to concrete types BEFORE `lower_block` mangles the
/// struct-constructor type-arg list. Without pre-substitution
/// `mangle_type_for_name(Self)` produces "unknown" and the call site
/// resolves to a phantom `TakeIter__unknown__int64_t` symbol.
pub fn substitute_function_body_pub(
    template: &ast::FunctionDef,
    subs: &[(String, Type)],
) -> ast::FunctionDef {
    substitute_function_body(template, subs)
}

/// Recursively substitute type parameters in an AST type.
pub(super) fn substitute_type(ty: &Type, subs: &[(String, Type)]) -> Type {
    match ty {
        // Self is a keyword type — look for a "Self" entry in subs.
        Type::SelfType => {
            for (param_name, concrete) in subs {
                if param_name == "Self" {
                    return concrete.clone();
                }
            }
            ty.clone()
        }
        Type::Named { name, generic_args } if generic_args.is_empty() => {
            // Check if this is a type parameter that should be substituted
            for (param_name, concrete) in subs {
                if name.node == *param_name {
                    return concrete.clone();
                }
            }
            ty.clone()
        }
        Type::Named { name, generic_args } => {
            // Recursively substitute within generic args
            let new_args: Vec<Spanned<Type>> = generic_args.iter()
                .map(|arg| Spanned::dummy(substitute_type(&arg.node, subs)))
                .collect();
            Type::Named {
                name: name.clone(),
                generic_args: new_args,
            }
        }
        Type::Tuple(elems) => {
            Type::Tuple(elems.iter()
                .map(|e| Spanned::dummy(substitute_type(&e.node, subs)))
                .collect())
        }
        Type::Function { return_type, params, param_ownerships } => {
            Type::Function {
                return_type: Box::new(Spanned::dummy(substitute_type(&return_type.node, subs))),
                params: params.iter()
                    .map(|p| Spanned::dummy(substitute_type(&p.node, subs)))
                    .collect(),
                param_ownerships: param_ownerships.clone(),
            }
        }
        Type::Array { element, size } => {
            Type::Array {
                element: Box::new(Spanned::dummy(substitute_type(&element.node, subs))),
                size: size.clone(),
            }
        }
        Type::Slice { element } => {
            Type::Slice {
                element: Box::new(Spanned::dummy(substitute_type(&element.node, subs))),
            }
        }
        Type::Ref(inner) => {
            Type::Ref(Box::new(Spanned::dummy(substitute_type(&inner.node, subs))))
        }
        Type::Owned(inner) => {
            Type::Owned(Box::new(Spanned::dummy(substitute_type(&inner.node, subs))))
        }
        // Primitives and other types pass through unchanged
        _ => ty.clone(),
    }
}

/// Create a copy of a function definition with all type parameters substituted.
/// Used for transitive discovery: we substitute concrete types and re-scan the body.
///
/// Only the generic params that `subs` actually covers get removed. Method-level
/// generics on equip methods (`map[U, F]` inside `equip [T] VectorIter[T]:`) are
/// preserved so the rescan's `scan_function` picks them up as unresolved-param
/// context — otherwise `MapIter[int, U, F]` in the body would register as a
/// concrete struct instance with phantom `f: void` fields.
pub(super) fn substitute_function_body(
    template: &ast::FunctionDef,
    subs: &[(String, Type)],
) -> ast::FunctionDef {
    let mut func = template.clone();
    func.return_type = Spanned::dummy(substitute_type(&func.return_type.node, subs));
    for p in &mut func.params {
        p.node.type_ = Spanned::dummy(substitute_type(&p.node.type_.node, subs));
    }
    substitute_body_types(&mut func.body, subs);
    // Retain any generic params that `subs` didn't cover so scan_function keeps
    // them in the unresolved-params context. If every generic is substituted,
    // clear the list entirely.
    let covered: std::collections::HashSet<&str> = subs.iter().map(|(n, _)| n.as_str()).collect();
    if let Some(ref mut gp) = func.generic_params {
        gp.node.params.retain(|p| {
            let name = match &p.node {
                ast::GenericParam::Type { name, .. } => name.node.as_str(),
                ast::GenericParam::Const { name, .. } => name.node.as_str(),
            };
            !covered.contains(name)
        });
        if gp.node.params.is_empty() {
            func.generic_params = None;
        }
    }
    func
}

fn substitute_body_types(body: &mut ast::FunctionBody, subs: &[(String, Type)]) {
    match body {
        ast::FunctionBody::Block(block) => substitute_block_types(block, subs),
        ast::FunctionBody::Expression(expr) => substitute_expr_types(expr, subs),
        _ => {}
    }
}

fn substitute_block_types(block: &mut ast::Block, subs: &[(String, Type)]) {
    for stmt in &mut block.stmts {
        substitute_stmt_types(stmt, subs);
    }
}

fn substitute_stmt_types(stmt: &mut Spanned<Stmt>, subs: &[(String, Type)]) {
    match &mut stmt.node {
        Stmt::VarDecl { type_, value, .. } => {
            type_.node = substitute_type(&type_.node, subs);
            substitute_expr_types(value, subs);
        }
        Stmt::Assign { target, value } => {
            substitute_expr_types(target, subs);
            substitute_expr_types(value, subs);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            substitute_expr_types(target, subs);
            substitute_expr_types(value, subs);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) | Stmt::Throw(expr) => {
            substitute_expr_types(expr, subs);
        }
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            substitute_expr_types(condition, subs);
            substitute_block_types(then_body, subs);
            for (cond, body) in elif_branches {
                substitute_expr_types(cond, subs);
                substitute_block_types(body, subs);
            }
            if let Some(eb) = else_body {
                substitute_block_types(eb, subs);
            }
        }
        Stmt::While { condition, body, .. } => {
            substitute_expr_types(condition, subs);
            substitute_block_types(body, subs);
        }
        Stmt::For { iterable, body, .. } => {
            substitute_expr_types(iterable, subs);
            substitute_block_types(body, subs);
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            substitute_expr_types(scrutinee, subs);
            for arm in arms.iter_mut().filter_map(|i| i.arm_mut()) {
                substitute_expr_types(&mut arm.body, subs);
                if let Some(guard) = &mut arm.guard {
                    substitute_expr_types(guard, subs);
                }
            }
            if let Some(eb) = else_arm {
                substitute_block_types(eb, subs);
            }
        }
        Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            substitute_block_types(body, subs);
        }
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            substitute_expr_types(condition, subs);
            substitute_block_types(then_body, subs);
            for (cond, body) in elif_branches {
                substitute_expr_types(cond, subs);
                substitute_block_types(body, subs);
            }
            if let Some(eb) = else_body {
                substitute_block_types(eb, subs);
            }
        }
        Stmt::MetaFor { range, body, .. } => {
            substitute_expr_types(range, subs);
            substitute_block_types(body, subs);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            substitute_expr_types(scrutinee, subs);
            for (case_expr, body) in arms {
                substitute_expr_types(case_expr, subs);
                substitute_block_types(body, subs);
            }
            if let Some(eb) = else_arm {
                substitute_block_types(eb, subs);
            }
        }
        Stmt::MetaWhile { condition, body, .. } => {
            substitute_expr_types(condition, subs);
            substitute_block_types(body, subs);
        }
        Stmt::MetaConst { value, .. } => {
            substitute_expr_types(value, subs);
        }
        _ => {}
    }
}

fn substitute_expr_types(expr: &mut Spanned<Expr>, subs: &[(String, Type)]) {
    match &mut expr.node {
        Expr::Call { callee, generic_args, args } => {
            substitute_expr_types(callee, subs);
            if let Some(type_args) = generic_args {
                for arg in type_args.iter_mut() {
                    arg.node = substitute_type(&arg.node, subs);
                }
            }
            for arg in args {
                substitute_expr_types(&mut arg.node.value, subs);
            }
        }
        Expr::MethodCall { receiver, generic_args, args, .. } => {
            substitute_expr_types(receiver, subs);
            if let Some(type_args) = generic_args {
                for arg in type_args.iter_mut() {
                    arg.node = substitute_type(&arg.node, subs);
                }
            }
            for arg in args {
                substitute_expr_types(&mut arg.node.value, subs);
            }
        }
        Expr::StructLiteral { generic_args, args, .. } => {
            if let Some(type_args) = generic_args {
                for arg in type_args.iter_mut() {
                    arg.node = substitute_type(&arg.node, subs);
                }
            }
            for arg in args {
                substitute_expr_types(arg, subs);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            substitute_expr_types(left, subs);
            substitute_expr_types(right, subs);
        }
        Expr::UnaryOp { operand, .. } => {
            substitute_expr_types(operand, subs);
        }
        Expr::FieldAccess { object, .. } => {
            substitute_expr_types(object, subs);
        }
        Expr::Index { object, index } => {
            substitute_expr_types(object, subs);
            substitute_expr_types(index, subs);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            substitute_expr_types(condition, subs);
            substitute_expr_types(then_branch, subs);
            for (cond, body) in elif_branches {
                substitute_expr_types(cond, subs);
                substitute_expr_types(body, subs);
            }
            if let Some(eb) = else_branch {
                substitute_expr_types(eb, subs);
            }
        }
        Expr::Move { expr: inner }
        | Expr::Propagate { expr: inner }
        | Expr::MutableBorrow { expr: inner } => {
            substitute_expr_types(inner, subs);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { substitute_expr_types(s, subs); }
            if let Some(e) = end { substitute_expr_types(e, subs); }
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            substitute_expr_types(body, subs);
        }
        Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
            for e in elems {
                substitute_expr_types(e, subs);
            }
        }
        Expr::MetaOpInfix { left, right, .. } => {
            substitute_expr_types(left, subs);
            substitute_expr_types(right, subs);
        }
        Expr::MetaOpToken(_) => {}
        _ => {}
    }
}

/// D26 (Round XXXIII Batch C1): payload-free prelude enum thrown by the seven
/// fallible arithmetic operators (`+! -! *! /! %! <<! >>!`). Variants are
/// QUALIFIED-ONLY (`ArithError.Overflow`) — a plain trap becomes a value in the
/// ONE error channel (D23). Single source of truth for both the check-lane
/// resolve.rs registration AND the concrete TypeDef registration in
/// `ir/lowering/mod.rs`.
pub fn builtin_arith_error_enum() -> ast::EnumDef {
    use crate::parser::ast::*;
    ast::EnumDef {
        attributes: vec![],
        visibility: Visibility::Public,
        explicit_visibility: false,
        name: Spanned::dummy("ArithError".to_string()),
        generic_params: None,
        variants: vec![
            Spanned::dummy(Variant {
                name: Spanned::dummy("Overflow".to_string()),
                fields: VariantFields::Unit,
            }),
            Spanned::dummy(Variant {
                name: Spanned::dummy("DivByZero".to_string()),
                fields: VariantFields::Unit,
            }),
        ],
        doc_comment: None,
        span: crate::span::Span::dummy(),
    }
}

/// Inject built-in Option[T] and Result[T, E] enum templates if not present.
pub(super) fn inject_builtin_enums(enum_templates: &mut FxHashMap<String, ast::EnumDef>) {
    use crate::parser::ast::*;

    if !enum_templates.contains_key("Option") {
        enum_templates.insert("Option".to_string(), ast::EnumDef {
            attributes: vec![],
            visibility: Visibility::Public,
            explicit_visibility: false,
            name: Spanned::dummy("Option".to_string()),
            generic_params: Some(Spanned::dummy(GenericParams {
                params: vec![Spanned::dummy(GenericParam::Type { name: Spanned::dummy("T".to_string()), bounds: vec![] })],
            })),
            variants: vec![
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Some".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("T".to_string()),
                        generic_args: vec![],
                    })]),
                }),
                Spanned::dummy(Variant {
                    name: Spanned::dummy("None".to_string()),
                    fields: VariantFields::Unit,
                }),
            ],
            doc_comment: None,
            span: crate::span::Span::dummy(),
        });
    }

    // ArithError: D26 (Round XXXIII Batch C1) prelude enum thrown by the seven
    // fallible arithmetic operators. See `builtin_arith_error_enum`.
    if !enum_templates.contains_key("ArithError") {
        enum_templates.insert("ArithError".to_string(), builtin_arith_error_enum());
    }

    if !enum_templates.contains_key("Result") {
        enum_templates.insert("Result".to_string(), ast::EnumDef {
            attributes: vec![],
            visibility: Visibility::Public,
            explicit_visibility: false,
            name: Spanned::dummy("Result".to_string()),
            generic_params: Some(Spanned::dummy(GenericParams {
                params: vec![
                    Spanned::dummy(GenericParam::Type { name: Spanned::dummy("T".to_string()), bounds: vec![] }),
                    Spanned::dummy(GenericParam::Type { name: Spanned::dummy("E".to_string()), bounds: vec![] }),
                ],
            })),
            variants: vec![
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Ok".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("T".to_string()),
                        generic_args: vec![],
                    })]),
                }),
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Error".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("E".to_string()),
                        generic_args: vec![],
                    })]),
                }),
            ],
            doc_comment: None,
            span: crate::span::Span::dummy(),
        });
    }
}
