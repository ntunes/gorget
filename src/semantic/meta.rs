//! Compile-time meta constant evaluation.
//!
//! Three-phase pass run **before** semantic analysis:
//!
//! 1. **Evaluate:** Walk `module.items` top-to-bottom. For each `MetaConst`,
//!    evaluate its expression to a `MetaValue`. Build a symbol table.
//! 2. **Substitute:** Walk the entire AST, replacing every `Identifier` or
//!    `StringSegment::Interpolation` that matches a meta const with its literal.
//! 3. **Remove:** Strip all `MetaConst` and `MetaAssert` items from the module.
//!
//! After this pass, the rest of the compiler sees no meta constructs at all.

use rustc_hash::FxHashMap;

use crate::lexer::token::{StringKind, StringLiteral, StringSegment};
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};

// ═══════════════════════════════════════════════════════════════
// MetaValue — the result of compile-time evaluation
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum MetaValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

// ═══════════════════════════════════════════════════════════════
// Public entry point
// ═══════════════════════════════════════════════════════════════

/// Evaluate, substitute, and remove all meta constants from a module.
pub fn evaluate_meta_consts(module: &mut Module) -> Vec<SemanticError> {
    let mut errors = Vec::new();
    let mut env: FxHashMap<String, MetaValue> = FxHashMap::default();

    // Phase 1: Evaluate meta consts and meta asserts
    for item in &module.items {
        match &item.node {
            Item::MetaConst(mc) => {
                match eval_expr(&mc.value.node, &env, mc.value.span) {
                    Ok(value) => {
                        if let Err(e) = validate_type(&mc.type_.node, &value, mc.span) {
                            errors.push(e);
                        } else {
                            env.insert(mc.name.node.clone(), value);
                        }
                    }
                    Err(e) => errors.push(e),
                }
            }
            Item::MetaAssert(ma) => {
                match eval_expr(&ma.condition.node, &env, ma.condition.span) {
                    Ok(MetaValue::Bool(true)) => {} // assertion passes
                    Ok(MetaValue::Bool(false)) => {
                        let msg = if let Some(msg_expr) = &ma.message {
                            match eval_expr(&msg_expr.node, &env, msg_expr.span) {
                                Ok(v) => meta_value_to_string(&v),
                                Err(_) => "assertion failed".to_string(),
                            }
                        } else {
                            "assertion failed".to_string()
                        };
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::MetaEvalError { message: msg },
                            span: ma.span,
                        });
                    }
                    Ok(_) => {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::MetaEvalError {
                                message: "meta assert condition must be bool".to_string(),
                            },
                            span: ma.condition.span,
                        });
                    }
                    Err(e) => errors.push(e),
                }
            }
            _ => {}
        }
    }

    // Phase 2: Substitute meta const references throughout the AST
    for item in &mut module.items {
        substitute_item(&mut item.node, &env);
    }

    // Phase 3: Remove meta declarations (MetaConst + MetaAssert)
    module.items.retain(|item| {
        !matches!(&item.node, Item::MetaConst(_) | Item::MetaAssert(_))
    });

    errors
}

// ═══════════════════════════════════════════════════════════════
// Phase 1: Expression evaluator
// ═══════════════════════════════════════════════════════════════

fn eval_expr(
    expr: &Expr,
    env: &FxHashMap<String, MetaValue>,
    span: Span,
) -> Result<MetaValue, SemanticError> {
    match expr {
        Expr::IntLiteral(n) => Ok(MetaValue::Int(*n)),
        Expr::FloatLiteral(f) => Ok(MetaValue::Float(*f)),
        Expr::BoolLiteral(b) => Ok(MetaValue::Bool(*b)),
        Expr::StringLiteral(s) => {
            // Only plain string literals (no interpolation segments)
            if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_))) {
                return Err(meta_err("interpolated strings cannot be evaluated at compile time", span));
            }
            let text: String = s.segments.iter().map(|seg| match seg {
                StringSegment::Literal(s) => s.as_str(),
                StringSegment::Interpolation(_) => unreachable!(),
            }).collect();
            Ok(MetaValue::Str(text))
        }

        Expr::Identifier(name) => {
            if let Some(value) = env.get(name.as_str()) {
                Ok(value.clone())
            } else {
                Err(meta_err(&format!("undefined meta constant `{name}`"), span))
            }
        }

        Expr::UnaryOp { op, operand } => {
            let val = eval_expr(&operand.node, env, operand.span)?;
            match (op, &val) {
                (UnaryOp::Neg, MetaValue::Int(n)) => Ok(MetaValue::Int(-n)),
                (UnaryOp::Neg, MetaValue::Float(f)) => Ok(MetaValue::Float(-f)),
                (UnaryOp::Not, MetaValue::Bool(b)) => Ok(MetaValue::Bool(!b)),
                (UnaryOp::BitNot, MetaValue::Int(n)) => Ok(MetaValue::Int(!n)),
                _ => Err(meta_err(
                    &format!("unsupported unary operator `{op:?}` on {}", value_type_name(&val)),
                    span,
                )),
            }
        }

        Expr::BinaryOp { left, op, right } => {
            let lhs = eval_expr(&left.node, env, left.span)?;
            let rhs = eval_expr(&right.node, env, right.span)?;
            eval_binary_op(&lhs, *op, &rhs, span)
        }

        _ => Err(meta_err("expression cannot be evaluated at compile time", span)),
    }
}

fn eval_binary_op(
    lhs: &MetaValue,
    op: BinaryOp,
    rhs: &MetaValue,
    span: Span,
) -> Result<MetaValue, SemanticError> {
    match (lhs, op, rhs) {
        // Integer arithmetic
        (MetaValue::Int(a), BinaryOp::Add, MetaValue::Int(b)) => Ok(MetaValue::Int(a.wrapping_add(*b))),
        (MetaValue::Int(a), BinaryOp::Sub, MetaValue::Int(b)) => Ok(MetaValue::Int(a.wrapping_sub(*b))),
        (MetaValue::Int(a), BinaryOp::Mul, MetaValue::Int(b)) => Ok(MetaValue::Int(a.wrapping_mul(*b))),
        (MetaValue::Int(a), BinaryOp::Div, MetaValue::Int(b)) => {
            if *b == 0 {
                Err(meta_err("division by zero", span))
            } else {
                Ok(MetaValue::Int(a / b))
            }
        }
        (MetaValue::Int(a), BinaryOp::Mod, MetaValue::Int(b)) => {
            if *b == 0 {
                Err(meta_err("modulo by zero", span))
            } else {
                Ok(MetaValue::Int(a % b))
            }
        }

        // Integer bitwise
        (MetaValue::Int(a), BinaryOp::BitAnd, MetaValue::Int(b)) => Ok(MetaValue::Int(a & b)),
        (MetaValue::Int(a), BinaryOp::BitOr, MetaValue::Int(b)) => Ok(MetaValue::Int(a | b)),
        (MetaValue::Int(a), BinaryOp::BitXor, MetaValue::Int(b)) => Ok(MetaValue::Int(a ^ b)),
        (MetaValue::Int(a), BinaryOp::Shl, MetaValue::Int(b)) => Ok(MetaValue::Int(a << b)),
        (MetaValue::Int(a), BinaryOp::Shr, MetaValue::Int(b)) => Ok(MetaValue::Int(a >> b)),

        // Float arithmetic
        (MetaValue::Float(a), BinaryOp::Add, MetaValue::Float(b)) => Ok(MetaValue::Float(a + b)),
        (MetaValue::Float(a), BinaryOp::Sub, MetaValue::Float(b)) => Ok(MetaValue::Float(a - b)),
        (MetaValue::Float(a), BinaryOp::Mul, MetaValue::Float(b)) => Ok(MetaValue::Float(a * b)),
        (MetaValue::Float(a), BinaryOp::Div, MetaValue::Float(b)) => {
            if *b == 0.0 {
                Err(meta_err("division by zero", span))
            } else {
                Ok(MetaValue::Float(a / b))
            }
        }

        // Integer comparisons
        (MetaValue::Int(a), BinaryOp::Eq, MetaValue::Int(b)) => Ok(MetaValue::Bool(a == b)),
        (MetaValue::Int(a), BinaryOp::Neq, MetaValue::Int(b)) => Ok(MetaValue::Bool(a != b)),
        (MetaValue::Int(a), BinaryOp::Lt, MetaValue::Int(b)) => Ok(MetaValue::Bool(a < b)),
        (MetaValue::Int(a), BinaryOp::Gt, MetaValue::Int(b)) => Ok(MetaValue::Bool(a > b)),
        (MetaValue::Int(a), BinaryOp::LtEq, MetaValue::Int(b)) => Ok(MetaValue::Bool(a <= b)),
        (MetaValue::Int(a), BinaryOp::GtEq, MetaValue::Int(b)) => Ok(MetaValue::Bool(a >= b)),

        // Float comparisons
        (MetaValue::Float(a), BinaryOp::Eq, MetaValue::Float(b)) => Ok(MetaValue::Bool(a == b)),
        (MetaValue::Float(a), BinaryOp::Neq, MetaValue::Float(b)) => Ok(MetaValue::Bool(a != b)),
        (MetaValue::Float(a), BinaryOp::Lt, MetaValue::Float(b)) => Ok(MetaValue::Bool(a < b)),
        (MetaValue::Float(a), BinaryOp::Gt, MetaValue::Float(b)) => Ok(MetaValue::Bool(a > b)),
        (MetaValue::Float(a), BinaryOp::LtEq, MetaValue::Float(b)) => Ok(MetaValue::Bool(a <= b)),
        (MetaValue::Float(a), BinaryOp::GtEq, MetaValue::Float(b)) => Ok(MetaValue::Bool(a >= b)),

        // Bool comparisons
        (MetaValue::Bool(a), BinaryOp::Eq, MetaValue::Bool(b)) => Ok(MetaValue::Bool(a == b)),
        (MetaValue::Bool(a), BinaryOp::Neq, MetaValue::Bool(b)) => Ok(MetaValue::Bool(a != b)),

        // String comparisons
        (MetaValue::Str(a), BinaryOp::Eq, MetaValue::Str(b)) => Ok(MetaValue::Bool(a == b)),
        (MetaValue::Str(a), BinaryOp::Neq, MetaValue::Str(b)) => Ok(MetaValue::Bool(a != b)),

        // Logical operators
        (MetaValue::Bool(a), BinaryOp::And, MetaValue::Bool(b)) => Ok(MetaValue::Bool(*a && *b)),
        (MetaValue::Bool(a), BinaryOp::Or, MetaValue::Bool(b)) => Ok(MetaValue::Bool(*a || *b)),

        // String concatenation
        (MetaValue::Str(a), BinaryOp::Add, MetaValue::Str(b)) => {
            Ok(MetaValue::Str(format!("{a}{b}")))
        }

        _ => Err(meta_err(
            &format!(
                "unsupported binary operator `{op:?}` on {} and {}",
                value_type_name(lhs),
                value_type_name(rhs),
            ),
            span,
        )),
    }
}

// ═══════════════════════════════════════════════════════════════
// Type validation
// ═══════════════════════════════════════════════════════════════

fn validate_type(ty: &Type, value: &MetaValue, span: Span) -> Result<(), SemanticError> {
    let ok = match (ty, value) {
        (Type::Primitive(PrimitiveType::Int), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Int8), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Int16), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Int32), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Int64), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Uint), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Uint8), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Uint16), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Uint32), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Uint64), MetaValue::Int(_)) => true,
        (Type::Primitive(PrimitiveType::Float), MetaValue::Float(_)) => true,
        (Type::Primitive(PrimitiveType::Float32), MetaValue::Float(_)) => true,
        (Type::Primitive(PrimitiveType::Float64), MetaValue::Float(_)) => true,
        (Type::Primitive(PrimitiveType::Bool), MetaValue::Bool(_)) => true,
        (Type::Primitive(PrimitiveType::Str), MetaValue::Str(_)) => true,
        _ => false,
    };
    if ok {
        Ok(())
    } else {
        Err(meta_err(
            &format!(
                "type mismatch: declared {} but expression evaluates to {}",
                type_name(ty),
                value_type_name(value),
            ),
            span,
        ))
    }
}

// ═══════════════════════════════════════════════════════════════
// Phase 2: AST substitution
// ═══════════════════════════════════════════════════════════════

fn substitute_item(item: &mut Item, env: &FxHashMap<String, MetaValue>) {
    match item {
        Item::Function(f) => substitute_function(f, env),
        Item::Equip(eq) => {
            for method in &mut eq.items {
                substitute_function(&mut method.node, env);
            }
        }
        Item::ConstDecl(c) => substitute_expr(&mut c.value, env),
        Item::StaticDecl(s) => substitute_expr(&mut s.value, env),
        Item::Test(t) => {
            for binding in &mut t.with_bindings {
                substitute_expr(&mut binding.expr, env);
            }
            substitute_block(&mut t.body, env);
        }
        Item::SuiteSetup(s) => substitute_block(&mut s.body, env),
        Item::SuiteTeardown(s) => substitute_block(&mut s.body, env),
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                if let TraitItem::Method(f) = &mut trait_item.node {
                    substitute_function(f, env);
                }
            }
        }
        Item::Struct(_) | Item::Enum(_) | Item::Import(_)
        | Item::TypeAlias(_) | Item::Newtype(_) | Item::ExternBlock(_)
        | Item::Directive(_) | Item::MetaConst(_) | Item::MetaType(_)
        | Item::MetaTypeFunc(_) | Item::MetaAssert(_) | Item::MetaIf(_) => {}
    }
}

fn substitute_function(f: &mut FunctionDef, env: &FxHashMap<String, MetaValue>) {
    for param in &mut f.params {
        if let Some(default) = &mut param.node.default {
            substitute_expr(default, env);
        }
    }
    match &mut f.body {
        FunctionBody::Block(block) => substitute_block(block, env),
        FunctionBody::Expression(expr) => substitute_expr(expr, env),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn substitute_block(block: &mut Block, env: &FxHashMap<String, MetaValue>) {
    for stmt in &mut block.stmts {
        substitute_stmt(&mut stmt.node, env);
    }
}

fn substitute_stmt(stmt: &mut Stmt, env: &FxHashMap<String, MetaValue>) {
    match stmt {
        Stmt::VarDecl { value, .. } => substitute_expr(value, env),
        Stmt::Expr(expr) => substitute_expr(expr, env),
        Stmt::Assign { target, value } => {
            substitute_expr(target, env);
            substitute_expr(value, env);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            substitute_expr(target, env);
            substitute_expr(value, env);
        }
        Stmt::Return(Some(expr)) => substitute_expr(expr, env),
        Stmt::Throw(expr) => substitute_expr(expr, env),
        Stmt::Break(Some(expr)) => substitute_expr(expr, env),
        Stmt::Return(None) | Stmt::Break(None) | Stmt::Continue | Stmt::Pass => {}
        Stmt::For { iterable, body, else_body, .. } => {
            substitute_expr(iterable, env);
            substitute_block(body, env);
            if let Some(eb) = else_body { substitute_block(eb, env); }
        }
        Stmt::While { condition, body, else_body } => {
            substitute_expr(condition, env);
            substitute_block(body, env);
            if let Some(eb) = else_body { substitute_block(eb, env); }
        }
        Stmt::Loop { body } => substitute_block(body, env),
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            substitute_expr(condition, env);
            substitute_block(then_body, env);
            for (cond, body) in elif_branches {
                substitute_expr(cond, env);
                substitute_block(body, env);
            }
            if let Some(eb) = else_body { substitute_block(eb, env); }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            substitute_expr(scrutinee, env);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { substitute_expr(guard, env); }
                substitute_expr(&mut arm.body, env);
            }
            if let Some(ea) = else_arm { substitute_block(ea, env); }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                substitute_expr(&mut binding.expr, env);
            }
            substitute_block(body, env);
        }
        Stmt::Unsafe { body } => substitute_block(body, env),
        Stmt::Assert { condition, message } => {
            substitute_expr(condition, env);
            if let Some(msg) = message { substitute_expr(msg, env); }
        }
        Stmt::Item(item) => substitute_item(item, env),
    }
}

fn substitute_expr(expr: &mut Spanned<Expr>, env: &FxHashMap<String, MetaValue>) {
    // First: recurse into sub-expressions
    match &mut expr.node {
        Expr::UnaryOp { operand, .. } => substitute_expr(operand, env),
        Expr::BinaryOp { left, right, .. } => {
            substitute_expr(left, env);
            substitute_expr(right, env);
        }
        Expr::Call { callee, args, .. } => {
            substitute_expr(callee, env);
            for arg in args {
                substitute_expr(&mut arg.node.value, env);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            substitute_expr(receiver, env);
            for arg in args {
                substitute_expr(&mut arg.node.value, env);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => {
            substitute_expr(object, env);
        }
        Expr::Index { object, index } => {
            substitute_expr(object, env);
            substitute_expr(index, env);
        }
        Expr::NilCoalescing { lhs, rhs } => {
            substitute_expr(lhs, env);
            substitute_expr(rhs, env);
        }
        Expr::Try { expr: inner } | Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner } | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner } | Expr::Spawn { expr: inner }
        | Expr::TryCapture { expr: inner } => {
            substitute_expr(inner, env);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            substitute_expr(condition, env);
            substitute_expr(then_branch, env);
            for (cond, body) in elif_branches {
                substitute_expr(cond, env);
                substitute_expr(body, env);
            }
            if let Some(eb) = else_branch { substitute_expr(eb, env); }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            substitute_expr(scrutinee, env);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { substitute_expr(guard, env); }
                substitute_expr(&mut arm.body, env);
            }
            if let Some(ea) = else_arm { substitute_expr(ea, env); }
        }
        Expr::Block(block) | Expr::Do { body: block } => {
            substitute_block(block, env);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            substitute_expr(body, env);
        }
        Expr::ListComprehension { expr: comp_expr, iterable, condition, .. } => {
            substitute_expr(comp_expr, env);
            substitute_expr(iterable, env);
            if let Some(cond) = condition { substitute_expr(cond, env); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            substitute_expr(key, env);
            substitute_expr(value, env);
            substitute_expr(iterable, env);
            if let Some(cond) = condition { substitute_expr(cond, env); }
        }
        Expr::SetComprehension { expr: comp_expr, iterable, condition, .. } => {
            substitute_expr(comp_expr, env);
            substitute_expr(iterable, env);
            if let Some(cond) = condition { substitute_expr(cond, env); }
        }
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            for elem in elems { substitute_expr(elem, env); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                substitute_expr(k, env);
                substitute_expr(v, env);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for arg in args { substitute_expr(arg, env); }
        }
        Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
            substitute_expr(inner, env);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { substitute_expr(s, env); }
            if let Some(e) = end { substitute_expr(e, env); }
        }
        // Leaf nodes — no recursion needed
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::CharLiteral(_) | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } | Expr::It => {}
        // StringLiteral handled below
        Expr::StringLiteral(_) => {}
    }

    // Then: check if this is a meta-const reference to replace
    if let Expr::Identifier(name) = &expr.node {
        if let Some(value) = env.get(name.as_str()) {
            expr.node = meta_value_to_expr(value);
        }
    }

    // Also handle string interpolation segments
    if let Expr::StringLiteral(s) = &mut expr.node {
        for seg in &mut s.segments {
            if let StringSegment::Interpolation(name) = seg {
                if let Some(value) = env.get(name.as_str()) {
                    *seg = StringSegment::Literal(meta_value_to_string(value));
                }
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Value ↔ Expr conversions
// ═══════════════════════════════════════════════════════════════

fn meta_value_to_expr(value: &MetaValue) -> Expr {
    match value {
        MetaValue::Int(n) => Expr::IntLiteral(*n),
        MetaValue::Float(f) => Expr::FloatLiteral(*f),
        MetaValue::Bool(b) => Expr::BoolLiteral(*b),
        MetaValue::Str(s) => Expr::StringLiteral(StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Literal(s.clone())],
        }),
    }
}

fn meta_value_to_string(value: &MetaValue) -> String {
    match value {
        MetaValue::Int(n) => format!("{n}"),
        MetaValue::Float(f) => format!("{f}"),
        MetaValue::Bool(b) => format!("{b}"),
        MetaValue::Str(s) => s.clone(),
    }
}

// ═══════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════

fn meta_err(message: &str, span: Span) -> SemanticError {
    SemanticError {
        kind: SemanticErrorKind::MetaEvalError {
            message: message.to_string(),
        },
        span,
    }
}

fn value_type_name(v: &MetaValue) -> &'static str {
    match v {
        MetaValue::Int(_) => "int",
        MetaValue::Float(_) => "float",
        MetaValue::Bool(_) => "bool",
        MetaValue::Str(_) => "str",
    }
}

fn type_name(ty: &Type) -> &'static str {
    match ty {
        Type::Primitive(PrimitiveType::Int) => "int",
        Type::Primitive(PrimitiveType::Int8) => "int8",
        Type::Primitive(PrimitiveType::Int16) => "int16",
        Type::Primitive(PrimitiveType::Int32) => "int32",
        Type::Primitive(PrimitiveType::Int64) => "int64",
        Type::Primitive(PrimitiveType::Uint) => "uint",
        Type::Primitive(PrimitiveType::Uint8) => "uint8",
        Type::Primitive(PrimitiveType::Uint16) => "uint16",
        Type::Primitive(PrimitiveType::Uint32) => "uint32",
        Type::Primitive(PrimitiveType::Uint64) => "uint64",
        Type::Primitive(PrimitiveType::Float) => "float",
        Type::Primitive(PrimitiveType::Float32) => "float32",
        Type::Primitive(PrimitiveType::Float64) => "float64",
        Type::Primitive(PrimitiveType::Bool) => "bool",
        Type::Primitive(PrimitiveType::Str) => "str",
        Type::Primitive(PrimitiveType::StringType) => "String",
        Type::Primitive(PrimitiveType::Void) => "void",
        Type::Primitive(PrimitiveType::Char) => "char",
        _ => "<unknown>",
    }
}

// ═══════════════════════════════════════════════════════════════
// Unit tests
// ═══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_span() -> Span {
        Span { start: 0, end: 1 }
    }

    fn empty_env() -> FxHashMap<String, MetaValue> {
        FxHashMap::default()
    }

    // ── Literal evaluation ──

    #[test]
    fn eval_int_literal() {
        let result = eval_expr(&Expr::IntLiteral(42), &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(42))));
    }

    #[test]
    fn eval_float_literal() {
        let result = eval_expr(&Expr::FloatLiteral(3.14), &empty_env(), dummy_span());
        match result {
            Ok(MetaValue::Float(f)) => assert!((f - 3.14).abs() < f64::EPSILON),
            other => panic!("expected Float, got: {other:?}"),
        }
    }

    #[test]
    fn eval_bool_literal() {
        let result = eval_expr(&Expr::BoolLiteral(true), &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Bool(true))));
    }

    #[test]
    fn eval_string_literal() {
        let s = Expr::StringLiteral(StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Literal("hello".to_string())],
        });
        let result = eval_expr(&s, &empty_env(), dummy_span());
        match result {
            Ok(MetaValue::Str(s)) => assert_eq!(s, "hello"),
            other => panic!("expected Str, got: {other:?}"),
        }
    }

    // ── Binary arithmetic ──

    #[test]
    fn eval_arithmetic_precedence() {
        // 10 + 20 * 3 → we simulate the already-parsed AST: Add(10, Mul(20, 3))
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(Expr::IntLiteral(10), dummy_span())),
            op: BinaryOp::Add,
            right: Box::new(Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(Spanned::new(Expr::IntLiteral(20), dummy_span())),
                    op: BinaryOp::Mul,
                    right: Box::new(Spanned::new(Expr::IntLiteral(3), dummy_span())),
                },
                dummy_span(),
            )),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(70))));
    }

    // ── Meta const references ──

    #[test]
    fn eval_meta_const_reference() {
        let mut env = empty_env();
        env.insert("A".to_string(), MetaValue::Int(5));
        // B = A * 2
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(Expr::Identifier("A".to_string()), dummy_span())),
            op: BinaryOp::Mul,
            right: Box::new(Spanned::new(Expr::IntLiteral(2), dummy_span())),
        };
        let result = eval_expr(&expr, &env, dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(10))));
    }

    // ── Comparison operators ──

    #[test]
    fn eval_comparison() {
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(Expr::IntLiteral(5), dummy_span())),
            op: BinaryOp::Gt,
            right: Box::new(Spanned::new(Expr::IntLiteral(3), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Bool(true))));
    }

    // ── String concatenation ──

    #[test]
    fn eval_string_concat() {
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(
                Expr::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("a".to_string())],
                }),
                dummy_span(),
            )),
            op: BinaryOp::Add,
            right: Box::new(Spanned::new(
                Expr::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("b".to_string())],
                }),
                dummy_span(),
            )),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        match result {
            Ok(MetaValue::Str(s)) => assert_eq!(s, "ab"),
            other => panic!("expected Str, got: {other:?}"),
        }
    }

    // ── Unary operators ──

    #[test]
    fn eval_unary_neg() {
        let expr = Expr::UnaryOp {
            op: UnaryOp::Neg,
            operand: Box::new(Spanned::new(Expr::IntLiteral(5), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(-5))));
    }

    #[test]
    fn eval_unary_not() {
        let expr = Expr::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(Spanned::new(Expr::BoolLiteral(true), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Bool(false))));
    }

    #[test]
    fn eval_unary_bitnot() {
        let expr = Expr::UnaryOp {
            op: UnaryOp::BitNot,
            operand: Box::new(Spanned::new(Expr::IntLiteral(0xFF), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(i)) if i == !0xFFi64));
    }

    // ── Error cases ──

    #[test]
    fn eval_type_mismatch() {
        let result = validate_type(
            &Type::Primitive(PrimitiveType::Int),
            &MetaValue::Str("hello".to_string()),
            dummy_span(),
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(format!("{err}").contains("type mismatch"));
    }

    #[test]
    fn eval_unsupported_expr() {
        let expr = Expr::Call {
            callee: Box::new(Spanned::new(Expr::Identifier("f".to_string()), dummy_span())),
            generic_args: None,
            args: vec![],
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("cannot be evaluated at compile time"));
    }

    #[test]
    fn eval_division_by_zero() {
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(Expr::IntLiteral(10), dummy_span())),
            op: BinaryOp::Div,
            right: Box::new(Spanned::new(Expr::IntLiteral(0), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("division by zero"));
    }

    #[test]
    fn eval_undefined_meta_const() {
        let expr = Expr::Identifier("UNKNOWN".to_string());
        let result = eval_expr(&expr, &empty_env(), dummy_span());
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("undefined meta constant"));
    }

    // ── Full pipeline (evaluate + substitute + remove) ──

    #[test]
    fn full_pipeline_substitutes_and_removes() {
        // Build a module with:
        //   meta int X = 42
        //   void main(): print(X)
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                        name: Spanned::new("X".to_string(), dummy_span()),
                        value: Spanned::new(Expr::IntLiteral(42), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::Function(FunctionDef {
                        attributes: vec![],
                        visibility: Visibility::Private,
                        qualifiers: FunctionQualifiers::default(),
                        return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                        name: Spanned::new("main".to_string(), dummy_span()),
                        generic_params: None,
                        params: vec![],
                        throws: None,
                        where_clause: None,
                        body: FunctionBody::Block(Block {
                            stmts: vec![Spanned::new(
                                Stmt::Expr(Spanned::new(
                                    Expr::Call {
                                        callee: Box::new(Spanned::new(
                                            Expr::Identifier("print".to_string()),
                                            dummy_span(),
                                        )),
                                        generic_args: None,
                                        args: vec![Spanned::new(
                                            CallArg {
                                                name: None,
                                                ownership: Ownership::Borrow,
                                                value: Spanned::new(
                                                    Expr::Identifier("X".to_string()),
                                                    dummy_span(),
                                                ),
                                            },
                                            dummy_span(),
                                        )],
                                    },
                                    dummy_span(),
                                )),
                                dummy_span(),
                            )],
                            span: dummy_span(),
                        }),
                        doc_comment: None,
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");

        // MetaConst should be removed
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Function(_)));

        // X should be substituted with 42
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(call) = &block.stmts[0].node {
                    if let Expr::Call { args, .. } = &call.node {
                        assert!(
                            matches!(&args[0].node.value.node, Expr::IntLiteral(42)),
                            "expected IntLiteral(42), got: {:?}",
                            args[0].node.value.node,
                        );
                        return;
                    }
                }
            }
        }
        panic!("test structure unexpected");
    }

    #[test]
    fn string_interpolation_substitution() {
        // meta str NAME = "world"
        // print("{NAME}")  → print("world")
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Str), dummy_span()),
                        name: Spanned::new("NAME".to_string(), dummy_span()),
                        value: Spanned::new(
                            Expr::StringLiteral(StringLiteral {
                                kind: StringKind::Normal,
                                segments: vec![StringSegment::Literal("world".to_string())],
                            }),
                            dummy_span(),
                        ),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::Function(FunctionDef {
                        attributes: vec![],
                        visibility: Visibility::Private,
                        qualifiers: FunctionQualifiers::default(),
                        return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                        name: Spanned::new("main".to_string(), dummy_span()),
                        generic_params: None,
                        params: vec![],
                        throws: None,
                        where_clause: None,
                        body: FunctionBody::Block(Block {
                            stmts: vec![Spanned::new(
                                Stmt::Expr(Spanned::new(
                                    Expr::Call {
                                        callee: Box::new(Spanned::new(
                                            Expr::Identifier("print".to_string()),
                                            dummy_span(),
                                        )),
                                        generic_args: None,
                                        args: vec![Spanned::new(
                                            CallArg {
                                                name: None,
                                                ownership: Ownership::Borrow,
                                                value: Spanned::new(
                                                    Expr::StringLiteral(StringLiteral {
                                                        kind: StringKind::Normal,
                                                        segments: vec![StringSegment::Interpolation("NAME".to_string())],
                                                    }),
                                                    dummy_span(),
                                                ),
                                            },
                                            dummy_span(),
                                        )],
                                    },
                                    dummy_span(),
                                )),
                                dummy_span(),
                            )],
                            span: dummy_span(),
                        }),
                        doc_comment: None,
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty());

        // Verify interpolation segment was replaced with literal
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(call) = &block.stmts[0].node {
                    if let Expr::Call { args, .. } = &call.node {
                        if let Expr::StringLiteral(s) = &args[0].node.value.node {
                            assert_eq!(s.segments.len(), 1);
                            assert!(
                                matches!(&s.segments[0], StringSegment::Literal(lit) if lit == "world"),
                                "expected Literal(\"world\"), got: {:?}",
                                s.segments[0],
                            );
                            return;
                        }
                    }
                }
            }
        }
        panic!("test structure unexpected");
    }

    #[test]
    fn meta_assert_passes() {
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                        name: Spanned::new("X".to_string(), dummy_span()),
                        value: Spanned::new(Expr::IntLiteral(10), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaAssert(MetaAssert {
                        condition: Spanned::new(
                            Expr::BinaryOp {
                                left: Box::new(Spanned::new(Expr::Identifier("X".to_string()), dummy_span())),
                                op: BinaryOp::Gt,
                                right: Box::new(Spanned::new(Expr::IntLiteral(0), dummy_span())),
                            },
                            dummy_span(),
                        ),
                        message: Some(Spanned::new(
                            Expr::StringLiteral(StringLiteral {
                                kind: StringKind::Normal,
                                segments: vec![StringSegment::Literal("X must be positive".to_string())],
                            }),
                            dummy_span(),
                        )),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty());
        // Both meta items should be removed
        assert!(module.items.is_empty());
    }

    #[test]
    fn meta_assert_fails() {
        let mut module = Module {
            items: vec![Spanned::new(
                Item::MetaAssert(MetaAssert {
                    condition: Spanned::new(Expr::BoolLiteral(false), dummy_span()),
                    message: Some(Spanned::new(
                        Expr::StringLiteral(StringLiteral {
                            kind: StringKind::Normal,
                            segments: vec![StringSegment::Literal("oops".to_string())],
                        }),
                        dummy_span(),
                    )),
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert_eq!(errors.len(), 1);
        assert!(format!("{}", errors[0]).contains("oops"));
    }
}
