//! Compile-time meta evaluation: constants, type aliases, and conditional compilation.
//!
//! Multi-phase pass run **before** semantic analysis:
//!
//! 1. **Evaluate:** Walk `module.items` top-to-bottom. For each `MetaConst`,
//!    evaluate its expression to a `MetaValue`. For each `MetaType`, store the
//!    type alias. Process `MetaAssert` inline.
//! 1.5. **Flatten MetaIf:** Evaluate `MetaIf` conditions, splice winning branch
//!    items into the item list. Process any meta declarations in the winning branch.
//! 2. **Substitute:** Walk the entire AST, replacing every `Identifier` or
//!    `StringSegment::Interpolation` that matches a meta const with its literal.
//!    Replace every type annotation that matches a meta type alias.
//! 3. **Remove:** Strip all meta items from the module.
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

/// Evaluate, substitute, and remove all meta constructs from a module.
pub fn evaluate_meta_consts(module: &mut Module) -> Vec<SemanticError> {
    let mut errors = Vec::new();
    let mut env: FxHashMap<String, MetaValue> = FxHashMap::default();
    let mut type_env: FxHashMap<String, Type> = FxHashMap::default();

    // Phase 1: Evaluate meta consts, meta asserts, and meta type aliases
    for item in &module.items {
        process_meta_item(&item.node, &mut env, &mut type_env, &mut errors);
    }

    // Phase 1.5: Flatten MetaIf (conditional compilation)
    module.items = flatten_meta_ifs(module.items.clone(), &mut env, &mut type_env, &mut errors);

    // Phase 2: Substitute meta const references and type aliases throughout the AST
    for item in &mut module.items {
        substitute_item(&mut item.node, &env, &type_env);
    }

    // Phase 3: Remove all meta declarations
    module.items.retain(|item| {
        !matches!(
            &item.node,
            Item::MetaConst(_) | Item::MetaAssert(_) | Item::MetaType(_) | Item::MetaIf(_)
        )
    });

    errors
}

/// Process a single meta item: MetaConst, MetaAssert, or MetaType.
fn process_meta_item(
    item: &Item,
    env: &mut FxHashMap<String, MetaValue>,
    type_env: &mut FxHashMap<String, Type>,
    errors: &mut Vec<SemanticError>,
) {
    match item {
        Item::MetaConst(mc) => {
            match eval_expr(&mc.value.node, env, mc.value.span) {
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
            match eval_expr(&ma.condition.node, env, ma.condition.span) {
                Ok(MetaValue::Bool(true)) => {} // assertion passes
                Ok(MetaValue::Bool(false)) => {
                    let msg = if let Some(msg_expr) = &ma.message {
                        match eval_expr(&msg_expr.node, env, msg_expr.span) {
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
        Item::MetaType(mt) => {
            type_env.insert(mt.name.node.clone(), mt.type_.node.clone());
        }
        _ => {}
    }
}

// ═══════════════════════════════════════════════════════════════
// Phase 1.5: Flatten MetaIf (conditional compilation)
// ═══════════════════════════════════════════════════════════════

/// Evaluate `MetaIf` conditions and splice winning branch items into the item list.
/// Loops until no `MetaIf` items remain (handles nested meta-if in expanded branches).
fn flatten_meta_ifs(
    items: Vec<Spanned<Item>>,
    env: &mut FxHashMap<String, MetaValue>,
    type_env: &mut FxHashMap<String, Type>,
    errors: &mut Vec<SemanticError>,
) -> Vec<Spanned<Item>> {
    let mut result = items;
    loop {
        let mut changed = false;
        let mut new_items = Vec::with_capacity(result.len());
        for item in result {
            if let Item::MetaIf(meta_if) = &item.node {
                changed = true;
                let winning = pick_meta_if_branch(meta_if, env, errors);
                // Process any meta declarations in the winning branch
                for won_item in &winning {
                    process_meta_item(&won_item.node, env, type_env, errors);
                }
                new_items.extend(winning);
            } else {
                new_items.push(item);
            }
        }
        result = new_items;
        if !changed {
            break;
        }
    }
    result
}

/// Evaluate MetaIf conditions and return the winning branch's items.
fn pick_meta_if_branch(
    meta_if: &MetaIf,
    env: &FxHashMap<String, MetaValue>,
    errors: &mut Vec<SemanticError>,
) -> Vec<Spanned<Item>> {
    // Try the main condition
    match eval_expr(&meta_if.condition.node, env, meta_if.condition.span) {
        Ok(MetaValue::Bool(true)) => return meta_if.then_items.clone(),
        Ok(MetaValue::Bool(false)) => {} // fall through to elif/else
        Ok(_) => {
            errors.push(SemanticError {
                kind: SemanticErrorKind::MetaEvalError {
                    message: "meta if condition must be bool".to_string(),
                },
                span: meta_if.condition.span,
            });
            return vec![];
        }
        Err(e) => {
            errors.push(e);
            return vec![];
        }
    }

    // Try elif branches
    for (cond, branch_items) in &meta_if.elif_branches {
        match eval_expr(&cond.node, env, cond.span) {
            Ok(MetaValue::Bool(true)) => return branch_items.clone(),
            Ok(MetaValue::Bool(false)) => {} // try next
            Ok(_) => {
                errors.push(SemanticError {
                    kind: SemanticErrorKind::MetaEvalError {
                        message: "meta elif condition must be bool".to_string(),
                    },
                    span: cond.span,
                });
                return vec![];
            }
            Err(e) => {
                errors.push(e);
                return vec![];
            }
        }
    }

    // Else branch
    meta_if.else_items.clone().unwrap_or_default()
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

// ── Type substitution ──

fn substitute_type(ty: &mut Spanned<Type>, type_env: &FxHashMap<String, Type>) {
    match &mut ty.node {
        Type::Named { name, generic_args } => {
            // Recurse into generic args first
            for arg in generic_args.iter_mut() {
                substitute_type(arg, type_env);
            }
            // Replace bare alias references (no generic args)
            if generic_args.is_empty() {
                if let Some(replacement) = type_env.get(&name.node) {
                    ty.node = replacement.clone();
                }
            }
        }
        Type::Array { element, .. } => substitute_type(element, type_env),
        Type::Slice { element } => substitute_type(element, type_env),
        Type::Tuple(elems) => {
            for e in elems {
                substitute_type(e, type_env);
            }
        }
        Type::Function { return_type, params, .. } => {
            substitute_type(return_type, type_env);
            for p in params {
                substitute_type(p, type_env);
            }
        }
        Type::Primitive(_) | Type::SelfType | Type::Inferred => {}
    }
}

// ── Item / function / block / stmt / expr substitution ──

fn substitute_item(item: &mut Item, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    match item {
        Item::Function(f) => substitute_function(f, env, type_env),
        Item::Equip(eq) => {
            substitute_type(&mut eq.type_, type_env);
            if let Some(trait_) = &mut eq.trait_ {
                substitute_type(&mut trait_.trait_name, type_env);
            }
            for method in &mut eq.items {
                substitute_function(&mut method.node, env, type_env);
            }
        }
        Item::Struct(s) => {
            for field in &mut s.fields {
                substitute_type(&mut field.node.type_, type_env);
            }
        }
        Item::Enum(e) => {
            for variant in &mut e.variants {
                if let VariantFields::Tuple(types) = &mut variant.node.fields {
                    for ty in types {
                        substitute_type(ty, type_env);
                    }
                }
            }
        }
        Item::TypeAlias(ta) => {
            substitute_type(&mut ta.type_, type_env);
        }
        Item::Newtype(nt) => {
            substitute_type(&mut nt.inner_type, type_env);
        }
        Item::ConstDecl(c) => {
            substitute_type(&mut c.type_, type_env);
            substitute_expr(&mut c.value, env, type_env);
        }
        Item::StaticDecl(s) => {
            substitute_type(&mut s.type_, type_env);
            substitute_expr(&mut s.value, env, type_env);
        }
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                match &mut trait_item.node {
                    TraitItem::Method(f) => substitute_function(f, env, type_env),
                    TraitItem::AssociatedType(at) => {
                        if let Some(default) = &mut at.default {
                            substitute_type(default, type_env);
                        }
                    }
                }
            }
        }
        Item::ExternBlock(eb) => {
            for f in &mut eb.items {
                substitute_function(&mut f.node, env, type_env);
            }
        }
        Item::Test(t) => {
            for binding in &mut t.with_bindings {
                substitute_expr(&mut binding.expr, env, type_env);
            }
            substitute_block(&mut t.body, env, type_env);
        }
        Item::SuiteSetup(s) => substitute_block(&mut s.body, env, type_env),
        Item::SuiteTeardown(s) => substitute_block(&mut s.body, env, type_env),
        Item::Import(_) | Item::Directive(_) | Item::MetaConst(_) | Item::MetaType(_)
        | Item::MetaTypeFunc(_) | Item::MetaAssert(_) | Item::MetaIf(_) => {}
    }
}

fn substitute_function(f: &mut FunctionDef, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    substitute_type(&mut f.return_type, type_env);
    for param in &mut f.params {
        substitute_type(&mut param.node.type_, type_env);
        if let Some(default) = &mut param.node.default {
            substitute_expr(default, env, type_env);
        }
    }
    if let Some(throws) = &mut f.throws {
        substitute_type(throws, type_env);
    }
    match &mut f.body {
        FunctionBody::Block(block) => substitute_block(block, env, type_env),
        FunctionBody::Expression(expr) => substitute_expr(expr, env, type_env),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn substitute_block(block: &mut Block, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    for stmt in &mut block.stmts {
        substitute_stmt(&mut stmt.node, env, type_env);
    }
}

fn substitute_stmt(stmt: &mut Stmt, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    match stmt {
        Stmt::VarDecl { type_, value, .. } => {
            substitute_type(type_, type_env);
            substitute_expr(value, env, type_env);
        }
        Stmt::Expr(expr) => substitute_expr(expr, env, type_env),
        Stmt::Assign { target, value } => {
            substitute_expr(target, env, type_env);
            substitute_expr(value, env, type_env);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            substitute_expr(target, env, type_env);
            substitute_expr(value, env, type_env);
        }
        Stmt::Return(Some(expr)) => substitute_expr(expr, env, type_env),
        Stmt::Throw(expr) => substitute_expr(expr, env, type_env),
        Stmt::Break(Some(expr)) => substitute_expr(expr, env, type_env),
        Stmt::Return(None) | Stmt::Break(None) | Stmt::Continue | Stmt::Pass => {}
        Stmt::For { iterable, body, else_body, .. } => {
            substitute_expr(iterable, env, type_env);
            substitute_block(body, env, type_env);
            if let Some(eb) = else_body { substitute_block(eb, env, type_env); }
        }
        Stmt::While { condition, body, else_body } => {
            substitute_expr(condition, env, type_env);
            substitute_block(body, env, type_env);
            if let Some(eb) = else_body { substitute_block(eb, env, type_env); }
        }
        Stmt::Loop { body } => substitute_block(body, env, type_env),
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            substitute_expr(condition, env, type_env);
            substitute_block(then_body, env, type_env);
            for (cond, body) in elif_branches {
                substitute_expr(cond, env, type_env);
                substitute_block(body, env, type_env);
            }
            if let Some(eb) = else_body { substitute_block(eb, env, type_env); }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            substitute_expr(scrutinee, env, type_env);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { substitute_expr(guard, env, type_env); }
                substitute_expr(&mut arm.body, env, type_env);
            }
            if let Some(ea) = else_arm { substitute_block(ea, env, type_env); }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                match &mut arm.op {
                    SelectOp::Recv { type_, channel, .. } => {
                        substitute_type(type_, type_env);
                        substitute_expr(channel, env, type_env);
                    }
                    SelectOp::Send { channel, value } => {
                        substitute_expr(channel, env, type_env);
                        substitute_expr(value, env, type_env);
                    }
                }
                substitute_block(&mut arm.body, env, type_env);
            }
            if let Some(ea) = else_arm { substitute_block(ea, env, type_env); }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                substitute_expr(&mut binding.expr, env, type_env);
            }
            substitute_block(body, env, type_env);
        }
        Stmt::Unsafe { body } => substitute_block(body, env, type_env),
        Stmt::Assert { condition, message } => {
            substitute_expr(condition, env, type_env);
            if let Some(msg) = message { substitute_expr(msg, env, type_env); }
        }
        Stmt::Item(item) => substitute_item(item, env, type_env),
    }
}

fn substitute_expr(expr: &mut Spanned<Expr>, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    // First: recurse into sub-expressions (and substitute types where applicable)
    match &mut expr.node {
        Expr::UnaryOp { operand, .. } => substitute_expr(operand, env, type_env),
        Expr::BinaryOp { left, right, .. } => {
            substitute_expr(left, env, type_env);
            substitute_expr(right, env, type_env);
        }
        Expr::Call { callee, generic_args, args } => {
            substitute_expr(callee, env, type_env);
            if let Some(ga) = generic_args {
                for ty in ga { substitute_type(ty, type_env); }
            }
            for arg in args {
                substitute_expr(&mut arg.node.value, env, type_env);
            }
        }
        Expr::MethodCall { receiver, generic_args, args, .. } => {
            substitute_expr(receiver, env, type_env);
            if let Some(ga) = generic_args {
                for ty in ga { substitute_type(ty, type_env); }
            }
            for arg in args {
                substitute_expr(&mut arg.node.value, env, type_env);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => {
            substitute_expr(object, env, type_env);
        }
        Expr::Index { object, index } => {
            substitute_expr(object, env, type_env);
            substitute_expr(index, env, type_env);
        }
        Expr::NilCoalescing { lhs, rhs } => {
            substitute_expr(lhs, env, type_env);
            substitute_expr(rhs, env, type_env);
        }
        Expr::Try { expr: inner } | Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner } | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner } | Expr::Spawn { expr: inner }
        | Expr::TryCapture { expr: inner } => {
            substitute_expr(inner, env, type_env);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            substitute_expr(condition, env, type_env);
            substitute_expr(then_branch, env, type_env);
            for (cond, body) in elif_branches {
                substitute_expr(cond, env, type_env);
                substitute_expr(body, env, type_env);
            }
            if let Some(eb) = else_branch { substitute_expr(eb, env, type_env); }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            substitute_expr(scrutinee, env, type_env);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { substitute_expr(guard, env, type_env); }
                substitute_expr(&mut arm.body, env, type_env);
            }
            if let Some(ea) = else_arm { substitute_expr(ea, env, type_env); }
        }
        Expr::Block(block) | Expr::Do { body: block } => {
            substitute_block(block, env, type_env);
        }
        Expr::Closure { params, body, .. } => {
            for param in params {
                if let Some(ty) = &mut param.node.type_ {
                    substitute_type(ty, type_env);
                }
            }
            substitute_expr(body, env, type_env);
        }
        Expr::ImplicitClosure { body } => {
            substitute_expr(body, env, type_env);
        }
        Expr::ListComprehension { expr: comp_expr, iterable, condition, .. } => {
            substitute_expr(comp_expr, env, type_env);
            substitute_expr(iterable, env, type_env);
            if let Some(cond) = condition { substitute_expr(cond, env, type_env); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            substitute_expr(key, env, type_env);
            substitute_expr(value, env, type_env);
            substitute_expr(iterable, env, type_env);
            if let Some(cond) = condition { substitute_expr(cond, env, type_env); }
        }
        Expr::SetComprehension { expr: comp_expr, iterable, condition, .. } => {
            substitute_expr(comp_expr, env, type_env);
            substitute_expr(iterable, env, type_env);
            if let Some(cond) = condition { substitute_expr(cond, env, type_env); }
        }
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            for elem in elems { substitute_expr(elem, env, type_env); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                substitute_expr(k, env, type_env);
                substitute_expr(v, env, type_env);
            }
        }
        Expr::StructLiteral { generic_args, args, .. } => {
            if let Some(ga) = generic_args {
                for ty in ga { substitute_type(ty, type_env); }
            }
            for arg in args { substitute_expr(arg, env, type_env); }
        }
        Expr::As { expr: inner, type_ } => {
            substitute_expr(inner, env, type_env);
            substitute_type(type_, type_env);
        }
        Expr::Is { expr: inner, .. } => {
            substitute_expr(inner, env, type_env);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { substitute_expr(s, env, type_env); }
            if let Some(e) = end { substitute_expr(e, env, type_env); }
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

    // ── Meta type alias tests ──

    #[test]
    fn meta_type_alias_substitution() {
        // meta type Num = int
        // Num x = 5  → type becomes int
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaType(MetaType {
                        name: Spanned::new("Num".to_string(), dummy_span()),
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
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
                                Stmt::VarDecl {
                                    is_const: false,
                                    is_mutable: false,
                                    type_: Spanned::new(
                                        Type::Named {
                                            name: Spanned::new("Num".to_string(), dummy_span()),
                                            generic_args: vec![],
                                        },
                                        dummy_span(),
                                    ),
                                    pattern: Spanned::new(
                                        Pattern::Binding("x".to_string()),
                                        dummy_span(),
                                    ),
                                    value: Spanned::new(Expr::IntLiteral(5), dummy_span()),
                                },
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

        // MetaType should be removed
        assert_eq!(module.items.len(), 1);

        // Verify type was substituted to int
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::VarDecl { type_, .. } = &block.stmts[0].node {
                    assert!(
                        matches!(&type_.node, Type::Primitive(PrimitiveType::Int)),
                        "expected Primitive(Int), got: {:?}",
                        type_.node,
                    );
                    return;
                }
            }
        }
        panic!("test structure unexpected");
    }

    // ── Meta if tests ──

    #[test]
    fn meta_if_true_branch() {
        // meta bool FLAG = true
        // meta if FLAG:
        //     fn kept(): ...
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Bool), dummy_span()),
                        name: Spanned::new("FLAG".to_string(), dummy_span()),
                        value: Spanned::new(Expr::BoolLiteral(true), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaIf(MetaIf {
                        condition: Spanned::new(
                            Expr::Identifier("FLAG".to_string()),
                            dummy_span(),
                        ),
                        then_items: vec![Spanned::new(
                            Item::Function(FunctionDef {
                                attributes: vec![],
                                visibility: Visibility::Private,
                                qualifiers: FunctionQualifiers::default(),
                                return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                                name: Spanned::new("kept".to_string(), dummy_span()),
                                generic_params: None,
                                params: vec![],
                                throws: None,
                                where_clause: None,
                                body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )],
                        elif_branches: vec![],
                        else_items: Some(vec![Spanned::new(
                            Item::Function(FunctionDef {
                                attributes: vec![],
                                visibility: Visibility::Private,
                                qualifiers: FunctionQualifiers::default(),
                                return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                                name: Spanned::new("dropped".to_string(), dummy_span()),
                                generic_params: None,
                                params: vec![],
                                throws: None,
                                where_clause: None,
                                body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )]),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");

        // MetaConst removed, MetaIf replaced with then branch
        assert_eq!(module.items.len(), 1);
        if let Item::Function(f) = &module.items[0].node {
            assert_eq!(f.name.node, "kept");
        } else {
            panic!("expected Function(kept)");
        }
    }

    #[test]
    fn meta_if_false_else() {
        // meta bool FLAG = false
        // meta if FLAG:
        //     fn dropped(): ...
        // else:
        //     fn kept(): ...
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Bool), dummy_span()),
                        name: Spanned::new("FLAG".to_string(), dummy_span()),
                        value: Spanned::new(Expr::BoolLiteral(false), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaIf(MetaIf {
                        condition: Spanned::new(
                            Expr::Identifier("FLAG".to_string()),
                            dummy_span(),
                        ),
                        then_items: vec![Spanned::new(
                            Item::Function(FunctionDef {
                                attributes: vec![],
                                visibility: Visibility::Private,
                                qualifiers: FunctionQualifiers::default(),
                                return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                                name: Spanned::new("dropped".to_string(), dummy_span()),
                                generic_params: None,
                                params: vec![],
                                throws: None,
                                where_clause: None,
                                body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )],
                        elif_branches: vec![],
                        else_items: Some(vec![Spanned::new(
                            Item::Function(FunctionDef {
                                attributes: vec![],
                                visibility: Visibility::Private,
                                qualifiers: FunctionQualifiers::default(),
                                return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                                name: Spanned::new("kept".to_string(), dummy_span()),
                                generic_params: None,
                                params: vec![],
                                throws: None,
                                where_clause: None,
                                body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )]),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");

        assert_eq!(module.items.len(), 1);
        if let Item::Function(f) = &module.items[0].node {
            assert_eq!(f.name.node, "kept");
        } else {
            panic!("expected Function(kept)");
        }
    }

    #[test]
    fn meta_if_false_no_else() {
        // meta bool FLAG = false
        // meta if FLAG:
        //     fn dropped(): ...
        // (no else)
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Bool), dummy_span()),
                        name: Spanned::new("FLAG".to_string(), dummy_span()),
                        value: Spanned::new(Expr::BoolLiteral(false), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaIf(MetaIf {
                        condition: Spanned::new(
                            Expr::Identifier("FLAG".to_string()),
                            dummy_span(),
                        ),
                        then_items: vec![Spanned::new(
                            Item::Function(FunctionDef {
                                attributes: vec![],
                                visibility: Visibility::Private,
                                qualifiers: FunctionQualifiers::default(),
                                return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                                name: Spanned::new("dropped".to_string(), dummy_span()),
                                generic_params: None,
                                params: vec![],
                                throws: None,
                                where_clause: None,
                                body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )],
                        elif_branches: vec![],
                        else_items: None,
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };

        let errors = evaluate_meta_consts(&mut module);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");

        // MetaConst removed, MetaIf removed, nothing emitted
        assert!(module.items.is_empty());
    }

    #[test]
    fn meta_if_nested_const() {
        // meta bool FLAG = true
        // meta if FLAG:
        //     meta int BONUS = 42
        // → BONUS should be available for substitution
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Bool), dummy_span()),
                        name: Spanned::new("FLAG".to_string(), dummy_span()),
                        value: Spanned::new(Expr::BoolLiteral(true), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaIf(MetaIf {
                        condition: Spanned::new(
                            Expr::Identifier("FLAG".to_string()),
                            dummy_span(),
                        ),
                        then_items: vec![Spanned::new(
                            Item::MetaConst(MetaConst {
                                type_: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                                name: Spanned::new("BONUS".to_string(), dummy_span()),
                                value: Spanned::new(Expr::IntLiteral(42), dummy_span()),
                                span: dummy_span(),
                            }),
                            dummy_span(),
                        )],
                        elif_branches: vec![],
                        else_items: None,
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                // Function that uses BONUS
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
                                                    Expr::Identifier("BONUS".to_string()),
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

        // All meta items removed, only main() remains
        assert_eq!(module.items.len(), 1);
        assert!(matches!(&module.items[0].node, Item::Function(_)));

        // BONUS should be substituted with 42
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
}
