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
    List(Vec<MetaValue>),
}

const MAX_META_RECURSION: usize = 256;
const MAX_META_ITERATIONS: u64 = 100_000;

/// Context threaded through meta evaluation for built-in function access.
struct MetaContext<'a> {
    /// Build-time feature flags (from `--feature` CLI args).
    features: &'a [String],
    /// Module items — used by M7 to look up user-defined functions for compile-time evaluation.
    items: &'a [Spanned<Item>],
    /// Current call depth for recursion limit enforcement.
    call_depth: std::cell::Cell<usize>,
}

impl<'a> MetaContext<'a> {
    fn new(features: &'a [String], items: &'a [Spanned<Item>]) -> Self {
        Self { features, items, call_depth: std::cell::Cell::new(0) }
    }

    #[allow(dead_code)]
    fn empty() -> MetaContext<'static> {
        MetaContext { features: &[], items: &[], call_depth: std::cell::Cell::new(0) }
    }
}

// ═══════════════════════════════════════════════════════════════
// M7: MetaControlFlow — propagates return/break/continue
// ═══════════════════════════════════════════════════════════════

enum MetaControlFlow {
    /// Statement completed normally; continue to next statement.
    Continue,
    /// A `return` statement was hit; the function should return this value.
    Return(MetaValue),
    /// A `break` statement was hit; exit the current loop.
    Break,
    /// A `continue` statement was hit; go to the next loop iteration.
    LoopContinue,
}

// ═══════════════════════════════════════════════════════════════
// Public entry point
// ═══════════════════════════════════════════════════════════════

/// Evaluate, substitute, and remove all meta constructs from a module.
/// `features` is the list of enabled build-time feature flags (from `--feature` CLI args).
pub fn evaluate_meta_consts(module: &mut Module, features: &[String]) -> Vec<SemanticError> {
    let mut errors = Vec::new();
    let mut env: FxHashMap<String, MetaValue> = FxHashMap::default();
    let mut type_env: FxHashMap<String, Type> = FxHashMap::default();
    let mut type_func_env: FxHashMap<String, MetaTypeFunc> = FxHashMap::default();

    // Phase 1: Evaluate meta consts, meta asserts, meta type aliases, and meta type functions.
    // Scope the ctx borrow so it ends before we mutate module.items in Phase 1.5.
    {
        let ctx = MetaContext::new(features, &module.items);
        for item in &module.items {
            process_meta_item(&item.node, &mut env, &mut type_env, &mut type_func_env, &ctx, &mut errors);
        }
    }

    // Phase 1.5: Flatten MetaIf (conditional compilation).
    // Snapshot the current items for the context so user-defined functions are still accessible.
    let items_snapshot = module.items.clone();
    {
        let ctx = MetaContext::new(features, &items_snapshot);
        module.items = flatten_meta_ifs(module.items.clone(), &mut env, &mut type_env, &mut type_func_env, &ctx, &mut errors);
    }

    // Phase 2: Substitute meta const references and type aliases throughout the AST
    for item in &mut module.items {
        substitute_item(&mut item.node, &env, &type_env);
    }

    // Phase 3: Remove all meta declarations
    module.items.retain(|item| {
        !matches!(
            &item.node,
            Item::MetaConst(_) | Item::MetaAssert(_) | Item::MetaType(_)
            | Item::MetaTypeFunc(_) | Item::MetaIf(_)
        )
    });

    errors
}

/// Process a single meta item: MetaConst, MetaAssert, MetaType, or MetaTypeFunc.
fn process_meta_item(
    item: &Item,
    env: &mut FxHashMap<String, MetaValue>,
    type_env: &mut FxHashMap<String, Type>,
    type_func_env: &mut FxHashMap<String, MetaTypeFunc>,
    ctx: &MetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) {
    match item {
        Item::MetaConst(mc) => {
            match eval_expr(&mc.value.node, env, ctx, mc.value.span) {
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
            match eval_expr(&ma.condition.node, env, ctx, ma.condition.span) {
                Ok(MetaValue::Bool(true)) => {} // assertion passes
                Ok(MetaValue::Bool(false)) => {
                    let msg = if let Some(msg_expr) = &ma.message {
                        match eval_expr(&msg_expr.node, env, ctx, msg_expr.span) {
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
            match resolve_meta_type_rhs(&mt.rhs, env, type_env, type_func_env, ctx, mt.span) {
                Ok(resolved) => { type_env.insert(mt.name.node.clone(), resolved); }
                Err(e) => errors.push(e),
            }
        }
        Item::MetaTypeFunc(mtf) => {
            type_func_env.insert(mtf.name.node.clone(), mtf.clone());
        }
        _ => {}
    }
}

// ═══════════════════════════════════════════════════════════════
// M5/M6: MetaTypeRhs resolution
// ═══════════════════════════════════════════════════════════════

/// Resolve a `MetaTypeRhs` to a concrete `Type`.
fn resolve_meta_type_rhs(
    rhs: &MetaTypeRhs,
    env: &FxHashMap<String, MetaValue>,
    type_env: &FxHashMap<String, Type>,
    type_func_env: &FxHashMap<String, MetaTypeFunc>,
    ctx: &MetaContext<'_>,
    span: Span,
) -> Result<Type, SemanticError> {
    match rhs {
        MetaTypeRhs::Plain(ty) => Ok(resolve_type_via_env(&ty.node, type_env)),
        MetaTypeRhs::Conditional { then_type, condition, else_type } => {
            match eval_expr(&condition.node, env, ctx, condition.span)? {
                MetaValue::Bool(true)  => Ok(resolve_type_via_env(&then_type.node, type_env)),
                MetaValue::Bool(false) => Ok(resolve_type_via_env(&else_type.node, type_env)),
                _ => Err(meta_err("conditional type condition must be bool", condition.span)),
            }
        }
        MetaTypeRhs::Call { callee, args } => {
            let func = type_func_env.get(&callee.node).ok_or_else(|| {
                meta_err(
                    &format!("unknown meta type function `{}`", callee.node),
                    callee.span,
                )
            })?;
            let func = func.clone();
            call_meta_type_func(&func, args, env, type_env, type_func_env, ctx, span)
        }
    }
}

/// Resolve a type through the type alias environment.
/// For bare named types, substitutes them if they exist as aliases.
fn resolve_type_via_env(ty: &Type, type_env: &FxHashMap<String, Type>) -> Type {
    match ty {
        Type::Named { name, generic_args } if generic_args.is_empty() => {
            if let Some(resolved) = type_env.get(&name.node) {
                resolved.clone()
            } else {
                ty.clone()
            }
        }
        _ => ty.clone(),
    }
}

/// Call a meta type function, binding args to params and interpreting the body.
fn call_meta_type_func(
    func: &MetaTypeFunc,
    args: &[Spanned<Expr>],
    env: &FxHashMap<String, MetaValue>,
    type_env: &FxHashMap<String, Type>,
    type_func_env: &FxHashMap<String, MetaTypeFunc>,
    ctx: &MetaContext<'_>,
    call_span: Span,
) -> Result<Type, SemanticError> {
    if args.len() != func.params.len() {
        return Err(meta_err(
            &format!(
                "meta type function `{}` expects {} argument(s), got {}",
                func.name.node,
                func.params.len(),
                args.len()
            ),
            call_span,
        ));
    }

    // Evaluate args and bind to params in a local env
    let mut local_env = env.clone();
    for (param, arg) in func.params.iter().zip(args.iter()) {
        let val = eval_expr(&arg.node, env, ctx, arg.span)?;
        if let Err(e) = validate_type(&param.node.type_.node, &val, arg.span) {
            return Err(e);
        }
        local_env.insert(param.node.name.node.clone(), val);
    }

    eval_meta_type_body(&func.body, &local_env, type_env, type_func_env, ctx, call_span)
}

/// Interpret a meta type function body, returning the resolved `Type`.
fn eval_meta_type_body(
    body: &Block,
    env: &FxHashMap<String, MetaValue>,
    type_env: &FxHashMap<String, Type>,
    type_func_env: &FxHashMap<String, MetaTypeFunc>,
    ctx: &MetaContext<'_>,
    span: Span,
) -> Result<Type, SemanticError> {
    for stmt in &body.stmts {
        match &stmt.node {
            Stmt::Return(Some(expr)) => {
                return resolve_expr_as_type(&expr.node, type_env, expr.span);
            }
            Stmt::Return(None) => {
                return Err(meta_err("meta type function must return a type", stmt.span));
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                match eval_expr(&condition.node, env, ctx, condition.span)? {
                    MetaValue::Bool(true) => {
                        return eval_meta_type_body(then_body, env, type_env, type_func_env, ctx, span);
                    }
                    MetaValue::Bool(false) => {
                        // Try elif branches
                        for (elif_cond, elif_body) in elif_branches {
                            match eval_expr(&elif_cond.node, env, ctx, elif_cond.span)? {
                                MetaValue::Bool(true) => {
                                    return eval_meta_type_body(elif_body, env, type_env, type_func_env, ctx, span);
                                }
                                MetaValue::Bool(false) => {}
                                _ => {
                                    return Err(meta_err(
                                        "meta type function elif condition must be bool",
                                        elif_cond.span,
                                    ));
                                }
                            }
                        }
                        // Try else
                        if let Some(else_blk) = else_body {
                            return eval_meta_type_body(else_blk, env, type_env, type_func_env, ctx, span);
                        }
                        // No branch taken — continue to next statement (none should follow in valid code)
                    }
                    _ => {
                        return Err(meta_err(
                            "meta type function if condition must be bool",
                            condition.span,
                        ));
                    }
                }
            }
            Stmt::Pass => {} // skip
            _ => {}          // skip other statements
        }
    }
    Err(meta_err("meta type function did not return a type", span))
}

/// Map an expression (as it appears in a `return` statement in a type function body)
/// back to a `Type`. Only simple identifiers and primitive type names are supported.
fn resolve_expr_as_type(
    expr: &Expr,
    type_env: &FxHashMap<String, Type>,
    span: Span,
) -> Result<Type, SemanticError> {
    if let Expr::Identifier(name) = expr {
        // Check meta type aliases first
        if let Some(resolved) = type_env.get(name.as_str()) {
            return Ok(resolved.clone());
        }
        // Primitive type names
        let prim = match name.as_str() {
            "int"    => Some(PrimitiveType::Int),
            "int8"   => Some(PrimitiveType::Int8),
            "int16"  => Some(PrimitiveType::Int16),
            "int32"  => Some(PrimitiveType::Int32),
            "int64"  => Some(PrimitiveType::Int64),
            "uint"   => Some(PrimitiveType::Uint),
            "uint8"  => Some(PrimitiveType::Uint8),
            "uint16" => Some(PrimitiveType::Uint16),
            "uint32" => Some(PrimitiveType::Uint32),
            "uint64" => Some(PrimitiveType::Uint64),
            "float"  => Some(PrimitiveType::Float),
            "float32"=> Some(PrimitiveType::Float32),
            "float64"=> Some(PrimitiveType::Float64),
            "bool"   => Some(PrimitiveType::Bool),
            "str"    => Some(PrimitiveType::Str),
            "void"   => Some(PrimitiveType::Void),
            _ => None,
        };
        if let Some(p) = prim {
            return Ok(Type::Primitive(p));
        }
        // Bare user-defined type name
        return Ok(Type::Named {
            name: Spanned::new(name.clone(), span),
            generic_args: vec![],
        });
    }
    Err(meta_err(
        "meta type function return value must be a type name",
        span,
    ))
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
    type_func_env: &mut FxHashMap<String, MetaTypeFunc>,
    ctx: &MetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) -> Vec<Spanned<Item>> {
    let mut result = items;
    loop {
        let mut changed = false;
        let mut new_items = Vec::with_capacity(result.len());
        for item in result {
            if let Item::MetaIf(meta_if) = &item.node {
                changed = true;
                let winning = pick_meta_if_branch(meta_if, env, ctx, errors);
                // Process any meta declarations in the winning branch
                for won_item in &winning {
                    process_meta_item(&won_item.node, env, type_env, type_func_env, ctx, errors);
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
    ctx: &MetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) -> Vec<Spanned<Item>> {
    // Try the main condition
    match eval_expr(&meta_if.condition.node, env, ctx, meta_if.condition.span) {
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
        match eval_expr(&cond.node, env, ctx, cond.span) {
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
    ctx: &MetaContext<'_>,
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
            let val = eval_expr(&operand.node, env, ctx, operand.span)?;
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
            let lhs = eval_expr(&left.node, env, ctx, left.span)?;
            let rhs = eval_expr(&right.node, env, ctx, right.span)?;
            eval_binary_op(&lhs, *op, &rhs, span)
        }

        // Built-in meta functions: platform(), arch(), arch_word_bits(), feature(), debug(),
        //                          sizeof(Type), alignof(Type), typename(Type)
        Expr::Call { callee, args, .. } => {
            if let Expr::Identifier(name) = &callee.node {
                match name.as_str() {
                    "platform" => {
                        let platform = if cfg!(target_os = "macos") { "macos" }
                            else if cfg!(target_os = "linux") { "linux" }
                            else if cfg!(target_os = "windows") { "windows" }
                            else { "unknown" };
                        Ok(MetaValue::Str(platform.to_string()))
                    }
                    "arch" => {
                        let arch = if cfg!(target_arch = "x86_64") { "x86_64" }
                            else if cfg!(target_arch = "aarch64") { "aarch64" }
                            else if cfg!(target_arch = "arm") { "arm" }
                            else if cfg!(target_arch = "wasm32") { "wasm32" }
                            else { "unknown" };
                        Ok(MetaValue::Str(arch.to_string()))
                    }
                    "arch_word_bits" => {
                        Ok(MetaValue::Int((std::mem::size_of::<usize>() * 8) as i64))
                    }
                    "feature" => {
                        if args.len() != 1 {
                            return Err(meta_err("feature() takes exactly 1 argument", span));
                        }
                        let name_val = eval_expr(&args[0].node.value.node, env, ctx, args[0].node.value.span)?;
                        match name_val {
                            MetaValue::Str(feature_name) => {
                                Ok(MetaValue::Bool(ctx.features.iter().any(|f| f == &feature_name)))
                            }
                            _ => Err(meta_err("feature() argument must be a string literal", span)),
                        }
                    }
                    "debug" => {
                        Ok(MetaValue::Bool(ctx.features.iter().any(|f| f == "debug")))
                    }
                    "sizeof" => {
                        if args.len() != 1 {
                            return Err(meta_err("sizeof() takes exactly 1 argument", span));
                        }
                        let type_name = meta_expr_to_type_name(&args[0].node.value.node);
                        match meta_type_byte_size(&type_name) {
                            Some(size) => Ok(MetaValue::Int(size)),
                            None => Err(meta_err(
                                &format!("sizeof({type_name}): size unknown at compile time — \
                                    only primitive types (int, bool, str, cstr, etc.) are \
                                    supported; for generic types, use a meta type alias"),
                                span,
                            )),
                        }
                    }
                    "alignof" => {
                        if args.len() != 1 {
                            return Err(meta_err("alignof() takes exactly 1 argument", span));
                        }
                        let type_name = meta_expr_to_type_name(&args[0].node.value.node);
                        match meta_type_align_bytes(&type_name) {
                            Some(align) => Ok(MetaValue::Int(align)),
                            None => Err(meta_err(
                                &format!("alignof({type_name}): alignment unknown at compile time — \
                                    only primitive types are supported"),
                                span,
                            )),
                        }
                    }
                    "typename" => {
                        if args.len() != 1 {
                            return Err(meta_err("typename() takes exactly 1 argument", span));
                        }
                        let type_name = meta_expr_to_type_name(&args[0].node.value.node);
                        Ok(MetaValue::Str(type_name))
                    }
                    other => {
                        // M7: fall back to user-defined function lookup
                        match lookup_meta_function(other, ctx.items, span)? {
                            Some(func_def) => eval_meta_fn_call(&func_def, args, env, ctx, span),
                            None => Err(meta_err(
                                &format!("unknown meta function `{other}` — built-ins: \
                                    platform(), arch(), arch_word_bits(), feature(str), debug(), \
                                    sizeof(Type), alignof(Type), typename(Type); \
                                    or define a pure function in the same file"),
                                span,
                            )),
                        }
                    }
                }
            } else {
                Err(meta_err("meta function calls must use a simple function name", span))
            }
        }

        // M7: expression-position if (ternary-style)
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            match eval_expr(&condition.node, env, ctx, condition.span)? {
                MetaValue::Bool(true) => eval_expr(&then_branch.node, env, ctx, then_branch.span),
                MetaValue::Bool(false) => {
                    for (elif_cond, elif_val) in elif_branches {
                        match eval_expr(&elif_cond.node, env, ctx, elif_cond.span)? {
                            MetaValue::Bool(true) => {
                                return eval_expr(&elif_val.node, env, ctx, elif_val.span);
                            }
                            MetaValue::Bool(false) => {}
                            _ => return Err(meta_err("elif condition must evaluate to bool", elif_cond.span)),
                        }
                    }
                    if let Some(else_br) = else_branch {
                        eval_expr(&else_br.node, env, ctx, else_br.span)
                    } else {
                        Err(meta_err("if expression without else cannot be used in a meta context", span))
                    }
                }
                _ => Err(meta_err("if condition must evaluate to bool", condition.span)),
            }
        }

        _ => Err(meta_err("expression cannot be evaluated at compile time", span)),
    }
}

/// Convert a meta-context expression (which the parser has already parsed as a value
/// expression) back to a type name string. Type keywords appear as `Expr::Identifier`
/// (e.g. `int` → `Identifier("int")`), and single-arg generic types appear as
/// `Expr::Index` (e.g. `Vector[int]` → `Index { object: Identifier("Vector"),
/// index: Identifier("int") }`).
fn meta_expr_to_type_name(expr: &Expr) -> String {
    match expr {
        Expr::Identifier(name) => name.clone(),
        Expr::Index { object, index } => {
            let base = meta_expr_to_type_name(&object.node);
            let idx  = meta_expr_to_type_name(&index.node);
            format!("{base}[{idx}]")
        }
        _ => "?".to_string(),
    }
}

/// Byte size of a Gorget type as it is laid out in C on a 64-bit target.
/// Only primitive and built-in types are supported; user struct sizes are not
/// known during meta evaluation (which runs before layout computation).
fn meta_type_byte_size(name: &str) -> Option<i64> {
    match name {
        "bool"                          => Some(1),
        "int8"  | "uint8"              => Some(1),
        "int16" | "uint16"             => Some(2),
        "int32" | "uint32" | "float32" => Some(4),
        // int, uint, float, double — all 64-bit on Gorget's only current target
        "int"   | "int64"              => Some(8),
        "uint"  | "uint64"             => Some(8),
        "float" | "float64"            => Some(8),
        // str  = Str  { *u8 data, u64 len }            → 16 bytes
        "str"  => Some(16),
        // cstr = const char*                           →  8 bytes
        "cstr" => Some(8),
        // String = GorgetString { *u8, u64, u64, *Alloc } → 32 bytes
        "String" => Some(32),
        _      => None,
    }
}

/// Required alignment (in bytes) for a Gorget type on a 64-bit target.
fn meta_type_align_bytes(name: &str) -> Option<i64> {
    match name {
        "bool"                          => Some(1),
        "int8"  | "uint8"              => Some(1),
        "int16" | "uint16"             => Some(2),
        "int32" | "uint32" | "float32" => Some(4),
        "int"   | "int64"              => Some(8),
        "uint"  | "uint64"             => Some(8),
        "float" | "float64"            => Some(8),
        "str"    => Some(8),   // largest field is a pointer
        "cstr"   => Some(8),   // pointer-aligned
        "String" => Some(8),   // largest field is a pointer
        _        => None,
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
        (MetaValue::Int(a), BinaryOp::Rem, MetaValue::Int(b)) => {
            if *b == 0 {
                Err(meta_err("remainder by zero", span))
            } else {
                Ok(MetaValue::Int(a % b))
            }
        }
        (MetaValue::Int(a), BinaryOp::Mod, MetaValue::Int(b)) => {
            if *b == 0 {
                Err(meta_err("modulo by zero", span))
            } else {
                let r = a % b;
                Ok(MetaValue::Int(if r != 0 && ((r ^ b) < 0) { r + b } else { r }))
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
// M7: Compile-time user-defined function evaluation
// ═══════════════════════════════════════════════════════════════

/// Returns true if a type is supported as a meta function parameter or return type.
fn is_meta_compatible_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            PrimitiveType::Int
            | PrimitiveType::Int8
            | PrimitiveType::Int16
            | PrimitiveType::Int32
            | PrimitiveType::Int64
            | PrimitiveType::Uint
            | PrimitiveType::Uint8
            | PrimitiveType::Uint16
            | PrimitiveType::Uint32
            | PrimitiveType::Uint64
            | PrimitiveType::Float
            | PrimitiveType::Float32
            | PrimitiveType::Float64
            | PrimitiveType::Bool
            | PrimitiveType::Str
        )
    )
}

/// Scan `items` for a user-defined function named `name`.
/// Returns `Ok(Some(def))` if found and valid for compile-time evaluation,
/// `Ok(None)` if not found, or `Err` if found but invalid (generic, async, etc.).
fn lookup_meta_function(
    name: &str,
    items: &[Spanned<Item>],
    span: Span,
) -> Result<Option<FunctionDef>, SemanticError> {
    for item in items {
        if let Item::Function(f) = &item.node {
            if f.name.node == name {
                if f.generic_params.is_some() {
                    return Err(meta_err(
                        &format!("generic function `{name}` cannot be evaluated at compile time"),
                        span,
                    ));
                }
                if f.qualifiers.is_async {
                    return Err(meta_err(
                        &format!("async function `{name}` cannot be evaluated at compile time"),
                        span,
                    ));
                }
                if f.qualifiers.is_unsafe {
                    return Err(meta_err(
                        &format!("unsafe function `{name}` cannot be evaluated at compile time"),
                        span,
                    ));
                }
                for param in &f.params {
                    if !is_meta_compatible_type(&param.node.type_.node) {
                        return Err(meta_err(
                            &format!(
                                "parameter `{}` of function `{name}` has a type not supported \
                                 for compile-time evaluation (only int, float, bool, str allowed)",
                                param.node.name.node
                            ),
                            span,
                        ));
                    }
                }
                if !is_meta_compatible_type(&f.return_type.node)
                    && !matches!(f.return_type.node, Type::Primitive(PrimitiveType::Void))
                {
                    return Err(meta_err(
                        &format!(
                            "return type of function `{name}` is not supported for \
                             compile-time evaluation (only int, float, bool, str allowed)"
                        ),
                        span,
                    ));
                }
                return Ok(Some(f.clone()));
            }
        }
    }
    Ok(None)
}

/// Call a user-defined function at compile time.
fn eval_meta_fn_call(
    func: &FunctionDef,
    args: &[Spanned<CallArg>],
    env: &FxHashMap<String, MetaValue>,
    ctx: &MetaContext<'_>,
    call_span: Span,
) -> Result<MetaValue, SemanticError> {
    let depth = ctx.call_depth.get();
    if depth >= MAX_META_RECURSION {
        return Err(meta_err(
            &format!(
                "compile-time recursion limit ({MAX_META_RECURSION}) exceeded \
                 in function `{}`",
                func.name.node
            ),
            call_span,
        ));
    }

    if args.len() != func.params.len() {
        return Err(meta_err(
            &format!(
                "function `{}` expects {} argument(s), got {}",
                func.name.node,
                func.params.len(),
                args.len()
            ),
            call_span,
        ));
    }

    // Bind arguments to parameters in a fresh local environment.
    let mut local_env = env.clone();
    for (param, arg) in func.params.iter().zip(args.iter()) {
        let val = eval_expr(&arg.node.value.node, env, ctx, arg.node.value.span)?;
        validate_type(&param.node.type_.node, &val, arg.node.value.span)?;
        local_env.insert(param.node.name.node.clone(), val);
    }

    ctx.call_depth.set(depth + 1);
    let result = eval_meta_fn_body(&func.body, &mut local_env, ctx, func.span);
    ctx.call_depth.set(depth);
    result
}

/// Dispatch on a function body and evaluate it.
fn eval_meta_fn_body(
    body: &FunctionBody,
    env: &mut FxHashMap<String, MetaValue>,
    ctx: &MetaContext<'_>,
    fn_span: Span,
) -> Result<MetaValue, SemanticError> {
    match body {
        FunctionBody::Block(block) => {
            match eval_meta_block(block, env, ctx)? {
                MetaControlFlow::Return(v) => Ok(v),
                MetaControlFlow::Continue => {
                    Err(meta_err("compile-time function did not return a value", fn_span))
                }
                MetaControlFlow::Break | MetaControlFlow::LoopContinue => {
                    Err(meta_err("unexpected break/continue outside a loop", fn_span))
                }
            }
        }
        FunctionBody::Expression(expr) => eval_expr(&expr.node, env, ctx, expr.span),
        FunctionBody::Declaration | FunctionBody::Extern(_) => Err(meta_err(
            "extern/declaration functions cannot be called at compile time",
            fn_span,
        )),
    }
}

/// Execute a block, returning the first non-Continue control-flow signal.
fn eval_meta_block(
    block: &Block,
    env: &mut FxHashMap<String, MetaValue>,
    ctx: &MetaContext<'_>,
) -> Result<MetaControlFlow, SemanticError> {
    for stmt in &block.stmts {
        match eval_meta_stmt(&stmt.node, env, ctx, stmt.span)? {
            MetaControlFlow::Continue => {}
            flow => return Ok(flow),
        }
    }
    Ok(MetaControlFlow::Continue)
}

/// Execute a single statement. Returns a `MetaControlFlow` signal.
fn eval_meta_stmt(
    stmt: &Stmt,
    env: &mut FxHashMap<String, MetaValue>,
    ctx: &MetaContext<'_>,
    stmt_span: Span,
) -> Result<MetaControlFlow, SemanticError> {
    match stmt {
        Stmt::Pass => Ok(MetaControlFlow::Continue),

        Stmt::Return(None) => {
            Err(meta_err("compile-time function must return a value (bare `return` not allowed)", stmt_span))
        }
        Stmt::Return(Some(expr)) => {
            let val = eval_expr(&expr.node, env, ctx, expr.span)?;
            Ok(MetaControlFlow::Return(val))
        }

        Stmt::Break(_) => Ok(MetaControlFlow::Break),
        Stmt::Continue => Ok(MetaControlFlow::LoopContinue),

        Stmt::Expr(expr) => {
            // Evaluate for potential side-effect-free function call; discard result.
            eval_expr(&expr.node, env, ctx, expr.span)?;
            Ok(MetaControlFlow::Continue)
        }

        Stmt::VarDecl { pattern, value, .. } => {
            let val = eval_expr(&value.node, env, ctx, value.span)?;
            match &pattern.node {
                Pattern::Binding(name) => {
                    env.insert(name.clone(), val);
                }
                _ => {
                    return Err(meta_err(
                        "only simple variable bindings are supported in compile-time functions",
                        pattern.span,
                    ));
                }
            }
            Ok(MetaControlFlow::Continue)
        }

        Stmt::Assign { target, value } => {
            let val = eval_expr(&value.node, env, ctx, value.span)?;
            match &target.node {
                Expr::Identifier(name) => {
                    if env.contains_key(name.as_str()) {
                        env.insert(name.clone(), val);
                        Ok(MetaControlFlow::Continue)
                    } else {
                        Err(meta_err(
                            &format!("assignment to undeclared variable `{name}`"),
                            target.span,
                        ))
                    }
                }
                _ => Err(meta_err(
                    "only simple variable assignments are supported in compile-time functions",
                    target.span,
                )),
            }
        }

        Stmt::CompoundAssign { target, op, value } => {
            let rhs = eval_expr(&value.node, env, ctx, value.span)?;
            match &target.node {
                Expr::Identifier(name) => {
                    let lhs = env.get(name.as_str()).cloned().ok_or_else(|| {
                        meta_err(&format!("undeclared variable `{name}`"), target.span)
                    })?;
                    let result = eval_binary_op(&lhs, *op, &rhs, stmt_span)?;
                    env.insert(name.clone(), result);
                    Ok(MetaControlFlow::Continue)
                }
                _ => Err(meta_err(
                    "only simple compound assignments are supported in compile-time functions",
                    target.span,
                )),
            }
        }

        Stmt::If { condition, then_body, elif_branches, else_body } => {
            match eval_expr(&condition.node, env, ctx, condition.span)? {
                MetaValue::Bool(true) => eval_meta_block(then_body, env, ctx),
                MetaValue::Bool(false) => {
                    for (elif_cond, elif_body) in elif_branches {
                        match eval_expr(&elif_cond.node, env, ctx, elif_cond.span)? {
                            MetaValue::Bool(true) => return eval_meta_block(elif_body, env, ctx),
                            MetaValue::Bool(false) => {}
                            _ => {
                                return Err(meta_err("elif condition must be bool", elif_cond.span))
                            }
                        }
                    }
                    if let Some(else_blk) = else_body {
                        eval_meta_block(else_blk, env, ctx)
                    } else {
                        Ok(MetaControlFlow::Continue)
                    }
                }
                _ => Err(meta_err("if condition must be bool", condition.span)),
            }
        }

        Stmt::While { condition, body, .. } => {
            let mut iterations: u64 = 0;
            loop {
                match eval_expr(&condition.node, env, ctx, condition.span)? {
                    MetaValue::Bool(false) => break,
                    MetaValue::Bool(true) => {}
                    _ => {
                        return Err(meta_err("while condition must be bool", condition.span))
                    }
                }
                iterations += 1;
                if iterations > MAX_META_ITERATIONS {
                    return Err(meta_err(
                        &format!(
                            "compile-time iteration limit ({MAX_META_ITERATIONS}) exceeded"
                        ),
                        condition.span,
                    ));
                }
                match eval_meta_block(body, env, ctx)? {
                    MetaControlFlow::Continue | MetaControlFlow::LoopContinue => {}
                    MetaControlFlow::Break => break,
                    r @ MetaControlFlow::Return(_) => return Ok(r),
                }
            }
            Ok(MetaControlFlow::Continue)
        }

        Stmt::Loop { body } => {
            let mut iterations: u64 = 0;
            loop {
                iterations += 1;
                if iterations > MAX_META_ITERATIONS {
                    return Err(meta_err(
                        &format!(
                            "compile-time iteration limit ({MAX_META_ITERATIONS}) exceeded"
                        ),
                        stmt_span,
                    ));
                }
                match eval_meta_block(body, env, ctx)? {
                    MetaControlFlow::Continue | MetaControlFlow::LoopContinue => {}
                    MetaControlFlow::Break => break,
                    r @ MetaControlFlow::Return(_) => return Ok(r),
                }
            }
            Ok(MetaControlFlow::Continue)
        }

        Stmt::For { pattern, iterable, body, .. } => {
            // Only integer range iteration is supported at compile time.
            match &iterable.node {
                Expr::Range { start, end, inclusive } => {
                    let start_val = match start {
                        Some(s) => match eval_expr(&s.node, env, ctx, s.span)? {
                            MetaValue::Int(n) => n,
                            _ => return Err(meta_err("range start must be int", s.span)),
                        },
                        None => 0,
                    };
                    let end_val = match end {
                        Some(e) => match eval_expr(&e.node, env, ctx, e.span)? {
                            MetaValue::Int(n) => n,
                            _ => return Err(meta_err("range end must be int", e.span)),
                        },
                        None => {
                            return Err(meta_err(
                                "open-ended range not supported in compile-time for-loop",
                                iterable.span,
                            ))
                        }
                    };
                    let loop_var = match &pattern.node {
                        Pattern::Binding(name) => name.clone(),
                        _ => {
                            return Err(meta_err(
                                "only simple variable bindings are supported in \
                                 compile-time for-loop patterns",
                                pattern.span,
                            ))
                        }
                    };
                    let upper = if *inclusive { end_val + 1 } else { end_val };
                    let mut iterations: u64 = 0;
                    let mut i = start_val;
                    while i < upper {
                        iterations += 1;
                        if iterations > MAX_META_ITERATIONS {
                            return Err(meta_err(
                                &format!(
                                    "compile-time iteration limit ({MAX_META_ITERATIONS}) exceeded"
                                ),
                                iterable.span,
                            ));
                        }
                        env.insert(loop_var.clone(), MetaValue::Int(i));
                        match eval_meta_block(body, env, ctx)? {
                            MetaControlFlow::Continue | MetaControlFlow::LoopContinue => {}
                            MetaControlFlow::Break => break,
                            r @ MetaControlFlow::Return(_) => return Ok(r),
                        }
                        i += 1;
                    }
                    Ok(MetaControlFlow::Continue)
                }
                _ => Err(meta_err(
                    "only range-based for-loops are supported in compile-time functions",
                    iterable.span,
                )),
            }
        }

        Stmt::Assert { condition, message } => {
            match eval_expr(&condition.node, env, ctx, condition.span)? {
                MetaValue::Bool(true) => Ok(MetaControlFlow::Continue),
                MetaValue::Bool(false) => {
                    let msg = if let Some(msg_expr) = message {
                        match eval_expr(&msg_expr.node, env, ctx, msg_expr.span) {
                            Ok(v) => meta_value_to_string(&v),
                            Err(_) => "assertion failed".to_string(),
                        }
                    } else {
                        "assertion failed".to_string()
                    };
                    Err(meta_err(&msg, condition.span))
                }
                _ => Err(meta_err("assert condition must be bool", condition.span)),
            }
        }

        Stmt::Throw(_)
        | Stmt::Match { .. }
        | Stmt::Select { .. }
        | Stmt::With { .. }
        | Stmt::Unsafe { .. }
        | Stmt::Item(_) => Err(meta_err(
            "this statement type is not supported in compile-time function evaluation",
            stmt_span,
        )),

        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } | Stmt::MetaMatch { .. } | Stmt::MetaWhile { .. } => Err(meta_err(
            "`meta if`/`meta for`/`meta match`/`meta while` in function body requires generic type parameters \
             and is evaluated at monomorphization time, not in compile-time functions",
            stmt_span,
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
        (Type::Primitive(PrimitiveType::CStr), MetaValue::Str(_)) => true,
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
            if let Some(replacement) = type_env.get(&name.node) {
                if generic_args.is_empty() {
                    // Bare alias: replace the whole type
                    ty.node = replacement.clone();
                } else if let Type::Named { name: repl_name, .. } = replacement {
                    // Generic alias: substitute only the base name (keep existing generic args)
                    // e.g. `Map[str, int]` where `Map = Dict` → `Dict[str, int]`
                    *name = repl_name.clone();
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
        Item::Module { items, .. } => {
            for si in items {
                substitute_item(&mut si.node, env, type_env);
            }
        }
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
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            substitute_expr(condition, env, type_env);
            substitute_block(then_body, env, type_env);
            for (cond, body) in elif_branches {
                substitute_expr(cond, env, type_env);
                substitute_block(body, env, type_env);
            }
            if let Some(eb) = else_body { substitute_block(eb, env, type_env); }
        }
        Stmt::MetaFor { range, body, .. } => {
            substitute_expr(range, env, type_env);
            substitute_block(body, env, type_env);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            substitute_expr(scrutinee, env, type_env);
            // Case exprs are meta literals — substitute in them too (for consistency).
            for (case_expr, body) in arms {
                substitute_expr(case_expr, env, type_env);
                substitute_block(body, env, type_env);
            }
            if let Some(eb) = else_arm { substitute_block(eb, env, type_env); }
        }
        Stmt::MetaWhile { condition, body, .. } => {
            substitute_expr(condition, env, type_env);
            substitute_block(body, env, type_env);
        }
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
        // Dot-shorthand: recurse into args
        Expr::DotShorthand { args, .. } => {
            for arg in args.iter_mut() {
                substitute_expr(&mut arg.node.value, env, type_env);
            }
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

    // Also substitute meta type alias names used as constructors in expressions.
    // e.g.  Map[str, int]()  where  meta type Map = Dict  →  Dict[str, int]()
    // Only Named aliases can appear as expression-level identifiers (primitives can't).
    if let Expr::Identifier(name) = &mut expr.node {
        if let Some(Type::Named { name: repl_name, .. }) = type_env.get(name.as_str()) {
            *name = repl_name.node.clone();
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
        MetaValue::List(_) => {
            // Lists are not representable as a single AST expression;
            // they are only used internally by meta for iteration.
            panic!("meta List value cannot be substituted into AST expression position")
        }
    }
}

fn meta_value_to_string(value: &MetaValue) -> String {
    match value {
        MetaValue::Int(n) => format!("{n}"),
        MetaValue::Float(f) => format!("{f}"),
        MetaValue::Bool(b) => format!("{b}"),
        MetaValue::Str(s) => s.clone(),
        MetaValue::List(items) => {
            let parts: Vec<String> = items.iter().map(meta_value_to_string).collect();
            format!("[{}]", parts.join(", "))
        }
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
        MetaValue::List(_) => "list",
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
        Type::Primitive(PrimitiveType::CStr) => "cstr",
        Type::Primitive(PrimitiveType::StringType) => "String",
        Type::Primitive(PrimitiveType::Void) => "void",
        Type::Primitive(PrimitiveType::Char) => "char",
        _ => "<unknown>",
    }
}

// ═══════════════════════════════════════════════════════════════
// Delayed meta evaluation (Phase 1.4 — monomorphization time)
// ═══════════════════════════════════════════════════════════════

/// Context for evaluating `meta if`/`meta for` inside generic bodies.
/// Created once per monomorphized instantiation with the concrete type bindings.
pub struct DelayedMetaContext<'a> {
    /// Generic type parameter bindings, e.g. `[("T", Type::Primitive(Int))]`.
    pub type_subs: &'a [(String, Type)],
    /// Feature flags from CLI (`--feature`).
    pub features: &'a [String],
    /// Phase 0 meta constant values (already-evaluated at module level).
    pub meta_env: &'a FxHashMap<String, MetaValue>,
    /// Module AST items (for Phase 2 reflection builtins; empty slice for Phase 1).
    pub items: &'a [Spanned<Item>],
    /// Trait registry for `implements()` builtin.
    pub trait_registry: &'a crate::semantic::traits::TraitRegistry,
    /// Type registry for struct/enum reflection builtins.
    pub type_registry: &'a crate::ir::types::TypeRegistry,
}

/// Convert an AST `Type` to a canonical string representation used by `typename()`.
pub fn type_to_canonical_name(ty: &Type) -> String {
    match ty {
        Type::Primitive(p) => match p {
            PrimitiveType::Int => "int",
            PrimitiveType::Int8 => "int8",
            PrimitiveType::Int16 => "int16",
            PrimitiveType::Int32 => "int32",
            PrimitiveType::Int64 => "int64",
            PrimitiveType::Uint => "uint",
            PrimitiveType::Uint8 => "uint8",
            PrimitiveType::Uint16 => "uint16",
            PrimitiveType::Uint32 => "uint32",
            PrimitiveType::Uint64 => "uint64",
            PrimitiveType::Float => "float",
            PrimitiveType::Float32 => "float32",
            PrimitiveType::Float64 => "float64",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Str => "str",
            PrimitiveType::CStr => "cstr",
            PrimitiveType::StringType => "String",
            PrimitiveType::Void => "void",
            PrimitiveType::Char => "char",
        }.to_string(),
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                name.node.clone()
            } else {
                let args: Vec<String> = generic_args.iter()
                    .map(|a| type_to_canonical_name(&a.node))
                    .collect();
                format!("{}[{}]", name.node, args.join(", "))
            }
        }
        Type::Tuple(elems) => {
            let parts: Vec<String> = elems.iter()
                .map(|e| type_to_canonical_name(&e.node))
                .collect();
            format!("({})", parts.join(", "))
        }
        _ => "<unknown>".to_string(),
    }
}

/// Compare two `MetaValue`s for equality (used by `meta match`).
fn meta_values_eq(a: &MetaValue, b: &MetaValue) -> bool {
    match (a, b) {
        (MetaValue::Int(x),   MetaValue::Int(y))   => x == y,
        (MetaValue::Bool(x),  MetaValue::Bool(y))  => x == y,
        (MetaValue::Str(x),   MetaValue::Str(y))   => x == y,
        (MetaValue::Float(x), MetaValue::Float(y)) => x == y,
        (MetaValue::List(a), MetaValue::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| meta_values_eq(x, y))
        }
        _ => false,
    }
}

/// Evaluate a meta expression in delayed context (monomorphization time).
/// Like `eval_expr` but additionally resolves `typename(T)` via type parameter bindings.
///
/// Handles compound expressions (BinaryOp, UnaryOp) by recursing so that
/// `typename(T) == "int"` correctly resolves `T` even when nested inside a larger
/// expression tree.
fn eval_delayed_expr(
    expr: &Expr,
    ctx: &DelayedMetaContext<'_>,
    span: Span,
) -> Result<MetaValue, SemanticError> {
    match expr {
        // Recurse through compound expressions so inner typename(T)/sizeof(T) are resolved.
        Expr::BinaryOp { left, op, right } => {
            let lhs = eval_delayed_expr(&left.node, ctx, left.span)?;
            let rhs = eval_delayed_expr(&right.node, ctx, right.span)?;
            eval_binary_op(&lhs, *op, &rhs, span)
        }
        Expr::UnaryOp { op, operand } => {
            let val = eval_delayed_expr(&operand.node, ctx, operand.span)?;
            match (op, &val) {
                (UnaryOp::Neg, MetaValue::Int(n)) => Ok(MetaValue::Int(-n)),
                (UnaryOp::Neg, MetaValue::Float(f)) => Ok(MetaValue::Float(-f)),
                (UnaryOp::Not, MetaValue::Bool(b)) => Ok(MetaValue::Bool(!b)),
                (UnaryOp::BitNot, MetaValue::Int(n)) => Ok(MetaValue::Int(!n)),
                _ => Err(meta_err(
                    &format!("unsupported unary operator in delayed meta expression"),
                    span,
                )),
            }
        }

        // Built-in calls that need type param resolution
        Expr::Call { callee, args, .. } => {
            if let Expr::Identifier(name) = &callee.node {
                match name.as_str() {
                    "typename" => {
                        if args.len() != 1 {
                            return Err(meta_err("typename() takes exactly 1 argument", span));
                        }
                        let type_name_str = meta_expr_to_type_name(&args[0].node.value.node);
                        // Check if it's a generic type param — resolve to concrete type name
                        for (param, concrete_ty) in ctx.type_subs {
                            if *param == type_name_str {
                                return Ok(MetaValue::Str(type_to_canonical_name(concrete_ty)));
                            }
                        }
                        // Not a type param: return as-is (e.g. typename(int) → "int")
                        return Ok(MetaValue::Str(type_name_str));
                    }
                    "sizeof" => {
                        if args.len() != 1 {
                            return Err(meta_err("sizeof() takes exactly 1 argument", span));
                        }
                        let type_name_str = meta_expr_to_type_name(&args[0].node.value.node);
                        // Resolve generic param to concrete type first
                        let resolved_name = ctx.type_subs.iter()
                            .find(|(p, _)| *p == type_name_str)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(type_name_str);
                        // Use existing primitive size table
                        match meta_type_byte_size(&resolved_name) {
                            Some(size) => return Ok(MetaValue::Int(size)),
                            None => return Err(meta_err(
                                &format!(
                                    "sizeof({resolved_name}): size unknown — \
                                     only primitive types are supported in Phase 1"
                                ),
                                span,
                            )),
                        }
                    }
                    "typeof" => {
                        if args.len() != 1 {
                            return Err(meta_err("typeof() takes exactly 1 argument", span));
                        }
                        let type_name_str = meta_expr_to_type_name(&args[0].node.value.node);
                        for (param, concrete_ty) in ctx.type_subs {
                            if *param == type_name_str {
                                return Ok(MetaValue::Str(type_to_canonical_name(concrete_ty)));
                            }
                        }
                        return Ok(MetaValue::Str(type_name_str));
                    }
                    "bitwidth" => {
                        if args.len() != 1 {
                            return Err(meta_err("bitwidth() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let bits: i64 = match type_name.as_str() {
                            "int8" | "uint8" | "bool"               => 8,
                            "int16" | "uint16"                       => 16,
                            "int32" | "uint32" | "float32"           => 32,
                            "int" | "int64" | "uint" | "uint64" |
                            "float" | "float64"                      => 64,
                            other => return Err(meta_err(
                                &format!("bitwidth({other}): unknown or non-primitive type"), span)),
                        };
                        return Ok(MetaValue::Int(bits));
                    }
                    "min_val" => {
                        if args.len() != 1 {
                            return Err(meta_err("min_val() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let val: i64 = match type_name.as_str() {
                            "int8"   => i8::MIN as i64,
                            "int16"  => i16::MIN as i64,
                            "int32"  => i32::MIN as i64,
                            "int" | "int64" => i64::MIN,
                            "uint8" | "uint16" | "uint32" | "uint64" | "uint" => 0,
                            other => return Err(meta_err(
                                &format!("min_val({other}): requires an integer type"), span)),
                        };
                        return Ok(MetaValue::Int(val));
                    }
                    "max_val" => {
                        if args.len() != 1 {
                            return Err(meta_err("max_val() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let val: i64 = match type_name.as_str() {
                            "int8"   => i8::MAX as i64,
                            "int16"  => i16::MAX as i64,
                            "int32"  => i32::MAX as i64,
                            "int" | "int64" => i64::MAX,
                            "uint8"  => u8::MAX as i64,
                            "uint16" => u16::MAX as i64,
                            "uint32" => u32::MAX as i64,
                            "uint" | "uint64" => i64::MAX,   // saturated at i64::MAX
                            other => return Err(meta_err(
                                &format!("max_val({other}): requires an integer type"), span)),
                        };
                        return Ok(MetaValue::Int(val));
                    }
                    "implements" => {
                        if args.len() != 2 {
                            return Err(meta_err("implements() takes exactly 2 arguments", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        // arg1 must be a string literal (the trait name)
                        let trait_name = match eval_delayed_expr(&args[1].node.value.node, ctx, span)? {
                            MetaValue::Str(s) => s,
                            _ => return Err(meta_err("implements(): second argument must be a trait name string", span)),
                        };
                        let found = ctx.trait_registry.has_trait_impl_by_name(&type_name, &trait_name);
                        return Ok(MetaValue::Bool(found));
                    }
                    "field_names" => {
                        if args.len() != 1 {
                            return Err(meta_err("field_names() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Struct(s) = &type_def.kind {
                                    let names = s.fields.iter()
                                        .map(|f| MetaValue::Str(f.name.clone()))
                                        .collect();
                                    return Ok(MetaValue::List(names));
                                }
                                return Err(meta_err(&format!("field_names: `{type_name}` is not a struct"), span));
                            }
                            None => return Err(meta_err(
                                &format!("field_names: unknown type `{type_name}` (struct must be used before reflection)"),
                                span)),
                        }
                    }
                    "field_count" => {
                        if args.len() != 1 {
                            return Err(meta_err("field_count() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Struct(s) = &type_def.kind {
                                    return Ok(MetaValue::Int(s.fields.len() as i64));
                                }
                                return Err(meta_err(&format!("field_count: `{type_name}` is not a struct"), span));
                            }
                            None => return Err(meta_err(
                                &format!("field_count: unknown type `{type_name}`"), span)),
                        }
                    }
                    "has_field" => {
                        if args.len() != 2 {
                            return Err(meta_err("has_field() takes exactly 2 arguments", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let field_name = match eval_delayed_expr(&args[1].node.value.node, ctx, span)? {
                            MetaValue::Str(s) => s,
                            _ => return Err(meta_err("has_field(): second argument must be a field name string", span)),
                        };
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Struct(s) = &type_def.kind {
                                    let found = s.fields.iter().any(|f| f.name == field_name);
                                    return Ok(MetaValue::Bool(found));
                                }
                                return Ok(MetaValue::Bool(false));
                            }
                            None => return Ok(MetaValue::Bool(false)),
                        }
                    }
                    "field_type" => {
                        if args.len() != 2 {
                            return Err(meta_err("field_type() takes exactly 2 arguments", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let field_name = match eval_delayed_expr(&args[1].node.value.node, ctx, span)? {
                            MetaValue::Str(s) => s,
                            _ => return Err(meta_err("field_type(): second argument must be a field name string", span)),
                        };
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Struct(s) = &type_def.kind {
                                    if let Some(field) = s.fields.iter().find(|f| f.name == field_name) {
                                        let ft = ctx.type_registry.type_name(field.type_id)
                                            .unwrap_or_else(|| "unknown".to_string());
                                        return Ok(MetaValue::Str(ft));
                                    }
                                    return Err(meta_err(
                                        &format!("field_type: `{type_name}` has no field `{field_name}`"), span));
                                }
                                return Err(meta_err(&format!("field_type: `{type_name}` is not a struct"), span));
                            }
                            None => return Err(meta_err(
                                &format!("field_type: unknown type `{type_name}`"), span)),
                        }
                    }
                    "variant_names" => {
                        if args.len() != 1 {
                            return Err(meta_err("variant_names() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Enum(e) = &type_def.kind {
                                    let names = e.variants.iter()
                                        .map(|v| MetaValue::Str(v.name.clone()))
                                        .collect();
                                    return Ok(MetaValue::List(names));
                                }
                                return Err(meta_err(&format!("variant_names: `{type_name}` is not an enum"), span));
                            }
                            None => return Err(meta_err(
                                &format!("variant_names: unknown type `{type_name}`"), span)),
                        }
                    }
                    "variant_count" => {
                        if args.len() != 1 {
                            return Err(meta_err("variant_count() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Enum(e) = &type_def.kind {
                                    return Ok(MetaValue::Int(e.variants.len() as i64));
                                }
                                return Err(meta_err(&format!("variant_count: `{type_name}` is not an enum"), span));
                            }
                            None => return Err(meta_err(
                                &format!("variant_count: unknown type `{type_name}`"), span)),
                        }
                    }
                    _ => {}
                }
            }
            // Non-typename/sizeof calls: delegate to Phase 0 evaluator
            let meta_ctx = MetaContext::new(ctx.features, ctx.items);
            eval_expr(expr, ctx.meta_env, &meta_ctx, span)
        }

        // All other leaf/terminal expressions: delegate to Phase 0 evaluator.
        // This handles literals, identifiers (meta constants), etc.
        _ => {
            let meta_ctx = MetaContext::new(ctx.features, ctx.items);
            eval_expr(expr, ctx.meta_env, &meta_ctx, span)
        }
    }
}

/// Evaluate and splice out all `Stmt::MetaIf`/`Stmt::MetaFor` nodes in a block.
/// Called at monomorphization time, after type substitution, before GIR lowering.
///
/// Modifies `block.stmts` in place: each `MetaIf`/`MetaFor` is replaced by the
/// statements from the winning branch (or the unrolled loop body).  Recurses into
/// nested blocks (inside regular `if`, `while`, `for`, etc.) so that nested delayed
/// meta constructs are also eliminated.
pub fn evaluate_delayed_meta_block(
    block: &mut Block,
    ctx: &DelayedMetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) {
    let mut i = 0;
    while i < block.stmts.len() {
        let replacement = match &block.stmts[i].node {
            Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
                let cond_span = condition.span;
                match eval_delayed_expr(&condition.node, ctx, cond_span) {
                    Ok(MetaValue::Bool(true)) => {
                        let mut body = then_body.clone();
                        evaluate_delayed_meta_block(&mut body, ctx, errors);
                        Some(body.stmts)
                    }
                    Ok(MetaValue::Bool(false)) => {
                        // Try elif branches
                        let mut taken: Option<Vec<Spanned<Stmt>>> = None;
                        for (elif_cond, elif_body) in elif_branches.iter() {
                            match eval_delayed_expr(&elif_cond.node, ctx, elif_cond.span) {
                                Ok(MetaValue::Bool(true)) => {
                                    let mut body = elif_body.clone();
                                    evaluate_delayed_meta_block(&mut body, ctx, errors);
                                    taken = Some(body.stmts);
                                    break;
                                }
                                Ok(MetaValue::Bool(false)) => {}
                                Ok(_) => {
                                    errors.push(meta_err(
                                        "meta elif condition must evaluate to bool",
                                        elif_cond.span,
                                    ));
                                    break;
                                }
                                Err(e) => {
                                    errors.push(e);
                                    break;
                                }
                            }
                        }
                        if let Some(stmts) = taken {
                            Some(stmts)
                        } else if let Some(else_body) = else_body {
                            let mut body = else_body.clone();
                            evaluate_delayed_meta_block(&mut body, ctx, errors);
                            Some(body.stmts)
                        } else {
                            Some(vec![]) // No branch taken — emit nothing
                        }
                    }
                    Ok(_) => {
                        errors.push(meta_err(
                            "meta if condition must evaluate to bool",
                            cond_span,
                        ));
                        Some(vec![])
                    }
                    Err(e) => {
                        errors.push(e);
                        Some(vec![])
                    }
                }
            }

            Stmt::MetaFor { var_name, range, body, .. } => {
                let range_span = range.span;
                // Evaluate range bounds
                match eval_delayed_meta_range(&range.node, ctx, range_span) {
                    Ok((start_val, end_val, inclusive)) => {
                        let upper = if inclusive { end_val + 1 } else { end_val };
                        let loop_var = var_name.node.clone();
                        let mut result_stmts: Vec<Spanned<Stmt>> = Vec::new();
                        for val in start_val..upper {
                            // Build a child context with loop var added to meta_env
                            let mut child_env = (*ctx.meta_env).clone();
                            child_env.insert(loop_var.clone(), MetaValue::Int(val));
                            let child_ctx = DelayedMetaContext { meta_env: &child_env, ..*ctx };
                            let mut loop_body = body.clone();
                            evaluate_delayed_meta_block(&mut loop_body, &child_ctx, errors);
                            result_stmts.extend(loop_body.stmts);
                        }
                        Some(result_stmts)
                    }
                    Err(_range_err) => {
                        // Not an integer range — try evaluating as a list expression
                        match eval_delayed_expr(&range.node, ctx, range.span) {
                            Ok(MetaValue::List(items)) => {
                                let mut result_stmts: Vec<Spanned<Stmt>> = Vec::new();
                                for item_val in items {
                                    let mut child_env = (*ctx.meta_env).clone();
                                    child_env.insert(var_name.node.clone(), item_val);
                                    let child_ctx = DelayedMetaContext { meta_env: &child_env, ..*ctx };
                                    let mut loop_body = body.clone();
                                    evaluate_delayed_meta_block(&mut loop_body, &child_ctx, errors);
                                    result_stmts.extend(loop_body.stmts);
                                }
                                Some(result_stmts)
                            }
                            Ok(_) => {
                                errors.push(meta_err(
                                    "meta for: range must be an integer range (x..y) or a list (e.g. field_names(T))",
                                    range.span,
                                ));
                                Some(vec![])
                            }
                            Err(e) => { errors.push(e); Some(vec![]) }
                        }
                    }
                }
            }

            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                let subject = eval_delayed_expr(&scrutinee.node, ctx, scrutinee.span);
                match subject {
                    Ok(subject_val) => {
                        let mut taken = None;
                        for (case_expr, case_body) in arms.iter() {
                            match eval_delayed_expr(&case_expr.node, ctx, case_expr.span) {
                                Ok(case_val) if meta_values_eq(&subject_val, &case_val) => {
                                    let mut body = case_body.clone();
                                    evaluate_delayed_meta_block(&mut body, ctx, errors);
                                    taken = Some(body.stmts);
                                    break;
                                }
                                Ok(_) => {}
                                Err(e) => {
                                    errors.push(e);
                                    break;
                                }
                            }
                        }
                        if let Some(stmts) = taken {
                            Some(stmts)
                        } else if let Some(else_body) = else_arm {
                            let mut body = else_body.clone();
                            evaluate_delayed_meta_block(&mut body, ctx, errors);
                            Some(body.stmts)
                        } else {
                            Some(vec![])
                        }
                    }
                    Err(e) => {
                        errors.push(e);
                        Some(vec![])
                    }
                }
            }

            Stmt::MetaWhile { condition, body, span } => {
                const MAX_META_ITERATIONS: usize = 100_000;
                let mut result_stmts = Vec::new();
                let mut iter_count = 0usize;
                loop {
                    match eval_delayed_expr(&condition.node, ctx, condition.span) {
                        Ok(MetaValue::Bool(true)) => {
                            if iter_count >= MAX_META_ITERATIONS {
                                errors.push(meta_err(
                                    "meta while exceeded iteration limit (100000)",
                                    *span,
                                ));
                                break;
                            }
                            let mut loop_body = body.clone();
                            evaluate_delayed_meta_block(&mut loop_body, ctx, errors);
                            result_stmts.extend(loop_body.stmts);
                            iter_count += 1;
                        }
                        Ok(MetaValue::Bool(false)) => break,
                        Ok(_) => {
                            errors.push(meta_err(
                                "meta while condition must evaluate to bool",
                                condition.span,
                            ));
                            break;
                        }
                        Err(e) => {
                            errors.push(e);
                            break;
                        }
                    }
                }
                Some(result_stmts)
            }

            _ => None, // Not a meta stmt — recurse into sub-blocks below
        };

        if let Some(replacement_stmts) = replacement {
            // Replace the MetaIf/MetaFor with its expanded stmts
            let n = replacement_stmts.len();
            block.stmts.splice(i..i + 1, replacement_stmts);
            // Process newly inserted stmts (they may contain nested MetaIf/For)
            // by NOT advancing i — the loop will encounter them next
            let _ = n; // already processed recursively
        } else {
            // Recurse into nested blocks for non-meta stmts
            recurse_delayed_meta_in_stmt(&mut block.stmts[i].node, ctx, errors);
            i += 1;
        }
    }
}

/// Evaluate an integer range expression in delayed meta context.
/// Returns (start, end, inclusive).
fn eval_delayed_meta_range(
    range_expr: &Expr,
    ctx: &DelayedMetaContext<'_>,
    span: Span,
) -> Result<(i64, i64, bool), SemanticError> {
    match range_expr {
        Expr::Range { start, end, inclusive } => {
            let start_val = match start {
                Some(s) => match eval_delayed_expr(&s.node, ctx, s.span)? {
                    MetaValue::Int(n) => n,
                    _ => return Err(meta_err("meta for range start must be an integer", s.span)),
                },
                None => 0,
            };
            let end_val = match end {
                Some(e) => match eval_delayed_expr(&e.node, ctx, e.span)? {
                    MetaValue::Int(n) => n,
                    _ => return Err(meta_err("meta for range end must be an integer", e.span)),
                },
                None => return Err(meta_err(
                    "open-ended range not supported in meta for",
                    span,
                )),
            };
            Ok((start_val, end_val, *inclusive))
        }
        _ => Err(meta_err(
            "meta for requires a range expression (e.g. `0..n`)",
            span,
        )),
    }
}

/// Recurse the delayed meta evaluator into sub-blocks of a non-meta statement.
fn recurse_delayed_meta_in_stmt(
    stmt: &mut Stmt,
    ctx: &DelayedMetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) {
    match stmt {
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            evaluate_delayed_meta_block(then_body, ctx, errors);
            for (_, body) in elif_branches {
                evaluate_delayed_meta_block(body, ctx, errors);
            }
            if let Some(eb) = else_body {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        Stmt::While { body, else_body, .. } => {
            evaluate_delayed_meta_block(body, ctx, errors);
            if let Some(eb) = else_body {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        Stmt::For { body, else_body, .. } => {
            evaluate_delayed_meta_block(body, ctx, errors);
            if let Some(eb) = else_body {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        Stmt::Loop { body } | Stmt::Unsafe { body } => {
            evaluate_delayed_meta_block(body, ctx, errors);
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms {
                if let Expr::Block(block) = &mut arm.body.node {
                    evaluate_delayed_meta_block(block, ctx, errors);
                }
            }
            if let Some(eb) = else_arm {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        Stmt::With { body, .. } => {
            evaluate_delayed_meta_block(body, ctx, errors);
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                evaluate_delayed_meta_block(&mut arm.body, ctx, errors);
            }
            if let Some(eb) = else_arm {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        // MetaMatch: recurse into all case bodies + else arm
        // (needed when a meta match is nested inside a regular if/while/etc.)
        Stmt::MetaMatch { arms, else_arm, .. } => {
            for (_, body) in arms {
                evaluate_delayed_meta_block(body, ctx, errors);
            }
            if let Some(eb) = else_arm {
                evaluate_delayed_meta_block(eb, ctx, errors);
            }
        }
        // MetaWhile: recurse into body
        // (needed when a meta while is nested inside a regular if/while/etc.)
        Stmt::MetaWhile { condition: _, body, .. } => {
            evaluate_delayed_meta_block(body, ctx, errors);
        }
        // Leaf stmts — no sub-blocks
        _ => {}
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

    fn no_ctx() -> MetaContext<'static> {
        MetaContext::empty()
    }

    // ── Literal evaluation ──

    #[test]
    fn eval_int_literal() {
        let result = eval_expr(&Expr::IntLiteral(42), &empty_env(), &no_ctx(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(42))));
    }

    #[test]
    fn eval_float_literal() {
        let result = eval_expr(&Expr::FloatLiteral(3.14), &empty_env(), &no_ctx(), dummy_span());
        match result {
            Ok(MetaValue::Float(f)) => assert!((f - 3.14).abs() < f64::EPSILON),
            other => panic!("expected Float, got: {other:?}"),
        }
    }

    #[test]
    fn eval_bool_literal() {
        let result = eval_expr(&Expr::BoolLiteral(true), &empty_env(), &no_ctx(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Bool(true))));
    }

    #[test]
    fn eval_string_literal() {
        let s = Expr::StringLiteral(StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Literal("hello".to_string())],
        });
        let result = eval_expr(&s, &empty_env(), &no_ctx(), dummy_span());
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
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
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
        let result = eval_expr(&expr, &env, &no_ctx(), dummy_span());
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
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
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
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
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
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Int(-5))));
    }

    #[test]
    fn eval_unary_not() {
        let expr = Expr::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(Spanned::new(Expr::BoolLiteral(true), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
        assert!(matches!(result, Ok(MetaValue::Bool(false))));
    }

    #[test]
    fn eval_unary_bitnot() {
        let expr = Expr::UnaryOp {
            op: UnaryOp::BitNot,
            operand: Box::new(Spanned::new(Expr::IntLiteral(0xFF), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
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
        // Calling an unknown function name in meta context should error
        let expr = Expr::Call {
            callee: Box::new(Spanned::new(Expr::Identifier("unknown_fn".to_string()), dummy_span())),
            generic_args: None,
            args: vec![],
        };
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("unknown meta function"));
    }

    #[test]
    fn eval_division_by_zero() {
        let expr = Expr::BinaryOp {
            left: Box::new(Spanned::new(Expr::IntLiteral(10), dummy_span())),
            op: BinaryOp::Div,
            right: Box::new(Spanned::new(Expr::IntLiteral(0), dummy_span())),
        };
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
        assert!(result.is_err());
        assert!(format!("{}", result.unwrap_err()).contains("division by zero"));
    }

    #[test]
    fn eval_undefined_meta_const() {
        let expr = Expr::Identifier("UNKNOWN".to_string());
        let result = eval_expr(&expr, &empty_env(), &no_ctx(), dummy_span());
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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
                        rhs: MetaTypeRhs::Plain(Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span())),
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

        let errors = evaluate_meta_consts(&mut module, &[]);
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

    // ── M5: Conditional types ──

    #[test]
    fn meta_conditional_type_true() {
        // meta type Map = Dict if true else HashMap  →  Dict
        let mut module = Module {
            items: vec![Spanned::new(
                Item::MetaType(MetaType {
                    name: Spanned::new("Map".to_string(), dummy_span()),
                    rhs: MetaTypeRhs::Conditional {
                        then_type: Spanned::new(
                            Type::Named { name: Spanned::new("Dict".to_string(), dummy_span()), generic_args: vec![] },
                            dummy_span(),
                        ),
                        condition: Spanned::new(Expr::BoolLiteral(true), dummy_span()),
                        else_type: Spanned::new(
                            Type::Named { name: Spanned::new("HashMap".to_string(), dummy_span()), generic_args: vec![] },
                            dummy_span(),
                        ),
                    },
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };
        let errors = evaluate_meta_consts(&mut module, &[]);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        // MetaType removed
        assert_eq!(module.items.len(), 0);
    }

    #[test]
    fn meta_conditional_type_false() {
        // meta type Map = Dict if false else HashMap  →  HashMap
        // Verify via substitution: declare a var of type Map, check it becomes HashMap
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaType(MetaType {
                        name: Spanned::new("Map".to_string(), dummy_span()),
                        rhs: MetaTypeRhs::Conditional {
                            then_type: Spanned::new(
                                Type::Named { name: Spanned::new("Dict".to_string(), dummy_span()), generic_args: vec![] },
                                dummy_span(),
                            ),
                            condition: Spanned::new(Expr::BoolLiteral(false), dummy_span()),
                            else_type: Spanned::new(
                                Type::Named { name: Spanned::new("HashMap".to_string(), dummy_span()), generic_args: vec![] },
                                dummy_span(),
                            ),
                        },
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
                                        Type::Named { name: Spanned::new("Map".to_string(), dummy_span()), generic_args: vec![] },
                                        dummy_span(),
                                    ),
                                    pattern: Spanned::new(Pattern::Binding("x".to_string()), dummy_span()),
                                    value: Spanned::new(Expr::IntLiteral(0), dummy_span()),
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
        let errors = evaluate_meta_consts(&mut module, &[]);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::VarDecl { type_, .. } = &block.stmts[0].node {
                    assert!(
                        matches!(&type_.node, Type::Named { name, .. } if name.node == "HashMap"),
                        "expected HashMap, got: {:?}", type_.node,
                    );
                    return;
                }
            }
        }
        panic!("test structure unexpected");
    }

    #[test]
    fn meta_conditional_type_with_meta_const() {
        // meta bool ORDERED = true
        // meta type Map = Dict if ORDERED else HashMap  →  Dict
        let mut module = Module {
            items: vec![
                Spanned::new(
                    Item::MetaConst(MetaConst {
                        type_: Spanned::new(Type::Primitive(PrimitiveType::Bool), dummy_span()),
                        name: Spanned::new("ORDERED".to_string(), dummy_span()),
                        value: Spanned::new(Expr::BoolLiteral(true), dummy_span()),
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
                Spanned::new(
                    Item::MetaType(MetaType {
                        name: Spanned::new("Map".to_string(), dummy_span()),
                        rhs: MetaTypeRhs::Conditional {
                            then_type: Spanned::new(
                                Type::Named { name: Spanned::new("Dict".to_string(), dummy_span()), generic_args: vec![] },
                                dummy_span(),
                            ),
                            condition: Spanned::new(Expr::Identifier("ORDERED".to_string()), dummy_span()),
                            else_type: Spanned::new(
                                Type::Named { name: Spanned::new("HashMap".to_string(), dummy_span()), generic_args: vec![] },
                                dummy_span(),
                            ),
                        },
                        span: dummy_span(),
                    }),
                    dummy_span(),
                ),
            ],
            span: dummy_span(),
        };
        let errors = evaluate_meta_consts(&mut module, &[]);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert_eq!(module.items.len(), 0);
    }

    #[test]
    fn meta_conditional_type_non_bool_error() {
        let mut module = Module {
            items: vec![Spanned::new(
                Item::MetaType(MetaType {
                    name: Spanned::new("X".to_string(), dummy_span()),
                    rhs: MetaTypeRhs::Conditional {
                        then_type: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                        condition: Spanned::new(Expr::IntLiteral(1), dummy_span()),
                        else_type: Spanned::new(Type::Primitive(PrimitiveType::Float), dummy_span()),
                    },
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };
        let errors = evaluate_meta_consts(&mut module, &[]);
        assert_eq!(errors.len(), 1);
        assert!(format!("{}", errors[0]).contains("bool"));
    }

    // ── M6: Type functions ──

    #[test]
    fn meta_type_func_basic() {
        // meta type sized_int(int bits): if bits <= 8: return int8 else: return int64
        // meta type Small = sized_int(8)  →  int8
        let func_item = Item::MetaTypeFunc(MetaTypeFunc {
            name: Spanned::new("sized_int".to_string(), dummy_span()),
            params: vec![Spanned::new(
                Param {
                    type_: Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                    name: Spanned::new("bits".to_string(), dummy_span()),
                    default: None,
                    ownership: Ownership::Borrow,
                    is_live: false,
                    live_group: None,
                },
                dummy_span(),
            )],
            body: Block {
                stmts: vec![Spanned::new(
                    Stmt::If {
                        condition: Spanned::new(
                            Expr::BinaryOp {
                                left: Box::new(Spanned::new(Expr::Identifier("bits".to_string()), dummy_span())),
                                op: BinaryOp::LtEq,
                                right: Box::new(Spanned::new(Expr::IntLiteral(8), dummy_span())),
                            },
                            dummy_span(),
                        ),
                        then_body: Block {
                            stmts: vec![Spanned::new(
                                Stmt::Return(Some(Spanned::new(Expr::Identifier("int8".to_string()), dummy_span()))),
                                dummy_span(),
                            )],
                            span: dummy_span(),
                        },
                        elif_branches: vec![],
                        else_body: Some(Block {
                            stmts: vec![Spanned::new(
                                Stmt::Return(Some(Spanned::new(Expr::Identifier("int64".to_string()), dummy_span()))),
                                dummy_span(),
                            )],
                            span: dummy_span(),
                        }),
                    },
                    dummy_span(),
                )],
                span: dummy_span(),
            },
            span: dummy_span(),
        });

        let call_item = Item::MetaType(MetaType {
            name: Spanned::new("Small".to_string(), dummy_span()),
            rhs: MetaTypeRhs::Call {
                callee: Spanned::new("sized_int".to_string(), dummy_span()),
                args: vec![Spanned::new(Expr::IntLiteral(8), dummy_span())],
            },
            span: dummy_span(),
        });

        let mut module = Module {
            items: vec![
                Spanned::new(func_item, dummy_span()),
                Spanned::new(call_item, dummy_span()),
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
                                        Type::Named { name: Spanned::new("Small".to_string(), dummy_span()), generic_args: vec![] },
                                        dummy_span(),
                                    ),
                                    pattern: Spanned::new(Pattern::Binding("x".to_string()), dummy_span()),
                                    value: Spanned::new(Expr::IntLiteral(7), dummy_span()),
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

        let errors = evaluate_meta_consts(&mut module, &[]);
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
        assert_eq!(module.items.len(), 1);
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::VarDecl { type_, .. } = &block.stmts[0].node {
                    assert!(
                        matches!(&type_.node, Type::Primitive(PrimitiveType::Int8)),
                        "expected Primitive(Int8), got: {:?}", type_.node,
                    );
                    return;
                }
            }
        }
        panic!("test structure unexpected");
    }

    #[test]
    fn meta_type_func_unknown_error() {
        let mut module = Module {
            items: vec![Spanned::new(
                Item::MetaType(MetaType {
                    name: Spanned::new("X".to_string(), dummy_span()),
                    rhs: MetaTypeRhs::Call {
                        callee: Spanned::new("no_such_fn".to_string(), dummy_span()),
                        args: vec![],
                    },
                    span: dummy_span(),
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };
        let errors = evaluate_meta_consts(&mut module, &[]);
        assert_eq!(errors.len(), 1);
        assert!(format!("{}", errors[0]).contains("no_such_fn"), "error: {}", errors[0]);
    }
}
