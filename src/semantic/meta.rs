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
    /// Compile-time operator token — carried by `meta op` parameters.
    Op(crate::parser::ast::BinaryOp),
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
    /// Directory containing the source file — used to resolve relative paths in embed_file().
    /// `None` when no source path is available; falls back to the process working directory.
    source_dir: Option<std::path::PathBuf>,
}

impl<'a> MetaContext<'a> {
    fn new(features: &'a [String], items: &'a [Spanned<Item>]) -> Self {
        Self { features, items, call_depth: std::cell::Cell::new(0), source_dir: None }
    }

    fn with_source_dir(features: &'a [String], items: &'a [Spanned<Item>], source_dir: Option<std::path::PathBuf>) -> Self {
        Self { features, items, call_depth: std::cell::Cell::new(0), source_dir }
    }

    #[allow(dead_code)]
    fn empty() -> MetaContext<'static> {
        MetaContext { features: &[], items: &[], call_depth: std::cell::Cell::new(0), source_dir: None }
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

/// Expand `type` alias declarations in the module AST.
///
/// Collects all `Item::TypeAlias` definitions, then uses the existing meta substitution
/// infrastructure to rewrite every type annotation and constructor expression. Handles
/// simple aliases (`type Count = int`) and pass-through aliases (`type Pair = Vector`
/// where `Pair[int]` → `Vector[int]`). Generic aliases with type params
/// (`type StringMap[V] = Dict[str, V]`) are expanded via a dedicated type-level rewrite.
///
/// Must run **after** meta evaluation and derive expansion, but **before** name resolution.
/// Fix up constructor calls where an alias maps to a generic type.
/// E.g., `IntList()` where `type IntList = Vector[int]` → `Vector[int]()`.
/// Uses the *original* alias name (before substitution) to identify calls to rewrite.
fn fixup_constructor_calls_in_item(
    item: &mut Item,
    fixups: &FxHashMap<String, (String, Vec<Spanned<Type>>)>,
) {
    match item {
        Item::Function(f) => {
            if let FunctionBody::Block(block) = &mut f.body {
                fixup_calls_in_block(&mut block.stmts, fixups);
            }
        }
        Item::Equip(eq) => {
            for method in &mut eq.items {
                if let FunctionBody::Block(block) = &mut method.node.body {
                    fixup_calls_in_block(&mut block.stmts, fixups);
                }
            }
        }
        Item::Test(t) => fixup_calls_in_block(&mut t.body.stmts, fixups),
        Item::Bench(b) => fixup_calls_in_block(&mut b.body.stmts, fixups),
        Item::Module { items, .. } => {
            for si in items {
                fixup_constructor_calls_in_item(&mut si.node, fixups);
            }
        }
        _ => {}
    }
}

fn fixup_calls_in_block(
    stmts: &mut [Spanned<Stmt>],
    fixups: &FxHashMap<String, (String, Vec<Spanned<Type>>)>,
) {
    for stmt in stmts {
        match &mut stmt.node {
            Stmt::VarDecl { value, .. } => fixup_calls_in_expr(value, fixups),
            Stmt::Expr(e) => fixup_calls_in_expr(e, fixups),
            Stmt::Assign { value, .. } => fixup_calls_in_expr(value, fixups),
            Stmt::CompoundAssign { value, .. } => fixup_calls_in_expr(value, fixups),
            Stmt::For { iterable, body, else_body, .. } => {
                fixup_calls_in_expr(iterable, fixups);
                fixup_calls_in_block(&mut body.stmts, fixups);
                if let Some(eb) = else_body {
                    fixup_calls_in_block(&mut eb.stmts, fixups);
                }
            }
            Stmt::While { condition, body, .. } => {
                fixup_calls_in_expr(condition, fixups);
                fixup_calls_in_block(&mut body.stmts, fixups);
            }
            Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
                fixup_calls_in_expr(condition, fixups);
                fixup_calls_in_block(&mut then_body.stmts, fixups);
                for (cond, body) in elif_branches {
                    fixup_calls_in_expr(cond, fixups);
                    fixup_calls_in_block(&mut body.stmts, fixups);
                }
                if let Some(eb) = else_body {
                    fixup_calls_in_block(&mut eb.stmts, fixups);
                }
            }
            Stmt::Return(Some(e)) => fixup_calls_in_expr(e, fixups),
            Stmt::Match { scrutinee, arms, else_arm, .. } => {
                fixup_calls_in_expr(scrutinee, fixups);
                for arm in arms {
                    if let MatchItem::Arm(a) = arm {
                        fixup_calls_in_expr(&mut a.body, fixups);
                    }
                }
                if let Some(ea) = else_arm {
                    fixup_calls_in_block(&mut ea.stmts, fixups);
                }
            }
            _ => {}
        }
    }
}

fn fixup_calls_in_expr(
    expr: &mut Spanned<Expr>,
    fixups: &FxHashMap<String, (String, Vec<Spanned<Type>>)>,
) {
    match &mut expr.node {
        Expr::Call { callee, generic_args, args } => {
            // If callee is an alias name and no generic args yet, inject them
            if let Expr::Identifier(name) = &callee.node {
                if generic_args.is_none() {
                    if let Some((real_name, gen_args)) = fixups.get(name.as_str()) {
                        callee.node = Expr::Identifier(real_name.clone());
                        // Non-generic struct alias → leave args `None` so the
                        // rewritten call is byte-identical to a direct
                        // constructor call (`SlotKey(7, 0)`); only inject
                        // `Some(..)` for generic aliases that carry args.
                        *generic_args = if gen_args.is_empty() {
                            None
                        } else {
                            Some(gen_args.clone())
                        };
                    }
                }
            }
            fixup_calls_in_expr(callee, fixups);
            for arg in args {
                fixup_calls_in_expr(&mut arg.node.value, fixups);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            fixup_calls_in_expr(left, fixups);
            fixup_calls_in_expr(right, fixups);
        }
        Expr::UnaryOp { operand, .. } => fixup_calls_in_expr(operand, fixups),
        Expr::MethodCall { receiver, args, .. } => {
            fixup_calls_in_expr(receiver, fixups);
            for arg in args {
                fixup_calls_in_expr(&mut arg.node.value, fixups);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            fixup_calls_in_expr(object, fixups);
        }
        Expr::Index { object, index } => {
            fixup_calls_in_expr(object, fixups);
            fixup_calls_in_expr(index, fixups);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            fixup_calls_in_expr(condition, fixups);
            fixup_calls_in_expr(then_branch, fixups);
            for (cond, body) in elif_branches {
                fixup_calls_in_expr(cond, fixups);
                fixup_calls_in_expr(body, fixups);
            }
            if let Some(eb) = else_branch {
                fixup_calls_in_expr(eb, fixups);
            }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            fixup_calls_in_expr(scrutinee, fixups);
            for arm in arms {
                fixup_calls_in_expr(&mut arm.body, fixups);
            }
            if let Some(ea) = else_arm {
                fixup_calls_in_expr(ea, fixups);
            }
        }
        Expr::Block(block) => fixup_calls_in_block(&mut block.stmts, fixups),
        Expr::TupleLiteral(elems) => {
            for e in elems {
                fixup_calls_in_expr(e, fixups);
            }
        }
        Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner } | Expr::Await { expr: inner, .. } | Expr::Spawn { expr: inner, .. }
        | Expr::SpawnBlocking { expr: inner, .. } => {
            fixup_calls_in_expr(inner, fixups);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            fixup_calls_in_expr(body, fixups);
        }
        _ => {}
    }
}

/// Expand generic type aliases in all type annotations within an item.
fn expand_generic_aliases_in_item(
    item: &mut Item,
    generic: &FxHashMap<String, (Vec<String>, Type)>,
) {
    match item {
        Item::Function(f) => expand_generic_aliases_in_function(f, generic),
        Item::Equip(eq) => {
            expand_generic_alias_in_type(&mut eq.type_, generic);
            if let Some(trait_) = &mut eq.trait_ {
                expand_generic_alias_in_type(&mut trait_.trait_name, generic);
            }
            for method in &mut eq.items {
                expand_generic_aliases_in_function(&mut method.node, generic);
            }
        }
        Item::Struct(s) => {
            for field in &mut s.fields {
                expand_generic_alias_in_type(&mut field.node.type_, generic);
            }
        }
        Item::Enum(e) => {
            for variant in &mut e.variants {
                if let VariantFields::Tuple(types) = &mut variant.node.fields {
                    for ty in types {
                        expand_generic_alias_in_type(ty, generic);
                    }
                }
            }
        }
        Item::ConstDecl(c) => expand_generic_alias_in_type(&mut c.type_, generic),
        Item::StaticDecl(s) => expand_generic_alias_in_type(&mut s.type_, generic),
        Item::Trait(t) => {
            for ti in &mut t.items {
                if let TraitItem::Method(f) = &mut ti.node {
                    expand_generic_aliases_in_function(f, generic);
                }
            }
        }
        Item::ExternBlock(eb) => {
            for f in &mut eb.items {
                expand_generic_aliases_in_function(&mut f.node, generic);
            }
        }
        Item::Module { items, .. } => {
            for si in items {
                expand_generic_aliases_in_item(&mut si.node, generic);
            }
        }
        _ => {}
    }
}

fn expand_generic_aliases_in_function(
    f: &mut FunctionDef,
    generic: &FxHashMap<String, (Vec<String>, Type)>,
) {
    expand_generic_alias_in_type(&mut f.return_type, generic);
    for param in &mut f.params {
        expand_generic_alias_in_type(&mut param.node.type_, generic);
    }
    if let Some(throws_type) = &mut f.throws {
        expand_generic_alias_in_type(throws_type, generic);
    }
    // Walk the body for VarDecl type annotations
    if let FunctionBody::Block(block) = &mut f.body {
        for stmt in &mut block.stmts {
            if let Stmt::VarDecl { ref mut type_, .. } = stmt.node {
                expand_generic_alias_in_type(type_, generic);
            }
        }
    }
}

fn expand_generic_alias_in_type(
    ty: &mut Spanned<Type>,
    generic: &FxHashMap<String, (Vec<String>, Type)>,
) {
    match &mut ty.node {
        Type::Named { name, generic_args } => {
            // First, recurse into generic args
            for arg in generic_args.iter_mut() {
                expand_generic_alias_in_type(arg, generic);
            }
            // Check if this is a generic alias usage
            if let Some((param_names, underlying)) = generic.get(&name.node) {
                if generic_args.len() == param_names.len() {
                    let substituted = substitute_alias_params(underlying, param_names, generic_args);
                    ty.node = substituted;
                    // Recurse in case substitution introduced more aliases
                    expand_generic_alias_in_type(ty, generic);
                }
            }
        }
        Type::Array { element, .. } => expand_generic_alias_in_type(element, generic),
        Type::Slice { element } => expand_generic_alias_in_type(element, generic),
        Type::Tuple(elems) => {
            for e in elems {
                expand_generic_alias_in_type(e, generic);
            }
        }
        Type::Function { return_type, params, .. } => {
            expand_generic_alias_in_type(return_type, generic);
            for p in params {
                expand_generic_alias_in_type(p, generic);
            }
        }
        Type::Ref(inner) | Type::Owned(inner) => {
            expand_generic_alias_in_type(inner, generic);
        }
        Type::Pointer(inner) => {
            expand_generic_alias_in_type(inner, generic);
        }
        Type::Primitive(_) | Type::SelfType | Type::Inferred => {}
    }
}

/// Substitute generic param names in a type with the provided args.
fn substitute_alias_params(
    ty: &Type,
    param_names: &[String],
    args: &[Spanned<Type>],
) -> Type {
    match ty {
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                if let Some(idx) = param_names.iter().position(|p| p == &name.node) {
                    if idx < args.len() {
                        return args[idx].node.clone();
                    }
                }
            }
            let new_args: Vec<Spanned<Type>> = generic_args
                .iter()
                .map(|arg| Spanned {
                    node: substitute_alias_params(&arg.node, param_names, args),
                    span: arg.span,
                })
                .collect();
            Type::Named { name: name.clone(), generic_args: new_args }
        }
        Type::Tuple(elems) => Type::Tuple(
            elems.iter().map(|e| Spanned {
                node: substitute_alias_params(&e.node, param_names, args),
                span: e.span,
            }).collect(),
        ),
        Type::Function { return_type, params, param_ownerships } => Type::Function {
            return_type: Box::new(Spanned {
                node: substitute_alias_params(&return_type.node, param_names, args),
                span: return_type.span,
            }),
            params: params.iter().map(|p| Spanned {
                node: substitute_alias_params(&p.node, param_names, args),
                span: p.span,
            }).collect(),
            param_ownerships: param_ownerships.clone(),
        },
        Type::Array { element, size } => Type::Array {
            element: Box::new(Spanned {
                node: substitute_alias_params(&element.node, param_names, args),
                span: element.span,
            }),
            size: size.clone(),
        },
        Type::Slice { element } => Type::Slice {
            element: Box::new(Spanned {
                node: substitute_alias_params(&element.node, param_names, args),
                span: element.span,
            }),
        },
        Type::Ref(inner) => Type::Ref(Box::new(Spanned {
            node: substitute_alias_params(&inner.node, param_names, args),
            span: inner.span,
        })),
        Type::Owned(inner) => Type::Owned(Box::new(Spanned {
            node: substitute_alias_params(&inner.node, param_names, args),
            span: inner.span,
        })),
        _ => ty.clone(),
    }
}

/// Evaluate, substitute, and remove all meta constructs from a module.
/// `features` is the list of enabled build-time feature flags (from `--feature` CLI args).
pub fn evaluate_meta_consts(module: &mut Module, features: &[String]) -> Vec<SemanticError> {
    evaluate_meta_consts_impl(module, features, None)
}

pub fn evaluate_meta_consts_with_source_dir(
    module: &mut Module,
    features: &[String],
    source_dir: Option<std::path::PathBuf>,
) -> Vec<SemanticError> {
    evaluate_meta_consts_impl(module, features, source_dir)
}

fn evaluate_meta_consts_impl(
    module: &mut Module,
    features: &[String],
    source_dir: Option<std::path::PathBuf>,
) -> Vec<SemanticError> {
    let mut errors = Vec::new();
    let mut env: FxHashMap<String, MetaValue> = FxHashMap::default();
    let mut type_env: FxHashMap<String, Type> = FxHashMap::default();
    let mut type_func_env: FxHashMap<String, MetaTypeFunc> = FxHashMap::default();
    let mut generic_aliases: FxHashMap<String, (Vec<String>, Type)> = FxHashMap::default();

    // Phase 1: Evaluate meta consts, meta asserts, meta type aliases, and meta type functions.
    // Also collect `type` aliases (both simple and generic) into the same type_env.
    // Scope the ctx borrow so it ends before we mutate module.items in Phase 1.5.
    {
        let ctx = MetaContext::with_source_dir(features, &module.items, source_dir.clone());
        for item in &module.items {
            process_meta_item(&item.node, &mut env, &mut type_env, &mut type_func_env, &ctx, &mut errors);
        }
    }
    // Collect `type` aliases (both simple and generic) into the type envs.
    // Recurses into `Item::Module` so aliases declared in *imported* modules —
    // which `loader::merge_modules` wraps in an `Item::Module` node — are
    // collected too. Without this, an imported `type Entity = SlotKey` is never
    // erased and survives into resolve as an opaque `DefKind::TypeAlias` with no
    // struct body (Bug B).
    collect_type_aliases(&module.items, &mut type_env, &mut generic_aliases);

    // Phase 1.5: Flatten MetaIf (conditional compilation).
    // Snapshot the current items for the context so user-defined functions are still accessible.
    let items_snapshot = module.items.clone();
    {
        let ctx = MetaContext::with_source_dir(features, &items_snapshot, source_dir);
        module.items = flatten_meta_ifs(module.items.clone(), &mut env, &mut type_env, &mut type_func_env, &ctx, &mut errors);
    }

    // Phase 1.75: Fix up constructor calls for aliases whose underlying type has generic args.
    // E.g., `IntList()` where `type IntList = Vector[int]` → `Vector[int]()`.
    // Must run BEFORE substitute_item (which only renames identifiers without adding args).
    {
        let mut constructor_fixups: FxHashMap<String, (String, Vec<Spanned<Type>>)> = FxHashMap::default();
        for (alias_name, underlying) in &type_env {
            if let Type::Named { name, generic_args } = underlying {
                // Generic aliases (`type IntList = Vector[int]`) inject the
                // underlying generic args. Non-generic struct aliases
                // (`type Handle = SlotKey`) carry empty args — the rewrite
                // site renames the callee identifier only (and emits `None`,
                // matching a plain non-generic constructor call). Primitive
                // and function aliases never reach here (they're not
                // `Type::Named`), so `Count`/`Op` are correctly left alone.
                constructor_fixups.insert(
                    alias_name.clone(),
                    (name.node.clone(), generic_args.clone()),
                );
            }
        }
        if !constructor_fixups.is_empty() {
            for item in &mut module.items {
                fixup_constructor_calls_in_item(&mut item.node, &constructor_fixups);
            }
        }
    }

    // Phase 2: Substitute meta const references and type aliases throughout the AST
    for item in &mut module.items {
        substitute_item(&mut item.node, &env, &type_env);
    }

    // Phase 2.5: Expand generic type aliases (requires param substitution)
    if !generic_aliases.is_empty() {
        for item in &mut module.items {
            expand_generic_aliases_in_item(&mut item.node, &generic_aliases);
        }
    }

    // Phase 3: Remove all meta declarations and type aliases — at the top level
    // AND inside imported `Item::Module` wrappers. Edits (a)+(b) collect and
    // rewrite the *uses* of an imported alias, but its *declaration* lives
    // nested in an `Item::Module`; without recursing here it survives Phase 3,
    // reaches resolve, and re-creates the opaque `DefKind::TypeAlias` (Bug B).
    remove_meta_and_alias_items(&mut module.items);

    errors
}

/// Remove meta declarations and `type` aliases from `items`, recursing one level
/// into `Item::Module` (imported modules) — the removal counterpart to
/// `collect_type_aliases`. (`merge_modules` nests only one level.)
fn remove_meta_and_alias_items(items: &mut Vec<Spanned<Item>>) {
    items.retain(|item| {
        !matches!(
            &item.node,
            Item::MetaConst(_) | Item::MetaAssert(_) | Item::MetaLog(_)
            | Item::MetaType(_) | Item::MetaTypeFunc(_) | Item::MetaIf(_)
            | Item::TypeAlias(_)
        )
    });
    for item in items.iter_mut() {
        if let Item::Module { items: sub_items, .. } = &mut item.node {
            remove_meta_and_alias_items(sub_items);
        }
    }
}

/// Collect `type X = ...` aliases: non-generic into `type_env`, generic into
/// `generic_aliases`. Recurses one level into `Item::Module` so aliases from
/// imported modules are collected. (`merge_modules` produces only flat sibling
/// `Item::Module` nodes — single-level recursion is sufficient.)
fn collect_type_aliases(
    items: &[Spanned<Item>],
    type_env: &mut FxHashMap<String, Type>,
    generic_aliases: &mut FxHashMap<String, (Vec<String>, Type)>,
) {
    for item in items {
        match &item.node {
            Item::TypeAlias(ta) => {
                let param_names: Vec<String> = ta.generic_params.as_ref().map_or_else(Vec::new, |gp| {
                    gp.node.params.iter().filter_map(|p| match &p.node {
                        GenericParam::Type { name, .. } => Some(name.node.clone()),
                        _ => None,
                    }).collect()
                });
                if param_names.is_empty() {
                    type_env.insert(ta.name.node.clone(), ta.type_.node.clone());
                } else {
                    generic_aliases.insert(ta.name.node.clone(), (param_names, ta.type_.node.clone()));
                }
            }
            Item::Module { items, .. } => {
                collect_type_aliases(items, type_env, generic_aliases);
            }
            _ => {}
        }
    }
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
        Item::MetaLog(ml) => {
            let parts: Vec<String> = ml.args.iter().filter_map(|arg| {
                match eval_expr(&arg.node, env, ctx, arg.span) {
                    Ok(v) => Some(meta_value_to_string(&v)),
                    Err(e) => { errors.push(e); None }
                }
            }).collect();
            eprintln!("[meta] {}", parts.join(" "));
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
            "str"    => Some(PrimitiveType::StringType),
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
            } else if let Item::Module { path, items: mod_items } = item.node {
                // Recurse into imported module items to flatten their MetaIf blocks
                let flattened = flatten_meta_ifs(mod_items, env, type_env, type_func_env, ctx, errors);
                new_items.push(Spanned {
                    node: Item::Module { path, items: flattened },
                    span: item.span,
                });
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
        Expr::StringLiteral(s, _) => {
            // Only plain string literals (no interpolation segments)
            if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_, _))) {
                return Err(meta_err("interpolated strings cannot be evaluated at compile time", span));
            }
            let text: String = s.segments.iter().map(|seg| match seg {
                StringSegment::Literal(s) => s.as_str(),
                StringSegment::Interpolation(_, _) => unreachable!(),
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
                        // Normalize deprecated "str" → canonical "String"
                        let type_name = if type_name == "str" { "String".to_string() } else { type_name };
                        Ok(MetaValue::Str(type_name))
                    }
                    "embed_file" => {
                        if args.len() != 1 {
                            return Err(meta_err("embed_file() takes exactly 1 argument", span));
                        }
                        let path_val = eval_expr(&args[0].node.value.node, env, ctx, args[0].node.value.span)?;
                        let rel_path = match path_val {
                            MetaValue::Str(s) => s,
                            _ => return Err(meta_err("embed_file(): argument must be a string literal path", span)),
                        };
                        let full_path = match &ctx.source_dir {
                            Some(dir) => dir.join(&rel_path),
                            None => std::path::PathBuf::from(&rel_path),
                        };
                        match std::fs::read_to_string(&full_path) {
                            Ok(contents) => Ok(MetaValue::Str(contents)),
                            Err(e) => Err(meta_err(
                                &format!("embed_file(\"{rel_path}\"): {e}"),
                                span,
                            )),
                        }
                    }
                    other => {
                        // M7: fall back to user-defined function lookup
                        match lookup_meta_function(other, ctx.items, span)? {
                            Some(func_def) => eval_meta_fn_call(&func_def, args, env, ctx, span),
                            None => Err(meta_err(
                                &format!("unknown meta function `{other}` — built-ins: \
                                    platform(), arch(), arch_word_bits(), feature(str), debug(), \
                                    sizeof(Type), alignof(Type), typename(Type), embed_file(str); \
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
        // After meta-variable substitution in a `meta for` loop, what was an identifier
        // (e.g. `ftype`) becomes a StringLiteral holding the resolved type name.
        // Extract the plain text so `T is numeric` still works post-substitution.
        Expr::StringLiteral(s, _) => s.segments.iter().filter_map(|seg| {
            if let StringSegment::Literal(t) = seg { Some(t.as_str()) } else { None }
        }).collect(),
        Expr::Index { object, index } => {
            let base = meta_expr_to_type_name(&object.node);
            let idx  = meta_expr_to_type_name(&index.node);
            format!("{base}[{idx}]")
        }
        _ => "?".to_string(),
    }
}

/// Extract a plain name string from a pattern used in `T is <pattern>`.
/// Handles `Pattern::Binding` (identifiers and type-keyword names) and
/// `Pattern::Constructor` (e.g. `T is Some(...)` — uses the final path segment).
fn pattern_to_name(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Binding(name) => name.clone(),
        Pattern::Constructor { path, .. } => {
            // Use the last segment of the path (e.g. `Color.Red` → "Red")
            path.last().map(|s| s.node.clone()).unwrap_or_else(|| "?".to_string())
        }
        _ => "?".to_string(),
    }
}

/// Evaluate `T is Category` in a delayed meta context (monomorphization time).
///
/// Returns `true` if the resolved type name matches the given category or exact
/// type name. Category keywords (`int`, `float`, `signed`, `unsigned`, `numeric`,
/// `Enum`, `Struct`) match entire families of types; everything else is an exact
/// string match against the canonical type name produced by `type_to_canonical_name`.
fn eval_type_is_check(resolved: &str, category: &str, type_registry: &crate::ir::types::TypeRegistry) -> bool {
    match category {
        // Broad category: any integer type (signed or unsigned)
        "int" | "integer" => matches!(resolved,
            "int8" | "int16" | "int32" | "int" | "int64" |
            "uint8" | "uint16" | "uint32" | "uint" | "uint64"),
        // Broad category: any floating-point type
        "float" => matches!(resolved, "float32" | "float" | "float64"),
        // Signed integers only
        "signed" => matches!(resolved, "int8" | "int16" | "int32" | "int" | "int64"),
        // Unsigned integers only
        "unsigned" => matches!(resolved, "uint8" | "uint16" | "uint32" | "uint" | "uint64"),
        // Any numeric type (integer or float)
        "numeric" => matches!(resolved,
            "int8" | "int16" | "int32" | "int" | "int64" |
            "uint8" | "uint16" | "uint32" | "uint" | "uint64" |
            "float32" | "float" | "float64"),
        // Single-member categories (also exact matches)
        "bool" | "str" | "String" | "char" | "void" => resolved == category,
        // Registry-backed categories: check if the resolved type is an enum or struct.
        "Enum" | "enum" => {
            type_registry.get_type_def(resolved)
                .map(|def| matches!(def.kind, crate::ir::types::TypeDefKind::Enum(_)))
                .unwrap_or(false)
        }
        "Struct" | "struct" => {
            type_registry.get_type_def(resolved)
                .map(|def| matches!(def.kind, crate::ir::types::TypeDefKind::Struct(_)))
                .unwrap_or(false)
        }
        // Exact match for everything else: float32, int8, uint64, MyStruct, etc.
        other => resolved == other,
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
        // str/String = Str { *u8, u64, u64, *Alloc } → 32 bytes (unified)
        "str" | "String" => Some(32),
        // cstr = const char*                           →  8 bytes
        "cstr" => Some(8),
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
        (MetaValue::Int(a), BinaryOp::Shl, MetaValue::Int(b)) => {
            if *b < 0 || *b >= 64 {
                Err(meta_err(&format!("shift amount {b} out of range (0..63)"), span))
            } else {
                Ok(MetaValue::Int(a << b))
            }
        }
        (MetaValue::Int(a), BinaryOp::Shr, MetaValue::Int(b)) => {
            if *b < 0 || *b >= 64 {
                Err(meta_err(&format!("shift amount {b} out of range (0..63)"), span))
            } else {
                Ok(MetaValue::Int(a >> b))
            }
        }

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
            | PrimitiveType::StringType
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

        Stmt::Break => Ok(MetaControlFlow::Break),
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

        Stmt::While { condition, body, else_body } => {
            let mut iterations: u64 = 0;
            // `did_break` distinguishes natural completion (condition went
            // false) from an explicit `break`; the `else` body runs only on
            // natural completion, matching runtime `while … else` semantics
            // (language-reference: "Supports else (runs if loop exits normally
            // without break)"). Without this, compile-time evaluation of a
            // function diverged from its runtime evaluation.
            let mut did_break = false;
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
                    MetaControlFlow::Break => {
                        did_break = true;
                        break;
                    }
                    r @ MetaControlFlow::Return(_) => return Ok(r),
                }
            }
            if !did_break {
                if let Some(else_blk) = else_body {
                    return eval_meta_block(else_blk, env, ctx);
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

        Stmt::For { pattern, iterable, body, else_body, .. } => {
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
                    // `else` runs only on natural completion (loop ran to the
                    // end of the range), not after a `break` — matching runtime
                    // `for … else` semantics.
                    let mut did_break = false;
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
                            MetaControlFlow::Break => {
                                did_break = true;
                                break;
                            }
                            r @ MetaControlFlow::Return(_) => return Ok(r),
                        }
                        i += 1;
                    }
                    if !did_break {
                        if let Some(else_blk) = else_body {
                            return eval_meta_block(else_blk, env, ctx);
                        }
                    }
                    Ok(MetaControlFlow::Continue)
                }
                _ => Err(meta_err(
                    "only range-based for-loops are supported in compile-time functions",
                    iterable.span,
                )),
            }
        }

        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
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
        | Stmt::NamedScope { .. }
        | Stmt::OnError { .. }
        | Stmt::Snapshot { .. }
        | Stmt::Item(_) => Err(meta_err(
            "this statement type is not supported in compile-time function evaluation",
            stmt_span,
        )),

        Stmt::MetaLog { args, .. } => {
            // meta log is always valid in compile-time functions — evaluate and print.
            let meta_ctx = MetaContext::new(ctx.features, ctx.items);
            let parts: Vec<String> = args.iter().filter_map(|arg| {
                match eval_expr(&arg.node, env, &meta_ctx, arg.span) {
                    Ok(v) => Some(meta_value_to_string(&v)),
                    Err(_) => None,
                }
            }).collect();
            eprintln!("[meta] {}", parts.join(" "));
            Ok(MetaControlFlow::Continue)
        }
        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } | Stmt::MetaMatch { .. }
        | Stmt::MetaWhile { .. } | Stmt::MetaConst { .. } => Err(meta_err(
            "`meta if`/`meta for`/`meta match`/`meta while`/`meta const` in function body requires generic type parameters \
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
        (Type::Primitive(PrimitiveType::StringType), MetaValue::Str(_)) => true,
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
        Type::Ref(inner) | Type::Owned(inner) | Type::Pointer(inner) => {
            substitute_type(inner, type_env);
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
            substitute_block(&mut t.body, env, type_env);
        }
        Item::Bench(b) => {
            substitute_block(&mut b.body, env, type_env);
        }
        Item::SuiteSetup(s) => substitute_block(&mut s.body, env, type_env),
        Item::SuiteTeardown(s) => substitute_block(&mut s.body, env, type_env),
        Item::Import(_) | Item::Directive(_) | Item::MetaConst(_) | Item::MetaType(_)
        | Item::MetaTypeFunc(_) | Item::MetaAssert(_) | Item::MetaIf(_) | Item::MetaLog(_) => {}
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

fn substitute_pattern(pattern: &mut Spanned<Pattern>, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    match &mut pattern.node {
        Pattern::Constructor { path, fields } => {
            // Substitute the last path segment if it is a meta string variable
            // e.g. `case vname(c):` with vname="IntCol" → `case IntCol(c):`
            if let Some(last) = path.last_mut() {
                if let Some(MetaValue::Str(s)) = env.get(&last.node) {
                    last.node = s.clone();
                }
            }
            for field in fields.iter_mut() {
                substitute_pattern(field, env, type_env);
            }
        }
        Pattern::Tuple(patterns) | Pattern::Or(patterns) => {
            for p in patterns.iter_mut() {
                substitute_pattern(p, env, type_env);
            }
        }
        Pattern::Literal(expr) => substitute_expr(expr, env, type_env),
        Pattern::DotShorthand { fields, .. } => {
            for f in fields.iter_mut() {
                substitute_pattern(f, env, type_env);
            }
        }
        Pattern::Wildcard | Pattern::Binding(_) | Pattern::Rest => {}
    }
}

pub fn substitute_match_arm(arm: &mut MatchArm, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    substitute_pattern(&mut arm.pattern, env, type_env);
    if let Some(guard) = &mut arm.guard { substitute_expr(guard, env, type_env); }
    substitute_expr(&mut arm.body, env, type_env);
}

/// Convert a Gorget type-name string (e.g. "int", "float", "str") to an AST `Type`.
/// Public so it can be used by the generic scanner to build type substitution environments.
/// Used to build the `type_env` so that meta string variables (like `T` in
/// `meta for vname, T in variant_payloads(Column)`) can be substituted into
/// generic type argument positions (e.g. `col_slice_inner[T]` → `col_slice_inner[int]`).
pub fn meta_str_to_type(s: &str) -> Type {
    use crate::parser::ast::{PrimitiveType, Type as AstType};
    use crate::span::Spanned;
    match s {
        "int"     => AstType::Primitive(PrimitiveType::Int),
        "int8"    => AstType::Primitive(PrimitiveType::Int8),
        "int16"   => AstType::Primitive(PrimitiveType::Int16),
        "int32"   => AstType::Primitive(PrimitiveType::Int32),
        "int64"   => AstType::Primitive(PrimitiveType::Int64),
        "uint"    => AstType::Primitive(PrimitiveType::Uint),
        "uint8"   => AstType::Primitive(PrimitiveType::Uint8),
        "uint16"  => AstType::Primitive(PrimitiveType::Uint16),
        "uint32"  => AstType::Primitive(PrimitiveType::Uint32),
        "uint64"  => AstType::Primitive(PrimitiveType::Uint64),
        "float"   => AstType::Primitive(PrimitiveType::Float),
        "float32" => AstType::Primitive(PrimitiveType::Float32),
        "float64" => AstType::Primitive(PrimitiveType::Float64),
        "bool"    => AstType::Primitive(PrimitiveType::Bool),
        "str"     => AstType::Primitive(PrimitiveType::StringType),
        "String"  => AstType::Primitive(PrimitiveType::StringType),
        "void"    => AstType::Primitive(PrimitiveType::Void),
        other     => AstType::Named { name: Spanned::dummy(other.to_string()), generic_args: vec![] },
    }
}

/// Expand every `MatchItem::MetaFor` in `arms` into concrete `MatchItem::Arm` nodes in-place.
/// Called from `evaluate_delayed_meta_block` before processing match arm bodies.
fn expand_match_meta_for(
    arms: &mut Vec<MatchItem>,
    ctx: &DelayedMetaContext<'_>,
    errors: &mut Vec<SemanticError>,
) {
    let mut result: Vec<MatchItem> = Vec::with_capacity(arms.len());
    let local_env = ctx.meta_env;

    for item in arms.drain(..) {
        match item {
            MatchItem::Arm(_) => result.push(item),
            MatchItem::MetaFor { ref vars, ref range, ref arm_template, .. } => {
                let range_span = range.span;
                match eval_delayed_expr(&range.node, ctx, range_span) {
                    Ok(MetaValue::List(items)) => {
                        for item_val in items {
                            let mut child_env = local_env.clone();
                            if vars.len() == 1 {
                                child_env.insert(vars[0].node.clone(), item_val);
                            } else if let MetaValue::List(parts) = item_val {
                                for (var, part) in vars.iter().zip(parts.into_iter()) {
                                    child_env.insert(var.node.clone(), part);
                                }
                            } else {
                                errors.push(meta_err(
                                    "meta for (match): multi-variable destructuring requires a list of lists",
                                    range_span,
                                ));
                                break;
                            }
                            // Build type_env from string meta values so that meta vars
                            // can be used as generic type arguments (e.g. `fn[T]`).
                            let type_env: FxHashMap<String, Type> = child_env.iter()
                                .filter_map(|(k, v)| {
                                    if let MetaValue::Str(s) = v {
                                        Some((k.clone(), meta_str_to_type(s)))
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            let mut concrete_arm = arm_template.clone();
                            substitute_match_arm(&mut concrete_arm, &child_env, &type_env);
                            // Also recurse into the arm body if it's a block
                            let child_ctx = DelayedMetaContext { meta_env: &child_env, ..*ctx };
                            if let Expr::Block(block) = &mut concrete_arm.body.node {
                                evaluate_delayed_meta_block(block, &child_ctx, errors);
                            }
                            result.push(MatchItem::Arm(concrete_arm));
                        }
                    }
                    Ok(_) => {
                        errors.push(meta_err(
                            "meta for (match): range must evaluate to a list (e.g. variant_payloads(T))",
                            range_span,
                        ));
                    }
                    Err(e) => errors.push(e),
                }
            }
        }
    }
    *arms = result;
}

fn substitute_stmt(stmt: &mut Stmt, env: &FxHashMap<String, MetaValue>, type_env: &FxHashMap<String, Type>) {
    match stmt {
        Stmt::VarDecl { type_, value, .. } => {
            substitute_type(type_, type_env);
            substitute_expr(value, env, type_env);
        }
        Stmt::Expr(expr) => {
            substitute_expr(expr, env, type_env);
            // Post-substitution: rewrite field_set(obj, "field", value) → obj.field = value
            if let Expr::Call { ref callee, ref args, .. } = expr.node {
                if let Expr::Identifier(ref cname) = callee.node {
                    if cname == "field_set" && args.len() == 3 {
                        if let Expr::StringLiteral(ref s, _) = args[1].node.value.node {
                            if !s.has_interpolation() {
                                let field_name: String = s.segments.iter()
                                    .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                                    .collect();
                                if !field_name.is_empty() {
                                    let obj_expr = args[0].node.value.clone();
                                    let val_expr = args[2].node.value.clone();
                                    let field_span = args[1].node.value.span;
                                    let target = Spanned::new(Expr::FieldAccess {
                                        object: Box::new(obj_expr),
                                        field: Spanned::new(field_name, field_span),
                                    }, expr.span);
                                    *stmt = Stmt::Assign {
                                        target,
                                        value: val_expr,
                                    };
                                }
                            }
                        }
                    }
                }
            }
        }
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
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::Pass => {}
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
            for item in arms.iter_mut() {
                match item {
                    MatchItem::Arm(arm) => {
                        substitute_match_arm(arm, env, type_env);
                    }
                    MatchItem::MetaFor { range, arm_template, .. } => {
                        substitute_expr(range, env, type_env);
                        substitute_match_arm(arm_template, env, type_env);
                    }
                }
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
        Stmt::NamedScope { body, .. } => substitute_block(body, env, type_env),
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            substitute_expr(condition, env, type_env);
            if let Some(msg) = message { substitute_expr(msg, env, type_env); }
        }
        Stmt::Snapshot { value, .. } => {
            substitute_expr(value, env, type_env);
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
        Stmt::MetaLog { args, .. } => {
            for arg in args { substitute_expr(arg, env, type_env); }
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
        Stmt::MetaConst { value, .. } => {
            substitute_expr(value, env, type_env);
        }
        Stmt::OnError { body } => {
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
            // Special case: if the callee is a bare identifier bound to a meta string,
            // substitute it as an identifier rename (e.g. `vname(...)` → `Circle(...)`)
            // rather than converting it to a string literal (`"Circle"(...)`).
            if let Expr::Identifier(ref cname) = callee.node {
                if let Some(MetaValue::Str(s)) = env.get(cname.as_str()) {
                    callee.node = Expr::Identifier(s.clone());
                }
            }
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
        Expr::DefaultOp { lhs, rhs } => {
            substitute_expr(lhs, env, type_env);
            substitute_expr(rhs, env, type_env);
        }
        Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner } | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner } | Expr::Spawn { expr: inner, .. }
        | Expr::SpawnBlocking { expr: inner, .. } => {
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
        // Meta op: recurse into operands; operator substitution handled below
        Expr::MetaOpInfix { left, right, .. } => {
            substitute_expr(left, env, type_env);
            substitute_expr(right, env, type_env);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, transform, .. } => {
            substitute_expr(expr, env, type_env);
            substitute_expr(transform, env, type_env);
        }
        Expr::Catch { expr, recovery, .. } => {
            substitute_expr(expr, env, type_env);
            substitute_expr(recovery, env, type_env);
        }
        Expr::FaultCatch { expr, handler, .. } => {
            substitute_expr(expr, env, type_env);
            substitute_expr(handler, env, type_env);
        }
        // Leaf nodes — no recursion needed
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } | Expr::It => {}
        // StringLiteral handled below
        Expr::StringLiteral(_, _) => {}
    }

    // Substitute MetaOpInfix → BinaryOp when the op_name is bound to a MetaValue::Op
    {
        let found_op = if let Expr::MetaOpInfix { ref op_name, .. } = expr.node {
            env.get(op_name.as_str()).and_then(|v| {
                if let MetaValue::Op(op) = v { Some(*op) } else { None }
            })
        } else {
            None
        };
        if let Some(bin_op) = found_op {
            let old = std::mem::replace(&mut expr.node, Expr::IntLiteral(0));
            if let Expr::MetaOpInfix { left, right, .. } = old {
                expr.node = Expr::BinaryOp { left, op: bin_op, right };
            }
        }
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

    // Also handle string interpolation segments. When a meta-variable name
    // matches an interpolation, replace the segment with a Literal AND drop
    // the corresponding parser-supplied `interp_exprs` entry (it would now
    // be stale — a `Identifier("fname")` reference to a name that exists
    // only in the meta env, not the runtime scope, would otherwise trigger
    // a spurious "undefined name" diagnostic during resolution).
    if let Expr::StringLiteral(s, interp_exprs) = &mut expr.node {
        let mut interp_idx = 0usize;
        let mut to_drop: Vec<usize> = Vec::new();
        for seg in &mut s.segments {
            if let StringSegment::Interpolation(name, _) = seg {
                if let Some(value) = env.get(name.as_str()) {
                    *seg = StringSegment::Literal(meta_value_to_string(value));
                    to_drop.push(interp_idx);
                }
                interp_idx += 1;
            }
        }
        // Drop in reverse order so indices stay valid as we shrink the Vec.
        for idx in to_drop.into_iter().rev() {
            if idx < interp_exprs.len() {
                interp_exprs.remove(idx);
            }
        }
    }

    // Post-recursion: rewrite field_value(val, "field") → val.field
    // After substitution, the second arg should now be a plain string literal.
    if let Expr::Call { ref callee, ref args, .. } = expr.node {
        if let Expr::Identifier(ref cname) = callee.node {
            if cname == "field_value" && args.len() == 2 {
                if let Expr::StringLiteral(ref s, _) = args[1].node.value.node {
                    if !s.has_interpolation() {
                        let field_name: String = s.segments.iter()
                            .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                            .collect();
                        if !field_name.is_empty() {
                            let val_expr = args[0].node.value.clone();
                            let field_span = args[1].node.value.span;
                            expr.node = Expr::FieldAccess {
                                object: Box::new(val_expr),
                                field: Spanned::new(field_name, field_span),
                            };
                        }
                    }
                }
            }
        }
    }

    // Post-recursion: rewrite make_variant(T, "Variant") → Expr::Path ["T", "Variant"]
    // After meta substitution the second arg is a plain string literal.
    if let Expr::Call { ref callee, ref args, .. } = expr.node {
        if let Expr::Identifier(ref cname) = callee.node {
            if cname == "make_variant" && args.len() == 2 {
                if let Expr::StringLiteral(ref s, _) = args[1].node.value.node {
                    if !s.has_interpolation() {
                        let variant_name: String = s.segments.iter()
                            .filter_map(|seg| if let StringSegment::Literal(l) = seg { Some(l.as_str()) } else { None })
                            .collect();
                        if !variant_name.is_empty() {
                            if let Expr::Identifier(type_name) = args[0].node.value.node.clone() {
                                let type_span = args[0].node.value.span;
                                let var_span  = args[1].node.value.span;
                                expr.node = Expr::Path {
                                    segments: vec![
                                        Spanned::new(type_name, type_span),
                                        Spanned::new(variant_name, var_span),
                                    ],
                                };
                            }
                        }
                    }
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
        }, Vec::new()),
        MetaValue::List(_) => {
            // Lists are not representable as a single AST expression;
            // they are only used internally by meta for iteration.
            panic!("meta List value cannot be substituted into AST expression position")
        }
        MetaValue::Op(_) => {
            // Op tokens are consumed by MetaOpInfix substitution; they never appear standalone.
            panic!("meta Op value cannot be substituted into AST expression position")
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
        MetaValue::Op(op) => format!("{op:?}"),
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
        MetaValue::Op(_) => "op",
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
        Type::Primitive(PrimitiveType::CStr) => "cstr",
        Type::Primitive(PrimitiveType::StringType) => "String",
        Type::Primitive(PrimitiveType::Void) => "void",
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
            PrimitiveType::CStr => "cstr",
            PrimitiveType::StringType => "String",
            PrimitiveType::Void => "void",
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

        // `T is Category` / `T is not Category` — type predicate
        Expr::Is { expr, negated, pattern } => {
            // Resolve LHS to canonical type name (handles generic type params)
            let raw = meta_expr_to_type_name(&expr.node);
            let resolved = ctx.type_subs.iter()
                .find(|(p, _)| *p == raw)
                .map(|(_, ty)| type_to_canonical_name(ty))
                .unwrap_or_else(|| raw.clone());

            let category = pattern_to_name(&pattern.node);
            let result = eval_type_is_check(&resolved, &category, ctx.type_registry);
            Ok(MetaValue::Bool(if *negated { !result } else { result }))
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
                                        let ft = ctx.type_registry.type_id_to_canonical_name(field.type_id);
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
                    "fields" => {
                        if args.len() != 1 {
                            return Err(meta_err("fields() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Struct(s) = &type_def.kind {
                                    let pairs = s.fields.iter().map(|f| {
                                        let raw_name = ctx.type_registry.type_id_to_canonical_name(f.type_id);
                                        // Normalize GIR-internal names to Gorget language names:
                                        // "Str" → "String", "GorgetString" → "String"
                                        let ty_name = match raw_name.as_str() {
                                            "Str" => "String".to_string(),
                                            "GorgetString" => "String".to_string(),
                                            other => other.to_string(),
                                        };
                                        MetaValue::List(vec![
                                            MetaValue::Str(f.name.clone()),
                                            MetaValue::Str(ty_name),
                                        ])
                                    }).collect();
                                    return Ok(MetaValue::List(pairs));
                                }
                                return Err(meta_err(&format!("fields: `{type_name}` is not a struct"), span));
                            }
                            None => return Err(meta_err(
                                &format!("fields: unknown type `{type_name}`"), span)),
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
                    "variant_payloads" => {
                        // variant_payloads(T) — returns a list of [variant_name, inner_type_arg] pairs
                        // for enum variants that each hold exactly one generic payload.
                        // e.g. for Column: [["IntCol","int"],["FloatCol","float"],...]
                        if args.len() != 1 {
                            return Err(meta_err("variant_payloads() takes exactly 1 argument", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Enum(e) = &type_def.kind {
                                    let pairs: Vec<MetaValue> = e.variants.iter().map(|v| {
                                        let inner = if v.fields.len() == 1 {
                                            // Extract the inner type from the GIR canonical name.
                                            // Monomorphised types are mangled as "Base__TypeArg"
                                            // (e.g. TypedColumn__int64_t).  Split on the first "__"
                                            // and reverse-map the C-level suffix to a Gorget name.
                                            let raw_name = ctx.type_registry.type_id_to_canonical_name(v.fields[0].type_id);
                                            if let Some(idx) = raw_name.find("__") {
                                                let suffix = &raw_name[idx + 2..];
                                                match suffix {
                                                    "int64_t"      => "int".to_string(),
                                                    "int32_t"      => "int32".to_string(),
                                                    "int16_t"      => "int16".to_string(),
                                                    "int8_t"       => "int8".to_string(),
                                                    "uint64_t"     => "uint".to_string(),
                                                    "uint32_t"     => "uint32".to_string(),
                                                    "uint16_t"     => "uint16".to_string(),
                                                    "uint8_t"      => "uint8".to_string(),
                                                    "double"       => "float".to_string(),
                                                    "float"        => "float32".to_string(),
                                                    "bool"         => "bool".to_string(),
                                                    "Str"          => "String".to_string(),
                                                    "GorgetString" => "String".to_string(),
                                                    other          => other.to_string(),
                                                }
                                            } else {
                                                // Primitive or non-generic named type — canonical name as-is,
                                                // with the same "Str"→"String" normalisation used by fields().
                                                match raw_name.as_str() {
                                                    "Str"          => "String".to_string(),
                                                    "GorgetString" => "String".to_string(),
                                                    other          => other.to_string(),
                                                }
                                            }
                                        } else {
                                            // Multi-field or unit variant: fall back to empty string
                                            String::new()
                                        };
                                        MetaValue::List(vec![
                                            MetaValue::Str(v.name.clone()),
                                            MetaValue::Str(inner),
                                        ])
                                    }).collect();
                                    return Ok(MetaValue::List(pairs));
                                }
                                return Err(meta_err(&format!("variant_payloads: `{type_name}` is not an enum"), span));
                            }
                            None => return Err(meta_err(
                                &format!("variant_payloads: unknown type `{type_name}`"), span)),
                        }
                    }
                    "enum_ordinal" => {
                        if args.len() != 2 {
                            return Err(meta_err("enum_ordinal() takes exactly 2 arguments: (T, \"VariantName\")", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let variant_name = match eval_delayed_expr(&args[1].node.value.node, ctx, span)? {
                            MetaValue::Str(s) => s,
                            _ => return Err(meta_err("enum_ordinal(): second argument must be a variant name string", span)),
                        };
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Enum(e) = &type_def.kind {
                                    match e.variants.iter().position(|v| v.name == variant_name) {
                                        Some(idx) => return Ok(MetaValue::Int(idx as i64)),
                                        None => return Err(meta_err(
                                            &format!("enum_ordinal: `{type_name}` has no variant `{variant_name}`"),
                                            span)),
                                    }
                                }
                                return Err(meta_err(&format!("enum_ordinal: `{type_name}` is not an enum"), span));
                            }
                            None => return Err(meta_err(
                                &format!("enum_ordinal: unknown type `{type_name}`"), span)),
                        }
                    }
                    "enum_from_ordinal" => {
                        if args.len() != 2 {
                            return Err(meta_err("enum_from_ordinal() takes exactly 2 arguments: (T, n)", span));
                        }
                        let raw = meta_expr_to_type_name(&args[0].node.value.node);
                        let type_name = ctx.type_subs.iter().find(|(p, _)| *p == raw)
                            .map(|(_, ty)| type_to_canonical_name(ty))
                            .unwrap_or(raw);
                        let ordinal = match eval_delayed_expr(&args[1].node.value.node, ctx, span)? {
                            MetaValue::Int(n) => n,
                            _ => return Err(meta_err("enum_from_ordinal(): second argument must be an integer", span)),
                        };
                        match ctx.type_registry.get_type_def(&type_name) {
                            Some(type_def) => {
                                if let crate::ir::types::TypeDefKind::Enum(e) = &type_def.kind {
                                    if ordinal < 0 {
                                        return Err(meta_err(
                                            &format!("enum_from_ordinal: ordinal {ordinal} is negative"),
                                            span,
                                        ));
                                    }
                                    match e.variants.get(ordinal as usize) {
                                        Some(v) => return Ok(MetaValue::Str(v.name.clone())),
                                        None => return Err(meta_err(
                                            &format!("enum_from_ordinal: ordinal {ordinal} out of range \
                                                      for `{type_name}` ({} variants)", e.variants.len()),
                                            span)),
                                    }
                                }
                                return Err(meta_err(&format!("enum_from_ordinal: `{type_name}` is not an enum"), span));
                            }
                            None => return Err(meta_err(
                                &format!("enum_from_ordinal: unknown type `{type_name}`"), span)),
                        }
                    }
                    "field_value" => {
                        return Err(meta_err(
                            "field_value() accesses a runtime struct field and cannot be used as a \
                             compile-time meta const; use it directly in a runtime statement: \
                             `auto v = field_value(val, fname)` or inline: `print(\"{field_value(val, fname)}\")`",
                            span,
                        ));
                    }
                    "field_set" => {
                        return Err(meta_err(
                            "field_set() assigns a runtime struct field and cannot be used as a \
                             compile-time meta const; use it directly in a runtime statement: \
                             `field_set(obj, fname, value)`",
                            span,
                        ));
                    }
                    "make_variant" => {
                        return Err(meta_err(
                            "make_variant() constructs an enum variant at runtime and cannot be used as a \
                             compile-time meta const; use it directly in a runtime statement inside `meta for`",
                            span,
                        ));
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
/// Cheap recursive scan: does `block` (or any nested block reachable via
/// `Stmt::If`/`While`/`For`/`Loop`/`Match`/`Select`/`With`/`Unsafe`/`NamedScope`/
/// `MetaIf`/`MetaFor`/`MetaMatch`/`MetaWhile`) contain any "delayed meta"
/// statement (`Stmt::Meta*`) or `MatchItem::MetaFor`? Used at the three
/// `evaluate_delayed_meta_block` call sites (non-generic fn body, inherent
/// equip method body, generic equip method body) to elide the upfront
/// `block.clone()` when there's nothing for the meta-eval pass to do.
/// Read-only AST walk with early exit on first hit — no allocation.
pub fn block_has_delayed_meta(block: &Block) -> bool {
    block.stmts.iter().any(|s| stmt_has_delayed_meta(&s.node))
}

fn stmt_has_delayed_meta(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::MetaIf { .. }
        | Stmt::MetaFor { .. }
        | Stmt::MetaConst { .. }
        | Stmt::MetaMatch { .. }
        | Stmt::MetaWhile { .. }
        | Stmt::MetaLog { .. } => true,
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            block_has_delayed_meta(then_body)
                || elif_branches.iter().any(|(_, b)| block_has_delayed_meta(b))
                || else_body.as_ref().map_or(false, block_has_delayed_meta)
        }
        Stmt::While { body, else_body, .. } => {
            block_has_delayed_meta(body)
                || else_body.as_ref().map_or(false, block_has_delayed_meta)
        }
        Stmt::For { body, else_body, .. } => {
            block_has_delayed_meta(body)
                || else_body.as_ref().map_or(false, block_has_delayed_meta)
        }
        Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            block_has_delayed_meta(body)
        }
        Stmt::With { body, .. } => block_has_delayed_meta(body),
        Stmt::Match { arms, else_arm, .. } => {
            arms.iter().any(|item| match item {
                MatchItem::MetaFor { .. } => true,
                MatchItem::Arm(a) => match &a.body.node {
                    Expr::Block(b) => block_has_delayed_meta(b),
                    _ => false,
                },
            }) || else_arm.as_ref().map_or(false, block_has_delayed_meta)
        }
        Stmt::Select { arms, else_arm } => {
            arms.iter().any(|arm| block_has_delayed_meta(&arm.body))
                || else_arm.as_ref().map_or(false, block_has_delayed_meta)
        }
        Stmt::OnError { body } => block_has_delayed_meta(body),
        // Leaf stmts — no sub-blocks to traverse. We do NOT scan expressions
        // for nested closures/blocks: `evaluate_delayed_meta_block` itself
        // does not recurse into expressions either (see
        // `recurse_delayed_meta_in_stmt`), so a closure body that contains
        // meta nodes is handled by a separate call when its enclosing
        // function is lowered, not by walking the outer function's AST.
        _ => false,
    }
}

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
    let mut local_env = (*ctx.meta_env).clone();

    // If the initial env is non-empty (e.g., from meta op bindings pre-loaded in
    // lower_generic_function), do an upfront sweep so all MetaOpInfix nodes that
    // reference those bindings are resolved to BinaryOp before further processing.
    if !local_env.is_empty() {
        let empty_type_env = FxHashMap::default();
        for stmt in &mut block.stmts {
            substitute_stmt(&mut stmt.node, &local_env, &empty_type_env);
        }
    }

    let mut i = 0;
    while i < block.stmts.len() {
        // ── MetaConst: evaluate, bind, substitute remaining stmts, remove ──
        if let Stmt::MetaConst { name, value, .. } = &block.stmts[i].node {
            let name_str = name.node.clone();
            let val_expr = value.node.clone();
            let val_span = value.span;
            let val_result = {
                let cur = DelayedMetaContext { meta_env: &local_env, ..*ctx };
                eval_delayed_expr(&val_expr, &cur, val_span)
            }; // cur dropped — local_env borrow released
            match val_result {
                Ok(val) => {
                    local_env.insert(name_str, val);
                    let empty_type_env = FxHashMap::default();
                    for stmt in &mut block.stmts[i + 1..] {
                        substitute_stmt(&mut stmt.node, &local_env, &empty_type_env);
                    }
                }
                Err(e) => errors.push(e),
            }
            block.stmts.remove(i);
            continue;
        }

        // ── All other statements: build cur from local_env ──
        let replacement = {
            let cur = DelayedMetaContext { meta_env: &local_env, ..*ctx };
            match &block.stmts[i].node {
                Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
                    let cond_span = condition.span;
                    match eval_delayed_expr(&condition.node, &cur, cond_span) {
                        Ok(MetaValue::Bool(true)) => {
                            let mut body = then_body.clone();
                            evaluate_delayed_meta_block(&mut body, &cur, errors);
                            Some(body.stmts)
                        }
                        Ok(MetaValue::Bool(false)) => {
                            // Try elif branches
                            let mut taken: Option<Vec<Spanned<Stmt>>> = None;
                            for (elif_cond, elif_body) in elif_branches.iter() {
                                match eval_delayed_expr(&elif_cond.node, &cur, elif_cond.span) {
                                    Ok(MetaValue::Bool(true)) => {
                                        let mut body = elif_body.clone();
                                        evaluate_delayed_meta_block(&mut body, &cur, errors);
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
                                evaluate_delayed_meta_block(&mut body, &cur, errors);
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

                Stmt::MetaFor { vars, range, body, .. } => {
                    let range_span = range.span;
                    // Evaluate range bounds
                    match eval_delayed_meta_range(&range.node, &cur, range_span) {
                        Ok((start_val, end_val, inclusive)) => {
                            if vars.len() > 1 {
                                errors.push(meta_err(
                                    "meta for: multi-variable destructuring is not valid for integer ranges",
                                    range.span,
                                ));
                                Some(vec![])
                            } else {
                                let upper = if inclusive { end_val + 1 } else { end_val };
                                let loop_var = vars[0].node.clone();
                                let mut result_stmts: Vec<Spanned<Stmt>> = Vec::new();
                                let empty_type_env = FxHashMap::default();
                                for val in start_val..upper {
                                    // Build a child context with loop var added to local_env
                                    let mut child_env = local_env.clone();
                                    child_env.insert(loop_var.clone(), MetaValue::Int(val));
                                    let child_ctx = DelayedMetaContext { meta_env: &child_env, ..*ctx };
                                    let mut loop_body = body.clone();
                                    substitute_block(&mut loop_body, &child_env, &empty_type_env);
                                    evaluate_delayed_meta_block(&mut loop_body, &child_ctx, errors);
                                    result_stmts.extend(loop_body.stmts);
                                }
                                Some(result_stmts)
                            }
                        }
                        Err(_range_err) => {
                            // Not an integer range — try evaluating as a list expression
                            match eval_delayed_expr(&range.node, &cur, range.span) {
                                Ok(MetaValue::List(items)) => {
                                    let mut result_stmts: Vec<Spanned<Stmt>> = Vec::new();
                                    let empty_type_env = FxHashMap::default();
                                    for item_val in items {
                                        let mut child_env = local_env.clone();
                                        if vars.len() == 1 {
                                            // Single variable — bind item directly
                                            child_env.insert(vars[0].node.clone(), item_val);
                                        } else {
                                            // Multi-variable — item must be a list; bind positionally
                                            if let MetaValue::List(parts) = item_val {
                                                for (var, part) in vars.iter().zip(parts.into_iter()) {
                                                    child_env.insert(var.node.clone(), part);
                                                }
                                            } else {
                                                errors.push(meta_err(
                                                    "meta for: multi-variable destructuring requires a list of lists \
                                                     (use fields(T) to get (name, type) pairs)",
                                                    range.span,
                                                ));
                                                break;
                                            }
                                        }
                                        let child_ctx = DelayedMetaContext { meta_env: &child_env, ..*ctx };
                                        let mut loop_body = body.clone();
                                        // Substitute loop-variable values into string interpolations and
                                        // identifier references BEFORE evaluating inner meta constructs.
                                        // This enables `print("{fname}:{ftype}")` and ensures that
                                        // `meta if ftype is numeric:` sees the resolved type string.
                                        substitute_block(&mut loop_body, &child_env, &empty_type_env);
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
                    let subject = eval_delayed_expr(&scrutinee.node, &cur, scrutinee.span);
                    match subject {
                        Ok(subject_val) => {
                            let mut taken = None;
                            for (case_expr, case_body) in arms.iter() {
                                match eval_delayed_expr(&case_expr.node, &cur, case_expr.span) {
                                    Ok(case_val) if meta_values_eq(&subject_val, &case_val) => {
                                        let mut body = case_body.clone();
                                        evaluate_delayed_meta_block(&mut body, &cur, errors);
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
                                evaluate_delayed_meta_block(&mut body, &cur, errors);
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
                        match eval_delayed_expr(&condition.node, &cur, condition.span) {
                            Ok(MetaValue::Bool(true)) => {
                                if iter_count >= MAX_META_ITERATIONS {
                                    errors.push(meta_err(
                                        "meta while exceeded iteration limit (100000)",
                                        *span,
                                    ));
                                    break;
                                }
                                let mut loop_body = body.clone();
                                evaluate_delayed_meta_block(&mut loop_body, &cur, errors);
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

                Stmt::MetaLog { args, .. } => {
                    // Evaluate and print each arg to stderr, then remove the stmt.
                    let parts: Vec<String> = args.iter().filter_map(|arg| {
                        match eval_delayed_expr(&arg.node, &cur, arg.span) {
                            Ok(v) => Some(meta_value_to_string(&v)),
                            Err(e) => { errors.push(e); None }
                        }
                    }).collect();
                    eprintln!("[meta] {}", parts.join(" "));
                    Some(vec![]) // Remove the stmt
                }

                _ => None, // Not a meta stmt — recurse into sub-blocks below
            }
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
            let cur = DelayedMetaContext { meta_env: &local_env, ..*ctx };
            recurse_delayed_meta_in_stmt(&mut block.stmts[i].node, &cur, errors);
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
        Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            evaluate_delayed_meta_block(body, ctx, errors);
        }
        Stmt::Match { arms, else_arm, .. } => {
            // First expand any MetaFor items into concrete arms
            expand_match_meta_for(arms, ctx, errors);
            // Then recurse into arm bodies
            for arm in arms.iter_mut().filter_map(|i| i.arm_mut()) {
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
        }, Vec::new());
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
                }, Vec::new()),
                dummy_span(),
            )),
            op: BinaryOp::Add,
            right: Box::new(Spanned::new(
                Expr::StringLiteral(StringLiteral {
                    kind: StringKind::Normal,
                    segments: vec![StringSegment::Literal("b".to_string())],
                }, Vec::new()),
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
                        type_: Spanned::new(Type::Primitive(PrimitiveType::StringType), dummy_span()),
                        name: Spanned::new("NAME".to_string(), dummy_span()),
                        value: Spanned::new(
                            Expr::StringLiteral(StringLiteral {
                                kind: StringKind::Normal,
                                segments: vec![StringSegment::Literal("world".to_string())],
                            }, Vec::new()),
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
                                                        segments: vec![StringSegment::Interpolation("NAME".to_string(), None)],
                                                    }, Vec::new()),
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
                        if let Expr::StringLiteral(s, _) = &args[0].node.value.node {
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
                            }, Vec::new()),
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
                        }, Vec::new()),
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
                        body: FunctionBody::Block(Block {
                            stmts: vec![Spanned::new(
                                Stmt::VarDecl {
                                    is_const: false,
                                    is_mutable: false,
                                    shared: SharedKind::None,
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
                                        body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                                param_abis: vec![],
                                extern_abi: None,
                                returns_borrowed: false,
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
                                        body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                                param_abis: vec![],
                                extern_abi: None,
                                returns_borrowed: false,
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
                                        body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                                param_abis: vec![],
                                extern_abi: None,
                                returns_borrowed: false,
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
                                        body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                                param_abis: vec![],
                                extern_abi: None,
                                returns_borrowed: false,
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
                                        body: FunctionBody::Block(Block { stmts: vec![], span: dummy_span() }),
                                doc_comment: None,
                                span: dummy_span(),
                                param_abis: vec![],
                                extern_abi: None,
                                returns_borrowed: false,
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
                        body: FunctionBody::Block(Block {
                            stmts: vec![Spanned::new(
                                Stmt::VarDecl {
                                    is_const: false,
                                    is_mutable: false,
                                    shared: SharedKind::None,
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
                    is_meta_op: false,
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
                        body: FunctionBody::Block(Block {
                            stmts: vec![Spanned::new(
                                Stmt::VarDecl {
                                    is_const: false,
                                    is_mutable: false,
                                    shared: SharedKind::None,
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
                        param_abis: vec![],
                        extern_abi: None,
                        returns_borrowed: false,
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
