//! Post-resolution rewrite pass.
//!
//! After name resolution builds the resolution map and struct_fields,
//! this pass walks the AST mutably and converts `Expr::Call` nodes whose
//! callee resolves to `DefKind::Struct` into `Expr::StructLiteral` nodes.
//! This activates the dead `StructLiteral` match arms across the compiler
//! and removes the need for struct-as-Call workarounds in the borrow checker.

use crate::lexer::token::StringSegment;
use crate::parser::ast::*;
use crate::parser::visitor::{walk_expr, ExprVisitor};
use crate::span::{Span, Spanned};

use super::errors::SemanticErrorKind;
use super::resolve::ResolutionMap;
use super::scope::{DefKind, ScopeTable};

/// D26 (Round XXXIII Batch C1) — pre-`collect_top_level` auto-infer of
/// `throws ArithError` for every fn whose body contains a fallible-arith op
/// (`+! -! *! /! %! <<! >>!`). Silent (owner ruling 2026-08-06).
///
/// Runs BEFORE `collect_top_level` so `FunctionInfo.throws_type_id` picks up
/// the auto-inferred throws, AND before the IR lowering (which reads
/// `func.throws.declares_throws()` directly at ten+ sites) — mutating the AST
/// makes both readers see the same signature the user could have written by
/// hand as `throws ArithError`.
///
/// Explicit `throws E` (any E) wins over auto-infer — the walk skips a fn
/// whose `throws` is `ThrowsSpec::Explicit(_)` unchanged, matching how the
/// D29 disposition table's `explicit-throws-wins` rule reads. `Inferred(!)`
/// remains as-is (A31's inferred-error-set spelling is a distinct feature).
///
/// Extern fns and Declarations have no body — skipped by construction.
/// Nested item bodies (`Stmt::Item`) are walked by the outer `walk_module_items`
/// recursion, so a nested fn with a fallible op also auto-infers.
pub fn rewrite_d26_auto_infer_throws(module: &mut Module) {
    for item in &mut module.items {
        walk_item_for_auto_infer(&mut item.node);
    }
}

fn walk_item_for_auto_infer(item: &mut Item) {
    match item {
        Item::Function(f) => {
            auto_infer_fn(f);
        }
        Item::Equip(e) => {
            for method in &mut e.items {
                auto_infer_fn(&mut method.node);
            }
        }
        Item::Module { items, .. } => {
            for it in items {
                walk_item_for_auto_infer(&mut it.node);
            }
        }
        _ => {}
    }
}

fn auto_infer_fn(f: &mut FunctionDef) {
    // Explicit `throws E` (any E) wins — user's declaration is untouched.
    if f.throws.explicit_type().is_some() {
        return;
    }
    // `main()` can only throw `int` (per D26 spec + E_MainThrowsNonInt);
    // auto-inferring `throws ArithError` on it would be immediately rejected
    // by the check-lane. Users must capture (`Result[int, ArithError] r = ...`)
    // or `catch`-handle every fallible op in `main` — never propagate. This
    // matches the D29 discipline: `main` is the top-level; there is no
    // caller to propagate to.
    if f.name.node == "main" {
        return;
    }
    if !body_contains_fallible_arith(&f.body) {
        return;
    }
    // Synthesize `throws ArithError` at the fn's return-type span so a
    // downstream diagnostic points somewhere non-crazy. This mutates the AST
    // — the checker (`current_function_throws` via `FunctionInfo`) and the
    // lowering (`func.throws.declares_throws()` direct reads) both see the
    // same signature after this pass runs.
    let synth_span = f.return_type.span;
    f.throws = ThrowsSpec::Explicit(Spanned {
        node: Type::Named {
            name: Spanned {
                node: "ArithError".to_string(),
                span: synth_span,
            },
            generic_args: Vec::new(),
        },
        span: synth_span,
    });
}

/// Body-walker helper: true iff any `Expr::BinaryOp` in the body has an
/// `is_fallible_arith()` op. Skips closures (a fallible op inside a closure
/// belongs to that closure's own signature — filed follow-up for closure
/// auto-infer). Skips nested item bodies (`Stmt::Item`) — those are walked
/// separately by `walk_item_for_auto_infer`.
fn body_contains_fallible_arith(body: &FunctionBody) -> bool {
    struct Scanner {
        found: bool,
    }
    impl ExprVisitor for Scanner {
        fn visit_expr(&mut self, expr: &Spanned<Expr>) {
            if self.found {
                return;
            }
            if let Expr::BinaryOp { op, .. } = &expr.node {
                if op.is_fallible_arith() {
                    self.found = true;
                    return;
                }
            }
            if matches!(&expr.node, Expr::Closure { .. } | Expr::ImplicitClosure { .. }) {
                return;
            }
            walk_expr(self, expr);
        }
    }
    let mut scanner = Scanner { found: false };
    match body {
        FunctionBody::Block(block) => {
            for stmt in &block.stmts {
                scanner.visit_stmt(stmt);
                if scanner.found {
                    return true;
                }
            }
        }
        FunctionBody::Expression(expr) => {
            scanner.visit_expr(expr);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
    scanner.found
}

/// Rewrite struct constructor calls to `Expr::StructLiteral`.
///
/// For each `Expr::Call` whose callee is an `Expr::Identifier` that resolves
/// to `DefKind::Struct`, replace the node with `Expr::StructLiteral`.
/// Enum variant calls (`DefKind::Variant`) are NOT rewritten.
pub fn rewrite_struct_calls(module: &mut Module, resolution_map: &ResolutionMap, scopes: &ScopeTable) -> Vec<(SemanticErrorKind, Span)> {
    let mut errors = Vec::new();
    for item in &mut module.items {
        rewrite_item(&mut item.node, resolution_map, scopes, &mut errors);
    }
    errors
}

/// Rewrite imported-alias names back to their source names throughout the
/// (entry-file portion of the) module AST.
///
/// For each `from X import Y as Z` in the entry file, every reference to
/// `Z` in identifier or path-head position is rewritten to `Y`. The IR
/// backend lowers calls and lookups by surface name; without this rewrite,
/// it would emit references to the local alias `Z` (which doesn't exist
/// at the C-symbol layer) instead of the real `Y`.
///
/// The walk skips `Item::Module` wrappers (i.e. imported modules) — only
/// the entry file's items see its aliases. Type-position rewrites also
/// apply (struct/enum aliases could be imported under another name).
pub fn rewrite_import_aliases(
    module: &mut Module,
    aliases: &rustc_hash::FxHashMap<String, String>,
) {
    if aliases.is_empty() {
        return;
    }
    for item in &mut module.items {
        if matches!(&item.node, Item::Module { .. }) {
            // Imported modules use their own (unaliased) names.
            continue;
        }
        rename_item(&mut item.node, aliases);
    }
}

fn rename_item(item: &mut Item, aliases: &rustc_hash::FxHashMap<String, String>) {
    match item {
        Item::Function(f) => rename_function(f, aliases),
        Item::Equip(eq) => {
            rename_type(&mut eq.type_.node, aliases);
            if let Some(et) = &mut eq.trait_ {
                rename_type(&mut et.trait_name.node, aliases);
            }
            for method in &mut eq.items {
                rename_function(&mut method.node, aliases);
            }
        }
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                if let TraitItem::Method(f) = &mut trait_item.node {
                    rename_function(f, aliases);
                }
            }
        }
        Item::Struct(s) => {
            for field in &mut s.fields {
                rename_type(&mut field.node.type_.node, aliases);
            }
        }
        Item::Enum(e) => {
            for variant in &mut e.variants {
                if let VariantFields::Tuple(fields) = &mut variant.node.fields {
                    for field in fields {
                        rename_type(&mut field.node, aliases);
                    }
                }
            }
        }
        Item::ConstDecl(c) => {
            rename_type(&mut c.type_.node, aliases);
            rename_expr(&mut c.value, aliases);
        }
        Item::StaticDecl(s) => {
            rename_type(&mut s.type_.node, aliases);
            rename_expr(&mut s.value, aliases);
        }
        Item::TypeAlias(ta) => {
            rename_type(&mut ta.type_.node, aliases);
        }
        Item::Newtype(n) => {
            rename_type(&mut n.inner_type.node, aliases);
        }
        Item::Test(t) => rename_block(&mut t.body, aliases),
        Item::Bench(b) => rename_block(&mut b.body, aliases),
        Item::SuiteSetup(s) => rename_block(&mut s.body, aliases),
        Item::SuiteTeardown(s) => rename_block(&mut s.body, aliases),
        Item::ExternBlock(eb) => {
            for f in &mut eb.items {
                rename_function(&mut f.node, aliases);
            }
        }
        Item::Module { .. }
        | Item::Import(_)
        | Item::Directive(_)
        | Item::MetaConst(_)
        | Item::MetaType(_)
        | Item::MetaTypeFunc(_)
        | Item::MetaAssert(_)
        | Item::MetaIf(_)
        | Item::MetaLog(_) => {}
    }
}

fn rename_function(f: &mut FunctionDef, aliases: &rustc_hash::FxHashMap<String, String>) {
    rename_type(&mut f.return_type.node, aliases);
    for p in &mut f.params {
        rename_type(&mut p.node.type_.node, aliases);
        if let Some(default) = &mut p.node.default {
            rename_expr(default, aliases);
        }
    }
    if let Some(throws) = f.throws.explicit_type_mut() {
        rename_type(&mut throws.node, aliases);
    }
    match &mut f.body {
        FunctionBody::Block(b) => rename_block(b, aliases),
        FunctionBody::Expression(e) => rename_expr(e, aliases),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn rename_block(block: &mut Block, aliases: &rustc_hash::FxHashMap<String, String>) {
    for stmt in &mut block.stmts {
        rename_stmt(&mut stmt.node, aliases);
    }
}

fn rename_stmt(stmt: &mut Stmt, aliases: &rustc_hash::FxHashMap<String, String>) {
    match stmt {
        Stmt::Expr(e) => rename_expr(e, aliases),
        Stmt::VarDecl { type_, value, .. } => {
            rename_type(&mut type_.node, aliases);
            rename_expr(value, aliases);
        }
        Stmt::Return(opt) => {
            if let Some(v) = opt {
                rename_expr(v, aliases);
            }
        }
        Stmt::Throw(e) => rename_expr(e, aliases),
        Stmt::Break | Stmt::Continue | Stmt::Pass => {}
        Stmt::Assign { target, value } => {
            rename_expr(target, aliases);
            rename_expr(value, aliases);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            rename_expr(target, aliases);
            rename_expr(value, aliases);
        }
        Stmt::While { condition, body, else_body } => {
            rename_expr(condition, aliases);
            rename_block(body, aliases);
            if let Some(e) = else_body { rename_block(e, aliases); }
        }
        Stmt::Loop { body } => rename_block(body, aliases),
        Stmt::For { iterable, body, else_body, .. } => {
            rename_expr(iterable, aliases);
            rename_block(body, aliases);
            if let Some(e) = else_body { rename_block(e, aliases); }
        }
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            rename_expr(condition, aliases);
            rename_block(then_body, aliases);
            for (c, b) in elif_branches {
                rename_expr(c, aliases);
                rename_block(b, aliases);
            }
            if let Some(e) = else_body { rename_block(e, aliases); }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            rename_expr(scrutinee, aliases);
            for arm in arms {
                rename_match_item(arm, aliases);
            }
            if let Some(e) = else_arm { rename_block(e, aliases); }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                rename_select_arm(arm, aliases);
            }
            if let Some(e) = else_arm { rename_block(e, aliases); }
        }
        Stmt::With { bindings, body } => {
            for b in bindings {
                rename_expr(&mut b.expr, aliases);
            }
            rename_block(body, aliases);
        }
        Stmt::OnError { body } | Stmt::NamedScope { body, .. } => {
            rename_block(body, aliases);
        }
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            rename_expr(condition, aliases);
            if let Some(m) = message { rename_expr(m, aliases); }
        }
        Stmt::Snapshot { value, .. } => rename_expr(value, aliases),
        Stmt::Item(inner) => rename_item(inner, aliases),
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            rename_expr(condition, aliases);
            rename_block(then_body, aliases);
            for (c, b) in elif_branches {
                rename_expr(c, aliases);
                rename_block(b, aliases);
            }
            if let Some(e) = else_body { rename_block(e, aliases); }
        }
        Stmt::MetaFor { range, body, .. } => {
            rename_expr(range, aliases);
            rename_block(body, aliases);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            rename_expr(scrutinee, aliases);
            for (case_expr, body) in arms {
                rename_expr(case_expr, aliases);
                rename_block(body, aliases);
            }
            if let Some(e) = else_arm { rename_block(e, aliases); }
        }
        Stmt::MetaWhile { condition, body, .. } => {
            rename_expr(condition, aliases);
            rename_block(body, aliases);
        }
        Stmt::MetaConst { value, .. } => rename_expr(value, aliases),
        Stmt::MetaLog { args, .. } => {
            for a in args { rename_expr(a, aliases); }
        }
    }
}

fn rename_match_item(item: &mut MatchItem, aliases: &rustc_hash::FxHashMap<String, String>) {
    match item {
        MatchItem::Arm(arm) => rename_match_arm(arm, aliases),
        MatchItem::MetaFor { range, arm_template, .. } => {
            rename_expr(range, aliases);
            rename_match_arm(arm_template, aliases);
        }
    }
}

fn rename_match_arm(arm: &mut MatchArm, aliases: &rustc_hash::FxHashMap<String, String>) {
    if let Some(g) = &mut arm.guard {
        rename_expr(g, aliases);
    }
    rename_expr(&mut arm.body, aliases);
}

fn rename_select_arm(arm: &mut SelectArm, aliases: &rustc_hash::FxHashMap<String, String>) {
    rename_select_op(&mut arm.op, aliases);
    rename_block(&mut arm.body, aliases);
}

fn rename_select_op(op: &mut SelectOp, aliases: &rustc_hash::FxHashMap<String, String>) {
    match op {
        SelectOp::Recv { channel, type_, .. } => {
            rename_type(&mut type_.node, aliases);
            rename_expr(channel, aliases);
        }
        SelectOp::Send { channel, value, .. } => {
            rename_expr(channel, aliases);
            rename_expr(value, aliases);
        }
    }
}

fn rename_expr(expr: &mut Spanned<Expr>, aliases: &rustc_hash::FxHashMap<String, String>) {
    match &mut expr.node {
        Expr::Identifier(name) => {
            if let Some(src) = aliases.get(name) {
                *name = src.clone();
            }
        }
        Expr::Path { segments } => {
            // Only the head segment refers to a top-level binding. Subsequent
            // segments are field/variant names within the head's type and
            // must not be rewritten.
            if let Some(first) = segments.first_mut() {
                if let Some(src) = aliases.get(&first.node) {
                    first.node = src.clone();
                }
            }
        }
        Expr::UnaryOp { operand, .. } => rename_expr(operand, aliases),
        Expr::BinaryOp { left, right, .. } => {
            rename_expr(left, aliases);
            rename_expr(right, aliases);
        }
        Expr::Call { callee, args, generic_args } => {
            rename_expr(callee, aliases);
            if let Some(gs) = generic_args {
                for g in gs {
                    rename_type(&mut g.node, aliases);
                }
            }
            for arg in args {
                rename_expr(&mut arg.node.value, aliases);
            }
        }
        Expr::MethodCall { receiver, args, generic_args, .. } => {
            rename_expr(receiver, aliases);
            if let Some(gs) = generic_args {
                for g in gs {
                    rename_type(&mut g.node, aliases);
                }
            }
            for arg in args {
                rename_expr(&mut arg.node.value, aliases);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => rename_expr(object, aliases),
        Expr::Index { object, index } => {
            rename_expr(object, aliases);
            rename_expr(index, aliases);
        }
        Expr::DefaultOp { lhs, rhs } => {
            rename_expr(lhs, aliases);
            rename_expr(rhs, aliases);
        }
        Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner } | Expr::Await { expr: inner, .. }
        | Expr::Spawn { expr: inner, .. } | Expr::SpawnBlocking { expr: inner, .. } => {
            rename_expr(inner, aliases);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            rename_expr(condition, aliases);
            rename_expr(then_branch, aliases);
            for (c, b) in elif_branches {
                rename_expr(c, aliases);
                rename_expr(b, aliases);
            }
            if let Some(eb) = else_branch {
                rename_expr(eb, aliases);
            }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            rename_expr(scrutinee, aliases);
            for arm in arms {
                if let Some(g) = &mut arm.guard {
                    rename_expr(g, aliases);
                }
                rename_expr(&mut arm.body, aliases);
            }
            if let Some(ea) = else_arm {
                rename_expr(ea, aliases);
            }
        }
        Expr::Block(b) | Expr::Do { body: b, .. } => rename_block(b, aliases),
        Expr::Closure { body, .. } => {
            // Closure params carry an optional type annotation via ClosureParam,
            // but those types are name-resolved by the resolver, not by us.
            // We could walk them, but they're internal to the closure's scope.
            // Body is the load-bearing path for renaming.
            rename_expr(body, aliases);
        }
        Expr::ImplicitClosure { body } => rename_expr(body, aliases),
        Expr::ListComprehension { expr: e, iterable, condition, .. } => {
            rename_expr(e, aliases);
            rename_expr(iterable, aliases);
            if let Some(c) = condition { rename_expr(c, aliases); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            rename_expr(key, aliases);
            rename_expr(value, aliases);
            rename_expr(iterable, aliases);
            if let Some(c) = condition { rename_expr(c, aliases); }
        }
        Expr::SetComprehension { expr: e, iterable, condition, .. } => {
            rename_expr(e, aliases);
            rename_expr(iterable, aliases);
            if let Some(c) = condition { rename_expr(c, aliases); }
        }
        Expr::ArrayLiteral(elems, _) | Expr::TupleLiteral(elems) => {
            for e in elems { rename_expr(e, aliases); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                rename_expr(k, aliases);
                rename_expr(v, aliases);
            }
        }
        Expr::StructLiteral { args, generic_args, name } => {
            // The struct name itself can be an aliased import.
            if let Some(src) = aliases.get(&name.node) {
                name.node = src.clone();
            }
            if let Some(gs) = generic_args {
                for g in gs { rename_type(&mut g.node, aliases); }
            }
            for a in args { rename_expr(a, aliases); }
        }
        Expr::As { expr: inner, type_ } => {
            rename_expr(inner, aliases);
            rename_type(&mut type_.node, aliases);
        }
        Expr::Is { expr: inner, .. } => rename_expr(inner, aliases),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { rename_expr(s, aliases); }
            if let Some(e) = end { rename_expr(e, aliases); }
        }
        Expr::DotShorthand { args, .. } => {
            for arg in args.iter_mut() {
                rename_expr(&mut arg.node.value, aliases);
            }
        }
        Expr::MetaOpInfix { left, right, .. } => {
            rename_expr(left, aliases);
            rename_expr(right, aliases);
        }
        Expr::Rethrow { expr, transform, .. } => {
            rename_expr(expr, aliases);
            rename_expr(transform, aliases);
        }
        Expr::Catch { expr, recovery, .. } => {
            rename_expr(expr, aliases);
            rename_expr(recovery, aliases);
        }
        Expr::StringLiteral(_, interp_exprs) => {
            for e in interp_exprs {
                rename_expr(e, aliases);
            }
        }
        Expr::MetaOpToken(_) | Expr::IntLiteral(_) | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_) | Expr::NoneLiteral | Expr::SelfExpr
        | Expr::ReturnValue | Expr::It => {}
    }
}

fn rename_type(ty: &mut Type, aliases: &rustc_hash::FxHashMap<String, String>) {
    match ty {
        Type::Named { name, generic_args } => {
            if let Some(src) = aliases.get(&name.node) {
                name.node = src.clone();
            }
            for arg in generic_args {
                rename_type(&mut arg.node, aliases);
            }
        }
        Type::Tuple(elems) => {
            for e in elems {
                rename_type(&mut e.node, aliases);
            }
        }
        Type::Array { element, .. } | Type::Slice { element } => {
            rename_type(&mut element.node, aliases);
        }
        Type::Function { params, return_type, .. } => {
            rename_type(&mut return_type.node, aliases);
            for p in params {
                rename_type(&mut p.node, aliases);
            }
        }
        Type::Ref(inner) | Type::Owned(inner) => {
            rename_type(&mut inner.node, aliases);
        }
        _ => {}
    }
}

fn rewrite_item(item: &mut Item, res: &ResolutionMap, scopes: &ScopeTable, errors: &mut Vec<(SemanticErrorKind, Span)>) {
    match item {
        Item::Function(f) => rewrite_function(f, res, scopes, errors),
        Item::Equip(eq) => {
            for method in &mut eq.items {
                rewrite_function(&mut method.node, res, scopes, errors);
            }
        }
        Item::ConstDecl(c) => rewrite_expr(&mut c.value, res, scopes, errors),
        Item::StaticDecl(s) => rewrite_expr(&mut s.value, res, scopes, errors),
        Item::Test(t) => {
            rewrite_block(&mut t.body, res, scopes, errors);
        }
        Item::Bench(b) => {
            rewrite_block(&mut b.body, res, scopes, errors);
        }
        Item::SuiteSetup(s) => rewrite_block(&mut s.body, res, scopes, errors),
        Item::SuiteTeardown(s) => rewrite_block(&mut s.body, res, scopes, errors),
        Item::Trait(t) => {
            for trait_item in &mut t.items {
                if let TraitItem::Method(f) = &mut trait_item.node {
                    rewrite_function(f, res, scopes, errors);
                }
            }
        }
        Item::Struct(_) | Item::Enum(_) | Item::Import(_)
        | Item::TypeAlias(_) | Item::Newtype(_) | Item::ExternBlock(_)
        | Item::Directive(_) | Item::MetaConst(_) | Item::MetaType(_)
        | Item::MetaTypeFunc(_) | Item::MetaAssert(_) | Item::MetaIf(_) | Item::MetaLog(_) => {}
        Item::Module { items, .. } => {
            for si in items {
                rewrite_item(&mut si.node, res, scopes, errors);
            }
        }
    }
}

fn rewrite_function(f: &mut FunctionDef, res: &ResolutionMap, scopes: &ScopeTable, errors: &mut Vec<(SemanticErrorKind, Span)>) {
    // Rewrite default parameter expressions
    for param in &mut f.params {
        if let Some(default) = &mut param.node.default {
            rewrite_expr(default, res, scopes, errors);
        }
    }
    match &mut f.body {
        FunctionBody::Block(block) => rewrite_block(block, res, scopes, errors),
        FunctionBody::Expression(expr) => rewrite_expr(expr, res, scopes, errors),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn rewrite_block(block: &mut Block, res: &ResolutionMap, scopes: &ScopeTable, errors: &mut Vec<(SemanticErrorKind, Span)>) {
    for stmt in &mut block.stmts {
        rewrite_stmt(&mut stmt.node, res, scopes, errors);
    }
}

fn rewrite_stmt(stmt: &mut Stmt, res: &ResolutionMap, scopes: &ScopeTable, errors: &mut Vec<(SemanticErrorKind, Span)>) {
    match stmt {
        Stmt::VarDecl { value, .. } => rewrite_expr(value, res, scopes, errors),
        Stmt::Expr(expr) => {
            rewrite_expr(expr, res, scopes, errors);
            // Rewrite field_set(obj, "field", value) → obj.field = value
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
            rewrite_expr(target, res, scopes, errors);
            rewrite_expr(value, res, scopes, errors);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target, res, scopes, errors);
            rewrite_expr(value, res, scopes, errors);
        }
        Stmt::Return(Some(expr)) => rewrite_expr(expr, res, scopes, errors),
        Stmt::Throw(expr) => rewrite_expr(expr, res, scopes, errors),
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::Pass => {}
        Stmt::For { iterable, body, else_body, .. } => {
            rewrite_expr(iterable, res, scopes, errors);
            rewrite_block(body, res, scopes, errors);
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes, errors); }
        }
        Stmt::While { condition, body, else_body } => {
            rewrite_expr(condition, res, scopes, errors);
            rewrite_block(body, res, scopes, errors);
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes, errors); }
        }
        Stmt::Loop { body } => rewrite_block(body, res, scopes, errors),
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            rewrite_expr(condition, res, scopes, errors);
            rewrite_block(then_body, res, scopes, errors);
            for (cond, body) in elif_branches {
                rewrite_expr(cond, res, scopes, errors);
                rewrite_block(body, res, scopes, errors);
            }
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes, errors); }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            rewrite_expr(scrutinee, res, scopes, errors);
            for arm in arms.iter_mut().filter_map(|i| i.arm_mut()) {
                if let Some(guard) = &mut arm.guard { rewrite_expr(guard, res, scopes, errors); }
                rewrite_expr(&mut arm.body, res, scopes, errors);
            }
            if let Some(ea) = else_arm { rewrite_block(ea, res, scopes, errors); }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                match &mut arm.op {
                    SelectOp::Recv { channel, .. } => rewrite_expr(channel, res, scopes, errors),
                    SelectOp::Send { channel, value } => {
                        rewrite_expr(channel, res, scopes, errors);
                        rewrite_expr(value, res, scopes, errors);
                    }
                }
                rewrite_block(&mut arm.body, res, scopes, errors);
            }
            if let Some(ea) = else_arm { rewrite_block(ea, res, scopes, errors); }
        }
        Stmt::With { bindings, body } => {
            for binding in bindings {
                rewrite_expr(&mut binding.expr, res, scopes, errors);
            }
            rewrite_block(body, res, scopes, errors);
        }
        Stmt::NamedScope { body, .. } => rewrite_block(body, res, scopes, errors),
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            rewrite_expr(condition, res, scopes, errors);
            if let Some(msg) = message { rewrite_expr(msg, res, scopes, errors); }
        }
        Stmt::Snapshot { value, .. } => {
            rewrite_expr(value, res, scopes, errors);
        }
        Stmt::Item(item) => rewrite_item(item, res, scopes, errors),
        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            // Conditions are meta expressions — skip rewriting them; rewrite the bodies.
            rewrite_block(then_body, res, scopes, errors);
            for (_, body) in elif_branches {
                rewrite_block(body, res, scopes, errors);
            }
            if let Some(eb) = else_body { rewrite_block(eb, res, scopes, errors); }
        }
        Stmt::MetaFor { body, .. } => {
            // Range is a meta expression — skip; rewrite the body.
            rewrite_block(body, res, scopes, errors);
        }
        Stmt::MetaMatch { arms, else_arm, .. } => {
            // Scrutinee and case exprs are meta expressions — skip; rewrite bodies only.
            for (_, body) in arms {
                rewrite_block(body, res, scopes, errors);
            }
            if let Some(eb) = else_arm { rewrite_block(eb, res, scopes, errors); }
        }
        Stmt::MetaWhile { body, .. } => {
            // Condition is a meta expression — skip; rewrite the body.
            rewrite_block(body, res, scopes, errors);
        }

        Stmt::MetaConst { .. } => {
            // Entirely a meta expression — evaluated at monomorphization time; skip.
        }

        Stmt::MetaLog { .. } => {
            // Compile-time diagnostic — removed before GIR lowering; skip.
        }
        Stmt::OnError { body } => {
            rewrite_block(body, res, scopes, errors);
        }
    }
}

fn rewrite_expr(expr: &mut Spanned<Expr>, res: &ResolutionMap, scopes: &ScopeTable, errors: &mut Vec<(SemanticErrorKind, Span)>) {
    // First, recurse into sub-expressions
    match &mut expr.node {
        Expr::UnaryOp { operand, .. } => rewrite_expr(operand, res, scopes, errors),
        Expr::BinaryOp { left, right, .. } => {
            rewrite_expr(left, res, scopes, errors);
            rewrite_expr(right, res, scopes, errors);
        }
        Expr::Call { callee, args, .. } => {
            rewrite_expr(callee, res, scopes, errors);
            for arg in args {
                rewrite_expr(&mut arg.node.value, res, scopes, errors);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            rewrite_expr(receiver, res, scopes, errors);
            for arg in args {
                rewrite_expr(&mut arg.node.value, res, scopes, errors);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. }
        | Expr::OptionalChain { object, .. } => {
            rewrite_expr(object, res, scopes, errors);
        }
        Expr::Index { object, index } => {
            rewrite_expr(object, res, scopes, errors);
            rewrite_expr(index, res, scopes, errors);
        }
        Expr::DefaultOp { lhs, rhs } => {
            rewrite_expr(lhs, res, scopes, errors);
            rewrite_expr(rhs, res, scopes, errors);
        }
        Expr::Move { expr: inner }
        | Expr::Propagate { expr: inner }
        | Expr::MutableBorrow { expr: inner } | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner, .. } | Expr::Spawn { expr: inner, .. }
        | Expr::SpawnBlocking { expr: inner, .. } => {
            rewrite_expr(inner, res, scopes, errors);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            rewrite_expr(condition, res, scopes, errors);
            rewrite_expr(then_branch, res, scopes, errors);
            for (cond, body) in elif_branches {
                rewrite_expr(cond, res, scopes, errors);
                rewrite_expr(body, res, scopes, errors);
            }
            if let Some(eb) = else_branch { rewrite_expr(eb, res, scopes, errors); }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            rewrite_expr(scrutinee, res, scopes, errors);
            for arm in arms {
                if let Some(guard) = &mut arm.guard { rewrite_expr(guard, res, scopes, errors); }
                rewrite_expr(&mut arm.body, res, scopes, errors);
            }
            if let Some(ea) = else_arm { rewrite_expr(ea, res, scopes, errors); }
        }
        Expr::Block(block) | Expr::Do { body: block, .. } => {
            rewrite_block(block, res, scopes, errors);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            rewrite_expr(body, res, scopes, errors);
        }
        Expr::ListComprehension { expr: comp_expr, iterable, condition, .. } => {
            rewrite_expr(comp_expr, res, scopes, errors);
            rewrite_expr(iterable, res, scopes, errors);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes, errors); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            rewrite_expr(key, res, scopes, errors);
            rewrite_expr(value, res, scopes, errors);
            rewrite_expr(iterable, res, scopes, errors);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes, errors); }
        }
        Expr::SetComprehension { expr: comp_expr, iterable, condition, .. } => {
            rewrite_expr(comp_expr, res, scopes, errors);
            rewrite_expr(iterable, res, scopes, errors);
            if let Some(cond) = condition { rewrite_expr(cond, res, scopes, errors); }
        }
        Expr::ArrayLiteral(elems, _) | Expr::TupleLiteral(elems) => {
            for elem in elems { rewrite_expr(elem, res, scopes, errors); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                rewrite_expr(k, res, scopes, errors);
                rewrite_expr(v, res, scopes, errors);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for arg in args { rewrite_expr(arg, res, scopes, errors); }
        }
        Expr::As { expr: inner, .. } | Expr::Is { expr: inner, .. } => {
            rewrite_expr(inner, res, scopes, errors);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { rewrite_expr(s, res, scopes, errors); }
            if let Some(e) = end { rewrite_expr(e, res, scopes, errors); }
        }
        // Dot-shorthand: recurse into args
        Expr::DotShorthand { args, .. } => {
            for arg in args.iter_mut() {
                rewrite_expr(&mut arg.node.value, res, scopes, errors);
            }
        }
        Expr::MetaOpInfix { left, right, .. } => {
            rewrite_expr(left, res, scopes, errors);
            rewrite_expr(right, res, scopes, errors);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, transform, .. } => {
            rewrite_expr(expr, res, scopes, errors);
            rewrite_expr(transform, res, scopes, errors);
        }
        Expr::Catch { expr, recovery, .. } => {
            rewrite_expr(expr, res, scopes, errors);
            rewrite_expr(recovery, res, scopes, errors);
        }
        // Leaf nodes
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
        | Expr::StringLiteral(_, _) | Expr::NoneLiteral
        | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. }
        | Expr::ReturnValue | Expr::It => {}
    }

    // Rewrite field_value(val, "field") → val.field (for literal usage outside meta for)
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
                            return;
                        }
                    }
                }
            }
        }
    }

    // Rewrite make_variant(T, "Variant") → Expr::Path ["T", "Variant"]
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
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    // Now check if this expression is a Call that should become a StructLiteral.
    // Skip collection types — they have special C constructors (gorget_array_new,
    // GorgetDict__new, etc.) that don't use compound literal syntax.
    const COLLECTION_TYPES: &[&str] = &[
        "Vector", "Deque", "Dict", "HashMap", "Set", "HashSet", "Box", "Channel", "Arena", "TrackingAllocator", "PoolAllocator", "TlsfAllocator",
        "FixedBufferAllocator", "FallbackAllocator",
    ];
    if let Expr::Call { callee, .. } = &expr.node {
        if let Expr::Identifier(cname) = &callee.node {
            if COLLECTION_TYPES.contains(&cname.as_str()) {
                return;
            }
            let callee_span_start = callee.span.start;
            // Prefer the resolution-map entry (respects shadowing /
            // scope-specific names). Fall back to a name lookup when the
            // resolver didn't record an entry — happens for identifiers
            // inside trait default-method bodies, which the resolver
            // currently doesn't walk. Without this fallback, a struct
            // constructor like `TakeIter[Self, T](self, n)` inside an
            // `Iterator[T]` default body stays an `Expr::Call` and lowers
            // as a function call to `TakeIter__...` (undefined at link
            // time) instead of a struct literal that emits field-by-field.
            let def_id_opt = res.get(&callee_span_start)
                .copied()
                .or_else(|| scopes.lookup(cname));
            if let Some(def_id) = def_id_opt {
                let def = scopes.get_def(def_id);
                // Verify the definition name matches the callee name.
                // This prevents span collisions from derive-generated code
                // (which has overlapping spans) from causing false rewrites.
                if def.kind == DefKind::Struct && def.name == *cname {
                    // Extract fields from the Call and build StructLiteral
                    let call = std::mem::replace(&mut expr.node, Expr::NoneLiteral);
                    if let Expr::Call { callee, generic_args, args } = call {
                        // Check for duplicate named fields before stripping names
                        let mut seen_names = rustc_hash::FxHashSet::default();
                        for arg in &args {
                            if let Some(ref name) = arg.node.name {
                                if !seen_names.insert(name.node.clone()) {
                                    errors.push((
                                        SemanticErrorKind::DuplicateStructField {
                                            field: name.node.clone(),
                                        },
                                        name.span,
                                    ));
                                }
                            }
                        }

                        let name_str = if let Expr::Identifier(n) = callee.node {
                            n
                        } else {
                            unreachable!()
                        };
                        // CallArg.ownership carries `!arg` / `&arg` info that
                        // disappears when we drop to bare `Spanned<Expr>` for
                        // StructLiteral. Preserve `!` by wrapping the value
                        // in `Expr::Move` so downstream lowering (struct field
                        // ownership boundary) can see the explicit move
                        // intent and skip the implicit clone.
                        let bare_args: Vec<Spanned<Expr>> = args.into_iter()
                            .map(|a| {
                                let span = a.span;
                                let value = a.node.value;
                                match a.node.ownership {
                                    crate::parser::ast::Ownership::Move => {
                                        Spanned::new(Expr::Move { expr: Box::new(value) }, span)
                                    }
                                    _ => value,
                                }
                            })
                            .collect();
                        expr.node = Expr::StructLiteral {
                            name: Spanned::new(name_str, callee.span),
                            generic_args,
                            args: bare_args,
                        };
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;
    use rustc_hash::FxHashMap;

    fn dummy_span() -> Span {
        Span { start: 0, end: 1 }
    }

    #[test]
    fn rewrite_converts_struct_call_to_struct_literal() {
        // Build a minimal module with a single expression statement: MyStruct(42)
        let callee_span = Span { start: 100, end: 108 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("MyStruct".to_string()), callee_span)),
                generic_args: None,
                args: vec![Spanned::new(
                    CallArg {
                        name: None,
                        ownership: Ownership::Borrow,
                        value: Spanned::new(Expr::IntLiteral(42), dummy_span()),
                    },
                    dummy_span(),
                )],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    explicit_visibility: false,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: ThrowsSpec::No,
                    body: FunctionBody::Block(Block::synthetic(
                        vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        dummy_span(),
                    )),
                    doc_comment: None,
                    span: dummy_span(),
                    param_abis: vec![],
                    extern_abi: None,
                    returns_borrowed: false,
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        // Set up scope table with MyStruct as a Struct def
        let mut scopes = ScopeTable::new();
        let struct_def_id = scopes.define("MyStruct".to_string(), DefKind::Struct, callee_span).unwrap();
        let _ = struct_def_id;

        // Set up resolution map: callee span start → struct def id
        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, struct_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify the expression was rewritten
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    match &expr.node {
                        Expr::StructLiteral { name, generic_args, args } => {
                            assert_eq!(name.node, "MyStruct");
                            assert!(generic_args.is_none());
                            assert_eq!(args.len(), 1);
                            assert!(matches!(args[0].node, Expr::IntLiteral(42)));
                        }
                        other => panic!("Expected StructLiteral, got: {:?}", other),
                    }
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }

    #[test]
    fn rewrite_does_not_convert_variant_call() {
        let callee_span = Span { start: 200, end: 204 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("Some".to_string()), callee_span)),
                generic_args: None,
                args: vec![Spanned::new(
                    CallArg {
                        name: None,
                        ownership: Ownership::Borrow,
                        value: Spanned::new(Expr::IntLiteral(1), dummy_span()),
                    },
                    dummy_span(),
                )],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    explicit_visibility: false,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: ThrowsSpec::No,
                    body: FunctionBody::Block(Block::synthetic(
                        vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        dummy_span(),
                    )),
                    doc_comment: None,
                    span: dummy_span(),
                    param_abis: vec![],
                    extern_abi: None,
                    returns_borrowed: false,
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        let mut scopes = ScopeTable::new();
        let variant_def_id = scopes.define("Some".to_string(), DefKind::Variant, callee_span).unwrap();

        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, variant_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify it was NOT rewritten (should still be Call)
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    assert!(matches!(expr.node, Expr::Call { .. }), "Expected Call (not rewritten), got: {:?}", expr.node);
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }

    #[test]
    fn rewrite_preserves_generic_args() {
        let callee_span = Span { start: 300, end: 304 };
        let call_expr = Spanned::new(
            Expr::Call {
                callee: Box::new(Spanned::new(Expr::Identifier("Pair".to_string()), callee_span)),
                generic_args: Some(vec![
                    Spanned::new(Type::Primitive(PrimitiveType::Int), dummy_span()),
                    Spanned::new(Type::Primitive(PrimitiveType::StringType), dummy_span()),
                ]),
                args: vec![
                    Spanned::new(
                        CallArg {
                            name: None,
                            ownership: Ownership::Borrow,
                            value: Spanned::new(Expr::IntLiteral(10), dummy_span()),
                        },
                        dummy_span(),
                    ),
                    Spanned::new(
                        CallArg {
                            name: None,
                            ownership: Ownership::Borrow,
                            value: Spanned::new(Expr::StringLiteral(
                                crate::lexer::token::StringLiteral {
                                kind: crate::lexer::token::StringKind::Normal,
                                segments: vec![crate::lexer::token::StringSegment::Literal("hi".to_string())],
                            },
                            Vec::new(),
                            ), dummy_span()),
                        },
                        dummy_span(),
                    ),
                ],
            },
            dummy_span(),
        );

        let mut module = Module {
            items: vec![Spanned::new(
                Item::Function(FunctionDef {
                    attributes: vec![],
                    visibility: Visibility::Private,
                    explicit_visibility: false,
                    qualifiers: FunctionQualifiers::default(),
                    return_type: Spanned::new(Type::Primitive(PrimitiveType::Void), dummy_span()),
                    name: Spanned::new("test_fn".to_string(), dummy_span()),
                    generic_params: None,
                    params: vec![],
                    throws: ThrowsSpec::No,
                    body: FunctionBody::Block(Block::synthetic(
                        vec![Spanned::new(Stmt::Expr(call_expr), dummy_span())],
                        dummy_span(),
                    )),
                    doc_comment: None,
                    span: dummy_span(),
                    param_abis: vec![],
                    extern_abi: None,
                    returns_borrowed: false,
                }),
                dummy_span(),
            )],
            span: dummy_span(),
        };

        let mut scopes = ScopeTable::new();
        let struct_def_id = scopes.define("Pair".to_string(), DefKind::Struct, callee_span).unwrap();

        let mut resolution_map: ResolutionMap = FxHashMap::default();
        resolution_map.insert(callee_span.start, struct_def_id);

        rewrite_struct_calls(&mut module, &resolution_map, &scopes);

        // Verify rewritten with generic_args preserved
        if let Item::Function(f) = &module.items[0].node {
            if let FunctionBody::Block(block) = &f.body {
                if let Stmt::Expr(expr) = &block.stmts[0].node {
                    match &expr.node {
                        Expr::StructLiteral { name, generic_args, args } => {
                            assert_eq!(name.node, "Pair");
                            assert!(generic_args.is_some());
                            assert_eq!(generic_args.as_ref().unwrap().len(), 2);
                            assert_eq!(args.len(), 2);
                        }
                        other => panic!("Expected StructLiteral, got: {:?}", other),
                    }
                    return;
                }
            }
        }
        panic!("Test structure unexpected");
    }
}
