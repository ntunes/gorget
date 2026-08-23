use rustc_hash::FxHashMap;

use crate::parser::ast::*;
use crate::parser::visitor::visit_expr_children;
use crate::span::{Span, Spanned};

use crate::semantic::errors::{SemanticError, SemanticErrorKind};
use crate::semantic::ids::DefId;
use crate::semantic::resolve::ResolutionMap;
use crate::semantic::scope::{DefKind, ScopeTable};

use super::type_utils::all_spanned_items;

// ─── Private-in-Public Detection ──────────────────────────

/// Builtin/prelude type names that are always considered public.
const BUILTIN_TYPE_NAMES: &[&str] = &[
    "int", "int8", "int16", "int32", "int64",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "float", "float32", "float64", "bool", "str", "String", "char",
    "void", "Option", "Result", "Vector", "Dict", "Set", "HashSet", "HashMap",
    "Channel", "Shared", "Weak", "Mutex", "Guard", "Future", "TaskGroup",
    "Arena", "Slice", "Box",
];

/// Collect visibility of user-defined types from the AST.
fn collect_type_visibility(items: &[Spanned<Item>]) -> FxHashMap<String, Visibility> {
    let mut vis_map = FxHashMap::default();
    for item in all_spanned_items(items) {
        match &item.node {
            Item::Struct(s) => {
                vis_map.insert(s.name.node.clone(), s.visibility);
            }
            Item::Enum(e) => {
                vis_map.insert(e.name.node.clone(), e.visibility);
            }
            Item::Trait(t) => {
                vis_map.insert(t.name.node.clone(), t.visibility);
            }
            _ => {}
        }
    }
    vis_map
}

/// Check if a type name refers to a private user-defined type.
/// Returns the names of private types found.
fn check_type_visibility(ty: &Type, type_vis: &FxHashMap<String, Visibility>) -> Vec<String> {
    let mut private_types = Vec::new();
    match ty {
        Type::Named { name, generic_args } => {
            if !BUILTIN_TYPE_NAMES.contains(&name.node.as_str()) {
                if let Some(&vis) = type_vis.get(&name.node) {
                    if vis == Visibility::Private {
                        private_types.push(name.node.clone());
                    }
                }
            }
            // Recursively check generic type arguments
            for arg in generic_args {
                private_types.extend(check_type_visibility(&arg.node, type_vis));
            }
        }
        Type::Tuple(elems) => {
            for elem in elems {
                private_types.extend(check_type_visibility(&elem.node, type_vis));
            }
        }
        Type::Array { element, .. } | Type::Slice { element } => {
            private_types.extend(check_type_visibility(&element.node, type_vis));
        }
        Type::Function { params, return_type, .. } => {
            private_types.extend(check_type_visibility(&return_type.node, type_vis));
            for param in params {
                private_types.extend(check_type_visibility(&param.node, type_vis));
            }
        }
        Type::Ref(inner) | Type::Owned(inner) => {
            private_types.extend(check_type_visibility(&inner.node, type_vis));
        }
        _ => {}
    }
    private_types
}

/// Check all public functions for private types in their signatures.
pub(super) fn check_private_in_public(
    items: &[Spanned<Item>],
    _scopes: &ScopeTable,
    errors: &mut Vec<SemanticError>,
) {
    let type_vis = collect_type_visibility(items);

    for item in all_spanned_items(items) {
        match &item.node {
            Item::Function(func) => {
                if func.visibility != Visibility::Public {
                    continue;
                }
                // Check return type
                for private_type in check_type_visibility(&func.return_type.node, &type_vis) {
                    errors.push(SemanticError {
                        kind: SemanticErrorKind::PrivateTypeInPublicSignature {
                            type_name: private_type,
                            fn_name: func.name.node.clone(),
                            position: "return type".to_string(),
                        },
                        span: func.return_type.span,
                    });
                }
                // Check parameter types
                for param in &func.params {
                    for private_type in check_type_visibility(&param.node.type_.node, &type_vis) {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::PrivateTypeInPublicSignature {
                                type_name: private_type,
                                fn_name: func.name.node.clone(),
                                position: "parameter".to_string(),
                            },
                            span: param.node.type_.span,
                        });
                    }
                }
            }
            Item::Equip(impl_block) => {
                for method in &impl_block.items {
                    if method.node.visibility != Visibility::Public {
                        continue;
                    }
                    // Check return type
                    for private_type in check_type_visibility(&method.node.return_type.node, &type_vis) {
                        errors.push(SemanticError {
                            kind: SemanticErrorKind::PrivateTypeInPublicSignature {
                                type_name: private_type,
                                fn_name: method.node.name.node.clone(),
                                position: "return type".to_string(),
                            },
                            span: method.node.return_type.span,
                        });
                    }
                    // Check parameter types (skip self)
                    for param in &method.node.params {
                        for private_type in check_type_visibility(&param.node.type_.node, &type_vis) {
                            errors.push(SemanticError {
                                kind: SemanticErrorKind::PrivateTypeInPublicSignature {
                                    type_name: private_type,
                                    fn_name: method.node.name.node.clone(),
                                    position: "parameter".to_string(),
                                },
                                span: param.node.type_.span,
                            });
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Recursively check items, descending into `Item::Module` wrappers.
// ─── Phase 4: Type-position usage walker for unused-import check ──────

/// Walk every `Type` annotation reachable from the module's items and
/// register the resolved DefId of each `Type::Named` into `out`. The resolver
/// only inserts into `resolution_map` for expression-position uses
/// (`Expr::Identifier`, `Expr::Path`, etc.); type annotations never enter
/// the map. Without this walk, an import used ONLY as a type annotation
/// would falsely warn as unused — snag #7 (2026-05-05).
///
/// Conservative: any module-scope-resolvable name in any reachable type
/// position counts as a use. Walks via the shared `ExprVisitor` framework
/// for stmt/expr recursion (so new Stmt/Expr variants are caught at compile
/// time), with explicit type-position recursion for the type AST.
pub(super) fn collect_used_type_def_ids(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
    out: &mut rustc_hash::FxHashSet<DefId>,
) {
    use crate::parser::visitor::ExprVisitor;

    struct TypeWalker<'a> {
        scopes: &'a ScopeTable,
        out: &'a mut rustc_hash::FxHashSet<DefId>,
    }

    impl<'a> TypeWalker<'a> {
        fn walk_type(&mut self, ty: &Type) {
            match ty {
                Type::Named { name, generic_args } => {
                    if let Some(def_id) = self.scopes.lookup(&name.node) {
                        self.out.insert(def_id);
                    }
                    for arg in generic_args { self.walk_type(&arg.node); }
                }
                Type::Tuple(elems) => for e in elems { self.walk_type(&e.node); },
                Type::Function { params, return_type, .. } => {
                    for p in params { self.walk_type(&p.node); }
                    self.walk_type(&return_type.node);
                }
                Type::Ref(inner) | Type::Owned(inner) | Type::Pointer(inner) => {
                    self.walk_type(&inner.node);
                }
                Type::Slice { element } => self.walk_type(&element.node),
                Type::Array { element, .. } => self.walk_type(&element.node),
                // Primitives, SelfType, Inferred — no named def to resolve.
                Type::Primitive(_) | Type::SelfType | Type::Inferred => {}
            }
        }
    }

    impl<'a> ExprVisitor for TypeWalker<'a> {
        fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
            // Hook every type annotation reachable through statement-level syntax.
            if let Stmt::VarDecl { type_, .. } = &stmt.node {
                self.walk_type(&type_.node);
            }
            crate::parser::visitor::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Spanned<Expr>) {
            match &expr.node {
                Expr::Closure { params, .. } => {
                    for p in params {
                        if let Some(ty) = &p.node.type_ {
                            self.walk_type(&ty.node);
                        }
                    }
                }
                Expr::Rethrow { error_binding: Some((ty, _)), .. } => {
                    self.walk_type(&ty.node);
                }
                // MethodCall on an Identifier receiver matching an Enum: this is
                // the shape produced by the loader's `qualify_expr` pass when a
                // bare non-generic enum-variant ctor call (e.g. `FxPure()`) is
                // rewritten to `SideEffects.FxPure()` (Call → MethodCall with the
                // enum type as receiver). The variant name `FxPure` is the import,
                // but it no longer appears as an Expr::Identifier anywhere — only
                // as `MethodCall.method`. Credit the import by looking the method
                // name up in scope. Spurious lookups for real method calls (e.g.
                // `vec.push(x)`) are harmless: `push` is either not in scope or
                // refers to a definition that no one imports, so adding its def
                // to `used_def_ids` doesn't affect the unused-import check.
                Expr::MethodCall { receiver, method, .. } => {
                    if let Expr::Identifier(recv_name) = &receiver.node {
                        if let Some(recv_def) = self.scopes.lookup(recv_name) {
                            let recv_kind = self.scopes.get_def(recv_def).kind;
                            if matches!(recv_kind, crate::semantic::scope::DefKind::Enum | crate::semantic::scope::DefKind::Import) {
                                if let Some(method_def) = self.scopes.lookup(&method.node) {
                                    self.out.insert(method_def);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
            crate::parser::visitor::walk_expr(self, expr);
        }
    }

    let mut walker = TypeWalker { scopes, out };

    fn walk_func(walker: &mut TypeWalker, func: &FunctionDef) {
        for p in &func.params { walker.walk_type(&p.node.type_.node); }
        walker.walk_type(&func.return_type.node);
        if let Some(throws) = func.throws.explicit_type() { walker.walk_type(&throws.node); }
        match &func.body {
            FunctionBody::Block(b) => walker.visit_block(b),
            FunctionBody::Expression(e) => walker.visit_expr(e),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }

    fn walk_item(walker: &mut TypeWalker, item: &Item) {
        match item {
            Item::Function(func) => walk_func(walker, func),
            Item::Struct(s) => for f in &s.fields { walker.walk_type(&f.node.type_.node); },
            Item::Enum(e) => for v in &e.variants {
                if let VariantFields::Tuple(fields) = &v.node.fields {
                    for f in fields { walker.walk_type(&f.node); }
                }
            },
            Item::TypeAlias(a) => walker.walk_type(&a.type_.node),
            Item::Newtype(n) => walker.walk_type(&n.inner_type.node),
            Item::ConstDecl(c) => {
                walker.walk_type(&c.type_.node);
                walker.visit_expr(&c.value);
            }
            Item::StaticDecl(s) => {
                walker.walk_type(&s.type_.node);
                walker.visit_expr(&s.value);
            }
            Item::Equip(eq) => {
                walker.walk_type(&eq.type_.node);
                if let Some(t) = &eq.trait_ { walker.walk_type(&t.trait_name.node); }
                for m in &eq.items { walk_func(walker, &m.node); }
            }
            Item::Trait(t) => {
                for ti in &t.items {
                    if let TraitItem::Method(m) = &ti.node { walk_func(walker, m); }
                }
            }
            Item::ExternBlock(eb) => {
                for f in &eb.items { walk_func(walker, &f.node); }
            }
            Item::Test(t) => walker.visit_block(&t.body),
            Item::Bench(b) => walker.visit_block(&b.body),
            Item::SuiteSetup(s) => walker.visit_block(&s.body),
            Item::SuiteTeardown(s) => walker.visit_block(&s.body),
            Item::Module { items: inner, .. } => {
                for sub in inner { walk_item(walker, &sub.node); }
            }
            _ => {}
        }
    }

    for item in items {
        walk_item(&mut walker, &item.node);
    }
}

// ─── Phase 4: Unused Import Collection ──────────────────────

/// Collect DefIds for imported names (non-recursive — only top-level imports,
/// not imports inside `Item::Module` wrappers which are from imported files).
pub(super) fn collect_imported_defs(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
    out: &mut Vec<(DefId, String, Span)>,
) {
    for item in items {
        match &item.node {
            Item::Import(import) => {
                match import {
                    ImportStmt::Simple { path, .. } => {
                        if let Some(last) = path.last() {
                            if let Some(def_id) = scopes.lookup(&last.node) {
                                out.push((def_id, last.node.clone(), last.span));
                            }
                        }
                    }
                    ImportStmt::Grouped { names, .. } => {
                        for name in names {
                            if let Some(def_id) = scopes.lookup(&name.node) {
                                out.push((def_id, name.node.clone(), name.span));
                            }
                        }
                    }
                    ImportStmt::From { names, wildcard, .. } => {
                        // Skip GLOB entries — enum type imports are used
                        // implicitly through their variants, which may not
                        // appear in resolution_map. They share `names` with the
                        // plain entries (one author-ordered vector), so the
                        // skip is a filter rather than a field that is ignored.
                        for n in names.iter().filter(|n| !n.glob) {
                            let local = n.local_name();
                            if let Some(def_id) = scopes.lookup(&local.node) {
                                out.push((def_id, local.node.clone(), local.span));
                            }
                        }
                        // Skip module-level wildcard — names are user-driven via
                        // their use sites; the wildcard itself doesn't have a
                        // single binding span.
                        let _ = wildcard;
                    }
                }
            }
            // Don't recurse into Item::Module — imported module code has its own imports
            _ => {}
        }
    }
}

// ─── Pass 5c: Purity inference ──────────────────────────────

/// Infer purity for all functions in a module.
///
/// Two-pass approach:
/// 1. First pass: compute local purity (ignoring callee purity) for each function.
/// 2. Second pass: propagate callee purity through the call graph (fixed-point).
pub(super) fn infer_purity(
    module: &Module,
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
) -> crate::semantic::purity::PurityByName {
    use crate::semantic::purity::{Purity, PurityByName};

    let mut result: PurityByName = PurityByName::default();
    let mut call_graph: FxHashMap<String, Vec<String>> = FxHashMap::default(); // caller → callees

    // Pass 1: Compute local purity for each function
    infer_purity_items(&module.items, scopes, resolution_map, &mut result, &mut call_graph);

    // Pass 2: Propagate callee purity (fixed-point iteration)
    // Each function's purity is the join of its local purity and all callees' purity.
    let mut changed = true;
    let mut iterations = 0;
    while changed && iterations < 100 {
        changed = false;
        iterations += 1;
        for (caller, callees) in &call_graph {
            let mut new_purity = result.get(caller).copied().unwrap_or(Purity::Pure);
            for callee in callees {
                let callee_purity = result.get(callee).copied()
                    .unwrap_or(Purity::HasSideEffects); // unknown callee → impure
                new_purity = new_purity.join(callee_purity);
            }
            if let Some(existing) = result.get_mut(caller) {
                if *existing != new_purity {
                    *existing = new_purity;
                    changed = true;
                }
            }
        }
    }

    result
}

/// Walk AST items and compute local purity for each function.
fn infer_purity_items(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
    result: &mut crate::semantic::purity::PurityByName,
    call_graph: &mut FxHashMap<String, Vec<String>>,
) {
    for item in items {
        match &item.node {
            Item::Module { items: inner, .. } => {
                infer_purity_items(inner, scopes, resolution_map, result, call_graph);
            }
            Item::Function(f) => {
                let (purity, callees) = infer_function_purity(f, scopes, resolution_map);
                result.insert(f.name.node.clone(), purity);
                if !callees.is_empty() {
                    call_graph.insert(f.name.node.clone(), callees);
                }
            }
            Item::Equip(equip) => {
                let type_name = match &equip.type_.node {
                    Type::Named { name, .. } => name.node.clone(),
                    _ => continue,
                };
                for method in &equip.items {
                    let mangled = format!("{}__{}", type_name, method.node.name.node);
                    let (purity, callees) = infer_function_purity(&method.node, scopes, resolution_map);
                    result.insert(mangled.clone(), purity);
                    if !callees.is_empty() {
                        call_graph.insert(mangled, callees);
                    }
                }
            }
            _ => {}
        }
    }
}

/// Infer local purity for a single function definition.
/// Returns (local_purity, list_of_callee_names).
fn infer_function_purity(
    func: &FunctionDef,
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
) -> (crate::semantic::purity::Purity, Vec<String>) {
    use crate::semantic::purity::{Purity, PurityAccumulator};

    let mut acc = PurityAccumulator::new();
    let mut callees = Vec::new();

    // Extern/Declaration functions are impure by default
    match &func.body {
        FunctionBody::Extern(_) | FunctionBody::Declaration => {
            return (Purity::HasSideEffects, callees);
        }
        _ => {}
    }

    // &/! params mean function may mutate args
    for param in &func.params {
        if matches!(param.node.ownership, Ownership::MutableBorrow | Ownership::Move) {
            acc.mutates_param();
            break;
        }
    }

    // Walk the body
    match &func.body {
        FunctionBody::Block(block) => {
            purity_walk_block(block, scopes, resolution_map, &mut acc, &mut callees);
        }
        FunctionBody::Expression(expr) => {
            purity_walk_expr(expr, scopes, resolution_map, &mut acc, &mut callees);
        }
        _ => {}
    }

    (acc.finish(), callees)
}

/// Walk a block for purity analysis.
fn purity_walk_block(
    block: &Block,
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
    acc: &mut crate::semantic::purity::PurityAccumulator,
    callees: &mut Vec<String>,
) {
    for stmt in &block.stmts {
        purity_walk_stmt(&stmt.node, scopes, resolution_map, acc, callees);
    }
}

/// Walk a statement for purity analysis.
fn purity_walk_stmt(
    stmt: &Stmt,
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
    acc: &mut crate::semantic::purity::PurityAccumulator,
    callees: &mut Vec<String>,
) {
    match stmt {
        Stmt::VarDecl { value, .. } => {
            purity_walk_expr(value, scopes, resolution_map, acc, callees);
        }
        Stmt::Assign { target, value } => {
            // Check if target is a global
            if let Expr::Identifier(_) = &target.node {
                if let Some(&def_id) = resolution_map.get(&target.span.start) {
                    let kind = scopes.get_def(def_id).kind;
                    if kind == DefKind::Static {
                        acc.writes_global();
                    }
                }
            }
            purity_walk_expr(target, scopes, resolution_map, acc, callees);
            purity_walk_expr(value, scopes, resolution_map, acc, callees);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            if let Expr::Identifier(_) = &target.node {
                if let Some(&def_id) = resolution_map.get(&target.span.start) {
                    let kind = scopes.get_def(def_id).kind;
                    if kind == DefKind::Static {
                        acc.writes_global();
                    }
                }
            }
            purity_walk_expr(target, scopes, resolution_map, acc, callees);
            purity_walk_expr(value, scopes, resolution_map, acc, callees);
        }
        Stmt::Return(Some(expr)) | Stmt::Throw(expr) => {
            purity_walk_expr(expr, scopes, resolution_map, acc, callees);
        }
        Stmt::Expr(expr) => {
            purity_walk_expr(expr, scopes, resolution_map, acc, callees);
        }
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            purity_walk_expr(condition, scopes, resolution_map, acc, callees);
            purity_walk_block(then_body, scopes, resolution_map, acc, callees);
            for (cond, block) in elif_branches {
                purity_walk_expr(cond, scopes, resolution_map, acc, callees);
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
            if let Some(block) = else_body {
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
        }
        Stmt::While { condition, body, else_body } => {
            purity_walk_expr(condition, scopes, resolution_map, acc, callees);
            purity_walk_block(body, scopes, resolution_map, acc, callees);
            if let Some(block) = else_body {
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
        }
        Stmt::For { iterable, body, else_body, .. } => {
            purity_walk_expr(iterable, scopes, resolution_map, acc, callees);
            purity_walk_block(body, scopes, resolution_map, acc, callees);
            if let Some(block) = else_body {
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            purity_walk_expr(scrutinee, scopes, resolution_map, acc, callees);
            for item in arms {
                if let Some(arm) = item.arm() {
                    if let Some(guard) = &arm.guard {
                        purity_walk_expr(guard, scopes, resolution_map, acc, callees);
                    }
                    purity_walk_expr(&arm.body, scopes, resolution_map, acc, callees);
                }
            }
            if let Some(block) = else_arm {
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
        }
        Stmt::With { body, .. } => {
            acc.accesses_shared(); // `with` blocks access shared state
            purity_walk_block(body, scopes, resolution_map, acc, callees);
        }
        Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } | Stmt::OnError { body } => {
            purity_walk_block(body, scopes, resolution_map, acc, callees);
        }
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            purity_walk_expr(condition, scopes, resolution_map, acc, callees);
            if let Some(msg) = message {
                purity_walk_expr(msg, scopes, resolution_map, acc, callees);
            }
        }
        Stmt::Item(item) => {
            if let Item::Function(_) = &**item {
                // Nested function — purity is computed separately; skip body
            }
        }
        _ => {}
    }
}

/// Walk an expression for purity analysis.
/// Uses a generic sub-expression visitor to avoid coupling to every AST variant.
fn purity_walk_expr(
    expr: &Spanned<Expr>,
    scopes: &ScopeTable,
    resolution_map: &ResolutionMap,
    acc: &mut crate::semantic::purity::PurityAccumulator,
    callees: &mut Vec<String>,
) {
    match &expr.node {
        Expr::Identifier(_) => {
            // Check if reading a global variable
            if let Some(&def_id) = resolution_map.get(&expr.span.start) {
                let kind = scopes.get_def(def_id).kind;
                if kind == DefKind::Static {
                    acc.reads_global();
                }
            }
        }
        Expr::Call { callee, args, .. } => {
            // Record callee name for call graph propagation
            if let Expr::Identifier(name) = &callee.node {
                callees.push(name.clone());
            } else if let Expr::Path { segments } = &callee.node {
                if let Some(last) = segments.last() {
                    callees.push(last.node.clone());
                }
            }
            purity_walk_expr(callee, scopes, resolution_map, acc, callees);
            for arg in args {
                purity_walk_expr(&arg.node.value, scopes, resolution_map, acc, callees);
            }
        }
        Expr::MethodCall { receiver, method, args, .. } => {
            let method_name = method.node.clone();
            callees.push(method_name);
            purity_walk_expr(receiver, scopes, resolution_map, acc, callees);
            for arg in args {
                purity_walk_expr(&arg.node.value, scopes, resolution_map, acc, callees);
            }
        }
        Expr::Await { expr: inner, .. } | Expr::Spawn { expr: inner, .. } | Expr::SpawnBlocking { expr: inner, .. } => {
            acc.accesses_shared();
            purity_walk_expr(inner, scopes, resolution_map, acc, callees);
        }
        _ => {
            // Generic sub-expression walk for all other variants. Recursion is
            // DELEGATED to the one exhaustive child enumeration
            // (`crate::parser::visitor::visit_expr_children`) rather than
            // hand-rolled here: the local copy this replaced silently skipped
            // 11 of the 47 `Expr` variants, 9 of them child-bearing (`??`,
            // `catch`, `?.`, set/dict comprehensions, `Block`, `Do`,
            // `DotShorthand`, `MetaOpInfix`), which made purity analysis miss
            // real effects inside those positions. See the chokepoint's header
            // for the three compile-time guards that keep it total.
            //
            // ⚠ COLLECT-THEN-WALK, deliberately: two closures capturing `acc`
            // and `callees` at once is `E0524`. The children are collected
            // first, then walked. Order between the two lists is immaterial —
            // `Purity::join` is `std::cmp::max` and `callees` is a set the
            // call-graph pass takes a fixpoint over.
            let mut child_exprs: Vec<&Spanned<Expr>> = Vec::new();
            let mut child_blocks: Vec<&Block> = Vec::new();
            visit_expr_children(
                &expr.node,
                &mut |child| child_exprs.push(child),
                &mut |block| child_blocks.push(block),
            );
            for child in child_exprs {
                purity_walk_expr(child, scopes, resolution_map, acc, callees);
            }
            for block in child_blocks {
                purity_walk_block(block, scopes, resolution_map, acc, callees);
            }
        }
    }
}
