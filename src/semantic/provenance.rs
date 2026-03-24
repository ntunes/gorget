//! Pass 4.5: String provenance inference.
//!
//! After type checking (Pass 4) assigns all String-typed bindings `owned_string_id`
//! (conservative default), this pass downgrades provably-view bindings to `string_id`
//! (Str/Copy/reference type). The borrow checker (Pass 5) then sees the real
//! representation and enforces lifetimes for views, move semantics for owned.
//!
//! Downgraded to view:
//! 1. Bare borrow String parameters (no `&`/`!`)
//! 2. For-loop bindings over String collections
//! 3. Match bindings from String scrutinees
//! 4. VarDecl bindings whose RHS is provably a view (identifier, field access,
//!    view-returning method, static literal, etc.)
//!
//! Left as owned (no downgrade):
//! - VarDecl bindings whose RHS allocates (f-string, concat, etc.)
//! - `!` (Move) parameters
//! - Function return types (unless ALL returns are provably views)
//! - Struct/enum fields (structs own their data)

use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::{Span, Spanned};

use super::ids::{DefId, ScopeId, TypeId};
use super::resolve::{FunctionInfo, ResolutionMap};
use super::scope::ScopeTable;
use super::types::{ResolvedType, TypeTable};

/// Provenance of a String binding: View (Str, Copy) or Owned (GorgetString, Move).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringProvenance {
    View,
    Owned,
}

/// View-returning string methods (return a pointer into the receiver, no allocation).
const VIEW_METHODS: &[&str] = &[
    "trim", "strip", "lstrip", "rstrip",
    "removeprefix", "removesuffix",
    "byte_slice", "substring", "char_at", "as_str", "slice",
];

/// Run provenance inference on all functions in the module.
/// Rewrites `DefInfo.type_id` from `owned_string_id` → `string_id` for view bindings.
/// Returns a set of DefIds that were downgraded to view strings.
pub fn infer_string_provenance(
    module: &Module,
    scopes: &mut ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    expr_types: &FxHashMap<Span, TypeId>,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    method_resolutions: &FxHashMap<usize, DefId>,
) {
    let string_id = types.string_id;
    let owned_string_id = types.owned_string_id;

    let mut ctx = ProvenanceCtx {
        scopes,
        types,
        resolution_map,
        expr_types,
        function_info,
        method_resolutions,
        string_id,
        owned_string_id,
        analyzed_functions: FxHashSet::default(),
        mut_borrowed_strings: FxHashSet::default(),
    };

    // Phase 1: Process imported modules first so their return types are resolved
    // before local functions reference them.
    // Phase 2: Local items (functions, equip methods, tests)
    infer_items(&mut ctx, &module.items);
}

/// Rewrite AST type annotations to match provenance-adjusted semantic type_ids.
/// Recursively rewrite `PrimitiveType::StringType` → `PrimitiveType::StringView` in a type tree.
/// Handles bare `String`, generic args like `Vector[String]`, tuples, etc.
fn rewrite_type_to_str(ty: &mut Type) {
    match ty {
        Type::Primitive(PrimitiveType::StringType) => {
            *ty = Type::Primitive(PrimitiveType::StringView);
        }
        Type::Named { generic_args, .. } => {
            for arg in generic_args {
                rewrite_type_to_str(&mut arg.node);
            }
        }
        Type::Tuple(elems) => {
            for elem in elems {
                rewrite_type_to_str(&mut elem.node);
            }
        }
        Type::Array { element, .. } | Type::Slice { element } => {
            rewrite_type_to_str(&mut element.node);
        }
        Type::Function { return_type, params, .. } => {
            rewrite_type_to_str(&mut return_type.node);
            for p in params {
                rewrite_type_to_str(&mut p.node);
            }
        }
        Type::Ref(inner) | Type::Owned(inner) => {
            rewrite_type_to_str(&mut inner.node);
        }
        _ => {}
    }
}

/// After str→StringType unification, all string annotations are `StringType`.
/// Provenance downgrades some bindings' type_ids to `string_id` (view).
/// This pass rewrites the AST `StringType` → `Str` for those bindings so the
/// IR lowering emits correct drop elaboration and calling conventions.
pub fn rewrite_ast_string_types(
    module: &mut Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    function_info: &FxHashMap<DefId, FunctionInfo>,
) {
    let string_id = types.string_id;
    let owned_string_id = types.owned_string_id;

    for item in &mut module.items {
        rewrite_item(&mut item.node, scopes, types, function_info, string_id, owned_string_id);
    }
}

fn rewrite_item(
    item: &mut Item,
    scopes: &ScopeTable,
    types: &TypeTable,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    string_id: TypeId,
    owned_string_id: TypeId,
) {
    match item {
        Item::Function(f) => {
            rewrite_function(f, scopes, types, function_info, string_id, owned_string_id);
        }
        Item::Equip(equip) => {
            for method in &mut equip.items {
                rewrite_function(&mut method.node, scopes, types, function_info, string_id, owned_string_id);
            }
        }
        Item::Trait(trait_def) => {
            for trait_item in &mut trait_def.items {
                if let TraitItem::Method(f) = &mut trait_item.node {
                    rewrite_function(f, scopes, types, function_info, string_id, owned_string_id);
                }
            }
        }
        // Struct and enum field types are NOT rewritten to Str — structs must OWN
        // their string data (GorgetString) so recursive drop can free them. Field
        // LOADS return Str views at IR lowering time, not at type definition time.
        Item::Struct(_) | Item::Enum(_) => {}
        Item::Test(t) => {
            rewrite_block_stmts(&mut t.body.stmts, scopes, string_id);
        }
        Item::Module { items, .. } => {
            for sub in items {
                rewrite_item(&mut sub.node, scopes, types, function_info, string_id, owned_string_id);
            }
        }
        _ => {}
    }
}

fn rewrite_function(
    func: &mut FunctionDef,
    scopes: &ScopeTable,
    _types: &TypeTable,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    string_id: TypeId,
    _owned_string_id: TypeId,
) {
    // Rewrite parameter types: bare borrow String params are always views.
    // Use ownership sigil rather than DEF lookup — this handles trait default methods
    // whose DefIds may not be in the scope table.
    for param in &mut func.params {
        if matches!(param.node.type_.node, Type::Primitive(PrimitiveType::StringType))
            && param.node.ownership == Ownership::Borrow
            && param.node.name.node != "self"
        {
            param.node.type_.node = Type::Primitive(PrimitiveType::StringView);
        }
    }

    // Rewrite return type based on FunctionInfo
    if matches!(func.return_type.node, Type::Primitive(PrimitiveType::StringType)) {
        if let Some(def_id) = scopes.lookup_def_by_span(&func.name.node, func.name.span) {
            if let Some(fi) = function_info.get(&def_id) {
                if fi.return_type_id == Some(string_id) {
                    func.return_type.node = Type::Primitive(PrimitiveType::StringView);
                }
            }
        }
    }

    // Rewrite VarDecl types in function body
    if let FunctionBody::Block(block) = &mut func.body {
        rewrite_block_stmts(&mut block.stmts, scopes, string_id);
    }
}

fn rewrite_block_stmts(
    stmts: &mut [Spanned<Stmt>],
    scopes: &ScopeTable,
    string_id: TypeId,
) {
    for stmt in stmts {
        rewrite_stmt(&mut stmt.node, scopes, string_id);
    }
}

fn rewrite_stmt(
    stmt: &mut Stmt,
    scopes: &ScopeTable,
    string_id: TypeId,
) {
    match stmt {
        Stmt::VarDecl { type_, pattern, .. } => {
            if matches!(type_.node, Type::Primitive(PrimitiveType::StringType)) {
                if let Pattern::Binding(name) = &pattern.node {
                    if let Some(def_id) = scopes.lookup_def_by_span(name, pattern.span) {
                        let def = scopes.get_def(def_id);
                        if let Some(tid) = def.type_id {
                            if tid == string_id {
                                type_.node = Type::Primitive(PrimitiveType::StringView);
                            }
                        }
                    }
                }
            }
        }
        Stmt::NamedScope { body, .. } | Stmt::Unsafe { body } => {
            rewrite_block_stmts(&mut body.stmts, scopes, string_id);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            rewrite_block_stmts(&mut then_body.stmts, scopes, string_id);
            for (_, elif_body) in elif_branches {
                rewrite_block_stmts(&mut elif_body.stmts, scopes, string_id);
            }
            if let Some(else_b) = else_body {
                rewrite_block_stmts(&mut else_b.stmts, scopes, string_id);
            }
        }
        Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
            rewrite_block_stmts(&mut body.stmts, scopes, string_id);
        }
        Stmt::For { body, .. } => {
            rewrite_block_stmts(&mut body.stmts, scopes, string_id);
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms {
                if let MatchItem::Arm(arm) = arm {
                    // Match arm body is Spanned<Expr> — check for block expressions
                    if let Expr::Block(block) = &mut arm.body.node {
                        rewrite_block_stmts(&mut block.stmts, scopes, string_id);
                    }
                }
            }
            if let Some(else_b) = else_arm {
                rewrite_block_stmts(&mut else_b.stmts, scopes, string_id);
            }
        }
        Stmt::With { body, .. } => {
            rewrite_block_stmts(&mut body.stmts, scopes, string_id);
        }
        Stmt::Select { arms, else_arm, .. } => {
            for arm in arms {
                rewrite_block_stmts(&mut arm.body.stmts, scopes, string_id);
            }
            if let Some(else_b) = else_arm {
                rewrite_block_stmts(&mut else_b.stmts, scopes, string_id);
            }
        }
        _ => {}
    }
}

struct ProvenanceCtx<'a> {
    scopes: &'a mut ScopeTable,
    types: &'a TypeTable,
    resolution_map: &'a ResolutionMap,
    expr_types: &'a FxHashMap<Span, TypeId>,
    function_info: &'a mut FxHashMap<DefId, FunctionInfo>,
    method_resolutions: &'a FxHashMap<usize, DefId>,
    string_id: TypeId,
    owned_string_id: TypeId,
    /// Functions whose return types have been analyzed by provenance.
    analyzed_functions: FxHashSet<DefId>,
    /// String locals that are `&`-borrowed in the current function — skip downgrade.
    mut_borrowed_strings: FxHashSet<DefId>,
}

impl<'a> ProvenanceCtx<'a> {
    fn is_owned_string(&self, tid: TypeId) -> bool {
        tid == self.owned_string_id
    }

    fn is_any_string(&self, tid: TypeId) -> bool {
        tid == self.string_id || tid == self.owned_string_id
    }

    /// Classify an expression's string provenance.
    ///
    /// View = borrows from another variable (pointer copy).
    /// Owned = allocates new data or takes ownership (GorgetString).
    fn classify_expr(&self, expr: &Spanned<Expr>) -> StringProvenance {
        match &expr.node {
            // Plain string literals → View (static data, no allocation, cap=0).
            // F-strings (interpolated) → Owned (allocates a new GorgetString).
            Expr::StringLiteral(lit) if !lit.has_interpolation() => StringProvenance::View,
            Expr::StringLiteral(_) => StringProvenance::Owned,

            // Concat → Owned (allocates new GorgetString)
            Expr::BinaryOp { op: BinaryOp::Add, .. } => StringProvenance::Owned,

            // Move → Owned (takes ownership)
            Expr::Move { .. } => StringProvenance::Owned,

            // Identifier (without move) → View (borrows from source variable)
            Expr::Identifier(_) | Expr::SelfExpr => StringProvenance::View,

            // Function call → check callee's return type; if view, result is a view
            Expr::Call { callee, args: _, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    if let Some(def_id) = self.scopes.lookup_from_scope(ScopeId(0), name) {
                        if self.callee_returns_view(def_id) {
                            return StringProvenance::View;
                        }
                    }
                    // Fallback: resolution map lookup
                    if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                        if self.callee_returns_view(def_id) {
                            return StringProvenance::View;
                        }
                    }
                }
                // Fallback: if the type checker resolved this to string_id (view), trust it.
                if let Some(&tid) = self.expr_types.get(&expr.span) {
                    if tid == self.string_id {
                        return StringProvenance::View;
                    }
                }
                // Final fallback: check function type via DefInfo.type_id for
                // foreign/extern functions not in function_info.
                if let Some(&def_id) = self.resolution_map.get(&callee.span.start) {
                    if self.callee_def_returns_view(def_id) {
                        return StringProvenance::View;
                    }
                }
                StringProvenance::Owned
            }

            // Method call → check VIEW_METHODS list, callee's return type, or expr_types.
            Expr::MethodCall { receiver, method, .. } => {
                if self.is_string_expr(receiver) && VIEW_METHODS.contains(&method.node.as_str()) {
                    return StringProvenance::View;
                }
                if let Some(&def_id) = self.method_resolutions.get(&method.span.start) {
                    if self.callee_returns_view(def_id) {
                        return StringProvenance::View;
                    }
                }
                // Fallback: if the type checker resolved this expression to string_id (view),
                // trust it. Covers built-in methods like .unwrap() on Option[str].
                if let Some(&tid) = self.expr_types.get(&expr.span) {
                    if tid == self.string_id {
                        return StringProvenance::View;
                    }
                }
                // unwrap/expect on collection access results (e.g., vec.get(i).unwrap())
                // return shallow copies borrowed from the collection → View.
                // But unwrap on I/O results (e.g., read_line().unwrap()) returns owned.
                if matches!(method.node.as_str(), "unwrap" | "expect" | "unwrap_or_default") {
                    if let Expr::MethodCall { method: inner_method, .. } = &receiver.node {
                        // Collection .get() returns Option with borrowed element
                        if matches!(inner_method.node.as_str(), "get" | "first" | "last"
                            | "peek" | "front" | "back") {
                            return StringProvenance::View;
                        }
                    }
                }
                StringProvenance::Owned
            }

            // Field access → View (borrows from object's field)
            Expr::FieldAccess { object, .. } => self.classify_expr(object),

            // Match expression → conservative: Owned if any arm is Owned
            Expr::Match { arms, else_arm, .. } => {
                let any_owned = arms.iter().any(|arm| {
                    self.classify_expr(&arm.body) == StringProvenance::Owned
                }) || else_arm.as_ref().map_or(false, |e| {
                    self.classify_expr(e) == StringProvenance::Owned
                });
                if any_owned { StringProvenance::Owned } else { StringProvenance::View }
            }

            // If expression → conservative: Owned if any branch is Owned
            Expr::If { then_branch, elif_branches, else_branch, .. } => {
                let mut any_owned = self.classify_expr(then_branch) == StringProvenance::Owned;
                for (_, body) in elif_branches {
                    if self.classify_expr(body) == StringProvenance::Owned {
                        any_owned = true;
                    }
                }
                if let Some(else_br) = else_branch {
                    if self.classify_expr(else_br) == StringProvenance::Owned {
                        any_owned = true;
                    }
                }
                if any_owned { StringProvenance::Owned } else { StringProvenance::View }
            }

            // Indexing/slicing a string → View (returns a view/slice, no allocation)
            Expr::Index { .. } => StringProvenance::View,

            // Everything else → Owned (conservative)
            _ => StringProvenance::Owned,
        }
    }

    /// Check if a callee's return type has been downgraded to view (string_id).
    fn callee_returns_view(&self, def_id: DefId) -> bool {
        if let Some(fi) = self.function_info.get(&def_id) {
            if let Some(ret_tid) = fi.return_type_id {
                return ret_tid == self.string_id;
            }
        }
        false
    }

    /// Check if a callee's return type is view by examining the function type
    /// in the scope/type table. This handles extern/foreign functions not in function_info.
    fn callee_def_returns_view(&self, def_id: DefId) -> bool {
        let def = self.scopes.get_def(def_id);
        if let Some(tid) = def.type_id {
            if let ResolvedType::Function { return_type, .. } = self.types.get(tid) {
                return *return_type == self.string_id;
            }
        }
        false
    }

    /// Classify a return expression's provenance. Unlike `classify_expr`, plain
    /// string literals (no interpolation) are View here because they point to static
    /// data that outlives any function call (no dangling risk). F-strings allocate
    /// and are Owned. Returning an owned variable transfers ownership → Owned.
    fn classify_return_expr(&self, expr: &Spanned<Expr>) -> StringProvenance {
        match &expr.node {
            Expr::StringLiteral(lit) if !lit.has_interpolation() => StringProvenance::View,
            // Returning an owned variable transfers ownership → Owned return.
            Expr::Identifier(name) => {
                if let Some(def_id) = self.find_binding_def(name, expr.span) {
                    let def = self.scopes.get_def(def_id);
                    if def.type_id == Some(self.owned_string_id) {
                        return StringProvenance::Owned;
                    }
                }
                StringProvenance::View
            }
            // Field access in return context: if the root object is a local variable
            // (not a parameter), the view would dangle — treat as Owned so the IR
            // auto-clones. Views from parameters are safe (caller keeps them alive).
            Expr::FieldAccess { object, .. } => {
                if self.return_expr_borrows_from_local(object) {
                    StringProvenance::Owned
                } else {
                    self.classify_expr(expr)
                }
            }
            // Method call on local in return context: same logic — view from local
            // would dangle, treat as Owned.
            Expr::MethodCall { receiver, method, .. } => {
                // View-returning methods (trim, slice, etc.) on local objects can't escape
                if VIEW_METHODS.contains(&method.node.as_str())
                    && self.return_expr_borrows_from_local(receiver)
                {
                    StringProvenance::Owned
                } else {
                    self.classify_expr(expr)
                }
            }
            _ => self.classify_expr(expr),
        }
    }

    /// Check if an expression's root borrows from a local variable (not a parameter).
    fn return_expr_borrows_from_local(&self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Identifier(name) => {
                if let Some(def_id) = self.find_binding_def(name, expr.span) {
                    let def = self.scopes.get_def(def_id);
                    // Parameters have DefKind::Param; locals have DefKind::Variable
                    matches!(def.kind, crate::semantic::scope::DefKind::Variable)
                } else {
                    false
                }
            }
            Expr::FieldAccess { object, .. } => self.return_expr_borrows_from_local(object),
            Expr::MethodCall { receiver, .. } => self.return_expr_borrows_from_local(receiver),
            _ => false,
        }
    }

    fn is_string_expr(&self, expr: &Spanned<Expr>) -> bool {
        if let Some(&tid) = self.expr_types.get(&expr.span) {
            return self.is_any_string(tid);
        }
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.find_binding_def(name, expr.span) {
                let def = self.scopes.get_def(def_id);
                if let Some(tid) = def.type_id {
                    return self.is_any_string(tid);
                }
            }
        }
        matches!(&expr.node, Expr::StringLiteral(_))
    }

    fn is_string_ast_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Primitive(PrimitiveType::StringType))
    }

    fn downgrade_to_view(&mut self, def_id: DefId) {
        let def = self.scopes.get_def_mut(def_id);
        if let Some(tid) = def.type_id {
            if tid == self.owned_string_id {
                def.type_id = Some(self.string_id);
            }
        }
    }

    fn find_binding_def(&self, name: &str, span: Span) -> Option<DefId> {
        self.scopes.lookup_def_by_span(name, span)
            .or_else(|| self.resolution_map.get(&span.start).copied())
    }

    /// Find the function's def_id, with fallback for equip methods.
    fn find_func_def_id(&self, func: &FunctionDef) -> Option<DefId> {
        self.scopes.lookup_from_scope(ScopeId(0), &func.name.node)
            .or_else(|| self.scopes.lookup_def_by_span(&func.name.node, func.name.span))
            .or_else(|| self.resolution_map.get(&func.name.span.start).copied())
            .or_else(|| {
                // Fallback for equip methods: search function_info by name/span
                self.function_info.keys().find(|&&did| {
                    let def = self.scopes.get_def(did);
                    def.name == func.name.node && def.span == func.name.span
                }).copied()
            })
    }

    /// Collect DefIds of String locals that are `&`-borrowed in the function body.
    /// If a variable is `&`-borrowed, the callee could replace its value with an
    /// owned string, so we conservatively keep it as owned (no downgrade).
    fn collect_mut_borrowed_strings(&self, stmts: &[Spanned<Stmt>]) -> FxHashSet<DefId> {
        let mut result = FxHashSet::default();
        for stmt in stmts {
            self.scan_stmt_for_mut_borrows(&stmt.node, &mut result);
        }
        result
    }

    fn scan_stmt_for_mut_borrows(&self, stmt: &Stmt, out: &mut FxHashSet<DefId>) {
        match stmt {
            Stmt::Expr(expr) | Stmt::Return(Some(expr)) => {
                self.scan_expr_for_mut_borrows(&expr.node, out);
            }
            Stmt::VarDecl { value, .. } => {
                self.scan_expr_for_mut_borrows(&value.node, out);
            }
            Stmt::Assign { value, .. } => {
                self.scan_expr_for_mut_borrows(&value.node, out);
            }
            Stmt::CompoundAssign { value, .. } => {
                self.scan_expr_for_mut_borrows(&value.node, out);
            }
            Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
                self.scan_expr_for_mut_borrows(&condition.node, out);
                for s in &then_body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                for (c, b) in elif_branches {
                    self.scan_expr_for_mut_borrows(&c.node, out);
                    for s in &b.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
                if let Some(eb) = else_body {
                    for s in &eb.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
            }
            Stmt::While { condition, body, .. } => {
                self.scan_expr_for_mut_borrows(&condition.node, out);
                for s in &body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            Stmt::Loop { body, .. } => {
                for s in &body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            Stmt::For { body, .. } => {
                for s in &body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            Stmt::Match { arms, else_arm, .. } => {
                for arm in arms {
                    if let MatchItem::Arm(a) = arm {
                        self.scan_expr_for_mut_borrows(&a.body.node, out);
                    }
                }
                if let Some(eb) = else_arm {
                    for s in &eb.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                for s in &body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    for s in &arm.body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
                if let Some(eb) = else_arm {
                    for s in &eb.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
            }
            Stmt::MetaFor { body, .. } => {
                for s in &body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                for s in &then_body.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                for (_, b) in elif_branches {
                    for s in &b.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
                if let Some(eb) = else_body {
                    for s in &eb.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
                }
            }
            _ => {}
        }
    }

    fn scan_expr_for_mut_borrows(&self, expr: &Expr, out: &mut FxHashSet<DefId>) {
        match expr {
            Expr::MutableBorrow { expr: inner } => {
                if let Expr::Identifier(name) = &inner.node {
                    if let Some(def_id) = self.find_binding_def(name, inner.span) {
                        let def = self.scopes.get_def(def_id);
                        if let Some(tid) = def.type_id {
                            if self.is_any_string(tid) {
                                out.insert(def_id);
                            }
                        }
                    }
                }
            }
            Expr::Call { args, .. } | Expr::MethodCall { args, .. } => {
                for arg in args {
                    if let Expr::MutableBorrow { expr: inner } = &arg.node.value.node {
                        if let Expr::Identifier(name) = &inner.node {
                            if let Some(def_id) = self.find_binding_def(name, inner.span) {
                                let def = self.scopes.get_def(def_id);
                                if let Some(tid) = def.type_id {
                                    if self.is_any_string(tid) {
                                        out.insert(def_id);
                                    }
                                }
                            }
                        }
                    }
                    self.scan_expr_for_mut_borrows(&arg.node.value.node, out);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.scan_expr_for_mut_borrows(&left.node, out);
                self.scan_expr_for_mut_borrows(&right.node, out);
            }
            Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
                self.scan_expr_for_mut_borrows(&condition.node, out);
                self.scan_expr_for_mut_borrows(&then_branch.node, out);
                for (c, b) in elif_branches {
                    self.scan_expr_for_mut_borrows(&c.node, out);
                    self.scan_expr_for_mut_borrows(&b.node, out);
                }
                if let Some(eb) = else_branch {
                    self.scan_expr_for_mut_borrows(&eb.node, out);
                }
            }
            Expr::Block(block) => {
                for s in &block.stmts { self.scan_stmt_for_mut_borrows(&s.node, out); }
            }
            _ => {}
        }
    }

    /// Infer provenance for a function's parameters, body, and return type.
    fn infer_function(&mut self, func: &FunctionDef) {
        let func_def_id = self.find_func_def_id(func);

        // Collect &-borrowed String locals so we don't downgrade them
        self.mut_borrowed_strings = match &func.body {
            FunctionBody::Block(block) => self.collect_mut_borrowed_strings(&block.stmts),
            _ => FxHashSet::default(),
        };

        // Parameters — bare borrow String params become views
        for (i, param) in func.params.iter().enumerate() {
            if !self.is_string_ast_type(&param.node.type_.node) {
                continue;
            }
            if param.node.ownership == Ownership::Borrow {
                if let Some(def_id) = self.find_binding_def(
                    &param.node.name.node, param.node.name.span
                ) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            self.downgrade_to_view(def_id);
                            if let Some(fid) = func_def_id {
                                if let Some(fi) = self.function_info.get_mut(&fid) {
                                    if i < fi.param_type_ids.len() {
                                        fi.param_type_ids[i] = Some(self.string_id);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Recurse into body for VarDecl/for-loop/match bindings
        match &func.body {
            FunctionBody::Block(block) => self.infer_block(block),
            _ => {}
        }

        // Return type — downgrade to view if ALL returns are provably views.
        if self.is_string_ast_type(&func.return_type.node) {
            if let Some(def_id) = func_def_id {
                let should_downgrade = match &func.body {
                    FunctionBody::Block(block) => {
                        let mut returns = Vec::new();
                        collect_return_exprs(&block.stmts, &mut returns);
                        !returns.is_empty() && returns.iter().all(|e| {
                            self.classify_return_expr(e) == StringProvenance::View
                        })
                    }
                    FunctionBody::Expression(expr) => {
                        self.classify_return_expr(expr.as_ref()) == StringProvenance::View
                    }
                    FunctionBody::Declaration | FunctionBody::Extern(_) => {
                        // No body to analyze. Only known view-returning methods get downgraded.
                        // Do NOT downgrade based on bare-borrow params — many stdlib functions
                        // (regex_escape, path_join, replace_all, etc.) take borrowed strings
                        // but return owned strings.
                        VIEW_METHODS.contains(&func.name.node.as_str())
                    }
                };

                if should_downgrade {
                    if let Some(fi) = self.function_info.get_mut(&def_id) {
                        if let Some(ret_tid) = fi.return_type_id {
                            if ret_tid == self.owned_string_id {
                                fi.return_type_id = Some(self.string_id);
                            }
                        }
                    }
                }
                // Mark this function as analyzed so callee_returns_view trusts its type.
                self.analyzed_functions.insert(def_id);
            }
        }
    }

    /// Infer provenance for all statements in a block.
    fn infer_block(&mut self, block: &Block) {
        // Collect String VarDecl bindings and classify their RHS
        let mut string_vars: Vec<(DefId, StringProvenance)> = Vec::new();
        for stmt in &block.stmts {
            self.collect_string_decls(&stmt.node, &mut string_vars);
        }

        // Scan ALL assignments to check for reassignment that changes provenance
        for stmt in &block.stmts {
            self.check_assignments(&stmt.node, &mut string_vars);
        }

        // Apply downgrades for variables that stayed View (skip &-borrowed vars)
        for (def_id, prov) in &string_vars {
            if *prov == StringProvenance::View && !self.mut_borrowed_strings.contains(def_id) {
                self.downgrade_to_view(*def_id);
            }
        }

        // Recurse into nested blocks
        for stmt in &block.stmts {
            self.recurse_into_stmt(&stmt.node);
        }
    }

    /// Collect String-typed VarDecl bindings with their initial provenance.
    fn collect_string_decls(&self, stmt: &Stmt, out: &mut Vec<(DefId, StringProvenance)>) {
        if let Stmt::VarDecl { type_, pattern, value, .. } = stmt {
            let prov = self.classify_expr(value);
            // For simple string-typed bindings
            if self.is_string_ast_type(&type_.node) {
                self.collect_string_pattern(pattern, prov, out);
                return;
            }
            // For auto/inferred types or tuple types: check each binding's
            // resolved type_id (set by the type checker) for owned strings.
            if matches!(type_.node, Type::Inferred | Type::Tuple(_)) {
                self.collect_string_pattern(pattern, prov, out);
            }
        }
    }

    /// Recursively collect String-typed bindings from a pattern.
    fn collect_string_pattern(
        &self,
        pattern: &Spanned<Pattern>,
        prov: StringProvenance,
        out: &mut Vec<(DefId, StringProvenance)>,
    ) {
        match &pattern.node {
            Pattern::Binding(name) => {
                if let Some(def_id) = self.find_binding_def(name, pattern.span) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            out.push((def_id, prov.clone()));
                        }
                    }
                }
            }
            Pattern::Tuple(pats) | Pattern::Constructor { fields: pats, .. } => {
                for p in pats {
                    self.collect_string_pattern(p, prov.clone(), out);
                }
            }
            _ => {}
        }
    }

    /// Check assignments that might upgrade a View variable to Owned.
    fn check_assignments(&self, stmt: &Stmt, vars: &mut Vec<(DefId, StringProvenance)>) {
        match stmt {
            Stmt::Assign { target, value } => {
                if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                    for (vid, prov) in vars.iter_mut() {
                        if *vid == def_id && *prov == StringProvenance::View {
                            if self.classify_expr(value) == StringProvenance::Owned {
                                *prov = StringProvenance::Owned;
                            }
                        }
                    }
                }
            }
            Stmt::CompoundAssign { target, .. } => {
                if let Some(&def_id) = self.resolution_map.get(&target.span.start) {
                    for (vid, prov) in vars.iter_mut() {
                        if *vid == def_id {
                            *prov = StringProvenance::Owned; // += always allocates
                        }
                    }
                }
            }
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                for s in &then_body.stmts { self.check_assignments(&s.node, vars); }
                for (_, branch) in elif_branches {
                    for s in &branch.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_br) = else_body {
                    for s in &else_br.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } | Stmt::For { body, .. } => {
                for s in &body.stmts { self.check_assignments(&s.node, vars); }
            }
            Stmt::Match { arms, else_arm, .. } => {
                for arm in arms {
                    if let MatchItem::Arm(a) = arm {
                        if let Expr::Block(block) = &a.body.node {
                            for s in &block.stmts { self.check_assignments(&s.node, vars); }
                        }
                    }
                }
                if let Some(else_block) = else_arm {
                    for s in &else_block.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    for s in &arm.body.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_block) = else_arm {
                    for s in &else_block.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                for s in &body.stmts { self.check_assignments(&s.node, vars); }
            }
            Stmt::MetaFor { body, .. } => {
                for s in &body.stmts { self.check_assignments(&s.node, vars); }
            }
            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                for s in &then_body.stmts { self.check_assignments(&s.node, vars); }
                for (_, branch) in elif_branches {
                    for s in &branch.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_br) = else_body {
                    for s in &else_br.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            Stmt::MetaMatch { arms, else_arm, .. } => {
                for (_, body) in arms {
                    for s in &body.stmts { self.check_assignments(&s.node, vars); }
                }
                if let Some(else_br) = else_arm {
                    for s in &else_br.stmts { self.check_assignments(&s.node, vars); }
                }
            }
            _ => {}
        }
    }

    fn recurse_into_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                self.infer_block(then_body);
                for (_, branch) in elif_branches {
                    self.infer_block(branch);
                }
                if let Some(else_br) = else_body {
                    self.infer_block(else_br);
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
                self.infer_block(body);
            }
            Stmt::For { pattern, iterable, body, .. } => {
                self.maybe_downgrade_for_binding(pattern, iterable);
                self.infer_block(body);
            }
            Stmt::Match { scrutinee: _, arms, else_arm, .. } => {
                // Downgrade all string-typed pattern bindings to view.
                // Match bindings borrow from the scrutinee for the arm's duration,
                // whether the scrutinee is a string directly or a generic type
                // containing strings (Option[str], Result[str, E], etc.).
                for arm in arms {
                    if let MatchItem::Arm(a) = arm {
                        self.downgrade_pattern_bindings(&a.pattern);
                        if let Expr::Block(block) = &a.body.node {
                            self.infer_block(block);
                        }
                    }
                }
                if let Some(else_block) = else_arm {
                    self.infer_block(else_block);
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    self.infer_block(&arm.body);
                }
                if let Some(else_block) = else_arm {
                    self.infer_block(else_block);
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                self.infer_block(body);
            }
            _ => {}
        }
    }

    fn maybe_downgrade_for_binding(&mut self, pattern: &Spanned<Pattern>, iterable: &Spanned<Expr>) {
        if let Some(&tid) = self.expr_types.get(&iterable.span) {
            if let ResolvedType::Generic(_, args) = self.types.get(tid) {
                if args.len() == 1 && self.is_any_string(args[0]) {
                    self.downgrade_pattern_bindings(pattern);
                }
            }
        }
    }

    fn downgrade_pattern_bindings(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.node {
            Pattern::Binding(name) => {
                if let Some(def_id) = self.find_binding_def(name, pattern.span) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(tid) = def.type_id {
                        if self.is_owned_string(tid) {
                            self.downgrade_to_view(def_id);
                        }
                    }
                }
            }
            Pattern::Tuple(pats) | Pattern::Constructor { fields: pats, .. } => {
                for p in pats {
                    self.downgrade_pattern_bindings(p);
                }
            }
            Pattern::Or(pats) => {
                for p in pats {
                    self.downgrade_pattern_bindings(p);
                }
            }
            _ => {}
        }
    }
}

/// Collect all return expressions from a list of statements.
fn collect_return_exprs<'a>(stmts: &'a [Spanned<Stmt>], out: &mut Vec<&'a Spanned<Expr>>) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Return(Some(expr)) => out.push(expr),
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                collect_return_exprs(&then_body.stmts, out);
                for (_, branch) in elif_branches {
                    collect_return_exprs(&branch.stmts, out);
                }
                if let Some(else_br) = else_body {
                    collect_return_exprs(&else_br.stmts, out);
                }
            }
            Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            Stmt::For { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            Stmt::Match { arms, else_arm, .. } => {
                for item in arms {
                    if let MatchItem::Arm(arm) = item {
                        if let Expr::Block(block) = &arm.body.node {
                            collect_return_exprs(&block.stmts, out);
                        }
                    }
                }
                if let Some(else_block) = else_arm {
                    collect_return_exprs(&else_block.stmts, out);
                }
            }
            Stmt::Select { arms, else_arm, .. } => {
                for arm in arms {
                    collect_return_exprs(&arm.body.stmts, out);
                }
                if let Some(else_block) = else_arm {
                    collect_return_exprs(&else_block.stmts, out);
                }
            }
            Stmt::With { body, .. } | Stmt::NamedScope { body, .. } | Stmt::Unsafe { body, .. } => {
                collect_return_exprs(&body.stmts, out);
            }
            _ => {}
        }
    }
}

/// Walk all items in the module. Process imported modules first so their
/// return types are resolved before local functions reference them.
fn infer_items(ctx: &mut ProvenanceCtx, items: &[Spanned<Item>]) {
    // Pass 1: imported modules (Item::Module wrappers from merge_modules)
    for item in items {
        if let Item::Module { items: inner, .. } = &item.node {
            infer_items(ctx, inner);
        }
    }
    // Pass 2: local items
    for item in items {
        match &item.node {
            Item::Module { .. } => {} // already processed
            Item::Function(f) => ctx.infer_function(f),
            Item::Equip(equip) => {
                for method in &equip.items {
                    ctx.infer_function(&method.node);
                }
            }
            Item::Trait(trait_def) => {
                for trait_item in &trait_def.items {
                    if let TraitItem::Method(f) = &trait_item.node {
                        ctx.infer_function(f);
                    }
                }
            }
            Item::Test(t) => ctx.infer_block(&t.body),
            Item::Bench(b) => ctx.infer_block(&b.body),
            Item::SuiteSetup(s) => ctx.infer_block(&s.body),
            Item::SuiteTeardown(s) => ctx.infer_block(&s.body),
            _ => {}
        }
    }
}
