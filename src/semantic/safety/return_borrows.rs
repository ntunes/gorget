use rustc_hash::{FxHashMap, FxHashSet};

use crate::parser::ast::*;
use crate::span::Spanned;

use crate::semantic::ids::DefId;
use crate::semantic::scope::DefKind;
use crate::semantic::resolve::{FunctionInfo, ResolutionMap};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::{self as types, TypeTable};

use super::{BorrowOrigin, BorrowCaptureMode, CaptureEntry};
use super::type_utils::all_spanned_items;

// ─── Pass 5a: Compute return_borrows_from ─────────────────

/// Compute `return_borrows_from` for each function by analyzing its body.
/// This is a lightweight pre-pass before the main borrow check.
pub(super) fn compute_all_return_borrows(
    module: &Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    ref_type_structs: &FxHashSet<DefId>,
) {
    for item in all_spanned_items(&module.items) {
        match &item.node {
            Item::Function(f) => {
                compute_function_return_borrows(f, scopes, types, resolution_map, function_info, ref_type_structs);
            }
            Item::Equip(impl_block) => {
                for method in &impl_block.items {
                    compute_function_return_borrows(&method.node, scopes, types, resolution_map, function_info, ref_type_structs);
                }
            }
            _ => {}
        }
    }
}

fn compute_function_return_borrows(
    func: &FunctionDef,
    scopes: &ScopeTable,
    types: &TypeTable,
    resolution_map: &ResolutionMap,
    function_info: &mut FxHashMap<DefId, FunctionInfo>,
    ref_type_structs: &FxHashSet<DefId>,
) {
    let def_id = match scopes.lookup(&func.name.node) {
        Some(id) => id,
        None => return,
    };

    let ret_type_id = match function_info.get(&def_id).and_then(|fi| fi.return_type_id) {
        Some(id) => id,
        None => return,
    };

    // Only relevant if the return type is a reference or callable type
    if !types::is_reference_type(ret_type_id, types, ref_type_structs)
        && !types::is_callable_type(ret_type_id, types)
    {
        return;
    }

    // Phase 3: Check for explicit `live` annotations first (they override body analysis).
    // Body analysis — trace return expressions back to params
    // Build a map from param names to their indices for this function
    let param_name_to_idx: FxHashMap<String, usize> = {
        let info = match function_info.get(&def_id) {
            Some(i) => i,
            None => return,
        };
        info.param_names.iter().enumerate()
            .map(|(i, name)| (name.clone(), i))
            .collect()
    };

    let mut borrows_from = FxHashSet::default();

    // Build local alias map for Block bodies (expression bodies have no locals)
    let local_aliases = match &func.body {
        FunctionBody::Block(block) => build_local_alias_map(block, &param_name_to_idx, &*function_info, resolution_map, scopes),
        _ => LocalAliasMap::default(),
    };

    // Shared reborrow for trace functions (compute_function_return_borrows holds &mut)
    let fi_ref: &FxHashMap<DefId, FunctionInfo> = &*function_info;

    match &func.body {
        FunctionBody::Expression(expr) => {
            trace_expr_to_params(expr, &param_name_to_idx, &local_aliases, fi_ref, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Block(block) => {
            trace_block_returns_to_params(block, &param_name_to_idx, &local_aliases, fi_ref, resolution_map, scopes, &mut borrows_from);
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // No body — apply elision rules
            let info = match function_info.get(&def_id) {
                Some(i) => i,
                None => return,
            };
            let ref_param_indices: Vec<usize> = info.param_type_ids.iter()
                .enumerate()
                .filter(|(_, tid)| tid.map_or(false, |id|
                    types::is_reference_type(id, types, ref_type_structs)
                    || types::is_callable_type(id, types)))
                .map(|(i, _)| i)
                .collect();
            if ref_param_indices.len() == 1 {
                borrows_from.insert(ref_param_indices[0]);
            } else if !info.param_names.is_empty() && info.param_names[0] == "self" {
                borrows_from.insert(0);
            } else if ref_param_indices.is_empty() {
                // No reference-type params → return can't borrow from any param.
                // Mark as static so callers don't get Unknown origin.
                if let Some(fi) = function_info.get_mut(&def_id) {
                    fi.return_origin_is_static = true;
                }
                return;
            }
        }
    }

    if !borrows_from.is_empty() {
        let mut result: Vec<usize> = borrows_from.into_iter().collect();
        result.sort();
        if let Some(fi) = function_info.get_mut(&def_id) {
            fi.return_borrows_from = result;
        }
    } else {
        // Elision fallback for functions with bodies that didn't trace to any param
        let info = match function_info.get(&def_id) {
            Some(i) => i,
            None => return,
        };
        let ref_param_indices: Vec<usize> = info.param_type_ids.iter()
            .enumerate()
            .filter(|(_, tid)| tid.map_or(false, |id|
                types::is_reference_type(id, types, ref_type_structs)
                || types::is_callable_type(id, types)))
            .map(|(i, _)| i)
            .collect();
        if ref_param_indices.len() == 1 {
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_borrows_from = ref_param_indices;
            }
        } else if !info.param_names.is_empty() && info.param_names[0] == "self" {
            // Method with &self — borrows from self
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_borrows_from = vec![0];
            }
        }

        // If function has a body and return_borrows_from is still empty after
        // body analysis + elision, the return is provably static.
        let info = function_info.get(&def_id).unwrap();
        if info.has_body && info.return_borrows_from.is_empty() {
            if let Some(fi) = function_info.get_mut(&def_id) {
                fi.return_origin_is_static = true;
            }
        }
    }
}

// ─── Visitor: Captured Reference Origin Collector ─────────────

/// Walks a closure body collecting origins of captured reference-type variables.
/// Skips nested closures (own capture scope) and closure parameters.
pub(super) struct CapturedRefOriginCollector<'a> {
    pub(super) resolution_map: &'a ResolutionMap,
    pub(super) scopes: &'a ScopeTable,
    pub(super) types: &'a TypeTable,
    pub(super) ref_type_structs: &'a FxHashSet<DefId>,
    pub(super) var_origins: &'a FxHashMap<DefId, BorrowOrigin>,
    pub(super) param_names: &'a FxHashSet<&'a str>,
    pub(super) origins: Vec<BorrowOrigin>,
}

impl crate::parser::visitor::ExprVisitor for CapturedRefOriginCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                if self.param_names.contains(name.as_str()) {
                    return;
                }
                if let Some(&def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(def_id);
                    if def.kind == DefKind::Variable {
                        if let Some(type_id) = def.type_id {
                            if types::is_reference_type(type_id, self.types, self.ref_type_structs)
                                || types::is_callable_type(type_id, self.types)
                            {
                                if let Some(origin) = self.var_origins.get(&def_id) {
                                    self.origins.push(origin.clone());
                                }
                            }
                        }
                    }
                }
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Default walk handles all other variants exhaustively
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    // visit_stmt and visit_block: use default walk_stmt/walk_block.
    // This covers Stmt::With, Assert, and all other statement variants
    // that the previous manual walker missed with its `_ => {}` catch-all.
}

// ─── Visitor: Captured Mutation Collector ─────────────────────

/// Walks a closure body collecting names of variables that are mutated
/// (assigned, compound-assigned, receiver of a `&self` method call, or
/// passed as an `&` call/method arg — contractually a write under D31)
/// inside the body. Excludes locals declared inside the closure itself.
pub(super) struct CapturedMutationCollector<'a> {
    pub(super) locals: FxHashSet<String>,
    pub(super) mutated: FxHashSet<String>,
    /// Method call span start → DefId (for checking if method takes &self).
    pub(super) method_resolutions: &'a FxHashMap<usize, super::super::MethodResolution>,
    /// Function/method info (for checking param_ownerships[0] == MutableBorrow).
    pub(super) function_info: &'a FxHashMap<DefId, FunctionInfo>,
}

/// Extract the root identifier name from a nested FieldAccess/Index chain.
fn extract_root_name(expr: &Expr) -> Option<&str> {
    let mut e = expr;
    loop {
        match e {
            Expr::Identifier(name) => return Some(name),
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                e = &object.node;
            }
            _ => return None,
        }
    }
}

impl crate::parser::visitor::ExprVisitor for CapturedMutationCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            // Skip nested closures — they have their own mutation scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Detect method calls that take &self (mutable borrow) as mutations.
            // Also detect any `&`-sigil args (contractually a write under D31).
            Expr::MethodCall { receiver, method, args, .. } => {
                if let Some(method_def_id) = self.method_resolutions.get(&method.span.start).and_then(|r| r.def_id) {
                    if let Some(info) = self.function_info.get(&method_def_id) {
                        if info.param_ownerships.first() == Some(&Ownership::MutableBorrow) {
                            if let Some(name) = extract_root_name(&receiver.node) {
                                if !self.locals.contains(name) {
                                    self.mutated.insert(name.to_string());
                                }
                            }
                        }
                    }
                }
                for arg in args {
                    if arg.node.ownership == Ownership::MutableBorrow {
                        if let Some(name) = extract_root_name(&arg.node.value.node) {
                            if !self.locals.contains(name) {
                                self.mutated.insert(name.to_string());
                            }
                        }
                    }
                }
                crate::parser::visitor::walk_expr(self, expr);
            }
            // Detect `&`-sigil call args (contractually a write under D31).
            Expr::Call { args, .. } => {
                for arg in args {
                    if arg.node.ownership == Ownership::MutableBorrow {
                        if let Some(name) = extract_root_name(&arg.node.value.node) {
                            if !self.locals.contains(name) {
                                self.mutated.insert(name.to_string());
                            }
                        }
                    }
                }
                crate::parser::visitor::walk_expr(self, expr);
            }
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl { pattern, .. } => {
                if let Pattern::Binding(name) = &pattern.node {
                    self.locals.insert(name.clone());
                }
            }
            Stmt::Assign { target, .. } | Stmt::CompoundAssign { target, .. } => {
                // Extract root identifier from assignment target
                if let Some(name) = extract_root_name(&target.node) {
                    if !self.locals.contains(name) {
                        self.mutated.insert(name.to_string());
                    }
                }
            }
            _ => {}
        }
        crate::parser::visitor::walk_stmt(self, stmt);
    }
}

// ─── Visitor: Capture Set Collector ──────────────────────────

/// Walks a closure body collecting ALL free variables (not just reference-typed
/// ones like `CapturedRefOriginCollector`). For each captured variable, records
/// its name, DefId, capture mode (Read/Mutable), and whether it has a borrowed
/// origin. Used by spawn enforcement to decide if a closure is safe to spawn.
pub(super) struct CaptureSetCollector<'a> {
    pub(super) resolution_map: &'a ResolutionMap,
    pub(super) scopes: &'a ScopeTable,
    pub(super) types: &'a TypeTable,
    pub(super) ref_type_structs: &'a FxHashSet<DefId>,
    pub(super) var_origins: &'a FxHashMap<DefId, BorrowOrigin>,
    pub(super) param_names: &'a FxHashSet<&'a str>,
    pub(super) local_names: FxHashSet<String>,
    pub(super) seen: FxHashSet<DefId>,
    pub(super) captures: Vec<CaptureEntry>,
    pub(super) mutated_names: &'a FxHashSet<String>,
}

impl crate::parser::visitor::ExprVisitor for CaptureSetCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                // Skip closure params and locals declared inside the closure
                if self.param_names.contains(name.as_str())
                    || self.local_names.contains(name.as_str())
                {
                    return;
                }
                // Resolve DefId
                let Some(&def_id) = self.resolution_map.get(&expr.span.start) else {
                    return;
                };
                let def = self.scopes.get_def(def_id);
                if def.kind != DefKind::Variable {
                    return;
                }
                // Dedup
                if !self.seen.insert(def_id) {
                    return;
                }

                let mode = if self.mutated_names.contains(name.as_str()) {
                    BorrowCaptureMode::Mutable
                } else {
                    BorrowCaptureMode::Read
                };

                // Determine if this variable has a borrowed origin
                let has_borrowed_origin = if let Some(origin) = self.var_origins.get(&def_id) {
                    !matches!(origin, BorrowOrigin::Static)
                } else if let Some(type_id) = def.type_id {
                    // No explicit origin tracked — check if the type is inherently
                    // reference-like (str, &T, etc.). If so, treat as borrowed
                    // conservatively (the origin wasn't tracked, so we can't prove
                    // it's Static).
                    types::is_reference_type(type_id, self.types, self.ref_type_structs)
                } else {
                    false
                };

                self.captures.push(CaptureEntry {
                    def_id,
                    name: name.clone(),
                    mode,
                    has_borrowed_origin,
                });
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) {
        // Track locals declared inside the closure body so they aren't
        // counted as captures.
        if let Stmt::VarDecl { pattern, .. } = &stmt.node {
            if let Pattern::Binding(name) = &pattern.node {
                self.local_names.insert(name.clone());
            }
        }
        crate::parser::visitor::walk_stmt(self, stmt);
    }
}

// ─── Visitor: Closure Body Param Tracer ──────────────────────

/// Walks a closure body to find references to enclosing function parameters.
/// Skips nested closures (they have their own capture scope).
pub(super) struct ClosureBodyParamTracer<'a> {
    pub(super) outer_params: &'a FxHashMap<String, usize>,
    pub(super) outer_aliases: &'a LocalAliasMap,
    pub(super) closure_params: &'a FxHashSet<&'a str>,
    pub(super) result: &'a mut FxHashSet<usize>,
}

impl crate::parser::visitor::ExprVisitor for ClosureBodyParamTracer<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                if !self.closure_params.contains(name.as_str()) {
                    if let Some(&idx) = self.outer_params.get(name) {
                        self.result.insert(idx);
                    } else if let Some(indices) = self.outer_aliases.get(name) {
                        self.result.extend(indices);
                    }
                }
            }
            // Skip nested closures — they have their own capture scope
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            // Default walk handles all other variants exhaustively
            _ => crate::parser::visitor::walk_expr(self, expr),
        }
    }

    // visit_stmt and visit_block: use default walk_stmt/walk_block.
    // This covers all statement variants exhaustively, fixing coverage gaps
    // in the previous manual trace_closure_body_stmts (which missed Assign,
    // CompoundAssign, For, While, Loop, Match, With, Unsafe, Assert, etc.).
}

// ─── Local Alias Map ─────────────────────────────────────────

/// Maps local variable names to the set of param indices their values may originate from.
/// Over-approximates via union: assignments in different branches are merged.
pub(super) type LocalAliasMap = FxHashMap<String, FxHashSet<usize>>;

/// Build a map from local variable names to the param indices they may alias.
/// Walks all statements in the function body before return-tracing begins.
fn build_local_alias_map(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) -> LocalAliasMap {
    let mut aliases = LocalAliasMap::default();
    build_aliases_from_block(block, param_names, &mut aliases, function_info, resolution_map, scopes);
    aliases
}

fn build_aliases_from_block(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    aliases: &mut LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) {
    for stmt in &block.stmts {
        build_aliases_from_stmt(&stmt.node, param_names, aliases, function_info, resolution_map, scopes);
    }
}

fn build_aliases_from_stmt(
    stmt: &Stmt,
    param_names: &FxHashMap<String, usize>,
    aliases: &mut LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) {
    match stmt {
        Stmt::VarDecl { pattern, value, .. } => {
            if let Pattern::Binding(name) = &pattern.node {
                let indices = collect_param_indices(&value.node, param_names, aliases, function_info, resolution_map, scopes);
                if !indices.is_empty() {
                    aliases.entry(name.clone()).or_default().extend(indices);
                }
            }
        }
        Stmt::Assign { target, value } => {
            if let Expr::Identifier(name) = &target.node {
                // Skip params — they already have direct entries
                if !param_names.contains_key(name) {
                    let indices = collect_param_indices(&value.node, param_names, aliases, function_info, resolution_map, scopes);
                    if !indices.is_empty() {
                        // Union with existing (conservative for reassignment)
                        aliases.entry(name.clone()).or_default().extend(indices);
                    }
                }
            }
        }
        // Recurse into control flow — over-approximate by unioning all branches
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            build_aliases_from_block(then_body, param_names, aliases, function_info, resolution_map, scopes);
            for (_, body) in elif_branches {
                build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
            }
            if let Some(else_body) = else_body {
                build_aliases_from_block(else_body, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms.iter().filter_map(|i| i.arm()) {
                if let Expr::Block(block) = &arm.body.node {
                    build_aliases_from_block(block, param_names, aliases, function_info, resolution_map, scopes);
                }
            }
            if let Some(else_arm) = else_arm {
                build_aliases_from_block(else_arm, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                build_aliases_from_block(&arm.body, param_names, aliases, function_info, resolution_map, scopes);
            }
            if let Some(else_arm) = else_arm {
                build_aliases_from_block(else_arm, param_names, aliases, function_info, resolution_map, scopes);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body } => {
            build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
        }
        Stmt::With { body, .. } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            build_aliases_from_block(body, param_names, aliases, function_info, resolution_map, scopes);
        }
        _ => {}
    }
}

/// Trace an expression to the set of param indices it may originate from.
/// Consults both `param_names` (direct params) and `aliases` (local variables).
fn collect_param_indices(
    expr: &Expr,
    param_names: &FxHashMap<String, usize>,
    aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
) -> FxHashSet<usize> {
    let mut result = FxHashSet::default();
    match expr {
        Expr::Identifier(name) => {
            if let Some(&idx) = param_names.get(name) {
                result.insert(idx);
            } else if let Some(indices) = aliases.get(name) {
                result.extend(indices);
            }
        }
        Expr::SelfExpr => {
            if param_names.contains_key("self") {
                result.insert(0);
            }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            result.extend(collect_param_indices(&object.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Index { object, .. } => {
            result.extend(collect_param_indices(&object.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Call { callee, args, .. } => {
            // Resolve callee and look up return_borrows_from
            let callee_def_id = match &callee.node {
                Expr::Identifier(_) => resolution_map.get(&callee.span.start).copied(),
                Expr::Path { segments } => segments.first().and_then(|s| resolution_map.get(&s.span.start).copied()),
                _ => None,
            };
            if let Some(def_id) = callee_def_id {
                if let Some(info) = function_info.get(&def_id) {
                    if !info.return_borrows_from.is_empty() {
                        for &idx in &info.return_borrows_from {
                            if let Some(arg) = args.get(idx) {
                                result.extend(collect_param_indices(&arg.node.value.node, param_names, aliases, function_info, resolution_map, scopes));
                            }
                        }
                        return result;
                    }
                }
            }
            // Callee not resolved or no return_borrows_from — no info
        }
        Expr::If { then_branch, elif_branches, else_branch, .. } => {
            result.extend(collect_param_indices(&then_branch.node, param_names, aliases, function_info, resolution_map, scopes));
            for (_, body) in elif_branches {
                result.extend(collect_param_indices(&body.node, param_names, aliases, function_info, resolution_map, scopes));
            }
            if let Some(else_br) = else_branch {
                result.extend(collect_param_indices(&else_br.node, param_names, aliases, function_info, resolution_map, scopes));
            }
        }
        Expr::DefaultOp { lhs, rhs } => {
            result.extend(collect_param_indices(&lhs.node, param_names, aliases, function_info, resolution_map, scopes));
            result.extend(collect_param_indices(&rhs.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::Deref { expr: inner } => {
            result.extend(collect_param_indices(&inner.node, param_names, aliases, function_info, resolution_map, scopes));
        }
        _ => {}
    }
    result
}

/// Trace a return expression back through variable assignments to find which params flow to it.
fn trace_expr_to_params(
    expr: &Spanned<Expr>,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match &expr.node {
        Expr::Identifier(name) => {
            if let Some(&idx) = param_names.get(name) {
                result.insert(idx);
            } else if let Some(indices) = local_aliases.get(name) {
                result.extend(indices);
            }
        }

        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            trace_expr_to_params(object, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Index { object, .. } => {
            trace_expr_to_params(object, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::If { then_branch, elif_branches, else_branch, .. } => {
            trace_expr_to_params(then_branch, param_names, local_aliases, function_info, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_expr_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_br) = else_branch {
                trace_expr_to_params(else_br, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }

        Expr::Block(block) | Expr::Do { body: block, .. } => {
            if let Some(last) = block.stmts.last() {
                if let Stmt::Expr(e) = &last.node {
                    trace_expr_to_params(e, param_names, local_aliases, function_info, resolution_map, scopes, result);
                }
            }
        }

        Expr::SelfExpr => {
            if param_names.contains_key("self") {
                result.insert(0);
            }
        }

        Expr::StructLiteral { args, .. } => {
            for arg in args {
                trace_expr_to_params(arg, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }

        Expr::Call { callee, args, .. } => {
            // Resolve callee and trace through its return_borrows_from
            let callee_def_id = match &callee.node {
                Expr::Identifier(_) => resolution_map.get(&callee.span.start).copied(),
                Expr::Path { segments } => segments.first().and_then(|s| resolution_map.get(&s.span.start).copied()),
                _ => None,
            };
            if let Some(def_id) = callee_def_id {
                if let Some(info) = function_info.get(&def_id) {
                    if !info.return_borrows_from.is_empty() {
                        for &idx in &info.return_borrows_from {
                            if let Some(arg) = args.get(idx) {
                                trace_expr_to_params(&arg.node.value, param_names, local_aliases, function_info, resolution_map, scopes, result);
                            }
                        }
                    }
                }
            }
        }

        Expr::DefaultOp { lhs, rhs } => {
            trace_expr_to_params(lhs, param_names, local_aliases, function_info, resolution_map, scopes, result);
            trace_expr_to_params(rhs, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Move { expr: inner } | Expr::Propagate { expr: inner } | Expr::Deref { expr: inner } => {
            trace_expr_to_params(inner, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }

        Expr::Closure { body, params, .. } => {
            use crate::parser::visitor::ExprVisitor;
            let closure_param_names: FxHashSet<&str> = params.iter()
                .map(|p| p.node.name.node.as_str())
                .collect();
            let mut tracer = ClosureBodyParamTracer {
                outer_params: param_names,
                outer_aliases: local_aliases,
                closure_params: &closure_param_names,
                result,
            };
            tracer.visit_expr(body);
        }

        _ => {}
    }
}

/// Walk a block looking for Return statements and trace them to params.
fn trace_block_returns_to_params(
    block: &Block,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    for stmt in &block.stmts {
        trace_stmt_returns_to_params(&stmt.node, param_names, local_aliases, function_info, resolution_map, scopes, result);
    }
}

fn trace_stmt_returns_to_params(
    stmt: &Stmt,
    param_names: &FxHashMap<String, usize>,
    local_aliases: &LocalAliasMap,
    function_info: &FxHashMap<DefId, FunctionInfo>,
    resolution_map: &ResolutionMap,
    scopes: &ScopeTable,
    result: &mut FxHashSet<usize>,
) {
    match stmt {
        Stmt::Return(Some(expr)) => {
            trace_expr_to_params(expr, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            trace_block_returns_to_params(then_body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            for (_, body) in elif_branches {
                trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_body) = else_body {
                trace_block_returns_to_params(else_body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::Match { arms, else_arm, .. } => {
            for arm in arms.iter().filter_map(|i| i.arm()) {
                if let Expr::Block(block) = &arm.body.node {
                    trace_block_returns_to_params(block, param_names, local_aliases, function_info, resolution_map, scopes, result);
                }
            }
            if let Some(else_arm) = else_arm {
                trace_block_returns_to_params(else_arm, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::Select { arms, else_arm } => {
            for arm in arms {
                trace_block_returns_to_params(&arm.body, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
            if let Some(else_arm) = else_arm {
                trace_block_returns_to_params(else_arm, param_names, local_aliases, function_info, resolution_map, scopes, result);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body } => {
            trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        Stmt::With { body, .. } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            trace_block_returns_to_params(body, param_names, local_aliases, function_info, resolution_map, scopes, result);
        }
        _ => {}
    }
}
