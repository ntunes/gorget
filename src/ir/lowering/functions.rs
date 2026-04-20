use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr, FunctionBody, FunctionDef, GenericParam, Ownership, Stmt, Type};
use crate::semantic::meta::{self as meta, DelayedMetaContext, MetaValue};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;
use super::generics;
use super::stmts::lower_block;

/// Pre-scan a function body to find variable names unsafe for CoW aliasing:
/// reassigned, !-moved, or used as RHS for Move-type VarDecls.
/// CoW skips aliasing for these because the LIR can't change local types
/// mid-function (static ref_locals), and MoveZero on a source invalidates aliases.
fn prescan_cow_unsafe_names(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashSet<String> {
    let mut declared: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    let mut unsafe_names: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    prescan_block(body, &mut declared, &mut unsafe_names);
    unsafe_names
}

fn prescan_block(
    stmts: &[Spanned<Stmt>],
    declared: &mut rustc_hash::FxHashSet<String>,
    unsafe_names: &mut rustc_hash::FxHashSet<String>,
) {
    use crate::parser::ast::Pattern;
    for stmt in stmts {
        match &stmt.node {
            Stmt::VarDecl { pattern, value, .. } => {
                if let Pattern::Binding(name) = &pattern.node {
                    declared.insert(name.clone());
                }
                // Scan for !-moved args in the value expression
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::Assign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if declared.contains(name.as_str()) {
                        unsafe_names.insert(name.clone());
                    }
                }
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if declared.contains(name.as_str()) {
                        unsafe_names.insert(name.clone());
                    }
                }
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::Expr(expr) => {
                prescan_expr_moves(&expr.node, declared, unsafe_names);
            }
            Stmt::Return(Some(expr)) => {
                prescan_expr_moves(&expr.node, declared, unsafe_names);
            }
            Stmt::While { body, else_body, .. } => {
                prescan_block(&body.stmts, declared, unsafe_names);
                if let Some(eb) = else_body {
                    prescan_block(&eb.stmts, declared, unsafe_names);
                }
            }
            Stmt::For { body, .. } => {
                prescan_block(&body.stmts, declared, unsafe_names);
            }
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                prescan_block(&then_body.stmts, declared, unsafe_names);
                for (_, branch_body) in elif_branches {
                    prescan_block(&branch_body.stmts, declared, unsafe_names);
                }
                if let Some(eb) = else_body {
                    prescan_block(&eb.stmts, declared, unsafe_names);
                }
            }
            _ => {}
        }
    }
}

/// Scan an expression for `!name` (Move) arguments in function/method calls.
fn prescan_expr_moves(
    expr: &Expr,
    declared: &rustc_hash::FxHashSet<String>,
    unsafe_names: &mut rustc_hash::FxHashSet<String>,
) {
    match expr {
        Expr::Call { args, callee, .. } => {
            prescan_expr_moves(&callee.node, declared, unsafe_names);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        if declared.contains(name.as_str()) {
                            unsafe_names.insert(name.clone());
                        }
                    }
                }
                prescan_expr_moves(&arg.node.value.node, declared, unsafe_names);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            prescan_expr_moves(&receiver.node, declared, unsafe_names);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        if declared.contains(name.as_str()) {
                            unsafe_names.insert(name.clone());
                        }
                    }
                }
                prescan_expr_moves(&arg.node.value.node, declared, unsafe_names);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            prescan_expr_moves(&left.node, declared, unsafe_names);
            prescan_expr_moves(&right.node, declared, unsafe_names);
        }
        _ => {}
    }
}

/// Flow-sensitive CoW analysis: for each statement, compute the set of variable names
/// that are reassigned or !-moved on any forward execution path.
///
/// Algorithm: reverse walk of the AST (same shape as liveness analysis). At each
/// statement, the "reassigned-after" set is the accumulation of all reassignments
/// that appear textually after it. At branches (if/else), the sets are unioned
/// (any-path: if a name is reassigned in any branch, it's unsafe for CoW).
/// At loops, the loop body's reassignments are included (conservative but correct).
fn compute_cow_reassigned_after(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<String>> {
    let mut result = rustc_hash::FxHashMap::default();
    let mut future = rustc_hash::FxHashSet::default();
    cow_after_block(body, &mut future, &mut result);
    result
}

fn cow_after_block(
    stmts: &[Spanned<Stmt>],
    future: &mut rustc_hash::FxHashSet<String>,
    result: &mut rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<String>>,
) {
    for stmt in stmts.iter().rev() {
        // Record the "reassigned-after" set at this statement's position
        result.insert(stmt.span.start, future.clone());
        // Collect reassignments from this statement
        cow_after_stmt(&stmt.node, future, result);
    }
}

fn cow_after_stmt(
    stmt: &Stmt,
    future: &mut rustc_hash::FxHashSet<String>,
    result: &mut rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<String>>,
) {
    match stmt {
        Stmt::VarDecl { value, .. } => {
            cow_after_expr_moves(&value.node, future);
        }
        Stmt::Assign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                future.insert(name.clone());
            }
            cow_after_expr_moves(&value.node, future);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                future.insert(name.clone());
            }
            cow_after_expr_moves(&value.node, future);
        }
        Stmt::Expr(expr) => {
            cow_after_expr_moves(&expr.node, future);
        }
        Stmt::Return(Some(expr)) => {
            cow_after_expr_moves(&expr.node, future);
        }
        Stmt::While { body, else_body, .. } => {
            // Loop body reassignments are visible to statements before the loop
            cow_after_block(&body.stmts, future, result);
            if let Some(eb) = else_body {
                cow_after_block(&eb.stmts, future, result);
            }
        }
        Stmt::For { body, .. } => {
            cow_after_block(&body.stmts, future, result);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            // Union all branches: if a name is reassigned in any branch, it's unsafe
            let saved = future.clone();
            cow_after_block(&then_body.stmts, future, result);
            let then_set = future.clone();
            *future = saved.clone();
            for (_, branch_body) in elif_branches {
                cow_after_block(&branch_body.stmts, future, result);
                // Accumulate into future (union)
            }
            if let Some(eb) = else_body {
                cow_after_block(&eb.stmts, future, result);
            }
            // Union: include then-branch reassignments too
            future.extend(then_set);
        }
        Stmt::Match { arms, else_arm, .. } => {
            let saved = future.clone();
            for item in arms {
                if let crate::parser::ast::MatchItem::Arm(arm) = item {
                    let mut branch = saved.clone();
                    if let Expr::Block(block) = &arm.body.node {
                        cow_after_block(&block.stmts, &mut branch, result);
                    }
                    future.extend(branch);
                }
            }
            if let Some(b) = else_arm {
                let mut branch = saved;
                cow_after_block(&b.stmts, &mut branch, result);
                future.extend(branch);
            }
        }
        _ => {}
    }
}

/// Collect !-moved names from expressions (same as prescan_expr_moves but
/// inserts into the forward-reassigned set).
fn cow_after_expr_moves(
    expr: &Expr,
    future: &mut rustc_hash::FxHashSet<String>,
) {
    match expr {
        Expr::Call { args, callee, .. } => {
            cow_after_expr_moves(&callee.node, future);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        future.insert(name.clone());
                    }
                }
                cow_after_expr_moves(&arg.node.value.node, future);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            cow_after_expr_moves(&receiver.node, future);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        future.insert(name.clone());
                    }
                }
                cow_after_expr_moves(&arg.node.value.node, future);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            cow_after_expr_moves(&left.node, future);
            cow_after_expr_moves(&right.node, future);
        }
        _ => {}
    }
}

/// Pre-scan: count how many times each declared name is USED (read) in the function body.
/// Names with exactly 1 use-site (beyond their declaration) are "single-use" — they can
/// be auto-moved at push/constructor sites instead of cloned.
fn prescan_name_use_counts(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashMap<String, u32> {
    let mut counts: rustc_hash::FxHashMap<String, u32> = rustc_hash::FxHashMap::default();
    count_uses_in_block(body, &mut counts);
    counts
}

fn count_uses_in_block(stmts: &[Spanned<Stmt>], counts: &mut rustc_hash::FxHashMap<String, u32>) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::VarDecl { value, .. } => {
                count_uses_in_expr(&value.node, counts);
            }
            Stmt::Assign { target, value, .. } => {
                count_uses_in_expr(&target.node, counts);
                count_uses_in_expr(&value.node, counts);
            }
            Stmt::Return(Some(expr)) => count_uses_in_expr(&expr.node, counts),
            Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
                count_uses_in_expr(&condition.node, counts);
                count_uses_in_block(&then_body.stmts, counts);
                for (cond, body) in elif_branches {
                    count_uses_in_expr(&cond.node, counts);
                    count_uses_in_block(&body.stmts, counts);
                }
                if let Some(body) = else_body {
                    count_uses_in_block(&body.stmts, counts);
                }
            }
            Stmt::While { condition, body, .. } => {
                count_uses_in_expr(&condition.node, counts);
                count_uses_in_block(&body.stmts, counts);
            }
            Stmt::For { iterable, body, .. } => {
                count_uses_in_expr(&iterable.node, counts);
                count_uses_in_block(&body.stmts, counts);
            }
            Stmt::Match { scrutinee, arms, else_arm, .. } => {
                count_uses_in_expr(&scrutinee.node, counts);
                for item in arms {
                    if let crate::parser::ast::MatchItem::Arm(arm) = item {
                        count_uses_in_expr(&arm.body.node, counts);
                    }
                }
                if let Some(body) = else_arm {
                    count_uses_in_block(&body.stmts, counts);
                }
            }
            Stmt::Expr(expr) => count_uses_in_expr(&expr.node, counts),
            _ => {}
        }
    }
}

fn count_uses_in_expr(expr: &Expr, counts: &mut rustc_hash::FxHashMap<String, u32>) {
    match expr {
        Expr::Identifier(name) => {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
        Expr::Call { callee, args, .. } => {
            count_uses_in_expr(&callee.node, counts);
            for arg in args { count_uses_in_expr(&arg.node.value.node, counts); }
        }
        Expr::MethodCall { receiver, args, .. } => {
            count_uses_in_expr(&receiver.node, counts);
            for arg in args { count_uses_in_expr(&arg.node.value.node, counts); }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            count_uses_in_expr(&object.node, counts);
        }
        Expr::Index { object, index, .. } => {
            count_uses_in_expr(&object.node, counts);
            count_uses_in_expr(&index.node, counts);
        }
        Expr::BinaryOp { left, right, .. } => {
            count_uses_in_expr(&left.node, counts);
            count_uses_in_expr(&right.node, counts);
        }
        Expr::UnaryOp { operand, .. } | Expr::Move { expr: operand } => {
            count_uses_in_expr(&operand.node, counts);
        }
        Expr::Block(block) => count_uses_in_block(&block.stmts, counts),
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            count_uses_in_expr(&condition.node, counts);
            count_uses_in_expr(&then_branch.node, counts);
            for (cond, body) in elif_branches {
                count_uses_in_expr(&cond.node, counts);
                count_uses_in_expr(&body.node, counts);
            }
            if let Some(body) = else_branch {
                count_uses_in_expr(&body.node, counts);
            }
        }
        Expr::StringLiteral(_) => {} // f-string interpolations don't affect move analysis
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            count_uses_in_expr(&body.node, counts);
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            count_uses_in_expr(&scrutinee.node, counts);
            for arm in arms { count_uses_in_expr(&arm.body.node, counts); }
            if let Some(body) = else_arm { count_uses_in_expr(&body.node, counts); }
        }
        _ => {} // literals, self, etc.
    }
}

/// Lower a single function definition into the GIR module.
///
/// `name_override` — when `Some`, use this as the GIR/C function name instead of
/// `func.name.node`.  Used by module-scoped name mangling (Phase 5) so that functions
/// from non-entry modules get their module-path prefix in the emitted C symbol while
/// the rest of the lowering logic (body, params, drops) remains unchanged.
pub fn lower_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    func: &FunctionDef,
    name_override: Option<&str>,
) {
    let func_span = func.span;
    let name: &str = name_override.unwrap_or(func.name.node.as_str());
    let is_main = name == "main";

    // Map return type — use fn_sigs if available (handles `throws` → Result)
    let return_type = if is_main && func.throws.is_none() {
        I32_TYPE
    } else if let Some((_, ret_ty)) = ctx.fn_sigs.get(name) {
        *ret_ty
    } else {
        ctx.type_mapper.map_ast_type(&func.return_type.node)
    };

    // Map parameters — MutableBorrow params become MutPtr types;
    // Resource-type params are passed by pointer (const Ptr for bare, MutPtr for &)
    let params: Vec<(TypeId, Option<&str>)> = func
        .params
        .iter()
        .map(|p| {
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            let param_name = p.node.name.node.as_str();
            (gir_type, Some(param_name))
        })
        .collect();

    // Register standalone function signature in fn_sigs with BASE types
    // (before pointer wrapping) so callers can detect resource-type parameters
    // and forward pointers instead of copying structs.
    if !ctx.fn_sigs.contains_key(name) {
        let base_param_types: Vec<TypeId> = func
            .params
            .iter()
            .map(|p| ctx.type_mapper.map_ast_type(&p.node.type_.node))
            .collect();
        ctx.fn_sigs.insert(name.to_string(), (base_param_types, return_type));
    }

    let mut builder = FunctionBuilder::new(name.to_string(), return_type, &params);

    // Clear and register locals for this function
    ctx.clear_locals();
    ctx.func_state.current_fn_name = name.to_string();

    // Register parameters as locals
    ctx.func_state.callable_return_types.clear();
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32); // _1, _2, ...
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            // Bare-borrow resource param: Ptr, no auto-deref, clone-on-mutation
            ctx.set_bare_param(local_id);
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            // ! resource params and & trivial params: MutPtr, auto-deref + write-through
            ctx.func_state.mut_capture_locals.insert(local_id, base_type);
        }
        // ! string params: caller transfers ownership — mark as owned
        // so clone_resource_args_for_init skips the clone.
        if ctx.type_mapper.is_string_type(base_type)
            && matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
        {
            ctx.set_owned(local_id);
        }
        // Track callable parameter return types
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &[], ctx) {
            ctx.func_state.callable_return_types.insert(local_id, ret_type);
        }
    }

    // Track throws context for Result wrapping in return/throw
    ctx.func_state.current_throws_result_type = if func.throws.is_some() {
        Some(return_type)
    } else {
        None
    };

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator so that ref-counted
    // types (Channel, Shared, Weak) passed by value are released at scope exit.
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32);
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
    }

    // Pre-scan: find variables unsafe for CoW (reassigned, !-moved).
    // Also count name uses and compute liveness for auto-move (Phase 1f).
    if let FunctionBody::Block(block) = &func.body {
        ctx.func_state.cow_reassigned_names = prescan_cow_unsafe_names(&block.stmts);
        ctx.func_state.cow_reassigned_after = compute_cow_reassigned_after(&block.stmts);
        ctx.func_state.name_use_counts = prescan_name_use_counts(&block.stmts);
        ctx.func_state.liveness = super::liveness::compute_function_liveness(&block.stmts);
    }

    // NOTE: move_override_params is NOT used for non-generic functions.
    // The callee-side move-through-Ptr optimization is only safe when the
    // CALLER's argument is also last-use, which the callee cannot verify.
    // This optimization exists in lower_generic_function for historical reasons
    // but is a known soundness concern — it zeroes the caller's slot through
    // the Ptr, corrupting the caller's data if used after the call.
    // The safe approach (caller-side drop suppression) is deferred to Phase 2.

    // Lower the body
    match &func.body {
        FunctionBody::Block(block) => {
            // Run delayed meta expansion (e.g. `meta for` inside match arms using
            // variant_payloads(T)) for non-generic functions.  For generic functions
            // this is done inside lower_generic_function with type substitutions.
            let expanded_block;
            let block = {
                let empty_subs: Vec<(String, crate::parser::ast::Type)> = vec![];
                let empty_env = rustc_hash::FxHashMap::default();
                let delayed_ctx = DelayedMetaContext {
                    type_subs:      &empty_subs,
                    features:       &[],
                    meta_env:       &empty_env,
                    items:          &[],
                    trait_registry: &ctx.analysis.traits,
                    type_registry:  &ctx.type_registry,
                };
                let mut meta_errors = Vec::new();
                let mut cloned = block.clone();
                meta::evaluate_delayed_meta_block(&mut cloned, &delayed_ctx, &mut meta_errors);
                for e in &meta_errors {
                    eprintln!("[delayed-meta fn] {e:?}");
                }
                expanded_block = cloned;
                &expanded_block
            };
            lower_block(ctx, &mut builder, block);

            // Add implicit return if the last block has no terminator
            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                // P2.6: Emit scope drops before implicit return
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if is_main && func.throws.is_none() {
                    builder.assign(
                        Place::local(LocalId(0)),
                        FunctionBuilder::const_i32(0),
                    );
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if is_main && func.throws.is_some() {
                    // throws main: implicit success → Ok(unit) wrapped in Result
                    let type_name = ctx.type_registry.type_name(return_type)
                        .unwrap_or_else(|| "Result".to_string());
                    let ok_val = builder.enum_init(
                        type_name,
                        "Ok",
                        return_type,
                        vec![FunctionBuilder::const_unit()],
                    );
                    builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(ok_val));
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    // Non-void function without explicit return — emit return _0
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                // Explicit return already handled drops via emit_early_exit_drops.
                // Just pop the scope tracking without emitting more drops.
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // Clone borrowed operands at the return boundary (BareParam, CowBorrow, etc.).
            // Skip when return type is Ptr — the caller expects a borrow, not an owned clone.
            let ret_type = builder.locals[0].type_id;
            if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
            }
            let returned_local = match &operand {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    Some(place.local)
                }
                _ => None,
            };
            builder.assign(Place::local(LocalId(0)), operand);
            ctx.drops.emit_early_exit_drops(
                &mut builder, &ctx.type_registry,
                DropScopeKind::Function, returned_local,
            );
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // Not handled in lowering — skip
            // Pop the Function scope we pushed
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            return;
        }
    }

    ctx.flush_ownership_to_locals(&mut builder);
    let mut func = builder.build();
    func.display_name = Some(name.to_string());
    func.def_span = Some(func_span);
    module.functions.push(func);
}

/// Lower an equip method into a standalone GIR function with mangled name.
pub fn lower_equip_method(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method: &FunctionDef,
    type_name: &str,
    equipped_type: &Type,
) {
    let method_name = &method.name.node;
    let mangled_name = format!("{type_name}__{method_name}");

    let return_type = if method.throws.is_some() {
        // `int parse(self, String input) throws String` → Result[int, String]
        let ok_type = ctx.type_mapper.map_ast_type_mut(&method.return_type.node, &mut ctx.type_registry);
        let err_type = ctx.type_mapper.map_ast_type_mut(&method.throws.as_ref().unwrap().node, &mut ctx.type_registry);
        let ok_c = crate::ir::lowering::types::mangle_type_for_name(&method.return_type.node);
        let err_c = crate::ir::lowering::types::mangle_type_for_name(&method.throws.as_ref().unwrap().node);
        let result_name = format!("Result__{ok_c}__{err_c}");
        if let Some(&id) = ctx.type_mapper.named_types.get(&result_name) {
            id
        } else {
            use crate::ir::types::*;
            let type_def = TypeDef {
                name: result_name.clone(),
                kind: TypeDefKind::Enum(EnumDef {
                    variants: vec![
                        EnumVariant {
                            name: "Ok".to_string(),
                            fields: vec![StructField { name: "_0".to_string(), type_id: ok_type }],
                        },
                        EnumVariant {
                            name: "Error".to_string(),
                            fields: vec![StructField { name: "_0".to_string(), type_id: err_type }],
                        },
                    ],
                }),
                metadata: TypeMetadata {
                    enum_category: Some(EnumCategory::Result),
                    ..Default::default()
                },
            };
            ctx.type_registry.add_type_def(type_def);
            let type_id = ctx.type_registry.insert(GirType::Named(result_name.clone()));
            ctx.type_mapper.register_named(result_name, type_id);
            type_id
        }
    } else {
        ctx.type_mapper.map_ast_type(&method.return_type.node)
    };

    // Trivial getter clone elision: return Ptr(T) instead of T.
    // The normal body lowering produces Ptr(T) from IndexLoad/FieldLoad on borrowed self;
    // by matching the return type, we skip the return-boundary materialization clone.
    let is_trivial_getter = ctx.trivial_getter_methods.contains(&mangled_name);
    let return_type = if is_trivial_getter {
        ctx.register_ptr_type(return_type)
    } else {
        return_type
    };

    // Check if method has a self parameter (static methods don't)
    let has_self = method.params.first()
        .map(|p| p.node.name.node == "self")
        .unwrap_or(false);

    // Build parameters: optional self pointer + explicit params
    let mut params: Vec<(TypeId, Option<&str>)> = Vec::new();
    let self_ptr_type = if has_self {
        let self_type_id = ctx.type_mapper.map_ast_type(equipped_type);
        let self_needs_mut_ptr = method.params.first()
            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
            .unwrap_or(false);
        // Scalar primitive receivers (int, float, bool, uint8, …) pass
        // by value to match the call-site dispatch (see methods.rs
        // `recv_type_id.0 < PRIMITIVE_TYPE_COUNT` branch). Only the
        // mutable-borrow form goes through a pointer so the callee can
        // write back.
        let is_scalar = self_type_id.0 < crate::ir::types::PRIMITIVE_TYPE_COUNT;
        let spt = if self_needs_mut_ptr {
            ctx.register_mut_ptr_type(self_type_id)
        } else if is_scalar {
            self_type_id
        } else {
            ctx.register_ptr_type(self_type_id)
        };
        params.push((spt, Some("self")));
        Some(spt)
    } else {
        None
    };
    // Track consuming self (!self) for field load optimization
    let self_is_consuming = has_self && method.params.first()
        .map(|p| matches!(p.node.ownership, Ownership::Move))
        .unwrap_or(false);
    for p in &method.params {
        if p.node.name.node == "self" {
            continue; // self handled above
        }
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        params.push((gir_type, Some(p.node.name.node.as_str())));
    }

    let mut builder = FunctionBuilder::new(mangled_name.clone(), return_type, &params);

    // Clear and register locals
    ctx.clear_locals();
    ctx.func_state.current_fn_name = mangled_name.clone();
    ctx.func_state.callable_return_types.clear();
    ctx.func_state.consuming_self = self_is_consuming;

    // Register self as local _1 (only if method has self)
    let mut param_idx = if let Some(spt) = self_ptr_type {
        ctx.register_local("self", LocalId(1), spt);
        // Mark immutable self as BareParam so CoW materializes on mutation.
        if !self_is_consuming && matches!(ctx.type_registry.get(spt), Some(GirType::Ptr(_))) {
            ctx.set_bare_param(LocalId(1));
        }
        2u32
    } else {
        1u32
    };

    // Register other params
    for p in &method.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            ctx.set_bare_param(LocalId(param_idx));
        } else if matches!(p.node.ownership, crate::parser::ast::Ownership::MutableBorrow)
            && ctx.type_registry.is_resource_type(base_type)
        {
            ctx.set_ref(LocalId(param_idx));
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            ctx.func_state.mut_capture_locals.insert(LocalId(param_idx), base_type);
        }
        // Track callable parameter return types for indirect call lowering
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &[], ctx) {
            ctx.func_state.callable_return_types.insert(LocalId(param_idx), ret_type);
        }
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator
    {
        let mut pidx = if self_ptr_type.is_some() { 2u32 } else { 1u32 };
        for p in &method.params {
            if p.node.name.node == "self" {
                continue;
            }
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
            pidx += 1;
        }
    }

    // Track throws context for Result wrapping in return/throw statements
    ctx.func_state.current_throws_result_type = if method.throws.is_some() {
        Some(return_type)
    } else {
        None
    };

    // Register equip method signature in fn_sigs so callers see the Result return type
    if !ctx.fn_sigs.contains_key(&mangled_name) {
        let base_param_types: Vec<TypeId> = method
            .params
            .iter()
            .filter(|p| p.node.name.node != "self")
            .map(|p| ctx.type_mapper.map_ast_type(&p.node.type_.node))
            .collect();
        ctx.fn_sigs.insert(mangled_name.clone(), (base_param_types, return_type));
    }

    // Pre-scan: find variables unsafe for CoW + count name uses + liveness for auto-move.
    if let FunctionBody::Block(block) = &method.body {
        ctx.func_state.cow_reassigned_names = prescan_cow_unsafe_names(&block.stmts);
        ctx.func_state.cow_reassigned_after = compute_cow_reassigned_after(&block.stmts);
        ctx.func_state.name_use_counts = prescan_name_use_counts(&block.stmts);
        ctx.func_state.liveness = super::liveness::compute_function_liveness(&block.stmts);
    }

    // Lower the body
    match &method.body {
        FunctionBody::Block(block) => {
            // Evaluate delayed meta blocks (meta if/for) with Self bound to the equipped type.
            let mut block = block.clone();
            let self_subs = vec![("Self".to_string(), equipped_type.clone())];
            let empty_env = rustc_hash::FxHashMap::default();
            let delayed_ctx = DelayedMetaContext {
                type_subs:      &self_subs,
                features:       &[],
                meta_env:       &empty_env,
                items:          &[],
                trait_registry: &ctx.analysis.traits,
                type_registry:  &ctx.type_registry,
            };
            let mut meta_errors = Vec::new();
            meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
            for e in &meta_errors {
                eprintln!("[delayed-meta equip] {e:?}");
            }
            lower_block(ctx, &mut builder, &block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // Clone borrowed operands at the return boundary.
            // Skip when return type is Ptr — the caller expects a borrow.
            let ret_type = builder.locals[0].type_id;
            if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
            }
            let returned_local = match &operand {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    Some(place.local)
                }
                _ => None,
            };
            builder.assign(Place::local(LocalId(0)), operand);
            ctx.drops.emit_early_exit_drops(
                &mut builder, &ctx.type_registry,
                DropScopeKind::Function, returned_local,
            );
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    ctx.flush_ownership_to_locals(&mut builder);
    let mut func = builder.build();
    func.display_name = Some(format!("{type_name}.{method_name}"));
    module.functions.push(func);
}

/// Lower a monomorphized instance of a generic function.
///
/// `type_args` are the concrete type arguments (e.g., `[int]` for `identity[int]`).
/// `mangled_name` is the fully mangled name (e.g., `identity__int64_t`).
///
/// The function body is lowered with type parameter substitutions active,
/// so references to `T` in the template resolve to the concrete type.
pub fn lower_generic_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    template: &FunctionDef,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
    meta_op_bindings: &[(String, ast::BinaryOp)],
) {
    let subs = build_subs(template.generic_params.as_ref(), type_args);

    // Build a meta env pre-populated with any compile-time operator bindings.
    // Used by evaluate_delayed_meta_block so MetaOpInfix nodes get substituted
    // to real BinaryOp expressions during monomorphization.
    let mut meta_env_map: rustc_hash::FxHashMap<String, MetaValue> =
        rustc_hash::FxHashMap::default();
    for (param_name, op) in meta_op_bindings {
        meta_env_map.insert(param_name.clone(), MetaValue::Op(*op));
    }

    // Evaluate delayed meta blocks (meta if/for inside generic bodies) with
    // the concrete type substitutions.  Modifies a local clone of the template
    // so the original template is left intact for subsequent instantiations.
    let template_with_meta_evaluated;
    let template = if subs.is_empty() && meta_env_map.is_empty() {
        template
    } else {
        let mut cloned = template.clone();
        let delayed_ctx = DelayedMetaContext {
            type_subs:      &subs,
            features:       &[],
            meta_env:       &meta_env_map,
            items:          &[],
            trait_registry: &ctx.analysis.traits,
            type_registry:  &ctx.type_registry,
        };
        if let FunctionBody::Block(ref mut block) = cloned.body {
            let mut errors = Vec::new();
            meta::evaluate_delayed_meta_block(block, &delayed_ctx, &mut errors);
            // Errors are non-fatal here (will surface as missing symbols); log if any.
            if !errors.is_empty() {
                for e in &errors {
                    eprintln!("[delayed-meta] {e:?}");
                }
            }
        }
        template_with_meta_evaluated = cloned;
        &template_with_meta_evaluated
    };

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    // Map return type with substitutions
    let return_type = substitute_and_map_type(ctx, &template.return_type.node, &subs);

    // Pre-scan: detect bare Move-type params that are directly returned.
    // These need Move (not Borrow) semantics to avoid double-free.
    // In expression bodies `T f[T](T x) = x`, or block bodies with `return x`,
    // a borrow param would create a shallow copy without transferring ownership.
    let mut move_override_params: std::collections::HashSet<String> = std::collections::HashSet::new();
    if ctx.type_registry.is_resource_type(return_type) {
        let returned_param_name = match &template.body {
            FunctionBody::Expression(expr) => {
                if let Expr::Identifier(name) = &expr.node {
                    Some(name.as_str())
                } else { None }
            }
            FunctionBody::Block(block) => {
                // Check if the last statement is `return x` where x is a param.
                // The param must be the last use — earlier statements may read
                // through it but must not consume or reassign it.
                block.stmts.last().and_then(|stmt| {
                    if let Stmt::Return(Some(expr)) = &stmt.node {
                        if let Expr::Identifier(name) = &expr.node {
                            // Verify this is the param's last use via liveness
                            Some(name.as_str())
                        } else { None }
                    } else { None }
                })
            }
            _ => None,
        };
        if let Some(name) = returned_param_name {
            for p in &template.params {
                if !p.node.is_meta_op
                    && p.node.name.node == name
                    && p.node.ownership == Ownership::Borrow
                {
                    let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
                    if ctx.type_registry.is_resource_type(base_type) {
                        move_override_params.insert(name.to_string());
                    }
                }
            }
        }
    }

    // Map parameters with substitutions — skip meta op params (no runtime representation),
    // MutableBorrow params become MutPtr; Borrow params of Move types also become MutPtr
    let params: Vec<(TypeId, Option<String>)> = template
        .params
        .iter()
        .filter(|p| !p.node.is_meta_op)
        .map(|p| {
            let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            let ownership = if move_override_params.contains(&p.node.name.node) {
                Ownership::Move
            } else {
                p.node.ownership
            };
            let gir_type = ctx.resolve_param_type(base_type, ownership);
            (gir_type, Some(p.node.name.node.clone()))
        })
        .collect();

    let param_refs: Vec<(TypeId, Option<&str>)> = params
        .iter()
        .map(|(tid, name)| (*tid, name.as_deref()))
        .collect();

    let mut builder = FunctionBuilder::new(mangled_name, return_type, &param_refs);

    // Clear and register locals — assign sequential LocalIds to runtime params only
    // (meta op params carry no runtime value and are skipped).
    ctx.clear_locals();
    ctx.func_state.current_fn_name = mangled_name.to_string();
    ctx.func_state.callable_return_types.clear();
    // Store move_override_params in context so return-statement lowering can zero sources.
    // Must be set AFTER clear_locals() which resets it.
    ctx.func_state.move_override_params = move_override_params.clone();

    let mut local_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        local_idx += 1;
        let local_id = LocalId(local_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let ownership = if move_override_params.contains(&p.node.name.node) {
            Ownership::Move
        } else {
            p.node.ownership
        };
        let gir_type = ctx.resolve_param_type(base_type, ownership);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        if ctx.is_ref_param(base_type, ownership) {
            ctx.set_bare_param(local_id);
        } else if matches!(ownership, Ownership::MutableBorrow)
            && ctx.type_registry.is_resource_type(base_type)
        {
            ctx.set_ref(local_id);
        } else if ctx.is_mut_ref_param(base_type, ownership) {
            ctx.func_state.mut_capture_locals.insert(local_id, base_type);
        }
        // Track callable parameter return types for indirect call lowering
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, &subs, ctx) {
            ctx.func_state.callable_return_types.insert(local_id, ret_type);
        }
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator.
    // Skip meta op params — they have no runtime local slot.
    let mut drop_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        drop_idx += 1;
        let local_id = LocalId(drop_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let ownership = if move_override_params.contains(&p.node.name.node) {
            Ownership::Move
        } else {
            p.node.ownership
        };
        let gir_type = ctx.resolve_param_type(base_type, ownership);
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
    }

    // Lower the body
    match &template.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                // For move-overridden params in block bodies with implicit return:
                // zero the source through the pointer to prevent caller double-free.
                if !move_override_params.is_empty() {
                    for name in &move_override_params {
                        if let Some((local_id, _)) = ctx.lookup_local(name) {
                            builder.move_zero(Place {
                                local: local_id,
                                projections: vec![Projection::Deref],
                            });
                        }
                    }
                }
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // Clone borrowed operands at the return boundary.
            // Skip when return type is Ptr — the caller expects a borrow.
            let ret_type = builder.locals[0].type_id;
            if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
            }
            let returned_local = match &operand {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    Some(place.local)
                }
                _ => None,
            };
            builder.assign(Place::local(LocalId(0)), operand);
            // For move-overridden params: zero the source through the pointer
            // to prevent the caller from double-freeing.
            if !move_override_params.is_empty() {
                if let Expr::Identifier(name) = &expr.node {
                    if let Some((local_id, _)) = ctx.lookup_local(name) {
                        builder.move_zero(Place {
                            local: local_id,
                            projections: vec![Projection::Deref],
                        });
                    }
                }
            }
            ctx.drops.emit_early_exit_drops(
                &mut builder, &ctx.type_registry,
                DropScopeKind::Function, returned_local,
            );
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            ctx.generics.type_name_subs.clear();
            ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
            return;
        }
    }

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
    ctx.flush_ownership_to_locals(&mut builder);
    module.functions.push(builder.build());
}

/// Lower monomorphized equip methods for a generic type instantiation.
///
/// For each method in the equip block, creates a GIR function named
/// `{mangled_type_name}__{method_name}` with substituted types.
pub fn lower_generic_equip_methods(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
) {
    lower_generic_equip_methods_with_defaults(ctx, module, equip, type_args, mangled_type_name, None);
}

/// Lower monomorphized equip methods for a generic type instantiation,
/// with optional AST module for default trait method emission.
pub fn lower_generic_equip_methods_with_defaults(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
    ast_module: Option<&ast::Module>,
) {
    let subs = build_equip_subs(equip, type_args);

    // Substituted equipped type — used for Self binding in delayed meta evaluation.
    let substituted_equipped_type = generics::substitute_type_pub(&equip.type_.node, &subs);

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Add substitution for the equipped type itself (e.g., Pair__T → Pair__int64_t)
    // This handles cases where the method body references the struct/enum being equipped
    if let Type::Named { name, generic_args } = &equip.type_.node {
        let base_name = &name.node;
        if !generic_args.is_empty() {
            // Mangle the template name (with generic params as wildcards)
            // For Pair[T], we want "Pair__T"
            let template_mangled = super::types::mangle_generic_name(base_name, generic_args);
            // mangled_type_name is already the concrete name (e.g., "Pair__int64_t")
            if template_mangled != mangled_type_name {
                ctx.generics.type_name_subs.insert(template_mangled, mangled_type_name.to_string());
            }
        }
    }

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    for method in &equip.items {
        let method_def = &method.node;
        // Methods with their own generic params (e.g. `map[U, F]` inside
        // `equip [T] VectorIter[T]:`) can't be lowered here — the equip-level
        // subs only cover T, so the return type `MapIter[T, U, F]` would
        // substitute to `MapIter[int, U, F]` which map_ast_type_mut resolves
        // to UNIT_TYPE. Per-call-site mono via `lower_method_instance`
        // handles these with merged equip + method substitutions.
        if method_def.generic_params.is_some() {
            continue;
        }
        let method_mangled = format!("{mangled_type_name}__{}", method_def.name.node);
        lower_equip_method_with_subs(
            ctx, module, method_def, &subs,
            mangled_type_name, &method_mangled,
            &substituted_equipped_type,
        );
    }

    // Emit default trait methods that aren't overridden in the equip block
    if let (Some(ast_mod), Some(trait_ref)) = (ast_module, &equip.trait_) {
        use crate::parser::ast::{Item, TraitItem};
        let trait_name = super::traits::extract_trait_name(&trait_ref.trait_name.node);
        if !trait_name.is_empty() {
            let implemented: Vec<String> = equip.items.iter()
                .map(|m| m.node.name.node.clone())
                .collect();
            // Substituted equipped type already computed above; re-borrow for this scope.
            let substituted_type = substituted_equipped_type.clone();
            // Find trait def and emit defaults
            for item in &ast_mod.items {
                if let Item::Trait(trait_def) = &item.node {
                    if trait_def.name.node == trait_name {
                        for trait_item in &trait_def.items {
                            if let TraitItem::Method(default_method) = &trait_item.node {
                                let method_name = &default_method.name.node;
                                if implemented.contains(method_name) {
                                    continue;
                                }
                                match &default_method.body {
                                    FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                                    FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
                                }
                                // Method-level-generic defaults (e.g.
                                // `bool any[F](&self, F pred)`) follow the
                                // same per-call-site mono path as inherent
                                // method-generic equip methods — skip the
                                // bulk lowering and let lower_method_instance
                                // handle them at each call site.
                                if default_method.generic_params.is_some() {
                                    continue;
                                }
                                // Emit as {mangled_type_name}__{method_name}.
                                // Threading the equip-level subs lets the
                                // signature substitute `T → int64_t` (etc.)
                                // before map_ast_type — without this,
                                // `Option[T] last(&self)` resolves to
                                // `Option[UNIT]` and the call site sees a
                                // void return. Defaults additionally bind
                                // `Self` so adapter constructors like
                                // `TakeIter[Self, T] take(self, int n)`
                                // resolve to the equipping type.
                                let mut default_subs = subs.clone();
                                default_subs.push(("Self".to_string(), substituted_type.clone()));
                                let method_mangled = format!("{mangled_type_name}__{method_name}");
                                // Refresh generic_type_params so the body
                                // lowering sees `Self → equipping_type`
                                // when it resolves type-arg lists like
                                // `TakeIter[Self, T](self, n)`.
                                build_generic_type_params(ctx, &default_subs);
                                // Pre-substitute the body so struct-constructor
                                // type-arg lists resolve before mangling. Without
                                // this `TakeIter[Self, T]` mangles to
                                // `TakeIter__unknown__int64_t` because
                                // `mangle_type_for_name(Self)` returns "unknown".
                                let substituted_method = generics::substitute_function_body_pub(
                                    default_method, &default_subs,
                                );
                                lower_equip_method_with_subs(
                                    ctx, module, &substituted_method, &default_subs,
                                    mangled_type_name, &method_mangled,
                                    &substituted_type,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
}

/// Lower a single equip method body with the given substitutions. Shared
/// between the equip-block bulk path (`lower_generic_equip_methods_with_defaults`)
/// and the per-call-site method-instance path (`lower_method_instance`). The
/// two differ only in how they build `subs` and `method_mangled`.
fn lower_equip_method_with_subs(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method_def: &ast::FunctionDef,
    subs: &[(String, Type)],
    mangled_type_name: &str,
    method_mangled: &str,
    substituted_equipped_type: &Type,
) {
    // Use map_ast_type_mut so that generic return types like Option[T] get
    // registered (not silently resolved to UNIT_TYPE) after substitution.
    let substituted_ret = generics::substitute_type_pub(&method_def.return_type.node, subs);
    let return_type = ctx.type_mapper.map_ast_type_mut(&substituted_ret, &mut ctx.type_registry);

    // Self pointer type — only for methods with a self parameter
    let has_self = method_def.params.first()
        .map(|p| p.node.name.node == "self")
        .unwrap_or(false);

    let self_type_id = ctx.type_mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
    let self_is_mutable = method_def.params.first()
        .map(|p| {
            p.node.name.node == "self" &&
            matches!(p.node.ownership, Ownership::MutableBorrow)
        })
        .unwrap_or(false);

    let self_ptr_type = if self_is_mutable {
        ctx.register_mut_ptr_type(self_type_id)
    } else {
        ctx.register_ptr_type(self_type_id)
    };

    let mut params: Vec<(TypeId, Option<&str>)> = if has_self {
        vec![(self_ptr_type, Some("self"))]
    } else {
        vec![]
    };
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        params.push((gir_type, Some(p.node.name.node.as_str())));
    }

    let mut builder = FunctionBuilder::new(method_mangled, return_type, &params);

    ctx.clear_locals();
    ctx.func_state.callable_return_types.clear();
    let mut param_idx = if has_self {
        ctx.register_local("self", LocalId(1), self_ptr_type);
        2u32
    } else {
        1u32
    };
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            ctx.set_bare_param(LocalId(param_idx));
        } else if matches!(p.node.ownership, Ownership::MutableBorrow)
            && ctx.type_registry.is_resource_type(base_type)
        {
            ctx.set_ref(LocalId(param_idx));
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            ctx.func_state.mut_capture_locals.insert(LocalId(param_idx), base_type);
        }
        // Track callable parameter return types for indirect call lowering
        if let Some(ret_type) = extract_callable_return_type(&p.node.type_.node, subs, ctx) {
            ctx.func_state.callable_return_types.insert(LocalId(param_idx), ret_type);
        }
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator
    {
        let mut pidx = if has_self { 2u32 } else { 1u32 };
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
            pidx += 1;
        }
    }

    match &method_def.body {
        FunctionBody::Block(block) => {
            // Evaluate delayed meta blocks (meta if/for) with Self bound to the equipped type.
            let mut block = block.clone();
            let self_subs = vec![("Self".to_string(), substituted_equipped_type.clone())];
            let empty_env = rustc_hash::FxHashMap::default();
            let delayed_ctx = DelayedMetaContext {
                type_subs:      &self_subs,
                features:       &[],
                meta_env:       &empty_env,
                items:          &[],
                trait_registry: &ctx.analysis.traits,
                type_registry:  &ctx.type_registry,
            };
            let mut meta_errors = Vec::new();
            meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
            for e in &meta_errors {
                eprintln!("[delayed-meta generic-equip] {e:?}");
            }
            lower_block(ctx, &mut builder, &block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }
        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // Clone borrowed operands at the return boundary.
            // Skip when return type is Ptr — the caller expects a borrow.
            let ret_type = builder.locals[0].type_id;
            if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
            }
            let returned_local = match &operand {
                Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                    Some(place.local)
                }
                _ => None,
            };
            builder.assign(Place::local(LocalId(0)), operand);
            ctx.drops.emit_early_exit_drops(
                &mut builder, &ctx.type_registry,
                DropScopeKind::Function, returned_local,
            );
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    ctx.flush_ownership_to_locals(&mut builder);
    module.functions.push(builder.build());
}

/// Lower a per-call-site monomorphisation of a method-level-generic equip
/// method into a free-function-shaped symbol. Used for calls like
/// `v.iter().map[int, int(int)](f)` that the bulk equip-lowering path skips
/// because the equip-level subs don't cover the method-level generics.
///
/// Merges equip-level substitutions (e.g. `T→int` from `VectorIter[int]`)
/// with method-level substitutions (e.g. `U→int`, `F→int(int)` from the
/// call site's generic args). The resulting body has all type params
/// substituted; the emitted function is named by `mangled_symbol` and is
/// called directly by the MethodCall dispatch path (see
/// `src/ir/lowering/exprs/methods.rs`).
pub fn lower_method_instance(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    method: &ast::FunctionDef,
    equip_type_args: &[Spanned<Type>],
    method_type_args: &[Spanned<Type>],
    mangled_type_name: &str,
    mangled_symbol: &str,
) {
    // Equip-level subs (T → concrete from receiver).
    let mut subs = build_equip_subs(equip, equip_type_args);
    // Method-level subs (U, F → concrete from call-site targs).
    if let Some(ref gp) = method.generic_params {
        for (param, arg) in gp.node.params.iter().zip(method_type_args.iter()) {
            let name = match &param.node {
                GenericParam::Type { name: s, .. } => s.node.clone(),
                GenericParam::Const { name, .. } => name.node.clone(),
            };
            subs.push((name, arg.node.clone()));
        }
    }

    let substituted_equipped_type = generics::substitute_type_pub(&equip.type_.node, &subs);

    // Shared context setup: type-name subs + generic type param TypeId map.
    // Both drive method-body lowering decisions (struct init, method dispatch).
    build_type_name_subs(ctx, &subs);
    if let Type::Named { name, generic_args } = &equip.type_.node {
        let base_name = &name.node;
        if !generic_args.is_empty() {
            let template_mangled = super::types::mangle_generic_name(base_name, generic_args);
            if template_mangled != mangled_type_name {
                ctx.generics.type_name_subs.insert(template_mangled, mangled_type_name.to_string());
            }
        }
    }
    build_generic_type_params(ctx, &subs);

    lower_equip_method_with_subs(
        ctx, module, method, &subs,
        mangled_type_name, mangled_symbol,
        &substituted_equipped_type,
    );

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
}

/// Build type parameter substitutions from generic params + concrete type args.
fn build_subs(
    generic_params: Option<&Spanned<ast::GenericParams>>,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    let mut subs = Vec::new();
    if let Some(params) = generic_params {
        for (param, arg) in params.node.params.iter().zip(type_args.iter()) {
            let name = match &param.node {
                GenericParam::Type { name: s, .. } => s.node.clone(),
                GenericParam::Const { name, .. } => name.node.clone(),
            };
            subs.push((name, arg.node.clone()));
        }
    }
    subs
}

/// Build type parameter substitutions for an equip block.
fn build_equip_subs(
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    if let Some(ref gp) = equip.generic_params {
        return build_subs(Some(gp), type_args);
    }
    // Fall back: extract params from the equipped type's generic args
    if let Type::Named { generic_args, .. } = &equip.type_.node {
        let mut subs = Vec::new();
        for (param_type, arg) in generic_args.iter().zip(type_args.iter()) {
            if let Type::Named { name, generic_args: inner } = &param_type.node {
                if inner.is_empty() {
                    subs.push((name.node.clone(), arg.node.clone()));
                }
            }
        }
        return subs;
    }
    Vec::new()
}

/// Substitute type parameters in an AST type and map to GIR TypeId.
fn substitute_and_map_type(
    ctx: &LoweringContext,
    ty: &Type,
    subs: &[(String, Type)],
) -> TypeId {
    let substituted = generics::substitute_type_pub(ty, subs);
    ctx.type_mapper.map_ast_type(&substituted)
}

/// Extract the return type of a callable/function parameter type.
///
/// For parameters like `Callable[T(int)]` or `int(int)`, extracts the return type
/// after applying generic substitutions. Returns None if the type isn't a callable.
fn extract_callable_return_type(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
) -> Option<TypeId> {
    extract_callable_return_type_bounded(ty, subs, ctx, 8)
}

fn extract_callable_return_type_bounded(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
    depth: u32,
) -> Option<TypeId> {
    if depth == 0 {
        return None;
    }
    match ty {
        // Callable[RetType(Params...)] or MutCallable[...] or ConsumeCallable[...]
        Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if name_str == "Callable" || name_str == "MutCallable" || name_str == "ConsumeCallable" {
                // The generic_args should contain a single Function type
                if let Some(func_type) = generic_args.first() {
                    if let Type::Function { return_type, .. } = &func_type.node {
                        let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
                        return Some(ret_type);
                    }
                }
                return None;
            }
            // Generic type param bound to a Function or Callable at the call
            // site — recurse after substitution so method-level-generic params
            // like `F f` (F → int(int)) get their return type picked up.
            if generic_args.is_empty() {
                for (param, concrete) in subs {
                    if param == name_str {
                        // Guard against degenerate self-loops: `T → Named{T, []}`
                        // (shouldn't happen, but cheap to check).
                        if let Type::Named { name: cn, generic_args: cgs } = concrete {
                            if cgs.is_empty() && cn.node == *name_str {
                                return None;
                            }
                        }
                        return extract_callable_return_type_bounded(concrete, subs, ctx, depth - 1);
                    }
                }
            }
            None
        }
        // Direct function type: RetType(Params...)
        Type::Function { return_type, .. } => {
            let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
            Some(ret_type)
        }
        _ => None,
    }
}

/// Build generic type parameter → concrete TypeId substitutions.
///
/// For each type parameter (e.g., T), maps it to the concrete TypeId
/// (e.g., I64_TYPE for int). This enables `map_type_with_subs` to resolve
/// bare type parameters in variable declarations inside generic bodies.
fn build_generic_type_params(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
    for (param_name, concrete_ty) in subs {
        let type_id = ctx.type_mapper.map_ast_type(concrete_ty);
        ctx.generics.generic_type_params.insert(param_name.clone(), type_id);
        ctx.generics.generic_param_ast_types.insert(param_name.clone(), concrete_ty.clone());
    }
}

/// Build type name substitution map for generic body lowering.
///
/// For each registered type name that contains a type parameter placeholder
/// (e.g., `Container__T`), computes the concrete mangled name (e.g.,
/// `Container__int64_t`) and stores the mapping in ctx.generics.type_name_subs.
fn build_type_name_subs(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.generics.type_name_subs.clear();

    // Build a map of param-mangled-fragment → concrete-mangled-fragment.
    // E.g., for sub T → int:  "T" → "int64_t"
    // For sub T → str:  "T" → "Str"
    let fragment_subs: Vec<(String, String)> = subs.iter().map(|(param, concrete_ty)| {
        let concrete_name = super::types::mangle_type_for_name(concrete_ty);
        (param.clone(), concrete_name)
    }).collect();

    // Store fragment subs for on-the-fly resolution of names not in the pre-computed map
    ctx.generics.generic_fragment_subs = fragment_subs.clone();

    // Scan all known type names in the registry for template patterns.
    // For each name like "Container__T", substitute "T" → "int64_t" to get "Container__int64_t".
    let type_names: Vec<String> = ctx.type_registry.type_defs().iter()
        .map(|def| def.name.clone())
        .collect();
    for name in type_names {
        let mut substituted = name.clone();
        let mut changed = false;
        for (param, concrete) in &fragment_subs {
            // Match `__T` at end of name AND `__T__` anywhere in the middle —
            // both can fire on the same name (e.g. `A__T__B__T`), so we don't
            // short-circuit between them.
            let pattern_mid = format!("__{param}__");
            if substituted.contains(&pattern_mid) {
                substituted = substituted.replace(&pattern_mid, &format!("__{concrete}__"));
                changed = true;
            }
            let pattern_end = format!("__{param}");
            if substituted.ends_with(&pattern_end) {
                let prefix = &substituted[..substituted.len() - pattern_end.len()];
                substituted = format!("{prefix}__{concrete}");
                changed = true;
            }
        }
        if changed && name != substituted {
            ctx.generics.type_name_subs.insert(name, substituted);
        }
    }
}
