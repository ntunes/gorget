mod calls;
mod collections;
mod methods;
mod operators;
mod shared;
pub(crate) mod spawn;
mod type_reg;

pub(in crate::ir::lowering) use calls::*;
use collections::*;
pub(in crate::ir::lowering) use methods::*;
use operators::*;
pub(in crate::ir::lowering) use shared::*;
pub(in crate::ir::lowering) use spawn::*;
pub(in crate::ir::lowering) use type_reg::*;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, Ownership};
use crate::span::Spanned;

use super::context::LoweringContext;

/// Known blocking function names that should trigger `with shared_var:` auto-refresh.
/// These are yield points where another task could modify a shared variable.
const BLOCKING_CALL_NAMES: &[&str] = &[
    "sleep", "read_file", "write_file", "append_file",
    "readdir", "http_get", "http_post", "http_put", "http_delete",
];

/// Check if an expression is a call to a known blocking function.
fn is_blocking_call_name(expr: &Expr) -> bool {
    if let Expr::Identifier(name) = expr {
        BLOCKING_CALL_NAMES.contains(&name.as_str())
    } else {
        false
    }
}

/// Lower an expression to GIR instructions, returning the result `Operand`.
pub fn lower_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) -> Operand {
    lower_expr_inner(ctx, builder, expr, None)
}

/// Lower an expression with optional type registry access for mutable operations.
fn lower_expr_inner(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
    _registry: Option<&mut TypeRegistry>,
) -> Operand {
    builder.set_span(expr.span);
    match &expr.node {
        Expr::IntLiteral(n) => Operand::Constant(Constant::I64(*n)),

        Expr::FloatLiteral(n) => Operand::Constant(Constant::F64(*n)),

        Expr::BoolLiteral(b) => Operand::Constant(Constant::Bool(*b)),

        Expr::StringLiteral(lit) => {
            if !lit.has_interpolation() {
                let text = lit.as_plain_text();
                Operand::Constant(Constant::Str(text))
            } else {
                lower_string_interpolation(ctx, builder, lit)
            }
        }

        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                // If this is a shared variable:
                // - In spawn arg context (shared_pass_raw), return the raw Mutex local
                // - Otherwise, auto-emit lock+get for transparent access
                if let Some(info) = ctx.shared.locals.get(&local_id) {
                    let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                    use super::context::SharedLocalKind;
                    if ctx.shared.pass_raw {
                        return Operand::Copy(Place::local(hidden_local));
                    }
                    return match kind {
                        SharedLocalKind::SharedArc => emit_shared_get(ctx, builder, hidden_local, inner_type),
                        SharedLocalKind::Atomic => {
                            let atomic_name = atomic_type_name_for(inner_type);
                            emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name)
                        }
                        SharedLocalKind::Mutex => {
                            let inner_c = ctx.c_type_name_for_id(inner_type);
                            let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                            emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type)
                        }
                        SharedLocalKind::RwLock => emit_rwlock_read_get(ctx, builder, hidden_local, inner_type),
                    };
                }
                // If this is a &/! param (MutPtr), deref to get the value.
                // ref_locals (bare-borrow Ptr params) are NOT auto-deref'd —
                // they stay as Ptr throughout the callee body.
                if let Some(&value_type) = ctx.mut_capture_locals.get(&local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    let tmp = builder.add_local(value_type, None);
                    builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                    // ! param owns its data — the deref copy is also owned.
                    // Prevents ensure_owned_string from cloning in constructors.
                    if ctx.owned_locals.contains(&local_id) {
                        ctx.owned_locals.insert(tmp);
                    }
                    Operand::Copy(Place::local(tmp))
                } else {
                    Operand::Copy(Place::local(local_id))
                }
            } else if let Some(constant) = ctx.module_constants.get(name) {
                Operand::Constant(constant.clone())
            } else if ctx.global_names.contains(name.as_str()) {
                // Module-level static variable — reference by name in C
                Operand::Constant(Constant::GlobalRef(name.clone()))
            } else if ctx.fn_sigs.contains_key(name.as_str()) {
                // Named function reference (for passing as Callable argument)
                Operand::Constant(Constant::FuncRef(name.clone()))
            } else if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
                // Bare nullary enum variant (e.g., `Red` after glob import)
                let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                let dst = builder.enum_init(&enum_name, &variant_name, type_id, vec![]);
                FunctionBuilder::copy(dst)
            } else {
                // Could be a function name or unknown — produce a constant placeholder
                Operand::Constant(Constant::I64(0))
            }
        }

        Expr::BinaryOp { left, op, right } => {
            lower_binary_op(ctx, builder, left, *op, right)
        }

        Expr::UnaryOp { op, operand } => {
            lower_unary_op(ctx, builder, *op, operand)
        }

        Expr::Call { callee, args, generic_args } => {
            // None() call → Constant::Null (the Assign handler converts to tagged enum)
            if matches!(callee.node, Expr::NoneLiteral) {
                return Operand::Constant(Constant::Null);
            }
            // Check if this is a blocking call that should trigger with-shared refresh
            let is_blocking = is_blocking_call_name(&callee.node);
            let result = lower_call(ctx, builder, callee, args, generic_args.as_deref());
            if is_blocking {
                shared::emit_with_shared_refresh(ctx, builder);
            }
            result
        }

        // -- P2.1: Struct operations --
        Expr::StructLiteral { name, args, generic_args } => {
            lower_struct_literal(ctx, builder, &name.node, args, generic_args.as_deref())
        }

        Expr::FieldAccess { object, field } => {
            lower_field_access(ctx, builder, object, &field.node)
        }

        // -- P2.2: Method calls --
        Expr::MethodCall { receiver, method, args, .. } => {
            lower_method_call(ctx, builder, receiver, &method.node, args)
        }

        // -- Index access --
        Expr::Index { object, index } => {
            lower_index_access(ctx, builder, object, index)
        }

        // -- P2.6: Move/Borrow --
        Expr::Move { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            // Copy value to a temp BEFORE zeroing the source, so we don't read zeroed data
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let place_clone = place.clone();
                let local_type = if (place_clone.local.0 as usize) < builder.locals.len() {
                    builder.local_type(place_clone.local)
                } else {
                    I64_TYPE
                };
                let tmp = builder.add_local(local_type, None);
                builder.assign(Place::local(tmp), val);
                ctx.move_zero_and_mark(builder, place_clone.local);
                FunctionBuilder::copy(tmp)
            } else {
                val
            }
        }

        Expr::MutableBorrow { expr: inner } => {
            // Special case: &name where name is already a pointer param.
            // Skip the auto-deref that Identifier normally does — just forward the pointer.
            if let Expr::Identifier(name) = &inner.node {
                if let Some((local_id, _)) = ctx.lookup_local(name) {
                    if ctx.ref_locals.contains(&local_id)
                        || ctx.mut_capture_locals.contains_key(&local_id)
                    {
                        return FunctionBuilder::copy(local_id);
                    }
                }
            }
            let val = lower_expr(ctx, builder, inner);
            // GlobalRef → GlobalRefPtr: emit &global_name directly.
            if let Operand::Constant(Constant::GlobalRef(name)) = &val {
                return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
            }
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.local_type(place.local)
                } else {
                    UNIT_TYPE
                };
                // If the value is already a pointer (e.g., &self where self is Node*),
                // just forward it — don't create a double pointer.
                let is_already_ptr = matches!(
                    ctx.type_registry.get(local_type),
                    Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                );
                if is_already_ptr {
                    return FunctionBuilder::copy(place.local);
                }
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }

        // -- P2.4: Closures --
        Expr::Closure { params, body, is_move, .. } => {
            let mut cl = std::mem::take(&mut ctx.closures);
            let result = cl.lower_closure(ctx, builder, params, body, *is_move);
            ctx.closures = cl;
            result
        }

        // -- If expression (ternary) --
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            lower_if_expr(ctx, builder, condition, then_branch, elif_branches, else_branch.as_deref())
        }

        // -- P3.2: Match expression --
        Expr::Match { scrutinee, arms, else_arm } => {
            lower_match_expr(ctx, builder, scrutinee, arms, else_arm.as_deref())
        }

        // -- P3.4: Miscellaneous expressions --

        Expr::NoneLiteral => {
            Operand::Constant(Constant::Null)
        }

        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::It => {
            if let Some((local_id, _)) = ctx.lookup_local("it") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::Block(block) => {
            lower_block_expr(ctx, builder, block)
        }

        Expr::Do { body } => {
            lower_block_expr(ctx, builder, body)
        }

        Expr::As { expr: inner, type_ } => {
            let val = lower_expr(ctx, builder, inner);
            let target_type = ctx.type_mapper.map_ast_type(&type_.node);
            let dst = builder.cast(target_type, val);
            FunctionBuilder::copy(dst)
        }

        Expr::TupleLiteral(elems) => {
            let operands: Vec<Operand> = elems.iter()
                .map(|e| lower_expr(ctx, builder, e))
                .collect();
            // Infer element types using builder locals (handles nested tuples)
            let elem_types: Vec<TypeId> = operands.iter()
                .map(|op| infer_operand_type_full(ctx, op, builder))
                .collect();
            // Track which locals are used as tuple elements (for return MoveZero)
            let elem_locals: Vec<LocalId> = operands.iter()
                .filter_map(|op| match op {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                    _ => None,
                })
                .collect();
            let type_id = register_tuple_type(ctx, &elem_types);
            let dst = builder.tuple_init(operands, type_id);
            if !elem_locals.is_empty() {
                ctx.tuple_element_locals.insert(dst, elem_locals);
            }
            FunctionBuilder::copy(dst)
        }

        Expr::ArrayLiteral(elems) => {
            lower_array_literal(ctx, builder, elems)
        }

        Expr::Deref { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let mut deref_place = place.clone();
                deref_place.projections.push(Projection::Deref);
                // Need to determine the dereferenced type.
                // Box[T] types are GirType::Named("Box__X"), not GirType::Ptr,
                // so use deref_inner_type() which handles both.
                let local_idx = place.local.0 as usize;
                let deref_type = if local_idx < builder.locals.len() {
                    let ptr_type = builder.locals[local_idx].type_id;
                    ctx.deref_inner_type(ptr_type).unwrap_or(I64_TYPE)
                } else {
                    I64_TYPE
                };
                let dst = builder.add_local(deref_type, None);
                builder.assign(Place::local(dst), Operand::Copy(deref_place));
                return FunctionBuilder::copy(dst);
            }
            val
        }

        Expr::TupleFieldAccess { object, index } => {
            let obj = lower_expr(ctx, builder, object);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
                // Resolve the field type from the tuple's TypeDef
                let local_idx = place.local.0 as usize;
                let elem_type = if local_idx < builder.locals.len() {
                    let tuple_type_id = builder.locals[local_idx].type_id;
                    resolve_tuple_field_type(ctx, tuple_type_id, *index)
                } else {
                    I64_TYPE
                };
                let dst = builder.field_load(place.clone(), *index as u32, elem_type);
                return FunctionBuilder::copy(dst);
            }
            Operand::Constant(Constant::Unit)
        }

        Expr::Is { expr: inner, negated, pattern } => {
            let val = lower_expr(ctx, builder, inner);
            let scrut_type = infer_operand_type_full(ctx, &val, builder);
            let scrut_local = builder.add_local(scrut_type, None);
            builder.assign(Place::local(scrut_local), val);

            let cond = super::stmts::lower_pattern_condition(
                ctx, builder, pattern, scrut_local, scrut_type,
            );
            if *negated {
                let neg = builder.un_op(UnOp::Not, BOOL_TYPE, cond);
                FunctionBuilder::copy(neg)
            } else {
                cond
            }
        }

        Expr::DefaultOp { lhs, rhs } => {
            // lhs ?? rhs: check if lhs is None, if so evaluate rhs
            let lhs_val = lower_expr(ctx, builder, lhs);
            let lhs_type = infer_operand_type_full(ctx, &lhs_val, builder);
            let lhs_local = builder.add_local(lhs_type, None);
            builder.assign(Place::local(lhs_local), lhs_val);

            // Check tag: if tag != None_tag, use lhs; else evaluate rhs
            let tag = builder.tag_of(FunctionBuilder::copy(lhs_local));
            // None is conventionally the last variant (tag = num_variants - 1)
            // Use 0 as "has value" heuristic: tag == 0 means first variant (Some/Ok)
            let is_some = builder.cmp(
                CmpOp::Eq,
                I32_TYPE,
                FunctionBuilder::copy(tag),
                Operand::Constant(Constant::I32(0)),
            );

            let result_id = builder.add_local(lhs_type, None);
            let then_bb = builder.new_block();
            let else_bb = builder.new_block();
            let merge_bb = builder.new_block();

            builder.branch(FunctionBuilder::copy(is_some), then_bb, else_bb);

            // Has value: extract it (field 0 of variant 0)
            builder.switch_to(then_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::copy(lhs_local));
            builder.jump(merge_bb);

            // None: evaluate rhs
            builder.switch_to(else_bb);
            let rhs_val = lower_expr(ctx, builder, rhs);
            builder.assign(Place::local(result_id), rhs_val);
            builder.jump(merge_bb);

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Expr::Path { segments } => {
            // Qualified enum variant path: Color.Red (2+ segments)
            if segments.len() >= 2 {
                let enum_name = &segments[0].node;
                let variant_name = &segments.last().unwrap().node;
                if let Some(type_id) = ctx.type_mapper.lookup_named(enum_name) {
                    if let Some(type_def) = ctx.type_registry.get_type_def(enum_name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            if e.variants.iter().any(|v| &v.name == variant_name) {
                                let dst = builder.enum_init(enum_name, variant_name, type_id, vec![]);
                                return FunctionBuilder::copy(dst);
                            }
                        }
                    }
                }
            }
            // Single-segment path — try as enum variant (prelude: None, Some, Ok, Error)
            if let Some(last) = segments.last() {
                if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&last.node) {
                    let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                    let dst = builder.enum_init(&enum_name, &variant_name, type_id, vec![]);
                    return FunctionBuilder::copy(dst);
                }
                // Try as identifier
                if let Some((local_id, _)) = ctx.lookup_local(&last.node) {
                    return Operand::Copy(Place::local(local_id));
                }
            }
            Operand::Constant(Constant::Unit)
        }

        // P3.5.2: Dict literals
        Expr::DictLiteral(pairs) => {
            lower_dict_literal(ctx, builder, pairs)
        }

        // P3.5.3: List comprehensions
        Expr::ListComprehension { expr: comp_expr, variable, iterable, condition, .. } => {
            lower_list_comprehension(ctx, builder, comp_expr, variable, iterable, condition.as_deref())
        }

        // P3.5.4: Dict comprehensions
        Expr::DictComprehension { key, value, variables, iterable, condition } => {
            lower_dict_comprehension(ctx, builder, key, value, variables, iterable, condition.as_deref())
        }

        // P3.5.4: Set comprehensions
        Expr::SetComprehension { expr: comp_expr, variable, iterable, condition } => {
            lower_set_comprehension(ctx, builder, comp_expr, variable, iterable, condition.as_deref())
        }

        // P3.5.5: Implicit closures
        Expr::ImplicitClosure { body } => {
            let param = ast::ClosureParam {
                type_: None,
                ownership: ast::Ownership::Borrow,
                name: Spanned::dummy("it".to_string()),
            };
            let params = vec![Spanned::dummy(param)];
            let mut cl = std::mem::take(&mut ctx.closures);
            let result = cl.lower_closure(ctx, builder, &params, body, false);
            ctx.closures = cl;
            result
        }

        // P3.5.6: Optional chaining
        Expr::OptionalChain { object, field } => {
            lower_optional_chain(ctx, builder, object, field)
        }

        // P3.5.7: Range expressions (standalone)
        Expr::Range { start, end, inclusive } => {
            lower_range_expr(ctx, builder, start.as_deref(), end.as_deref(), *inclusive)
        }

        // Await: check if this is awaiting a Task (spawn result) and dispatch via __gorget_await_<fn>.
        // In synchronous GIR mode for non-task expressions, just lower the inner expression.
        Expr::Await { expr } => {
            let inner = lower_expr(ctx, builder, expr);
            // Extract receiver local before inner is consumed by the call.
            let inner_local = match &inner {
                Operand::Copy(place) | Operand::Move(place)
                    if place.projections.is_empty() => Some(place.local),
                _ => None,
            };
            // Direct local lookup (simple `await task` case)
            let task_local = inner_local.and_then(|lid| {
                ctx.spawn.result_locals.get(&lid).cloned()
                    .map(|fn_name| (Some(lid), fn_name))
            });
            // Fallback: type-based lookup for indexed tasks (e.g., `await tasks[j]`)
            let resolved = task_local.or_else(|| {
                let type_id = inner_local.map(|lid| builder.local_type(lid));
                type_id.and_then(|tid| {
                    ctx.spawn.task_type_fns.get(&tid).and_then(|fns| {
                        if fns.len() == 1 { Some((None, fns[0].clone())) } else { None }
                    })
                })
            });
            if let Some((maybe_local_id, fn_name)) = resolved {
                let ret_type = ctx.fn_sigs.get(fn_name.as_str())
                    .map(|(_, r)| *r)
                    .unwrap_or(UNIT_TYPE);

                let await_fn = format!("__gorget_await_{fn_name}");
                let result = if ret_type == UNIT_TYPE {
                    builder.call_void(&await_fn, vec![inner]);
                    Operand::Constant(Constant::Unit)
                } else {
                    let dst = builder.call(&await_fn, vec![inner], ret_type);
                    FunctionBuilder::copy(dst)
                };

                // Zero out the Task local after await to prevent double-join in drop.
                // For direct spawn results, maybe_local_id is Some. For tasks from
                // other sources (e.g. Vector.remove().unwrap()), use inner_local.
                let zero_local = maybe_local_id.or(inner_local);
                if let Some(local_id) = zero_local {
                    ctx.move_zero_and_mark(builder, local_id);
                }

                // Auto-refresh `with shared_var:` bindings after await
                emit_with_shared_refresh(ctx, builder);

                return result;
            }
            inner
        }

        // Spawn: emit __gorget_spawn_<fn>(args) call, which creates a pthread.
        // Task result locals are tracked in spawn_result_locals for await dispatch.
        Expr::Spawn { expr } => {
            if let Expr::Call { callee, args: call_args, .. } = &expr.node {
                // ── Case A: spawn c(args) where c is a local closure variable ──
                if let Expr::Identifier(fn_name) = &callee.node {
                    if let Some((local_id, local_type_id)) = ctx.lookup_local(fn_name) {
                        if let Some(type_name) = ctx.type_name_for_id(local_type_id).map(|s| s.to_string()) {
                            if ctx.lookup_closure_info(&type_name).is_some() {
                                let (call_fn_name, struct_type_id, captures) =
                                    ctx.lookup_closure_info(&type_name)
                                        .map(|(cfn, stid, caps)| {
                                            (cfn.to_string(), stid, caps.to_vec())
                                        })
                                        .unwrap();
                                let call_args_cloned: Vec<_> = call_args.iter().cloned().collect();
                                return lower_closure_spawn(
                                    ctx, builder,
                                    local_id, local_type_id,
                                    &type_name, &call_fn_name, struct_type_id,
                                    &captures, &call_args_cloned,
                                );
                            }
                        }
                    }
                }

                // ── Case B: spawn ((): body)(args) — inline closure literal ──
                if let Expr::Closure { params, body, is_move, .. } = &callee.node {
                    let params_cloned = params.clone();
                    let body_cloned = body.clone();
                    let is_move_val = *is_move;
                    let call_args_cloned: Vec<_> = call_args.iter().cloned().collect();

                    let mut cl = std::mem::take(&mut ctx.closures);
                    let closure_op = cl.lower_closure(ctx, builder, &params_cloned, &body_cloned, is_move_val);
                    ctx.closures = cl;

                    if let Operand::Copy(ref place) | Operand::Move(ref place) = closure_op {
                        if place.projections.is_empty() {
                            let closure_local = place.local;
                            let closure_type_id = builder.local_type(closure_local);
                            if let Some(type_name) = ctx.type_name_for_id(closure_type_id).map(|s| s.to_string()) {
                                if ctx.lookup_closure_info(&type_name).is_some() {
                                    let (call_fn_name, struct_type_id, captures) =
                                        ctx.lookup_closure_info(&type_name)
                                            .map(|(cfn, stid, caps)| {
                                                (cfn.to_string(), stid, caps.to_vec())
                                            })
                                            .unwrap();
                                    return lower_closure_spawn(
                                        ctx, builder,
                                        closure_local, closure_type_id,
                                        &type_name, &call_fn_name, struct_type_id,
                                        &captures, &call_args_cloned,
                                    );
                                }
                            }
                        }
                    }
                    // Inline closure lowering succeeded but no closure info found — fall through
                }

                // ── Direct function call spawn (original path) ──
                if let Expr::Identifier(fn_name) = &callee.node {
                    // Resolve the actual C symbol name (Phase 5 mangled for module functions,
                    // or bare Gorget name for entry-module functions).  The spawn infrastructure
                    // (context struct, thread wrapper, spawn/await helpers) is keyed by this
                    // C name so that the internal call uses the right symbol.
                    let c_name = ctx.extern_bindings.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_else(|| fn_name.clone());

                    // fn_sigs is keyed by the Gorget bare name for lookup purposes.
                    let callee_param_types = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(p, _)| p.clone())
                        .unwrap_or_default();
                    let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(_, r)| *r)
                        .unwrap_or(I64_TYPE);

                    // Detect shared args: check each call arg against shared_locals
                    let param_ownerships = ctx.fn_param_ownerships.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_default();

                    let mut shared_spawn_args: Vec<SharedSpawnArg> = Vec::new();
                    let mut has_any_shared = false;
                    for (i, arg) in call_args.iter().enumerate() {
                        if let Expr::Identifier(arg_name) = &arg.node.value.node {
                            if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                if let Some(info) = ctx.shared.locals.get(&local_id) {
                                    let (inner_type, wrapper_type, kind, ast_shared) = (info.inner_type, info.wrapper_type, info.kind, info.ast_shared);
                                    has_any_shared = true;
                                    // Only auto-decided shared vars get token wrappers.
                                    // User overrides (shared(atomic), shared(rwlock)) pass
                                    // the raw sync primitive — the callee uses it directly.
                                    if ast_shared == ast::SharedKind::Auto {
                                        let is_mutable = param_ownerships.get(i)
                                            .map_or(false, |o| matches!(o, Ownership::MutableBorrow));
                                        shared_spawn_args.push(SharedSpawnArg {
                                            arg_index: i,
                                            kind,
                                            inner_type,
                                            wrapper_type,
                                            is_mutable,
                                            decl_order: local_id.0,
                                        });
                                    }
                                }
                            }
                        }
                    }

                    // Detect inner shared spawn: when an arg is a param of the
                    // current function (not a declared shared), record the mapping
                    // so the shared_async transform can propagate the wrapper.
                    if shared_spawn_args.is_empty() {
                        let mut inner_mappings: Vec<(usize, usize)> = Vec::new();
                        for (i, arg) in call_args.iter().enumerate() {
                            if let Expr::Identifier(arg_name) = &arg.node.value.node {
                                if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                    let idx = local_id.0 as usize;
                                    // Is this a param? params are locals _1.._N
                                    if idx >= 1 && idx <= builder.params.len() {
                                        let param_idx = idx - 1; // 0-based param index
                                        // Only record if the callee expects a mutable borrow
                                        let callee_is_mut = param_ownerships.get(i)
                                            .map_or(false, |o| matches!(o, Ownership::MutableBorrow));
                                        if callee_is_mut {
                                            inner_mappings.push((i, param_idx));
                                        }
                                    }
                                }
                            }
                        }
                        if !inner_mappings.is_empty() {
                            let callee_has_awaits = ctx.shared.fn_ast_bodies.get(fn_name.as_str())
                                .map_or(false, |func_def| {
                                    if let crate::parser::ast::FunctionBody::Block(block) = &func_def.body {
                                        block.stmts.iter().any(|s| super::context::stmt_has_await(&s.node))
                                    } else {
                                        false
                                    }
                                });
                            builder.inner_shared_spawns.push(crate::ir::InnerSharedSpawn {
                                callee_name: c_name.clone(),
                                callee_param_types: callee_param_types.clone(),
                                callee_return_type: fn_ret_type,
                                shared_arg_mappings: inner_mappings,
                                callee_has_awaits,
                                callee_param_ownerships: param_ownerships.clone(),
                            });
                        }
                    }

                    // Map return TypeId → C type name → Task__<c_type> name.
                    // Normalize GorgetString→GorgetStringView so the task type matches user annotations
                    // like `Task[str]` which mangle to Task__GorgetStringView.
                    let ret_c = ctx.type_name_for_id(fn_ret_type)
                        .unwrap_or("int64_t")
                        .to_string();
                    let ret_c = if ret_c == "GorgetString" { "GorgetStringView".to_string() } else { ret_c };
                    let task_name = if fn_ret_type == UNIT_TYPE {
                        "Task__void".to_string()
                    } else {
                        format!("Task__{ret_c}")
                    };
                    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
                        tid
                    } else {
                        // Register Task TypeDef with Move semantics + RAII join-on-drop.
                        ctx.type_registry.add_type_def(TypeDef {
                            name: task_name.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                                copy_semantics: CopySemantics::Resource,
                                ..Default::default()
                            },
                        });
                        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
                        ctx.type_mapper.register_named(task_name.clone(), tid);
                        tid
                    };

                    if !shared_spawn_args.is_empty() {
                        // Check if callee is async with await points — needs
                        // async-aware token management (release at await, reacquire after).
                        let callee_has_awaits = ctx.shared.fn_ast_bodies.get(fn_name.as_str())
                            .map_or(false, |func_def| {
                                if let crate::parser::ast::FunctionBody::Block(block) = &func_def.body {
                                    block.stmts.iter().any(|s| super::context::stmt_has_await(&s.node))
                                } else {
                                    false
                                }
                            });

                        let wrapper_name = if callee_has_awaits {
                            format!("__shared_async_{c_name}")
                        } else {
                            format!("__shared_token_{c_name}")
                        };

                        if !ctx.spawn.fn_names.contains_key(&wrapper_name) {
                            if callee_has_awaits {
                                // Async-aware variant: defer generation until after all functions
                                // are lowered. The GIR-to-GIR transform will operate on the
                                // already-lowered source function.
                                use crate::ir::transforms::shared_async::{SharedArgSpec, PendingSharedVariant};
                                let specs: Vec<SharedArgSpec> = shared_spawn_args.iter().map(|sa| {
                                    let inner_c = ctx.c_type_name_for_id(sa.inner_type);
                                    let mutex_mangled = format!("Mutex__{inner_c}");
                                    let guard_mangled = format!("Guard__{inner_c}");
                                    let mutex_type = ctx.type_mapper.lookup_named(&mutex_mangled)
                                        .unwrap_or(sa.inner_type);
                                    let guard_type = ctx.type_mapper.lookup_named(&guard_mangled)
                                        .unwrap_or(sa.inner_type);
                                    SharedArgSpec {
                                        arg_index: sa.arg_index,
                                        inner_type: sa.inner_type,
                                        wrapper_type: sa.wrapper_type,
                                        mutex_type,
                                        guard_type,
                                        is_mutable: sa.is_mutable,
                                        decl_order: sa.decl_order,
                                        inner_c_name: inner_c,
                                    }
                                }).collect();
                                ctx.shared.pending_variants.push(PendingSharedVariant {
                                    source_fn_name: c_name.clone(),
                                    variant_name: wrapper_name.clone(),
                                    shared_args: specs,
                                    return_type: fn_ret_type,
                                });
                            } else {
                                // Synchronous wrapper: lock for entire call, no await points.
                                let wrapper_fn = build_shared_token_wrapper(
                                    ctx,
                                    &wrapper_name,
                                    &c_name,
                                    &callee_param_types,
                                    &shared_spawn_args,
                                    fn_ret_type,
                                );
                                ctx.spawn.wrapper_fns.push(wrapper_fn);
                            }

                            // Register wrapper signature: wrapper params → return type
                            let wrapper_param_types: Vec<TypeId> = callee_param_types.iter().enumerate()
                                .map(|(i, &callee_type)| {
                                    shared_spawn_args.iter()
                                        .find(|sa| sa.arg_index == i)
                                        .map(|sa| sa.wrapper_type)
                                        .unwrap_or(callee_type)
                                })
                                .collect();
                            ctx.fn_sigs.insert(wrapper_name.clone(), (wrapper_param_types, fn_ret_type));

                            let param_names: Vec<String> = (0..callee_param_types.len())
                                .map(|i| format!("__p{i}"))
                                .collect();
                            ctx.fn_param_names.insert(wrapper_name.clone(), param_names);
                        }

                        ctx.spawn.pending_fn = Some(wrapper_name.clone());
                        ctx.spawn.fn_names.insert(wrapper_name.clone(), true);
                        ctx.spawn.register_task_type_fn(task_type, wrapper_name.clone());

                        // Lower args: shared vars pass the raw sync primitive
                        ctx.shared.pass_raw = true;
                        let lowered_args: Vec<Operand> = call_args.iter()
                            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                            .collect();
                        ctx.shared.pass_raw = false;

                        let spawn_fn = format!("__gorget_spawn_{wrapper_name}");
                        let dst = builder.call(&spawn_fn, lowered_args, task_type);
                        return FunctionBuilder::copy(dst);
                    } else {
                        // No Auto shared args — spawn the original function directly.
                        // If there are user-overridden shared vars, pass them raw.
                        ctx.spawn.pending_fn = Some(c_name.clone());
                        ctx.spawn.fn_names.insert(c_name.clone(), true);
                        ctx.spawn.register_task_type_fn(task_type, c_name.clone());

                        if has_any_shared {
                            ctx.shared.pass_raw = true;
                        }
                        let lowered_args: Vec<Operand> = call_args.iter()
                            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                            .collect();
                        ctx.shared.pass_raw = false;
                        let spawn_fn = format!("__gorget_spawn_{c_name}");
                        let dst = builder.call(&spawn_fn, lowered_args, task_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // ── Case D: spawn receiver.method(args) — method call ──
            if let Expr::MethodCall { receiver, method, args: call_args, .. } = &expr.node {
                return lower_method_spawn(ctx, builder, receiver, &method.node, call_args);
            }
            // Fallback: direct call (no tracking)
            lower_expr(ctx, builder, expr)
        }

        // spawn blocking fn(args) — runs on the expandable blocking pool
        Expr::SpawnBlocking { expr } => {
            if let Expr::Call { callee, args: call_args, .. } = &expr.node {
                if let Expr::Identifier(fn_name) = &callee.node {
                    let c_name = ctx.extern_bindings.get(fn_name.as_str())
                        .cloned()
                        .unwrap_or_else(|| fn_name.clone());

                    let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                        .map(|(_, r)| *r)
                        .unwrap_or(I64_TYPE);

                    // Normalize GorgetString→GorgetStringView so the task type matches user annotations
                    // like `Task[str]` which mangle to Task__GorgetStringView.
                    let ret_c = ctx.type_name_for_id(fn_ret_type)
                        .unwrap_or("int64_t")
                        .to_string();
                    let ret_c = if ret_c == "GorgetString" { "GorgetStringView".to_string() } else { ret_c };
                    let task_name = if fn_ret_type == UNIT_TYPE {
                        "Task__void".to_string()
                    } else {
                        format!("Task__{ret_c}")
                    };
                    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
                        tid
                    } else {
                        ctx.type_registry.add_type_def(TypeDef {
                            name: task_name.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                                copy_semantics: CopySemantics::Resource,
                                ..Default::default()
                            },
                        });
                        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
                        ctx.type_mapper.register_named(task_name.clone(), tid);
                        tid
                    };

                    ctx.spawn.pending_fn = Some(c_name.clone());
                    ctx.spawn.fn_names.insert(c_name.clone(), true);
                    ctx.spawn.blocking_fn_names.insert(c_name.clone());
                    ctx.spawn.register_task_type_fn(task_type, c_name.clone());

                    let lowered_args: Vec<Operand> = call_args.iter()
                        .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                        .collect();
                    let spawn_fn = format!("__gorget_spawn_{c_name}");
                    let dst = builder.call(&spawn_fn, lowered_args, task_type);
                    return FunctionBuilder::copy(dst);
                }
            }
            lower_expr(ctx, builder, expr)
        }

        // Dot-shorthand variant: .Red() or .Blue(42)
        // Resolves to the enum variant using the expected type from context.
        Expr::DotShorthand { variant, args } => {
            let variant_name = variant.node.clone();
            let lowered_args: Vec<Operand> = args.iter()
                .map(|a| lower_expr(ctx, builder, &a.node.value))
                .collect();

            // 1. Try expected_type (set by VarDecl, Assign, Return, or function arg)
            if let Some(et) = ctx.expected_type {
                if let Some(type_name) = ctx.type_registry.type_name(et) {
                    if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                        if let TypeDefKind::Enum(ref e) = type_def.kind {
                            if e.variants.iter().any(|v| v.name == variant_name) {
                                let dst = ctx.emit_enum_init_owned(builder, &type_name, &variant_name, et, lowered_args);
                                return FunctionBuilder::copy(dst);
                            }
                        }
                    }
                }
            }

            // 2. Fallback: variant map (for user-defined non-generic enums)
            if let Some((enum_name, vn)) = ctx.resolve_enum_variant(&variant_name) {
                let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                let dst = ctx.emit_enum_init_owned(builder, &enum_name, &vn, type_id, lowered_args);
                return FunctionBuilder::copy(dst);
            }

            Operand::Constant(Constant::Unit)
        }
        Expr::MetaOpInfix { .. } => {
            // Should have been substituted by the meta pass before lowering.
            panic!("MetaOpInfix not substituted before GIR lowering — meta substitution pass incomplete")
        }
        Expr::MetaOpToken(_) => {
            // Should have been filtered out at the call site before reaching here.
            panic!("MetaOpToken not filtered out before GIR lowering — call lowering incomplete")
        }
        Expr::Rethrow { expr: inner, error_binding, transform } => {
            lower_rethrow_expr(ctx, builder, inner, error_binding.as_ref(), transform)
        }
        Expr::Catch { expr: inner, error_binding, recovery } => {
            lower_catch_expr(ctx, builder, inner, error_binding, recovery)
        }
    }
}

/// Lower a struct literal (constructor call).
/// Resolve Option/Result variant constructors (Some, None, Ok, Error) with type-aware logic.
/// Returns Some(operand) if the call is a recognized built-in variant, None otherwise.
fn resolve_option_result_variant(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &str,
    args: &[Spanned<Expr>],
) -> Option<Operand> {
    match name {
        "Some" if args.len() == 1 => {
            let field_op = lower_expr(ctx, builder, &args[0]);
            let inner_type = infer_operand_type_full(ctx, &field_op, builder);
            let mangled = format!("Option__{}", format_type_for_mangle(inner_type, &ctx.type_registry));
            let type_id = ctx.type_mapper.lookup_named(&mangled)
                .or_else(|| {
                    // Fall back to expected type from context (e.g., VarDecl target)
                    ctx.expected_type.and_then(|et| {
                        let name = ctx.type_registry.type_name(et)?;
                        let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option)
                            || name.starts_with("Option__");
                        if is_option {
                            Some(et)
                        } else {
                            None
                        }
                    })
                })
                .unwrap_or(UNIT_TYPE);
            let type_name = ctx.type_registry.type_name(type_id).unwrap_or_else(|| mangled.clone());
            let dst = builder.enum_init(&type_name, "Some", type_id, vec![field_op]);
            ctx.owned_locals.insert(dst);
            Some(FunctionBuilder::copy(dst))
        }
        "None" if args.is_empty() => {
            // None() has no arguments — determine type from context
            let (type_name, type_id) = if let Some(et) = ctx.expected_type {
                let name = ctx.type_registry.type_name(et)
                    .unwrap_or_else(|| "Option__int64_t".to_string());
                let is_option = ctx.type_registry.enum_category(et) == Some(EnumCategory::Option)
                    || name.starts_with("Option__");
                if is_option {
                    (name, et)
                } else {
                    // Expected type isn't Option — fall back to enum_variants
                    return None;
                }
            } else {
                // No context — fall back to enum_variants
                return None;
            };
            let dst = builder.enum_init(&type_name, "None", type_id, vec![]);
            ctx.owned_locals.insert(dst);
            Some(FunctionBuilder::copy(dst))
        }
        "Ok" if args.len() == 1 => {
            // Ok(value) — determine Result type from context (expected_type)
            if let Some(et) = ctx.expected_type {
                let name = ctx.type_registry.type_name(et).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(et) == Some(EnumCategory::Result)
                    || name.starts_with("Result__");
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let consumed = if let Operand::Copy(ref p) | Operand::Move(ref p) = field_op {
                        if p.projections.is_empty() { Some(p.local) } else { None }
                    } else { None };
                    let dst = builder.enum_init(&name, "Ok", et, vec![field_op]);
                    ctx.owned_locals.insert(dst);
                    if let Some(local) = consumed {
                        ctx.move_zero_and_mark(builder, local);
                    }
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            // Also check current_throws_result_type
            if let Some(rt) = ctx.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(rt) == Some(EnumCategory::Result)
                    || name.starts_with("Result__");
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let consumed = if let Operand::Copy(ref p) | Operand::Move(ref p) = field_op {
                        if p.projections.is_empty() { Some(p.local) } else { None }
                    } else { None };
                    let dst = builder.enum_init(&name, "Ok", rt, vec![field_op]);
                    ctx.owned_locals.insert(dst);
                    if let Some(local) = consumed {
                        ctx.move_zero_and_mark(builder, local);
                    }
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            None // Fall through to generic enum_variants
        }
        "Error" if args.len() == 1 => {
            // Error(value) — determine Result type from context
            if let Some(et) = ctx.expected_type {
                let name = ctx.type_registry.type_name(et).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(et) == Some(EnumCategory::Result)
                    || name.starts_with("Result__");
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = builder.enum_init(&name, "Error", et, vec![field_op]);
                    ctx.owned_locals.insert(dst);
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            if let Some(rt) = ctx.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                let is_result = ctx.type_registry.enum_category(rt) == Some(EnumCategory::Result)
                    || name.starts_with("Result__");
                if is_result {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = builder.enum_init(&name, "Error", rt, vec![field_op]);
                    ctx.owned_locals.insert(dst);
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            None
        }
        _ => None,
    }
}

fn lower_struct_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &str,
    args: &[Spanned<Expr>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    // Intercept String("...") constructor → gorget_string_from_str(str)
    // Intercept String(capacity) constructor → gorget_string_with_capacity(int)
    if name == "String" && args.len() == 1 {
        let arg_op = lower_expr(ctx, builder, &args[0]);
        let owned_type = ctx.type_mapper.owned_string_type;
        // Check if the arg is an integer (capacity) vs string (content)
        let arg_type = super::exprs::infer_operand_type_full(ctx, &arg_op, builder);
        let fn_name = if arg_type == I64_TYPE || arg_type == I32_TYPE {
            "gorget_string_with_capacity"
        } else {
            "gorget_string_from_str"
        };
        let dst = ctx.call_extern_tracked(builder, fn_name, vec![arg_op], owned_type);
        return FunctionBuilder::copy(dst);
    }
    // String() with no args → empty GorgetString
    if name == "String" && args.is_empty() {
        return Operand::Constant(Constant::Unit); // C backend handles Unit → gorget_string_new("")
    }

    // Box(value) constructor → heap allocation via __gorget_box_alloc
    if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        // Determine inner type name for the mangled Box type
        let inner_c = if let Some(rest) = name.strip_prefix("Box__") {
            // Already mangled (e.g., "Box__int64_t") — use the suffix directly
            rest.to_string()
        } else {
            ctx.type_name_for_id(val_type)
                .unwrap_or("int64_t")
                .to_string()
        };
        let box_mangled = format!("Box__{inner_c}");
        let box_type = if let Some(tid) = ctx.type_mapper.lookup_named(&box_mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(box_mangled.clone()));
            ctx.type_mapper.register_named(box_mangled.clone(), tid);
            super::exprs::ensure_box_type_def(ctx, &box_mangled, val_type);
            tid
        };
        // Emit: __gorget_box_alloc_T(value) → T* with heap alloc
        let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
        let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
        return FunctionBuilder::copy(dst);
    }

    // WaitGroup() → gorget_waitgroup_new()
    if name == "WaitGroup" && args.is_empty() {
        let wg_type = ctx.type_mapper.lookup_named("WaitGroup").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_waitgroup_new", vec![], wg_type);
        return FunctionBuilder::copy(dst);
    }

    // Semaphore(n) → gorget_semaphore_new(n)
    if name == "Semaphore" && args.len() == 1 {
        let n_op = lower_expr(ctx, builder, &args[0]);
        let s_type = ctx.type_mapper.lookup_named("Semaphore").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_semaphore_new", vec![n_op], s_type);
        return FunctionBuilder::copy(dst);
    }

    // OnceFlag() → gorget_onceflag_new()
    if name == "OnceFlag" && args.is_empty() {
        let of_type = ctx.type_mapper.lookup_named("OnceFlag").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_onceflag_new", vec![], of_type);
        return FunctionBuilder::copy(dst);
    }

    // Determine the effective type name (mangled if generic)
    let effective_name = if let Some(type_args) = generic_args {
        if !type_args.is_empty() {
            let mangled = super::types::mangle_generic_name(name, type_args);
            // Apply type name substitutions for generic monomorphization
            ctx.resolve_type_name(&mangled)
        } else {
            name.to_string()
        }
    } else {
        name.to_string()
    };

    // Intercept Channel__T constructor → Channel__T__new(cap) — capacity arg would be dropped
    // by generic struct init, so we route through a named constructor function.
    if effective_name.starts_with("Channel__") && args.len() == 1 {
        let cap = lower_expr(ctx, builder, &args[0]);
        let chan_type = if let Some(tid) = ctx.type_mapper.lookup_named(&effective_name) {
            tid
        } else {
            // Lazily register if not pre-registered by map_ast_type_mut (body-only usage).
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(effective_name.clone()));
            ctx.type_mapper.register_named(effective_name.clone(), tid);
            ensure_channel_type_def(ctx, &effective_name);
            tid
        };
        let new_fn = format!("{effective_name}__new");
        let dst = builder.call(&new_fn, vec![cap], chan_type);
        return FunctionBuilder::copy(dst);
    }

    // Intercept Shared__T constructor → gorget_shared_new(sizeof(T), &val) → GorgetShared*
    if (effective_name == "Shared" || effective_name.starts_with("Shared__")) && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        let inner_c = if let Some(rest) = effective_name.strip_prefix("Shared__") {
            rest.to_string()
        } else {
            ctx.c_type_name_for_id(val_type)
        };
        let shared_mangled = format!("Shared__{inner_c}");
        let shared_type = if let Some(tid) = ctx.type_mapper.lookup_named(&shared_mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(shared_mangled.clone()));
            ctx.type_mapper.register_named(shared_mangled.clone(), tid);
            ensure_shared_type_def(ctx, &shared_mangled, val_type);
            tid
        };
        let new_fn = format!("{shared_mangled}__new");
        let dst = builder.call(&new_fn, vec![val_op.clone()], shared_type);
        // Shared[T](v) takes ownership of v's data via a shallow memcpy into the shared
        // block. If v is a Move-semantics local (e.g. Vector/GorgetArray), mark it as
        // moved so the drop elaborator emits a null-guarded DropIfAlive instead of an
        // unconditional free — otherwise the shared block would hold a dangling data pointer.
        if let Operand::Copy(place) = &val_op {
            if place.projections.is_empty() {
                if is_resource_type_local(place.local, builder, &ctx.type_registry) {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
        return FunctionBuilder::copy(dst);
    }

    // Intercept Mutex__T constructor → gorget_mutex_new(sizeof(T), &val) → GorgetMutex*
    if (effective_name == "Mutex" || effective_name.starts_with("Mutex__")) && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
        let inner_c = if let Some(rest) = effective_name.strip_prefix("Mutex__") {
            rest.to_string()
        } else {
            ctx.c_type_name_for_id(val_type)
        };
        let mutex_mangled = format!("Mutex__{inner_c}");
        let mutex_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mutex_mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(mutex_mangled.clone()));
            ctx.type_mapper.register_named(mutex_mangled.clone(), tid);
            ensure_mutex_type_def(ctx, &mutex_mangled, val_type);
            tid
        };
        let new_fn = format!("{mutex_mangled}__new");
        let dst = builder.call(&new_fn, vec![val_op], mutex_type);
        return FunctionBuilder::copy(dst);
    }

    // AtomicInt(val) → gorget_atomic_int_new(val)
    if effective_name == "AtomicInt" && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let at_type = ctx.type_mapper.lookup_named("AtomicInt").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_atomic_int_new", vec![val_op], at_type);
        return FunctionBuilder::copy(dst);
    }

    // AtomicBool(val) → gorget_atomic_bool_new(val)
    if effective_name == "AtomicBool" && args.len() == 1 {
        let val_op = lower_expr(ctx, builder, &args[0]);
        let at_type = ctx.type_mapper.lookup_named("AtomicBool").unwrap_or(BOOL_TYPE);
        let dst = builder.call_extern("gorget_atomic_bool_new", vec![val_op], at_type);
        return FunctionBuilder::copy(dst);
    }

    // Barrier(n) → gorget_barrier_new(n)
    if effective_name == "Barrier" && args.len() == 1 {
        let n_op = lower_expr(ctx, builder, &args[0]);
        let b_type = ctx.type_mapper.lookup_named("Barrier").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_barrier_new", vec![n_op], b_type);
        return FunctionBuilder::copy(dst);
    }

    // CondVar() → gorget_condvar_new()
    if effective_name == "CondVar" && args.is_empty() {
        let cv_type = ctx.type_mapper.lookup_named("CondVar").unwrap_or(I64_TYPE);
        let dst = builder.call_extern("gorget_condvar_new", vec![], cv_type);
        return FunctionBuilder::copy(dst);
    }

    // RWLock[T](val) → RWLock__T__new(val) — follows the Mutex pattern
    if effective_name == "RWLock" || effective_name.starts_with("RWLock__") {
        if !args.is_empty() {
            let val_op = lower_expr(ctx, builder, &args[0]);
            let val_type = super::exprs::infer_operand_type_full(ctx, &val_op, builder);
            let inner_c = if let Some(rest) = effective_name.strip_prefix("RWLock__") {
                rest.to_string()
            } else {
                ctx.c_type_name_for_id(val_type)
            };
            let rw_mangled = format!("RWLock__{inner_c}");
            let rw_type = if let Some(tid) = ctx.type_mapper.lookup_named(&rw_mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(rw_mangled.clone()));
                ctx.type_mapper.register_named(rw_mangled.clone(), tid);
                tid
            };
            let new_fn = format!("{rw_mangled}__new");
            let dst = builder.call(&new_fn, vec![val_op], rw_type);
            return FunctionBuilder::copy(dst);
        }
    }

    // Intercept TaskGroup.new() static constructor
    if effective_name == "TaskGroup" && args.is_empty() {
        let tg_mangled = "TaskGroup";
        let tg_type = if let Some(tid) = ctx.type_mapper.lookup_named(tg_mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(tg_mangled.to_string()));
            ctx.type_mapper.register_named(tg_mangled.to_string(), tid);
            ensure_task_group_type_def(ctx, tg_mangled);
            tid
        };
        let dst = builder.call("gorget_task_group_new", vec![], tg_type);
        return FunctionBuilder::copy(dst);
    }

    // Check if this is an Option/Result variant constructor — resolve with type-aware logic
    // to avoid ambiguity when multiple monomorphized types share variant names.
    if let Some(result) = resolve_option_result_variant(ctx, builder, name, args) {
        return result;
    }

    // Check if this is an enum variant constructor
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&effective_name) {
        let field_operands: Vec<Operand> = args.iter()
            .map(|arg| lower_expr(ctx, builder, arg))
            .collect();
        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }
    // Also check the base name for non-generic enum variants
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
        let field_operands: Vec<Operand> = args.iter()
            .map(|arg| lower_expr(ctx, builder, arg))
            .collect();
        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }

    // Regular struct literal
    let mut field_operands: Vec<Operand> = args.iter()
        .map(|arg| lower_expr(ctx, builder, arg))
        .collect();

    // Auto-clone Ptr(collection) operands used as struct fields.
    // Field loads return Ptr(T) for collection fields. When passed to StructInit,
    // the struct needs an owned copy (T), not a pointer. Clone via runtime function.
    for (i, op) in field_operands.iter_mut().enumerate() {
        if let Operand::Copy(place) | Operand::Move(place) = op {
            if place.projections.is_empty() {
                let idx = place.local.0 as usize;
                if idx < builder.locals.len() {
                    let local_type = builder.locals[idx].type_id;
                    if let Some(inner) = ctx.pointee_type(local_type) {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                            if let Some(arg_span) = args.get(i).map(|a| a.span) {
                                ctx.warn_implicit_clone(arg_span, inner, crate::ir::ImplicitCloneReason::StructFieldFromBorrow);
                            }
                            let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(place.local)], inner);
                            *op = FunctionBuilder::copy(cloned);
                        }
                    }
                }
            }
        }
    }

    // Clone string param locals before struct init — params hold views of
    // caller's data; structs must own their fields.
    for op in field_operands.iter_mut() {
        if let Operand::Copy(place) | Operand::Move(place) = op {
            if place.projections.is_empty() {
                if let Some(owned_op) = ctx.ensure_owned_string(builder, place.local) {
                    *op = owned_op;
                }
            }
        }
    }

    // Unregister GorgetString temps used as struct fields — they may be stored
    // as Str views that outlive the current scope.
    // TODO: narrow or remove once owned_locals tracking is comprehensive
    unregister_gorget_string_args(ctx, builder, &field_operands);

    // Phase 1f: clone multi-use resource args BEFORE struct init.
    clone_multi_use_resource_args(ctx, builder, &mut field_operands, args);

    let type_id = ctx.type_mapper.lookup_named(&effective_name).unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(&effective_name, type_id, field_operands.clone());
    ctx.owned_locals.insert(dst);

    // Phase 1f: MoveZero single-use/temp sources AFTER struct init.
    move_zero_consumed_args(ctx, builder, &field_operands);

    FunctionBuilder::copy(dst)
}

/// Lower a field access expression.
fn lower_field_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Operand {
    // Check for qualified enum variant without parens: Color.Red
    // If the object is a type name (not a local) and the field is a variant, emit EnumInit.
    if let Expr::Identifier(name) = &object.node {
        if ctx.lookup_local(name).is_none() && !ctx.module_constants.contains_key(name) {
            if let Some(type_id) = ctx.type_mapper.lookup_named(name) {
                if let Some(type_def) = ctx.type_registry.get_type_def(name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if e.variants.iter().any(|v| v.name == field_name) {
                            let dst = builder.enum_init(name, field_name, type_id, vec![]);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
        }
    }

    // For mut_capture_locals (mutable borrow params), use the pointer directly
    // so field access goes through the pointer (*ptr).field instead of copying
    let obj = if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.mut_capture_locals.contains_key(&local_id) {
                Operand::Copy(Place::local(local_id))
            } else {
                lower_expr(ctx, builder, object)
            }
        } else {
            lower_expr(ctx, builder, object)
        }
    } else {
        lower_expr(ctx, builder, object)
    };

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // Guard[T] auto-deref: guard.field → (*get_ptr(&guard)).field
            if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
                let type_name = type_name.to_string();
                if let Some((inner_suffix, _is_read_only)) = guard_inner_suffix(&type_name) {
                    let (inner_ptr_local, inner_type) = emit_guard_get_ptr(
                        ctx, builder, place, local_type_id, &type_name, inner_suffix,
                    );
                    let deref_place = Place {
                        local: inner_ptr_local,
                        projections: vec![Projection::Deref],
                    };
                    if let Some(inner_type_name) = ctx.type_name_for_id(inner_type) {
                        let inner_type_name = inner_type_name.to_string();
                        if let Some((field_idx, field_type)) = ctx.lookup_field(&inner_type_name, field_name) {
                            // Same resource-type field conversion as the non-Ptr path:
                            // collections → Ptr(T), GorgetString → Str view.
                            let result_type = if ctx.type_registry.is_collection_type(field_type) {
                                ctx.type_registry.insert(GirType::Ptr(field_type))
                            } else if field_type == ctx.type_mapper.owned_string_type {
                                ctx.type_mapper.string_view_type
                            } else {
                                field_type
                            };
                            let dst = builder.field_load(deref_place, field_idx, result_type);
                            return FunctionBuilder::copy(dst);
                        }
                        if let Some(type_def) = ctx.type_registry.get_type_def(&inner_type_name) {
                            if let TypeDefKind::Struct(ref s) = type_def.kind {
                                for (i, field) in s.fields.iter().enumerate() {
                                    if field.name == field_name {
                                        let result_type = if ctx.type_registry.is_collection_type(field.type_id) {
                                            ctx.type_registry.insert(GirType::Ptr(field.type_id))
                                        } else if field.type_id == ctx.type_mapper.owned_string_type {
                                            ctx.type_mapper.string_view_type
                                        } else {
                                            field.type_id
                                        };
                                        let dst = builder.field_load(deref_place, i as u32, result_type);
                                        return FunctionBuilder::copy(dst);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // If the local is a raw pointer (e.g., self in equip methods), dereference it
            // to get the underlying struct type for field access.
            // Box[T] types use explicit `*box` dereference in Gorget, handled by Expr::Deref.
            let (effective_type_id, base_place) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    // Pointer type: add Deref projection → (*_N).field
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (local_type_id, place.clone())
                };

            // Look up the type name, then the field info
            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                // Special case: GorgetString.data — return the GorgetString itself.
                // GorgetString TypeDef has no registered fields in GIR (it's opaque),
                // but accessing .data is valid for printf (%.*s handles it correctly).
                if type_name == "GorgetString" && field_name == "data" {
                    return obj;
                }
                // First try the struct_fields cache
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    // Resource-type fields: return a reference/view instead of a shallow copy.
                    // - Collections: Ptr(T) — prevents shared heap buffer double-free
                    // - GorgetString: Str (view) — same layout, non-owning, all consumers handle Str
                    // Auto-clone fires when assigned to an explicit-type variable.
                    let result_type = if ctx.type_registry.is_collection_type(field_type) {
                        ctx.type_registry.insert(GirType::Ptr(field_type))
                    } else if field_type == ctx.type_mapper.owned_string_type {
                        ctx.type_mapper.string_view_type
                    } else {
                        field_type
                    };
                    let dst = builder.field_load(base_place.clone(), field_idx, result_type);
                    return FunctionBuilder::copy(dst);
                }
                // Fallback: read directly from TypeDef
                if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, field) in s.fields.iter().enumerate() {
                            if field.name == field_name {
                                let result_type = if ctx.type_registry.is_collection_type(field.type_id) {
                                    ctx.type_registry.insert(GirType::Ptr(field.type_id))
                                } else if field.type_id == ctx.type_mapper.owned_string_type {
                                    ctx.type_mapper.string_view_type
                                } else {
                                    field.type_id
                                };
                                let dst = builder.field_load(base_place.clone(), i as u32, result_type);
                                return FunctionBuilder::copy(dst);
                            }
                        }
                    }
                }
            }
        }
    }

    // Fallback: can't resolve field
    Operand::Constant(Constant::Unit)
}

/// Resolve a field access expression to a Place (with projections) and the field's type,
/// WITHOUT copying the field to a temp. This allows borrowing the field in-place.
/// Returns `Some((place, field_type_id))` if the expression is a resolvable field access.
pub(super) fn try_resolve_field_place(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Option<(Place, TypeId)> {
    // Lower the object expression to get its local
    let obj = match &object.node {
        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                Operand::Copy(Place::local(local_id))
            } else {
                return None;
            }
        }
        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                return None;
            }
        }
        // Recursive case: chained field access (e.g., o.nested.items)
        Expr::FieldAccess { object: inner_obj, field: inner_field } => {
            if let Some((inner_place, _inner_type)) = try_resolve_field_place(ctx, builder, inner_obj, &inner_field.node) {
                Operand::Copy(inner_place)
            } else {
                return None;
            }
        }
        _ => return None,
    };

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let mut current_type = builder.locals[local_idx].type_id;

            // Walk existing projections to find the effective type at the end
            for proj in &place.projections {
                match proj {
                    Projection::Deref => {
                        if let Some(pointee) = ctx.pointee_type(current_type) {
                            current_type = pointee;
                        }
                    }
                    Projection::Field(idx) => {
                        if let Some(tn) = ctx.type_name_for_id(current_type) {
                            if let Some(type_def) = ctx.type_registry.get_type_def(tn) {
                                if let TypeDefKind::Struct(ref s) = type_def.kind {
                                    if (*idx as usize) < s.fields.len() {
                                        current_type = s.fields[*idx as usize].type_id;
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // If the resolved type is a pointer, dereference it
            let (effective_type_id, mut base_place) =
                if let Some(pointee) = ctx.pointee_type(current_type) {
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (current_type, place.clone())
                };

            // Look up the field
            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    base_place.projections.push(Projection::Field(field_idx));
                    return Some((base_place, field_type));
                }
                if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, field) in s.fields.iter().enumerate() {
                            if field.name == field_name {
                                base_place.projections.push(Projection::Field(i as u32));
                                return Some((base_place, field.type_id));
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Convert an index expression to a mangle fragment for generic type name construction.
/// e.g. `SparseSet[Health].new()` → receiver is `Index { object: "SparseSet", index: "Health" }`
/// Returns `Some("Health")` for `Identifier("Health")` or `Some("int64_t")` for `Identifier("int")`.
pub(super) fn index_expr_to_mangle_fragment(expr: &Expr) -> Option<String> {
    if let Expr::Identifier(name) = expr {
        let fragment = match name.as_str() {
            "int" => "int64_t",
            "float" => "double",
            "bool" => "bool",
            "str" | "String" => "GorgetStringView",
            "char" => "char",
            "byte" | "uint8" => "uint8_t",
            "uint16" => "uint16_t",
            "uint32" => "uint32_t",
            "uint64" => "uint64_t",
            "int8" => "int8_t",
            "int16" => "int16_t",
            "int32" => "int32_t",
            other => other,
        };
        Some(fragment.to_string())
    } else {
        None
    }
}

/// Lower a method call on a concrete (non-trait-object) type.
/// Lower an if expression (used as ternary).
fn lower_if_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_branch: &Spanned<Expr>,
    elif_branches: &[(Spanned<Expr>, Spanned<Expr>)],
    else_branch: Option<&Spanned<Expr>>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);

    // Allocate result local — we use I64_TYPE initially, then retroactively fix
    // the type after lowering the then-branch so the C backend sees the correct type.
    let result_id = builder.add_local(I64_TYPE, None);

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(cond, then_bb, else_bb);

    // Then branch
    builder.switch_to(then_bb);
    let then_val = lower_expr(ctx, builder, then_branch);
    // Fix the result local's type to match the actual then-branch type
    let result_type = infer_operand_type_full(ctx, &then_val, builder);
    if result_type != I64_TYPE {
        builder.set_local_type(result_id, result_type);
    }
    builder.assign(Place::local(result_id), then_val);
    builder.jump(merge_bb);

    // Elif branches — chain as nested if-else in the else block
    let mut current_else_bb = else_bb;
    for (elif_cond, elif_body) in elif_branches {
        builder.switch_to(current_else_bb);
        let elif_cond_val = lower_expr(ctx, builder, elif_cond);
        let elif_then_bb = builder.new_block();
        let next_else_bb = builder.new_block();
        builder.branch(elif_cond_val, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        let elif_val = lower_expr(ctx, builder, elif_body);
        builder.assign(Place::local(result_id), elif_val);
        builder.jump(merge_bb);

        current_else_bb = next_else_bb;
    }

    // Final else branch
    builder.switch_to(current_else_bb);
    if let Some(else_expr) = else_branch {
        let else_val = lower_expr(ctx, builder, else_expr);
        builder.assign(Place::local(result_id), else_val);
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::Unit));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}


/// Resolve the inner TypeId from a type name (e.g., "int64_t" → I64_TYPE).
pub fn resolve_none_tag(ctx: &LoweringContext, type_id: TypeId) -> i32 {
    if let Some(GirType::Named(name)) = ctx.type_registry.get(type_id) {
        if let Some(type_def) = ctx.type_registry.get_type_def(name) {
            if let TypeDefKind::Enum(ref e) = type_def.kind {
                // Find "None" variant by name, or fall back to last variant
                for (i, v) in e.variants.iter().enumerate() {
                    if v.name == "None" {
                        return i as i32;
                    }
                }
                return (e.variants.len() - 1) as i32;
            }
        }
    }
    1 // Default None tag for Option
}

/// Lower a call argument, respecting ownership (MutableBorrow creates a BorrowMut).
///
/// `callee_param_type` is the callee's declared parameter type from fn_sigs.
/// When the callee has a resource-type param, it's passed by pointer (const Ptr for bare,
/// MutPtr for &). We use the callee's param type (not the caller's local type) to decide,
/// avoiding mismatches like passing String to a function taking str.
fn lower_match_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchArm],
    else_arm: Option<&Spanned<Expr>>,
) -> Operand {
    // Lower scrutinee to a temp local
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    // Allocate result local (placeholder type — will be overwritten)
    let result_local = builder.add_local(I64_TYPE, None);
    let merge_bb = builder.new_block();

    for (i, arm) in arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        let cond = super::stmts::lower_pattern_condition(
            ctx, builder, &arm.pattern, scrut_local, scrut_type,
        );
        builder.branch(cond, arm_body_bb, next_test_bb);

        builder.switch_to(arm_body_bb);
        super::stmts::emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        let arm_val = lower_expr(ctx, builder, &arm.body);
        builder.assign(Place::local(result_local), arm_val);
        builder.jump(merge_bb);

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_expr) = else_arm {
        let else_val = lower_expr(ctx, builder, else_expr);
        builder.assign(Place::local(result_local), else_val);
    } else {
        builder.assign(Place::local(result_local), Operand::Constant(Constant::Unit));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Emit Result unwrap with error propagation.
/// Takes an already-lowered Result operand, branches on tag:
///   Ok → returns extracted Ok value
///   Error → emits on_error cleanups, early-exit drops, returns error
pub fn emit_result_auto_propagate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    result_operand: Operand,
    result_type: TypeId,
) -> Operand {
    let val_local = builder.add_local(result_type, None);
    builder.assign(Place::local(val_local), result_operand);

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, result_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value (field 0 of variant 0)
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load(
        Place::local(val_local),
        "Ok",
        0,
        ok_field_type,
    );
    builder.jump(merge_bb);

    // Error path: propagate error via early return
    builder.switch_to(err_bb);
    let err_val = builder.enum_field_load(
        Place::local(val_local),
        "Error",
        0,
        err_field_type,
    );
    // Re-wrap error in the *current* function's Result type and return.
    let fn_result_type = ctx.current_throws_result_type.or_else(|| {
        let ret_type = builder.locals[0].type_id;
        let type_name = ctx.type_registry.type_name(ret_type)?;
        let is_result = ctx.type_registry.enum_category(ret_type) == Some(EnumCategory::Result)
            || type_name.starts_with("Result__");
        if is_result {
            Some(ret_type)
        } else {
            None
        }
    });
    if let Some(fn_res_type) = fn_result_type {
        let type_name = ctx.type_registry.type_name(fn_res_type).unwrap_or_else(|| "Result".to_string());
        let err_dst = builder.enum_init(type_name, "Error", fn_res_type, vec![FunctionBuilder::copy(err_val)]);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
    } else {
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_val));
    }
    super::stmts::emit_on_error_cleanups(ctx, builder);
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
}

/// Extract Ok and Error field types from a Result type definition.
fn extract_result_field_types(ctx: &LoweringContext, result_type: TypeId) -> (TypeId, TypeId) {
    let type_name = ctx.type_registry.type_name(result_type);
    if let Some(ref name) = type_name {
        if let Some(td) = ctx.type_registry.get_type_def(name) {
            if let crate::ir::types::TypeDefKind::Enum(ref e) = td.kind {
                let ok_ty = e.variants.iter().find(|v| v.name == "Ok")
                    .and_then(|v| v.fields.first().map(|f| f.type_id))
                    .unwrap_or(I64_TYPE);
                let err_ty = e.variants.iter().find(|v| v.name == "Error")
                    .and_then(|v| v.fields.first().map(|f| f.type_id))
                    .unwrap_or(I64_TYPE);
                return (ok_ty, err_ty);
            }
        }
    }
    (I64_TYPE, I64_TYPE)
}

/// Check if a type is a Result type and the current function can propagate errors.
/// Returns the Result TypeId if auto-propagation should occur.
///
/// Triggers when:
/// 1. The operand type is `Result__*`, AND
/// 2. The current function can propagate: has `throws` OR returns `Result`
pub fn should_auto_propagate(ctx: &LoweringContext, builder: &FunctionBuilder, type_id: TypeId) -> Option<TypeId> {
    let type_name = ctx.type_registry.type_name(type_id)?;
    let is_result = ctx.type_registry.enum_category(type_id) == Some(EnumCategory::Result)
        || type_name.starts_with("Result__");
    if !is_result {
        return None;
    }
    // Check if current function can propagate
    if ctx.current_throws_result_type.is_some() {
        return Some(type_id);
    }
    let ret_type = builder.locals[0].type_id;
    let ret_name = ctx.type_registry.type_name(ret_type)?;
    let ret_is_result = ctx.type_registry.enum_category(ret_type) == Some(EnumCategory::Result)
        || ret_name.starts_with("Result__");
    if ret_is_result {
        return Some(type_id);
    }
    None
}

/// If operand is Result-typed and current function can propagate, auto-unwrap.
/// Otherwise return operand unchanged.
///
/// Skips auto-propagation when the expected destination type is itself a Result
/// (e.g., `Result[int, str] r = risky()` should keep the Result).
pub fn maybe_auto_propagate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
) -> Operand {
    // If the destination expects a Result, don't unwrap
    if let Some(expected) = ctx.expected_type {
        if let Some(name) = ctx.type_registry.type_name(expected) {
            let is_result = ctx.type_registry.enum_category(expected) == Some(EnumCategory::Result)
                || name.starts_with("Result__");
            if is_result {
                return operand;
            }
        }
    }
    let op_type = infer_operand_type_full(ctx, &operand, builder);
    if let Some(result_type) = should_auto_propagate(ctx, builder, op_type) {
        emit_result_auto_propagate(ctx, builder, operand, result_type)
    } else {
        operand
    }
}

/// Lower a rethrow expression:
///   `expr rethrow (Type name): transform`  (binding form)
///   `expr rethrow transform`               (bare form)
///
/// Like auto-propagation, but the error path evaluates a transform expression
/// and throws that instead. The binding form makes the original error available
/// to the transform; the bare form discards it.
fn lower_rethrow_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
    error_binding: Option<&(Spanned<crate::parser::ast::Type>, Spanned<String>)>,
    transform: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type_full(ctx, &val, builder);
    let val_local = builder.add_local(val_type, None);
    builder.assign(Place::local(val_local), val);

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, val_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value (identical to lower_try_expr)
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load(
        Place::local(val_local),
        "Ok",
        0,
        ok_field_type,
    );
    builder.jump(merge_bb);

    // Error path: optionally bind error to name, evaluate transform, throw that
    builder.switch_to(err_bb);
    if let Some((_error_type, error_name)) = error_binding {
        let err_val = builder.enum_field_load(
            Place::local(val_local),
            "Error",
            0,
            err_field_type,
        );
        let err_local = builder.add_local(err_field_type, Some(&error_name.node));
        builder.assign(Place::local(err_local), FunctionBuilder::copy(err_val));
        ctx.register_local(&error_name.node, err_local, err_field_type);
    }

    // Evaluate the transform expression — this produces the new error value
    let new_err = lower_expr(ctx, builder, transform);

    // Wrap the transformed error in the current function's Result.Error and return
    if let Some(result_type) = ctx.current_throws_result_type {
        let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
        let err_dst = builder.enum_init(type_name, "Error", result_type, vec![new_err]);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
    } else {
        builder.assign(Place::local(LocalId(0)), new_err);
    }
    // Emit on_error cleanups before drops
    super::stmts::emit_on_error_cleanups(ctx, builder);
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
}

/// Lower a `catch` expression: `expr catch (name): recovery`.
/// On Ok: returns the unwrapped Ok value.
/// On Error: binds error to `name`, evaluates `recovery`, returns that.
/// The overall expression always succeeds (never throws).
fn lower_catch_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
    error_binding: &Spanned<String>,
    recovery: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type_full(ctx, &val, builder);
    let val_local = builder.add_local(val_type, None);
    builder.assign(Place::local(val_local), val);

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = extract_result_field_types(ctx, val_type);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    // Allocate result local for the merged value (Ok type)
    let result_local = builder.add_local(ok_field_type, None);

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value, store into result
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load(
        Place::local(val_local),
        "Ok",
        0,
        ok_field_type,
    );
    builder.assign(Place::local(result_local), FunctionBuilder::copy(ok_val));
    builder.jump(merge_bb);

    // Error path: bind error, evaluate recovery, store into result
    builder.switch_to(err_bb);
    let err_val = builder.enum_field_load(
        Place::local(val_local),
        "Error",
        0,
        err_field_type,
    );
    let err_local = builder.add_local(err_field_type, Some(&error_binding.node));
    builder.assign(Place::local(err_local), FunctionBuilder::copy(err_val));
    ctx.register_local(&error_binding.node, err_local, err_field_type);

    let recovery_val = lower_expr(ctx, builder, recovery);
    builder.assign(Place::local(result_local), recovery_val);
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Lower a block expression — the last expression in the block is the value.
fn lower_block_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &ast::Block,
) -> Operand {
    if block.stmts.is_empty() {
        return Operand::Constant(Constant::Unit);
    }

    // Lower all but the last statement normally
    for stmt in &block.stmts[..block.stmts.len() - 1] {
        super::stmts::lower_stmt(ctx, builder, stmt);
    }

    // If the last statement is an expression or an if/match used as tail value,
    // lower it and return as value
    let last = &block.stmts[block.stmts.len() - 1];
    match &last.node {
        ast::Stmt::Expr(expr) => lower_expr(ctx, builder, expr),
        // if/elif/else used as tail expression in a block
        ast::Stmt::If { condition, then_body, elif_branches, else_body } => {
            // Build Expr::If chain from the statement form
            let else_expr = build_if_chain_expr(ctx, builder, condition, then_body, elif_branches, else_body);
            else_expr
        }
        // match used as tail expression
        ast::Stmt::Match { scrutinee, arms, else_arm } => {
            lower_match_stmt_as_expr(ctx, builder, scrutinee, arms.as_slice(), else_arm.as_ref())
        }
        _ => {
            super::stmts::lower_stmt(ctx, builder, last);
            Operand::Constant(Constant::Unit)
        }
    }
}

/// Lower a Stmt::Match used as a tail expression in a block.
fn lower_match_stmt_as_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchItem],
    else_arm: Option<&ast::Block>,
) -> Operand {
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    let result_local = builder.add_local(I64_TYPE, None);
    let merge_bb = builder.new_block();

    let concrete_arms: Vec<&ast::MatchArm> = arms.iter().filter_map(|i| i.arm()).collect();
    for (i, arm) in concrete_arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < concrete_arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        let cond = super::stmts::lower_pattern_condition(
            ctx, builder, &arm.pattern, scrut_local, scrut_type,
        );
        builder.branch(cond, arm_body_bb, next_test_bb);

        builder.switch_to(arm_body_bb);
        super::stmts::emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        let arm_val = lower_expr(ctx, builder, &arm.body);
        builder.assign(Place::local(result_local), arm_val);
        builder.jump(merge_bb);

        if next_test_bb != merge_bb {
            builder.switch_to(next_test_bb);
        }
    }

    if let Some(else_block) = else_arm {
        let else_val = lower_block_expr(ctx, builder, else_block);
        builder.assign(Place::local(result_local), else_val);
        builder.jump(merge_bb);
    } else if !concrete_arms.is_empty() {
        // No else arm but we're on the fallthrough block — jump to merge
        builder.jump(merge_bb);
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Build a value-producing if-chain from Stmt::If components.
/// Each branch body's last statement is treated as the result expression.
fn build_if_chain_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_body: &ast::Block,
    elif_branches: &[(Spanned<Expr>, ast::Block)],
    else_body: &Option<ast::Block>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);
    let result_id = builder.add_local(I64_TYPE, None);
    let merge_bb = builder.new_block();

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    builder.branch(cond, then_bb, else_bb);

    // Then branch
    builder.switch_to(then_bb);
    super::stmts::emit_is_bindings(ctx, builder, condition);
    let then_val = lower_block_expr(ctx, builder, then_body);
    // Guard: if the branch terminated via return/break/continue, don't overwrite its terminator.
    if !builder.is_terminated() {
        builder.assign(Place::local(result_id), then_val);
        builder.jump(merge_bb);
    }

    // Elif branches
    let mut current_else_bb = else_bb;
    for (elif_cond, elif_body) in elif_branches {
        builder.switch_to(current_else_bb);
        let elif_cond_val = lower_expr(ctx, builder, elif_cond);
        let elif_then_bb = builder.new_block();
        let next_else_bb = builder.new_block();
        builder.branch(elif_cond_val, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        super::stmts::emit_is_bindings(ctx, builder, elif_cond);
        let elif_val = lower_block_expr(ctx, builder, elif_body);
        if !builder.is_terminated() {
            builder.assign(Place::local(result_id), elif_val);
            builder.jump(merge_bb);
        }

        current_else_bb = next_else_bb;
    }

    // Else branch
    builder.switch_to(current_else_bb);
    if let Some(else_block) = else_body {
        let else_val = lower_block_expr(ctx, builder, else_block);
        if !builder.is_terminated() {
            builder.assign(Place::local(result_id), else_val);
            builder.jump(merge_bb);
        }
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::I64(0)));
        builder.jump(merge_bb);
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

// ---- P3.5.0: String Interpolation ----

/// Lower an interpolated string literal to `gorget_string_format(fmt, args...)`.
fn lower_string_interpolation(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    lit: &crate::lexer::token::StringLiteral,
) -> Operand {
    let mut format_str = String::new();
    let mut args: Vec<Operand> = Vec::new();

    for segment in &lit.segments {
        match segment {
            StringSegment::Literal(text) => {
                format_str.push_str(text);
            }
            StringSegment::Interpolation(var_name, fmt_spec) => {
                lower_interp_segment(ctx, builder, var_name,
                    &mut format_str, &mut args, fmt_spec.as_deref());
            }
        }
    }

    // Emit CallExtern("gorget_string_format", [fmt_str, ...args]) → GorgetString
    let owned_string_type = ctx.type_mapper.owned_string_type;
    let mut all_args = vec![Operand::Constant(Constant::Str(format_str))];
    all_args.extend(args);
    let dst = builder.call_extern("gorget_string_format", all_args, owned_string_type);
    // Register for drop — needs_drop() handles type filtering.
    ctx.drops.register_local(dst, owned_string_type, &ctx.type_registry);
    ctx.owned_locals.insert(dst);
    FunctionBuilder::copy(dst)
}

/// Phase 1f: clone resource-type args that can't be moved BEFORE StructInit/EnumInit.
///
/// Must clone when:
/// - The source is a bare borrow param (cow_ptr_params) — can't move, caller owns it
/// - The source is a multi-use named local — alive after the constructor
///
/// Can move (no clone needed) when:
/// - The source is a single-use named local — dead after, will be MoveZero'd
/// - The source is a temp — single-use by definition
fn clone_multi_use_resource_args(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &mut Vec<Operand>,
    ast_args: &[Spanned<Expr>],
) {
    for (i, op) in args.iter_mut().enumerate() {
        if let Operand::Copy(place) = op {
            if place.projections.is_empty() {
                let local = place.local;
                if is_resource_type_local(local, builder, &ctx.type_registry) {
                    // Skip if already cloned by ensure_owned_string
                    if ctx.owned_locals.contains(&local) && !ctx.is_named_local(local) {
                        continue;
                    }
                    // Must clone if: bare borrow param, multi-use named local,
                    // or field access on a struct (the parent owns the field data,
                    // MoveZero on the temp doesn't zero the parent's field).
                    let is_borrow_param = ctx.cow_ptr_params.contains(&local);
                    let is_field_access = ast_args.get(i)
                        .map(|arg| matches!(&arg.node, Expr::FieldAccess { .. }))
                        .unwrap_or(false);
                    let is_multi_use = ast_args.get(i)
                        .and_then(|arg| if let Expr::Identifier(name) = &arg.node {
                            Some(!ctx.is_single_use(name))
                        } else { None })
                        .unwrap_or(false);
                    if is_borrow_param || is_multi_use || is_field_access {
                        let local_type = builder.local_type(local);
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(local_type) {
                            let ptr_type = ctx.register_ptr_type(local_type);
                            let ptr = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr, crate::ir::instructions::Place::local(local));
                            let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr)], local_type);
                            ctx.drops.register_local(cloned, local_type, &ctx.type_registry);
                            *op = FunctionBuilder::copy(cloned);
                        }
                    }
                }
            }
        }
    }
}

/// MoveZero resource-type operands AFTER StructInit/EnumInit.
/// Single-use/temp sources are zeroed (zero-cost transfer). Multi-use sources
/// that were cloned by clone_multi_use_resource_args are already replaced —
/// the clone local gets MoveZero'd (it's single-use by definition).
fn move_zero_consumed_args(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &[Operand],
) {
    for op in args {
        if let Operand::Copy(place) = op {
            if place.projections.is_empty() {
                let is_resource = is_resource_type_local(place.local, builder, &ctx.type_registry);
                let is_string_view = builder.local_type(place.local) == ctx.type_mapper.string_view_type;
                if (is_resource || is_string_view) && !ctx.drops.is_moved(place.local) {
                    ctx.move_zero_and_mark(builder, place.local);
                }
            }
        }
    }
}

/// Register a GorgetString temp for drop at function scope.
/// Uses function scope (not block scope) because `str` views into the GorgetString
/// Register a GorgetString temp for drop at the current block scope.
/// Callers that consume the temp for str views (VarDecl, Assign, field assign)
/// must call `ctx.drops.unregister()` to prevent use-after-free.
/// Callers that consume the temp for String variables must call `mark_moved()`
/// to prevent double-free.
///
/// Note: block scope means loop-body temps are freed each iteration (good),
/// but temps whose str views escape the block (e.g. passed to functions that
/// store the view in structs) will cause use-after-free. Those call sites
/// need to use `String` parameters instead of `str`.
/// Check whether GorgetString temps should be unregistered (leaked) for a call.
/// Returns false (safe to keep in drop tracking) when:
/// - The callee is void-returning with no mutable-reference params
/// - The callee returns GorgetString (owned) — it creates new allocations,
///   doesn't store views from the args
/// - The callee returns a primitive (int, float, bool) — can't store views
pub fn should_unregister_string_args(
    ctx: &LoweringContext,
    callee_name: &str,
    ret_type: crate::ir::types::TypeId,
) -> bool {
    use crate::ir::types::{UNIT_TYPE, PRIMITIVE_TYPE_COUNT};
    // Void return: str view can't escape via result
    if ret_type == UNIT_TYPE {
        // Still check for ByMutPtr params below
    }
    // Primitive return (int, float, bool): can't store str views
    else if ret_type.0 < PRIMITIVE_TYPE_COUNT {
        return false;
    }
    // GorgetString return: callee produces a new allocation, doesn't store arg views
    else if ret_type == ctx.type_mapper.owned_string_type {
        return false;
    }
    // StringView return: callee might return a view into the arg (safe — same scope)
    else if ret_type == ctx.type_mapper.string_view_type {
        return false;
    }
    // Collection return (Vector, Dict, etc.): collections own their string data
    else if ctx.type_registry.is_collection_type(ret_type) {
        return false;
    }
    // Non-void, non-primitive, non-string, non-collection: might store str views
    else {
        return true;
    }
    // Check for ByMutPtr params — str view could escape through mutable ref
    if let Some(abis) = ctx.fn_param_abis.get(callee_name) {
        use super::context::ParamABI;
        return abis.iter().any(|abi| *abi == ParamABI::ByMutPtr);
    }
    // Unknown callee (runtime/extern): void-returning + not in fn_param_abis.
    // Safe: C runtime functions take Str by value (no mutable refs in Gorget sense),
    // and void return means no str view escapes via the result. Gorget-defined
    // functions with ByMutPtr params are always registered in fn_param_abis.
    false
}

/// Unregister GorgetString temps used as call/struct arguments.
/// When a GorgetString temp is passed to a function that takes `str`, the function
/// may store the str view in a struct that outlives the current scope. Freeing the
/// GorgetString at scope exit would create a use-after-free. Instead, we unregister
/// the temp (accepting a leak) to preserve correctness.
pub fn unregister_gorget_string_args(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    args: &[Operand],
) {
    let owned_string_type = ctx.type_mapper.owned_string_type;
    for arg in args {
        if let Operand::Copy(place) | Operand::Move(place) = arg {
            if place.projections.is_empty() {
                let idx = place.local.0 as usize;
                if idx < builder.locals.len()
                    && builder.locals[idx].type_id == owned_string_type
                {
                    // Don't unregister owned locals — constructors now clone
                    // string views, so returned structs own their data.
                    // The caller's string is independent and safe to drop.
                    if ctx.owned_locals.contains(&place.local) {
                        continue;
                    }
                    ctx.drops.unregister(place.local);
                }
            }
        }
    }
}

/// Infer the GIR type of an operand by examining its structure.
/// Register (or reuse) a Tuple TypeDef for the given element types.
/// Infer operand type using both ctx locals and builder locals.
/// This handles compiler temporaries (tuples, struct inits, etc.) that aren't in ctx.locals.
/// Extract the local ID from an operand if it's a simple local reference.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::lexer::token::{StringKind, StringLiteral};
    use crate::parser::ast::CallArg;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> (crate::semantic::AnalysisResult, LoweringContext<'static>) {
        // We need a 'static AnalysisResult to satisfy lifetime requirements.
        // Use a leaked box for tests only.
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        let ctx = LoweringContext::new(analysis, mapper, reg);
        // Return a dummy analysis (not used) and the context
        (crate::ir::lowering::empty_analysis_for_test(), ctx)
    }

    #[test]
    fn lower_literals() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let int_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::IntLiteral(42)));
        assert!(matches!(int_op, Operand::Constant(Constant::I64(42))));

        let float_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::FloatLiteral(3.14)));
        assert!(matches!(float_op, Operand::Constant(Constant::F64(f)) if (f - 3.14).abs() < 1e-10));

        let bool_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::BoolLiteral(true)));
        assert!(matches!(bool_op, Operand::Constant(Constant::Bool(true))));

        let str_op = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![StringSegment::Literal("hello".into())],
            })),
        );
        assert!(matches!(str_op, Operand::Constant(Constant::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn lower_binary_op_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let a_id = LocalId(1);
        let b_id = LocalId(2);
        ctx.register_local("a", a_id, I64_TYPE);
        ctx.register_local("b", b_id, I64_TYPE);

        let mut builder = FunctionBuilder::new(
            "test",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("a".into()))),
                op: ast::BinaryOp::Add,
                right: Box::new(spanned(Expr::Identifier("b".into()))),
            }),
        );

        assert!(matches!(result, Operand::Copy(_)));
        assert_eq!(builder.blocks[0].instructions.len(), 1);
        assert!(matches!(
            builder.blocks[0].instructions[0],
            Instruction::BinOp { op: BinOp::Add, .. }
        ));
    }

    #[test]
    fn lower_print_interpolation() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let lit = StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Interpolation("x".into(), None)],
        };
        let args = vec![spanned(CallArg {
            name: None,
            ownership: ast::Ownership::Borrow,
            value: spanned(Expr::StringLiteral(lit)),
        })];

        lower_print_call(&mut ctx, &mut builder, &args);

        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::CallExtern { func, .. } if func == "printf"
        ));
    }

    // ---- P3.2: Match expression tests ----

    #[test]
    fn lower_match_expr_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::{MatchArm, Pattern};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Match {
                scrutinee: Box::new(spanned(Expr::Identifier("x".into()))),
                arms: vec![
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(10)),
                        span: Span { start: 0, end: 0 },
                    },
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(20)),
                        span: Span { start: 0, end: 0 },
                    },
                ],
                else_arm: Some(Box::new(spanned(Expr::IntLiteral(0)))),
            }),
        );

        // Result should be a Copy of the result local
        assert!(matches!(result, Operand::Copy(_)));

        // Should have Branch terminators for pattern checks
        let has_branch = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Branch { .. }))
        });
        assert!(has_branch, "Match expr should have Branch terminators");

        // Should have Assign to result local in arm bodies
        let assign_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter()
                .filter(|inst| matches!(inst, Instruction::Assign { .. }))
                .count())
            .sum();
        assert!(assign_count >= 3, "Should have assigns for scrutinee + arms");
    }

    // ---- P3.4: Miscellaneous expression tests ----

    #[test]
    fn lower_self_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let self_id = LocalId(1);
        ctx.register_local("self", self_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("self"))]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::SelfExpr));
        assert!(matches!(result, Operand::Copy(ref p) if p.local == LocalId(1)));
    }

    #[test]
    fn lower_block_expr_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[]);

        use crate::parser::ast::{Block, Stmt};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Block(Block {
                stmts: vec![spanned(Stmt::Expr(spanned(Expr::IntLiteral(42))))],
                span: Span { start: 0, end: 0 },
            })),
        );

        // The block's last expression (42) should be the value
        assert!(matches!(result, Operand::Constant(Constant::I64(42))));
    }

    #[test]
    fn lower_cast_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::As {
                expr: Box::new(spanned(Expr::IntLiteral(42))),
                type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Float)),
            }),
        );

        // Should produce a Copy of the cast result local
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a Cast instruction
        let has_cast = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::Cast { .. })
        });
        assert!(has_cast, "Should have Cast instruction for 'as' expression");
    }

    #[test]
    fn lower_tuple_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::TupleLiteral(vec![
                spanned(Expr::IntLiteral(1)),
                spanned(Expr::IntLiteral(2)),
                spanned(Expr::IntLiteral(3)),
            ])),
        );

        assert!(matches!(result, Operand::Copy(_)));
        // Should have a TupleInit instruction
        let has_tuple_init = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::TupleInit { .. })
        });
        assert!(has_tuple_init, "Should have TupleInit instruction");
    }

    #[test]
    fn lower_is_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::Pattern;

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Is {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                negated: false,
                pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(5))))),
            }),
        );

        // Should produce a boolean condition (Copy of Cmp result)
        assert!(matches!(result, Operand::Copy(_)));
    }

    #[test]
    fn lower_none_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::NoneLiteral));
        assert!(matches!(result, Operand::Constant(Constant::Null)));
    }

    // ---- P3.5.0: String Interpolation ----

    #[test]
    fn lower_plain_string_stays_constant() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![StringSegment::Literal("hello".into())],
            })),
        );
        assert!(
            matches!(result, Operand::Constant(Constant::Str(ref s)) if s == "hello"),
            "Plain string should stay as Constant::Str"
        );
    }

    #[test]
    fn lower_interpolated_string_calls_format() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![
                    StringSegment::Literal("value: ".into()),
                    StringSegment::Interpolation("x".into(), None),
                ],
            })),
        );
        // Should return Copy (of the gorget_string_format result local)
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a CallExtern to gorget_string_format
        let has_format = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_string_format")
        });
        assert!(has_format, "Interpolated string should call gorget_string_format");
    }

    // ---- P3.5.1: Array Literals ----

    #[test]
    fn lower_array_literal_nonempty() {
        let (_analysis, mut ctx) = make_test_ctx();
        // Register GorgetArray type
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ArrayLiteral(vec![
                spanned(Expr::IntLiteral(1)),
                spanned(Expr::IntLiteral(2)),
                spanned(Expr::IntLiteral(3)),
            ])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Count gorget_array_new + gorget_array_push calls
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let new_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        }).count();
        let push_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push")
        }).count();
        assert_eq!(new_count, 1, "Should have 1 gorget_array_new call");
        assert_eq!(push_count, 3, "Should have 3 gorget_array_push calls");
    }

    #[test]
    fn lower_array_literal_empty() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ArrayLiteral(vec![])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let has_new = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        });
        assert!(has_new, "Empty array should still call gorget_array_new");
        let push_count: usize = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .filter(|inst| matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push"))
            .count();
        assert_eq!(push_count, 0, "Empty array should have no push calls");
    }

    // ---- P3.5.2: Dict Literals ----

    #[test]
    fn lower_dict_literal_nonempty() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::DictLiteral(vec![
                (spanned(Expr::IntLiteral(1)), spanned(Expr::IntLiteral(10))),
                (spanned(Expr::IntLiteral(2)), spanned(Expr::IntLiteral(20))),
            ])),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        // Should have a __new call
        let new_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__new"))
        }).count();
        assert_eq!(new_count, 1, "Should have 1 dict __new call");
        // Should have 2 __put calls
        let put_count = all_insts.iter().filter(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__put"))
        }).count();
        assert_eq!(put_count, 2, "Should have 2 dict __put calls");
    }

    // ---- P3.5.3: List Comprehensions ----

    #[test]
    fn lower_list_comprehension_range() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        use crate::parser::ast::{Ownership, Pattern};

        // [x * x for x in 0..5]
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ListComprehension {
                expr: Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Mul,
                    right: Box::new(spanned(Expr::Identifier("x".into()))),
                })),
                variable: spanned(Pattern::Binding("x".into())),
                ownership: Ownership::Borrow,
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(5)))),
                    inclusive: false,
                })),
                condition: None,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have gorget_array_new + gorget_array_push
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let has_new = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_new")
        });
        let has_push = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_array_push")
        });
        assert!(has_new, "List comprehension should have gorget_array_new");
        assert!(has_push, "List comprehension should have gorget_array_push");
        // Should have a Cmp (loop condition) and Branch
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { .. }))
        });
        assert!(has_cmp, "List comprehension should have loop condition Cmp");
    }

    #[test]
    fn lower_list_comprehension_with_filter() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        use crate::parser::ast::{Ownership, Pattern};

        // [x for x in 0..10 if x > 5]
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::ListComprehension {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                variable: spanned(Pattern::Binding("x".into())),
                ownership: Ownership::Borrow,
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                    inclusive: false,
                })),
                condition: Some(Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Gt,
                    right: Box::new(spanned(Expr::IntLiteral(5))),
                }))),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have at least 2 Branch terminators (loop condition + filter)
        let branch_count = builder.blocks.iter()
            .filter(|bb| matches!(bb.terminator, Some(Terminator::Branch { .. })))
            .count();
        assert!(branch_count >= 2, "Should have >= 2 Branch terminators (loop + filter), got {branch_count}");
    }

    // ---- P3.5.4: Dict and Set Comprehensions ----

    #[test]
    fn lower_dict_comprehension_range() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // {x: x * 10 for x in 0..3}
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::DictComprehension {
                key: Box::new(spanned(Expr::Identifier("x".into()))),
                value: Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Mul,
                    right: Box::new(spanned(Expr::IntLiteral(10))),
                })),
                variables: vec![spanned("x".to_string())],
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(3)))),
                    inclusive: false,
                })),
                condition: None,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let all_insts: Vec<_> = builder.blocks.iter()
            .flat_map(|bb| bb.instructions.iter())
            .collect();
        let has_new = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__new"))
        });
        let has_put = all_insts.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func.ends_with("__put"))
        });
        assert!(has_new, "Dict comprehension should have __new call");
        assert!(has_put, "Dict comprehension should have __put call");
    }

    #[test]
    fn lower_set_comprehension_with_filter() {
        let (_analysis, mut ctx) = make_test_ctx();
        let array_type = ctx.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        ctx.type_mapper.register_named("GorgetArray".to_string(), array_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // {x for x in 0..10 if x > 5}
        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::SetComprehension {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                variable: spanned("x".to_string()),
                iterable: Box::new(spanned(Expr::Range {
                    start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                    end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                    inclusive: false,
                })),
                condition: Some(Box::new(spanned(Expr::BinaryOp {
                    left: Box::new(spanned(Expr::Identifier("x".into()))),
                    op: ast::BinaryOp::Gt,
                    right: Box::new(spanned(Expr::IntLiteral(5))),
                }))),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have condition branch
        let branch_count = builder.blocks.iter()
            .filter(|bb| matches!(bb.terminator, Some(Terminator::Branch { .. })))
            .count();
        assert!(branch_count >= 2, "Set comprehension with filter should have >= 2 branches");
    }

    // ---- P3.5.6: Optional Chaining ----

    #[test]
    fn lower_optional_chain_produces_branch() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::OptionalChain {
                object: Box::new(spanned(Expr::Identifier("x".into()))),
                field: spanned("field".to_string()),
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a Cmp (not null check) and Branch
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { op: CmpOp::Ne, .. }))
        });
        assert!(has_cmp, "Optional chain should have a Ne comparison");
        let has_branch = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Branch { .. }))
        });
        assert!(has_branch, "Optional chain should have a Branch");
        // Null path should assign Null
        let has_null_assign = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, Instruction::Assign { value: Operand::Constant(Constant::Null), .. })
            })
        });
        assert!(has_null_assign, "Optional chain should assign Null on else path");
    }

    // ---- P3.5.7: Range Expressions ----

    #[test]
    fn lower_range_expr_produces_struct_init() {
        let (_analysis, mut ctx) = make_test_ctx();
        // Register GorgetRange type
        let range_def = crate::ir::types::TypeDef {
            name: "GorgetRange".to_string(),
            kind: crate::ir::types::TypeDefKind::Struct(crate::ir::types::StructDef {
                fields: vec![
                    crate::ir::types::StructField { name: "start".to_string(), type_id: I64_TYPE },
                    crate::ir::types::StructField { name: "end".to_string(), type_id: I64_TYPE },
                    crate::ir::types::StructField { name: "inclusive".to_string(), type_id: BOOL_TYPE },
                ],
            }),
            metadata: crate::ir::types::TypeMetadata::default(),
        };
        ctx.type_registry.add_type_def(range_def);
        let range_type = ctx.type_registry.insert(GirType::Named("GorgetRange".to_string()));
        ctx.type_mapper.register_named("GorgetRange".to_string(), range_type);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Range {
                start: Some(Box::new(spanned(Expr::IntLiteral(0)))),
                end: Some(Box::new(spanned(Expr::IntLiteral(10)))),
                inclusive: false,
            }),
        );
        assert!(matches!(result, Operand::Copy(_)));
        let has_struct_init = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::StructInit { type_name, .. } if type_name == "GorgetRange")
        });
        assert!(has_struct_init, "Range expr should produce a StructInit for GorgetRange");
    }
}
