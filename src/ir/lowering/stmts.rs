use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, BinaryOp, Block, Expr, Pattern, SelectOp, Stmt};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::{lower_expr, infer_operand_type_full};

/// Lower a block of statements.
pub fn lower_block(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &Block,
) {
    for stmt in &block.stmts {
        lower_stmt(ctx, builder, stmt);
    }
}

/// Lower a single statement.
pub fn lower_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    stmt: &Spanned<Stmt>,
) {
    builder.set_span(stmt.span);
    match &stmt.node {
        Stmt::VarDecl {
            type_,
            pattern,
            value,
            ..
        } => lower_var_decl(ctx, builder, type_, pattern, value),

        Stmt::Assign { target, value } => lower_assign(ctx, builder, target, value),

        Stmt::CompoundAssign { target, op, value } => {
            lower_compound_assign(ctx, builder, target, *op, value)
        }

        Stmt::Return(expr) => lower_return(ctx, builder, expr.as_ref()),

        Stmt::Expr(expr) => {
            lower_expr(ctx, builder, expr);
        }

        Stmt::Pass => {
            builder.nop();
        }

        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => lower_if(ctx, builder, condition, then_body, elif_branches, else_body),

        Stmt::While {
            condition,
            body,
            else_body,
            ..
        } => lower_while(ctx, builder, condition, body, else_body.as_ref()),

        Stmt::For {
            pattern,
            iterable,
            body,
            else_body,
            ..
        } => lower_for(ctx, builder, pattern, iterable, body, else_body.as_ref()),

        Stmt::Loop { body } => lower_loop(ctx, builder, body),

        Stmt::Break(_) => lower_break(ctx, builder),

        Stmt::Continue => lower_continue(ctx, builder),

        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => lower_match_stmt(ctx, builder, scrutinee, arms, else_arm),

        Stmt::Throw(expr) => lower_throw(ctx, builder, expr),

        Stmt::Assert { condition, message } => lower_assert(ctx, builder, condition, message.as_ref()),

        Stmt::With { bindings, body } => lower_with(ctx, builder, bindings, body),

        Stmt::Unsafe { body } => lower_block(ctx, builder, body),

        Stmt::Item(_) => { /* Nested items are hoisted — no-op in GIR */ }

        Stmt::Select { arms, else_arm: _ } => lower_select(ctx, builder, arms),

        // meta if/for should have been evaluated and removed before GIR lowering.
        // If they appear here it means they were in a non-generic context (a semantic
        // error should have been emitted) — emit nothing.
        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } => {}
    }
}

/// Lower a variable declaration.
fn lower_var_decl(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_: &Spanned<ast::Type>,
    pattern: &Spanned<Pattern>,
    value: &Spanned<Expr>,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            let gir_type = ctx.resolve_var_type(type_, value);
            // For explicit Callable[...] declarations, use map_ast_type_mut to register a FnPtr TypeId.
            // resolve_var_type → map_type_with_subs → map_ast_type (immutable) returns UNIT_TYPE for
            // Callable generics; map_ast_type_mut (mutable) creates the actual FnPtr TypeId so the
            // local is declared as GorgetClosure and dispatched via __gorget_closure_call_N.
            let gir_type = if gir_type == crate::ir::types::UNIT_TYPE {
                if let ast::Type::Named { ref name, ref generic_args } = type_.node {
                    if matches!(name.node.as_str(), "Callable" | "MutCallable" | "ConsumeCallable")
                        && !generic_args.is_empty()
                    {
                        ctx.type_mapper.map_ast_type_mut(&type_.node, &mut ctx.type_registry)
                    } else {
                        gir_type
                    }
                } else {
                    gir_type
                }
            } else {
                gir_type
            };
            // Box[Callable[...]] variables pre-register with a "Box__Callable__unknown" type from the
            // generic collector. We need to reinfer from the actual RHS to get the real closure type.
            let gir_type_is_box_callable = ctx.type_name_for_id(gir_type)
                .map(|n| n.starts_with("Box__Callable__") || n.starts_with("Box__MutCallable__") || n.starts_with("Box__ConsumeCallable__"))
                .unwrap_or(false);
            let local_id = builder.add_local(gir_type, Some(name));
            ctx.register_local(name, local_id, gir_type);
            // P2.6: Register Move-type locals for drop at scope exit
            ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
            // Set expected type hint so enum variant constructors (Some, None, Ok, Error)
            // can pick the correctly-monomorphized type
            let prev_expected = ctx.expected_type;
            ctx.expected_type = Some(gir_type);
            let operand = lower_expr(ctx, builder, value);
            ctx.expected_type = prev_expected;
            // If this was a Spawn expression, register the task local → spawned fn mapping
            if let Some(fn_name) = ctx.pending_spawn_fn.take() {
                ctx.spawn_result_locals.insert(local_id, fn_name);
            }
            // For auto/inferred types, closure values, and Box[Callable[...]] variables,
            // re-infer from the lowered operand to pick up the actual concrete type.
            // Also handle Shared[T]/Mutex[T]/Channel[T] and the non-generic TaskGroup,
            // whose TypeIds are registered lazily inside lower_call — at declaration time
            // map_ast_type returns UNIT_TYPE, but after the RHS is lowered the TypeId is
            // registered and the operand carries it.
            let gir_type_is_lazy_generic = gir_type == crate::ir::types::UNIT_TYPE && {
                if let ast::Type::Named { ref name, ref generic_args, .. } = type_.node {
                    // TaskGroup has no generic args; the others require at least one.
                    name.node.as_str() == "TaskGroup"
                        || (!generic_args.is_empty()
                            && matches!(name.node.as_str(),
                                "Shared" | "Weak" | "Mutex" | "Guard" | "Channel" | "Task"))
                } else {
                    false
                }
            };
            // Don't reinfer when gir_type is FnPtr (explicit Callable[T] declaration):
            // the Assign handler will pack closures/FuncRefs into GorgetClosure form.
            let gir_type_is_fnptr = matches!(ctx.type_registry.get(gir_type), Some(GirType::FnPtr { .. }));
            let needs_reinfer = !gir_type_is_fnptr && (
                matches!(type_.node, ast::Type::Inferred)
                || matches!(value.node, ast::Expr::Closure { .. } | ast::Expr::ImplicitClosure { .. })
                || gir_type_is_box_callable
                || gir_type_is_lazy_generic
            );
            if needs_reinfer {
                let inferred = infer_operand_type_with_builder(ctx, &operand, builder);
                if inferred != gir_type {
                    builder.locals[local_id.0 as usize].type_id = inferred;
                    ctx.register_local(name, local_id, inferred);
                    // Also update the drop elaborator with the correct type — the initial
                    // registration used `gir_type` which may have been I64_TYPE (no-drop),
                    // but the real type (e.g., Wrapper, Container) does need dropping.
                    ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                }
            }
            builder.assign(Place::local(local_id), operand);
        }

        Pattern::Tuple(parts) => {
            // Lower the RHS expression first — it should produce a tuple (struct) value
            let operand = lower_expr(ctx, builder, value);
            let tuple_type = infer_operand_type_with_builder(ctx, &operand, builder);

            // Store the tuple in a temp local
            let tuple_local = builder.add_local(tuple_type, None);
            builder.assign(Place::local(tuple_local), operand);

            // Extract each field and bind it to the corresponding pattern variable
            for (i, part) in parts.iter().enumerate() {
                let field_type = super::exprs::resolve_tuple_field_type(ctx, tuple_type, i);
                let field_local = builder.field_load(Place::local(tuple_local), i as u32, field_type);

                if let Pattern::Binding(name) = &part.node {
                    ctx.register_local(name, field_local, field_type);
                    ctx.drops.register_local(field_local, field_type, &ctx.type_registry);
                } else {
                    // Nested destructuring — recurse via emit_pattern_bindings
                    emit_pattern_bindings(ctx, builder, part, field_local, field_type);
                }
            }
        }

        _ => {
            // Other pattern forms not yet supported in VarDecl
        }
    }
}

/// Lower an assignment.
fn lower_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    match &target.node {
        Expr::Identifier(name) => {
            if let Some((local_id, type_id)) = ctx.lookup_local(name) {
                // Check if old value needs dropping
                let needs_drop = {
                    use crate::ir::types::GirType;
                    if let Some(GirType::Named(type_name)) = ctx.type_registry.get(type_id) {
                        let type_name = type_name.clone();
                        if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                            type_def.metadata.drop_strategy != super::super::types::DropStrategy::None
                        } else { false }
                    } else { false }
                };
                // Compute new value FIRST (it may reference the old value, e.g. s = s + x)
                let prev_expected = ctx.expected_type;
                ctx.expected_type = Some(type_id);
                let operand = lower_expr(ctx, builder, value);
                ctx.expected_type = prev_expected;
                // P2.6: Drop old value AFTER computing new value, BEFORE assigning
                if needs_drop {
                    builder.drop(Place::local(local_id));
                }
                // If this is a mutable capture pointer, write through the pointer
                if ctx.mut_capture_locals.contains_key(&local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    builder.assign(deref_place, operand);
                } else {
                    builder.assign(Place::local(local_id), operand);
                }
            }
        }
        Expr::FieldAccess { object, field } => {
            lower_field_assign(ctx, builder, object, &field.node, value);
        }
        Expr::Index { object, index } => {
            lower_index_assign(ctx, builder, object, index, value);
        }
        _ => {
            // Other target types not yet supported
        }
    }
}

/// Emit a drop for a field place if its type is droppable (has a non-None DropStrategy).
fn emit_field_drop_if_needed(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    place: &Place,
    field_type: TypeId,
) {
    use crate::ir::types::GirType;
    // Check if the field type has a drop strategy
    let needs_drop = if let Some(GirType::Named(type_name)) = ctx.type_registry.get(field_type) {
        if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            type_def.metadata.drop_strategy != super::super::types::DropStrategy::None
        } else {
            // Collections (GorgetArray, GorgetDict, etc.) are always droppable
            type_name.starts_with("GorgetArray")
                || type_name.starts_with("GorgetDict")
                || type_name.starts_with("GorgetMap")
                || type_name.starts_with("GorgetSet")
                || type_name.starts_with("GorgetString")
        }
    } else {
        false
    };
    if needs_drop {
        builder.drop(place.clone());
    }
}

/// Lower a field assignment: `obj.field = value`
fn lower_field_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
    value: &Spanned<Expr>,
) {
    use crate::ir::types::TypeDefKind;

    // For mut_capture_locals (mutable borrow params), use the pointer local directly
    // instead of lower_expr which would copy the deref'd value to a temp
    let obj = if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.mut_capture_locals.contains_key(&local_id) {
                // Return the raw pointer local (not deref'd)
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
    let rhs = lower_expr(ctx, builder, value);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // If the local is a pointer, dereference to get the struct type
            let (effective_type_id, base_place) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (local_type_id, place.clone())
                };

            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                let type_name = type_name.to_string();
                if let Some((field_idx, field_type)) = ctx.lookup_field(&type_name, field_name) {
                    let mut target_place = base_place;
                    target_place.projections.push(Projection::Field(field_idx));
                    // Drop old field value before reassignment if it's droppable
                    emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
                    builder.assign(target_place, rhs);
                    return;
                }
                // Fallback: look up from TypeDef
                if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, f) in s.fields.iter().enumerate() {
                            if f.name == field_name {
                                let mut target_place = base_place;
                                target_place.projections.push(Projection::Field(i as u32));
                                // Drop old field value before reassignment if it's droppable
                                emit_field_drop_if_needed(ctx, builder, &target_place, f.type_id);
                                builder.assign(target_place, rhs);
                                return;
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Lower an index assignment: `obj[index] = value`
fn lower_index_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    let obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);
    let val = lower_expr(ctx, builder, value);

    // Determine the receiver type to dispatch correctly
    let obj_type = infer_operand_type_full(ctx, &obj, builder);
    let type_name = ctx.type_name_for_id(obj_type).unwrap_or("").to_string();
    let is_vector = type_name.starts_with("Vector__") || type_name == "GorgetArray";
    let is_dict = type_name.starts_with("Dict__") || type_name.starts_with("HashMap__")
        || type_name == "GorgetMap";

    if is_vector {
        // Vector[i] = val → Vector__T__set(&arr, index, val)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__set");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx, val],
            );
        }
    } else if is_dict {
        // Dict[key] = val → Dict__K__V__put(&dict, key, val)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__put");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx, val],
            );
        }
    } else {
        // Check for IndexMut / set equip method (operator overload)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let candidates = [
                format!("{type_name}__set"),
                format!("IndexMut_for_{type_name}__set"),
                format!("{type_name}____setitem__"),
            ];
            for set_name in &candidates {
                if ctx.fn_sigs.contains_key(set_name.as_str()) {
                    let ptr_type = ctx.register_mut_ptr_type(obj_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    builder.call_void(
                        set_name.clone(),
                        vec![FunctionBuilder::copy(ptr_local), idx, val],
                    );
                    return;
                }
            }
        }
    }
    // String index assignment not supported (strings are immutable views)
}

/// Lower a compound assignment (e.g., `x += 1`).
fn lower_compound_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    op: ast::BinaryOp,
    value: &Spanned<Expr>,
) {
    if let Expr::Identifier(name) = &target.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            let is_mut_capture = ctx.mut_capture_locals.contains_key(&local_id);
            let value_type = if is_mut_capture {
                ctx.mut_capture_locals[&local_id]
            } else {
                type_id
            };
            // Read current value (deref if mutable capture)
            let cur_val = if is_mut_capture {
                let deref_place = Place {
                    local: local_id,
                    projections: vec![Projection::Deref],
                };
                let tmp = builder.add_local(value_type, None);
                builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                FunctionBuilder::copy(tmp)
            } else {
                FunctionBuilder::copy(local_id)
            };

            let rhs = lower_expr(ctx, builder, value);
            let is_string = value_type == ctx.type_mapper.str_type
                || value_type == ctx.type_mapper.owned_string_type;

            // String concatenation via += → gorget_str_cat (returns GorgetString)
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.call_extern(
                    "gorget_str_cat",
                    vec![cur_val, rhs],
                    owned_type,
                );
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                builder.assign(dst, FunctionBuilder::copy(tmp));
                return;
            }

            // Check for operator overload on Named types
            let overload_method = match op {
                ast::BinaryOp::Add => Some("add"),
                ast::BinaryOp::Sub => Some("sub"),
                ast::BinaryOp::Mul => Some("mul"),
                ast::BinaryOp::Div => Some("div"),
                ast::BinaryOp::Rem => Some("rem"),
                ast::BinaryOp::Mod => Some("mod"),
                _ => None,
            }.and_then(|method| {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(value_type).cloned() {
                    let mangled = format!("{type_name}__{method}");
                    let has_method = ctx.fn_sigs.contains_key(&mangled)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__{method}")));
                    if has_method {
                        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                            mangled
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__{method}")))
                                .cloned()
                                .unwrap_or(mangled)
                        };
                        Some(effective_name)
                    } else { None }
                } else { None }
            });

            let tmp = if let Some(effective_name) = overload_method {
                // Borrow lhs for self parameter
                let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = cur_val {
                    let ptr_type = ctx.register_ptr_type(value_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, place.clone());
                    FunctionBuilder::copy(ptr_local)
                } else {
                    cur_val
                };
                builder.call(effective_name, vec![self_ptr, rhs], value_type)
            } else {
                let gir_op = match op {
                    ast::BinaryOp::Add => BinOp::Add,
                    ast::BinaryOp::Sub => BinOp::Sub,
                    ast::BinaryOp::Mul => BinOp::Mul,
                    ast::BinaryOp::Div => BinOp::Div,
                    ast::BinaryOp::Rem => BinOp::Rem,
                    ast::BinaryOp::Mod => BinOp::Mod,
                    ast::BinaryOp::AddWrap => BinOp::AddWrap,
                    ast::BinaryOp::SubWrap => BinOp::SubWrap,
                    ast::BinaryOp::MulWrap => BinOp::MulWrap,
                    ast::BinaryOp::BitAnd => BinOp::BitAnd,
                    ast::BinaryOp::BitOr => BinOp::BitOr,
                    ast::BinaryOp::BitXor => BinOp::BitXor,
                    ast::BinaryOp::Shl => BinOp::Shl,
                    ast::BinaryOp::Shr => BinOp::Shr,
                    _ => BinOp::Add, // fallback
                };
                builder.bin_op(gir_op, value_type, cur_val, rhs)
            };
            let dst = if is_mut_capture {
                Place { local: local_id, projections: vec![Projection::Deref] }
            } else {
                Place::local(local_id)
            };
            builder.assign(dst, FunctionBuilder::copy(tmp));
        }
    }
}

/// Lower a return statement.
fn lower_return(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: Option<&Spanned<Expr>>,
) {
    if let Some(expr) = expr {
        // Check if the return expression is already an explicit Ok/Error variant
        // (used in throws functions). If so, skip the automatic Result wrapping —
        // the expression itself already produces a Result.
        let is_explicit_result_variant = matches!(&expr.node,
            Expr::Call { callee, .. } if matches!(&callee.node,
                Expr::Identifier(name) if name == "Ok" || name == "Error" || name == "Some" || name == "None"
            )
        );
        // Set expected type from function return type so variant constructors resolve correctly
        let prev_expected = ctx.expected_type;
        let ret_type = builder.locals[0].type_id;
        ctx.expected_type = Some(ret_type);
        let operand = lower_expr(ctx, builder, expr);
        ctx.expected_type = prev_expected;
        // Identify the local being returned (to exclude from drops — it's being moved out)
        let returned_local = match &operand {
            Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                Some(place.local)
            }
            _ => None,
        };
        if let Some(result_type) = ctx.current_throws_result_type {
            if is_explicit_result_variant {
                // Expression already produced a Result — assign directly, no wrapping
                builder.assign(Place::local(LocalId(0)), operand);
            } else {
                // Wrap value in Result.Ok — the operand's local is consumed (moved into Result)
                let ok_dst = {
                    let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                    builder.enum_init(type_name, "Ok", result_type, vec![operand])
                };
                builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(ok_dst));
                // Zero out the original local (its value is now owned by the Result)
                if let Some(local) = returned_local {
                    builder.move_zero(Place::local(local));
                    ctx.drops.mark_moved(local);
                }
            }
        } else {
            builder.assign(Place::local(LocalId(0)), operand);
        }
        // P2.6: Emit cleanup drops for all scopes being exited
        // Exclude the local being returned (it's moved into _0, not consumed)
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, returned_local);
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        // P2.6: Emit cleanup drops for all scopes being exited
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
        builder.ret(FunctionBuilder::const_unit());
    }
}

/// Lower an if/elif/else statement.
fn lower_if(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_body: &Block,
    elif_branches: &[(Spanned<Expr>, Block)],
    else_body: &Option<Block>,
) {
    let merge_bb = builder.new_block();

    // Lower the condition
    let cond = lower_expr(ctx, builder, condition);

    let then_bb = builder.new_block();
    let first_else_bb = if !elif_branches.is_empty() || else_body.is_some() {
        builder.new_block()
    } else {
        merge_bb
    };

    builder.branch(cond, then_bb, first_else_bb);

    // Then branch
    builder.switch_to(then_bb);
    ctx.drops.push_scope(DropScopeKind::Block);
    emit_is_bindings(ctx, builder, condition);
    lower_block(ctx, builder, then_body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    if !block_always_returns(then_body) {
        builder.jump(merge_bb);
    }

    // Elif branches
    let mut current_else_bb = first_else_bb;
    for (i, (elif_cond, elif_body)) in elif_branches.iter().enumerate() {
        builder.switch_to(current_else_bb);
        let elif_cond_op = lower_expr(ctx, builder, elif_cond);

        let elif_then_bb = builder.new_block();
        let next_else_bb = if i + 1 < elif_branches.len() || else_body.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        builder.branch(elif_cond_op, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        ctx.drops.push_scope(DropScopeKind::Block);
        emit_is_bindings(ctx, builder, elif_cond);
        lower_block(ctx, builder, elif_body);
        ctx.drops.pop_scope(builder, &ctx.type_registry);
        if !block_always_returns(elif_body) {
            builder.jump(merge_bb);
        }

        current_else_bb = next_else_bb;
    }

    // Else branch
    if let Some(else_body) = else_body {
        builder.switch_to(current_else_bb);
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        ctx.drops.pop_scope(builder, &ctx.type_registry);
        if !block_always_returns(else_body) {
            builder.jump(merge_bb);
        }
    }

    builder.switch_to(merge_bb);
}

/// Lower a while loop.
fn lower_while(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // For while-else: use a break flag to skip else on break
    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    // Jump from current block to header
    builder.jump(header_bb);

    // Header: evaluate condition, branch
    builder.switch_to(header_bb);
    let cond = lower_expr(ctx, builder, condition);
    let natural_exit = if else_arm.is_some() {
        else_exit_bb.unwrap()
    } else {
        exit_bb
    };
    builder.branch(cond, body_bb, natural_exit);

    // Body: execute, jump back to header (wrapped in Loop scope for drop cleanup)
    builder.switch_to(body_bb);
    emit_is_bindings(ctx, builder, condition);
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(header_bb);

    // Else block: executed when loop completes naturally (no break)
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);

        // Break exit goes directly to exit (skipping else)
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    // Continue from exit
    builder.switch_to(exit_bb);
}

/// Lower a for loop over a range (`for i in start..end`).
fn lower_for(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    if let Pattern::Binding(var_name) = &pattern.node {
        if let Expr::Range {
            start: Some(start),
            end: Some(end),
            inclusive,
        } = &iterable.node
        {
            lower_for_range(ctx, builder, var_name, start, end, *inclusive, body, else_arm);
            return;
        }
    }

    // Detect `for (i, elem) in collection.enumerate():` and lower as index-tracked loop
    if let Pattern::Tuple(parts) = &pattern.node {
        if parts.len() == 2 {
            if let Expr::MethodCall { receiver, method, args: call_args, .. } = &iterable.node {
                if method.node == "enumerate" && call_args.is_empty() {
                    lower_for_enumerate(ctx, builder, parts, receiver, body, else_arm);
                    return;
                }
            }
        }
    }

    // Lower the iterable and check its type for string/collection iteration
    let iter_op = lower_expr(ctx, builder, iterable);
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);

    // Extract the binding name (or use a temp for pattern destructuring)
    let var_name = if let Pattern::Binding(name) = &pattern.node {
        name.clone()
    } else {
        "__for_elem".to_string()
    };

    if iter_type == ctx.type_mapper.str_type || iter_type == ctx.type_mapper.owned_string_type {
        lower_for_string(ctx, builder, &var_name, iter_op, body, else_arm);
    } else {
        // Determine collection kind from the named type
        let collection_kind = if let Operand::Copy(ref p) | Operand::Move(ref p) = iter_op {
            let local_idx = p.local.0 as usize;
            if local_idx < builder.locals.len() {
                let tid = builder.locals[local_idx].type_id;
                if let Some(GirType::Named(name)) = ctx.type_registry.get(tid) {
                    if name.starts_with("GorgetArray") || name.starts_with("Vector__") {
                        Some("array")
                    } else if name.starts_with("Dict__") || name.starts_with("GorgetDict") {
                        Some("dict")
                    } else if name.starts_with("HashMap__") || name.starts_with("GorgetMap") {
                        Some("hashmap")
                    } else if name.starts_with("Set__") || name.starts_with("GorgetSet") || name.starts_with("HashSet__") {
                        Some("set")
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        match collection_kind {
            Some("array") => lower_for_array(ctx, builder, &var_name, iter_op, body, else_arm, pattern),
            Some("dict") | Some("hashmap") => lower_for_dict(ctx, builder, iter_op, body, else_arm, pattern),
            Some("set") => lower_for_set(ctx, builder, &var_name, iter_op, body, else_arm),
            _ => {
                // Try Iterable/Iterator trait dispatch for user-defined types
                if let Some(type_name) = ctx.type_registry.get(iter_type)
                    .and_then(|gt| if let GirType::Named(n) = gt { Some(n.clone()) } else { None })
                {
                    lower_for_iterable(ctx, builder, &var_name, iter_op, &type_name, body, else_arm, pattern);
                }
            }
        }
    }
}

/// Lower `for ch in str_value: body` — iterate UTF-8 codepoints.
fn lower_for_string(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let str_type = ctx.type_mapper.str_type;

    // Store the iterable in a local
    let iter_local = builder.add_local(str_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // byte_pos = 0
    let byte_pos = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(byte_pos), Operand::Constant(Constant::I64(0)));

    // len = iter.len (byte length)
    let len_local = builder.add_local(I64_TYPE, None);
    // Access .len field (field index 1 for Str: {data, len})
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
    };
    builder.assign(Place::local(len_local), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    // Header: byte_pos < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(len_local));
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    // Body
    builder.switch_to(body_bb);
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // cplen = gorget_utf8_codepoint_len((unsigned char)data[byte_pos])
    // We'll call this as an extern that takes a Str and byte offset → returns int
    let cplen = builder.call_extern(
        "gorget_utf8_codepoint_len_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        I64_TYPE,
    );

    // ch = (Str){ .data = iter.data + byte_pos, .len = cplen }
    // We'll construct this as a StructInit with computed fields via extern
    let ch_local = builder.call_extern(
        "gorget_str_codepoint_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        str_type,
    );
    ctx.register_local(var_name, ch_local, str_type);

    // Lower the body
    lower_block(ctx, builder, body);

    // byte_pos += cplen
    let new_pos = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(cplen));
    builder.assign(Place::local(byte_pos), FunctionBuilder::copy(new_pos));

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for elem in array: body` — iterate array elements by index.
fn lower_for_array(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // Store the iterable in a local
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 1 of GorgetArray: {data, len, elem_size, cap})
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    // Body
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // elem = iter[idx] — load element from array
    let elem_type = super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    ctx.register_local(var_name, elem, elem_type);

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        super::stmts::emit_pattern_bindings(ctx, builder, pattern, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for (i, elem) in collection.enumerate(): body` — iterate with index tracking.
/// Instead of materializing an enumerate array, we emit a regular for-loop with a counter.
fn lower_for_enumerate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    parts: &[Spanned<Pattern>],
    receiver: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    

    // Lower the receiver collection
    let iter_op = lower_expr(ctx, builder, receiver);
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);

    // Store the iterable in a local
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 1 of GorgetArray)
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    // Body
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Bind index variable (first tuple element)
    if let Pattern::Binding(idx_name) = &parts[0].node {
        let idx_local = builder.add_local(I64_TYPE, Some(idx_name));
        builder.assign(Place::local(idx_local), FunctionBuilder::copy(idx));
        ctx.register_local(idx_name, idx_local, I64_TYPE);
    }

    // Bind element variable (second tuple element) — load from array
    let elem_type = super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    if let Pattern::Binding(elem_name) = &parts[1].node {
        ctx.register_local(elem_name, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for k, v in dict: body` — iterate Dict or HashMap entries.
fn lower_for_dict(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // Store the iterable in a local
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    let dict_id = iter_local.0;

    // Determine key/value type names from the collection type
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    // Parse: Dict__KeyType__ValueType or HashMap__KeyType__ValueType
    let (key_c_type, val_c_type) = parse_dict_kv_types(&type_name);
    // Look up the TypeIds for key/value types
    let key_type = ctx.lookup_type_by_name(&key_c_type).unwrap_or(I64_TYPE);
    let val_type = ctx.lookup_type_by_name(&val_c_type).unwrap_or(I64_TYPE);

    // oi = 0 (outer iteration index)
    let oi = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(oi), Operand::Constant(Constant::I64(0)));

    // limit = cap (iterate over all slots, check states for USED)
    let limit = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{} = (int64_t)_{}.cap;", limit.0, dict_id));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), Some(builder.new_block()))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    // Header: oi < limit
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(oi), FunctionBuilder::copy(limit));
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    // Body
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // idx = oi (direct index into capacity)
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), FunctionBuilder::copy(oi));

    // state check: if states[idx] != 1, skip to incr
    let state = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{s} = (int64_t)_{dict}.states[(size_t)_{idx}];",
        s = state.0, dict = dict_id, idx = idx.0));
    let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

    let elem_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
    builder.switch_to(elem_bb);

    // Extract key/value bindings
    match &pattern.node {
        Pattern::Tuple(parts) if parts.len() == 2 => {
            let k_name = if let Pattern::Binding(n) = &parts[0].node { n.clone() } else { "__k".to_string() };
            let v_name = if let Pattern::Binding(n) = &parts[1].node { n.clone() } else { "__v".to_string() };

            let k_local = builder.add_local(key_type, Some(&k_name));
            builder.inline_c(format!("_{k} = (({key_c_type}*)_{dict}.keys)[(size_t)_{idx}];",
                k = k_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(&k_name, k_local, key_type);

            let v_local = builder.add_local(val_type, Some(&v_name));
            builder.inline_c(format!("_{v} = (({val_c_type}*)_{dict}.values)[(size_t)_{idx}];",
                v = v_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(&v_name, v_local, val_type);
        }
        Pattern::Binding(name) => {
            // Single binding: bind the key
            let k_local = builder.add_local(key_type, Some(name));
            builder.inline_c(format!("_{k} = (({key_c_type}*)_{dict}.keys)[(size_t)_{idx}];",
                k = k_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(name, k_local, key_type);
        }
        _ => {}
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_oi = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(oi), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(oi), FunctionBuilder::copy(new_oi));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for elem in set: body` — iterate Set elements.
fn lower_for_set(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);
    let set_id = iter_local.0;

    // Parse element type from Set__T
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    let elem_c_type = parse_set_elem_type(&type_name);
    let elem_type = ctx.lookup_type_by_name(&elem_c_type).unwrap_or(I64_TYPE);

    // i = 0
    let i_local = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(i_local), Operand::Constant(Constant::I64(0)));

    // cap (use assignment form — local declared at fn scope)
    let cap = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{} = (int64_t)_{}.cap;", cap.0, set_id));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), Some(builder.new_block()))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(i_local), FunctionBuilder::copy(cap));
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // state check (assignment form)
    let state = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{s} = (int64_t)_{set}.states[(size_t)_{i}];",
        s = state.0, set = set_id, i = i_local.0));
    let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

    let elem_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
    builder.switch_to(elem_bb);

    // Bind element (assignment form with correct type, cast void* keys)
    let elem_local = builder.add_local(elem_type, Some(var_name));
    builder.inline_c(format!("_{e} = (({elem_c_type}*)_{set}.keys)[(size_t)_{i}];",
        e = elem_local.0, set = set_id, i = i_local.0));
    ctx.register_local(var_name, elem_local, elem_type);

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_i = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(i_local), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(i_local), FunctionBuilder::copy(new_i));
    builder.jump(header_bb);

    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for var in iterable: body` for user-defined Iterable[T] types.
/// Generates: iter = Type__iter(&collection); loop { opt = Iter__next(&iter); if None: break; var = opt.Some._0; body }
fn lower_for_iterable(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    type_name: &str,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // 1. Find the iter() method for this type
    // Search fn_sigs for patterns like: Iterable__T_for_TypeName__iter or TypeName__iter
    let iter_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__iter") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__iter");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    let iter_fn_name = match iter_fn_name {
        Some(name) => name,
        None => return, // No iter() method found — silently skip
    };

    // 2. Get the iterator return type from fn_sigs
    let (_, iter_ret_type) = match ctx.fn_sigs.get(&iter_fn_name) {
        Some(sig) => sig.clone(),
        None => return,
    };

    // Get the iterator type name
    let iter_type_name = match ctx.type_registry.type_name(iter_ret_type) {
        Some(name) => name,
        None => return,
    };

    // 3. Find the next() method for the iterator type
    let next_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__next") && k.contains(&format!("_for_{iter_type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{iter_type_name}__next");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    let next_fn_name = match next_fn_name {
        Some(name) => name,
        None => return,
    };

    // 4. Get the Option return type from next()
    let (_, option_ret_type) = match ctx.fn_sigs.get(&next_fn_name) {
        Some(sig) => sig.clone(),
        None => return,
    };

    // Determine the element type from the Option type name (Option__T → T)
    let option_type_name = ctx.type_registry.type_name(option_ret_type)
        .unwrap_or_else(|| "Option__int64_t".to_string());
    let elem_c_type = option_type_name.strip_prefix("Option__")
        .unwrap_or("int64_t");
    let elem_type = ctx.type_mapper.lookup_named(elem_c_type).unwrap_or(I64_TYPE);

    // 5. Store the iterable and call iter()
    let iter_type_full = infer_operand_type_full(ctx, &iter_op, builder);
    let collection_local = builder.add_local(iter_type_full, None);
    builder.assign(Place::local(collection_local), iter_op);

    // Call iter(&collection) → iterator
    let self_ptr_type = ctx.register_ptr_type(iter_type_full);
    let self_ref = builder.borrow(Place::local(collection_local), self_ptr_type);
    let iterator_local = builder.call_extern(
        &iter_fn_name,
        vec![FunctionBuilder::copy(self_ref)],
        iter_ret_type,
    );

    // 6. Build the loop structure
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    builder.jump(header_bb);

    // Header: call next(&iterator) → Option
    builder.switch_to(header_bb);
    let iter_ptr_type = ctx.register_mut_ptr_type(iter_ret_type);
    let iter_ref = builder.borrow_mut(Place::local(iterator_local), iter_ptr_type);
    let opt_result = builder.call_extern(
        &next_fn_name,
        vec![FunctionBuilder::copy(iter_ref)],
        option_ret_type,
    );

    // Check tag: if tag != 0 (None), exit
    let tag_val = builder.tag_of(FunctionBuilder::copy(opt_result));
    let is_none = builder.cmp(
        CmpOp::Ne,
        I32_TYPE,
        FunctionBuilder::copy(tag_val),
        Operand::Constant(Constant::I32(0)),
    );
    let natural_exit = if else_arm.is_some() { else_exit_bb.unwrap() } else { exit_bb };
    builder.branch(FunctionBuilder::copy(is_none), natural_exit, body_bb);

    // Body: extract value from Some variant
    builder.switch_to(body_bb);
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Extract: elem = opt_result.data.Some._0
    let elem_local = builder.enum_field_load(
        Place::local(opt_result),
        "Some",
        0,
        elem_type,
    );
    ctx.register_local(var_name, elem_local, elem_type);

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        super::stmts::emit_pattern_bindings(ctx, builder, pattern, elem_local, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Parse Dict/HashMap type name to extract key/value C type strings.
fn parse_dict_kv_types(type_name: &str) -> (String, String) {
    // Dict__Str__int64_t → ("Str", "int64_t")
    // HashMap__int64_t__Str → ("int64_t", "Str")
    let stripped = type_name
        .strip_prefix("Dict__")
        .or_else(|| type_name.strip_prefix("HashMap__"))
        .or_else(|| type_name.strip_prefix("GorgetDict__"))
        .or_else(|| type_name.strip_prefix("GorgetMap__"))
        .unwrap_or(type_name);
    // Split on __ to get key and value types
    // But type names themselves can contain __ (e.g. Option__int64_t)
    // Simple heuristic: first known type boundary
    if let Some(pos) = find_kv_split(stripped) {
        (stripped[..pos].to_string(), stripped[pos + 2..].to_string())
    } else {
        ("int64_t".to_string(), "int64_t".to_string())
    }
}

/// Parse Set type name to extract element C type string.
fn parse_set_elem_type(type_name: &str) -> String {
    type_name
        .strip_prefix("Set__")
        .or_else(|| type_name.strip_prefix("HashSet__"))
        .or_else(|| type_name.strip_prefix("GorgetSet__"))
        .unwrap_or("int64_t")
        .to_string()
}

/// Find the __ separator between key and value types.
fn find_kv_split(s: &str) -> Option<usize> {
    // Known primitive suffixes to try splitting on
    let primitives = ["int64_t", "int32_t", "int16_t", "int8_t",
                      "uint64_t", "uint32_t", "uint16_t", "uint8_t",
                      "double", "float", "bool", "Str", "GorgetString"];
    for prim in &primitives {
        if s.starts_with(prim) && s[prim.len()..].starts_with("__") {
            return Some(prim.len());
        }
    }
    // Try splitting at each __ position
    let mut i = 0;
    while let Some(pos) = s[i..].find("__") {
        let abs_pos = i + pos;
        if abs_pos > 0 && abs_pos + 2 < s.len() {
            return Some(abs_pos);
        }
        i = abs_pos + 2;
    }
    None
}

/// Lower `for var in start..end: body` or `for var in start..=end: body`.
fn lower_for_range(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    start: &Spanned<Expr>,
    end: &Spanned<Expr>,
    inclusive: bool,
    body: &Block,
    else_arm: Option<&Block>,
) {
    // Create loop variable — type inferred from the start expression.
    // For literal bounds (e.g. `0..n`) this gives I64_TYPE; for typed variables
    // (e.g. `start..end` where start: uint8) it preserves the narrower type.
    let start_val = lower_expr(ctx, builder, start);
    let loop_type = infer_operand_type_full(ctx, &start_val, builder);
    let loop_var = builder.add_local(loop_type, Some(var_name));
    builder.assign(Place::local(loop_var), start_val);
    ctx.register_local(var_name, loop_var, loop_type);

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // For for-else: separate break target from natural exit
    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    // Jump to header
    builder.jump(header_bb);

    // Header: compare loop var with end
    builder.switch_to(header_bb);
    let end_val = lower_expr(ctx, builder, end);
    let cmp_op = if inclusive { CmpOp::Le } else { CmpOp::Lt };
    let cond = builder.cmp(cmp_op, loop_type, FunctionBuilder::copy(loop_var), end_val);
    let natural_exit = if else_arm.is_some() {
        else_exit_bb.unwrap()
    } else {
        exit_bb
    };
    builder.branch(FunctionBuilder::copy(cond), body_bb, natural_exit);

    // Body (wrapped in Loop scope for drop cleanup)
    // Continue target is incr_bb (not header_bb) so the loop variable gets incremented
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(incr_bb);

    // Increment: loop_var = loop_var + 1
    builder.switch_to(incr_bb);
    let one = Operand::Constant(Constant::I64(1));
    let incremented = builder.bin_op(BinOp::Add, loop_type, FunctionBuilder::copy(loop_var), one);
    builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
    builder.jump(header_bb);

    // Else block: executed when loop completes naturally (no break)
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
        builder.jump(exit_bb);

        // Break exit goes directly to exit (skipping else)
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    // Exit
    builder.switch_to(exit_bb);
}

/// Lower an infinite `loop: body` statement.
fn lower_loop(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
) {
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // Jump into the loop body
    builder.jump(body_bb);

    // Body: execute, jump back to body (infinite loop)
    builder.switch_to(body_bb);
    ctx.push_loop(body_bb, exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(body_bb);

    // Exit (reached via break)
    builder.switch_to(exit_bb);
}

/// Lower a `break` statement.
fn lower_break(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let exit_bb = loop_info.exit_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop, None);
        builder.jump(exit_bb);
        // Create unreachable block to absorb dead code after break
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

/// Lower a `continue` statement.
fn lower_continue(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let header_bb = loop_info.header_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop, None);
        builder.jump(header_bb);
        // Create unreachable block to absorb dead code after continue
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

// ---- P3.1: Match Statements ----

/// Lower a match statement to GIR using Branch chains.
fn lower_match_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchArm],
    else_arm: &Option<Block>,
) {
    // Lower scrutinee to a temp local
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = super::exprs::infer_operand_type_full(ctx, &scrut_op, builder);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    let merge_bb = builder.new_block();

    // Process each arm as a test-body chain
    for (i, arm) in arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        // Emit pattern condition check
        let cond = lower_pattern_condition(ctx, builder, &arm.pattern, scrut_local, scrut_type);

        if arm.guard.is_some() {
            // Pattern match → check guard → arm body
            let guard_bb = builder.new_block();
            builder.branch(cond, guard_bb, next_test_bb);

            builder.switch_to(guard_bb);
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            let guard_cond = lower_expr(ctx, builder, arm.guard.as_ref().unwrap());
            builder.branch(guard_cond, arm_body_bb, next_test_bb);

            builder.switch_to(arm_body_bb);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
        } else {
            builder.branch(cond, arm_body_bb, next_test_bb);

            // Arm body
            builder.switch_to(arm_body_bb);
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
        }

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_body) = else_arm {
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
    }

    builder.switch_to(merge_bb);
}

/// Lower a pattern condition to a boolean Operand.
pub fn lower_pattern_condition(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) -> Operand {
    match &pattern.node {
        Pattern::Wildcard => FunctionBuilder::const_bool(true),

        Pattern::Literal(expr) => {
            // None literal: compare enum tag instead of struct == NULL
            if matches!(expr.node, Expr::NoneLiteral) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                let none_tag = super::exprs::resolve_none_tag(ctx, scrut_type);
                let cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(none_tag)),
                );
                return FunctionBuilder::copy(cmp);
            }
            let lit_op = lower_expr(ctx, builder, expr);
            let cmp = builder.cmp(
                CmpOp::Eq,
                scrut_type,
                FunctionBuilder::copy(scrut_local),
                lit_op,
            );
            FunctionBuilder::copy(cmp)
        }

        Pattern::Binding(name) => {
            // Check if this is an enum variant name (unit variant match)
            if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            // Plain variable binding — always matches
            FunctionBuilder::const_bool(true)
        }

        Pattern::Constructor { path, .. } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return FunctionBuilder::const_bool(true);
            };
            // Qualified path (Color.Red): use first segment as enum name
            let (enum_name, variant_name) = if path.len() >= 2 {
                (path[0].node.clone(), variant_name)
            } else {
                // Bare variant: look up via enum_variants (prelude: Ok, Error, Some, None)
                match ctx.resolve_enum_variant(&variant_name) {
                    Some((en, vn)) => (en, vn),
                    None => return FunctionBuilder::const_bool(true),
                }
            };
            let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
            if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                let cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(variant_tag as i32)),
                );
                return FunctionBuilder::copy(cmp);
            }
            FunctionBuilder::const_bool(true)
        }

        Pattern::Or(alts) => {
            // Short-circuit OR: if any alternative matches, return true
            let result_id = builder.add_local(BOOL_TYPE, None);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));

            let merge_bb = builder.new_block();

            for (i, alt) in alts.iter().enumerate() {
                let cond = lower_pattern_condition(ctx, builder, alt, scrut_local, scrut_type);
                let next_bb = if i + 1 < alts.len() {
                    builder.new_block()
                } else {
                    merge_bb
                };
                let true_bb = builder.new_block();
                builder.branch(cond, true_bb, next_bb);

                builder.switch_to(true_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
                builder.jump(merge_bb);

                if i + 1 < alts.len() {
                    builder.switch_to(next_bb);
                }
            }

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Pattern::Tuple(_) | Pattern::Rest => {
            // Structural match — always matches if types match
            FunctionBuilder::const_bool(true)
        }

        Pattern::DotShorthand { variant, .. } => {
            // Use scrutinee type to look up the enum name, then compare tag
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            if let Some(ref en) = enum_name {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(en, &variant.node) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            FunctionBuilder::const_bool(true)
        }
    }
}

/// Emit pattern bindings — assign destructured values to local variables.
pub fn emit_pattern_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            // If not an enum variant, bind the scrutinee value
            if ctx.resolve_enum_variant(name).is_none() {
                ctx.register_local(name, scrut_local, scrut_type);
            }
        }

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return;
            };

            // Use scrutinee type to find the enum name (avoids ambiguous variant lookups
            // when multiple monomorphized enums share variant names like "Some"/"None"/"Ok"/"Err")
            // For qualified paths (Color.Red), path[0] gives us the explicit enum name.
            let enum_name = if path.len() >= 2 {
                Some(path[0].node.clone())
            } else {
                ctx.type_registry.type_name(scrut_type)
                    .or_else(|| {
                        // Fallback: pointer type → dereference to find pointee name
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                            ctx.type_registry.type_name(inner)
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        // Last resort: use variant name lookup (may be ambiguous for generics)
                        ctx.resolve_enum_variant(&variant_name).map(|(en, _)| en)
                    })
            };
            let enum_name = if let Some(en) = enum_name {
                en
            } else {
                return;
            };

            for (i, field_pat) in fields.iter().enumerate() {
                // Determine the field type from the enum variant definition
                let field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            if let Some(f) = v.fields.get(i) {
                                f.type_id
                            } else {
                                I64_TYPE
                            }
                        } else {
                            I64_TYPE
                        }
                    } else {
                        I64_TYPE
                    }
                } else {
                    I64_TYPE
                };

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );

                // Recurse on sub-pattern
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Tuple(elems) => {
            for (i, elem_pat) in elems.iter().enumerate() {
                // Use field_load with field index
                let elem_type = I64_TYPE; // placeholder — real type needs registry
                let dst = builder.field_load(Place::local(scrut_local), i as u32, elem_type);
                emit_pattern_bindings(ctx, builder, elem_pat, dst, elem_type);
            }
        }

        Pattern::DotShorthand { variant, fields } => {
            // Look up enum name from scrutinee type (same as Constructor)
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            let enum_name = if let Some(en) = enum_name { en } else { return; };
            let variant_name = variant.node.clone();

            for (i, field_pat) in fields.iter().enumerate() {
                let field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            v.fields.get(i).map(|f| f.type_id).unwrap_or(I64_TYPE)
                        } else { I64_TYPE }
                    } else { I64_TYPE }
                } else { I64_TYPE };

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Or(_) | Pattern::Rest => {
            // No bindings
        }
    }
}

// ---- P3.3: Error Handling ----

/// Lower a `throw expr` statement.
fn lower_throw(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) {
    let val = lower_expr(ctx, builder, expr);
    if let Some(result_type) = ctx.current_throws_result_type {
        // Wrap error in Result.Error and return
        let err_dst = {
                let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                builder.enum_init(type_name, "Error", result_type, vec![val])
            };
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        let code = Operand::Constant(Constant::I32(1));
        builder.call_extern("gorget_throw", vec![val, code], UNIT_TYPE);
        builder.unreachable();
    }
    // Create unreachable block for dead code after throw
    let dead_bb = builder.new_block();
    builder.switch_to(dead_bb);
}

/// Lower an `assert condition [, message]` statement.
fn lower_assert(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    message: Option<&Spanned<Expr>>,
) {
    // If strip-asserts directive is active, emit nothing (skip assert entirely)
    if ctx.strip_asserts {
        return;
    }

    // For binary comparison conditions without a custom message, emit a rich diagnostic
    // that includes the actual left/right values (like `assert 1 == 2` → shows "left: 1, right: 2").
    // Only applies to primitive numeric/bool types — strings and structs fall through to the
    // simple path (they need special comparison logic via gorget_str_eq, etc.).
    if message.is_none() {
        if let Expr::BinaryOp { left, op, right } = &condition.node {
            if let Some((op_str, cmp_op)) = comparison_op_info(*op) {
                let lhs_op = lower_expr(ctx, builder, left);
                let rhs_op = lower_expr(ctx, builder, right);
                let lhs_type = infer_operand_type_full(ctx, &lhs_op, builder);
                let rhs_type = infer_operand_type_full(ctx, &rhs_op, builder);

                if is_primitive_type_for_assert(lhs_type) && is_primitive_type_for_assert(rhs_type) {
                    let cond_local = builder.cmp(cmp_op, lhs_type, lhs_op.clone(), rhs_op.clone());

                    let pass_bb = builder.new_block();
                    let fail_bb = builder.new_block();
                    builder.branch(Operand::Copy(Place::local(cond_local)), pass_bb, fail_bb);
                    builder.switch_to(fail_bb);

                    let (lhs_fmt, lhs_arg) = assert_printf_info(&lhs_op, lhs_type);
                    let (rhs_fmt, rhs_arg) = assert_printf_info(&rhs_op, rhs_type);
                    builder.inline_c(format!(
                        "gorget_panic(gorget_format(\"assertion failed: left {op_str} right\\n  left:  {lhs_fmt}\\n  right: {rhs_fmt}\", {lhs_arg}, {rhs_arg}));"
                    ));
                    builder.unreachable();
                    builder.switch_to(pass_bb);
                    return;
                }
            }
        }
    }

    let cond = lower_expr(ctx, builder, condition);

    let pass_bb = builder.new_block();
    let fail_bb = builder.new_block();

    builder.branch(cond, pass_bb, fail_bb);

    // Fail path: panic with message (allows test-mode setjmp to catch it).
    builder.switch_to(fail_bb);
    if let Some(msg) = message {
        // Custom message provided — lower it and pass to gorget_panic.
        let msg_op = lower_expr(ctx, builder, msg);
        builder.call_extern("gorget_panic", vec![msg_op], UNIT_TYPE);
        builder.unreachable();
        builder.switch_to(pass_bb);
        return;
    }
    // No custom message: generate a static message based on the expression shape.
    let panic_msg = generate_assert_static_msg(condition);
    builder.call_extern(
        "gorget_panic",
        vec![Operand::Constant(Constant::Str(panic_msg))],
        UNIT_TYPE,
    );
    builder.unreachable();

    // Pass path: continue
    builder.switch_to(pass_bb);
}

/// Return `(op_str, CmpOp)` for a comparison BinaryOp, or None for non-comparison ops.
fn comparison_op_info(op: BinaryOp) -> Option<(&'static str, CmpOp)> {
    match op {
        BinaryOp::Eq    => Some(("==", CmpOp::Eq)),
        BinaryOp::Neq   => Some(("!=", CmpOp::Ne)),
        BinaryOp::Lt    => Some(("<",  CmpOp::Lt)),
        BinaryOp::Gt    => Some((">",  CmpOp::Gt)),
        BinaryOp::LtEq  => Some(("<=", CmpOp::Le)),
        BinaryOp::GtEq  => Some((">=", CmpOp::Ge)),
        _ => None,
    }
}

/// Return true if type_id is a primitive numeric/bool type suitable for assert rich diagnostics.
/// Strings and named types need special comparison logic and are excluded.
fn is_primitive_type_for_assert(type_id: TypeId) -> bool {
    matches!(type_id,
        I64_TYPE | I32_TYPE | I16_TYPE | I8_TYPE |
        U64_TYPE | U32_TYPE | U16_TYPE | U8_TYPE |
        F64_TYPE | F32_TYPE | BOOL_TYPE | CHAR_TYPE
    )
}

/// Return `(printf_format_spec, c_expression)` for an assert diagnostic operand.
/// Only called for primitive types (guaranteed by is_primitive_type_for_assert).
fn assert_printf_info(op: &Operand, type_id: TypeId) -> (String, String) {
    let c_expr = operand_to_c_str(op);
    if type_id == F64_TYPE || type_id == F32_TYPE {
        ("%g".to_string(), format!("(double){c_expr}"))
    } else if type_id == BOOL_TYPE {
        ("%s".to_string(), format!("({c_expr}) ? \"true\" : \"false\""))
    } else if type_id == CHAR_TYPE {
        ("%lld".to_string(), format!("(long long)({c_expr})"))
    } else {
        // All integer types: treat as int64_t
        ("%lld".to_string(), format!("(long long)({c_expr})"))
    }
}

/// Convert a GIR operand to its C expression string (for embedding in InlineC).
fn operand_to_c_str(op: &Operand) -> String {
    match op {
        Operand::Copy(place) | Operand::Move(place) => {
            let mut s = format!("_{}", place.local.0);
            for proj in &place.projections {
                match proj {
                    Projection::Deref => s = format!("(*{s})"),
                    Projection::Field(i) => s = format!("{s}.__field_{i}"),
                    _ => {}
                }
            }
            s
        }
        Operand::Constant(c) => match c {
            Constant::I64(n) => format!("{n}LL"),
            Constant::I32(n) => n.to_string(),
            Constant::F64(f) => format!("{f}"),
            Constant::Bool(b) => if *b { "1".to_string() } else { "0".to_string() },
            Constant::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\n");
                format!("\"{}\"", escaped)
            }
            _ => "0".to_string(),
        },
    }
}

/// Generate a static assertion failure message for an assertion condition.
/// For binary comparisons, includes the operator name (e.g., "left == right").
fn generate_assert_static_msg(condition: &Spanned<Expr>) -> String {
    if let Expr::BinaryOp { op, .. } = &condition.node {
        let op_str = match op {
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::LtEq => "<=",
            BinaryOp::GtEq => ">=",
            _ => return "assertion failed".to_string(),
        };
        format!("assertion failed: left {op_str} right")
    } else {
        "assertion failed".to_string()
    }
}

// ---- P3.5: With statement ----

/// Check if an expression is an allocator constructor (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, FallbackAllocator).
fn is_allocator_constructor(expr: &Expr) -> bool {
    if let Expr::Call { callee, .. } = expr {
        if let Expr::Identifier(name) = &callee.node {
            return matches!(name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator");
        }
    }
    false
}

/// Lower a `with bindings: body` statement.
fn lower_with(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    bindings: &[ast::WithBinding],
    body: &Block,
) {
    ctx.drops.push_scope(DropScopeKind::Block);

    let mut allocator_locals = Vec::new();

    for binding in bindings {
        let is_alloc = is_allocator_constructor(&binding.expr.node);
        let val = lower_expr(ctx, builder, &binding.expr);
        let type_id = super::exprs::infer_operand_type_full(ctx, &val, builder);
        let local_id = builder.add_local(type_id, Some(&binding.name.node));
        ctx.register_local(&binding.name.node, local_id, type_id);
        ctx.drops.register_local(local_id, type_id, &ctx.type_registry);
        builder.assign(Place::local(local_id), val);

        // If this is an allocator, push it as the active thread-local allocator
        if is_alloc {
            builder.push_allocator(FunctionBuilder::copy(local_id));
            allocator_locals.push(local_id);
        }
    }

    lower_block(ctx, builder, body);

    // Drop all non-allocator locals FIRST (while the allocator is still alive),
    // then pop + destroy allocators. This avoids use-after-free when collections
    // allocated within the `with` scope try to dealloc via the active allocator.
    ctx.drops.pop_scope(builder, &ctx.type_registry);

    for &local_id in allocator_locals.iter().rev() {
        builder.pop_allocator();
        let type_id = builder.locals[local_id.0 as usize].type_id;
        let type_name = ctx.type_name_for_id(type_id);
        let destroy_fn = match type_name.as_deref() {
            Some("PoolAllocator") => Some("gorget_pool_destroy"),
            Some("TlsfAllocator") => Some("gorget_tlsf_destroy"),
            Some("TrackingAllocator") => Some("gorget_tracking_destroy"),
            Some("Arena") => Some("gorget_arena_destroy"),
            Some("FixedBufferAllocator") => Some("gorget_fba_destroy"),
            Some("FallbackAllocator") => Some("gorget_fallback_destroy"),
            _ => None,
        };
        if let Some(fn_name) = destroy_fn {
            builder.call_void(fn_name, vec![FunctionBuilder::copy(local_id)]);
        }
    }
}

/// If the condition is an `Expr::Is { expr, pattern, .. }`, emit pattern bindings
/// so that variables bound by the pattern (e.g., `if x is Some(v):`) become usable
/// in the then-branch.
pub fn emit_is_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
) {
    if let Expr::Is { expr: inner, pattern, negated } = &condition.node {
        if *negated {
            return; // `is not` — no bindings in the then branch
        }
        // Re-lower the inner expression to get the scrutinee local
        let val = lower_expr(ctx, builder, inner);
        let scrut_type = super::exprs::infer_operand_type_full(ctx, &val, builder);
        let scrut_local = if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
            place.local
        } else {
            let tmp = builder.add_local(scrut_type, None);
            builder.assign(Place::local(tmp), val);
            tmp
        };
        emit_pattern_bindings(ctx, builder, pattern, scrut_local, scrut_type);
    }
    // Also handle `condition and is_expr` compound conditions
    if let Expr::BinaryOp { left, op: ast::BinaryOp::And, right } = &condition.node {
        emit_is_bindings(ctx, builder, left);
        emit_is_bindings(ctx, builder, right);
    }
}

/// Infer operand type using both ctx locals and builder locals (for intermediates like tuples).
pub fn infer_operand_type_with_builder(
    ctx: &LoweringContext,
    operand: &Operand,
    builder: &FunctionBuilder,
) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals (user-named variables)
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            // Fall back to builder locals (compiler temporaries)
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                return builder.locals[idx].type_id;
            }
            I64_TYPE
        }
        other => super::exprs::infer_operand_type(ctx, other),
    }
}

/// Check if a block always ends with a return statement.
fn block_always_returns(block: &Block) -> bool {
    if let Some(last) = block.stmts.last() {
        matches!(last.node, Stmt::Return(_))
    } else {
        false
    }
}

/// Stub for `select` statement lowering in synchronous GIR mode.
/// The async backend handles select via its own codegen path; in the synchronous
/// GIR path we emit a no-op (the C backend for async will never see this path).
/// Lower a `select` statement using a spin-wait loop over channel arms.
///
/// ```text
/// loop_header → try_arm_0 → (ready) → body_arm_0 → exit_bb
///                         → (not ready) → try_arm_1 → (ready) → body_arm_1 → exit_bb
///                                       → (not ready) → loop_header
/// ```
/// Lower a `select` statement using a spin-wait loop over channel arms.
///
/// ```text
/// loop_header → try_arm_0 → (ready) → body_arm_0 → exit_bb
///                         → (not ready) → try_arm_1 → (ready) → body_arm_1 → exit_bb
///                                       → (not ready) → loop_header
/// ```
fn lower_select(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    arms: &[ast::SelectArm],
) {
    let num_arms = arms.len();
    if num_arms == 0 {
        return;
    }

    let loop_header = builder.new_block();
    let exit_bb = builder.new_block();

    // Allocate try and body blocks for each arm
    let try_blocks: Vec<_> = (0..num_arms).map(|_| builder.new_block()).collect();
    let body_blocks: Vec<_> = (0..num_arms).map(|_| builder.new_block()).collect();

    // Entry: jump to spin loop header
    builder.jump(loop_header);

    // Loop header: jump to first try block
    builder.switch_to(loop_header);
    builder.jump(try_blocks[0]);

    for (i, arm) in arms.iter().enumerate() {
        let next_block = if i + 1 < num_arms { try_blocks[i + 1] } else { loop_header };

        match &arm.op {
            SelectOp::Recv { channel, name, .. } => {
                // Try block: poll the channel; if ready, jump to body; else try next arm
                builder.switch_to(try_blocks[i]);

                // Lower the channel expression
                let ch_op = lower_expr(ctx, builder, channel);
                let ch_type = infer_operand_type_full(ctx, &ch_op, builder);

                // Get a mutable pointer to the channel
                let ch_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = ch_op {
                    let ptr_type = ctx.register_mut_ptr_type(ch_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    Operand::Copy(Place::local(ptr_local))
                } else {
                    let temp = builder.add_local(ch_type, None);
                    builder.assign(Place::local(temp), ch_op.clone());
                    let ptr_type = ctx.register_mut_ptr_type(ch_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, Place::local(temp));
                    Operand::Copy(Place::local(ptr_local))
                };

                // Determine element type from Channel__T name
                let ch_type_name = ctx.type_name_for_id(ch_type)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "Channel__int64_t".to_string());
                let elem_suffix = ch_type_name.strip_prefix("Channel__").unwrap_or("int64_t");
                let elem_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);

                // Allocate output slot and get a mutable pointer to it
                let out_local = builder.add_local(elem_type, None);
                let out_ptr_type = ctx.register_mut_ptr_type(elem_type);
                let out_ptr_local = builder.add_local(out_ptr_type, None);
                builder.emit_borrow_mut(out_ptr_local, Place::local(out_local));
                let out_ptr_op = Operand::Copy(Place::local(out_ptr_local));

                // Call poll_recv(&ch, &out, NULL) → bool
                let poll_fn = format!("{ch_type_name}__poll_recv");
                let result_local = builder.call(
                    &poll_fn,
                    vec![ch_ptr, out_ptr_op, Operand::Constant(Constant::Null)],
                    BOOL_TYPE,
                );
                let result_op = Operand::Copy(Place::local(result_local));

                // Branch: if ready → body block, else → next arm (or loop header)
                builder.branch(result_op, body_blocks[i], next_block);

                // Body block: bind variable, lower body, jump to exit
                builder.switch_to(body_blocks[i]);
                let var_name = &name.node;
                ctx.register_local(var_name, out_local, elem_type);
                lower_block(ctx, builder, &arm.body);
                builder.jump(exit_bb);
            }
            SelectOp::Send { .. } => {
                // Send arms not yet implemented — treat as always-not-ready
                builder.switch_to(try_blocks[i]);
                builder.jump(next_block);
                builder.switch_to(body_blocks[i]);
                builder.jump(exit_bb);
            }
        }
    }

    builder.switch_to(exit_bb);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> LoweringContext<'static> {
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        LoweringContext::new(analysis, mapper, reg)
    }

    #[test]
    fn lower_var_decl_test() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::VarDecl {
            is_const: false,
            is_mutable: false,
            type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Int)),
            pattern: spanned(Pattern::Binding("x".into())),
            value: spanned(Expr::IntLiteral(42)),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created a local and an assign
        assert!(ctx.lookup_local("x").is_some());
        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::Assign { .. }
        ));
    }

    #[test]
    fn lower_if_else_test() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::If {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("x".into()))),
                op: ast::BinaryOp::Gt,
                right: Box::new(spanned(Expr::IntLiteral(0))),
            }),
            then_body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            elif_branches: vec![],
            else_body: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created multiple blocks (entry + merge + then + else)
        assert!(builder.blocks.len() >= 3);
        // Entry block should end with a Branch terminator
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
    }

    #[test]
    fn lower_while_loop_test() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have header, body, exit blocks
        assert!(builder.blocks.len() >= 4); // entry + header + body + exit
        // Entry block should jump to header
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(_))
        ));
    }

    // ---- P3.0: Break, Continue, Loop tests ----

    #[test]
    fn lower_loop_basic() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have: entry(bb0) → body(bb1), exit(bb2), dead(bb3)
        assert!(builder.blocks.len() >= 3);
        // Entry block should jump to body
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(BlockId(1)))
        ));
        // Body block back-edge (body_bb → body_bb) won't be present since break overrides it;
        // the break inside the body should jump to exit_bb
    }

    #[test]
    fn lower_break_in_loop() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop: break
        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // The body block (bb1) should contain a Jump to the exit block (bb2)
        // break emits: jump to exit_bb, then creates dead block
        let body_block = &builder.blocks[1];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            // Break should jump to exit_bb (bb2)
            assert_eq!(*target, BlockId(2), "break should jump to exit block");
        } else {
            panic!("Body block should have Jump terminator from break");
        }
    }

    #[test]
    fn lower_continue_in_while() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        // while i < 10: continue
        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Continue)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // bb0=entry, bb1=header, bb2=body, bb3=exit, bb4=dead(from continue)
        // Body block (bb2) should jump back to header (bb1)
        let body_block = &builder.blocks[2];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            assert_eq!(*target, BlockId(1), "continue should jump to header block");
        } else {
            panic!("Body block should have Jump terminator from continue");
        }
    }

    #[test]
    fn lower_nested_break() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop:
        //   loop:
        //     break   <- should break inner loop only
        let inner_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });
        let outer_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![inner_loop],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &outer_loop);

        // After lowering, the inner break should target inner exit, not outer exit.
        // The structure is:
        // bb0: entry → jump to outer_body (bb1)
        // bb1: outer body → inner stuff starts here
        //   bb3: inner body → break jumps to inner exit (bb4)
        //   bb4: inner exit → falls through
        // bb2: outer exit
        // The key assertion: inner break doesn't reach outer exit

        // Verify we have enough blocks
        assert!(builder.blocks.len() >= 5, "Should have at least 5 blocks for nested loops");
    }

    // ---- P3.1: Match statement tests ----

    #[test]
    fn lower_match_literal() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![
                ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(10)),
                    span: Span { start: 0, end: 0 },
                },
                ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(20)),
                    span: Span { start: 0, end: 0 },
                },
            ],
            else_arm: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should create blocks for scrutinee + each arm + else + merge
        assert!(builder.blocks.len() >= 5);
        // Entry block: assign scrutinee, then branch on first pattern
        // There should be Cmp instructions for literal matching
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { .. }))
        });
        assert!(has_cmp, "Should have Cmp instructions for literal pattern matching");
    }

    #[test]
    fn lower_match_binding() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchArm {
                pattern: spanned(Pattern::Binding("val".into())),
                guard: None,
                body: spanned(Expr::IntLiteral(42)),
                span: Span { start: 0, end: 0 },
            }],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // The binding pattern should register "val" as a local alias
        assert!(ctx.lookup_local("val").is_some(), "Pattern binding should register 'val'");
    }

    #[test]
    fn lower_match_or_pattern() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        // match x: case 1 | 2 | 3: pass
        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchArm {
                pattern: spanned(Pattern::Or(vec![
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(3))))),
                ])),
                guard: None,
                body: spanned(Expr::IntLiteral(0)),
                span: Span { start: 0, end: 0 },
            }],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have multiple Cmp instructions (one per alternative) and Branch terminators
        let cmp_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter().filter(|inst| matches!(inst, Instruction::Cmp { .. })).count())
            .sum();
        assert!(cmp_count >= 3, "Or pattern should have at least 3 Cmp instructions, got {cmp_count}");
    }

    // ---- P3.3: Error handling tests ----

    #[test]
    fn lower_assert_true() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Assert {
            condition: spanned(Expr::BoolLiteral(true)),
            message: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have Branch terminator in entry block
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
        // Fail block should have Unreachable terminator
        let has_unreachable = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Unreachable))
        });
        assert!(has_unreachable, "Assert fail path should have Unreachable terminator");
    }

    #[test]
    fn lower_throw_stmt() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Throw(spanned(Expr::StringLiteral(
            crate::lexer::token::StringLiteral {
                kind: crate::lexer::token::StringKind::Normal,
                segments: vec![crate::lexer::token::StringSegment::Literal("error".into())],
            },
        ))));

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have a CallExtern to gorget_throw + Unreachable
        let has_throw = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_throw")
        });
        assert!(has_throw, "Should call gorget_throw");
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Unreachable)
        ));
    }
}
