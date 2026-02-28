use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, Ownership};
use crate::parser::Parser;
use crate::span::Spanned;

use super::context::LoweringContext;

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
                // If this is a mutable capture pointer, deref to get the value
                if let Some(&value_type) = ctx.mut_capture_locals.get(&local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    let tmp = builder.add_local(value_type, None);
                    builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                    Operand::Copy(Place::local(tmp))
                } else {
                    Operand::Copy(Place::local(local_id))
                }
            } else if let Some(constant) = ctx.module_constants.get(name) {
                Operand::Constant(constant.clone())
            } else if ctx.fn_sigs.contains_key(name.as_str()) {
                // Named function reference (for passing as Callable argument)
                Operand::Constant(Constant::FuncRef(name.clone()))
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
            lower_call(ctx, builder, callee, args, generic_args.as_deref())
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
                    builder.locals[place_clone.local.0 as usize].type_id
                } else {
                    I64_TYPE
                };
                let tmp = builder.add_local(local_type, None);
                builder.assign(Place::local(tmp), val);
                builder.move_zero(place_clone.clone());
                ctx.drops.mark_moved(place_clone.local);
                FunctionBuilder::copy(tmp)
            } else {
                val
            }
        }

        Expr::MutableBorrow { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.locals[place.local.0 as usize].type_id
                } else {
                    UNIT_TYPE
                };
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
        Expr::If { condition, then_branch, else_branch, .. } => {
            lower_if_expr(ctx, builder, condition, then_branch, else_branch.as_deref())
        }

        // -- P3.2: Match expression --
        Expr::Match { scrutinee, arms, else_arm } => {
            lower_match_expr(ctx, builder, scrutinee, arms, else_arm.as_deref())
        }

        // -- P3.3: Try/error handling --
        Expr::Try { expr: inner } => {
            lower_try_expr(ctx, builder, inner)
        }

        Expr::TryCapture { expr: inner } => {
            // try expr → evaluate, extract Ok value or return zero-default on error
            let val = lower_expr(ctx, builder, inner);
            let val_type = infer_operand_type_full(ctx, &val, builder);
            let val_local = builder.add_local(val_type, None);
            builder.assign(Place::local(val_local), val);

            // Look up Ok field type
            let ok_field_type = {
                let type_name = ctx.type_registry.type_name(val_type);
                if let Some(ref name) = type_name {
                    if let Some(td) = ctx.type_registry.get_type_def(name) {
                        if let crate::ir::types::TypeDefKind::Enum(ref e) = td.kind {
                            e.variants.iter().find(|v| v.name == "Ok" || v.name == "Some")
                                .and_then(|v| v.fields.first().map(|f| f.type_id))
                                .unwrap_or(I64_TYPE)
                        } else { I64_TYPE }
                    } else { I64_TYPE }
                } else { I64_TYPE }
            };

            let tag = builder.tag_of(FunctionBuilder::copy(val_local));
            let is_ok = builder.cmp(CmpOp::Eq, I32_TYPE, FunctionBuilder::copy(tag), Operand::Constant(Constant::I32(0)));

            let ok_bb = builder.new_block();
            let err_bb = builder.new_block();
            let merge_bb = builder.new_block();

            builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

            // Ok path: extract value
            builder.switch_to(ok_bb);
            let ok_val = builder.enum_field_load(Place::local(val_local), "Ok", 0, ok_field_type);
            let result_local = builder.add_local(ok_field_type, None);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(ok_val));
            builder.jump(merge_bb);

            // Error path: zero-default
            builder.switch_to(err_bb);
            let zero = match ok_field_type {
                id if id == BOOL_TYPE => Operand::Constant(Constant::Bool(false)),
                id if id == F64_TYPE => Operand::Constant(Constant::F64(0.0)),
                id if id == F32_TYPE => Operand::Constant(Constant::F32(0.0)),
                _ => Operand::Constant(Constant::I64(0)),
            };
            builder.assign(Place::local(result_local), zero);
            builder.jump(merge_bb);

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_local)
        }

        // -- P3.4: Miscellaneous expressions --
        Expr::CharLiteral(ch) => {
            Operand::Constant(Constant::Char(*ch as u32))
        }

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
            let type_id = register_tuple_type(ctx, &elem_types);
            let dst = builder.tuple_init(operands, type_id);
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
                // Need to determine the dereferenced type
                let local_idx = place.local.0 as usize;
                let deref_type = if local_idx < builder.locals.len() {
                    let ptr_type = builder.locals[local_idx].type_id;
                    ctx.pointee_type(ptr_type).unwrap_or(I64_TYPE)
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

        Expr::NilCoalescing { lhs, rhs } => {
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
            // Qualified path — try to resolve as enum variant
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

        // Await: in synchronous GIR mode, just lower the inner expression
        Expr::Await { expr } => {
            lower_expr(ctx, builder, expr)
        }

        // Spawn: in synchronous GIR mode, just lower the inner expression directly
        // (spawn compute(x) becomes compute(x) — Task is immediately resolved)
        Expr::Spawn { expr } => {
            lower_expr(ctx, builder, expr)
        }

        // Deferred: Select
        _ => Operand::Constant(Constant::Unit),
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
                        if name.starts_with("Option__") {
                            Some(et)
                        } else {
                            None
                        }
                    })
                })
                .unwrap_or(UNIT_TYPE);
            let type_name = ctx.type_registry.type_name(type_id).unwrap_or_else(|| mangled.clone());
            let dst = builder.enum_init(&type_name, "Some", type_id, vec![field_op]);
            Some(FunctionBuilder::copy(dst))
        }
        "None" if args.is_empty() => {
            // None() has no arguments — determine type from context
            let (type_name, type_id) = if let Some(et) = ctx.expected_type {
                let name = ctx.type_registry.type_name(et)
                    .unwrap_or_else(|| "Option__int64_t".to_string());
                if name.starts_with("Option__") {
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
            Some(FunctionBuilder::copy(dst))
        }
        "Ok" if args.len() == 1 => {
            // Ok(value) — determine Result type from context (expected_type)
            if let Some(et) = ctx.expected_type {
                let name = ctx.type_registry.type_name(et).unwrap_or_default();
                if name.starts_with("Result__") {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let consumed = consumed_local_id(&field_op);
                    let dst = builder.enum_init(&name, "Ok", et, vec![field_op]);
                    // Mark consumed local as moved AFTER enum_init copies its value
                    if let Some(local) = consumed {
                        mark_consumed_local_by_id(ctx, builder, local);
                    }
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            // Also check current_throws_result_type
            if let Some(rt) = ctx.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                if name.starts_with("Result__") {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let consumed = consumed_local_id(&field_op);
                    let dst = builder.enum_init(&name, "Ok", rt, vec![field_op]);
                    if let Some(local) = consumed {
                        mark_consumed_local_by_id(ctx, builder, local);
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
                if name.starts_with("Result__") {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = builder.enum_init(&name, "Error", et, vec![field_op]);
                    return Some(FunctionBuilder::copy(dst));
                }
            }
            if let Some(rt) = ctx.current_throws_result_type {
                let name = ctx.type_registry.type_name(rt).unwrap_or_default();
                if name.starts_with("Result__") {
                    let field_op = lower_expr(ctx, builder, &args[0]);
                    let dst = builder.enum_init(&name, "Error", rt, vec![field_op]);
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
        let dst = builder.call_extern(fn_name, vec![arg_op], owned_type);
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
        let box_type = ctx.type_mapper.lookup_named(&box_mangled).unwrap_or(I64_TYPE);
        // Emit: __gorget_box_alloc_T(value) → T* with heap alloc
        let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
        let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
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
        let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }
    // Also check the base name for non-generic enum variants
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
        let field_operands: Vec<Operand> = args.iter()
            .map(|arg| lower_expr(ctx, builder, arg))
            .collect();

        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }

    // Regular struct literal
    let field_operands: Vec<Operand> = args.iter()
        .map(|arg| lower_expr(ctx, builder, arg))
        .collect();

    let type_id = ctx.type_mapper.lookup_named(&effective_name).unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(&effective_name, type_id, field_operands);
    FunctionBuilder::copy(dst)
}

/// Lower a field access expression.
fn lower_field_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Operand {
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

            // If the local is a pointer (e.g., self in equip methods), dereference it
            // to get the underlying struct type for field access.
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
                // First try the struct_fields cache
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    let dst = builder.field_load(base_place, field_idx, field_type);
                    return FunctionBuilder::copy(dst);
                }
                // Fallback: read directly from TypeDef (handles dynamically-registered
                // types like tuples that may not be in the struct_fields cache)
                if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, field) in s.fields.iter().enumerate() {
                            if field.name == field_name {
                                let dst = builder.field_load(base_place, i as u32, field.type_id);
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
fn try_resolve_field_place(
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

/// Lower a method call on a concrete (non-trait-object) type.
fn lower_method_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    receiver: &Spanned<Expr>,
    method_name: &str,
    args: &[Spanned<ast::CallArg>],
) -> Operand {
    // Static method call: Type.method(args) where receiver is a type name, not a value
    if let Expr::Identifier(name) = &receiver.node {
        if ctx.lookup_local(name).is_none() && !ctx.module_constants.contains_key(name) {
            // Box.new(value) → heap allocation
            if name == "Box" && method_name == "new" && !args.is_empty() {
                let val = lower_expr(ctx, builder, &args[0].node.value);
                let inner_type = infer_operand_type_full(ctx, &val, builder);
                let inner_c = ctx.type_name_for_id(inner_type).unwrap_or("int64_t").to_string();
                let box_type_name = format!("Box__{inner_c}");
                let box_type = if let Some(tid) = ctx.type_mapper.lookup_named(&box_type_name) {
                    tid
                } else {
                    let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(box_type_name.clone()));
                    ctx.type_mapper.register_named(box_type_name.clone(), tid);
                    tid
                };
                let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
                let dst = builder.call(alloc_fn, vec![val], box_type);
                return FunctionBuilder::copy(dst);
            }

            // Check if this is a known type name (including primitives like int, float, bool)
            let is_primitive_type = matches!(name.as_str(), "int" | "float" | "bool" | "uint8" | "uint16" | "uint32" | "uint64"
                | "int8" | "int16" | "int32" | "str" | "char" | "byte");
            if is_primitive_type || ctx.type_mapper.lookup_named(name).is_some() || ctx.resolve_enum_variant(name).is_some() {
                let lowered_args: Vec<Operand> = args.iter()
                    .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                    .collect();

                // Map Gorget primitive names to C type names for method lookup
                let c_type_name = match name.as_str() {
                    "int" => "int64_t",
                    "float" => "double",
                    "bool" => "bool",
                    "str" => "Str",
                    "char" => "char",
                    "byte" | "uint8" => "uint8_t",
                    "uint16" => "uint16_t",
                    "uint32" => "uint32_t",
                    "uint64" => "uint64_t",
                    "int8" => "int8_t",
                    "int16" => "int16_t",
                    "int32" => "int32_t",
                    _ => name.as_str(),
                };
                let mangled = format!("{c_type_name}__{method_name}");
                // Check fn_sigs for direct name or trait-mangled name (Trait_for_Type__method)
                let effective_name = if ctx.fn_sigs.contains_key(mangled.as_str()) {
                    mangled.clone()
                } else {
                    // Search for *_for_Type__method pattern — may have multiple overloads
                    let suffix = format!("_for_{name}__{method_name}");
                    let candidates: Vec<&String> = ctx.fn_sigs.keys()
                        .filter(|k| k.ends_with(&suffix))
                        .collect();

                    if candidates.len() == 1 {
                        candidates[0].clone()
                    } else if candidates.len() > 1 {
                        // Multiple overloads — match by arg types
                        let arg_types: Vec<TypeId> = lowered_args.iter()
                            .map(|a| infer_operand_type_full(ctx, a, builder))
                            .collect();
                        candidates.iter()
                            .find(|k| {
                                if let Some((params, _)) = ctx.fn_sigs.get(k.as_str()) {
                                    params.len() == arg_types.len()
                                        && params.iter().zip(arg_types.iter()).all(|(p, a)| *p == *a)
                                } else {
                                    false
                                }
                            })
                            .map(|k| (*k).clone())
                            .unwrap_or(mangled.clone())
                    } else {
                        mangled.clone()
                    }
                };

                let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
                    *ret
                } else {
                    // Try to infer return type — for .from(), .default() etc., return the type itself
                    ctx.type_mapper.lookup_named(name).unwrap_or(I64_TYPE)
                };
                if ret_type == UNIT_TYPE {
                    builder.call_void(effective_name, lowered_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = builder.call(effective_name, lowered_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
        }
    }

    // Before lowering the receiver expression, check if it's a field access that
    // we may need to borrow in-place (e.g., self.values.push(x) should not copy values).
    let field_place_info = if let Expr::FieldAccess { object, field } = &receiver.node {
        try_resolve_field_place(ctx, builder, object, &field.node)
    } else {
        None
    };

    let recv = lower_expr(ctx, builder, receiver);

    // .await() → pass-through (GIR runs everything synchronously)
    if method_name == "await" {
        return recv;
    }

    // Primitive .hash() → runtime hash functions
    if method_name == "hash" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == I64_TYPE || recv_type == I32_TYPE {
            let dst = builder.call_extern("__gorget_hash_int", vec![recv], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == BOOL_TYPE {
            let cast = builder.cast(I64_TYPE, recv);
            let dst = builder.call_extern("__gorget_hash_int", vec![FunctionBuilder::copy(cast)], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }
        // Str.hash() is handled by the normal method dispatch path below
    }

    // .unwrap() / .expect() / .unwrap_or() on Option/Result → inline extraction
    // On non-Option/Result types → pass-through (unwrap is a no-op)
    if matches!(method_name, "unwrap" | "expect" | "unwrap_or") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| n.starts_with("Option") || n.starts_with("Result"))
            .unwrap_or(false);
        if !is_option_or_result {
            // Not an Option/Result — unwrap is a no-op
            return recv;
        }
        // For Option/Result, extract the inner value via extern call that C backend handles
        if let Some(ref tn) = type_name {
            let is_result = tn.starts_with("Result__");
            let inner_type = if tn.starts_with("Option__") {
                let inner_name = &tn["Option__".len()..];
                resolve_inner_type(ctx, inner_name)
            } else if is_result {
                // Result__Ok__Err → extract Ok type (strip error type from end)
                let rest = &tn["Result__".len()..];
                // Try stripping common error suffixes
                let ok_name = ["__Str", "__int64_t", "__bool", "__double"].iter()
                    .find_map(|suffix| rest.strip_suffix(suffix))
                    .unwrap_or_else(|| {
                        // Fallback: split at last __ separator
                        rest.rfind("__").map(|pos| &rest[..pos]).unwrap_or(rest)
                    });
                resolve_inner_type(ctx, ok_name)
            } else {
                I64_TYPE
            };

            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                let ptr_type = ctx.register_ptr_type(
                    infer_operand_type_full(ctx, &recv, builder),
                );
                let borrow = builder.add_local(ptr_type, None);
                builder.emit_borrow(borrow, place.clone());

                if method_name == "unwrap_or" {
                    // unwrap_or(default) → (tag == 0) ? data.Variant._0 : default
                    let default_val = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let extern_name = if is_result { "__result_unwrap_or" } else { "__option_unwrap_or" };
                    let dst = builder.call_extern(
                        extern_name,
                        vec![FunctionBuilder::copy(borrow), default_val],
                        inner_type,
                    );
                    return FunctionBuilder::copy(dst);
                } else {
                    // unwrap() / expect() → direct extraction
                    let extern_name = if is_result { "__result_unwrap" } else { "__option_unwrap" };
                    let dst = builder.call_extern(
                        extern_name,
                        vec![FunctionBuilder::copy(borrow)],
                        inner_type,
                    );
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // .is_some() / .is_none() / .is_ok() / .is_err() on Option/Result → tag check
    // On non-Option/Result types → pass-through (return false)
    if matches!(method_name, "is_some" | "is_none" | "is_ok" | "is_err") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| n.starts_with("Option") || n.starts_with("Result"))
            .unwrap_or(false);
        if !is_option_or_result {
            // Not an Option/Result — return false
            return Operand::Constant(Constant::Bool(false));
        }
        // Tag check: is_some/is_ok → tag == 0; is_none/is_err → tag != 0
        if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let ptr_type = ctx.register_ptr_type(
                infer_operand_type_full(ctx, &recv, builder),
            );
            let borrow = builder.add_local(ptr_type, None);
            builder.emit_borrow(borrow, place.clone());
            let extern_name = match method_name {
                "is_some" | "is_ok" => "__option_is_some",
                _ => "__option_is_none",
            };
            let dst = builder.call_extern(
                extern_name,
                vec![FunctionBuilder::copy(borrow)],
                BOOL_TYPE,
            );
            return FunctionBuilder::copy(dst);
        }
    }

    // Handle .len() for strings and collections
    if method_name == "len" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == ctx.type_mapper.str_type || recv_type == ctx.type_mapper.owned_string_type {
            // Str/GorgetString: .len() = codepoint count → call gorget_str_codepoint_count()
            let dst = builder.call_extern(
                "gorget_str_codepoint_count",
                vec![recv],
                I64_TYPE,
            );
            return FunctionBuilder::copy(dst);
        }
        // GorgetArray: .len is field 1 (element count, no function call needed)
        if let Some(GirType::Named(name)) = ctx.type_registry.get(recv_type) {
            if name.starts_with("GorgetArray") || name.starts_with("Vector__") {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let mut len_place = place.clone();
                    len_place.projections.push(Projection::Field(1));
                    let tmp = builder.add_local(I64_TYPE, None);
                    builder.assign(Place::local(tmp), Operand::Copy(len_place));
                    return FunctionBuilder::copy(tmp);
                }
            }
        }
    }
    // Handle .byte_len() for strings → direct field access
    if method_name == "byte_len" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == ctx.type_mapper.str_type || recv_type == ctx.type_mapper.owned_string_type {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                let mut len_place = place.clone();
                len_place.projections.push(Projection::Field(1));
                let tmp = builder.add_local(I64_TYPE, None);
                builder.assign(Place::local(tmp), Operand::Copy(len_place));
                return FunctionBuilder::copy(tmp);
            }
        }
    }

    // Determine the receiver type and mangle the method name
    let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);

    if let Some(type_name) = type_name {
        // Box[T].get() → call Box__T__get(b) passing value directly (not borrow)
        if type_name.starts_with("Box__") && method_name == "get" {
            let inner_type_name = &type_name["Box__".len()..];
            let inner_type = ctx.type_mapper.lookup_named(inner_type_name).unwrap_or(I64_TYPE);
            let mangled = format!("{type_name}__get");
            let dst = builder.call(mangled, vec![recv], inner_type);
            return FunctionBuilder::copy(dst);
        }

        // Box[T].set(val) → call Box__T__set(&b, val) passing borrow of box + value
        if type_name.starts_with("Box__") && method_name == "set" && !args.is_empty() {
            let val = lower_expr(ctx, builder, &args[0].node.value);
            let mangled = format!("{type_name}__set");
            // set takes (&box, val) — pass pointer to the box local
            let recv_place = match &recv {
                Operand::Copy(p) | Operand::Move(p) => p.clone(),
                _ => {
                    // If recv is not a place, store it in a temp
                    let box_type = ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE);
                    let tmp = builder.add_local(box_type, None);
                    builder.assign(Place::local(tmp), recv);
                    Place::local(tmp)
                }
            };
            let ptr_type = ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE);
            let recv_ref = builder.borrow_mut(recv_place, ptr_type);
            builder.call_void(mangled, vec![FunctionBuilder::copy(recv_ref), val]);
            return Operand::Constant(Constant::Unit);
        }

        // Iterator adapter expansion: fold/map/filter/collect on Iterator types
        if matches!(method_name, "fold" | "map" | "filter" | "collect") {
            if let Some(result) = try_lower_iterator_adapter(
                ctx, builder, &type_name, method_name, recv.clone(), args,
            ) {
                return result;
            }
        }

        let mangled = format!("{type_name}__{method_name}");

        // Build args: &receiver + explicit args
        let mut call_args = Vec::new();

        // Create a borrow of the receiver for the self parameter.
        // Mutating methods need a mutable borrow (&self → MutPtr).
        let is_mutating = matches!(method_name,
            "push" | "pop" | "put" | "remove" | "insert" | "extend"
            | "clear" | "sort" | "reverse" | "append" | "set"
            | "push_back" | "push_front" | "pop_back" | "pop_front"
            | "add" | "push_line" | "push_str" | "push_char"
        );

        // Determine if we need a mutable borrow (from explicit list or fn_sigs)
        let needs_mut = is_mutating || ctx.fn_sigs.get(&mangled)
            .and_then(|(params, _)| params.first())
            .map(|&p| matches!(ctx.type_registry.get(p), Some(GirType::MutPtr(_))))
            .unwrap_or(false);

        // If receiver is a field access, borrow the field in-place instead of
        // borrowing a copy (which would mutate the copy, not the original).
        if let Some((field_place, field_type_id)) = &field_place_info {
            if needs_mut {
                let pt = ctx.register_mut_ptr_type(*field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow_mut(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            } else {
                let pt = ctx.register_ptr_type(*field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            }
        } else if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let recv_type_id = if (place.local.0 as usize) < builder.locals.len() {
                builder.locals[place.local.0 as usize].type_id
            } else {
                UNIT_TYPE
            };
            // If receiver is already a pointer (e.g., self in equip methods), pass directly
            let is_ptr = matches!(
                ctx.type_registry.get(recv_type_id),
                Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
            );
            if is_ptr {
                call_args.push(FunctionBuilder::copy(place.local));
            } else if needs_mut {
                let pt = ctx.register_mut_ptr_type(recv_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow_mut(pl, place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            } else {
                let pt = ctx.register_ptr_type(recv_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow(pl, place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            }
        } else if matches!(&recv, Operand::Constant(Constant::Str(_))) {
            // String constant receiver: materialize into a local for borrow
            let str_type = ctx.type_mapper.str_type;
            let tmp_local = builder.add_local(str_type, None);
            builder.assign(Place::local(tmp_local), recv);
            let pt = ctx.register_ptr_type(str_type);
            let pl = builder.add_local(pt, None);
            builder.emit_borrow(pl, Place::local(tmp_local));
            call_args.push(FunctionBuilder::copy(pl));
        } else {
            call_args.push(recv);
        }

        for arg in args {
            call_args.push(lower_expr(ctx, builder, &arg.node.value));
        }

        // Resolve function name: try Type__method first, fallback to Trait_for_Type__method
        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
            mangled.clone()
        } else {
            let suffix = format!("_for_{type_name}__{method_name}");
            ctx.fn_sigs.keys()
                .find(|k| k.ends_with(&suffix))
                .cloned()
                .unwrap_or(mangled.clone())
        };

        // For Vector.get(), auto-register Option[T] and override return type
        let fn_sig_ret = ctx.fn_sigs.get(&effective_name).map(|(_, ret)| *ret);
        if method_name == "get"
            && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
        {
            let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let option_name = format!("Option__{elem_type_name}");
            if ctx.lookup_type_by_name(&option_name).is_none() {
                let inner_type = resolve_inner_type(ctx, elem_type_name);
                ctx.ensure_option_type_registered(&option_name, inner_type);
            }
        }
        // For index_of/find on strings/collections, register Option[int] return type
        if matches!(method_name, "index_of" | "find") {
            let option_name = "Option__int64_t";
            if ctx.lookup_type_by_name(option_name).is_none() {
                ctx.ensure_option_type_registered(option_name, I64_TYPE);
            }
        }
        let ret_type = if let Some(ret) = fn_sig_ret {
            // Vector.get() returns Option[T], not T — override fn_sigs
            if method_name == "get"
                && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
            {
                let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
                let option_name = format!("Option__{elem_type_name}");
                ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
            } else if matches!(method_name, "index_of" | "find") {
                ctx.lookup_type_by_name("Option__int64_t").unwrap_or(ret)
            } else {
                ret
            }
        } else {
            // Infer return type for known collection/runtime methods
            infer_collection_method_return_type(ctx, &type_name, method_name)
        };

        // Resolve extern bindings: use the C symbol name instead of the Gorget mangled name
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        if ret_type == UNIT_TYPE {
            builder.call_void(call_name, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(call_name, call_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    } else {
        // Can't determine receiver type — fallback
        Operand::Constant(Constant::Unit)
    }
}

/// Infer return type for known collection/runtime methods when fn_sigs has no entry.
fn infer_collection_method_return_type(
    ctx: &LoweringContext,
    type_name: &str,
    method_name: &str,
) -> TypeId {
    let is_vector = type_name.starts_with("Vector__") || type_name == "GorgetArray";
    let is_dict = type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") || type_name == "GorgetMap";
    let is_set = type_name.starts_with("Set__") || type_name == "GorgetSet";
    let is_string = type_name == "Str" || type_name == "GorgetString";

    match method_name {
        // Methods returning int
        "len" | "count" | "capacity" => I64_TYPE,
        // Methods returning Option[int]
        "index_of" | "find" => {
            ctx.lookup_type_by_name("Option__int64_t").unwrap_or(I64_TYPE)
        }
        // Methods returning bool
        "contains" | "is_empty" | "starts_with" | "ends_with"
        | "is_subset" | "is_superset" | "is_disjoint"
        | "has" | "contains_key" | "has_key" => {
            BOOL_TYPE
        }
        // Vector.get → Option[T] (bounds-checked safe access)
        "get" if is_vector => {
            let elem_type_name = type_name.strip_prefix("Vector__")
                .unwrap_or("int64_t");
            let option_name = format!("Option__{elem_type_name}");
            ctx.lookup_type_by_name(&option_name).unwrap_or(I64_TYPE)
        }
        // Dict/HashMap.get / get_or / get_or_put → value type (I64_TYPE as default)
        "get" | "get_or" | "get_or_put" if is_dict => I64_TYPE,
        // Vector.pop → element type
        "pop" if is_vector => I64_TYPE,
        // Vector.clone / .sorted / .slice → same collection type
        "clone" | "sorted" | "slice" if is_vector => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                UNIT_TYPE
            }
        }
        // Set operations → same set type
        "union" | "intersection" | "difference" | "symmetric_difference" if is_set => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                UNIT_TYPE
            }
        }
        // String methods returning Str (view operations)
        "trim" | "strip" | "lstrip" | "rstrip" | "removeprefix" | "removesuffix"
        | "byte_slice" | "substring" | "char_at" if is_string => {
            ctx.type_mapper.str_type
        }
        // String methods returning GorgetString (allocating)
        "to_upper" | "to_lower" | "replace" | "repeat" | "pad_left" | "pad_right"
        | "join" if is_string => {
            ctx.type_mapper.owned_string_type
        }
        // String .str() / .as_str() → Str
        "str" | "as_str" if is_string => ctx.type_mapper.str_type,
        // String .split() / .chars() → Vector__Str
        "split" | "chars" if is_string => {
            ctx.lookup_type_by_name("Vector__Str")
                .unwrap_or_else(|| ctx.lookup_type_by_name("GorgetArray").unwrap_or(UNIT_TYPE))
        }
        // String .bytes() → Vector__uint8_t
        "bytes" if is_string => {
            ctx.lookup_type_by_name("Vector__uint8_t")
                .unwrap_or_else(|| ctx.lookup_type_by_name("GorgetArray").unwrap_or(UNIT_TYPE))
        }
        // String .codepoints() → Vector__int64_t
        "codepoints" if is_string => {
            ctx.lookup_type_by_name("Vector__int64_t")
                .unwrap_or_else(|| ctx.lookup_type_by_name("GorgetArray").unwrap_or(UNIT_TYPE))
        }
        // Dict/HashMap .keys() / .values() / .items() → GorgetArray
        "keys" | "values" | "items" if is_dict => {
            if let Some(type_id) = ctx.lookup_type_by_name("GorgetArray") {
                type_id
            } else {
                UNIT_TYPE
            }
        }
        // Higher-order methods returning new collections
        "filter" | "map" | "enumerate" | "flat_map" | "zip" if is_vector => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                // Fallback to a generic GorgetArray type
                ctx.lookup_type_by_name("GorgetArray").unwrap_or(I64_TYPE)
            }
        }
        "filter" | "map" if is_dict => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                // Dict/HashMap filter returns a map, not an array
                ctx.type_mapper.named_types.get("GorgetMap")
                    .or_else(|| ctx.type_mapper.named_types.get("GorgetDict"))
                    .copied()
                    .unwrap_or(I64_TYPE)
            }
        }
        "filter" | "map" if is_set => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                ctx.lookup_type_by_name("GorgetSet")
                    .or_else(|| ctx.lookup_type_by_name("GorgetArray"))
                    .unwrap_or(I64_TYPE)
            }
        }
        // fold/reduce return a scalar value
        "fold" | "reduce" | "any" | "all" if is_vector || is_set || is_dict => I64_TYPE,
        // forEach → void
        "for_each" | "each" if is_vector || is_set || is_dict => UNIT_TYPE,
        // reversed/unique/flatten → same collection
        "reversed" | "unique" | "flatten" if is_vector => {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                ctx.lookup_type_by_name("GorgetArray").unwrap_or(I64_TYPE)
            }
        }
        // Vector.remove → element type (for Option wrapping, unwrap is pass-through on non-Option)
        "remove" if is_vector => I64_TYPE,
        // Set.remove → bool
        "remove" if is_set => BOOL_TYPE,
        // Dict/HashMap.remove → bool
        "remove" if is_dict => BOOL_TYPE,
        // Set.add → void (was incorrectly handled)
        "add" if is_set => UNIT_TYPE,
        // Option/Result combinator methods that return the same Option/Result type
        "map" | "and_then" | "or_else" | "or" | "flatten" | "filter"
            if type_name.starts_with("Option__") || type_name.starts_with("Result__") =>
        {
            // map/and_then/or/or_else/flatten return the same Option/Result type
            // (map may change inner type, but without full type inference we return self type)
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                UNIT_TYPE
            }
        }
        "map_err" if type_name.starts_with("Result__") => {
            // map_err returns the same Result type (error type may change, but same structural type)
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                type_id
            } else {
                UNIT_TYPE
            }
        }
        "unwrap_or_else" if type_name.starts_with("Option__") => {
            // unwrap_or_else returns the inner type (T from Option[T])
            let inner_name = &type_name["Option__".len()..];
            ctx.lookup_type_by_name(inner_name)
                .or_else(|| ctx.type_mapper.lookup_named(inner_name))
                .unwrap_or(I64_TYPE)
        }
        "unwrap_or_else" if type_name.starts_with("Result__") => {
            // unwrap_or_else returns the Ok type (T from Result[T, E])
            let rest = &type_name["Result__".len()..];
            let ok_name = ["__Str", "__int64_t", "__bool", "__double"].iter()
                .find_map(|suffix| rest.strip_suffix(suffix))
                .unwrap_or_else(|| {
                    rest.rfind("__").map(|pos| &rest[..pos]).unwrap_or(rest)
                });
            ctx.lookup_type_by_name(ok_name)
                .or_else(|| ctx.type_mapper.lookup_named(ok_name))
                .unwrap_or(I64_TYPE)
        }
        "unwrap_err" if type_name.starts_with("Result__") => {
            // unwrap_err returns the Err type (E from Result[T, E])
            let rest = &type_name["Result__".len()..];
            let parts: Vec<&str> = rest.splitn(2, "__").collect();
            if parts.len() > 1 {
                ctx.lookup_type_by_name(parts[1])
                    .or_else(|| ctx.type_mapper.lookup_named(parts[1]))
                    .unwrap_or(I64_TYPE)
            } else {
                I64_TYPE
            }
        }
        _ => UNIT_TYPE,
    }
}

// ---- Iterator Adapter Inline Expansion ----

/// Try to expand fold/map/filter/collect as inline GIR loops for Iterator types.
/// Returns Some(result) if the receiver type has a __next method (implements Iterator),
/// None otherwise (falls through to regular method dispatch).
fn try_lower_iterator_adapter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_name: &str,
    method_name: &str,
    recv: Operand,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    // Find __next method for this type (same lookup as lower_for_iterable)
    let next_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__next") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__next");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        })?;

    // Get Option return type from next()
    let (_, option_ret_type) = ctx.fn_sigs.get(&next_fn_name)?.clone();

    // Extract element type from Option__T
    let option_type_name = ctx.type_registry.type_name(option_ret_type)
        .unwrap_or_else(|| "Option__int64_t".to_string());
    let elem_c_type = option_type_name.strip_prefix("Option__")
        .unwrap_or("int64_t");
    let elem_type = ctx.type_mapper.lookup_named(elem_c_type).unwrap_or(I64_TYPE);

    // Store the iterator for repeated borrow_mut
    let iter_type = infer_operand_type_full(ctx, &recv, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), recv);

    match method_name {
        "fold" => lower_iter_fold(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "map" => lower_iter_map(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "filter" => lower_iter_filter(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "collect" => lower_iter_collect(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type),
        _ => None,
    }
}

/// Build the common iterator loop pattern: call next(), check Option tag, branch.
/// Returns (header_bb, exit_bb, elem_local) with builder positioned at the body block.
fn build_iter_next_loop(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
) -> (BlockId, BlockId, LocalId) {
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();
    builder.jump(header_bb);

    // Header: call next(&mut iter) → Option
    builder.switch_to(header_bb);
    let iter_ptr_type = ctx.register_mut_ptr_type(iter_type);
    let iter_ref = builder.borrow_mut(Place::local(iter_local), iter_ptr_type);
    let opt_result = builder.call_extern(
        next_fn_name,
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
    builder.branch(FunctionBuilder::copy(is_none), exit_bb, body_bb);

    // Body: extract element from Some._0
    builder.switch_to(body_bb);
    let elem_local = builder.enum_field_load(
        Place::local(opt_result),
        "Some",
        0,
        elem_type,
    );

    (header_bb, exit_bb, elem_local)
}

/// Call a closure operand with the given arguments.
/// Handles: known closure struct types, callable parameters, and function refs.
fn call_closure_in_adapter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    closure_op: &Operand,
    call_args: Vec<Operand>,
    fallback_ret_type: TypeId,
) -> Operand {
    if let Operand::Copy(place) | Operand::Move(place) = closure_op {
        let local_idx = place.local.0 as usize;
        let local_type_id = if local_idx < builder.locals.len() {
            builder.locals[local_idx].type_id
        } else {
            UNIT_TYPE
        };
        if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
            let type_name = type_name.to_string();
            if let Some((call_fn, _)) = ctx.lookup_closure_info(&type_name) {
                let call_fn = call_fn.to_string();
                // Closure call: __Closure_N__call(&closure, args...)
                let ptr_type = ctx.type_registry.insert(GirType::Ptr(local_type_id));
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow(ptr_local, place.clone());
                let mut final_args = vec![FunctionBuilder::copy(ptr_local)];
                final_args.extend(call_args);

                let ret_type = ctx.fn_sigs.get(call_fn.as_str())
                    .map(|(_, ret)| *ret)
                    .unwrap_or(fallback_ret_type);

                if ret_type == UNIT_TYPE {
                    builder.call_void(&call_fn, final_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = builder.call(&call_fn, final_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
        }
        // Fallback: __callable_N for callable parameters
        let callable_name = format!("__callable_{}", place.local.0);
        let mut final_args = vec![closure_op.clone()];
        final_args.extend(call_args);
        let dst = builder.call(callable_name, final_args, fallback_ret_type);
        return FunctionBuilder::copy(dst);
    }
    if let Operand::Constant(Constant::FuncRef(name)) = closure_op {
        let ret_type = ctx.fn_sigs.get(name.as_str())
            .map(|(_, ret)| *ret)
            .unwrap_or(fallback_ret_type);
        let dst = builder.call(name.clone(), call_args, ret_type);
        return FunctionBuilder::copy(dst);
    }
    Operand::Constant(Constant::Unit)
}

/// Inline expansion of iter.fold(initial, closure)
fn lower_iter_fold(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.len() < 2 { return None; }

    let initial = lower_expr(ctx, builder, &args[0].node.value);
    let closure_op = lower_expr(ctx, builder, &args[1].node.value);

    let acc_type = infer_operand_type_full(ctx, &initial, builder);
    let acc_local = builder.add_local(acc_type, None);
    builder.assign(Place::local(acc_local), initial);

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: acc = closure(acc, elem)
    let new_acc = call_closure_in_adapter(
        ctx, builder, &closure_op,
        vec![FunctionBuilder::copy(acc_local), FunctionBuilder::copy(elem_local)],
        acc_type,
    );
    builder.assign(Place::local(acc_local), new_acc);
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(acc_local))
}

/// Inline expansion of iter.map(closure) → GorgetArray
fn lower_iter_map(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.is_empty() { return None; }

    let closure_op = lower_expr(ctx, builder, &args[0].node.value);

    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: mapped = closure(elem), push to result
    let mapped = call_closure_in_adapter(
        ctx, builder, &closure_op,
        vec![FunctionBuilder::copy(elem_local)],
        elem_type,
    );
    let mapped_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(mapped_local), mapped);

    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(mapped_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Inline expansion of iter.filter(predicate) → GorgetArray
fn lower_iter_filter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.is_empty() { return None; }

    let predicate_op = lower_expr(ctx, builder, &args[0].node.value);

    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: if predicate(elem), push to result
    let keep = call_closure_in_adapter(
        ctx, builder, &predicate_op,
        vec![FunctionBuilder::copy(elem_local)],
        BOOL_TYPE,
    );
    let push_bb = builder.new_block();
    builder.branch(keep, push_bb, header_bb);

    builder.switch_to(push_bb);
    let el_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(el_local), FunctionBuilder::copy(elem_local));
    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(el_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Inline expansion of iter.collect() → GorgetArray
fn lower_iter_collect(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
) -> Option<Operand> {
    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: push elem to result
    let el_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(el_local), FunctionBuilder::copy(elem_local));
    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(el_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Lower an index access expression.
fn lower_index_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        // Infer element type from the base collection type
        let base_type = infer_operand_type_full(ctx, &obj, builder);

        // Check if the type has a get() equip method (Index trait / operator overload)
        // Skip for built-in collection types — use direct index_load instead
        // (Vector.get() returns Option[T] but v[i] returns T directly)
        if let Some(type_name) = infer_type_name_from_operand_full(ctx, &obj, builder) {
            let is_builtin_collection = type_name.starts_with("Vector__")
                || type_name == "GorgetArray"
                || type_name.starts_with("Dict__")
                || type_name.starts_with("HashMap__")
                || type_name.starts_with("Set__");
            if !is_builtin_collection {
                // Try Type__get (from equip Type with Index) or __getitem__
                let candidates = [
                    format!("{type_name}__get"),
                    format!("Index_for_{type_name}__get"),
                    format!("{type_name}____getitem__"),
                ];
                for get_name in &candidates {
                    if ctx.fn_sigs.contains_key(get_name.as_str()) {
                        let ret_type = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(_, ret)| *ret)
                            .unwrap_or(I64_TYPE);
                        // Only dispatch if the first param is a pointer to our type (it's a method)
                        let is_method = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(params, _)| {
                                params.first().map(|&p| {
                                    matches!(ctx.type_registry.get(p), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)))
                                }).unwrap_or(false)
                            })
                            .unwrap_or(false);
                        if !is_method { continue; }
                        let pt = ctx.register_ptr_type(base_type);
                        let pl = builder.add_local(pt, None);
                        builder.emit_borrow(pl, place.clone());
                        let dst = builder.call(get_name.clone(), vec![FunctionBuilder::copy(pl), idx], ret_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
        }

        let elem_type = if base_type == ctx.type_mapper.str_type
            || base_type == ctx.type_mapper.owned_string_type
        {
            ctx.type_mapper.str_type // indexing a string returns Str
        } else {
            // Try to infer element type from collection type name
            infer_collection_element_type(ctx, base_type)
        };
        let dst = builder.index_load(place.clone(), idx, elem_type);
        return FunctionBuilder::copy(dst);
    }

    Operand::Constant(Constant::Unit)
}

/// Infer the element type of a collection from its TypeId.
/// Returns the element TypeId, or I64_TYPE if unknown.
pub(super) fn infer_collection_element_type(ctx: &LoweringContext, collection_type: TypeId) -> TypeId {
    if let Some(GirType::Named(name)) = ctx.type_registry.get(collection_type) {
        // Vector__T → look up T as a type
        if let Some(elem_name) = name.strip_prefix("Vector__") {
            return resolve_type_name_to_id(ctx, elem_name);
        }
        // Dict__K__V → V is the value type (for indexing)
        if let Some(rest) = name.strip_prefix("Dict__").or_else(|| name.strip_prefix("Map__")) {
            if let Some(pos) = rest.find("__") {
                let val_name = &rest[pos + 2..];
                return resolve_type_name_to_id(ctx, val_name);
            }
        }
    }
    I64_TYPE
}

/// Resolve a mangled C type name back to a TypeId.
fn resolve_type_name_to_id(ctx: &LoweringContext, name: &str) -> TypeId {
    match name {
        "int64_t" => I64_TYPE,
        "int32_t" => I32_TYPE,
        "int16_t" => I16_TYPE,
        "int8_t" => I8_TYPE,
        "uint64_t" => U64_TYPE,
        "uint32_t" => U32_TYPE,
        "uint16_t" => U16_TYPE,
        "uint8_t" => U8_TYPE,
        "double" => F64_TYPE,
        "float" => F32_TYPE,
        "bool" => BOOL_TYPE,
        "Str" => ctx.type_mapper.str_type,
        "GorgetString" => ctx.type_mapper.owned_string_type,
        _ => {
            // Try named type lookup
            if let Some(tid) = ctx.type_mapper.lookup_named(name) {
                return tid;
            }
            // Insert as a new Named type
            I64_TYPE
        }
    }
}

/// Lower an if expression (used as ternary).
fn lower_if_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_branch: &Spanned<Expr>,
    else_branch: Option<&Spanned<Expr>>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);

    // Allocate a result local
    let result_type = infer_operand_type_full(ctx, &cond, builder);
    let _ = result_type;
    let result_id = builder.add_local(I64_TYPE, None); // placeholder type

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(cond, then_bb, else_bb);

    builder.switch_to(then_bb);
    let then_val = lower_expr(ctx, builder, then_branch);
    builder.assign(Place::local(result_id), then_val);
    builder.jump(merge_bb);

    builder.switch_to(else_bb);
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

/// Infer the type name of an operand (for method dispatch).
fn infer_type_name_from_operand_full(
    ctx: &LoweringContext,
    operand: &Operand,
    builder: &FunctionBuilder,
) -> Option<String> {
    let type_id = match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals (named variables)
            let mut tid = None;
            for (_, (lid, local_tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    tid = Some(*local_tid);
                    break;
                }
            }
            // Fall back to builder locals (temporaries)
            if tid.is_none() {
                let idx = place.local.0 as usize;
                if idx < builder.locals.len() {
                    tid = Some(builder.locals[idx].type_id);
                }
            }
            tid?
        }
        Operand::Constant(c) => match c {
            Constant::Char(_) => return Some("char".to_string()),
            Constant::Str(_) => return Some("Str".to_string()),
            Constant::Bool(_) => return Some("bool".to_string()),
            Constant::I64(_) => return Some("int64_t".to_string()),
            Constant::F64(_) => return Some("double".to_string()),
            _ => return None,
        },
    };

    // Resolve through pointer types
    let effective_tid = ctx.pointee_type(type_id).unwrap_or(type_id);

    // Check primitive types
    if effective_tid == ctx.type_mapper.str_type {
        return Some("Str".to_string());
    }
    if effective_tid == ctx.type_mapper.owned_string_type {
        return Some("GorgetString".to_string());
    }
    if effective_tid == CHAR_TYPE {
        return Some("char".to_string());
    }

    // Check named types (match both the original type_id and the dereferenced effective_tid,
    // since opaque pointer types like PoolAllocator are registered as Ptr(Named(...)))
    ctx.type_mapper.named_types.iter()
        .find_map(|(name, &id)| if id == effective_tid || id == type_id { Some(name.clone()) } else { None })
}

/// Resolve the inner TypeId from a type name (e.g., "int64_t" → I64_TYPE).
fn resolve_inner_type(ctx: &mut LoweringContext, inner_name: &str) -> TypeId {
    match inner_name {
        "int64_t" => I64_TYPE,
        "int32_t" => I32_TYPE,
        "double" => F64_TYPE,
        "float" => F32_TYPE,
        "bool" => BOOL_TYPE,
        "uint8_t" => U8_TYPE,
        "uint16_t" => U16_TYPE,
        "uint32_t" => U32_TYPE,
        "uint64_t" => U64_TYPE,
        "int8_t" => I8_TYPE,
        "int16_t" => I16_TYPE,
        "Str" => ctx.type_mapper.str_type,
        "GorgetString" => ctx.type_mapper.owned_string_type,
        name => {
            if let Some(id) = ctx.type_mapper.lookup_named(name) {
                return id;
            }
            // Collection types (Vector__X, Dict__X__Y, etc.) might not be registered yet —
            // register them on-the-fly as Named types so the C backend can emit the right typedef.
            if name.starts_with("Vector__") || name.starts_with("Dict__")
                || name.starts_with("HashMap__") || name.starts_with("Set__")
                || name.starts_with("HashSet__")
            {
                let type_id = ctx.type_registry.insert(GirType::Named(name.to_string()));
                ctx.type_mapper.register_named(name.to_string(), type_id);
                return type_id;
            }
            I64_TYPE
        }
    }
}

/// Resolve the tag value for None in an enum type.
/// For Option[T], None is conventionally the last variant (tag = num_variants - 1).
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

/// Lower a binary operation.
fn lower_binary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<Expr>,
    op: ast::BinaryOp,
    right: &Spanned<Expr>,
) -> Operand {
    use ast::BinaryOp as AstOp;

    // Short-circuit: `and` / `or`
    if op == AstOp::And || op == AstOp::Or {
        return lower_short_circuit(ctx, builder, left, op, right);
    }

    let lhs = lower_expr(ctx, builder, left);
    let rhs = lower_expr(ctx, builder, right);

    // Determine result type from lhs operand type (use _full to check builder temps too)
    let operand_type = infer_operand_type_full(ctx, &lhs, builder);
    let is_string = operand_type == ctx.type_mapper.str_type
        || operand_type == ctx.type_mapper.owned_string_type;

    match op {
        // Comparison operators → bool result
        AstOp::Eq | AstOp::Neq | AstOp::Lt | AstOp::Gt | AstOp::LtEq | AstOp::GtEq => {
            // Option/enum == None: compare tag instead of struct == NULL
            if matches!(op, AstOp::Eq | AstOp::Neq) {
                let (enum_operand, is_none) = match (&lhs, &rhs) {
                    (_, Operand::Constant(Constant::Null)) => (Some(&lhs), true),
                    (Operand::Constant(Constant::Null), _) => (Some(&rhs), true),
                    _ => (None, false),
                };
                if is_none {
                    if let Some(enum_op) = enum_operand {
                        // Get the tag of the enum value
                        let tag = builder.tag_of(enum_op.clone());
                        // None is conventionally the last variant (tag = num_variants - 1)
                        // For Option, None = tag 1; for most enums, last variant
                        // Look up variant count from type registry
                        let none_tag = resolve_none_tag(ctx, operand_type);
                        let cmp_op = if op == AstOp::Eq { CmpOp::Eq } else { CmpOp::Ne };
                        let dst = builder.cmp(
                            cmp_op,
                            I32_TYPE,
                            FunctionBuilder::copy(tag),
                            Operand::Constant(Constant::I32(none_tag)),
                        );
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            // String equality: use gorget_str_eq instead of pointer comparison
            if is_string && matches!(op, AstOp::Eq | AstOp::Neq) {
                let str_type = ctx.type_mapper.str_type;
                let dst = builder.call_extern("gorget_str_eq", vec![lhs, rhs], BOOL_TYPE);
                let _ = str_type;
                if op == AstOp::Neq {
                    let neg = builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(dst));
                    return FunctionBuilder::copy(neg);
                }
                return FunctionBuilder::copy(dst);
            }

            // Struct == / != : dispatch to Type__eq() if available
            if matches!(op, AstOp::Eq | AstOp::Neq) {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
                    let eq_method = format!("{type_name}__eq");
                    if ctx.fn_sigs.contains_key(&eq_method) {
                        // Borrow lhs for self parameter
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs
                        };
                        let dst = builder.call(
                            eq_method,
                            vec![self_ptr, rhs],
                            BOOL_TYPE,
                        );
                        if op == AstOp::Neq {
                            let neg = builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(dst));
                            return FunctionBuilder::copy(neg);
                        }
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            // Comparable trait dispatch: Type__compare(self, other) → int, then compare with 0
            if matches!(op, AstOp::Lt | AstOp::Gt | AstOp::LtEq | AstOp::GtEq) {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
                    let compare_method = format!("{type_name}__compare");
                    let has_compare = ctx.fn_sigs.contains_key(&compare_method)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__compare")));
                    if has_compare {
                        let effective_name = if ctx.fn_sigs.contains_key(&compare_method) {
                            compare_method
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__compare")))
                                .cloned()
                                .unwrap_or(compare_method)
                        };
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs.clone()
                        };
                        let cmp_result = builder.call(effective_name, vec![self_ptr, rhs], I64_TYPE);
                        let cmp_op = match op {
                            AstOp::Lt => CmpOp::Lt,
                            AstOp::Gt => CmpOp::Gt,
                            AstOp::LtEq => CmpOp::Le,
                            AstOp::GtEq => CmpOp::Ge,
                            _ => unreachable!(),
                        };
                        let dst = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(cmp_result), Operand::Constant(Constant::I64(0)));
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            let cmp_op = match op {
                AstOp::Eq => CmpOp::Eq,
                AstOp::Neq => CmpOp::Ne,
                AstOp::Lt => CmpOp::Lt,
                AstOp::Gt => CmpOp::Gt,
                AstOp::LtEq => CmpOp::Le,
                AstOp::GtEq => CmpOp::Ge,
                _ => unreachable!(),
            };
            let dst = builder.cmp(cmp_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }

        // String concatenation: use gorget_str_cat (returns GorgetString, not Str)
        AstOp::Add if is_string => {
            let owned_type = ctx.type_mapper.owned_string_type;
            let dst = builder.call_extern("gorget_str_cat", vec![lhs, rhs], owned_type);
            FunctionBuilder::copy(dst)
        }

        // `in` operator → contains check
        AstOp::In => {
            // Determine collection type to use the right contains function
            let rhs_type = infer_operand_type_full(ctx, &rhs, builder);
            let rhs_type_name = ctx.type_name_for_id(rhs_type)
                .map(|s| s.to_string())
                .unwrap_or_default();
            let is_map = rhs_type_name.starts_with("Dict__") || rhs_type_name.starts_with("HashMap__");
            let is_set = rhs_type_name.starts_with("Set__") || rhs_type_name.starts_with("HashSet__");
            let is_string = rhs_type == ctx.type_mapper.str_type || rhs_type == ctx.type_mapper.owned_string_type;
            if is_map || is_set {
                // Map/Set contains: need pointer to collection and pointer to element
                let fn_name = if is_map { "gorget_map_contains" } else { "gorget_set_contains" };
                let coll_ptr_type = ctx.register_ptr_type(rhs_type);
                let coll_ptr = builder.add_local(coll_ptr_type, None);
                let rhs_local = match &rhs {
                    Operand::Copy(p) | Operand::Move(p) => p.local,
                    _ => {
                        let tmp = builder.add_local(rhs_type, None);
                        builder.assign(Place::local(tmp), rhs);
                        tmp
                    }
                };
                builder.emit_borrow(coll_ptr, Place::local(rhs_local));
                // Element also needs to be a pointer
                let lhs_type = infer_operand_type_full(ctx, &lhs, builder);
                let elem_local = builder.add_local(lhs_type, None);
                builder.assign(Place::local(elem_local), lhs);
                let elem_ptr_type = ctx.register_ptr_type(lhs_type);
                let elem_ptr = builder.add_local(elem_ptr_type, None);
                builder.emit_borrow(elem_ptr, Place::local(elem_local));
                let dst = builder.call_extern(fn_name, vec![FunctionBuilder::copy(coll_ptr), FunctionBuilder::copy(elem_ptr)], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            } else if is_string {
                let dst = builder.call_extern("gorget_str_contains", vec![rhs, lhs], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            } else {
                let dst = builder.call_extern("gorget_array_contains", vec![rhs, lhs], BOOL_TYPE);
                FunctionBuilder::copy(dst)
            }
        }

        // Arithmetic operators → check for operator overloading, then fall back to primitives
        _ => {
            // Check for operator overload method on Named types
            let op_method_name = match op {
                AstOp::Add => Some("add"),
                AstOp::Sub => Some("sub"),
                AstOp::Mul => Some("mul"),
                AstOp::Div => Some("div"),
                AstOp::Mod => Some("mod"),
                _ => None,
            };
            if let Some(method) = op_method_name {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
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
                        // Borrow lhs for self parameter
                        let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = lhs {
                            let ptr_type = ctx.register_ptr_type(operand_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            FunctionBuilder::copy(ptr_local)
                        } else {
                            lhs.clone()
                        };
                        let dst = builder.call(effective_name, vec![self_ptr, rhs], operand_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }

            let bin_op = match op {
                AstOp::Add => BinOp::Add,
                AstOp::Sub => BinOp::Sub,
                AstOp::Mul => BinOp::Mul,
                AstOp::Div => BinOp::Div,
                AstOp::Mod => BinOp::Rem,
                AstOp::BitAnd => BinOp::BitAnd,
                AstOp::BitOr => BinOp::BitOr,
                AstOp::BitXor => BinOp::BitXor,
                AstOp::Shl => BinOp::Shl,
                AstOp::Shr => BinOp::Shr,
                AstOp::AddWrap => BinOp::AddWrap,
                AstOp::SubWrap => BinOp::SubWrap,
                AstOp::MulWrap => BinOp::MulWrap,
                _ => BinOp::Add, // fallback
            };
            let dst = builder.bin_op(bin_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Lower short-circuit `and`/`or` via basic block branching.
fn lower_short_circuit(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<Expr>,
    op: ast::BinaryOp,
    right: &Spanned<Expr>,
) -> Operand {
    let lhs = lower_expr(ctx, builder, left);

    // Allocate a result local
    let result_id = builder.add_local(BOOL_TYPE, None);

    let rhs_bb = builder.new_block();
    let merge_bb = builder.new_block();

    match op {
        ast::BinaryOp::And => {
            let false_bb = builder.new_block();
            builder.branch(lhs, rhs_bb, false_bb);

            // false_bb: assign false, jump merge
            builder.switch_to(false_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            // If lhs was an `Is` expression, emit pattern bindings now (in the
            // true-branch, before the guard/rhs is evaluated).
            super::stmts::emit_is_bindings(ctx, builder, left);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        ast::BinaryOp::Or => {
            let true_bb = builder.new_block();
            builder.branch(lhs, true_bb, rhs_bb);

            // true_bb: assign true, jump merge
            builder.switch_to(true_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        _ => unreachable!(),
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

/// Lower a unary operation.
fn lower_unary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    op: ast::UnaryOp,
    operand: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, operand);
    let operand_type = infer_operand_type_full(ctx, &val, builder);

    // Check for unary operator overload (e.g., `-v` → Vec2__neg)
    if matches!(op, ast::UnaryOp::Neg) {
        if let Some(GirType::Named(type_name)) = ctx.type_registry.get(operand_type).cloned() {
            let mangled = format!("{type_name}__neg");
            let has_method = ctx.fn_sigs.contains_key(&mangled)
                || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__neg")));
            if has_method {
                let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                    mangled
                } else {
                    ctx.fn_sigs.keys()
                        .find(|k| k.ends_with(&format!("_for_{type_name}__neg")))
                        .cloned()
                        .unwrap_or(mangled)
                };
                let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                    let ptr_type = ctx.register_ptr_type(operand_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, place.clone());
                    FunctionBuilder::copy(ptr_local)
                } else {
                    val.clone()
                };
                let dst = builder.call(effective_name, vec![self_ptr], operand_type);
                return FunctionBuilder::copy(dst);
            }
        }
    }

    let gir_op = match op {
        ast::UnaryOp::Neg => UnOp::Neg,
        ast::UnaryOp::Not => UnOp::Not,
        ast::UnaryOp::BitNot => UnOp::BitNot,
        ast::UnaryOp::Deref => return val, // Phase 1: no pointer derefs
    };

    let result_type = if gir_op == UnOp::Not { BOOL_TYPE } else { operand_type };
    let dst = builder.un_op(gir_op, result_type, val);
    FunctionBuilder::copy(dst)
}

/// Lower a call argument, respecting ownership (MutableBorrow creates a BorrowMut).
fn lower_call_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    arg: &Spanned<ast::CallArg>,
) -> Operand {
    let val = lower_expr(ctx, builder, &arg.node.value);
    match arg.node.ownership {
        Ownership::MutableBorrow => {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.locals[place.local.0 as usize].type_id
                } else {
                    UNIT_TYPE
                };
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }
        _ => val,
    }
}

/// Resolve named arguments and default parameter values for a function call.
/// Reorders named args to match parameter order and fills in defaults for missing params.
fn resolve_call_args<'a>(
    ctx: &LoweringContext,
    fn_name: &str,
    args: &'a [Spanned<ast::CallArg>],
) -> Vec<Spanned<ast::CallArg>> {
    let param_names = match ctx.fn_param_names.get(fn_name) {
        Some(names) => names,
        None => return args.to_vec(), // no param info → pass through unchanged
    };

    let has_named = args.iter().any(|a| a.node.name.is_some());
    let has_defaults = ctx.fn_defaults.contains_key(fn_name);

    if !has_named && !has_defaults {
        return args.to_vec();
    }
    if !has_named && args.len() >= param_names.len() {
        return args.to_vec(); // all params supplied positionally, no reorder needed
    }

    // Build a slot array matching parameter order
    let mut slots: Vec<Option<Spanned<ast::CallArg>>> = vec![None; param_names.len()];

    // Place positional args first
    let mut positional_idx = 0;
    for arg in args {
        if arg.node.name.is_some() {
            // Named arg — place by name
            let arg_name = arg.node.name.as_ref().unwrap().node.as_str();
            if let Some(pos) = param_names.iter().position(|p| p == arg_name) {
                slots[pos] = Some(arg.clone());
            }
        } else {
            // Positional — skip past already-filled slots from named args
            while positional_idx < slots.len() && slots[positional_idx].is_some() {
                positional_idx += 1;
            }
            if positional_idx < slots.len() {
                slots[positional_idx] = Some(arg.clone());
                positional_idx += 1;
            }
        }
    }

    // Fill in defaults for any remaining empty slots
    if let Some(defaults) = ctx.fn_defaults.get(fn_name) {
        for (param_idx, default_expr) in defaults {
            if *param_idx < slots.len() && slots[*param_idx].is_none() {
                slots[*param_idx] = Some(Spanned::dummy(ast::CallArg {
                    name: None,
                    ownership: ast::Ownership::Borrow,
                    value: Spanned::dummy(default_expr.clone()),
                }));
            }
        }
    }

    // Collect filled slots (skip any remaining None — shouldn't happen for valid code)
    slots.into_iter().flatten().collect()
}

/// Lower a function call.
fn lower_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    callee: &Spanned<Expr>,
    args: &[Spanned<ast::CallArg>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    if let Expr::Identifier(name) = &callee.node {
        if name == "print" {
            lower_print_call(ctx, builder, args);
            return Operand::Constant(Constant::Unit);
        }

        // chr(n) → cast int to char (uint32_t)
        if name == "chr" && args.len() == 1 {
            let arg = lower_expr(ctx, builder, &args[0].node.value);
            let dst = builder.cast(CHAR_TYPE, arg);
            return FunctionBuilder::copy(dst);
        }

        // ord(c) → cast char to int
        if name == "ord" && args.len() == 1 {
            let arg = lower_expr(ctx, builder, &args[0].node.value);
            let dst = builder.cast(I64_TYPE, arg);
            return FunctionBuilder::copy(dst);
        }

        // Box(value) constructor → heap allocation via __gorget_box_alloc
        if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let val_type = infer_operand_type_full(ctx, &val_op, builder);
            let inner_c = if let Some(rest) = name.strip_prefix("Box__") {
                rest.to_string()
            } else {
                ctx.type_name_for_id(val_type)
                    .unwrap_or("int64_t")
                    .to_string()
            };
            let box_mangled = format!("Box__{inner_c}");
            let box_type = ctx.type_mapper.lookup_named(&box_mangled).unwrap_or(I64_TYPE);
            let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
            let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
            return FunctionBuilder::copy(dst);
        }

        // String("hello") constructor → gorget_string_from_str
        // String(capacity) constructor → gorget_string_with_capacity
        if name == "String" {
            if args.len() == 1 {
                let arg_op = lower_expr(ctx, builder, &args[0].node.value);
                let owned_type = ctx.type_mapper.owned_string_type;
                let arg_type = infer_operand_type_full(ctx, &arg_op, builder);
                let fn_name = if arg_type == I64_TYPE || arg_type == I32_TYPE {
                    "gorget_string_with_capacity"
                } else {
                    "gorget_string_from_str"
                };
                let dst = builder.call_extern(fn_name, vec![arg_op], owned_type);
                return FunctionBuilder::copy(dst);
            } else if args.is_empty() {
                let owned_type = ctx.type_mapper.owned_string_type;
                let dst = builder.call_extern(
                    "gorget_string_from_str",
                    vec![Operand::Constant(Constant::Str(String::new()))],
                    owned_type,
                );
                return FunctionBuilder::copy(dst);
            }
        }

        // format("...") → string interpolation or gorget_string_from_str
        if name == "format" && args.len() == 1 {
            if let Expr::StringLiteral(lit) = &args[0].node.value.node {
                if lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_))) {
                    return lower_string_interpolation(ctx, builder, lit);
                } else {
                    // Plain string literal → gorget_string_from_str(str_literal)
                    let str_op = lower_expr(ctx, builder, &args[0].node.value);
                    let owned_type = ctx.type_mapper.owned_string_type;
                    let dst = builder.call_extern("gorget_string_from_str", vec![str_op], owned_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Allocator constructors → runtime functions
        if name == "Arena" && args.len() == 1 {
            let cap_op = lower_expr(ctx, builder, &args[0].node.value);
            let arena_type = ctx.type_mapper.lookup_named("Arena").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_arena_new", vec![cap_op], arena_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "TrackingAllocator" && args.is_empty() {
            let ta_type = ctx.type_mapper.lookup_named("TrackingAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_tracking_new", vec![], ta_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "PoolAllocator" && args.len() == 2 {
            let a1 = lower_expr(ctx, builder, &args[0].node.value);
            let a2 = lower_expr(ctx, builder, &args[1].node.value);
            let pool_type = ctx.type_mapper.lookup_named("PoolAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_pool_new", vec![a1, a2], pool_type);
            return FunctionBuilder::copy(dst);
        }

        // Collection constructors: Dict[K,V](), HashMap[K,V](), Set[K](), HashSet[K](), Vector[T]()
        if matches!(name.as_str(), "Dict" | "HashMap" | "Set" | "HashSet" | "Vector") {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let mangled = super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    // Register the collection type if not present
                    let coll_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        tid
                    };
                    // Check for alloc= named argument
                    let alloc_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
                    });
                    let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                        .filter(|a| !a.node.name.as_ref().map_or(false, |n| n.node == "alloc"))
                        .collect();

                    if positional_args.is_empty() {
                        let new_fn = format!("{mangled}__new");
                        if let Some(alloc_a) = alloc_arg {
                            // alloc= present: push allocator, construct, pop allocator
                            let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                            builder.push_allocator(alloc_op);
                            let coll_local = builder.call_extern(&new_fn, vec![], coll_type);
                            builder.pop_allocator();
                            return FunctionBuilder::copy(coll_local);
                        } else {
                            let coll_local = builder.call_extern(&new_fn, vec![], coll_type);
                            return FunctionBuilder::copy(coll_local);
                        }
                    }
                    // Fall through for positional args — type is registered, regular call will use correct return type
                }
            }
        }

        // Determine effective function name (mangled if generic call)
        let effective_name = if let Some(type_args) = generic_args {
            if !type_args.is_empty() {
                let mangled = super::types::mangle_generic_name(name, type_args);
                // Apply type name substitutions for generic monomorphization
                ctx.resolve_type_name(&mangled)
            } else {
                name.clone()
            }
        } else {
            name.clone()
        };

        // Check if this is an Option/Result variant constructor — resolve with type-aware logic
        {
            let call_arg_values: Vec<Spanned<Expr>> = args.iter()
                .map(|a| a.node.value.clone())
                .collect();
            if let Some(result) = resolve_option_result_variant(ctx, builder, name, &call_arg_values) {
                return result;
            }
        }

        // Check if this is an enum variant constructor
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&effective_name) {
            let field_operands: Vec<Operand> = args.iter()
                .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                .collect();

            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
            return FunctionBuilder::copy(dst);
        }
        // Also check base name for non-generic enum variants
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
            let field_operands: Vec<Operand> = args.iter()
                .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                .collect();

            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
            return FunctionBuilder::copy(dst);
        }

        // Check if this is a closure variable call (e.g., `add_x(5)` where `add_x` is a closure)
        if let Some((local_id, local_type_id)) = ctx.lookup_local(&effective_name) {
            let type_name = ctx.type_name_for_id(local_type_id);
            if let Some(type_name) = type_name {
                let type_name = type_name.to_string();
                if let Some((call_fn, _)) = ctx.lookup_closure_info(&type_name) {
                    let call_fn = call_fn.to_string();
                    // Closure call: __Closure_N__call(&closure_var, args...)
                    // The __call function expects a pointer to the closure struct
                    let ptr_type = ctx.type_registry.insert(GirType::Ptr(local_type_id));
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, Place::local(local_id));
                    let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                    for arg in args {
                        call_args.push(lower_expr(ctx, builder, &arg.node.value));
                    }
                    let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(call_fn.as_str()) {
                        *ret
                    } else {
                        I64_TYPE
                    };
                    if ret_type == UNIT_TYPE {
                        builder.call_void(call_fn, call_args);
                        return Operand::Constant(Constant::Unit);
                    } else {
                        let dst = builder.call(call_fn, call_args, ret_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // Callable parameter call: local exists with void* type (UNIT_TYPE)
            // Emit as __callable_N where N is the local ID, which the C backend
            // will recognize and emit as an indirect function pointer call.
            if local_type_id == UNIT_TYPE {
                let mut call_args = vec![FunctionBuilder::copy(local_id)];
                for arg in args {
                    call_args.push(lower_expr(ctx, builder, &arg.node.value));
                }
                let callable_name = format!("__callable_{}", local_id.0);
                // Look up tracked callable return type, fall back to I64_TYPE
                let ret_type = ctx.callable_return_types.get(&local_id)
                    .copied()
                    .unwrap_or(I64_TYPE);
                let dst = builder.call(callable_name, call_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
        }

        // Regular function call (use effective name for generic functions)
        // Resolve named args + default params before lowering
        let resolved_args = resolve_call_args(ctx, &effective_name, args);
        let lowered_args: Vec<Operand> = resolved_args
            .iter()
            .map(|arg| lower_call_arg(ctx, builder, arg))
            .collect();

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        // Resolve extern bindings: use the C symbol name instead of the Gorget name
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        if ret_type == UNIT_TYPE {
            builder.call_void(call_name, lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(call_name, lowered_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    } else {
        // Non-identifier callee — not handled in Phase 1
        Operand::Constant(Constant::Unit)
    }
}

/// Lower a `print(...)` call to a `printf` extern call.
pub fn lower_print_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &[Spanned<ast::CallArg>],
) {
    if args.is_empty() {
        // print() with no args → printf("\n")
        let fmt = Operand::Constant(Constant::Str("\n".to_string()));
        builder.call_extern("printf", vec![fmt], I32_TYPE);
        return;
    }

    // Check for named arguments: newline=false, file=stderr
    let mut add_newline = true;
    let mut use_stderr = false;
    for arg in args.iter().skip(1) {
        if let Some(ref name) = arg.node.name {
            match name.node.as_str() {
                "newline" => {
                    if let Expr::BoolLiteral(false) = &arg.node.value.node {
                        add_newline = false;
                    }
                }
                "file" => {
                    if let Expr::Identifier(id) = &arg.node.value.node {
                        if id == "stderr" {
                            use_stderr = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }

    let arg_expr = &args[0].node.value;

    match &arg_expr.node {
        Expr::StringLiteral(lit) => {
            let mut format_str = String::new();
            let mut printf_args: Vec<Operand> = Vec::new();

            for segment in &lit.segments {
                match segment {
                    StringSegment::Literal(text) => {
                        format_str.push_str(text);
                    }
                    StringSegment::Interpolation(var_name) => {
                        lower_interp_segment(ctx, builder, var_name,
                            &mut format_str, &mut printf_args);
                    }
                }
            }

            if add_newline {
                format_str.push('\n');
            }

            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
        _ => {
            // General expression (identifier, method call, etc.) — lower and infer type
            let val = lower_expr(ctx, builder, arg_expr);
            let type_id = infer_operand_type_full(ctx, &val, builder);
            let (spec, extra_args) = format_for_printf(ctx, builder, type_id, val);
            let nl = if add_newline { "\n" } else { "" };
            let fmt = format!("{spec}{nl}");
            let fmt_op = Operand::Constant(Constant::Str(fmt));
            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
    }
}

/// Lower a single interpolation segment in a print/format context.
/// Handles simple variable lookups and re-parses complex expressions (method calls, field access, etc.).
fn lower_interp_segment(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    format_str: &mut String,
    printf_args: &mut Vec<Operand>,
) {
    // 1. Try simple variable lookup first
    if let Some((local_id, type_id)) = ctx.lookup_local(var_name) {
        // If this is a mutable capture/borrow pointer, deref to get the value
        if let Some(&value_type) = ctx.mut_capture_locals.get(&local_id) {
            let deref_place = Place {
                local: local_id,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(value_type, None);
            builder.assign(Place::local(tmp), Operand::Copy(deref_place));
            let (spec, args) = format_for_printf(ctx, builder, value_type, FunctionBuilder::copy(tmp));
            format_str.push_str(&spec);
            printf_args.extend(args);
        } else {
            let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(local_id));
            format_str.push_str(&spec);
            printf_args.extend(args);
        }
        return;
    }

    // 2. Try re-parsing as a full expression (handles method calls, field access, operators)
    if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
        let val = lower_expr(ctx, builder, &parsed_expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        // Store result in a temp local so we can take its address / reuse
        let tmp = builder.add_local(type_id, None);
        builder.assign(Place::local(tmp), val);
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp));
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 3. Fallback — insert literal text
    format_str.push_str(var_name);
}

/// Given a type and an operand, return the printf format specifier and the
/// argument list. For Str types, returns `%.*s` with two args (len, data).
/// For bool, returns `%s` with ternary. For other types, returns the standard specifier.
fn format_for_printf(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_id: TypeId,
    operand: Operand,
) -> (String, Vec<Operand>) {
    if type_id == ctx.type_mapper.str_type || type_id == ctx.type_mapper.owned_string_type {
        // Str/GorgetString → %.*s with (int)expr.len, expr.data
        ("%.*s".to_string(), vec![operand])
    } else if type_id == BOOL_TYPE {
        ("%s".to_string(), vec![operand])
    } else if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(type_id).cloned() {
        // Struct type — check if it has a Displayable `display` method
        let display_method = format!("{type_name}__display");
        let has_display = ctx.fn_sigs.contains_key(&display_method)
            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__display")));
        if has_display {
            // Call Type__display(&val) → Str, then use %.*s
            let effective_method = if ctx.fn_sigs.contains_key(&display_method) {
                display_method
            } else {
                ctx.fn_sigs.keys()
                    .find(|k| k.ends_with(&format!("_for_{type_name}__display")))
                    .cloned()
                    .unwrap_or(display_method)
            };
            // Create borrow of the operand for self parameter
            let self_type = ctx.register_ptr_type(type_id);
            let self_ptr = builder.add_local(self_type, None);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                builder.emit_borrow(self_ptr, place.clone());
            }
            let str_type = ctx.type_mapper.str_type;
            let result = builder.call(effective_method, vec![FunctionBuilder::copy(self_ptr)], str_type);
            ("%.*s".to_string(), vec![FunctionBuilder::copy(result)])
        } else {
            // No display method — fall through to default formatting
            let spec = ctx.type_mapper.format_specifier(type_id);
            (spec.to_string(), vec![operand])
        }
    } else {
        // For narrow integer types, cast to int64_t/uint64_t to match %lld/%llu format
        let needs_widen = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE;
        let needs_unsigned_widen = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE;
        let effective_op = if needs_widen {
            let tmp = builder.cast(I64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else if needs_unsigned_widen {
            let tmp = builder.cast(U64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else {
            operand
        };
        let spec = ctx.type_mapper.format_specifier(type_id);
        (spec.to_string(), vec![effective_op])
    }
}

/// Lower a match expression (value-producing).
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

/// Lower a try expression (`expr?`) on a Result type.
fn lower_try_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type_full(ctx, &val, builder);
    let val_local = builder.add_local(val_type, None);
    builder.assign(Place::local(val_local), val);

    // Look up Ok/Error field types from the Result type definition
    let (ok_field_type, err_field_type) = {
        let type_name = ctx.type_registry.type_name(val_type);
        if let Some(ref name) = type_name {
            if let Some(td) = ctx.type_registry.get_type_def(name) {
                if let crate::ir::types::TypeDefKind::Enum(ref e) = td.kind {
                    let ok_ty = e.variants.iter().find(|v| v.name == "Ok")
                        .and_then(|v| v.fields.first().map(|f| f.type_id))
                        .unwrap_or(I64_TYPE);
                    let err_ty = e.variants.iter().find(|v| v.name == "Error")
                        .and_then(|v| v.fields.first().map(|f| f.type_id))
                        .unwrap_or(I64_TYPE);
                    (ok_ty, err_ty)
                } else { (I64_TYPE, I64_TYPE) }
            } else { (I64_TYPE, I64_TYPE) }
        } else { (I64_TYPE, I64_TYPE) }
    };

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
    // Check `current_throws_result_type` first (for `throws` functions),
    // then check the function's return place (for explicit Result return types).
    let fn_result_type = ctx.current_throws_result_type.or_else(|| {
        let ret_type = builder.locals[0].type_id;
        let type_name = ctx.type_registry.type_name(ret_type)?;
        if type_name.starts_with("Result__") {
            Some(ret_type)
        } else {
            None
        }
    });
    if let Some(result_type) = fn_result_type {
        let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
        let err_dst = builder.enum_init(type_name, "Error", result_type, vec![FunctionBuilder::copy(err_val)]);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
    } else {
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_val));
    }
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
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
            lower_match_stmt_as_expr(ctx, builder, scrutinee, arms, else_arm.as_ref())
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
    arms: &[ast::MatchArm],
    else_arm: Option<&ast::Block>,
) -> Operand {
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = infer_operand_type_full(ctx, &scrut_op, builder);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

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

        if next_test_bb != merge_bb {
            builder.switch_to(next_test_bb);
        }
    }

    if let Some(else_block) = else_arm {
        let else_val = lower_block_expr(ctx, builder, else_block);
        builder.assign(Place::local(result_local), else_val);
        builder.jump(merge_bb);
    } else if !arms.is_empty() {
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
    builder.assign(Place::local(result_id), then_val);
    builder.jump(merge_bb);

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
        builder.assign(Place::local(result_id), elif_val);
        builder.jump(merge_bb);

        current_else_bb = next_else_bb;
    }

    // Else branch
    builder.switch_to(current_else_bb);
    if let Some(else_block) = else_body {
        let else_val = lower_block_expr(ctx, builder, else_block);
        builder.assign(Place::local(result_id), else_val);
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::I64(0)));
    }
    builder.jump(merge_bb);

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
            StringSegment::Interpolation(var_name) => {
                lower_interp_segment(ctx, builder, var_name,
                    &mut format_str, &mut args);
            }
        }
    }

    // Emit CallExtern("gorget_string_format", [fmt_str, ...args]) → GorgetString
    let owned_string_type = ctx.type_mapper.owned_string_type;
    let mut all_args = vec![Operand::Constant(Constant::Str(format_str))];
    all_args.extend(args);
    let dst = builder.call_extern("gorget_string_format", all_args, owned_string_type);
    FunctionBuilder::copy(dst)
}

// ---- P3.5.1: Array Literals ----

/// Lower `[e1, e2, ...]` to `gorget_array_new(sizeof(elem))` + N `gorget_array_push` calls.
fn lower_array_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elems: &[Spanned<Expr>],
) -> Operand {
    let array_type = ctx.type_mapper.lookup_named("GorgetArray").unwrap_or(UNIT_TYPE);

    // Infer element type from first element
    let elem_type = if !elems.is_empty() {
        let first = lower_expr(ctx, builder, &elems[0]);
        let etype = infer_operand_type_full(ctx, &first, builder);
        // Create the array
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(etype))],
            array_type,
        );
        // Push first element
        let elem_local = builder.add_local(etype, None);
        builder.assign(Place::local(elem_local), first);
        let ref_local = builder.borrow(Place::local(elem_local), ctx.register_ptr_type(etype));
        let arr_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(array_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(ref_local)],
            UNIT_TYPE,
        );
        // Push remaining elements
        for elem_expr in &elems[1..] {
            let elem_val = lower_expr(ctx, builder, elem_expr);
            let el = builder.add_local(etype, None);
            builder.assign(Place::local(el), elem_val);
            let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(etype));
            let ar_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(array_type));
            builder.call_extern(
                "gorget_array_push",
                vec![FunctionBuilder::copy(ar_ref), FunctionBuilder::copy(el_ref)],
                UNIT_TYPE,
            );
        }
        FunctionBuilder::copy(arr_local)
    } else {
        // Empty array — use I64 as default element size
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            array_type,
        );
        FunctionBuilder::copy(arr_local)
    };
    elem_type
}

// ---- P3.5.2: Dict Literals ----

/// Lower `{"a": 1, "b": 2}` to `Dict__K__V__new()` + N `Dict__K__V__put()` calls.
fn lower_dict_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pairs: &[(Spanned<Expr>, Spanned<Expr>)],
) -> Operand {
    if pairs.is_empty() {
        // Use expected type from VarDecl context to determine dict type
        if let Some(expected_type) = ctx.expected_type {
            if let Some(type_name) = ctx.type_registry.type_name(expected_type) {
                if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") {
                    let new_fn = format!("{type_name}__new");
                    let dict_local = builder.call_extern(&new_fn, vec![], expected_type);
                    return FunctionBuilder::copy(dict_local);
                }
            }
        }
        return Operand::Constant(Constant::Unit);
    }

    // Lower first pair to infer key/value types
    let first_key = lower_expr(ctx, builder, &pairs[0].0);
    let first_val = lower_expr(ctx, builder, &pairs[0].1);
    let key_type = infer_operand_type_full(ctx, &first_key, builder);
    let val_type = infer_operand_type_full(ctx, &first_val, builder);

    // Compute mangled dict type name
    let key_c = type_id_to_mangle_name(ctx, key_type);
    let val_c = type_id_to_mangle_name(ctx, val_type);
    let mangled = format!("Dict__{key_c}__{val_c}");

    // Register dict type if not present
    let dict_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
        tid
    } else {
        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
        ctx.type_mapper.register_named(mangled.clone(), tid);
        tid
    };

    let new_fn = format!("{mangled}__new");
    let put_fn = format!("{mangled}__put");

    // Create the dict
    let dict_local = builder.call_extern(&new_fn, vec![], dict_type);

    // Insert first pair
    let dict_ref = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
    builder.call_extern(
        &put_fn,
        vec![FunctionBuilder::copy(dict_ref), first_key, first_val],
        UNIT_TYPE,
    );

    // Insert remaining pairs
    for (key_expr, val_expr) in &pairs[1..] {
        let k = lower_expr(ctx, builder, key_expr);
        let v = lower_expr(ctx, builder, val_expr);
        let dr = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
        builder.call_extern(
            &put_fn,
            vec![FunctionBuilder::copy(dr), k, v],
            UNIT_TYPE,
        );
    }

    FunctionBuilder::copy(dict_local)
}

/// Map a TypeId to a C-compatible mangle fragment for dict/set type names.
fn type_id_to_mangle_name(ctx: &LoweringContext, type_id: TypeId) -> String {
    if type_id == I64_TYPE { return "int64_t".to_string(); }
    if type_id == I32_TYPE { return "int32_t".to_string(); }
    if type_id == I16_TYPE { return "int16_t".to_string(); }
    if type_id == I8_TYPE { return "int8_t".to_string(); }
    if type_id == U64_TYPE { return "uint64_t".to_string(); }
    if type_id == U32_TYPE { return "uint32_t".to_string(); }
    if type_id == U16_TYPE { return "uint16_t".to_string(); }
    if type_id == U8_TYPE { return "uint8_t".to_string(); }
    if type_id == F64_TYPE { return "double".to_string(); }
    if type_id == F32_TYPE { return "float".to_string(); }
    if type_id == BOOL_TYPE { return "bool".to_string(); }
    if type_id == ctx.type_mapper.str_type { return "Str".to_string(); }
    if type_id == ctx.type_mapper.owned_string_type { return "GorgetString".to_string(); }
    // Named types
    if let Some(name) = ctx.type_name_for_id(type_id) {
        return name.to_string();
    }
    "int64_t".to_string() // fallback
}

// ---- P3.5.3: List Comprehensions ----

/// Lower `[expr for var in iterable if condition]` to a loop that builds an array.
fn lower_list_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<ast::Pattern>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let array_type = ctx.type_mapper.lookup_named("GorgetArray").unwrap_or(UNIT_TYPE);

    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        // Create accumulator array (use I64 as default element size)
        let acc_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            array_type,
        );

        // Create loop variable
        let var_name = match &variable.node {
            ast::Pattern::Binding(name) => name.clone(),
            _ => "_comp_var".to_string(),
        };
        let loop_var = builder.add_local(I64_TYPE, Some(&var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(&var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let push_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        // Header: compare loop var with end
        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        // Body: optionally check condition
        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, push_bb.unwrap(), incr_bb);
            builder.switch_to(push_bb.unwrap());
        }

        // Push element
        let elem_val = lower_expr(ctx, builder, comp_expr);
        let elem_type = infer_operand_type_full(ctx, &elem_val, builder);
        let el = builder.add_local(elem_type, None);
        builder.assign(Place::local(el), elem_val);
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(elem_type));
        let arr_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(array_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
        builder.jump(incr_bb);

        // Increment
        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        // Exit
        builder.switch_to(exit_bb);
        FunctionBuilder::copy(acc_local)
    } else {
        // Non-range iterables: emit placeholder
        builder.nop();
        let acc_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            array_type,
        );
        FunctionBuilder::copy(acc_local)
    }
}

// ---- P3.5.4: Dict and Set Comprehensions ----

/// Lower `{key: value for var in iterable if condition}`.
fn lower_dict_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    key_expr: &Spanned<Expr>,
    val_expr: &Spanned<Expr>,
    variables: &[Spanned<String>],
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        let var_name = if let Some(first) = variables.first() {
            first.node.clone()
        } else {
            "_dict_comp_var".to_string()
        };

        // We need to infer dict type — use I64 placeholders
        let mangled = "Dict__int64_t__int64_t".to_string();
        let dict_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
            ctx.type_mapper.register_named(mangled.clone(), tid);
            tid
        };

        let new_fn = format!("{mangled}__new");
        let put_fn = format!("{mangled}__put");

        let dict_local = builder.call_extern(&new_fn, vec![], dict_type);

        // Create loop variable
        let loop_var = builder.add_local(I64_TYPE, Some(&var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(&var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let put_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, put_bb.unwrap(), incr_bb);
            builder.switch_to(put_bb.unwrap());
        }

        let k = lower_expr(ctx, builder, key_expr);
        let v = lower_expr(ctx, builder, val_expr);
        let dr = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
        builder.call_extern(&put_fn, vec![FunctionBuilder::copy(dr), k, v], UNIT_TYPE);
        builder.jump(incr_bb);

        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        builder.switch_to(exit_bb);
        FunctionBuilder::copy(dict_local)
    } else {
        builder.nop();
        Operand::Constant(Constant::Unit)
    }
}

/// Lower `{expr for var in iterable if condition}` (set comprehension).
fn lower_set_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<String>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let set_type = ctx.type_mapper.lookup_named("GorgetSet")
        .or_else(|| ctx.type_mapper.lookup_named("GorgetArray"))
        .unwrap_or(UNIT_TYPE);

    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        let acc_local = builder.call_extern(
            "gorget_set_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            set_type,
        );

        let var_name = &variable.node;
        let loop_var = builder.add_local(I64_TYPE, Some(var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let push_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, push_bb.unwrap(), incr_bb);
            builder.switch_to(push_bb.unwrap());
        }

        let elem_val = lower_expr(ctx, builder, comp_expr);
        let elem_type = infer_operand_type_full(ctx, &elem_val, builder);
        let el = builder.add_local(elem_type, None);
        builder.assign(Place::local(el), elem_val);
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(elem_type));
        let set_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(set_type));
        builder.call_extern(
            "gorget_set_add",
            vec![FunctionBuilder::copy(set_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
        builder.jump(incr_bb);

        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        builder.switch_to(exit_bb);
        FunctionBuilder::copy(acc_local)
    } else {
        builder.nop();
        Operand::Constant(Constant::Unit)
    }
}

// ---- P3.5.6: Optional Chaining ----

/// Lower `obj?.field` to a null-check + conditional field access.
fn lower_optional_chain(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field: &Spanned<String>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let obj_type = infer_operand_type_full(ctx, &obj, builder);
    let obj_local = builder.add_local(obj_type, None);
    builder.assign(Place::local(obj_local), obj);

    // Check if not null
    let not_null = builder.cmp(
        CmpOp::Ne,
        obj_type,
        FunctionBuilder::copy(obj_local),
        Operand::Constant(Constant::Null),
    );

    let result_local = builder.add_local(I64_TYPE, None); // placeholder result type
    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(not_null), then_bb, else_bb);

    // then: access the field
    builder.switch_to(then_bb);
    // Try to resolve field via struct field cache
    let field_val = if let Some(type_name) = ctx.type_name_for_id(obj_type) {
        if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
            let dst = builder.field_load(Place::local(obj_local), field_idx, field_type);
            FunctionBuilder::copy(dst)
        } else {
            Operand::Constant(Constant::Null)
        }
    } else {
        // Try through pointer dereference
        if let Some(pointee) = ctx.pointee_type(obj_type) {
            if let Some(type_name) = ctx.type_name_for_id(pointee) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
                    let mut deref_place = Place::local(obj_local);
                    deref_place.projections.push(Projection::Deref);
                    let dst = builder.field_load(deref_place, field_idx, field_type);
                    FunctionBuilder::copy(dst)
                } else {
                    Operand::Constant(Constant::Null)
                }
            } else {
                Operand::Constant(Constant::Null)
            }
        } else {
            Operand::Constant(Constant::Null)
        }
    };
    builder.assign(Place::local(result_local), field_val);
    builder.jump(merge_bb);

    // else: assign null
    builder.switch_to(else_bb);
    builder.assign(Place::local(result_local), Operand::Constant(Constant::Null));
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

// ---- P3.5.7: Range Expressions (standalone) ----

/// Lower `start..end` or `start..=end` to a `GorgetRange` struct init.
fn lower_range_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    start: Option<&Spanned<Expr>>,
    end: Option<&Spanned<Expr>>,
    inclusive: bool,
) -> Operand {
    let start_val = if let Some(s) = start {
        lower_expr(ctx, builder, s)
    } else {
        Operand::Constant(Constant::I64(0))
    };
    let end_val = if let Some(e) = end {
        lower_expr(ctx, builder, e)
    } else {
        Operand::Constant(Constant::I64(0))
    };
    let inclusive_val = Operand::Constant(Constant::Bool(inclusive));

    let range_type = ctx.type_mapper.lookup_named("GorgetRange").unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(
        "GorgetRange",
        range_type,
        vec![start_val, end_val, inclusive_val],
    );
    FunctionBuilder::copy(dst)
}

/// Infer the GIR type of an operand by examining its structure.
/// Register (or reuse) a Tuple TypeDef for the given element types.
fn register_tuple_type(ctx: &mut LoweringContext, elem_types: &[TypeId]) -> TypeId {
    use crate::ir::types::{format_type_for_mangle, GirType, StructDef, StructField, TypeDef, TypeDefKind, TypeMetadata, CopySemantics};

    // Build mangled name: Tuple__T1__T2__...
    let mut name = "Tuple".to_string();
    for &tid in elem_types {
        name.push_str("__");
        name.push_str(&format_type_for_mangle(tid, &ctx.type_registry));
    }

    // Reuse existing TypeDef if already registered
    if let Some(existing) = ctx.type_mapper.lookup_named(&name) {
        // Ensure struct_fields is populated even if the type was pre-registered
        // (e.g., by map_ast_type_mut during fn_sigs pre-scan, which doesn't
        // have access to struct_fields)
        if !ctx.struct_fields.contains_key(&(name.clone(), "_0".to_string())) {
            if let Some(type_def) = ctx.type_registry.get_type_def(&name) {
                if let TypeDefKind::Struct(ref s) = type_def.kind {
                    for (i, field) in s.fields.iter().enumerate() {
                        ctx.struct_fields.insert(
                            (name.clone(), field.name.clone()),
                            (i as u32, field.type_id),
                        );
                    }
                }
            }
        }
        return existing;
    }

    // Create struct fields: _0, _1, _2, ...
    let fields: Vec<StructField> = elem_types.iter().enumerate()
        .map(|(i, &tid)| StructField { name: format!("_{i}"), type_id: tid })
        .collect();

    ctx.type_registry.add_type_def(TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields: fields.clone() }),
        metadata: TypeMetadata {
            copy_semantics: CopySemantics::Copy,
            ..TypeMetadata::default()
        },
    });

    // Also populate struct_fields cache so lookup_field() works
    // (populate_struct_fields() runs once before function lowering,
    // so dynamically-created tuple types need manual insertion)
    for (i, field) in fields.iter().enumerate() {
        ctx.struct_fields.insert(
            (name.clone(), field.name.clone()),
            (i as u32, field.type_id),
        );
    }

    let type_id = ctx.type_registry.insert(GirType::Named(name.clone()));
    ctx.type_mapper.register_named(name, type_id);
    type_id
}

/// Resolve the element type at a given index from a tuple TypeDef.
pub fn resolve_tuple_field_type(ctx: &LoweringContext, tuple_type_id: TypeId, index: usize) -> TypeId {
    if let Some(type_name) = ctx.type_name_for_id(tuple_type_id) {
        if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            if let TypeDefKind::Struct(ref s) = type_def.kind {
                if let Some(field) = s.fields.get(index) {
                    return field.type_id;
                }
            }
        }
    }
    I64_TYPE // fallback
}

/// Infer operand type using both ctx locals and builder locals.
/// This handles compiler temporaries (tuples, struct inits, etc.) that aren't in ctx.locals.
/// Extract the local ID from an operand if it's a simple local reference.
fn consumed_local_id(operand: &Operand) -> Option<LocalId> {
    if let Operand::Copy(place) | Operand::Move(place) = operand {
        if place.projections.is_empty() {
            return Some(place.local);
        }
    }
    None
}

/// Mark a consumed local as moved so it won't be double-freed at scope exit.
/// When a local's value is moved into a container (Result.Ok, etc.), the original
/// local's data pointer is shared — we must zero it to prevent the scope drop
/// from freeing data that's now owned by the container.
fn mark_consumed_local_by_id(ctx: &mut LoweringContext, builder: &mut FunctionBuilder, local: LocalId) {
    builder.move_zero(Place::local(local));
    ctx.drops.mark_moved(local);
}

pub fn infer_operand_type_full(ctx: &LoweringContext, operand: &Operand, builder: &FunctionBuilder) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            // Fall back to builder locals
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                return builder.locals[idx].type_id;
            }
            I64_TYPE
        }
        other => infer_operand_type(ctx, other),
    }
}

pub fn infer_operand_type(ctx: &LoweringContext, operand: &Operand) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // Look up the local's type
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            I64_TYPE // fallback
        }
        Operand::Constant(c) => match c {
            Constant::Bool(_) => BOOL_TYPE,
            Constant::I8(_) => I8_TYPE,
            Constant::I16(_) => I16_TYPE,
            Constant::I32(_) => I32_TYPE,
            Constant::I64(_) => I64_TYPE,
            Constant::U8(_) => U8_TYPE,
            Constant::U16(_) => U16_TYPE,
            Constant::U32(_) => U32_TYPE,
            Constant::U64(_) => U64_TYPE,
            Constant::Char(_) => CHAR_TYPE,
            Constant::F32(_) => F32_TYPE,
            Constant::F64(_) => F64_TYPE,
            Constant::Str(_) => ctx.type_mapper.str_type,
            Constant::Null => UNIT_TYPE,
            Constant::Unit => UNIT_TYPE,
            Constant::SizeOf(_) => U64_TYPE,
            Constant::FuncRef(_) => UNIT_TYPE, // treated as void* at call site
        },
    }
}

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
            segments: vec![StringSegment::Interpolation("x".into())],
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
    fn lower_char_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::CharLiteral('A')));
        assert!(matches!(result, Operand::Constant(Constant::Char(65))));
    }

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
                    StringSegment::Interpolation("x".into()),
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
