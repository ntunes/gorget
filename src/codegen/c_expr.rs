/// Expression codegen: convert Gorget expressions to C expression strings.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{BinaryOp, Expr, PrimitiveType, Type, UnaryOp};
use crate::parser::Parser;
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

/// Check if an expression is a C lvalue (can take its address directly).
fn is_lvalue(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_) | Expr::SelfExpr => true,
        Expr::FieldAccess { object, .. } => is_lvalue(&object.node),
        Expr::TupleFieldAccess { object, .. } => is_lvalue(&object.node),
        _ => false,
    }
}

impl CodegenContext<'_> {
    /// Generate a C expression string from a Gorget expression.
    pub fn gen_expr(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::IntLiteral(n) => format!("INT64_C({n})"),
            Expr::FloatLiteral(f) => format!("{f}"),
            Expr::BoolLiteral(b) => if *b { "true" } else { "false" }.to_string(),
            Expr::CharLiteral(c) => format!("'{}'", escape_char(*c)),
            Expr::StringLiteral(s) => self.gen_string_literal(s),
            Expr::NoneLiteral => {
                // Try to resolve to monomorphized Option None constructor via type hint
                if let Some(mangled) = self.resolve_unit_variant_from_type_hint("Option", "None") {
                    format!("{}()", c_mangle::mangle_variant(&mangled, "None"))
                } else {
                    "NULL".to_string()
                }
            }

            Expr::Identifier(name) => c_mangle::escape_keyword(name),

            Expr::SelfExpr => "self".to_string(),

            Expr::Path { segments } => {
                // For now, join with __ (e.g., Color.Red → Color__Red)
                if segments.len() == 2 {
                    c_mangle::mangle_variant(&segments[0].node, &segments[1].node)
                } else {
                    segments
                        .iter()
                        .map(|s| s.node.as_str())
                        .collect::<Vec<_>>()
                        .join("__")
                }
            }

            Expr::UnaryOp { op, operand } => {
                let inner = self.gen_expr(operand);
                match op {
                    UnaryOp::Neg => format!("(-{inner})"),
                    UnaryOp::Not => format!("(!{inner})"),
                    UnaryOp::Deref => format!("(*{inner})"),
                }
            }

            Expr::BinaryOp { left, op, right } => {
                // `x in coll` → desugared to coll.contains(x)
                if *op == BinaryOp::In {
                    return self.gen_in_operator(left, right);
                }
                // Auto-hook: Eq/Neq on struct types → Equatable trait call
                if matches!(op, BinaryOp::Eq | BinaryOp::Neq) {
                    if let Some(type_name) = self.try_equatable_type(left) {
                        let l = self.gen_expr(left);
                        let r = self.gen_expr(right);
                        let mangled = c_mangle::mangle_trait_method("Equatable", &type_name, "eq");
                        let needs_temp = !is_lvalue(&left.node);
                        let eq_call = if needs_temp {
                            format!("({{ __typeof__({l}) __recv = {l}; {mangled}(&__recv, {r}); }})")
                        } else {
                            format!("{mangled}(&{l}, {r})")
                        };
                        return if *op == BinaryOp::Neq {
                            format!("(!{eq_call})")
                        } else {
                            eq_call
                        };
                    }
                }
                // String concatenation: str + str → gorget_str_concat(a, b)
                if *op == BinaryOp::Add {
                    if let Some(type_id) = self.resolve_expr_type_id(left) {
                        if type_id == self.types.string_id {
                            let l = self.gen_expr(left);
                            let r = self.gen_expr(right);
                            return format!("gorget_str_concat({l}, {r})");
                        }
                    }
                }
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let c_op = binary_op_to_c(*op);
                if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
                    format!("({{ __typeof__({r}) __d = {r}; if (__d == 0) gorget_panic(\"division by zero\"); {l} {c_op} __d; }})")
                } else if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul) && !self.overflow_wrap {
                    let macro_name = match op {
                        BinaryOp::Add => "GORGET_CHECKED_ADD",
                        BinaryOp::Sub => "GORGET_CHECKED_SUB",
                        BinaryOp::Mul => "GORGET_CHECKED_MUL",
                        _ => unreachable!(),
                    };
                    format!("{macro_name}({l}, {r})")
                } else {
                    format!("({l} {c_op} {r})")
                }
            }

            Expr::Call { callee, generic_args, args } => {
                if let Some(type_args) = generic_args {
                    self.gen_generic_call(callee, type_args, args)
                } else {
                    self.gen_call(callee, args)
                }
            }

            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                if let Some(type_args) = generic_args {
                    self.gen_generic_method_call(receiver, &method.node, type_args, args)
                } else {
                    self.gen_method_call(receiver, &method.node, args)
                }
            }

            Expr::FieldAccess { object, field } => {
                let obj = self.gen_expr(object);
                let field_name = c_mangle::escape_keyword(&field.node);
                if self.current_self_type.is_some() && matches!(object.node, Expr::SelfExpr) {
                    format!("self->{field_name}")
                } else {
                    format!("{obj}.{field_name}")
                }
            }

            Expr::TupleFieldAccess { object, index } => {
                let obj = self.gen_expr(object);
                format!("{obj}._{index}")
            }

            Expr::Index { object, index } => {
                let obj = self.gen_expr(object);
                // Detect string receiver for runtime-checked indexing/slicing
                if self.is_string_expr(object) {
                    if let Expr::Range { start, end, inclusive } = &index.node {
                        let s = start.as_ref().map(|e| self.gen_expr(e)).unwrap_or_else(|| "INT64_C(0)".to_string());
                        let e = if let Some(end_expr) = end.as_ref() {
                            let ev = self.gen_expr(end_expr);
                            if *inclusive {
                                format!("({ev} + 1)")
                            } else {
                                ev
                            }
                        } else {
                            format!("(int64_t)strlen({obj})")
                        };
                        format!("gorget_string_slice({obj}, {s}, {e})")
                    } else {
                        let idx = self.gen_expr(index);
                        format!("gorget_string_at({obj}, {idx})")
                    }
                } else if self.is_vector_expr(object) {
                    if let Expr::Range { start, end, inclusive } = &index.node {
                        let s = start.as_ref().map(|e| self.gen_expr(e)).unwrap_or_else(|| "INT64_C(0)".to_string());
                        let e = if let Some(end_expr) = end.as_ref() {
                            let ev = self.gen_expr(end_expr);
                            if *inclusive {
                                format!("({ev} + 1)")
                            } else {
                                ev
                            }
                        } else {
                            format!("(int64_t){obj}.len")
                        };
                        format!("({{ GorgetArray __slice_src = {obj}; gorget_array_slice(&__slice_src, {s}, {e}); }})")
                    } else {
                        let idx = self.gen_expr(index);
                        let elem_type = self.infer_vector_elem_type(object);
                        format!("GORGET_ARRAY_AT({elem_type}, {obj}, {idx})")
                    }
                } else {
                    let idx = self.gen_expr(index);
                    format!("{obj}[{idx}]")
                }
            }

            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                // Ranges are handled specially in for-loop codegen; standalone range is not valid C.
                let s = start
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                let e = end
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                format!(
                    "/* range {s}..{}{e} */",
                    if *inclusive { "=" } else { "" }
                )
            }

            Expr::StructLiteral { name, args } => {
                let field_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(a)).collect();
                let fields_str = field_exprs.join(", ");
                let type_name = &name.node;
                format!("({type_name}){{{fields_str}}}")
            }

            Expr::ArrayLiteral(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                format!("{{{}}}", elems.join(", "))
            }

            Expr::TupleLiteral(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                let c_field_types: Vec<String> = elements
                    .iter()
                    .map(|e| self.infer_c_type_from_expr(&e.node))
                    .collect();
                let tuple_name = self.register_tuple_typedef(&c_field_types);
                format!("({tuple_name}){{{}}}", elems.join(", "))
            }

            Expr::As { expr, type_ } => {
                let inner = self.gen_expr(expr);
                let c_type = self.type_to_c(&type_.node);
                format!("(({c_type}){inner})")
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond = self.gen_expr(condition);
                let then_val = self.gen_expr(then_branch);
                let else_val = else_branch
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                format!("({cond} ? {then_val} : {else_val})")
            }

            Expr::Move { expr } | Expr::MutableBorrow { expr } => {
                // In C output, ownership/borrow semantics are erased
                self.gen_expr(expr)
            }

            Expr::Deref { expr } => {
                // For Box[T] pointers, generate actual dereference
                let inner = self.gen_expr(expr);
                if self.is_box_expr(expr) {
                    format!("(*{inner})")
                } else {
                    // Non-box: ownership semantics erased
                    inner
                }
            }

            Expr::Closure {
                params,
                body,
                ..
            } => {
                self.gen_closure_expr(params, body)
            }

            Expr::ImplicitClosure { body } => {
                // Implicit closure with `it` parameter
                let param = crate::parser::ast::ClosureParam {
                    type_: None,
                    ownership: crate::parser::ast::Ownership::Borrow,
                    name: crate::span::Spanned {
                        node: "it".to_string(),
                        span: body.span,
                    },
                };
                let params = vec![crate::span::Spanned {
                    node: param,
                    span: body.span,
                }];
                self.gen_closure_expr(&params, body)
            }

            Expr::ListComprehension {
                expr: comp_expr,
                variable,
                iterable,
                condition,
                ..
            } => {
                self.gen_list_comprehension(comp_expr, variable, iterable, condition.as_deref())
            }

            Expr::SetComprehension {
                expr: comp_expr,
                variable,
                iterable,
                condition,
            } => {
                self.gen_set_comprehension(comp_expr, variable, iterable, condition.as_deref())
            }

            Expr::DictComprehension {
                key,
                value: dict_val,
                variables,
                iterable,
                condition,
            } => {
                self.gen_dict_comprehension(key, dict_val, variables, iterable, condition.as_deref())
            }

            Expr::TryCapture { expr: catch_expr } => {
                // GCC statement expression: try the expression, on error return default
                let inner = self.gen_expr(catch_expr);
                format!(
                    "({{ __typeof__({inner}) __catch_val; \
                    if (GORGET_TRY) {{ __catch_val = {inner}; GORGET_CATCH_END; }} \
                    else {{ GORGET_CATCH_END; memset(&__catch_val, 0, sizeof(__catch_val)); }} \
                    __catch_val; }})"
                )
            }

            Expr::Is {
                expr: is_expr,
                negated,
                pattern,
            } => {
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(is_expr);
                let val = self.gen_expr(is_expr);
                let cond = self.pattern_to_condition_expr(&pattern.node, &val, enum_c_type.as_deref());
                if *negated {
                    format!("(!({cond}))")
                } else {
                    format!("({cond})")
                }
            }

            Expr::It => "it".to_string(),

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => self.gen_match_expr(scrutinee, arms, else_arm.as_deref()),

            Expr::Block(block) => self.gen_block_expr(block),

            Expr::Do { body } => self.gen_block_expr(body),

            Expr::OptionalChain { object, field } => {
                let obj = self.gen_expr(object);
                let field_name = c_mangle::escape_keyword(&field.node);
                format!("({{ __typeof__({obj}) __oc_tmp = {obj}; __oc_tmp != NULL ? __oc_tmp->{field_name} : NULL; }})")
            }

            Expr::NilCoalescing { lhs, rhs } => {
                let l = self.gen_expr(lhs);
                let r = self.gen_expr(rhs);
                format!("({{ __typeof__({l}) __nc_tmp = {l}; __nc_tmp != NULL ? __nc_tmp : {r}; }})")
            }

            Expr::Try { expr: try_expr } => {
                // Check if inner expression has Result type via expr_types map
                if let Some(&type_id) = self.expr_types.get(&try_expr.span) {
                    if let crate::semantic::types::ResolvedType::Generic(def_id, ref args) = self.types.get(type_id).clone() {
                        let base_name = self.scopes.get_def(def_id).name.clone();
                        if base_name == "Result" && args.len() == 2 {
                            return self.gen_result_try(try_expr, &args);
                        }
                    }
                }
                // Fallback: existing setjmp/longjmp behavior
                let inner = self.gen_expr(try_expr);
                format!(
                    "({{ __typeof__({inner}) __try_val; \
                    if (GORGET_TRY) {{ __try_val = {inner}; GORGET_CATCH_END; }} \
                    else {{ GORGET_CATCH_END; \
                    if (__gorget_jmp_top >= 0) longjmp(__gorget_jmp_stack[__gorget_jmp_top], 1); \
                    else {{ fprintf(stderr, \"Unhandled error: %s\\n\", __gorget_last_error.message); exit(1); }} }} \
                    __try_val; }})"
                )
            }

            Expr::Await { expr: await_expr } => {
                let inner = self.gen_expr(await_expr);
                format!("/* await */ {inner}")
            }

            Expr::Spawn { expr: spawn_expr } => {
                let inner = self.gen_expr(spawn_expr);
                format!("/* spawn */ {inner}")
            }
        }
    }

    /// Generate Result-based `?` operator: unwrap Ok or early-return Error.
    fn gen_result_try(&self, try_expr: &Spanned<Expr>, args: &[crate::semantic::ids::TypeId]) -> String {
        let inner = self.gen_expr(try_expr);
        let t_c = c_types::type_id_to_c(args[0], self.types, self.scopes);
        let e_c = c_types::type_id_to_c(args[1], self.types, self.scopes);
        let inner_mangled = c_mangle::mangle_generic("Result", &[t_c, e_c.clone()]);
        let tag_error = c_mangle::mangle_tag(&inner_mangled, "Error");

        // Get function's return type for Error constructor
        let fn_ret = self.current_function_return_c_type.borrow().clone()
            .unwrap_or_else(|| inner_mangled.clone());
        let ret_error_ctor = c_mangle::mangle_variant(&fn_ret, "Error");

        // Unique temp name for nested ? support
        let try_id = {
            let mut c = self.try_counter.borrow_mut();
            let id = *c;
            *c += 1;
            id
        };

        format!(
            "({{ {inner_mangled} __try_r{try_id} = {inner}; \
            if (__try_r{try_id}.tag == {tag_error}) {{ \
            return {ret_error_ctor}(__try_r{try_id}.data.Error._0); }} \
            __try_r{try_id}.data.Ok._0; }})"
        )
    }

    /// Generate a plain C string literal (no interpolation).
    fn gen_string_literal(&self, s: &StringLit) -> String {
        let mut result = String::from("\"");
        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    result.push_str(&escape_string(text));
                }
                StringSegment::Interpolation(_) => {
                    // Interpolations inside string literals are only handled in print context
                    result.push_str("%s");
                }
            }
        }
        result.push('"');
        result
    }

    /// Generate a function/builtin call.
    fn gen_call(
        &self,
        callee: &Spanned<Expr>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        // Handle None() as a variant constructor for Option[T]
        if matches!(&callee.node, Expr::NoneLiteral) && args.is_empty() {
            if let Some(mangled) = self.resolve_unit_variant_from_type_hint("Option", "None") {
                return format!("{}()", c_mangle::mangle_variant(&mangled, "None"));
            }
        }

        // Check for built-in print/println
        if let Expr::Identifier(name) = &callee.node {
            match name.as_str() {
                "print" | "println" => return self.gen_print_call(args, name == "println"),
                "format" => return self.gen_format_call(args),
                "len" => {
                    if let Some(arg) = args.first() {
                        let a = self.gen_expr(&arg.node.value);
                        return format!("(sizeof({a}) / sizeof({a}[0]))");
                    }
                }
                "read_file" => {
                    if let Some(arg) = args.first() {
                        let path = self.gen_expr(&arg.node.value);
                        return format!("gorget_read_file({path})");
                    }
                }
                "write_file" | "append_file" => {
                    let func = if name == "write_file" { "gorget_write_file" } else { "gorget_append_file" };
                    if args.len() >= 2 {
                        let path = self.gen_expr(&args[0].node.value);
                        let content = self.gen_expr(&args[1].node.value);
                        return format!("{func}({path}, {content})");
                    }
                }
                "file_exists" => {
                    if let Some(arg) = args.first() {
                        let path = self.gen_expr(&arg.node.value);
                        return format!("gorget_file_exists({path})");
                    }
                }
                "delete_file" => {
                    if let Some(arg) = args.first() {
                        let path = self.gen_expr(&arg.node.value);
                        return format!("gorget_delete_file({path})");
                    }
                }
                _ => {}
            }

            // Handle Box(value) constructor → heap allocation
            if name == "Box" {
                if let Some(arg) = args.first() {
                    let inner = self.gen_expr(&arg.node.value);
                    let inner_type = self.box_inner_c_type(&arg.node.value.node);
                    return format!(
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                    );
                }
            }

            // Check if this is a struct constructor
            if let Some(def_id) = self.scopes.lookup(name) {
                let def = self.scopes.get_def(def_id);
                if def.kind == crate::semantic::scope::DefKind::Struct
                    || def.kind == crate::semantic::scope::DefKind::Newtype
                {
                    let field_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    let fields = field_exprs.join(", ");
                    return format!("({name}){{{fields}}}");
                }
                // Check for enum variant constructor
                if def.kind == crate::semantic::scope::DefKind::Variant {
                    // Find which enum this variant belongs to
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, vid) in &info.variants {
                            if *vid == def_id {
                                let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                                let field_exprs: Vec<String> =
                                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                                let fields = field_exprs.join(", ");
                                // For built-in generic enum templates, use decl_type_hint
                                if self.generic_enum_templates.borrow().contains_key(&enum_name) {
                                    if let Some(mangled) = self.resolve_unit_variant_from_type_hint(&enum_name, vname) {
                                        return format!(
                                            "{}({fields})",
                                            c_mangle::mangle_variant(&mangled, vname)
                                        );
                                    }
                                }
                                return format!(
                                    "{}({fields})",
                                    c_mangle::mangle_variant(&enum_name, vname)
                                );
                            }
                        }
                    }
                }
            }

            // Check if this is a GorgetClosure variable — dispatch through .fn_ptr
            if self.closure_vars.borrow().contains(name.as_str()) {
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                let arg_types: Vec<String> = args
                    .iter()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .collect();
                let mut cast_params = vec!["void*".to_string()];
                cast_params.extend(arg_types);
                let cast = format!("int64_t (*)({})", cast_params.join(", "));
                let mut call_args = vec![format!("{name}.env")];
                call_args.extend(arg_exprs);
                return format!("(({cast})({name}.fn_ptr))({})", call_args.join(", "));
            }
        }

        // Check if callee is a Path expression (static method call like Point.origin())
        if let Expr::Path { segments } = &callee.node {
            if segments.len() == 2 {
                let type_name = &segments[0].node;
                let method_name = &segments[1].node;

                // Handle Box.new(value) → heap allocation
                if type_name == "Box" && method_name == "new" {
                    if let Some(arg) = args.first() {
                        let inner = self.gen_expr(&arg.node.value);
                        let inner_type = self.box_inner_c_type(&arg.node.value.node);
                        return format!(
                            "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                        );
                    }
                }

                // Handle File.open(path) and File.create(path)
                if type_name == "File" {
                    match method_name.as_str() {
                        "open" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_expr(&arg.node.value);
                                return format!("gorget_file_open({path_arg}, \"r\")");
                            }
                        }
                        "create" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_expr(&arg.node.value);
                                return format!("gorget_file_open({path_arg}, \"w\")");
                            }
                        }
                        _ => {}
                    }
                }

                let mangled = c_mangle::mangle_method(type_name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
            }
        }

        // General function call
        let callee_str = self.gen_expr(callee);
        let callee_name = c_mangle::escape_keyword(&callee_str);
        let arg_exprs = self.resolve_call_args(callee, args);
        format!("{callee_name}({})", arg_exprs.join(", "))
    }

    /// Resolve call arguments: reorder named args to match param order and
    /// fill in default values for missing optional params.
    fn resolve_call_args(
        &self,
        callee: &Spanned<Expr>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> Vec<String> {
        let has_named = args.iter().any(|a| a.node.name.is_some());
        let func_info = if let Expr::Identifier(_) = &callee.node {
            self.resolution_map.get(&callee.span.start)
                .and_then(|def_id| self.function_info.get(def_id))
        } else {
            None
        };
        let has_defaults = func_info.map_or(false, |fi| fi.param_defaults.iter().any(|d| d.is_some()));

        if (!has_named && !has_defaults) || func_info.is_none() {
            // Simple positional — original behavior
            return args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
        }

        let fi = func_info.unwrap();
        let param_names = &fi.param_names;
        let param_defaults = &fi.param_defaults;

        // Build a slot for each param
        let mut slots: Vec<Option<String>> = vec![None; param_names.len()];

        // Place positional args first, then named args
        let mut positional_idx = 0;
        for arg in args {
            if let Some(ref name) = arg.node.name {
                if let Some(pos) = param_names.iter().position(|pn| pn == &name.node) {
                    slots[pos] = Some(self.gen_expr(&arg.node.value));
                }
            } else {
                if positional_idx < slots.len() {
                    slots[positional_idx] = Some(self.gen_expr(&arg.node.value));
                }
                positional_idx += 1;
            }
        }

        // Fill missing slots with defaults
        for (i, slot) in slots.iter_mut().enumerate() {
            if slot.is_none() {
                if let Some(Some(default_expr)) = param_defaults.get(i) {
                    *slot = Some(self.gen_expr(default_expr));
                }
            }
        }

        slots.into_iter().map(|s| s.unwrap_or_else(|| "0".to_string())).collect()
    }

    /// Generate a method call: `receiver.method(args)` → `Type__method(&receiver, args)`
    fn gen_method_call(
        &self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        // Check if receiver is a trait object → vtable dispatch
        if let Some(_trait_name) = self.resolve_trait_object_type(receiver) {
            let recv = self.gen_expr(receiver);
            let mut all_args = vec![format!("{recv}.data")];
            for arg in args {
                all_args.push(self.gen_expr(&arg.node.value));
            }
            return format!("{recv}.vtable->{method_name}({})", all_args.join(", "));
        }

        // Check if receiver is a built-in collection or primitive type
        if let Some(builtin_code) = self.try_gen_builtin_method(receiver, method_name, args) {
            return builtin_code;
        }

        // Check if receiver is a type name (static method call like Point.origin())
        if let Expr::Identifier(name) = &receiver.node {
            // Handle File.open(path) and File.create(path) as static constructors
            if name == "File" {
                match method_name {
                    "open" => {
                        if let Some(arg) = args.first() {
                            let path_arg = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"r\")");
                        }
                    }
                    "create" => {
                        if let Some(arg) = args.first() {
                            let path_arg = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"w\")");
                        }
                    }
                    _ => {}
                }
            }

            let is_type = self
                .resolution_map
                .get(&receiver.span.start)
                .map(|def_id| self.scopes.get_def(*def_id))
                .or_else(|| {
                    self.scopes
                        .lookup_by_name_anywhere(name)
                        .map(|def_id| self.scopes.get_def(def_id))
                })
                .map_or(false, |def| {
                    matches!(def.kind, DefKind::Struct | DefKind::Enum)
                });

            if is_type {
                let mangled = c_mangle::mangle_method(name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
            }
        }

        let recv = self.gen_expr(receiver);
        // Try to figure out the receiver type for mangling
        let type_name = self.infer_receiver_type(receiver);
        // Check if this method comes from a trait impl (not inherent)
        let mangled = if let Some(trait_name) = self.find_trait_for_method(&type_name, method_name) {
            c_mangle::mangle_trait_method(&trait_name, &type_name, method_name)
        } else {
            c_mangle::mangle_method(&type_name, method_name)
        };

        // For non-lvalue receivers (e.g. function calls), we can't take `&recv`
        // directly. Use a GCC statement expression to stash the result in a temp.
        let needs_temp = !is_lvalue(&receiver.node);
        // Inside a method body, `self` is already a pointer (const T* self),
        // so pass it directly instead of taking &self.
        let is_self_ptr = self.current_self_type.is_some() && matches!(receiver.node, Expr::SelfExpr);
        if needs_temp {
            let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            let mut call_args = format!("&__recv");
            for a in &arg_exprs {
                call_args.push_str(", ");
                call_args.push_str(a);
            }
            format!("({{ __typeof__({recv}) __recv = {recv}; {mangled}({call_args}); }})")
        } else {
            let self_arg = if is_self_ptr {
                recv.clone()
            } else {
                format!("&{recv}")
            };
            let mut all_args = vec![self_arg];
            for arg in args {
                all_args.push(self.gen_expr(&arg.node.value));
            }
            format!("{mangled}({})", all_args.join(", "))
        }
    }

    /// Try to generate code for a built-in method call on a collection or primitive type.
    /// Returns `Some(code)` if the receiver is a known built-in type, `None` otherwise.
    fn try_gen_builtin_method(
        &self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> Option<String> {
        let type_name = self.infer_receiver_type(receiver);

        // Also check the C-level type for cases where infer_receiver_type
        // returns the Gorget name vs the C type
        let c_type = self.infer_receiver_c_type(receiver);

        let is_vector = matches!(type_name.as_str(), "Vector" | "List" | "Array")
            || c_type.as_deref() == Some("GorgetArray");
        let is_map = matches!(type_name.as_str(), "Dict" | "HashMap" | "Map")
            || c_type.as_deref().map_or(false, |t| t.starts_with("GorgetMap__"));
        let is_set = matches!(type_name.as_str(), "Set" | "HashSet")
            || c_type.as_deref() == Some("GorgetSet");
        let is_string = matches!(type_name.as_str(), "str" | "String")
            || matches!(c_type.as_deref(), Some("const char*"));
        let is_option = type_name == "Option";
        let is_result = type_name == "Result";
        let is_box = type_name == "Box";
        let is_file = type_name == "File" || c_type.as_deref() == Some("GorgetFile");
        let is_iterator = !is_vector && !is_map && !is_set && !is_string
            && !is_option && !is_result && !is_box && !is_file
            && matches!(method_name, "collect" | "filter" | "map" | "fold")
            && self.traits.impls.iter().any(|i|
                i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator")
            );

        if !is_vector && !is_map && !is_set && !is_string && !is_option && !is_result && !is_box && !is_file && !is_iterator {
            return None;
        }

        let recv = self.gen_expr(receiver);
        let needs_temp = !is_lvalue(&receiver.node);

        if is_iterator {
            return Some(self.gen_iterator_method(&recv, method_name, args, receiver, &type_name));
        }
        if is_vector {
            return Some(self.gen_vector_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_map {
            return Some(self.gen_map_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_set {
            return Some(self.gen_set_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_string {
            return self.gen_string_method(&recv, method_name, args, needs_temp);
        }
        if is_option {
            return Some(self.gen_option_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_result {
            return Some(self.gen_result_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_box {
            return Some(self.gen_box_method(&recv, method_name, args));
        }
        if is_file {
            return Some(self.gen_file_method(&recv, method_name, args, needs_temp));
        }

        None
    }

    /// Generate code for `needle in collection` expressions.
    /// Desugars to the appropriate `.contains()` call per collection type.
    fn gen_in_operator(&self, needle: &Spanned<Expr>, collection: &Spanned<Expr>) -> String {
        let type_name = self.infer_receiver_type(collection);
        let c_type = self.infer_receiver_c_type(collection);

        let is_vector = matches!(type_name.as_str(), "Vector" | "List" | "Array")
            || c_type.as_deref() == Some("GorgetArray");
        let is_map = matches!(type_name.as_str(), "Dict" | "HashMap" | "Map")
            || c_type.as_deref().map_or(false, |t| t.starts_with("GorgetMap__"));
        let is_set = matches!(type_name.as_str(), "Set" | "HashSet")
            || c_type.as_deref() == Some("GorgetSet");
        let is_string = matches!(type_name.as_str(), "str" | "String")
            || matches!(c_type.as_deref(), Some("const char*"));

        let coll = self.gen_expr(collection);
        let elem = self.gen_expr(needle);
        let needs_temp = !is_lvalue(&collection.node);

        let coll_ref = if needs_temp {
            format!("({{ __typeof__({coll}) __recv = {coll}; &__recv; }})")
        } else {
            format!("&{coll}")
        };

        if is_vector {
            let elem_type = self.infer_vector_elem_type(collection);
            format!("({{ {elem_type} __needle = {elem}; gorget_array_contains({coll_ref}, &__needle, sizeof({elem_type})); }})")
        } else if is_map {
            let (key_type, val_type) = self.infer_map_kv_types(collection);
            let mangled = c_mangle::mangle_generic("GorgetMap", &[key_type, val_type]);
            format!("{mangled}__contains({coll_ref}, {elem})")
        } else if is_set {
            let elem_type = self.infer_c_type_from_expr(&needle.node);
            format!("({{ {elem_type} __needle = {elem}; gorget_set_contains({coll_ref}, &__needle); }})")
        } else if is_string {
            format!("(strstr({coll}, {elem}) != NULL)")
        } else {
            format!("/* unsupported `in` for type {type_name} */ false")
        }
    }

    /// Canonical TypeId resolution for any expression.
    /// Handles Identifier, literals, BinaryOp, UnaryOp, Call, MethodCall, Deref, FieldAccess.
    pub(super) fn resolve_expr_type_id(
        &self,
        expr: &Spanned<Expr>,
    ) -> Option<crate::semantic::ids::TypeId> {
        match &expr.node {
            Expr::Identifier(name) => {
                self.resolution_map
                    .get(&expr.span.start)
                    .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                    .or_else(|| {
                        self.scopes
                            .lookup_by_name_anywhere(name)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                    })
            }
            Expr::IntLiteral(_) => Some(self.types.int_id),
            Expr::FloatLiteral(_) => Some(self.types.float_id),
            Expr::BoolLiteral(_) => Some(self.types.bool_id),
            Expr::StringLiteral(_) => Some(self.types.string_id),
            Expr::BinaryOp { op, left, .. } => match op {
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::LtEq
                | BinaryOp::GtEq
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::In => Some(self.types.bool_id),
                _ => self.resolve_expr_type_id(left),
            },
            Expr::UnaryOp { operand, .. } => self.resolve_expr_type_id(operand),
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    let def_id = self
                        .resolution_map
                        .get(&callee.span.start)
                        .copied()
                        .or_else(|| self.scopes.lookup_by_name_anywhere(name));
                    if let Some(did) = def_id {
                        if let Some(fi) = self.function_info.get(&did) {
                            return fi.return_type_id;
                        }
                    }
                }
                None
            }
            Expr::MethodCall {
                receiver, method, ..
            } => {
                let recv_c_type = self.infer_receiver_c_type(receiver);
                if let Some(tid) = recv_c_type
                    .as_deref()
                    .and_then(|rt| self.builtin_method_return_type(rt, &method.node))
                {
                    return Some(tid);
                }
                let type_name = self.infer_receiver_type(receiver);
                for impl_info in &self.traits.impls {
                    if impl_info.self_type_name == type_name {
                        if let Some((_def_id, sig)) =
                            impl_info.methods.get(method.node.as_str())
                        {
                            return Some(sig.return_type);
                        }
                    }
                }
                None
            }
            Expr::Deref { expr: inner } => {
                let inner_tid = self.resolve_expr_type_id(inner)?;
                if let crate::semantic::types::ResolvedType::Generic(def_id, args) =
                    self.types.get(inner_tid)
                {
                    if self.scopes.get_def(*def_id).name == "Box" && args.len() == 1 {
                        return Some(args[0]);
                    }
                }
                None
            }
            Expr::FieldAccess { object, field } => {
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type, field.node.clone());
                    if let Some(ast_type) = self.field_type_names.get(&key) {
                        return self.ast_type_to_type_id(ast_type);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Resolve the mangled C enum type name for a match scrutinee.
    /// For generic enums (e.g. `Option[int]`), returns the mangled name (`Option__int64_t`).
    /// For non-generic enums, returns the raw name. Returns `None` for non-enum types.
    pub(super) fn resolve_enum_c_type_for_scrutinee(
        &self,
        scrutinee: &Spanned<Expr>,
    ) -> Option<String> {
        let type_id = self.resolve_expr_type_id(scrutinee)?;
        match self.types.get(type_id) {
            crate::semantic::types::ResolvedType::Generic(def_id, args) => {
                let base = c_types::def_name_to_c(*def_id, self.scopes);
                let c_args: Vec<String> = args
                    .iter()
                    .map(|tid| c_types::type_id_to_c(*tid, self.types, self.scopes))
                    .collect();
                Some(c_mangle::mangle_generic(&base, &c_args))
            }
            crate::semantic::types::ResolvedType::Defined(def_id) => {
                Some(c_types::def_name_to_c(*def_id, self.scopes))
            }
            _ => None,
        }
    }

    /// Infer the C type of a receiver expression via the TypeId, if available.
    pub(super) fn infer_receiver_c_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            return Some(c_types::type_id_to_c(tid, self.types, self.scopes));
        }
        if let Expr::FieldAccess { object, field } = &expr.node {
            let obj_type = self.infer_receiver_type(object);
            if obj_type != "Unknown" {
                let key = (obj_type, field.node.clone());
                if let Some(field_type) = self.field_type_names.get(&key) {
                    return Some(c_types::ast_type_to_c(field_type, self.scopes));
                }
            }
        }
        None
    }

    /// Infer the element C type for a Vector receiver from its TypeId.
    pub(super) fn infer_vector_elem_type(&self, receiver: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if let Some(&elem_tid) = args.first() {
                    return c_types::type_id_to_c(elem_tid, self.types, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Compute the mangled GorgetMap name for a Dict receiver.
    fn infer_map_mangled(&self, receiver: &Spanned<Expr>) -> String {
        let (key_type, val_type) = self.infer_map_kv_types(receiver);
        c_mangle::mangle_generic("GorgetMap", &[key_type, val_type])
    }

    /// Infer the key and value C types for a Map receiver from its TypeId.
    pub(super) fn infer_map_kv_types(&self, receiver: &Spanned<Expr>) -> (String, String) {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if args.len() >= 2 {
                    let key = c_types::type_id_to_c(args[0], self.types, self.scopes);
                    let val = c_types::type_id_to_c(args[1], self.types, self.scopes);
                    return (key, val);
                }
            }
        }
        ("int64_t".to_string(), "int64_t".to_string())
    }

    /// Generate code for Vector method calls.
    fn gen_vector_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let elem_type = self.infer_vector_elem_type(receiver);

        // For methods that need &recv, wrap non-lvalue receivers in a temp
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        match method_name {
            "push" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __push_val = {arg}; gorget_array_push({recv_ref}, &__push_val); }})")
            }
            "len" => {
                format!("(int64_t)gorget_array_len({recv_ref})")
            }
            "get" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; GORGET_ARRAY_AT({elem_type}, __recv, {idx}); }})")
                } else {
                    format!("GORGET_ARRAY_AT({elem_type}, {recv}, {idx})")
                }
            }
            "pop" => {
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; {elem_type} __popped = GORGET_ARRAY_AT({elem_type}, __recv, __recv.len - 1); __recv.len--; __popped; }})")
                } else {
                    format!("({{ {elem_type} __popped = GORGET_ARRAY_AT({elem_type}, {recv}, {recv}.len - 1); {recv}.len--; __popped; }})")
                }
            }
            "set" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __set_val = {val}; gorget_array_set({recv_ref}, {idx}, &__set_val); }})")
            }
            "remove" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; {elem_type} __removed = GORGET_ARRAY_AT({elem_type}, __recv, {idx}); gorget_array_remove(&__recv, {idx}); __removed; }})")
                } else {
                    format!("({{ {elem_type} __removed = GORGET_ARRAY_AT({elem_type}, {recv}, {idx}); gorget_array_remove(&{recv}, {idx}); __removed; }})")
                }
            }
            "clear" => {
                format!("gorget_array_clear({recv_ref})")
            }
            "is_empty" => {
                format!("(gorget_array_len({recv_ref}) == 0)")
            }
            "contains" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __needle = {arg}; gorget_array_contains({recv_ref}, &__needle, sizeof({elem_type})); }})")
            }
            "reserve" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("gorget_array_reserve({recv_ref}, (size_t)({arg}))")
            }
            "filter" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type("bool");
                format!(
                    "({{ GorgetArray __filt_src = {recv}; \
                    GorgetArray __filt_result = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __filt_i = 0; __filt_i < __filt_src.len; __filt_i++) {{ \
                        {elem_type} __filt_elem = GORGET_ARRAY_AT({elem_type}, __filt_src, __filt_i); \
                        if ({closure_fn}(__filt_elem)) {{ gorget_array_push(&__filt_result, &__filt_elem); }} \
                    }} \
                    __filt_result; }})"
                )
            }
            "map" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.patch_last_closure_return_type(&body_c_type);
                format!(
                    "({{ GorgetArray __map_src = {recv}; \
                    GorgetArray __map_result = gorget_array_new(sizeof({body_c_type})); \
                    for (size_t __map_i = 0; __map_i < __map_src.len; __map_i++) {{ \
                        {elem_type} __map_elem = GORGET_ARRAY_AT({elem_type}, __map_src, __map_i); \
                        {body_c_type} __map_out = {closure_fn}(__map_elem); \
                        gorget_array_push(&__map_result, &__map_out); \
                    }} \
                    __map_result; }})"
                )
            }
            "fold" => {
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                let closure_fn = self.gen_expr(&args[1].node.value);
                self.patch_last_closure_return_type(&init_c_type);
                format!(
                    "({{ GorgetArray __fold_src = {recv}; \
                    {init_c_type} __fold_acc = {init}; \
                    for (size_t __fold_i = 0; __fold_i < __fold_src.len; __fold_i++) {{ \
                        {elem_type} __fold_elem = GORGET_ARRAY_AT({elem_type}, __fold_src, __fold_i); \
                        __fold_acc = {closure_fn}(__fold_acc, __fold_elem); \
                    }} \
                    __fold_acc; }})"
                )
            }
            "reduce" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type(&elem_type);
                format!(
                    "({{ GorgetArray __red_src = {recv}; \
                    {elem_type} __red_acc = GORGET_ARRAY_AT({elem_type}, __red_src, 0); \
                    for (size_t __red_i = 1; __red_i < __red_src.len; __red_i++) {{ \
                        {elem_type} __red_elem = GORGET_ARRAY_AT({elem_type}, __red_src, __red_i); \
                        __red_acc = {closure_fn}(__red_acc, __red_elem); \
                    }} \
                    __red_acc; }})"
                )
            }
            _ => {
                // Unknown method — fall through to normal dispatch
                format!("/* unknown Vector method {method_name} */ 0")
            }
        }
    }

    /// Generate code for Iterator adapter method calls (collect, filter, map, fold).
    /// Uses the while/next/break pattern from for-loop iterator codegen.
    fn gen_iterator_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        type_name: &str,
    ) -> String {
        // Get the element C type from the Iterator[T] impl
        let elem_c_type = self.get_iterator_elem_c_type(receiver);

        // Register Option[elem_c_type] so the typedef is emitted
        let option_mangled = self.register_generic(
            "Option",
            &[elem_c_type.clone()],
            super::GenericInstanceKind::Enum,
        );
        let tag_none = super::c_mangle::mangle_tag(&option_mangled, "None");
        let next_fn = super::c_mangle::mangle_trait_method("Iterator", type_name, "next");

        match method_name {
            "collect" => {
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({elem_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        gorget_array_push(&__it_result, &__it_elem); \
                    }} \
                    __it_result; }})"
                )
            }
            "filter" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type("bool");
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({elem_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        if ({closure_fn}(__it_elem)) {{ gorget_array_push(&__it_result, &__it_elem); }} \
                    }} \
                    __it_result; }})"
                )
            }
            "map" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.patch_last_closure_return_type(&body_c_type);
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({body_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        {body_c_type} __it_out = {closure_fn}(__it_elem); \
                        gorget_array_push(&__it_result, &__it_out); \
                    }} \
                    __it_result; }})"
                )
            }
            "fold" => {
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                let closure_fn = self.gen_expr(&args[1].node.value);
                self.patch_last_closure_return_type(&init_c_type);
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    {init_c_type} __it_acc = {init}; \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        __it_acc = {closure_fn}(__it_acc, __it_elem); \
                    }} \
                    __it_acc; }})"
                )
            }
            _ => format!("/* unknown Iterator method {method_name} */ 0"),
        }
    }

    /// Generate code for Map/Dict method calls.
    fn gen_map_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let mangled = self.infer_map_mangled(receiver);
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        match method_name {
            "put" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__put({recv_ref}, {key}, {val})")
            }
            "get" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("*{mangled}__get_ptr({recv_ref}, {key})")
            }
            "contains" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__contains({recv_ref}, {key})")
            }
            "len" => {
                format!("(int64_t){recv}.count")
            }
            "remove" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__remove({recv_ref}, {key})")
            }
            "clear" => {
                format!("{mangled}__clear({recv_ref})")
            }
            "is_empty" => {
                format!("({recv}.count == 0)")
            }
            "filter" => {
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type("bool");
                format!(
                    "({{ {mangled} __dfilt_src = {recv}; \
                    {mangled} __dfilt_result = {mangled}__new(); \
                    for (size_t __dfilt_i = 0; __dfilt_i < __dfilt_src.cap; __dfilt_i++) {{ \
                        if (__dfilt_src.states[__dfilt_i] != 1) continue; \
                        {key_type} __dfilt_k = __dfilt_src.keys[__dfilt_i]; \
                        {val_type} __dfilt_v = __dfilt_src.values[__dfilt_i]; \
                        if ({closure_fn}(__dfilt_k, __dfilt_v)) {{ {mangled}__put(&__dfilt_result, __dfilt_k, __dfilt_v); }} \
                    }} \
                    __dfilt_result; }})"
                )
            }
            "fold" => {
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                let closure_fn = self.gen_expr(&args[1].node.value);
                self.patch_last_closure_return_type(&init_c_type);
                format!(
                    "({{ {mangled} __dfold_src = {recv}; \
                    {init_c_type} __dfold_acc = {init}; \
                    for (size_t __dfold_i = 0; __dfold_i < __dfold_src.cap; __dfold_i++) {{ \
                        if (__dfold_src.states[__dfold_i] != 1) continue; \
                        {key_type} __dfold_k = __dfold_src.keys[__dfold_i]; \
                        {val_type} __dfold_v = __dfold_src.values[__dfold_i]; \
                        __dfold_acc = {closure_fn}(__dfold_acc, __dfold_k, __dfold_v); \
                    }} \
                    __dfold_acc; }})"
                )
            }
            _ => {
                format!("/* unknown Dict method {method_name} */ 0")
            }
        }
    }

    /// Generate code for Set method calls.
    fn gen_set_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        match method_name {
            "add" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_add({recv_ref}, &__e); }})")
            }
            "contains" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_contains({recv_ref}, &__e); }})")
            }
            "len" => {
                format!("(int64_t)gorget_set_len({recv_ref})")
            }
            "remove" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_remove({recv_ref}, &__e); }})")
            }
            "clear" => {
                format!("gorget_set_clear({recv_ref})")
            }
            "is_empty" => {
                format!("(gorget_set_len({recv_ref}) == 0)")
            }
            "filter" => {
                // Set elem type is the first generic type arg, same as Vector
                let elem_type = self.infer_vector_elem_type(receiver);
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type("bool");
                format!(
                    "({{ GorgetSet __sfilt_src = {recv}; \
                    GorgetSet __sfilt_result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __sfilt_i = 0; __sfilt_i < __sfilt_src.cap; __sfilt_i++) {{ \
                        if (__sfilt_src.states[__sfilt_i] != 1) continue; \
                        {elem_type} __sfilt_elem = *({elem_type}*)((char*)__sfilt_src.keys + __sfilt_i * __sfilt_src.key_size); \
                        if ({closure_fn}(__sfilt_elem)) {{ gorget_set_add(&__sfilt_result, &__sfilt_elem); }} \
                    }} \
                    __sfilt_result; }})"
                )
            }
            "fold" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                let closure_fn = self.gen_expr(&args[1].node.value);
                self.patch_last_closure_return_type(&init_c_type);
                format!(
                    "({{ GorgetSet __sfold_src = {recv}; \
                    {init_c_type} __sfold_acc = {init}; \
                    for (size_t __sfold_i = 0; __sfold_i < __sfold_src.cap; __sfold_i++) {{ \
                        if (__sfold_src.states[__sfold_i] != 1) continue; \
                        {elem_type} __sfold_elem = *({elem_type}*)((char*)__sfold_src.keys + __sfold_i * __sfold_src.key_size); \
                        __sfold_acc = {closure_fn}(__sfold_acc, __sfold_elem); \
                    }} \
                    __sfold_acc; }})"
                )
            }
            _ => {
                format!("/* unknown Set method {method_name} */ 0")
            }
        }
    }

    /// Generate code for String method calls.
    fn gen_string_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        needs_temp: bool,
    ) -> Option<String> {
        match method_name {
            "len" => {
                if needs_temp {
                    Some(format!("({{ const char* __s = {recv}; (int64_t)strlen(__s); }})"))
                } else {
                    Some(format!("(int64_t)strlen({recv})"))
                }
            }
            "contains" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("(strstr({recv}, {arg}) != NULL)"))
            }
            "starts_with" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_starts_with({recv}, {arg})"))
            }
            "ends_with" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_ends_with({recv}, {arg})"))
            }
            "is_empty" => {
                Some(format!("(strlen({recv}) == 0)"))
            }
            "trim" => {
                Some(format!("gorget_string_trim({recv})"))
            }
            "to_upper" => {
                Some(format!("gorget_string_to_upper({recv})"))
            }
            "to_lower" => {
                Some(format!("gorget_string_to_lower({recv})"))
            }
            "replace" => {
                let old_arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                let new_arg = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_replace({recv}, {old_arg}, {new_arg})"))
            }
            "split" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_split({recv}, {arg})"))
            }
            _ => None, // Not a known string method — fall through
        }
    }


    /// Generate code for Box built-in methods: get(), set().
    fn gen_box_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        match method_name {
            "get" => format!("(*{recv})"),
            "set" => {
                if let Some(arg) = args.first() {
                    let val = self.gen_expr(&arg.node.value);
                    format!("({{ *{recv} = {val}; }})")
                } else {
                    format!("/* Box.set() missing arg */")
                }
            }
            "new" => {
                // Box.new(value) as a method call (when Box is the receiver)
                if let Some(arg) = args.first() {
                    let inner = self.gen_expr(&arg.node.value);
                    let inner_type = self.box_inner_c_type(&arg.node.value.node);
                    format!(
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                    )
                } else {
                    format!("/* Box.new() missing arg */")
                }
            }
            _ => format!("/* unknown Box method: {method_name} */"),
        }
    }

    /// Generate code for File instance method calls.
    fn gen_file_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        needs_temp: bool,
    ) -> String {
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __tmp = {recv}; &__tmp; }})")
        } else {
            format!("&{recv}")
        };
        match method_name {
            // Static constructors (File.open / File.create)
            "open" => {
                if let Some(arg) = args.first() {
                    let path_arg = self.gen_expr(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"r\")")
                } else {
                    format!("/* File.open() missing arg */")
                }
            }
            "create" => {
                if let Some(arg) = args.first() {
                    let path_arg = self.gen_expr(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"w\")")
                } else {
                    format!("/* File.create() missing arg */")
                }
            }
            // Instance methods
            "read_all" => format!("gorget_file_read_all({recv_ref})"),
            "write" => {
                if let Some(arg) = args.first() {
                    let content = self.gen_expr(&arg.node.value);
                    format!("gorget_file_write({recv_ref}, {content})")
                } else {
                    format!("/* File.write() missing arg */")
                }
            }
            "close" => format!("gorget_file_close({recv_ref})"),
            _ => format!("/* unknown File method: {method_name} */"),
        }
    }

    /// Check if an expression has Box type (for deciding whether to generate a real dereference).
    fn is_box_expr(&self, expr: &Spanned<Expr>) -> bool {
        if let Expr::Identifier(name) = &expr.node {
            let type_id = self.resolution_map.get(&expr.span.start)
                .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                .or_else(|| {
                    self.scopes.lookup_by_name_anywhere(name)
                        .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                });
            if let Some(tid) = type_id {
                if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                    return self.scopes.get_def(*def_id).name == "Box";
                }
            }
        }
        false
    }

    /// Infer the inner C type for a Box allocation.
    /// Uses `decl_type_hint` to extract T from `Box[T]`, falling back to the argument's inferred type.
    fn box_inner_c_type(&self, arg_expr: &Expr) -> String {
        let hint = self.decl_type_hint.borrow();
        if let Some(Type::Named { name, generic_args }) = hint.as_ref() {
            if name.node == "Box" && generic_args.len() == 1 {
                return c_types::ast_type_to_c(&generic_args[0].node, self.scopes);
            }
        }
        // Fallback: infer from the argument expression
        self.infer_c_type_from_expr(arg_expr)
    }

    /// Resolve a unit variant constructor using the decl_type_hint.
    /// Returns the monomorphized enum name if the hint matches the expected enum.
    fn resolve_unit_variant_from_type_hint(&self, enum_name: &str, _variant_name: &str) -> Option<String> {
        let hint = self.decl_type_hint.borrow();
        if let Some(Type::Named { name, generic_args }) = hint.as_ref() {
            if name.node == enum_name && !generic_args.is_empty() {
                let c_type_args: Vec<String> = generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect();
                let mangled = self.register_generic(enum_name, &c_type_args, super::GenericInstanceKind::Enum);
                return Some(mangled);
            }
        }
        None
    }

    /// Infer the monomorphized C type name for a generic enum receiver (e.g. `Option__int64_t`).
    fn infer_generic_enum_mangled_name(&self, receiver: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, args) = self.types.get(tid) {
                let base = self.scopes.get_def(*def_id).name.clone();
                let c_args: Vec<String> = args.iter()
                    .map(|&a| c_types::type_id_to_c(a, self.types, self.scopes))
                    .collect();
                return c_mangle::mangle_generic(&base, &c_args);
            }
        }
        // Fallback: use the receiver C type directly
        self.infer_receiver_c_type(receiver).unwrap_or_else(|| "Option__int64_t".to_string())
    }

    /// Extract the C return type from a closure's body expression.
    fn infer_closure_body_c_type(&self, arg: &Spanned<Expr>) -> String {
        if let Expr::Closure { body, .. } = &arg.node {
            return self.infer_c_type_from_expr(&body.node);
        }
        "int64_t".to_string()
    }

    /// Patch the return type of the most recently lifted closure.
    fn patch_last_closure_return_type(&self, return_type: &str) {
        let mut closures = self.lifted_closures.borrow_mut();
        if let Some(last) = closures.last_mut() {
            last.return_type = return_type.to_string();
        }
    }

    /// Extract the C type args from a generic receiver expression.
    fn infer_generic_type_args(&self, receiver: &Spanned<Expr>) -> Vec<String> {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                return args.iter()
                    .map(|&a| c_types::type_id_to_c(a, self.types, self.scopes))
                    .collect();
            }
        }
        vec![]
    }

    /// Generate code for Option method calls.
    fn gen_option_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let mangled = self.infer_generic_enum_mangled_name(receiver);
        let tag_some = c_mangle::mangle_tag(&mangled, "Some");
        let tag_none = c_mangle::mangle_tag(&mangled, "None");

        match method_name {
            "unwrap" => {
                format!("({{ {mangled} __opt = {recv}; if (__opt.tag == {tag_none}) {{ fprintf(stderr, \"unwrap called on None\\n\"); exit(1); }} __opt.data.Some._0; }})")
            }
            "unwrap_or" => {
                let default = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt.data.Some._0 : {default}; }})")
            }
            "is_some" => {
                if needs_temp {
                    format!("({{ {mangled} __opt = {recv}; __opt.tag == {tag_some}; }})")
                } else {
                    format!("{recv}.tag == {tag_some}")
                }
            }
            "is_none" => {
                if needs_temp {
                    format!("({{ {mangled} __opt = {recv}; __opt.tag == {tag_none}; }})")
                } else {
                    format!("{recv}.tag == {tag_none}")
                }
            }
            "map" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                // Determine closure body return C type for output Option type
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                // Patch the lifted closure's return type
                self.patch_last_closure_return_type(&body_c_type);
                // Register output Option type (may be same as input for same-type map)
                let output_mangled = self.register_generic("Option", &[body_c_type], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&output_mangled, "Some");
                let ctor_none = c_mangle::mangle_variant(&output_mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; {output_mangled} __result; \
                    if (__opt.tag == {tag_some}) {{ __result = {ctor_some}({closure_fn}(__opt.data.Some._0)); }} \
                    else {{ __result = {ctor_none}(); }} \
                    __result; }})"
                )
            }
            "and_then" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                // Closure returns full Option type — patch return type
                self.patch_last_closure_return_type(&mangled);
                let ctor_none = c_mangle::mangle_variant(&mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; {mangled} __result; \
                    if (__opt.tag == {tag_some}) {{ __result = {closure_fn}(__opt.data.Some._0); }} \
                    else {{ __result = {ctor_none}(); }} \
                    __result; }})"
                )
            }
            "or_else" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                // Closure returns full Option type — patch return type
                self.patch_last_closure_return_type(&mangled);
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt : {closure_fn}(); }})"
                )
            }
            _ => format!("/* unknown Option method {method_name} */ 0"),
        }
    }

    /// Generate code for Result method calls.
    fn gen_result_method(
        &self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let mangled = self.infer_generic_enum_mangled_name(receiver);
        let tag_ok = c_mangle::mangle_tag(&mangled, "Ok");
        let tag_error = c_mangle::mangle_tag(&mangled, "Error");

        match method_name {
            "unwrap" => {
                format!("({{ {mangled} __res = {recv}; if (__res.tag == {tag_error}) {{ fprintf(stderr, \"unwrap called on Error\\n\"); exit(1); }} __res.data.Ok._0; }})")
            }
            "unwrap_or" => {
                let default = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res.data.Ok._0 : {default}; }})")
            }
            "is_ok" => {
                if needs_temp {
                    format!("({{ {mangled} __res = {recv}; __res.tag == {tag_ok}; }})")
                } else {
                    format!("{recv}.tag == {tag_ok}")
                }
            }
            "is_err" => {
                if needs_temp {
                    format!("({{ {mangled} __res = {recv}; __res.tag == {tag_error}; }})")
                } else {
                    format!("{recv}.tag == {tag_error}")
                }
            }
            "map" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.patch_last_closure_return_type(&body_c_type);
                // Result[T,E].map() -> Result[U,E]: need E type for output
                let type_args = self.infer_generic_type_args(receiver);
                let error_c_type = type_args.get(1).cloned().unwrap_or_else(|| "int64_t".to_string());
                let output_mangled = self.register_generic("Result", &[body_c_type, error_c_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_ok = c_mangle::mangle_variant(&output_mangled, "Ok");
                let ctor_error = c_mangle::mangle_variant(&output_mangled, "Error");
                format!(
                    "({{ {mangled} __res = {recv}; {output_mangled} __result; \
                    if (__res.tag == {tag_ok}) {{ __result = {ctor_ok}({closure_fn}(__res.data.Ok._0)); }} \
                    else {{ __result = {ctor_error}(__res.data.Error._0); }} \
                    __result; }})"
                )
            }
            "and_then" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type(&mangled);
                let ctor_error = c_mangle::mangle_variant(&mangled, "Error");
                format!(
                    "({{ {mangled} __res = {recv}; {mangled} __result; \
                    if (__res.tag == {tag_ok}) {{ __result = {closure_fn}(__res.data.Ok._0); }} \
                    else {{ __result = {ctor_error}(__res.data.Error._0); }} \
                    __result; }})"
                )
            }
            "or_else" => {
                let closure_fn = self.gen_expr(&args[0].node.value);
                self.patch_last_closure_return_type(&mangled);
                format!(
                    "({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res : {closure_fn}(__res.data.Error._0); }})"
                )
            }
            _ => format!("/* unknown Result method {method_name} */ 0"),
        }
    }

    /// Check if a receiver expression has a trait object type, returning the trait name if so.
    fn resolve_trait_object_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.resolution_map.get(&expr.span.start) {
                let def = self.scopes.get_def(*def_id);
                if let Some(type_id) = def.type_id {
                    if let crate::semantic::types::ResolvedType::TraitObject(trait_def_id) =
                        self.types.get(type_id)
                    {
                        return Some(self.scopes.get_def(*trait_def_id).name.clone());
                    }
                }
            }
            // Fallback: search all scopes (codegen doesn't track current scope)
            if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    if let crate::semantic::types::ResolvedType::TraitObject(trait_def_id) =
                        self.types.get(type_id)
                    {
                        return Some(self.scopes.get_def(*trait_def_id).name.clone());
                    }
                }
            }
        }
        None
    }

    /// Generate a generic function call: `func[int](args)` → `func__int64_t(args)`
    fn gen_generic_call(
        &self,
        callee: &Spanned<Expr>,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let c_type_args: Vec<String> = type_args
            .iter()
            .map(|a| self.type_to_c(&a.node))
            .collect();

        let base_name = match &callee.node {
            Expr::Identifier(name) => name.clone(),
            Expr::Path { segments } if segments.len() == 2 => {
                let type_name = &segments[0].node;
                let method_name = &segments[1].node;
                let mangled = c_mangle::mangle_method(type_name, method_name);
                let full_mangled = c_mangle::mangle_generic(&mangled, &c_type_args);
                self.register_generic(&mangled, &c_type_args, super::GenericInstanceKind::Function);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{full_mangled}({})", arg_exprs.join(", "));
            }
            _ => {
                let callee_str = self.gen_expr(callee);
                callee_str
            }
        };

        // Check if the callee is a built-in collection constructor
        match base_name.as_str() {
            "Vector" | "List" | "Array" => {
                let elem_size = if c_type_args.is_empty() {
                    "sizeof(int64_t)".to_string()
                } else {
                    format!("sizeof({})", c_type_args[0])
                };
                if args.len() == 1 {
                    // Vector[T](n) → with_capacity
                    let cap = self.gen_expr(&args[0].node.value);
                    return format!("gorget_array_with_capacity({elem_size}, (size_t)({cap}))");
                }
                return format!("gorget_array_new({elem_size})");
            }
            "Dict" | "HashMap" | "Map" => {
                let mangled = c_mangle::mangle_generic("GorgetMap", &c_type_args);
                self.register_generic("GorgetMap", &c_type_args, super::GenericInstanceKind::Map);
                return format!("{mangled}__new()");
            }
            "Set" | "HashSet" => {
                let elem_size = c_type_args.first()
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                return format!("gorget_set_new({elem_size})");
            }
            _ => {}
        }

        // Check if the callee is a generic struct constructor
        if self.generic_struct_templates.borrow().contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Struct);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        // Check if the callee is a generic enum variant constructor
        if self.generic_enum_templates.borrow().contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Enum);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        let mangled = c_mangle::mangle_generic(&base_name, &c_type_args);
        self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Function);
        let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
        format!("{mangled}({})", arg_exprs.join(", "))
    }

    /// Generate a generic method call: `obj.method[T](args)` → `Type__method__T(&obj, args)`
    fn gen_generic_method_call(
        &self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let recv = self.gen_expr(receiver);
        let type_name = self.infer_receiver_type(receiver);
        let c_type_args: Vec<String> = type_args
            .iter()
            .map(|a| self.type_to_c(&a.node))
            .collect();
        let base_method = c_mangle::mangle_method(&type_name, method_name);
        let mangled = c_mangle::mangle_generic(&base_method, &c_type_args);
        self.register_generic(&base_method, &c_type_args, super::GenericInstanceKind::Function);
        let mut all_args = vec![format!("&{recv}")];
        for arg in args {
            all_args.push(self.gen_expr(&arg.node.value));
        }
        format!("{mangled}({})", all_args.join(", "))
    }

    /// Map a `TypeId` to the Gorget type name (for mangling).
    fn type_name_from_type_id(&self, type_id: crate::semantic::ids::TypeId) -> Option<String> {
        match self.types.get(type_id) {
            crate::semantic::types::ResolvedType::Defined(tid) => {
                Some(self.scopes.get_def(*tid).name.clone())
            }
            crate::semantic::types::ResolvedType::Generic(tid, _) => {
                Some(self.scopes.get_def(*tid).name.clone())
            }
            _ => None,
        }
    }

    /// Try to infer the type name of a receiver expression (best-effort).
    /// Check if a method on a type comes from a trait equip (not inherent impl).
    /// Returns the trait name if found, None if it's an inherent method.
    fn find_trait_for_method(&self, type_name: &str, method_name: &str) -> Option<String> {
        // First check if there's an inherent impl with this method — inherent wins
        for impl_info in &self.traits.impls {
            if impl_info.self_type_name == type_name
                && impl_info.trait_.is_none()
                && impl_info.methods.contains_key(method_name)
            {
                return None; // Inherent method takes priority
            }
        }
        // Then check trait impls
        for impl_info in &self.traits.impls {
            if impl_info.self_type_name == type_name && impl_info.trait_.is_some() {
                // Check if the method is directly in the equip block
                if impl_info.methods.contains_key(method_name) {
                    return impl_info.trait_name.clone();
                }
                // Check if the trait (or its parents) defines this method with a default
                if let Some(trait_def_id) = impl_info.trait_ {
                    if self.trait_hierarchy_has_method(trait_def_id, method_name) {
                        return impl_info.trait_name.clone();
                    }
                }
            }
        }
        None
    }

    /// Check if a trait (or any of its parent traits) defines a method.
    fn trait_hierarchy_has_method(
        &self,
        trait_def_id: crate::semantic::ids::DefId,
        method_name: &str,
    ) -> bool {
        if let Some(trait_info) = self.traits.traits.get(&trait_def_id) {
            if trait_info.methods.contains_key(method_name) {
                return true;
            }
            for &parent_id in &trait_info.extends {
                if self.trait_hierarchy_has_method(parent_id, method_name) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn infer_receiver_type(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::Identifier(name) => {
                if let Some(type_id) = self.resolve_expr_type_id(expr) {
                    if let Some(name) = self.type_name_from_type_id(type_id) {
                        return name;
                    }
                }
                name.clone()
            }
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    // Try resolution_map first, then search all definitions
                    let def_id = self.resolution_map.get(&callee.span.start).copied()
                        .or_else(|| {
                            // lookup_by_name_anywhere only finds Variable/Const/Function,
                            // so search all definitions for struct/newtype/variant/function
                            self.scopes.lookup_by_name_anywhere(name)
                        })
                        .or_else(|| {
                            // Search struct_fields keys (which are DefIds of struct defs)
                            for (def_id, _) in self.struct_fields {
                                if self.scopes.get_def(*def_id).name == *name {
                                    return Some(*def_id);
                                }
                            }
                            None
                        });

                    if let Some(def_id) = def_id {
                        let def = self.scopes.get_def(def_id);
                        // Struct/newtype constructor — type is the callee name
                        if matches!(def.kind, DefKind::Struct | DefKind::Newtype) {
                            return name.clone();
                        }
                        // Enum variant constructor — type is the parent enum
                        if def.kind == DefKind::Variant {
                            for (enum_def_id, info) in self.enum_variants {
                                for (_, vid) in &info.variants {
                                    if *vid == def_id {
                                        return self.scopes.get_def(*enum_def_id).name.clone();
                                    }
                                }
                            }
                        }
                        // Function call — use return type
                        if let Some(func_info) = self.function_info.get(&def_id) {
                            if let Some(ret_type_id) = func_info.return_type_id {
                                if let Some(name) = self.type_name_from_type_id(ret_type_id) {
                                    return name;
                                }
                            }
                        }
                    }
                }
                "Unknown".to_string()
            }
            Expr::StructLiteral { name, .. } => name.node.clone(),
            Expr::SelfExpr => self
                .current_self_type
                .clone()
                .unwrap_or_else(|| "Self".to_string()),
            Expr::MethodCall { receiver, method, .. } => {
                let recv_type = self.infer_receiver_type(receiver);
                if recv_type == "Unknown" {
                    return "Unknown".to_string();
                }
                // Look up method return type in trait registry (inherent + trait impls)
                for impl_info in &self.traits.impls {
                    if impl_info.self_type_name == recv_type {
                        if let Some((_def_id, sig)) = impl_info.methods.get(method.node.as_str()) {
                            if let Some(name) = self.type_name_from_type_id(sig.return_type) {
                                return name;
                            }
                        }
                    }
                }
                "Unknown".to_string()
            }
            Expr::FieldAccess { object, field } => {
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type, field.node.clone());
                    if let Some(field_type) = self.field_type_names.get(&key) {
                        return Self::type_to_name(field_type);
                    }
                }
                "Unknown".to_string()
            }
            Expr::StringLiteral(_) => "str".to_string(),
            _ => "Unknown".to_string(),
        }
    }

    /// Extract the base type name from an AST Type.
    fn type_to_name(ty: &Type) -> String {
        match ty {
            Type::Named { name, .. } => name.node.clone(),
            Type::Primitive(p) => match p {
                PrimitiveType::Int | PrimitiveType::Int8 | PrimitiveType::Int16
                | PrimitiveType::Int32 | PrimitiveType::Int64 => "int".to_string(),
                PrimitiveType::Uint | PrimitiveType::Uint8 | PrimitiveType::Uint16
                | PrimitiveType::Uint32 | PrimitiveType::Uint64 => "uint".to_string(),
                PrimitiveType::Float | PrimitiveType::Float32 | PrimitiveType::Float64 => "float".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::Str | PrimitiveType::StringType => "str".to_string(),
                PrimitiveType::Char => "char".to_string(),
                PrimitiveType::Void => "void".to_string(),
            },
            _ => "Unknown".to_string(),
        }
    }

    /// Check if an expression evaluates to a string (const char*) type.
    pub(super) fn is_string_expr(&self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::StringLiteral(_) => true,
            Expr::Identifier(_) => {
                let type_name = self.infer_receiver_type(expr);
                if type_name == "str" {
                    return true;
                }
                let c_type = self.infer_receiver_c_type(expr);
                matches!(c_type.as_deref(), Some("const char*"))
            }
            Expr::MethodCall { .. } | Expr::Call { .. } => {
                let c_type = self.infer_c_type_from_expr(&expr.node);
                c_type == "const char*"
            }
            Expr::Index { object, index } => {
                // str[range] returns str
                self.is_string_expr(object) && matches!(&index.node, Expr::Range { .. })
            }
            _ => false,
        }
    }

    /// Check if an expression evaluates to a Vector (GorgetArray) type.
    fn is_vector_expr(&self, expr: &Spanned<Expr>) -> bool {
        let type_name = self.infer_receiver_type(expr);
        if matches!(type_name.as_str(), "Vector" | "List" | "Array") {
            return true;
        }
        let c_type = self.infer_receiver_c_type(expr);
        c_type.as_deref() == Some("GorgetArray")
    }

    /// If `expr` has a Defined (struct) type that implements Equatable, return the type name.
    fn try_equatable_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        let type_name = self.infer_receiver_type(expr);
        // Exclude primitives and builtins
        if matches!(type_name.as_str(), "Unknown" | "int" | "float" | "bool" | "str" | "char"
            | "Vector" | "Dict" | "Set" | "Option" | "Result") {
            return None;
        }
        if self.traits.has_trait_impl_by_name(&type_name, "Equatable") {
            Some(type_name)
        } else {
            None
        }
    }

    /// Generate a print/println call, handling string interpolation.
    /// `print()` always appends a newline (like Python). `println()` is an alias.
    pub fn gen_print_call(
        &self,
        args: &[Spanned<crate::parser::ast::CallArg>],
        _is_println: bool,
    ) -> String {
        if args.is_empty() {
            return "printf(\"\\n\")".to_string();
        }

        let arg = &args[0].node.value;

        // Check if the argument is a string literal with interpolations
        if let Expr::StringLiteral(s) = &arg.node {
            // print() always adds a newline
            return self.gen_printf_from_string_lit(s, true);
        }

        // Non-string argument: try to print as the correct type
        let expr = self.gen_expr(arg);

        // Look up the argument's type to determine the correct printf format
        if let Some(type_id) = self.infer_interp_expr_type(arg) {
            let (fmt, arg_expr) = self.format_for_type_id(type_id, &expr);
            return format!("printf(\"{fmt}\\n\", {arg_expr})");
        }

        format!("printf(\"%lld\\n\", (long long){expr})")
    }

    /// Generate a `gorget_format(...)` call that returns `const char*`.
    fn gen_format_call(
        &self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        if args.is_empty() {
            return "gorget_format(\"\")".to_string();
        }

        let arg = &args[0].node.value;

        // String literal with interpolations → reuse interpolation_format
        if let Expr::StringLiteral(s) = &arg.node {
            return self.gen_gorget_format_from_string_lit(s);
        }

        // Non-string argument: format a single value
        let expr = self.gen_expr(arg);

        if let Some(type_id) = self.infer_interp_expr_type(arg) {
            let (fmt, arg_expr) = self.format_for_type_id(type_id, &expr);
            return format!("gorget_format(\"{fmt}\", {arg_expr})");
        }

        format!("gorget_format(\"%lld\", (long long){expr})")
    }

    /// Generate `gorget_format("fmt", args...)` from a StringLit.
    fn gen_gorget_format_from_string_lit(&self, s: &StringLit) -> String {
        let mut format_parts = Vec::new();
        let mut format_args = Vec::new();

        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    format_parts.push(escape_string(text));
                }
                StringSegment::Interpolation(var_name) => {
                    let (fmt, arg_expr) = self.interpolation_format(var_name);
                    format_parts.push(fmt);
                    format_args.push(arg_expr);
                }
            }
        }

        let format_str = format_parts.join("");
        if format_args.is_empty() {
            format!("gorget_format(\"{format_str}\")")
        } else {
            format!("gorget_format(\"{format_str}\", {})", format_args.join(", "))
        }
    }

    /// Generate printf from a StringLit with possible interpolation segments.
    fn gen_printf_from_string_lit(&self, s: &StringLit, is_println: bool) -> String {
        let mut format_parts = Vec::new();
        let mut printf_args = Vec::new();

        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    format_parts.push(escape_string(text));
                }
                StringSegment::Interpolation(var_name) => {
                    // Determine the format specifier based on the variable's type
                    let (fmt, arg_expr) = self.interpolation_format(var_name);
                    format_parts.push(fmt);
                    printf_args.push(arg_expr);
                }
            }
        }

        if is_println {
            format_parts.push("\\n".to_string());
        }

        let format_str = format_parts.join("");
        if printf_args.is_empty() {
            format!("printf(\"{format_str}\")")
        } else {
            format!("printf(\"{format_str}\", {})", printf_args.join(", "))
        }
    }

    /// Determine printf format specifier and C expression for an interpolated variable.
    fn interpolation_format(&self, var_name: &str) -> (String, String) {
        // Try to look up the variable's type
        let escaped = c_mangle::escape_keyword(var_name);

        // Handle dotted paths like "t._0" or "nested._1._0" by resolving
        // the base variable type, then following tuple field accesses.
        if var_name.contains('.') {
            let parts: Vec<&str> = var_name.splitn(2, '.').collect();
            let base = parts[0];
            let field_path = parts[1];
            if let Some(def_id) = self.scopes.lookup_by_name_anywhere(base) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    if let Some(resolved_id) = self.resolve_field_type(type_id, field_path) {
                        return self.format_for_type_id(resolved_id, &escaped);
                    }
                }
            }
        } else {
            // Search all scopes for the variable (codegen doesn't track current scope)
            if let Some(def_id) = self.scopes.lookup_by_name_anywhere(var_name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    return self.format_for_type_id(type_id, &escaped);
                }
            }
        }

        // Try parsing as a full expression (handles method calls, function calls, operators)
        if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
            let c_expr = self.gen_expr(&parsed_expr);
            let type_id = self.infer_interp_expr_type(&parsed_expr);
            if let Some(tid) = type_id {
                return self.format_for_type_id(tid, &c_expr);
            }
            // Default: assume int for unknown expression types
            return ("%lld".to_string(), format!("(long long){c_expr}"));
        }

        // Default: assume int64_t
        ("%lld".to_string(), format!("(long long){escaped}"))
    }

    /// Resolve a dotted field path against a type, returning the final TypeId.
    /// Handles tuple field access ("_0", "_1._0") and struct field access ("name", "msg.sender").
    fn resolve_field_type(
        &self,
        type_id: crate::semantic::ids::TypeId,
        field_path: &str,
    ) -> Option<crate::semantic::ids::TypeId> {
        use crate::semantic::types::ResolvedType;

        let (field, rest) = match field_path.split_once('.') {
            Some((f, r)) => (f, Some(r)),
            None => (field_path, None),
        };

        // Tuple field access: _0, _1, etc.
        if let Some(idx_str) = field.strip_prefix('_') {
            if let Ok(idx) = idx_str.parse::<usize>() {
                if let ResolvedType::Tuple(elems) = self.types.get(type_id) {
                    if let Some(&elem_type_id) = elems.get(idx) {
                        return match rest {
                            Some(remaining) => self.resolve_field_type(elem_type_id, remaining),
                            None => Some(elem_type_id),
                        };
                    }
                }
            }
        }

        // Struct field access
        if let ResolvedType::Defined(def_id) = self.types.get(type_id) {
            let struct_name = self.scopes.get_def(*def_id).name.clone();
            let key = (struct_name, field.to_string());
            if let Some(ast_type) = self.field_type_names.get(&key) {
                if let Some(field_tid) = self.ast_type_to_type_id(ast_type) {
                    return match rest {
                        Some(remaining) => self.resolve_field_type(field_tid, remaining),
                        None => Some(field_tid),
                    };
                }
            }
        }

        None
    }

    /// Convert an AST `Type` to a semantic `TypeId`.
    fn ast_type_to_type_id(&self, ty: &Type) -> Option<crate::semantic::ids::TypeId> {
        match ty {
            Type::Primitive(p) => match p {
                PrimitiveType::Int | PrimitiveType::Int64 => Some(self.types.int_id),
                PrimitiveType::Float | PrimitiveType::Float64 => Some(self.types.float_id),
                PrimitiveType::Bool => Some(self.types.bool_id),
                PrimitiveType::Char => Some(self.types.char_id),
                PrimitiveType::Str | PrimitiveType::StringType => Some(self.types.string_id),
                PrimitiveType::Void => Some(self.types.void_id),
                _ => None,
            },
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                self.scopes.lookup_by_name_anywhere(&name.node)
                    .and_then(|def_id| {
                        let def = self.scopes.get_def(def_id);
                        def.type_id
                    })
            }
            _ => None,
        }
    }

    /// Get printf format + expression for a given TypeId.
    fn format_for_type_id(
        &self,
        type_id: crate::semantic::ids::TypeId,
        expr: &str,
    ) -> (String, String) {
        use crate::semantic::types::ResolvedType;
        match self.types.get(type_id) {
            ResolvedType::Primitive(prim) => {
                let fmt = c_types::printf_format_for_primitive(*prim);
                let arg = match prim {
                    PrimitiveType::Bool => format!("{expr} ? \"true\" : \"false\""),
                    PrimitiveType::Int | PrimitiveType::Int64 => {
                        format!("(long long){expr}")
                    }
                    PrimitiveType::Uint | PrimitiveType::Uint64 => {
                        format!("(unsigned long long){expr}")
                    }
                    _ => expr.to_string(),
                };
                (fmt.to_string(), arg)
            }
            ResolvedType::Void => ("%s".to_string(), "\"void\"".to_string()),
            ResolvedType::Generic(def_id, args) if self.scopes.get_def(*def_id).name == "Box" && args.len() == 1 => {
                // Box[T]: auto-dereference and format the inner type
                let deref_expr = format!("(*{expr})");
                self.format_for_type_id(args[0], &deref_expr)
            }
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                if self.traits.has_trait_impl_by_name(&name, "Displayable") {
                    let mangled = c_mangle::mangle_trait_method("Displayable", &name, "display");
                    // Use a GCC statement expression to handle non-lvalue exprs
                    let call = format!("({{ __typeof__({expr}) __tmp = {expr}; {mangled}(&__tmp); }})");
                    ("%s".to_string(), call)
                } else {
                    // Fallback: print the type name
                    ("%s".to_string(), format!("\"<{name}>\""))
                }
            }
            ResolvedType::Array(_, _)
            | ResolvedType::Tuple(_)
            | ResolvedType::Function { .. }
            | ResolvedType::TraitObject(_)
            | ResolvedType::Slice(_) => {
                panic!("non-primitive type cannot be used in string interpolation")
            }
            ResolvedType::Error | ResolvedType::Never | ResolvedType::Var(_) => {
                ("%lld".to_string(), format!("(long long){expr}"))
            }
        }
    }

    /// Infer the result TypeId of a sub-parsed interpolation expression.
    fn infer_interp_expr_type(&self, expr: &Spanned<Expr>) -> Option<crate::semantic::ids::TypeId> {
        self.resolve_expr_type_id(expr)
    }

    /// Map (receiver C type, method name) → return TypeId for known builtins.
    pub(super) fn builtin_method_return_type(
        &self,
        receiver_type: &str,
        method: &str,
    ) -> Option<crate::semantic::ids::TypeId> {
        // GorgetMap/GorgetSet use prefix matching because monomorphized names
        // are mangled (e.g. "GorgetMap__int64_t__int64_t").
        if receiver_type == "GorgetArray" {
            return match method {
                "len" | "get" | "pop" => Some(self.types.int_id),
                _ => None,
            };
        }
        if receiver_type.starts_with("GorgetMap") {
            return match method {
                "len" | "get" => Some(self.types.int_id),
                "contains" => Some(self.types.bool_id),
                _ => None,
            };
        }
        if receiver_type.starts_with("GorgetSet") {
            return match method {
                "len" => Some(self.types.int_id),
                "contains" => Some(self.types.bool_id),
                _ => None,
            };
        }
        match (receiver_type, method) {
            ("const char*", "len") => Some(self.types.int_id),
            ("const char*", "contains" | "starts_with" | "ends_with" | "is_empty") => {
                Some(self.types.bool_id)
            }
            ("const char*", "trim" | "to_upper" | "to_lower" | "replace") => {
                Some(self.types.string_id)
            }
            _ => None,
        }
    }

    /// Generate a closure expression via lambda lifting.
    fn gen_closure_expr(
        &self,
        params: &[Spanned<crate::parser::ast::ClosureParam>],
        body: &Spanned<Expr>,
    ) -> String {
        use super::LiftedClosure;

        let id = {
            let mut counter = self.closure_counter.borrow_mut();
            let id = *counter;
            *counter += 1;
            id
        };

        let fn_name = c_mangle::mangle_closure(id);
        let env_name = c_mangle::mangle_closure_env(id);

        // Build parameter list
        let closure_params: Vec<(String, String)> = params
            .iter()
            .map(|p| {
                let name = c_mangle::escape_keyword(&p.node.name.node);
                let ty = p
                    .node
                    .type_
                    .as_ref()
                    .map(|t| c_types::ast_type_to_c(&t.node, self.scopes))
                    .unwrap_or_else(|| "int64_t".to_string());
                (name, ty)
            })
            .collect();

        // Collect free variables (simple heuristic: identifiers referenced in body
        // that are not in params). For now, we collect none — full free variable
        // analysis would require a scope walk. This suffices for simple closures.
        let param_names: std::collections::HashSet<&str> =
            params.iter().map(|p| p.node.name.node.as_str()).collect();
        let captures = self.collect_free_vars(&body.node, &param_names);

        // Generate the body expression
        let body_expr = self.gen_expr(body);

        let lifted = LiftedClosure {
            id,
            captures: captures.clone(),
            params: closure_params,
            return_type: self.infer_c_type_from_expr(&body.node),
            body: body_expr,
        };

        self.lifted_closures.borrow_mut().push(lifted);

        // At the creation site: emit a bare function pointer (no captures)
        // or allocate env and create GorgetClosure (with captures).
        if captures.is_empty() {
            fn_name
        } else {
            // Use a GCC statement expression to allocate and populate the env
            let mut parts = Vec::new();
            parts.push(format!("{env_name}* __env = ({env_name}*)malloc(sizeof({env_name}))"));
            for (cap_name, _) in &captures {
                parts.push(format!("__env->{cap_name} = {cap_name}"));
            }
            let stmts = parts.join("; ");
            format!(
                "({{ {stmts}; (GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)__env}}; }})"
            )
        }
    }

    /// Collect free variable references from an expression (simple walk).
    pub(super) fn collect_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
    ) -> Vec<(String, String)> {
        let mut free = Vec::new();
        let mut seen = std::collections::HashSet::new();
        self.walk_free_vars(expr, bound, &mut seen, &mut free);
        free
    }

    fn walk_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
        seen: &mut std::collections::HashSet<String>,
        free: &mut Vec<(String, String)>,
    ) {
        match expr {
            Expr::Identifier(name) if !bound.contains(name.as_str()) && name != "self" => {
                // Skip global definitions (functions, enum variants, structs, etc.)
                // — they don't need to be captured, they're available globally in C.
                let is_global = self.scopes.is_global_def(name);
                if !is_global && seen.insert(name.clone()) {
                    let ty = self.infer_c_type_from_expr(expr);
                    free.push((c_mangle::escape_keyword(name), ty));
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.walk_free_vars(&left.node, bound, seen, free);
                self.walk_free_vars(&right.node, bound, seen, free);
            }
            Expr::UnaryOp { operand, .. } => {
                self.walk_free_vars(&operand.node, bound, seen, free);
            }
            Expr::Call { callee, args, .. } => {
                self.walk_free_vars(&callee.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.walk_free_vars(&object.node, bound, seen, free);
            }
            Expr::MethodCall {
                receiver, args, ..
            } => {
                self.walk_free_vars(&receiver.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_free_vars(&condition.node, bound, seen, free);
                self.walk_free_vars(&then_branch.node, bound, seen, free);
                if let Some(eb) = else_branch {
                    self.walk_free_vars(&eb.node, bound, seen, free);
                }
            }
            Expr::Index { object, index } => {
                self.walk_free_vars(&object.node, bound, seen, free);
                self.walk_free_vars(&index.node, bound, seen, free);
            }
            _ => {}
        }
    }

    /// Generate a list comprehension as a GCC statement expression.
    fn gen_list_comprehension(
        &self,
        expr: &Spanned<Expr>,
        variable: &Spanned<crate::parser::ast::Pattern>,
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let elem_expr = self.gen_expr(expr);
        let elem_type = self.infer_c_type_from_expr(&expr.node);

        let var_name = match &variable.node {
            crate::parser::ast::Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_v".to_string(),
        };

        // Check if iterable is a range
        if let Expr::Range {
            start,
            end,
            inclusive,
        } = &iterable.node
        {
            let start_expr = start
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let end_expr = end
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let cmp = if *inclusive { "<=" } else { "<" };

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ GorgetArray __comp = gorget_array_new(sizeof({elem_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_array_push(&__comp, &__elem); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetArray __comp = gorget_array_new(sizeof({elem_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            {elem_type} {var_name} = {iter}[__i]; \
            {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_array_push(&__comp, &__elem); }} \
            }} __comp; }})"
        )
    }

    /// Generate a set comprehension as a GCC statement expression.
    fn gen_set_comprehension(
        &self,
        expr: &Spanned<Expr>,
        variable: &Spanned<String>,
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let elem_expr = self.gen_expr(expr);
        let elem_type = self.infer_c_type_from_expr(&expr.node);
        let var_name = c_mangle::escape_keyword(&variable.node);

        // Check if iterable is a range
        if let Expr::Range {
            start,
            end,
            inclusive,
        } = &iterable.node
        {
            let start_expr = start
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let end_expr = end
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let cmp = if *inclusive { "<=" } else { "<" };

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ GorgetSet __comp = gorget_set_new(sizeof({elem_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_set_add(&__comp, &__elem); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetSet __comp = gorget_set_new(sizeof({elem_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            {elem_type} {var_name} = {iter}[__i]; \
            {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_set_add(&__comp, &__elem); }} \
            }} __comp; }})"
        )
    }

    /// Generate a dict comprehension as a GCC statement expression.
    fn gen_dict_comprehension(
        &self,
        key: &Spanned<Expr>,
        value: &Spanned<Expr>,
        variables: &[Spanned<String>],
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let key_expr = self.gen_expr(key);
        let val_expr = self.gen_expr(value);
        let key_type = self.infer_c_type_from_expr(&key.node);
        let val_type = self.infer_c_type_from_expr(&value.node);
        let mangled = c_mangle::mangle_generic("GorgetMap", &[key_type.clone(), val_type.clone()]);
        self.register_generic("GorgetMap", &[key_type.clone(), val_type.clone()], super::GenericInstanceKind::Map);
        let var_name = variables
            .first()
            .map(|v| c_mangle::escape_keyword(&v.node))
            .unwrap_or_else(|| "__gorget_v".to_string());

        // Check if iterable is a range
        if let Expr::Range {
            start,
            end,
            inclusive,
        } = &iterable.node
        {
            let start_expr = start
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let end_expr = end
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let cmp = if *inclusive { "<=" } else { "<" };

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ {mangled} __comp = {mangled}__new(); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
                {mangled}__put(&__comp, __k, __v); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ {mangled} __comp = {mangled}__new(); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            __typeof__({iter}[0]) {var_name} = {iter}[__i]; \
            {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
            {mangled}__put(&__comp, __k, __v); }} \
            }} __comp; }})"
        )
    }

    /// Convert a pattern to a C boolean condition for `is` expressions.
    /// `enum_c_type` overrides the enum name used for tag constants (needed for generic enums
    /// where the monomorphized name like `Option__int64_t` differs from the raw name `Option`).
    fn pattern_to_condition_expr(&self, pattern: &crate::parser::ast::Pattern, scrutinee: &str, enum_c_type: Option<&str>) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Literal(lit) if matches!(lit.node, Expr::NoneLiteral) => {
                // None literal as pattern: generate tag check for the None variant
                for (enum_def_id, info) in self.enum_variants {
                    for (vname, _) in &info.variants {
                        if vname == "None" {
                            let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                            let tag = c_mangle::mangle_tag(effective, "None");
                            return format!("{scrutinee}.tag == {tag}");
                        }
                    }
                }
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(name) => {
                // A bare identifier may be a unit enum variant (parser can't distinguish).
                // Check if the name matches a known variant; if so, generate a tag check.
                for (enum_def_id, info) in self.enum_variants {
                    for (vname, _) in &info.variants {
                        if vname == name {
                            let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                            let tag = c_mangle::mangle_tag(effective, name);
                            return format!("{scrutinee}.tag == {tag}");
                        }
                    }
                }
                "1".to_string()
            }
            Pattern::Constructor { path, .. } => {
                if path.len() == 2 {
                    // Explicit path like Shape.Circle — use enum_c_type override if available
                    let effective = enum_c_type.unwrap_or(&path[0].node);
                    let tag = c_mangle::mangle_tag(effective, &path[1].node);
                    format!("{scrutinee}.tag == {tag}")
                } else if path.len() == 1 {
                    // Try to find the enum for this variant
                    let variant_name = &path[0].node;
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, _) in &info.variants {
                            if vname == variant_name {
                                let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                                let tag = c_mangle::mangle_tag(effective, variant_name);
                                return format!("{scrutinee}.tag == {tag}");
                            }
                        }
                    }
                    "1".to_string()
                } else {
                    "1".to_string()
                }
            }
            Pattern::Or(alternatives) => {
                let conds: Vec<String> = alternatives
                    .iter()
                    .map(|p| self.pattern_to_condition_expr(&p.node, scrutinee, enum_c_type))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) | Pattern::Rest => "1".to_string(),
        }
    }

    /// Generate a match expression as a GCC statement expression.
    fn gen_match_expr(
        &self,
        scrutinee: &Spanned<Expr>,
        arms: &[crate::parser::ast::MatchArm],
        else_arm: Option<&Spanned<Expr>>,
    ) -> String {
        let scrut_expr = self.gen_expr(scrutinee);
        let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

        let mut parts = Vec::new();
        parts.push(format!("__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}"));

        // Build if-else chain; each arm assigns to __gorget_match_result
        let mut arm_parts = Vec::new();
        let mut first_body: Option<String> = None;
        let mut first = true;
        for arm in arms {
            let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut", enum_c_type.as_deref());
            let full_cond = if let Some(guard) = &arm.guard {
                let guard_expr = self.gen_expr(guard);
                let guard_bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
                if guard_bindings.is_empty() {
                    format!("({cond}) && ({guard_expr})")
                } else {
                    format!("({cond}) && ({{ {guard_bindings}({guard_expr}); }})")
                }
            } else {
                cond
            };

            let bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
            let body = self.gen_expr(&arm.body);

            if first {
                first_body = Some(body.clone());
                arm_parts.push(format!("if ({full_cond}) {{ {bindings}__gorget_match_result = {body}; }}"));
                first = false;
            } else {
                arm_parts.push(format!("else if ({full_cond}) {{ {bindings}__gorget_match_result = {body}; }}"));
            }
        }

        if let Some(else_expr) = else_arm {
            let else_body = self.gen_expr(else_expr);
            arm_parts.push(format!("else {{ __gorget_match_result = {else_body}; }}"));
        }

        // Determine a result type from the first arm body (reuse already-generated body)
        let result_type = if let Some(body) = &first_body {
            format!("__typeof__(({}))", body)
        } else {
            "int64_t".to_string()
        };

        let chain = arm_parts.join(" ");
        format!(
            "({{ {result_type} __gorget_match_result; {} {chain} __gorget_match_result; }})",
            parts.join("; ") + ";"
        )
    }

    /// Generate inline variable bindings for a pattern (returns C statements as a string).
    fn pattern_bindings_inline(&self, pattern: &crate::parser::ast::Pattern, scrutinee: &str) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                format!("__typeof__({scrutinee}) {escaped} = {scrutinee}; ")
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path_expr(path) {
                    let mut result = String::new();
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access = format!("{scrutinee}.data.{variant_name}._{i}");
                        result.push_str(&self.pattern_bindings_inline(&field_pat.node, &field_access));
                    }
                    result
                } else {
                    String::new()
                }
            }
            Pattern::Tuple(elements) => {
                let mut result = String::new();
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    result.push_str(&self.pattern_bindings_inline(&elem.node, &field_access));
                }
                result
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.pattern_bindings_inline(&first.node, scrutinee)
                } else {
                    String::new()
                }
            }
            _ => String::new(),
        }
    }

    /// Look up which enum owns a variant given a path (for expression context).
    fn find_enum_for_variant_path_expr(&self, path: &[Spanned<String>]) -> Option<(String, String)> {
        if path.len() == 2 {
            return Some((path[0].node.clone(), path[1].node.clone()));
        }
        let variant_name = if path.len() == 1 {
            &path[0].node
        } else {
            return None;
        };
        for (enum_def_id, info) in self.enum_variants {
            for (vname, _) in &info.variants {
                if vname == variant_name {
                    let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                    return Some((enum_name, variant_name.clone()));
                }
            }
        }
        None
    }

    /// Generate a block/do expression as a GCC statement expression.
    fn gen_block_expr(&self, block: &crate::parser::ast::Block) -> String {
        if block.stmts.is_empty() {
            return "(void)0".to_string();
        }

        let mut parts = Vec::new();
        let last_idx = block.stmts.len() - 1;

        for (i, stmt) in block.stmts.iter().enumerate() {
            if i == last_idx {
                // Last statement: if it's an expression statement, use it as the result value
                if let crate::parser::ast::Stmt::Expr(expr) = &stmt.node {
                    let val = self.gen_expr(expr);
                    parts.push(format!("{val};"));
                } else if let Some(inline) = self.gen_value_producing_stmt(&stmt.node) {
                    // Stmt::If / Stmt::Match with value-producing branches
                    parts.push(inline);
                } else {
                    // Non-expression final statement: emit as statement, result is (void)0
                    parts.push(self.stmt_to_inline_string(&stmt.node));
                    parts.push("(void)0;".to_string());
                }
            } else {
                parts.push(self.stmt_to_inline_string(&stmt.node));
            }
        }

        format!("({{ {} }})", parts.join(" "))
    }

    /// Try to generate a statement as a value-producing GCC statement expression.
    /// Returns `Some(code)` for `Stmt::If` and `Stmt::Match` whose branches end
    /// with an expression statement, `None` otherwise.
    fn gen_value_producing_stmt(&self, stmt: &crate::parser::ast::Stmt) -> Option<String> {
        use crate::parser::ast::Stmt;
        match stmt {
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                // Need at least an else branch to produce a value in all paths
                let else_body = else_body.as_ref()?;
                // Each branch must end with an expression statement
                let then_val = self.block_tail_expr(then_body)?;
                let else_val = self.block_tail_expr(else_body)?;

                let cond = self.gen_expr(condition);
                let then_code = self.gen_expr(&then_val);
                let result_type = format!("__typeof__(({}))  ", then_code);

                let mut code = format!("{result_type} __gorget_do_result; ");
                code.push_str(&format!("if ({cond}) {{ "));
                code.push_str(&self.block_stmts_except_last_inline(then_body));
                code.push_str(&format!("__gorget_do_result = {then_code}; }} "));

                for (elif_cond, elif_body) in elif_branches {
                    let elif_val = self.block_tail_expr(elif_body)?;
                    let ec = self.gen_expr(elif_cond);
                    let elif_code = self.gen_expr(&elif_val);
                    code.push_str(&format!("else if ({ec}) {{ "));
                    code.push_str(&self.block_stmts_except_last_inline(elif_body));
                    code.push_str(&format!("__gorget_do_result = {elif_code}; }} "));
                }

                let else_code = self.gen_expr(&else_val);
                code.push_str("else { ");
                code.push_str(&self.block_stmts_except_last_inline(else_body));
                code.push_str(&format!("__gorget_do_result = {else_code}; }} "));
                code.push_str("__gorget_do_result;");

                Some(code)
            }
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                // Need at least one arm to produce a value
                let first_arm = arms.first()?;
                let first_body = self.gen_expr(&first_arm.body);
                let result_type = format!("__typeof__(({}))  ", first_body);

                let scrut_expr = self.gen_expr(scrutinee);
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

                let mut code = format!(
                    "__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}; \
                     {result_type} __gorget_do_result; "
                );

                let mut first = true;
                for arm in arms {
                    let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut", enum_c_type.as_deref());
                    let full_cond = if let Some(guard) = &arm.guard {
                        let guard_expr = self.gen_expr(guard);
                        format!("({cond}) && ({guard_expr})")
                    } else {
                        cond
                    };
                    let bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
                    let body = self.gen_expr(&arm.body);

                    if first {
                        code.push_str(&format!("if ({full_cond}) {{ {bindings}__gorget_do_result = {body}; }} "));
                        first = false;
                    } else {
                        code.push_str(&format!("else if ({full_cond}) {{ {bindings}__gorget_do_result = {body}; }} "));
                    }
                }

                if let Some(else_body) = else_arm {
                    let else_val = self.block_tail_expr(else_body)?;
                    let else_code = self.gen_expr(else_val);
                    code.push_str("else { ");
                    code.push_str(&self.block_stmts_except_last_inline(else_body));
                    code.push_str(&format!("__gorget_do_result = {else_code}; }} "));
                }

                code.push_str("__gorget_do_result;");
                Some(code)
            }
            _ => None,
        }
    }

    /// Extract the tail expression from a block (last stmt must be Stmt::Expr).
    fn block_tail_expr<'b>(&self, block: &'b crate::parser::ast::Block) -> Option<&'b Spanned<Expr>> {
        if let Some(last) = block.stmts.last() {
            if let crate::parser::ast::Stmt::Expr(expr) = &last.node {
                return Some(expr);
            }
        }
        None
    }

    /// Generate all statements in a block except the last one, as inline C code.
    fn block_stmts_except_last_inline(&self, block: &crate::parser::ast::Block) -> String {
        if block.stmts.len() <= 1 {
            return String::new();
        }
        let mut result = String::new();
        for stmt in &block.stmts[..block.stmts.len() - 1] {
            result.push_str(&self.stmt_to_inline_string(&stmt.node));
            result.push(' ');
        }
        result
    }

    /// Generate inline C code for a statement (for use inside GCC statement expressions).
    fn stmt_to_inline_string(&self, stmt: &crate::parser::ast::Stmt) -> String {
        use crate::parser::ast::Stmt;
        match stmt {
            Stmt::Expr(expr) => {
                let e = self.gen_expr(expr);
                format!("{e};")
            }
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
                ..
            } => {
                let const_prefix = if *is_const { "const " } else { "" };
                match &pattern.node {
                    crate::parser::ast::Pattern::Binding(name) => {
                        let escaped = c_mangle::escape_keyword(name);
                        let c_type = match &type_.node {
                            crate::parser::ast::Type::Inferred => self.infer_c_type_from_expr(&value.node),
                            _ => self.type_to_c(&type_.node),
                        };
                        let val = self.gen_expr(value);
                        let decl = c_types::c_declare(&c_type, &escaped);
                        format!("{const_prefix}{decl} = {val};")
                    }
                    _ => {
                        let val = self.gen_expr(value);
                        format!("/* pattern decl */ (void){val};")
                    }
                }
            }
            Stmt::Assign { target, value } => {
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                format!("{t} = {v};")
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    format!("return {};", self.gen_expr(e))
                } else {
                    "return;".to_string()
                }
            }
            _ => "/* stmt */ (void)0;".to_string(),
        }
    }
}

/// Convert a BinaryOp to its C operator string.
fn binary_op_to_c(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add | BinaryOp::AddWrap => "+",
        BinaryOp::Sub | BinaryOp::SubWrap => "-",
        BinaryOp::Mul | BinaryOp::MulWrap => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::LtEq => "<=",
        BinaryOp::GtEq => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::In => panic!("`in` as a binary expression is not yet implemented in codegen")
    }
}

/// Escape a character for a C char literal.
fn escape_char(c: char) -> String {
    match c {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\0' => "\\0".to_string(),
        c if c.is_ascii_graphic() || c == ' ' => c.to_string(),
        c => format!("\\x{:02x}", c as u32),
    }
}

/// Escape a string for a C string literal (without outer quotes).
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\0' => result.push_str("\\0"),
            '%' => result.push_str("%%"),
            c => result.push(c),
        }
    }
    result
}
