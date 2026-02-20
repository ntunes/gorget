/// Expression codegen: convert Gorget expressions to C expression strings.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{BinaryOp, Expr, PrimitiveType, Type, UnaryOp};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

/// Map a C element type to its qsort comparator function name.
pub(super) fn sort_comparator_for_type(elem_type: &str) -> &'static str {
    match elem_type {
        "int64_t" | "int32_t" | "int16_t" | "int8_t" => "__gorget_cmp_i64",
        "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" => "__gorget_cmp_i64",
        "double" | "float" => "__gorget_cmp_f64",
        "const char*" => "__gorget_cmp_str",
        "char" => "__gorget_cmp_char",
        "bool" => "__gorget_cmp_bool",
        _ => "__gorget_cmp_i64", // fallback
    }
}

/// Check if an expression is a C lvalue (can take its address directly).
pub(super) fn is_lvalue(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_) | Expr::SelfExpr => true,
        Expr::FieldAccess { object, .. } => is_lvalue(&object.node),
        Expr::TupleFieldAccess { object, .. } => is_lvalue(&object.node),
        Expr::Index { object, .. } => is_lvalue(&object.node),
        Expr::UnaryOp { op: UnaryOp::Deref, .. } => true,
        _ => false,
    }
}

/// Generate `&expr` for lvalues, or a temp-var statement expression for rvalues.
pub(super) fn addr_of(c_expr: &str, ast_expr: &Expr) -> String {
    if is_lvalue(ast_expr) {
        format!("&{c_expr}")
    } else {
        format!("({{ __typeof__({c_expr}) __tmp = {c_expr}; &__tmp; }})")
    }
}

impl CodegenContext<'_> {
    /// Generate a C expression string from a Gorget expression.
    pub fn gen_expr(&mut self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::IntLiteral(n) => format!("INT64_C({n})"),
            Expr::FloatLiteral(f) => format!("{f}"),
            Expr::BoolLiteral(b) => if *b { "true" } else { "false" }.to_string(),
            Expr::CharLiteral(c) => format!("'{}'", escape_char(*c)),
            Expr::StringLiteral(s) => {
                if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_))) {
                    self.gen_gorget_format_from_string_lit(s)
                } else {
                    self.gen_string_literal(s)
                }
            }
            Expr::NoneLiteral => {
                // Try to resolve to monomorphized Option None constructor via type hint
                if let Some(mangled) = self.resolve_unit_variant_from_type_hint("Option", "None") {
                    format!("{}()", c_mangle::mangle_variant(&mangled, "None"))
                } else {
                    "NULL".to_string()
                }
            }

            Expr::Identifier(name) => {
                // Stdlib I/O constants → C stdio macros
                if (name == "stderr" || name == "stdout") && self.is_stdlib_static(name) {
                    return name.clone();
                }
                // Stdlib constants → GORGET_ prefixed C constants
                if self.is_stdlib_const(name) {
                    return format!("GORGET_{name}");
                }
                // Top-level functions get `gg_` prefix to avoid C library collisions
                if self.function_names.contains(name.as_str()) {
                    return c_mangle::escape_function(name);
                }
                let escaped = c_mangle::escape_keyword(name);
                if self.mutable_captures.contains(&escaped) {
                    format!("(*__env->{escaped})")
                } else if self.pointer_params.contains(&escaped) {
                    format!("(*{escaped})")
                } else {
                    escaped
                }
            }

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
                // Neg trait dispatch for user-defined types
                if *op == UnaryOp::Neg {
                    if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(operand, "Neg") {
                        return self.gen_unary_op_trait_call("Neg", "neg", &type_name, &trait_type_args, operand);
                    }
                }
                let inner = self.gen_expr(operand);
                match op {
                    UnaryOp::Neg => format!("(-{inner})"),
                    UnaryOp::Not => format!("(!{inner})"),
                    UnaryOp::BitNot => format!("(~{inner})"),
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
                    if let Some((type_name, trait_type_args)) = self.try_equatable_type(left) {
                        let l = self.gen_expr(left);
                        let r = self.gen_expr(right);
                        let mangled = c_mangle::mangle_trait_method("Equatable", &type_name, "eq", &trait_type_args);
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
                    // String Eq/Neq: use strcmp instead of pointer comparison
                    let is_str = self.resolve_expr_type_id(left).map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id)
                        || self.resolve_expr_type_id(right).map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id)
                        || matches!(&left.node, Expr::StringLiteral(_))
                        || matches!(&right.node, Expr::StringLiteral(_));
                    if is_str {
                        let l = self.gen_expr(left);
                        let r = self.gen_expr(right);
                        // Coerce GorgetString operands to .data for strcmp
                        let l_type = self.infer_c_type_from_expr(&left.node);
                        let r_type = self.infer_c_type_from_expr(&right.node);
                        let l_str = if l_type == "GorgetString" { self.coerce_string_to_str(&l) } else { l };
                        let r_str = if r_type == "GorgetString" { self.coerce_string_to_str(&r) } else { r };
                        return if *op == BinaryOp::Neq {
                            format!("(strcmp({l_str}, {r_str}) != 0)")
                        } else {
                            format!("(strcmp({l_str}, {r_str}) == 0)")
                        };
                    }
                }
                // String concatenation: str/String + str/String → GorgetString
                if *op == BinaryOp::Add {
                    let left_is_str = self.resolve_expr_type_id(left)
                        .map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id);
                    let right_is_str = self.resolve_expr_type_id(right)
                        .map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id);
                    if left_is_str || right_is_str {
                        let l = self.gen_expr(left);
                        let r = self.gen_expr(right);
                        // Coerce both operands to const char*
                        let l_type = self.infer_c_type_from_expr(&left.node);
                        let r_type = self.infer_c_type_from_expr(&right.node);
                        let l_str = if l_type == "GorgetString" { self.coerce_string_to_str(&l) } else { l };
                        let r_str = if r_type == "GorgetString" { self.coerce_string_to_str(&r) } else { r };
                        return format!("gorget_string_from_concat({l_str}, {r_str})");
                    }
                }
                // Vector concatenation: vec + vec → clone left, extend with right
                if *op == BinaryOp::Add && self.is_vector_expr(left) {
                    let l = self.gen_expr(left);
                    let r = self.gen_expr(right);
                    return format!(
                        "({{ GorgetArray __cat = gorget_array_clone(&{l}); gorget_array_extend(&__cat, &{r}); __cat; }})"
                    );
                }
                // Operator trait dispatch for user-defined types
                if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod) {
                    let (trait_name, method) = match op {
                        BinaryOp::Add => ("Add", "add"),
                        BinaryOp::Sub => ("Sub", "sub"),
                        BinaryOp::Mul => ("Mul", "mul"),
                        BinaryOp::Div => ("Div", "div"),
                        BinaryOp::Mod => ("Rem", "rem"),
                        _ => unreachable!(),
                    };
                    if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(left, trait_name) {
                        return self.gen_binary_op_trait_call(trait_name, method, &type_name, &trait_type_args, left, right);
                    }
                }
                // Comparable trait dispatch for comparison operators
                if matches!(op, BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq) {
                    if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(left, "Comparable") {
                        // Check for specific override method first (lt, gt, lte, gte)
                        let specific = match op {
                            BinaryOp::Lt => "lt",
                            BinaryOp::Gt => "gt",
                            BinaryOp::LtEq => "lte",
                            BinaryOp::GtEq => "gte",
                            _ => unreachable!(),
                        };
                        let gorget_type = self.infer_receiver_type(left);
                        if self.traits.has_method_for_type(&gorget_type, specific) {
                            return self.gen_binary_op_trait_call("Comparable", specific, &type_name, &trait_type_args, left, right);
                        }
                        // Default: derive from compare() result compared to 0
                        let compare_call = self.gen_binary_op_trait_call("Comparable", "compare", &type_name, &trait_type_args, left, right);
                        let cmp = match op {
                            BinaryOp::Lt => "< 0",
                            BinaryOp::Gt => "> 0",
                            BinaryOp::LtEq => "<= 0",
                            BinaryOp::GtEq => ">= 0",
                            _ => unreachable!(),
                        };
                        return format!("({compare_call} {cmp})");
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
                    self.gen_generic_method_call(receiver, method, type_args, args)
                } else {
                    self.gen_method_call(receiver, method, args)
                }
            }

            Expr::FieldAccess { object, field } => {
                let field_name = c_mangle::escape_keyword(&field.node);
                if self.current_self_type.is_some() && matches!(object.node, Expr::SelfExpr) {
                    format!("self->{field_name}")
                } else if let Expr::Identifier(name) = &object.node {
                    let escaped = c_mangle::escape_keyword(name);
                    if self.pointer_params.contains(&escaped) {
                        format!("{escaped}->{field_name}")
                    } else {
                        format!("{escaped}.{field_name}")
                    }
                } else {
                    let obj = self.gen_expr(object);
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
                } else if self.is_gorget_map_expr(object) {
                    let idx = self.gen_expr(index);
                    let mangled = self.infer_map_mangled(object);
                    let (_, val_type) = self.infer_map_kv_types(object);
                    format!(
                        "({{ {val_type}* __gp = {mangled}__get_ptr(&{obj}, {idx}); \
                        if (!__gp) gorget_panic(\"key not found in map\"); \
                        *__gp; }})"
                    )
                } else if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(object, "Index") {
                    let idx = self.gen_expr(index);
                    let mangled = c_mangle::mangle_trait_method("Index", &type_name, "get", &trait_type_args);
                    if !is_lvalue(&object.node) {
                        format!("({{ __typeof__({obj}) __recv = {obj}; {mangled}(&__recv, {idx}); }})")
                    } else {
                        format!("{mangled}(&{obj}, {idx})")
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

            Expr::StructLiteral { name, generic_args, args } => {
                let c_name = if let Some(ga) = generic_args {
                    // Generic struct with explicit type args: use context-aware
                    // type_to_c for substitution (handles T → int64_t in monomorphized bodies)
                    let c_type_args: Vec<String> = ga.iter()
                        .map(|t| self.type_to_c(&t.node))
                        .collect();
                    if self.generic_struct_templates.contains_key(&name.node) {
                        self.register_generic(&name.node, &c_type_args, super::GenericInstanceKind::Struct)
                    } else {
                        c_mangle::mangle_generic(&name.node, &c_type_args)
                    }
                } else if self.generic_struct_templates.contains_key(&name.node) {
                    // Generic struct without explicit type args (e.g. Box(42)):
                    // infer type args from declaration type hint
                    if let Some(crate::parser::ast::Type::Named { generic_args: hint_args, .. }) = &self.decl_type_hint {
                        if !hint_args.is_empty() {
                            let c_type_args: Vec<String> = hint_args.iter()
                                .map(|t| self.type_to_c(&t.node))
                                .collect();
                            self.register_generic(&name.node, &c_type_args, super::GenericInstanceKind::Struct)
                        } else {
                            name.node.clone()
                        }
                    } else {
                        name.node.clone()
                    }
                } else if let Some(def_id) = self.scopes.lookup(&name.node) {
                    c_types::def_name_to_c(def_id, self.scopes)
                } else {
                    name.node.clone()
                };

                // Hoist struct name and field names for per-field type hint + coercion
                let struct_name = name.node.clone();
                let field_names: Vec<String> = self.scopes.lookup(&struct_name)
                    .and_then(|def_id| self.struct_fields.get(&def_id))
                    .map(|info| info.fields.iter().map(|(n, _)| n.clone()).collect())
                    .unwrap_or_default();

                // Build per-field TypeIds for str↔String coercion
                let field_type_ids: Vec<Option<crate::semantic::ids::TypeId>> = {
                    field_names.iter().map(|fname| {
                        let key = (struct_name.clone(), fname.clone());
                        self.field_type_names.get(&key).and_then(|ast_type| {
                            match ast_type {
                                Type::Primitive(PrimitiveType::Str) => Some(self.types.string_id),
                                Type::Primitive(PrimitiveType::StringType) => Some(self.types.owned_string_id),
                                _ => None,
                            }
                        })
                    }).collect()
                };

                // Queue move-zeroing for consumed droppable args
                self.queue_constructor_move_zeros_exprs(args);

                let saved_hint = self.decl_type_hint.clone();
                let field_exprs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                    // Set per-field type hint so nested generic constructors
                    // (e.g. Some("hi") in an Option[str] field) resolve correctly
                    if let Some(fname) = field_names.get(i) {
                        let key = (struct_name.clone(), fname.clone());
                        if let Some(field_type) = self.field_type_names.get(&key) {
                            self.decl_type_hint = Some(field_type.clone());
                        }
                    }
                    let expr = self.gen_expr(a);
                    let ptid = field_type_ids.get(i).copied().flatten();
                    self.coerce_arg_to_str(expr, a, ptid)
                }).collect();
                self.decl_type_hint = saved_hint;
                let fields_str = field_exprs.join(", ");
                format!("({c_name}){{{fields_str}}}")
            }

            Expr::ArrayLiteral(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                format!("{{{}}}", elems.join(", "))
            }

            Expr::DictLiteral(pairs) => {
                if pairs.is_empty() {
                    // Empty dict literal — infer K/V from decl_type_hint (AST Type)
                    if let Some(crate::parser::ast::Type::Named { name, generic_args }) = self.decl_type_hint.as_ref() {
                        if matches!(name.node.as_str(), "Dict" | "HashMap") && generic_args.len() >= 2 {
                            let key_c = self.type_to_c(&generic_args[0].node);
                            let val_c = self.type_to_c(&generic_args[1].node);
                            let ordered = name.node == "Dict";
                            let base = if ordered { "GorgetDict" } else { "GorgetMap" };
                            let mangled = self.register_generic(base, &[key_c, val_c], super::GenericInstanceKind::Map { ordered });
                            return format!("{mangled}__new()");
                        }
                    }
                    // Fallback: can't infer, emit empty Dict[int64_t, int64_t]
                    let mangled = self.register_generic("GorgetDict", &["int64_t".to_string(), "int64_t".to_string()], super::GenericInstanceKind::Map { ordered: true });
                    format!("{mangled}__new()")
                } else {
                    let key_type = self.infer_c_type_from_expr(&pairs[0].0.node);
                    let val_type = self.infer_c_type_from_expr(&pairs[0].1.node);
                    let mangled = self.register_generic("GorgetDict", &[key_type.clone(), val_type.clone()], super::GenericInstanceKind::Map { ordered: true });
                    let mut puts = String::new();
                    for (k, v) in pairs {
                        let kc = self.gen_expr(k);
                        let vc = self.gen_expr(v);
                        puts.push_str(&format!("{mangled}__put(&__dl, {kc}, {vc}); "));
                    }
                    format!("({{ {mangled} __dl = {mangled}__new(); {puts}__dl; }})")
                }
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

            Expr::Move { expr } => {
                // Record the move so we can zero the source after the statement
                if let Expr::Identifier(name) = &expr.node {
                    let escaped = c_mangle::escape_keyword(name);
                    self.pending_move_zeros.push(escaped);
                } else if let Expr::FieldAccess { object, field } = &expr.node {
                    self.queue_field_move_zero(&object.node, &field.node);
                }
                self.gen_expr(expr)
            }

            Expr::MutableBorrow { expr } => {
                self.gen_expr(expr)
            }

            Expr::Deref { expr } => {
                // Gorget `*x` is exclusively Box[T] dereference — always emit C deref
                let inner = self.gen_expr(expr);
                format!("(*{inner})")
            }

            Expr::Closure {
                params,
                body,
                is_move,
                ..
            } => {
                self.gen_closure_expr(params, body, *is_move)
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
                self.gen_closure_expr(&params, body, false)
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
    fn gen_result_try(&mut self, try_expr: &Spanned<Expr>, args: &[crate::semantic::ids::TypeId]) -> String {
        let inner = self.gen_expr(try_expr);
        let t_c = self.type_id_to_c_substituted(args[0]);
        let e_c = self.type_id_to_c_substituted(args[1]);
        let inner_mangled = c_mangle::mangle_generic("Result", &[t_c, e_c.clone()]);
        let tag_error = c_mangle::mangle_tag(&inner_mangled, "Error");

        // Get function's return type for Error constructor
        let fn_ret = self.current_function_return_c_type.clone()
            .unwrap_or_else(|| inner_mangled.clone());
        let ret_error_ctor = c_mangle::mangle_variant(&fn_ret, "Error");

        // Unique temp name for nested ? support
        let try_id = {
            let id = self.try_counter;
            self.try_counter += 1;
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
    fn gen_string_literal(&mut self, s: &StringLit) -> String {
        let mut result = String::from("\"");
        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    result.push_str(&escape_string(text));
                }
                StringSegment::Interpolation(_) => {
                    unreachable!("gen_string_literal called with interpolation segment; should use gen_gorget_format_from_string_lit");
                }
            }
        }
        result.push('"');
        result
    }

    /// Canonical TypeId resolution for any expression.
    /// Handles Identifier, literals, BinaryOp, UnaryOp, Call, MethodCall, Deref, FieldAccess.
    pub(super) fn resolve_expr_type_id(
        &mut self,
        expr: &Spanned<Expr>,
    ) -> Option<crate::semantic::ids::TypeId> {
        match &expr.node {
            Expr::Identifier(name) => {
                self.resolution_map
                    .get(&expr.span.start)
                    // Guard against cross-module span collisions: verify the
                    // resolved definition actually matches this identifier.
                    .filter(|def_id| self.scopes.get_def(**def_id).name == *name)
                    .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                    .or_else(|| {
                        self.scoped_lookup(name)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                    })
                    .or_else(|| {
                        // Pattern-bound variables from VarDecl destructuring
                        self.pattern_var_types.get(name.as_str()).copied()
                    })
            }
            Expr::IntLiteral(_) => Some(self.types.int_id),
            Expr::FloatLiteral(_) => Some(self.types.float_id),
            Expr::BoolLiteral(_) => Some(self.types.bool_id),
            Expr::StringLiteral(s) => {
                use crate::lexer::token::StringSegment;
                if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_))) {
                    Some(self.types.owned_string_id)
                } else {
                    Some(self.types.string_id)
                }
            }
            Expr::BinaryOp { op, left, right, .. } => match op {
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::LtEq
                | BinaryOp::GtEq
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::In => Some(self.types.bool_id),
                BinaryOp::Add => {
                    // str/String + anything → String (owned)
                    let left_is_str = self.resolve_expr_type_id(left)
                        .map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id);
                    let right_is_str = self.resolve_expr_type_id(right)
                        .map_or(false, |t| t == self.types.string_id || t == self.types.owned_string_id);
                    if left_is_str || right_is_str {
                        Some(self.types.owned_string_id)
                    } else {
                        self.resolve_expr_type_id(left)
                    }
                }
                _ => self.resolve_expr_type_id(left),
            },
            Expr::UnaryOp { operand, .. } => self.resolve_expr_type_id(operand),
            Expr::Call { callee, args, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    // Check builtin return types first
                    match name.as_str() {
                        "format" => {
                            return Some(self.types.owned_string_id);
                        }
                        "rand" | "getchar" | "time" | "term_cols" | "term_rows" | "len" => {
                            return Some(self.types.int_id);
                        }
                        // abs/min/max return float when given float arguments
                        "abs" | "min" | "max" => {
                            if let Some(first_arg) = args.first() {
                                if self.resolve_expr_type_id(&first_arg.node.value)
                                    .map_or(false, |t| t == self.types.float_id)
                                {
                                    return Some(self.types.float_id);
                                }
                            }
                        }
                        _ => {}
                    }
                    let def_id = self
                        .resolution_map
                        .get(&callee.span.start)
                        .filter(|did| self.scopes.get_def(**did).name == *name)
                        .copied()
                        .or_else(|| self.scoped_lookup(name));
                    if let Some(did) = def_id {
                        if let Some(fi) = self.function_info.get(&did) {
                            return fi.return_type_id;
                        }
                        // Variant constructor: return the parent enum's TypeId
                        if self.scopes.get_def(did).kind == crate::semantic::scope::DefKind::Variant {
                            for (enum_def_id, info) in self.enum_variants {
                                if info.variants.iter().any(|(_, vid)| *vid == did) {
                                    return self.types.try_defined_id(*enum_def_id);
                                }
                            }
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
                // Trait object receiver: look up method return type from trait definition
                if let Some(trait_name) = self.resolve_trait_object_type(receiver) {
                    if let Some(trait_def_id) = self.scoped_lookup(&trait_name) {
                        if let Some(trait_info) = self.traits.traits.get(&trait_def_id) {
                            if let Some(sig) = trait_info.methods.get(method.node.as_str()) {
                                return Some(sig.return_type);
                            }
                            // Walk extends chain for inherited methods
                            for &parent_id in &trait_info.extends {
                                if let Some(parent_info) = self.traits.traits.get(&parent_id) {
                                    if let Some(sig) = parent_info.methods.get(method.node.as_str()) {
                                        return Some(sig.return_type);
                                    }
                                }
                            }
                        }
                    }
                }
                // Fallback: use semantic expr_types map (populated by the type checker)
                self.expr_types.get(&expr.span).copied()
            }
            Expr::Index { object, index } => {
                if let Some(tid) = self.resolve_expr_type_id(object) {
                    if let crate::semantic::types::ResolvedType::Generic(def_id, args) = self.types.get(tid) {
                        let def_name = self.scopes.get_def(*def_id).name.clone();
                        if matches!(def_name.as_str(), "Dict" | "HashMap") && args.len() >= 2 {
                            // Dict[K,V] / HashMap[K,V] indexing → return V
                            return Some(args[1]);
                        }
                        // Vector[T] indexing → return T
                        if let Some(&elem_tid) = args.first() {
                            return Some(elem_tid);
                        }
                    }
                }
                // String slicing with range → str; single index → char
                if self.is_string_expr(object) {
                    if matches!(&index.node, Expr::Range { .. }) {
                        return Some(self.types.string_id);
                    }
                    return Some(self.types.int_id); // char is printed as int
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
                // Check if the object has a known primitive type via type_id
                if let Some(obj_tid) = self.resolve_expr_type_id(object) {
                    if obj_tid == self.types.owned_string_id {
                        // GorgetString field access
                        return match field.node.as_str() {
                            "data" => Some(self.types.string_id),
                            "len" | "cap" => Some(self.types.int_id),
                            _ => None,
                        };
                    }
                }
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type, field.node.clone());
                    if let Some(ast_type) = self.field_type_names.get(&key).cloned() {
                        return self.ast_type_to_type_id(&ast_type);
                    }
                }
                None
            }
            Expr::CharLiteral(_) => Some(self.types.char_id),
            Expr::StructLiteral { name, .. } => {
                self.scoped_lookup(&name.node)
                    .and_then(|did| self.scopes.get_def(did).type_id)
            }
            Expr::Block(block) | Expr::Do { body: block } => {
                if let Some(last) = block.stmts.last() {
                    if let crate::parser::ast::Stmt::Expr(e) = &last.node {
                        return self.resolve_expr_type_id(e);
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
        &mut self,
        scrutinee: &Spanned<Expr>,
    ) -> Option<String> {
        // For `self` in a monomorphized method body, use the current self type
        // (which is the mangled type name, e.g., "Wrapper__int64_t")
        if matches!(&scrutinee.node, Expr::SelfExpr) {
            if let Some(self_type) = &self.current_self_type {
                return Some(self_type.clone());
            }
        }
        // For field access on a struct, resolve directly from the field's AST type.
        // This handles generic enum fields (e.g. `g.message` where message is `Option[str]`)
        // that may not have a matching Generic entry in the TypeTable.
        if let Expr::FieldAccess { object, field } = &scrutinee.node {
            let obj_type = self.infer_receiver_type(object);
            if obj_type != "Unknown" {
                let key = (obj_type, field.node.clone());
                if let Some(ast_type) = self.field_type_names.get(&key).cloned() {
                    if let Type::Named { name, generic_args } = &ast_type {
                        if !generic_args.is_empty() {
                            let c_type_args: Vec<String> = generic_args.iter()
                                .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                                .collect();
                            return Some(c_mangle::mangle_generic(&name.node, &c_type_args));
                        }
                        // Non-generic named type — return C name
                        if let Some(def_id) = self.scoped_lookup(&name.node) {
                            return Some(c_types::def_name_to_c(def_id, self.scopes));
                        }
                        return Some(name.node.clone());
                    }
                }
            }
        }
        let type_id = self.resolve_expr_type_id(scrutinee)?;
        match self.types.get(type_id) {
            crate::semantic::types::ResolvedType::Generic(def_id, args) => {
                let base = c_types::def_name_to_c(*def_id, self.scopes);
                let c_args: Vec<String> = args
                    .iter()
                    .map(|tid| self.type_id_to_c_substituted(*tid))
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
    pub(super) fn infer_receiver_c_type(&mut self, expr: &Spanned<Expr>) -> Option<String> {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            let result = self.type_id_to_c_substituted(tid);
            if !result.contains("error") {
                return Some(result);
            }
        }
        // Fallback: check monomorphized parameter C types
        if let Expr::Identifier(name) = &expr.node {
            let escaped = c_mangle::escape_keyword(name);
            if let Some((_, c_type)) = self.monomorphized_param_c_types.iter().find(|(n, _)| *n == escaped) {
                return Some(c_type.clone());
            }
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
    pub(super) fn infer_vector_elem_type(&mut self, receiver: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if let Some(&elem_tid) = args.first() {
                    return self.type_id_to_c_substituted(elem_tid);
                }
            }
            // Auto-promoted array literals: ResolvedType::Array(elem_tid, _)
            if let crate::semantic::types::ResolvedType::Array(elem_tid, _) = self.types.get(tid) {
                return self.type_id_to_c_substituted(*elem_tid);
            }
        }
        // Fallback for field access: extract elem type from the AST type annotation
        // (resolve_expr_type_id can't resolve generic types like Vector[Task] from fields)
        if let Expr::FieldAccess { object, field } = &receiver.node {
            let obj_type = self.infer_receiver_type(object);
            if obj_type != "Unknown" {
                let key = (obj_type, field.node.clone());
                if let Some(ast_type) = self.field_type_names.get(&key) {
                    if let Type::Named { generic_args, .. } = ast_type {
                        if let Some(arg) = generic_args.first() {
                            return c_types::ast_type_to_c(&arg.node, self.scopes);
                        }
                    }
                }
            }
        }
        "int64_t".to_string()
    }

    /// Compute the mangled GorgetMap name for a Dict receiver.
    pub(super) fn infer_map_mangled(&mut self, receiver: &Spanned<Expr>) -> String {
        let (key_type, val_type) = self.infer_map_kv_types(receiver);
        let base = if self.is_ordered_map_expr(receiver) { "GorgetDict" } else { "GorgetMap" };
        c_mangle::mangle_generic(base, &[key_type, val_type])
    }

    /// Infer the key and value C types for a Map receiver from its TypeId.
    pub(super) fn infer_map_kv_types(&mut self, receiver: &Spanned<Expr>) -> (String, String) {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if args.len() >= 2 {
                    let key = self.type_id_to_c_substituted(args[0]);
                    let val = self.type_id_to_c_substituted(args[1]);
                    return (key, val);
                }
            }
        }
        ("int64_t".to_string(), "int64_t".to_string())
    }
}

/// Convert a BinaryOp to its C operator string.
pub(super) fn binary_op_to_c(op: BinaryOp) -> &'static str {
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
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::In => panic!("`in` as a binary expression is not yet implemented in codegen")
    }
}

/// Escape a character for a C char literal.
pub(super) fn escape_char(c: char) -> String {
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
pub(super) fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\0' => result.push_str("\\0"),
            c => result.push(c),
        }
    }
    result
}

/// Escape a string for use inside a printf/fprintf format string.
/// Same as escape_string but also doubles `%` to `%%`.
pub(super) fn escape_printf_string(s: &str) -> String {
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
