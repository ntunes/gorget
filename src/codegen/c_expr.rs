/// Expression codegen: convert Gorget expressions to C expression strings.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{BinaryOp, Expr, PrimitiveType, UnaryOp};
use crate::parser::Parser;
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate a C expression string from a Gorget expression.
    pub fn gen_expr(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::IntLiteral(n) => format!("INT64_C({n})"),
            Expr::FloatLiteral(f) => format!("{f}"),
            Expr::BoolLiteral(b) => if *b { "true" } else { "false" }.to_string(),
            Expr::CharLiteral(c) => format!("'{}'", escape_char(*c)),
            Expr::StringLiteral(s) => self.gen_string_literal(s),
            Expr::NoneLiteral => "NULL".to_string(),

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
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let c_op = binary_op_to_c(*op);
                format!("({l} {c_op} {r})")
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
                let idx = self.gen_expr(index);
                format!("{obj}[{idx}]")
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
                let c_type = c_types::ast_type_to_c(&type_.node, self.scopes);
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

            Expr::Move { expr } | Expr::MutableBorrow { expr } | Expr::Deref { expr } => {
                // In C output, ownership/borrow semantics are erased
                self.gen_expr(expr)
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
                let val = self.gen_expr(is_expr);
                let cond = self.pattern_to_condition_expr(&pattern.node, &val);
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
                let inner = self.gen_expr(try_expr);
                // ? operator: on error, propagate by longjmp-ing directly (error struct is already populated)
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
        // Check for built-in print/println
        if let Expr::Identifier(name) = &callee.node {
            match name.as_str() {
                "print" | "println" => return self.gen_print_call(args, name == "println"),
                "len" => {
                    if let Some(arg) = args.first() {
                        let a = self.gen_expr(&arg.node.value);
                        return format!("(sizeof({a}) / sizeof({a}[0]))");
                    }
                }
                _ => {}
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
                                let enum_name = &self.scopes.get_def(*enum_def_id).name;
                                let field_exprs: Vec<String> =
                                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                                let fields = field_exprs.join(", ");
                                return format!(
                                    "{}({fields})",
                                    c_mangle::mangle_variant(enum_name, vname)
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
                        let inner_type = self.infer_c_type_from_expr(&arg.node.value.node);
                        return format!(
                            "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; (void*)__box_tmp; }})"
                        );
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
        let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
        format!("{callee_name}({})", arg_exprs.join(", "))
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
        let mangled = c_mangle::mangle_method(&type_name, method_name);

        // For non-lvalue receivers (e.g. function calls), we can't take `&recv`
        // directly. Use a GCC statement expression to stash the result in a temp.
        let needs_temp = !matches!(receiver.node, Expr::Identifier(_) | Expr::SelfExpr);
        if needs_temp {
            let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            let mut call_args = format!("&__recv");
            for a in &arg_exprs {
                call_args.push_str(", ");
                call_args.push_str(a);
            }
            format!("({{ __typeof__({recv}) __recv = {recv}; {mangled}({call_args}); }})")
        } else {
            let mut all_args = vec![format!("&{recv}")];
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
            || c_type.as_deref() == Some("GorgetMap");
        let is_set = matches!(type_name.as_str(), "Set" | "HashSet")
            || c_type.as_deref() == Some("GorgetSet");
        let is_string = matches!(type_name.as_str(), "str" | "String")
            || matches!(c_type.as_deref(), Some("const char*"));

        if !is_vector && !is_map && !is_set && !is_string {
            return None;
        }

        let recv = self.gen_expr(receiver);
        let needs_temp = !matches!(receiver.node, Expr::Identifier(_) | Expr::SelfExpr);

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
            return self.gen_string_method(&recv, method_name, needs_temp);
        }

        None
    }

    /// Infer the C type of a receiver expression via the TypeId, if available.
    fn infer_receiver_c_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        if let Expr::Identifier(name) = &expr.node {
            let type_id = self.resolution_map.get(&expr.span.start)
                .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                .or_else(|| {
                    self.scopes.lookup_by_name_anywhere(name)
                        .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                });
            if let Some(tid) = type_id {
                return Some(c_types::type_id_to_c(tid, self.types, self.scopes));
            }
        }
        None
    }

    /// Infer the element C type for a Vector receiver from its TypeId.
    fn infer_vector_elem_type(&self, receiver: &Spanned<Expr>) -> String {
        if let Expr::Identifier(name) = &receiver.node {
            let type_id = self.resolution_map.get(&receiver.span.start)
                .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                .or_else(|| {
                    self.scopes.lookup_by_name_anywhere(name)
                        .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                });
            if let Some(tid) = type_id {
                if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                    if let Some(&elem_tid) = args.first() {
                        return c_types::type_id_to_c(elem_tid, self.types, self.scopes);
                    }
                }
            }
        }
        "int64_t".to_string()
    }

    /// Infer the key and value C types for a Map receiver from its TypeId.
    fn infer_map_kv_types(&self, receiver: &Spanned<Expr>) -> (String, String) {
        if let Expr::Identifier(name) = &receiver.node {
            let type_id = self.resolution_map.get(&receiver.span.start)
                .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                .or_else(|| {
                    self.scopes.lookup_by_name_anywhere(name)
                        .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                });
            if let Some(tid) = type_id {
                if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                    if args.len() >= 2 {
                        let key = c_types::type_id_to_c(args[0], self.types, self.scopes);
                        let val = c_types::type_id_to_c(args[1], self.types, self.scopes);
                        return (key, val);
                    }
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
                    format!("({{ __typeof__({recv}) __recv = {recv}; __recv.len--; GORGET_ARRAY_AT({elem_type}, __recv, __recv.len); }})")
                } else {
                    format!("({{ {recv}.len--; GORGET_ARRAY_AT({elem_type}, {recv}, {recv}.len); }})")
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
            _ => {
                // Unknown method — fall through to normal dispatch
                format!("/* unknown Vector method {method_name} */ 0")
            }
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
                let key_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                let val_type = args.get(1)
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {key_type} __k = {key}; {val_type} __v = {val}; gorget_map_put({recv_ref}, &__k, &__v); }})")
            }
            "get" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let key_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                let (_, val_type) = self.infer_map_kv_types(receiver);
                format!("({{ {key_type} __k = {key}; *({val_type}*)gorget_map_get({recv_ref}, &__k); }})")
            }
            "contains" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let key_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {key_type} __k = {key}; gorget_map_contains({recv_ref}, &__k); }})")
            }
            "len" => {
                format!("(int64_t)gorget_map_len({recv_ref})")
            }
            "remove" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let key_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {key_type} __k = {key}; gorget_map_remove({recv_ref}, &__k); }})")
            }
            "clear" => {
                format!("gorget_map_clear({recv_ref})")
            }
            "is_empty" => {
                format!("(gorget_map_len({recv_ref}) == 0)")
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
        _receiver: &Spanned<Expr>,
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
            _ => None, // Not a known string method — fall through
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
            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
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
                if c_type_args.is_empty() {
                    return "gorget_array_new(sizeof(int64_t))".to_string();
                }
                let elem_size = format!("sizeof({})", c_type_args[0]);
                return format!("gorget_array_new({elem_size})");
            }
            "Dict" | "HashMap" | "Map" => {
                let key_size = c_type_args.first()
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                let val_size = c_type_args.get(1)
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                return format!("gorget_map_new({key_size}, {val_size})");
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
            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
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
    fn infer_receiver_type(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::Identifier(name) => {
                // Try resolution_map first, then fallback to scope lookup
                let type_id = self.resolution_map.get(&expr.span.start)
                    .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                    .or_else(|| {
                        self.scopes.lookup_by_name_anywhere(name)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                    });

                if let Some(type_id) = type_id {
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
            _ => "Unknown".to_string(),
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
        if let Expr::Identifier(name) = &arg.node {
            if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    let (fmt, arg_expr) = self.format_for_type_id(type_id, &expr);
                    return format!("printf(\"{fmt}\\n\", {arg_expr})");
                }
            }
        }

        format!("printf(\"%lld\\n\", (long long){expr})")
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
    /// Handles tuple field access like "_0", "_1._0", etc.
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

        None
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
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                let name = &self.scopes.get_def(*def_id).name;
                panic!("type '{name}' cannot be used in string interpolation")
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
        match &expr.node {
            Expr::Identifier(name) => {
                let def_id = self.scopes.lookup_by_name_anywhere(name)?;
                self.scopes.get_def(def_id).type_id
            }
            Expr::MethodCall {
                receiver, method, ..
            } => {
                let recv_type = self.infer_receiver_c_type(receiver);
                recv_type
                    .as_deref()
                    .and_then(|rt| self.builtin_method_return_type(rt, &method.node))
            }
            Expr::IntLiteral(_) => Some(self.types.int_id),
            Expr::FloatLiteral(_) => Some(self.types.float_id),
            Expr::BoolLiteral(_) => Some(self.types.bool_id),
            Expr::BinaryOp { .. } => Some(self.types.int_id),
            _ => None,
        }
    }

    /// Map (receiver C type, method name) → return TypeId for known builtins.
    fn builtin_method_return_type(
        &self,
        receiver_type: &str,
        method: &str,
    ) -> Option<crate::semantic::ids::TypeId> {
        match (receiver_type, method) {
            ("GorgetArray", "len" | "get" | "pop") => Some(self.types.int_id),
            ("GorgetMap", "len" | "get") => Some(self.types.int_id),
            ("GorgetMap", "contains") => Some(self.types.bool_id),
            ("GorgetSet", "len") => Some(self.types.int_id),
            ("GorgetSet", "contains") => Some(self.types.bool_id),
            ("const char*", "len") => Some(self.types.int_id),
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
            return_type: "int64_t".to_string(), // default; refined later
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
                if seen.insert(name.clone()) {
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
                "({{ GorgetMap __comp = gorget_map_new(sizeof({key_type}), sizeof({val_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
                gorget_map_put(&__comp, &__k, &__v); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetMap __comp = gorget_map_new(sizeof({key_type}), sizeof({val_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            __typeof__({iter}[0]) {var_name} = {iter}[__i]; \
            {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
            gorget_map_put(&__comp, &__k, &__v); }} \
            }} __comp; }})"
        )
    }

    /// Convert a pattern to a C boolean condition for `is` expressions.
    fn pattern_to_condition_expr(&self, pattern: &crate::parser::ast::Pattern, scrutinee: &str) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
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
                            let enum_name = &self.scopes.get_def(*enum_def_id).name;
                            let tag = c_mangle::mangle_tag(enum_name, name);
                            return format!("{scrutinee}.tag == {tag}");
                        }
                    }
                }
                "1".to_string()
            }
            Pattern::Constructor { path, .. } => {
                if path.len() == 2 {
                    let tag = c_mangle::mangle_tag(&path[0].node, &path[1].node);
                    format!("{scrutinee}.tag == {tag}")
                } else if path.len() == 1 {
                    // Try to find the enum for this variant
                    let variant_name = &path[0].node;
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, _) in &info.variants {
                            if vname == variant_name {
                                let enum_name = &self.scopes.get_def(*enum_def_id).name;
                                let tag = c_mangle::mangle_tag(enum_name, variant_name);
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
                    .map(|p| self.pattern_to_condition_expr(&p.node, scrutinee))
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

        let mut parts = Vec::new();
        parts.push(format!("__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}"));

        // Build if-else chain; each arm assigns to __gorget_match_result
        let mut arm_parts = Vec::new();
        let mut first_body: Option<String> = None;
        let mut first = true;
        for arm in arms {
            let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut");
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

                let mut code = format!(
                    "__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}; \
                     {result_type} __gorget_do_result; "
                );

                let mut first = true;
                for arm in arms {
                    let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut");
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
            } => {
                let const_prefix = if *is_const { "const " } else { "" };
                match &pattern.node {
                    crate::parser::ast::Pattern::Binding(name) => {
                        let escaped = c_mangle::escape_keyword(name);
                        let c_type = match &type_.node {
                            crate::parser::ast::Type::Inferred => self.infer_c_type_from_expr(&value.node),
                            _ => c_types::ast_type_to_c(&type_.node, self.scopes),
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
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
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
