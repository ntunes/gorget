/// Generic/trait/inference helper methods for expression codegen.
use crate::parser::ast::{Expr, PrimitiveType, Type};
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

impl CodegenContext<'_> {
    pub(super) fn resolve_trait_object_type(&mut self, expr: &Spanned<Expr>) -> Option<String> {
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.resolution_map.get(&expr.span.start)
                .filter(|did| self.scopes.get_def(**did).name == *name)
            {
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
            if let Some(def_id) = self.scoped_lookup(name) {
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
    pub(super) fn gen_generic_call(
        &mut self,
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

                // Default trait: Type[T].default()
                if method_name == "default" {
                    match type_name.as_str() {
                        "Vector" | "List" | "Array" => {
                            let elem_size = if c_type_args.is_empty() {
                                "sizeof(int64_t)".to_string()
                            } else {
                                format!("sizeof({})", c_type_args[0])
                            };
                            return format!("gorget_array_new({elem_size})");
                        }
                        "Dict" => {
                            let mangled = c_mangle::mangle_generic("GorgetDict", &c_type_args);
                            self.register_generic("GorgetDict", &c_type_args, super::GenericInstanceKind::Map { ordered: true });
                            return format!("{mangled}__new()");
                        }
                        "HashMap" => {
                            let mangled = c_mangle::mangle_generic("GorgetMap", &c_type_args);
                            self.register_generic("GorgetMap", &c_type_args, super::GenericInstanceKind::Map { ordered: false });
                            return format!("{mangled}__new()");
                        }
                        "Option" => {
                            let mangled = c_mangle::mangle_generic("Option", &c_type_args);
                            self.register_generic("Option", &c_type_args, super::GenericInstanceKind::Enum);
                            return format!("{}()", c_mangle::mangle_variant(&mangled, "None"));
                        }
                        _ => {
                            let mangled_type = c_mangle::mangle_generic(type_name, &c_type_args);
                            let func = c_mangle::mangle_trait_method("Default", &mangled_type, "default", &[]);
                            return format!("{func}()");
                        }
                    }
                }

                // From trait: Type[T].from(value) → From__argtype_for_Type__T__from(value)
                if method_name == "from" {
                    let mangled_type = c_mangle::mangle_generic(type_name, &c_type_args);
                    let trait_type_args: Vec<String> = args.first()
                        .map(|a| vec![self.infer_c_type_from_expr(&a.node.value.node)])
                        .unwrap_or_default();
                    let func = c_mangle::mangle_trait_method("From", &mangled_type, "from", &trait_type_args);
                    self.register_generic(type_name, &c_type_args, super::GenericInstanceKind::Struct);
                    let arg_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    return format!("{func}({})", arg_exprs.join(", "));
                }

                // TryFrom trait: Type[T].try_from(value) → TryFrom__argtype_for_Type__T__try_from(value)
                if method_name == "try_from" {
                    let mangled_type = c_mangle::mangle_generic(type_name, &c_type_args);
                    let trait_type_args: Vec<String> = args.first()
                        .map(|a| vec![self.infer_c_type_from_expr(&a.node.value.node)])
                        .unwrap_or_default();
                    let func = c_mangle::mangle_trait_method("TryFrom", &mangled_type, "try_from", &trait_type_args);
                    self.register_generic(type_name, &c_type_args, super::GenericInstanceKind::Struct);
                    let arg_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    return format!("{func}({})", arg_exprs.join(", "));
                }

                let mangled = c_mangle::mangle_method(type_name, method_name);
                let full_mangled = c_mangle::mangle_generic(&mangled, &c_type_args);
                self.register_generic(&mangled, &c_type_args, super::GenericInstanceKind::Function);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| {
                        let expr = self.gen_expr(&a.node.value);
                        self.wrap_borrow_arg(expr, &a.node.value.node, a.node.ownership)
                    }).collect();
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
                let alloc_expr = self.extract_alloc_arg(args);
                let pos_args = Self::filter_alloc_arg(args);
                let elem_size = if c_type_args.is_empty() {
                    "sizeof(int64_t)".to_string()
                } else {
                    format!("sizeof({})", c_type_args[0])
                };
                let ctor = if pos_args.len() == 1 {
                    // Vector[T](n) → with_capacity
                    let cap = self.gen_expr(&pos_args[0].node.value);
                    format!("gorget_array_with_capacity({elem_size}, (size_t)({cap}))")
                } else {
                    format!("gorget_array_new({elem_size})")
                };
                return if let Some(alloc) = alloc_expr {
                    self.wrap_with_alloc(&alloc, &ctor, "GorgetArray")
                } else {
                    ctor
                };
            }
            "Dict" => {
                let alloc_expr = self.extract_alloc_arg(args);
                let mangled = c_mangle::mangle_generic("GorgetDict", &c_type_args);
                self.register_generic("GorgetDict", &c_type_args, super::GenericInstanceKind::Map { ordered: true });
                let ctor = format!("{mangled}__new()");
                return if let Some(alloc) = alloc_expr {
                    self.wrap_with_alloc(&alloc, &ctor, &mangled)
                } else {
                    ctor
                };
            }
            "HashMap" => {
                let alloc_expr = self.extract_alloc_arg(args);
                let mangled = c_mangle::mangle_generic("GorgetMap", &c_type_args);
                self.register_generic("GorgetMap", &c_type_args, super::GenericInstanceKind::Map { ordered: false });
                let ctor = format!("{mangled}__new()");
                return if let Some(alloc) = alloc_expr {
                    self.wrap_with_alloc(&alloc, &ctor, &mangled)
                } else {
                    ctor
                };
            }
            "Set" | "HashSet" => {
                let alloc_expr = self.extract_alloc_arg(args);
                let elem_size = c_type_args.first()
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                let ctor = format!("gorget_set_new({elem_size})");
                return if let Some(alloc) = alloc_expr {
                    self.wrap_with_alloc(&alloc, &ctor, "GorgetSet")
                } else {
                    ctor
                };
            }
            "Channel" => {
                let alloc_expr = self.extract_alloc_arg(args);
                let pos_args = Self::filter_alloc_arg(args);
                let elem_size = c_type_args.first()
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                let cap = if !pos_args.is_empty() {
                    self.gen_expr(&pos_args[0].node.value)
                } else {
                    "1".to_string()
                };
                let ctor = format!("gorget_channel_new((size_t)({cap}), {elem_size})");
                return if let Some(alloc) = alloc_expr {
                    self.wrap_with_alloc(&alloc, &ctor, "GorgetChannel*")
                } else {
                    ctor
                };
            }
            _ => {}
        }

        // Check if the callee is a generic struct constructor
        if self.generic_struct_templates.contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Struct);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        // Check if the callee is a generic enum variant constructor
        if self.generic_enum_templates.contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Enum);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        let mangled = c_mangle::mangle_generic(&base_name, &c_type_args);
        self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Function);

        // Look up the template to determine Callable-typed params and their signatures
        let subs = self.build_type_substitutions(
            self.generic_fn_templates.get(&base_name).and_then(|t| t.generic_params.as_ref()),
            &c_type_args,
        );
        let callable_param_info: Option<Vec<Option<(Vec<String>, String, Vec<crate::parser::ast::Ownership>)>>> =
            self.generic_fn_templates.get(&base_name).map(|tmpl| {
                tmpl.params.iter()
                    .filter(|p| p.node.name.node != "self")
                    .map(|p| {
                        if let crate::parser::ast::Type::Named { name, generic_args } = &p.node.type_.node {
                            if matches!(name.node.as_str(), "Callable" | "MutCallable" | "ConsumeCallable") {
                                if generic_args.len() == 1 {
                                    if let crate::parser::ast::Type::Function { return_type, params: fn_params, param_ownerships } = &generic_args[0].node {
                                        let ret_c = self.substitute_type(&return_type.node, &subs);
                                        let param_c: Vec<String> = fn_params.iter().enumerate()
                                            .map(|(i, fp)| {
                                                let base = self.substitute_type(&fp.node, &subs);
                                                let ownership = param_ownerships.get(i).copied()
                                                    .unwrap_or(crate::parser::ast::Ownership::Borrow);
                                                if matches!(ownership, crate::parser::ast::Ownership::MutableBorrow) {
                                                    format!("{base}*")
                                                } else {
                                                    base
                                                }
                                            })
                                            .collect();
                                        return Some((param_c, ret_c, param_ownerships.clone()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .collect()
            });

        let arg_exprs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
            let expr = self.gen_expr(&a.node.value);
            // Wrap named functions passed to Callable-typed params in GorgetClosure
            let expr = if let Some(Some((param_c, ret_c, _ownerships))) =
                callable_param_info.as_ref().and_then(|info| info.get(i))
            {
                if !self.closure_vars.contains(expr.as_str()) && !expr.starts_with("(GorgetClosure)") {
                    // Create an adapter closure for the named function
                    let id = self.closure_counter;
                    self.closure_counter += 1;
                    let struct_name = format!("__Closure_{id}");
                    let fn_name = super::c_mangle::mangle_closure(id);
                    let closure_params: Vec<(String, String)> = param_c.iter().enumerate()
                        .map(|(j, ct)| (format!("__p{j}"), ct.clone()))
                        .collect();
                    let arg_names: Vec<&str> = closure_params.iter().map(|(n, _)| n.as_str()).collect();
                    let call_expr = format!("{expr}({})", arg_names.join(", "));

                    self.lifted_closures.push(super::LiftedClosure {
                        id,
                        struct_name,
                        captures: vec![],
                        params: closure_params,
                        return_type: ret_c.clone(),
                        body: call_expr,
                    });
                    format!("(GorgetClosure){{.fn_ptr = (void*){fn_name}_fn, .env = NULL}}")
                } else {
                    expr
                }
            } else {
                expr
            };
            self.wrap_borrow_arg(expr, &a.node.value.node, a.node.ownership)
        }).collect();
        format!("{mangled}({})", arg_exprs.join(", "))
    }

    /// Generate a generic method call: `obj.method[T](args)` → `Type__method__T(&obj, args)`
    pub(super) fn gen_generic_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &Spanned<String>,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let method_name = method.node.as_str();
        let is_pointer_param = matches!(&receiver.node, Expr::Identifier(name) if self.pointer_params.contains(&c_mangle::escape_keyword(name)));
        let recv = if is_pointer_param {
            if let Expr::Identifier(name) = &receiver.node {
                c_mangle::escape_keyword(name)
            } else {
                unreachable!()
            }
        } else {
            self.gen_expr(receiver)
        };
        let type_name = self.infer_receiver_type(receiver);
        let c_type_args: Vec<String> = type_args
            .iter()
            .map(|a| self.type_to_c(&a.node))
            .collect();
        let base_method = c_mangle::mangle_method(&type_name, method_name);
        let mangled = c_mangle::mangle_generic(&base_method, &c_type_args);
        self.register_generic(&base_method, &c_type_args, super::GenericInstanceKind::Function);
        let self_arg = if is_pointer_param { recv.clone() } else { format!("&{recv}") };
        let mut all_args = vec![self_arg];
        for arg in args {
            let expr = self.gen_expr(&arg.node.value);
            all_args.push(self.wrap_borrow_arg(expr, &arg.node.value.node, arg.node.ownership));
        }
        format!("{mangled}({})", all_args.join(", "))
    }

    /// Map a `TypeId` to the Gorget type name (for mangling).
    pub(super) fn type_name_from_type_id(&mut self, type_id: crate::semantic::ids::TypeId) -> Option<String> {
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

    /// Check if a method on a type comes from a trait equip (not inherent impl).
    /// Returns the trait name and the trait's generic type args (as C type strings)
    /// if found, None if it's an inherent method.
    pub(super) fn find_trait_for_method(&mut self, type_name: &str, method_name: &str) -> Option<(String, Vec<String>)> {
        // For generic instantiations (e.g. "Wrapper__int64_t"), also try the base name
        // ("Wrapper") since impls are registered under base names.
        let base_name: Option<String> = self.generic_instances.iter()
            .find(|i| i.mangled_name == type_name)
            .map(|i| i.base_name.clone());
        let mut names_to_check = vec![type_name.to_string()];
        if let Some(ref base) = base_name {
            names_to_check.push(base.clone());
        }

        // First check if there's an inherent impl with this method — inherent wins
        for impl_info in &self.traits.impls {
            if names_to_check.iter().any(|n| impl_info.self_type_name == n.as_str())
                && impl_info.trait_.is_none()
                && impl_info.methods.contains_key(method_name)
            {
                return None; // Inherent method takes priority
            }
        }
        // Then check trait impls
        for impl_info in &self.traits.impls {
            if names_to_check.iter().any(|n| impl_info.self_type_name == n.as_str()) && impl_info.trait_.is_some() {
                // Check if the method is directly in the equip block
                if impl_info.methods.contains_key(method_name) {
                    let trait_type_args: Vec<String> = impl_info.trait_generic_args.iter()
                        .map(|a| super::c_types::ast_type_to_c(a, self.scopes))
                        .collect();
                    return impl_info.trait_name.clone().map(|n| (n, trait_type_args));
                }
                // Check if the trait (or its parents) defines this method with a default
                if let Some(trait_def_id) = impl_info.trait_ {
                    if self.trait_hierarchy_has_method(trait_def_id, method_name) {
                        let trait_type_args: Vec<String> = impl_info.trait_generic_args.iter()
                            .map(|a| super::c_types::ast_type_to_c(a, self.scopes))
                            .collect();
                        return impl_info.trait_name.clone().map(|n| (n, trait_type_args));
                    }
                }
            }
        }
        None
    }

    /// Check if a trait (or any of its parent traits) defines a method.
    fn trait_hierarchy_has_method(
        &mut self,
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

    pub(super) fn infer_receiver_type(&mut self, expr: &Spanned<Expr>) -> String {
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
                    let def_id = self.resolution_map.get(&callee.span.start)
                        .filter(|did| self.scopes.get_def(**did).name == *name)
                        .copied()
                        .or_else(|| {
                            // Scope-aware fallback for Variable/Const/Function
                            self.scoped_lookup(name)
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
                // Builtin method return types for stdlib types
                if recv_type == "Response" {
                    return match method.node.as_str() {
                        "status" => "int",
                        "body" | "header" => "str",
                        _ => "Unknown",
                    }.to_string();
                }
                if recv_type == "Client" {
                    return match method.node.as_str() {
                        "get" | "post" | "put" | "delete" | "patch" | "head" => "Result",
                        _ => "Unknown",
                    }.to_string();
                }
                if recv_type == "Socket" {
                    return match method.node.as_str() {
                        "read" | "read_exact" => "Vector",
                        "read_line" => "String",
                        "write" | "write_str" => "int",
                        _ => "Unknown",
                    }.to_string();
                }
                if recv_type == "TlsSocket" {
                    return match method.node.as_str() {
                        "read" | "read_exact" => "Vector",
                        "read_line" => "String",
                        "write" | "write_str" => "int",
                        _ => "Unknown",
                    }.to_string();
                }
                if recv_type == "CipherContext" {
                    return match method.node.as_str() {
                        "encrypt" | "decrypt" => "Vector",
                        _ => "Unknown",
                    }.to_string();
                }
                // Builtin collection methods returning Option
                if matches!(recv_type.as_str(), "Vector" | "List" | "Array" | "Dict" | "HashMap")
                    && method.node == "get"
                {
                    return "Option".to_string();
                }
                if matches!(recv_type.as_str(), "Vector" | "List" | "Array")
                    && matches!(method.node.as_str(), "pop" | "remove" | "index_of")
                {
                    return "Option".to_string();
                }
                if matches!(recv_type.as_str(), "str" | "String")
                    && method.node == "index_of"
                {
                    return "Option".to_string();
                }
                // Option methods that return Option
                if recv_type == "Option" && matches!(method.node.as_str(),
                    "map" | "and_then" | "or_else" | "or" | "filter" | "flatten"
                ) {
                    return "Option".to_string();
                }
                // Result methods that return Result
                if recv_type == "Result" && matches!(method.node.as_str(),
                    "map" | "and_then" | "or_else" | "or" | "map_err"
                ) {
                    return "Result".to_string();
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
                // Fallback: use semantic expr_types map
                if let Some(&type_id) = self.expr_types.get(&expr.span) {
                    if let Some(name) = self.type_name_from_type_id(type_id) {
                        return name;
                    }
                }
                "Unknown".to_string()
            }
            Expr::FieldAccess { object, field } => {
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type.clone(), field.node.clone());
                    if let Some(field_type) = self.field_type_names.get(&key) {
                        return self.type_to_resolved_name(field_type);
                    }
                    // Fallback: for monomorphized generic types like "SparseSet__Health",
                    // try the base name "SparseSet" since field_type_names may use it.
                    if let Some(pos) = obj_type.find("__") {
                        let base = &obj_type[..pos];
                        let fallback_key = (base.to_string(), field.node.clone());
                        if let Some(field_type) = self.field_type_names.get(&fallback_key) {
                            return self.type_to_resolved_name(field_type);
                        }
                    }
                }
                "Unknown".to_string()
            }
            Expr::Index { object, index } => {
                // String slice: str[range] → str; single index → char
                if self.is_string_expr(object) {
                    if matches!(&index.node, Expr::Range { .. }) {
                        return "str".to_string();
                    }
                    return "char".to_string();
                }
                "Unknown".to_string()
            }
            Expr::StringLiteral(_) => "str".to_string(),
            _ => "Unknown".to_string(),
        }
    }

    /// Infer the mangled C type name for a receiver, handling generic instantiations.
    /// For `Pair[int]` returns `"Pair__int64_t"`, for non-generic `Point` returns `"Point"`.
    /// Falls back to `infer_receiver_type()` for non-generic types.
    pub(super) fn infer_receiver_mangled_type(&mut self, expr: &Spanned<Expr>) -> String {
        if let Some(type_id) = self.resolve_expr_type_id(expr) {
            // If the type is a generic param with an active substitution, use it directly.
            if let crate::semantic::types::ResolvedType::Defined(def_id) = self.types.get(type_id) {
                let def = self.scopes.get_def(*def_id);
                if def.kind == DefKind::GenericParam {
                    if let Some((_, c_type)) = self.type_subs.iter().find(|(n, _)| *n == def.name) {
                        return c_type.clone();
                    }
                }
            }
            if let crate::semantic::types::ResolvedType::Generic(def_id, args) = self.types.get(type_id) {
                let base = c_types::def_name_to_c(*def_id, self.scopes);
                let c_args: Vec<String> = args
                    .iter()
                    .map(|tid| self.type_id_to_c_substituted(*tid))
                    .collect();
                let result = c_mangle::mangle_generic(&base, &c_args);
                // If substitution produced clean results, use them.
                // Otherwise fall through to AST-based resolution via monomorphized_param_c_types.
                if !result.contains("error") {
                    return result;
                }
            }
        }
        // Fallback: check monomorphized parameter C types (set during generic body codegen).
        // This handles cases where TypeId-based resolution fails for multi-param generics.
        if let Expr::Identifier(name) = &expr.node {
            let escaped = c_mangle::escape_keyword(name);
            if let Some((_, c_type)) = self.monomorphized_param_c_types.iter().find(|(n, _)| *n == escaped) {
                return c_type.clone();
            }
        }
        self.infer_receiver_type(expr)
    }

    /// Extract the base type name from an AST Type (static, no generic mangling).
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
                PrimitiveType::Str | PrimitiveType::CStr | PrimitiveType::StringType => "str".to_string(),
                PrimitiveType::Char => "char".to_string(),
                PrimitiveType::Void => "void".to_string(),
            },
            _ => "Unknown".to_string(),
        }
    }

    /// Resolve an AST Type to a name suitable for method dispatch.
    /// For user-defined generic types like SparseSet[Health], returns the
    /// mangled name "SparseSet__Health". For built-in types, returns the base name.
    pub(super) fn type_to_resolved_name(&self, ty: &Type) -> String {
        match ty {
            Type::Named { name, generic_args } if !generic_args.is_empty() => {
                // Built-in collection/wrapper types — return base name
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" | "Dict" | "Map" | "HashMap"
                    | "Box" | "Rc" | "Arc" | "Weak" | "Cell" | "RefCell" | "Mutex" | "RwLock"
                    | "Option" | "Result" | "Iterator" => name.node.clone(),
                    _ => {
                        // User-defined generic type → mangle for method dispatch
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        c_mangle::mangle_generic(&name.node, &c_args)
                    }
                }
            }
            _ => Self::type_to_name(ty),
        }
    }

    /// Check if `name` resolves to a stdlib item of the given kind (synthetic def with dummy span).
    fn is_stdlib_item(&self, name: &str, kind: DefKind) -> bool {
        self.scopes
            .lookup(name)
            .map(|did| {
                let def = self.scopes.get_def(did);
                def.kind == kind && def.span == crate::span::Span::dummy()
            })
            .unwrap_or(false)
    }

    pub(super) fn is_stdlib_call(&self, name: &str) -> bool {
        self.is_stdlib_item(name, DefKind::Function)
    }

    pub(super) fn is_stdlib_static(&self, name: &str) -> bool {
        self.is_stdlib_item(name, DefKind::Static)
    }

    pub(super) fn is_stdlib_const(&self, name: &str) -> bool {
        self.is_stdlib_item(name, DefKind::Const)
    }

    pub(super) fn is_string_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::StringLiteral(_) => true,
            Expr::Identifier(_) => {
                let type_name = self.infer_receiver_type(expr);
                if type_name == "str" || type_name == "cstr" || type_name == "String" {
                    return true;
                }
                let c_type = self.infer_receiver_c_type(expr);
                matches!(c_type.as_deref(), Some("const char*") | Some("GorgetString"))
            }
            Expr::MethodCall { .. } | Expr::Call { .. } => {
                let c_type = self.infer_c_type_from_expr(&expr.node);
                c_type == "const char*" || c_type == "GorgetString"
            }
            Expr::FieldAccess { .. } => {
                let c_type = self.infer_c_type_from_expr(&expr.node);
                c_type == "const char*" || c_type == "GorgetString"
            }
            Expr::Index { object, index } => {
                // str[range] returns str
                self.is_string_expr(object) && matches!(&index.node, Expr::Range { .. })
            }
            _ => false,
        }
    }

    /// Check if an expression evaluates to a Vector (GorgetArray) type.
    pub(super) fn is_vector_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        let type_name = self.infer_receiver_type(expr);
        if matches!(type_name.as_str(), "Vector" | "List" | "Array") {
            return true;
        }
        // Check if variable was auto-promoted from array literal to GorgetArray
        if let Expr::Identifier(name) = &expr.node {
            if self.vector_vars.contains(&c_mangle::escape_keyword(name)) {
                return true;
            }
        }
        let c_type = self.infer_receiver_c_type(expr);
        c_type.as_deref() == Some("GorgetArray")
    }

    /// If `expr` has a user-defined type that implements the given trait, return the mangled
    /// type name and the trait's generic type args (as C type strings).
    /// Excludes primitives and built-in collection types.
    pub(super) fn try_operator_trait_type(&mut self, expr: &Spanned<Expr>, trait_name: &str) -> Option<(String, Vec<String>)> {
        let type_name = self.infer_receiver_type(expr);
        // Exclude primitives and builtins — these have hardcoded codegen paths
        if matches!(type_name.as_str(), "Unknown" | "int" | "float" | "bool" | "str" | "char"
            | "String" | "Vector" | "Dict" | "HashMap" | "Set" | "HashSet" | "Option" | "Result") {
            return None;
        }
        if self.traits.has_trait_impl_by_name(&type_name, trait_name) {
            let mangled = self.infer_receiver_mangled_type(expr);
            let trait_type_args = self.lookup_trait_type_args(&type_name, trait_name);
            Some((mangled, trait_type_args))
        } else {
            None
        }
    }

    /// If `expr` has a Defined (struct) type that implements Equatable, return the mangled type name
    /// and trait type args.
    pub(super) fn try_equatable_type(&mut self, expr: &Spanned<Expr>) -> Option<(String, Vec<String>)> {
        self.try_operator_trait_type(expr, "Equatable")
    }

    /// Generate a binary operator trait call: `Trait_for_Type__method(&left, right)`
    /// Handles rvalue receivers via statement expression with temp variable.
    pub(super) fn gen_binary_op_trait_call(
        &mut self,
        trait_name: &str,
        method_name: &str,
        type_name: &str,
        trait_type_args: &[String],
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
    ) -> String {
        let l = self.gen_expr(left);
        let r = self.gen_expr(right);
        let mangled = c_mangle::mangle_trait_method(trait_name, type_name, method_name, trait_type_args);
        if !super::c_expr::is_lvalue(&left.node) {
            format!("({{ __typeof__({l}) __recv = {l}; {mangled}(&__recv, {r}); }})")
        } else {
            format!("{mangled}(&{l}, {r})")
        }
    }

    /// Generate a unary operator trait call: `Trait_for_Type__method(&operand)`
    /// Handles rvalue receivers via statement expression with temp variable.
    pub(super) fn gen_unary_op_trait_call(
        &mut self,
        trait_name: &str,
        method_name: &str,
        type_name: &str,
        trait_type_args: &[String],
        operand: &Spanned<Expr>,
    ) -> String {
        let inner = self.gen_expr(operand);
        let mangled = c_mangle::mangle_trait_method(trait_name, type_name, method_name, trait_type_args);
        if !super::c_expr::is_lvalue(&operand.node) {
            format!("({{ __typeof__({inner}) __recv = {inner}; {mangled}(&__recv); }})")
        } else {
            format!("{mangled}(&{inner})")
        }
    }

    /// Look up the trait's generic type args (as C type strings) for a given type+trait pair.
    /// Handles monomorphized generic types by falling back to the base name if the mangled
    /// name doesn't match any impl's self_type_name.
    pub(super) fn lookup_trait_type_args(&self, type_name: &str, trait_name: &str) -> Vec<String> {
        // Try the type name directly first
        let args = self.traits.trait_generic_args_by_name(type_name, trait_name);
        if !args.is_empty() {
            return args.iter()
                .map(|a| super::c_types::ast_type_to_c(a, self.scopes))
                .collect();
        }
        // For monomorphized generic types (e.g. "SparseSet__Health"), try the base name
        let base_name = self.generic_instances.iter()
            .find(|i| i.mangled_name == type_name)
            .map(|i| i.base_name.as_str());
        if let Some(base) = base_name {
            let args = self.traits.trait_generic_args_by_name(base, trait_name);
            return args.iter()
                .map(|a| super::c_types::ast_type_to_c(a, self.scopes))
                .collect();
        }
        Vec::new()
    }
}
