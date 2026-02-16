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
            "Set" | "HashSet" => {
                let elem_size = c_type_args.first()
                    .map(|t| format!("sizeof({t})"))
                    .unwrap_or_else(|| "sizeof(int64_t)".to_string());
                return format!("gorget_set_new({elem_size})");
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
        let arg_exprs: Vec<String> = args.iter().map(|a| {
            let expr = self.gen_expr(&a.node.value);
            self.wrap_borrow_arg(expr, &a.node.value.node, a.node.ownership)
        }).collect();
        format!("{mangled}({})", arg_exprs.join(", "))
    }

    /// Generate a generic method call: `obj.method[T](args)` → `Type__method__T(&obj, args)`
    pub(super) fn gen_generic_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
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
    /// Returns the trait name if found, None if it's an inherent method.
    pub(super) fn find_trait_for_method(&mut self, type_name: &str, method_name: &str) -> Option<String> {
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
                        "read_line" => "str",
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
                return c_mangle::mangle_generic(&base, &c_args);
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
                PrimitiveType::Str | PrimitiveType::StringType => "str".to_string(),
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
                if type_name == "str" || type_name == "String" {
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

    /// If `expr` has a Defined (struct) type that implements Equatable, return the mangled type name.
    /// For generic types like `Pair[int]`, returns `"Pair__int64_t"`.
    pub(super) fn try_equatable_type(&mut self, expr: &Spanned<Expr>) -> Option<String> {
        let type_name = self.infer_receiver_type(expr);
        // Exclude primitives and builtins
        if matches!(type_name.as_str(), "Unknown" | "int" | "float" | "bool" | "str" | "char"
            | "Vector" | "Dict" | "Set" | "Option" | "Result") {
            return None;
        }
        if self.traits.has_trait_impl_by_name(&type_name, "Equatable") {
            // Use mangled name for generic types (e.g., Pair[int] → Pair__int64_t)
            Some(self.infer_receiver_mangled_type(expr))
        } else {
            None
        }
    }
}
