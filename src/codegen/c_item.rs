/// Top-level item codegen: functions, structs, enums, impl blocks, const/static.
use crate::parser::ast::*;
use super::c_emitter::CEmitter;
use super::c_mangle;
use super::c_types;
use super::{CodegenContext, DropScopeKind};

impl CodegenContext<'_> {
    // ─── Forward Declarations ────────────────────────────────

    /// Emit forward declarations for all types and functions.
    pub fn emit_forward_declarations(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Forward Declarations ──");

        // Forward-declare structs (skip generic templates)
        for item in &module.items {
            if let Item::Struct(s) = &item.node {
                if s.generic_params.is_some() {
                    continue; // Generic template — emitted per-instantiation
                }
                let name = &s.name.node;
                emitter.emit_line(&format!("typedef struct {name} {name};"));
            }
        }

        // Forward-declare enums (skip generic templates)
        for item in &module.items {
            if let Item::Enum(e) = &item.node {
                if e.generic_params.is_some() {
                    continue; // Generic template — emitted per-instantiation
                }
                let name = &e.name.node;
                emitter.emit_line(&format!("typedef struct {name} {name};"));
            }
        }

        // Forward-declare newtypes
        for item in &module.items {
            if let Item::Newtype(nt) = &item.node {
                let name = &nt.name.node;
                emitter.emit_line(&format!("typedef struct {name} {name};"));
            }
        }

        // Forward-declare vtable and trait object structs
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                let vtable_name = c_mangle::mangle_vtable_struct(&t.name.node);
                let trait_obj_name = c_mangle::mangle_trait_obj(&t.name.node);
                emitter.emit_line(&format!("typedef struct {vtable_name} {vtable_name};"));
                emitter.emit_line(&format!("typedef struct {trait_obj_name} {trait_obj_name};"));
            }
        }

        emitter.blank_line();
    }

    // ─── Type Definitions ────────────────────────────────────

    /// Emit all type definitions (structs, enums).
    pub fn emit_type_definitions(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Type Definitions ──");

        for item in &module.items {
            match &item.node {
                Item::Struct(s) => self.emit_struct_def(s, emitter),
                Item::Enum(e) => self.emit_enum_def(e, emitter),
                Item::TypeAlias(a) => self.emit_type_alias(a, emitter),
                Item::Newtype(nt) => self.emit_newtype(nt, emitter),
                Item::Trait(t) => self.emit_trait_def(t, emitter),
                _ => {}
            }
        }

        emitter.blank_line();
    }

    /// Emit a struct definition.
    fn emit_struct_def(&self, s: &StructDef, emitter: &mut CEmitter) {
        if s.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        let name = &s.name.node;
        emitter.emit_line(&format!("struct {name} {{"));
        emitter.indent();
        for field in &s.fields {
            let field_type = c_types::ast_type_to_c(&field.node.type_.node, self.scopes);
            let field_name = c_mangle::escape_keyword(&field.node.name.node);
            let decl = c_types::c_declare(&field_type, &field_name);
            emitter.emit_line(&format!("{decl};"));
        }
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();
    }

    /// Emit an enum definition as tagged union.
    fn emit_enum_def(&self, e: &EnumDef, emitter: &mut CEmitter) {
        if e.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        let name = &e.name.node;

        // Tag enum
        emitter.emit_line(&format!("typedef enum {{"));
        emitter.indent();
        for (i, variant) in e.variants.iter().enumerate() {
            let tag = c_mangle::mangle_tag(name, &variant.node.name.node);
            if i < e.variants.len() - 1 {
                emitter.emit_line(&format!("{tag},"));
            } else {
                emitter.emit_line(&format!("{tag}"));
            }
        }
        emitter.dedent();
        emitter.emit_line(&format!("}} {name}_Tag;"));
        emitter.blank_line();

        // Data structs for variants with fields
        let mut has_data_variants = false;
        for variant in &e.variants {
            if let VariantFields::Tuple(fields) = &variant.node.fields {
                has_data_variants = true;
                let data_name = c_mangle::mangle_variant_data(name, &variant.node.name.node);
                emitter.emit_line(&format!("typedef struct {{"));
                emitter.indent();
                for (i, field) in fields.iter().enumerate() {
                    let field_type = c_types::ast_type_to_c(&field.node, self.scopes);
                    let decl = c_types::c_declare(&field_type, &format!("_{i}"));
                    emitter.emit_line(&format!("{decl};"));
                }
                emitter.dedent();
                emitter.emit_line(&format!("}} {data_name};"));
                emitter.blank_line();
            }
        }

        // Tagged union struct
        emitter.emit_line(&format!("struct {name} {{"));
        emitter.indent();
        emitter.emit_line(&format!("{name}_Tag tag;"));
        if has_data_variants {
            emitter.emit_line("union {");
            emitter.indent();
            for variant in &e.variants {
                if let VariantFields::Tuple(_) = &variant.node.fields {
                    let data_name =
                        c_mangle::mangle_variant_data(name, &variant.node.name.node);
                    let field_name = &variant.node.name.node;
                    emitter.emit_line(&format!("{data_name} {field_name};"));
                }
            }
            emitter.dedent();
            emitter.emit_line("} data;");
        }
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();

        // Inline constructor functions
        for variant in &e.variants {
            let variant_name = &variant.node.name.node;
            let constructor = c_mangle::mangle_variant(name, variant_name);
            let tag = c_mangle::mangle_tag(name, variant_name);

            match &variant.node.fields {
                VariantFields::Unit => {
                    emitter.emit_line(&format!(
                        "static inline {name} {constructor}(void) {{ return ({name}){{.tag = {tag}}}; }}"
                    ));
                }
                VariantFields::Tuple(fields) => {
                    let params: Vec<String> = fields
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let t = c_types::ast_type_to_c(&f.node, self.scopes);
                            format!("{t} _{i}")
                        })
                        .collect();
                    let assigns: Vec<String> = (0..fields.len())
                        .map(|i| format!("._{i} = _{i}"))
                        .collect();
                    let data_name = c_mangle::mangle_variant_data(name, variant_name);
                    emitter.emit_line(&format!(
                        "static inline {name} {constructor}({}) {{ return ({name}){{.tag = {tag}, .data.{variant_name} = ({data_name}){{{}}}}};  }}",
                        params.join(", "),
                        assigns.join(", ")
                    ));
                }
            }
        }
        emitter.blank_line();
    }

    // ─── Function Declarations ───────────────────────────────

    /// Emit function declarations (prototypes).
    pub fn emit_function_declarations(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Function Declarations ──");

        for item in &module.items {
            match &item.node {
                Item::Function(f) => {
                    if f.name.node != "main" {
                        self.emit_function_prototype(f, None, emitter);
                    }
                }
                Item::Equip(impl_block) => {
                    let type_name = self.impl_type_name(impl_block);
                    let trait_name = self.impl_trait_name(impl_block);
                    for method in &impl_block.items {
                        self.emit_function_prototype(
                            &method.node,
                            Some((&type_name, trait_name.as_deref())),
                            emitter,
                        );
                    }
                }
                Item::ExternBlock(ext) => {
                    if let Some(abi) = &ext.abi {
                        emitter.emit_line(&format!("// extern \"{}\"", abi.node));
                    } else {
                        emitter.emit_line("// extern");
                    }
                    for func in &ext.items {
                        let (ret_type, func_name, params) = self.function_signature(&func.node, None);
                        emitter.emit_line(&format!("extern {ret_type} {func_name}({params});"));
                    }
                }
                _ => {}
            }
        }

        emitter.blank_line();
    }

    /// Emit a single function prototype.
    fn emit_function_prototype(
        &self,
        f: &FunctionDef,
        method_info: Option<(&str, Option<&str>)>,
        emitter: &mut CEmitter,
    ) {
        if f.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        let (ret_type, func_name, params) = self.function_signature(f, method_info);
        emitter.emit_line(&format!("{ret_type} {func_name}({params});"));
    }

    // ─── Function Definitions ────────────────────────────────

    /// Emit all function definitions.
    pub fn emit_function_definitions(&mut self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Function Definitions ──");

        for item in &module.items {
            match &item.node {
                Item::Function(f) => {
                    self.emit_function_def(f, None, emitter);
                }
                Item::Equip(impl_block) => {
                    let type_name = self.impl_type_name(impl_block);
                    let trait_name = self.impl_trait_name(impl_block);
                    self.current_self_type = Some(type_name.clone());
                    for method in &impl_block.items {
                        self.emit_function_def(
                            &method.node,
                            Some((&type_name, trait_name.as_deref())),
                            emitter,
                        );
                    }
                    self.current_self_type = None;
                }
                Item::ConstDecl(c) => {
                    self.emit_const_decl(c, emitter);
                }
                Item::StaticDecl(s) => {
                    self.emit_static_decl(s, emitter);
                }
                _ => {}
            }
        }
    }

    /// Emit a single function definition.
    fn emit_function_def(
        &mut self,
        f: &FunctionDef,
        method_info: Option<(&str, Option<&str>)>,
        emitter: &mut CEmitter,
    ) {
        if f.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        let is_main = f.name.node == "main" && method_info.is_none();
        let (ret_type, func_name, params) = if is_main {
            ("int".to_string(), "main".to_string(), "void".to_string())
        } else {
            self.function_signature(f, method_info)
        };

        // Track whether this function throws
        self.current_function_throws = f.throws.is_some();

        match &f.body {
            FunctionBody::Block(block) => {
                emitter.emit_line(&format!("{ret_type} {func_name}({params}) {{"));
                emitter.indent();

                if method_info.is_some() {
                    // Set current_self_type for method bodies
                    if let Some((type_name, _)) = method_info {
                        self.current_self_type = Some(type_name.to_string());
                    }
                }

                self.push_drop_scope(DropScopeKind::Function);
                self.gen_block(block, emitter);
                self.pop_drop_scope(emitter);

                if is_main {
                    emitter.emit_line("return 0;");
                }

                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Expression(expr) => {
                emitter.emit_line(&format!("{ret_type} {func_name}({params}) {{"));
                emitter.indent();
                let e = self.gen_expr(expr);
                emitter.emit_line(&format!("return {e};"));
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Declaration => {
                // External declaration — no body
            }
        }
    }

    /// Build the (return_type, mangled_name, param_list) for a function.
    fn function_signature(
        &self,
        f: &FunctionDef,
        method_info: Option<(&str, Option<&str>)>,
    ) -> (String, String, String) {
        let ret_type = if matches!(f.return_type.node, Type::SelfType) {
            if let Some((type_name, _)) = method_info {
                type_name.to_string()
            } else {
                c_types::ast_type_to_c(&f.return_type.node, self.scopes)
            }
        } else {
            c_types::ast_type_to_c(&f.return_type.node, self.scopes)
        };

        let func_name = if let Some((type_name, trait_name)) = method_info {
            if let Some(tname) = trait_name {
                c_mangle::mangle_trait_method(tname, type_name, &f.name.node)
            } else {
                c_mangle::mangle_method(type_name, &f.name.node)
            }
        } else {
            c_mangle::escape_keyword(&f.name.node)
        };

        let mut params_vec: Vec<String> = Vec::new();

        // Add self parameter for methods
        let has_self = f.params.iter().any(|p| p.node.name.node == "self");
        if has_self {
            if let Some((type_name, _)) = method_info {
                params_vec.push(format!("const {type_name}* self"));
            }
        }

        // Add remaining parameters
        for param in &f.params {
            if param.node.name.node == "self" {
                continue;
            }
            let param_type = if matches!(param.node.type_.node, Type::SelfType) {
                if let Some((type_name, _)) = method_info {
                    type_name.to_string()
                } else {
                    c_types::ast_type_to_c(&param.node.type_.node, self.scopes)
                }
            } else {
                c_types::ast_type_to_c(&param.node.type_.node, self.scopes)
            };
            let param_name = c_mangle::escape_keyword(&param.node.name.node);
            params_vec.push(c_types::c_declare(&param_type, &param_name));
        }

        let params = if params_vec.is_empty() {
            "void".to_string()
        } else {
            params_vec.join(", ")
        };

        (ret_type, func_name, params)
    }

    /// Emit a const declaration.
    fn emit_const_decl(&self, c: &ConstDecl, emitter: &mut CEmitter) {
        let c_type = c_types::ast_type_to_c(&c.type_.node, self.scopes);
        let name = c_mangle::escape_keyword(&c.name.node);
        let val = self.gen_expr(&c.value);
        emitter.emit_line(&format!("static const {c_type} {name} = {val};"));
    }

    /// Emit a static declaration.
    fn emit_static_decl(&self, s: &StaticDecl, emitter: &mut CEmitter) {
        let c_type = c_types::ast_type_to_c(&s.type_.node, self.scopes);
        let name = c_mangle::escape_keyword(&s.name.node);
        let val = self.gen_expr(&s.value);
        emitter.emit_line(&format!("static {c_type} {name} = {val};"));
    }

    // ─── Type Aliases & Newtypes ────────────────────────────

    /// Emit a type alias as a C typedef.
    fn emit_type_alias(&self, alias: &TypeAlias, emitter: &mut CEmitter) {
        let target = c_types::ast_type_to_c(&alias.type_.node, self.scopes);
        let name = &alias.name.node;
        emitter.emit_line(&format!("typedef {target} {name};"));
    }

    /// Emit a newtype as a wrapper struct.
    fn emit_newtype(&self, nt: &NewtypeDef, emitter: &mut CEmitter) {
        let inner = c_types::ast_type_to_c(&nt.inner_type.node, self.scopes);
        let name = &nt.name.node;
        emitter.emit_line(&format!("struct {name} {{"));
        emitter.indent();
        emitter.emit_line(&format!("{inner} value;"));
        emitter.dedent();
        emitter.emit_line("};");
    }

    // ─── Trait Definitions ─────────────────────────────────

    /// Emit a trait definition: vtable struct + trait object struct.
    fn emit_trait_def(&self, t: &TraitDef, emitter: &mut CEmitter) {
        let name = &t.name.node;
        let methods: Vec<String> = t.items.iter().filter_map(|item| {
            match &item.node {
                TraitItem::Method(f) => Some(f.name.node.clone()),
                TraitItem::AssociatedType(a) => Some(format!("type {}", a.name.node)),
            }
        }).collect();
        emitter.emit_line(&format!("/* trait {name}: {} */", methods.join(", ")));

        // Emit vtable struct
        let vtable_name = c_mangle::mangle_vtable_struct(name);
        emitter.emit_line(&format!("struct {vtable_name} {{"));
        emitter.indent();
        for item in &t.items {
            if let TraitItem::Method(f) = &item.node {
                let ret_type = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
                let method_name = &f.name.node;

                // Build parameter list: self → const void* (or void* for &self/!self)
                let mut param_types = Vec::new();
                for param in &f.params {
                    if param.node.name.node == "self" {
                        match param.node.ownership {
                            Ownership::MutableBorrow | Ownership::Move => {
                                param_types.push("void*".to_string());
                            }
                            _ => {
                                param_types.push("const void*".to_string());
                            }
                        }
                    } else {
                        param_types.push(c_types::ast_type_to_c(&param.node.type_.node, self.scopes));
                    }
                }
                let params_str = if param_types.is_empty() {
                    "void".to_string()
                } else {
                    param_types.join(", ")
                };
                emitter.emit_line(&format!("{ret_type} (*{method_name})({params_str});"));
            }
        }
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();

        // Emit trait object struct
        let trait_obj_name = c_mangle::mangle_trait_obj(name);
        emitter.emit_line(&format!("struct {trait_obj_name} {{"));
        emitter.indent();
        emitter.emit_line("void* data;");
        emitter.emit_line(&format!("const {vtable_name}* vtable;"));
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();
    }

    // ─── Vtable Instances ─────────────────────────────────

    /// Emit static vtable instances for all trait impl blocks.
    pub fn emit_vtable_instances(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        // Collect trait definitions for method ordering
        let mut trait_defs: std::collections::HashMap<String, &TraitDef> = std::collections::HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }

        for item in &module.items {
            if let Item::Equip(impl_block) = &item.node {
                // Only trait impls (not inherent impls)
                let Some(trait_ref) = &impl_block.trait_ else {
                    continue;
                };
                let trait_name = match &trait_ref.trait_name.node {
                    Type::Named { name, .. } => name.node.clone(),
                    _ => continue,
                };
                let type_name = self.impl_type_name(impl_block);

                let Some(trait_def) = trait_defs.get(&trait_name) else {
                    continue;
                };

                let vtable_type = c_mangle::mangle_vtable_struct(&trait_name);
                let vtable_instance = c_mangle::mangle_vtable_instance(&trait_name, &type_name);

                emitter.emit_line(&format!("static const {vtable_type} {vtable_instance} = {{"));
                emitter.indent();

                // Emit function pointer assignments in trait method order
                for trait_item in &trait_def.items {
                    if let TraitItem::Method(method) = &trait_item.node {
                        let method_name = &method.name.node;
                        let impl_fn = c_mangle::mangle_trait_method(&trait_name, &type_name, method_name);

                        // Build the cast type for the function pointer
                        let ret_type = c_types::ast_type_to_c(&method.return_type.node, self.scopes);
                        let mut cast_params = Vec::new();
                        for param in &method.params {
                            if param.node.name.node == "self" {
                                match param.node.ownership {
                                    Ownership::MutableBorrow | Ownership::Move => {
                                        cast_params.push("void*".to_string());
                                    }
                                    _ => {
                                        cast_params.push("const void*".to_string());
                                    }
                                }
                            } else {
                                cast_params.push(c_types::ast_type_to_c(&param.node.type_.node, self.scopes));
                            }
                        }
                        let cast_params_str = if cast_params.is_empty() {
                            "void".to_string()
                        } else {
                            cast_params.join(", ")
                        };

                        emitter.emit_line(&format!(
                            ".{method_name} = ({ret_type} (*)({cast_params_str})){impl_fn},"
                        ));
                    }
                }

                emitter.dedent();
                emitter.emit_line("};");
                emitter.blank_line();
            }
        }
    }

    // ─── Lifted Closures ────────────────────────────────────

    /// Emit all lifted closure environment structs and functions.
    pub fn emit_lifted_closures(&self, emitter: &mut CEmitter) {
        for closure in self.lifted_closures.borrow().iter() {
            let env_name = super::c_mangle::mangle_closure_env(closure.id);
            let fn_name = super::c_mangle::mangle_closure(closure.id);

            // Environment struct
            if !closure.captures.is_empty() {
                emitter.emit_line(&format!("typedef struct {{"));
                emitter.indent();
                for (cap_name, cap_type) in &closure.captures {
                    emitter.emit_line(&format!("{cap_type} {cap_name};"));
                }
                emitter.dedent();
                emitter.emit_line(&format!("}} {env_name};"));
                emitter.blank_line();
            }

            // Closure function — include env param only for capturing closures
            let mut params_vec: Vec<String> = Vec::new();
            if !closure.captures.is_empty() {
                params_vec.push("void* __env_ptr".to_string());
            }
            for (p_name, p_type) in &closure.params {
                params_vec.push(c_types::c_declare(p_type, p_name));
            }
            let params_str = params_vec.join(", ");

            emitter.emit_line(&format!(
                "static inline {} {fn_name}({params_str}) {{",
                closure.return_type
            ));
            emitter.indent();

            // Unpack environment
            if !closure.captures.is_empty() {
                emitter.emit_line(&format!(
                    "{env_name}* __env = ({env_name}*)__env_ptr;"
                ));
                for (cap_name, cap_type) in &closure.captures {
                    emitter.emit_line(&format!(
                        "{cap_type} {cap_name} = __env->{cap_name};"
                    ));
                }
            }

            emitter.emit_line(&format!("return {};", closure.body));
            emitter.dedent();
            emitter.emit_line("}");
            emitter.blank_line();
        }
    }

    // ─── Generic Monomorphization ────────────────────────────

    /// Pre-scan the module AST to discover and register generic type usages.
    /// This must run before codegen so that monomorphized types are emitted before use.
    pub fn discover_generic_usages(&self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Function(f) => self.scan_function_for_generics(f),
                Item::Equip(impl_block) => {
                    for method in &impl_block.items {
                        self.scan_function_for_generics(&method.node);
                    }
                }
                _ => {}
            }
        }
    }

    /// Scan a function for generic type usages in declarations and calls.
    fn scan_function_for_generics(&self, f: &FunctionDef) {
        if f.generic_params.is_some() {
            return; // Don't scan inside generic templates
        }
        // Scan parameter types
        for param in &f.params {
            self.scan_type_for_generics(&param.node.type_.node);
        }
        // Scan return type
        self.scan_type_for_generics(&f.return_type.node);
        // Scan body
        match &f.body {
            FunctionBody::Block(block) => self.scan_block_for_generics(block),
            FunctionBody::Expression(expr) => self.scan_expr_for_generics(expr),
            FunctionBody::Declaration => {}
        }
    }

    fn scan_block_for_generics(&self, block: &crate::parser::ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt_for_generics(&stmt.node);
        }
    }

    fn scan_stmt_for_generics(&self, stmt: &crate::parser::ast::Stmt) {
        match stmt {
            Stmt::VarDecl { type_, value, .. } => {
                self.scan_type_for_generics(&type_.node);
                self.scan_expr_for_generics(value);
            }
            Stmt::Expr(expr) => self.scan_expr_for_generics(expr),
            Stmt::Assign { target, value } => {
                self.scan_expr_for_generics(target);
                self.scan_expr_for_generics(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.scan_expr_for_generics(target);
                self.scan_expr_for_generics(value);
            }
            Stmt::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.scan_expr_for_generics(expr);
                }
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.scan_expr_for_generics(condition);
                self.scan_block_for_generics(then_body);
                for (cond, body) in elif_branches {
                    self.scan_expr_for_generics(cond);
                    self.scan_block_for_generics(body);
                }
                if let Some(body) = else_body {
                    self.scan_block_for_generics(body);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.scan_expr_for_generics(condition);
                self.scan_block_for_generics(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr_for_generics(iterable);
                self.scan_block_for_generics(body);
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.scan_expr_for_generics(scrutinee);
                for arm in arms {
                    self.scan_expr_for_generics(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_generics(guard);
                    }
                }
                if let Some(else_body) = else_arm {
                    self.scan_block_for_generics(else_body);
                }
            }
            Stmt::Loop { body } => self.scan_block_for_generics(body),
            Stmt::Throw(expr) => self.scan_expr_for_generics(expr),
            _ => {}
        }
    }

    fn scan_expr_for_generics(&self, expr: &crate::span::Spanned<Expr>) {
        match &expr.node {
            Expr::Call { callee, generic_args, args } => {
                if let Some(type_args) = generic_args {
                    let c_type_args: Vec<String> = type_args
                        .iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    if let Expr::Identifier(name) = &callee.node {
                        let kind = if self.generic_struct_templates.borrow().contains_key(name) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.borrow().contains_key(name) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(name, &c_type_args, kind);
                    }
                }
                self.scan_expr_for_generics(callee);
                for arg in args {
                    self.scan_expr_for_generics(&arg.node.value);
                }
            }
            Expr::MethodCall { receiver, generic_args, args, .. } => {
                if let Some(type_args) = generic_args {
                    let _c_type_args: Vec<String> = type_args
                        .iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    // Method generic instantiation registered during codegen
                }
                self.scan_expr_for_generics(receiver);
                for arg in args {
                    self.scan_expr_for_generics(&arg.node.value);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.scan_expr_for_generics(left);
                self.scan_expr_for_generics(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.scan_expr_for_generics(operand);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                self.scan_expr_for_generics(condition);
                self.scan_expr_for_generics(then_branch);
                if let Some(eb) = else_branch {
                    self.scan_expr_for_generics(eb);
                }
            }
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.scan_expr_for_generics(object);
            }
            Expr::Index { object, index } => {
                self.scan_expr_for_generics(object);
                self.scan_expr_for_generics(index);
            }
            _ => {}
        }
    }

    fn scan_type_for_generics(&self, ty: &Type) {
        if let Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" | "Dict" | "Map" | "HashMap" => {}
                    _ => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        let kind = if self.generic_struct_templates.borrow().contains_key(&name.node) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.borrow().contains_key(&name.node) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(&name.node, &c_args, kind);
                    }
                }
            }
        }
    }

    /// Collect generic struct/enum/function templates from the module.
    pub fn collect_generic_templates(&self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Struct(s) if s.generic_params.is_some() => {
                    self.generic_struct_templates
                        .borrow_mut()
                        .insert(s.name.node.clone(), s.clone());
                }
                Item::Enum(e) if e.generic_params.is_some() => {
                    self.generic_enum_templates
                        .borrow_mut()
                        .insert(e.name.node.clone(), e.clone());
                }
                Item::Function(f) if f.generic_params.is_some() => {
                    self.generic_fn_templates
                        .borrow_mut()
                        .insert(f.name.node.clone(), f.clone());
                }
                _ => {}
            }
        }

        // Inject built-in Option[T] and Result[T,E] generic enum templates.
        use crate::span::{Span, Spanned};
        let mut enums = self.generic_enum_templates.borrow_mut();
        if !enums.contains_key("Option") {
            enums.insert("Option".to_string(), EnumDef {
                attributes: vec![],
                visibility: Visibility::Public,
                name: Spanned::dummy("Option".to_string()),
                generic_params: Some(Spanned::dummy(GenericParams {
                    params: vec![Spanned::dummy(GenericParam::Type(Spanned::dummy("T".to_string())))],
                })),
                variants: vec![
                    Spanned::dummy(Variant {
                        name: Spanned::dummy("Some".to_string()),
                        fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                            name: Spanned::dummy("T".to_string()),
                            generic_args: vec![],
                        })]),
                    }),
                    Spanned::dummy(Variant {
                        name: Spanned::dummy("None".to_string()),
                        fields: VariantFields::Unit,
                    }),
                ],
                doc_comment: None,
                span: Span::dummy(),
            });
        }
        if !enums.contains_key("Result") {
            enums.insert("Result".to_string(), EnumDef {
                attributes: vec![],
                visibility: Visibility::Public,
                name: Spanned::dummy("Result".to_string()),
                generic_params: Some(Spanned::dummy(GenericParams {
                    params: vec![
                        Spanned::dummy(GenericParam::Type(Spanned::dummy("T".to_string()))),
                        Spanned::dummy(GenericParam::Type(Spanned::dummy("E".to_string()))),
                    ],
                })),
                variants: vec![
                    Spanned::dummy(Variant {
                        name: Spanned::dummy("Ok".to_string()),
                        fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                            name: Spanned::dummy("T".to_string()),
                            generic_args: vec![],
                        })]),
                    }),
                    Spanned::dummy(Variant {
                        name: Spanned::dummy("Error".to_string()),
                        fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                            name: Spanned::dummy("E".to_string()),
                            generic_args: vec![],
                        })]),
                    }),
                ],
                doc_comment: None,
                span: Span::dummy(),
            });
        }
    }

    /// Register a generic instantiation and return its mangled name.
    pub fn register_generic(
        &self,
        base: &str,
        c_type_args: &[String],
        kind: super::GenericInstanceKind,
    ) -> String {
        let mangled = c_mangle::mangle_generic(base, c_type_args);
        let mut instances = self.generic_instances.borrow_mut();
        if !instances.iter().any(|i| i.mangled_name == mangled) {
            instances.push(super::GenericInstance {
                base_name: base.to_string(),
                mangled_name: mangled.clone(),
                c_type_args: c_type_args.to_vec(),
                kind,
            });
        }
        mangled
    }

    /// Emit all registered generic instantiations.
    pub fn emit_generic_instantiations(&self, emitter: &mut CEmitter) {
        let instances = self.generic_instances.borrow().clone();
        if instances.is_empty() {
            return;
        }
        emitter.emit_line("// ── Generic Instantiations ──");
        for inst in &instances {
            match inst.kind {
                super::GenericInstanceKind::Struct => {
                    if let Some(template) = self.generic_struct_templates.borrow().get(&inst.base_name) {
                        let template = template.clone();
                        self.emit_monomorphized_struct(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                super::GenericInstanceKind::Enum => {
                    if let Some(template) = self.generic_enum_templates.borrow().get(&inst.base_name) {
                        let template = template.clone();
                        self.emit_monomorphized_enum(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                super::GenericInstanceKind::Function => {
                    if let Some(template) = self.generic_fn_templates.borrow().get(&inst.base_name) {
                        let template = template.clone();
                        self.emit_monomorphized_function(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
            }
        }
        emitter.blank_line();
    }

    /// Emit a monomorphized struct definition.
    fn emit_monomorphized_struct(
        &self,
        template: &StructDef,
        c_type_args: &[String],
        mangled: &str,
        emitter: &mut CEmitter,
    ) {
        let subs = self.build_type_substitutions(template.generic_params.as_ref(), c_type_args);
        emitter.emit_line(&format!("typedef struct {mangled} {mangled};"));
        emitter.emit_line(&format!("struct {mangled} {{"));
        emitter.indent();
        for field in &template.fields {
            let ft = self.substitute_type(&field.node.type_.node, &subs);
            let fn_ = c_mangle::escape_keyword(&field.node.name.node);
            emitter.emit_line(&format!("{ft} {fn_};"));
        }
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();
    }

    /// Emit a monomorphized enum definition.
    fn emit_monomorphized_enum(
        &self,
        template: &EnumDef,
        c_type_args: &[String],
        mangled: &str,
        emitter: &mut CEmitter,
    ) {
        let subs = self.build_type_substitutions(template.generic_params.as_ref(), c_type_args);

        // Tag enum
        emitter.emit_line("typedef enum {");
        emitter.indent();
        for (i, variant) in template.variants.iter().enumerate() {
            let tag = c_mangle::mangle_tag(mangled, &variant.node.name.node);
            if i < template.variants.len() - 1 {
                emitter.emit_line(&format!("{tag},"));
            } else {
                emitter.emit_line(&format!("{tag}"));
            }
        }
        emitter.dedent();
        emitter.emit_line(&format!("}} {mangled}_Tag;"));
        emitter.blank_line();

        // Data structs for variants with fields
        let mut has_data_variants = false;
        for variant in &template.variants {
            if let VariantFields::Tuple(fields) = &variant.node.fields {
                has_data_variants = true;
                let data_name = c_mangle::mangle_variant_data(mangled, &variant.node.name.node);
                emitter.emit_line("typedef struct {");
                emitter.indent();
                for (i, field) in fields.iter().enumerate() {
                    let field_type = self.substitute_type(&field.node, &subs);
                    emitter.emit_line(&format!("{field_type} _{i};"));
                }
                emitter.dedent();
                emitter.emit_line(&format!("}} {data_name};"));
                emitter.blank_line();
            }
        }

        // Tagged union struct
        emitter.emit_line(&format!("typedef struct {mangled} {mangled};"));
        emitter.emit_line(&format!("struct {mangled} {{"));
        emitter.indent();
        emitter.emit_line(&format!("{mangled}_Tag tag;"));
        if has_data_variants {
            emitter.emit_line("union {");
            emitter.indent();
            for variant in &template.variants {
                if let VariantFields::Tuple(_) = &variant.node.fields {
                    let data_name = c_mangle::mangle_variant_data(mangled, &variant.node.name.node);
                    let field_name = &variant.node.name.node;
                    emitter.emit_line(&format!("{data_name} {field_name};"));
                }
            }
            emitter.dedent();
            emitter.emit_line("} data;");
        }
        emitter.dedent();
        emitter.emit_line("};");
        emitter.blank_line();

        // Inline constructor functions
        for variant in &template.variants {
            let variant_name = &variant.node.name.node;
            let constructor = c_mangle::mangle_variant(mangled, variant_name);
            let tag = c_mangle::mangle_tag(mangled, variant_name);

            match &variant.node.fields {
                VariantFields::Unit => {
                    emitter.emit_line(&format!(
                        "static inline {mangled} {constructor}(void) {{ return ({mangled}){{.tag = {tag}}}; }}"
                    ));
                }
                VariantFields::Tuple(fields) => {
                    let params: Vec<String> = fields
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let t = self.substitute_type(&f.node, &subs);
                            format!("{t} _{i}")
                        })
                        .collect();
                    let assigns: Vec<String> = (0..fields.len())
                        .map(|i| format!("._{i} = _{i}"))
                        .collect();
                    let data_name = c_mangle::mangle_variant_data(mangled, variant_name);
                    emitter.emit_line(&format!(
                        "static inline {mangled} {constructor}({}) {{ return ({mangled}){{.tag = {tag}, .data.{variant_name} = ({data_name}){{{}}}}};  }}",
                        params.join(", "),
                        assigns.join(", ")
                    ));
                }
            }
        }
        emitter.blank_line();
    }

    /// Emit a monomorphized function definition (expression-body only for now).
    fn emit_monomorphized_function(
        &self,
        template: &FunctionDef,
        c_type_args: &[String],
        mangled: &str,
        emitter: &mut CEmitter,
    ) {
        let subs = self.build_type_substitutions(template.generic_params.as_ref(), c_type_args);

        let ret_type = self.substitute_type(&template.return_type.node, &subs);

        let mut params_vec: Vec<String> = Vec::new();
        for param in &template.params {
            if param.node.name.node == "self" {
                continue;
            }
            let param_type = self.substitute_type(&param.node.type_.node, &subs);
            let param_name = c_mangle::escape_keyword(&param.node.name.node);
            params_vec.push(c_types::c_declare(&param_type, &param_name));
        }
        let params = if params_vec.is_empty() {
            "void".to_string()
        } else {
            params_vec.join(", ")
        };

        // Emit prototype
        emitter.emit_line(&format!("{ret_type} {mangled}({params});"));

        // Emit definition
        match &template.body {
            FunctionBody::Expression(expr) => {
                emitter.emit_line(&format!("{ret_type} {mangled}({params}) {{"));
                emitter.indent();
                let e = self.gen_expr(expr);
                emitter.emit_line(&format!("return {e};"));
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Block(block) => {
                emitter.emit_line(&format!("{ret_type} {mangled}({params}) {{"));
                emitter.indent();
                self.gen_block(block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Declaration => {
                // External declaration — no body
            }
        }
    }

    /// Build a substitution map from generic param names to concrete C types.
    fn build_type_substitutions(
        &self,
        generic_params: Option<&crate::span::Spanned<crate::parser::ast::GenericParams>>,
        c_type_args: &[String],
    ) -> Vec<(String, String)> {
        let mut subs = Vec::new();
        if let Some(params) = generic_params {
            for (i, param) in params.node.params.iter().enumerate() {
                if let crate::parser::ast::GenericParam::Type(name) = &param.node {
                    if let Some(c_type) = c_type_args.get(i) {
                        subs.push((name.node.clone(), c_type.clone()));
                    }
                }
            }
        }
        subs
    }

    /// Substitute type parameters in an AST Type, returning a C type string.
    fn substitute_type(
        &self,
        ty: &crate::parser::ast::Type,
        subs: &[(String, String)],
    ) -> String {
        match ty {
            crate::parser::ast::Type::Named { name, generic_args } if generic_args.is_empty() => {
                // Check if this is a type parameter
                for (param_name, c_type) in subs {
                    if name.node == *param_name {
                        return c_type.clone();
                    }
                }
                // Not a type parameter, use normal type mapping
                c_types::ast_type_to_c(ty, self.scopes)
            }
            crate::parser::ast::Type::Named { name, generic_args } => {
                // Nested generic type — substitute recursively
                let c_args: Vec<String> = generic_args
                    .iter()
                    .map(|a| self.substitute_type(&a.node, subs))
                    .collect();
                // Check built-in collections
                match name.node.as_str() {
                    "Vector" | "List" | "Array" => "GorgetArray".to_string(),
                    "Set" => "GorgetSet".to_string(),
                    "Dict" | "Map" | "HashMap" => "GorgetMap".to_string(),
                    _ => c_mangle::mangle_generic(&name.node, &c_args),
                }
            }
            // Type::Ref removed
            _ => c_types::ast_type_to_c(ty, self.scopes),
        }
    }

    /// Map an AST type to C and register any generic instantiations found.
    pub fn type_to_c_with_registration(&self, ty: &crate::parser::ast::Type) -> String {
        if let crate::parser::ast::Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" | "Dict" | "Map" | "HashMap" => {}
                    _ => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        // Determine kind
                        let kind = if self.generic_struct_templates.borrow().contains_key(&name.node) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.borrow().contains_key(&name.node) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(&name.node, &c_args, kind);
                    }
                }
            }
        }
        c_types::ast_type_to_c(ty, self.scopes)
    }

    // ─── Tuple Typedefs ──────────────────────────────────────

    /// Register a tuple typedef, deduplicating by name. Returns the mangled name.
    pub fn register_tuple_typedef(&self, c_field_types: &[String]) -> String {
        let name = c_mangle::mangle_tuple(c_field_types);
        let mut typedefs = self.tuple_typedefs.borrow_mut();
        if !typedefs.iter().any(|(n, _)| *n == name) {
            typedefs.push((name.clone(), c_field_types.to_vec()));
        }
        name
    }

    /// Pre-scan the module AST to discover tuple types in type annotations.
    pub fn discover_tuple_types(&self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Function(f) => self.scan_function_for_tuples(f),
                Item::Equip(impl_block) => {
                    for method in &impl_block.items {
                        self.scan_function_for_tuples(&method.node);
                    }
                }
                Item::Struct(s) if s.generic_params.is_none() => {
                    for field in &s.fields {
                        self.scan_type_for_tuples(&field.node.type_.node);
                    }
                }
                Item::Enum(e) if e.generic_params.is_none() => {
                    for variant in &e.variants {
                        if let VariantFields::Tuple(fields) = &variant.node.fields {
                            for field in fields {
                                self.scan_type_for_tuples(&field.node);
                            }
                        }
                    }
                }
                Item::TypeAlias(a) => self.scan_type_for_tuples(&a.type_.node),
                Item::Newtype(nt) => self.scan_type_for_tuples(&nt.inner_type.node),
                _ => {}
            }
        }
    }

    /// Scan a function's signature and body for tuple types.
    fn scan_function_for_tuples(&self, f: &FunctionDef) {
        if f.generic_params.is_some() {
            return;
        }
        self.scan_type_for_tuples(&f.return_type.node);
        for param in &f.params {
            self.scan_type_for_tuples(&param.node.type_.node);
        }
        // Scan body for tuple literal expressions
        match &f.body {
            FunctionBody::Block(block) => self.scan_block_for_tuples(block),
            FunctionBody::Expression(expr) => self.scan_expr_for_tuples(expr),
            FunctionBody::Declaration => {}
        }
    }

    /// Scan a block for tuple literal expressions.
    fn scan_block_for_tuples(&self, block: &crate::parser::ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt_for_tuples(&stmt.node);
        }
    }

    /// Scan a statement for tuple literal expressions.
    fn scan_stmt_for_tuples(&self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl { type_, value, .. } => {
                self.scan_type_for_tuples(&type_.node);
                self.scan_expr_for_tuples(value);
            }
            Stmt::Expr(expr) => self.scan_expr_for_tuples(expr),
            Stmt::Assign { target, value } => {
                self.scan_expr_for_tuples(target);
                self.scan_expr_for_tuples(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.scan_expr_for_tuples(target);
                self.scan_expr_for_tuples(value);
            }
            Stmt::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.scan_expr_for_tuples(expr);
                }
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.scan_expr_for_tuples(condition);
                self.scan_block_for_tuples(then_body);
                for (cond, body) in elif_branches {
                    self.scan_expr_for_tuples(cond);
                    self.scan_block_for_tuples(body);
                }
                if let Some(body) = else_body {
                    self.scan_block_for_tuples(body);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.scan_expr_for_tuples(condition);
                self.scan_block_for_tuples(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr_for_tuples(iterable);
                self.scan_block_for_tuples(body);
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.scan_expr_for_tuples(scrutinee);
                for arm in arms {
                    self.scan_expr_for_tuples(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_tuples(guard);
                    }
                }
                if let Some(else_body) = else_arm {
                    self.scan_block_for_tuples(else_body);
                }
            }
            Stmt::Loop { body } => self.scan_block_for_tuples(body),
            Stmt::Throw(expr) => self.scan_expr_for_tuples(expr),
            _ => {}
        }
    }

    /// Scan an expression for tuple literals and register their typedefs.
    fn scan_expr_for_tuples(&self, expr: &crate::span::Spanned<Expr>) {
        match &expr.node {
            Expr::TupleLiteral(elements) => {
                // Register inner tuples first (depth-first)
                for elem in elements {
                    self.scan_expr_for_tuples(elem);
                }
                let c_field_types: Vec<String> = elements
                    .iter()
                    .map(|e| self.infer_c_type_from_expr(&e.node))
                    .collect();
                self.register_tuple_typedef(&c_field_types);
            }
            Expr::Call { callee, args, .. } => {
                self.scan_expr_for_tuples(callee);
                for arg in args {
                    self.scan_expr_for_tuples(&arg.node.value);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.scan_expr_for_tuples(left);
                self.scan_expr_for_tuples(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.scan_expr_for_tuples(operand);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                self.scan_expr_for_tuples(condition);
                self.scan_expr_for_tuples(then_branch);
                if let Some(eb) = else_branch {
                    self.scan_expr_for_tuples(eb);
                }
            }
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.scan_expr_for_tuples(object);
            }
            Expr::Index { object, index } => {
                self.scan_expr_for_tuples(object);
                self.scan_expr_for_tuples(index);
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.scan_expr_for_tuples(receiver);
                for arg in args {
                    self.scan_expr_for_tuples(&arg.node.value);
                }
            }
            _ => {}
        }
    }

    /// Recursively scan a type for tuples, registering typedefs for any found.
    fn scan_type_for_tuples(&self, ty: &Type) {
        match ty {
            Type::Tuple(fields) => {
                let c_field_types: Vec<String> = fields
                    .iter()
                    .map(|f| c_types::ast_type_to_c(&f.node, self.scopes))
                    .collect();
                self.register_tuple_typedef(&c_field_types);
                // Recurse into nested tuple fields
                for field in fields {
                    self.scan_type_for_tuples(&field.node);
                }
            }
            Type::Array { element, .. } | Type::Slice { element } => {
                self.scan_type_for_tuples(&element.node);
            }
            Type::Function { return_type, params } => {
                self.scan_type_for_tuples(&return_type.node);
                for p in params {
                    self.scan_type_for_tuples(&p.node);
                }
            }
            Type::Named { generic_args, .. } => {
                for arg in generic_args {
                    self.scan_type_for_tuples(&arg.node);
                }
            }
            _ => {}
        }
    }

    /// Emit all registered tuple typedefs.
    pub fn emit_tuple_typedefs(&self, emitter: &mut CEmitter) {
        let typedefs = self.tuple_typedefs.borrow();
        if typedefs.is_empty() {
            return;
        }
        emitter.emit_line("// ── Tuple Typedefs ──");
        for (name, field_types) in typedefs.iter() {
            let fields: Vec<String> = field_types
                .iter()
                .enumerate()
                .map(|(i, t)| format!("{t} _{i};"))
                .collect();
            emitter.emit_line(&format!(
                "typedef struct {{ {} }} {name};",
                fields.join(" ")
            ));
        }
        emitter.blank_line();
    }

    // ─── Helpers ─────────────────────────────────────────────

    /// Extract the type name from an impl block.
    fn impl_type_name(&self, impl_block: &EquipBlock) -> String {
        match &impl_block.type_.node {
            Type::Named { name, .. } => name.node.clone(),
            Type::Primitive(p) => c_types::primitive_to_c(*p).to_string(),
            _ => "Unknown".to_string(),
        }
    }

    /// Extract the trait name from an impl block (if it's a trait impl).
    fn impl_trait_name(&self, impl_block: &EquipBlock) -> Option<String> {
        impl_block.trait_.as_ref().map(|t| {
            match &t.trait_name.node {
                Type::Named { name, .. } => name.node.clone(),
                _ => "Unknown".to_string(),
            }
        })
    }
}
