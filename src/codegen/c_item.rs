/// Top-level item codegen: functions, structs, enums, impl blocks, const/static.
use std::collections::{HashMap, HashSet};
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

        // Forward-declare structs (skip generic templates and stdlib synthetic structs)
        for item in &module.items {
            if let Item::Struct(s) = &item.node {
                if s.generic_params.is_some() || s.span == crate::span::Span::dummy() {
                    continue;
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

        // Forward-declare generic instantiations (structs and enums)
        for inst in &self.generic_instances {
            match inst.kind {
                super::GenericInstanceKind::Struct | super::GenericInstanceKind::Enum => {
                    let name = &inst.mangled_name;
                    emitter.emit_line(&format!("typedef struct {name} {name};"));
                }
                _ => {}
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
        if s.span == crate::span::Span::dummy() {
            return; // Stdlib synthetic struct — C definition provided by runtime
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

        // Collect trait defs for default method lookup
        let mut trait_defs: HashMap<String, &TraitDef> = HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }

        for item in &module.items {
            match &item.node {
                Item::Function(f) => {
                    // Skip main (emitted separately) and stdlib synthetic defs
                    // (they map to C runtime functions, not user code)
                    if f.name.node != "main" && f.span != crate::span::Span::dummy() {
                        self.emit_function_prototype(f, None, emitter);
                    }
                }
                Item::Equip(impl_block) => {
                    // Skip equip blocks for generic types — emitted per-instantiation
                    if self.is_generic_equip(impl_block) {
                        continue;
                    }
                    let type_name = self.impl_type_name(impl_block);
                    let trait_name = self.impl_trait_name(impl_block);
                    // Emit prototypes for explicitly implemented methods
                    for method in &impl_block.items {
                        self.emit_function_prototype(
                            &method.node,
                            Some((&type_name, trait_name.as_deref())),
                            emitter,
                        );
                    }
                    // Emit prototypes for default/inherited methods not overridden
                    if let Some(tname) = &trait_name {
                        if let Some(trait_def) = trait_defs.get(tname.as_str()) {
                            let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);
                            for (method, _) in &all_methods {
                                if !Self::equip_has_method(impl_block, &method.name.node) {
                                    self.emit_function_prototype(
                                        method,
                                        Some((&type_name, Some(tname))),
                                        emitter,
                                    );
                                }
                            }
                        }
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

        // Collect trait defs for default method lookup
        let mut trait_defs: HashMap<String, &TraitDef> = HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }

        for item in &module.items {
            match &item.node {
                Item::Function(f) => {
                    // Skip stdlib synthetic defs (Declaration body + dummy span)
                    if f.span != crate::span::Span::dummy() {
                        self.emit_function_def(f, None, emitter);
                    }
                }
                Item::Equip(impl_block) => {
                    // Skip equip blocks for generic types — emitted per-instantiation
                    if self.is_generic_equip(impl_block) {
                        continue;
                    }
                    let type_name = self.impl_type_name(impl_block);
                    let trait_name = self.impl_trait_name(impl_block);
                    self.current_self_type = Some(type_name.clone());
                    // Emit explicitly implemented methods
                    for method in &impl_block.items {
                        self.emit_function_def(
                            &method.node,
                            Some((&type_name, trait_name.as_deref())),
                            emitter,
                        );
                    }
                    // Emit default/inherited method bodies not overridden
                    if let Some(tname) = &trait_name {
                        if let Some(trait_def) = trait_defs.get(tname.as_str()) {
                            let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);
                            for (method, _) in &all_methods {
                                if !Self::equip_has_method(impl_block, &method.name.node) {
                                    if !matches!(method.body, FunctionBody::Declaration) {
                                        // Default method body — emit as-is
                                        self.emit_function_def(
                                            method,
                                            Some((&type_name, Some(tname))),
                                            emitter,
                                        );
                                    } else if let Some(ref via) = impl_block.via_field {
                                        // No default body — delegate via field
                                        self.emit_via_forwarding_method(
                                            method,
                                            &type_name,
                                            tname,
                                            &via.node,
                                            impl_block,
                                            emitter,
                                        );
                                    }
                                }
                            }
                        }
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

        // If this is a test module, emit the test runner main()
        if self.is_test_module {
            self.emit_test_runner_main(module, emitter);
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
            ("int".to_string(), "main".to_string(), "int argc, char** argv".to_string())
        } else {
            self.function_signature(f, method_info)
        };

        // Track whether this function throws
        self.current_function_throws = f.throws.is_some();

        // Track return type for Result-based ? codegen
        self.current_function_return_c_type = if is_main {
            None
        } else {
            Some(c_types::ast_type_to_c(&f.return_type.node, self.scopes))
        };

        // Track mutable borrow params as pointer params for body codegen.
        let prev_pointer_params = std::mem::take(&mut self.pointer_params);
        for param in &f.params {
            if param.node.name.node == "self" {
                continue;
            }
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                self.pointer_params
                    .insert(c_mangle::escape_keyword(&param.node.name.node));
            }
        }

        // Compute the Gorget-display name for this function (used in trace output).
        let gorget_name = if let Some((type_name, _)) = method_info {
            format!("{}.{}", type_name, f.name.node)
        } else {
            f.name.node.clone()
        };

        // Save/set the current function Gorget name for return tracing.
        let prev_gorget_name = self.current_function_gorget_name.take();
        self.current_function_gorget_name = Some(gorget_name.clone());

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

                // Set decl_type_hint from return type so variant constructors
                // (None, Some, Ok, Error) resolve to monomorphized names.
                let prev_hint = self.decl_type_hint.clone();
                if let Type::Named { generic_args, .. } = &f.return_type.node {
                    if !generic_args.is_empty() {
                        self.decl_type_hint = Some(f.return_type.node.clone());
                    }
                }

                self.push_drop_scope(DropScopeKind::Function);
                if is_main {
                    emitter.emit_line("gorget_init_args(argc, argv);");
                    if self.trace {
                        let trace_path = self.trace_filename.replace('\\', "\\\\").replace('"', "\\\"");
                        emitter.emit_line(&format!(
                            "__gorget_trace_init(\"{trace_path}\");"
                        ));
                    }
                }

                // Trace function entry
                if self.trace && !is_main {
                    self.emit_trace_entry(f, &gorget_name, emitter);
                }

                self.gen_block(block, emitter);
                self.pop_drop_scope(emitter);

                self.decl_type_hint = prev_hint;

                // Trace implicit return for void functions (and main)
                if self.trace && !is_main {
                    if matches!(f.return_type.node, Type::Primitive(PrimitiveType::Void)) {
                        emitter.emit_line("__gorget_trace_depth--;");
                        let s = format!(
                            r#"fprintf(__gorget_trace_fp, "{{\"type\":\"return\",\"fn\":\"{gorget_name}\",\"value\":null,\"depth\":%d}}\n", __gorget_trace_depth);"#
                        );
                        emitter.emit_line(&s);
                    }
                }

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

                if self.trace {
                    self.emit_trace_entry(f, &gorget_name, emitter);
                }

                let e = self.gen_expr(expr);

                if self.trace {
                    let ret_c_type = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
                    let formatter = c_types::trace_formatter_for_c_type(&ret_c_type);
                    emitter.emit_line(&format!("__typeof__({e}) __trace_ret = {e};"));
                    emitter.emit_line("__gorget_trace_depth--;");
                    let s = format!(
                        r#"fprintf(__gorget_trace_fp, "{{\"type\":\"return\",\"fn\":\"{gorget_name}\",\"value\":");"#
                    );
                    emitter.emit_line(&s);
                    if formatter == "__gorget_trace_val_void" {
                        emitter.emit_line(r#"fprintf(__gorget_trace_fp, "null");"#);
                    } else {
                        emitter.emit_line(&format!(
                            "{formatter}(__gorget_trace_fp, __trace_ret);"
                        ));
                    }
                    emitter.emit_line(
                        r#"fprintf(__gorget_trace_fp, ",\"depth\":%d}\n", __gorget_trace_depth);"#
                    );
                    emitter.emit_line("return __trace_ret;");
                } else {
                    emitter.emit_line(&format!("return {e};"));
                }

                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Declaration => {
                // External declaration — no body
            }
        }

        self.current_function_gorget_name = prev_gorget_name;

        self.pointer_params = prev_pointer_params;
    }

    /// Emit trace entry (call) instrumentation for a function.
    fn emit_trace_entry(
        &self,
        f: &FunctionDef,
        gorget_name: &str,
        emitter: &mut CEmitter,
    ) {
        // Emit: {"type":"call","fn":"NAME","args":{...},"depth":N}\n
        // We use multiple fprintf/formatter calls and let the C compiler optimize.

        // Open the JSON object with type, fn, and args key
        let s = format!(
            r#"fprintf(__gorget_trace_fp, "{{\"type\":\"call\",\"fn\":\"{gorget_name}\",\"args\":{{");"#
        );
        emitter.emit_line(&s);

        // Emit each non-self parameter as "name":value
        let mut first = true;
        for param in &f.params {
            if param.node.name.node == "self" {
                continue;
            }
            let param_name = &param.node.name.node;
            let c_param_name = c_mangle::escape_keyword(param_name);
            let c_type = c_types::ast_type_to_c(&param.node.type_.node, self.scopes);
            let formatter = c_types::trace_formatter_for_c_type(&c_type);

            let comma = if first { "" } else { "," };
            first = false;

            let s = format!(
                r#"fprintf(__gorget_trace_fp, "{comma}\"{param_name}\":");"#
            );
            emitter.emit_line(&s);

            if formatter == "__gorget_trace_val_void" {
                emitter.emit_line(r#"fprintf(__gorget_trace_fp, "null");"#);
            } else {
                emitter.emit_line(&format!(
                    "{formatter}(__gorget_trace_fp, {c_param_name});"
                ));
            }
        }

        // Close args object and add depth
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "},\"depth\":%d}\n", __gorget_trace_depth);"#
        );
        emitter.emit_line("__gorget_trace_depth++;");
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
            c_mangle::escape_function(&f.name.node)
        };

        let mut params_vec: Vec<String> = Vec::new();

        // Add self parameter for methods
        let self_param = f.params.iter().find(|p| p.node.name.node == "self");
        let has_self = self_param.is_some();
        if has_self {
            if let Some((type_name, _)) = method_info {
                let is_mutable = self_param
                    .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
                    .unwrap_or(false);
                if is_mutable {
                    params_vec.push(format!("{type_name}* self"));
                } else {
                    params_vec.push(format!("const {type_name}* self"));
                }
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
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                params_vec.push(format!("{param_type}* {param_name}"));
            } else {
                params_vec.push(c_types::c_declare(&param_type, &param_name));
            }
        }

        let params = if params_vec.is_empty() {
            "void".to_string()
        } else {
            params_vec.join(", ")
        };

        (ret_type, func_name, params)
    }

    /// Emit a const declaration.
    fn emit_const_decl(&mut self, c: &ConstDecl, emitter: &mut CEmitter) {
        let c_type = c_types::ast_type_to_c(&c.type_.node, self.scopes);
        let name = c_mangle::escape_keyword(&c.name.node);
        let val = self.gen_expr(&c.value);
        emitter.emit_line(&format!("static const {c_type} {name} = {val};"));
    }

    /// Emit a static declaration. Skip stdlib statics (dummy span).
    fn emit_static_decl(&mut self, s: &StaticDecl, emitter: &mut CEmitter) {
        if s.span == crate::span::Span::dummy() {
            return; // stdlib static — codegen maps directly to C macros
        }
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
    /// The vtable includes slots for inherited parent trait methods.
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

        // Emit parent trait method slots first (for trait inheritance)
        self.emit_vtable_method_slots_for_parents(t, emitter);

        // Emit own method slots
        for item in &t.items {
            if let TraitItem::Method(f) = &item.node {
                self.emit_vtable_method_slot(f, emitter);
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

    /// Emit vtable function pointer slot for a single method.
    fn emit_vtable_method_slot(&self, f: &FunctionDef, emitter: &mut CEmitter) {
        let ret_type = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
        let method_name = &f.name.node;

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

    /// Recursively emit vtable method slots for parent traits.
    fn emit_vtable_method_slots_for_parents(&self, t: &TraitDef, emitter: &mut CEmitter) {
        for parent_bound in &t.extends {
            let parent_name = &parent_bound.node.name.node;
            // Look up the parent trait's TraitInfo to find its methods
            if let Some(parent_info) = self.traits.traits.values().find(|ti| ti.name == *parent_name) {
                // Also recursively emit grandparent methods.
                // We need the parent's AST to recurse, but for simplicity we use
                // the TraitInfo which already has the flat method list.
                // For deep hierarchies, we rely on the parent's own emit_trait_def
                // having done the work — we just add the methods here.
                for (method_name, sig) in &parent_info.methods {
                    // Reconstruct the vtable slot from the signature info.
                    // We need to map TypeId back to C types — use the type table.
                    let ret_type = self.type_id_to_c(sig.return_type);
                    let mut param_types = Vec::new();
                    if sig.has_self {
                        match sig.self_ownership {
                            Some(Ownership::MutableBorrow) | Some(Ownership::Move) => {
                                param_types.push("void*".to_string());
                            }
                            _ => {
                                param_types.push("const void*".to_string());
                            }
                        }
                    }
                    for &param_tid in &sig.params {
                        param_types.push(self.type_id_to_c(param_tid));
                    }
                    let params_str = if param_types.is_empty() {
                        "void".to_string()
                    } else {
                        param_types.join(", ")
                    };
                    emitter.emit_line(&format!("{ret_type} (*{method_name})({params_str});"));
                }
            }
        }
    }

    // ─── Vtable Instances ─────────────────────────────────

    /// Emit static vtable instances for all trait impl blocks.
    /// Handles default methods (uses trait body when equip block doesn't override)
    /// and trait inheritance (includes parent trait methods in vtable).
    pub fn emit_vtable_instances(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        // Collect trait definitions for method ordering
        let mut trait_defs: HashMap<String, &TraitDef> = HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }

        for item in &module.items {
            if let Item::Equip(impl_block) = &item.node {
                // Skip generic equip blocks — vtables emitted per-instantiation
                if self.is_generic_equip(impl_block) {
                    continue;
                }
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

                // Collect all methods including inherited ones
                let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);

                for (method, _defining_trait) in &all_methods {
                    let method_name = &method.name.node;
                    // The impl function is always mangled with the leaf trait name + type
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

                emitter.dedent();
                emitter.emit_line("};");
                emitter.blank_line();
            }
        }
    }

    // ─── Lifted Closures ────────────────────────────────────

    /// Emit all lifted closure environment structs and functions.
    pub fn emit_lifted_closures(&self, emitter: &mut CEmitter) {
        use super::CaptureMode;

        for closure in self.lifted_closures.iter() {
            let env_name = super::c_mangle::mangle_closure_env(closure.id);
            let fn_name = super::c_mangle::mangle_closure(closure.id);

            // Environment struct
            if !closure.captures.is_empty() {
                emitter.emit_line(&format!("typedef struct {{"));
                emitter.indent();
                for (cap_name, cap_type, mode) in &closure.captures {
                    match mode {
                        CaptureMode::ByMutRef => {
                            emitter.emit_line(&format!("{cap_type}* {cap_name};"));
                        }
                        CaptureMode::ByValue => {
                            emitter.emit_line(&format!("{cap_type} {cap_name};"));
                        }
                    }
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
                // Only unpack ByValue captures into local variables.
                // ByMutRef captures are accessed via (*__env->NAME) in the body.
                for (cap_name, cap_type, mode) in &closure.captures {
                    if *mode == CaptureMode::ByValue {
                        emitter.emit_line(&format!(
                            "{cap_type} {cap_name} = __env->{cap_name};"
                        ));
                    }
                }
            }

            if closure.return_type == "void" {
                emitter.emit_line(&format!("{};", closure.body));
            } else {
                emitter.emit_line(&format!("return {};", closure.body));
            }
            emitter.dedent();
            emitter.emit_line("}");
            emitter.blank_line();
        }
    }

    // ─── Generic Monomorphization ────────────────────────────

    /// Pre-scan the module AST to discover and register generic type usages.
    /// This must run before codegen so that monomorphized types are emitted before use.
    // Note: this walker is structurally similar to discover_tuple_types below.
    // A shared visitor was considered but rejected — the duplication is mechanical,
    // both walkers are stable, and the abstraction cost outweighs the savings.
    pub fn discover_generic_usages(&mut self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Function(f) => self.scan_function_for_generics(f),
                Item::Equip(impl_block) => {
                    // Skip generic equip blocks — their bodies contain template
                    // type params (e.g. Pair[T]) that would register spurious instances
                    if !self.is_generic_equip(impl_block) {
                        for method in &impl_block.items {
                            self.scan_function_for_generics(&method.node);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Scan a function for generic type usages in declarations and calls.
    fn scan_function_for_generics(&mut self, f: &FunctionDef) {
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

    fn scan_block_for_generics(&mut self, block: &crate::parser::ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt_for_generics(&stmt.node);
        }
    }

    fn scan_stmt_for_generics(&mut self, stmt: &crate::parser::ast::Stmt) {
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

    fn scan_expr_for_generics(&mut self, expr: &crate::span::Spanned<Expr>) {
        match &expr.node {
            Expr::Call { callee, generic_args, args } => {
                if let Some(type_args) = generic_args {
                    let c_type_args: Vec<String> = type_args
                        .iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    if let Expr::Identifier(name) = &callee.node {
                        match name.as_str() {
                            "Dict" | "Map" | "HashMap" => {
                                self.register_generic("GorgetMap", &c_type_args, super::GenericInstanceKind::Map);
                            }
                            _ => {
                                let kind = if self.generic_struct_templates.contains_key(name) {
                                    super::GenericInstanceKind::Struct
                                } else if self.generic_enum_templates.contains_key(name) {
                                    super::GenericInstanceKind::Enum
                                } else {
                                    super::GenericInstanceKind::Function
                                };
                                self.register_generic(name, &c_type_args, kind);
                            }
                        }
                    }
                }
                self.scan_expr_for_generics(callee);
                for arg in args {
                    self.scan_expr_for_generics(&arg.node.value);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
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

    fn scan_type_for_generics(&mut self, ty: &Type) {
        if let Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" => {}
                    "Dict" | "Map" | "HashMap" => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        self.register_generic("GorgetMap", &c_args, super::GenericInstanceKind::Map);
                    }
                    _ => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        let kind = if self.generic_struct_templates.contains_key(&name.node) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.contains_key(&name.node) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(&name.node, &c_args, kind);
                    }
                }
                // Recurse into generic args to discover nested instantiations
                // e.g. Vector[Pair[int, int]] needs Pair__int64_t__int64_t registered
                for arg in generic_args {
                    self.scan_type_for_generics(&arg.node);
                }
            }
        }
    }

    /// Collect generic struct/enum/function templates from the module.
    pub fn collect_generic_templates(&mut self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Struct(s) if s.generic_params.is_some() => {
                    self.generic_struct_templates
                        .insert(s.name.node.clone(), s.clone());
                }
                Item::Enum(e) if e.generic_params.is_some() => {
                    self.generic_enum_templates
                        .insert(e.name.node.clone(), e.clone());
                }
                Item::Function(f) if f.generic_params.is_some() => {
                    self.generic_fn_templates
                        .insert(f.name.node.clone(), f.clone());
                }
                Item::Equip(impl_block) => {
                    // Collect equip blocks whose type is a generic struct/enum
                    if let Type::Named { name, generic_args } = &impl_block.type_.node {
                        if !generic_args.is_empty() {
                            self.generic_equip_templates
                                .entry(name.node.clone())
                                .or_default()
                                .push(impl_block.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Inject built-in Option[T] and Result[T,E] generic enum templates.
        use crate::span::{Span, Spanned};
        let enums = &mut self.generic_enum_templates;
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
        &mut self,
        base: &str,
        c_type_args: &[String],
        kind: super::GenericInstanceKind,
    ) -> String {
        let mangled = c_mangle::mangle_generic(base, c_type_args);
        if !self.generic_instances.iter().any(|i| i.mangled_name == mangled) {
            self.generic_instances.push(super::GenericInstance {
                base_name: base.to_string(),
                mangled_name: mangled.clone(),
                c_type_args: c_type_args.to_vec(),
                kind,
            });
        }
        mangled
    }

    /// Emit struct/enum definitions for generic instantiations.
    /// Must be called before regular type definitions so that structs like World
    /// can use monomorphized generic types (e.g., SparseSet__Health) as fields.
    pub fn emit_generic_type_definitions(&mut self, emitter: &mut CEmitter) {
        let has_types = self.generic_instances.iter().any(|i| matches!(
            i.kind,
            super::GenericInstanceKind::Struct | super::GenericInstanceKind::Enum | super::GenericInstanceKind::Map
        ));
        if !has_types {
            return;
        }
        emitter.emit_line("// ── Generic Instantiations ──");
        for i in 0..self.generic_instances.len() {
            let inst = self.generic_instances[i].clone();
            match inst.kind {
                super::GenericInstanceKind::Struct => {
                    let template = self.generic_struct_templates.get(&inst.base_name).cloned();
                    if let Some(template) = template {
                        self.emit_monomorphized_struct(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                        let subs = self.build_type_substitutions(
                            template.generic_params.as_ref(),
                            &inst.c_type_args,
                        );
                        for field in &template.fields {
                            let subst_type = self.substitute_type_ast(
                                &field.node.type_.node,
                                &subs,
                            );
                            self.field_type_names.insert(
                                (inst.mangled_name.clone(), field.node.name.node.clone()),
                                subst_type,
                            );
                        }
                    }
                }
                super::GenericInstanceKind::Enum => {
                    let template = self.generic_enum_templates.get(&inst.base_name).cloned();
                    if let Some(template) = template {
                        self.emit_monomorphized_enum(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                super::GenericInstanceKind::Map => {
                    self.emit_monomorphized_map(&inst.c_type_args, &inst.mangled_name, emitter);
                }
                _ => {}
            }
        }
        emitter.blank_line();
    }

    /// Emit method definitions for generic instantiations (equip blocks + generic functions).
    /// Must be called after regular type definitions so that method signatures
    /// can reference user-defined types like Health by value.
    pub fn emit_generic_method_definitions(&mut self, emitter: &mut CEmitter) {
        for i in 0..self.generic_instances.len() {
            let inst = self.generic_instances[i].clone();
            match inst.kind {
                super::GenericInstanceKind::Struct | super::GenericInstanceKind::Enum => {
                    // Emit monomorphized equip block methods for this type
                    let equip_blocks = self.generic_equip_templates
                        .get(&inst.base_name)
                        .cloned()
                        .unwrap_or_default();
                    let generic_params = self.generic_struct_templates
                        .get(&inst.base_name)
                        .and_then(|t| t.generic_params.clone())
                        .or_else(|| {
                            self.generic_enum_templates
                                .get(&inst.base_name)
                                .and_then(|t| t.generic_params.clone())
                        });
                    for equip_block in &equip_blocks {
                        let trait_name = self.impl_trait_name(&equip_block);
                        for method in &equip_block.items {
                            self.emit_monomorphized_equip_method(
                                &method.node,
                                generic_params.as_ref(),
                                &inst.c_type_args,
                                &inst.mangled_name,
                                trait_name.as_deref(),
                                emitter,
                            );
                        }
                    }
                }
                super::GenericInstanceKind::Function => {
                    let template = self.generic_fn_templates.get(&inst.base_name).cloned();
                    if let Some(template) = template {
                        self.emit_monomorphized_function(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                _ => {}
            }
        }
    }

    /// Emit a monomorphized map (Dict) struct and its inline functions.
    fn emit_monomorphized_map(
        &self,
        c_type_args: &[String],
        mangled: &str,
        emitter: &mut CEmitter,
    ) {
        let key_type = c_type_args.first().map(|s| s.as_str()).unwrap_or("int64_t");
        let val_type = c_type_args.get(1).map(|s| s.as_str()).unwrap_or("int64_t");
        let is_str_key = key_type == "const char*";

        let hash_expr = |var: &str| -> String {
            if is_str_key {
                format!("__gorget_hash_str({var})")
            } else {
                format!("__gorget_fnv1a(&{var}, sizeof({key_type}))")
            }
        };
        let eq_expr = |a: &str, b: &str| -> String {
            if is_str_key {
                format!("strcmp({a}, {b}) == 0")
            } else {
                format!("memcmp(&{a}, &{b}, sizeof({key_type})) == 0")
            }
        };

        let hash_key = hash_expr("key");
        let hash_old = hash_expr("old_keys[i]");
        let eq_put = eq_expr("m->keys[idx]", "key");
        let eq_get = eq_expr("m->keys[idx]", "key");

        emitter.emit(&format!(r#"typedef struct {mangled} {mangled};
struct {mangled} {{
    {key_type}* keys;
    {val_type}* values;
    uint8_t* states;
    size_t count;
    size_t cap;
}};

static inline void {mangled}__grow({mangled}* m) {{
    size_t old_cap = m->cap;
    {key_type}* old_keys = m->keys;
    {val_type}* old_values = m->values;
    uint8_t* old_states = m->states;
    size_t new_cap = old_cap == 0 ? 16 : old_cap * 2;
    m->keys = ({key_type}*)calloc(new_cap, sizeof({key_type}));
    m->values = ({val_type}*)calloc(new_cap, sizeof({val_type}));
    m->states = (uint8_t*)calloc(new_cap, 1);
    m->cap = new_cap;
    m->count = 0;
    for (size_t i = 0; i < old_cap; i++) {{
        if (old_states[i] == 1) {{
            uint64_t h = {hash_old};
            size_t idx = (size_t)(h % new_cap);
            while (m->states[idx] != 0) {{ idx = (idx + 1) % new_cap; }}
            m->keys[idx] = old_keys[i];
            m->values[idx] = old_values[i];
            m->states[idx] = 1;
            m->count++;
        }}
    }}
    free(old_keys); free(old_values); free(old_states);
}}

static inline {mangled} {mangled}__new(void) {{
    return ({mangled}){{NULL, NULL, NULL, 0, 0}};
}}

static inline void {mangled}__put({mangled}* m, {key_type} key, {val_type} value) {{
    if (m->cap == 0 || m->count * 4 >= m->cap * 3) {{ {mangled}__grow(m); }}
    uint64_t h = {hash_key};
    size_t idx = (size_t)(h % m->cap);
    size_t first_tombstone = (size_t)-1;
    for (size_t __probes = 0; __probes < m->cap; __probes++) {{
        if (m->states[idx] == 0) {{
            size_t target = first_tombstone != (size_t)-1 ? first_tombstone : idx;
            m->keys[target] = key;
            m->values[target] = value;
            m->states[target] = 1;
            m->count++;
            return;
        }}
        if (m->states[idx] == 2 && first_tombstone == (size_t)-1) {{ first_tombstone = idx; }}
        if (m->states[idx] == 1 && {eq_put}) {{
            m->values[idx] = value;
            return;
        }}
        idx = (idx + 1) % m->cap;
    }}
    if (first_tombstone != (size_t)-1) {{
        m->keys[first_tombstone] = key;
        m->values[first_tombstone] = value;
        m->states[first_tombstone] = 1;
        m->count++;
    }}
}}

static inline {val_type}* {mangled}__get_ptr({mangled}* m, {key_type} key) {{
    if (m->cap == 0) return NULL;
    uint64_t h = {hash_key};
    size_t idx = (size_t)(h % m->cap);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {{
        if (m->states[idx] == 0) return NULL;
        if (m->states[idx] == 1 && {eq_get}) {{
            return &m->values[idx];
        }}
        idx = (idx + 1) % m->cap;
    }}
    return NULL;
}}

static inline bool {mangled}__contains({mangled}* m, {key_type} key) {{
    return {mangled}__get_ptr(m, key) != NULL;
}}

static inline bool {mangled}__remove({mangled}* m, {key_type} key) {{
    if (m->cap == 0) return false;
    uint64_t h = {hash_key};
    size_t idx = (size_t)(h % m->cap);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {{
        if (m->states[idx] == 0) return false;
        if (m->states[idx] == 1 && {eq_get}) {{
            m->states[idx] = 2;
            m->count--;
            return true;
        }}
        idx = (idx + 1) % m->cap;
    }}
    return false;
}}

static inline void {mangled}__clear({mangled}* m) {{
    if (m->states) memset(m->states, 0, m->cap);
    m->count = 0;
}}

static inline void {mangled}__free({mangled}* m) {{
    free(m->keys); free(m->values); free(m->states);
    m->keys = NULL; m->values = NULL; m->states = NULL;
    m->count = 0; m->cap = 0;
}}

"#));
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
        &mut self,
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

        // Activate type substitutions so that body codegen sees T → concrete type
        let prev_subs = std::mem::replace(&mut self.type_subs, subs);

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

        // Restore previous substitutions
        self.type_subs = prev_subs;
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
                    "Dict" | "Map" | "HashMap" => c_mangle::mangle_generic("GorgetMap", &c_args),
                    _ => c_mangle::mangle_generic(&name.node, &c_args),
                }
            }
            // Type::Ref removed
            _ => c_types::ast_type_to_c(ty, self.scopes),
        }
    }

    /// Map an AST type to C and register any generic instantiations found.
    pub fn type_to_c_with_registration(&mut self, ty: &crate::parser::ast::Type) -> String {
        if let crate::parser::ast::Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" => {}
                    "Dict" | "Map" | "HashMap" => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        self.register_generic("GorgetMap", &c_args, super::GenericInstanceKind::Map);
                    }
                    _ => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        // Determine kind
                        let kind = if self.generic_struct_templates.contains_key(&name.node) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.contains_key(&name.node) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(&name.node, &c_args, kind);
                    }
                }
            }
        }
        self.type_to_c(ty)
    }

    // ─── Tuple Typedefs ──────────────────────────────────────

    /// Register a tuple typedef, deduplicating by name. Returns the mangled name.
    // Linear dedup is fine here — typical programs have <30 tuple types,
    // where a scan over short strings is faster than HashSet hashing overhead.
    pub fn register_tuple_typedef(&mut self, c_field_types: &[String]) -> String {
        let name = c_mangle::mangle_tuple(c_field_types);
        if !self.tuple_typedefs.iter().any(|(n, _)| *n == name) {
            self.tuple_typedefs.push((name.clone(), c_field_types.to_vec()));
        }
        name
    }

    /// Pre-scan the module AST to discover tuple types in type annotations.
    pub fn discover_tuple_types(&mut self, module: &crate::parser::ast::Module) {
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
    fn scan_function_for_tuples(&mut self, f: &FunctionDef) {
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
    fn scan_block_for_tuples(&mut self, block: &crate::parser::ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt_for_tuples(&stmt.node);
        }
    }

    /// Scan a statement for tuple literal expressions.
    fn scan_stmt_for_tuples(&mut self, stmt: &Stmt) {
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
    fn scan_expr_for_tuples(&mut self, expr: &crate::span::Spanned<Expr>) {
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
    fn scan_type_for_tuples(&mut self, ty: &Type) {
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
        if self.tuple_typedefs.is_empty() {
            return;
        }
        emitter.emit_line("// ── Tuple Typedefs ──");
        for (name, field_types) in self.tuple_typedefs.iter() {
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

    // ─── Trait Helpers ────────────────────────────────────────

    /// Collect all methods for a trait, including inherited parent methods.
    /// Returns (method_ast, defining_trait_name) tuples in parent-first order.
    fn collect_all_trait_methods<'b>(
        &self,
        trait_def: &'b TraitDef,
        trait_defs: &'b HashMap<String, &'b TraitDef>,
    ) -> Vec<(&'b FunctionDef, String)> {
        let mut visited = HashSet::new();
        self.collect_all_trait_methods_inner(trait_def, trait_defs, &mut visited)
    }

    fn collect_all_trait_methods_inner<'b>(
        &self,
        trait_def: &'b TraitDef,
        trait_defs: &'b HashMap<String, &'b TraitDef>,
        visited: &mut HashSet<String>,
    ) -> Vec<(&'b FunctionDef, String)> {
        if !visited.insert(trait_def.name.node.clone()) {
            return Vec::new(); // cycle detected — skip
        }
        let mut methods = Vec::new();
        // Recursively collect parent methods first
        for parent_bound in &trait_def.extends {
            let parent_name = &parent_bound.node.name.node;
            if let Some(parent_def) = trait_defs.get(parent_name.as_str()) {
                methods.extend(self.collect_all_trait_methods_inner(parent_def, trait_defs, visited));
            }
        }
        // Then own methods
        for item in &trait_def.items {
            if let TraitItem::Method(f) = &item.node {
                methods.push((f, trait_def.name.node.clone()));
            }
        }
        methods
    }

    /// Check if a method name is provided in an equip block.
    fn equip_has_method(impl_block: &EquipBlock, method_name: &str) -> bool {
        impl_block.items.iter().any(|m| m.node.name.node == method_name)
    }

    /// Emit a forwarding function that delegates a trait method through a field (`via` delegation).
    fn emit_via_forwarding_method(
        &self,
        method: &FunctionDef,
        type_name: &str,
        trait_name: &str,
        field_name: &str,
        _impl_block: &EquipBlock,
        emitter: &mut CEmitter,
    ) {
        // Resolve the field's type name from the field_type_names map
        let field_type_name = self.field_type_names
            .get(&(type_name.to_string(), field_name.to_string()))
            .and_then(|ty| match ty {
                Type::Named { name, .. } => Some(name.node.clone()),
                Type::Primitive(p) => Some(c_types::primitive_to_c(*p).to_string()),
                _ => None,
            });
        let Some(field_type_name) = field_type_name else {
            return; // Cannot resolve field type — skip
        };

        // Build the wrapper function signature (for the Outer type)
        let (ret_type, func_name, params) =
            self.function_signature(method, Some((type_name, Some(trait_name))));

        // Build the target function name (for the field's type)
        let target_fn = c_mangle::mangle_trait_method(trait_name, &field_type_name, &method.name.node);

        // Build the forwarding arguments
        let self_param = method.params.iter().find(|p| p.node.name.node == "self");
        let is_mutable = self_param
            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
            .unwrap_or(false);

        let mut arg_parts = Vec::new();
        // Forward self as &self->field (pointer to the field)
        if self_param.is_some() {
            if is_mutable {
                arg_parts.push(format!("&self->{field_name}"));
            } else {
                arg_parts.push(format!("&self->{field_name}"));
            }
        }
        // Forward remaining parameters as-is
        for param in &method.params {
            if param.node.name.node == "self" {
                continue;
            }
            arg_parts.push(c_mangle::escape_keyword(&param.node.name.node));
        }
        let args = arg_parts.join(", ");

        // Emit the forwarding function
        emitter.emit_line(&format!("{ret_type} {func_name}({params}) {{"));
        emitter.indent();
        let is_void = matches!(method.return_type.node, Type::Primitive(PrimitiveType::Void));
        if is_void {
            emitter.emit_line(&format!("{target_fn}({args});"));
        } else {
            emitter.emit_line(&format!("return {target_fn}({args});"));
        }
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }

    /// Check if an equip block is for a generic type (should be deferred for monomorphization).
    fn is_generic_equip(&self, impl_block: &EquipBlock) -> bool {
        if let Type::Named { name, generic_args } = &impl_block.type_.node {
            if !generic_args.is_empty()
                && (self.generic_struct_templates.contains_key(&name.node)
                    || self.generic_enum_templates.contains_key(&name.node))
            {
                return true;
            }
        }
        false
    }

    /// Emit a monomorphized method from a generic equip block.
    fn emit_monomorphized_equip_method(
        &mut self,
        method: &FunctionDef,
        struct_generic_params: Option<&crate::span::Spanned<GenericParams>>,
        c_type_args: &[String],
        mangled_type_name: &str,
        trait_name: Option<&str>,
        emitter: &mut CEmitter,
    ) {
        let subs = self.build_type_substitutions(struct_generic_params, c_type_args);

        let ret_type = self.substitute_type(&method.return_type.node, &subs);

        // Build function name
        let func_name = if let Some(tname) = trait_name {
            c_mangle::mangle_trait_method(tname, mangled_type_name, &method.name.node)
        } else {
            c_mangle::mangle_method(mangled_type_name, &method.name.node)
        };

        // Build parameters
        let mut params_vec: Vec<String> = Vec::new();
        let self_param = method.params.iter().find(|p| p.node.name.node == "self");
        if let Some(sp) = self_param {
            let is_mutable = matches!(
                sp.node.ownership,
                Ownership::MutableBorrow | Ownership::Move
            );
            if is_mutable {
                params_vec.push(format!("{mangled_type_name}* self"));
            } else {
                params_vec.push(format!("const {mangled_type_name}* self"));
            }
        }
        for param in &method.params {
            if param.node.name.node == "self" {
                continue;
            }
            let param_type = self.substitute_type(&param.node.type_.node, &subs);
            let param_name = c_mangle::escape_keyword(&param.node.name.node);
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                params_vec.push(format!("{param_type}* {param_name}"));
            } else {
                params_vec.push(c_types::c_declare(&param_type, &param_name));
            }
        }
        let params = if params_vec.is_empty() {
            "void".to_string()
        } else {
            params_vec.join(", ")
        };

        // Emit prototype
        emitter.emit_line(&format!("{ret_type} {func_name}({params});"));

        // Activate substitutions and self type for body codegen
        self.type_subs = subs;
        let prev_self_type = self.current_self_type.take();
        self.current_self_type = Some(mangled_type_name.to_string());

        // Track mutable borrow params as pointer params for body codegen
        let prev_pointer_params = std::mem::take(&mut self.pointer_params);
        for param in &method.params {
            if param.node.name.node == "self" {
                continue;
            }
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                self.pointer_params
                    .insert(c_mangle::escape_keyword(&param.node.name.node));
            }
        }

        // Emit definition
        match &method.body {
            FunctionBody::Expression(expr) => {
                emitter.emit_line(&format!("{ret_type} {func_name}({params}) {{"));
                emitter.indent();
                let e = self.gen_expr(expr);
                emitter.emit_line(&format!("return {e};"));
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Block(block) => {
                emitter.emit_line(&format!("{ret_type} {func_name}({params}) {{"));
                emitter.indent();
                self.gen_block(block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Declaration => {}
        }

        self.type_subs.clear();
        self.current_self_type = prev_self_type;
        self.pointer_params = prev_pointer_params;
    }

    /// Apply type parameter substitutions to an AST Type, returning a new AST Type.
    /// Used to register substituted field types for monomorphized generic structs.
    fn substitute_type_ast(
        &self,
        ty: &Type,
        subs: &[(String, String)],
    ) -> Type {
        match ty {
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                // Check if this is a type parameter being substituted
                for (param_name, c_type) in subs {
                    if name.node == *param_name {
                        // Map C type back to an AST type for field_type_names
                        return match c_type.as_str() {
                            "int64_t" => Type::Primitive(PrimitiveType::Int),
                            "double" => Type::Primitive(PrimitiveType::Float),
                            "bool" => Type::Primitive(PrimitiveType::Bool),
                            "const char*" => Type::Primitive(PrimitiveType::Str),
                            "char" => Type::Primitive(PrimitiveType::Char),
                            _ => Type::Named {
                                name: crate::span::Spanned::dummy(c_type.clone()),
                                generic_args: vec![],
                            },
                        };
                    }
                }
                ty.clone()
            }
            Type::Named { name, generic_args } => {
                // Recursively substitute generic args
                let new_args: Vec<crate::span::Spanned<Type>> = generic_args
                    .iter()
                    .map(|a| crate::span::Spanned::dummy(self.substitute_type_ast(&a.node, subs)))
                    .collect();
                Type::Named {
                    name: name.clone(),
                    generic_args: new_args,
                }
            }
            _ => ty.clone(),
        }
    }

    /// Convert a TypeId to a C type string (convenience wrapper).
    fn type_id_to_c(&self, type_id: crate::semantic::ids::TypeId) -> String {
        c_types::type_id_to_c(type_id, self.types, self.scopes)
    }

    /// Resolve an AST type to C, respecting active generic substitutions.
    /// During monomorphized function body codegen, type params (e.g. `T`) are
    /// replaced with their concrete C types. Outside that context, this falls
    /// back to `ast_type_to_c`.
    pub fn type_to_c(&self, ty: &crate::parser::ast::Type) -> String {
        if !self.type_subs.is_empty() {
            self.substitute_type(ty, &self.type_subs)
        } else {
            c_types::ast_type_to_c(ty, self.scopes)
        }
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

    // ─── Test Runner ────────────────────────────────────────────

    /// Check whether a test definition matches the tag filter.
    fn test_matches_tag(&self, test: &crate::parser::ast::TestDef) -> bool {
        if self.test_tag_filter.is_empty() {
            return true; // No filter — run all tests
        }
        for attr in &test.attributes {
            if attr.node.name.node == "tag" {
                for arg in &attr.node.args {
                    if let crate::parser::ast::AttributeArg::StringLiteral(s) = arg {
                        if self.test_tag_filter.contains(s) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Emit the test runner `main()` function for test modules.
    pub fn emit_test_runner_main(&mut self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Test Runner ──");
        emitter.emit_line("int main(int argc, char** argv) {");
        emitter.indent();
        emitter.emit_line("gorget_init_args(argc, argv);");
        emitter.emit_line("int __test_passed = 0, __test_failed = 0;");
        emitter.blank_line();

        // Suite setup (inlined)
        for item in &module.items {
            if let Item::SuiteSetup(s) = &item.node {
                emitter.emit_line("// suite setup");
                emitter.emit_line("{");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Function);
                self.gen_block(&s.body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
        }

        // Each test
        for item in &module.items {
            if let Item::Test(t) = &item.node {
                if !self.test_matches_tag(t) {
                    continue;
                }
                let test_name = &t.name.node;
                let escaped_name = test_name.replace('\\', "\\\\").replace('"', "\\\"");
                emitter.emit_line(&format!("printf(\"  test: {escaped_name} ... \");"));
                emitter.emit_line("fflush(stdout);");
                emitter.emit_line("{");
                emitter.indent();
                emitter.emit_line("__gorget_in_test = 1;");
                emitter.emit_line("__gorget_test_fail_msg = NULL;");
                emitter.emit_line("int __cleanup_mark = __gorget_cleanup_top;");
                emitter.emit_line("if (setjmp(__gorget_test_jmp) == 0) {");
                emitter.indent();
                self.in_test_body = true;
                self.push_drop_scope(DropScopeKind::Function);
                self.gen_block(&t.body, emitter);
                self.pop_drop_scope(emitter);
                self.in_test_body = false;
                emitter.emit_line("__gorget_cleanup_top = __cleanup_mark;");
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line("__gorget_cleanup_run(__cleanup_mark);");
                emitter.emit_line("__gorget_in_test = 0;");
                emitter.emit_line("if (!__gorget_test_fail_msg) {");
                emitter.indent();
                emitter.emit_line("__test_passed++;");
                emitter.emit_line("printf(\"PASS\\n\");");
                emitter.dedent();
                emitter.emit_line("} else {");
                emitter.indent();
                emitter.emit_line("__test_failed++;");
                emitter.emit_line("printf(\"FAIL: %s\\n\", __gorget_test_fail_msg);");
                emitter.dedent();
                emitter.emit_line("}");
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
        }

        // Suite teardown (inlined)
        for item in &module.items {
            if let Item::SuiteTeardown(s) = &item.node {
                emitter.emit_line("// suite teardown");
                emitter.emit_line("{");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Function);
                self.gen_block(&s.body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
        }

        emitter.emit_line("printf(\"\\n%d passed, %d failed\\n\", __test_passed, __test_failed);");
        emitter.emit_line("return __test_failed > 0 ? 1 : 0;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }
}
