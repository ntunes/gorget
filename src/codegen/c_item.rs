/// Top-level item codegen: functions, structs, enums, impl blocks, const/static.
use std::collections::{HashMap, HashSet, VecDeque};
use crate::parser::ast::*;
use crate::span::Spanned;
use super::c_emitter::CEmitter;
use super::c_mangle;
use super::c_types;
use super::{CodegenContext, DropAction, DropEntry, DropScopeKind};

/// Analysis result for an async function body.
#[allow(dead_code)]
struct AsyncAnalysis {
    params: Vec<(String, String)>,                  // (name, c_type)
    locals: Vec<(String, String, Option<Type>)>,    // (name, c_type, ast_type for drop)
    await_count: usize,
    sub_futures: Vec<(usize, String)>,              // (index, future_c_type)
    inner_return_c_type: String,                    // T, not Future[T]
    future_type_name: String,                       // "Future__int64_t"
    drop_entries: Vec<DropEntry>,                   // non-Copy fields needing cleanup
}

/// Classification of iterable collection types for async for-loop state lifting.
enum AsyncIterableKind {
    Vector(String),           // elem C type
    Dict(String, String),     // (key, val) C types — ordered
    HashMap(String, String),  // (key, val) C types — unordered
    String,
    Set(String),              // elem C type
    Unknown,                  // fallback to busy-poll
}

/// Build a mangled name for a Callable/MutCallable/ConsumeCallable trait signature.
/// E.g., Callable[int(int, float)] → "Callable__int64_t__int64_t__double"
pub(super) fn callable_sig_name(prefix: &str, param_c_types: &[String], ret_c_type: &str) -> String {
    let ret_mangled = c_mangle::mangle_c_type(ret_c_type);
    let params_mangled: Vec<String> = param_c_types.iter()
        .map(|t| c_mangle::mangle_c_type(t))
        .collect();
    if params_mangled.is_empty() {
        format!("{prefix}__{ret_mangled}")
    } else {
        format!("{prefix}__{ret_mangled}__{}", params_mangled.join("__"))
    }
}

/// A single Await node found during depth-first expression traversal.
struct CollectedAwait<'a> {
    /// The expression inside the Await (e.g., the Call producing a Future).
    inner_expr: &'a Spanned<Expr>,
    /// span.start of the Await node itself (used as replacement key).
    await_span_start: usize,
    /// True when inner is NOT a Call (task-await on a Task[T] variable).
    is_task: bool,
}

/// Depth-first left-to-right collection of Await nodes from an expression tree.
/// Inner (nested) awaits are collected before outer ones so they are emitted first.
fn collect_awaits_from_spanned<'a>(expr: &'a Spanned<Expr>, out: &mut Vec<CollectedAwait<'a>>) {
    match &expr.node {
        Expr::Await { expr: inner } => {
            // Recurse into inner first (handles nested awaits like `await f(await g())`)
            collect_awaits_from_spanned(inner, out);
            // Collect both Future-awaits (inner is Call) and Task-awaits (inner is Identifier/etc.)
            out.push(CollectedAwait {
                inner_expr: inner,
                await_span_start: expr.span.start,
                is_task: !matches!(inner.node, Expr::Call { .. }),
            });
        }
        Expr::Call { callee, args, .. } => {
            collect_awaits_from_spanned(callee, out);
            for a in args {
                collect_awaits_from_spanned(&a.node.value, out);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            collect_awaits_from_spanned(left, out);
            collect_awaits_from_spanned(right, out);
        }
        Expr::UnaryOp { operand, .. } => collect_awaits_from_spanned(operand, out),
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            collect_awaits_from_spanned(object, out);
        }
        Expr::Index { object, index } => {
            collect_awaits_from_spanned(object, out);
            collect_awaits_from_spanned(index, out);
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_awaits_from_spanned(receiver, out);
            for a in args {
                collect_awaits_from_spanned(&a.node.value, out);
            }
        }
        Expr::As { expr: inner, .. }
        | Expr::Try { expr: inner }
        | Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner }
        | Expr::TryCapture { expr: inner } => collect_awaits_from_spanned(inner, out),
        Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
            for e in elems {
                collect_awaits_from_spanned(e, out);
            }
        }
        Expr::NilCoalescing { lhs, rhs } => {
            collect_awaits_from_spanned(lhs, out);
            collect_awaits_from_spanned(rhs, out);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { collect_awaits_from_spanned(s, out); }
            if let Some(e) = end { collect_awaits_from_spanned(e, out); }
        }
        _ => {}
    }
}

/// Collect all Await nodes from a spanned expression, returned in evaluation order.
fn collect_awaits_vec<'a>(expr: &'a Spanned<Expr>) -> Vec<CollectedAwait<'a>> {
    let mut out = Vec::new();
    collect_awaits_from_spanned(expr, &mut out);
    out
}

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

    /// Emit all type definitions (structs, enums, type aliases, newtypes, traits).
    /// Structs and non-generic enums are topologically sorted together so that
    /// an enum embedding a struct by value (e.g. `Token` with `TkString(StringLiteral)`)
    /// is emitted after the struct it depends on.
    pub fn emit_type_definitions(&mut self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        emitter.emit_line("// ── Type Definitions ──");

        // Emit type aliases, newtypes, and traits first (no by-value embedding issues)
        for item in &module.items {
            match &item.node {
                Item::TypeAlias(a) => self.emit_type_alias(a, emitter),
                Item::Newtype(nt) => self.emit_newtype(nt, emitter),
                Item::Trait(t) => self.emit_trait_def(t, emitter),
                _ => {}
            }
        }

        // Unified type def tracking for topological sort of structs + enums
        enum TypeDefEntry<'a> {
            Struct(&'a StructDef),
            Enum(&'a EnumDef),
        }

        let mut type_defs: Vec<TypeDefEntry> = Vec::new();
        let mut name_to_idx: HashMap<String, usize> = HashMap::new();
        for item in &module.items {
            match &item.node {
                Item::Struct(s) if s.generic_params.is_none() && s.span != crate::span::Span::dummy() => {
                    name_to_idx.insert(s.name.node.clone(), type_defs.len());
                    type_defs.push(TypeDefEntry::Struct(s));
                }
                Item::Enum(e) if e.generic_params.is_none() => {
                    name_to_idx.insert(e.name.node.clone(), type_defs.len());
                    type_defs.push(TypeDefEntry::Enum(e));
                }
                _ => {}
            }
        }

        // Build dependency graph
        let n = type_defs.len();
        let mut deps: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, td) in type_defs.iter().enumerate() {
            let mut dep_names = Vec::new();
            match td {
                TypeDefEntry::Struct(s) => {
                    for field in &s.fields {
                        Self::collect_value_type_dep_names(&field.node.type_.node, &mut dep_names);
                    }
                }
                TypeDefEntry::Enum(e) => {
                    for variant in &e.variants {
                        if let VariantFields::Tuple(types) = &variant.node.fields {
                            for ty in types {
                                Self::collect_value_type_dep_names(&ty.node, &mut dep_names);
                            }
                        }
                    }
                }
            }
            for dep_name in &dep_names {
                if let Some(&j) = name_to_idx.get(dep_name) {
                    if j != i {
                        deps[i].push(j);
                    }
                }
            }
        }

        // Topological sort (Kahn's algorithm)
        // in_degree[i] = number of i's dependencies
        let mut in_degree = vec![0usize; n];
        for (i, d) in deps.iter().enumerate() {
            in_degree[i] = d.len();
        }

        // Reverse adjacency: rev_deps[j] = list of i's that depend on j
        let mut rev_deps: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, d) in deps.iter().enumerate() {
            for &j in d {
                rev_deps[j].push(i);
            }
        }

        let mut queue: VecDeque<usize> = VecDeque::new();
        for i in 0..n {
            if in_degree[i] == 0 {
                queue.push_back(i);
            }
        }

        let mut sorted_indices: Vec<usize> = Vec::new();
        while let Some(j) = queue.pop_front() {
            sorted_indices.push(j);
            for &i in &rev_deps[j] {
                in_degree[i] -= 1;
                if in_degree[i] == 0 {
                    queue.push_back(i);
                }
            }
        }

        // Emit in topological order, interleaving generic enum instances as needed
        for &idx in &sorted_indices {
            match &type_defs[idx] {
                TypeDefEntry::Struct(s) => {
                    let mut enum_deps = Vec::new();
                    for field in &s.fields {
                        self.collect_field_generic_enum_deps(&field.node.type_.node, &mut enum_deps);
                    }
                    for (mangled, base_name, c_type_args) in &enum_deps {
                        if !self.emitted_in_type_defs.contains(mangled) {
                            if let Some(template) = self.generic_enum_templates.get(base_name).cloned() {
                                self.emit_monomorphized_enum(&template, c_type_args, mangled, emitter);
                                self.emitted_in_type_defs.insert(mangled.clone());
                            }
                        }
                    }
                    self.emit_struct_def(s, emitter);
                }
                TypeDefEntry::Enum(e) => {
                    self.emit_enum_def(e, emitter);
                }
            }
        }

        // Emit any remaining type defs (cycles — shouldn't happen but be safe)
        if sorted_indices.len() < n {
            let sorted_set: HashSet<usize> = sorted_indices.iter().copied().collect();
            for i in 0..n {
                if !sorted_set.contains(&i) {
                    match &type_defs[i] {
                        TypeDefEntry::Struct(s) => {
                            let mut enum_deps = Vec::new();
                            for field in &s.fields {
                                self.collect_field_generic_enum_deps(&field.node.type_.node, &mut enum_deps);
                            }
                            for (mangled, base_name, c_type_args) in &enum_deps {
                                if !self.emitted_in_type_defs.contains(mangled) {
                                    if let Some(template) = self.generic_enum_templates.get(base_name).cloned() {
                                        self.emit_monomorphized_enum(&template, c_type_args, mangled, emitter);
                                        self.emitted_in_type_defs.insert(mangled.clone());
                                    }
                                }
                            }
                            self.emit_struct_def(s, emitter);
                        }
                        TypeDefEntry::Enum(e) => {
                            self.emit_enum_def(e, emitter);
                        }
                    }
                }
            }
        }

        emitter.blank_line();
    }

    /// Collect named types that appear as by-value field types (dependencies for ordering).
    /// Non-generic Named types are direct dependencies. For generic Named types, we recurse
    /// into their type args to capture transitive dependencies (e.g., `Option[Color]` → `Color`).
    fn collect_value_type_dep_names(ty: &Type, out: &mut Vec<String>) {
        match ty {
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                out.push(name.node.clone());
            }
            Type::Named { name, generic_args } if !generic_args.is_empty() => {
                // Pointer-based wrappers store their type args behind pointers,
                // so the args don't need to be fully defined — skip them.
                let pointer_based = matches!(
                    name.node.as_str(),
                    "Box" | "Rc" | "Arc" | "Weak" | "Vector" | "Dict" | "HashMap"
                );
                if !pointer_based {
                    for arg in generic_args {
                        Self::collect_value_type_dep_names(&arg.node, out);
                    }
                }
            }
            _ => {}
        }
    }

    /// Collect non-pointer-safe generic enum dependencies from a field's AST type.
    /// Returns `(mangled_name, base_name, c_type_args)` triples for generic enums
    /// whose type args include non-pointer-safe (user-defined value) types.
    /// Inner deps are collected before outer (recursion-first).
    fn collect_field_generic_enum_deps(
        &self,
        ty: &Type,
        out: &mut Vec<(String, String, Vec<String>)>,
    ) {
        if let Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                // Recurse into type args first (inner deps come before outer)
                for arg in generic_args {
                    self.collect_field_generic_enum_deps(&arg.node, out);
                }
                // If this is a generic enum template with non-pointer-safe args,
                // it must be emitted during the struct topo sort phase
                if self.generic_enum_templates.contains_key(&name.node) {
                    let c_type_args: Vec<String> = generic_args.iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    if !c_type_args.iter().all(|a| Self::is_pointer_safe_type_arg(a)) {
                        let mangled = c_mangle::mangle_generic(&name.node, &c_type_args);
                        if !out.iter().any(|(m, _, _)| m == &mangled) {
                            out.push((mangled, name.node.clone(), c_type_args));
                        }
                    }
                }
            }
        }
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
                    let trait_type_args = self.impl_trait_type_args(impl_block);
                    // Emit prototypes for explicitly implemented methods
                    for method in &impl_block.items {
                        self.emit_function_prototype(
                            &method.node,
                            Some((&type_name, trait_name.as_deref(), &trait_type_args)),
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
                                        Some((&type_name, Some(tname), &trait_type_args)),
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
        method_info: Option<(&str, Option<&str>, &[String])>,
        emitter: &mut CEmitter,
    ) {
        if f.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        if matches!(f.body, FunctionBody::Extern(_)) {
            return; // Extern binding — no C prototype needed
        }
        // Declaration bodies from synthetic modules (dummy span) get hardcoded dispatch
        // and don't need prototypes. But Declaration bodies from user code (e.g. via-delegation)
        // DO get real function definitions and need forward declarations.
        if matches!(f.body, FunctionBody::Declaration) && f.name.span == crate::span::Span::dummy() {
            return;
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
                        // In test mode, skip user's main() — the test runner provides main()
                        if self.is_test_module && f.name.node == "main" {
                            continue;
                        }
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
                    let trait_type_args = self.impl_trait_type_args(impl_block);
                    self.current_self_type = Some(type_name.clone());
                    // Emit explicitly implemented methods
                    for method in &impl_block.items {
                        self.emit_function_def(
                            &method.node,
                            Some((&type_name, trait_name.as_deref(), &trait_type_args)),
                            emitter,
                        );
                    }
                    // Emit default/inherited method bodies not overridden
                    if let Some(tname) = &trait_name {
                        if let Some(trait_def) = trait_defs.get(tname.as_str()) {
                            let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);
                            for (method, _) in &all_methods {
                                if !Self::equip_has_method(impl_block, &method.name.node) {
                                    if !matches!(method.body, FunctionBody::Declaration | FunctionBody::Extern(_)) {
                                        // Default method body — emit as-is
                                        self.emit_function_def(
                                            method,
                                            Some((&type_name, Some(tname), &trait_type_args)),
                                            emitter,
                                        );
                                    } else if let Some(ref via) = impl_block.via_field {
                                        // No default body — delegate via field
                                        self.emit_via_forwarding_method(
                                            method,
                                            &type_name,
                                            tname,
                                            &via.node,
                                            &trait_type_args,
                                            impl_block,
                                            emitter,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    self.current_self_type = None;
                    self.self_is_mutable = false;
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
        method_info: Option<(&str, Option<&str>, &[String])>,
        emitter: &mut CEmitter,
    ) {
        if f.generic_params.is_some() {
            return; // Generic template — emitted per-instantiation
        }
        let is_main = f.name.node == "main" && method_info.is_none();

        // Async functions get state-machine transformation instead of normal codegen.
        // Declaration-bodied async functions (stdlib builtins like sleep) are skipped —
        // their codegen happens via call-site dispatch in c_expr_call.rs.
        if f.qualifiers.is_async && !matches!(f.body, FunctionBody::Declaration) {
            self.emit_async_function(f, method_info, is_main, emitter);
            return;
        }
        let (ret_type, func_name, params) = if is_main {
            ("int".to_string(), "main".to_string(), "int argc, char** argv".to_string())
        } else {
            self.function_signature(f, method_info)
        };

        // Track whether this function throws
        self.current_function_throws = f.throws.is_some();

        // Track return type for Result-based ? codegen and ret_tmp_type
        self.current_function_return_c_type = if is_main {
            None
        } else {
            Some(ret_type.clone())
        };

        // Track mutable borrow params as pointer params for body codegen.
        let prev_pointer_params = std::mem::take(&mut self.pointer_params);
        let prev_self_is_mutable = self.self_is_mutable;
        // Save/restore closure_vars and fn_type_signatures for Fn-typed params.
        let prev_closure_vars = self.closure_vars.clone();
        let prev_fn_type_sigs = self.fn_type_signatures.clone();
        for param in &f.params {
            if param.node.name.node == "self" {
                continue;
            }
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                self.pointer_params
                    .insert(c_mangle::escape_keyword(&param.node.name.node));
            }
            // Detect Callable/MutCallable/ConsumeCallable-typed params — register for closure dispatch
            if let Type::Named { name, generic_args } = &param.node.type_.node {
                let callable_kind = match name.node.as_str() {
                    "Callable" => Some(super::CallableKind::Callable),
                    "MutCallable" => Some(super::CallableKind::MutCallable),
                    "ConsumeCallable" => Some(super::CallableKind::ConsumeCallable),
                    _ => None,
                };
                if let Some(kind) = callable_kind {
                    if generic_args.len() == 1 {
                        let escaped = c_mangle::escape_keyword(&param.node.name.node);
                        self.closure_vars.insert(escaped.clone());
                        // Extract signature from the function type generic arg
                        if let Type::Function { return_type, params: fn_params, param_ownerships } = &generic_args[0].node {
                            let ret_c = c_types::ast_type_to_c(&return_type.node, self.scopes);
                            let param_c: Vec<String> = fn_params.iter().enumerate()
                                .map(|(i, p)| {
                                    let base = c_types::ast_type_to_c(&p.node, self.scopes);
                                    let ownership = param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
                                    if matches!(ownership, Ownership::MutableBorrow) {
                                        format!("{base}*")
                                    } else {
                                        base
                                    }
                                })
                                .collect();
                            self.fn_type_signatures.insert(escaped.clone(), (param_c.clone(), ret_c.clone(), param_ownerships.clone()));
                            // Register the signature for vtable generation
                            let sig = (kind, param_c, ret_c);
                            if !self.fn_trait_sigs.contains(&sig) {
                                self.fn_trait_sigs.push(sig);
                            }
                        }
                    }
                }
            }
        }

        // Set current function scope for scope-aware variable lookup.
        let prev_function_scope = self.current_function_scope.take();
        let scope_key = (f.name.node.clone(), f.name.span.start);
        if let Some(&scope_id) = self.function_body_scopes.get(&scope_key) {
            self.current_function_scope = Some(scope_id);
        }

        // Compute the Gorget-display name for this function (used in trace output).
        let gorget_name = if let Some((type_name, _, _)) = method_info {
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

                if let Some((type_name, _, _)) = method_info {
                    self.current_self_type = Some(type_name.to_string());
                    let self_param = f.params.iter().find(|p| p.node.name.node == "self");
                    self.self_is_mutable = self_param
                        .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
                        .unwrap_or(false);
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

                // Pre-scan for escaping closures (returned from this function)
                let escaping = self.scan_escaping_closures(block);
                self.escaping_closure_vars = escaping;

                self.gen_block(block, emitter);
                self.escaping_closure_vars.clear();
                self.pop_drop_scope(emitter);

                self.decl_type_hint = prev_hint;

                // Trace implicit return for void functions (and main)
                if self.trace && !is_main {
                    if matches!(f.return_type.node, Type::Primitive(PrimitiveType::Void)) {
                        self.emit_trace_return(emitter);
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
                    emitter.emit_line(&format!("__typeof__({e}) __trace_ret = {e};"));
                    self.emit_trace_return(emitter);
                    emitter.emit_line("return __trace_ret;");
                } else {
                    emitter.emit_line(&format!("return {e};"));
                }

                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {
                // External declaration — no body
            }
        }

        self.current_function_gorget_name = prev_gorget_name;

        self.pointer_params = prev_pointer_params;
        self.self_is_mutable = prev_self_is_mutable;
        self.closure_vars = prev_closure_vars;
        self.fn_type_signatures = prev_fn_type_sigs;
        self.current_function_scope = prev_function_scope;
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
        method_info: Option<(&str, Option<&str>, &[String])>,
    ) -> (String, String, String) {
        let ret_type = if f.qualifiers.is_async {
            // Async functions return Future[T] where T is the declared return type
            let inner = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
            c_mangle::mangle_generic("Future", &[inner])
        } else if matches!(f.return_type.node, Type::SelfType) {
            if let Some((type_name, _, _)) = method_info {
                type_name.to_string()
            } else {
                c_types::ast_type_to_c(&f.return_type.node, self.scopes)
            }
        } else if matches!(f.return_type.node, Type::Function { .. }) {
            // Function types as return values use GorgetClosure (supports both
            // capturing and non-capturing closures).
            "GorgetClosure".to_string()
        } else {
            c_types::ast_type_to_c(&f.return_type.node, self.scopes)
        };

        let func_name = if let Some((type_name, trait_name, trait_type_args)) = method_info {
            if let Some(tname) = trait_name {
                c_mangle::mangle_trait_method(tname, type_name, &f.name.node, trait_type_args)
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
            if let Some((type_name, _, _)) = method_info {
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
                if let Some((type_name, _, _)) = method_info {
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

    /// Emit a const declaration. Skip stdlib consts (dummy span).
    fn emit_const_decl(&mut self, c: &ConstDecl, emitter: &mut CEmitter) {
        if c.span == crate::span::Span::dummy() {
            return; // stdlib const — codegen maps directly to C runtime constants
        }
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
                let trait_type_args = self.impl_trait_type_args(impl_block);

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
                    let impl_fn = c_mangle::mangle_trait_method(&trait_name, &type_name, method_name, &trait_type_args);

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

        // Emit vtable instances for generic equip blocks (per-instantiation)
        for inst in &self.generic_instances {
            match inst.kind {
                super::GenericInstanceKind::Struct | super::GenericInstanceKind::Enum => {}
                _ => continue,
            }
            let equip_blocks = match self.generic_equip_templates.get(&inst.base_name) {
                Some(blocks) => blocks.clone(),
                None => continue,
            };
            let generic_params = self.generic_struct_templates
                .get(&inst.base_name)
                .and_then(|t| t.generic_params.clone())
                .or_else(|| {
                    self.generic_enum_templates
                        .get(&inst.base_name)
                        .and_then(|t| t.generic_params.clone())
                });
            let subs = self.build_type_substitutions(generic_params.as_ref(), &inst.c_type_args);

            for equip_block in &equip_blocks {
                let Some(trait_ref) = &equip_block.trait_ else {
                    continue;
                };
                let trait_name = match &trait_ref.trait_name.node {
                    Type::Named { name, .. } => name.node.clone(),
                    _ => continue,
                };
                let Some(trait_def) = trait_defs.get(&trait_name) else {
                    continue;
                };

                // For monomorphized equip blocks, trait type args may contain generic params
                // (e.g., T) that need substitution before converting to C types.
                let raw_trait_args = Self::impl_trait_type_args_raw(equip_block);
                let trait_type_args: Vec<String> = raw_trait_args.iter()
                    .map(|ty| self.substitute_type(ty, &subs))
                    .collect();

                let vtable_type = c_mangle::mangle_vtable_struct(&trait_name);
                let vtable_instance = c_mangle::mangle_vtable_instance(&trait_name, &inst.mangled_name);

                emitter.emit_line(&format!("static const {vtable_type} {vtable_instance} = {{"));
                emitter.indent();

                let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);

                for (method, _defining_trait) in &all_methods {
                    let method_name = &method.name.node;
                    let impl_fn = c_mangle::mangle_trait_method(&trait_name, &inst.mangled_name, method_name, &trait_type_args);

                    // Use substitute_type for return/param types (trait methods may reference generic params)
                    let ret_type = self.substitute_type(&method.return_type.node, &subs);
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
                            cast_params.push(self.substitute_type(&param.node.type_.node, &subs));
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

    /// Emit all lifted closure structs, call functions, and vtable adapters.
    pub fn emit_lifted_closures(&mut self, emitter: &mut CEmitter) {
        use super::CaptureMode;

        // Take closures out to avoid borrow issues
        let closures = std::mem::take(&mut self.lifted_closures);

        for closure in closures.iter() {
            let env_name = super::c_mangle::mangle_closure_env(closure.id);
            let fn_name = super::c_mangle::mangle_closure(closure.id);
            let struct_name = &closure.struct_name;

            // Per-closure struct typedef (replaces env struct)
            // Non-capturing closures get an empty struct for vtable compatibility.
            emitter.emit_line(&format!("typedef struct {{"));
            if !closure.captures.is_empty() {
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
                // Also typedef the old env name as an alias for backward compat
                emitter.emit_line(&format!("}} {struct_name};"));
                emitter.emit_line(&format!("typedef {struct_name} {env_name};"));
            } else {
                emitter.indent();
                emitter.emit_line("char __empty;");
                emitter.dedent();
                emitter.emit_line(&format!("}} {struct_name};"));
            }
            emitter.blank_line();

            // Legacy closure function (void* __env_ptr) — still needed for GorgetClosure dispatch
            if !closure.captures.is_empty() {
                let mut params_vec: Vec<String> = Vec::new();
                params_vec.push("void* __env_ptr".to_string());
                for (p_name, p_type) in &closure.params {
                    params_vec.push(c_types::c_declare(p_type, p_name));
                }
                let params_str = params_vec.join(", ");

                emitter.emit_line(&format!(
                    "static inline {} {fn_name}({params_str}) {{",
                    closure.return_type
                ));
                emitter.indent();
                emitter.emit_line(&format!(
                    "{env_name}* __env = ({env_name}*)__env_ptr;"
                ));
                for (cap_name, cap_type, mode) in &closure.captures {
                    if *mode == CaptureMode::ByValue {
                        emitter.emit_line(&format!(
                            "{cap_type} {cap_name} = __env->{cap_name};"
                        ));
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
            } else {
                // Non-capturing: bare function (no env)
                let mut params_vec: Vec<String> = Vec::new();
                for (p_name, p_type) in &closure.params {
                    params_vec.push(c_types::c_declare(p_type, p_name));
                }
                let params_str = if params_vec.is_empty() { "void".to_string() } else { params_vec.join(", ") };

                emitter.emit_line(&format!(
                    "static inline {} {fn_name}({params_str}) {{",
                    closure.return_type
                ));
                emitter.indent();
                if closure.return_type == "void" {
                    emitter.emit_line(&format!("{};", closure.body));
                } else {
                    emitter.emit_line(&format!("return {};", closure.body));
                }
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }

            // Typed __Closure_N__call function for vtable dispatch.
            // Takes const void* self for Callable compatibility.
            {
                let call_name = format!("{struct_name}__call");
                let mut call_params = vec!["const void* __self".to_string()];
                for (p_name, p_type) in &closure.params {
                    call_params.push(c_types::c_declare(p_type, p_name));
                }
                let call_params_str = call_params.join(", ");
                emitter.emit_line(&format!(
                    "static inline {} {call_name}({call_params_str}) {{",
                    closure.return_type
                ));
                emitter.indent();

                if !closure.captures.is_empty() {
                    // Cast to non-const for ByMutRef captures (which store pointers).
                    // Alias as __env so body expressions like (*__env->name) work.
                    emitter.emit_line(&format!(
                        "{struct_name}* __env = ({struct_name}*)__self;"
                    ));
                    for (cap_name, cap_type, mode) in &closure.captures {
                        if *mode == CaptureMode::ByValue {
                            emitter.emit_line(&format!(
                                "{cap_type} {cap_name} = __env->{cap_name};"
                            ));
                        }
                    }
                } else {
                    emitter.emit_line("(void)__self;");
                }

                if closure.return_type == "void" {
                    // For non-capturing closures, call the bare function.
                    // For capturing ones, inline the body (already unpacked captures).
                    if closure.captures.is_empty() {
                        let arg_names: Vec<&str> = closure.params.iter().map(|(n, _)| n.as_str()).collect();
                        emitter.emit_line(&format!("{fn_name}({});", arg_names.join(", ")));
                    } else {
                        emitter.emit_line(&format!("{};", closure.body));
                    }
                } else {
                    if closure.captures.is_empty() {
                        let arg_names: Vec<&str> = closure.params.iter().map(|(n, _)| n.as_str()).collect();
                        emitter.emit_line(&format!("return {fn_name}({});", arg_names.join(", ")));
                    } else {
                        emitter.emit_line(&format!("return {};", closure.body));
                    }
                }

                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }

            // Legacy adapter for non-capturing closures (void* env, ignored)
            if closure.captures.is_empty() {
                let adapter_name = format!("{fn_name}_fn");
                let mut adapter_params = vec!["void* __env_ptr".to_string()];
                for (p_name, p_type) in &closure.params {
                    adapter_params.push(c_types::c_declare(p_type, p_name));
                }
                let adapter_params_str = adapter_params.join(", ");
                let arg_names: Vec<&str> = closure.params.iter().map(|(n, _)| n.as_str()).collect();
                let call = format!("{fn_name}({})", arg_names.join(", "));
                emitter.emit_line(&format!(
                    "static inline {} {adapter_name}({adapter_params_str}) {{",
                    closure.return_type
                ));
                emitter.indent();
                emitter.emit_line("(void)__env_ptr;");
                if closure.return_type == "void" {
                    emitter.emit_line(&format!("{call};"));
                } else {
                    emitter.emit_line(&format!("return {call};"));
                }
                emitter.dedent();
                emitter.emit_line("}");
                emitter.blank_line();
            }
        }

        // Emit Callable vtable types and instances for closures used with Callable[sig] params
        self.emit_callable_vtables(&closures, emitter);

        // Restore closures
        self.lifted_closures = closures;
    }

    /// Emit Callable/MutCallable/ConsumeCallable vtable and trait object typedefs,
    /// plus vtable instances for each closure that needs them.
    fn emit_callable_vtables(&self, closures: &[super::LiftedClosure], emitter: &mut CEmitter) {
        if self.fn_trait_sigs.is_empty() {
            return;
        }

        // De-duplicate signatures
        let mut seen_sigs: Vec<(super::CallableKind, Vec<String>, String)> = Vec::new();
        for (kind, params, ret) in &self.fn_trait_sigs {
            let entry = (*kind, params.clone(), ret.clone());
            if !seen_sigs.contains(&entry) {
                seen_sigs.push(entry);
            }
        }

        emitter.emit_line("// ── Callable Trait Vtables ──");
        for (kind, param_c_types, ret_c_type) in &seen_sigs {
            let kind_prefix = match kind {
                super::CallableKind::Callable => "Callable",
                super::CallableKind::MutCallable => "MutCallable",
                super::CallableKind::ConsumeCallable => "ConsumeCallable",
            };
            let sig_name = callable_sig_name(kind_prefix, param_c_types, ret_c_type);
            let vtable_name = format!("{sig_name}__VTable");
            let traitobj_name = format!("{sig_name}__TraitObj");

            // VTable struct typedef — use const void* for all variants since
            // mutability enforcement is at the Gorget semantic level, not C.
            let self_ptr = "const void*";
            let call_params: Vec<String> = std::iter::once(self_ptr.to_string())
                .chain(param_c_types.iter().cloned())
                .collect();
            emitter.emit_line(&format!(
                "typedef struct {{ {ret_c_type} (*call)({params}); }} {vtable_name};",
                params = call_params.join(", ")
            ));

            // TraitObj struct typedef
            emitter.emit_line(&format!(
                "typedef struct {{ const void* data; const {vtable_name}* vtable; }} {traitobj_name};"
            ));
            emitter.blank_line();

            // Emit vtable instances only for closures whose signature matches
            for closure in closures {
                let closure_params: Vec<String> = closure.params.iter()
                    .map(|(_, t)| t.clone())
                    .collect();
                if &closure_params == param_c_types && closure.return_type == *ret_c_type {
                    let struct_name = &closure.struct_name;
                    let call_fn = format!("{struct_name}__call");
                    let vtable_inst = format!("{sig_name}__{struct_name}__vtable");
                    emitter.emit_line(&format!(
                        "static const {vtable_name} {vtable_inst} = {{ .call = {call_fn} }};"
                    ));
                }
            }

            // Emit vtable instances for named functions used as Callable args
            // (These would be registered separately if needed — for now, closures only)
            emitter.blank_line();
        }
    }

    // ─── Generic Monomorphization ────────────────────────────

    /// Discover generic type instantiations from semantic analysis data.
    /// The TypeTable contains authoritative `ResolvedType::Generic` entries for
    /// every generic type the program uses. This replaces the fragile AST walker
    /// for struct/enum/map instantiations.
    pub fn discover_generic_type_usages_from_semantic(&mut self, module: &crate::parser::ast::Module) {
        use crate::semantic::scope::DefKind;

        let instantiations = self.types.collect_generic_instantiations();

        for (def_id, type_args) in instantiations {
            // Skip non-concrete instantiations (still contain type parameters or errors)
            let all_concrete = type_args.iter().all(|tid| {
                self.is_concrete_type_id(*tid)
            });
            if !all_concrete {
                continue;
            }

            let def = self.scopes.get_def(def_id);

            // Skip generic params themselves (e.g. T in Generic(T_DefId, []))
            if def.kind == DefKind::GenericParam {
                continue;
            }

            let base_name = def.name.clone();

            let c_type_args: Vec<String> = type_args
                .iter()
                .map(|tid| self.type_id_to_c(*tid))
                .collect();

            // Ensure tuple type args have their typedefs registered early
            for tid in &type_args {
                if let crate::semantic::types::ResolvedType::Tuple(elems) = self.types.get(*tid) {
                    let c_field_types: Vec<String> = elems.iter()
                        .map(|e| self.type_id_to_c(*e))
                        .collect();
                    self.register_tuple_typedef(&c_field_types);
                }
            }

            match base_name.as_str() {
                // Built-in collection types handled by the runtime — no monomorphization needed
                "Vector" | "List" | "Array" | "Set" | "Box" => continue,
                "Dict" => {
                    self.register_generic(
                        "GorgetDict",
                        &c_type_args,
                        super::GenericInstanceKind::Map { ordered: true },
                    );
                }
                "HashMap" => {
                    self.register_generic(
                        "GorgetMap",
                        &c_type_args,
                        super::GenericInstanceKind::Map { ordered: false },
                    );
                }
                _ => {
                    let kind = if self.generic_struct_templates.contains_key(&base_name) {
                        super::GenericInstanceKind::Struct
                    } else if self.generic_enum_templates.contains_key(&base_name) {
                        super::GenericInstanceKind::Enum
                    } else {
                        // Not a type template — skip (function calls handled separately)
                        continue;
                    };
                    self.register_generic(&base_name, &c_type_args, kind);
                }
            }
        }

        // Second pass: scan AST enum/struct variant field types for generic type
        // references (e.g., Option[Box[Expr]]) that aren't in the TypeTable because
        // variant fields are stored as AST Type nodes, not resolved into TypeIds.
        for item in &module.items {
            match &item.node {
                Item::Enum(e) if e.generic_params.is_none() => {
                    for variant in &e.variants {
                        if let VariantFields::Tuple(fields) = &variant.node.fields {
                            for field in fields {
                                self.discover_generic_from_ast_type(&field.node);
                            }
                        }
                    }
                }
                Item::Struct(s) if s.generic_params.is_none() => {
                    for field in &s.fields {
                        self.discover_generic_from_ast_type(&field.node.type_.node);
                    }
                }
                _ => {}
            }
        }
    }

    /// Walk an AST Type and register any generic type instances it references.
    /// This catches types like `Option[Box[Expr]]` used in variant fields or
    /// struct fields that don't appear in the TypeTable.
    fn discover_generic_from_ast_type(&mut self, ty: &crate::parser::ast::Type) {
        if let crate::parser::ast::Type::Named { name, generic_args } = ty {
            if !generic_args.is_empty() {
                // Recurse into type args first
                for arg in generic_args {
                    self.discover_generic_from_ast_type(&arg.node);
                }
                // Runtime-provided types don't need monomorphization
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" | "Box" | "Channel" => return,
                    "Dict" => {
                        let c_args: Vec<String> = generic_args.iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        self.register_generic(
                            "GorgetDict",
                            &c_args,
                            super::GenericInstanceKind::Map { ordered: true },
                        );
                        return;
                    }
                    "HashMap" => {
                        let c_args: Vec<String> = generic_args.iter()
                            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                            .collect();
                        self.register_generic(
                            "GorgetMap",
                            &c_args,
                            super::GenericInstanceKind::Map { ordered: false },
                        );
                        return;
                    }
                    "Callable" | "MutCallable" | "ConsumeCallable" => return,
                    _ => {}
                }
                let c_args: Vec<String> = generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect();
                let kind = if self.generic_enum_templates.contains_key(&name.node) {
                    super::GenericInstanceKind::Enum
                } else if self.generic_struct_templates.contains_key(&name.node) {
                    super::GenericInstanceKind::Struct
                } else {
                    return;
                };
                self.register_generic(&name.node, &c_args, kind);
            }
        }
    }

    /// Check whether a TypeId refers to a concrete (fully resolved) type.
    /// Returns false for generic parameters, error sentinels, and inference variables.
    fn is_concrete_type_id(&self, tid: crate::semantic::ids::TypeId) -> bool {
        use crate::semantic::scope::DefKind;
        use crate::semantic::types::ResolvedType;

        match self.types.get(tid) {
            ResolvedType::Defined(def_id) => {
                self.scopes.get_def(*def_id).kind != DefKind::GenericParam
            }
            ResolvedType::Generic(_, args) => {
                args.iter().all(|a| self.is_concrete_type_id(*a))
            }
            ResolvedType::Tuple(elems) => {
                elems.iter().all(|e| self.is_concrete_type_id(*e))
            }
            ResolvedType::Array(elem, _) | ResolvedType::Slice(elem) => {
                self.is_concrete_type_id(*elem)
            }
            ResolvedType::Function { params, return_type, .. } => {
                params.iter().all(|p| self.is_concrete_type_id(*p))
                    && self.is_concrete_type_id(*return_type)
            }
            ResolvedType::Error | ResolvedType::Var(_) => false,
            // Primitives, Void, Never, TraitObject are concrete
            _ => true,
        }
    }

    /// Discover generic function call instantiations by scanning the AST.
    /// Function calls with explicit type args (e.g., `max[int](a, b)`) don't produce
    /// `ResolvedType::Generic` entries in the TypeTable, so we need a focused AST scan.
    /// This walker is exhaustive — no `_ => {}` catch-all.
    pub fn discover_generic_function_usages(&mut self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Function(f) => self.scan_function_for_generic_calls(f),
                Item::Equip(impl_block) => {
                    // Skip generic equip blocks — their bodies contain template
                    // type params (e.g. Pair[T]) that would register spurious instances
                    if !self.is_generic_equip(impl_block) {
                        for method in &impl_block.items {
                            self.scan_function_for_generic_calls(&method.node);
                        }
                    }
                }
                // Non-code items: no function calls to scan
                Item::Struct(_) | Item::Enum(_) | Item::Trait(_) | Item::TypeAlias(_)
                | Item::Newtype(_) | Item::ExternBlock(_) | Item::ConstDecl(_)
                | Item::StaticDecl(_) | Item::Import(_) | Item::Directive(_)
                | Item::Test(_) | Item::SuiteSetup(_) | Item::SuiteTeardown(_)
                | Item::MetaConst(_) | Item::MetaType(_) | Item::MetaTypeFunc(_)
                | Item::MetaAssert(_) | Item::MetaIf(_) => {}
            }
        }
    }

    /// Scan a function body for generic function calls.
    fn scan_function_for_generic_calls(&mut self, f: &FunctionDef) {
        if f.generic_params.is_some() {
            return; // Don't scan inside generic templates
        }
        match &f.body {
            FunctionBody::Block(block) => self.scan_block_for_generic_calls(block),
            FunctionBody::Expression(expr) => self.scan_expr_for_generic_calls(expr),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }

    fn scan_block_for_generic_calls(&mut self, block: &crate::parser::ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt_for_generic_calls(&stmt.node);
        }
    }

    fn scan_stmt_for_generic_calls(&mut self, stmt: &crate::parser::ast::Stmt) {
        match stmt {
            Stmt::VarDecl { value, .. } => self.scan_expr_for_generic_calls(value),
            Stmt::Expr(expr) => self.scan_expr_for_generic_calls(expr),
            Stmt::Assign { target, value } => {
                self.scan_expr_for_generic_calls(target);
                self.scan_expr_for_generic_calls(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.scan_expr_for_generic_calls(target);
                self.scan_expr_for_generic_calls(value);
            }
            Stmt::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.scan_expr_for_generic_calls(expr);
                }
            }
            Stmt::Throw(expr) => self.scan_expr_for_generic_calls(expr),
            Stmt::Break(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.scan_expr_for_generic_calls(expr);
                }
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.scan_expr_for_generic_calls(condition);
                self.scan_block_for_generic_calls(then_body);
                for (cond, body) in elif_branches {
                    self.scan_expr_for_generic_calls(cond);
                    self.scan_block_for_generic_calls(body);
                }
                if let Some(body) = else_body {
                    self.scan_block_for_generic_calls(body);
                }
            }
            Stmt::While { condition, body, else_body } => {
                self.scan_expr_for_generic_calls(condition);
                self.scan_block_for_generic_calls(body);
                if let Some(body) = else_body {
                    self.scan_block_for_generic_calls(body);
                }
            }
            Stmt::For { iterable, body, else_body, .. } => {
                self.scan_expr_for_generic_calls(iterable);
                self.scan_block_for_generic_calls(body);
                if let Some(body) = else_body {
                    self.scan_block_for_generic_calls(body);
                }
            }
            Stmt::Loop { body } => self.scan_block_for_generic_calls(body),
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.scan_expr_for_generic_calls(scrutinee);
                for arm in arms {
                    self.scan_expr_for_generic_calls(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_generic_calls(guard);
                    }
                }
                if let Some(else_body) = else_arm {
                    self.scan_block_for_generic_calls(else_body);
                }
            }
            Stmt::Select { arms, else_arm } => {
                for arm in arms {
                    match &arm.op {
                        SelectOp::Recv { channel, .. } => self.scan_expr_for_generic_calls(channel),
                        SelectOp::Send { channel, value } => {
                            self.scan_expr_for_generic_calls(channel);
                            self.scan_expr_for_generic_calls(value);
                        }
                    }
                    self.scan_block_for_generic_calls(&arm.body);
                }
                if let Some(else_body) = else_arm {
                    self.scan_block_for_generic_calls(else_body);
                }
            }
            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.scan_expr_for_generic_calls(&binding.expr);
                }
                self.scan_block_for_generic_calls(body);
            }
            Stmt::Unsafe { body } => self.scan_block_for_generic_calls(body),
            Stmt::Assert { condition, message } => {
                self.scan_expr_for_generic_calls(condition);
                if let Some(msg) = message {
                    self.scan_expr_for_generic_calls(msg);
                }
            }
            // Leaf statements: no expressions to scan
            Stmt::Continue | Stmt::Pass => {}
            // Nested items: handled at top level
            Stmt::Item(_) => {}
        }
    }

    fn scan_expr_for_generic_calls(&mut self, expr: &crate::span::Spanned<Expr>) {
        match &expr.node {
            Expr::Call { callee, generic_args, args } => {
                if let Some(type_args) = generic_args {
                    let c_type_args: Vec<String> = type_args
                        .iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    if let Expr::Identifier(name) = &callee.node {
                        // Type constructors (Dict, HashMap) are handled by semantic discovery;
                        // here we only register function calls.
                        let kind = if self.generic_fn_templates.contains_key(name) {
                            super::GenericInstanceKind::Function
                        } else if self.generic_struct_templates.contains_key(name) {
                            super::GenericInstanceKind::Struct
                        } else if self.generic_enum_templates.contains_key(name) {
                            super::GenericInstanceKind::Enum
                        } else {
                            super::GenericInstanceKind::Function
                        };
                        self.register_generic(name, &c_type_args, kind);
                    }
                }
                self.scan_expr_for_generic_calls(callee);
                for arg in args {
                    self.scan_expr_for_generic_calls(&arg.node.value);
                }
            }
            Expr::MethodCall { receiver, generic_args, args, .. } => {
                if let Some(type_args) = generic_args {
                    let c_type_args: Vec<String> = type_args
                        .iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    // Method-level generic args (e.g., receiver.method[int]())
                    // would need the method's template — register if found
                    if let Expr::Identifier(_) = &receiver.node {
                        // Generic method calls are rare; register_generic deduplicates
                        for _arg in &c_type_args { /* traversal below covers subexprs */ }
                    }
                    let _ = c_type_args; // consumed or intentionally unused
                }
                self.scan_expr_for_generic_calls(receiver);
                for arg in args {
                    self.scan_expr_for_generic_calls(&arg.node.value);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.scan_expr_for_generic_calls(left);
                self.scan_expr_for_generic_calls(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.scan_expr_for_generic_calls(operand);
            }
            Expr::If { condition, then_branch, elif_branches, else_branch } => {
                self.scan_expr_for_generic_calls(condition);
                self.scan_expr_for_generic_calls(then_branch);
                for (cond, branch) in elif_branches {
                    self.scan_expr_for_generic_calls(cond);
                    self.scan_expr_for_generic_calls(branch);
                }
                if let Some(eb) = else_branch {
                    self.scan_expr_for_generic_calls(eb);
                }
            }
            Expr::Match { scrutinee, arms, else_arm } => {
                self.scan_expr_for_generic_calls(scrutinee);
                for arm in arms {
                    self.scan_expr_for_generic_calls(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.scan_expr_for_generic_calls(guard);
                    }
                }
                if let Some(eb) = else_arm {
                    self.scan_expr_for_generic_calls(eb);
                }
            }
            Expr::Block(block) | Expr::Do { body: block } => {
                self.scan_block_for_generic_calls(block);
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                self.scan_expr_for_generic_calls(body);
            }
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                self.scan_expr_for_generic_calls(object);
            }
            Expr::Index { object, index } => {
                self.scan_expr_for_generic_calls(object);
                self.scan_expr_for_generic_calls(index);
            }
            Expr::NilCoalescing { lhs, rhs } => {
                self.scan_expr_for_generic_calls(lhs);
                self.scan_expr_for_generic_calls(rhs);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { self.scan_expr_for_generic_calls(s); }
                if let Some(e) = end { self.scan_expr_for_generic_calls(e); }
            }
            Expr::OptionalChain { object, .. } => {
                self.scan_expr_for_generic_calls(object);
            }
            Expr::Try { expr } | Expr::Move { expr } | Expr::MutableBorrow { expr }
            | Expr::Deref { expr } | Expr::Await { expr } | Expr::Spawn { expr }
            | Expr::TryCapture { expr } => {
                self.scan_expr_for_generic_calls(expr);
            }
            Expr::As { expr, .. } | Expr::Is { expr, .. } => {
                self.scan_expr_for_generic_calls(expr);
            }
            Expr::ListComprehension { expr, iterable, condition, .. } => {
                self.scan_expr_for_generic_calls(expr);
                self.scan_expr_for_generic_calls(iterable);
                if let Some(c) = condition { self.scan_expr_for_generic_calls(c); }
            }
            Expr::DictComprehension { key, value, iterable, condition, .. } => {
                self.scan_expr_for_generic_calls(key);
                self.scan_expr_for_generic_calls(value);
                self.scan_expr_for_generic_calls(iterable);
                if let Some(c) = condition { self.scan_expr_for_generic_calls(c); }
            }
            Expr::SetComprehension { expr, iterable, condition, .. } => {
                self.scan_expr_for_generic_calls(expr);
                self.scan_expr_for_generic_calls(iterable);
                if let Some(c) = condition { self.scan_expr_for_generic_calls(c); }
            }
            Expr::ArrayLiteral(exprs) | Expr::TupleLiteral(exprs) => {
                for e in exprs { self.scan_expr_for_generic_calls(e); }
            }
            Expr::StructLiteral { args, .. } => {
                for a in args { self.scan_expr_for_generic_calls(a); }
            }
            Expr::DictLiteral(pairs) => {
                for (k, v) in pairs {
                    self.scan_expr_for_generic_calls(k);
                    self.scan_expr_for_generic_calls(v);
                }
            }
            // Leaf expressions: no sub-expressions
            Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_)
            | Expr::CharLiteral(_) | Expr::StringLiteral(_) | Expr::NoneLiteral
            | Expr::Identifier(_) | Expr::SelfExpr | Expr::Path { .. } | Expr::It => {}
        }
    }

    /// Collect generic struct/enum/function templates from the module.
    pub fn collect_generic_templates(&mut self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            match &item.node {
                Item::Struct(s) if s.generic_params.is_some() => {
                    // Skip runtime-handled generics (their codegen is hardcoded, not monomorphized)
                    if s.name.node != "Channel" {
                        self.generic_struct_templates
                            .insert(s.name.node.clone(), s.clone());
                    }
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

    /// Check if a C type arg is "pointer-safe" — it doesn't need a full user-type
    /// definition, only a forward declaration (or is a primitive/pointer).
    fn is_pointer_safe_type_arg(c_type: &str) -> bool {
        c_type.ends_with('*')
            || c_type.starts_with("int") || c_type.starts_with("uint")
            || c_type == "double" || c_type == "float" || c_type == "bool" || c_type == "char"
            || c_type == "const char*"
            || c_type.starts_with("GorgetArray") || c_type.starts_with("GorgetSet")
            || c_type.starts_with("GorgetDict") || c_type.starts_with("GorgetMap")
            || c_type.starts_with("GorgetString")
            || c_type.starts_with("Tuple_") || c_type.starts_with("GorgetTuple_")
    }

    /// Emit generic Struct, Map, and pointer-safe Enum type definitions (phase 1).
    /// Structs and Maps use pointer-based storage for type args.
    /// Enums whose type args are all pointer-safe (primitives, pointers) also go here
    /// so that user types (e.g., `Expr`) can contain `Option[Box[Expr]]` fields.
    /// Must be called before regular type definitions.
    pub fn emit_generic_type_definitions_phase1(&mut self, emitter: &mut CEmitter) {
        let has_types = self.generic_instances.iter().any(|i| matches!(
            i.kind,
            super::GenericInstanceKind::Struct | super::GenericInstanceKind::Map { .. }
        ) || (matches!(i.kind, super::GenericInstanceKind::Enum)
              && i.c_type_args.iter().all(|a| Self::is_pointer_safe_type_arg(a))));
        if !has_types {
            return;
        }
        emitter.emit_line("// ── Generic Instantiations (Phase 1) ──");
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
                super::GenericInstanceKind::Map { ordered } => {
                    self.emit_map_struct_def(&inst.c_type_args, &inst.mangled_name, ordered, emitter);
                }
                super::GenericInstanceKind::Enum if inst.c_type_args.iter().all(|a| Self::is_pointer_safe_type_arg(a)) => {
                    let template = self.generic_enum_templates.get(&inst.base_name).cloned();
                    if let Some(template) = template {
                        self.emit_monomorphized_enum(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                _ => {}
            }
        }
        emitter.blank_line();
    }

    /// Emit generic Enum type definitions (phase 2).
    /// Generic enums whose type args contain user-defined value types need those
    /// types fully defined first. Enums already emitted in phase 1 (pointer-safe
    /// args) or during the struct topo sort (interleaved) are skipped.
    /// Must be called after regular type definitions.
    pub fn emit_generic_type_definitions_phase2(&mut self, emitter: &mut CEmitter) {
        let has_enums = self.generic_instances.iter().any(|i|
            matches!(i.kind, super::GenericInstanceKind::Enum)
            && !i.c_type_args.iter().all(|a| Self::is_pointer_safe_type_arg(a))
            && !self.emitted_in_type_defs.contains(&i.mangled_name)
        );
        if !has_enums {
            return;
        }
        emitter.emit_line("// ── Generic Instantiations (Phase 2) ──");
        for i in 0..self.generic_instances.len() {
            let inst = self.generic_instances[i].clone();
            if let super::GenericInstanceKind::Enum = inst.kind {
                // Skip enums already emitted in phase 1 or during struct topo sort
                if inst.c_type_args.iter().all(|a| Self::is_pointer_safe_type_arg(a)) {
                    continue;
                }
                if self.emitted_in_type_defs.contains(&inst.mangled_name) {
                    continue;
                }
                let template = self.generic_enum_templates.get(&inst.base_name).cloned();
                if let Some(template) = template {
                    self.emit_monomorphized_enum(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                }
            }
        }
        emitter.blank_line();
    }

    /// Emit forward declarations (prototypes only) for all generic method and
    /// function instantiations.  Called in the declarations section, before
    /// vtable instances and before any function definitions, so that generic
    /// equip methods can call non-generic functions (and vice-versa) without
    /// ordering issues.
    pub fn emit_generic_method_declarations(&self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        let mut trait_defs: HashMap<String, &TraitDef> = HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }
        emitter.emit_line("// ── Generic Method Declarations ──");
        for i in 0..self.generic_instances.len() {
            let inst = &self.generic_instances[i];
            match &inst.kind {
                super::GenericInstanceKind::Struct | super::GenericInstanceKind::Enum => {
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
                        let trait_name = self.impl_trait_name(equip_block);
                        // Compute trait type args with substitution for generic params
                        let subs = self.build_type_substitutions(generic_params.as_ref(), &inst.c_type_args);
                        let raw_trait_args = Self::impl_trait_type_args_raw(equip_block);
                        let trait_type_args: Vec<String> = raw_trait_args.iter()
                            .map(|ty| self.substitute_type(ty, &subs))
                            .collect();
                        // Prototypes for explicitly implemented methods
                        for method in &equip_block.items {
                            let (ret_type, func_name, params, _) = self.monomorphized_equip_signature(
                                &method.node, generic_params.as_ref(), &inst.c_type_args,
                                &inst.mangled_name, trait_name.as_deref(), &trait_type_args,
                            );
                            emitter.emit_line(&format!("{ret_type} {func_name}({params});"));
                        }
                        // Prototypes for default/inherited methods not overridden
                        if let Some(tname) = &trait_name {
                            if let Some(trait_def) = trait_defs.get(tname.as_str()) {
                                let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);
                                for (method, _) in &all_methods {
                                    if !Self::equip_has_method(equip_block, &method.name.node) {
                                        if !matches!(method.body, FunctionBody::Declaration | FunctionBody::Extern(_)) {
                                            let (ret_type, func_name, params, _) = self.monomorphized_equip_signature(
                                                method, generic_params.as_ref(), &inst.c_type_args,
                                                &inst.mangled_name, Some(tname), &trait_type_args,
                                            );
                                            emitter.emit_line(&format!("{ret_type} {func_name}({params});"));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                super::GenericInstanceKind::Function => {
                    let template = self.generic_fn_templates.get(&inst.base_name);
                    if let Some(template) = template {
                        let (ret_type, params, _) = self.monomorphized_function_signature(template, &inst.c_type_args);
                        emitter.emit_line(&format!("{ret_type} {}({params});", inst.mangled_name));
                    }
                }
                super::GenericInstanceKind::Map { .. } => {
                    // Map functions use `static inline` — no forward declarations needed.
                }
            }
        }
        emitter.blank_line();
    }

    /// Emit method definitions for generic instantiations (equip blocks + generic functions).
    /// Must be called after all declarations so that method bodies can reference
    /// any function in the program.
    pub fn emit_generic_method_definitions(&mut self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        // Collect trait definitions for default method lookup
        let mut trait_defs: HashMap<String, &TraitDef> = HashMap::new();
        for item in &module.items {
            if let Item::Trait(t) = &item.node {
                trait_defs.insert(t.name.node.clone(), t);
            }
        }
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
                        // Compute trait type args with substitution for generic params
                        let subs = self.build_type_substitutions(generic_params.as_ref(), &inst.c_type_args);
                        let raw_trait_args = Self::impl_trait_type_args_raw(&equip_block);
                        let trait_type_args: Vec<String> = raw_trait_args.iter()
                            .map(|ty| self.substitute_type(ty, &subs))
                            .collect();
                        // Emit explicitly implemented methods
                        for method in &equip_block.items {
                            self.emit_monomorphized_equip_method(
                                &method.node,
                                generic_params.as_ref(),
                                &inst.c_type_args,
                                &inst.mangled_name,
                                trait_name.as_deref(),
                                &trait_type_args,
                                emitter,
                            );
                        }
                        // Emit default/inherited method bodies not overridden
                        if let Some(tname) = &trait_name {
                            if let Some(trait_def) = trait_defs.get(tname.as_str()) {
                                let all_methods = self.collect_all_trait_methods(trait_def, &trait_defs);
                                for (method, _) in &all_methods {
                                    if !Self::equip_has_method(&equip_block, &method.name.node) {
                                        if !matches!(method.body, FunctionBody::Declaration | FunctionBody::Extern(_)) {
                                            self.emit_monomorphized_equip_method(
                                                method,
                                                generic_params.as_ref(),
                                                &inst.c_type_args,
                                                &inst.mangled_name,
                                                Some(tname),
                                                &trait_type_args,
                                                emitter,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                super::GenericInstanceKind::Function => {
                    let template = self.generic_fn_templates.get(&inst.base_name).cloned();
                    if let Some(template) = template {
                        self.emit_monomorphized_function(&template, &inst.c_type_args, &inst.mangled_name, emitter);
                    }
                }
                super::GenericInstanceKind::Map { ordered } => {
                    self.emit_map_functions(&inst.c_type_args, &inst.mangled_name, ordered, emitter);
                }
            }
        }
    }

    /// Emit only the struct definition for a monomorphized map (Dict or HashMap).
    /// Called in phase 1 (type definitions), before user-defined types that may
    /// reference this map are fully defined.
    fn emit_map_struct_def(
        &self,
        c_type_args: &[String],
        mangled: &str,
        ordered: bool,
        emitter: &mut CEmitter,
    ) {
        let key_type = c_type_args.first().map(|s| s.as_str()).unwrap_or("int64_t");
        let val_type = c_type_args.get(1).map(|s| s.as_str()).unwrap_or("int64_t");

        if ordered {
            emitter.emit(&format!(
                "typedef struct {mangled} {mangled};\n\
                 struct {mangled} {{\n\
                 \x20   {key_type}* keys;\n\
                 \x20   {val_type}* values;\n\
                 \x20   uint8_t* states;\n\
                 \x20   size_t count;\n\
                 \x20   size_t cap;\n\
                 \x20   size_t* order;\n\
                 \x20   size_t order_len;\n\
                 \x20   size_t tombstones;\n\
                 \x20   GorgetAllocator* alloc;\n\
                 }};\n\n"
            ));
        } else {
            emitter.emit(&format!(
                "typedef struct {mangled} {mangled};\n\
                 struct {mangled} {{\n\
                 \x20   {key_type}* keys;\n\
                 \x20   {val_type}* values;\n\
                 \x20   uint8_t* states;\n\
                 \x20   size_t count;\n\
                 \x20   size_t cap;\n\
                 \x20   GorgetAllocator* alloc;\n\
                 }};\n\n"
            ));
        }
    }

    /// Emit inline functions for a monomorphized map (Dict or HashMap).
    /// Called in phase 2 (method definitions), after all type definitions are
    /// complete so that `sizeof(ValueType)` is valid even for recursive types.
    fn emit_map_functions(
        &self,
        c_type_args: &[String],
        mangled: &str,
        ordered: bool,
        emitter: &mut CEmitter,
    ) {
        let key_type = c_type_args.first().map(|s| s.as_str()).unwrap_or("int64_t");
        let val_type = c_type_args.get(1).map(|s| s.as_str()).unwrap_or("int64_t");
        let is_str_key = key_type == "const char*" || key_type == "Str";

        let hash_expr = |var: &str| -> String {
            if is_str_key {
                if key_type == "Str" {
                    format!("__gorget_hash_str_len({var}.data, {var}.len)")
                } else {
                    format!("__gorget_hash_str({var})")
                }
            } else {
                format!("__gorget_fnv1a(&{var}, sizeof({key_type}))")
            }
        };
        let eq_expr = |a: &str, b: &str| -> String {
            if is_str_key {
                if key_type == "Str" {
                    format!("gorget_str_eq({a}, {b})")
                } else {
                    format!("strcmp({a}, {b}) == 0")
                }
            } else {
                format!("memcmp(&{a}, &{b}, sizeof({key_type})) == 0")
            }
        };

        let hash_key = hash_expr("key");
        let hash_old = hash_expr("old_keys[i]");
        let eq_put = eq_expr("m->keys[idx]", "key");
        let eq_get = eq_expr("m->keys[idx]", "key");

        // ── __grow ──
        if ordered {
            emitter.emit(&format!(r#"static inline void {mangled}__grow({mangled}* m) {{
    GorgetAllocator* __a = m->alloc;
    size_t old_cap = m->cap;
    {key_type}* old_keys = m->keys;
    {val_type}* old_values = m->values;
    uint8_t* old_states = m->states;
    size_t* old_order = m->order;
    size_t old_order_len = m->order_len;
    size_t new_cap = old_cap == 0 ? 16 : old_cap * 2;
    m->keys = ({key_type}*)GORGET_CALLOC(new_cap, sizeof({key_type}));
    m->values = ({val_type}*)GORGET_CALLOC(new_cap, sizeof({val_type}));
    m->states = (uint8_t*)GORGET_CALLOC(new_cap, 1);
    m->order = (size_t*)GORGET_CALLOC(new_cap, sizeof(size_t));
    m->cap = new_cap;
    m->count = 0;
    m->order_len = 0;
    m->tombstones = 0;
    for (size_t oi = 0; oi < old_order_len; oi++) {{
        size_t i = old_order[oi];
        if (old_states[i] != 1) continue;
        uint64_t h = {hash_old};
        size_t idx = (size_t)(h % new_cap);
        while (m->states[idx] != 0) {{ idx = (idx + 1) % new_cap; }}
        m->keys[idx] = old_keys[i];
        m->values[idx] = old_values[i];
        m->states[idx] = 1;
        m->order[m->order_len++] = idx;
        m->count++;
    }}
    __a->dealloc(__a->ctx, old_keys, old_cap * sizeof({key_type}));
    __a->dealloc(__a->ctx, old_values, old_cap * sizeof({val_type}));
    __a->dealloc(__a->ctx, old_states, old_cap);
    __a->dealloc(__a->ctx, old_order, old_cap * sizeof(size_t));
}}

"#));
        } else {
            emitter.emit(&format!(r#"static inline void {mangled}__grow({mangled}* m) {{
    GorgetAllocator* __a = m->alloc;
    size_t old_cap = m->cap;
    {key_type}* old_keys = m->keys;
    {val_type}* old_values = m->values;
    uint8_t* old_states = m->states;
    size_t new_cap = old_cap == 0 ? 16 : old_cap * 2;
    m->keys = ({key_type}*)GORGET_CALLOC(new_cap, sizeof({key_type}));
    m->values = ({val_type}*)GORGET_CALLOC(new_cap, sizeof({val_type}));
    m->states = (uint8_t*)GORGET_CALLOC(new_cap, 1);
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
    __a->dealloc(__a->ctx, old_keys, old_cap * sizeof({key_type}));
    __a->dealloc(__a->ctx, old_values, old_cap * sizeof({val_type}));
    __a->dealloc(__a->ctx, old_states, old_cap);
}}

"#));
        }

        // ── __new ──
        if ordered {
            emitter.emit(&format!(
                "static inline {mangled} {mangled}__new(void) {{\n\
                 \x20   return ({mangled}){{NULL, NULL, NULL, 0, 0, NULL, 0, 0, __gorget_current_alloc}};\n\
                 }}\n\n"
            ));
        } else {
            emitter.emit(&format!(
                "static inline {mangled} {mangled}__new(void) {{\n\
                 \x20   return ({mangled}){{NULL, NULL, NULL, 0, 0, __gorget_current_alloc}};\n\
                 }}\n\n"
            ));
        }

        // ── __put ──
        // Ordered mode: new keys always go into empty slots (never reuse tombstones).
        // This ensures stale order-array entries pointing to tombstoned slots are
        // correctly skipped during iteration, preventing double-reporting of keys.
        if ordered {
            emitter.emit(&format!(r#"static inline void {mangled}__put({mangled}* m, {key_type} key, {val_type} value) {{
    if (m->cap == 0 || (m->count + m->tombstones) * 4 >= m->cap * 3) {{ {mangled}__grow(m); }}
    uint64_t h = {hash_key};
    size_t idx = (size_t)(h % m->cap);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {{
        if (m->states[idx] == 0) {{
            m->keys[idx] = key;
            m->values[idx] = value;
            m->states[idx] = 1;
            m->count++;
            m->order[m->order_len++] = idx;
            return;
        }}
        if (m->states[idx] == 1 && {eq_put}) {{
            m->values[idx] = value;
            return;
        }}
        idx = (idx + 1) % m->cap;
    }}
}}

"#));
        } else {
            emitter.emit(&format!(r#"static inline void {mangled}__put({mangled}* m, {key_type} key, {val_type} value) {{
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

"#));
        }

        // ── __get_ptr, __contains (identical for both) ──
        emitter.emit(&format!(r#"static inline {val_type}* {mangled}__get_ptr({mangled}* m, {key_type} key) {{
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

"#));

        // ── __remove ──
        if ordered {
            emitter.emit(&format!(r#"static inline bool {mangled}__remove({mangled}* m, {key_type} key) {{
    if (m->cap == 0) return false;
    uint64_t h = {hash_key};
    size_t idx = (size_t)(h % m->cap);
    for (size_t __probes = 0; __probes < m->cap; __probes++) {{
        if (m->states[idx] == 0) return false;
        if (m->states[idx] == 1 && {eq_get}) {{
            m->states[idx] = 2;
            m->count--;
            m->tombstones++;
            return true;
        }}
        idx = (idx + 1) % m->cap;
    }}
    return false;
}}

"#));
        } else {
            emitter.emit(&format!(r#"static inline bool {mangled}__remove({mangled}* m, {key_type} key) {{
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

"#));
        }

        // ── __clear ──
        if ordered {
            emitter.emit(&format!(
                "static inline void {mangled}__clear({mangled}* m) {{\n\
                 \x20   if (m->states) memset(m->states, 0, m->cap);\n\
                 \x20   m->count = 0;\n\
                 \x20   m->order_len = 0;\n\
                 \x20   m->tombstones = 0;\n\
                 }}\n\n"
            ));
        } else {
            emitter.emit(&format!(
                "static inline void {mangled}__clear({mangled}* m) {{\n\
                 \x20   if (m->states) memset(m->states, 0, m->cap);\n\
                 \x20   m->count = 0;\n\
                 }}\n\n"
            ));
        }

        // ── __free ──
        if ordered {
            emitter.emit(&format!(
                "static inline void {mangled}__free({mangled}* m) {{\n\
                 \x20   GorgetAllocator* __a = m->alloc;\n\
                 \x20   if (m->keys) __a->dealloc(__a->ctx, m->keys, m->cap * sizeof({key_type}));\n\
                 \x20   if (m->values) __a->dealloc(__a->ctx, m->values, m->cap * sizeof({val_type}));\n\
                 \x20   if (m->states) __a->dealloc(__a->ctx, m->states, m->cap);\n\
                 \x20   if (m->order) __a->dealloc(__a->ctx, m->order, m->cap * sizeof(size_t));\n\
                 \x20   m->keys = NULL; m->values = NULL; m->states = NULL; m->order = NULL;\n\
                 \x20   m->count = 0; m->cap = 0; m->order_len = 0; m->tombstones = 0;\n\
                 }}\n\n"
            ));
        } else {
            emitter.emit(&format!(
                "static inline void {mangled}__free({mangled}* m) {{\n\
                 \x20   GorgetAllocator* __a = m->alloc;\n\
                 \x20   if (m->keys) __a->dealloc(__a->ctx, m->keys, m->cap * sizeof({key_type}));\n\
                 \x20   if (m->values) __a->dealloc(__a->ctx, m->values, m->cap * sizeof({val_type}));\n\
                 \x20   if (m->states) __a->dealloc(__a->ctx, m->states, m->cap);\n\
                 \x20   m->keys = NULL; m->values = NULL; m->states = NULL;\n\
                 \x20   m->count = 0; m->cap = 0;\n\
                 }}\n\n"
            ));
        }

        // ── __clone ──
        if ordered {
            emitter.emit(&format!(
                "static inline {mangled} {mangled}__clone(const {mangled}* src) {{\n\
                 \x20   GorgetAllocator* __a = __gorget_current_alloc;\n\
                 \x20   {mangled} dst;\n\
                 \x20   dst.alloc = __a;\n\
                 \x20   dst.count = src->count; dst.cap = src->cap;\n\
                 \x20   dst.order_len = src->order_len; dst.tombstones = src->tombstones;\n\
                 \x20   if (src->cap == 0) {{\n\
                 \x20       dst.keys = NULL; dst.values = NULL; dst.states = NULL; dst.order = NULL;\n\
                 \x20       return dst;\n\
                 \x20   }}\n\
                 \x20   dst.keys = ({key_type}*)__a->alloc(__a->ctx, src->cap * sizeof({key_type}));\n\
                 \x20   memcpy(dst.keys, src->keys, src->cap * sizeof({key_type}));\n\
                 \x20   dst.values = ({val_type}*)__a->alloc(__a->ctx, src->cap * sizeof({val_type}));\n\
                 \x20   memcpy(dst.values, src->values, src->cap * sizeof({val_type}));\n\
                 \x20   dst.states = (uint8_t*)__a->alloc(__a->ctx, src->cap);\n\
                 \x20   memcpy(dst.states, src->states, src->cap);\n\
                 \x20   dst.order = (size_t*)__a->alloc(__a->ctx, src->cap * sizeof(size_t));\n\
                 \x20   memcpy(dst.order, src->order, src->cap * sizeof(size_t));\n\
                 \x20   return dst;\n\
                 }}\n\n"
            ));
        } else {
            emitter.emit(&format!(
                "static inline {mangled} {mangled}__clone(const {mangled}* src) {{\n\
                 \x20   GorgetAllocator* __a = __gorget_current_alloc;\n\
                 \x20   {mangled} dst;\n\
                 \x20   dst.alloc = __a;\n\
                 \x20   dst.count = src->count; dst.cap = src->cap;\n\
                 \x20   if (src->cap == 0) {{\n\
                 \x20       dst.keys = NULL; dst.values = NULL; dst.states = NULL;\n\
                 \x20       return dst;\n\
                 \x20   }}\n\
                 \x20   dst.keys = ({key_type}*)__a->alloc(__a->ctx, src->cap * sizeof({key_type}));\n\
                 \x20   memcpy(dst.keys, src->keys, src->cap * sizeof({key_type}));\n\
                 \x20   dst.values = ({val_type}*)__a->alloc(__a->ctx, src->cap * sizeof({val_type}));\n\
                 \x20   memcpy(dst.values, src->values, src->cap * sizeof({val_type}));\n\
                 \x20   dst.states = (uint8_t*)__a->alloc(__a->ctx, src->cap);\n\
                 \x20   memcpy(dst.states, src->states, src->cap);\n\
                 \x20   return dst;\n\
                 }}\n\n"
            ));
        }
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
    pub fn emit_monomorphized_enum(
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

    /// Compute the C signature for a monomorphized generic function without emitting anything.
    /// Returns `(ret_type, params, type_subs)`.
    fn monomorphized_function_signature(
        &self,
        template: &FunctionDef,
        c_type_args: &[String],
    ) -> (String, String, Vec<(String, String)>) {
        let subs = self.build_type_substitutions(template.generic_params.as_ref(), c_type_args);

        let ret_type = self.substitute_type(&template.return_type.node, &subs);

        let mut params_vec: Vec<String> = Vec::new();
        for param in &template.params {
            if param.node.name.node == "self" {
                continue;
            }
            // Callable/MutCallable/ConsumeCallable-typed params use GorgetClosure
            // (not the raw function pointer produced by substitute_type)
            let is_callable_param = matches!(&param.node.type_.node,
                Type::Named { name, .. } if matches!(name.node.as_str(), "Callable" | "MutCallable" | "ConsumeCallable")
            );
            let param_type = if is_callable_param {
                "GorgetClosure".to_string()
            } else {
                self.substitute_type(&param.node.type_.node, &subs)
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

        (ret_type, params, subs)
    }

    /// Emit a monomorphized function definition.
    fn emit_monomorphized_function(
        &mut self,
        template: &FunctionDef,
        c_type_args: &[String],
        mangled: &str,
        emitter: &mut CEmitter,
    ) {
        let (ret_type, params, subs) = self.monomorphized_function_signature(template, c_type_args);
        let id_subs = self.build_type_id_substitutions(template.generic_params.as_ref(), c_type_args);

        // Build param_c_types: substituted C type for each function parameter.
        // Used as fallback in body codegen when TypeId-based resolution fails.
        let param_c_types: Vec<(String, String)> = template.params.iter()
            .filter(|p| p.node.name.node != "self")
            .map(|p| {
                let name = super::c_mangle::escape_keyword(&p.node.name.node);
                let c_type = self.substitute_type(&p.node.type_.node, &subs);
                (name, c_type)
            })
            .collect();

        // Activate type substitutions so that body codegen sees T → concrete type
        let prev_subs = std::mem::replace(&mut self.type_subs, subs.clone());
        let prev_id_subs = std::mem::replace(&mut self.type_id_subs, id_subs);
        let prev_param_c_types = std::mem::replace(&mut self.monomorphized_param_c_types, param_c_types);

        // Track mutable borrow params as pointer params and register Callable-typed params
        let prev_pointer_params = std::mem::take(&mut self.pointer_params);
        let prev_closure_vars = self.closure_vars.clone();
        let prev_fn_type_sigs = self.fn_type_signatures.clone();
        for param in &template.params {
            if param.node.name.node == "self" {
                continue;
            }
            if matches!(param.node.ownership, Ownership::MutableBorrow) {
                self.pointer_params
                    .insert(c_mangle::escape_keyword(&param.node.name.node));
            }
            // Detect Callable/MutCallable/ConsumeCallable-typed params — register for closure dispatch
            if let Type::Named { name, generic_args } = &param.node.type_.node {
                let callable_kind = match name.node.as_str() {
                    "Callable" => Some(super::CallableKind::Callable),
                    "MutCallable" => Some(super::CallableKind::MutCallable),
                    "ConsumeCallable" => Some(super::CallableKind::ConsumeCallable),
                    _ => None,
                };
                if let Some(kind) = callable_kind {
                    if generic_args.len() == 1 {
                        let escaped = c_mangle::escape_keyword(&param.node.name.node);
                        self.closure_vars.insert(escaped.clone());
                        // Extract signature from the function type generic arg, with type substitutions applied
                        if let Type::Function { return_type, params: fn_params, param_ownerships } = &generic_args[0].node {
                            let ret_c = self.substitute_type(&return_type.node, &subs);
                            let param_c: Vec<String> = fn_params.iter().enumerate()
                                .map(|(i, p)| {
                                    let base = self.substitute_type(&p.node, &subs);
                                    let ownership = param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
                                    if matches!(ownership, Ownership::MutableBorrow) {
                                        format!("{base}*")
                                    } else {
                                        base
                                    }
                                })
                                .collect();
                            self.fn_type_signatures.insert(escaped.clone(), (param_c.clone(), ret_c.clone(), param_ownerships.clone()));
                            // Register the signature for vtable generation
                            let sig = (kind, param_c, ret_c);
                            if !self.fn_trait_sigs.contains(&sig) {
                                self.fn_trait_sigs.push(sig);
                            }
                        }
                    }
                }
            }
        }

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
            FunctionBody::Declaration | FunctionBody::Extern(_) => {
                // External declaration — no body
            }
        }

        // Restore previous substitutions and state
        self.type_subs = prev_subs;
        self.type_id_subs = prev_id_subs;
        self.monomorphized_param_c_types = prev_param_c_types;
        self.pointer_params = prev_pointer_params;
        self.closure_vars = prev_closure_vars;
        self.fn_type_signatures = prev_fn_type_sigs;
    }

    /// Build a substitution map from generic param names to concrete C types.
    pub(super) fn build_type_substitutions(
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

    /// Build a parallel substitution map from generic param names to TypeIds.
    /// Returns `None` entries for mangled/complex C types that can't be reverse-mapped.
    fn build_type_id_substitutions(
        &self,
        generic_params: Option<&crate::span::Spanned<crate::parser::ast::GenericParams>>,
        c_type_args: &[String],
    ) -> Vec<(String, crate::semantic::ids::TypeId)> {
        let mut subs = Vec::new();
        if let Some(params) = generic_params {
            for (i, param) in params.node.params.iter().enumerate() {
                if let crate::parser::ast::GenericParam::Type(name) = &param.node {
                    if let Some(c_type) = c_type_args.get(i) {
                        if let Some(tid) = self.resolve_c_type_to_type_id(c_type) {
                            subs.push((name.node.clone(), tid));
                        }
                    }
                }
            }
        }
        subs
    }

    /// Reverse-map a C type string to its semantic TypeId.
    /// Returns `None` for mangled generic names or unknown types.
    fn resolve_c_type_to_type_id(&self, c_type: &str) -> Option<crate::semantic::ids::TypeId> {
        // Primitives
        match c_type {
            "int64_t" => return Some(self.types.int_id),
            "double" => return Some(self.types.float_id),
            "bool" => return Some(self.types.bool_id),
            "char" => return Some(self.types.char_id),
            "const char*" => return Some(self.types.string_id),
            "GorgetString" => return Some(self.types.owned_string_id),
            "void" => return Some(self.types.void_id),
            "int8_t" | "int16_t" | "int32_t" | "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t" | "float" => {
                // Less common primitives — fall through to named lookup
            }
            _ => {}
        }
        // Named types (struct, enum, newtype): look up by name in scope
        // Skip mangled names (contain "__") — these are complex generic instantiations
        if !c_type.contains("__") && !c_type.contains('*') && !c_type.contains(' ') {
            if let Some(def_id) = self.scopes.lookup(c_type) {
                let def = self.scopes.get_def(def_id);
                if let Some(tid) = def.type_id {
                    return Some(tid);
                }
            }
        }
        None
    }

    /// Substitute type parameters in an AST Type, returning a C type string.
    pub(super) fn substitute_type(
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
                    "Box" if c_args.len() == 1 => format!("{}*", c_args[0]),
                    "Vector" | "List" | "Array" => "GorgetArray".to_string(),
                    "Set" => "GorgetSet".to_string(),
                    "Dict" => c_mangle::mangle_generic("GorgetDict", &c_args),
                    "HashMap" => c_mangle::mangle_generic("GorgetMap", &c_args),
                    // Callable[sig] → function pointer type (substituted sig is already a C fn ptr)
                    "Callable" | "MutCallable" | "ConsumeCallable" if c_args.len() == 1 => {
                        c_args[0].clone()
                    }
                    _ => c_mangle::mangle_generic(&name.node, &c_args),
                }
            }
            crate::parser::ast::Type::Function { return_type, params, param_ownerships } => {
                // Substitute recursively through function type (needed for generic callable params)
                let ret = self.substitute_type(&return_type.node, subs);
                let param_types: Vec<String> = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        let base = self.substitute_type(&p.node, subs);
                        let ownership = param_ownerships.get(i).copied()
                            .unwrap_or(crate::parser::ast::Ownership::Borrow);
                        if ownership == crate::parser::ast::Ownership::MutableBorrow {
                            format!("{base}*")
                        } else {
                            base
                        }
                    })
                    .collect();
                let params_str = if param_types.is_empty() {
                    "void".to_string()
                } else {
                    param_types.join(", ")
                };
                format!("{ret} (*)({params_str})")
            }
            // Type::Ref removed
            _ => c_types::ast_type_to_c(ty, self.scopes),
        }
    }

    // ─── Tuple Typedefs ──────────────────────────────────────

    // ─── Async/Await State Machine Codegen ────────────────────

    /// Pre-scan the module for async functions and register Future[T] types.
    pub fn discover_future_types(&mut self, module: &crate::parser::ast::Module) {
        for item in &module.items {
            if let Item::Function(f) = &item.node {
                if f.qualifiers.is_async {
                    let inner_c = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
                    let mangled = c_mangle::mangle_generic("Future", &[inner_c.clone()]);
                    self.future_types.entry(mangled).or_insert(inner_c);
                }
            }
        }
    }

    /// Emit typedefs for all registered Future[T] types.
    pub fn emit_future_typedefs(&self, emitter: &mut CEmitter) {
        if self.future_types.is_empty() {
            return;
        }
        emitter.emit_line("// ── Future Types ──");
        for (mangled, inner_c_type) in &self.future_types {
            emitter.emit_line(&format!("typedef struct {mangled} {mangled};"));
            emitter.emit_line(&format!("struct {mangled} {{"));
            emitter.indent();
            emitter.emit_line(&format!("int (*poll)({mangled}*, GorgetWaker*);"));
            emitter.emit_line("void* state;");
            if inner_c_type != "void" {
                emitter.emit_line(&format!("{inner_c_type} result;"));
            }
            emitter.dedent();
            emitter.emit_line("};");
            emitter.blank_line();
        }
    }

    /// Emit Task[T] typedefs, SpawnCtx structs, and worker functions for each Future[T].
    /// Only emitted when `has_spawn` is true.
    pub fn emit_task_typedefs(&self, emitter: &mut CEmitter) {
        if !self.has_spawn || self.future_types.is_empty() {
            return;
        }
        emitter.emit_line("// ── Task Types (Spawn Infrastructure) ──");
        for (future_mangled, inner_c_type) in &self.future_types {
            // future_mangled is e.g. "Future__int64_t", suffix is "int64_t"
            let suffix = &future_mangled["Future__".len()..];
            let ctx_name = format!("__SpawnCtx__{suffix}");
            let task_name = format!("Task__{suffix}");

            // SpawnCtx: GorgetTask base + embedded Future
            emitter.emit_line(&format!("typedef struct {ctx_name} {{"));
            emitter.indent();
            emitter.emit_line("GorgetTask base;");
            emitter.emit_line(&format!("{future_mangled} future;"));
            emitter.dedent();
            emitter.emit_line(&format!("}} {ctx_name};"));
            emitter.blank_line();

            // Worker function: polls future to completion, signals done
            emitter.emit_line(&format!("static void {ctx_name}__run(GorgetTask* __base) {{"));
            emitter.indent();
            emitter.emit_line(&format!("{ctx_name}* __ctx = ({ctx_name}*)__base;"));
            emitter.emit_line("__GorgetWorkerWakerCtx __wwctx;");
            emitter.emit_line("pthread_mutex_init(&__wwctx.mtx, NULL);");
            emitter.emit_line("pthread_cond_init(&__wwctx.cond, NULL);");
            emitter.emit_line("__wwctx.woken = 1;");
            emitter.emit_line("GorgetWaker __ww = { __gorget_worker_waker_wake, &__wwctx };");
            emitter.emit_line("while (__ctx->future.poll(&__ctx->future, &__ww) != GORGET_POLL_READY) {");
            emitter.indent();
            emitter.emit_line("pthread_mutex_lock(&__wwctx.mtx);");
            emitter.emit_line("while (!__wwctx.woken) pthread_cond_wait(&__wwctx.cond, &__wwctx.mtx);");
            emitter.emit_line("__wwctx.woken = 0;");
            emitter.emit_line("pthread_mutex_unlock(&__wwctx.mtx);");
            emitter.dedent();
            emitter.emit_line("}");
            emitter.emit_line("pthread_mutex_destroy(&__wwctx.mtx);");
            emitter.emit_line("pthread_cond_destroy(&__wwctx.cond);");
            emitter.emit_line("pthread_mutex_lock(&__base->mtx);");
            emitter.emit_line("__base->done = 1;");
            emitter.emit_line("GorgetWaker __pw = __base->parent_waker;");
            emitter.emit_line("pthread_mutex_unlock(&__base->mtx);");
            emitter.emit_line("pthread_cond_broadcast(&__base->cond);");
            emitter.emit_line("if (__pw.wake) __pw.wake(&__pw);");
            emitter.dedent();
            emitter.emit_line("}");
            emitter.blank_line();

            // Task handle typedef
            emitter.emit_line(&format!("typedef struct {{"));
            emitter.indent();
            emitter.emit_line("GorgetTask* _task;");
            if inner_c_type != "void" {
                // Not stored here — result lives in SpawnCtx.future.result
            }
            emitter.dedent();
            emitter.emit_line(&format!("}} {task_name};"));
            emitter.blank_line();
        }
    }

    /// Emit the hand-written C runtime for `async sleep()`.
    /// Must be called after `emit_future_typedefs()` since it references `Future__void`.
    pub fn emit_sleep_runtime(&self, emitter: &mut CEmitter) {
        if !self.has_async_sleep { return; }
        emitter.emit_line("// ── Async Sleep Runtime ──");
        // State struct
        emitter.emit_line("typedef struct {");
        emitter.indent();
        emitter.emit_line("double seconds;");
        emitter.emit_line("int started;");
        emitter.emit_line("GorgetWaker waker;");
        emitter.emit_line("volatile int done;");
        emitter.dedent();
        emitter.emit_line("} __GorgetSleepState;");
        emitter.blank_line();

        // Background thread function
        emitter.emit_line("static void* __gorget_sleep_thread(void* arg) {");
        emitter.indent();
        emitter.emit_line("__GorgetSleepState* __s = (__GorgetSleepState*)arg;");
        emitter.emit_line("struct timespec __ts;");
        emitter.emit_line("__ts.tv_sec = (time_t)__s->seconds;");
        emitter.emit_line("__ts.tv_nsec = (long)((__s->seconds - (double)__ts.tv_sec) * 1e9);");
        emitter.emit_line("nanosleep(&__ts, NULL);");
        emitter.emit_line("__s->done = 1;");
        emitter.emit_line("GorgetWaker __w = __s->waker;");
        emitter.emit_line("if (__w.wake) __w.wake(&__w);");
        emitter.emit_line("return NULL;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();

        // Poll function
        emitter.emit_line("static int __gorget_sleep_poll(Future__void* __future, GorgetWaker* __waker) {");
        emitter.indent();
        emitter.emit_line("__GorgetSleepState* __s = (__GorgetSleepState*)__future->state;");
        emitter.emit_line("if (__s->done) {");
        emitter.indent();
        emitter.emit_line("GORGET_FREE(__s, sizeof(__GorgetSleepState));");
        emitter.emit_line("__future->state = NULL;");
        emitter.emit_line("return GORGET_POLL_READY;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.emit_line("if (!__s->started) {");
        emitter.indent();
        emitter.emit_line("__s->started = 1;");
        emitter.emit_line("__s->waker = *__waker;");
        emitter.emit_line("pthread_t __th;");
        emitter.emit_line("pthread_create(&__th, NULL, __gorget_sleep_thread, __s);");
        emitter.emit_line("pthread_detach(__th);");
        emitter.dedent();
        emitter.emit_line("} else {");
        emitter.indent();
        emitter.emit_line("__s->waker = *__waker;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.emit_line("return GORGET_POLL_PENDING;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();

        // Constructor
        emitter.emit_line("static Future__void gorget_async_sleep(double seconds) {");
        emitter.indent();
        emitter.emit_line("__GorgetSleepState* __s = (__GorgetSleepState*)GORGET_CALLOC(1, sizeof(__GorgetSleepState));");
        emitter.emit_line("__s->seconds = seconds;");
        emitter.emit_line("return (Future__void){.poll = __gorget_sleep_poll, .state = __s};");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }

    /// Top-level async function handler: analyze → emit state struct → poll → constructor.
    fn emit_async_function(
        &mut self,
        f: &FunctionDef,
        method_info: Option<(&str, Option<&str>, &[String])>,
        is_main: bool,
        emitter: &mut CEmitter,
    ) {
        let inner_c_type = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
        let future_type = c_mangle::mangle_generic("Future", &[inner_c_type.clone()]);

        // Register the future type if not already registered
        self.future_types.entry(future_type.clone()).or_insert(inner_c_type.clone());

        let func_name = if is_main {
            "gg__async_main".to_string()
        } else if let Some((type_name, trait_name, trait_type_args)) = method_info {
            if let Some(tname) = trait_name {
                c_mangle::mangle_trait_method(tname, type_name, &f.name.node, trait_type_args)
            } else {
                c_mangle::mangle_method(type_name, &f.name.node)
            }
        } else {
            c_mangle::escape_function(&f.name.node)
        };

        // Analyze the async body
        let analysis = self.analyze_async_function(f);
        let state_name = format!("__AsyncState_{}", f.name.node);

        // 1. Emit state struct
        self.emit_async_state_struct(&state_name, &analysis, emitter);

        // 2. Emit poll function
        let poll_name = format!("{func_name}__poll");
        self.emit_async_poll_function(
            f, &poll_name, &state_name, &future_type, &inner_c_type, &analysis, emitter,
        );

        // 3. Emit constructor
        self.emit_async_constructor(
            &func_name, &state_name, &future_type, &poll_name, f, emitter,
        );

        // 4. Emit C main wrapper for async main
        if is_main {
            emitter.emit_line("int main(int argc, char** argv) {");
            emitter.indent();
            emitter.emit_line("gorget_init_args(argc, argv);");
            emitter.emit_line(&format!("{future_type} __f = gg__async_main();"));
            if self.has_spawn || self.has_async_sleep {
                // Event-driven: sleep on condvar, woken by worker/sleep thread via waker
                emitter.emit_line("while (__f.poll(&__f, &__gorget_main_waker) != GORGET_POLL_READY) {");
                emitter.indent();
                emitter.emit_line("pthread_mutex_lock(&__gorget_main_mtx);");
                emitter.emit_line("while (!__gorget_main_woken) pthread_cond_wait(&__gorget_main_cond, &__gorget_main_mtx);");
                emitter.emit_line("__gorget_main_woken = 0;");
                emitter.emit_line("pthread_mutex_unlock(&__gorget_main_mtx);");
                emitter.dedent();
                emitter.emit_line("}");
            } else {
                // No spawn or sleep: noop waker, simple busy-poll
                emitter.emit_line("while (__f.poll(&__f, &__gorget_noop_waker) != GORGET_POLL_READY) {}");
            }
            emitter.emit_line("if (__f.state) GORGET_FREE(__f.state, 0);");
            if self.has_spawn {
                emitter.emit_line("__gorget_executor_shutdown();");
            }
            emitter.emit_line("return 0;");
            emitter.dedent();
            emitter.emit_line("}");
            emitter.blank_line();
        }
    }

    /// Analyze an async function body to collect params, locals, and await points.
    fn analyze_async_function(&self, f: &FunctionDef) -> AsyncAnalysis {
        let mut params: Vec<(String, String)> = Vec::new();
        for p in &f.params {
            if p.node.name.node == "self" {
                continue;
            }
            let c_type = c_types::ast_type_to_c(&p.node.type_.node, self.scopes);
            params.push((p.node.name.node.clone(), c_type));
        }

        let inner_c_type = c_types::ast_type_to_c(&f.return_type.node, self.scopes);
        let future_type_name = c_mangle::mangle_generic("Future", &[inner_c_type.clone()]);

        let mut locals: Vec<(String, String, Option<Type>)> = Vec::new();
        let mut await_count = 0;
        let mut sub_futures: Vec<(usize, String)> = Vec::new();

        // Walk the body recursively to collect locals and count awaits
        if let FunctionBody::Block(block) = &f.body {
            self.analyze_async_block(block, &params, &mut locals, &mut await_count, &mut sub_futures, f);
        } else if let FunctionBody::Expression(expr) = &f.body {
            if Self::expr_contains_await(&expr.node) {
                let is_direct_single = matches!(&expr.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                let collected = collect_awaits_vec(expr);
                for ca in &collected {
                    let sub_future_type = self.infer_await_future_type(&ca.inner_expr.node, f);
                    sub_futures.push((await_count, sub_future_type));
                    if !is_direct_single {
                        let result_c_type = self.infer_await_result_c_type(&ca.inner_expr.node, f);
                        let tmp_name = format!("__await_tmp_{}", await_count);
                        if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                            locals.push((tmp_name, result_c_type, None));
                        }
                    }
                    await_count += 1;
                }
            }
        }

        // Compute drop entries for non-Copy params and locals
        let mut drop_entries: Vec<DropEntry> = Vec::new();
        for p in &f.params {
            if p.node.name.node == "self" { continue; }
            if let Some(action) = self.drop_action_for_type(&p.node.type_.node) {
                drop_entries.push(DropEntry {
                    var_name: c_mangle::escape_keyword(&p.node.name.node),
                    action,
                });
            }
        }
        for (name, c_type, ast_type) in &locals {
            let action = if let Some(ty) = ast_type {
                self.drop_action_for_type(ty)
            } else {
                self.drop_action_for_c_type(c_type)
            };
            if let Some(action) = action {
                drop_entries.push(DropEntry {
                    var_name: c_mangle::escape_keyword(name),
                    action,
                });
            }
        }

        AsyncAnalysis {
            params,
            locals,
            await_count,
            sub_futures,
            inner_return_c_type: inner_c_type,
            future_type_name,
            drop_entries,
        }
    }

    /// Recursively analyze a block in an async function, collecting locals and await points.
    fn analyze_async_block(
        &self,
        block: &Block,
        params: &[(String, String)],
        locals: &mut Vec<(String, String, Option<Type>)>,
        await_count: &mut usize,
        sub_futures: &mut Vec<(usize, String)>,
        context_fn: &FunctionDef,
    ) {
        for stmt in &block.stmts {
            self.analyze_async_stmt(&stmt.node, params, locals, await_count, sub_futures, context_fn);
        }
    }

    /// Analyze an expression for awaits and allocate sub-future fields + temp locals.
    /// Used for conditions, iterables, and other expression-position awaits in control flow.
    fn analyze_expr_awaits(
        &self,
        expr: &Spanned<Expr>,
        locals: &mut Vec<(String, String, Option<Type>)>,
        await_count: &mut usize,
        sub_futures: &mut Vec<(usize, String)>,
        context_fn: &FunctionDef,
    ) {
        let collected = collect_awaits_vec(expr);
        for ca in &collected {
            if ca.is_task {
                // Task-await: no sub-future, but allocate __await_tmp_N for result
                let result_c_type = self.infer_task_await_result_c_type_readonly(&ca.inner_expr.node);
                let tmp_name = format!("__await_tmp_{}", *await_count);
                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                    locals.push((tmp_name, result_c_type, None));
                }
            } else {
                // Future-await: allocate sub-future + temp
                let sub_future_type = self.infer_await_future_type(&ca.inner_expr.node, context_fn);
                sub_futures.push((*await_count, sub_future_type));
                let result_c_type = self.infer_await_result_c_type(&ca.inner_expr.node, context_fn);
                let tmp_name = format!("__await_tmp_{}", *await_count);
                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                    locals.push((tmp_name, result_c_type, None));
                }
            }
            *await_count += 1;
        }
    }

    /// Analyze a single statement in an async function, recursing into control flow bodies.
    fn analyze_async_stmt(
        &self,
        stmt: &Stmt,
        params: &[(String, String)],
        locals: &mut Vec<(String, String, Option<Type>)>,
        await_count: &mut usize,
        sub_futures: &mut Vec<(usize, String)>,
        context_fn: &FunctionDef,
    ) {
        match stmt {
            Stmt::VarDecl { pattern, type_, value, .. } => {
                if let Pattern::Binding(name) = &pattern.node {
                    // Deduplicate: don't add if already collected (e.g., same name in multiple branches)
                    if !locals.iter().any(|(n, _, _)| n == name) {
                        let c_type = if matches!(type_.node, Type::Inferred) {
                            self.infer_async_var_type(&value.node, params, locals, context_fn)
                        } else {
                            c_types::ast_type_to_c(&type_.node, self.scopes)
                        };
                        let ast_type = if matches!(type_.node, Type::Inferred) {
                            None
                        } else {
                            Some(type_.node.clone())
                        };
                        locals.push((name.clone(), c_type, ast_type));
                    }
                }
                if Self::expr_contains_await(&value.node) && !Self::is_task_await_static(&value.node) {
                    let is_direct_single = matches!(&value.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    let collected = collect_awaits_vec(value);
                    for ca in &collected {
                        if ca.is_task {
                            // Task-await: no sub-future, but allocate temp for expression-position
                            if !is_direct_single {
                                let result_c_type = self.infer_task_await_result_c_type_readonly(&ca.inner_expr.node);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        } else {
                            let sub_future_type = self.infer_await_future_type(&ca.inner_expr.node, context_fn);
                            sub_futures.push((*await_count, sub_future_type));
                            if !is_direct_single {
                                let result_c_type = self.infer_await_result_c_type(&ca.inner_expr.node, context_fn);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        }
                        *await_count += 1;
                    }
                }
            }
            Stmt::Assign { value, .. } => {
                if Self::expr_contains_await(&value.node) && !Self::is_task_await_static(&value.node) {
                    let is_direct_single = matches!(&value.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    let collected = collect_awaits_vec(value);
                    for ca in &collected {
                        if ca.is_task {
                            if !is_direct_single {
                                let result_c_type = self.infer_task_await_result_c_type_readonly(&ca.inner_expr.node);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        } else {
                            let sub_future_type = self.infer_await_future_type(&ca.inner_expr.node, context_fn);
                            sub_futures.push((*await_count, sub_future_type));
                            if !is_direct_single {
                                let result_c_type = self.infer_await_result_c_type(&ca.inner_expr.node, context_fn);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        }
                        *await_count += 1;
                    }
                }
            }
            Stmt::Return(Some(expr)) | Stmt::Expr(expr) => {
                if Self::expr_contains_await(&expr.node) && !Self::is_task_await_static(&expr.node) {
                    let is_direct_single = matches!(&expr.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    let collected = collect_awaits_vec(expr);
                    for ca in &collected {
                        if ca.is_task {
                            if !is_direct_single {
                                let result_c_type = self.infer_task_await_result_c_type_readonly(&ca.inner_expr.node);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        } else {
                            let sub_future_type = self.infer_await_future_type(&ca.inner_expr.node, context_fn);
                            sub_futures.push((*await_count, sub_future_type));
                            if !is_direct_single {
                                let result_c_type = self.infer_await_result_c_type(&ca.inner_expr.node, context_fn);
                                let tmp_name = format!("__await_tmp_{}", *await_count);
                                if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                    locals.push((tmp_name, result_c_type, None));
                                }
                            }
                        }
                        *await_count += 1;
                    }
                }
                // Allocate temp state fields for channel send operations
                if Self::expr_is_channel_op(&expr.node) {
                    if let Expr::MethodCall { receiver, method, .. } = &expr.node {
                        if method.node == "send" {
                            let send_n = locals.iter().filter(|(n, _, _)| n.starts_with("__ch_send_tmp_")).count();
                            let tmp_name = format!("__ch_send_tmp_{send_n}");
                            let elem_c_type = self.infer_channel_elem_c_type_readonly(receiver);
                            if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                locals.push((tmp_name, elem_c_type, None));
                            }
                        }
                    }
                }
            }
            // Only recurse into control flow bodies that contain await or channel ops.
            // Variables in non-await/non-channel-op bodies stay as normal C locals (not state struct fields).
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                // Analyze condition for awaits (always expression-position)
                if Self::expr_contains_await(&condition.node) && !Self::is_task_await_static(&condition.node) {
                    self.analyze_expr_awaits(condition, locals, await_count, sub_futures, context_fn);
                }
                if Self::block_contains_await(then_body) || Self::block_contains_channel_op(then_body) {
                    self.analyze_async_block(then_body, params, locals, await_count, sub_futures, context_fn);
                }
                for (elif_cond, elif_body) in elif_branches {
                    // Analyze each elif condition for awaits
                    if Self::expr_contains_await(&elif_cond.node) && !Self::is_task_await_static(&elif_cond.node) {
                        self.analyze_expr_awaits(elif_cond, locals, await_count, sub_futures, context_fn);
                    }
                    if Self::block_contains_await(elif_body) || Self::block_contains_channel_op(elif_body) {
                        self.analyze_async_block(elif_body, params, locals, await_count, sub_futures, context_fn);
                    }
                }
                if let Some(else_b) = else_body {
                    if Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b) {
                        self.analyze_async_block(else_b, params, locals, await_count, sub_futures, context_fn);
                    }
                }
            }
            Stmt::While { condition, body, .. } => {
                let cond_has_await = Self::expr_contains_await(&condition.node) && !Self::is_task_await_static(&condition.node);
                let body_has_await = Self::block_contains_await(body);
                let body_has_channel_op = Self::block_contains_channel_op(body);
                if cond_has_await {
                    self.analyze_expr_awaits(condition, locals, await_count, sub_futures, context_fn);
                }
                if body_has_await || body_has_channel_op || cond_has_await {
                    // If condition has await or body has channel ops, body locals must be lifted too
                    self.analyze_async_block(body, params, locals, await_count, sub_futures, context_fn);
                }
            }
            Stmt::Loop { body } => {
                if Self::block_contains_await(body) || Self::block_contains_channel_op(body) {
                    self.analyze_async_block(body, params, locals, await_count, sub_futures, context_fn);
                }
            }
            Stmt::For { pattern, iterable, body, else_body, .. } => {
                let body_has_await = Self::block_contains_await(body);
                let body_has_channel_op = Self::block_contains_channel_op(body);
                let iterable_has_await = Self::expr_contains_await(&iterable.node) && !Self::is_task_await_static(&iterable.node);
                if iterable_has_await {
                    self.analyze_expr_awaits(iterable, locals, await_count, sub_futures, context_fn);
                }
                if body_has_await || body_has_channel_op || iterable_has_await {
                    // Allocate break-flag for for/else
                    if else_body.is_some() {
                        let broke_n = locals.iter().filter(|(n, _, _)| n.starts_with("__for_broke_")).count();
                        let broke_name = format!("__for_broke_{broke_n}");
                        if !locals.iter().any(|(n, _, _)| n == &broke_name) {
                            locals.push((broke_name, "bool".to_string(), None));
                        }
                    }
                    if let Expr::Range { .. } = &iterable.node {
                        // Range for-loop: just lift the loop variable as int64_t
                        if let Pattern::Binding(name) = &pattern.node {
                            if !locals.iter().any(|(n, _, _)| n == name) {
                                locals.push((name.clone(), "int64_t".to_string(), None));
                            }
                        }
                    } else {
                        // Non-range: classify iterable and lift appropriate state vars
                        let for_n = locals.iter().filter(|(n, _, _)| n.starts_with("__for_idx_")).count();
                        let kind = self.classify_iterable_readonly(iterable);
                        match &kind {
                            AsyncIterableKind::Vector(elem_type) => {
                                let idx_name = format!("__for_idx_{for_n}");
                                if !locals.iter().any(|(n, _, _)| n == &idx_name) {
                                    locals.push((idx_name, "size_t".to_string(), None));
                                }
                                if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), elem_type.clone(), None));
                                    }
                                }
                            }
                            AsyncIterableKind::Dict(key_type, val_type) => {
                                let oi_name = format!("__for_oi_{for_n}");
                                let idx_name = format!("__for_idx_{for_n}");
                                if !locals.iter().any(|(n, _, _)| n == &oi_name) {
                                    locals.push((oi_name, "size_t".to_string(), None));
                                }
                                if !locals.iter().any(|(n, _, _)| n == &idx_name) {
                                    locals.push((idx_name, "size_t".to_string(), None));
                                }
                                // Extract key/value names from tuple pattern
                                if let Pattern::Tuple(elems) = &pattern.node {
                                    if let Some(k_pat) = elems.first() {
                                        if let Pattern::Binding(k_name) = &k_pat.node {
                                            if !locals.iter().any(|(n, _, _)| n == k_name) {
                                                locals.push((k_name.clone(), key_type.clone(), None));
                                            }
                                        }
                                    }
                                    if let Some(v_pat) = elems.get(1) {
                                        if let Pattern::Binding(v_name) = &v_pat.node {
                                            if !locals.iter().any(|(n, _, _)| n == v_name) {
                                                locals.push((v_name.clone(), val_type.clone(), None));
                                            }
                                        }
                                    }
                                } else if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), key_type.clone(), None));
                                    }
                                }
                            }
                            AsyncIterableKind::HashMap(key_type, val_type) => {
                                let idx_name = format!("__for_idx_{for_n}");
                                if !locals.iter().any(|(n, _, _)| n == &idx_name) {
                                    locals.push((idx_name, "size_t".to_string(), None));
                                }
                                if let Pattern::Tuple(elems) = &pattern.node {
                                    if let Some(k_pat) = elems.first() {
                                        if let Pattern::Binding(k_name) = &k_pat.node {
                                            if !locals.iter().any(|(n, _, _)| n == k_name) {
                                                locals.push((k_name.clone(), key_type.clone(), None));
                                            }
                                        }
                                    }
                                    if let Some(v_pat) = elems.get(1) {
                                        if let Pattern::Binding(v_name) = &v_pat.node {
                                            if !locals.iter().any(|(n, _, _)| n == v_name) {
                                                locals.push((v_name.clone(), val_type.clone(), None));
                                            }
                                        }
                                    }
                                } else if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), key_type.clone(), None));
                                    }
                                }
                            }
                            AsyncIterableKind::String => {
                                let idx_name = format!("__for_idx_{for_n}");
                                let len_name = format!("__for_len_{for_n}");
                                if !locals.iter().any(|(n, _, _)| n == &idx_name) {
                                    locals.push((idx_name, "size_t".to_string(), None));
                                }
                                if !locals.iter().any(|(n, _, _)| n == &len_name) {
                                    locals.push((len_name, "size_t".to_string(), None));
                                }
                                if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), "char".to_string(), None));
                                    }
                                }
                            }
                            AsyncIterableKind::Set(elem_type) => {
                                let idx_name = format!("__for_idx_{for_n}");
                                if !locals.iter().any(|(n, _, _)| n == &idx_name) {
                                    locals.push((idx_name, "size_t".to_string(), None));
                                }
                                if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), elem_type.clone(), None));
                                    }
                                }
                            }
                            AsyncIterableKind::Unknown => {
                                // Busy-poll fallback: just lift the element var
                                if let Pattern::Binding(name) = &pattern.node {
                                    if !locals.iter().any(|(n, _, _)| n == name) {
                                        locals.push((name.clone(), "int64_t".to_string(), None));
                                    }
                                }
                            }
                        }
                    }
                    self.analyze_async_block(body, params, locals, await_count, sub_futures, context_fn);
                }
            }
            Stmt::Select { arms, else_arm } => {
                let mut send_tmp_count = 0;
                for arm in arms {
                    match &arm.op {
                        SelectOp::Recv { type_, name, .. } => {
                            let c_type = self.type_to_c(&type_.node);
                            let escaped = c_mangle::escape_keyword(&name.node);
                            if !locals.iter().any(|(n, _, _)| n == &escaped) {
                                locals.push((escaped, c_type, None));
                            }
                        }
                        SelectOp::Send { channel, .. } => {
                            let tmp_name = format!("__sel_send_tmp_{}", self.async_select_counter * 100 + send_tmp_count);
                            send_tmp_count += 1;
                            let elem_c_type = self.infer_channel_elem_c_type_readonly(channel);
                            if !locals.iter().any(|(n, _, _)| n == &tmp_name) {
                                locals.push((tmp_name, elem_c_type, None));
                            }
                        }
                    }
                    self.analyze_async_block(&arm.body, params, locals, await_count, sub_futures, context_fn);
                }
                if let Some(else_arm) = else_arm {
                    self.analyze_async_block(else_arm, params, locals, await_count, sub_futures, context_fn);
                }
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                let any_arm_has_suspend = arms.iter().any(|arm| match &arm.body.node {
                    Expr::Block(block) => Self::block_contains_await(block) || Self::block_contains_channel_op(block),
                    other => Self::expr_contains_await(other) || Self::expr_contains_channel_op(other),
                });
                let else_has_suspend = else_arm.as_ref().map_or(false, |b| Self::block_contains_await(b) || Self::block_contains_channel_op(b));

                if any_arm_has_suspend || else_has_suspend {
                    // Add scrutinee temp variable
                    let match_idx = locals.iter().filter(|(n, _, _)| n.starts_with("__match_scrut_")).count();
                    let scrut_name = format!("__match_scrut_{match_idx}");
                    let scrut_c_type = self.infer_async_var_type(&scrutinee.node, params, locals, context_fn);
                    locals.push((scrut_name, scrut_c_type.clone(), None));

                    // Collect pattern-bound variables from arms that contain await or channel ops
                    for arm in arms {
                        let arm_has_suspend = match &arm.body.node {
                            Expr::Block(block) => Self::block_contains_await(block) || Self::block_contains_channel_op(block),
                            other => Self::expr_contains_await(other) || Self::expr_contains_channel_op(other),
                        };
                        if arm_has_suspend {
                            let bindings = self.collect_async_pattern_binding_c_types(
                                &arm.pattern.node, scrutinee, &scrut_c_type,
                            );
                            for (name, c_type) in bindings {
                                if !locals.iter().any(|(n, _, _)| n == &name) {
                                    locals.push((name, c_type, None));
                                }
                            }
                            // Recurse into arm body
                            if let Expr::Block(block) = &arm.body.node {
                                self.analyze_async_block(block, params, locals, await_count, sub_futures, context_fn);
                            }
                        }
                    }
                    // Recurse into else arm if it has await or channel ops
                    if let Some(else_b) = else_arm {
                        if else_has_suspend {
                            self.analyze_async_block(else_b, params, locals, await_count, sub_futures, context_fn);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Check if an Await expression is awaiting a Task (not a Future).
    /// Task-await inner expressions are Identifiers (variables) or Spawn expressions,
    /// NOT Call expressions (which produce Futures from async functions).
    fn is_task_await_static(expr: &Expr) -> bool {
        if let Expr::Await { expr: inner } = expr {
            // If the inner expression is a Call to an async function, it's a Future-await.
            // If it's an Identifier (a Task variable), Spawn, etc., it's a Task-await.
            !matches!(inner.node, Expr::Call { .. })
        } else {
            false
        }
    }

    /// Check if a block contains any Await expression (recursive into control flow).
    fn block_contains_await(block: &Block) -> bool {
        block.stmts.iter().any(|s| Self::stmt_contains_await(&s.node))
    }

    /// Check if a statement (or its sub-blocks) contains any Await expression.
    fn stmt_contains_await(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::VarDecl { value, .. } => Self::expr_contains_await(&value.node),
            Stmt::Assign { value, .. } => Self::expr_contains_await(&value.node),
            Stmt::CompoundAssign { value, .. } => Self::expr_contains_await(&value.node),
            Stmt::Expr(e) | Stmt::Return(Some(e)) => Self::expr_contains_await(&e.node),
            Stmt::If { condition, then_body, elif_branches, else_body } =>
                Self::expr_contains_await(&condition.node)
                || Self::block_contains_await(then_body)
                || elif_branches.iter().any(|(c, b)| Self::expr_contains_await(&c.node) || Self::block_contains_await(b))
                || else_body.as_ref().map_or(false, Self::block_contains_await),
            Stmt::While { condition, body, .. } =>
                Self::expr_contains_await(&condition.node) || Self::block_contains_await(body),
            Stmt::Loop { body } => Self::block_contains_await(body),
            Stmt::For { iterable, body, .. } =>
                Self::expr_contains_await(&iterable.node) || Self::block_contains_await(body),
            Stmt::Match { arms, else_arm, .. } => {
                arms.iter().any(|arm| match &arm.body.node {
                    Expr::Block(block) => Self::block_contains_await(block),
                    other => Self::expr_contains_await(other),
                }) || else_arm.as_ref().map_or(false, Self::block_contains_await)
            }
            Stmt::Select { .. } => true,
            _ => false,
        }
    }

    /// Check if an expression contains an Await node.
    fn expr_contains_await(expr: &Expr) -> bool {
        match expr {
            Expr::Await { .. } => true,
            Expr::Call { callee, args, .. } => {
                Self::expr_contains_await(&callee.node)
                    || args.iter().any(|a| Self::expr_contains_await(&a.node.value.node))
            }
            Expr::BinaryOp { left, right, .. } => {
                Self::expr_contains_await(&left.node) || Self::expr_contains_await(&right.node)
            }
            Expr::UnaryOp { operand, .. } => Self::expr_contains_await(&operand.node),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                Self::expr_contains_await(&object.node)
            }
            Expr::Index { object, index } => {
                Self::expr_contains_await(&object.node) || Self::expr_contains_await(&index.node)
            }
            Expr::MethodCall { receiver, args, .. } => {
                Self::expr_contains_await(&receiver.node)
                    || args.iter().any(|a| Self::expr_contains_await(&a.node.value.node))
            }
            Expr::As { expr: inner, .. }
            | Expr::Try { expr: inner }
            | Expr::Move { expr: inner }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::TryCapture { expr: inner } => Self::expr_contains_await(&inner.node),
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                elems.iter().any(|e| Self::expr_contains_await(&e.node))
            }
            Expr::NilCoalescing { lhs, rhs } => {
                Self::expr_contains_await(&lhs.node) || Self::expr_contains_await(&rhs.node)
            }
            Expr::Range { start, end, .. } => {
                start.as_ref().map_or(false, |s| Self::expr_contains_await(&s.node))
                    || end.as_ref().map_or(false, |e| Self::expr_contains_await(&e.node))
            }
            _ => false,
        }
    }

    /// Check if an expression is `await <task>` where the inner expression is a Task[T],
    /// not a Future[T]. Task-await is blocking (condvar wait), NOT a suspension point.
    fn is_task_await(&mut self, expr: &Expr) -> bool {
        if let Expr::Await { expr: inner } = expr {
            let c_type = self.infer_c_type_from_expr(&inner.node);
            c_type.starts_with("Task__")
        } else {
            false
        }
    }

    // ── Channel op detection (AST-level, conservative) ──

    /// Check if an expression is a potential channel send/recv MethodCall.
    /// Conservative: any MethodCall named "send"/"recv" matches.
    /// False positives only cause unnecessary async routing (safe).
    fn expr_is_channel_op(expr: &Expr) -> bool {
        matches!(expr, Expr::MethodCall { method, .. } if method.node == "send" || method.node == "recv")
    }

    /// Check if an expression tree contains a potential channel op.
    fn expr_contains_channel_op(expr: &Expr) -> bool {
        match expr {
            Expr::MethodCall { receiver, method, args, .. } => {
                if method.node == "send" || method.node == "recv" {
                    return true;
                }
                Self::expr_contains_channel_op(&receiver.node)
                    || args.iter().any(|a| Self::expr_contains_channel_op(&a.node.value.node))
            }
            Expr::Call { callee, args, .. } => {
                Self::expr_contains_channel_op(&callee.node)
                    || args.iter().any(|a| Self::expr_contains_channel_op(&a.node.value.node))
            }
            Expr::BinaryOp { left, right, .. } => {
                Self::expr_contains_channel_op(&left.node) || Self::expr_contains_channel_op(&right.node)
            }
            Expr::UnaryOp { operand, .. } => Self::expr_contains_channel_op(&operand.node),
            Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
                Self::expr_contains_channel_op(&object.node)
            }
            Expr::Index { object, index } => {
                Self::expr_contains_channel_op(&object.node) || Self::expr_contains_channel_op(&index.node)
            }
            Expr::As { expr: inner, .. }
            | Expr::Try { expr: inner }
            | Expr::Move { expr: inner }
            | Expr::MutableBorrow { expr: inner }
            | Expr::Deref { expr: inner }
            | Expr::TryCapture { expr: inner } => Self::expr_contains_channel_op(&inner.node),
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                elems.iter().any(|e| Self::expr_contains_channel_op(&e.node))
            }
            _ => false,
        }
    }

    /// Check if a statement (or its sub-blocks) contains a potential channel op.
    fn stmt_contains_channel_op(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::VarDecl { value, .. } => Self::expr_contains_channel_op(&value.node),
            Stmt::Assign { value, .. } => Self::expr_contains_channel_op(&value.node),
            Stmt::CompoundAssign { value, .. } => Self::expr_contains_channel_op(&value.node),
            Stmt::Expr(e) | Stmt::Return(Some(e)) => Self::expr_contains_channel_op(&e.node),
            Stmt::If { condition, then_body, elif_branches, else_body } =>
                Self::expr_contains_channel_op(&condition.node)
                || Self::block_contains_channel_op(then_body)
                || elif_branches.iter().any(|(c, b)| Self::expr_contains_channel_op(&c.node) || Self::block_contains_channel_op(b))
                || else_body.as_ref().map_or(false, Self::block_contains_channel_op),
            Stmt::While { condition, body, .. } =>
                Self::expr_contains_channel_op(&condition.node) || Self::block_contains_channel_op(body),
            Stmt::Loop { body } => Self::block_contains_channel_op(body),
            Stmt::For { iterable, body, .. } =>
                Self::expr_contains_channel_op(&iterable.node) || Self::block_contains_channel_op(body),
            Stmt::Match { arms, else_arm, .. } => {
                arms.iter().any(|arm| match &arm.body.node {
                    Expr::Block(block) => Self::block_contains_channel_op(block),
                    other => Self::expr_contains_channel_op(other),
                }) || else_arm.as_ref().map_or(false, Self::block_contains_channel_op)
            }
            Stmt::Select { .. } => true,
            _ => false,
        }
    }

    /// Check if a block contains any potential channel op.
    fn block_contains_channel_op(block: &Block) -> bool {
        block.stmts.iter().any(|s| Self::stmt_contains_channel_op(&s.node))
    }

    // ── Type-aware channel detection (for emit phase) ──

    /// Check if an expression is an async channel send (only inside async poll body).
    fn is_async_channel_send(&mut self, expr: &Expr) -> bool {
        if self.async_lifted_vars.is_none() { return false; }
        if let Expr::MethodCall { receiver, method, .. } = expr {
            if method.node == "send" {
                let c_type = self.infer_c_type_from_expr(&receiver.node);
                return c_type == "GorgetChannel*";
            }
        }
        false
    }

    /// Check if an expression is an async channel recv (only inside async poll body).
    fn is_async_channel_recv(&mut self, expr: &Expr) -> bool {
        if self.async_lifted_vars.is_none() { return false; }
        if let Expr::MethodCall { receiver, method, .. } = expr {
            if method.node == "recv" {
                let c_type = self.infer_c_type_from_expr(&receiver.node);
                return c_type == "GorgetChannel*";
            }
        }
        false
    }

    /// Infer the element C type of a Channel[T] from the receiver expression (&mut self version).
    fn infer_channel_elem_c_type(&mut self, receiver: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, type_args) = self.types.get(tid) {
                if let Some(&elem_tid) = type_args.first() {
                    return crate::codegen::c_types::type_id_to_c(elem_tid, self.types, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Readonly version of channel element type inference for analysis phase.
    /// Uses scoped_lookup on Identifier receivers instead of resolve_expr_type_id.
    fn infer_channel_elem_c_type_readonly(&self, receiver: &Spanned<Expr>) -> String {
        let tid = match &receiver.node {
            Expr::Identifier(name) => {
                self.scoped_lookup(name)
                    .and_then(|def_id| self.scopes.get_def(def_id).type_id)
            }
            _ => None,
        };
        if let Some(tid) = tid {
            if let crate::semantic::types::ResolvedType::Generic(_, type_args) = self.types.get(tid) {
                if let Some(&elem_tid) = type_args.first() {
                    return crate::codegen::c_types::type_id_to_c(elem_tid, self.types, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Infer the Future[T] C type for an await expression.
    /// For `await callee(args)`, the callee is an async function returning Future[T].
    /// The return_type_id for async functions is already Future[T], so we use it directly.
    fn infer_await_future_type(&self, expr: &Expr, _context_fn: &FunctionDef) -> String {
        // Unwrap Await to get the inner call expression
        let inner = match expr {
            Expr::Await { expr: inner } => &inner.node,
            _ => expr,
        };

        // For Call expressions, look up the callee's return type
        if let Expr::Call { callee, .. } = inner {
            if let Expr::Identifier(name) = &callee.node {
                // Look up the function info to find its return type
                if let Some(def_id) = self.scoped_lookup(name) {
                    if let Some(fn_info) = self.function_info.get(&def_id) {
                        if let Some(ret_tid) = fn_info.return_type_id {
                            // return_type_id is already Future[T] for async functions
                            return c_types::type_id_to_c(ret_tid, self.types, self.scopes);
                        }
                    }
                }
            }
        }

        // Fallback: Future__void
        "Future__void".to_string()
    }

    /// Infer the C type for an auto-typed variable in an async function body.
    fn infer_async_var_type(
        &self,
        expr: &Expr,
        params: &[(String, String)],
        locals: &[(String, String, Option<Type>)],
        context_fn: &FunctionDef,
    ) -> String {
        match expr {
            Expr::Await { expr: inner } => {
                // `auto x = await callee(args)` — type is the inner T of the callee's Future[T]
                if let Expr::Call { callee, .. } = &inner.node {
                    if let Expr::Identifier(name) = &callee.node {
                        if let Some(def_id) = self.scoped_lookup(name) {
                            if let Some(fn_info) = self.function_info.get(&def_id) {
                                if let Some(ret_tid) = fn_info.return_type_id {
                                    // ret_tid is Future[T] — extract inner T
                                    return self.extract_future_inner_c_type(ret_tid);
                                }
                            }
                        }
                    }
                }
                "int64_t".to_string()
            }
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    if let Some(def_id) = self.scoped_lookup(name) {
                        if let Some(fn_info) = self.function_info.get(&def_id) {
                            if let Some(ret_tid) = fn_info.return_type_id {
                                return c_types::type_id_to_c(ret_tid, self.types, self.scopes);
                            }
                        }
                    }
                }
                "int64_t".to_string()
            }
            Expr::IntLiteral(_) => "int64_t".to_string(),
            Expr::FloatLiteral(_) => "double".to_string(),
            Expr::BoolLiteral(_) => "bool".to_string(),
            Expr::StringLiteral(_) => "const char*".to_string(),
            Expr::CharLiteral(_) => "char".to_string(),
            Expr::Identifier(name) => {
                // Look up in params first
                for (n, t) in params {
                    if n == name {
                        return t.clone();
                    }
                }
                // Then locals (3-tuple)
                for (n, t, _) in locals {
                    if n == name {
                        return t.clone();
                    }
                }
                "int64_t".to_string()
            }
            Expr::BinaryOp { left, .. } => {
                self.infer_async_var_type(&left.node, params, locals, context_fn)
            }
            _ => "int64_t".to_string(),
        }
    }

    /// Extract the inner T from a Future[T] type id, returning the C type string.
    /// If the type is Generic(Future_def_id, [inner_tid]), returns type_id_to_c(inner_tid).
    /// Otherwise falls back to the full type.
    fn extract_future_inner_c_type(&self, tid: crate::semantic::ids::TypeId) -> String {
        use crate::semantic::types::ResolvedType;
        match self.types.get(tid) {
            ResolvedType::Generic(_def_id, args) if !args.is_empty() => {
                c_types::type_id_to_c(args[0], self.types, self.scopes)
            }
            _ => c_types::type_id_to_c(tid, self.types, self.scopes),
        }
    }

    /// Infer the C type of an await expression's result (the inner T of Future[T]).
    /// Used to determine the type of `__await_tmp_N` state fields for expression-position awaits.
    fn infer_await_result_c_type(&self, inner_expr: &Expr, context_fn: &FunctionDef) -> String {
        // Wrap in a synthetic Await to reuse infer_await_future_type which unwraps Await
        let future_c_type = self.infer_await_future_type(inner_expr, context_fn);
        // future_c_type is "Future__X" — look it up in future_types to get the inner type
        if let Some(inner) = self.future_types.get(&future_c_type) {
            return inner.clone();
        }
        // Fallback: parse the mangled name
        if let Some(suffix) = future_c_type.strip_prefix("Future__") {
            return suffix.to_string();
        }
        "int64_t".to_string()
    }

    /// Infer the C result type for a task-await expression (read-only, works in analysis pass).
    /// For `task_var.await()` where `task_var` is `Task[T]`, returns the C type of `T`.
    /// Uses scoped_lookup to find the Task[T] type and strips the `Task__` prefix.
    fn infer_task_await_result_c_type_readonly(&self, inner_expr: &Expr) -> String {
        if let Expr::Identifier(name) = inner_expr {
            if let Some(def_id) = self.scoped_lookup(name) {
                let def = self.scopes.get_def(def_id);
                if let Some(tid) = def.type_id {
                    let c_type = c_types::type_id_to_c(tid, self.types, self.scopes);
                    if let Some(suffix) = c_type.strip_prefix("Task__") {
                        return suffix.to_string();
                    }
                }
            }
        }
        "int64_t".to_string()
    }

    /// Check if a name matches a known enum variant (to distinguish genuine bindings from
    /// unit-variant pattern matches like `case None:`).
    fn is_enum_variant_name(&self, name: &str) -> bool {
        self.enum_variants.values().any(|info| info.variants.iter().any(|(vname, _)| vname == name))
    }

    /// Collect pattern-bound variable names and their C types for async state struct lifting.
    /// Returns `Vec<(name, c_type)>` for all bindings in the pattern.
    fn collect_async_pattern_binding_c_types(
        &self,
        pattern: &Pattern,
        scrutinee: &Spanned<Expr>,
        scrutinee_c_type: &str,
    ) -> Vec<(String, String)> {
        let mut result = Vec::new();
        self.collect_async_pattern_bindings_inner(pattern, scrutinee, scrutinee_c_type, &mut result);
        result
    }

    fn collect_async_pattern_bindings_inner(
        &self,
        pattern: &Pattern,
        scrutinee: &Spanned<Expr>,
        scrutinee_c_type: &str,
        out: &mut Vec<(String, String)>,
    ) {
        match pattern {
            Pattern::Binding(name) => {
                // Skip if this is actually an enum variant name (e.g., `None`)
                if self.is_enum_variant_name(name) {
                    return;
                }
                out.push((name.clone(), scrutinee_c_type.to_string()));
            }
            Pattern::Constructor { path, fields } => {
                // Resolve variant field types from enum_variants + generic substitution
                let variant_name = if path.len() == 1 {
                    &path[0].node
                } else if path.len() >= 2 {
                    &path[1].node
                } else {
                    return;
                };

                // Find the enum that owns this variant
                let mut enum_def_id = None;
                for (def_id, info) in self.enum_variants {
                    if info.variants.iter().any(|(v, _)| v == variant_name) {
                        enum_def_id = Some(*def_id);
                        break;
                    }
                }
                let Some(enum_def_id) = enum_def_id else { return };
                let Some(info) = self.enum_variants.get(&enum_def_id) else { return };

                // Build generic substitution map: param_name → concrete TypeId
                let subst_tids = if let Some(scrut_tid) = self.resolve_scrutinee_type_id_readonly(scrutinee) {
                    match self.types.get(scrut_tid) {
                        crate::semantic::types::ResolvedType::Generic(_, args) => args.clone(),
                        _ => vec![],
                    }
                } else {
                    vec![]
                };

                let subst: HashMap<String, crate::semantic::ids::TypeId> = info
                    .generic_param_names
                    .iter()
                    .zip(subst_tids.iter())
                    .map(|(name, &tid)| (name.clone(), tid))
                    .collect();

                // Find variant's field AST types
                let field_types = info
                    .variant_field_types
                    .iter()
                    .find(|(name, _)| name == variant_name)
                    .map(|(_, types)| types.as_slice());

                for (i, field_pat) in fields.iter().enumerate() {
                    let field_c_type = if let Some(field_types) = field_types {
                        if let Some(spanned_ty) = field_types.get(i) {
                            self.resolve_field_c_type_with_subst(&spanned_ty.node, &subst)
                        } else {
                            "int64_t".to_string()
                        }
                    } else {
                        "int64_t".to_string()
                    };
                    self.collect_async_pattern_bindings_inner(&field_pat.node, scrutinee, &field_c_type, out);
                }
            }
            Pattern::Tuple(elements) => {
                // For tuples, we don't have easy type info per element — use int64_t fallback
                for elem in elements {
                    self.collect_async_pattern_bindings_inner(&elem.node, scrutinee, "int64_t", out);
                }
            }
            Pattern::Or(alternatives) => {
                // Bind from first alternative (all must bind same names)
                if let Some(first) = alternatives.first() {
                    self.collect_async_pattern_bindings_inner(&first.node, scrutinee, scrutinee_c_type, out);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Resolve a variant field's AST Type to a C type string, substituting generic params.
    fn resolve_field_c_type_with_subst(
        &self,
        ast_type: &Type,
        subst: &HashMap<String, crate::semantic::ids::TypeId>,
    ) -> String {
        if let Type::Named { name, generic_args } = ast_type {
            if generic_args.is_empty() {
                // Check if this is a generic param that needs substitution
                if let Some(&tid) = subst.get(&name.node) {
                    return c_types::type_id_to_c(tid, self.types, self.scopes);
                }
            }
        }
        c_types::ast_type_to_c(ast_type, self.scopes)
    }

    /// Read-only TypeId resolution for scrutinee expressions during async analysis.
    /// Uses resolution_map (not scoped_lookup which needs function scope set).
    fn resolve_scrutinee_type_id_readonly(&self, scrutinee: &Spanned<Expr>) -> Option<crate::semantic::ids::TypeId> {
        match &scrutinee.node {
            Expr::Identifier(name) => {
                self.resolution_map
                    .get(&scrutinee.span.start)
                    .filter(|def_id| self.scopes.get_def(**def_id).name == *name)
                    .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                    .or_else(|| {
                        self.scoped_lookup(name)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                    })
            }
            _ => None,
        }
    }

    /// Classify the iterable collection kind for async state lifting (read-only analysis pass).
    /// Uses expr_types and resolution_map to resolve TypeId without &mut self.
    fn classify_iterable_readonly(&self, iterable: &Spanned<Expr>) -> AsyncIterableKind {
        use crate::semantic::types::ResolvedType;

        // Check for string literal or str-typed expression
        if matches!(&iterable.node, Expr::StringLiteral(_)) {
            return AsyncIterableKind::String;
        }

        if let Some(tid) = self.resolve_iterable_type_id_readonly(iterable) {
            match self.types.get(tid) {
                ResolvedType::Primitive(crate::parser::ast::PrimitiveType::Str) => {
                    return AsyncIterableKind::String;
                }
                ResolvedType::Generic(def_id, args) => {
                    let def_name = self.scopes.get_def(*def_id).name.clone();
                    match def_name.as_str() {
                        "Vector" | "List" | "Array" => {
                            let elem = if let Some(&elem_tid) = args.first() {
                                self.type_id_to_c_substituted(elem_tid)
                            } else {
                                "int64_t".to_string()
                            };
                            return AsyncIterableKind::Vector(elem);
                        }
                        "Dict" => {
                            let (k, v) = if args.len() >= 2 {
                                (
                                    self.type_id_to_c_substituted(args[0]),
                                    self.type_id_to_c_substituted(args[1]),
                                )
                            } else {
                                ("int64_t".to_string(), "int64_t".to_string())
                            };
                            return AsyncIterableKind::Dict(k, v);
                        }
                        "HashMap" => {
                            let (k, v) = if args.len() >= 2 {
                                (
                                    self.type_id_to_c_substituted(args[0]),
                                    self.type_id_to_c_substituted(args[1]),
                                )
                            } else {
                                ("int64_t".to_string(), "int64_t".to_string())
                            };
                            return AsyncIterableKind::HashMap(k, v);
                        }
                        "Set" | "HashSet" => {
                            let elem = if let Some(&elem_tid) = args.first() {
                                self.type_id_to_c_substituted(elem_tid)
                            } else {
                                "int64_t".to_string()
                            };
                            return AsyncIterableKind::Set(elem);
                        }
                        _ => {}
                    }
                }
                // Auto-promoted array literals
                ResolvedType::Array(elem_tid, _) => {
                    let elem = self.type_id_to_c_substituted(*elem_tid);
                    return AsyncIterableKind::Vector(elem);
                }
                _ => {}
            }
        }

        // Check vector_vars as fallback (auto-promoted array literals)
        if let Expr::Identifier(name) = &iterable.node {
            if self.vector_vars.contains(&crate::codegen::c_mangle::escape_keyword(name)) {
                return AsyncIterableKind::Vector("int64_t".to_string());
            }
        }

        AsyncIterableKind::Unknown
    }

    /// Read-only TypeId resolution for iterable expressions during async analysis.
    /// Tries expr_types first, then resolution_map/scoped_lookup.
    fn resolve_iterable_type_id_readonly(&self, expr: &Spanned<Expr>) -> Option<crate::semantic::ids::TypeId> {
        // Try expr_types (Span→TypeId map from type checker) first
        if let Some(&tid) = self.expr_types.get(&expr.span) {
            return Some(tid);
        }
        // Fall back to resolution_map-based lookup (same as resolve_scrutinee_type_id_readonly)
        match &expr.node {
            Expr::Identifier(name) => {
                self.resolution_map
                    .get(&expr.span.start)
                    .filter(|def_id| self.scopes.get_def(**def_id).name == *name)
                    .and_then(|def_id| self.scopes.get_def(*def_id).type_id)
                    .or_else(|| {
                        self.scoped_lookup(name)
                            .and_then(|def_id| self.scopes.get_def(def_id).type_id)
                    })
            }
            _ => None,
        }
    }

    /// Emit cleanup for non-Copy fields in async state struct before freeing.
    /// `skip_var` is the escaped field name being returned (ownership transferred to result),
    /// which must not be dropped.
    /// Emit suspension points for each await within an expression.
    /// After this call, `async_await_replacements` maps each Await span to its temp field reference,
    /// so a subsequent `gen_expr` on the same expression will substitute the temporaries.
    fn emit_expr_position_awaits(
        &mut self,
        expr: &Spanned<Expr>,
        emitter: &mut CEmitter,
        state_idx: &mut usize,
        sub_idx: &mut usize,
    ) {
        let collected = collect_awaits_vec(expr);
        for ca in &collected {
            let tmp_field = format!("__await_tmp_{}", *sub_idx);

            if ca.is_task {
                // Task-await: non-blocking waker-based suspension (same pattern as statement-level task-await)
                let task_expr = self.gen_expr(ca.inner_expr);
                // Infer the Task[T] type to get the SpawnCtx suffix
                let inner_c_type_task = self.infer_c_type_from_expr(&ca.inner_expr.node);
                let suffix = inner_c_type_task.strip_prefix("Task__").unwrap_or("void");
                let ctx_name = format!("__SpawnCtx__{suffix}");

                *state_idx += 1;
                emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                emitter.dedent();
                emitter.emit_line(&format!("case {}:", *state_idx));
                emitter.indent();

                emitter.emit_line("{");
                emitter.indent();
                emitter.emit_line(&format!("GorgetTask* __td = ({task_expr})._task;"));
                emitter.emit_line("pthread_mutex_lock(&__td->mtx);");
                emitter.emit_line("if (!__td->done) {");
                emitter.indent();
                emitter.emit_line("__td->parent_waker = *__waker;");
                emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");
                emitter.emit_line("return GORGET_POLL_PENDING;");
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");

                if suffix != "void" {
                    emitter.emit_line(&format!(
                        "__self->{tmp_field} = (({ctx_name}*)__td)->future.result;"
                    ));
                }
                emitter.emit_line("pthread_mutex_destroy(&__td->mtx);");
                emitter.emit_line("pthread_cond_destroy(&__td->cond);");
                emitter.emit_line("GORGET_FREE(__td, 0);");
                emitter.dedent();
                emitter.emit_line("}");
            } else {
                // Future-await: existing sub-future poll logic
                let inner_c = self.gen_expr(ca.inner_expr);
                let sub_field = format!("__sub{}", *sub_idx);

                // Store the sub-future and transition to next state
                emitter.emit_line(&format!("__self->{sub_field} = {inner_c};"));
                *state_idx += 1;
                emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                emitter.dedent();

                // Poll case label
                emitter.emit_line(&format!("case {}:", *state_idx));
                emitter.indent();
                emitter.emit_line(&format!(
                    "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                ));

                // Extract result into temp field
                emitter.emit_line(&format!("__self->{tmp_field} = __self->{sub_field}.result;"));
            }

            // Register replacement so gen_expr substitutes this Await node
            self.async_await_replacements.insert(
                ca.await_span_start,
                format!("__self->{tmp_field}"),
            );
            *sub_idx += 1;
        }
    }

    fn emit_async_cleanup(
        drop_entries: &[DropEntry],
        skip_var: Option<&str>,
        emitter: &mut CEmitter,
    ) {
        for entry in drop_entries.iter().rev() {
            if let Some(skip) = skip_var {
                if entry.var_name == skip { continue; }
            }
            let prefixed = DropEntry {
                var_name: format!("__self->{}", entry.var_name),
                action: entry.action.clone(),
            };
            Self::emit_drop_entry(&prefixed, 0, emitter);
        }
        emitter.emit_line("GORGET_FREE(__self, 0); __future->state = NULL;");
    }

    /// Determine which state struct field is being returned (for ownership transfer).
    /// Returns the escaped field name if the return expression is a simple identifier
    /// or a move of a simple identifier.
    fn async_return_skip_var(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name) => Some(c_mangle::escape_keyword(name)),
            Expr::Move { expr: inner } => Self::async_return_skip_var(&inner.node),
            _ => None,
        }
    }

    /// Emit pattern bindings for async match arms, assigning to state struct fields
    /// (`__self->x = ...`) instead of local declarations.
    fn emit_async_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        scrutinee: &str,
        emitter: &mut CEmitter,
    ) {
        match pattern {
            Pattern::Binding(name) => {
                if self.is_enum_variant_name(name) {
                    return; // Unit variant, not a binding
                }
                let escaped = c_mangle::escape_keyword(name);
                emitter.emit_line(&format!("__self->{escaped} = {scrutinee};"));
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access = format!("{scrutinee}.data.{variant_name}._{i}");
                        self.emit_async_pattern_bindings(&field_pat.node, &field_access, emitter);
                    }
                }
            }
            Pattern::Tuple(elements) => {
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    self.emit_async_pattern_bindings(&elem.node, &field_access, emitter);
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.emit_async_pattern_bindings(&first.node, scrutinee, emitter);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}
        }
    }

    /// Emit the state struct for an async function.
    fn emit_async_state_struct(
        &self,
        state_name: &str,
        analysis: &AsyncAnalysis,
        emitter: &mut CEmitter,
    ) {
        emitter.emit_line(&format!("typedef struct {{"));
        emitter.indent();
        emitter.emit_line("int __state;");

        // Parameters
        for (name, c_type) in &analysis.params {
            emitter.emit_line(&format!("{} {};", c_type, c_mangle::escape_keyword(name)));
        }

        // Locals
        for (name, c_type, _) in &analysis.locals {
            emitter.emit_line(&format!("{} {};", c_type, c_mangle::escape_keyword(name)));
        }

        // Sub-future fields
        for (idx, future_c_type) in &analysis.sub_futures {
            emitter.emit_line(&format!("{future_c_type} __sub{idx};"));
        }

        emitter.dedent();
        emitter.emit_line(&format!("}} {state_name};"));
        emitter.blank_line();
    }

    /// Emit the poll function for an async function.
    fn emit_async_poll_function(
        &mut self,
        f: &FunctionDef,
        poll_name: &str,
        state_name: &str,
        future_type: &str,
        inner_c_type: &str,
        analysis: &AsyncAnalysis,
        emitter: &mut CEmitter,
    ) {
        emitter.emit_line(&format!(
            "static int {poll_name}({future_type}* __future, GorgetWaker* __waker) {{"
        ));
        emitter.indent();
        emitter.emit_line(&format!(
            "{state_name}* __self = ({state_name}*)__future->state;"
        ));
        emitter.emit_line("switch (__self->__state) {");

        // Build the set of lifted variable names (params + locals)
        let mut lifted = HashSet::new();
        for (name, _) in &analysis.params {
            lifted.insert(c_mangle::escape_keyword(name));
        }
        for (name, _, _) in &analysis.locals {
            lifted.insert(c_mangle::escape_keyword(name));
        }

        // Save and set async context
        let prev_lifted = self.async_lifted_vars.take();
        let prev_sub_counter = self.async_sub_counter;
        let prev_match_counter = self.async_match_counter;
        let prev_select_counter = self.async_select_counter;
        let prev_for_counter = self.async_for_counter;
        let prev_for_else_counter = self.async_for_else_counter;
        let prev_channel_op_counter = self.async_channel_op_counter;
        let prev_break_flag = self.async_break_flag.take();
        self.async_lifted_vars = Some(lifted);
        self.async_sub_counter = 0;
        self.async_match_counter = 0;
        self.async_select_counter = 0;
        self.async_for_counter = 0;
        self.async_for_else_counter = 0;
        self.async_channel_op_counter = 0;

        // Track which params are pointer params (none for async — all are in state struct)
        let prev_pointer_params = std::mem::take(&mut self.pointer_params);

        // Set current function scope for scope-aware variable lookup.
        let prev_function_scope = self.current_function_scope.take();
        let scope_key = (f.name.node.clone(), f.name.span.start);
        if let Some(&scope_id) = self.function_body_scopes.get(&scope_key) {
            self.current_function_scope = Some(scope_id);
        }

        // Emit poll body states
        match &f.body {
            FunctionBody::Block(block) => {
                self.emit_async_poll_body(block, future_type, inner_c_type, analysis, emitter);
            }
            FunctionBody::Expression(expr) => {
                emitter.indent();
                emitter.emit_line("case 0:");
                emitter.indent();
                if Self::expr_contains_await(&expr.node) {
                    let mut state_idx: usize = 0;
                    let mut sub_idx: usize = 0;
                    let is_direct_single = matches!(&expr.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    if is_direct_single {
                        if let Expr::Await { expr: await_inner } = &expr.node {
                            let inner_expr = self.gen_expr(await_inner);
                            let sub_field = format!("__sub{}", sub_idx);
                            emitter.emit_line(&format!("__self->{sub_field} = {inner_expr};"));
                            state_idx += 1;
                            emitter.emit_line(&format!("__self->__state = {};", state_idx));
                            emitter.dedent();
                            emitter.emit_line(&format!("case {}:", state_idx));
                            emitter.indent();
                            emitter.emit_line(&format!(
                                "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                            ));
                            if inner_c_type != "void" {
                                emitter.emit_line(&format!(
                                    "__future->result = __self->{sub_field}.result;"
                                ));
                            }
                        }
                    } else {
                        self.emit_expr_position_awaits(expr, emitter, &mut state_idx, &mut sub_idx);
                        let residual = self.gen_expr(expr);
                        self.async_await_replacements.clear();
                        if inner_c_type != "void" {
                            emitter.emit_line(&format!("__future->result = {residual};"));
                        }
                    }
                } else {
                    let e = self.gen_expr(expr);
                    if inner_c_type != "void" {
                        emitter.emit_line(&format!("__future->result = {e};"));
                    }
                }
                let skip = Self::async_return_skip_var(&expr.node);
                Self::emit_async_cleanup(&analysis.drop_entries, skip.as_deref(), emitter);
                emitter.emit_line("return GORGET_POLL_READY;");
                emitter.dedent();
                emitter.dedent();
            }
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        // Restore context
        self.async_lifted_vars = prev_lifted;
        self.async_sub_counter = prev_sub_counter;
        self.async_match_counter = prev_match_counter;
        self.async_select_counter = prev_select_counter;
        self.async_for_counter = prev_for_counter;
        self.async_for_else_counter = prev_for_else_counter;
        self.async_channel_op_counter = prev_channel_op_counter;
        self.async_break_flag = prev_break_flag;
        self.pointer_params = prev_pointer_params;
        self.current_function_scope = prev_function_scope;

        emitter.emit_line("}");
        emitter.emit_line("return GORGET_POLL_READY;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }

    /// Emit the body of the poll function as a series of switch cases.
    fn emit_async_poll_body(
        &mut self,
        block: &Block,
        _future_type: &str,
        inner_c_type: &str,
        analysis: &AsyncAnalysis,
        emitter: &mut CEmitter,
    ) {
        let mut state_idx: usize = 0;
        let mut sub_idx: usize = 0;

        // Start case 0
        emitter.indent();
        emitter.emit_line(&format!("case {state_idx}:"));
        emitter.indent();

        self.emit_async_stmts(&block.stmts, inner_c_type, &mut state_idx, &mut sub_idx, &analysis.drop_entries, emitter);

        // Implicit return for void functions
        if inner_c_type == "void" {
            Self::emit_async_cleanup(&analysis.drop_entries, None, emitter);
            emitter.emit_line("return GORGET_POLL_READY;");
        }

        emitter.dedent();
        emitter.dedent();
    }

    /// Emit a list of statements in async poll body context, handling await suspension points.
    fn emit_async_stmts(
        &mut self,
        stmts: &[Spanned<Stmt>],
        inner_c_type: &str,
        state_idx: &mut usize,
        sub_idx: &mut usize,
        drop_entries: &[DropEntry],
        emitter: &mut CEmitter,
    ) {
        for stmt in stmts {
            self.emit_async_stmt(&stmt.node, stmt.span, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
        }
    }

    /// Emit a single statement in async poll body context.
    /// For statements with await, emits Duff's device case labels.
    /// For control flow with await in bodies, recurses into bodies.
    fn emit_async_stmt(
        &mut self,
        stmt: &Stmt,
        span: crate::span::Span,
        inner_c_type: &str,
        state_idx: &mut usize,
        sub_idx: &mut usize,
        drop_entries: &[DropEntry],
        emitter: &mut CEmitter,
    ) {
        match stmt {
            Stmt::VarDecl { pattern, type_: _, value, is_const: _, .. } => {
                if self.is_async_channel_recv(&value.node) {
                    // Channel recv as suspension point: poll_recv directly into state field
                    if let Expr::MethodCall { receiver, .. } = &value.node {
                        let recv_c = self.gen_expr(receiver);
                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();
                        if let Pattern::Binding(name) = &pattern.node {
                            let escaped = c_mangle::escape_keyword(name);
                            emitter.emit_line(&format!(
                                "if (!gorget_channel_poll_recv({recv_c}, &__self->{escaped}, __waker)) return GORGET_POLL_PENDING;"
                            ));
                        }
                    }
                } else if Self::expr_contains_await(&value.node) && !self.is_task_await(&value.node) {
                    let is_direct_single = matches!(&value.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    if is_direct_single {
                        // Direct single await: existing path
                        if let Expr::Await { expr: await_inner } = &value.node {
                            let inner_expr = self.gen_expr(await_inner);
                            let sub_field = format!("__sub{}", *sub_idx);
                            emitter.emit_line(&format!("__self->{sub_field} = {inner_expr};"));
                            *state_idx += 1;
                            emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                            emitter.dedent();

                            emitter.emit_line(&format!("case {}:", *state_idx));
                            emitter.indent();
                            emitter.emit_line(&format!(
                                "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                            ));

                            if let Pattern::Binding(name) = &pattern.node {
                                let escaped = c_mangle::escape_keyword(name);
                                emitter.emit_line(&format!(
                                    "__self->{escaped} = __self->{sub_field}.result;"
                                ));
                            }
                            *sub_idx += 1;
                        }
                    } else {
                        // Expression-position await(s): emit each await as temp, then evaluate residual
                        self.emit_expr_position_awaits(value, emitter, state_idx, sub_idx);
                        let residual = self.gen_expr(value);
                        self.async_await_replacements.clear();
                        if let Pattern::Binding(name) = &pattern.node {
                            let escaped = c_mangle::escape_keyword(name);
                            emitter.emit_line(&format!("__self->{escaped} = {residual};"));
                        }
                    }
                } else if self.is_task_await(&value.node) {
                    // Non-blocking task-await: suspend until task completes
                    if let Expr::Await { expr: await_inner } = &value.node {
                        let inner_c_type_task = self.infer_c_type_from_expr(&await_inner.node);
                        let suffix = &inner_c_type_task["Task__".len()..];
                        let ctx_name = format!("__SpawnCtx__{suffix}");
                        let task_expr = self.gen_expr(await_inner);

                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();

                        emitter.emit_line("{");
                        emitter.indent();
                        emitter.emit_line(&format!("GorgetTask* __td = ({task_expr})._task;"));
                        emitter.emit_line("pthread_mutex_lock(&__td->mtx);");
                        emitter.emit_line("if (!__td->done) {");
                        emitter.indent();
                        emitter.emit_line("__td->parent_waker = *__waker;");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");
                        emitter.emit_line("return GORGET_POLL_PENDING;");
                        emitter.dedent();
                        emitter.emit_line("}");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");

                        if let Pattern::Binding(name) = &pattern.node {
                            let escaped = c_mangle::escape_keyword(name);
                            if suffix != "void" {
                                emitter.emit_line(&format!(
                                    "__self->{escaped} = (({ctx_name}*)__td)->future.result;"
                                ));
                            }
                        }
                        emitter.emit_line("pthread_mutex_destroy(&__td->mtx);");
                        emitter.emit_line("pthread_cond_destroy(&__td->cond);");
                        emitter.emit_line("GORGET_FREE(__td, 0);");
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                } else {
                    // VarDecl without await
                    if let Pattern::Binding(name) = &pattern.node {
                        let escaped = c_mangle::escape_keyword(name);
                        let val = self.gen_expr(value);
                        emitter.emit_line(&format!("__self->{escaped} = {val};"));
                    }
                }
            }

            Stmt::Assign { target, value } if self.is_async_channel_recv(&value.node) => {
                // Channel recv as suspension point in assignment
                if let Expr::MethodCall { receiver, .. } = &value.node {
                    let recv_c = self.gen_expr(receiver);
                    let target_c = self.gen_expr(target);
                    *state_idx += 1;
                    emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                    emitter.dedent();
                    emitter.emit_line(&format!("case {}:", *state_idx));
                    emitter.indent();
                    emitter.emit_line(&format!(
                        "if (!gorget_channel_poll_recv({recv_c}, &{target_c}, __waker)) return GORGET_POLL_PENDING;"
                    ));
                }
            }

            Stmt::Assign { target, value } if Self::expr_contains_await(&value.node) && !self.is_task_await(&value.node) => {
                let is_direct_single = matches!(&value.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                if is_direct_single {
                    // Direct single await: existing path
                    if let Expr::Await { expr: await_inner } = &value.node {
                        let inner_expr = self.gen_expr(await_inner);
                        let sub_field = format!("__sub{}", *sub_idx);
                        emitter.emit_line(&format!("__self->{sub_field} = {inner_expr};"));
                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();

                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                        ));

                        let target_c = self.gen_expr(target);
                        emitter.emit_line(&format!(
                            "{target_c} = __self->{sub_field}.result;"
                        ));
                        *sub_idx += 1;
                    }
                } else {
                    // Expression-position await(s)
                    self.emit_expr_position_awaits(value, emitter, state_idx, sub_idx);
                    let residual = self.gen_expr(value);
                    self.async_await_replacements.clear();
                    let target_c = self.gen_expr(target);
                    emitter.emit_line(&format!("{target_c} = {residual};"));
                }
            }

            Stmt::Assign { target, value } if self.is_task_await(&value.node) => {
                // Non-blocking task-await in assignment
                if let Expr::Await { expr: await_inner } = &value.node {
                    let inner_c_type_task = self.infer_c_type_from_expr(&await_inner.node);
                    let suffix = &inner_c_type_task["Task__".len()..];
                    let ctx_name = format!("__SpawnCtx__{suffix}");
                    let task_expr = self.gen_expr(await_inner);

                    *state_idx += 1;
                    emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                    emitter.dedent();
                    emitter.emit_line(&format!("case {}:", *state_idx));
                    emitter.indent();

                    emitter.emit_line("{");
                    emitter.indent();
                    emitter.emit_line(&format!("GorgetTask* __td = ({task_expr})._task;"));
                    emitter.emit_line("pthread_mutex_lock(&__td->mtx);");
                    emitter.emit_line("if (!__td->done) {");
                    emitter.indent();
                    emitter.emit_line("__td->parent_waker = *__waker;");
                    emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");
                    emitter.emit_line("return GORGET_POLL_PENDING;");
                    emitter.dedent();
                    emitter.emit_line("}");
                    emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");

                    let target_c = self.gen_expr(target);
                    if suffix != "void" {
                        emitter.emit_line(&format!(
                            "{target_c} = (({ctx_name}*)__td)->future.result;"
                        ));
                    }
                    emitter.emit_line("pthread_mutex_destroy(&__td->mtx);");
                    emitter.emit_line("pthread_cond_destroy(&__td->cond);");
                    emitter.emit_line("GORGET_FREE(__td, 0);");
                    emitter.dedent();
                    emitter.emit_line("}");
                }
            }

            Stmt::Return(Some(expr)) => {
                let mut skip: Option<String> = None;
                if Self::expr_contains_await(&expr.node) && !self.is_task_await(&expr.node) {
                    let is_direct_single = matches!(&expr.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    if is_direct_single {
                        // Direct single await: existing path
                        if let Expr::Await { expr: await_inner } = &expr.node {
                            let inner_expr = self.gen_expr(await_inner);
                            let sub_field = format!("__sub{}", *sub_idx);
                            emitter.emit_line(&format!("__self->{sub_field} = {inner_expr};"));
                            *state_idx += 1;
                            emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                            emitter.dedent();

                            emitter.emit_line(&format!("case {}:", *state_idx));
                            emitter.indent();
                            emitter.emit_line(&format!(
                                "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                            ));
                            if inner_c_type != "void" {
                                emitter.emit_line(&format!(
                                    "__future->result = __self->{sub_field}.result;"
                                ));
                            }
                            *sub_idx += 1;
                        }
                    } else {
                        // Expression-position await(s)
                        self.emit_expr_position_awaits(expr, emitter, state_idx, sub_idx);
                        let residual = self.gen_expr(expr);
                        self.async_await_replacements.clear();
                        if inner_c_type != "void" {
                            emitter.emit_line(&format!("__future->result = {residual};"));
                        }
                    }
                } else if self.is_task_await(&expr.node) {
                    // Non-blocking task-await in return
                    if let Expr::Await { expr: await_inner } = &expr.node {
                        let inner_c_type_task = self.infer_c_type_from_expr(&await_inner.node);
                        let suffix = &inner_c_type_task["Task__".len()..];
                        let ctx_name = format!("__SpawnCtx__{suffix}");
                        let task_expr = self.gen_expr(await_inner);

                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();

                        emitter.emit_line("{");
                        emitter.indent();
                        emitter.emit_line(&format!("GorgetTask* __td = ({task_expr})._task;"));
                        emitter.emit_line("pthread_mutex_lock(&__td->mtx);");
                        emitter.emit_line("if (!__td->done) {");
                        emitter.indent();
                        emitter.emit_line("__td->parent_waker = *__waker;");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");
                        emitter.emit_line("return GORGET_POLL_PENDING;");
                        emitter.dedent();
                        emitter.emit_line("}");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");

                        if inner_c_type != "void" && suffix != "void" {
                            emitter.emit_line(&format!(
                                "__future->result = (({ctx_name}*)__td)->future.result;"
                            ));
                        }
                        emitter.emit_line("pthread_mutex_destroy(&__td->mtx);");
                        emitter.emit_line("pthread_cond_destroy(&__td->cond);");
                        emitter.emit_line("GORGET_FREE(__td, 0);");
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                } else {
                    skip = Self::async_return_skip_var(&expr.node);
                    let e = self.gen_expr(expr);
                    if inner_c_type != "void" {
                        emitter.emit_line(&format!("__future->result = {e};"));
                    }
                }
                Self::emit_async_cleanup(drop_entries, skip.as_deref(), emitter);
                emitter.emit_line("return GORGET_POLL_READY;");
            }

            Stmt::Return(None) => {
                Self::emit_async_cleanup(drop_entries, None, emitter);
                emitter.emit_line("return GORGET_POLL_READY;");
            }

            Stmt::Expr(expr) => {
                if self.is_async_channel_send(&expr.node) {
                    // Channel send as suspension point
                    if let Expr::MethodCall { receiver, args, .. } = &expr.node {
                        let recv_c = self.gen_expr(receiver);
                        let val_c = self.gen_expr(&args[0].node.value);
                        let tmp_n = self.async_channel_op_counter;
                        self.async_channel_op_counter += 1;
                        let tmp_field = format!("__ch_send_tmp_{tmp_n}");

                        // Store value in state struct (persists across suspension)
                        emitter.emit_line(&format!("__self->{tmp_field} = {val_c};"));

                        // Suspension point
                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "if (!gorget_channel_poll_send({recv_c}, &__self->{tmp_field}, __waker)) return GORGET_POLL_PENDING;"
                        ));
                    }
                } else if self.is_async_channel_recv(&expr.node) {
                    // Channel recv as bare expression statement (result discarded)
                    if let Expr::MethodCall { receiver, .. } = &expr.node {
                        let recv_c = self.gen_expr(receiver);
                        let elem_c_type = self.infer_channel_elem_c_type(receiver);
                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "{{ {elem_c_type} __ch_discard; if (!gorget_channel_poll_recv({recv_c}, &__ch_discard, __waker)) return GORGET_POLL_PENDING; }}"
                        ));
                    }
                } else if Self::expr_contains_await(&expr.node) && !self.is_task_await(&expr.node) {
                    let is_direct_single = matches!(&expr.node, Expr::Await { expr: inner } if !Self::expr_contains_await(&inner.node));
                    if is_direct_single {
                        // Direct single await: existing path
                        if let Expr::Await { expr: await_inner } = &expr.node {
                            let inner_expr = self.gen_expr(await_inner);
                            let sub_field = format!("__sub{}", *sub_idx);
                            emitter.emit_line(&format!("__self->{sub_field} = {inner_expr};"));
                            *state_idx += 1;
                            emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                            emitter.dedent();

                            emitter.emit_line(&format!("case {}:", *state_idx));
                            emitter.indent();
                            emitter.emit_line(&format!(
                                "if (__self->{sub_field}.poll(&__self->{sub_field}, __waker) != GORGET_POLL_READY) return GORGET_POLL_PENDING;"
                            ));
                            *sub_idx += 1;
                        }
                    } else {
                        // Expression-position await(s)
                        self.emit_expr_position_awaits(expr, emitter, state_idx, sub_idx);
                        let residual = self.gen_expr(expr);
                        self.async_await_replacements.clear();
                        emitter.emit_line(&format!("{residual};"));
                    }
                } else if self.is_task_await(&expr.node) {
                    // Non-blocking task-await as expression statement (void result)
                    if let Expr::Await { expr: await_inner } = &expr.node {
                        let inner_c_type_task = self.infer_c_type_from_expr(&await_inner.node);
                        let suffix = &inner_c_type_task["Task__".len()..];
                        let task_expr = self.gen_expr(await_inner);

                        *state_idx += 1;
                        emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                        emitter.dedent();
                        emitter.emit_line(&format!("case {}:", *state_idx));
                        emitter.indent();

                        emitter.emit_line("{");
                        emitter.indent();
                        emitter.emit_line(&format!("GorgetTask* __td = ({task_expr})._task;"));
                        emitter.emit_line("pthread_mutex_lock(&__td->mtx);");
                        emitter.emit_line("if (!__td->done) {");
                        emitter.indent();
                        emitter.emit_line("__td->parent_waker = *__waker;");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");
                        emitter.emit_line("return GORGET_POLL_PENDING;");
                        emitter.dedent();
                        emitter.emit_line("}");
                        emitter.emit_line("pthread_mutex_unlock(&__td->mtx);");

                        // Extract result if non-void (even though it's discarded, the ctx_name is needed for cleanup)
                        let _ = suffix; // result discarded for expr-stmt
                        emitter.emit_line("pthread_mutex_destroy(&__td->mtx);");
                        emitter.emit_line("pthread_cond_destroy(&__td->cond);");
                        emitter.emit_line("GORGET_FREE(__td, 0);");
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                } else {
                    let e = self.gen_expr(expr);
                    emitter.emit_line(&format!("{e};"));
                }
            }

            // While with await/channel-op in condition or body — Duff's device
            Stmt::While { condition, body, .. }
                if Self::expr_contains_await(&condition.node) || Self::block_contains_await(body)
                || Self::block_contains_channel_op(body) =>
            {
                let cond_has_await = Self::expr_contains_await(&condition.node);
                if cond_has_await {
                    // Transform into for(;;) { await-cond; if (!cond) break; body }
                    // so condition awaits re-evaluate each iteration via Duff's device
                    emitter.emit_line("for (;;) {");
                    emitter.indent();
                    self.emit_expr_position_awaits(condition, emitter, state_idx, sub_idx);
                    let cond = self.gen_expr(condition);
                    self.async_await_replacements.clear();
                    emitter.emit_line(&format!("if (!({cond})) break;"));
                    self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                } else {
                    // Only body has await — existing codegen
                    let cond = self.gen_expr(condition);
                    emitter.emit_line(&format!("while ({cond}) {{"));
                    emitter.indent();
                    self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                }
            }

            // Loop with await/channel-op in body — Duff's device: case labels inside while(1) body
            Stmt::Loop { body } if Self::block_contains_await(body) || Self::block_contains_channel_op(body) => {
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            // If/elif/else with await/channel-op in condition or any branch — Duff's device
            Stmt::If { condition, then_body, elif_branches, else_body }
                if Self::expr_contains_await(&condition.node)
                || Self::block_contains_await(then_body) || Self::block_contains_channel_op(then_body)
                || elif_branches.iter().any(|(c, b)| Self::expr_contains_await(&c.node) || Self::block_contains_await(b) || Self::block_contains_channel_op(b))
                || else_body.as_ref().map_or(false, |b| Self::block_contains_await(b) || Self::block_contains_channel_op(b)) =>
            {
                // Pre-evaluate condition awaits before the if
                if Self::expr_contains_await(&condition.node) {
                    self.emit_expr_position_awaits(condition, emitter, state_idx, sub_idx);
                }
                let cond = self.gen_expr(condition);
                self.async_await_replacements.clear();
                emitter.emit_line(&format!("if ({cond}) {{"));
                emitter.indent();
                self.emit_async_stmts(&then_body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                emitter.dedent();
                let mut extra_nesting: usize = 0;
                for (elif_cond, elif_body) in elif_branches {
                    if Self::expr_contains_await(&elif_cond.node) {
                        // Elif condition has await: nest inside else { await; if (cond) { ... } }
                        emitter.emit_line("} else {");
                        emitter.indent();
                        self.emit_expr_position_awaits(elif_cond, emitter, state_idx, sub_idx);
                        let c = self.gen_expr(elif_cond);
                        self.async_await_replacements.clear();
                        emitter.emit_line(&format!("if ({c}) {{"));
                        emitter.indent();
                        self.emit_async_stmts(&elif_body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                        extra_nesting += 1;
                    } else {
                        let c = self.gen_expr(elif_cond);
                        emitter.emit_line(&format!("}} else if ({c}) {{"));
                        emitter.indent();
                        self.emit_async_stmts(&elif_body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                    }
                }
                if let Some(else_b) = else_body {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                    emitter.dedent();
                }
                emitter.emit_line("}");
                // Close extra nesting from elif-with-await
                for _ in 0..extra_nesting {
                    emitter.dedent();
                    emitter.emit_line("}");
                }
            }

            // For with await/channel-op in iterable or body — Duff's device
            Stmt::For { pattern, iterable, body, else_body, .. }
                if Self::expr_contains_await(&iterable.node) || Self::block_contains_await(body)
                || Self::block_contains_channel_op(body) =>
            {
                // Compute break-flag for for/else
                let broke_flag = if else_body.is_some() {
                    let broke_n = self.async_for_else_counter;
                    self.async_for_else_counter += 1;
                    Some(format!("__self->__for_broke_{broke_n}"))
                } else {
                    None
                };

                if let Some(ref flag) = broke_flag {
                    emitter.emit_line(&format!("{flag} = false;"));
                }

                // Save and set break-flag context
                let prev_break_flag = self.async_break_flag.take();
                self.async_break_flag = broke_flag.clone();

                if let Expr::Range { start, end, inclusive } = &iterable.node {
                    // Pre-evaluate iterable awaits (range bounds) before the loop
                    let iterable_has_await = Self::expr_contains_await(&iterable.node);
                    if iterable_has_await {
                        self.emit_expr_position_awaits(iterable, emitter, state_idx, sub_idx);
                    }
                    if let Pattern::Binding(name) = &pattern.node {
                        let var = c_mangle::escape_keyword(name);
                        let start_expr = start.as_ref()
                            .map(|e| self.gen_expr(e))
                            .unwrap_or_else(|| "0".to_string());
                        let end_expr = end.as_ref()
                            .map(|e| self.gen_expr(e))
                            .unwrap_or_else(|| "0".to_string());
                        let cmp = if *inclusive { "<=" } else { "<" };
                        emitter.emit_line(&format!(
                            "for (__self->{var} = {start_expr}; __self->{var} {cmp} {end_expr}; __self->{var}++) {{"
                        ));
                        if iterable_has_await {
                            self.async_await_replacements.clear();
                        }
                        emitter.indent();
                        self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                        if let Some(else_b) = else_body {
                            if let Some(ref flag) = broke_flag {
                                emitter.emit_line(&format!("if (!{flag}) {{"));
                                emitter.indent();
                            }
                            if Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b) {
                                self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                            } else {
                                self.gen_block(else_b, emitter);
                            }
                            if broke_flag.is_some() {
                                emitter.dedent();
                                emitter.emit_line("}");
                            }
                        }
                    }
                } else if self.is_gorget_array_expr(iterable) {
                    // Vector[T] with await in body
                    let for_n = self.async_for_counter;
                    self.async_for_counter += 1;
                    let idx_field = format!("__for_idx_{for_n}");
                    let iter_expr = self.gen_expr(iterable);
                    let elem_type = self.infer_vector_elem_type(iterable);
                    if let Pattern::Binding(name) = &pattern.node {
                        let var = c_mangle::escape_keyword(name);
                        emitter.emit_line(&format!(
                            "for (__self->{idx_field} = 0; __self->{idx_field} < gorget_array_len(&{iter_expr}); __self->{idx_field}++) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "__self->{var} = GORGET_ARRAY_AT({elem_type}, {iter_expr}, __self->{idx_field});"
                        ));
                        self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                        if let Some(else_b) = else_body {
                            if let Some(ref flag) = broke_flag {
                                emitter.emit_line(&format!("if (!{flag}) {{"));
                                emitter.indent();
                            }
                            if Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b) {
                                self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                            } else {
                                self.gen_block(else_b, emitter);
                            }
                            if broke_flag.is_some() {
                                emitter.dedent();
                                emitter.emit_line("}");
                            }
                        }
                    }
                } else if self.is_gorget_map_expr(iterable) {
                    // Dict[K,V] or HashMap[K,V] with await in body
                    let for_n = self.async_for_counter;
                    self.async_for_counter += 1;
                    let ordered = self.is_ordered_map_expr(iterable);
                    let iter_expr = self.gen_expr(iterable);
                    let idx_field = format!("__for_idx_{for_n}");

                    // Extract key/value names from pattern
                    let (k_name, v_name) = if let Pattern::Tuple(elems) = &pattern.node {
                        let k = match elems.first().map(|e| &e.node) {
                            Some(Pattern::Binding(name)) => c_mangle::escape_keyword(name),
                            _ => "__gorget_k".to_string(),
                        };
                        let v = match elems.get(1).map(|e| &e.node) {
                            Some(Pattern::Binding(name)) => c_mangle::escape_keyword(name),
                            _ => "__gorget_v".to_string(),
                        };
                        (k, v)
                    } else if let Pattern::Binding(name) = &pattern.node {
                        (c_mangle::escape_keyword(name), "__gorget_v".to_string())
                    } else {
                        ("__gorget_k".to_string(), "__gorget_v".to_string())
                    };

                    if ordered {
                        let oi_field = format!("__for_oi_{for_n}");
                        emitter.emit_line(&format!(
                            "for (__self->{oi_field} = 0; __self->{oi_field} < {iter_expr}.order_len; __self->{oi_field}++) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "__self->{idx_field} = {iter_expr}.order[__self->{oi_field}];"
                        ));
                        emitter.emit_line(&format!(
                            "if ({iter_expr}.states[__self->{idx_field}] != 1) continue;"
                        ));
                    } else {
                        emitter.emit_line(&format!(
                            "for (__self->{idx_field} = 0; __self->{idx_field} < {iter_expr}.cap; __self->{idx_field}++) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "if ({iter_expr}.states[__self->{idx_field}] != 1) continue;"
                        ));
                    }
                    emitter.emit_line(&format!(
                        "__self->{k_name} = {iter_expr}.keys[__self->{idx_field}];"
                    ));
                    emitter.emit_line(&format!(
                        "__self->{v_name} = {iter_expr}.values[__self->{idx_field}];"
                    ));
                    self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                    if let Some(else_b) = else_body {
                        if let Some(ref flag) = broke_flag {
                            emitter.emit_line(&format!("if (!{flag}) {{"));
                            emitter.indent();
                        }
                        if Self::block_contains_await(else_b) {
                            self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        } else {
                            self.gen_block(else_b, emitter);
                        }
                        if broke_flag.is_some() {
                            emitter.dedent();
                            emitter.emit_line("}");
                        }
                    }
                } else if self.is_string_expr(iterable) {
                    // String iteration with await in body
                    let for_n = self.async_for_counter;
                    self.async_for_counter += 1;
                    let idx_field = format!("__for_idx_{for_n}");
                    let len_field = format!("__for_len_{for_n}");
                    let iter_expr = self.gen_expr(iterable);
                    if let Pattern::Binding(name) = &pattern.node {
                        let var = c_mangle::escape_keyword(name);
                        emitter.emit_line(&format!(
                            "__self->{len_field} = {iter_expr}.len;"
                        ));
                        emitter.emit_line(&format!(
                            "for (__self->{idx_field} = 0; __self->{idx_field} < __self->{len_field}; __self->{idx_field}++) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "__self->{var} = {iter_expr}.data[__self->{idx_field}];"
                        ));
                        self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                        if let Some(else_b) = else_body {
                            if let Some(ref flag) = broke_flag {
                                emitter.emit_line(&format!("if (!{flag}) {{"));
                                emitter.indent();
                            }
                            if Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b) {
                                self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                            } else {
                                self.gen_block(else_b, emitter);
                            }
                            if broke_flag.is_some() {
                                emitter.dedent();
                                emitter.emit_line("}");
                            }
                        }
                    }
                } else if self.is_gorget_set_expr(iterable) {
                    // Set[T] with await in body
                    let for_n = self.async_for_counter;
                    self.async_for_counter += 1;
                    let idx_field = format!("__for_idx_{for_n}");
                    let iter_expr = self.gen_expr(iterable);
                    let elem_type = self.infer_set_elem_type(iterable);
                    if let Pattern::Binding(name) = &pattern.node {
                        let var = c_mangle::escape_keyword(name);
                        emitter.emit_line(&format!(
                            "for (__self->{idx_field} = 0; __self->{idx_field} < {iter_expr}.cap; __self->{idx_field}++) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line(&format!(
                            "if ({iter_expr}.states[__self->{idx_field}] != 1) continue;"
                        ));
                        emitter.emit_line(&format!(
                            "__self->{var} = *({elem_type}*)((char*){iter_expr}.keys + __self->{idx_field} * {iter_expr}.key_size);"
                        ));
                        self.emit_async_stmts(&body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                        if let Some(else_b) = else_body {
                            if let Some(ref flag) = broke_flag {
                                emitter.emit_line(&format!("if (!{flag}) {{"));
                                emitter.indent();
                            }
                            if Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b) {
                                self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                            } else {
                                self.gen_block(else_b, emitter);
                            }
                            if broke_flag.is_some() {
                                emitter.dedent();
                                emitter.emit_line("}");
                            }
                        }
                    }
                } else {
                    // Unknown iterable with await: busy-poll fallback
                    self.gen_stmt(stmt, span, emitter);
                }

                // Restore break-flag context
                self.async_break_flag = prev_break_flag;
            }

            // Match with await/channel-op in any arm — Duff's device
            Stmt::Match { scrutinee, arms, else_arm }
                if arms.iter().any(|arm| match &arm.body.node {
                    Expr::Block(block) => Self::block_contains_await(block) || Self::block_contains_channel_op(block),
                    other => Self::expr_contains_await(other) || Self::expr_contains_channel_op(other),
                }) || else_arm.as_ref().map_or(false, |b| Self::block_contains_await(b) || Self::block_contains_channel_op(b)) =>
            {
                let match_idx = self.async_match_counter;
                self.async_match_counter += 1;
                let scrut_field = format!("__match_scrut_{match_idx}");

                // Evaluate scrutinee into state struct
                let scrut_expr = self.gen_expr(scrutinee);
                emitter.emit_line(&format!("__self->{scrut_field} = {scrut_expr};"));

                // Resolve enum C type for tag comparison
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

                let mut first = true;
                for arm in arms {
                    let arm_has_await = match &arm.body.node {
                        Expr::Block(block) => Self::block_contains_await(block) || Self::block_contains_channel_op(block),
                        other => Self::expr_contains_await(other) || Self::expr_contains_channel_op(other),
                    };

                    let pattern_cond = self.pattern_to_condition(
                        &arm.pattern.node,
                        &format!("__self->{scrut_field}"),
                        enum_c_type.as_deref(),
                    );

                    let full_cond = if let Some(guard) = &arm.guard {
                        let guard_expr = self.gen_expr(guard);
                        let bindings = self.stmt_pattern_bindings_inline(
                            &arm.pattern.node,
                            &format!("__self->{scrut_field}"),
                        );
                        if bindings.is_empty() {
                            format!("({pattern_cond}) && ({guard_expr})")
                        } else {
                            format!("({pattern_cond}) && ({{ {bindings}({guard_expr}); }})")
                        }
                    } else {
                        pattern_cond
                    };

                    if first {
                        emitter.emit_line(&format!("if ({full_cond}) {{"));
                        first = false;
                    } else {
                        emitter.emit_line(&format!("}} else if ({full_cond}) {{"));
                    }
                    emitter.indent();

                    if arm_has_await {
                        // Async arm: emit bindings to state struct, then async stmts
                        self.emit_async_pattern_bindings(
                            &arm.pattern.node,
                            &format!("__self->{scrut_field}"),
                            emitter,
                        );
                        self.register_pattern_var_types(&arm.pattern.node, scrutinee);
                        if let Expr::Block(block) = &arm.body.node {
                            self.emit_async_stmts(&block.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                        }
                    } else {
                        // Non-async arm: normal local bindings + gen_block
                        self.emit_pattern_bindings(
                            &arm.pattern.node,
                            &format!("__self->{scrut_field}"),
                            emitter,
                        );
                        self.register_pattern_var_types(&arm.pattern.node, scrutinee);
                        if let Expr::Block(block) = &arm.body.node {
                            self.gen_block(block, emitter);
                        } else {
                            let body = self.gen_expr(&arm.body);
                            emitter.emit_line(&format!("{body};"));
                        }
                    }
                    emitter.dedent();
                }

                if let Some(else_b) = else_arm {
                    let else_has_suspend = Self::block_contains_await(else_b) || Self::block_contains_channel_op(else_b);
                    emitter.emit_line("} else {");
                    emitter.indent();
                    if else_has_suspend {
                        self.emit_async_stmts(&else_b.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                    } else {
                        self.gen_block(else_b, emitter);
                    }
                    emitter.dedent();
                }
                if !arms.is_empty() || else_arm.is_some() {
                    emitter.emit_line("}");
                }
            }

            // Select: channel multiplexing with probe-then-register pattern
            Stmt::Select { arms, else_arm } => {
                let sel_n = self.async_select_counter;
                self.async_select_counter += 1;

                // Build arm info: for each arm, generate channel expr and field name
                let mut arm_infos: Vec<(&str, String, String)> = Vec::new();
                let mut send_tmp_idx = 0;
                for arm in arms {
                    match &arm.op {
                        SelectOp::Send { channel, value } => {
                            let ch_c = self.gen_expr(channel);
                            let val_c = self.gen_expr(value);
                            let tmp = format!("__sel_send_tmp_{}", sel_n * 100 + send_tmp_idx);
                            send_tmp_idx += 1;
                            emitter.emit_line(&format!("__self->{tmp} = {val_c};"));
                            arm_infos.push(("send", ch_c, tmp));
                        }
                        SelectOp::Recv { name, channel, .. } => {
                            let ch_c = self.gen_expr(channel);
                            let escaped = c_mangle::escape_keyword(&name.node);
                            arm_infos.push(("recv", ch_c, escaped));
                        }
                    }
                }

                // Suspension point
                *state_idx += 1;
                emitter.emit_line(&format!("__self->__state = {};", *state_idx));
                emitter.dedent();
                emitter.emit_line(&format!("case {}:", *state_idx));
                emitter.indent();

                // Probe phase — try each arm with NULL waker
                let mut first = true;
                for (i, (kind, ch, field)) in arm_infos.iter().enumerate() {
                    let poll_call = match *kind {
                        "recv" => format!("gorget_channel_poll_recv({ch}, &__self->{field}, NULL)"),
                        "send" => format!("gorget_channel_poll_send({ch}, &__self->{field}, NULL)"),
                        _ => unreachable!(),
                    };

                    if first {
                        emitter.emit_line(&format!("if ({poll_call}) {{"));
                        first = false;
                    } else {
                        emitter.emit_line(&format!("}} else if ({poll_call}) {{"));
                    }
                    emitter.indent();

                    // Emit arm body
                    self.emit_async_stmts(&arms[i].body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);

                    emitter.dedent();
                }

                // else branch
                emitter.emit_line("} else {");
                emitter.indent();
                if let Some(else_body) = else_arm {
                    // Non-blocking: execute else body
                    self.emit_async_stmts(&else_body.stmts, inner_c_type, state_idx, sub_idx, drop_entries, emitter);
                } else {
                    // Blocking: register waker with all channels, suspend
                    for (kind, ch, field) in &arm_infos {
                        let register_call = match *kind {
                            "recv" => format!("gorget_channel_poll_recv({ch}, &__self->{field}, __waker);"),
                            "send" => format!("gorget_channel_poll_send({ch}, &__self->{field}, __waker);"),
                            _ => unreachable!(),
                        };
                        emitter.emit_line(&register_call);
                    }
                    emitter.emit_line("return GORGET_POLL_PENDING;");
                }
                emitter.dedent();
                emitter.emit_line("}");
            }

            // Break: set break-flag if inside async for/else, then break
            Stmt::Break(_) => {
                if let Some(flag) = &self.async_break_flag {
                    emitter.emit_line(&format!("{flag} = true;"));
                }
                emitter.emit_line("break;");
            }
            Stmt::Continue => {
                emitter.emit_line("continue;");
            }

            _ => {
                // Other statements: delegate to gen_stmt
                self.gen_stmt(stmt, span, emitter);
            }
        }
    }

    /// Emit the constructor function for an async function.
    fn emit_async_constructor(
        &self,
        func_name: &str,
        state_name: &str,
        future_type: &str,
        poll_name: &str,
        f: &FunctionDef,
        emitter: &mut CEmitter,
    ) {
        // Build parameter list
        let mut params_vec: Vec<String> = Vec::new();
        for p in &f.params {
            if p.node.name.node == "self" {
                continue;
            }
            let c_type = c_types::ast_type_to_c(&p.node.type_.node, self.scopes);
            let escaped = c_mangle::escape_keyword(&p.node.name.node);
            params_vec.push(format!("{c_type} {escaped}"));
        }
        let params_str = if params_vec.is_empty() {
            "void".to_string()
        } else {
            params_vec.join(", ")
        };

        emitter.emit_line(&format!("{future_type} {func_name}({params_str}) {{"));
        emitter.indent();
        emitter.emit_line(&format!(
            "{state_name}* __s = ({state_name}*)GORGET_ALLOC(sizeof({state_name}));"
        ));
        emitter.emit_line(&format!("memset(__s, 0, sizeof({state_name}));"));

        // Copy parameters into state struct
        for p in &f.params {
            if p.node.name.node == "self" {
                continue;
            }
            let escaped = c_mangle::escape_keyword(&p.node.name.node);
            emitter.emit_line(&format!("__s->{escaped} = {escaped};"));
        }

        emitter.emit_line(&format!(
            "return ({future_type}){{.poll = {poll_name}, .state = __s}};"
        ));
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }

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
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
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
            Stmt::Select { arms, else_arm } => {
                for arm in arms {
                    match &arm.op {
                        SelectOp::Recv { channel, .. } => self.scan_expr_for_tuples(channel),
                        SelectOp::Send { channel, value } => {
                            self.scan_expr_for_tuples(channel);
                            self.scan_expr_for_tuples(value);
                        }
                    }
                    self.scan_block_for_tuples(&arm.body);
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
            Type::Function { return_type, params, .. } => {
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
        trait_type_args: &[String],
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
            self.function_signature(method, Some((type_name, Some(trait_name), trait_type_args)));

        // Build the target function name (for the field's type)
        let target_fn = c_mangle::mangle_trait_method(trait_name, &field_type_name, &method.name.node, trait_type_args);

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

    /// Compute the C signature for a monomorphized equip method without emitting anything.
    /// Returns `(ret_type, func_name, params, type_subs)`.
    fn monomorphized_equip_signature(
        &self,
        method: &FunctionDef,
        struct_generic_params: Option<&crate::span::Spanned<GenericParams>>,
        c_type_args: &[String],
        mangled_type_name: &str,
        trait_name: Option<&str>,
        trait_type_args: &[String],
    ) -> (String, String, String, Vec<(String, String)>) {
        let subs = self.build_type_substitutions(struct_generic_params, c_type_args);

        let ret_type = self.substitute_type(&method.return_type.node, &subs);

        let func_name = if let Some(tname) = trait_name {
            c_mangle::mangle_trait_method(tname, mangled_type_name, &method.name.node, trait_type_args)
        } else {
            c_mangle::mangle_method(mangled_type_name, &method.name.node)
        };

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

        (ret_type, func_name, params, subs)
    }

    /// Emit a monomorphized method from a generic equip block.
    fn emit_monomorphized_equip_method(
        &mut self,
        method: &FunctionDef,
        struct_generic_params: Option<&crate::span::Spanned<GenericParams>>,
        c_type_args: &[String],
        mangled_type_name: &str,
        trait_name: Option<&str>,
        trait_type_args: &[String],
        emitter: &mut CEmitter,
    ) {
        let (ret_type, func_name, params, subs) = self.monomorphized_equip_signature(
            method, struct_generic_params, c_type_args, mangled_type_name, trait_name, trait_type_args,
        );
        let id_subs = self.build_type_id_substitutions(struct_generic_params, c_type_args);

        // Build param_c_types for method parameters (excluding self)
        let param_c_types: Vec<(String, String)> = method.params.iter()
            .filter(|p| p.node.name.node != "self")
            .map(|p| {
                let name = super::c_mangle::escape_keyword(&p.node.name.node);
                let c_type = self.substitute_type(&p.node.type_.node, &subs);
                (name, c_type)
            })
            .collect();

        // Activate substitutions and self type for body codegen
        let prev_subs = std::mem::replace(&mut self.type_subs, subs);
        let prev_id_subs = std::mem::replace(&mut self.type_id_subs, id_subs);
        let prev_param_c_types = std::mem::replace(&mut self.monomorphized_param_c_types, param_c_types);
        let prev_self_type = self.current_self_type.take();
        self.current_self_type = Some(mangled_type_name.to_string());
        let prev_return_c_type = self.current_function_return_c_type.take();
        self.current_function_return_c_type = Some(ret_type.clone());
        let prev_self_is_mutable = self.self_is_mutable;
        let self_param = method.params.iter().find(|p| p.node.name.node == "self");
        self.self_is_mutable = self_param
            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
            .unwrap_or(false);

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
                let e = self.coerce_return_value(e, &expr.node);
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
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }

        self.type_subs = prev_subs;
        self.type_id_subs = prev_id_subs;
        self.monomorphized_param_c_types = prev_param_c_types;
        self.current_self_type = prev_self_type;
        self.current_function_return_c_type = prev_return_c_type;
        self.self_is_mutable = prev_self_is_mutable;
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
                            "Str" => Type::Primitive(PrimitiveType::Str),
                            "const char*" => Type::Primitive(PrimitiveType::CStr),
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

    /// Extract the trait's generic type arguments as C type strings from an equip block.
    /// Returns an empty Vec for non-generic traits (e.g. `Displayable`).
    /// Returns `["int64_t"]` for `From[int]`, `["const char*"]` for `From[str]`, etc.
    fn impl_trait_type_args(&self, impl_block: &EquipBlock) -> Vec<String> {
        impl_block.trait_.as_ref().map_or_else(Vec::new, |t| {
            if let Type::Named { generic_args, .. } = &t.trait_name.node {
                generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect()
            } else {
                Vec::new()
            }
        })
    }

    /// Extract the trait's generic type arguments as raw AST types (for monomorphization
    /// where we need to apply type substitutions before converting to C).
    fn impl_trait_type_args_raw(impl_block: &EquipBlock) -> Vec<Type> {
        impl_block.trait_.as_ref().map_or_else(Vec::new, |t| {
            if let Type::Named { generic_args, .. } = &t.trait_name.node {
                generic_args.iter().map(|a| a.node.clone()).collect()
            } else {
                Vec::new()
            }
        })
    }

    // ─── Test Runner ────────────────────────────────────────────

    /// Check whether a test definition should run based on name filter,
    /// exclude-tag, and include-tag filters.
    fn should_run_test(&self, test: &crate::parser::ast::TestDef) -> bool {
        // Name filter: skip if test name doesn't contain substring
        if let Some(ref filter) = self.test_name_filter {
            if !test.name.node.contains(filter.as_str()) {
                return false;
            }
        }
        // Exclusion wins: if any tag is excluded, skip
        if !self.test_exclude_tags.is_empty() {
            for attr in &test.attributes {
                if attr.node.name.node == "tag" {
                    for arg in &attr.node.args {
                        if let crate::parser::ast::AttributeArg::StringLiteral(s) = arg {
                            if self.test_exclude_tags.contains(s) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
        // Inclusion: if --tag was specified, only run matching tests
        if !self.test_tag_filter.is_empty() {
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
            return false;
        }
        true
    }

    /// Emit the test runner `main()` function for test modules.
    pub fn emit_test_runner_main(&mut self, module: &crate::parser::ast::Module, emitter: &mut CEmitter) {
        // Pre-count matching tests at compile time
        let test_count = module.items.iter().filter(|item| {
            if let Item::Test(t) = &item.node {
                self.should_run_test(t)
            } else {
                false
            }
        }).count();

        emitter.emit_line("// ── Test Runner ──");
        emitter.emit_line("int main(int argc, char** argv) {");
        emitter.indent();
        emitter.emit_line("gorget_init_args(argc, argv);");
        if self.trace {
            let trace_path = self.trace_filename.replace('\\', "\\\\").replace('"', "\\\"");
            emitter.emit_line(&format!(
                "__gorget_trace_init(\"{trace_path}\");"
            ));
        }
        emitter.emit_line("int __test_passed = 0, __test_failed = 0;");
        emitter.emit_line("struct timespec __total_start, __total_end;");
        emitter.emit_line("clock_gettime(CLOCK_MONOTONIC, &__total_start);");
        emitter.emit_line(&format!("printf(\"Running {test_count} tests...\\n\");"));
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
                if !self.should_run_test(t) {
                    continue;
                }
                let test_name = &t.name.node;
                let escaped_name = test_name.replace('\\', "\\\\").replace('"', "\\\"");

                // Detect @should_panic attribute
                let should_panic = t.attributes.iter().any(|a| a.node.name.node == "should_panic");
                let expected_msg: Option<&str> = t.attributes.iter()
                    .find(|a| a.node.name.node == "should_panic")
                    .and_then(|a| a.node.args.first())
                    .and_then(|arg| if let crate::parser::ast::AttributeArg::StringLiteral(s) = arg { Some(s.as_str()) } else { None });

                if self.trace {
                    emitter.emit_line(&format!(
                        "fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_start\\\",\\\"name\\\":\\\"{escaped_name}\\\"}}\\n\");"
                    ));
                }
                emitter.emit_line(&format!("printf(\"  test: {escaped_name} ... \");"));
                emitter.emit_line("fflush(stdout);");
                emitter.emit_line("{");
                emitter.indent();
                emitter.emit_line("__gorget_in_test = 1;");
                emitter.emit_line("__gorget_test_fail_msg = NULL;");
                emitter.emit_line("int __cleanup_mark = __gorget_cleanup_top;");

                // Timing start
                emitter.emit_line("struct timespec __t_start, __t_end;");
                emitter.emit_line("clock_gettime(CLOCK_MONOTONIC, &__t_start);");

                emitter.emit_line("if (setjmp(__gorget_test_jmp) == 0) {");
                emitter.indent();
                self.in_test_body = true;
                self.push_drop_scope(DropScopeKind::Function);

                // Emit with-binding declarations
                for binding in &t.with_bindings {
                    let val = self.gen_expr(&binding.expr);
                    let escaped = c_mangle::escape_keyword(&binding.name.node);
                    let c_type = if let Some(def_id) = self.scoped_lookup(&binding.name.node) {
                        let def = self.scopes.get_def(def_id);
                        if let Some(type_id) = def.type_id {
                            c_types::type_id_to_c(type_id, self.types, self.scopes)
                        } else {
                            self.infer_c_type_from_expr(&binding.expr.node)
                        }
                    } else {
                        self.infer_c_type_from_expr(&binding.expr.node)
                    };
                    let decl = c_types::c_declare(&c_type, &escaped);
                    emitter.emit_line(&format!("{decl} = {val};"));
                    if self.traits.has_trait_impl_by_name(&c_type, "Drop") {
                        let drop_fn = c_mangle::mangle_trait_method("Drop", &c_type, "drop", &[]);
                        self.register_droppable(
                            &escaped,
                            DropAction::UserDrop { type_name: c_type.clone() },
                        );
                        emitter.emit_line(&format!(
                            "__gorget_cleanup_push((__gorget_cleanup_fn){drop_fn}, (void*)&{escaped});"
                        ));
                    }
                }

                self.gen_block(&t.body, emitter);
                self.pop_drop_scope(emitter);
                self.in_test_body = false;
                emitter.emit_line("__gorget_cleanup_top = __cleanup_mark;");
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line("__gorget_cleanup_run(__cleanup_mark);");
                emitter.emit_line("__gorget_in_test = 0;");

                // Timing end
                emitter.emit_line("clock_gettime(CLOCK_MONOTONIC, &__t_end);");
                emitter.emit_line("long __t_ms = (__t_end.tv_sec - __t_start.tv_sec) * 1000 + (__t_end.tv_nsec - __t_start.tv_nsec) / 1000000;");

                // Pass/fail logic (inverted for @should_panic)
                let trace_pass = if self.trace {
                    format!(
                        "fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped_name}\\\",\\\"status\\\":\\\"pass\\\",\\\"duration_ms\\\":%ld}}\\n\", __t_ms);"
                    )
                } else {
                    String::new()
                };
                let trace_fail = if self.trace {
                    format!(
                        "fprintf(__gorget_trace_fp, \"{{\\\"type\\\":\\\"test_end\\\",\\\"name\\\":\\\"{escaped_name}\\\",\\\"status\\\":\\\"fail\\\",\\\"duration_ms\\\":%ld}}\\n\", __t_ms);"
                    )
                } else {
                    String::new()
                };

                if should_panic {
                    if let Some(msg) = expected_msg {
                        let escaped_msg = msg.replace('\\', "\\\\").replace('"', "\\\"");
                        emitter.emit_line(&format!(
                            "if (__gorget_test_fail_msg && strstr(__gorget_test_fail_msg, \"{escaped_msg}\")) {{"
                        ));
                        emitter.indent();
                        emitter.emit_line("__test_passed++;");
                        if self.trace { emitter.emit_line(&trace_pass); }
                        emitter.emit_line("printf(\"PASS (%ldms)\\n\", __t_ms);");
                        emitter.dedent();
                        emitter.emit_line("} else if (__gorget_test_fail_msg) {");
                        emitter.indent();
                        emitter.emit_line("__test_failed++;");
                        if self.trace { emitter.emit_line(&trace_fail); }
                        emitter.emit_line(&format!(
                            "printf(\"FAIL: expected panic containing \\\"{escaped_msg}\\\", got: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);"
                        ));
                        emitter.dedent();
                        emitter.emit_line("} else {");
                        emitter.indent();
                        emitter.emit_line("__test_failed++;");
                        if self.trace { emitter.emit_line(&trace_fail); }
                        emitter.emit_line("printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                        emitter.dedent();
                        emitter.emit_line("}");
                    } else {
                        emitter.emit_line("if (__gorget_test_fail_msg) {");
                        emitter.indent();
                        emitter.emit_line("__test_passed++;");
                        if self.trace { emitter.emit_line(&trace_pass); }
                        emitter.emit_line("printf(\"PASS (%ldms)\\n\", __t_ms);");
                        emitter.dedent();
                        emitter.emit_line("} else {");
                        emitter.indent();
                        emitter.emit_line("__test_failed++;");
                        if self.trace { emitter.emit_line(&trace_fail); }
                        emitter.emit_line("printf(\"FAIL: expected panic but test passed (%ldms)\\n\", __t_ms);");
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                } else {
                    emitter.emit_line("if (!__gorget_test_fail_msg) {");
                    emitter.indent();
                    emitter.emit_line("__test_passed++;");
                    if self.trace { emitter.emit_line(&trace_pass); }
                    emitter.emit_line("printf(\"PASS (%ldms)\\n\", __t_ms);");
                    emitter.dedent();
                    emitter.emit_line("} else {");
                    emitter.indent();
                    emitter.emit_line("__test_failed++;");
                    if self.trace { emitter.emit_line(&trace_fail); }
                    emitter.emit_line("printf(\"FAIL: %s (%ldms)\\n\", __gorget_test_fail_msg, __t_ms);");
                    emitter.dedent();
                    emitter.emit_line("}");
                }

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

        emitter.emit_line("clock_gettime(CLOCK_MONOTONIC, &__total_end);");
        emitter.emit_line("long __total_ms = (__total_end.tv_sec - __total_start.tv_sec) * 1000 + (__total_end.tv_nsec - __total_start.tv_nsec) / 1000000;");
        emitter.emit_line("printf(\"\\n%d passed, %d failed (%ldms)\\n\", __test_passed, __test_failed, __total_ms);");
        emitter.emit_line("return __test_failed > 0 ? 1 : 0;");
        emitter.dedent();
        emitter.emit_line("}");
        emitter.blank_line();
    }
}
