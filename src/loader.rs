use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::errors::ParseError;
use crate::parser::ast::{
    Block, CallArg, Expr, FunctionBody, FunctionDef, ImportStmt, Item, Module,
    Pattern, Stmt,
};
use crate::parser::Parser;
use crate::span::{Span, Spanned};

// ══════════════════════════════════════════════════════════════
// Errors
// ══════════════════════════════════════════════════════════════

#[derive(Debug)]
pub enum LoadError {
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    Cycle {
        path: PathBuf,
        chain: Vec<PathBuf>,
    },
    Parse {
        path: PathBuf,
        errors: Vec<ParseError>,
        source: String,
    },
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Io { path, error } => {
                write!(f, "cannot read '{}': {}", path.display(), error)
            }
            LoadError::Cycle { path, chain } => {
                let chain_str: Vec<_> = chain.iter().map(|p| p.display().to_string()).collect();
                write!(
                    f,
                    "import cycle detected: {} -> {}",
                    chain_str.join(" -> "),
                    path.display()
                )
            }
            LoadError::Parse { path, .. } => {
                write!(f, "parse errors in '{}'", path.display())
            }
        }
    }
}

// ══════════════════════════════════════════════════════════════
// Module Loader
// ══════════════════════════════════════════════════════════════

pub struct ModuleLoader {
    loaded: HashSet<PathBuf>,
    load_stack: Vec<PathBuf>,
    /// Package name → source directory, populated from resolved dependencies.
    dep_paths: HashMap<String, PathBuf>,
    /// Next byte offset for module span uniqueness. Each module is parsed at
    /// a cumulative offset so that all span-keyed maps work across modules.
    next_offset: usize,
}

/// Map a dotted import path to a filesystem path.
/// `["foo", "bar"]` relative to `base` → `<base>/foo/bar.gg`
pub fn resolve_import_path(base: &Path, segments: &[String]) -> PathBuf {
    let mut path = base.to_path_buf();
    for seg in segments {
        path = path.join(seg);
    }
    path.with_extension("gg")
}

/// Extract all import paths from a parsed module.
/// Returns `(dotted_path_segments, span)` for each import.
pub fn extract_imports(module: &Module) -> Vec<(Vec<String>, Span)> {
    let mut imports = Vec::new();
    for item in &module.items {
        if let Item::Import(import) = &item.node {
            match import {
                ImportStmt::Simple { path, span } => {
                    let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                    imports.push((segments, *span));
                }
                ImportStmt::Grouped { path, span, .. } => {
                    // `import a.b.{X, Y}` — the module to load is `a.b`
                    let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                    imports.push((segments, *span));
                }
                ImportStmt::From { path, span, .. } => {
                    // `from a.b import X` — the module to load is `a.b`
                    let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                    imports.push((segments, *span));
                }
            }
        }
    }
    imports
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            loaded: HashSet::new(),
            load_stack: Vec::new(),
            dep_paths: HashMap::new(),
            next_offset: 0,
        }
    }

    /// Create a loader with package dependency paths.
    pub fn with_dep_paths(dep_paths: HashMap<String, PathBuf>) -> Self {
        Self {
            loaded: HashSet::new(),
            load_stack: Vec::new(),
            dep_paths,
            next_offset: 0,
        }
    }

    /// Recursively load a module and all its imports.
    ///
    /// `entry` is the path to the main `.gg` file. `entry_source` and `entry_module`
    /// are the already-read source and parsed AST for the entry file (to avoid re-parsing).
    ///
    /// Returns `(path, logical_path, source, module)` quads for all loaded files, with the
    /// entry first. `logical_path` is the import path segments (e.g. `["gg", "csv"]`); empty
    /// for the entry module.
    pub fn load_all(
        &mut self,
        entry: &Path,
        entry_source: String,
        entry_module: Module,
    ) -> Result<Vec<(PathBuf, Vec<String>, String, Module)>, LoadError> {
        let canonical = entry
            .canonicalize()
            .map_err(|e| LoadError::Io {
                path: entry.to_path_buf(),
                error: e,
            })?;

        let mut results = Vec::new();
        self.loaded.insert(canonical.clone());
        self.load_stack.push(canonical.clone());

        // Entry module was parsed at offset 0; next module starts after it (+1 separator)
        self.next_offset = entry_source.len() + 1;

        // Collect imports from the entry module
        let imports = extract_imports(&entry_module);
        let base_dir = canonical.parent().unwrap().to_path_buf();

        // Entry module has an empty logical path.
        results.push((canonical.clone(), Vec::new(), entry_source, entry_module));

        // Recursively load each import
        for (segments, _span) in imports {
            self.load_recursive(&base_dir, &segments, &mut results)?;
        }

        self.load_stack.pop();
        Ok(results)
    }

    fn load_recursive(
        &mut self,
        base_dir: &Path,
        segments: &[String],
        results: &mut Vec<(PathBuf, Vec<String>, String, Module)>,
    ) -> Result<(), LoadError> {
        // Intercept virtual built-in modules (std.* and gg.*) before filesystem resolution
        if crate::stdlib::is_builtin_module(segments) {
            let virtual_path = PathBuf::from(format!("<{}>", segments.join(".")));
            if self.loaded.contains(&virtual_path) {
                return Ok(());
            }

            // Try synthetic (compiler-generated) module first
            if let Some(module) = crate::stdlib::generate_builtin_module(segments) {
                self.loaded.insert(virtual_path.clone());
                results.push((virtual_path, segments.to_vec(), String::new(), module));
                return Ok(());
            }

            // Try file-based built-in module (real .gg source embedded in binary)
            if let Some(source) = crate::stdlib::builtin_module_source(segments) {
                let offset = self.next_offset;
                self.next_offset = offset + source.len() + 1;
                let mut parser = Parser::new_with_offset(source, offset);
                let module = parser.parse_module();
                assert!(
                    parser.errors.is_empty(),
                    "parse errors in embedded built-in module {}: {:?}",
                    segments.join("."),
                    parser.errors
                );

                self.loaded.insert(virtual_path.clone());
                self.load_stack.push(virtual_path.clone());

                // Recurse into this module's imports FIRST (post-order) so that
                // dependency structs appear before the structs that use them.
                let imports = extract_imports(&module);
                for (segs, _span) in imports {
                    self.load_recursive(base_dir, &segs, results)?;
                }
                results.push((virtual_path.clone(), segments.to_vec(), source.to_string(), module));

                self.load_stack.pop();
                return Ok(());
            }
        }

        // Try local filesystem first
        let file_path = resolve_import_path(base_dir, segments);

        // If the local file doesn't exist, try package dependencies.
        // The first segment of the import is the package name (e.g. `import mylib`
        // or `from mylib import foo`).
        let file_path = if !file_path.exists() {
            if let Some(dep_dir) = segments.first().and_then(|name| self.dep_paths.get(name.as_str())) {
                if segments.len() == 1 {
                    // `import mylib` → look for `<dep_dir>/<mylib>.gg`
                    let pkg_file = dep_dir.join(format!("{}.gg", segments[0]));
                    if pkg_file.exists() {
                        pkg_file
                    } else {
                        file_path
                    }
                } else {
                    // `from mylib.sub import X` → resolve sub-path within dep dir
                    resolve_import_path(dep_dir, &segments[1..])
                }
            } else {
                file_path
            }
        } else {
            file_path
        };

        let canonical = file_path.canonicalize().map_err(|e| LoadError::Io {
            path: file_path.clone(),
            error: e,
        })?;

        // Skip if already loaded
        if self.loaded.contains(&canonical) {
            return Ok(());
        }

        // Cycle detection
        if self.load_stack.contains(&canonical) {
            return Err(LoadError::Cycle {
                path: canonical,
                chain: self.load_stack.clone(),
            });
        }

        // Read and parse
        let source = fs::read_to_string(&file_path).map_err(|e| LoadError::Io {
            path: file_path.clone(),
            error: e,
        })?;

        let offset = self.next_offset;
        self.next_offset = offset + source.len() + 1;
        let mut parser = Parser::new_with_offset(&source, offset);
        let module = parser.parse_module();

        if !parser.errors.is_empty() {
            return Err(LoadError::Parse {
                path: file_path,
                errors: parser.errors,
                source,
            });
        }

        self.loaded.insert(canonical.clone());
        self.load_stack.push(canonical.clone());

        // Collect imports from this module and recurse FIRST (post-order)
        // so that dependency structs appear before the structs that use them.
        let imports = extract_imports(&module);
        let this_dir = canonical.parent().unwrap().to_path_buf();

        for (segs, _span) in imports {
            self.load_recursive(&this_dir, &segs, results)?;
        }
        results.push((canonical.clone(), segments.to_vec(), source, module));

        self.load_stack.pop();
        Ok(())
    }
}

// ══════════════════════════════════════════════════════════════
// Variant Qualification Rewrite
// ══════════════════════════════════════════════════════════════

/// Rewrite bare enum variant references in an imported module to qualified form.
///
/// For a module that defines `enum LogLevel: Debug, Info, Warn, Error`, this
/// rewrites:
///   - `Expr::Call { callee: Identifier("Debug"), args }` → `MethodCall { receiver: Identifier("LogLevel"), method: "Debug", args }`
///   - `Pattern::Constructor { path: ["Debug"], fields }` → `path: ["LogLevel", "Debug"]`
///   - `Pattern::Binding("Debug")` (unit variant) → `Pattern::Constructor { path: ["LogLevel", "Debug"], fields: [] }`
///
/// This runs BEFORE merge so that imported modules' internal variant references
/// are already qualified when semantic analysis sees them.
/// Prelude variant names that must never be overridden by user enum auto-qualification.
const PRELUDE_VARIANTS: &[&str] = &["Ok", "Error", "Some", "None"];

/// Build a variant_name → enum_name map from a single module's non-generic enum definitions,
/// excluding prelude names.
fn build_variant_map_from_module(module: &Module) -> HashMap<String, String> {
    let mut vm = HashMap::new();
    for item in &module.items {
        if let Item::Enum(e) = &item.node {
            if e.generic_params.is_some() {
                continue;
            }
            let enum_name = e.name.node.clone();
            for variant in &e.variants {
                let vname = variant.node.name.node.clone();
                if !PRELUDE_VARIANTS.contains(&vname.as_str()) {
                    vm.insert(vname, enum_name.clone());
                }
            }
        }
    }
    vm
}

/// Build a global variant_name → enum_name map from ALL modules (for cross-module qualification).
fn build_variant_map_from_all(modules: &[(PathBuf, String, Module)]) -> HashMap<String, String> {
    let mut vm = HashMap::new();
    for (_, _, module) in modules {
        for item in &module.items {
            if let Item::Enum(e) = &item.node {
                if e.generic_params.is_some() {
                    continue;
                }
                let enum_name = e.name.node.clone();
                for variant in &e.variants {
                    let vname = variant.node.name.node.clone();
                    if !PRELUDE_VARIANTS.contains(&vname.as_str()) {
                        // First-writer-wins: the defining module takes priority
                        vm.entry(vname).or_insert_with(|| enum_name.clone());
                    }
                }
            }
        }
    }
    vm
}

/// Rewrite bare variant references in a module using the provided variant map.
fn qualify_module_with_map(module: &mut Module, vm: &HashMap<String, String>) {
    if vm.is_empty() {
        return;
    }
    for item in &mut module.items {
        qualify_item(&mut item.node, vm);
    }
}

/// Rewrite bare variant references in a module using its own enum definitions.
/// Used for backward compatibility in non-merge contexts.
pub fn qualify_variant_refs(module: &mut Module) {
    let vm = build_variant_map_from_module(module);
    qualify_module_with_map(module, &vm);
}

fn qualify_item(item: &mut Item, vm: &HashMap<String, String>) {
    match item {
        Item::Function(f) => qualify_function(f, vm),
        Item::Equip(eq) => {
            for method in &mut eq.items {
                qualify_function(&mut method.node, vm);
            }
        }
        Item::ConstDecl(c) => qualify_expr(&mut c.value, vm),
        Item::StaticDecl(s) => qualify_expr(&mut s.value, vm),
        Item::Test(t) => {
            for binding in &mut t.with_bindings {
                qualify_expr(&mut binding.expr, vm);
            }
            qualify_block(&mut t.body, vm);
        }
        Item::SuiteSetup(s) => qualify_block(&mut s.body, vm),
        Item::SuiteTeardown(s) => qualify_block(&mut s.body, vm),
        _ => {}
    }
}

fn qualify_function(f: &mut FunctionDef, vm: &HashMap<String, String>) {
    for param in &mut f.params {
        if let Some(default) = &mut param.node.default {
            qualify_expr(default, vm);
        }
    }
    match &mut f.body {
        FunctionBody::Block(block) => qualify_block(block, vm),
        FunctionBody::Expression(expr) => qualify_expr(expr, vm),
        FunctionBody::Declaration | FunctionBody::Extern(_) => {}
    }
}

fn qualify_block(block: &mut Block, vm: &HashMap<String, String>) {
    for stmt in &mut block.stmts {
        qualify_stmt(&mut stmt.node, vm);
    }
}

fn qualify_stmt(stmt: &mut Stmt, vm: &HashMap<String, String>) {
    match stmt {
        Stmt::VarDecl { value, .. } => qualify_expr(value, vm),
        Stmt::Assign { target, value } => {
            qualify_expr(target, vm);
            qualify_expr(value, vm);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            qualify_expr(target, vm);
            qualify_expr(value, vm);
        }
        Stmt::Expr(e) => qualify_expr(e, vm),
        Stmt::Return(Some(e)) => qualify_expr(e, vm),
        Stmt::Return(None) => {}
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            qualify_expr(condition, vm);
            qualify_block(then_body, vm);
            for (cond, body) in elif_branches {
                qualify_expr(cond, vm);
                qualify_block(body, vm);
            }
            if let Some(e) = else_body {
                qualify_block(e, vm);
            }
        }
        Stmt::While { condition, body, else_body } => {
            qualify_expr(condition, vm);
            qualify_block(body, vm);
            if let Some(e) = else_body {
                qualify_block(e, vm);
            }
        }
        Stmt::Loop { body } => qualify_block(body, vm),
        Stmt::For { iterable, body, else_body, .. } => {
            qualify_expr(iterable, vm);
            qualify_block(body, vm);
            if let Some(e) = else_body {
                qualify_block(e, vm);
            }
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            qualify_expr(scrutinee, vm);
            for arm in arms {
                qualify_pattern(&mut arm.pattern, vm);
                if let Some(guard) = &mut arm.guard {
                    qualify_expr(guard, vm);
                }
                qualify_expr(&mut arm.body, vm);
            }
            if let Some(b) = else_arm {
                qualify_block(b, vm);
            }
        }
        Stmt::Throw(e) => qualify_expr(e, vm),
        Stmt::Break(_) | Stmt::Continue | Stmt::Pass => {}
        Stmt::With { bindings, body } => {
            for b in bindings { qualify_expr(&mut b.expr, vm); }
            qualify_block(body, vm);
        }
        Stmt::Assert { condition, message } => {
            qualify_expr(condition, vm);
            if let Some(m) = message { qualify_expr(m, vm); }
        }
        Stmt::Unsafe { body } => qualify_block(body, vm),
        Stmt::Item(_) | Stmt::Select { .. } => {}
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            qualify_expr(condition, vm);
            qualify_block(then_body, vm);
            for (cond, body) in elif_branches {
                qualify_expr(cond, vm);
                qualify_block(body, vm);
            }
            if let Some(eb) = else_body {
                qualify_block(eb, vm);
            }
        }
        Stmt::MetaFor { range, body, .. } => {
            qualify_expr(range, vm);
            qualify_block(body, vm);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            qualify_expr(scrutinee, vm);
            // Case exprs are meta literals — no qualification needed; qualify bodies only.
            for (_, body) in arms {
                qualify_block(body, vm);
            }
            if let Some(eb) = else_arm {
                qualify_block(eb, vm);
            }
        }
        Stmt::MetaWhile { condition, body, .. } => {
            qualify_expr(condition, vm);
            qualify_block(body, vm);
        }
    }
}

fn qualify_expr(expr: &mut Spanned<Expr>, vm: &HashMap<String, String>) {
    match &mut expr.node {
        // Leaves
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::CharLiteral(_)
        | Expr::NoneLiteral
        | Expr::SelfExpr
        | Expr::It
        | Expr::StringLiteral(_) => {}

        Expr::Path { .. } => {}

        Expr::Identifier(_) => {
            // Standalone identifier that is a user enum variant → rewrite to Path
            // (e.g., unit variants used as expressions in non-call position)
            // We detect the pattern in the Call handler below; nothing to do here.
        }

        // The key rewrite: bare variant call → qualified method call
        Expr::Call { callee, args, .. } => {
            // Rewrite args first
            for arg in args.iter_mut() {
                qualify_expr(&mut arg.node.value, vm);
            }
            // Then check if callee is a bare variant identifier
            if let Expr::Identifier(name) = &callee.node {
                if let Some(enum_name) = vm.get(name.as_str()) {
                    let variant_name = name.clone();
                    let callee_span = callee.span;
                    let enum_name = enum_name.clone();
                    // Rewrite: Call { Identifier(v), args } → MethodCall { Identifier(E), v, args }
                    let new_args: Vec<Spanned<CallArg>> = std::mem::take(args);
                    expr.node = Expr::MethodCall {
                        receiver: Box::new(Spanned::new(
                            Expr::Identifier(enum_name),
                            callee_span,
                        )),
                        method: Spanned::new(variant_name, callee_span),
                        args: new_args,
                        generic_args: None,
                    };
                    return;
                }
            }
            // Also recurse into callee in the non-rewrite case
            qualify_expr(callee, vm);
        }

        Expr::MethodCall { receiver, args, .. } => {
            qualify_expr(receiver, vm);
            for arg in args.iter_mut() {
                qualify_expr(&mut arg.node.value, vm);
            }
        }

        Expr::BinaryOp { left, right, .. } => {
            qualify_expr(left, vm);
            qualify_expr(right, vm);
        }

        Expr::UnaryOp { operand, .. } => qualify_expr(operand, vm),

        Expr::FieldAccess { object, .. } => qualify_expr(object, vm),
        Expr::TupleFieldAccess { object, .. } => qualify_expr(object, vm),
        Expr::Index { object, index } => {
            qualify_expr(object, vm);
            qualify_expr(index, vm);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start { qualify_expr(s, vm); }
            if let Some(e) = end { qualify_expr(e, vm); }
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            qualify_expr(condition, vm);
            qualify_expr(then_branch, vm);
            for (cond, body) in elif_branches {
                qualify_expr(cond, vm);
                qualify_expr(body, vm);
            }
            if let Some(e) = else_branch { qualify_expr(e, vm); }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            qualify_expr(scrutinee, vm);
            for arm in arms {
                qualify_pattern(&mut arm.pattern, vm);
                if let Some(g) = &mut arm.guard { qualify_expr(g, vm); }
                qualify_expr(&mut arm.body, vm);
            }
            if let Some(e) = else_arm { qualify_expr(e, vm); }
        }
        Expr::Block(block) => qualify_block(block, vm),
        Expr::Do { body } => qualify_block(body, vm),
        Expr::Try { expr: inner }
        | Expr::Move { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner }
        | Expr::Spawn { expr: inner }
        | Expr::TryCapture { expr: inner } => qualify_expr(inner, vm),
        Expr::NilCoalescing { lhs, rhs } => {
            qualify_expr(lhs, vm);
            qualify_expr(rhs, vm);
        }
        Expr::OptionalChain { object, .. } => qualify_expr(object, vm),
        Expr::Is { expr: inner, pattern, .. } => {
            qualify_expr(inner, vm);
            qualify_pattern(pattern, vm);
        }
        Expr::As { expr: inner, .. } => qualify_expr(inner, vm),
        Expr::Closure { body, .. } => qualify_expr(body, vm),
        Expr::ImplicitClosure { body } => qualify_expr(body, vm),
        Expr::ArrayLiteral(elems) | Expr::TupleLiteral(elems) => {
            for e in elems { qualify_expr(e, vm); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                qualify_expr(k, vm);
                qualify_expr(v, vm);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for v in args { qualify_expr(v, vm); }
        }
        Expr::ListComprehension { expr: inner, iterable, condition, .. } => {
            qualify_expr(inner, vm);
            qualify_expr(iterable, vm);
            if let Some(c) = condition { qualify_expr(c, vm); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            qualify_expr(key, vm);
            qualify_expr(value, vm);
            qualify_expr(iterable, vm);
            if let Some(c) = condition { qualify_expr(c, vm); }
        }
        Expr::SetComprehension { expr: inner, iterable, condition, .. } => {
            qualify_expr(inner, vm);
            qualify_expr(iterable, vm);
            if let Some(c) = condition { qualify_expr(c, vm); }
        }

        // Dot-shorthand: recurse into args but don't rewrite — enum name resolved at type-check time
        Expr::DotShorthand { args, .. } => {
            for arg in args.iter_mut() {
                qualify_expr(&mut arg.node.value, vm);
            }
        }
    }
}

fn qualify_pattern(pattern: &mut Spanned<Pattern>, vm: &HashMap<String, String>) {
    match &mut pattern.node {
        Pattern::Binding(name) => {
            // Bare uppercase identifier that is an enum variant → unit variant Constructor
            if let Some(enum_name) = vm.get(name.as_str()) {
                let span = pattern.span;
                let vname = name.clone();
                let ename = enum_name.clone();
                pattern.node = Pattern::Constructor {
                    path: vec![
                        Spanned::new(ename, span),
                        Spanned::new(vname, span),
                    ],
                    fields: vec![],
                };
            }
        }
        Pattern::Constructor { path, fields } => {
            // If already qualified, leave as-is
            if path.len() == 1 {
                let vname = path[0].node.clone();
                if let Some(enum_name) = vm.get(vname.as_str()) {
                    let span = path[0].span;
                    let ename = enum_name.clone();
                    path.insert(0, Spanned::new(ename, span));
                }
            }
            for f in fields { qualify_pattern(f, vm); }
        }
        Pattern::Or(alts) => {
            for alt in alts { qualify_pattern(alt, vm); }
        }
        Pattern::Tuple(elems) => {
            for e in elems { qualify_pattern(e, vm); }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {}

        // Dot-shorthand: recurse into fields but don't rewrite — enum name resolved at type-check time
        Pattern::DotShorthand { fields, .. } => {
            for f in fields { qualify_pattern(f, vm); }
        }
    }
}

/// Merge all loaded modules into a single `Module`.
///
/// The entry file's items come first at the top level. Each imported module's items
/// are wrapped in an `Item::Module { path, items }` node that preserves module
/// identity through the semantic pipeline. This allows the resolver to enforce
/// per-module scoping and `private` visibility.
pub fn merge_modules(modules: Vec<(PathBuf, Vec<String>, String, Module)>) -> Module {
    if modules.len() == 1 {
        // Single-file: build variant map from this module and qualify in-place.
        let (_path, _logical, _source, mut module) = modules.into_iter().next().unwrap();
        let vm = build_variant_map_from_module(&module);
        qualify_module_with_map(&mut module, &vm);
        return module;
    }

    // Multi-file: build a global variant map from ALL modules for cross-module qualification.
    // The variant map needs the raw (PathBuf, String, Module) triples, so extract them.
    let raw_for_vm: Vec<(PathBuf, String, Module)> = modules.iter()
        .map(|(p, _, s, m)| (p.clone(), s.clone(), m.clone()))
        .collect();
    let global_vm = build_variant_map_from_all(&raw_for_vm);

    let mut all_items: Vec<Spanned<Item>> = Vec::new();

    for (i, (_path, logical_path, _source, mut module)) in modules.into_iter().enumerate() {
        // Apply global qualification to every module (including entry).
        qualify_module_with_map(&mut module, &global_vm);
        if i == 0 {
            // Entry file: keep all items at the top level (including its import statements,
            // so the resolver can register imported names into the global scope).
            all_items.extend(module.items);
        } else {
            // Non-entry modules: wrap in Item::Module to preserve module identity.
            // Exclude the module's own import statements — they were used during
            // loading but must not pollute the entry module's import-resolution pass.
            let mod_items: Vec<Spanned<Item>> = module
                .items
                .into_iter()
                .filter(|item| !matches!(item.node, Item::Import(_)))
                .collect();

            let span = if let (Some(first), Some(last)) = (mod_items.first(), mod_items.last()) {
                first.span.merge(last.span)
            } else {
                Span::dummy()
            };

            all_items.push(Spanned {
                node: Item::Module {
                    path: logical_path,
                    items: mod_items,
                },
                span,
            });
        }
    }

    let span = if let (Some(first), Some(last)) = (all_items.first(), all_items.last()) {
        first.span.merge(last.span)
    } else {
        Span::dummy()
    };

    Module {
        items: all_items,
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_single_segment() {
        let base = Path::new("/project");
        let result = resolve_import_path(base, &["math".to_string()]);
        assert_eq!(result, PathBuf::from("/project/math.gg"));
    }

    #[test]
    fn resolve_multi_segment() {
        let base = Path::new("/project");
        let result = resolve_import_path(base, &["util".to_string(), "greet".to_string()]);
        assert_eq!(result, PathBuf::from("/project/util/greet.gg"));
    }

    #[test]
    fn extract_no_imports() {
        let module = Module {
            items: vec![],
            span: Span::dummy(),
        };
        assert!(extract_imports(&module).is_empty());
    }
}
