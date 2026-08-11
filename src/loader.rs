use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::errors::ParseError;
use crate::parser::ast::{
    Block, CallArg, Expr, FunctionBody, FunctionDef, ImportStmt, Item, MetaIf, Module,
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
    /// The entry file's parent directory, used as a fallback for cross-directory
    /// imports in multi-file projects (e.g. renderer/ importing from client/).
    entry_base_dir: Option<PathBuf>,
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

/// Known "root namespaces" that resolve from the project root rather
/// than the importing file's directory. Today: `compiler` (for
/// `compiler/data/{schema,resources}.gg`). Add to this list when a new
/// top-level compiler-data namespace appears (e.g. `compiler.diagnostics`).
fn is_project_root_namespace(segments: &[String]) -> bool {
    matches!(segments.first().map(String::as_str), Some("compiler"))
}

/// Walk up from `start` until we find an ancestor directory containing
/// a subdirectory named `root_dir_name`. Returns that ancestor (the
/// effective project root for resolving `<root_dir_name>.*` imports).
fn find_project_root_for(start: &Path, root_dir_name: &str) -> Option<PathBuf> {
    let mut cur: Option<&Path> = Some(start);
    while let Some(d) = cur {
        if d.join(root_dir_name).is_dir() {
            return Some(d.to_path_buf());
        }
        cur = d.parent();
    }
    None
}

/// Extract all import paths from a parsed module.
/// Returns `(dotted_path_segments, span)` for each import.
pub fn extract_imports(module: &Module) -> Vec<(Vec<String>, Span)> {
    let mut imports = Vec::new();
    extract_imports_from_items(&module.items, &mut imports);
    imports
}

/// Check whether the module uses `@derive(Hashable)` on any struct or enum —
/// the derived `equip T with Hashable: void hash(self, FxHasher &h)` needs
/// std.hash's FxHasher struct and its methods at link time.
fn module_uses_hashable_derive(module: &Module) -> bool {
    use crate::parser::ast::{AttributeArg};
    for item in &module.items {
        let attrs = match &item.node {
            Item::Struct(s) => &s.attributes,
            Item::Enum(e) => &e.attributes,
            _ => continue,
        };
        for attr in attrs {
            if attr.node.name.node != "derive" { continue; }
            for arg in &attr.node.args {
                if let AttributeArg::Identifier(name) = arg {
                    if name == "Hashable" { return true; }
                }
            }
        }
    }
    false
}

/// Check whether the module should auto-load `std.iter`.
///
/// Auto-load when:
/// - The module doesn't already import `std.iter` explicitly (harmless
///   but a useful early-exit — redundant loads are deduped elsewhere).
/// - The module doesn't define a struct / enum / type-alias / trait
///   named `VectorIter` (user shadowing — see
///   `vector_iter_userdef.gg`).
/// - AND the module AST references anything that's only satisfied by
///   `std.iter`: an adapter struct name (`TakeIter` / `MapIter` / …),
///   one of `std.iter`'s free functions (`sum_iter` / `join_iter` / …),
///   or a `.iter()` method call.
///
/// This makes `v.iter().take(3).filter(p).map(f).collect()` work in
/// a scratch file without `from std.iter import ...` boilerplate,
/// while leaving user-shadowing fixtures alone.
fn module_should_auto_load_std_iter(module: &Module) -> bool {
    if module_shadows_vector_iter(module) {
        return false;
    }
    if module_imports_std_iter(module) {
        return false;
    }
    ast_mentions_std_iter_need(&module.items)
}

fn module_shadows_vector_iter(module: &Module) -> bool {
    for item in &module.items {
        let name = match &item.node {
            Item::Struct(s) => &s.name.node,
            Item::Enum(e) => &e.name.node,
            Item::TypeAlias(a) => &a.name.node,
            Item::Newtype(n) => &n.name.node,
            Item::Trait(t) => &t.name.node,
            _ => continue,
        };
        if name == "VectorIter" {
            return true;
        }
    }
    false
}

fn module_imports_std_iter(module: &Module) -> bool {
    for item in &module.items {
        if let Item::Import(import) = &item.node {
            let segments: Vec<&str> = match import {
                ImportStmt::Simple { path, .. }
                | ImportStmt::Grouped { path, .. }
                | ImportStmt::From { path, .. } => {
                    path.iter().map(|s| s.node.as_str()).collect()
                }
            };
            if segments == ["std", "iter"] {
                return true;
            }
        }
    }
    false
}

/// Names that only `std.iter` provides — finding any of them in the
/// AST is a strong signal the module needs the module loaded.
/// Includes `Iterator` / `Iterable`: both are trait placeholder names
/// in the prelude, but their actual definitions (with default methods
/// like `count`/`collect`/`take`/…) live in `std.iter`. Modules that
/// `equip X with Iterator[T]:` or call `.iter()` need those defaults.
///
/// Note: `sum_iter`, `product_iter`, `min_iter`, `max_iter`, `join_iter`
/// were retired from `lib/std/iter.gg` on 2026-04-27 (replaced by
/// `Iterator` default methods). Kept here for backwards-compatible
/// auto-load triggering: old user code referencing them still gets
/// `std.iter` loaded so the migration error is clear.
const STD_ITER_NAMES: &[&str] = &[
    "Iterator", "Iterable", "IntoIterable", "Drainable",
    "VectorIter", "VectorDrain", "TakeIter", "SkipIter", "ChainIter",
    "MapIter", "FilterIter", "TakeWhileIter", "DropWhileIter",
    "FilterMapIter", "InspectIter", "EnumerateIter", "ZipIter",
    "WindowsIter", "ChunksIter", "DictIter", "SetIter",
    // retired free-fn names — kept as detection sentinels (see above)
    "sum_iter", "product_iter", "min_iter", "max_iter",
    "join_iter", "set_iter",
];

fn ast_mentions_std_iter_need(items: &[Spanned<Item>]) -> bool {
    use crate::parser::ast::TraitItem;
    for item in items {
        match &item.node {
            Item::Function(f) => {
                if function_mentions_iter(f) { return true; }
            }
            Item::Equip(equip) => {
                for method in &equip.items {
                    if function_mentions_iter(&method.node) { return true; }
                }
            }
            Item::Trait(t) => {
                for trait_item in &t.items {
                    if let TraitItem::Method(m) = &trait_item.node {
                        if function_mentions_iter(m) { return true; }
                    }
                }
            }
            Item::ConstDecl(c) => {
                if expr_mentions_iter(&c.value) { return true; }
            }
            Item::StaticDecl(s) => {
                if expr_mentions_iter(&s.value) { return true; }
            }
            Item::Test(t) => {
                if block_mentions_iter(&t.body) { return true; }
            }
            Item::Bench(b) => {
                if block_mentions_iter(&b.body) { return true; }
            }
            Item::SuiteSetup(s) => {
                if block_mentions_iter(&s.body) { return true; }
            }
            Item::SuiteTeardown(s) => {
                if block_mentions_iter(&s.body) { return true; }
            }
            Item::Module { items: inner, .. } => {
                if ast_mentions_std_iter_need(inner) { return true; }
            }
            _ => {}
        }
    }
    false
}

fn function_mentions_iter(f: &crate::parser::ast::FunctionDef) -> bool {
    use crate::parser::ast::FunctionBody;
    for p in &f.params {
        if type_mentions_iter(&p.node.type_.node) { return true; }
    }
    if type_mentions_iter(&f.return_type.node) { return true; }
    match &f.body {
        FunctionBody::Block(b) => block_mentions_iter(b),
        FunctionBody::Expression(e) => expr_mentions_iter(e),
        _ => false,
    }
}

fn block_mentions_iter(block: &crate::parser::ast::Block) -> bool {
    for stmt in &block.stmts {
        if stmt_mentions_iter(stmt) { return true; }
    }
    false
}

fn stmt_mentions_iter(stmt: &Spanned<crate::parser::ast::Stmt>) -> bool {
    use crate::parser::ast::Stmt;
    match &stmt.node {
        Stmt::VarDecl { type_, value, .. } => {
            type_mentions_iter(&type_.node) || expr_mentions_iter(value)
        }
        Stmt::Assign { target, value }
        | Stmt::CompoundAssign { target, value, .. } => {
            expr_mentions_iter(target) || expr_mentions_iter(value)
        }
        Stmt::Return(Some(e)) | Stmt::Expr(e) | Stmt::Throw(e) => {
            expr_mentions_iter(e)
        }
        Stmt::If { condition, then_body, elif_branches, else_body } => {
            expr_mentions_iter(condition)
                || block_mentions_iter(then_body)
                || elif_branches.iter().any(|(c, b)| {
                    expr_mentions_iter(c) || block_mentions_iter(b)
                })
                || else_body.as_ref().map_or(false, |b| block_mentions_iter(b))
        }
        Stmt::While { condition, body, .. } => {
            expr_mentions_iter(condition) || block_mentions_iter(body)
        }
        Stmt::For { iterable, body, .. } => {
            expr_mentions_iter(iterable) || block_mentions_iter(body)
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            if expr_mentions_iter(scrutinee) { return true; }
            for item in arms {
                if let Some(arm) = item.arm() {
                    if expr_mentions_iter(&arm.body) { return true; }
                    if let Some(g) = &arm.guard {
                        if expr_mentions_iter(g) { return true; }
                    }
                }
            }
            else_arm.as_ref().map_or(false, |b| block_mentions_iter(b))
        }
        Stmt::Loop { body }
        | Stmt::Unsafe { body }
        | Stmt::NamedScope { body, .. } => block_mentions_iter(body),
        Stmt::With { bindings, body } => {
            bindings.iter().any(|b| expr_mentions_iter(&b.expr))
                || block_mentions_iter(body)
        }
        _ => false,
    }
}

fn expr_mentions_iter(expr: &Spanned<crate::parser::ast::Expr>) -> bool {
    use crate::parser::ast::Expr;
    match &expr.node {
        Expr::Identifier(n) => STD_ITER_NAMES.contains(&n.as_str()),
        Expr::MethodCall { receiver, method, args, .. } => {
            // `.iter()` triggers loading std.iter (Iterable trait + adapters).
            // `.drain()` does the same (Drainable trait + drain iterators).
            if method.node == "iter" || method.node == "drain" { return true; }
            if expr_mentions_iter(receiver) { return true; }
            args.iter().any(|a| expr_mentions_iter(&a.node.value))
        }
        Expr::Call { callee, args, .. } => {
            if expr_mentions_iter(callee) { return true; }
            args.iter().any(|a| expr_mentions_iter(&a.node.value))
        }
        Expr::StructLiteral { name, args, .. } => {
            if STD_ITER_NAMES.contains(&name.node.as_str()) { return true; }
            args.iter().any(|a| expr_mentions_iter(a))
        }
        Expr::BinaryOp { left, right, .. } => {
            expr_mentions_iter(left) || expr_mentions_iter(right)
        }
        Expr::UnaryOp { operand, .. } => expr_mentions_iter(operand),
        Expr::FieldAccess { object, .. }
        | Expr::TupleFieldAccess { object, .. } => expr_mentions_iter(object),
        Expr::Index { object, index } => {
            expr_mentions_iter(object) || expr_mentions_iter(index)
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            expr_mentions_iter(condition)
                || expr_mentions_iter(then_branch)
                || elif_branches.iter().any(|(c, b)| {
                    expr_mentions_iter(c) || expr_mentions_iter(b)
                })
                || else_branch.as_ref().map_or(false, |e| expr_mentions_iter(e))
        }
        Expr::Range { start, end, .. } => {
            start.as_ref().map_or(false, |s| expr_mentions_iter(s))
                || end.as_ref().map_or(false, |e| expr_mentions_iter(e))
        }
        Expr::Move { expr: inner }
        | Expr::Propagate { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::OptionalChain { object: inner, .. } => expr_mentions_iter(inner),
        Expr::DefaultOp { lhs, rhs } => {
            expr_mentions_iter(lhs) || expr_mentions_iter(rhs)
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
            expr_mentions_iter(body)
        }
        Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems, _) => {
            elems.iter().any(|e| expr_mentions_iter(e))
        }
        Expr::Block(block) => block_mentions_iter(block),
        _ => false,
    }
}

fn type_mentions_iter(ty: &crate::parser::ast::Type) -> bool {
    use crate::parser::ast::Type;
    match ty {
        Type::Named { name, generic_args } => {
            if STD_ITER_NAMES.contains(&name.node.as_str()) { return true; }
            generic_args.iter().any(|a| type_mentions_iter(&a.node))
        }
        Type::Tuple(elems) => elems.iter().any(|e| type_mentions_iter(&e.node)),
        Type::Ref(inner) | Type::Owned(inner) | Type::Pointer(inner) => {
            type_mentions_iter(&inner.node)
        }
        Type::Function { return_type, params, .. } => {
            type_mentions_iter(&return_type.node)
                || params.iter().any(|p| type_mentions_iter(&p.node))
        }
        Type::Array { element, .. } | Type::Slice { element } => {
            type_mentions_iter(&element.node)
        }
        _ => false,
    }
}

/// Recursively extract imports from items, including inside `meta if` blocks.
/// For `meta if platform()` conditions, we evaluate at load time to only load
/// modules for the current platform. For other meta if conditions, we conservatively
/// extract imports from all branches.
fn extract_imports_from_items(items: &[Spanned<Item>], imports: &mut Vec<(Vec<String>, Span)>) {
    for item in items {
        match &item.node {
            Item::Import(import) => {
                match import {
                    ImportStmt::Simple { path, span } => {
                        let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                        imports.push((segments, *span));
                    }
                    ImportStmt::Grouped { path, span, .. } => {
                        let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                        imports.push((segments, *span));
                    }
                    ImportStmt::From { path, span, .. } => {
                        let segments: Vec<String> = path.iter().map(|s| s.node.clone()).collect();
                        imports.push((segments, *span));
                    }
                }
            }
            Item::MetaIf(meta_if) => {
                extract_imports_from_meta_if(meta_if, imports);
            }
            _ => {}
        }
    }
}

/// Extract imports from a `meta if` block, evaluating `platform()` conditions
/// at load time to avoid loading platform-irrelevant modules.
fn extract_imports_from_meta_if(meta_if: &MetaIf, imports: &mut Vec<(Vec<String>, Span)>) {
    // Try to evaluate the condition as a platform check
    match eval_platform_condition(&meta_if.condition.node) {
        Some(true) => {
            // Condition is true — only extract from then branch
            extract_imports_from_items(&meta_if.then_items, imports);
        }
        Some(false) => {
            // Condition is false — try elif branches, then else
            for (cond, branch_items) in &meta_if.elif_branches {
                match eval_platform_condition(&cond.node) {
                    Some(true) => {
                        extract_imports_from_items(branch_items, imports);
                        return;
                    }
                    Some(false) => continue,
                    None => {
                        // Can't evaluate — conservatively include this and remaining
                        extract_imports_from_items(branch_items, imports);
                    }
                }
            }
            if let Some(else_items) = &meta_if.else_items {
                extract_imports_from_items(else_items, imports);
            }
        }
        None => {
            // Can't evaluate condition — conservatively extract from all branches
            extract_imports_from_items(&meta_if.then_items, imports);
            for (_cond, branch_items) in &meta_if.elif_branches {
                extract_imports_from_items(branch_items, imports);
            }
            if let Some(else_items) = &meta_if.else_items {
                extract_imports_from_items(else_items, imports);
            }
        }
    }
}

/// Try to evaluate a `platform() == "..."` or `platform() != "..."` condition
/// at load time. Returns `Some(bool)` if evaluable, `None` otherwise.
fn eval_platform_condition(expr: &Expr) -> Option<bool> {
    let current_platform = if cfg!(target_os = "macos") { "macos" }
        else if cfg!(target_os = "linux") { "linux" }
        else if cfg!(target_os = "windows") { "windows" }
        else { "unknown" };

    use crate::parser::ast::BinaryOp as BinOp;

    match expr {
        Expr::BinaryOp { op, left, right, .. } => {
            // Check for `platform() == "..."` or `platform() != "..."`
            let extract_platform_str = |a: &Expr, b: &Expr| -> Option<String> {
                if let Expr::Call { callee, args, .. } = a {
                    if args.is_empty() {
                        if let Expr::Identifier(n) = &callee.node {
                            if n == "platform" {
                                if let Expr::StringLiteral(sl, _) = b {
                                    return Some(sl.as_plain_text());
                                }
                            }
                        }
                    }
                }
                None
            };
            let string_val = extract_platform_str(&left.node, &right.node)
                .or_else(|| extract_platform_str(&right.node, &left.node));
            let string_val = match string_val {
                Some(s) => s,
                None => return None,
            };
            match op {
                BinOp::Eq => Some(current_platform == string_val.as_str()),
                BinOp::Neq => Some(current_platform != string_val.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}


impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            loaded: HashSet::new(),
            load_stack: Vec::new(),
            dep_paths: HashMap::new(),
            next_offset: 0,
            entry_base_dir: None,
        }
    }

    /// Create a loader with package dependency paths.
    pub fn with_dep_paths(dep_paths: HashMap<String, PathBuf>) -> Self {
        Self {
            loaded: HashSet::new(),
            load_stack: Vec::new(),
            dep_paths,
            next_offset: 0,
            entry_base_dir: None,
        }
    }

    /// Recursively load a module and all its imports.
    ///
    /// `entry` is the path to the main `.gg` file. `entry_source` and `entry_module`
    /// are the already-read source and parsed AST for the entry file (to avoid re-parsing).
    ///
    /// Returns `(path, logical_path, source, module)` quads for all loaded files, with the
    /// entry first. `logical_path` is the import path segments (e.g. `["xtd", "csv"]`); empty
    /// for the entry module.
    /// Load the entry module and all its transitive imports, returning each
    /// loaded module along with the global byte offset the loader assigned
    /// it. Spans in the AST are global byte offsets across the entire merged
    /// source; the offsets returned here let the diagnostic reporter map a
    /// span back to its originating file without re-deriving the offsets
    /// (which silently drifts past synthetic / empty-source modules — see
    /// the multi-file diagnostic-routing fix on 2026-04-30).
    ///
    /// Each tuple element is `(canonical_path, logical_segments, source,
    /// module, base_offset)`. Synthetic modules (no source text) carry a
    /// `base_offset` equal to `next_offset` at the time they were pushed —
    /// they don't claim any byte range and the reporter should filter them
    /// out before binary-search by checking `source.is_empty()`.
    pub fn load_all(
        &mut self,
        entry: &Path,
        entry_source: String,
        entry_module: Module,
    ) -> Result<Vec<(PathBuf, Vec<String>, String, Module, usize)>, LoadError> {
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

        // Store entry base dir for cross-directory fallback resolution
        self.entry_base_dir = Some(base_dir.clone());

        // Auto-load std.hash when the module uses @derive(Hashable) —
        // the derived `equip T with Hashable: void hash(self, FxHasher &h)`
        // references FxHasher and its write_int / write_string / finish
        // methods from std.hash. Without the auto-load users would hit
        // link-time errors for those symbols after the derive.
        //
        // Skipped for hot-reload modules: the host binary emits a
        // Hasher vtable global even when the code doesn't use it, and
        // the host's function-set is pruned tighter than the guest's —
        // the vtable would dangle against undefined Hasher_for_FxHasher
        // symbols. Hot-reload programs that need @derive(Hashable) can
        // still import std.hash explicitly.
        let uses_hashable_derive = module_uses_hashable_derive(&entry_module);
        let is_hot_reload = entry_module.items.iter().any(|i| matches!(
            &i.node,
            crate::parser::ast::Item::Directive(d) if d.name == "hot-reload"
        ));

        // Entry module has an empty logical path. Loaded at offset 0.
        results.push((canonical.clone(), Vec::new(), entry_source, entry_module, 0));

        if uses_hashable_derive && !is_hot_reload {
            let hash_segments = vec!["std".to_string(), "hash".to_string()];
            self.load_recursive(&base_dir, &hash_segments, &mut results)?;
        }

        // Auto-load `std.iter` when the entry module references iterator
        // names or calls `.iter()` — see `module_should_auto_load_std_iter`.
        // Shadowing + existing-import checks avoid collisions with fixtures
        // that define their own iterator types.
        //
        // Known tension (TODO — tracked in
        // `docs/devbook/22-modules-packages.md`): `try_lower_iterator_adapter`
        // in `src/ir/lowering/exprs/methods.rs` eagerly materialises
        // `.map()` / `.filter()` etc. into `GorgetArray` literals (the old
        // "one-step" path), while typecheck with std.iter loaded dispatches
        // those same calls to lazy `Iterator[T]` default adapters. If a
        // fixture hits the mismatch (`Vector[int]` from typecheck vs
        // `GorgetArray` from IR) the next-hop `.collect()` fails. Resolve by
        // either deleting the eager shortcut + migrating callers, or by making
        // typecheck prefer the eager shortcut when one is registered.
        let auto_load_iter = results.last()
            .map(|(_, _, _, m, _)| module_should_auto_load_std_iter(m))
            .unwrap_or(false);
        if auto_load_iter {
            let iter_segments = vec!["std".to_string(), "iter".to_string()];
            self.load_recursive(&base_dir, &iter_segments, &mut results)?;
        }

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
        results: &mut Vec<(PathBuf, Vec<String>, String, Module, usize)>,
    ) -> Result<(), LoadError> {
        // Intercept virtual built-in modules (std.* and gg.*) before filesystem resolution
        if crate::stdlib::is_builtin_module(segments) {
            let virtual_path = PathBuf::from(format!("<{}>", segments.join(".")));
            if self.loaded.contains(&virtual_path) {
                return Ok(());
            }

            // Try synthetic (compiler-generated) module first.
            // No source text → no spans of its own → no byte range claimed.
            // We carry `next_offset` as the recorded base so the tuple shape
            // is uniform, but the reporter filters these out via empty source.
            if let Some(module) = crate::stdlib::generate_builtin_module(segments) {
                self.loaded.insert(virtual_path.clone());
                results.push((virtual_path, segments.to_vec(), String::new(), module, self.next_offset));
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
                results.push((virtual_path.clone(), segments.to_vec(), source.to_string(), module, offset));

                self.load_stack.pop();
                return Ok(());
            }
        }

        // Try local filesystem first (relative to importing file's directory)
        let file_path = resolve_import_path(base_dir, segments);

        // If the local file doesn't exist, try the entry file's base directory
        // as a fallback. This supports cross-directory imports in multi-file
        // projects (e.g. renderer/backend.gg importing from client/view.gg).
        let file_path = if !file_path.exists() {
            if let Some(entry_dir) = &self.entry_base_dir {
                let entry_path = resolve_import_path(entry_dir, segments);
                if entry_path.exists() {
                    entry_path
                } else {
                    file_path
                }
            } else {
                file_path
            }
        } else {
            file_path
        };

        // Project-rooted imports: if the first segment is a known "root
        // namespace" (today: `compiler`), walk up from the entry directory
        // to find an ancestor containing a directory with that name, then
        // resolve from there. Lets `from compiler.data.schema import …`
        // work from any file under the project — the canonical
        // compiler-data files live at `<project_root>/compiler/data/*.gg`
        // regardless of where the importing file sits.
        let file_path = if !file_path.exists() && is_project_root_namespace(segments) {
            self.entry_base_dir.as_ref()
                .and_then(|entry_dir| find_project_root_for(entry_dir, &segments[0]))
                .map(|root| resolve_import_path(&root, segments))
                .filter(|p| p.exists())
                .unwrap_or(file_path)
        } else {
            file_path
        };

        // If the file still doesn't exist, try package dependencies.
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

        let module_base_offset = self.next_offset;
        self.next_offset = module_base_offset + source.len() + 1;
        let mut parser = Parser::new_with_offset(&source, module_base_offset);
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
        results.push((canonical.clone(), segments.to_vec(), source, module, module_base_offset));

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
/// excluding prelude names. Ambiguous variant names (defined on more than one
/// enum in the same module) are excluded — same rationale as the multi-module
/// builder; let the downstream resolver disambiguate from scrutinee type /
/// expected type.
fn build_variant_map_from_module(module: &Module) -> HashMap<String, String> {
    let mut sightings: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
    for item in &module.items {
        if let Item::Enum(e) = &item.node {
            if e.generic_params.is_some() {
                continue;
            }
            let enum_name = e.name.node.clone();
            for variant in &e.variants {
                let vname = variant.node.name.node.clone();
                if !PRELUDE_VARIANTS.contains(&vname.as_str()) {
                    sightings
                        .entry(vname)
                        .or_default()
                        .insert(enum_name.clone());
                }
            }
        }
    }
    sightings
        .into_iter()
        .filter_map(|(vname, enums)| {
            if enums.len() == 1 {
                enums.into_iter().next().map(|en| (vname, en))
            } else {
                None
            }
        })
        .collect()
}

/// Build a global variant_name → enum_name map from ALL modules (for cross-module qualification).
///
/// **Ambiguity handling.** When a variant name is defined on *more than one* enum
/// across the merged set (e.g. both `Json` and `TomlValue` expose an `Arr`
/// variant), qualifying the bare name to *either* enum is a layering violation:
/// the loader has no idea which one the user meant at the use site. The
/// downstream pattern-lowering and type-checking passes do — they know the
/// scrutinee type / expected type — so leaving the variant un-qualified delegates
/// the decision to them.
///
/// Concretely: ambiguous variant names are **excluded from the returned map**.
/// `qualify_pattern` then leaves their paths at length 1, and
/// `lower_pattern_condition` / `emit_pattern_bindings` resolve the enum from the
/// scrutinee's static type (the existing `type_name(scrut_type)` branch that
/// covers prelude variants). Bare `Arr(x)` constructor calls in expression
/// position similarly stay unqualified and resolve through the typechecker's
/// `decl_type_hint`-driven path.
///
/// Pre-fix behaviour was "first-writer-wins" which silently routed bare
/// references to whichever module loaded first — a textbook
/// `[layering-discipline]` rule-2 violation (resolving abstractions by
/// identifier-string lookup, ignoring typed context). Surfaced by the
/// gorget-conformance Phase 1 harness once it started importing both
/// `xtd/json` and `xtd/toml` in the same program (see the 2026-05-19
/// cross-module-method-resolution bug fixture).
fn build_variant_map_from_all(modules: &[(PathBuf, String, Module)]) -> HashMap<String, String> {
    // First pass: collect every (variant_name, enum_name) sighting so we can
    // detect collisions before committing to a winner.
    let mut sightings: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
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
                        sightings
                            .entry(vname)
                            .or_default()
                            .insert(enum_name.clone());
                    }
                }
            }
        }
    }
    // Second pass: keep only the unambiguous entries.
    sightings
        .into_iter()
        .filter_map(|(vname, enums)| {
            if enums.len() == 1 {
                enums.into_iter().next().map(|en| (vname, en))
            } else {
                None
            }
        })
        .collect()
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
            qualify_block(&mut t.body, vm);
        }
        Item::Bench(b) => {
            qualify_block(&mut b.body, vm);
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
            for arm in arms.iter_mut().filter_map(|i| i.arm_mut()) {
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
        Stmt::Break | Stmt::Continue | Stmt::Pass => {}
        Stmt::With { bindings, body } => {
            for b in bindings { qualify_expr(&mut b.expr, vm); }
            qualify_block(body, vm);
        }
        Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
            qualify_expr(condition, vm);
            if let Some(m) = message { qualify_expr(m, vm); }
        }
        Stmt::Snapshot { value, .. } => {
            qualify_expr(value, vm);
        }
        Stmt::Unsafe { body } => qualify_block(body, vm),
        Stmt::NamedScope { body, .. } => qualify_block(body, vm),
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
        Stmt::MetaConst { value, .. } => {
            qualify_expr(value, vm);
        }
        Stmt::MetaLog { args, .. } => {
            for arg in args {
                qualify_expr(arg, vm);
            }
        }
        Stmt::OnError { body } => {
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
        | Expr::NoneLiteral
        | Expr::SelfExpr
        | Expr::It
        | Expr::StringLiteral(_, _) => {}

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
        Expr::Move { expr: inner }
        | Expr::Propagate { expr: inner }
        | Expr::MutableBorrow { expr: inner }
        | Expr::Deref { expr: inner }
        | Expr::Await { expr: inner, .. }
        | Expr::Spawn { expr: inner, .. }
        | Expr::SpawnBlocking { expr: inner, .. } => qualify_expr(inner, vm),
        Expr::DefaultOp { lhs, rhs } => {
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
        Expr::ArrayLiteral(elems, _) | Expr::TupleLiteral(elems) => {
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
        Expr::MetaOpInfix { left, right, .. } => {
            qualify_expr(left, vm);
            qualify_expr(right, vm);
        }
        Expr::MetaOpToken(_) => {}
        Expr::Rethrow { expr, transform, .. } => {
            qualify_expr(expr, vm);
            qualify_expr(transform, vm);
        }
        Expr::Catch { expr, recovery, .. } => {
            qualify_expr(expr, vm);
            qualify_expr(recovery, vm);
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
///
/// **Variant qualification scoping.** Bare-name variant references (`case Arr(x)`,
/// `Arr(items)` constructor calls) are rewritten to qualified form *per module*
/// using *only* that module's own enum definitions plus the cross-module
/// **unambiguous** sightings. Variant names that collide across modules
/// (e.g. `Arr` defined on both `Json` and `TomlValue`) are NOT auto-qualified —
/// the downstream pattern-lowering / type-checking passes have the scrutinee
/// type / expected type and can disambiguate. Per-module qualification means
/// each module's *own* `case Arr(x)` always wins for its locally-defined enum,
/// regardless of which other modules ship the same variant name elsewhere.
/// See `build_variant_map_from_all` for the rationale.
pub fn merge_modules(modules: Vec<(PathBuf, Vec<String>, String, Module, usize)>) -> Module {
    if modules.len() == 1 {
        // Single-file: build variant map from this module and qualify in-place.
        let (_path, _logical, _source, mut module, _offset) = modules.into_iter().next().unwrap();
        let vm = build_variant_map_from_module(&module);
        qualify_module_with_map(&mut module, &vm);
        return module;
    }

    // Multi-file: each module qualifies bare variants using its OWN enum
    // definitions first (so `xtd/toml.gg`'s `case Arr(x)` always picks
    // `TomlValue.Arr`, even when `xtd/json.gg` is also loaded). On top of
    // that we layer the unambiguous cross-module sightings — variant names
    // defined on exactly one enum anywhere in the merged set — so modules
    // that *use* a foreign enum's variants without defining them locally
    // still get qualification. Ambiguous variants stay un-qualified and
    // resolve via context downstream.
    let raw_for_vm: Vec<(PathBuf, String, Module)> = modules.iter()
        .map(|(p, _, s, m, _)| (p.clone(), s.clone(), m.clone()))
        .collect();
    let global_unambiguous = build_variant_map_from_all(&raw_for_vm);

    let mut all_items: Vec<Spanned<Item>> = Vec::new();

    for (i, (_path, logical_path, _source, mut module, _offset)) in modules.into_iter().enumerate() {
        // Per-module qualification: local enum defs override the global map
        // (so each module gets its own bare-variant scoping back). Cross-
        // module entries fill in only where the local module has nothing
        // to say about that variant name.
        let local_vm = build_variant_map_from_module(&module);
        let mut effective_vm = global_unambiguous.clone();
        for (vname, ename) in &local_vm {
            effective_vm.insert(vname.clone(), ename.clone());
        }
        qualify_module_with_map(&mut module, &effective_vm);
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
