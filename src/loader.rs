use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::errors::ParseError;
use crate::parser::ast::{ImportStmt, Item, Module};
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
        }
    }

    /// Recursively load a module and all its imports.
    ///
    /// `entry` is the path to the main `.gg` file. `entry_source` and `entry_module`
    /// are the already-read source and parsed AST for the entry file (to avoid re-parsing).
    ///
    /// Returns `(path, source, module)` triples for all loaded files, with the entry first.
    pub fn load_all(
        &mut self,
        entry: &Path,
        entry_source: String,
        entry_module: Module,
    ) -> Result<Vec<(PathBuf, String, Module)>, LoadError> {
        let canonical = entry
            .canonicalize()
            .map_err(|e| LoadError::Io {
                path: entry.to_path_buf(),
                error: e,
            })?;

        let mut results = Vec::new();
        self.loaded.insert(canonical.clone());
        self.load_stack.push(canonical.clone());

        // Collect imports from the entry module
        let imports = extract_imports(&entry_module);
        let base_dir = canonical.parent().unwrap().to_path_buf();

        results.push((canonical.clone(), entry_source, entry_module));

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
        results: &mut Vec<(PathBuf, String, Module)>,
    ) -> Result<(), LoadError> {
        // Intercept virtual stdlib modules before filesystem resolution
        if crate::stdlib::is_stdlib_module(segments) {
            let virtual_path = PathBuf::from(format!("<std.{}>", segments[1]));
            if self.loaded.contains(&virtual_path) {
                return Ok(());
            }
            if let Some(module) = crate::stdlib::generate_stdlib_module(segments) {
                self.loaded.insert(virtual_path.clone());
                results.push((virtual_path, String::new(), module));
                return Ok(());
            }
        }

        let file_path = resolve_import_path(base_dir, segments);

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

        let mut parser = Parser::new(&source);
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

        // Collect imports from this module and recurse
        let imports = extract_imports(&module);
        let this_dir = canonical.parent().unwrap().to_path_buf();

        results.push((canonical.clone(), source, module));

        for (segs, _span) in imports {
            self.load_recursive(&this_dir, &segs, results)?;
        }

        self.load_stack.pop();
        Ok(())
    }
}

/// Merge all loaded modules into a single `Module`.
///
/// The entry file's items come first, then imported modules' items
/// (excluding their `Item::Import` nodes to avoid re-processing).
pub fn merge_modules(modules: Vec<(PathBuf, String, Module)>) -> Module {
    if modules.len() == 1 {
        // Single-file fast path — no merging needed
        return modules.into_iter().next().unwrap().2;
    }

    let mut all_items: Vec<Spanned<Item>> = Vec::new();

    for (i, (_path, _source, module)) in modules.into_iter().enumerate() {
        if i == 0 {
            // Entry file: keep all items (including its imports, so the resolver
            // can register the imported names)
            all_items.extend(module.items);
        } else {
            // Imported modules: include everything except their own imports
            all_items.extend(
                module
                    .items
                    .into_iter()
                    .filter(|item| !matches!(item.node, Item::Import(_))),
            );
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
