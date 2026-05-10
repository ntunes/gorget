//! Compiler-source-tree lints — ratchets that lock layering discipline.
//!
//! These tests scan `src/**/*.rs` for patterns that the layering-discipline
//! doc (`docs/internals/layering-discipline.md`) and structural-guards
//! Tier 3a flag as anti-patterns, and assert the count never grows. As
//! migrations land, the budget tightens. A new site that bypasses typed
//! metadata for name-prefix routing fails the test until either it's
//! migrated to typed metadata or the budget is intentionally raised.
//!
//! See `docs/internals/structural-guards.md` §3a for the full design.

use std::fs;
use std::path::Path;

/// Every mangled monomorphized-type prefix the compiler emits. Adding a new
/// builtin protocol with a `base_name: "X"` requires adding `X` here so the
/// ratchet covers `starts_with("X__")` checks against it.
const MANGLED_PREFIXES: &[&str] = &[
    "Vector", "Deque",
    "Dict", "HashMap",
    "Set", "HashSet",
    "Mutex", "RWLock", "Channel", "Shared", "Weak",
    "Guard", "ReadGuard", "WriteGuard",
    "Box", "Task", "Heap",
    "Tuple",
    "Callable", "MutCallable", "ConsumeCallable",
    "Option", "Result",
];

/// Walk `src/**/*.rs` and count `starts_with("X__")` calls where `X` is a
/// known mangled-type prefix. These are the layering-discipline violations
/// the ratchet locks against.
fn count_name_prefix_sites() -> usize {
    count_name_prefix_in_tree("src", "rs")
}

/// Walk `tests/fixtures/self_host_*/**/*.gg` and count the same pattern in
/// self-host source. Phase A migrated self-host's classification consumers
/// to read through `build_resource_metadata` (the single source of truth);
/// this ratchet locks in those gains so a regression is caught at lint time.
///
/// See `docs/internals/self-host-resource-model.md` §3.3 step 4 (the
/// "promote" step) and §6.1 (Tier E.1 lints ratchet).
fn count_name_prefix_sites_self_host() -> usize {
    let alternation = MANGLED_PREFIXES.join("|");
    let pattern = regex::Regex::new(
        &format!(r#"starts_with\("({alternation})__"\)"#)
    ).unwrap();
    let mut count = 0;
    let entries = match fs::read_dir("tests/fixtures") {
        Ok(e) => e,
        Err(_) => return 0,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let name = match path.file_name().and_then(|n| n.to_str()) {
            Some(n) => n,
            None => continue,
        };
        if !name.starts_with("self_host_") {
            continue;
        }
        // Walk the dir's own .gg files; skip symlinks so shared
        // parser.gg / lexer.gg / ast.gg aren't double-counted across
        // self_host_lowerer (symlinked into self_host_typechecker), etc.
        let dir_entries = match fs::read_dir(&path) {
            Ok(e) => e,
            Err(_) => continue,
        };
        for de in dir_entries.flatten() {
            let p = de.path();
            if p.is_symlink() {
                continue;
            }
            if p.extension().map_or(true, |e| e != "gg") {
                continue;
            }
            let content = match fs::read_to_string(&p) {
                Ok(s) => s,
                Err(_) => continue,
            };
            count += pattern.find_iter(&content).count();
        }
    }
    count
}

fn count_name_prefix_in_tree(root: &str, ext: &str) -> usize {
    let alternation = MANGLED_PREFIXES.join("|");
    let pattern = regex::Regex::new(
        &format!(r#"starts_with\("({alternation})__"\)"#)
    ).unwrap();
    let mut count = 0;
    visit(root, &mut |path| {
        if path.extension().map_or(true, |e| e != ext) {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        count += pattern.find_iter(&content).count();
    });
    count
}

fn visit(dir: impl AsRef<Path>, f: &mut dyn FnMut(&Path)) {
    let entries = match fs::read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            visit(&path, f);
        } else {
            f(&path);
        }
    }
}

/// Tier 3a ratchet: the count of name-prefix routing sites in the compiler
/// must never grow. Decreases freely as migrations land — bring the budget
/// down when you remove a site so the next regression is caught immediately.
///
/// **If this fails**: a new `starts_with("Vector__"/"Box__"/...)` was added.
/// Either:
///   1. Migrate it to typed metadata (preferred — see recent commits like
///      `is_trait_box` flag, `combinator_result_struct_id`, `enum_kind`, etc.).
///   2. If the new site is a legitimate registrar / C-emit-boundary spelling
///      that genuinely cannot be typed (rare), bump `BUDGET` deliberately
///      with a comment explaining why and link the PR.
///
/// **As you migrate**: lower the budget in the same commit that retires
/// the site(s).
///
/// Baseline 2026-05-10: 375 (initial 438; cumulative -76 across migrations
/// in stmts/assigns.rs, stmts/for_loops.rs, stmts/mod.rs, lir/lower/operands.rs,
/// ir/lowering/exprs/methods.rs, lir/lower/insts.rs, c_lir/emit_call_extern.rs,
/// llvm/mod.rs, exprs/mod.rs, context.rs (dead Result__ fallback in
/// unwrap_result_ok_type, +1 dead Result__ fallback in fn_result_type),
/// c_lir/emit_types.rs (3 sites in combinator helper emission factored to
/// single is_result computed from typed enum_kind),
/// lir/lower/types.rs (-13 sites: retired Vector__/Deque__/Dict__/HashMap__/
/// Set__/HashSet__/Heap__/Task__/Box__/Guard__/ReadGuard__/WriteGuard__
/// prefix arms in `opaque_runtime_size`; sizes now resolve through
/// `LirModule::struct_aliases` to typed `StructDef.computed_c_size`).
/// Source-of-truth count — re-derive with
/// `grep -roE 'starts_with\("(...)__"\)' src/ | wc -l`.
/// (Counts occurrences, not lines — a line with two matches counts twice.)
#[test]
fn no_growth_in_name_prefix_routing() {
    /// Maximum allowed count of name-prefix routing sites in src/. Decrease
    /// when you migrate sites to typed metadata.
    /// Bumped 304 → 305 (2026-05-10): Tier 2a Phase 3 FATAL promotion
    /// (`082f26e9`) added a `starts_with("Task__")` predicate in the
    /// validator's Result-fallback path — single-site, registrar-adjacent.
    /// Lowered 305 → 292 (2026-05-10): opaque_runtime_size 13 prefix-arm
    /// retirement via typed struct_aliases.
    const BUDGET: usize = 292;

    let count = count_name_prefix_sites();
    assert!(
        count <= BUDGET,
        "Name-prefix routing count grew beyond budget: {count} > {BUDGET}.\n\n\
         The layering-discipline ratchet (Tier 3a per structural-guards.md) \
         bars new `starts_with(\"X__\")` sites where X is a mangled-type prefix. \
         Either migrate the new site to typed metadata (read a typed flag on \
         StructDef / TypeMetadata / LirExtern instead of pattern-matching the \
         name) or, if it's a genuine registrar / C-emit-boundary spelling, \
         raise BUDGET in tests/lints.rs with a comment explaining why.\n\n\
         To find new sites:\n  \
         grep -rEn 'starts_with(\"({}|...)__\")' src/ | grep -v target/\n\n\
         If the count went DOWN (great!), drop BUDGET in this file to lock in \
         the new floor.",
        MANGLED_PREFIXES.iter().take(3).copied().collect::<Vec<_>>().join("|"),
    );
}

/// Tier E.1 (per `docs/internals/self-host-resource-model.md` §6.1):
/// the same ratchet applied to self-host's `.gg` source. Phase A migrated
/// the classification consumers in `lir_lower.gg` and `lower.gg` to read
/// through `build_resource_metadata` (the single source of truth); this
/// test locks those gains in. Symlinked files (parser.gg, lexer.gg, ast.gg
/// shared between self_host_lowerer and self_host_typechecker) are skipped
/// so they aren't double-counted.
///
/// The remaining sites are intrinsic and load-bearing:
///   - Inside `build_resource_metadata` itself (the prefix → metadata
///     classifier — *the* place name-matching is allowed).
///   - Inside Pass 1 dispatcher's typed match arms doing prefix-length
///     slicing to extract `T` from `Vector__T` etc. (name parsing, not
///     classification — the metadata doesn't carry parsed names).
///   - `Option__` / `Result__` prelude variant detection (out of Phase A
///     scope; tracked separately under prelude codegen).
///   - Name-parsing helpers (`collection_element_type` and dict-literal
///     hint extraction).
///
/// **If this fails**: a new `starts_with("Vector__"/...)` was added in
/// self-host code outside `build_resource_metadata`. Either:
///   1. Migrate it to read `build_resource_metadata(name)` / `resource_meta_for`.
///   2. If it's necessary name parsing inside an already-migrated dispatcher
///      arm, bump `BUDGET` deliberately with a comment.
///
/// Baseline 2026-05-10: 52 (after Phase A.1–A.4 + 12/N migrations).
#[test]
fn no_growth_in_self_host_name_prefix_routing() {
    const BUDGET: usize = 52;

    let count = count_name_prefix_sites_self_host();
    assert!(
        count <= BUDGET,
        "Self-host name-prefix routing count grew beyond budget: {count} > {BUDGET}.\n\n\
         Phase A migrated the classification consumers in self-host's lir_lower.gg \
         and lower.gg to read through `build_resource_metadata` (the single source \
         of truth). Adding a new `starts_with(\"Vector__\"/\"Box__\"/...)` outside \
         that function or its inner name-parsing dispatchers is a layering regression. \
         Either:\n  \
         1. Read the typed metadata: `match build_resource_metadata(name): case Some(rmeta): ...`\n  \
         2. If it's a genuinely necessary name-parsing site (extracting T from Vector__T), \
         add it to Pass 1 dispatcher's already-migrated arms.\n\n\
         If the count went DOWN (great!), drop BUDGET in this file to lock the new floor.\n\n\
         To find sites:\n  \
         grep -rnE --include='*.gg' 'starts_with(\"({}|...)__\")' tests/fixtures/self_host_*",
        MANGLED_PREFIXES.iter().take(3).copied().collect::<Vec<_>>().join("|"),
    );
}
