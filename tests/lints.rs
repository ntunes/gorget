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
    let alternation = MANGLED_PREFIXES.join("|");
    let pattern = regex::Regex::new(
        &format!(r#"starts_with\("({alternation})__"\)"#)
    ).unwrap();

    let mut count = 0;
    visit("src", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
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
/// Baseline 2026-05-10: 375 (initial 438; cumulative -63 across migrations
/// in stmts/assigns.rs, stmts/for_loops.rs, stmts/mod.rs, lir/lower/operands.rs,
/// ir/lowering/exprs/methods.rs, lir/lower/insts.rs, c_lir/emit_call_extern.rs,
/// llvm/mod.rs, exprs/mod.rs, context.rs (dead Result__ fallback in
/// unwrap_result_ok_type, +1 dead Result__ fallback in fn_result_type),
/// c_lir/emit_types.rs (3 sites in combinator helper emission factored to
/// single is_result computed from typed enum_kind).
/// Source-of-truth count — re-derive with
/// `grep -roE 'starts_with\("(...)__"\)' src/ | wc -l`.
/// (Counts occurrences, not lines — a line with two matches counts twice.)
#[test]
fn no_growth_in_name_prefix_routing() {
    /// Maximum allowed count of name-prefix routing sites in src/. Decrease
    /// when you migrate sites to typed metadata.
    const BUDGET: usize = 340;

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
