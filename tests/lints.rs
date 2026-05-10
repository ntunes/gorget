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

/// Tier 2d — sidecar absence. Static check that no parallel
/// `HashMap<key, value>` sidecar exists in the codebase tracking a fact
/// already on a typed metadata field. Per `docs/internals/structural-
/// guards.md` Tier 2d:
///
/// > **Why it matters.** Layering discipline rule 3: one source of truth
/// > per axis. Sidecars accumulate quietly; the validator catches them
/// > at introduction time. This is the discipline meta-rule with the
/// > highest payoff because parallel sidecars are how multi-step
/// > inconsistencies enter the codebase.
///
/// The watchlist below names value types whose ONE source of truth is
/// already a typed field on `TypeMetadata` / `Local` / `Inst`. Any
/// `HashMap<*, T>` / `FxHashMap<*, T>` / `BTreeMap<*, T>` declaration in
/// `src/**/*.rs` for a watched T is a Rule 3 violation — the lookup
/// should consult the typed field via the canonical accessor (e.g.
/// `registry.get_type_def(name).map(|td| td.metadata.drop_strategy)`),
/// not maintain a parallel registry.
///
/// **Baseline 2026-05-10: 0 sidecars across all watched types.** This
/// is the post-Phase-D floor (sidecar maps from earlier eras —
/// `mut_capture_locals`, `view_returning_temps`, `is_resource` callback,
/// etc. — already retired). The lint locks the floor; new sidecars
/// fail the test until either migrated to the typed field or
/// explicitly allowlisted with a citation.
///
/// **If this fails**: a new `HashMap<*, DropStrategy>` (or another
/// watched type) was introduced. Either:
///   1. Migrate to read the typed field directly. If the lookup needs
///      a different access pattern, expose a typed accessor on the
///      owning struct.
///   2. If genuinely independent (e.g., a per-pass scratch map computed
///      from the typed field, NOT a parallel persistent registry),
///      add an allowlist entry with file:line + comment justifying
///      why it's not a sidecar.
const SIDECAR_VALUE_TYPES: &[&str] = &[
    // TypeMetadata fields — `src/ir/types.rs::TypeMetadata`
    "DropStrategy",
    "CopySemantics",
    "CollectionKind",
    "EnumKind",
    "EnumCategory",
    // Phase D Local field — `src/ir/mod.rs::Local`
    "LocalOwnership",
    "BorrowOrigin",
];

/// Canonical key types for IR/Type-system lookups. Sidecars take the
/// shape `HashMap<{key}, {value}>` where key ∈ {LocalId, TypeId, String}.
const SIDECAR_KEY_TYPES: &[&str] = &[
    "LocalId",
    "TypeId",
    "String",
];

fn count_sidecar_declarations() -> usize {
    let mut total = 0;
    for value in SIDECAR_VALUE_TYPES {
        for key in SIDECAR_KEY_TYPES {
            // Match `HashMap<key, value>`, `FxHashMap<key, value>`,
            // `BTreeMap<key, value>`. Tolerant of whitespace. Comment-
            // line matches are ignored — a doc-comment that *describes*
            // a retired sidecar (e.g. `/// replaces the legacy
            // `FxHashMap<LocalId, LocalOwnership>` snapshot`) is not a
            // sidecar.
            // Accept optional `mod::path::` prefix on both key and value
            // types so qualified-path declarations (e.g.
            // `FxHashMap<crate::ir::types::TypeId, ::DropStrategy>`)
            // still match.
            let pattern_str = format!(
                r"(?:Hash|FxHash|BTree)Map\s*<\s*(?:\w+::)*{key}\s*,\s*(?:\w+::)*{value}\s*>"
            );
            let pattern = regex::Regex::new(&pattern_str).unwrap();
            visit("src", &mut |path| {
                if path.extension().map_or(true, |e| e != "rs") {
                    return;
                }
                let content = match fs::read_to_string(path) {
                    Ok(s) => s,
                    Err(_) => return,
                };
                for line in content.lines() {
                    let trimmed = line.trim_start();
                    if trimmed.starts_with("//") {
                        continue;
                    }
                    total += pattern.find_iter(line).count();
                }
            });
        }
    }
    total
}

/// Tier 2d ratchet: typed-metadata fields have ONE source of truth,
/// not parallel sidecar maps. New sidecar declarations fail the test.
///
/// Baseline 2026-05-10: 0. The floor is clean — older sidecars
/// (`mut_capture_locals`, `view_returning_temps`, `is_resource`
/// callback) were retired during Phase D / Phase A migrations.
#[test]
fn no_typed_metadata_sidecars() {
    const BUDGET: usize = 0;

    let count = count_sidecar_declarations();
    assert!(
        count <= BUDGET,
        "Tier 2d sidecar absence violated: {count} > {BUDGET}.\n\n\
         A new `HashMap<key, value>` / `FxHashMap` / `BTreeMap` was \
         introduced in src/ where the value type is a watched typed-\
         metadata axis (DropStrategy / CopySemantics / CollectionKind / \
         EnumKind / EnumCategory / LocalOwnership / BorrowOrigin). The \
         canonical home for these facts is the typed field on \
         TypeMetadata / Local / Inst; a parallel registry is a Layering \
         discipline rule 3 violation (`docs/internals/layering-\
         discipline.md`).\n\n\
         To find new sites:\n  \
         grep -rnE 'HashMap\\s*<\\s*(LocalId|TypeId|String)\\s*,\\s*(DropStrategy|CopySemantics|CollectionKind|EnumKind|EnumCategory|LocalOwnership|BorrowOrigin)\\s*>' src/\n\n\
         Either:\n  \
         1. Migrate the lookup to read the typed field directly via \
            the canonical accessor.\n  \
         2. If the map is a per-pass scratch computed from the typed \
            field (not a parallel persistent registry), add an \
            allowlist entry to SIDECAR_VALUE_TYPES with citation."
    );
}

/// Tier 3b — Phase D state coherence. Per `docs/internals/structural-
/// guards.md` Tier 3b:
///
/// > **Rule.** `LocalOwnership` is the source of truth for ownership and
/// > borrow tracking. Any consumer reading `drops.is_registered`,
/// > `is_named_local`, `is_owned_local`, etc. as proxies for ownership
/// > is a discipline violation that should migrate to the typed
/// > accessor.
///
/// The proxies — `is_named_local`, `is_owned_local`, `drops.is_registered`,
/// `drops.is_moved` — predate Phase D's typed `Local.ownership` field
/// and have been retired site-by-site over the past sessions (Phase D4
/// closed `is_named_local` from `lower_var_decl_assign_mode`'s decision
/// tree). The full retirement is multi-session; this ratchet locks the
/// current floor so further migration is one-way.
///
/// **What counts as a proxy read:**
///   - `<expr>.is_named_local(...)` — duplicates `Local.ownership ==
///     Owned/Borrowed{Param}` (named locals get a Borrowed-Param
///     ownership slot at fn entry).
///   - `<expr>.is_owned_local(builder, local)` — duplicates
///     `Local.ownership == Owned | FreshOwned`.
///   - `drops.is_registered(local)` — duplicates "is the local
///     drop-tracked" which Phase D's `LocalOwnership` axis already
///     encodes (Borrowed/View slots aren't drop-tracked; Owned/
///     FreshOwned/Untracked-resource slots are).
///   - `drops.is_moved(local)` — duplicates `LocalOwnership` post-
///     move-zero state (the move-zero is the source of truth in the IR
///     shape, not a sidecar flag).
///
/// **Comment-line matches are skipped** so doc-comments that *describe*
/// the proxies (e.g. when documenting their retirement) don't trip the
/// lint. Definition lines (`fn is_named_local(...)`, `fn is_registered(...)`)
/// are excluded — they're the canonical implementations, not proxy
/// reads.
///
/// **Baseline 2026-05-10: 64 proxy reads** (across `src/ir/lowering/...`).
/// Each future migration that retires a site decreases the budget; a
/// new proxy read fails the test.
const PHASE_D_PROXY_PATTERNS: &[&str] = &[
    r"\.is_named_local\s*\(",
    r"\.is_owned_local\s*\(",
    r"\.drops\s*\.\s*is_registered\s*\(",
    r"\.drops\s*\.\s*is_moved\s*\(",
];

fn count_phase_d_proxy_reads() -> usize {
    let mut total = 0;
    for pat_str in PHASE_D_PROXY_PATTERNS {
        let pattern = regex::Regex::new(pat_str).unwrap();
        visit("src", &mut |path| {
            if path.extension().map_or(true, |e| e != "rs") {
                return;
            }
            let content = match fs::read_to_string(path) {
                Ok(s) => s,
                Err(_) => return,
            };
            for line in content.lines() {
                let trimmed = line.trim_start();
                if trimmed.starts_with("//") {
                    continue;
                }
                // Skip the function definition lines themselves —
                // `pub fn is_named_local(...)`, etc.
                if trimmed.contains("fn is_named_local")
                    || trimmed.contains("fn is_owned_local")
                    || trimmed.contains("fn is_registered")
                    || trimmed.contains("fn is_moved")
                {
                    continue;
                }
                total += pattern.find_iter(line).count();
            }
        });
    }
    total
}

/// Tier 3b ratchet: Phase D state coherence. Proxy reads of
/// `is_named_local` / `is_owned_local` / `drops.is_registered` /
/// `drops.is_moved` should migrate to typed `Local.ownership` reads.
/// New proxy reads fail the test.
///
/// Baseline 2026-05-10: 64 proxy reads. Lower as Phase D migration
/// proceeds.
#[test]
fn no_growth_in_phase_d_proxy_reads() {
    /// Maximum allowed proxy-read count. Lower as Phase D migrates
    /// `is_named_local`/`is_owned_local`/`drops.is_registered`/
    /// `drops.is_moved` callsites to typed `Local.ownership` reads.
    const BUDGET: usize = 64;

    let count = count_phase_d_proxy_reads();
    assert!(
        count <= BUDGET,
        "Phase D proxy-read count grew beyond budget: {count} > {BUDGET}.\n\n\
         Tier 3b (`docs/internals/structural-guards.md`) bars new proxy \
         reads of ownership state. The typed source of truth is \
         `Local.ownership` (Phase D's `LocalOwnership` field). Proxies \
         duplicate the same fact and drift from each other under \
         complex CFG paths.\n\n\
         To find new sites:\n  \
         grep -rnE '\\.(is_named_local|is_owned_local)\\s*\\(|drops\\s*\\.\\s*(is_registered|is_moved)\\s*\\(' src/ | grep -v '//'\n\n\
         Either:\n  \
         1. Migrate to read `builder.locals[local.0 as usize].ownership` \
            (or `ctx.source_ownership(...)` for operands).\n  \
         2. If the proxy is genuinely needed (e.g. inside the proxy's \
            own implementation), exclude its file/line via the \
            comment-skip / fn-def-skip already in this lint.\n\n\
         If the count went DOWN, lower BUDGET in this file to lock the \
         new floor."
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
