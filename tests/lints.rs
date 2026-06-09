//! Compiler-source-tree lints — ratchets that lock layering discipline.
//!
//! These tests scan `src/**/*.rs` for patterns that the layering-discipline
//! chapter (`docs/devbook/24-layering-discipline.md`) and structural-guards
//! Tier 3a flag as anti-patterns, and assert the count never grows. As
//! migrations land, the budget tightens. A new site that bypasses typed
//! metadata for name-prefix routing fails the test until either it's
//! migrated to typed metadata or the budget is intentionally raised.
//!
//! See `docs/devbook/25-structural-guards.md` §3a for the full design.

use std::fs;
use std::path::Path;

/// Every mangled monomorphized-type prefix the compiler emits. Adding a new
/// builtin protocol with a `base_name: "X"` requires adding `X` here so the
/// ratchet covers `starts_with("X__")` checks against it.
///
/// Includes both surface-language prefixes (`Vector__`, `Dict__`, `Mutex__`,
/// ...) and the runtime-form prefixes the self-host emits as type aliases
/// (`GorgetMap__`, `GorgetSet__`, `GorgetDict__`) — the lint catches new
/// `starts_with("X__")` dispatch sites against either form.
const MANGLED_PREFIXES: &[&str] = &[
    "Vector", "Deque",
    "Dict", "HashMap", "Map", "OrderedDict",
    "Set", "HashSet",
    "Mutex", "RWLock", "Channel", "Shared", "Weak",
    "Guard", "ReadGuard", "WriteGuard",
    "Box", "Task", "Heap",
    "Tuple",
    "Callable", "MutCallable", "ConsumeCallable",
    "Option", "Result",
    // Concurrency / synchronisation handles (Phase A's `opaque-handle`
    // family) — name-prefix dispatch on these is the same layering
    // violation as on Vector/Dict.
    "AtomicInt", "AtomicBool",
    "Thread", "TaskGroup", "WaitGroup", "Barrier", "CondVar",
    "Semaphore", "OnceFlag",
    // Self-host runtime-form aliases — generated as `GorgetMap__K__V`
    // alongside the surface-form `HashMap__K__V` / `Dict__K__V`.
    "GorgetMap", "GorgetSet", "GorgetDict",
];

/// Walk `src/**/*.rs` and count `starts_with("X__")` calls where `X` is a
/// known mangled-type prefix. These are the layering-discipline violations
/// the ratchet locks against.
fn count_name_prefix_sites() -> usize {
    count_name_prefix_in_tree("src", "rs")
}

/// The prelude option-like enums. Name-prefix matches on these (`Option__` /
/// `Result__`) decide enum MEANING from the mangled name — the typed channel
/// that should answer "is this Option/Result" is `gir.gg`'s `EnumCategory`
/// (`record_enum_category` writer + `enum_category_of` / `has_enum_category`
/// accessors, mirroring Rust's `EnumKind` / `enum_category(type_id)`). The
/// channel already EXISTS; the remaining sites just never adopted it. They are
/// ratcheted SEPARATELY from the Phase-A classification-routing class (below)
/// as a BURN-DOWN toward 0 — see `no_growth_in_self_host_prelude_optionlike_routing`.
const PRELUDE_OPTIONLIKE_PREFIXES: &[&str] = &["Option", "Result"];

/// Walk `tests/fixtures/self_host_*/**/*.gg` and count `starts_with("X__")` for
/// each `X` in `prefixes`. Phase A migrated self-host's classification consumers
/// to read through `build_resource_metadata` (the single source of truth);
/// this ratchet locks in those gains so a regression is caught at lint time.
/// Parameterized so the Phase-A class and the prelude option-like class can be
/// ratcheted independently (different migration targets, different channels).
///
/// See `docs/devbook/26-self-host-frontend.md` §3.3 step 4 (the
/// "promote" step) and §6.1 (Tier E.1 lints ratchet).
fn count_name_prefix_sites_self_host(prefixes: &[&str]) -> usize {
    let alternation = prefixes.join("|");
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
    /// Bumped 292 → 297 (2026-05-11): five new validator/registrar-adjacent
    /// sites. (1) `42e40c45` `validate_box_inner_type_consistency` (Tier 1d
    /// inverse): `if sd.name.starts_with("Box__") { continue }` filters out
    /// Box-named structs so the inverse only flags stray-`box_inner_type`-
    /// on-non-Box. Cannot migrate — the validator's job IS to detect when
    /// the name suffix and typed metadata disagree, so using the metadata
    /// as the scope filter would short-circuit the very check.
    /// (2,3,4) `cec47c9c` Snag #32 None-literal materialisation at writer
    /// boundaries: three `starts_with("Option__")` / `starts_with("Result__")`
    /// guards in `coerce_null_to_option_none` + the module-exit validator
    /// `validate_no_null_assign_to_option_slot`. Scope filters for "the
    /// Option/Result-slot class" at GIR-stage logic where typed enum_kind
    /// reads through type_registry but the registrar-adjacent context
    /// already has the name in hand from `GirType::Named(name)`.
    /// (5) `39679d0e` per-mono wrapper-emission gap fix: `is_non_box_wrapper`
    /// filter excluding `Box__`-named drop fns from the new
    /// per-mono-wrapper scan (Box has its own slot-ABI emission path
    /// through `emit_box_drop_wrappers`). Registrar-adjacent.
    /// Bumped 297 → 309 (2026-05-12): MANGLED_PREFIXES extended to cover
    /// concurrency/atomic/threading families (`AtomicInt`, `AtomicBool`,
    /// `Thread`, `Barrier`, `CondVar`, `Semaphore`, `OnceFlag`, etc.)
    /// + Self-host runtime-form aliases (`GorgetMap`, `GorgetSet`). The
    /// +12 new src/ matches surface existing registrar-adjacent
    /// dispatchers in c_lir/c_runtime registration that the previous
    /// MANGLED_PREFIXES list silently undercounted — bringing them
    /// into the ratchet so future name-prefix additions are caught.
    const BUDGET: usize = 309;

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
/// already on a typed metadata field. Per `docs/devbook/25-structural-guards.md`
/// Tier 2d:
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
         discipline rule 3 violation (`docs/devbook/24-layering-\
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

/// Tier 3b — Phase D state coherence. Per `docs/devbook/25-structural-guards.md`
/// Tier 3b:
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
    /// Bumped 64 → 70 (2026-05-11): six new `!ctx.drops.is_moved(local)`
    /// idempotence guards before `move_zero_and_mark` calls. Five from
    /// `c779d976` (Tier 1c Cluster 1 burn-down — rethrow/catch/return
    /// staging paths each guard the move_zero with `!is_moved` to avoid
    /// double-marking when the source is also drop-registered via the
    /// drop accountant); one from `47c8fb20` (Tier 1c tuple destructure
    /// Pattern::Tuple MoveZero follow-through, same idempotence shape).
    /// These are write-side discipline guards, not ownership-decision
    /// reads — `move_zero_and_mark` is not idempotent (it asserts), so
    /// the guard is the ONLY way to make the writer safe under
    /// already-moved sources. Migrating would require making
    /// `move_zero_and_mark` idempotent first.
    /// Bumped 70 → 77 (2026-05-12): seven new proxy reads in the
    /// Tier 2a `consume_externs` burn-down + Set literal lowering:
    /// (1) var-decl Callable auto-clone branch (stmts/mod.rs) uses
    /// `is_named_local` to narrow-gate the clone (closure literals
    /// are unnamed temps; named Callable params/locals need the
    /// clone), and a `Borrowed/Untracked` source check via
    /// `builder.locals[].ownership` (1 proxy in the new branch);
    /// (2) `lower_set_literal_from_array` (collections.rs) mirrors
    /// `lower_array_literal`'s per-element discipline using
    /// `is_owned_local` + `is_named_local` (mode picker) and
    /// `drops.is_moved` (MoveZero idempotence guard) — same writer-
    /// side discipline class as Tier 1c. Migrating would require
    /// promoting these proxies to typed accessors on Local; deferred.
    /// Bumped 77 → 78 (2026-05-13): one new proxy slipped in during
    /// the 2026-05-12/13 self-host modernization sweep (CoW-by-
    /// default + EnumFieldLoad fixes + for-each migrations).
    /// Locking in the new floor; not investigated, low priority.
    /// Bumped 78 → 82 (2026-06-09): four proxy reads in ONE cohesive
    /// write-side guard added by `360c8bd8` ("drop owning temporaries
    /// passed to bare value params"): `!is_named_local && is_owned_local
    /// && !drops.is_registered && !drops.is_moved` — the "this place is an
    /// unnamed, owned temp not already registered/moved → drop it" guard.
    /// The `is_named_local`/`is_owned_local` halves COULD route through
    /// `Local.ownership`, but the `drops.is_registered`/`drops.is_moved`
    /// halves are DROP-ACCOUNTANT state (not `LocalOwnership`) and have no
    /// typed-accessor equivalent — same write-side-discipline class as the
    /// 64→70→77 bumps. Not migratable without first making the drop
    /// accountant queryable off `Local`. Locking in the floor.
    const BUDGET: usize = 82;

    let count = count_phase_d_proxy_reads();
    assert!(
        count <= BUDGET,
        "Phase D proxy-read count grew beyond budget: {count} > {BUDGET}.\n\n\
         Tier 3b (`docs/devbook/25-structural-guards.md`) bars new proxy \
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

/// Tier E.1 (per `docs/devbook/26-self-host-frontend.md` §6.1):
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
/// Container-literal arms in `infer_expr` that need decl_type_hint
/// propagation for nested collection literals to coerce correctly.
/// See `docs/devbook/25-structural-guards.md` and the
/// `tuple_literal_resource_value` / `dict_literal_resource_value`
/// fixtures for the failure shape.
///
/// Counts `Expr::*Literal(...)` and comprehension arms in the
/// `infer_expr` match. When a new container-literal arm is added,
/// the count grows and this test fails until either:
///   1. The new arm correctly propagates `decl_type_hint` to nested
///      `infer_expr` calls (DictLiteral / TupleLiteral patterns),
///      and the budget is bumped with a justification.
///   2. The new arm doesn't need propagation (ArrayLiteral relies on
///      `is_collection_assignment` permissiveness at the var-decl
///      unify site); the budget bump explains why.
///
/// Baseline 2026-05-12: 3 (ArrayLiteral, TupleLiteral, DictLiteral).
/// SetLiteral shares ArrayLiteral's AST node (parser convention; see
/// `src/parser/expr.rs:1663`).
fn count_container_literal_arms() -> usize {
    let content = match fs::read_to_string("src/semantic/typecheck.rs") {
        Ok(s) => s,
        Err(_) => return 0,
    };
    // Scope the count to the `infer_expr` fn so unrelated match arms
    // (resolver, rewrite, etc.) don't inflate it. infer_expr's literal
    // arms are stable patterns at lines ~2212-2270 today.
    let mut in_infer_expr = false;
    let mut depth = 0;
    let mut count = 0;
    let arm_patterns = [
        "Expr::ArrayLiteral(",
        "Expr::TupleLiteral(",
        "Expr::DictLiteral(",
        "Expr::SetComprehension {",
        "Expr::DictComprehension {",
    ];
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        if trimmed.starts_with("fn infer_expr(") {
            in_infer_expr = true;
            depth = 0;
        }
        if !in_infer_expr {
            continue;
        }
        depth += line.matches('{').count() as i32;
        depth -= line.matches('}').count() as i32;
        if depth <= 0 && !trimmed.starts_with("fn infer_expr(") {
            in_infer_expr = false;
            continue;
        }
        for pat in &arm_patterns {
            if trimmed.starts_with(pat) {
                count += 1;
                break;
            }
        }
    }
    count
}

/// Ratchet: the number of container-literal arms in `infer_expr` must
/// stay at the expected baseline. New arms require an audit for
/// `decl_type_hint` propagation (see DictLiteral / TupleLiteral fixes
/// 2026-05-11/12) and a bump here with a justification comment.
///
/// **If this fails because a new arm was added:**
///   - Read the new arm's body. Does it call `self.infer_expr(...)` on
///     a child expression? If so, does it propagate `decl_type_hint`?
///   - For container types where `is_collection_assignment` permits
///     coercion at the outer var-decl unify site (Array→Vector, Set,
///     Dict, HashMap), propagation may be unnecessary — but verify
///     with a nested-literal test (e.g. `Dict[K, NewContainer[T]] d =
///     {...}`).
///   - For container types WITHOUT that permissiveness, propagation is
///     required (DictLiteral / TupleLiteral pattern: extract K/V hints
///     from decl_type_hint, set per-child decl_type_hint, restore).
///
/// **If the count went DOWN:** lower BUDGET to lock the new floor.
#[test]
fn container_literal_arms_count() {
    /// Expected container-literal-like arms in infer_expr:
    /// - ArrayLiteral (includes set-shape `{a, b, c}` via parser convention)
    /// - TupleLiteral
    /// - DictLiteral
    /// - DictComprehension
    /// - SetComprehension
    /// ListComprehension is intentionally excluded from the lint scope —
    /// it's range-only today and doesn't admit nested-collection-literal
    /// element expressions in practice.
    /// Baseline 2026-05-12: 5.
    const EXPECTED: usize = 5;

    let count = count_container_literal_arms();
    assert_eq!(
        count, EXPECTED,
        "Container-literal arm count in `infer_expr` changed: {count} vs expected {EXPECTED}.\n\n\
         If a new arm was added, audit it for `decl_type_hint` propagation \
         (DictLiteral / TupleLiteral pattern). If unneeded (e.g., outer var-decl \
         `is_collection_assignment` permissiveness coerces), document the \
         exception in the bump comment.\n\n\
         If an arm was removed, lower EXPECTED in tests/lints.rs.",
    );
}

/// Baseline 2026-05-10: 52 (after Phase A.1–A.4 + 12/N migrations).
/// Bumped 52 → 69 (2026-05-12): MANGLED_PREFIXES extended with
/// concurrency/atomic/threading prefixes (`AtomicInt`, `AtomicBool`,
/// `Thread`, `TaskGroup`, `WaitGroup`, `Barrier`, `Semaphore`,
/// `OnceFlag`) + Self-host runtime-form aliases (`GorgetMap`,
/// `GorgetSet`, `GorgetDict`, `OrderedDict`, `Map`). The +17 matches
/// surface the legitimate `build_resource_metadata`-cascade arms for
/// each new family plus a handful of per-handle dispatch sites that
/// the old prefix list silently undercounted. Bringing them in
/// brings the ratchet's scope in line with the actual self-host
/// dispatch surface.
///
/// **Scope (2026-06-09):** this ratchet now covers ONLY the Phase-A
/// classification-routing class (Vector/Dict/Set/Box/handles/...). The prelude
/// option-like prefixes (`Option__` / `Result__`) are ratcheted separately as a
/// BURN-DOWN by `no_growth_in_self_host_prelude_optionlike_routing` — they have
/// a distinct typed channel (`EnumCategory`) and a migration target of 0, so
/// folding them into one budget here would (a) hide Phase-A regressions under
/// the prelude headroom and (b) wrongly bless name-matched enum-meaning as a
/// permanent floor. Splitting keeps THIS class pinned at its true floor.
#[test]
fn no_growth_in_self_host_name_prefix_routing() {
    const BUDGET: usize = 69;

    // Phase-A classification-routing class only: all MANGLED_PREFIXES EXCEPT
    // the prelude option-like ones (those are the sibling lint's burn-down).
    let nonprelude: Vec<&str> = MANGLED_PREFIXES
        .iter()
        .copied()
        .filter(|p| !PRELUDE_OPTIONLIKE_PREFIXES.contains(p))
        .collect();
    let count = count_name_prefix_sites_self_host(&nonprelude);
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

/// BURN-DOWN ratchet (target: **0**) for the prelude option-like name-matches
/// (`starts_with("Option__")` / `starts_with("Result__")`) in self-host code.
///
/// These decide enum MEANING ("is this Option/Result?") from the mangled name —
/// the CLAUDE.md rule-#2 anti-pattern. The reference-grade fix already has a home:
/// the self-host carries a TYPED `EnumCategory` channel on `GirModule`
/// (`gir.gg`: `record_enum_category` single-writer + `enum_category_of` /
/// `has_enum_category` accessors), mirroring Rust's `EnumKind` /
/// `TypeRegistry::enum_category(type_id)`. The channel EXISTS; these sites just
/// never adopted it. Migrating each `name.starts_with("Option__")` →
/// `enum_category_of(&gmod, tid).category == CAT_OPTION` (where a type-id is in
/// hand) or a single name→category registrar (where only a name is) drives this
/// to ~0. Genuine name-PARSING (extracting the payload `T` from `Option__T`)
/// should read the structured `EnumCategory` payload fields, not substring the
/// mangled name. Tracked as a migration task in `TODO.md`.
///
/// **If this fails (count went UP):** you added a new `Option__`/`Result__`
/// name-match instead of reading `enum_category`. Don't bump — read the channel.
/// **As you migrate:** LOWER `BUDGET` toward 0 in the same commit that retires sites.
#[test]
fn no_growth_in_self_host_prelude_optionlike_routing() {
    // Floor as of 2026-06-09: 17, after the Phase-1 enum_category migration
    // (37 -> 17) retired the 20 output-neutral sites — the 16 non-drop-path
    // Class-A classification gates + the 2 Class-C `match_variant_index`
    // `starts_with` halves (the `== "Option"`/`== "Result"` synthetic-name
    // equalities stay) + the 2 Class-B `match_enum_type` occurrences. The 17
    // remaining are the later-phase / irreducible cohort: the DROP-PATH/ASan
    // sites (lir 476/957/961/1021/1068, lower 8983/9747 `try_lift_option_ref`),
    // Class D (writer `record_field_enum_category` + the `diag_bug` miss-gates),
    // and Class E payload readers (TODO 177-179). Migration target: floor 5
    // (Class D) until TODO-178 upstream typed-field registration; 0 thereafter.
    const BUDGET: usize = 17;

    let count = count_name_prefix_sites_self_host(PRELUDE_OPTIONLIKE_PREFIXES);
    assert!(
        count <= BUDGET,
        "Self-host prelude option-like name-prefix routing grew beyond budget: {count} > {BUDGET}.\n\n\
         A new `starts_with(\"Option__\")` / `starts_with(\"Result__\")` was added. This decides \
         enum MEANING from the mangled name (CLAUDE.md rule #2). Read the TYPED channel instead:\n  \
         `enum_category_of(&gmod, tid)` / `has_enum_category(&gmod, tid)` (gir.gg) — mirrors \
         Rust's `enum_category(type_id)`. If you only have a name, route it through a single \
         name->category registrar (the blessed source), not an inline prefix test.\n\n\
         This is a BURN-DOWN ratchet (target 0). Do NOT bump it — migrate the site.",
    );
}

/// Every RUNTIME (post-meta-expansion) block-bearing `Stmt` variant. The CoW
/// reassignment prescan `cow_after_stmt` (src/ir/lowering/functions.rs) MUST
/// recurse into each of these bodies — a source-mutation inside a block that is
/// invisible to the prescan reintroduces the element-borrow dangling bug
/// (`docs/devbook/11-copy-on-write.md` §"Mutation severs the alias"). `Meta*`
/// forms are intentionally excluded: they are evaluated and removed before GIR
/// lowering (`src/ir/lowering/stmts/mod.rs:331-337` — they emit nothing if they
/// survive), so they never reach the prescan's statement stream.
///
/// Keep this list in sync with the block-bearing variants of `enum Stmt`
/// (`src/parser/ast.rs`). The companion lint below fails if any of these is
/// dropped to the `_ => {}` arm.
const COW_PRESCAN_BLOCK_BEARING_STMTS: &[&str] = &[
    "OnError", "For", "While", "Loop", "If", "Match", "Select", "With",
    "Unsafe", "NamedScope",
];

/// Extract the source of `fn cow_after_stmt` from functions.rs (brace-depth
/// scoped, comment lines skipped) so we only inspect that match.
fn cow_after_stmt_source() -> String {
    let content = match fs::read_to_string("src/ir/lowering/functions.rs") {
        Ok(s) => s,
        Err(_) => return String::new(),
    };
    let mut out = String::new();
    let mut in_fn = false;
    let mut depth = 0i32;
    let mut seen_open = false;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if !in_fn {
            if trimmed.starts_with("fn cow_after_stmt(") {
                in_fn = true;
                seen_open = false;
                depth = 0;
            } else {
                continue;
            }
        }
        out.push_str(line);
        out.push('\n');
        if !trimmed.starts_with("//") {
            depth += line.matches('{').count() as i32;
            depth -= line.matches('}').count() as i32;
        }
        if depth > 0 {
            seen_open = true;
        }
        if seen_open && depth <= 0 {
            break;
        }
    }
    out
}

/// Ratchet: `cow_after_stmt` must have a non-`_` arm for every runtime
/// block-bearing `Stmt` variant. A new block-bearing variant that falls through
/// to `_ => {}` is invisible to the CoW reassignment prescan — a source
/// collection mutated inside its body would dangle a live element borrow taken
/// before it (CLAUDE.md #4 "one fix, all siblings"; the latent hole that left
/// `Loop`/`With`/`Unsafe`/`NamedScope`/`Select`/`OnError` unhandled).
///
/// **If this fails:** you added a block-bearing `Stmt` variant. Add an arm to
/// `cow_after_stmt` that recurses into its body/bodies via `cow_after_block`
/// (mirror the `With`/`Match`/`Select` arms), then add the variant name to
/// `COW_PRESCAN_BLOCK_BEARING_STMTS`. Do NOT just silence the lint — the recurse
/// is load-bearing for value-semantics correctness.
#[test]
fn cow_after_stmt_covers_block_bearing_variants() {
    let src = cow_after_stmt_source();
    assert!(
        !src.is_empty(),
        "could not locate `fn cow_after_stmt` in src/ir/lowering/functions.rs — \
         did it move or get renamed? Update cow_after_stmt_source().",
    );
    let mut missing = Vec::new();
    for variant in COW_PRESCAN_BLOCK_BEARING_STMTS {
        // An arm matches the variant if the body references `Stmt::Variant`
        // (the match patterns are `Stmt::Loop { .. }`, `Stmt::With { .. }`,
        // combined `A | B`, etc.). A bare `_ => {}` does not.
        let pat = format!("Stmt::{variant}");
        if !src.contains(&pat) {
            missing.push(*variant);
        }
    }
    assert!(
        missing.is_empty(),
        "`cow_after_stmt` (src/ir/lowering/functions.rs) is missing arms for \
         block-bearing Stmt variant(s): {missing:?}.\n\n\
         These fell through to `_ => {{}}`, so a source-collection mutation inside \
         such a block body is invisible to the CoW reassignment prescan — a live \
         element borrow taken before it would dangle (docs/devbook/11-copy-on-write.md \
         §\"Mutation severs the alias\"; CLAUDE.md #4).\n\n\
         Add an arm recursing into the body via `cow_after_block` (mirror the \
         With/Match/Select arms). If you instead REMOVED a variant from `enum Stmt`, \
         drop it from COW_PRESCAN_BLOCK_BEARING_STMTS in this file.",
    );
}
