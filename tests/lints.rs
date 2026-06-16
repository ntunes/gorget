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
    /// Lowered 309 → 257 (2026-06-16): two changes, no behaviour change.
    /// (1) Tightened the loose floor: the re-derived count had been 269
    /// (budget was 40 above the actual site count); locked to the floor.
    /// (2) Retired 12 dead "Legacy prefix fallback" sites — the
    /// Vector__/Deque__/Dict__/HashMap__/Set__/HashSet__ prefix arms in
    /// `lir/lower/operands.rs` (6) and `lir/lower/insts.rs` (6) that fired
    /// only when `resources::table().lookup(name)` returned None for those
    /// names. The table now carries MkPrefix entries for all six, so the
    /// fallbacks were unreachable (proven by panic-instrumenting both
    /// blocks and running the collection-fixture corpus + the full
    /// self-host self-compile — never hit).
    const BUDGET: usize = 257;

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
    /// Bumped 82 → 83 (2026-06-14): one new proxy read from Conformance
    /// Bug 2 (`store-to-static is a consuming position`, `assigns.rs:451`):
    /// the `!ctx.drops.is_moved(place.local)` idempotence guard before
    /// `move_zero_and_mark` in the static-assign branch. SAME write-side-
    /// discipline class as the 64→70→77→78→82 bumps — `move_zero_and_mark`
    /// is non-idempotent (it asserts), and `is_moved` is drop-accountant
    /// state with no `LocalOwnership` accessor, so it is not migratable
    /// without first making the drop accountant queryable off `Local`.
    /// Locking in the floor.
    const BUDGET: usize = 83;

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

/// Snag #11 sibling-guard ratchet (CLAUDE.md rule 4). Every auto-propagation
/// position in the typechecker must route through the SHARED error-type gate so
/// a cross-error-type propagation can't slip the (memory-unsafe) memcpy
/// miscompile. There are exactly two ways to reach the gate:
///   - the Route-B consumer guards call `self.auto_prop_skips_unify(...)` (which
///     internally calls `auto_prop_error_gate`), and
///   - the Route-A producer-peel calls `self.auto_prop_error_gate(...)` directly.
/// The total count of these gated propagation positions is pinned here; the next
/// propagation site added without going through one of them changes the count and
/// trips this lint, forcing it onto the shared E-checked path.
///
/// Baseline 2026-06-11: 14 `auto_prop_skips_unify` consumer sites + 1 Route-A
/// `auto_prop_error_gate` producer site = 15. (`auto_prop_error_gate`'s OTHER
/// in-source mention is its single call inside `auto_prop_skips_unify`, which is
/// the plumbing, not a propagation site — it's excluded by counting only `self.`
/// receiver calls and subtracting that one internal call.)
#[test]
fn snag11_auto_prop_gate_site_count() {
    const EXPECTED_SKIPS_UNIFY: usize = 14;
    const EXPECTED_ROUTE_A_GATE: usize = 1;

    let content = fs::read_to_string("src/semantic/typecheck.rs").unwrap_or_default();
    let mut skips_unify = 0usize;
    let mut route_a_gate = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        // Consumer-guard calls (Route B) — the `self.` receiver form, not the
        // `fn auto_prop_skips_unify(` definition.
        skips_unify += line.matches(".auto_prop_skips_unify(").count();
        // Route-A producer-peel direct gate call. The call INSIDE
        // `auto_prop_skips_unify`'s body (`self.auto_prop_error_gate(callee_err,
        // prop_span)`) is the plumbing, not a propagation position — exclude it
        // by name of its argument.
        for _ in 0..line.matches(".auto_prop_error_gate(").count() {
            if line.contains(".auto_prop_error_gate(callee_err,") {
                continue; // the internal plumbing call
            }
            route_a_gate += 1;
        }
    }
    assert_eq!(
        skips_unify, EXPECTED_SKIPS_UNIFY,
        "auto_prop_skips_unify call-site count changed: {skips_unify} vs {EXPECTED_SKIPS_UNIFY}.\n\n\
         If you added an auto-propagation consumer position (push/put/return/arg/\
         field-init/cond/index), it MUST call `auto_prop_skips_unify(declared, value, \
         span)` — NOT the bare `unify` — so the snag #11 cross-error-type gate runs. \
         Then bump EXPECTED_SKIPS_UNIFY here.\n\
         If you removed one, lower it. Never reach the auto-prop choke point without \
         the shared E-check.",
    );
    assert_eq!(
        route_a_gate, EXPECTED_ROUTE_A_GATE,
        "Route-A `auto_prop_error_gate` producer-peel site count changed: \
         {route_a_gate} vs {EXPECTED_ROUTE_A_GATE}.\n\n\
         The producer-peel (the `throws`-fn-call `Result[T,E] -> T` peel) must gate the \
         discarded `err_ty` via `auto_prop_error_gate(err_ty, span)`. A second peel site \
         without it re-opens the snag #11 hole. Bump only if you intentionally added \
         another producer-peel that ALSO gates.",
    );
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

/// Ratchet: the comprehension dispatch in the SELF-HOST `lower_expr_inner`
/// (`tests/fixtures/self_host_lowerer/lower_expr.gg`) is a 3-arm enumerated
/// class — `EListComp` / `ESetComp` / `EDictComp`. Each routes through the
/// shared `lower_*_comprehension` + `comp_make_*_acc` + `comp_synth_*_body`
/// helpers (which reuse the run-proven `lower_for_range`/`lower_for_string`/
/// `lower_for_vector` loop machinery). A future `E…Comp` variant that lands
/// in the `else:` fallback would SILENTLY miscompile to a Unit stub (the
/// pre-port behavior that made set/dict comps CRASH through the self-host),
/// so this lint forces the next comprehension variant through the shared path.
///
/// This is DISTINCT from `container_literal_arms_count` above: that lint scans
/// the RUST `infer_expr` (typecheck) and already lists Set/Dict comprehension;
/// THIS one scans the SELF-HOST Gorget-source `lower_expr_inner` dispatch
/// (GIR lowering). The two layers are pinned independently.
///
/// **If this fails because a new arm was added:** the new `case E…Comp(...)`
/// MUST call a shared `lower_*_comprehension` helper (not inline the loop, not
/// fall into the `else:` Unit stub) AND — for a set-shaped comp — convert the
/// raw `Box[SpannedExpr]` filter SENTINEL (`EIntLiteral(0)` = no `if`) to an
/// `Option` via `setcomp_filter_opt` before feeding the synth body, else an
/// unfiltered comp becomes `if 0:` → empty result. Then bump EXPECTED.
/// **If an arm was removed:** lower EXPECTED to lock the new floor.
#[test]
fn self_host_comprehension_dispatch_arms_count() {
    /// Baseline 2026-06-14: 3 (EListComp + ESetComp + EDictComp).
    const EXPECTED: usize = 3;

    // lower_expr.gg lives ONLY in self_host_lowerer (real file, not symlinked),
    // so no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower_expr.gg").unwrap_or_default();
    let mut arms = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with('#') {
            continue; // .gg comments
        }
        if trimmed.starts_with("case EListComp(")
            || trimmed.starts_with("case ESetComp(")
            || trimmed.starts_with("case EDictComp(")
        {
            arms += 1;
        }
    }

    assert_eq!(
        arms, EXPECTED,
        "Self-host `lower_expr_inner` comprehension dispatch-arm count changed: \
         {arms} vs {EXPECTED}.\n\n\
         The comprehension dispatch (EListComp/ESetComp/EDictComp) is an enumerated \
         class. A new `E…Comp` variant MUST route through a shared \
         `lower_*_comprehension` helper — NOT fall into the `else:` Unit stub (which \
         silently miscompiles to a Unit local and CRASHES the comp through the \
         self-host). Set-shaped comps must also convert the raw `Box[SpannedExpr]` \
         filter SENTINEL (`EIntLiteral(0)`) to an `Option` via `setcomp_filter_opt`. \
         Bump EXPECTED with a justification, or lower it if an arm was removed.",
    );
}

/// Snag #11 sibling-guard ratchet (CLAUDE.md rule 4 / "Sibling-site drift")
/// over the self-host trait-equip SYMBOL-MANGLE sites. The self-host mangles an
/// equip method's symbol via TWO routes, mirroring Rust gg
/// (`src/ir/lowering/traits.rs`):
///   - REGISTERED/vtable (a bodied `TraitDef`, the `did_split` path): BARE
///     `{trait}_for_{type}__method` via `mangle_trait_name` — NO generic suffix.
///   - UNREGISTERED (From/operators — no bodied TraitDef): the generic suffix
///     `{trait}__<arg>_for_{type}__method` via the CENTRALIZED
///     `mangle_trait_equip_name` helper (`traits.rs:1614`).
/// The unregistered route MUST go through `mangle_trait_equip_name` so the body
/// symbol matches Rust + the auto-prop From-conversion lookup
/// (`lower_match.gg maybe_emit_from_conversion`). A new equip-symbol site that
/// hand-rolls the suffix (or forgets it) re-opens the snag #11 OOB-read class.
///
/// Two counts are pinned so a new site is forced through the shared helper/gate:
///   (a) `mangle_trait_equip_name(` CALL sites — the unregistered-path mangle.
///       Baseline 2026-06-11: 2 (lower_closures.gg site 1 body +
///       lower.gg site 2 IEquip fn_sigs registration). The third occurrence is
///       the `fn` definition in lower_closures.gg (excluded — it's a `String `
///       return-typed signature line, not a call).
///   (b) `lower_equip_block(` CALL sites — each must pass the
///       `trait_is_registered` route flag so the gate inside picks BARE vs
///       suffixed. Baseline 2026-06-11: 6 (5 in lower.gg + 1 in lower_generics.gg;
///       the `void lower_equip_block(` definition in lower_closures.gg is
///       excluded).
///
/// **If this fails:** you added/removed an equip-mangle site. For (a) a new
/// unregistered-path mangle MUST call `mangle_trait_equip_name(tname, args)` —
/// never inline `mangle_trait_name(...) + "__" + ...`. For (b) a new
/// `lower_equip_block` caller MUST decide the route (`trait_is_registered` =
/// `trait_defs.contains(tname)` at the call site). Then bump the matching
/// baseline with a one-line justification.
#[test]
fn snag11_equip_symbol_mangle_site_count() {
    const EXPECTED_HELPER_CALLS: usize = 2;
    const EXPECTED_EQUIP_BLOCK_CALLS: usize = 6;

    // Self-host lowerer source. lower.gg / lower_closures.gg / lower_generics.gg
    // live ONLY in self_host_lowerer (not symlinked), so no double-count guard
    // is needed here.
    let files = [
        "tests/fixtures/self_host_lowerer/lower.gg",
        "tests/fixtures/self_host_lowerer/lower_closures.gg",
        "tests/fixtures/self_host_lowerer/lower_generics.gg",
    ];

    let mut helper_calls = 0usize;
    let mut equip_block_calls = 0usize;
    for f in &files {
        let content = fs::read_to_string(f).unwrap_or_default();
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("#") {
                continue; // .gg comments
            }
            // (a) helper CALL sites — exclude the `fn` definition (a `String `
            // return-typed signature, not a call expression).
            if !trimmed.starts_with("String mangle_trait_equip_name(") {
                helper_calls += line.matches("mangle_trait_equip_name(").count();
            }
            // (b) lower_equip_block CALL sites — exclude the `void` definition.
            if !trimmed.starts_with("void lower_equip_block(") {
                equip_block_calls += line.matches("lower_equip_block(").count();
            }
        }
    }

    assert_eq!(
        helper_calls, EXPECTED_HELPER_CALLS,
        "Self-host `mangle_trait_equip_name` CALL-site count changed: \
         {helper_calls} vs {EXPECTED_HELPER_CALLS}.\n\n\
         The UNREGISTERED trait-equip route (From/operators) must mangle its \
         symbol through the centralized `mangle_trait_equip_name(tname, args)` \
         helper so the body matches Rust gg + the auto-prop From lookup. Do NOT \
         hand-roll the generic suffix. Bump EXPECTED_HELPER_CALLS only when you \
         add/remove a genuine unregistered-path mangle site.",
    );
    assert_eq!(
        equip_block_calls, EXPECTED_EQUIP_BLOCK_CALLS,
        "Self-host `lower_equip_block` CALL-site count changed: \
         {equip_block_calls} vs {EXPECTED_EQUIP_BLOCK_CALLS}.\n\n\
         Every `lower_equip_block` caller MUST pass the `trait_is_registered` \
         route flag (`trait_defs.contains(tname)` at the call site) so the gate \
         inside picks BARE (registered/vtable) vs suffixed (unregistered). A new \
         caller that omits the route decision re-opens the snag #11 mangling \
         divergence. Bump EXPECTED_EQUIP_BLOCK_CALLS with a justification.",
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
/// Bumped 69 → 70 (2026-06-13): the self-host `thread_spawn`/Thread-method
/// port added ONE `thr_recv_tn.starts_with("Thread__")` site in
/// `lower_expr.gg`'s EMethodCall handler — the `Thread[T].join()`/`.id()`
/// intrinsic. This is a faithful mirror of Rust gg's identical dispatch
/// (`src/ir/lowering/exprs/methods.rs:1062` `ttn.starts_with("Thread__")`):
/// the receiver IS a `Thread__{ret_c}` opaque handle whose element C-type
/// rides in the name suffix (the `Thread__T__join` C symbol is the runtime
/// contract), so suffix-parsing here is the explicitly-allowed "extract T
/// from Vector__T" name-parsing case, not a classification-routing dodge.
/// Bumped 70 → 71 (2026-06-14): the self-host Vector sort-family port added ONE
/// `name.starts_with("Vector__")` site in `lir_lower.gg`'s `array_elem_for_sort`
/// — it strips the `Vector__` prefix + `__method` suffix to extract the element
/// type, choosing the typed qsort comparator suffix (`gorget_array_sort_int` /
/// `_float` / `_str` / `_generic`). This is the explicitly-allowed "extract T
/// from Vector__T" name-parsing case (the per-element C symbol IS the runtime
/// contract, mirroring Rust gg's `map_monomorphized_to_runtime`,
/// src/lir/lower/calls.rs:318), not a classification-routing dodge — the FAMILY
/// classification still goes through `type_category_for_name` upstream.
/// Bumped 71 → 74 (2026-06-15): the self-host Mutex/Guard ABI port added THREE
/// `recv_name.starts_with("Guard__"/"ReadGuard__"/"WriteGuard__")` sites in
/// `lower_types.gg`'s `infer_method_return_type` `get`-arm — they strip the
/// guard family prefix to recover the guarded element type T so `g.get()` types
/// as T (e.g. `Guard[bool].get()` → `bool`, not the I64 default that printed
/// `1` instead of `true`). This is the explicitly-allowed "extract T from
/// Mangled__T" name-parsing case (the per-element value type IS the runtime
/// contract for `gorget_guard_get`'s deref, mirroring Rust's inlined
/// FieldPtr+Load yielding the concrete inner type, src/lir/lower/insts.rs:3301),
/// NOT a classification-routing dodge — the Guard FAMILY classification still
/// goes through `resource_meta_for`/`type_runtime_map` upstream. (The sibling
/// `concurrency_elem_size_in_mod` size-extractor uses a VARIABLE prefix loop,
/// not a literal `starts_with`, so it is correctly not counted here.)
/// Retired the 2026-06-16 temporary 74 → 76 bump back to 74: the Dict `get_or`
/// inline-wrapper generator's TWO `inst_name.starts_with("Dict__"/"HashMap__")`
/// read-sites are GONE. The "this call is a Dict/HashMap get_or" fact is now
/// recorded UPSTREAM as a TYPED monomorphization-request registry
/// (`LirModule.dict_get_or_requests`, populated at the classified
/// `coll_cat == "GorgetMap"` + `method == "get_or"` arm in
/// `lir_lower.gg`'s `map_runtime_name`, with K/V read from the typed
/// `coll_key/val_type_map`) and DRAINED by `emit_dict_get_or_wrappers` at
/// codegen — no instruction scan, no name routing (Layering rule 4,
/// "resolve once, write through").
#[test]
fn no_growth_in_self_host_name_prefix_routing() {
    const BUDGET: usize = 74;

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
    // Floor as of 2026-06-14: 9 (counted per-OCCURRENCE — a single line can
    // hold two, e.g. `Option__ or Result__`). Phase 1 (37 -> 17) retired the
    // 20 output-neutral classification sites. Phase 2a (-2: 17 -> 15)
    // migrated the two DROP-PATH `Option[Ref[T]] -> Option[T]` lift
    // dst-classification gates to the typed channel: `try_lift_option_ref`'s
    // internal dst gate (lower_match.gg) and the return-site lift gate
    // (lower_stmt.gg) now read `enum_category_of(...).category ==
    // ENUM_CAT_OPTION and option_ref_payload_of(...) < 0` (probe-verified
    // identical to the former name-prefix test over the full driver
    // self-emit + Option/Result corpus, 4623 agreements / 0 disagreements;
    // ASan-clean on the drop-path fixtures; fixed_point re-converged).
    // Phase 2b (-6: 15 -> 9) retired the three further-migratable cohort
    // members the 2a comment flagged: (1) lir_lower.gg Pass-2 enum-kind
    // selector — PROBE-DEAD (prelude monos never enter `type_infos`, so the
    // arm fired 0 times over the full driver self-emit + Option/Result
    // corpus) -> deleted, leaving the plain `if tinfo.variants.len() > 0:
    // ek = 3`; (2) lir_lower.gg Pass-3 placeholder enum-kind branch SELECTOR
    // -> migrated to `pec.category == ENUM_CAT_OPTION` / `ENUM_CAT_RESULT`
    // (probe: channel category and the name prefix AGREE on all 3150
    // placeholder-struct fires, 0 disagreements); (3) lower_types.gg
    // `infer_method_return_type` unwrap name-prefix fallback -> deleted
    // (PROBE-DEAD: 0 fires; the typed channel + the typechecker side-table
    // cover every `.unwrap()` payload-type query that reaches the arm).
    // Probe-verified output-neutral (the P2a method: instrument old name-test
    // vs new typed-channel test, run over the full driver self-emit + corpus,
    // assert 0 disagreements / 0 dead-arm fires); fixed_point re-converged,
    // driver-emitted C byte-identical.
    //
    // The 9 remaining (per-occurrence) are the IRREDUCIBLE cohort:
    // lir_lower.gg 476 x2 (`is_generic_placeholder_name` name-shape
    // predicate, no tid/gmod in hand); lower_match.gg 737 x2
    // (`lookup_ctor_field_type` Class-D diag-gate, fires the loud miss
    // diagnostic), 820 (`result_payload_types` Class-D diag-gate);
    // lower_types.gg 894/897 (`record_field_enum_category`, THE blessed
    // name->category registrar — the channel SOURCE, not a reader),
    // 1737/1740 (`collection_element_type` — genuine name-PARSING, same
    // shape as the blessed Vector__/Set__ extractors). Migration to 0 from
    // here requires upstream typed-field registration (the registrar) /
    // structured-payload reads (the name-parser), not inline migration.
    const BUDGET: usize = 9;

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

// ─────────────────────────────────────────────────────────────────────────────
// #37 lazy-CoW view-producer enumeration guard
// ─────────────────────────────────────────────────────────────────────────────

/// Which route manufactures the view, deciding which detection arm must see it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ViewRoute {
    /// A runtime `.c` function whose body calls `gorget_str_view_region`.
    RuntimeC,
    /// A runtime `.c` function that manufactures the cap=0 header DIRECTLY
    /// (a blessed constructor; policed by the direct-manufacture ratchet).
    RuntimeCDirect,
    /// A SYNTHETIC callee: no `.c` body — backend `.rs` emitters write the
    /// `gorget_str_view_region` call into generated C.
    BackendSynthetic,
}

/// THE ENUMERATION. Every "view producer" — anything that manufactures a
/// cap=0 `Str` view aliasing another buffer — with its manufacture route and
/// the GIR-level mechanism that keeps the lazy-CoW default sound for it
/// (the four materialize hooks W3a/W3b/W3c/W3d, per
/// `docs/devbook/11-copy-on-write.md` §"View-producer enumeration rule" and
/// `docs/plans/brief_37_phase1_lazy_default.md` Appendix A).
///
/// **Adding a new view producer?** It is UNSOUND under the lazy-CoW default
/// unless a GIR materialize hook dominates every capture of its result while
/// the source is a lazy view. Cover it with one of the four hooks (or a new
/// sibling call site of `materialize_lazy_source_if_needed`), add the row
/// here AND to devbook/11's enumeration, and cite both in the PR.
const STR_VIEW_PRODUCERS: &[(&str, ViewRoute, &str)] = &[
    ("gorget_str_index",          ViewRoute::RuntimeC, "W3c index-base hook (lower_index_access)"),
    ("gorget_str_slice",          ViewRoute::RuntimeC, "W3c index-base hook + W3b receiver hook"),
    ("gorget_str_byte_slice",     ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_char_at",        ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_trim",           ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_lstrip_ws",      ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_rstrip_ws",      ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_strip",          ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_lstrip",         ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_rstrip",         ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_removeprefix",   ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_removesuffix",   ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_codepoint_at",   ViewRoute::BackendSynthetic, "W3d for-string hook (lower_for_string)"),
    ("gorget_string_borrow_view", ViewRoute::RuntimeCDirect, "W3a bind hook (the lazy bind producer itself)"),
];

/// `.rs` files allowed to spell `gorget_str_view_region` on a NON-comment
/// line, with the expected occurrence count. These are the backend emitters
/// of the synthetic `gorget_str_codepoint_at` shim (the C-emit boundary —
/// the name IS the contract with the runtime there). Comment-line mentions
/// (devbook citations in ir/lowering) are skipped, so docs stay free.
const VIEW_REGION_RS_EMITTERS: &[(&str, usize)] = &[
    ("src/backend/c_lir/emit_call_extern.rs", 1),
    ("src/backend/c_lir/emit_types.rs", 1),
];

/// Scan `src/backend/c/runtime/*.c` and return, for each non-comment call of
/// `gorget_str_view_region(`, the enclosing C function name (with file:line
/// for diagnostics). Function-definition and forward-declaration lines of
/// `gorget_str_view_region` itself are excluded — the definition is the
/// blessed constructor, not a producer.
fn runtime_c_view_region_callers() -> Vec<(String, String)> {
    let mut callers: Vec<(String, String)> = Vec::new();
    let fn_def = regex::Regex::new(
        // A C function definition at column 0: `static inline Str name(args) {`
        r"^[A-Za-z_][A-Za-z0-9_ \*]*?([A-Za-z_][A-Za-z0-9_]*)\s*\([^;{]*\)\s*\{\s*$"
    ).unwrap();
    // [W1 (vii)] The call detector tolerates whitespace between the callee
    // name and `(` — `return gorget_str_view_region ((const char*)..., 1);`
    // (GNU spacing) is RUN-PROVEN to slip past a glued-paren `contains`
    // check on all three lints silently.
    let call = regex::Regex::new(r"gorget_str_view_region\s*\(").unwrap();
    visit("src/backend/c/runtime", &mut |path| {
        if path.extension().map_or(true, |e| e != "c") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        let mut current_fn = String::new();
        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            // Skip comment lines: a commented-out call is documentation,
            // not a live producer (matches the comment-skip convention of
            // the sidecar / proxy-read lints above).
            if trimmed.starts_with("//") || trimmed.starts_with("*") || trimmed.starts_with("/*") {
                continue;
            }
            let is_def_line = if line.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
                if let Some(cap) = fn_def.captures(line) {
                    current_fn = cap[1].to_string();
                    true
                } else {
                    // [W1 (v)] A column-0 signature START the def regex
                    // cannot parse — brace-on-next-line / multi-line C
                    // signatures exist in this corpus
                    // (`tls_server_runtime.c:13-17`). Without a reset, a
                    // call inside such a function would silently
                    // MIS-ATTRIBUTE to the previous parsed function; if
                    // that one is already in the table, a NEW producer
                    // would pass unseen. Reset so its calls surface as
                    // `<unattributed>` → loud. (`;`-terminated lines are
                    // declarations, not signature starts.)
                    if line.contains('(') && !line.trim_end().ends_with(';') {
                        current_fn = String::new();
                    }
                    false
                }
            } else {
                false
            };
            if is_def_line {
                continue; // the def line of view_region itself is not a call
            }
            // Forward declarations (`static inline Str f(...);`) are not calls.
            if trimmed.starts_with("static") && trimmed.trim_end().ends_with(';') {
                continue;
            }
            if call.is_match(line) {
                if current_fn == "gorget_str_view_region" {
                    continue; // inside the blessed constructor's own body
                }
                let loc = format!("{}:{}", path.display(), idx + 1);
                if current_fn.is_empty() {
                    callers.push((format!("<unattributed at {loc}>"), loc));
                } else {
                    callers.push((current_fn.clone(), loc));
                }
            }
        }
    });
    callers
}

/// Per-file count of NON-comment `gorget_str_view_region` mentions in
/// `src/**/*.rs` — the backend-synthetic emitter arm.
fn rs_view_region_mentions() -> Vec<(String, usize)> {
    let mut counts: Vec<(String, usize)> = Vec::new();
    visit("src", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        let mut n = 0;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            n += line.matches("gorget_str_view_region").count();
        }
        if n > 0 {
            counts.push((path.to_string_lossy().replace('\\', "/"), n));
        }
    });
    counts.sort();
    counts
}

/// The enumeration guard: every view producer is detected on its manufacture
/// route AND maps to a covering mechanism in `STR_VIEW_PRODUCERS`.
///
/// **Arm a (runtime C):** the set of runtime `.c` functions whose bodies call
/// `gorget_str_view_region` must EXACTLY equal the `RuntimeC` rows.
/// **Arm b (backend synthetic):** the `.rs` files spelling
/// `gorget_str_view_region` on non-comment lines must EXACTLY match
/// `VIEW_REGION_RS_EMITTERS` (file + count).
/// **Arm c (typed-registry reconciliation):** every producer must be present
/// in `src/lir/runtime.rs` and declared via `sig(` — NEVER `sig_fresh(`. A
/// view tagged `returns_fresh: true` would let the CoW machinery elide the
/// clone guards that keep views from dangling (see `is_fresh_string`).
/// **Arm d (GIR axis reconciliation):** every `returns_view: true` method in
/// `src/ir/lowering/builtins.rs` must route to a producer in the table (or
/// `None` for the identity header copy `str`/`as_str`).
///
/// **If this fails**: you added (or re-routed) a view producer. Read the
/// table doc above — cover the producer with a GIR materialize hook
/// (W3a/W3b/W3c/W3d sibling), then add its row here and to
/// `docs/devbook/11-copy-on-write.md`'s enumeration. Do NOT just extend the
/// allowlist: an uncovered producer is a use-after-free generator under the
/// lazy-CoW default (and the class is proven ASan-blind — stdout fixtures
/// are the only net).
#[test]
fn str_view_producer_enumeration_is_closed() {
    // Arm a — runtime C producers.
    let callers = runtime_c_view_region_callers();
    let mut found: Vec<&str> = callers.iter().map(|(f, _)| f.as_str()).collect();
    found.sort();
    found.dedup();
    let mut expected: Vec<&str> = STR_VIEW_PRODUCERS
        .iter()
        .filter(|(_, route, _)| *route == ViewRoute::RuntimeC)
        .map(|(name, _, _)| *name)
        .collect();
    expected.sort();
    let unattributed: Vec<&(String, String)> =
        callers.iter().filter(|(f, _)| f.starts_with('<')).collect();
    assert!(
        unattributed.is_empty(),
        "could not attribute these gorget_str_view_region calls to an enclosing \
         C function (the def-line scanner in runtime_c_view_region_callers needs \
         updating for a new code shape): {unattributed:?}"
    );
    let missing: Vec<&&str> = expected.iter().filter(|e| !found.contains(*e)).collect();
    let new_producers: Vec<(&str, &str)> = callers
        .iter()
        .filter(|(f, _)| !expected.contains(&f.as_str()))
        .map(|(f, loc)| (f.as_str(), loc.as_str()))
        .collect();
    assert!(
        new_producers.is_empty() && missing.is_empty(),
        "View-producer enumeration drifted (runtime-C arm).\n\
         NEW producers (functions calling gorget_str_view_region, not in the table): {new_producers:?}\n\
         VANISHED producers (in the table, no longer calling it): {missing:?}\n\n\
         A function returning a cap=0 view aliasing another buffer is UNSOUND under \
         the lazy-CoW default unless a GIR materialize hook dominates every capture \
         of its result (docs/devbook/11-copy-on-write.md §\"View-producer enumeration \
         rule\"; docs/plans/brief_37_phase1_lazy_default.md Appendix A).\n\
         For a NEW producer: wire a hook (sibling call site of \
         materialize_lazy_source_if_needed — W3a bind / W3b receiver / W3c index \
         base / W3d for-string source), add a row to STR_VIEW_PRODUCERS in this \
         file naming the hook, and extend devbook/11's enumeration.\n\
         For a VANISHED producer: remove its row here and in devbook/11.",
    );

    // Arm b — backend-synthetic emitters.
    let rs_mentions = rs_view_region_mentions();
    let mut expected_rs: Vec<(String, usize)> = VIEW_REGION_RS_EMITTERS
        .iter()
        .map(|(f, n)| (f.to_string(), *n))
        .collect();
    expected_rs.sort();
    assert_eq!(
        rs_mentions, expected_rs,
        "View-producer enumeration drifted (backend-synthetic arm): the set of .rs \
         files spelling `gorget_str_view_region` on non-comment lines changed.\n\
         found:    {rs_mentions:?}\n\
         expected: {expected_rs:?}\n\n\
         A new emitter writes a view-manufacturing call into generated C — that is a \
         new view producer (the W3d `gorget_str_codepoint_at` class: synthetic callees \
         never appear in the runtime .c, which is exactly how the route was missed \
         pre-#37 — the enumeration rule needed two corrections in total). Cover \
         its GIR producer with a materialize hook, add the \
         producer row to STR_VIEW_PRODUCERS, and update VIEW_REGION_RS_EMITTERS + \
         devbook/11. Comment-line citations don't count — only live emit lines.",
    );

    // Arm c — LIR registry reconciliation.
    let registry = fs::read_to_string("src/lir/runtime.rs")
        .expect("src/lir/runtime.rs must exist (typed runtime registry)");
    for (name, _, mechanism) in STR_VIEW_PRODUCERS {
        let decl = registry
            .lines()
            .find(|l| l.contains(&format!("=> \"{name}\",")));
        let decl = decl.unwrap_or_else(|| {
            panic!(
                "view producer `{name}` ({mechanism}) has no entry in \
                 src/lir/runtime.rs — every producer must be a typed registry \
                 entry (devbook/24 rule 2: typed metadata, not name-matching)"
            )
        });
        assert!(
            decl.contains("sig(") && !decl.contains("sig_fresh("),
            "view producer `{name}` is declared `sig_fresh` in src/lir/runtime.rs:\n  {decl}\n\
             A cap=0 view MUST carry `returns_fresh: false` — `returns_fresh: true` \
             lets CoW elide the self-referential-reassignment clone guard and the \
             return-clone-elision check (`is_fresh_string`), turning the view into a \
             dangling alias. Change the declaration back to `sig(`.",
        );
    }

    // Arm d — GIR `returns_view` axis reconciliation.
    let builtins = fs::read_to_string("src/ir/lowering/builtins.rs")
        .expect("src/ir/lowering/builtins.rs must exist");
    let view_decl = regex::Regex::new(
        r#"name: "([A-Za-z_]+)", runtime_callee: (?:Some\("([a-z_]+)"\)|None)"#
    ).unwrap();
    let producer_names: Vec<&str> = STR_VIEW_PRODUCERS.iter().map(|(n, _, _)| *n).collect();
    for line in builtins.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") || !line.contains("returns_view: true") {
            continue;
        }
        let cap = view_decl.captures(line).unwrap_or_else(|| {
            panic!("unparseable returns_view decl line in builtins.rs: {line}")
        });
        if let Some(callee) = cap.get(2) {
            assert!(
                producer_names.contains(&callee.as_str()),
                "builtins.rs declares method `{}` with `returns_view: true` routing \
                 to runtime callee `{}`, which is NOT in STR_VIEW_PRODUCERS. Either \
                 the callee is a new view producer (cover it with a hook + add the \
                 row) or the `returns_view` tag is wrong.",
                &cap[1],
                callee.as_str(),
            );
        }
        // `None` callees (str/as_str identity header copy) are W3b-covered
        // through the same returns_view dispatch — nothing to reconcile.
    }
}

/// Ratchet (the LIR-rewrite fence — the partially-fenceable blind spot): the
/// count of view-producer callee MENTIONS in `src/lir/**/*.rs` must not grow.
///
/// The four GIR materialize hooks are keyed UPSTREAM (bind / receiver / index
/// base / for-string source). An LIR-level rewrite that changes a callee to a
/// view-returning one (the `IndexLoad → gorget_str_slice/str_index` precedent,
/// `src/lir/lower/insts.rs`) bypasses them unless the GIR shape it rewrites
/// was already hooked. This ratchet can't prove dominance, but it CAN make a
/// new mention of a view callee in the LIR layer fail loudly so the author
/// reconciles it against the enumeration before shipping.
///
/// Counted (non-comment lines): exact-quoted producer names
/// (`"gorget_str_slice"`, ...) and `RuntimeFn::` variant references
/// (`RuntimeFn::StrSlice`, ...) in src/lir/. Baseline 2026-06-10: 41 —
/// 14 registry decl lines (runtime.rs) + 6 variant refs in the arity-overload
/// rewrite (runtime.rs, strip→trim_ws family, view→view so W3b-covered) +
/// 14 return-type-table mentions (types.rs) + 3 GIR-name fixups (lower/calls.rs,
/// W3b-covered upstream) + 4 IndexLoad-rewrite mentions (lower/insts.rs,
/// W3c-covered upstream).
///
/// **If this fails (count went UP):** you added an LIR site naming a
/// view-returning callee. If it REWRITES some inst into a call of that
/// callee, verify a GIR materialize hook dominates every such rewritten
/// shape (or add the missing hook), reconcile against devbook/11's
/// enumeration, THEN bump with a justification comment. If it's registry /
/// type-table plumbing, bump with a one-liner.
#[test]
fn no_growth_in_lir_view_callee_rewrites() {
    const BUDGET: usize = 41;

    let names: Vec<&str> = STR_VIEW_PRODUCERS.iter().map(|(n, _, _)| *n).collect();
    let quoted = names
        .iter()
        .map(|n| format!(r#""{n}""#))
        .collect::<Vec<_>>()
        .join("|");
    // RuntimeFn variant spellings of the same producers (CamelCase of the
    // gorget_* names as declared in runtime.rs).
    let variants = [
        "StrIndex", "StrSlice", "StrByteSlice", "StrCharAt", "StrTrim",
        "StrLstripWs", "StrRstripWs", "StrStrip", "StrLstrip", "StrRstrip",
        "StrRemoveprefix", "StrRemovesuffix", "StrCodepointAt", "StringBorrowView",
    ];
    let variant_alt = variants
        .iter()
        .map(|v| format!(r"RuntimeFn::{v}\b"))
        .collect::<Vec<_>>()
        .join("|");
    let pattern = regex::Regex::new(&format!("{quoted}|{variant_alt}")).unwrap();

    let mut count = 0;
    visit("src/lir", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        for line in content.lines() {
            if line.trim_start().starts_with("//") {
                continue;
            }
            count += pattern.find_iter(line).count();
        }
    });
    assert!(
        count <= BUDGET,
        "View-producer callee mentions in src/lir/ grew beyond budget: {count} > {BUDGET}.\n\n\
         A new src/lir site names a view-returning callee (string literal or \
         RuntimeFn variant). The GIR materialize hooks (W3a-W3d) are keyed \
         upstream of LIR — a NEW rewrite targeting a view callee can bypass them \
         (the IndexLoad→str_slice/str_index precedent was exactly this shape and \
         needed its own hook, W3c).\n\n\
         Verify a GIR materialize hook dominates the rewritten shape (or add one — \
         sibling call site of materialize_lazy_source_if_needed), reconcile against \
         docs/devbook/11-copy-on-write.md's enumeration, then bump BUDGET with a \
         justification comment. If the count went DOWN, lower BUDGET.",
    );
}

/// Ratchet (the bypass fence): direct cap=0 `Str` header manufacture in the
/// runtime `.c` files must not grow. A view built with a raw struct literal
/// (`{ .data = ..., .cap = 0, ... }`) instead of `gorget_str_view_region`
/// is INVISIBLE to the enumeration guard's runtime-C arm — this ratchet is
/// what stops that bypass.
///
/// Matching is field-ORDER-INDEPENDENT [W1 (ii)]: a single-line brace group
/// counts when it contains BOTH `.data =` and `.cap = 0` (the `\b` keeps
/// `.cap = 01` from matching), in either order, so a reordered literal
/// cannot slip past the fence.
///
/// Baseline 2026-06-10: 7 —
///   runtime_string.c:56  GORGET_EMPTY_STR (static, .rodata, never freed)
///   runtime_string.c:61  GORGET_SLIT macro body (static literal views)
///   runtime_string.c:238 gorget_string_borrow_view (blessed producer, W3a)
///   runtime_string.c:744 gorget_str_view_region itself (THE blessed constructor)
///   runtime_string_extended.c:556/:564 replacen locals (ephemeral, bytes
///     copied into a fresh result before return)
///   runtime_string_extended.c:665 find_from local (ephemeral, search only)
///
/// **If this fails (count went UP):** a new direct cap=0 view literal was
/// added. If the view is RETURNED (or stored), route it through
/// `gorget_str_view_region` so the enumeration guard sees the producer, and
/// cover it per `str_view_producer_enumeration_is_closed`'s table. If it is
/// genuinely ephemeral (consumed before any caller-visible mutation), bump
/// with a justification comment naming the function.
/// NOTE: the pattern is single-line; a multi-line struct literal would evade
/// it. Keep view literals on one line (current style throughout).
#[test]
fn no_growth_in_runtime_c_direct_view_manufacture() {
    const BUDGET: usize = 7;

    // [W1 (ii)] Field-order-independent: find each single-line brace group,
    // then require BOTH fields inside it (in any order).
    let brace_group = regex::Regex::new(r"\{[^{}]*\}").unwrap();
    let data_field = regex::Regex::new(r"\.data\s*=").unwrap();
    let cap_zero = regex::Regex::new(r"\.cap\s*=\s*0\b").unwrap();
    let mut count = 0;
    let mut sites: Vec<String> = Vec::new();
    visit("src/backend/c/runtime", &mut |path| {
        if path.extension().map_or(true, |e| e != "c") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("*") || trimmed.starts_with("/*") {
                continue;
            }
            let n = brace_group
                .find_iter(line)
                .filter(|g| data_field.is_match(g.as_str()) && cap_zero.is_match(g.as_str()))
                .count();
            if n > 0 {
                count += n;
                sites.push(format!("{}:{}", path.display(), idx + 1));
            }
        }
    });
    assert!(
        count <= BUDGET,
        "Direct cap=0 Str view manufacture in runtime .c grew beyond budget: \
         {count} > {BUDGET}.\nSites: {sites:?}\n\n\
         A raw `{{ .data = ..., .cap = 0 }}` struct literal manufactures a view \
         the enumeration guard cannot see (it only attributes \
         `gorget_str_view_region` callers). If the new view is returned or \
         stored, build it with `gorget_str_view_region` instead and cover the \
         producer per STR_VIEW_PRODUCERS. If it is ephemeral (consumed before \
         any caller-visible mutation, like the replacen/find_from locals), bump \
         BUDGET with a justification naming the function. If the count went \
         DOWN, lower BUDGET.",
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

// ── Slot-coalescing operand-enumerator coverage (the frame fix) ──────────────
//
// The liveness pass that drives stack-slot coalescing reads operand value-ids
// from the self-host `inst_uses` (over `LirInst`) and `term_uses` (over
// `LirTerm`). A SINGLE missing operand arm → an under-live range → a slot-
// aliasing CLOBBER that `c_emit_comparison` AND the emit byte-diff are BLIND to
// (only the RUN gate would catch it). Per CLAUDE.md "Sibling-site drift — fix
// the class, not the instance" (rule 4 + the arm-count lint), these ratchets
// force EVERY `LirInst`/`LirTerm` variant through the operand enumerators so a
// future variant can't silently fall through (Gorget enforces match
// exhaustiveness, so a missing `case` already fails `gg check`; these lints ALSO
// trip if someone adds an `else: pass` catch-all that dodges the enumeration, by
// pinning the explicit per-variant arm count to the enum's variant count).

/// Count `enum <Name>:` body variants (lines indented under the enum whose
/// first non-space token is a CamelCase identifier — the variant name, with or
/// without a `(...)` payload).
fn count_self_host_enum_variants(path: &str, enum_name: &str) -> usize {
    let content = fs::read_to_string(path).unwrap_or_default();
    let header = format!("enum {enum_name}:");
    let mut in_enum = false;
    let mut count = 0;
    for line in content.lines() {
        if line.starts_with(&header) {
            in_enum = true;
            continue;
        }
        if !in_enum {
            continue;
        }
        // The enum body is the indented block; a non-indented non-empty line
        // ends it. Blank lines and comments inside the body are skipped.
        if !line.starts_with(' ') {
            if line.trim().is_empty() {
                continue;
            }
            break;
        }
        let t = line.trim_start();
        if t.is_empty() || t.starts_with('#') {
            continue;
        }
        // A variant line starts with an uppercase ASCII letter (the variant
        // name); field/comment continuation lines do not appear in these enums
        // (variants are one-per-line with inline `(...)` payloads).
        if t.as_bytes().first().is_some_and(|b| b.is_ascii_uppercase()) {
            count += 1;
        }
    }
    count
}

/// Count `case <Prefix>...` arms inside a named self-host function body. The
/// function body runs from its `<ret> <name>(` signature line to the next
/// top-level (column-0) `<ret> <name>(` definition.
fn count_case_arms_in_fn(path: &str, fn_sig_prefix: &str, case_prefix: &str) -> usize {
    let content = fs::read_to_string(path).unwrap_or_default();
    let mut in_fn = false;
    let mut count = 0;
    for line in content.lines() {
        if line.starts_with(fn_sig_prefix) {
            in_fn = true;
            continue;
        }
        if !in_fn {
            continue;
        }
        // A new column-0 definition (non-space, non-comment line that isn't a
        // continuation) ends the function. Comments and blanks don't.
        if !line.starts_with(' ') && !line.trim().is_empty() && !line.starts_with('#') {
            break;
        }
        let t = line.trim_start();
        if t.starts_with(&format!("case {case_prefix}")) {
            count += 1;
        }
    }
    count
}

/// Ratchet: the self-host `inst_uses` operand enumerator must cover EVERY
/// `LirInst` variant 1:1 — the same closed arm set as `inst_dst` and as the
/// `LirInst` enum itself. A new variant added without an `inst_uses` arm (or a
/// dodge via `else: pass`) drops its operands from the liveness pass → an
/// uncatchable slot-aliasing clobber.
#[test]
fn inst_uses_arms_count() {
    let lir = "tests/fixtures/self_host_lowerer/lir.gg";
    let codegen = "tests/fixtures/self_host_lowerer/lir_codegen.gg";
    let n_variants = count_self_host_enum_variants(lir, "LirInst");
    let n_dst = count_case_arms_in_fn(codegen, "int inst_dst(", "I");
    let n_uses = count_case_arms_in_fn(codegen, "Vector[int] inst_uses(", "I");
    assert!(
        n_variants > 0 && n_dst > 0 && n_uses > 0,
        "inst_uses_arms_count: failed to locate one of LirInst / inst_dst / \
         inst_uses (variants={n_variants}, dst={n_dst}, uses={n_uses}). Did a file \
         move or a signature change?",
    );
    assert_eq!(
        n_uses, n_variants,
        "self-host `inst_uses` arm count ({n_uses}) != `LirInst` variant count \
         ({n_variants}).\n\n\
         Slot-coalescing liveness reads operand value-ids from `inst_uses`. EVERY \
         `LirInst` variant must have an explicit `case` arm enumerating its operand \
         value-ids (port arm-for-arm vs the Rust gold `Inst::uses()` in \
         src/lir/mod.rs, reading operand POSITIONS from the lir.gg decl). A missing \
         operand = an under-live range = a slot-aliasing CLOBBER the emit-diff and \
         c_emit_comparison CANNOT catch (only the RUN gate would). Add the arm; do \
         NOT use `else: pass`.",
    );
    assert_eq!(
        n_uses, n_dst,
        "self-host `inst_uses` arm count ({n_uses}) != `inst_dst` arm count \
         ({n_dst}). The two enumerators must cover the identical `LirInst` arm set.",
    );
}

/// Ratchet: the self-host `term_uses` operand enumerator must cover EVERY
/// `LirTerm` variant 1:1 — the same closed arm set as `term_successors` and the
/// `LirTerm` enum. The terminator is where block-arg/phi liveness lives
/// (`TJump`/`TBranch`/`TSwitch` ARGS); a missing one is the SAME uncatchable
/// clobber class.
#[test]
fn term_uses_arms_count() {
    let lir = "tests/fixtures/self_host_lowerer/lir.gg";
    let ssa = "tests/fixtures/self_host_lowerer/lir_ssa.gg";
    let n_variants = count_self_host_enum_variants(lir, "LirTerm");
    let n_succ = count_case_arms_in_fn(ssa, "Vector[int] term_successors(", "T");
    let n_uses = count_case_arms_in_fn(ssa, "Vector[int] term_uses(", "T");
    assert!(
        n_variants > 0 && n_succ > 0 && n_uses > 0,
        "term_uses_arms_count: failed to locate one of LirTerm / term_successors / \
         term_uses (variants={n_variants}, succ={n_succ}, uses={n_uses}).",
    );
    assert_eq!(
        n_uses, n_variants,
        "self-host `term_uses` arm count ({n_uses}) != `LirTerm` variant count \
         ({n_variants}).\n\n\
         Slot-coalescing liveness reads terminator operand value-ids (incl. the \
         block-arg/phi values in TJump/TBranch/TSwitch ARGS) from `term_uses`. EVERY \
         `LirTerm` variant must have an explicit `case` arm (port vs the Rust gold \
         `Term::uses()` in src/lir/mod.rs). A missing terminator arg = a slot-aliasing \
         clobber the emit-diff CANNOT catch.",
    );
    assert_eq!(
        n_uses, n_succ,
        "self-host `term_uses` arm count ({n_uses}) != `term_successors` arm count \
         ({n_succ}). The two enumerators must cover the identical `LirTerm` arm set.",
    );

    // `coal_term_arg_lists` (the per-successor block-arg vectors used by the
    // coalescing address-escape soundness analysis, in lir_codegen.gg) must ALSO
    // cover every LirTerm variant — a missing arm would silently drop a
    // successor's block args from the escape scan → an address-escaped value
    // could be coalesced anyway → a use-after-coalesce clobber.
    let codegen = "tests/fixtures/self_host_lowerer/lir_codegen.gg";
    let n_arglists =
        count_case_arms_in_fn(codegen, "Vector[Vector[int]] coal_term_arg_lists(", "T");
    assert_eq!(
        n_arglists, n_variants,
        "self-host `coal_term_arg_lists` arm count ({n_arglists}) != `LirTerm` variant \
         count ({n_variants}). It must enumerate every terminator's block-arg lists in \
         successor order (matching term_successors) so the coalescing address-escape \
         scan sees every by-pointer block arg.",
    );
}

/// Sibling-site ratchet (CLAUDE.md rule 4 / "Sibling-site drift") over the
/// self-host `Param(` AST-constructor call sites. P0 (default-arg support) added
/// a 4th field `Option[SpannedExpr] default_value` to `struct Param` in all three
/// distinct `ast.gg` copies — so EVERY `Param(...)` constructor must now supply
/// the default (a captured `dflt` at the parse site, `None()` everywhere else).
/// A NEW `Param(` site that forgets the field would either fail to compile
/// (arity error) OR — worse, if someone "fixes" it by reordering — silently drop
/// a parsed default. Pin the count so a new construction site is forced through
/// the 4-field shape.
///
/// `Param(` is matched case-sensitively, so the lowercase `parse_param(` /
/// `parse_closure_param(` method calls do NOT collide. The `Param parse_param(`
/// method-definition lines use `Param ` (with a space) and are also excluded.
///
/// Baseline 2026-06-12: 22 (parser 7 + resolver 7 + typechecker 8). Each capture
/// site (one per copy: parser/resolver/typechecker `parse_param`) passes the
/// captured `dflt`; the other 19 pass `None()`.
#[test]
fn self_host_param_ctor_site_count() {
    const EXPECTED: usize = 22;

    // The three DISTINCT parser.gg copies (check + lowerer SYMLINK typechecker,
    // so they are not listed — counting them would double-count).
    let files = [
        "tests/fixtures/self_host_parser/parser.gg",
        "tests/fixtures/self_host_resolver/parser.gg",
        "tests/fixtures/self_host_typechecker/parser.gg",
    ];

    let mut count = 0usize;
    for f in &files {
        let content = fs::read_to_string(f).unwrap_or_default();
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with('#') {
                continue; // .gg comments
            }
            count += line.matches("Param(").count();
        }
    }

    assert_eq!(
        count, EXPECTED,
        "Self-host `Param(` constructor-call count changed: {count} vs {EXPECTED}.\n\n\
         `struct Param` carries a 4th field `Option[SpannedExpr] default_value` \
         (P0 default-arg support). EVERY `Param(...)` site must supply it — the \
         per-copy `parse_param` capture site passes the parsed `dflt`, all others \
         pass `None()`. If you added a construction site, give it the default \
         (capture the `= expr` if it's the param-parse path, else `None()`) and \
         bump EXPECTED. If you removed one, lower EXPECTED. Never reorder the \
         fields to dodge the arity — that silently drops parsed defaults.",
    );
}

/// Sibling-site ratchet (CLAUDE.md rule 4 / "Sibling-site drift") over the
/// self-host param-ownership registration sites in `lower.gg`. The pre-scan
/// builds three PARALLEL per-fn Dicts — `fn_borrow_params`, `fn_move_params`,
/// `fn_defaults` — keyed identically. P0 (default-arg fill) added `fn_defaults`
/// and registers it at EVERY `fn_move_params.put` sibling (function, equip,
/// struct/enum/prelude ctors, mono'd ctor, equip-short-key) so the call-site
/// fill (`lower_expr.gg lower_call`) can read `fn_defaults[call_name][idx]` for
/// any callable whose move-flags it already trusts. A new `.put` move site that
/// FORGETS to register defaults would leave `fn_defaults` short an entry → a
/// default-arg call to that callable would silently drop the default.
///
/// Pin `fn_defaults.put` == `fn_move_params.put`. (`fn_borrow_params.put` is
/// intentionally a SUBSET — only param-bearing fns/equips register borrow flags;
/// synthetic ctors are move-only — so it is NOT pinned equal here.)
///
/// Baseline 2026-06-12: 11 each.
#[test]
fn self_host_fn_defaults_registration_parity() {
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();

    let mut move_puts = 0usize;
    let mut default_puts = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with('#') {
            continue; // .gg comments
        }
        move_puts += line.matches("fn_move_params.put(").count();
        default_puts += line.matches("fn_defaults.put(").count();
    }

    assert!(
        move_puts > 0 && default_puts > 0,
        "self_host_fn_defaults_registration_parity: failed to locate the \
         fn_move_params / fn_defaults `.put` sites (move={move_puts}, \
         default={default_puts}).",
    );
    assert_eq!(
        default_puts, move_puts,
        "Self-host `fn_defaults.put` count ({default_puts}) != `fn_move_params.put` \
         count ({move_puts}).\n\n\
         The three per-fn param Dicts (fn_borrow_params / fn_move_params / \
         fn_defaults) must stay in lockstep: every `.put` that registers a \
         callable's move-flags MUST also register its default-arg vector (the \
         parsed defaults for a real param-bearing fn/equip, or an all-`None()` \
         vector for a synthetic ctor). A new move-registration `.put` without a \
         `fn_defaults.put` sibling would make a default-arg call to that callable \
         silently drop the default. Add the `fn_defaults.put` and the counts \
         re-balance.",
    );
}

/// Collect the NON-comment body lines of a named self-host function. The body
/// runs from its `<sig_prefix>` signature line to the next top-level (column-0)
/// definition. Comment-only lines (after trim, starting with `#`) and blanks do
/// NOT end the function and are STRIPPED from the returned body — mirrors
/// `count_case_arms_in_fn`'s comment-skipping (tests/lints.rs:1553) so a future
/// explanatory comment mentioning a `gorget_…` name can't false-trip a
/// divergence check. Returns the matched body lines (trimmed of leading ws).
fn self_host_fn_body_noncomment(content: &str, sig_prefix: &str) -> Vec<String> {
    let mut in_fn = false;
    let mut body = Vec::new();
    for line in content.lines() {
        if line.starts_with(sig_prefix) {
            in_fn = true;
            continue;
        }
        if !in_fn {
            continue;
        }
        // A new column-0 definition (non-space, non-comment, non-blank line)
        // ends the function. Comments and blanks don't.
        if !line.starts_with(' ') && !line.trim().is_empty() && !line.starts_with('#') {
            break;
        }
        let t = line.trim_start();
        if t.is_empty() || t.starts_with('#') {
            continue; // strip comments and blanks
        }
        body.push(t.to_string());
    }
    body
}

/// Single-source-of-truth ratchet for the "this runtime fn returns a raw C
/// string (`const char*`) and a GorgetString-typed slot needs the
/// `gorget_str_from_cstr` wrap" axis. The set lives in ONE function,
/// `runtime_fn_returns_cstr` (lir.gg); the two complementary wrap mechanisms —
/// EMIT `is_cstr_returning_fn` (lir_codegen.gg) and LOWERING
/// `is_cstr_returning_call` (lower_types.gg) — must be PURE delegations to it,
/// never re-inlined name lists.
///
/// History: the two were hand-synced inline lists that nearly diverged during
/// the loader work (a SVarDecl `String p = path_absolute(...)` skipped the
/// cstr->Str coercion and mistyped as I64). Feeding the full set to both sites
/// naively double-wraps getenv (`expected 'const char *' but argument is of
/// type 'Str'`); the companion guard (`var_type != gs_tid_decl` at
/// lower_stmt.gg) is what makes ONE registry safe for BOTH. STOPGAP: the
/// durable end-state is typed cstr provenance (Rust `ValueOrigin::CStr` /
/// `AbiKind::CStr`, src/lir/types.rs); until then this lint keeps the list
/// single-sourced.
#[test]
fn cstr_return_registry_single_source() {
    let lir = "tests/fixtures/self_host_lowerer/lir.gg";
    let codegen = "tests/fixtures/self_host_lowerer/lir_codegen.gg";
    let types = "tests/fixtures/self_host_lowerer/lower_types.gg";

    let lir_src = fs::read_to_string(lir).unwrap_or_default();
    let codegen_src = fs::read_to_string(codegen).unwrap_or_default();
    let types_src = fs::read_to_string(types).unwrap_or_default();

    // 1. EXACTLY one definition of the single source across self_host_lowerer.
    let defs = lir_src.matches("bool runtime_fn_returns_cstr(").count()
        + codegen_src.matches("bool runtime_fn_returns_cstr(").count()
        + types_src.matches("bool runtime_fn_returns_cstr(").count();
    assert_eq!(
        defs, 1,
        "Expected EXACTLY one definition of `runtime_fn_returns_cstr` across \
         self_host_lowerer (found {defs}). The cstr-return ABI is single-sourced \
         in lir.gg; do NOT define a second copy.",
    );

    // 2. Both siblings must be PURE delegations — zero `gorget_` literals in
    //    their (comment-stripped) bodies. A re-inlined name list trips this.
    let emit_body =
        self_host_fn_body_noncomment(&codegen_src, "bool is_cstr_returning_fn(");
    let lowering_body =
        self_host_fn_body_noncomment(&types_src, "bool is_cstr_returning_call(");
    assert!(
        !emit_body.is_empty() && !lowering_body.is_empty(),
        "cstr_return_registry_single_source: failed to locate one of \
         `is_cstr_returning_fn` (lir_codegen.gg) / `is_cstr_returning_call` \
         (lower_types.gg). Did a file move or a signature change?",
    );
    let emit_gorget = emit_body.iter().filter(|l| l.contains("gorget_")).count();
    let lowering_gorget = lowering_body.iter().filter(|l| l.contains("gorget_")).count();
    assert_eq!(
        emit_gorget, 0,
        "`is_cstr_returning_fn` (lir_codegen.gg) contains {emit_gorget} `gorget_` \
         literal(s) in its body — it must be a PURE delegation to \
         `runtime_fn_returns_cstr` (lir.gg).\n\n\
         The cstr-return ABI is single-sourced in lir.gg. Add new cstr-returning \
         runtime fns THERE, not by re-inlining a name list here — two hand-synced \
         lists nearly diverged once (double-wrap saga; see lir.gg comment).",
    );
    assert_eq!(
        lowering_gorget, 0,
        "`is_cstr_returning_call` (lower_types.gg) contains {lowering_gorget} \
         `gorget_` literal(s) in its body — it must delegate the terminal \
         name-set test to `runtime_fn_returns_cstr` (lir.gg).\n\n\
         The cstr-return ABI is single-sourced in lir.gg. Add new cstr-returning \
         runtime fns THERE, not by re-inlining a name list here — two hand-synced \
         lists nearly diverged once (double-wrap saga; see lir.gg comment).",
    );

    // 3. Each delegation body actually calls the single source.
    assert!(
        emit_body.iter().any(|l| l.contains("runtime_fn_returns_cstr(")),
        "`is_cstr_returning_fn` (lir_codegen.gg) must delegate to \
         `runtime_fn_returns_cstr(...)`.",
    );
    assert!(
        lowering_body.iter().any(|l| l.contains("runtime_fn_returns_cstr(")),
        "`is_cstr_returning_call` (lower_types.gg) must delegate to \
         `runtime_fn_returns_cstr(...)`.",
    );
}

/// Sibling-site ratchet (CLAUDE.md rule 4 / "Sibling-site drift") over the
/// await-dispatch value-route fallback. The `.await()` dispatcher exists in TWO
/// hand-synced copies — the postfix method-call form
/// (`src/ir/lowering/exprs/methods.rs`) and the prefix `Expr::Await` form
/// (`src/ir/lowering/exprs/mod.rs`). When the named `__gorget_await_<fn>` path
/// can't resolve a single producer fn (a collection-sourced Task[void] whose
/// TypeId maps to >1 distinct producer), BOTH must fall through to the
/// value-route helper `Task__void__await` — otherwise the await is silently
/// dropped and the task is only joined by its scope-end drop (nondeterministic
/// wrong output, the bug this fixed).
///
/// Pin the value-route call-site count at exactly 2 (one per await form). A new
/// third await form (or a refactor that drops one copy's fallback) trips this,
/// forcing it through the same `Task__void__await` value-route. If you instead
/// CENTRALIZE the resolve+fallback into one shared helper both forms call,
/// update this to assert the single call site.
#[test]
fn await_value_route_sibling_count() {
    const EXPECTED: usize = 2;

    let files = [
        "src/ir/lowering/exprs/methods.rs",
        "src/ir/lowering/exprs/mod.rs",
    ];

    let mut call_sites = 0usize;
    let mut zero_after = 0usize;
    for f in &files {
        let content = fs::read_to_string(f).unwrap_or_default();
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            // The GIR emit of the value-route helper call. The emit_types.rs C
            // definition uses a different spelling (quoted struct param), so
            // restricting to these two GIR-lowering files counts only the
            // dispatch call sites.
            call_sites += line.matches("call_void(\"Task__void__await\"").count();
        }
    }

    // Each value-route call MUST be paired with a move_zero_and_mark on the
    // receiver local (double-join guard); count those across the same files so a
    // future copy that forgets the zero also trips.
    for f in &files {
        let content = fs::read_to_string(f).unwrap_or_default();
        let call_idx: Vec<usize> = content
            .lines()
            .enumerate()
            .filter(|(_, l)| {
                let t = l.trim_start();
                !t.starts_with("//") && l.contains("call_void(\"Task__void__await\"")
            })
            .map(|(i, _)| i)
            .collect();
        let lines: Vec<&str> = content.lines().collect();
        for idx in call_idx {
            // The move_zero_and_mark guard is within a few lines after the call.
            let window_end = (idx + 4).min(lines.len());
            if lines[idx..window_end]
                .iter()
                .any(|l| l.contains("move_zero_and_mark"))
            {
                zero_after += 1;
            }
        }
    }

    assert_eq!(
        call_sites, EXPECTED,
        "Await value-route `Task__void__await` call-site count changed: \
         {call_sites} vs {EXPECTED}.\n\n\
         The `.await()` dispatcher has two hand-synced forms (postfix \
         methods.rs + prefix Expr::Await mod.rs). BOTH must fall through to the \
         `Task__void__await` value-route when the named `__gorget_await_<fn>` \
         path can't resolve a single producer fn — else a collection-sourced \
         Task[void] silently drops its await. If you added a third await form, \
         route it through the same value-route fallback and bump EXPECTED. If \
         you CENTRALIZED the two into one shared helper, set EXPECTED = 1.",
    );
    assert_eq!(
        zero_after, EXPECTED,
        "Await value-route `move_zero_and_mark` double-join guard count changed: \
         {zero_after} vs {EXPECTED} (call sites = {call_sites}).\n\n\
         Every `Task__void__await` value-route call MUST zero the receiver local \
         (`move_zero_and_mark`) right after, so scope-end `Task__void__drop` is a \
         no-op and the task isn't joined twice. A value-route site missing the \
         zero re-opens the double-free. Keep every fallback paired with its zero.",
    );
}
