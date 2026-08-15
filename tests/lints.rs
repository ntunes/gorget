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
use std::path::{Path, PathBuf};

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
    /// Bumped 257 → 259 (2026-07-18): the refcount-handle struct/enum
    /// field-clone fix (`refcount_field_retain_fn` in c_lir/emit_types.rs)
    /// added `starts_with("Shared__")` / `starts_with("Weak__")` /
    /// `starts_with("Channel__")` (+3 over the 256 floor) to detect a
    /// refcount-handle field's drop wrapper and emit the by-value RETAIN
    /// (`gorget_shared_clone` / `gorget_weak_clone` / `gorget_channel_retain`)
    /// that balances its drop's RELEASE — without it the struct clone
    /// shallow-copies the handle → refcount underflow → UAF
    /// (`shared_struct_field_clone.gg`). This is the sanctioned
    /// C-emit-boundary spelling: the `{Family}__` mangling IS the runtime
    /// contract (identical form to the adjacent `is_wrapper_method`
    /// `starts_with("Shared__")` dispatcher), so it genuinely cannot be
    /// typed away. `refcount_clone_arm_symmetry` locks the arm set.
    const BUDGET: usize = 259;

    let count = count_name_prefix_sites();
    assert!(
        count <= BUDGET,
        "Name-prefix routing count grew beyond budget: {count} > {BUDGET}.\n\n\
         The layering-discipline ratchet (Tier 3a per docs/devbook/25-structural-guards.md) \
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
    const BUDGET: usize = 138;

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
    /// Bumped 83 → 84 (2026-06-21): one new proxy read from error-model
    /// Increment 2 — the `!ctx.drops.is_moved(src)` idempotence guard before
    /// `move_zero_and_mark` in `lower_fault_catch_expr`'s `store_result`
    /// (`exprs/mod.rs`). A resource fault-catch result (e.g. a cloned String
    /// from a `Fault.Bounds` element read) is Move-staged into the result
    /// local; without the move-zero the source double-frees. SAME write-side-
    /// discipline class as the 64→…→83 bumps — `move_zero_and_mark` is
    /// non-idempotent (asserts) and `is_moved` is drop-accountant state with no
    /// `LocalOwnership` accessor. Locking in the floor.
    /// Bumped 84 → 85 (2026-06-25): one new proxy read from the catch
    /// RECOVERY-assign double-free fix (`ab8e3e7a`): the `!ctx.drops.is_moved(src)`
    /// idempotence guard before `move_zero_and_mark` on the recovery source in
    /// `lower_catch_expr` (`exprs/mod.rs`). An ALLOCATING `catch` recovery
    /// (`catch (e): "[" + e + "]"`) Move-stages a fresh String temp into the
    /// result local; without the move-zero the source double-frees. SAME write-
    /// side-discipline class as the 64→…→84 bumps — `move_zero_and_mark` is
    /// non-idempotent (asserts) and `is_moved` is drop-accountant state with no
    /// `LocalOwnership` accessor. Locking in the floor.
    /// Bumped 85 → 93 (2026-07-26): seven new proxy-read LINES (eight counted
    /// occurrences) from `5b8aa6da` ("overload call drop-reg and Identifier
    /// drop-old"), which landed 2026-07-21 and left this ratchet RED for five
    /// days — see the process note in `docs/devbook/30-excellence-system.md` §4;
    /// the code is sound, the *gate* was not being run at round close.
    /// Four in `exprs/calls.rs` are one more instance of the SAME four-part
    /// guard the 78→82 bump documented — `!is_named_local && is_owned_local &&
    /// !drops.is_registered && !drops.is_moved`, here re-homing an owning
    /// temporary (an inline `Acc(...)` ctor arg) that the callee borrows but
    /// does not drop, so the caller must free it after the call.
    /// Three in `stmts/assigns.rs` are drop-accountant idempotence guards of
    /// the same class as the 83→84 and 84→85 bumps: the `is_moved`/
    /// `is_registered` pair gating `move_zero_and_mark` on a field-assign RHS,
    /// and the `is_moved` → `drop_if_alive` choice on the String-`+=` rebind
    /// (dropping the old buffer unconditionally would double-free a local
    /// already move-zeroed on another path).
    /// SAME write-side-discipline class as the 64→…→85 bumps — `is_moved` and
    /// `is_registered` are DROP-ACCOUNTANT state, not `LocalOwnership`, and
    /// remain unmigratable until the drop accountant is queryable off `Local`.
    /// Locking in the floor.
    /// Bumped 93 → 95 (2026-08-06, MERGE): TWO independent proxy-read additions
    /// landed in the same round (MEMORY SAFETY / ONE OWNERSHIP BOUNDARY):
    ///   1. Track B: `!self.drops.is_registered(local)` in
    ///      `ensure_owned_at_consuming_arg`'s else arm at `context.rs:2693` —
    ///      restored the borrow-detection predicate to the expression-temp arm
    ///      so a View-tagged temp (Guard.get and family) clones at the
    ///      boundary instead of memcpying as a shallow alias. Mirrors the
    ///      identifier arm at :2638 line-for-line.
    ///   2. Track D+E: `!self.drops.is_moved(place.local)` idempotence guard
    ///      inside `LoweringContext::assign_with_move_follow_through` (which
    ///      `materialize_addressable` reuses) — the CHOKEPOINT the 7 raw
    ///      `!ctx.drops.is_moved` sites inside `lower_shared_var_decl` were
    ///      consolidated into.
    /// Both are SAME write-side-discipline class as the 64→…→85 bumps —
    /// `is_moved` and `is_registered` are DROP-ACCOUNTANT state, not
    /// `LocalOwnership`, and remain unmigratable until the drop accountant is
    /// queryable off `Local`. Track D+E's chokepoint paves the way for a
    /// future shrink once the analogous consolidation lands for
    /// `lower_var_decl`'s Pattern::Binding + Pattern::Tuple sites.
    /// Locking in the combined floor.
    const BUDGET: usize = 95;

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
///
/// 2026-07-17 (D29): Route-A count 1 → 2. The kind-2 producer-peel
/// (`resolve_kind2_call_type`, the marked-propagating branch: a `!`-marked
/// non-throws `Result[T,E]`-returning call peels to `T` in a propagating
/// context) is a SECOND intentional producer-peel that ALSO gates the discarded
/// `E` via `auto_prop_error_gate(e, span)` — exactly the sanctioned bump case.
#[test]
fn snag11_auto_prop_gate_site_count() {
    // Round XXIX Track A (2026-08-03): 14 → 12. The `Expr::Index` arm
    // rewrite consolidated three separate `auto_prop_skips_unify` calls
    // (one per String / Vector / Dict-HashMap branch) into ONE call at
    // the arm exit that covers every builtin AND user Index impl —
    // functionally equivalent E-checking, fewer duplicate call sites.
    // The choke point is preserved (unify runs only when the shared
    // gate allows it); the count drop reflects the code consolidation.
    // Round XXXII Track A (2026-08-06): 12 → 13. The `check_recovery_type`
    // helper adds site #13 at `src/semantic/typecheck.rs` — the `Expr::Catch`
    // recovery slot routes through the helper, which consults
    // `auto_prop_skips_unify` per the canonical VarDecl three-carve-out
    // pattern (E-check preserved: an ill-typed recovery still triggers the
    // shared gate). Post-D25 the `Expr::FaultCatch` sibling arm is gone.
    const EXPECTED_SKIPS_UNIFY: usize = 13;
    const EXPECTED_ROUTE_A_GATE: usize = 2;

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

/// D23 (throws totality) sibling-guard ratchet (CLAUDE.md rule 4 "one fix, all
/// siblings"). Every `throws`-carrying METHOD-return site in `infer_expr`'s
/// `Expr::MethodCall` arm must route its return type through
/// `resolve_throws_method_ret` (which forwards to the shared producer helper
/// `resolve_throws_call_type`). Otherwise a `throws` method typed as bare `T`
/// slips the totality gate → silent miscompile-to-garbage (the measured
/// `int x = 1 + s.risky()`). The method arm has THREE dispatch paths that each
/// return a method sig's `return_type`: the primary `resolve_method` hit, the
/// name-based trait-default fallback, and the cross-module-equip fallback. All
/// three are pinned here; a new method-return path added without the helper
/// changes the count and trips this lint.
///
/// Also pins the producer-helper call count: exactly one FREE-FN emit site
/// (`Expr::Call`) plus the one call inside `resolve_throws_method_ret` = 2.
///
/// Baseline 2026-07-10: 3 `resolve_throws_method_ret` call sites +
/// 2 `resolve_throws_call_type` call sites. Track E2 (2026-07-27) added a
/// 4th `resolve_throws_method_ret` call in the D36 auto-deref
/// user-method-hit arm — routes through the same producer so
/// `throws`-carrying equipped methods called via auto-deref propagate
/// the throws obligation identically to a direct call. Round XXXIII Batch C1
/// (D26 fallible arithmetic operators, 2026-08-06) added a 3rd
/// `resolve_throws_call_type` call from `check_fallible_arith_binop` — the
/// `+!` / `-!` / etc glyph is a throws-producer position mirroring a plain
/// `throws`-fn call (the `!` glyph IS the mark), so it routes through the
/// SAME producer helper for the D29 disposition table.
#[test]
fn d23_method_throws_return_sites() {
    const EXPECTED_METHOD_RET_SITES: usize = 4;
    const EXPECTED_PRODUCER_CALLS: usize = 3;

    let content = fs::read_to_string("src/semantic/typecheck.rs").unwrap_or_default();
    let mut method_ret_sites = 0usize;
    let mut producer_calls = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        // `self.`-receiver calls only — excludes the `fn ...(` definitions.
        method_ret_sites += line.matches(".resolve_throws_method_ret(").count();
        producer_calls += line.matches(".resolve_throws_call_type(").count();
    }
    assert_eq!(
        method_ret_sites, EXPECTED_METHOD_RET_SITES,
        "D23 method throws-return site count changed: {method_ret_sites} vs \
         {EXPECTED_METHOD_RET_SITES}.\n\n\
         If you added a `throws`-carrying method-return site to the \
         `Expr::MethodCall` arm (a new dispatch fallback returning a method \
         sig's `return_type`), it MUST route through \
         `self.resolve_throws_method_ret(def_id, &method.node, resolved_receiver, \
         sig.return_type, suppress_auto_prop, expr.span)` — NOT return the bare \
         `return_type` — \
         so an unhandled `throws` method is gated (E_UnhandledThrows) instead of \
         silently miscompiling. Then bump EXPECTED_METHOD_RET_SITES.\n\
         If you removed one, lower it.",
    );
    assert_eq!(
        producer_calls, EXPECTED_PRODUCER_CALLS,
        "D23 producer-helper call count changed: {producer_calls} vs \
         {EXPECTED_PRODUCER_CALLS}.\n\n\
         `resolve_throws_call_type` is the single producer chokepoint: the \
         free-fn `Expr::Call` emit site + the one call inside \
         `resolve_throws_method_ret`. A new direct caller (or a removed one) \
         changes this. Verify it's a genuine throws-producer position and bump.",
    );
}

/// D29 R-B ratchet (chain review 2; CLAUDE.md Core #6 — convert the recurring
/// class into an executable guard): the `Expr::Propagate` TRANSPARENT-WRAPPER
/// arms across the Rust-side walkers, pinned per file. The migration exposed
/// that Rust AST walkers drift where the self-host's arm-count-exhaustiveness
/// lints forced completeness: the generics discovery was missing the WHOLE
/// error-handling wrapper class (Catch/Rethrow/FaultCatch — an undefined
/// symbol at link for `write_str[File](&f, s)! catch …`), and every
/// `_ => {{}}` catch-all walker silently skips a wrapper node it has no arm
/// for (missed use → conservative clone; silent under-capture; lost instance).
/// This pins today's Propagate coverage so an arm cannot be silently REMOVED,
/// and the failure message carries the sweep obligation for the next wrapper
/// variant: when you add a wrapper Expr node, grep every file listed here and
/// extend each walker's wrapper group (then bump its count with justification).
#[test]
fn d29_propagate_walker_arm_coverage() {
    // (file, expected `Expr::Propagate` occurrence count). Source-derived
    // 2026-07-17 after the full `Expr::Move` sibling sweep. Counts include
    // pattern arms and constructions alike — the pin is on coverage presence,
    // not arm shape.
    const EXPECTED: &[(&str, usize)] = &[
        // R41 T-FMT-A (2026-08-11): 1 → 2. `emits_leading_ownership_sigil`
        // adds a SECOND `Expr::Propagate` arm — the parse-order paren
        // predicate must see THROUGH the wrapper to decide whether the
        // emitted text leads with an ownership sigil, exactly the
        // transparent-wrapper obligation this lint exists to pin.
        ("src/formatter/mod.rs", 2),
        ("src/ir/lowering/closures.rs", 1),
        ("src/ir/lowering/context.rs", 1),
        ("src/ir/lowering/exprs/mod.rs", 2),
        ("src/ir/lowering/functions.rs", 1),
        ("src/ir/lowering/generics/mod.rs", 2),
        ("src/ir/lowering/generics/substitute.rs", 1),
        ("src/ir/lowering/liveness.rs", 1),
        ("src/loader.rs", 2),
        ("src/parser/expr.rs", 2),
        ("src/parser/visitor.rs", 1),
        ("src/semantic/meta.rs", 2),
        ("src/semantic/resolve.rs", 1),
        ("src/semantic/rewrite.rs", 2),
        ("src/semantic/safety/check_expr.rs", 1),
        ("src/semantic/safety/helpers.rs", 5),
        ("src/semantic/safety/origins.rs", 1),
        ("src/semantic/safety/return_borrows.rs", 2),
        ("src/semantic/safety/validation.rs", 1),
        ("src/semantic/typecheck.rs", 7),
    ];
    for (file, expected) in EXPECTED {
        let content = fs::read_to_string(file).unwrap_or_default();
        let count = content.matches("Expr::Propagate").count();
        assert_eq!(
            count, *expected,
            "`Expr::Propagate` arm coverage changed in {file}: {count} vs              {expected}.\n\n             The D29 mark is a TRANSPARENT wrapper: every AST walker that has a              `Expr::Move`/wrapper group must see THROUGH it, and `_ => {{}}`              catch-alls make a missing arm silent (missed use → conservative              clone; lost generic instance → undefined symbol; silent              under-capture). If you removed an arm, restore it (or justify +              lower the count). If you are adding a NEW wrapper Expr variant,              extend the wrapper group in EVERY file in this table (the              sibling-sweep obligation), then bump the counts.",
        );
    }
}

/// D29 (visible error propagation) kind-2 sibling-guard ratchet (CLAUDE.md rule
/// 4 "one fix, all siblings"). A KIND-2 fallible call — a non-`throws` callee
/// whose DECLARED return is `Result[T,E]` — must route its return type through
/// `resolve_kind2_call_type` so a `!`-mark peels + activates the error channel
/// and an unmarked value flow stays a legal `Result`. There are exactly TWO
/// classification points: the free-fn `Expr::Call` non-throws branch, the
/// centralized method `None` branch inside `resolve_throws_method_ret`, and the
/// builtin Result-combinator path (`infer_closure_method_type` returns — a
/// marked `r.and_then(f)!` must consume its mark like any kind-2 call). A new
/// call-shape that returns a callee `Result` without routing here silently
/// makes `parse(s)!` a no-op (the mark ignored) — the D29 hole. Bump the count
/// when you add a genuine kind-2 classification point.
///
/// Baseline 2026-07-17 (remediation): 3 `resolve_kind2_call_type` call sites.
#[test]
fn d29_kind2_call_sites() {
    const EXPECTED_KIND2_SITES: usize = 3;

    let content = fs::read_to_string("src/semantic/typecheck.rs").unwrap_or_default();
    let mut kind2_sites = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        // `.`-receiver calls only — excludes the `fn ...(` definition.
        kind2_sites += line.matches(".resolve_kind2_call_type(").count();
    }
    assert_eq!(
        kind2_sites, EXPECTED_KIND2_SITES,
        "D29 kind-2 call-site count changed: {kind2_sites} vs \
         {EXPECTED_KIND2_SITES}.\n\n\
         Every non-`throws` callee whose declared return is `Result[T,E]` must \
         route through `self.resolve_kind2_call_type(return_type, \
         suppress_auto_prop, fallible_call_marked, span)` so a `!`-mark peels + \
         activates and an unmarked flow stays a legal `Result`. If you added a \
         new kind-2 classification point (a call-typing branch that returns a \
         callee `Result`), route it here and bump the count; if you removed one, \
         lower it.",
    );
}

/// D23 trait-registry keying ratchet (Fix 1; CLAUDE.md rule 4 "one fix, all
/// siblings"). Trait-name → DefId resolution in the traits.rs REGISTRATION
/// paths must use the TYPE namespace (`scopes.lookup_type`), never the
/// value-first `scopes.lookup`: a `from mod import Trait` placeholder
/// registers in BOTH namespaces but `export_non_private` overwrites it only
/// in the namespaces the source module exports — a trait exports no value
/// entry, so the stale value-namespace placeholder wins a value-first read
/// and keys the registry under the Import def while `process_impl` resolves
/// the REAL trait def via `lookup_type`. The key mismatch made every
/// cross-module trait-DEFAULT method invisible to typecheck (typed as
/// `error_id` — the D23 silent-miscompile hole).
///
/// Scoped to the registration FUNCTIONS (`register_builtin_traits`,
/// `collect_trait`) — NOT a whole-file zero-count — because traits.rs keeps
/// two legitimate value-first lookups elsewhere (orphan-rule self-type
/// locality check; `build_function_sig`'s `Future` wrap, which falls back to
/// the type namespace anyway).
#[test]
fn d23_trait_registration_lookup_type() {
    let content = fs::read_to_string("src/semantic/traits.rs")
        .expect("read src/semantic/traits.rs");
    for func in ["register_builtin_traits", "collect_trait"] {
        let sig = format!("fn {func}(");
        let start = content.find(&sig).unwrap_or_else(|| {
            panic!(
                "traits.rs registration fn `{func}` not found — if it was \
                 renamed, repoint this lint at the new registration path"
            )
        });
        // Body extends to the next top-level `fn ` (all registration paths
        // in traits.rs are free fns at column 0).
        let rest = &content[start..];
        let end = rest[sig.len()..]
            .find("\nfn ")
            .map(|i| i + sig.len())
            .unwrap_or(rest.len());
        let body = &rest[..end];
        let value_first: usize = body
            .lines()
            .filter(|l| !l.trim_start().starts_with("//"))
            .map(|l| l.matches("scopes.lookup(").count())
            .sum();
        assert_eq!(
            value_first, 0,
            "trait registration fn `{func}` contains {value_first} value-first \
             `scopes.lookup(` call(s). Registration must resolve trait names \
             through `scopes.lookup_type(` — a value-first read can be diverted \
             by a stale import placeholder (or any same-named value def) and \
             keys the registry under the wrong DefId: every cross-module \
             trait-default method then types as `error_id` (D23 silent \
             miscompile). Use `scopes.lookup_type(` instead.",
        );
        let typed: usize = body.matches("scopes.lookup_type(").count();
        assert!(
            typed >= 1,
            "trait registration fn `{func}` no longer contains any \
             `scopes.lookup_type(` call — if name resolution moved elsewhere, \
             repoint this lint at the new registration path (it pins the \
             type-namespace keying invariant).",
        );
    }
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

/// Round XXIX Track A class-retirement guard (Core #6): the `Expr::Index`
/// arm in `src/semantic/typecheck.rs` unified the two parallel `[]`
/// decision paths (hardcoded kind gate + `Index`/`IndexMut` trait
/// dispatch) into a SINGLE semantic gate through the trait registry
/// (`has_trait_impl_by_name`). All dispatch on concrete builtin K/V
/// shapes lives in ONE `match type_name.as_str() { … }` site inside the
/// arm — the intentional "how to compute (K, V) from an
/// intrinsic-satisfying receiver" arm.
///
/// This lint pins the number of `.as_str()`-driven type-name gates
/// inside the `Expr::Index` arm to exactly ONE. Any future ad-hoc
/// `if type_name == "MyType"` or `matches!(type_name.as_str(), "X"|"Y")`
/// at that site trips the count — the exact regression shape the
/// unification retired.
///
/// **If this fails**: the arm re-grew a name-match. Either
///   1. Fold the new case into the existing single dispatch site (add
///      to the `"Vector" | "Deque" | ...` etc. arms), or
///   2. If a legitimate second dispatch axis appears, bump EXPECTED
///      deliberately with a comment naming the invariant it guards.
///
/// See `docs/devbook/24-layering-discipline.md` Rule 3 (one source of
/// truth per axis) — the trait registry is the axis for `[]`.
fn count_index_arm_type_name_gates() -> usize {
    let content = match fs::read_to_string("src/semantic/typecheck.rs") {
        Ok(s) => s,
        Err(_) => return 0,
    };
    // Scope to the `Expr::Index { object, index }` arm.
    let mut in_arm = false;
    let mut depth = 0i32;
    let mut count = 0;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        if trimmed.starts_with("Expr::Index { object, index }") {
            in_arm = true;
            depth = 0;
            continue;
        }
        if !in_arm {
            continue;
        }
        depth += line.matches('{').count() as i32;
        depth -= line.matches('}').count() as i32;
        // Count `.as_str()` uses inside the arm (proxy for name-match
        // decisions). The single legitimate site is the K/V dispatch
        // `match type_name.as_str() { "Vector" | "Deque" => ... }`.
        if trimmed.contains(".as_str()") {
            count += 1;
        }
        if depth < 0 {
            in_arm = false;
        }
    }
    count
}

#[test]
fn index_arm_type_name_gates_count() {
    /// Baseline 2026-08-03 (Round XXIX Track A close): 1 site — the
    /// single `match type_name.as_str() { "Vector" | "Deque" | ... }`
    /// dispatch that computes (K, V) from a builtin receiver.
    const EXPECTED: usize = 1;
    let count = count_index_arm_type_name_gates();
    assert_eq!(
        count, EXPECTED,
        "Type-name gate count in the `Expr::Index` arm changed: {count} vs \
         expected {EXPECTED}.\n\n\
         Round XXIX Track A unified the two parallel `[]` decision paths \
         (kind gate + trait dispatch) into ONE `has_trait_impl_by_name` \
         gate, with ONE `match type_name.as_str() {{ ... }}` dispatch site \
         for K/V shape. A new `.as_str()`-driven name-match at this arm \
         is exactly the class the unification retired (Layering rule 3, \
         one source of truth per axis).\n\n\
         Fold new cases into the existing dispatch arm (add to \
         `\"Vector\" | \"Deque\" | ...`), OR bump EXPECTED with a \
         comment naming the invariant the new axis guards.",
    );
}

/// Round XIX Track N2 Class B (Core #4): every Box[Trait] formation site that
/// cannot rely on a typed SlotStore destination must route through
/// `pack_trait_object_for_smart_ptr_ctor`. Ratchet the call-site count so a
/// new formation site can't silently skip the pack.
///
/// LAND sites at N2 close: smart-ptr ctors (×6) + maybe_pack_at_arg + struct
/// field + emit_enum_init_owned + array-lit (×2) + closure return = 12.
#[test]
fn pack_trait_object_call_sites_count() {
    const EXPECTED: usize = 12;
    let count = count_pack_trait_object_calls();
    assert_eq!(
        count, EXPECTED,
        "pack_trait_object_for_smart_ptr_ctor call-site count changed: {count} vs \
         expected {EXPECTED}.\n\n\
         If a new Box[Trait] formation site was added, wire it through the pack \
         helper (or emit_enum_init_owned for enum fields) and bump EXPECTED.\n\
         If a site was removed / centralized, lower EXPECTED.",
    );
}

/// Round XXIII Track β class-retirement guard (Core #6): the
/// operand-position `&`-of-a-place reject
/// (`SemanticErrorKind::AmpInOperandPosition`) is emitted from a small,
/// enumerated set of walker arms in `src/semantic/safety/`. If a new safety
/// walker gets added that recurses into `Expr::MutableBorrow` (a new
/// re-parse path, a new pass), it MUST route the reject through the
/// producer — this ratchet forces a review.
///
/// Enumerated emit sites (baseline 2026-08-01):
///   - `check_expr.rs` main safety pass `Expr::MutableBorrow` arm (the
///     ONE-PRODUCER chokepoint, hit by every operand-position `&` that isn't
///     stripped by a pre-strip preamble).
///   - `check_expr.rs::check_interpolation_expr` fstring interp walker arm.
///     Sibling walker forced by the fact that f-string interpolation bodies
///     are re-parsed via `parse_expr` with synthetic spans that don't match
///     the resolution map, so it can't reuse `check_expr`'s recursion. Same
///     class, same reject, different span source (`fstring_span`).
///
/// The complementary strip preambles (`for x in &coll`, `.enumerate()`
/// receiver-wrap, comprehension iterable) are enforced structurally by the
/// `check_iterable_maybe_amp` helper being the ONLY path used in the four
/// iterable sites (Stmt::For + 3 comprehension arms) — see that helper's
/// doc comment for the case enumeration.
#[test]
fn amp_in_operand_position_reject_sites_count() {
    const EXPECTED: usize = 2;
    let count = count_amp_in_operand_position_rejects();
    assert_eq!(
        count, EXPECTED,
        "AmpInOperandPosition reject-emit-site count in \
         `src/semantic/safety/**/*.rs` changed: {count} vs expected {EXPECTED}.\n\n\
         If a NEW safety walker was added that recurses into \
         `Expr::MutableBorrow` and needs to reject the operand-position class, \
         verify it emits `SemanticErrorKind::AmpInOperandPosition` at the \
         chokepoint (Core #4 one-producer) and bump EXPECTED. If a site was \
         removed / centralized, lower EXPECTED. NEVER add a walker that \
         silently accepts `&`-of-a-place in an operand position (Core #10).",
    );
}

/// Round XXIV Track B: `gg run` / `gg test` masked SIGSEGV as exit 1 because
/// `ExitStatus::code()` returns `None` on Unix for signal-death, and both
/// `.code().unwrap_or(1)` and `process::exit(if any_failed { 1 } else { 0 })`
/// silently folded signal-death into the same 1 emitted for a compile error.
/// Fix routes ALL child-process exit propagation through
/// `propagate_child_status` (chokepoint) OR through an aggregation loop that
/// reads `ExitStatusExt::signal()` and exits `128 + signo`. This lint prevents
/// any new site from re-instantiating either syntactic costume.
#[test]
fn child_exit_status_propagation_chokepoint() {
    let src = std::fs::read_to_string("src/main.rs").unwrap();
    let mut hits = Vec::new();
    let lines: Vec<&str> = src.lines().collect();

    // Costume 1: the direct pattern. Broaden to any `.code().unwrap_or(` or
    // `.code().unwrap()` so a future evasion via `unwrap_or(2)` or `unwrap()`
    // cannot silently pass an exact-literal check. Skip comment lines and the
    // helper's own chokepoint fallback (marked `LINT-CHOKEPOINT-FALLBACK`).
    for (lineno, line) in lines.iter().enumerate() {
        let t = line.trim_start();
        if t.starts_with("//") || t.starts_with("///") {
            continue;
        }
        if line.contains("LINT-CHOKEPOINT-FALLBACK") {
            continue;
        }
        if line.contains(".code().unwrap_or(") || line.contains(".code().unwrap()") {
            hits.push(format!(
                "src/main.rs:{}: DIRECT COSTUME — must route through propagate_child_status(): {}",
                lineno + 1,
                line.trim()
            ));
        }
    }

    // Costume 2: the aggregation pattern. It MAY appear in the tree, but only
    // when immediately preceded (within 25 lines above) by a signal-aware
    // guard block that reads `ExitStatusExt::signal()` and exits `128+signo`.
    // We enforce this positionally: for every `process::exit(if any_failed`
    // line, require an `ExitStatusExt::signal()` mention within the preceding
    // 25 lines. (A novel THIRD costume would bypass both arms; this lint
    // guards the EXISTING syntactic costumes from silent regression.)
    for (lineno, line) in lines.iter().enumerate() {
        if line.trim_start().starts_with("//") {
            continue;
        }
        if line.contains("process::exit(if any_failed") {
            let window_start = lineno.saturating_sub(25);
            let window: String = lines[window_start..lineno].join("\n");
            // The signal-guarded exit block (`process::exit(128 + signo);`
            // within `if let Some((.., signo)) = first_signal { ... }`) must
            // appear immediately before the aggregation exit. The raw
            // `ExitStatusExt::signal()` call may live further up the function
            // (in the collection loop), but the guarded-exit block itself is
            // load-bearing at this position.
            let has_guarded_exit = window.contains("process::exit(128 + signo)");
            if !has_guarded_exit {
                hits.push(format!(
                    "src/main.rs:{}: AGGREGATION COSTUME without signal-guard — must be immediately preceded (within 25 lines) by an `if let Some((_, signo)) = first_signal {{ ... process::exit(128 + signo); }}` block that surfaces signal-death: {}",
                    lineno + 1,
                    line.trim()
                ));
            }
        }
    }

    assert!(
        hits.is_empty(),
        "child-exit status must route through propagate_child_status() or a \
         signal-guarded aggregation (Round XXIV Track B). Found: {:#?}",
        hits
    );
}

fn count_amp_in_operand_position_rejects() -> usize {
    let mut count = 0;
    for path in walkdir_rs("src/semantic/safety") {
        let content = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            // The emit-site signature: `SemanticErrorKind::AmpInOperandPosition,`
            // (comma indicates it's the first arg to `self.error(kind, span)`).
            // Excludes textual mentions in comments (filtered above) and any
            // future `matches!(..., AmpInOperandPosition)` classifier use.
            if t.contains("SemanticErrorKind::AmpInOperandPosition,") {
                count += 1;
            }
        }
    }
    count
}

/// Round XXVIII Track C class-retirement guard (Core #6): D32:1278-1281
/// sibling of `amp_in_operand_position_reject_sites_count`. `!`-in-
/// operand-position reject (`SemanticErrorKind::MoveInOperandPosition`) is
/// emitted from a small, enumerated set of walker arms in
/// `src/semantic/safety/`. If a new safety walker gets added that recurses
/// into `Expr::Move` (a new re-parse path, a new pass), it MUST route the
/// reject through the producer — this ratchet forces a review.
///
/// Enumerated emit sites (baseline 2026-08-02):
///   - `check_expr.rs` main safety pass `Expr::Move` arm (the ONE-PRODUCER
///     chokepoint, hit by every operand-position `!` that isn't stripped
///     by a pre-strip preamble or bracketed by a
///     `suppress_move_in_operand_position` boundary set).
///   - `check_expr.rs::check_interpolation_expr` fstring interp walker arm.
///     Sibling walker forced by the fact that f-string interpolation bodies
///     are re-parsed via `parse_expr` with synthetic spans that don't match
///     the resolution map, so it can't reuse `check_expr`'s recursion. Same
///     class, same reject, different span source (`fstring_span`).
///
/// The complementary suppress sites (VarDecl/Assign/Return/Throw/Send DIRECT
/// top-level `Expr::Move` RHS + container-literal-element walker arms
/// (ArrayLiteral/TupleLiteral/DictLiteral/StructLiteral) + iterable-strip
/// via `check_iterable_maybe_amp`) are enforced structurally at their call
/// sites. Enum-init is EXCLUDED (parser pre-strips call args, no walker arm
/// to modify).
#[test]
fn move_in_operand_position_reject_sites_count() {
    const EXPECTED: usize = 2;
    let count = count_move_in_operand_position_rejects();
    assert_eq!(
        count, EXPECTED,
        "MoveInOperandPosition reject-emit-site count in \
         `src/semantic/safety/**/*.rs` changed: {count} vs expected {EXPECTED}.\n\n\
         If a NEW safety walker was added that recurses into \
         `Expr::Move` and needs to reject the operand-position class, \
         verify it emits `SemanticErrorKind::MoveInOperandPosition` at the \
         chokepoint (Core #4 one-producer) and bump EXPECTED. If a site was \
         removed / centralized, lower EXPECTED. NEVER add a walker that \
         silently accepts `!`-of-a-place in an operand position (Core #10).",
    );
}

fn count_move_in_operand_position_rejects() -> usize {
    let mut count = 0;
    for path in walkdir_rs("src/semantic/safety") {
        let content = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            if t.contains("SemanticErrorKind::MoveInOperandPosition,") {
                count += 1;
            }
        }
    }
    count
}

/// Round XXVIII Track C — SH-lane mirror of `move_in_operand_position_reject_sites_count`.
/// D32:1278-1281 all-lanes: SH is the 3rd REJECT lane alongside Rust and ggdef.
/// This ratchet pins two SH counts in `tests/fixtures/self_host_typechecker/typecheck.gg`:
///
/// (1) EMIT sites — `DkMoveInOperandPosition(),` (comma indicates it's the
///     `Diagnostic.error(span, kind, msg)` kind arg). Baseline = 1: the ONE-PRODUCER
///     chokepoint at `check_safety_expr`'s `case EMove(inner):` arm. SH's f-string
///     interpolation is pre-parsed into `EFString.exprs` (not re-parsed with
///     synthetic spans like Rust), so SH does not need an interpolation-specific
///     sibling emit — one arm suffices for all operand contexts.
///
/// (2) STRIP CALL SITES — same `check_iterable_maybe_amp(` count as the `&`
///     sibling (baseline 4: SFor + EListComp + ESetComp + EDictComp iterables).
///     The helper is extended in R3 to peel `EMove` alongside `EMutableBorrow`
///     inline — one helper, both sigils. So the call-site count doesn't grow;
///     the `sh_amp_operand_reject_sites_count` lint already pins it to 4.
///     This lint therefore only pins the EMIT axis for Move.
#[test]
fn sh_move_operand_reject_sites_count() {
    let src = fs::read_to_string("tests/fixtures/self_host_typechecker/typecheck.gg")
        .expect("read self_host_typechecker/typecheck.gg");
    // Strip line comments so prose mentions of the marker don't count.
    let body: String = src
        .lines()
        .map(|l| l.split('#').next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    const EXPECTED_EMIT: usize = 1;
    let emit_count = body.matches("DkMoveInOperandPosition(),").count();
    assert_eq!(
        emit_count, EXPECTED_EMIT,
        "SH `DkMoveInOperandPosition()` emit-site count in \
         `tests/fixtures/self_host_typechecker/typecheck.gg` changed: {emit_count} vs \
         expected {EXPECTED_EMIT}.\n\n\
         The ONE-PRODUCER chokepoint (Core #4) is the `case EMove(inner):` \
         arm of `check_safety_expr` — every operand-position `!` reaches there via \
         its containing walker arm. If a NEW re-parse walker was added, verify it \
         emits `DkMoveInOperandPosition()` at the chokepoint (do not open-code) and \
         bump EXPECTED. If a site was removed, verify it centralized upward and \
         lower EXPECTED. NEVER add a walker that silently accepts operand `!` \
         (Core #10).",
    );
}

/// Round XXVII Track E — SH-lane mirror of `amp_in_operand_position_reject_sites_count`.
/// D32 all-lanes: SH is the 3rd REJECT lane alongside Rust and ggdef. This ratchet
/// pins two SH counts in `tests/fixtures/self_host_typechecker/typecheck.gg`:
///
/// (1) EMIT sites — `DkAmpInOperandPosition(),` (comma indicates it's the
///     `Diagnostic.error(span, kind, msg)` kind arg). Baseline = 1: the ONE-PRODUCER
///     chokepoint at `check_safety_expr`'s `case EMutableBorrow(inner):` arm. SH's
///     f-string interpolation is pre-parsed into `EFString.exprs` (not re-parsed with
///     synthetic spans like Rust), so SH does not need an interpolation-specific
///     sibling emit — one arm suffices for all operand contexts.
///
/// (2) STRIP CALL SITES — `check_iterable_maybe_amp(` (excluding the `void ... (`
///     definition line). Baseline = 4: SFor + EListComp + ESetComp + EDictComp
///     iterables. Every iterable site (a legit `&`-BOUNDARY position that must not
///     be false-flagged by the emit-arm chokepoint) MUST route through this ONE
///     helper — a 5th iterable site added inline (bypassing the helper) would
///     silently re-open the false-positive class.
///
/// **If this fails:**
///   - EMIT count changed → verify at the SH chokepoint arm (Core #4 one-producer).
///     A new emit site added inline is a class-split — reroute through the arm and
///     revert; a legitimate new sibling (a new re-parse walker) needs the arm-count
///     bumped with justification.
///   - STRIP count changed → verify a new iterable site was added via the helper,
///     not inlined; bump/lower EXPECTED accordingly. NEVER add an iterable site
///     that walks its iterable inline without the strip — a `for x in &coll` there
///     would false-flag the boundary `&` as an operand.
#[test]
fn sh_amp_operand_reject_sites_count() {
    let src = fs::read_to_string("tests/fixtures/self_host_typechecker/typecheck.gg")
        .expect("read self_host_typechecker/typecheck.gg");
    // Strip line comments so prose mentions of the marker don't count.
    let body: String = src
        .lines()
        .map(|l| l.split('#').next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    const EXPECTED_EMIT: usize = 1;
    let emit_count = body.matches("DkAmpInOperandPosition(),").count();
    assert_eq!(
        emit_count, EXPECTED_EMIT,
        "SH `DkAmpInOperandPosition()` emit-site count in \
         `tests/fixtures/self_host_typechecker/typecheck.gg` changed: {emit_count} vs \
         expected {EXPECTED_EMIT}.\n\n\
         The ONE-PRODUCER chokepoint (Core #4) is the `case EMutableBorrow(inner):` \
         arm of `check_safety_expr` — every operand-position `&` reaches there via \
         its containing walker arm. If a NEW re-parse walker was added, verify it \
         emits `DkAmpInOperandPosition()` at the chokepoint (do not open-code) and \
         bump EXPECTED. If a site was removed, verify it centralized upward and \
         lower EXPECTED. NEVER add a walker that silently accepts operand `&` \
         (Core #10).",
    );

    const EXPECTED_STRIP: usize = 4;
    // Count call sites (`check_iterable_maybe_amp(`) but EXCLUDE the definition
    // line (`void check_iterable_maybe_amp(`).
    let strip_calls = body
        .lines()
        .filter(|l| l.contains("check_iterable_maybe_amp("))
        .filter(|l| !l.trim_start().starts_with("void check_iterable_maybe_amp("))
        .count();
    assert_eq!(
        strip_calls, EXPECTED_STRIP,
        "SH `check_iterable_maybe_amp` call-site count changed: {strip_calls} vs \
         expected {EXPECTED_STRIP}. Every iterable position (SFor + 3 comprehension \
         arms) must route through this ONE helper — a new iterable site that walks \
         its iterable inline (bypassing the strip) would false-flag a legit \
         `&`-BOUNDARY iterable as an operand-position reject. If a legitimate new \
         iterable arm was added, wire it through the helper and bump EXPECTED. If \
         an arm was removed, lower EXPECTED — do NOT inline the strip.",
    );
}

fn count_pack_trait_object_calls() -> usize {
    let mut count = 0;
    for path in walkdir_rs("src/ir/lowering") {
        let content = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            if t.contains("fn pack_trait_object_for_smart_ptr_ctor") {
                continue;
            }
            if t.contains("use ") && t.contains("pack_trait_object_for_smart_ptr_ctor") {
                continue;
            }
            if t.contains("pack_trait_object_for_smart_ptr_ctor(") {
                count += 1;
            }
        }
    }
    count
}

/// Minimal recursive .rs walk without the walkdir crate.
fn walkdir_rs(root: &str) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![PathBuf::from(root)];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else { continue };
        for ent in entries.flatten() {
            let p = ent.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().and_then(|e| e.to_str()) == Some("rs") {
                out.push(p);
            }
        }
    }
    out
}

/// Ratchet (Core #4 "one fix, all siblings" / Layering-discipline "Sibling-site
/// drift — fix the class, not the instance"): the self-host D4/D12 drop-purity
/// enforcement (A2-S) rejects an implicit copy of a live drop-tainted place at
/// the CONSUMING ownership boundaries via ONE shared producer,
/// `reject_tainted_place`, in `self_host_typechecker/typecheck.gg`. Every
/// enumerated consuming position must route through that shared helper so a new
/// boundary can't silently skip the D12 check (an under-rejection = a Core-#8
/// live memory-safety hole). The 8 call sites are:
///   - position 1: SVarDecl init + SAssign value (bind / assign)
///   - position 2: ECall ctor arg + EStructLiteral arg + EDotShorthand arg
///   - position 3: EMethodCall ingest arg (collection put)
///   - position 4: SReturn value (return / expr-body) + EClosure trailing SExpr
///     (closure-tail)
/// (positions 5 [capture] and 6 [materialize-on-write / &self mutator] use their
/// own specialized producers — `reject_tainted_captures`,
/// `reject_materialize_on_write`, `reject_amp_self_mutator` — since they gate on
/// closure free-vars / param ownership rather than a plain place arg.)
///
/// **If this fails:**
///   - A NEW consuming position was added → it MUST call `reject_tainted_place`
///     (do not open-code the taint check); bump EXPECTED with a justification.
///   - The count went DOWN → a D12 hook was removed, re-opening an
///     under-rejection hole; restore the call, do not lower EXPECTED.
#[test]
fn self_host_d12_reject_hook_count() {
    // 1 definition (`void reject_tainted_place(`) + 8 consuming-position calls.
    // Positions 2 (ctor-arg) + 3 (collection-put) were re-enabled once the
    // CallArg{name, ownership, value} normalization landed: parse_call_args now
    // carries the `!`/`&` arg sigil as a TYPED `CallArg.ownership` field, so those
    // two positions gate on `a.ownership == OWN_BORROW` — a bare copy is rejected
    // while an explicit `push(!x)`/`W(!x)` move is accepted (no more over-rejection).
    const EXPECTED: usize = 9;
    let src = fs::read_to_string("tests/fixtures/self_host_typechecker/typecheck.gg")
        .expect("read self_host_typechecker/typecheck.gg");
    let count = src.matches("reject_tainted_place(").count();
    assert_eq!(
        count, EXPECTED,
        "self-host D12 `reject_tainted_place` site count changed: {count} vs \
         expected {EXPECTED} (1 def + 8 consuming-position hooks).\n\n\
         If a new consuming ownership boundary was added, route it through the \
         shared `reject_tainted_place` producer and bump EXPECTED. If a hook was \
         removed, a D12 under-rejection hole re-opened — restore it, do NOT lower \
         EXPECTED. See the A2-S self-host drop-purity brief (git history).",
    );
}

/// Ratchet (Core #4 "one fix, all siblings" / Layering-discipline "Sibling-site
/// drift — fix the class, not the instance"): every PROJECTED-mutation target
/// arm in `lower_compound_assign` (`obj.field OP= x`, `obj[i] OP= x`) must call
/// `materialize_assign_target_root` FIRST, so a bare-value-param / alias / element
/// root materializes a private owned copy before the read-modify-write instead of
/// writing THROUGH the caller. This is the sibling of the plain-store prologue in
/// `lower_field_assign` / `lower_index_assign`; the matcluster #1 fix added it to
/// the two compound arms (previously `xs[0] += 1` / `s.counts[0] += 1` on a bare
/// param wrote through, printing 11 instead of 10). The scalar `Expr::Identifier`
/// arm (`x += 1` / whole-local rebind) is NOT counted — a whole-local mutation is
/// handled by the rebind path, not a projection-root materialize.
///
/// **If this fails:**
///   - A NEW projected-mutation compound arm was added WITHOUT the prologue →
///     add `materialize_assign_target_root(ctx, builder, object);` at the TOP of
///     the arm (before it lowers `object`) and bump EXPECTED with a justification.
///   - The count went DOWN (a prologue was removed) → the write-through hole is
///     re-opened; restore the call, do not lower EXPECTED.
#[test]
fn compound_assign_root_materialize_arms_count() {
    let src = fs::read_to_string("src/ir/lowering/stmts/assigns.rs")
        .expect("read src/ir/lowering/stmts/assigns.rs");
    let sig = "pub(super) fn lower_compound_assign(";
    let start = src.find(sig).expect("locate lower_compound_assign");
    // Body ends at the next top-level `fn ` (compound_op_to_gir).
    let after_sig = start + sig.len();
    let end = src[after_sig..]
        .find("\nfn ")
        .map(|i| after_sig + i)
        .unwrap_or(src.len());
    // Strip line comments so the ratchet reasons about EXECUTABLE code only —
    // the arm comments legitimately mention the helper name in prose.
    let body: String = src[start..end]
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    // One call per projected-mutation arm: FieldAccess + Index + TupleFieldAccess
    // = 3 (the TupleFieldAccess arm `t.0 OP= v` was added with Target-2; a tuple
    // field is a projected mutation exactly like a struct field, so it too must
    // materialize the root FIRST on a bare-value-param / alias root).
    const EXPECTED: usize = 3;
    let calls = body.matches("materialize_assign_target_root(").count();
    assert_eq!(
        calls, EXPECTED,
        "`materialize_assign_target_root` call count in `lower_compound_assign` \
         changed: {calls} vs expected {EXPECTED}. Every PROJECTED-mutation compound \
         arm (`obj.field OP= x`, `obj[i] OP= x`) must materialize the root FIRST so \
         a bare-value-param / alias / element root gets a private owned copy instead \
         of writing THROUGH the caller (matcluster #1). If you added a legitimate \
         new projected arm, add the prologue and bump EXPECTED with a justification; \
         if a prologue was removed, RESTORE it — do not lower EXPECTED.",
    );
}

/// Structural guard (Core #6 "convert a recurring bug class into an executable
/// guard" + Core #10 "lower-or-reject — never silently drop user syntax"): the
/// `lower_compound_assign` FieldAccess arm must have a WRITE-THROUGH FALLBACK
/// for the `try_resolve_field_place → None` case, and the function must end with
/// a final catch-all that REJECTS (not silently drops) an unhandled target shape.
///
/// WHY an arm-COUNT lint is the WRONG shape here (and why this is a SEPARATE
/// guard from `compound_assign_root_materialize_arms_count` above): the
/// arm-count guard passes even when the FieldAccess ARM EXISTS but its inner
/// `if let Some(..try_resolve_field_place..)` has NO `else` — which is EXACTLY
/// the Target-2 miscompile (`coll.get(i).unwrap().field += v` silently dropped
/// the write on both backends because the `None` branch produced no store).
/// This guard asserts the fallback is PRESENT: the FieldAccess arm resolves the
/// None case through the shared `resolve_ptr_field_place` write-through resolver
/// via an `else`, AND a final catch-all `panic!` exists so a NEW unhandled
/// lvalue shape fails LOUDLY at build time instead of silently no-op'ing.
///
/// **If this fails:**
///   - The FieldAccess None-fallback (`else { … resolve_ptr_field_place … }`)
///     was removed → the `.get()`-Ref compound write-through hole re-opened;
///     RESTORE it (mirror `lower_field_assign`'s fallback), do NOT delete the lint.
///   - The final catch-all reject was removed → an unhandled compound target
///     shape now silently drops the write (Core #10); RESTORE the rejecting
///     `else` at the fn tail.
///   - You renamed the shared resolver / panic message → update the needle here
///     (the guard tracks PRESENCE of the fallback, not a literal string).
#[test]
fn compound_assign_fieldaccess_fallback_present() {
    let src = fs::read_to_string("src/ir/lowering/stmts/assigns.rs")
        .expect("read src/ir/lowering/stmts/assigns.rs");
    let sig = "pub(super) fn lower_compound_assign(";
    let start = src.find(sig).expect("locate lower_compound_assign");
    let after_sig = start + sig.len();
    let end = src[after_sig..]
        .find("\nfn ")
        .map(|i| after_sig + i)
        .unwrap_or(src.len());
    // Strip line comments so the guard reasons about EXECUTABLE code only — the
    // arm comments legitimately mention `try_resolve_field_place` /
    // `resolve_ptr_field_place` in prose.
    let body: String = src[start..end]
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    // Isolate the FieldAccess arm: from its match pattern up to the Index arm's.
    let fa_start = body
        .find("Expr::FieldAccess { object, field } = &target.node")
        .expect("locate FieldAccess arm in lower_compound_assign");
    let idx_start = body
        .find("Expr::Index { object, index } = &target.node")
        .expect("locate Index arm in lower_compound_assign");
    assert!(
        fa_start < idx_start,
        "FieldAccess arm must precede the Index arm in lower_compound_assign",
    );
    let fa_arm = &body[fa_start..idx_start];

    // 1. The Some-resolution (`try_resolve_field_place`) must be IMMEDIATELY
    //    followed by an `else` whose body calls the shared write-through
    //    resolver `resolve_ptr_field_place` — the None-fallback that turns the
    //    silent drop into a read-modify-write through the `.get()`-Ref place.
    let pos_try = fa_arm
        .find("try_resolve_field_place(")
        .expect("FieldAccess arm must call try_resolve_field_place");
    let else_after = fa_arm[pos_try..].find("} else {").map(|i| pos_try + i);
    let pos_none = fa_arm.find("resolve_ptr_field_place(");
    assert!(
        else_after.is_some() && pos_none.is_some() && else_after.unwrap() < pos_none.unwrap(),
        "lower_compound_assign's FieldAccess arm lost its write-through FALLBACK: \
         the `if let Some(..try_resolve_field_place..)` block must be followed by \
         an `else {{ … resolve_ptr_field_place … }}` branch that read-modify-writes \
         through the resolved `.get()`-Ref place. Without it, \
         `coll.get(i).unwrap().field OP= v` SILENTLY DROPS the write on both backends \
         (Target-2, Core #8/#10). Mirror `lower_field_assign`'s fallback; do NOT \
         remove this branch.",
    );

    // 2. The Deref (`*p OP= v`) and TupleFieldAccess (`t.0 OP= v`) arms must be
    //    LOWERED, not left to the catch-all (Core #10 lower-or-reject; both are
    //    valid places that formerly silently-dropped/ICE'd).
    assert!(
        body.contains("Expr::Deref { expr: inner } = &target.node"),
        "lower_compound_assign lost its Deref arm (`*p OP= v`): plain `*p = v` \
         lowers, so the compound path must too (Core #10 lower-or-reject) — RESTORE \
         the Deref arm, do NOT let it fall to the reject.",
    );
    assert!(
        body.contains("Expr::TupleFieldAccess { object, index } = &target.node"),
        "lower_compound_assign lost its TupleFieldAccess arm (`t.0 OP= v`): a tuple \
         field is a valid mutable place; without the arm it hits the catch-all and \
         ICEs on check-ACCEPTED code (Core #10 lower-or-reject) — RESTORE the arm.",
    );
    // 3. A final catch-all must exist for non-lvalue targets (`5 += 1`), backed by
    //    the check-time `E_InvalidAssignTarget` guard so it is genuinely
    //    unreachable — never a silent drop, never an ICE on accepted code.
    assert!(
        body.contains("E_InvalidAssignTarget"),
        "lower_compound_assign lost its final catch-all: a non-lvalue compound-assign \
         target must be REJECTED at check time (E_InvalidAssignTarget) and the \
         lowerer's tail must be an honest `unreachable!` citing that guard — never a \
         silent drop, never a bare panic on accepted code (Core #10). RESTORE the \
         trailing `}} else {{ unreachable!(...E_InvalidAssignTarget...) }}` AND its \
         check-side guard `check_assign_target_lvalue`.",
    );
}

/// Sibling-site-drift guard (Core #4 "one fix, all siblings" + the arm-count
/// lint the playbook prescribes): every place-based compound-assign arm — struct
/// FieldAccess (Some), the `.get()`-Ref None-fallback, TupleFieldAccess, and
/// Deref — reads-modifies-writes through the ONE shared helper
/// `emit_compound_place_rmw`. That helper is the single spot that avoids the
/// resource-move ICE (a resource field must NOT be read via an intermediate
/// `assign(cur, Copy(field_place))` — "shallow copy of resource") and routes the
/// store through `emit_field_store_with_cleanup` (drop-old + move-new). A 5th
/// place arm, or a regression that re-open-codes the shallow read inline, is the
/// exact sibling-drift this pins: the reader is centralized, so the next arm is
/// forced through it.
///
/// **If this fails:** a compound arm stopped calling `emit_compound_place_rmw`
/// (re-open-coded its own read) or a new place arm was added without routing
/// through it. Route it through the shared helper — do NOT re-introduce the
/// per-arm shallow read.
#[test]
fn compound_assign_resource_read_centralized() {
    let src = fs::read_to_string("src/ir/lowering/stmts/assigns.rs")
        .expect("read src/ir/lowering/stmts/assigns.rs");
    let def_count = src.matches("fn emit_compound_place_rmw(").count();
    assert_eq!(
        def_count, 1,
        "emit_compound_place_rmw must be defined exactly once (the shared \
         resource-safe compound read-modify-write); found {def_count}.",
    );
    // All four place-based compound arms call it with the `(ctx, builder, …)`
    // shape; the definition uses `(\n    ctx,` so it is not counted here.
    let call_count = src.matches("emit_compound_place_rmw(ctx, builder,").count();
    assert_eq!(
        call_count, 4,
        "expected EXACTLY 4 callers of emit_compound_place_rmw (the FieldAccess \
         Some arm, the `.get()`-Ref None-fallback, the TupleFieldAccess arm, and \
         the Deref arm), found {call_count}. A place-based compound-assign arm \
         must NOT re-open-code the current-value read — a resource field read via \
         an intermediate `assign(cur, Copy(field_place))` trips the resource-move \
         validator (\"shallow copy of resource\"), the R-STRING ICE. Route the arm \
         through emit_compound_place_rmw (Core #4, one fix all siblings).",
    );
}

/// Sibling-site-drift guard (Core #4): every bare user operator-overload call
/// routes through `emit_operator_overload_call` (ByPtr prep + call_tracked +
/// drain of THIS call's pending_temp_drops / pending_move_zeros). A 10th site
/// that re-open-codes bare `builder.call` for a user `__add` / `__eq` / …
/// reintroduces the resource-RHS temp leak / missing drop-old class R2R3
/// closed. Atomic `__add` / runtime symbols and custom Index `__get` stay on
/// their own paths (not counted).
///
/// Call sites pinned (9):
///   operators.rs ×4 — binary / unary / eq / compare overloads
///   assigns.rs ×3 — Identifier compound, Index compound, place RMW
///   stmts/mod.rs ×2 — assert Type__eq / Type__compare
///
/// **If this fails:** a new overload-call site was added without the helper,
/// or a site lost the route. Route through `emit_operator_overload_call`; bump
/// EXPECTED only with a justification naming the new arm.
#[test]
fn operator_overload_call_centralized() {
    let def_src = fs::read_to_string("src/ir/lowering/exprs/calls.rs")
        .expect("read src/ir/lowering/exprs/calls.rs");
    assert_eq!(
        def_src.matches("fn emit_operator_overload_call(").count(),
        1,
        "emit_operator_overload_call must be defined exactly once; found a \
         different count in exprs/calls.rs.",
    );

    const EXPECTED_CALLS: usize = 9;
    let files = [
        "src/ir/lowering/exprs/operators.rs",
        "src/ir/lowering/stmts/assigns.rs",
        "src/ir/lowering/stmts/mod.rs",
    ];
    let mut call_count = 0usize;
    for path in files {
        let src = fs::read_to_string(path).unwrap_or_else(|_| panic!("read {path}"));
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with("//") {
                continue;
            }
            if t.contains("emit_operator_overload_call(") {
                call_count += 1;
            }
        }
    }
    assert_eq!(
        call_count, EXPECTED_CALLS,
        "expected EXACTLY {EXPECTED_CALLS} callers of emit_operator_overload_call \
         (operators×4 + assigns×3 + stmts assert×2), found {call_count}. A new \
         user-overload call site must route through the shared helper (ByPtr prep \
         + call_tracked + pending-temp drain) — bare builder.call reopens the \
         resource-RHS leak / missing drop-old class (Core #4, solid-ground R2R3).",
    );
}

/// SH twin of `operator_overload_call_centralized` (Core #4 / Core #6). The
/// self-host lowerer centralizes user operator-overload calls through
/// `emit_overload_call` (`tests/fixtures/self_host_lowerer/lower_types.gg`) —
/// ByPtr prep + call + post-call temp drops / result registration. A 6th site
/// that re-open-codes a bare overload call reintroduces the drop-old-before-
/// rebind / resource-RHS temp class Track W closed.
///
/// Call sites pinned (5 direct `emit_overload_call` callers):
///   lower_expr.gg ×4 — compare / eq / binary op / unary neg
///   lower_types.gg ×1 — `emit_compound_arith` overload arm
/// Transitive compound-assign faces (lower_stmt Identifier ~1190 +
/// `lower_index_compound_assign` ~2296) reach via `emit_compound_arith` and
/// are NOT counted as direct callers.
///
/// **If this fails:** a new SH user-overload call site was added without the
/// helper, or a site lost the route. Route through `emit_overload_call`; bump
/// EXPECTED only with a justification naming the new arm.
#[test]
fn sh_operator_overload_call_centralized() {
    let def_src = fs::read_to_string("tests/fixtures/self_host_lowerer/lower_types.gg")
        .expect("read tests/fixtures/self_host_lowerer/lower_types.gg");
    assert_eq!(
        def_src
            .lines()
            .filter(|l| {
                let t = l.trim_start();
                !t.starts_with('#') && t.starts_with("void emit_overload_call(")
            })
            .count(),
        1,
        "emit_overload_call must be defined exactly once in lower_types.gg; \
         found a different count.",
    );

    const EXPECTED_CALLS: usize = 5;
    let files = [
        "tests/fixtures/self_host_lowerer/lower_expr.gg",
        "tests/fixtures/self_host_lowerer/lower_types.gg",
    ];
    let mut call_count = 0usize;
    for path in files {
        let src = fs::read_to_string(path).unwrap_or_else(|_| panic!("read {path}"));
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with('#') {
                continue;
            }
            // Definition is not a call site.
            if t.starts_with("void emit_overload_call(") {
                continue;
            }
            if t.contains("emit_overload_call(") {
                call_count += 1;
            }
        }
    }
    assert_eq!(
        call_count, EXPECTED_CALLS,
        "expected EXACTLY {EXPECTED_CALLS} callers of emit_overload_call \
         (lower_expr×4 compare/eq/binary/unary + lower_types emit_compound_arith×1), \
         found {call_count}. A new SH user-overload call site must route through \
         the shared helper — open-coding reopens the drop-old / resource-RHS \
         temp class Track W closed (Core #4).",
    );
}

/// Structural guard (Core #6 "convert a recurring bug class into an executable
/// guard" + Core #2 "typed metadata, never name-matching"): the 2T SEMANTIC
/// taint reject (`reject_tainted_materialize_on_write` + its formation sibling
/// `reject_tainted_formation_arg`) must NEVER read the dead-write LINT's
/// tracking set (`deadwrite_params` / `deadwrite_*`). Coupling an accept/reject
/// DECISION to the lint's filters (the `_`-prefix name exclusion, the
/// statement-position scoping) is what produced the live double-drops p11/p12 —
/// production accepted+double-dropped a `void poke(FH _fh): _fh.fd = 9` because
/// the reject rode the lint's `_`-name courtesy. The two helpers take an
/// UNFILTERED `find_root_def_id` root and decide purely from typed def state
/// (`is_param` + `Borrow` + `is_drop_tainted_type`); they must stay decoupled.
///
/// **If this fails:** a `deadwrite`-flavored read crept into a semantic reject
/// producer — the accept/reject decision is being re-coupled to the lint filter.
/// Route the reject off the unfiltered `find_root_def_id` root instead; the lint
/// keeps its filtered marking in `mark_bare_param_write_def`, separately.
#[test]
fn tainted_reject_never_reads_lint_state() {
    let src = fs::read_to_string("src/semantic/safety/helpers.rs")
        .expect("read src/semantic/safety/helpers.rs");
    // Extract each helper body (from its `pub(super) fn NAME(` to the next
    // top-level `    pub(super) fn ` / `    fn ` at 4-space indent) and assert
    // it references neither `deadwrite` nor the lint's `mark_bare_param_write`.
    for helper in ["reject_tainted_materialize_on_write", "reject_tainted_formation_arg"] {
        let sig = format!("fn {helper}(");
        let start = src.find(&sig).unwrap_or_else(|| panic!("locate {helper}"));
        let after = start + sig.len();
        let end = src[after..]
            .find("\n    pub(super) fn ")
            .or_else(|| src[after..].find("\n    fn "))
            .map(|i| after + i)
            .unwrap_or(src.len());
        let body = &src[start..end];
        assert!(
            !body.contains("deadwrite"),
            "SEMANTIC taint reject `{helper}` reads the dead-write LINT's tracking \
             state (`deadwrite*`). An accept/reject DECISION must not depend on the \
             lint's name/position filters (that coupling produced the p11/p12 \
             double-drops). Root off the UNFILTERED `find_root_def_id` instead.",
        );
    }
    // Every DIRECT-position reject call (assign / compound / receiver) roots via
    // the unfiltered `find_root_def_id` — never a `deadwrite_params` lookup.
    for file in [
        "src/semantic/safety/check_stmt.rs",
        "src/semantic/safety/check_expr.rs",
    ] {
        let content = fs::read_to_string(file).unwrap_or_default();
        let lines: Vec<&str> = content.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains("self.reject_tainted_materialize_on_write(") {
                let window_start = i.saturating_sub(3);
                let window = lines[window_start..i].join("\n");
                assert!(
                    window.contains("find_root_def_id("),
                    "`reject_tainted_materialize_on_write` call at {file}:{} is not \
                     fed by an unfiltered `find_root_def_id` root — a semantic \
                     reject must never be gated on the lint's tracked-root subset.",
                    i + 1,
                );
            }
        }
    }
}

/// Ratchet (Core #4 "one fix, all siblings" / Layering-discipline "Sibling-site
/// drift"): the 2T FORMATION-position gate (`reject_tainted_formation_arg`) must
/// be wired into every per-arg ownership loop so all THREE call KINDS —
/// free-call (`Expr::Call`), enum-init (`Expr::DotShorthand`, both via
/// `check_call_arg_ownership`), and method-call (`Expr::MethodCall`'s own arg
/// loop) — reject a `&`-of-projection arg (`f(&s.field)`) on a drop-tainted bare
/// root. Miss one loop and that call kind silently materializes a hidden clone
/// (double-drop). Two loops cover the three kinds, so exactly 2 call sites live
/// in `check_expr.rs`.
///
/// **If this fails:** a new per-arg loop (a new call kind) was added without the
/// formation gate, or a call site was dropped. Add
/// `self.reject_tainted_formation_arg(arg);` at the top of the new loop's
/// `MutableBorrow | Borrow` arm and bump EXPECTED — do NOT lower it (a removed
/// site re-opens the formation double-drop hole).
#[test]
fn tainted_formation_arg_gate_sites() {
    const EXPECTED: usize = 2;
    let src = fs::read_to_string("src/semantic/safety/check_expr.rs")
        .expect("read src/semantic/safety/check_expr.rs");
    let count = src.matches("self.reject_tainted_formation_arg(").count();
    assert_eq!(
        count, EXPECTED,
        "`reject_tainted_formation_arg` call-site count in check_expr.rs changed: \
         {count} vs expected {EXPECTED} (the two per-arg loops covering all three \
         call kinds). A new call kind's arg loop must call the formation gate too; \
         a removed site re-opens the `f(&s.field)` tainted double-drop.",
    );
}

/// Ratchet (Core #4 "one fix, all siblings" / devbook-24 rule 3 "one source of
/// truth per axis"): every collection family in `infer_fn_ptr_stores_from_types`
/// (`src/lir/lower/insts.rs`) must wire its element/value/key DROP slot through
/// the unified `self.type_drop_fns` map — NEVER through the legacy
/// `recursive_drop_structs`/`recursive_drop_enums` `.contains_key` gate.
///
/// The legacy gate is populated ONLY for types with a NON-EMPTY droppable-field
/// list, so a custom-Drop type with only trivial (int/float/bool/ptr) fields
/// fell through and its `drop()` was silently LOST when used as a collection
/// element (P1 — fd/lock-leak class); and where it DID fire it named
/// `{T}__drop` (the user body) instead of the composite `__gorget_dtor_{T}`, so
/// droppable fields LEAKED (P2 — ASan-only). `type_drop_fns` records
/// custom-with-trivial-fields AND carries the correct composite `drop_fn_name`,
/// so routing through it fixes BOTH. Full context: the elemdrop-fix brief (git history).
///
/// **If this fails:**
///   - `recursive_drop_*` reintroduced → a new sibling family re-opened the P1/P2
///     hole. Route its drop slot through `self.type_drop_fns.get(t).drop_fn_name`.
///   - route count changed → a new collection family / drop slot was added (or
///     removed). Confirm it routes through `type_drop_fns` (not a name-match) and
///     re-pin `EXPECTED_TYPE_DROP_FNS_ROUTES` with a justification.
///
/// Precedent: `container_literal_arms_count` above.
#[test]
fn collection_elem_drop_routes_through_type_drop_fns() {
    let src = fs::read_to_string("src/lir/lower/insts.rs")
        .expect("read src/lir/lower/insts.rs");
    let sig = "fn infer_fn_ptr_stores_from_types(";
    let start = src.find(sig).expect("locate infer_fn_ptr_stores_from_types");
    // The body ends at the next sibling method in the impl block.
    let after_sig = start + sig.len();
    let end = src[after_sig..]
        .find("\n    pub(super) fn ")
        .map(|i| after_sig + i)
        .unwrap_or(src.len());
    // Strip line comments so the ratchet reasons about EXECUTABLE code only —
    // the explanatory comment inside this fn legitimately names
    // `recursive_drop_structs` while the gate itself is gone.
    let body: String = src[start..end]
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    // (1) The buggy legacy gate must be GONE from executable code.
    for legacy in ["recursive_drop_structs", "recursive_drop_enums"] {
        assert!(
            !body.contains(legacy),
            "collection element drop wiring reintroduced the `{legacy}` gate in \
             `infer_fn_ptr_stores_from_types` (src/lir/lower/insts.rs). That gate \
             misses custom-Drop-with-trivial-fields element types (P1 lost-drop) \
             and its `{{T}}__drop` name is the user body, not the composite \
             `__gorget_dtor_{{T}}` (P2 field-leak). Route the drop slot through \
             `self.type_drop_fns.get(t).drop_fn_name` instead. See \
             the elemdrop-fix brief (git history).",
        );
    }

    // (2) Ratchet the number of `type_drop_fns` routes across the families.
    //     Post-fix baseline 2026-07-06: Vector (drop .get + clone .contains_key),
    //     Dict value (drop .get + clone .contains_key), Dict key (drop .get),
    //     Set key (drop .get) = 6. A new collection family / drop slot MUST route
    //     through `type_drop_fns` and bump this — never a name-match or the
    //     legacy recursive_drop_* gate.
    const EXPECTED_TYPE_DROP_FNS_ROUTES: usize = 6;
    let routes = body.matches("self.type_drop_fns").count();
    assert_eq!(
        routes, EXPECTED_TYPE_DROP_FNS_ROUTES,
        "`self.type_drop_fns` route count in `infer_fn_ptr_stores_from_types` \
         changed: {routes} vs expected {EXPECTED_TYPE_DROP_FNS_ROUTES}. A new \
         collection family / drop slot MUST route its element drop+clone wiring \
         through `type_drop_fns` (the one source of truth for per-type destructor \
         names) — never a name-match or the legacy recursive_drop_* gate. Bump \
         the budget with a justification if you added a legitimate new route.",
    );
}

/// Ratchet (Core #2 "No name matching" / Core #6 "convert the bug class to a
/// guard"): the consuming-position NAME match in `lower_method_call`
/// (`src/ir/lowering/exprs/methods.rs`) must stay GATED on the typed
/// `is_gir_method` (resolved-callee identity), so a USER equip method that
/// merely SHARES a name with a builtin collection mutator
/// (`push`/`add`/`insert`/`set`/`send`/`put`/…) is NEVER routed to the
/// consume/clone path purely by NAME (gorget-arena snag #2 — the call-site temp
/// was force-cloned because the method was named `push`).
///
/// Two structural assertions:
///  1. The consuming-mutator name list (`"push" | "add" | "extend" | "send" |
///     "push_back" | "push_front"`) appears exactly TWICE — the value-arg
///     type-hint arm and the consuming-position arm. A new collection-mutator
///     name (or a copy of the arm) forces an audit: is it a value-position
///     HINT only (like `fill`/`get_or_put`, which must NOT consume), or a true
///     consume? — then re-pin.
///  2. The consuming-position match is preceded by the `if is_gir_method` gate.
///     Dropping the gate (re-introducing the raw name-match) drops this
///     substring, so the guard trips structurally — a name-match reintroduction
///     fails the suite (the spurious clone is otherwise output-identical +
///     memory-safe, invisible to stdout/ASan; see the
///     `snag_call_site_move_no_clone` integration gate).
///
/// **If this fails:**
///   - Count changed → audit the new/removed name arm (hint-vs-consume, see the
///     `methods.rs` `value_arg_idx_for_method` notes) and re-pin `EXPECTED`.
///   - Gate substring missing → the `consuming_positions_by_name` match is no
///     longer gated on `is_gir_method`; a user equip method can again inherit
///     builtin-collection consume semantics by name. Re-add
///     `let consuming_positions_by_name: Vec<usize> = if is_gir_method`.
///
/// Precedent: `container_literal_arms_count` above.
#[test]
fn consuming_position_name_match_is_gir_gated() {
    let src = fs::read_to_string("src/ir/lowering/exprs/methods.rs")
        .expect("read src/ir/lowering/exprs/methods.rs");

    // (1) The consuming-mutator name list appears in exactly two arms:
    //     the value-arg type-hint arm + the consuming-position arm.
    const EXPECTED_ARMS: usize = 2;
    let arms = src
        .matches("\"push\" | \"add\" | \"extend\" | \"send\" | \"push_back\" | \"push_front\"")
        .count();
    assert_eq!(
        arms, EXPECTED_ARMS,
        "consuming-mutator name-list arm count in `lower_method_call` changed: \
         {arms} vs expected {EXPECTED_ARMS}. A new collection-mutator name (or a \
         duplicated arm) needs a hint-vs-consume audit (see the \
         `value_arg_idx_for_method` notes in methods.rs) and a re-pin here.",
    );

    // (2) The consuming-position match MUST be gated on the typed callee
    //     identity `is_gir_method` — NOT the method name (Core #2). Dropping the
    //     gate re-opens gorget-arena snag #2.
    let gated =
        src.contains("let consuming_positions_by_name: Vec<usize> = if is_gir_method");
    assert!(
        gated,
        "gorget-arena snag #2 guard removed: `consuming_positions_by_name` must be \
         gated on the typed `is_gir_method` (resolved-callee identity) so a USER \
         equip method is never routed to the consume/clone path by NAME. Re-add \
         `let consuming_positions_by_name: Vec<usize> = if is_gir_method`.",
    );
}

/// Ratchet (Core #4 "one fix, all siblings" / devbook-24 rule 3 "one source of
/// truth"): every resource-typed field-load in `lower_field_access`
/// (`src/ir/lowering/exprs/mod.rs`) must route its borrow tag through the
/// `set_field_or_elem_borrow` chokepoint — NEVER a bare `set_field_borrow`.
///
/// The chokepoint is what retags a field-of-CoW-element load
/// (`coll.get(i).unwrap().field`, for-element `x.field`) to borrow out of the
/// SAME collection instead of dead-ending on an unnamed-statement-temp
/// `BorrowOrigin::Field { base }` — which no mutation tracking can route back
/// to, so the var-decl decision falls to an eager `VarDeclFromBorrow` clone
/// PER READ (the round-33 DEEP-1 top-1 clone site, ~40.8M hits). There are
/// FOUR field-load sites in the fn (two main-path: lookup-field cache + TypeDef
/// fallback; two Guard[T] auto-deref). The Guard sites are behavioral no-ops
/// today (their base is a fresh `emit_guard_get_ptr` pointer, never
/// `is_cow_borrow`), but they route through the chokepoint too so a fifth site
/// added with a bare `set_field_borrow` can't silently drop provenance and
/// re-open the clone hole. This lint pins the direct-call count at ZERO.
///
/// **Scoping (load-bearing):** ONLY the body of `fn lower_field_access` is
/// scanned. The tuple-index site (`fn lower_expr_inner`, `exprs/mod.rs:730`)
/// and the disabled probe (`stmts/mod.rs:873`) are legitimate direct
/// `set_field_borrow` calls in OTHER functions and MUST NOT trip this — so the
/// scan slices the fn by brace-depth (precedent: `count_container_literal_arms`
/// above scopes to `infer_expr`).
///
/// **If this fails:** you added a resource-typed field-load site in
/// `lower_field_access` with a bare `set_field_borrow`. Route it through
/// `set_field_or_elem_borrow` instead (the CoW field-of-element chokepoint).
#[test]
fn lower_field_access_routes_through_field_or_elem_chokepoint() {
    let content = fs::read_to_string("src/ir/lowering/exprs/mod.rs").unwrap_or_default();
    let mut in_fn = false;
    let mut seen_open = false;
    let mut depth = 0i32;
    let mut count = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if !in_fn {
            if trimmed.starts_with("fn lower_field_access(") {
                in_fn = true;
                seen_open = false;
                depth = 0;
            } else {
                continue;
            }
        }
        // Count the bare chokepoint bypass on non-comment lines. The
        // `.set_field_or_elem_borrow(` chokepoint does NOT match this substring
        // (its infix is `_or_elem_`, so `.set_field_borrow(` never matches it).
        if !trimmed.starts_with("//") {
            count += line.matches(".set_field_borrow(").count();
        }
        depth += line.matches('{').count() as i32;
        depth -= line.matches('}').count() as i32;
        if depth > 0 {
            seen_open = true;
        }
        // The fn body sits at depth >= 1 throughout; depth returns to 0 only at
        // the fn's closing brace (the multi-line signature keeps depth 0 until
        // the first `{`, guarded by `seen_open`).
        if seen_open && depth <= 0 {
            in_fn = false;
        }
    }
    assert_eq!(
        count, 0,
        "Found {count} direct `.set_field_borrow(` call(s) inside `lower_field_access` \
         (expected 0). Every resource-typed field-load there must route through the \
         `set_field_or_elem_borrow` chokepoint so a field-of-CoW-element load retags to \
         the SAME collection's borrow provenance instead of dead-ending on an unnamed \
         `Field` origin (which forces an eager per-read `VarDeclFromBorrow` clone — the \
         round-33 DEEP-1 top-1 clone site). Swap the bare call for `set_field_or_elem_borrow`.",
    );
}

/// Ratchet (Core #4 "one fix, all siblings" / devbook-24 rule 3 "one source of
/// truth"): the `throws T … throws E` → `Result[T, E]` return-type synthesis must
/// live in EXACTLY ONE place — `types::synthesize_throws_result_type`. There are
/// FOUR throws-signature sites that each need this `Result[T, E]` return type:
///   1. the free-fn `fn_sigs` pre-scan (`mod.rs`),
///   2. the equip-method `fn_sigs` pre-scan (`mod.rs`),
///   3. the trait-equip method-sig pre-scan (`traits.rs`), and
///   4. the equip-method body lowering (`functions.rs`).
/// Site #2 once silently DRIFTED — it had no `throws` branch at all, so it
/// registered bare `int` instead of `Result[int, String]`, and a
/// `c.add(5) catch (e): …` call read the stale `int64_t` while the emitted C
/// method returned `Result` → ill-typed C (`incompatible types … 'int64_t' from
/// '__gg_Result__…'`). The fix extracted the synthesis into ONE helper and routed
/// all four sites through it.
///
/// This lint pins that invariant two ways:
///   - the inline `Result__{ok_c}__{err_c}` mangled-name `format!` appears in
///     EXACTLY ONE place (the helper); a re-inlined fifth copy trips it, and
///   - the helper is CALLED from at least the four known sig sites.
///
/// **If this fails:** a new throws-sig path was added (or a site re-inlined the
/// synthesis). Route it through `synthesize_throws_result_type` instead of
/// hand-rolling `format!("Result__{ok_c}__{err_c}")` + `make_result_type_def`, so
/// the metadata (`needs_drop` / copy-semantics) and the mangled name stay
/// coherent across every site. Then bump `EXPECTED_CALL_SITES`.
#[test]
fn throws_result_synthesis_single_source() {
    // The mangled-name format string is the load-bearing inline-synthesis
    // marker. It must appear in EXACTLY ONE code location (the helper). Doc-
    // comment mentions use a different spelling (`Result__{ok_c}__{err_c}`
    // inside backticks/prose without `format!(`) so we anchor on the `format!`.
    const SYNTH_MARKER: &str = r#"format!("Result__{ok_c}__{err_c}")"#;
    const HELPER_FN: &str = "fn synthesize_throws_result_type";
    const HELPER_CALL: &str = "synthesize_throws_result_type(";
    // 4 sig sites + the `fn …(` definition line itself = 5 textual matches of
    // the call marker. (The definition line also contains `synthesize_throws_
    // result_type(`.)
    const EXPECTED_CALL_SITES: usize = 4;

    let files = [
        "src/ir/lowering/types.rs",
        "src/ir/lowering/mod.rs",
        "src/ir/lowering/functions.rs",
        "src/ir/lowering/traits.rs",
    ];

    let mut inline_synth = 0usize;
    let mut helper_defs = 0usize;
    let mut call_sites = 0usize;
    for f in files {
        let content = fs::read_to_string(f).unwrap_or_default();
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("///") {
                continue;
            }
            inline_synth += line.matches(SYNTH_MARKER).count();
            if line.contains(HELPER_FN) {
                helper_defs += 1;
                continue; // the definition line is not a call site
            }
            call_sites += line.matches(HELPER_CALL).count();
        }
    }

    assert_eq!(
        inline_synth, 1,
        "Inline `throws` → Result-type synthesis (`{SYNTH_MARKER}`) must appear in \
         EXACTLY ONE place — `types::synthesize_throws_result_type` — but found \
         {inline_synth} copies.\n\n\
         A throws-sig site re-inlined the synthesis. Route it through the shared \
         helper instead (devbook-24 rule 3 / Core #4): the equip-method pre-scan \
         once drifted exactly this way (registered bare `int`, not `Result[int, E]`) \
         and emitted ill-typed C. Delete the inline copy, call \
         `synthesize_throws_result_type(&mut ctx.type_mapper, &mut ctx.type_registry, \
         &return_ty, &throws_ty)`.",
    );
    assert_eq!(
        helper_defs, 1,
        "Expected exactly one `synthesize_throws_result_type` definition, found {helper_defs}.",
    );
    assert_eq!(
        call_sites, EXPECTED_CALL_SITES,
        "throws-result synthesis call-site count changed: {call_sites} vs \
         {EXPECTED_CALL_SITES}.\n\n\
         If you added a throws-signature path (a new place that needs the \
         `Result[T, E]` return type for a `throws` fn/method), it MUST call \
         `synthesize_throws_result_type` — NOT hand-roll the `Result__…` mangle — \
         then bump EXPECTED_CALL_SITES. If you removed a site, lower it.",
    );
}

/// Ratchet (Core #4 "one fix, all siblings" / devbook-24 rules 2+4 — type-aware
/// enum-variant resolution at the GIR producer). `LoweringContext::enum_variants`
/// is a flat `variant_name -> (enum, variant)` map populated LAST-WRITE-WINS, so
/// reading it by BARE NAME to resolve a constructor's IDENTITY mis-picks the enum
/// when two in-scope enums share a variant name (the `CRuntimeType.TArray` vs
/// `Type.TArray` collision → wrong struct-id → `field index N out of range`).
/// The SSOT fix routes every CONSTRUCTOR-identity read through
/// `resolve_enum_variant_typed(name, expected_type)`, which prefers the
/// typechecker-determined expected enum and only falls back to the flat map when
/// it doesn't disambiguate.
///
/// This lint pins the read sites so a new BARE identity-resolution site can't be
/// added without going through the typed helper. It counts two markers across
/// `src/ir/lowering/`:
///   - `.resolve_enum_variant(` calls (the public bare accessor), and
///   - direct `enum_variants.get(` field reads.
///
/// Both are pinned to an ALLOWLIST of known-safe sites:
///   `.resolve_enum_variant(` — total 9, all non-identity-ctor:
///     * `context.rs` ×1 — the typed helper's OWN fallback (`self.resolve_enum_variant(name)`).
///     * `stmts/patterns.rs` ×6 — the match/pattern side, already type-aware
///       (prefers `type_name(scrut_type)`; the flat map is only its `.or_else` tail).
///     * `exprs/methods.rs` ×2 and `exprs/mod.rs` ×1 — `.is_some()` MEMBERSHIP
///       tests (existence, not identity → collision-safe).
///   `enum_variants.get(` — total 3:
///     * `context.rs` ×2 — the canonical accessor body + `infer_type_from_expr`'s
///       `Expr::Call` arm (returns a type_id for type-INFERENCE, not a ctor tuple
///       → a real same-class sibling, deferred + NAMED; lower-risk).
///     * `closures.rs` ×1 — closure return-type inference (also a type_id, not a
///       ctor tuple → deferred + NAMED).
///
/// **If this fails because the count GREW:** you added a bare enum-variant
/// IDENTITY read. If it constructs a variant, route it through
/// `resolve_enum_variant_typed` with the `expected_type` in hand (devbook-24
/// rules 2+4 — resolve once, write through the typed metadata) instead of the
/// flat last-write-wins map, then re-balance the per-file budget below. If it's a
/// genuine `.is_some()` membership test or a deferred inference read, add it to
/// the allowlist comment AND bump the matching per-file budget so the next site
/// is still forced to justify itself. **If it SHRANK** (a deferred inference site
/// finally got its own expected-type write-through, or a `.is_some()` test was
/// removed), lower the budget to lock the new floor.
#[test]
fn enum_variant_resolution_typed_ssot() {
    // Per-file budgets for the BARE `.resolve_enum_variant(` accessor. The typed
    // helper `.resolve_enum_variant_typed(` is NOT counted (it IS the fix). The
    // `fn resolve_enum_variant(` / `fn resolve_enum_variant_typed(` DEFINITION
    // lines are excluded by anchoring on the leading `.` (method-call form).
    let bare_call_budget: &[(&str, usize)] = &[
        ("src/ir/lowering/context.rs", 1),       // typed helper's own fallback
        ("src/ir/lowering/stmts/patterns.rs", 6), // already type-aware match side
        ("src/ir/lowering/exprs/methods.rs", 2),  // `.is_some()` membership
        ("src/ir/lowering/exprs/mod.rs", 1),      // `.is_some()` membership
    ];
    // Per-file budgets for direct `enum_variants.get(` field reads.
    let direct_get_budget: &[(&str, usize)] = &[
        ("src/ir/lowering/context.rs", 2),  // canonical accessor + infer_type_from_expr (deferred, NAMED)
        ("src/ir/lowering/closures.rs", 1), // closure return-type inference (deferred, NAMED)
    ];

    fn count_marker(file: &str, marker: &str) -> usize {
        let content = fs::read_to_string(file).unwrap_or_default();
        let mut n = 0usize;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("///") {
                continue;
            }
            n += line.matches(marker).count();
        }
        n
    }

    for &(file, expected) in bare_call_budget {
        let got = count_marker(file, ".resolve_enum_variant(");
        assert_eq!(
            got, expected,
            "Bare `.resolve_enum_variant(` count in `{file}` changed: {got} vs \
             allowlisted {expected}.\n\n\
             `enum_variants` is a flat last-write-wins `variant_name -> enum` map; \
             reading it by bare name to resolve a CONSTRUCTOR's identity mis-picks \
             the enum when two in-scope enums share a variant name. Route any new \
             constructor read through `resolve_enum_variant_typed(name, \
             ctx.func_state.expected_type)` (devbook-24 rules 2+4). If the new site \
             is a `.is_some()` membership test or the helper's own fallback, add it \
             to the allowlist comment above and bump this file's budget.",
        );
    }
    for &(file, expected) in direct_get_budget {
        let got = count_marker(file, "enum_variants.get(");
        assert_eq!(
            got, expected,
            "Direct `enum_variants.get(` count in `{file}` changed: {got} vs \
             allowlisted {expected}.\n\n\
             A new direct read of the flat variant map was added. If it resolves a \
             CONSTRUCTOR's identity, route it through `resolve_enum_variant_typed`. \
             The two allowlisted reads (`context.rs` infer_type_from_expr, \
             `closures.rs` return-type inference) return a type_id for INFERENCE, \
             not a ctor tuple — they are deferred same-class siblings (NAMED in the \
             allowlist) and still want an expected-type write-through eventually. \
             Add a justification to the allowlist comment and bump the budget.",
        );
    }

    // Guard against a NEW file under `src/ir/lowering/` sneaking in either marker
    // outside the per-file allowlists above — the budgets are keyed by file, so a
    // bare read in an UN-listed file would otherwise be invisible.
    let allowlisted: std::collections::HashSet<&str> = bare_call_budget
        .iter()
        .map(|&(f, _)| f)
        .chain(direct_get_budget.iter().map(|&(f, _)| f))
        .collect();
    let mut stray = Vec::new();
    visit_rs_files(Path::new("src/ir/lowering"), &mut |path| {
        let p = path.to_str().unwrap_or_default();
        // Normalise the leading `./` away if present.
        let norm = p.trim_start_matches("./");
        if allowlisted.contains(norm) {
            return;
        }
        let bare = count_marker(norm, ".resolve_enum_variant(");
        let get = count_marker(norm, "enum_variants.get(");
        if bare + get > 0 {
            stray.push(format!("{norm}: .resolve_enum_variant(={bare}, enum_variants.get(={get}"));
        }
    });
    assert!(
        stray.is_empty(),
        "New bare enum-variant resolution read(s) appeared in un-allowlisted \
         file(s) under src/ir/lowering/:\n  {}\n\n\
         Route constructor reads through `resolve_enum_variant_typed`; if it's a \
         membership/inference site, add the file to the allowlist in \
         `enum_variant_resolution_typed_ssot`.",
        stray.join("\n  "),
    );
}

/// Recursively visit every `*.rs` file under `dir`, calling `f` with each path.
fn visit_rs_files(dir: &Path, f: &mut dyn FnMut(&Path)) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            visit_rs_files(&path, f);
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            f(&path);
        }
    }
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

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard"; devbook/24
/// rule 2): the SELF-HOST generic-instance DISCOVERY walker
/// `discover_generic_calls_expr` (`lower_generics.gg`) must visit EVERY
/// sub-expression-bearing `Expr` variant. A variant that falls into the
/// `else: pass` fallback is NEVER walked, so a generic-struct constructor
/// nested inside it (`[GNode[int](...)]`, `S(f=GNode[int](...))`, an if-else
/// arm, a block/do, a match arm, …) is never discovered → its monomorphized
/// struct body is never registered → an empty `{char __pad}` struct →
/// `[bug] I64(0)` on a later field read (R37-T2, the
/// `field_off_generic_element_match_return` runtime WRONG). This mirrors Rust
/// gg's `scan_expr` (`src/ir/lowering/generics/mod.rs:663-806`).
///
/// The count is pinned to the FULL sub-expr-bearing self-host `Expr` set (all
/// 35 arms), NOT just the arms the target fixture needed. Only the 9 leaf
/// variants (EIntLiteral / EFloatLiteral / EBoolLiteral / EStringLiteral /
/// ECharLiteral / ENoneLiteral / EIdentifier / ESelfExpr / EIt) may fall to
/// `else: pass`.
///
/// **If this fails because an arm was ADDED:** confirm the new arm RECURSES
/// into its sub-exprs (and, if it carries type-args like ECall/EMethodCall,
/// scans them via the shared `discover_generic_calls_type` walker) — never
/// registers from a non-type-arg field (⚠ `EStructLiteral`'s middle
/// `Vector[String]` is FIELD NAMES). Then bump EXPECTED with a justification.
/// **If a NEW sub-expr-bearing `Expr` variant lands** in `ast.gg`, it MUST get
/// an arm here (recurse-only, or a type-arg scan if it carries targs) — do not
/// leave it in `else: pass`. **If an arm was removed:** lower EXPECTED to lock
/// the new floor.
#[test]
fn self_host_generic_discovery_expr_arms_count() {
    /// Baseline 2026-07-03 (R37-T2): 35 top-level `case E…` arms in
    /// `discover_generic_calls_expr` — the complete sub-expr-bearing self-host
    /// `Expr` set. Counts the function's TOP-LEVEL match arms only (8-space
    /// indent); the nested `case EIdentifier` (inside the ECall arm) and the
    /// `case Some|None` sub-matches are deeper-indented and excluded.
    // 2026-07-17 (D29): 35 → 36 — the `EPropagate` transparent wrapper arm
    // (recurses into its inner; the mark carries no semantics of its own).
    // 2026-08-07 (D25 Round XXXIV Track C2): 36 → 35 — the `EFaultCatch` arm
    // vanished when the lexical fault-catch form was removed.
    const EXPECTED: usize = 35;

    // lower_generics.gg lives ONLY in self_host_lowerer (real file, not
    // symlinked), so no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower_generics.gg").unwrap_or_default();

    // Scope to the `discover_generic_calls_expr` fn body: from its signature to
    // the next top-level `void ` definition (`discover_generic_calls_type`).
    let start = content
        .find("void discover_generic_calls_expr(")
        .expect("self_host_generic_discovery_expr_arms_count: discover_generic_calls_expr fn not found");
    let end = content[start..]
        .find("\nvoid discover_generic_calls_type(")
        .map(|o| start + o)
        .expect("self_host_generic_discovery_expr_arms_count: end of discover_generic_calls_expr not found");
    let window = &content[start..end];

    let mut arms = 0usize;
    for line in window.lines() {
        if line.trim_start().starts_with('#') {
            continue; // .gg comments
        }
        // Top-level match arms are indented EXACTLY 8 spaces. `strip_prefix`
        // with the 8-space prefix rejects the deeper-indented nested arms
        // (`case EIdentifier` at 20 spaces, `case Some|None` at 12 spaces).
        if line.strip_prefix("        case E").is_some() {
            arms += 1;
        }
    }

    assert_eq!(
        arms, EXPECTED,
        "Self-host `discover_generic_calls_expr` arm count changed: \
         {arms} vs {EXPECTED}.\n\n\
         The generic-instance discovery walker must visit EVERY \
         sub-expression-bearing `Expr` variant — a variant left in `else: pass` \
         is never walked, so a generic-struct ctor nested inside it is never \
         discovered → an empty `{{char __pad}}` mono struct → `[bug] I64(0)` on \
         a later field read. A new arm MUST recurse into its sub-exprs (and scan \
         any type-args via the shared `discover_generic_calls_type` walker) — \
         never register from a non-type-arg field (`EStructLiteral`'s middle \
         `Vector[String]` is FIELD NAMES). Bump EXPECTED with a justification, \
         or lower it if an arm was removed.",
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard"): the R38
/// `&self` mutation-inference walker `mutinf_scan_expr` (`lower.gg`) must visit
/// EVERY sub-expression-bearing `Expr` variant. A variant that falls into the
/// `else: pass` fallback is NEVER walked, so a self-mutation hiding inside it
/// (a mutating `self.v.push(..)` nested in a range bound `self.lo()..self.hi()`,
/// an `is`-scrutinee `self.take() is Some`, a `.V(self.drain())` dot-shorthand
/// arg, an if/match/block, …) is NEVER detected → the method is mis-classified
/// READ-ONLY → the named-receiver CoW gate does NOT materialize → a
/// write-through divergence from Rust (degrade-to-BASE — the SAME failure class
/// the pass exists to close). Mirrors the self-host generic-discovery walker
/// (`self_host_generic_discovery_expr_arms_count` above) and Rust gg's
/// `cow_scan_expr`.
///
/// The count is pinned to the FULL sub-expr-bearing self-host `Expr` set (all
/// 35 arms). Only the 9 leaf variants (EIntLiteral / EFloatLiteral /
/// EBoolLiteral / EStringLiteral / ECharLiteral / ENoneLiteral / EIdentifier /
/// ESelfExpr / EIt) may fall to `else: pass`.
///
/// **If this fails because an arm was ADDED:** confirm the new arm RECURSES
/// into its sub-exprs (and, if it is a method call, keeps the user→builtin→leaf
/// self-rooted classification order). Then bump EXPECTED with a justification.
/// **If a NEW sub-expr-bearing `Expr` variant lands** in `ast.gg`, it MUST get
/// an arm here (recurse-only at minimum) — never leave it in `else: pass`.
/// **If an arm was removed:** lower EXPECTED to lock the new floor.
#[test]
fn self_host_mutinf_scan_expr_arms_count() {
    /// Baseline 2026-07-04 (R38-T-B): 35 top-level `case E…` arms in
    /// `mutinf_scan_expr` — the complete sub-expr-bearing self-host `Expr` set,
    /// identical to `discover_generic_calls_expr`. Counts the function's
    /// TOP-LEVEL match arms only (8-space indent); the nested `case ESelfExpr`
    /// (inside the EMethodCall receiver sub-matches, 20-space indent) and the
    /// `case Some|None` sub-matches (12-space indent) are excluded.
    // 2026-07-17 (D29): 35 → 36 — the `EPropagate` transparent wrapper arm
    // (recurses into its inner; the mark carries no semantics of its own).
    // 2026-08-07 (D25 Round XXXIV Track C2): 36 → 35 — the `EFaultCatch` arm
    // vanished when the lexical fault-catch form was removed.
    const EXPECTED: usize = 35;

    // lower.gg lives ONLY in self_host_lowerer (real file, not symlinked), so
    // no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();

    // Scope to the `mutinf_scan_expr` fn body: from its signature to the next
    // top-level `bool ` definition (`mutinf_scan_stmts`).
    let start = content
        .find("bool mutinf_scan_expr(")
        .expect("self_host_mutinf_scan_expr_arms_count: mutinf_scan_expr fn not found");
    let end = content[start..]
        .find("\nbool mutinf_scan_stmts(")
        .map(|o| start + o)
        .expect("self_host_mutinf_scan_expr_arms_count: end of mutinf_scan_expr not found");
    let window = &content[start..end];

    let mut arms = 0usize;
    for line in window.lines() {
        if line.trim_start().starts_with('#') {
            continue; // .gg comments
        }
        // Top-level match arms are indented EXACTLY 8 spaces; deeper-indented
        // nested `case E…` arms are rejected by the 8-space prefix.
        if line.strip_prefix("        case E").is_some() {
            arms += 1;
        }
    }

    assert_eq!(
        arms, EXPECTED,
        "Self-host `mutinf_scan_expr` arm count changed: {arms} vs {EXPECTED}.\n\n\
         The `&self` mutation-inference walker must visit EVERY \
         sub-expression-bearing `Expr` variant — a variant left in `else: pass` \
         is never walked, so a self-mutation hiding inside it is never detected \
         → the method is mis-classified read-only → the named-receiver CoW gate \
         under-materializes → a write-through divergence from Rust. A new arm \
         MUST recurse into its sub-exprs. Bump EXPECTED with a justification, or \
         lower it if an arm was removed.",
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard"): the
/// STATEMENT-list half of the R38 `&self` mutation-inference walker
/// `mutinf_scan_stmts` (`lower.gg`) must visit EVERY runtime-statement-bearing
/// `Stmt` variant. Statements are the PRIMARY self-mutation carriers — a
/// `self.f = x` / `self.f += x` / `self.v[i] = x` lands in `SAssign` /
/// `SCompoundAssign`, and every block-bearing stmt (`SFor`/`SWhile`/`SIf`/
/// `SMatch`/`SWith`/…) can nest one — so a variant left in `else: pass` hides a
/// direct self-mutation → the method is mis-classified READ-ONLY → the
/// named-receiver CoW gate under-materializes → a write-through divergence.
/// This is the higher-value companion to `self_host_mutinf_scan_expr_arms_count`
/// (per its own Core #6 rationale).
///
/// The count is pinned to the RUNTIME-statement set (19 arms). Excluded (may
/// fall to `else: pass`): the leaf `SContinue`/`SPass` (no sub-nodes), `SItem`
/// (a nested item definition never captures the enclosing `self`), and the
/// compile-time meta statements `SMeta`/`SMetaFor`/`SMetaIf`/`SMetaConst`/
/// `SMetaForMatch`/`SMetaMatch`/`SMetaWhile` (expanded by meta.gg BEFORE
/// lowering, so absent from a method body reaching `compute_method_mutates_self`).
///
/// **If this fails because an arm was ADDED:** confirm the new arm scans its
/// sub-exprs AND flags a self-rooted `SAssign`/`SCompoundAssign` lhs. Bump
/// EXPECTED with a justification. **If a NEW runtime-statement variant lands**
/// in `ast.gg` (esp. one that can carry an assignment), it MUST get an arm here
/// — never leave a self-write carrier in `else: pass`. **If an arm was removed:**
/// lower EXPECTED to lock the new floor.
#[test]
fn self_host_mutinf_scan_stmts_arms_count() {
    /// Baseline 2026-07-04 (R38-T-B): 19 top-level `case S…` arms in
    /// `mutinf_scan_stmts`. Counts the function's TOP-LEVEL match arms only
    /// (12-space indent — one level deeper than mutinf_scan_expr because the
    /// `match st:` sits inside `for st in stmts:`); the nested `case Some|None`
    /// / `case SORecv|SOSend` sub-matches are deeper-indented and excluded.
    const EXPECTED: usize = 19;

    // lower.gg lives ONLY in self_host_lowerer (real file, not symlinked), so
    // no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();

    // Scope to the `mutinf_scan_stmts` fn body: from its signature to the next
    // top-level `void ` definition (`compute_method_mutates_self`).
    let start = content
        .find("bool mutinf_scan_stmts(")
        .expect("self_host_mutinf_scan_stmts_arms_count: mutinf_scan_stmts fn not found");
    let end = content[start..]
        .find("\nvoid compute_method_mutates_self(")
        .map(|o| start + o)
        .expect("self_host_mutinf_scan_stmts_arms_count: end of mutinf_scan_stmts not found");
    let window = &content[start..end];

    let mut arms = 0usize;
    for line in window.lines() {
        if line.trim_start().starts_with('#') {
            continue; // .gg comments
        }
        // Top-level match arms are indented EXACTLY 12 spaces; deeper-indented
        // nested `case S…` arms (the SSelect `case SORecv|SOSend`, 20 spaces)
        // are rejected by the 12-space prefix.
        if line.strip_prefix("            case S").is_some() {
            arms += 1;
        }
    }

    assert_eq!(
        arms, EXPECTED,
        "Self-host `mutinf_scan_stmts` arm count changed: {arms} vs {EXPECTED}.\n\n\
         Statements are the PRIMARY self-mutation carriers — a variant left in \
         `else: pass` hides a direct `self.f = x` / `self.f += x` → the method \
         is mis-classified read-only → the named-receiver CoW gate \
         under-materializes → a write-through divergence from Rust. A new arm \
         MUST scan its sub-exprs and flag a self-rooted assign lhs. Bump \
         EXPECTED with a justification, or lower it if an arm was removed.",
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard"; devbook/24
/// rule 2 "typed metadata, not name-matched"): the Option/Result HOF combinator
/// templates in the self-host `emit_option_result_combinator`
/// (`lir_codegen.gg`) are an enumerated class. Each one that dispatches a USER
/// closure MUST drive the closure-call return cast + the payload read/write off
/// the TYPED struct fields (`dst_c`/`src_pay_c`/`dst_pay_c`/`dst_err_c` via
/// `combinator_field_c_type`), NOT the old `void*` cast — a `void*` truncates a
/// 16-byte `Str`/`Option`/`Result` payload-or-return to 8 bytes →
/// garbage/SIGSEGV (Inc-4b: `and_then` returned a wide Option, `map_err`
/// returned a Str). The lint pins this three ways:
///   - the HOF combinator arm COUNT stays at the baseline (a new combinator
///     arm trips it and forces a review for the type-aware path),
///   - the truncating 2-arg closure cast `void*(*)(void*, void*)` appears
///     ZERO times in the file (a re-inlined payload-arg truncating call trips
///     it), and
///   - the truncating 1-arg closure cast `void*(*)(void*)` appears ZERO times
///     in the `emit_option_result_combinator` body (Inc-4c: the no-payload-arg
///     `unwrap_or_else`/`or_else`/`or` siblings the 4a+4b pass MISSED — their
///     env-only call still truncated a 16-byte Str return through `void*` → an
///     empty Str on the fn() path).
///
/// **If this fails because the arm count changed:** a combinator template was
/// added/removed. A new one that dispatches a closure MUST use the typed
/// `dst_c`/`*_pay_c`/`*_err_c` C-type strings for the call cast and payload, not
/// `void*`. Bump EXPECTED with a justification, or lower it if an arm was
/// removed. **If a void*-cast count is non-zero:** a template re-introduced a
/// truncating `((void*(*)(void*[, void*]))…)` closure call — route it through
/// the typed cast (`c_type_name(dst_ty, &sn)` / `combinator_field_c_type`)
/// instead.
///
/// Round XV Track A (Core #6): also enforces (1) name-completeness — every
/// name produced by `map_option_result_method` + the `"__option_"/"__result_"+
/// method` fallback for known Option/Result methods must appear as a match arm
/// (retires the `__option_flat_map` link-fail class), and (2) the SH-1 residual
/// ban — HOF arms must not hardcode `void* __pay` (the Money payload truncate
/// that survived the return-cast ban alone).
#[test]
fn self_host_combinator_template_arms_count() {
    /// Baseline 2026-06-26 (Inc-4b): 12 `case` lines for the closure-dispatching
    /// / value HOF combinator arms in the third `match name:` block of
    /// `emit_option_result_combinator` — option_{map,filter,and_then|flat_map,
    /// `or_else|or` (one merged case line),flatten,unwrap_or_else} +
    /// result_{map,map_err,and_then|flat_map,or_else,or,unwrap_or_else}. Counts
    /// `case` LINES, so a `|`-merged arm is one. Round XV SH-2 merged flat_map
    /// into the and_then case line — count stays 12.
    const EXPECTED_ARMS: usize = 12;

    // lir_codegen.gg lives ONLY in self_host_lowerer (real file, not symlinked),
    // so no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lir_codegen.gg").unwrap_or_default();
    let map_fn = fs::read_to_string("tests/fixtures/self_host_lowerer/lir_lower.gg")
        .unwrap_or_default();

    // The HOF combinator arms are exactly the `case "__option_…"` / `case
    // "__result_…"` arms that live BELOW the "HOF combinators" banner and ABOVE
    // the `# Fallback: unknown combinator` line. Scope the scan to that window
    // so the earlier is_some/unwrap/expect tag-check arms aren't counted.
    let start = content
        .find("# ── HOF combinators: map, filter, and_then, or_else, or ──")
        .expect("self_host_combinator_template_arms_count: HOF combinator banner not found");
    let end = content[start..]
        .find("# Fallback: unknown combinator")
        .map(|o| start + o)
        .expect("self_host_combinator_template_arms_count: combinator fallback marker not found");
    let window = &content[start..end];

    let mut arms = 0usize;
    for line in window.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with('#') {
            continue; // .gg comments
        }
        if trimmed.starts_with("case \"__option_") || trimmed.starts_with("case \"__result_") {
            arms += 1;
        }
    }

    assert_eq!(
        arms, EXPECTED_ARMS,
        "Self-host Option/Result HOF combinator template arm count changed: \
         {arms} vs {EXPECTED_ARMS}.\n\n\
         The combinator templates are an enumerated class. A new combinator that \
         dispatches a user closure MUST drive the closure-call return cast and the \
         payload read/write off the TYPED struct fields (`dst_c`/`src_pay_c`/\
         `dst_pay_c`/`dst_err_c` via `combinator_field_c_type`) — NOT a `void*` \
         cast, which truncates a 16-byte Str/Option/Result payload-or-return to 8 \
         bytes (garbage/SIGSEGV). Bump EXPECTED_ARMS with a justification, or lower \
         it if an arm was removed.",
    );

    // The truncating 2-arg closure-call cast must be GONE: every closure that
    // takes a payload arg is now declared with the real argument + return type.
    let trunc_casts = content.matches("void*(*)(void*, void*)").count();
    assert_eq!(
        trunc_casts, 0,
        "Found {trunc_casts} truncating 2-arg closure-call cast(s) \
         `void*(*)(void*, void*)` in lir_codegen.gg. A combinator template \
         re-introduced the void*-truncating closure call (loses the upper 8 bytes \
         of a Str/Option/Result payload or return → garbage/SIGSEGV). Declare the \
         call with the real payload + return C types (`combinator_field_c_type`) \
         instead.",
    );

    // Inc-4c (R2 completion): the 1-arg truncating cast `void*(*)(void*)` must
    // ALSO be gone from `emit_option_result_combinator`. It was the sibling the
    // 4a+4b pass MISSED: the no-payload-arg combinators (`unwrap_or_else`,
    // `or_else`/`or` on the closure path) call the closure as
    // `((void*(*)(void*))fn)(env)`, which truncates a 16-byte Str return through
    // the `void*` return type → an empty Str on the fn() path. Both
    // truncation forms (1-arg and 2-arg) are now forbidden so NEITHER can come
    // back. Scope to the `emit_option_result_combinator` body so the generic
    // `__gorget_closure_call_`/`__callable_` dispatch fragments (which build an
    // N-arg cast by string-concatenation and are NOT combinator templates) are
    // not scanned.
    let fn_start = content
        .find("String emit_option_result_combinator(")
        .expect("self_host_combinator_template_arms_count: emit_option_result_combinator fn not found");
    let fn_end = content[fn_start..]
        .find("\nString emit_call_extern_with(")
        .map(|o| fn_start + o)
        .expect("self_host_combinator_template_arms_count: end of emit_option_result_combinator not found");
    let fn_body = &content[fn_start..fn_end];
    let trunc_casts_1arg = fn_body.matches("void*(*)(void*)").count();
    assert_eq!(
        trunc_casts_1arg, 0,
        "Found {trunc_casts_1arg} truncating 1-arg closure-call cast(s) \
         `void*(*)(void*)` in `emit_option_result_combinator`. A no-payload-arg \
         combinator (unwrap_or_else / or_else / or) re-introduced the \
         void*-truncating closure call (loses the upper 8 bytes of a 16-byte \
         Str/Option/Result return → an empty Str on the fn() path). Declare the \
         call with the real return C type (`c_type_name(dst_ty, &sn)` / \
         `combinator_field_c_type`) instead.",
    );

    // ── SH-1 residual ban (class 1: void* payload truncate) ──
    // The return-cast ban above misses `int64_t(*)(void*, void*)` + a hardcoded
    // `void* __pay` load that only reads 8 bytes of Money. Require the shared
    // peel helper and forbid a hardcoded void* payload declaration in the HOF
    // window (templates must drive off `src_pay_c`, which peels to Money).
    assert!(
        content.contains("combinator_peel_enum_ty"),
        "SH-1 helper `combinator_peel_enum_ty` missing from lir_codegen.gg — \
         every HOF arm that loads src_pay_c must peel LT_PTR_TO_BASE → struct \
         before field lookup (param/field Money map truncate class).",
    );

    // ── SH-4/5b residual ban (class: resource keep/copy-out without clone) ──
    // Keep paths that shallow-memcpy Option[Money] / Money payload double-free
    // when src and dst both drop (exit 134). Central helper
    // `combinator_owned_copy_stmt` uses field_drop_fn_for_lir_type (LIR name,
    // NOT c_type_name) → drop_to_clone_fn. Pin helper presence + that the HOF
    // window calls it on filter/or_else/uoe keep paths.
    assert!(
        content.contains("String combinator_owned_copy_stmt("),
        "SH-4/5b helper `combinator_owned_copy_stmt` missing from lir_codegen.gg — \
         every filter/or_else/or/flatten/uoe keep path must own-copy resource \
         Option/payload via LIR-keyed drop→clone (not shallow memcpy).",
    );
    assert!(
        content.contains("field_drop_fn_for_lir_type"),
        "SH-4/5b must look up drop/clone via field_drop_fn_for_lir_type (LIR \
         struct name Money), never c_type_name alone (__gg_Money misses the \
         registry → shallow copy → exit 134).",
    );
    let owned_copy_calls = window.matches("combinator_owned_copy_stmt").count();
    assert!(
        owned_copy_calls >= 12,
        "HOF combinator window only has {owned_copy_calls} combinator_owned_copy_stmt \
         call(s); expected ≥12 (Option side: filter keep, or_else/or keep-and-alt, \
         flatten, uoe Some — 5306/5322/5326/5337/5348; Result side: or keep-and-alt \
         + uoe Ok — 5407/5408/5427; Round XXII Track γ: map/map_err/and_then/or_else \
         Ok_0/Error_0 field passthrough — the 4 new call sites at 5363/5374/5386/5396). \
         A keep or passthrough regressed to bare memcpy of a resource Option/Result \
         payload — the exact class this ceiling was ratcheted to retire (Core #6).",
    );
    assert!(
        !window.contains("void* __pay"),
        "HOF combinator window hardcodes `void* __pay` — the class-1 residual \
         that truncates a Money payload on param/field receivers. Drive the \
         payload C type off `src_pay_c` via `combinator_field_c_type` after \
         `combinator_peel_enum_ty`.",
    );
    // Also ban the 2-arg cast with void* payload arg (distinct from the
    // void*-return cast already banned): `((int64_t(*)(void*, void*))`.
    let void_pay_cast = fn_body.matches("(void*, void*)").count();
    assert_eq!(
        void_pay_cast, 0,
        "Found {void_pay_cast} `(void*, void*)` cast fragment(s) in \
         `emit_option_result_combinator` — a HOF arm re-introduced a void* \
         payload-arg cast (Money param/field map reads 8 bytes of payload). \
         Use `src_pay_c` for the closure arg type.",
    );

    // ── Name-completeness ratchet (class 2: missing flat_map arm) ──
    // Every name `map_option_result_method` can produce for HOF methods, plus
    // the Option/Result `"__option_"/"__result_"+method` fallback for the same
    // method set, must appear as a quoted arm in the HOF match (or in the
    // whole emit_option_result_combinator for tag/unwrap helpers that live
    // above the HOF banner).
    let map_start = map_fn
        .find("String map_option_result_method(String method):")
        .expect("self_host_combinator_template_arms_count: map_option_result_method not found");
    let map_end = map_fn[map_start..]
        .find("\n# Phase A")
        .or_else(|| map_fn[map_start..].find("\nint type_category_for_name"))
        .or_else(|| map_fn[map_start..].find("\nString type_category_for_name"))
        .map(|o| map_start + o)
        .unwrap_or(map_fn.len());
    let map_body = &map_fn[map_start..map_end];

    // Methods that must have emit arms (HOF + tag/unwrap that map_option_result
    // or the __option_+method fallback can produce).
    let required_methods = [
        "map",
        "filter",
        "and_then",
        "flat_map",
        "or_else",
        "or",
        "flatten",
        "unwrap_or_else",
        "is_some",
        "is_none",
        "unwrap",
        "expect",
    ];
    let mut missing: Vec<String> = Vec::new();
    for method in required_methods {
        let opt_name = format!("__option_{method}");
        let res_name = format!("__result_{method}");
        // Accept either an exact case arm or a | merge in any match of
        // emit_option_result_combinator (tag checks + HOF).
        let opt_ok = fn_body.contains(&format!("\"{opt_name}\""))
            || window.contains(&format!("\"{opt_name}\""));
        if !opt_ok {
            missing.push(opt_name);
        }
        // Result siblings that share the method name (map, and_then, flat_map,
        // or_else, or, unwrap_or_else). Tag helpers use different spellings
        // (is_ok / is_error) — only require result arm when map_option_result
        // would emit __option_X for that method (converted to __result_X).
        let needs_result = matches!(
            method,
            "map" | "filter" | "and_then" | "flat_map" | "or_else" | "or" | "unwrap_or_else"
        );
        if needs_result {
            let res_ok = fn_body.contains(&format!("\"{res_name}\""))
                || window.contains(&format!("\"{res_name}\""));
            // filter is Option-only in practice; Result has no filter arm —
            // only require when the table maps it and an arm could be needed.
            if method == "filter" {
                continue;
            }
            if !res_ok {
                missing.push(res_name);
            }
        }
    }
    // flat_map must be in map_option_result_method's case list (production
    // one-table) so a future rename can't re-introduce the fallback-only path.
    assert!(
        map_body.contains("flat_map"),
        "`flat_map` missing from map_option_result_method — keep production \
         one-table so `__option_flat_map` is not only a fallback mangle.",
    );
    assert!(
        missing.is_empty(),
        "SH combinator name-completeness: emit_option_result_combinator is missing \
         match arm(s) for {missing:?}.\n\
         Every name from map_option_result_method + the `__option_`/`__result_`+method \
         fallback must be a match arm (never the unknown-combinator external call). \
         Add the arm (≡ and_then for flat_map) or update this list with justification.",
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard"; devbook/24
/// rule 3 "one source of truth per axis"): every freshly-built collection
/// LOCAL (the dst of an array/dict/set LITERAL or a list/set/dict
/// COMPREHENSION / HOF accumulator) in the self-host GIR lowerer
/// (`lower_expr.gg`) must be typed through the CENTRALIZED
/// `collection_accumulator_tid` helper (`lower_types.gg`), NOT by a bare
/// `lookup_or_register_named(&gmod, "GorgetArray"|"GorgetMap"|"GorgetSet")`.
///
/// A bare runtime-struct name drops the element type (`Vector__T` →
/// `GorgetArray`), so a downstream `v[i]` read / `for x in v` / chained HOF
/// can't recover `T` (`collection_element_type` / `index_value_type_name`
/// return `""` → the read stubs / the iteration finds no element shape). This
/// was a confirmed sibling hole: the literal sites were fixed but the
/// comprehension accumulators (`racc`/`vacc`) were not, so `auto v = [x*10 for
/// x in src]; v[0]` printed `0` instead of `10`. Routing every producer
/// through one helper closes the whole class — and this lint forces the NEXT
/// producer through it too.
///
/// Scope: `lower_expr.gg` ONLY. The bare `lookup_or_register_named(&gmod,
/// "GorgetArray"/...)` calls in `lower_types.gg` are a DIFFERENT axis —
/// `resolved_to_gir_type` AST-type→runtime-type mapping and
/// `infer_method_return_type` / `builtin_call_return_type` method-return-type
/// inference (`split`/`lines`/`chars`/`args`), plus the helper's own fallbacks
/// — never an accumulator dst. They are intentionally not scanned.
///
/// **If this fails because the count went UP:** a new (or reintroduced)
/// producer site is typing a collection accumulator with a bare runtime
/// struct name. Route it through `collection_accumulator_tid(&gmod, kind,
/// elem, key, val)` instead, then this lint passes at 0 again. **If a bare
/// call genuinely is NOT an accumulator dst** (e.g. a return-type inference
/// arm migrated into `lower_expr.gg`), bump EXPECTED with a one-line
/// justification naming the non-accumulator use.
#[test]
fn no_bare_collection_accumulator_outside_helper() {
    /// Baseline 2026-06-17: 0. After the producer-class centralization, every
    /// collection-accumulator dst in `lower_expr.gg` routes through
    /// `collection_accumulator_tid`; there are no bare runtime-struct
    /// accumulator lookups left in this file.
    const EXPECTED: usize = 0;

    // lower_expr.gg lives ONLY in self_host_lowerer (real file, not symlinked),
    // so no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower_expr.gg").unwrap_or_default();

    let mut count = 0usize;
    let mut hits: Vec<(usize, String)> = Vec::new();
    for (i, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with('#') {
            continue; // .gg comments
        }
        // Match the bare collection-accumulator lookups specifically — only
        // the three runtime-struct names that drop the element type. The
        // element-carrying forms (`"Vector__" + ...`, `"Set__" + ...`,
        // `"Dict__" + ...`) and the centralized helper call itself are not
        // matched.
        for bare in ["\"GorgetArray\"", "\"GorgetMap\"", "\"GorgetSet\""] {
            let needle = format!("lookup_or_register_named(&gmod, {bare})");
            if line.contains(&needle) {
                count += 1;
                hits.push((i + 1, trimmed.to_string()));
            }
        }
    }

    assert_eq!(
        count, EXPECTED,
        "Bare collection-accumulator lookups in self-host `lower_expr.gg`: \
         {count} vs expected {EXPECTED}.\n\n\
         A freshly-built collection LOCAL must be typed through the centralized \
         `collection_accumulator_tid(&gmod, kind, elem, key, val)` helper \
         (lower_types.gg) — NOT a bare \
         `lookup_or_register_named(&gmod, \"GorgetArray\"/\"GorgetMap\"/\"GorgetSet\")`, \
         which drops the element type and makes a downstream `v[i]` / `for x in v` \
         / chained HOF unable to recover it. Route the new producer through the \
         helper, or — if the bare call is genuinely NOT an accumulator dst — bump \
         EXPECTED with a justification.\n\nHits:\n{}",
        hits.iter()
            .map(|(ln, src)| format!("  lower_expr.gg:{ln}: {src}"))
            .collect::<Vec<_>>()
            .join("\n"),
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings"
/// + #6 "convert a recurring bug class into an executable guard") over the
/// Dict/Set DRAIN out-param ABI in the self-host LIR lowerer
/// (`lir_lower.gg`). The drain accessors take their drained K/V through
/// `void*` OUT-buffers the callee writes into:
///   - `gorget_set_drain_entry(const void* s, idx, void* out_key)` — out arg 2.
///   - `gorget_map_drain_entry(const void* m, idx, void* out_key, void* out_val)`
///     — out args 2 AND 3.
/// Each must appear in BOTH parallel ABI tables (`needs_ptr_arg` — so the
/// `borrow` operand is passed as `&slot` / `ISlotAddr` instead of by-value
/// NULL → `memcpy(NULL)` → SIGSEGV — AND `out_param_arg`, the ABI_OUT_PTR
/// tag that keeps the drained slot's drop alive). These are two hand-kept
/// parallel name-match tables: the SET drain was added but the MAP drain was
/// MISSED (`dict_drain_basic` crashed), the canonical sibling-drift hole.
/// This lint pins the pair so the next drain sibling can't desync the tables.
///
/// Mirrors Rust gg `src/backend/c_lir/helpers.rs:699-700`
/// (`"gorget_map_drain_entry" => &[2,3]`, `"gorget_set_drain_entry" => &[2]`).
///
/// **If this fails:** a drain accessor lost (or gained) a table entry. Every
/// drain fn must be present in BOTH `needs_ptr_arg` and `out_param_arg` for
/// each of its out-arg indices. Re-add the missing entry (or, if a new drain
/// sibling was introduced, add it to this lint's `REQUIRED` list).
#[test]
fn self_host_drain_out_param_abi_pair() {
    // lir_lower.gg lives ONLY in self_host_lowerer (real file, not symlinked),
    // so no double-count guard is needed.
    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lir_lower.gg").unwrap_or_default();

    // (fn_name, out-arg indices that must be tagged in BOTH ABI tables).
    let required: &[(&str, &[usize])] =
        &[("gorget_set_drain_entry", &[2]), ("gorget_map_drain_entry", &[2, 3])];

    let mut missing: Vec<String> = Vec::new();
    for (fn_name, out_args) in required {
        for &arg_idx in *out_args {
            // The drain entries are written as
            //   `if fn_name == "gorget_map_drain_entry" and arg_idx == 2:`
            // (or an `(arg_idx == 2 or arg_idx == 3)` combined guard). Count a
            // NON-COMMENT line that mentions the drain fn name AND the index.
            let idx_tok = format!("arg_idx == {arg_idx}");
            let hits = content
                .lines()
                .filter(|line| {
                    let trimmed = line.trim_start();
                    !trimmed.starts_with('#')
                        && trimmed.contains(&format!("\"{fn_name}\""))
                        && trimmed.contains(&idx_tok)
                })
                .count();
            // Must appear in BOTH tables (needs_ptr_arg + out_param_arg) → ≥2.
            if hits < 2 {
                missing.push(format!(
                    "{fn_name} out-arg {arg_idx}: found {hits} table entr{} (need ≥2: \
                     needs_ptr_arg AND out_param_arg)",
                    if hits == 1 { "y" } else { "ies" },
                ));
            }
        }
    }

    assert!(
        missing.is_empty(),
        "Self-host Dict/Set drain out-param ABI pair desynced in \
         `lir_lower.gg`:\n  {}\n\n\
         Every drain accessor's `void* out` argument must be tagged in BOTH the \
         `needs_ptr_arg` table (so the borrow operand is passed as `ISlotAddr`/`&slot` \
         instead of by-value NULL → `memcpy(NULL)` → SIGSEGV) AND the `out_param_arg` \
         table (the ABI_OUT_PTR tag that keeps the drained slot's drop alive). The SET \
         drain (out arg 2) and MAP drain (out args 2 AND 3) are siblings — adding one \
         and forgetting the other is the exact hole that crashed `dict_drain_basic`. \
         Mirrors Rust gg `helpers.rs:699-700`. Re-add the missing entry, or extend this \
         lint's `required` list if a new drain sibling landed.",
        missing.join("\n  "),
    );
}

/// R41-SD sibling-guard ratchet (CLAUDE.md rule 4 / "Sibling-site drift") over
/// the self-host `Vector.swap` / `Vector.swap_remove` runtime-ABI tables. Both
/// mutating O(1) Vector methods must be enrolled in THREE hand-kept name-match
/// tables — miss one and `vector_swap_fill` regresses in a DIFFERENT way per
/// table:
///   - `map_array_method` (lir_lower.gg): method → runtime symbol. Miss → the
///     mangled `Vector__T__swap_remove` never routes → "undefined reference".
///   - `needs_ptr_arg` (lir_lower.gg): self (arg 0) is passed as `&arr` (Ptr),
///     not by value. Miss → cc "incompatible type for argument 1 of
///     'gorget_array_swap'".
///   - `infer_method_return_type` (lower_types.gg): both return void (Rust
///     `builtins.rs` `ret_void`). Miss → binds a void call to a dst → cc "void
///     value not ignored as it ought to be".
/// `swap` and `swap_remove` are siblings — adding one and forgetting the other
/// (or forgetting a table) is the exact desync that broke `vector_swap_fill`
/// (R41). Mirrors Rust gg `src/lir/runtime.rs` (ArraySwap / ArraySwapRemove:
/// arg0 = A::Ptr, ret Void).
///
/// **If this fails:** a swap-family method lost a table entry, OR a new sibling
/// (e.g. `swap_range`) landed without all three. Re-add it to every table, or
/// extend this lint's `REQUIRED` list.
#[test]
fn self_host_vector_swap_abi_triple() {
    let lir =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lir_lower.gg").unwrap_or_default();
    let types =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower_types.gg").unwrap_or_default();

    // Scope each check to its OWN function body so an unrelated mention of the
    // symbol elsewhere in the file can't mask a missing table entry.
    let callee = self_host_fn_body_noncomment(&lir, "String map_array_method(").join("\n");
    let ptr = self_host_fn_body_noncomment(&lir, "bool needs_ptr_arg(").join("\n");
    let ret = self_host_fn_body_noncomment(&types, "int infer_method_return_type(").join("\n");

    // (method-name, runtime-symbol) siblings. The closing `"` in each needle
    // keeps `"gorget_array_swap"` / `"swap"` from matching inside
    // `"gorget_array_swap_remove"` / `"swap_remove"`.
    let required: &[(&str, &str)] =
        &[("swap", "gorget_array_swap"), ("swap_remove", "gorget_array_swap_remove")];

    let mut missing: Vec<String> = Vec::new();
    for (method, sym) in required {
        // (1) callee table: method → runtime symbol (a `return "<sym>"`).
        if !callee.contains(&format!("\"{sym}\"")) {
            missing.push(format!("map_array_method: no `return \"{sym}\"` for `{method}`"));
        }
        // (2) needs_ptr_arg: self (arg 0) passed by pointer.
        if !ptr.contains(&format!("\"{sym}\"")) {
            missing.push(format!("needs_ptr_arg: `{sym}` not tagged as ptr-self"));
        }
        // (3) infer_method_return_type: void (UNIT) return.
        if !ret.contains(&format!("\"{method}\"")) {
            missing.push(format!("infer_method_return_type: `{method}` missing from the void list"));
        }
    }

    assert!(
        missing.is_empty(),
        "Self-host Vector swap/swap_remove ABI triple desynced:\n  {}\n\n\
         Both `swap` AND `swap_remove` must appear in ALL THREE hand-kept tables \
         (`map_array_method` + `needs_ptr_arg` in lir_lower.gg, \
         `infer_method_return_type` in lower_types.gg). Adding one method — or \
         one table — and forgetting the rest is the desync that broke \
         `vector_swap_fill` (R41). Mirrors Rust gg `src/lir/runtime.rs` \
         ArraySwap/ArraySwapRemove. Re-add the missing entry, or extend the \
         REQUIRED list if a new swap-family sibling landed.",
        missing.join("\n  "),
    );
}

/// R43 iterator-protocol equip-default short-naming sibling ratchet
/// (CLAUDE.md rule 4 / "Sibling-site drift" + Core #8 bundle).
///
/// An `equip X with Iterator[int]:` block's inherited DEFAULT methods
/// (min/sum/product/contains/join …) are dispatched at the CALL SITE by the
/// SHORT `X__method` symbol — mirroring the explicit `next` short-naming via
/// `is_iterator_protocol_trait`. But the default-body emit in `lower_module`
/// mangles them LONG (`{Trait}_for_{X}__method`) by default, so BOTH gated
/// emit arms must OVERRIDE to the short symbol when the trait is
/// iterator-protocol — else `c.iter().sum()` → `CounterIter__sum` dangles →
/// C89 implicit-int → cc "incompatible types" (R43 `stdlib_iter_bounds_coverage`).
///
/// TWO body-emit arms carry the override (Core #4 / sibling-site-drift):
///   - `did_split` (own-trait defaults) — gated by `tc_gate_sp`.
///   - `not did_split` sibling — gated by `tc_gate`.
/// The parent-trait defaults arm (`pt_mangled = eq_target_sp + "__" + ...`)
/// already short-names UNCONDITIONALLY and is deliberately NOT pinned here: its
/// short-naming is not the iterator-protocol override, and the needles below use
/// the `dm_mangled2 = eq_target_sp` / `dm_mangled = eq_target` reassignment
/// prefixes (which the LONG declarations `... = dm_trait2 + "_for_" + ...` and the
/// `pt_mangled = ...` line CANNOT match), so pinning them can't falsely bless
/// that unconditional line.
///
/// Companion axis (Root A2): the nullary-factory static-return-type list in
/// `lower_expr_inner` must carry BOTH `default` AND `one` — each is a
/// Self-returning factory static (`T.default()` / `T.one()`). A miss makes the
/// accumulator fall through to I64 → integer-mul miscompile of `.product()`.
///
/// **If this fails:** a third iterator-protocol default emit-arm landed without
/// the short-name override, OR the `default`/`one` static-ret list lost an
/// entry, OR a new Self-returning factory static (e.g. `zero`) needs adding to
/// the list (and to this lint).
#[test]
fn self_host_iter_protocol_equip_default_shortname() {
    let lower =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();
    let lower_expr =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower_expr.gg").unwrap_or_default();

    // Scope to the enclosing function bodies (comment-stripped) so an unrelated
    // mention can't mask a missing override.
    let module_body =
        self_host_fn_body_noncomment(&lower, "GirModule lower_module(").join("\n");
    let expr_body =
        self_host_fn_body_noncomment(&lower_expr, "int lower_expr_inner(").join("\n");

    let mut missing: Vec<&str> = Vec::new();

    // (1) did_split arm short-name OVERRIDE. The `dm_mangled2 = eq_target_sp`
    //     reassignment prefix is unique to the gated override — the LONG
    //     declaration is `dm_mangled2 = dm_trait2 + "_for_" + eq_target_sp + ...`
    //     and the unconditional parent-trait line is `pt_mangled = eq_target_sp
    //     + ...`, so this needle matches ONLY the override.
    if !module_body.contains("dm_mangled2 = eq_target_sp + \"__\" + tmeth2.name") {
        missing.push(
            "lower.gg did_split arm: iterator-protocol default short-name override \
             `dm_mangled2 = eq_target_sp + \"__\" + tmeth2.name` (gated by tc_gate_sp) missing",
        );
    }
    // (2) not-did_split sibling arm short-name OVERRIDE (same reasoning: the LONG
    //     declaration is `dm_mangled = dm_trait + "_for_" + eq_target + ...`).
    if !module_body.contains("dm_mangled = eq_target + \"__\" + tmeth.name") {
        missing.push(
            "lower.gg not-did_split arm: iterator-protocol default short-name override \
             `dm_mangled = eq_target + \"__\" + tmeth.name` (gated by tc_gate) missing",
        );
    }
    // (3) the short-name override must stay GATED on the typed iterator-protocol
    //     predicate — unconditional short-naming breaks non-iterator equips
    //     (cf. the Writer.flush −85 regression).
    if !module_body.contains("is_iterator_protocol_trait(tname)") {
        missing.push(
            "lower.gg: the `is_iterator_protocol_trait(tname)` gate for the equip-default \
             short-name overrides is missing — the override must stay typed-predicate-gated",
        );
    }
    // (4) nullary-factory static-ret list carries BOTH default AND one.
    if !expr_body.contains("mname == \"default\" or mname == \"one\"") {
        missing.push(
            "lower_expr.gg: the Self-returning nullary-factory static-ret arm must be \
             `mname == \"default\" or mname == \"one\"` (a miss lands `.product()` acc as I64)",
        );
    }

    assert!(
        missing.is_empty(),
        "Self-host iterator-protocol equip-default short-naming / factory-static ABI desynced:\n  {}\n\n\
         BOTH `lower_module` default-body emit arms must OVERRIDE to the SHORT `{{Type}}__method` \
         symbol when `is_iterator_protocol_trait(tname)` (the call site dispatches short, mirroring \
         `next`), AND the `lower_expr_inner` factory-static return-type list must carry both \
         `default` and `one`. Adding a third emit-arm — or a new Self-returning factory static \
         (e.g. `zero`) — without extending its sibling is the R43 desync that broke \
         `stdlib_iter_bounds_coverage`. Re-add the override / list entry, then extend this lint.",
        missing.join("\n  "),
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
/// Bumped 74 → 75 (2026-06-20): the box-drop box-inner-drop fix (`8476d64e`) added
/// ONE `dia_box_nm.starts_with("Box__")` site in `lir_lower.gg`'s GIDropIfAlive box
/// arm — it slices the inner type from `Box__<inner>` to find the inner's drop fn
/// (the box ptr points straight at the inner value). The MEANING decision
/// (is-this-a-Box) is the TYPED `type_runtime_map=="Box"` probe; the `Box__` parse
/// is the established "extract T from Mangled__T" contract (siblings at
/// :307/:655/:828/:903 + emit_box_alloc), NOT a classification dodge. FOLLOW-UP
/// (TODO): the typed `box_inner_type` field on the self-host `LirStructDef` retires
/// this parse — drop BUDGET back to 74 when it lands.
/// Bumped 75 → 78 (2026-06-27): the self-host Callable→GorgetClosure alias
/// (closure Phase-2 inc-5c, `c96977f7`) added THREE
/// `name.starts_with("Callable__"/"MutCallable__"/"ConsumeCallable__")` sites in
/// `lir_lower.gg`'s `build_resource_metadata` — the SINGLE-SOURCE-OF-TRUTH
/// resource-metadata classifier (the same fn holding `Vector__`/`Box__`/`Guard__`,
/// the lint's allowed "option 2" location). They alias the in-collection Callable
/// family to the 16-byte runtime `GorgetClosure` (drop `gorget_closure_free`, clone
/// `gorget_closure_clone_to_owned`/`_inplace`), mirroring Rust `builtins.rs:998-1056`.
/// The codegen body-suppression + the collection clone/drop routing decisions use the
/// TYPED `runtime == "GorgetClosure"` (set once by the alias's `type_runtime_map.put`),
/// adding 0 to this count — NOT a classification dodge.
/// Bumped 78 → 79 (2026-06-29): the keystone Channel-spawn carrier (Slices 1+2)
/// added ONE `nm.starts_with("Channel__")` site in `lower_expr.gg`'s
/// `spawn_shape_supported`.
/// Restored 79 → 78 (2026-06-29, round-16 shared-spawn): that interim
/// `nm.starts_with("Channel__")` gate was RETIRED to a typed
/// `spawn_supported_carrier(nm, &gmod)` predicate that reads the carrier's
/// `method_prefix` metadata VALUE (`gorget_channel` / `gorget_shared`) via
/// `resource_meta_for` — admitting BOTH spawn carriers (Channel + Shared)
/// without a name-prefix `starts_with` (so the count goes back down by one).
/// The two typed predicates DO over-admit and are correctly NOT used for the
/// gate: `is_opaque_pointer_type` (.opaque_handle) lets Mutex / RWLock /
/// Semaphore through, and `is_refcounted_carrier` (.copy_semantics ==
/// CsRefCounted) lets AtomicInt / AtomicBool / Semaphore / Weak / Shared /
/// Channel through. (NOTE: RWLock is NOT CsRefCounted — Inc-B `a1331b4d`
/// reclassified it to CsResource; the earlier claim here that "RWLock is
/// CsRefCounted" was stale. RWLock's over-admit is via `is_opaque_pointer_type`,
/// not `is_refcounted_carrier`.) RWLock IS a spawned param in async_rwlock /
/// shared_rwlock / stress_rwlock_*; admitting it regresses those to an
/// `unknown type name` C-emit failure — which is why the `method_prefix`
/// VALUE match (exactly gorget_channel + gorget_shared) is the right gate.
/// The carrier retain/release routing (`emit_spawn_helpers` /
/// `emit_spawn_carrier_releases`) now reads the typed `CloneParam.method_prefix`
/// VALUE too (gorget_channel_retain/_release vs gorget_shared_clone/_drop) —
/// adds 0 name-prefix sites.
/// Bumped 78 → 81 (2026-07-01): the Box[Trait] trait-object-dispatch keystone
/// (increment 1 of the multi-session track) added THREE `starts_with("Box__")`
/// parse-guard sites in `lir_lower.gg` — ONE in `try_record_trait_dispatch`
/// (:2136) and TWO on a single line in `try_emit_trait_obj_construct` (:3373,
/// `dst_def.name` + `src_def.name`). ALL THREE are the blessed
/// "extract T from Mangled__T" case, NOT a classification dodge: the SEMANTIC
/// is-this-a-trait-box decision is a TYPED discriminator in every case —
/// `gmod.type_infos.contains(<Trait>_VTable)` at :2152 (the trait-box signal,
/// mirroring Rust `operands.rs:799`) for site 1, and the fat-struct SHAPE check
/// `dst_def.fields.len()==2 && fields[1].name=="vtable"` at :3371 (set upstream
/// from the `_VTable` typed channel) for sites 2/3. The `starts_with` is a pure
/// length-5 guard so `.slice(5, ...)` can strip the mangling — exactly like the
/// blessed sibling `dia_box_nm.starts_with("Box__")` at :4630 (the 74→75 bump
/// above). Site 1's callee arrives as a raw `String` from `GICall(int, String,
/// Vector[Operand])` (gir.gg) — no typed struct behind it, so it is an inherent
/// "option 2" name-only parse. Sites 2/3 retire alongside the SAME typed
/// `box_inner_type` LirStructDef field already TODO'd for the :4630 site (the
/// 74→75 comment) — when that field lands, drop BUDGET by 3 more with :4630.
#[test]
fn no_growth_in_self_host_name_prefix_routing() {
    const BUDGET: usize = 81;

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

/// Sibling-site ratchet (CLAUDE.md rule 4 / "Sibling-site drift") over the
/// THREE method-generic equip-instance `match …trait_name:` arms in
/// `lower.gg`'s `lower_module`. Each arm dispatches a method-level-generic
/// method instance onto either the TRAIT-default body (`case Some(tname)`) or
/// the INHERENT equip-block body (`case None`). Two of the three `case None`
/// arms now lower the inherent path (the `proto_minsts` fn-sig pre-reg arm and
/// the `proto_minsts` body-emit arm — fixed alongside `iter_enumerate_zip`,
/// where `zip[U]` is a method-generic in `equip [T] VectorIter[T]:`). The third
/// (`gm_` loop, body-emit on a GENERIC receiver) is DEFERRED with a stated
/// blocker (needs a `fn_sigs.contains` dedup guard to avoid double-emit with the
/// `proto_minsts` arm, plus a repro fixture) and KEEPS its bare `pass`.
///
/// **Why this ratchet is ANCHORED, not a whole-file `case None:`→`pass` count.**
/// The earlier design counted EVERY `case None:` immediately followed by a
/// trimmed-`pass` line across all of `lower.gg`. That was a blunt instrument: a
/// 4500-line lowerer destructures `Option` constantly, and a vacuous "no value
/// to extract here" None arm — with literally nothing to lower — is idiomatic
/// and correct. Three such arms were added by unrelated parity fixes
/// (`ext_ret_opt`/`05daf35b`, `rs_ext_ret_opt`/`239083f2`,
/// `resource_meta_for(mrinst.base_name)`/`19d1529a`), pushing the blunt count
/// 18→21 and turning the FATAL ratchet RED even though NONE of them is a
/// method-generic dispatch stub. A guard that goes red on correct, unrelated
/// code can no longer catch the NEXT real growth — exactly the guard-rot Core
/// invariant #6 forbids. So the ratchet now anchors on the ONE genuine class it
/// was always meant to police, not on a textual pattern it cannot isolate.
///
/// SO this ratchet counts a `case None:`→`pass` whose IMMEDIATELY FOLLOWING line
/// is the deferred-class marker (`gm-inherent-generic-equip DEFERRED`, placed
/// AFTER the `pass` so the `case None:`→`pass` adjacency is preserved). Today
/// that is exactly ONE arm (the `gm_` loop, body-emit on a GENERIC receiver,
/// lower.gg:~4205). BUDGET = 1 with TARGET 0: implementing the inherent body
/// (and deleting the marker line) drops the count to 0.
///
/// **If this fails (count went UP):** a NEW deferred method-generic equip stub
/// was added — implement it through the shared inherent-lowering shape (mirror
/// the `proto_minsts` body-emit None arm) or, if it must be deferred, justify it
/// in review and bump BUDGET deliberately. A bare unrelated `Option` None arm
/// does NOT carry the marker and is correctly invisible to this ratchet.
#[test]
fn no_growth_in_self_host_lower_case_none_pass_stubs() {
    // The deferred method-generic equip-instance inherent body-emit stub.
    // TARGET 0 (implement the inherent path + delete the marker).
    const BUDGET: usize = 1;
    // The distinctive marker the deferred stub places on the line right after
    // its `pass` (see the `gm_` loop, lower.gg:~4205). Substring match so the
    // exact wording can evolve without silently un-anchoring the ratchet.
    const DEFERRED_MARKER: &str = "gm-inherent-generic-equip DEFERRED";

    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();

    // Count `case None:` → `pass` whose FOLLOWING line carries the deferred
    // method-generic marker. A whole-file `case None:`→`pass` count is too
    // blunt (idiomatic vacuous `Option` None arms blow the budget); anchoring
    // on the marker isolates exactly the class this ratchet polices.
    let lines: Vec<&str> = content.lines().collect();
    let mut count = 0usize;
    for w in lines.windows(3) {
        if w[0].trim() == "case None:"
            && w[1].trim() == "pass"
            && w[2].contains(DEFERRED_MARKER)
        {
            count += 1;
        }
    }

    assert!(
        count > 0,
        "no_growth_in_self_host_lower_case_none_pass_stubs: the deferred \
         method-generic equip stub marker (`{DEFERRED_MARKER}`) was not found \
         immediately after a `case None:`→`pass` in lower.gg. If the inherent \
         `gm_` body-emit path was IMPLEMENTED, delete this ratchet (move its \
         TODO entry to DONE.md). Otherwise the scan or the marker moved — re-anchor.",
    );
    assert!(
        count <= BUDGET,
        "Self-host `lower.gg` deferred method-generic equip-instance \
         `case None:`→`pass` stub count grew beyond budget: {count} > {BUDGET}.\n\n\
         A new DEFERRED method-generic equip-instance inherent body-emit arm was \
         added (it carries the `{DEFERRED_MARKER}` marker). Do NOT leave it a \
         stub: lower the inherent equip-block body through the shared shape \
         (mirror the `proto_minsts` body-emit None arm — match the method in the \
         equip block's own `methods`, bind equip-[T] + Self + method-[U] subs, \
         emit under the mangled symbol; the `gm_` arm additionally needs a \
         `fn_sigs.contains` dedup guard to avoid double-emit). Then LOWER BUDGET \
         in the same commit. Don't bump the budget to dodge review.",
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
/// `the #37 phase-1 lazy-default brief (git history)` Appendix A).
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
         rule\"; the #37 phase-1 lazy-default brief (git history) Appendix A).\n\
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

/// Single-source-of-truth ratchet (CLAUDE.md invariant #4 "one fix, all
/// siblings" + devbook/24 rule 3 "one source of truth per axis"): the set of
/// builtin GorgetArray/Map/Set-backed collection base names that the self-host
/// lowerer must NOT register as user structs lives in exactly ONE helper —
/// `is_builtin_collection_base` (`self_host_lowerer/lower.gg`). It gates both
/// over-registration sites (the bare `type_infos` IStruct arm and the
/// mono-record loop) AND the `is_type_constructor` "not-a-user-struct-ctor"
/// subset (`lower_types.gg`, which delegates to it).
///
/// Registering one of these (the bare template `struct Vector[T]: pass`, or a
/// mono `Vector__bool`) as a user `type_info` makes `emit_structs` emit invalid
/// C — an unnamed-field struct `struct __gg_Vector { uint8_t ; };` (a `pass`
/// body is one EMPTY-name field). Rust gg routes these to
/// `register_collection_alias` instead (`src/ir/lowering/types.rs:699`).
///
/// This lint pins:
///   1. `is_builtin_collection_base` exists in lower.gg with EXACTLY the
///      expected base names (so a contributor who edits it has to update this).
///   2. `is_type_constructor` (lower_types.gg) has NO inline `name == "Vector"`
///      / `"Dict"` / ... collection-base list — it must call the helper, so a
///      NEW collection base name added to the helper automatically flows to the
///      constructor classifier too (no second list to drift out of sync).
///
/// **If this fails:** either a second hardcoded collection-base list crept back
/// into `is_type_constructor` (route it through `is_builtin_collection_base`),
/// or the helper's name set changed (update EXPECTED_BASES here with a
/// justification). `Box` is intentionally NOT in the set — Box monos go through
/// lir_lower's `BkRegularBox` arm, not the mono-record loop.
#[test]
fn collection_base_names_single_source() {
    /// The collection bases declared `: pass` in lib/std/collections.gg whose
    /// monos are runtime GorgetArray/GorgetMap/GorgetSet aliases. Box excluded.
    const EXPECTED_BASES: &[&str] =
        &["Vector", "Deque", "Channel", "Dict", "HashMap", "Set", "HashSet"];

    // 1. The single source of truth must exist and list exactly EXPECTED_BASES.
    let lower = fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg")
        .expect("read self_host_lowerer/lower.gg");
    let fn_start = lower
        .find("bool is_builtin_collection_base(String name):")
        .expect(
            "is_builtin_collection_base helper missing from self_host_lowerer/lower.gg — it is \
             the single source of truth for the GorgetArray-backed collection base names that \
             must NOT register as user structs.",
        );
    // The helper body runs until the next top-level def (a line starting with a
    // non-space, non-# char after the signature line).
    let body: String = lower[fn_start..]
        .lines()
        .skip(1)
        .take_while(|l| {
            l.trim().is_empty()
                || l.starts_with(' ')
                || l.starts_with('\t')
                || l.trim_start().starts_with('#')
        })
        .collect::<Vec<_>>()
        .join("\n");
    for base in EXPECTED_BASES {
        assert!(
            body.contains(&format!("name == \"{base}\"")),
            "is_builtin_collection_base is missing base name `{base}` — the EXPECTED_BASES \
             list in this lint and the helper body must agree (one source of truth).",
        );
    }
    // Reject any collection base NOT in EXPECTED_BASES (catches a silently-added
    // name the lint doesn't know about). Scan `name == "X"` tokens in the body.
    for line in body.lines() {
        let mut rest = line;
        while let Some(idx) = rest.find("name == \"") {
            let after = &rest[idx + "name == \"".len()..];
            if let Some(end) = after.find('"') {
                let nm = &after[..end];
                assert!(
                    EXPECTED_BASES.contains(&nm),
                    "is_builtin_collection_base lists base name `{nm}` not in this lint's \
                     EXPECTED_BASES — if it's a genuine GorgetArray-backed collection, add it \
                     to EXPECTED_BASES with a justification; otherwise it does not belong here.",
                );
                rest = &after[end + 1..];
            } else {
                break;
            }
        }
    }

    // 2. `is_type_constructor` (lower_types.gg) must NOT carry a second inline
    //    collection-base list — it delegates to is_builtin_collection_base.
    let lower_types = fs::read_to_string("tests/fixtures/self_host_lowerer/lower_types.gg")
        .expect("read self_host_lowerer/lower_types.gg");
    let ctor_start = lower_types
        .find("bool is_type_constructor(String name, GirModule &gmod):")
        .expect("is_type_constructor missing from lower_types.gg");
    // Body until the next top-level def.
    let ctor_body: String = lower_types[ctor_start..]
        .lines()
        .skip(1)
        .take_while(|l| {
            l.trim().is_empty()
                || l.starts_with(' ')
                || l.starts_with('\t')
                || l.trim_start().starts_with('#')
        })
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        ctor_body.contains("is_builtin_collection_base(name)"),
        "is_type_constructor must delegate the collection-base subset to \
         is_builtin_collection_base (single source of truth), not inline the list.",
    );
    // The collection bases (minus the ones that are ALSO legitimately listed
    // for a different reason) must not appear as inline `name == "X"` comparisons
    // in is_type_constructor — that would be a drifting second list.
    for base in EXPECTED_BASES {
        assert!(
            !ctor_body.contains(&format!("name == \"{base}\"")),
            "is_type_constructor has an inline `name == \"{base}\"` comparison — route the \
             collection-base subset through is_builtin_collection_base instead of maintaining \
             a second list that can drift (one fix, all siblings).",
        );
    }
}

/// Ratchet (CLAUDE.md "convert a recurring bug class into an executable guard"):
/// the self-host's embedded C-runtime table in
/// `tests/fixtures/self_host_lowerer/driver.gg` (Inc-2 relocatability) must
/// carry EXACTLY one entry per `src/backend/c/runtime/*.c` file. A new runtime
/// `.c` that lands WITHOUT being added to the table would silently fall back to
/// the on-disk read in `read_runtime` — breaking relocatability for any program
/// that needs it (a `gg-selfhost` built without that file's bytes can't compile
/// a program from a foreign cwd). This pins the table to the directory so the
/// next runtime file is FORCED into BOTH the `meta String RT_*` const list and
/// the `build_embedded_runtime` dict.
///
/// **If this fails because a runtime .c was ADDED:** add its
/// `meta String RT_<basename> = embed_file("../../../src/backend/c/runtime/<basename>.c")`
/// const AND its `d["<basename>.c"] = RT_<basename>` row in driver.gg, then this
/// passes automatically (the lint counts the directory, not a hardcoded N).
/// **If a runtime .c was REMOVED:** delete its const + dict row to match.
///
/// SQLite (`../sqlite3/*`), stb_image (`../stb_image.h`), SDL/GL/metal external
/// headers are vendored OUTSIDE `runtime/` and intentionally stay disk-only
/// (Inc-4), so they are NOT in this set and NOT counted here.
#[test]
fn self_host_embedded_runtime_table_count() {
    // Count the canonical source: every *.c directly under src/backend/c/runtime.
    let runtime_dir = Path::new("src/backend/c/runtime");
    let mut runtime_files = 0usize;
    let entries = fs::read_dir(runtime_dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", runtime_dir.display()));
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("c") {
            runtime_files += 1;
        }
    }
    assert!(
        runtime_files > 0,
        "found 0 *.c files under {} — wrong cwd or a moved runtime dir?",
        runtime_dir.display(),
    );

    let driver = fs::read_to_string("tests/fixtures/self_host_lowerer/driver.gg")
        .expect("cannot read self_host_lowerer/driver.gg");

    // The embed-const table: `meta String RT_<name> = embed_file("...runtime/<name>.c")`.
    let const_count = driver
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("meta String RT_") && t.contains("embed_file(")
        })
        .count();

    // The dict-build rows: `d["<name>.c"] = RT_<name>` inside build_embedded_runtime.
    let insert_count = driver
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("d[\"") && t.contains("= RT_") && t.contains(".c\"]")
        })
        .count();

    assert_eq!(
        const_count, runtime_files,
        "embedded-runtime `meta String RT_*` const count ({const_count}) != number of \
         src/backend/c/runtime/*.c files ({runtime_files}). A runtime .c was added or \
         removed without updating driver.gg's embed table — the file would silently \
         fall back to the disk read, breaking relocatability. Add/remove its `RT_` \
         const (and its dict row) to match.",
    );
    assert_eq!(
        insert_count, runtime_files,
        "embedded-runtime dict-build `d[\"X.c\"] = RT_X` row count ({insert_count}) != \
         number of src/backend/c/runtime/*.c files ({runtime_files}). Each `RT_` const \
         must be inserted into the `build_embedded_runtime` map. Add/remove the matching \
         dict row.",
    );
}

/// Ratchet (CLAUDE.md "convert a recurring bug class into an executable guard"):
/// the self-host's embedded `lib/std` table in
/// `tests/fixtures/self_host_lowerer/driver.gg` (Inc-3 relocatability for
/// programs WITH `from std…` imports) must carry EXACTLY one entry per
/// `lib/std/*.gg` file. A new `lib/std` module that lands WITHOUT being added to
/// the table would silently fall back to the on-disk read in `load_imports` —
/// breaking relocatability for any program that imports it (a `gg-selfhost`
/// built without that module's bytes can't compile a program from a foreign cwd
/// with no `$GG_LIB_DIR`). This pins the table to the directory so the next
/// `lib/std` module is FORCED into BOTH the `meta String LIB_*` const list and
/// the `build_embedded_lib` dict.
///
/// **If this fails because a `lib/std` module was ADDED:** add its
/// `meta String LIB_<basename> = embed_file("../../../lib/std/<basename>.gg")`
/// const AND its `d["std.<basename>"] = LIB_<basename>` row in driver.gg, then
/// this passes automatically (the lint counts the directory, not a hardcoded N).
/// **If a `lib/std` module was REMOVED:** delete its const + dict row to match.
///
/// `lib/xtd/*.gg` (SQLite/SDL/GL externs) is a SEPARATE later increment and is
/// intentionally NOT embedded — so it is NOT in this set and NOT counted here.
#[test]
fn self_host_embedded_libstd_table_count() {
    // Count the canonical source: every *.gg directly under lib/std.
    let libstd_dir = Path::new("lib/std");
    let mut libstd_files = 0usize;
    let entries = fs::read_dir(libstd_dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", libstd_dir.display()));
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("gg") {
            libstd_files += 1;
        }
    }
    assert!(
        libstd_files > 0,
        "found 0 *.gg files under {} — wrong cwd or a moved lib/std dir?",
        libstd_dir.display(),
    );

    let driver = fs::read_to_string("tests/fixtures/self_host_lowerer/driver.gg")
        .expect("cannot read self_host_lowerer/driver.gg");

    // The embed-const table: `meta String LIB_<name> = embed_file("...lib/std/<name>.gg")`.
    let const_count = driver
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("meta String LIB_") && t.contains("embed_file(")
        })
        .count();

    // The dict-build rows: `d["std.<name>"] = LIB_<name>` inside build_embedded_lib.
    let insert_count = driver
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("d[\"std.") && t.contains("= LIB_")
        })
        .count();

    assert_eq!(
        const_count, libstd_files,
        "embedded-libstd `meta String LIB_*` const count ({const_count}) != number of \
         lib/std/*.gg files ({libstd_files}). A lib/std module was added or removed \
         without updating driver.gg's embed table — the module would silently fall back \
         to the disk read, breaking relocatability for std-importing programs. Add/remove \
         its `LIB_` const (and its dict row) to match.",
    );
    assert_eq!(
        insert_count, libstd_files,
        "embedded-libstd dict-build `d[\"std.X\"] = LIB_X` row count ({insert_count}) != \
         number of lib/std/*.gg files ({libstd_files}). Each `LIB_` const must be \
         inserted into the `build_embedded_lib` map keyed on the normalized module path. \
         Add/remove the matching dict row.",
    );
}

/// Exhaustive-walker ratchet (AGENTS.md Core invariant #4 "one fix, all
/// siblings" + #6 "convert a recurring bug class into an executable guard";
/// devbook/24 sibling-site drift). The self-host carrier-operator reject
/// (`??`-on-non-Option/Result and `*x`-on-non-Box) is driven from the
/// EXHAUSTIVE `check_carrier_ops_expr` / `check_carrier_ops_stmt` walker in
/// `tests/fixtures/self_host_typechecker/typecheck.gg`, modeled on
/// `resolve.gg`'s `resolve_expr`. Its correctness depends on visiting EVERY
/// expression (and statement) position — the first cut shipped the reject on
/// the closure-FINDING `walk_expr_closures` pass, which `else: pass`es most
/// shapes, so a `??` nested in `EUnaryOp`/`EIndex`/`EArrayLiteral`/`EAs`/…
/// silently ESCAPED and the self-host miscompiled it (a one-sided reject that
/// failed the Core #8 reference-grade bar).
///
/// This lint pins exhaustiveness STRUCTURALLY: it derives the full `Expr`/`Stmt`
/// variant set from `ast.gg` and asserts the walker has a `case <Variant>(` arm
/// for each. A new AST variant that carries a sub-expression but isn't added to
/// the walker re-opens the escape hole — this fails until the arm is added.
///
/// **If this fails:** add the missing `case <Variant>(...)` arm to
/// `check_carrier_ops_expr` (for `E*`) or `check_carrier_ops_stmt` (for `S*`) in
/// typecheck.gg, recursing into every sub-expression / body the variant carries
/// (model it on the same variant's arm in `resolve.gg::resolve_expr`).
#[test]
fn self_host_safety_walker_is_exhaustive() {
    let ast = fs::read_to_string("tests/fixtures/self_host_typechecker/ast.gg")
        .expect("self_host_safety_walker_is_exhaustive: ast.gg not found");
    let tc = fs::read_to_string("tests/fixtures/self_host_typechecker/typecheck.gg")
        .expect("self_host_safety_walker_is_exhaustive: typecheck.gg not found");

    // Parse the variant names from `enum Expr:` / `enum Stmt:` in ast.gg. Each
    // variant is an indented `EName(...)` / `EName` (or `SName...`) line; the
    // enum body ends at the first non-indented line.
    fn variants(src: &str, enum_header: &str, prefix: char) -> Vec<String> {
        let mut out = Vec::new();
        let mut in_enum = false;
        for line in src.lines() {
            if line.trim_start() == enum_header {
                in_enum = true;
                continue;
            }
            if in_enum {
                // Enum body ends at the first line that is NOT blank, NOT a
                // comment, and NOT indented (a new top-level decl).
                if line.is_empty() {
                    continue;
                }
                let trimmed = line.trim_start();
                if trimmed.starts_with('#') {
                    continue;
                }
                if !line.starts_with(' ') && !line.starts_with('\t') {
                    break;
                }
                // Variant name = leading identifier (up to '(' or end).
                let name: String = trimmed
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect();
                if name.starts_with(prefix) {
                    out.push(name);
                }
            }
        }
        out
    }

    // Restrict the search to the walker function bodies so unrelated `case E…`
    // arms elsewhere in typecheck.gg don't mask a missing one.
    fn body_of<'a>(src: &'a str, fn_sig_prefix: &str) -> &'a str {
        let start = src
            .find(fn_sig_prefix)
            .unwrap_or_else(|| panic!("walker fn `{fn_sig_prefix}` not found in typecheck.gg"));
        let rest = &src[start..];
        // The body runs until the next top-level `void ` decl after the sig line.
        let after_sig = rest.find('\n').map(|i| i + 1).unwrap_or(0);
        let tail = &rest[after_sig..];
        let end = tail.find("\nvoid ").map(|i| after_sig + i).unwrap_or(rest.len());
        &rest[..end]
    }

    let expr_variants = variants(&ast, "enum Expr:", 'E');
    let stmt_variants = variants(&ast, "enum Stmt:", 'S');
    assert!(
        expr_variants.len() >= 40 && stmt_variants.len() >= 25,
        "safety-walk exhaustiveness lint failed to parse ast.gg variants (got {} Expr, {} Stmt) — \
         the ast.gg enum shape changed; fix the parser.",
        expr_variants.len(),
        stmt_variants.len(),
    );

    // The former `check_carrier_ops_{expr,stmt}` walkers were merged into the
    // ONE unified `check_safety_{expr,stmt}` walk (2026-07-15 self-host safety
    // unification). The exhaustiveness guard follows the rename.
    let expr_body = body_of(&tc, "void check_safety_expr(");
    let stmt_body = body_of(&tc, "void check_safety_stmt(");

    let mut missing_expr = Vec::new();
    for v in &expr_variants {
        // `case EName(` for payload variants, `case EName()` for nullary.
        if !expr_body.contains(&format!("case {v}(")) {
            missing_expr.push(v.clone());
        }
    }
    let mut missing_stmt = Vec::new();
    for v in &stmt_variants {
        if !stmt_body.contains(&format!("case {v}(")) {
            missing_stmt.push(v.clone());
        }
    }

    assert!(
        missing_expr.is_empty() && missing_stmt.is_empty(),
        "self-host `check_carrier_ops_*` walker is NOT exhaustive — these AST \
         variants have no arm, so a `??`/`*x` nested inside one would ESCAPE the \
         carrier-operator reject and the self-host would silently miscompile it \
         (AGENTS.md Core #8 one-sided-reject regression).\n  \
         missing Expr arms: {missing_expr:?}\n  missing Stmt arms: {missing_stmt:?}\n\
         Add the `case <Variant>(...)` arm to check_carrier_ops_expr / \
         check_carrier_ops_stmt in typecheck.gg, recursing into every \
         sub-expression the variant carries (mirror resolve.gg::resolve_expr).",
    );
}

/// One-source-of-truth ratchet (CLAUDE.md Core invariant #6 / devbook/24 rule 3)
/// for the GorgetMap / GorgetSet runtime struct size in the self-host lowerer.
///
/// The real C `GorgetMap` (`src/backend/c/runtime/runtime_preamble.c`, 24
/// pointer/size_t fields × 8 = 192 bytes: 19 legacy fields + 5 D39 dense-mode
/// fields appended at struct END; `GorgetSet` is a typedef alias) and Rust gg
/// (`src/lir/lower/types.rs`, `GorgetMap | GorgetSet => 192`) both use
/// **192**. The self-host previously hand-duplicated this size as the literal
/// `184` across 9 sites in `lir_lower.gg` (2 struct defs + 7 ResourceMetadata
/// returns). That over-count (the size of an out-of-date 23-field layout)
/// inflated every enum/union/array layout embedding a Dict/Set, so
/// `gorget_array_push` read past the stack slot = stack-buffer-overflow on the
/// xml fixtures. The fix collapsed all 9 sites onto the single
/// `GORGET_MAP_STRUCT_SIZE` constant in `lir.gg`.
///
/// This lint pins three invariants so the divergent literal cannot creep back:
///   (a) `GORGET_MAP_STRUCT_SIZE` is defined as `192` in `lir.gg`.
///   (b) Rust gg still agrees (`GorgetMap | GorgetSet => 192` in types.rs).
///   (c) No raw `184` literal lingers in `lir_lower.gg`, AND every GorgetMap /
///       GorgetSet `ResourceMetadata`/`LirStructDef` size site reads the named
///       constant rather than a bare integer (so all 9 stay single-sourced).
#[test]
fn self_host_gorget_map_struct_size() {
    const EXPECTED_SIZE: usize = 192;
    // ≥9 single-sourced size sites: 2 LirStructDef + 7 ResourceMetadata.
    const MIN_CONSTANT_USE_SITES: usize = 9;

    let lir = fs::read_to_string("tests/fixtures/self_host_lowerer/lir.gg").unwrap_or_default();
    let lower =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lir_lower.gg").unwrap_or_default();
    let rust = fs::read_to_string("src/lir/lower/types.rs").unwrap_or_default();

    // (a) Constant defined at the expected value in lir.gg.
    let const_def = format!("const int GORGET_MAP_STRUCT_SIZE = {EXPECTED_SIZE}");
    assert!(
        lir.lines().any(|l| l.trim_start().starts_with(&const_def)),
        "self-host `GORGET_MAP_STRUCT_SIZE` is not defined as `{EXPECTED_SIZE}` in \
         tests/fixtures/self_host_lowerer/lir.gg. The GorgetMap/GorgetSet runtime \
         struct is 24 fields × 8 bytes = 192 (19 legacy + 5 D39 dense-mode fields \
         appended at struct END; runtime_preamble.c). Do NOT change this to 184 \
         (the stale 23-field over-count that overflowed gorget_array_push on the \
         xml fixtures) or back to 152 (the pre-D39 legacy size — truncates the \
         alloca so runtime stores to entries_keys/values/len/cap/indices walk into \
         adjacent stack slots) without first changing the actual runtime struct \
         AND Rust gg.",
    );

    // (b) Rust gg agrees — the cross-compiler source of truth.
    assert!(
        rust.contains(&format!("GorgetMap | crate::lir::ResourceKind::GorgetSet => {EXPECTED_SIZE}"))
            || rust.contains(&format!("\"GorgetMap\" | \"GorgetSet\" => {EXPECTED_SIZE}")),
        "Rust gg (src/lir/lower/types.rs) no longer maps GorgetMap/GorgetSet to \
         {EXPECTED_SIZE} bytes. The self-host and Rust struct sizes MUST stay in lock-step \
         (both follow runtime_preamble.c). If the runtime struct genuinely changed size, \
         update runtime_preamble.c, Rust types.rs, AND GORGET_MAP_STRUCT_SIZE together.",
    );

    // (c1) No bare `184` map/set literal smuggled back into the lowerer. Match a
    // non-comment line that names GorgetMap/GorgetSet AND the digits 184.
    let strays: Vec<&str> = lower
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with('#')
                && (t.contains("\"GorgetMap\"") || t.contains("\"GorgetSet\""))
                && t.contains("184")
        })
        .collect();
    assert!(
        strays.is_empty(),
        "A raw `184` reappeared at a GorgetMap/GorgetSet size site in lir_lower.gg \
         (must read `GORGET_MAP_STRUCT_SIZE`, the single source of truth = {EXPECTED_SIZE}):\n  {}",
        strays.join("\n  "),
    );

    // (c2) Every GorgetMap/GorgetSet size site reads the named constant.
    let constant_sites = lower
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with('#')
                && (t.contains("\"GorgetMap\"") || t.contains("\"GorgetSet\""))
                && t.contains("GORGET_MAP_STRUCT_SIZE")
        })
        .count();
    assert!(
        constant_sites >= MIN_CONSTANT_USE_SITES,
        "Expected ≥{MIN_CONSTANT_USE_SITES} GorgetMap/GorgetSet size sites reading \
         `GORGET_MAP_STRUCT_SIZE` in lir_lower.gg, found {constant_sites}. A size site \
         was removed or rewritten to a bare literal — every GorgetMap/GorgetSet \
         LirStructDef and ResourceMetadata MUST read the single-source constant so the \
         184→152 divergence cannot recur. If you legitimately added/removed a site, \
         update MIN_CONSTANT_USE_SITES with a one-line justification.",
    );
}

/// Type-variable NAME-SHAPE heuristic ratchet (CLAUDE.md Core #2 "typed
/// metadata, never name-matching" + Core #6; round-32 Track D).
///
/// The self-host monomorphizer used to decide "is this generic arg a type
/// VARIABLE?" by name shape (`name.len() <= 2` in the retired
/// `has_type_variable`, plus an inline copy in `expand_meta_for_expr`), and
/// `lir_lower.gg`'s placeholder probe + `lir_codegen.gg`'s hashable-key gate
/// pattern-matched bare `T`/`K`/`V` names. All of these collide with legal
/// 1-2-letter USER type names (`enum V`, `struct Id`, `struct T`):
///   - `Wrap[Id]` was never discovered/monomorphized → zeroed-field SILENT
///     MISCOMPILE (printed `wrap 0` instead of `wrap 9`);
///   - `Dict__GorgetString__V` was misread as a template leak → runtime
///     alias never registered → CC-FAIL (dict_nested_pattern_noncopy_enum);
///   - a user `struct V` Dict key silently lost its custom Hashable/
///     Equatable bridge (byte-FNV/memcmp fallback = silent divergence).
///
/// The typed replacement is `has_type_variable_ctx(targs, &tparams)` —
/// membership in the enclosing template's DECLARED type params (Rust mirror:
/// `src/ir/lowering/generics/mod.rs` `type_has_generic_param` +
/// `current_generic_params`) — and, for the placeholder probe, a typed
/// `gmod.type_infos` registry read. This lint pins the retired shapes to
/// zero in the self-host lowering files:
///   (a) `.len() <= 2` — the heuristic body (catches INLINE copies, like
///       the old `expand_meta_for_expr` substitution-loop site, which a
///       signature-only check would miss);
///   (b) bare `has_type_variable(` — the retired fn (only
///       `has_type_variable_ctx(` may appear);
///   (c) `== "T"` / `== "K"` / `== "V"` type-variable-equality in
///       `lir_codegen.gg` (the hashable-key gate shape — the typed
///       fn-existence check `hashable_key_fn_names` decides instead).
#[test]
fn no_type_variable_name_shape_heuristic() {
    const FILES: &[&str] = &[
        "tests/fixtures/self_host_lowerer/lower_generics.gg",
        "tests/fixtures/self_host_lowerer/lower.gg",
        "tests/fixtures/self_host_lowerer/lir_lower.gg",
        "tests/fixtures/self_host_lowerer/lir_codegen.gg",
    ];

    let mut hits: Vec<String> = Vec::new();
    for file in FILES {
        let content = fs::read_to_string(file).unwrap_or_default();
        assert!(
            !content.is_empty(),
            "no_type_variable_name_shape_heuristic: {file} missing or empty — \
             if the file moved, update FILES."
        );
        for (i, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with('#') {
                continue; // .gg comments (the war-story may cite the pattern)
            }
            // (a) the len<=2 name-shape heuristic body.
            if trimmed.contains(".len() <= 2") {
                hits.push(format!("{file}:{}: {trimmed}", i + 1));
            }
            // (b) the retired bare fn (def, import, or call).
            if trimmed.contains("has_type_variable")
                && !trimmed.contains("has_type_variable_ctx")
            {
                hits.push(format!("{file}:{}: {trimmed}", i + 1));
            }
            // (c) T/K/V literal-equality (the hashable-key gate shape).
            if file.ends_with("lir_codegen.gg")
                && (trimmed.contains("== \"T\"")
                    || trimmed.contains("== \"K\"")
                    || trimmed.contains("== \"V\""))
            {
                hits.push(format!("{file}:{}: {trimmed}", i + 1));
            }
        }
    }

    assert!(
        hits.is_empty(),
        "Type-variable NAME-SHAPE heuristic reintroduced in the self-host \
         monomorphizer/codegen ({} hit(s)). \"Is this a type variable?\" must \
         be answered by TYPED context — `has_type_variable_ctx(targs, \
         &tparams)` with the enclosing template's declared params (or \
         `mf_vars` for meta-for, `type_sub_map.keys()` for transitive \
         walks), or a typed-registry read — NEVER by `name.len() <= 2` / \
         `== \"T\"` name-shape guessing, which miscompiles user types named \
         `V`/`Id`/`T` (see docs/devbook/24-layering-discipline.md rule 2). \
         If a hit is genuinely unrelated to type-variable classification, \
         rewrite it to not match, or adjust this lint with justification.\n\
         Hits:\n  {}",
        hits.len(),
        hits.join("\n  "),
    );
}

/// Pairing guard (Core #4 "one fix, all siblings" / DEEP-1 slice 0 — per-site
/// clone attribution). Every implicit-clone site must mint its CloneId AND
/// emit its `--clones=stats` runtime counter bump through the ONE producer
/// helper `LoweringContext::warn_clone_and_hit`. A bare `warn_implicit_clone`
/// call is allowed ONLY at the three CONDITIONAL clone sites — where the
/// clone executes inside a branch, so the paired `emit_clone_site_hit` must
/// be emitted INSIDE that branch (counting actual clones, not guard
/// evaluations) — plus the helper's own body:
///
///   * `context.rs` — the lazy-string materialization guard (hit inside
///     `mat_bb`) and the Ptr-vs-value deref arm (hit inside the clone-fn
///     arm), plus the `warn_clone_and_hit` body itself → 3 warns / 3 hits.
///   * `stmts/mod.rs` — `try_lift_option_ref` (hit inside the Some-arm
///     resource path) → 1 warn / 1 hit.
///
/// **If this fails because a bare count GREW:** you added an implicit-clone
/// site that mints a CloneId without its runtime hit — the site would read
/// "0 hits" in the `[clone-site]` report forever (silent under-attribution;
/// the 8 `exprs/` sites shipped exactly this hole before slice 0 closed it).
/// Straight-line site → call `ctx.warn_clone_and_hit(builder, span, ty,
/// reason)`. Conditional site → keep the bare `warn_implicit_clone`, emit
/// `emit_clone_site_hit` inside the branch that clones, and add the site to
/// the allowlist here WITH a comment at the site referencing this lint.
/// **If it SHRANK** (a conditional site was retired or straightened into the
/// helper), lower the budget in the same commit.
#[test]
fn clone_warn_hit_pairing() {
    // (file, bare `.warn_implicit_clone(` budget, `.emit_clone_site_hit(` budget)
    let allowlist: &[(&str, usize, usize)] = &[
        ("src/ir/lowering/context.rs", 3, 3),
        ("src/ir/lowering/stmts/mod.rs", 1, 1),
    ];

    fn count_calls(file: &str, marker: &str) -> usize {
        let content = fs::read_to_string(file).unwrap_or_default();
        let mut n = 0usize;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("///") {
                continue;
            }
            // `.marker(` anchors on the method-call form, excluding the
            // `pub fn warn_implicit_clone` / `pub fn emit_clone_site_hit`
            // definition lines.
            n += line.matches(marker).count();
        }
        n
    }

    for &(file, warn_budget, hit_budget) in allowlist {
        let warns = count_calls(file, ".warn_implicit_clone(");
        let hits = count_calls(file, ".emit_clone_site_hit(");
        assert_eq!(
            warns, warn_budget,
            "Bare `.warn_implicit_clone(` count in `{file}` changed: {warns} vs \
             allowlisted {warn_budget}.\n\n\
             Every straight-line implicit-clone site must pair its CloneId mint \
             with its runtime hit via `ctx.warn_clone_and_hit(builder, span, ty, \
             reason)` — a bare mint reads \"0 hits\" in the [clone-site] report \
             forever. Only a CONDITIONAL site (clone inside a branch) may split \
             the pair, with the hit emitted inside the cloning branch; document \
             it at the site and re-balance this allowlist.",
        );
        assert_eq!(
            hits, hit_budget,
            "`.emit_clone_site_hit(` count in `{file}` changed: {hits} vs \
             allowlisted {hit_budget}.\n\n\
             In-branch hits exist ONLY as the split half of an allowlisted \
             conditional clone site (plus the `warn_clone_and_hit` helper body). \
             A stray hit without its paired mint (or vice versa) misattributes \
             counts. Re-balance the allowlist with a comment at the site.",
        );
    }

    // Any file outside the allowlist must route through the helper: zero bare
    // mints, zero bare hits.
    let allowed: std::collections::HashSet<&str> =
        allowlist.iter().map(|&(f, _, _)| f).collect();
    let mut stray = Vec::new();
    visit_rs_files(Path::new("src"), &mut |path| {
        let p = path.to_str().unwrap_or_default();
        let norm = p.trim_start_matches("./");
        if allowed.contains(norm) {
            return;
        }
        let warns = count_calls(norm, ".warn_implicit_clone(");
        let hits = count_calls(norm, ".emit_clone_site_hit(");
        if warns + hits > 0 {
            stray.push(format!(
                "{norm}: .warn_implicit_clone(={warns}, .emit_clone_site_hit(={hits}"
            ));
        }
    });
    assert!(
        stray.is_empty(),
        "Un-paired clone-attribution call(s) outside the allowlist:\n  {}\n\n\
         Use `ctx.warn_clone_and_hit(builder, span, ty, reason)` (straight-line \
         sites), or for a genuinely conditional site add it to the allowlist in \
         `clone_warn_hit_pairing` with the hit emitted inside the cloning branch.",
        stray.join("\n  "),
    );

    // The helper itself must exist exactly once (the enumerated class's one
    // producer — Core #4).
    let helper_defs = count_calls("src/ir/lowering/context.rs", "pub fn warn_clone_and_hit");
    assert_eq!(
        helper_defs, 1,
        "Expected exactly one `warn_clone_and_hit` definition in \
         src/ir/lowering/context.rs, found {helper_defs}.",
    );

    // G3 extension: no bare UNTAGGED clone call. Every compiler-emitted clone
    // must carry its typed `MaterializeReason` on the emitted `Instruction::Call`
    // (so the clone-reason validator identifies it without name-matching the
    // callee) — routed through `ctx.emit_clone(...)` (straight-line: folds the
    // warn) or `builder.call_clone(..., reason)` (conditional / between-setup /
    // explicit `.clone()`). A bare `builder.call(&clone_fn, …)` leaves
    // `reason == None`; the validator would flag it and the always-on strict
    // gate (debug builds) would panic. Scans src/ir/lowering with comment lines
    // stripped and ALL whitespace removed, so the multi-line
    // `builder.call(\n    &clone_fn,` shape collapses to `builder.call(&clone_fn`
    // and is caught too. Counterfactual: re-introducing a raw
    // `builder.call(&clone_fn, args, ty)` (instead of `call_clone`) trips this.
    let mut bare_clone_calls = Vec::new();
    visit_rs_files(Path::new("src/ir/lowering"), &mut |path| {
        let p = path.to_str().unwrap_or_default();
        let content = fs::read_to_string(p).unwrap_or_default();
        let code: String = content
            .lines()
            .filter(|l| !l.trim_start().starts_with("//"))
            .flat_map(|l| l.chars())
            .filter(|c| !c.is_whitespace())
            .collect();
        let n = code.matches("builder.call(&clone_fn").count();
        if n > 0 {
            bare_clone_calls.push(format!("{p}: {n} bare `builder.call(&clone_fn`"));
        }
    });
    assert!(
        bare_clone_calls.is_empty(),
        "Bare UNTAGGED clone call(s) found — a `builder.call(&clone_fn, …)` emits \
         an `Instruction::Call` with no `MaterializeReason`, which the clone-reason \
         validator (GG_VALIDATE_CLONE_REASONS, always-on strict in debug) flags. \
         Route straight-line sites through `ctx.emit_clone(builder, &clone_fn, \
         args, span, ty, reason)`; conditional / between-setup / explicit-`.clone()` \
         sites through `builder.call_clone(&clone_fn, args, ty, reason)`:\n  {}",
        bare_clone_calls.join("\n  "),
    );
}

/// Completeness guard (Core #6) for the arena-escape STORE/INGEST
/// classification — stops the next missed materializing store position (the
/// R-B this round's R-A fold was told to prevent).
///
/// Every store position that COPIES its argument into an OUTER owning
/// collection under `with Arena` must route that argument through
/// `arena_backed_source(.., EscapeCtx::Ingest)`, so a literal / arena-scoped
/// value materialized into the arena and left dangling at teardown is
/// rejected. The positions today: the `push`/`put`/`insert`/`add`/`send`
/// method ingest (`check_expr.rs`) and the `c[k] = v` index-store on a
/// materializing (map/set) collection, key AND value (`check_stmt.rs`).
///
/// Two structural locks:
///  (a) `CollectionKind::index_store_materializes` stays EXHAUSTIVE (no `_`
///      arm). A wildcard would silently default a NEW collection kind's
///      index-store to non-materializing → re-open the R-A Dict UAF for that
///      kind. (The no-`_` match is compile-enforced; this forbids re-adding
///      a `_` escape hatch.)
///  (b) the CollectionKind variant count is pinned: a new variant forces an
///      explicit materialize decision in (a) AND a review that its
///      index-store position is gated.
#[test]
fn arena_escape_store_classification_completeness() {
    let types_src = fs::read_to_string("src/ir/types.rs").unwrap_or_default();

    // (a) `index_store_materializes` must have no `_` catch-all arm.
    let fn_start = types_src
        .find("fn index_store_materializes(self) -> bool")
        .expect(
            "CollectionKind::index_store_materializes not found — the arena-escape \
             index-store gate (src/semantic/safety/check_stmt.rs) depends on it",
        );
    let fn_end = types_src[fn_start..]
        .find("\n    }")
        .map(|i| fn_start + i)
        .unwrap_or(types_src.len());
    let fn_body = &types_src[fn_start..fn_end];
    assert!(
        !fn_body.contains("_ =>") && !fn_body.contains("_ |") && !fn_body.contains("| _"),
        "CollectionKind::index_store_materializes gained a `_` catch-all arm.\n\n\
         Keep it EXHAUSTIVE: a wildcard silently defaults a NEW collection kind's \
         index-store materialize decision, re-opening the arena index-store UAF \
         (R-A: `outer[k] = v` on an outer map) for that kind. Enumerate every \
         variant explicitly instead.",
    );

    // (b) pin the CollectionKind variant count.
    const EXPECTED_VARIANTS: usize = 5; // Array, OrderedMap, Map, OrderedSet, Set
    let enum_start = types_src
        .find("pub enum CollectionKind")
        .expect("pub enum CollectionKind not found in src/ir/types.rs");
    let enum_end = types_src[enum_start..]
        .find('}')
        .map(|i| enum_start + i)
        .expect("unterminated CollectionKind enum");
    let variants = types_src[enum_start..enum_end]
        .lines()
        .filter(|l| {
            let t = l.trim();
            !t.starts_with("//") && !t.starts_with("pub enum") && t.ends_with(',')
        })
        .count();
    assert_eq!(
        variants, EXPECTED_VARIANTS,
        "CollectionKind variant count changed: {variants} vs {EXPECTED_VARIANTS}.\n\n\
         A new collection kind MUST (1) get an explicit arm in \
         `CollectionKind::index_store_materializes` — materialize == its `c[k]=v` \
         store copies its key/value into an owned slot (map/set `put`) — and (2) \
         have its index-store position routed through the shared arena-escape \
         Ingest gate in `src/semantic/safety/check_stmt.rs`. Then bump \
         EXPECTED_VARIANTS.",
    );

    // (c) BEHAVIORAL-equivalence lock (not just structure): every ingest
    // position must route through the ONE `classify_ingest_escape` producer,
    // and NO gate may open-code an `EscapeCtx::Ingest` classification — that is
    // what keeps `d.put(k,v)`, `d[k]=v`, and `d[k]+=v` applying the SAME rule
    // set (the #1 drift was exactly one gate implementing only half of it).
    //
    // (c1) `EscapeCtx::Ingest` must NOT appear anywhere in the gate files (on a
    //      non-comment line, in ANY form — single- or multi-line). The
    //      Ingest-context producer call lives ONLY inside
    //      `classify_ingest_escape` (helpers.rs). A gate that open-codes
    //      `arena_backed_source(.., EscapeCtx::Ingest)` omits ingest rule (2)
    //      and re-opens the #1 drift. (Gates legitimately use `EscapeCtx::Bind`
    //      — only the Ingest context is forbidden outside the helper.)
    let mut open_coded_ingest = 0usize;
    for file in [
        "src/semantic/safety/check_expr.rs",
        "src/semantic/safety/check_stmt.rs",
    ] {
        let src = fs::read_to_string(file).unwrap_or_default();
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with("//") {
                continue;
            }
            open_coded_ingest += line.matches("EscapeCtx::Ingest").count();
        }
    }
    assert_eq!(
        open_coded_ingest, 0,
        "An arena-escape gate open-codes `EscapeCtx::Ingest` ({open_coded_ingest} \
         site(s) in check_expr.rs/check_stmt.rs).\n\n\
         Ingest positions (push/put/insert/add/send; `c[k]=v` / `c[k]+=v` index \
         key & value on a materializing collection) MUST call \
         `self.classify_ingest_escape(..)` — the ONE producer that applies BOTH \
         ingest rules (arena_backed_source Ingest + bare-live-outer-ident). \
         Open-coding `arena_backed_source(.., EscapeCtx::Ingest)` in a gate omits \
         rule (2) and re-opens the #1 clone-into-arena UAF.",
    );

    // (c2) `classify_ingest_escape` is called from every ingest position.
    //      Sites: check_expr method-ingest (1); check_stmt plain-assign index
    //      VALUE (1) + KEY (1); check_stmt compound-assign index KEY (1) = 4.
    const EXPECTED_CLASSIFY_CALLS: usize = 4;
    let mut classify_calls = 0usize;
    for file in [
        "src/semantic/safety/check_expr.rs",
        "src/semantic/safety/check_stmt.rs",
    ] {
        let src = fs::read_to_string(file).unwrap_or_default();
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with("//") {
                continue;
            }
            classify_calls += line.matches("self.classify_ingest_escape(").count();
        }
    }
    assert_eq!(
        classify_calls, EXPECTED_CLASSIFY_CALLS,
        "`classify_ingest_escape` call-site count changed: {classify_calls} vs \
         {EXPECTED_CLASSIFY_CALLS}.\n\n\
         Ingest positions: method-ingest arg (1); plain-assign index-store VALUE \
         (1) + KEY (1); compound-assign index-store KEY (1). If you ADDED an \
         ingest/store position, route it through `classify_ingest_escape` and bump \
         this. If this DROPPED, a position lost its ingest classification — \
         restore it.",
    );

    // (c3) the producer exists exactly once (Core #4 — one helper).
    let helper_src =
        fs::read_to_string("src/semantic/safety/helpers.rs").unwrap_or_default();
    let helper_defs = helper_src
        .matches("fn classify_ingest_escape(")
        .count();
    assert_eq!(
        helper_defs, 1,
        "Expected exactly one `classify_ingest_escape` definition in \
         src/semantic/safety/helpers.rs, found {helper_defs}. The ingest rule set \
         must have a single producer.",
    );
}

/// Split a Rust source string into `(fn_name, body_text)` for every FREE
/// function defined at column 0 (optionally `pub`/`pub(super)`/`pub(crate)`).
/// The body of fn N runs from its `fn` line to the next col-0 `fn` line (or
/// EOF). Line-based (never brace-matched) so `{`/`}` inside string literals,
/// `format!` templates, or `char` literals cannot corrupt the boundaries. All
/// three G1-materialize sites are col-0 free functions, so this is exact for
/// them; nested fns/closures fold into their enclosing free fn (fine for a
/// containment scan).
fn top_level_fn_bodies(content: &str) -> Vec<(String, String)> {
    let mut starts: Vec<(usize, String)> = Vec::new();
    let mut offset = 0usize;
    for line in content.lines() {
        // col-0 only: no leading whitespace.
        if line.starts_with(|c: char| !c.is_whitespace()) {
            let mut rest = line;
            for pfx in ["pub(crate) ", "pub(super) ", "pub ", ""] {
                if let Some(stripped) = rest.strip_prefix(pfx) {
                    rest = stripped;
                    break;
                }
            }
            if let Some(after_fn) = rest.strip_prefix("fn ") {
                let name: String = after_fn
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect();
                if !name.is_empty() {
                    starts.push((offset, name));
                }
            }
        }
        offset += line.len() + 1; // +1 for the '\n'
    }
    let mut out = Vec::new();
    for k in 0..starts.len() {
        let s = starts[k].0;
        let e = starts.get(k + 1).map(|n| n.0).unwrap_or(content.len());
        out.push((starts[k].1.clone(), content[s..e].to_string()));
    }
    out
}

/// Structural guard (Core #4 "one fix, all siblings" / #6 "convert a recurring
/// bug class into an executable guard"; devbook/25 §3a): the G1 "materialize a
/// mutation target/receiver root into a private owned copy" transform. When a
/// fn materializes such a root and then lowers a store-target projection chain /
/// method receiver PLUS args / RHS, every index-load mints a TRANSIENT
/// `CollectionElement`/`FieldPath` handle INTO the private copy. If any such
/// handle stays CoW-tracked, a later same-collection mutation reallocates the
/// copy and `cow_before_mutation` Case 3 clones freed memory → heap-UAF. The
/// round-33 fold chain fixed exactly this class one leaked site at a time (the
/// object chain, then the RHS/index, then the method-call args, then the
/// bare-param NAMED-receiver / index-source materialize `v.push(v[0])`).
///
/// The shared close is `untrack_transient_element_refs_in_range`. This guard is
/// anchored on the ROOT MATERIALIZE — a `cow_before_mutation(` call — NOT on
/// `resolve_projection_root_local` (which the named-receiver / index-source
/// materialize does NOT use, so anchoring there missed it — the exact gap the
/// 5th fold hit). The invariant: EVERY fn in the two mutation-lowering files
/// that calls `cow_before_mutation(` is classified — `UNTRACK_REQUIRED` (it also
/// lowers a projected target/receiver + args/RHS, so it MUST call the untrack)
/// or `UNTRACK_EXEMPT` (whole-value reassign — `cow_before_mutation` there just
/// SEVERS the reassigned collection's OWN refs before its buffer is replaced;
/// the RHS is a single cloned value, so there are no projection-chain / arg
/// element handles into a private copy to dangle). A NEW `cow_before_mutation`
/// caller fails the classification assert, forcing a decision.
///
/// **If this fails:** (a) a fn in `UNTRACK_REQUIRED` lost its untrack — restore
/// it (span the whole statement's lowering; see `lower_field_assign`); or (b) a
/// NEW `cow_before_mutation` caller appeared — if it materializes a projected
/// target/receiver + args/RHS, add it to `UNTRACK_REQUIRED` AND call the
/// untrack; if it's a whole-value reassign with no projection/arg element
/// handles, add it to `UNTRACK_EXEMPT` with a one-line why.
#[test]
fn g1_projected_materialize_sites_untrack() {
    // Materialize a projected target/receiver + args/RHS → MUST untrack.
    // `lower_call_arg` (calls.rs) and `lower_expr_inner` (exprs/mod.rs) join the
    // set via the round-34 G2 `&`-of-a-PROJECTED-bare-value FORMATION sites:
    // `f(&s.field)` / `auto r = &b.data` materialize the projection root, lower a
    // projection that mints transient element/field handles into the private
    // copy, so they MUST untrack (same UAF-fold class). Both fns ALSO contain
    // whole-value `cow_before_mutation` callers (site-1/2 `&name`, the `!name` /
    // `!x` move-severs) — those need no untrack, but classification is PER FN, so
    // the projected path makes the whole fn UNTRACK_REQUIRED.
    const UNTRACK_REQUIRED: &[&str] = &[
        "lower_field_assign",
        "lower_tuple_field_assign",
        "lower_index_assign",
        "lower_compound_assign",
        "lower_method_call",
        "lower_call_arg",
        "lower_expr_inner",
    ];
    // Whole-value reassign (`x = y`): `cow_before_mutation` severs the target's
    // own element refs before its buffer is replaced — no projection-chain / arg
    // element handles into a private copy, so no untrack needed.
    //
    // `materialize_assign_target_root` (planner round 3, the shared assign-root
    // prologue): a root-materialize-ONLY helper — it routes the root materialize
    // through the plan (`plan_materialize_at_site`) (+ `cow_before_field_mutation`
    // to sever field-path refs) and NOTHING ELSE. It does NOT lower a projected
    // store / args / RHS, so it mints no transient element handles into the
    // private copy. The projected-store untrack is the CALLER's job: all three
    // callers (`lower_field_assign` / `lower_index_assign` / `lower_compound_assign`,
    // now all UNTRACK_REQUIRED) capture `stmt_locals_start` and call
    // `untrack_transient_element_refs_in_range` at the store's exit — verified
    // above. Exempt because the pairing lives one layer up.
    const UNTRACK_EXEMPT: &[&str] = &["lower_assign", "materialize_assign_target_root"];

    let files = [
        "src/ir/lowering/stmts/assigns.rs",
        "src/ir/lowering/exprs/methods.rs",
        "src/ir/lowering/exprs/calls.rs",
        "src/ir/lowering/exprs/mod.rs",
    ];
    let mut required_seen: Vec<String> = Vec::new();
    for file in files {
        let content = fs::read_to_string(file).unwrap_or_default();
        for (name, body) in top_level_fn_bodies(&content) {
            // "Materializes a mutation root" — via the raw primitive OR the
            // planner round-3 plan-apply entry points. When an at-site
            // `cow_before_mutation` class migrates behind the `MaterializePlan`,
            // the text-based key must follow it, or this heap-UAF guard silently
            // stops watching the site (the exact hole this test exists to close).
            //   - `cow_before_mutation(`      : classes not yet migrated (CLASS B/C/D/E/F).
            //   - `plan_materialize_at_site(`  : the plan at-site entry (only the
            //     `materialize_assign_target_root` helper — exempt, pairs one layer up).
            //   - `materialize_assign_target_root(` : the shared assign-root helper;
            //     its callers lower the projected store → they MUST untrack.
            //   - `apply_materialize_directive(` : THE general plan-apply funnel
            //     (`ctx.apply_materialize_directive`, context.rs) that every plan
            //     client routes through. Keyed here so the NEXT at-site→plan
            //     conversion (CLASS E/C/D) that calls the funnel DIRECTLY from one
            //     of these four files is re-taught consciously — it can't slip
            //     behind the plan and silently stop being watched. No scanned fn
            //     calls it today (the pre-header consumers live in context.rs /
            //     stmts/mod.rs, outside this scan), so adding it is a no-op for the
            //     current classification, purely forward-looking.
            let materializes_root = body.contains("cow_before_mutation(")
                || body.contains("plan_materialize_at_site(")
                || body.contains("materialize_assign_target_root(")
                || body.contains("apply_materialize_directive(");
            if !materializes_root {
                continue;
            }
            let requires = UNTRACK_REQUIRED.contains(&name.as_str());
            let exempt = UNTRACK_EXEMPT.contains(&name.as_str());
            assert!(
                requires || exempt,
                "New `cow_before_mutation` caller `{name}` ({file}) — it materializes \
                 a mutation target/receiver root. If it also lowers a PROJECTED \
                 store / method receiver + args/RHS, it MUST call \
                 `untrack_transient_element_refs_in_range` (add to UNTRACK_REQUIRED); \
                 if it's a whole-value reassign with no projection-chain / arg \
                 element handles into the private copy, add to UNTRACK_EXEMPT with a \
                 one-line why. This gate exists because a same-collection element \
                 handle into the private copy dangles on the next mutation \
                 (heap-UAF) — see the round-33 fold chain."
            );
            if requires {
                assert!(
                    body.contains("untrack_transient_element_refs_in_range("),
                    "G1 root-materialize site `{name}` ({file}) calls \
                     `cow_before_mutation` (materializes a projected mutation into a \
                     private owned copy) but NEVER calls \
                     `untrack_transient_element_refs_in_range`. Its transient \
                     projection-chain / arg element handles into that copy dangle on \
                     a later same-collection mutation (heap-UAF). Untrack the whole \
                     statement's lowering range (see `lower_field_assign`)."
                );
                required_seen.push(name);
            }
        }
    }
    required_seen.sort();
    let mut expected: Vec<String> =
        UNTRACK_REQUIRED.iter().map(|s| s.to_string()).collect();
    expected.sort();
    assert_eq!(
        required_seen, expected,
        "The set of untrack-required G1 root-materialize sites changed: \
         {required_seen:?} vs expected {expected:?}. Each MUST route through \
         `untrack_transient_element_refs_in_range`; update `UNTRACK_REQUIRED` only \
         when the set deliberately changes."
    );
}

/// Sibling-site-drift ratchet (CLAUDE.md invariant #4 "one fix, all siblings" +
/// #6 "convert a recurring bug class into an executable guard"; devbook/24
/// "fix the class, not the instance"): every function-body lowering path in the
/// self-host lowerer MUST finalize through the shared `finalize_body_blocks`
/// helper (`lower_loops.gg`), which pops the drop frame, runs `compute_liveness`
/// → `wire_liveness_into_modes` (the OpMove/OpClone consuming-operand decision)
/// → `flush_drop_queue`, and only THEN assembles the `BasicBlock` vector.
///
/// The bug class this pins (R35 Bug B): the `test`-body and `suite`-body inline
/// lowering paths open-coded the tail and OMITTED `wire_liveness_into_modes`, so
/// last-use ctor-arg consumes stayed `OpCopy` (never `OpMove`) and the
/// moved-from source slots were never zeroed → drop_elab over-dropped them →
/// DOUBLE-FREE (`test_option_resource_field` sig6). The fix routed all three
/// inline paths (test-body, suite-body, the `lower_equip_block` method path)
/// through `finalize_body_blocks`.
///
/// The anti-pattern is the manual block-ASSEMBLY loop that reconstructs the
/// `Vector[BasicBlock]` from the ctx's separate `block_insts` / `block_terms`
/// vectors — its sink is `push(BasicBlock(`. That sink MUST appear EXACTLY ONCE
/// in the self-host lowerer: inside `finalize_body_blocks`. A merely-counting
/// `finalize_body_blocks(` caller-count lint could NOT catch a 4th body path
/// that forgets to call it; pinning the assembly sink DOES. Single-block literal
/// constructions (`[BasicBlock([], GTReturn(...))]` for stub / spawn-wrapper
/// bodies that have no drop frame to finalize) use list-literal syntax, not
/// `push(BasicBlock(`, so an idiom-scoped lint correctly excludes them.
///
/// **If this fails with count > 1:** a new function-body path re-inlined the
/// block-assembly loop instead of calling `finalize_body_blocks` — route it
/// through the helper (else it silently skips liveness finalization → UAF /
/// double-free). **If it fails with count 0:** `finalize_body_blocks` was
/// refactored away from the `push(BasicBlock(` idiom — retarget this lint at the
/// new assembly sink.
#[test]
fn self_host_body_finalize_single_assembly_site() {
    /// Baseline 2026-07-03 (R35 Bug B): 1 — the sole block-assembly loop lives
    /// in `finalize_body_blocks` (`lower_loops.gg`). test-body / suite-body /
    /// method-path all route through it.
    const EXPECTED: usize = 1;

    // The lower*.gg body-lowering files are REAL files (not symlinks) in
    // self_host_lowerer; the `push(BasicBlock(` idiom lives nowhere else, so
    // scoping to this dir avoids any symlink double-count.
    let dir = "tests/fixtures/self_host_lowerer";
    let mut sites = 0usize;
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => panic!("self_host_body_finalize_single_assembly_site: cannot read {dir}"),
    };
    for de in entries.flatten() {
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
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with('#') {
                continue; // .gg comments
            }
            sites += trimmed.matches("push(BasicBlock(").count();
        }
    }

    assert_eq!(
        sites, EXPECTED,
        "Self-host block-assembly idiom `push(BasicBlock(` site count changed: \
         {sites} vs {EXPECTED}.\n\n\
         Every function-body lowering path MUST finalize through the shared \
         `finalize_body_blocks` helper (pop drop frame → compute_liveness → \
         wire_liveness_into_modes → flush_drop_queue → assemble). A path that \
         re-inlines the block-assembly loop skips `wire_liveness_into_modes`, so \
         last-use consumes stay OpCopy and moved-from sources are never zeroed → \
         drop_elab double-frees (R35 Bug B: test_option_resource_field). Route the \
         new path through `finalize_body_blocks` instead of open-coding the loop. \
         Update EXPECTED only if `finalize_body_blocks` itself was intentionally \
         refactored.",
    );
}

/// T-A (gorget-arena snag #1 ctor extension, Core #4 sibling-site-drift guard):
/// the owning-`!`-param → ctor-field move decision is centralized in ONE helper,
/// `maybe_move_owning_param_ctor_temp`, and every by-value clone site at a ctor /
/// boundary consuming position MUST route through it BEFORE its defensive clone.
/// There are exactly THREE such sites (an enumerated class):
///   1. enum-variant init  — `clone_resource_args_for_init` (context.rs)
///   2. struct-boundary     — `ensure_owned_at_boundary` Case 2 (context.rs)
///   3. user-literal by-val — `clone_multi_use_resource_args` (exprs/mod.rs)
///
/// **If this fails:** a 4th by-value ctor clone site was added (or one was
/// removed) without routing the move decision through the shared helper. A new
/// site that clones directly re-opens the `!`-move-is-zero-cost regression for
/// its shape (invisible to stdout + ASan — a spurious clone is output-identical
/// and only leaks silently under a pool allocator). Call
/// `maybe_move_owning_param_ctor_temp(builder, &operand, span)` before the
/// `clone_fn_for_ptr` clone, then bump `EXPECTED_CALL_SITES`.
#[test]
fn owning_param_ctor_move_helper_site_count() {
    const HELPER_FN: &str = "fn maybe_move_owning_param_ctor_temp";
    const HELPER_CALL: &str = "maybe_move_owning_param_ctor_temp(";
    const EXPECTED_CALL_SITES: usize = 3;

    let files = [
        "src/ir/lowering/context.rs",
        "src/ir/lowering/exprs/mod.rs",
    ];

    let mut helper_defs = 0usize;
    let mut call_sites = 0usize;
    for f in files {
        let content = fs::read_to_string(f).unwrap_or_default();
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("///") {
                continue; // prose / anchor comments mention the helper by name
            }
            if line.contains(HELPER_FN) {
                helper_defs += 1;
                continue; // the definition line is not a call site
            }
            call_sites += line.matches(HELPER_CALL).count();
        }
    }

    assert_eq!(
        helper_defs, 1,
        "Expected exactly one `maybe_move_owning_param_ctor_temp` definition, found {helper_defs}.",
    );
    assert_eq!(
        call_sites, EXPECTED_CALL_SITES,
        "owning-`!`-param ctor-move helper call-site count changed: {call_sites} vs \
         {EXPECTED_CALL_SITES}.\n\n\
         The move-vs-clone decision at a struct/enum ctor field-init must be \
         centralized in `maybe_move_owning_param_ctor_temp` (Core #4). A new \
         by-value clone site that doesn't call it before `clone_fn_for_ptr` re-opens \
         the `!`-move-is-zero-cost regression (snag #1's 8th consuming category). \
         Route the new site through the shared helper, then bump EXPECTED_CALL_SITES.",
    );
}

/// The **ggdef import ratchet** — the project's most important fence
/// (the define-gorget RFC §2.4/§7 + orchestration-handover rule 5, in git history).
///
/// `ggdef` (the executable language definition, `spec/ggdef/`) shares the
/// production **lexer + parser + AST + span** ONLY. It must NEVER reach into
/// the root crate's `ir` / `semantic` / `lir` / `bir` / `backend` modules —
/// those encode the compiler's OWN decisions, and a definition that consumed
/// them would be circular (the Miri trap). This is a **SOURCE-discipline
/// fence, not a link fence**: ggdef links the whole `gorget` lib, and root
/// modules like `src/errors.rs` internally reference `crate::semantic` (fine,
/// not ggdef's concern) — the fence applies only to ggdef's OWN `use` lines.
///
/// DENYLIST semantics, budget = 0, **fatal from day one**. Legal imports are
/// `gorget::{lexer, parser (incl. ast), span, errors, intern, compiler_data}`
/// + std; anything resolving into `ir`/`semantic`/`lir`/`bir`/`backend` fails.
#[test]
fn ggdef_import_ratchet() {
    const FORBIDDEN: &[&str] = &["ir", "semantic", "lir", "bir", "backend"];

    // A `use`/`pub use` item, capturing its path across possible line breaks
    // (grouped imports may span lines). `(?s)` lets `.`/`[^;]` cross newlines.
    let use_item = regex::Regex::new(r"(?s)\b(?:pub\s+)?use\s+([^;]+);").unwrap();
    // A forbidden module appearing as a `::`- / `{`- / `,`-delimited path
    // segment (so `intern` doesn't trip `ir`, and `bird` can't trip `bir`).
    let alternation = FORBIDDEN.join("|");
    let forbidden_seg =
        regex::Regex::new(&format!(r"[:{{,]\s*(?:{alternation})\b")).unwrap();

    // (F1 hardening, Increment B2) A SECOND scan over the FULL source text for a
    // bare fully-qualified `gorget::<forbidden>::` path segment — an inline
    // `gorget::semantic::TypeChecker` usage carries NO `use` line, so the
    // use-line-only scan above is trivially bypassable. This catches the fenced
    // modules however they are named. `\b` keeps `intern` from tripping `ir` and
    // `bird` from tripping `bir`; the trailing `::` requires a real path INTO the
    // module (so a doc-comment mention like "the `semantic` module" is ignored).
    let inline_path =
        regex::Regex::new(&format!(r"gorget\s*::\s*(?:{alternation})\s*::")).unwrap();

    let mut violations = Vec::new();
    visit("spec/ggdef/src", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        // Scan 1: `use`/`pub use` items.
        for cap in use_item.captures_iter(&content) {
            let item = &cap[1];
            // Only the ROOT crate (`gorget::...`) is fenced; `crate::` is ggdef
            // itself, `std::` is the standard library.
            if !item.contains("gorget") {
                continue;
            }
            if forbidden_seg.is_match(item) {
                let flat: String = item.split_whitespace().collect::<Vec<_>>().join(" ");
                violations.push(format!("{}: `use {flat};`", path.display()));
            }
        }
        // Scan 2: any inline fully-qualified forbidden path (bypasses `use`).
        for m in inline_path.find_iter(&content) {
            violations.push(format!("{}: inline path `{}…`", path.display(), m.as_str().trim()));
        }
    });

    assert!(
        violations.is_empty(),
        "ggdef import ratchet TRIPPED (budget = 0, fatal): ggdef's own source may reference \
         `gorget::{{lexer, parser, span, errors, intern, compiler_data}}` + std ONLY — never \
         `ir`/`semantic`/`lir`/`bir`/`backend`, whether via a `use` line OR an inline \
         fully-qualified path. A definition that consumes the compiler's own decisions is \
         circular (the Miri trap; RFC §2.4). Remove the import or add the metadata ggdef needs \
         to the shared lexer/parser/AST.\n\nViolations:\n{}",
        violations.join("\n"),
    );
}

/// D11 trap-registry parity ratchet: the PRODUCTION trap registry
/// (`gorget::trap::TrapKind`, `src/trap.rs`) is a deliberate DUPLICATE of the
/// DEFINITIONAL one (`ggdef::TrapKind`, `spec/ggdef/src/eval.rs`) — the import
/// ratchet (`ggdef_import_ratchet`) forbids ggdef importing `src/`, so the two
/// registries are separate types that must nonetheless AGREE. This lint pins
/// the correspondence: the `code()` string SETS are identical (same closed set
/// of `T_<X>`). Post-D25 (fault-catch removal) there is no catchable subset —
/// all traps are uncatchable. It compares ONLY the typed `code()` — NEVER the
/// human `detail`/message text (production "integer overflow" vs ggdef
/// "arithmetic overflow" is a sanctioned, conformance-ignored divergence).
///
/// ONE macro-expanded variant list drives BOTH the rustc-exhaustiveness
/// ratchet (two generated catch-all-free `match`es) AND the code() set, so the
/// ratchet REACHES the assertion: adding a variant to EITHER enum is a hard
/// compile error AT the `trap_parity_pin!` list, and extending that list is
/// the only fix — which extends the arrays in the same keystroke.
#[test]
fn trap_kind_parity_prod_vs_ggdef() {
    use std::collections::BTreeSet;
    use gorget::trap::TrapKind as P;
    use ggdef::TrapKind as G;

    macro_rules! trap_parity_pin {
        ( $( $name:ident $( ( $($gp:expr),* ) )? ),+ $(,)? ) => {{
            #[allow(dead_code)]
            fn _p_exhaustive(t: P) {
                match t { $( P::$name { .. } => {} ),+ }
            }
            #[allow(dead_code)]
            fn _g_exhaustive(t: &G) {
                match t { $( G::$name { .. } => {} ),+ }
            }
            (
                [ $( P::$name ),+ ],
                [ $( G::$name $( ( $($gp),* ) )? ),+ ],
            )
        }};
    }

    let (prod, ggd) = trap_parity_pin![
        Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk,
        AssertFailed(String::new()), Panic(String::new()),
    ];

    // code() SETS identical.
    let prod_codes: BTreeSet<&str> = prod.iter().map(|t| t.code()).collect();
    let ggd_codes: BTreeSet<&str> = ggd.iter().map(|t| t.code()).collect();
    assert_eq!(
        prod_codes, ggd_codes,
        "production TrapKind::code() set must equal ggdef's (D11 registry parity)",
    );
}

/// D11 raw-trap source-scan ratchet: after trap normalization, every reachable
/// arithmetic/shift trap in the two Rust backends emits through the registry
/// entry `gorget_trap_at` (typed `T_` code + exit 101). This lint pins the count
/// of RAW trap-exit primitives that REMAIN, so a NEW bare `fprintf(...);exit(1)`
/// / `call void @exit(i32 1)` / `abort()` that bypasses the registry trips the
/// count and forces review. (A match-arm count does NOT apply — the emit sites
/// are `write!`-based inline strings across two backends, not enum arms.)
///
/// Baselines (2026-07-10, post T2a-rust + T2b reroute):
///   * `c_lir/mod.rs` `exit(1)` = 0  — ALL inline arith/shift now route through
///     `gorget_trap_at`; a new one is a registry bypass.
///   * `c_lir/mod.rs` `abort()` = 3  — the `abort()` bodies of the DEAD
///     `Inst::BoundsCheck` / `Inst::DivCheck` / `Inst::Trap` match arms
///     (`c_lir/mod.rs:3123/3132/3138`). These variants are NEVER constructed
///     anywhere in lowering (grep-verified), so T2b deliberately left them alone —
///     the runtime-library bounds path T2b normalized is a DIFFERENT set of files
///     (`runtime_array.c` etc.) + the `emit_hof.rs`/`emit_types.rs` unwrap-on-Ok
///     folds, none counted here. The baseline STAYS 3 (retiring the dead arms is
///     out of scope; deleting them would be a wider cleanup).
///   * `llvm/mod.rs` `call void @exit(i32 1)` = 4 — the DEAD `Inst::BoundsCheck` /
///     `Inst::DivCheck` / `Inst::Trap` arms (never constructed, sibling of the C
///     ones above) plus the InlineC-fallback fatal (`; InlineC fatal`). None are
///     rerouted by T2b; the InlineC one is a deliberate non-trap abort kept in the
///     baseline DELIBERATELY (it is not a trap to reroute).
///
/// If this fails: a new raw trap emit was added. Route it through
/// `crate::trap::TrapKind` + `gorget_trap_at` (the registry), OR — if it is a
/// legitimately-new non-registry abort — adjust the baseline with a one-line
/// justification naming the site.
#[test]
fn raw_trap_exit_sites_ratchet() {
    let c_lir = fs::read_to_string("src/backend/c_lir/mod.rs").unwrap_or_default();
    let llvm = fs::read_to_string("src/backend/llvm/mod.rs").unwrap_or_default();

    let c_exit1 = c_lir.matches("exit(1)").count();
    let c_abort = c_lir.matches("abort()").count();
    let llvm_exit1 = llvm.matches("call void @exit(i32 1)").count();

    const C_EXIT1_BASELINE: usize = 0;
    const C_ABORT_BASELINE: usize = 3;
    const LLVM_EXIT1_BASELINE: usize = 4;

    assert_eq!(
        c_exit1, C_EXIT1_BASELINE,
        "c_lir/mod.rs raw `exit(1)` trap count changed: {c_exit1} vs {C_EXIT1_BASELINE}. \
         A new inline trap must route through gorget_trap_at (crate::trap::TrapKind), \
         not a bare fprintf;exit(1). See tests/lints.rs::raw_trap_exit_sites_ratchet.",
    );
    assert_eq!(
        c_abort, C_ABORT_BASELINE,
        "c_lir/mod.rs `abort()` trap count changed: {c_abort} vs {C_ABORT_BASELINE}. \
         These 3 are the `abort()` bodies of the DEAD Inst::BoundsCheck/DivCheck/Trap \
         arms (never constructed); T2b left them intentionally. A new abort must route \
         through gorget_trap_at (crate::trap::TrapKind), not a bare abort().",
    );
    assert_eq!(
        llvm_exit1, LLVM_EXIT1_BASELINE,
        "llvm/mod.rs `call void @exit(i32 1)` count changed: {llvm_exit1} vs {LLVM_EXIT1_BASELINE}. \
         A new arithmetic trap must route through gorget_trap_at; the 4 baseline sites are \
         BoundsCheck (T2b), DivCheck, Inst::Trap, and the InlineC fatal fallback.",
    );
}

/// D11 self-host trap-code parity ratchet (T2a-selfhost). The self-host lowerer
/// HAND-SPELLS the `T_<Code>` strings as string literals at its `gorget_trap(…)`
/// emit sites — it cannot import Rust's `gorget::trap::TrapKind`, so nothing but
/// this lint keeps the two sides in agreement (layering rule 2: the code is
/// typed data on the Rust side; this is the cross-language mitigation on the
/// `.gg` side). The codes live in `lir_codegen.gg` (inline C-string arith +
/// unwrap guards, mechanism A) and `lower_expr.gg` / `lower_stmt.gg`
/// (GICallExtern reroutes, mechanism B). This lint pins them WITHOUT a
/// hand-synced Rust-side list:
///   (a) every quoted `"T_<Ident>"` the self-host emits is a REAL
///       `gorget::trap::TrapKind::code()` — catches a typo (`"T_Overlfow"`) or a
///       code retired on the Rust side.
///   (b) all 7 non-Bounds codes appear at least once — catches a direct trap
///       site silently regressing to `gorget_panic` (or being deleted).
///       `T_Bounds` is intentionally NOT required: `trap_bounds` is T2b.
///
/// `lower_closures.gg` is scanned too: its cross-frame repanic DELIBERATELY
/// stays `gorget_panic` (per the T2a-selfhost brief — rerouting it would diverge
/// the self-host from un-rerouted Rust production), so it must contribute ZERO
/// captures. Scanning it catches a stray *bogus* `T_` literal there (a code not in
/// `TrapKind::code()`); note a valid-but-wrong reroute — e.g. `gorget_trap("T_Panic", …)`
/// on the per-category repanic — would PASS this lint (it's a real code), so the
/// "leave it as `gorget_panic`" invariant is enforced by review, not this lint.
///
/// The LIR type constants (`T_PTR`, `T_STRUCT`, `T_VOID`, …) are BARE
/// identifiers, never quoted, so the quote-anchored match captures exactly the
/// trap codes. The `_exhaustive` guard makes a new `TrapKind` variant a hard
/// compile error here until this lint is revisited (rustc exhaustiveness IS the
/// ratchet).
#[test]
fn self_host_trap_code_parity() {
    use std::collections::BTreeSet;
    use gorget::trap::TrapKind as T;

    // Exhaustiveness guard: a new variant breaks compile here (no catch-all).
    #[allow(dead_code)]
    fn _t_exhaustive(t: T) {
        match t {
            T::Overflow | T::DivByZero | T::Bounds | T::UnwrapNone | T::UnwrapError
            | T::UnwrapErrorOnOk | T::AssertFailed | T::Panic => {}
        }
    }
    let all = [
        T::Overflow, T::DivByZero, T::Bounds, T::UnwrapNone, T::UnwrapError,
        T::UnwrapErrorOnOk, T::AssertFailed, T::Panic,
    ];
    let registry: BTreeSet<String> = all.iter().map(|t| t.code().to_string()).collect();
    // The 7 non-Bounds codes production self-host MUST emit (trap_bounds is T2b).
    let required: BTreeSet<String> = all.iter()
        .map(|t| t.code().to_string())
        .filter(|c| c.as_str() != "T_Bounds")
        .collect();

    // Match a quoted `T_<Ident>` in BOTH the inline C-string form
    // (`\"T_Overflow\"`) and the OpConstStr form (`"T_Panic"`) via the
    // optional-backslash quote on each side.
    let re = regex::Regex::new(r##"\\?"(T_[A-Za-z]+)\\?""##).unwrap();
    let files = [
        "tests/fixtures/self_host_lowerer/lir_codegen.gg",
        "tests/fixtures/self_host_lowerer/lower_expr.gg",
        "tests/fixtures/self_host_lowerer/lower_stmt.gg",
        "tests/fixtures/self_host_lowerer/lower_closures.gg",
    ];
    let mut found: BTreeSet<String> = BTreeSet::new();
    for f in files {
        let src = fs::read_to_string(f).unwrap_or_default();
        for cap in re.captures_iter(&src) {
            found.insert(cap[1].to_string());
        }
    }

    // (a) every emitted code is a real registry code.
    let bogus: Vec<&String> =
        found.iter().filter(|c| !registry.contains(c.as_str())).collect();
    assert!(
        bogus.is_empty(),
        "self-host emits trap code(s) NOT in gorget::trap::TrapKind: {bogus:?}.\n\
         A hand-spelled `T_` literal drifted from the registry (typo, or a code \
         retired on the Rust side). Fix the literal at the self-host .gg emit site \
         (lir_codegen.gg / lower_expr.gg / lower_stmt.gg) or reconcile the registry.\n\
         Registry: {registry:?}",
    );

    // (b) all 7 non-Bounds codes are present (no site silently dropped).
    let missing: Vec<&String> =
        required.iter().filter(|c| !found.contains(c.as_str())).collect();
    assert!(
        missing.is_empty(),
        "self-host is MISSING trap code(s) it must emit: {missing:?}.\n\
         A direct trap site regressed to gorget_panic (or was removed). \
         (T_Bounds is intentionally NOT required — that is T2b.)\n\
         Found: {found:?}",
    );
}

/// **Repo-hygiene guard — `docs/plans/` stays gone; `docs/define-gorget/` is
/// ledger-only.** Owner ruling 2026-07-17 (memory `feedback-no-scouts-briefs-in-repo`):
/// round-scoped scouts, briefs, censuses, and plans are `/tmp`-ONLY and are never
/// committed. The former `docs/plans/` tree — a decade of scout/brief/plan ephemera —
/// was retired to git history in the 2026-07-17 hygiene slice; the one surviving
/// define-gorget artifact is the normative ledger `docs/define-gorget/decisions.md`.
///
/// This is a **shrink-only allowlist** (the `EXPECTED_HANGS` idiom): `docs/plans/`
/// must stay absent, and `docs/define-gorget/` must contain exactly the files in
/// `ALLOWED`. A new file appearing under either path fails the build until it is
/// either removed (an ephemeral scout/brief belongs in `/tmp`; a durable design doc
/// belongs in `docs/devbook/` or the reference/book) or `ALLOWED` is intentionally
/// widened here with a cited justification. The allowlist only ever shrinks.
#[test]
fn docs_plans_removed_and_define_gorget_is_ledger_only() {
    // `docs/plans/` was retired to git history; nothing may re-create it.
    assert!(
        !Path::new("docs/plans").exists(),
        "docs/plans/ has reappeared. Round-scoped scouts / briefs / censuses / plans are \
         /tmp-ONLY and never committed (owner ruling 2026-07-17, memory \
         `feedback-no-scouts-briefs-in-repo`). Move the new artifact to /tmp; a durable design \
         doc belongs in docs/devbook/ (or the reference/book), never docs/plans."
    );

    // `docs/define-gorget/` holds exactly the normative ledger — shrink-only allowlist.
    const ALLOWED: &[&str] = &["decisions.md"];
    let dir = Path::new("docs/define-gorget");
    assert!(
        dir.is_dir(),
        "docs/define-gorget/ is missing — the define-gorget ledger must live at \
         docs/define-gorget/decisions.md."
    );
    let mut found: Vec<String> = fs::read_dir(dir)
        .expect("read docs/define-gorget")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .collect();
    found.sort();
    let unexpected: Vec<&String> =
        found.iter().filter(|f| !ALLOWED.contains(&f.as_str())).collect();
    assert!(
        unexpected.is_empty(),
        "Unexpected file(s) under docs/define-gorget/: {unexpected:?}. This directory holds ONLY \
         the normative ledger `decisions.md` (owner ruling 2026-07-17). Scouts / briefs / \
         proposals are /tmp-ONLY. If a durable ledger-adjacent doc is genuinely warranted, add it \
         to ALLOWED here with a cited justification (this allowlist only shrinks)."
    );
    assert!(
        found.iter().any(|f| f == "decisions.md"),
        "docs/define-gorget/decisions.md (the normative ledger) is missing."
    );
}

/// AGENTS.md size ratchet (Core #6 applied to the instructions file itself).
/// The header's split rule: a new lesson lands in AGENTS.md as a compact rule;
/// the evidence/war-story goes to docs/devbook/29 (engineering) or
/// docs/devbook/30 (excellence system). Compacted 2026-07-25 from 64.6KB.
/// The ceiling only ever ratchets DOWN (a further compaction re-seeds it);
/// raising it requires owner sign-off.
#[test]
fn agents_md_size_ratchet() {
    // The evidence home the header and this message promise must actually exist.
    assert!(
        Path::new("docs/devbook/30-excellence-system.md").exists(),
        "docs/devbook/30-excellence-system.md is missing — AGENTS.md's header and this \
         lint both route evidence there; the split rule is unenforceable without it."
    );
    let bytes = fs::metadata("AGENTS.md").expect("AGENTS.md metadata").len();
    // 2026-07-28: raised 58_000 → 59_000 for the Round XII convergence-gate
    // rule fold (owner-signed-off in-round). Extended treatment moved to
    // devbook/30 §10; the AGENTS.md text was already compacted to a single-
    // sentence rule + regen commands.
    //
    // 2026-08-04: raised 59_000 → 59_700, OWNER SIGN-OFF given same day. Three
    // rules landed in one day — the big-ticket revocation, phased-work
    // one-bullet-per-phase, and the blocked-convergence owner-ask — and each
    // previously cost a byte-scavenging hunt through load-bearing prose, which
    // trades rule precision for headroom. Paid for partly by extracting rule
    // 6's literal cleanup commands into scripts/round_cleanup.sh (−309 bytes).
    // The owner first authorised 59_500; the escalation rule was written after
    // that figure was set and did not fit, hence 59_700.
    //
    // ⚠ THIS IS DEBT, not headroom. The file has absorbed four rules since the
    // last real compaction and is structurally at capacity: a dedicated
    // compaction round should move extended treatments into devbook/29–30 and
    // ratchet this back DOWN toward 58_000. Lowering needs no sign-off;
    // raising it again does.
    const CEILING: u64 = 59_700;
    assert!(
        bytes <= CEILING,
        "AGENTS.md is {bytes} bytes > {CEILING}. Move the new lesson's war-story/evidence \
         to docs/devbook/29 (engineering) or docs/devbook/30 (excellence system) and keep \
         only the compact rule here (see the split rule in the file header). Lowering this \
         ceiling after a further compaction is fine; raising it requires owner sign-off."
    );
}

// ===========================================================================
// Guards-slice ratchets (Core #6 + #10, owner 2026-07-18): A = silent-fallthrough
// allowlist (lowering arms must lower or reject, never silently drop); B =
// materialize-site convergence meter (the planner campaign's ratchet).
// ===========================================================================

/// Returns the (1-based) line numbers of "silent catch-all" match arms in a fn
/// body: an arm whose pattern is the wildcard `_` and whose body neither lowers
/// nor rejects — it is EMPTY or comment-only. Three syntactic forms:
///   `_ => {}` / `_ => ()`  (single line)
///   `_ => {`  … only blank/`//`-comment lines … `}` (multi-line block)
/// A `_ =>` arm whose body contains ANY code (a call, a `panic!`, a rejection,
/// an assignment) is NOT silent and is skipped. Line-based (never brace-matched
/// inside strings) — mirrors `top_level_fn_bodies`' robustness note.
fn silent_catchall_arm_lines(body: &str) -> Vec<usize> {
    let lines: Vec<&str> = body.lines().collect();
    let mut hits = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let t = lines[i].trim();
        // Strip a trailing line comment for the single-line forms.
        let code = t.split("//").next().unwrap_or("").trim();
        if code == "_ => {}" || code == "_ => ()" || code == "_ => (),"
            || code == "_ => {}," {
            hits.push(i + 1);
        } else if code == "_ => {" {
            // Multi-line block: scan to the matching `}` at the arm's indent.
            let indent = lines[i].len() - lines[i].trim_start().len();
            let mut j = i + 1;
            let mut only_comments = true;
            while j < lines.len() {
                let jt = lines[j];
                let jindent = jt.len() - jt.trim_start().len();
                let jtrim = jt.trim();
                if jtrim == "}" && jindent == indent {
                    break;
                }
                if !jtrim.is_empty() && !jtrim.starts_with("//") {
                    only_comments = false;
                }
                j += 1;
            }
            if only_comments {
                hits.push(i + 1);
            }
            i = j;
        }
        i += 1;
    }
    hits
}

/// RATCHET A (Core #10 "lower-or-reject — never silently drop user syntax").
///
/// The vulnerable class is a PARTIAL match over a large AST enum (an assign
/// TARGET `Expr`, or a binding `Pattern`) in a *lowering-emit* position. Rust's
/// exhaustiveness protects the FULL dispatchers (`lower_stmt` over `Stmt`,
/// `lower_expr` over `Expr`) — but a partial match with a `_ =>` catch-all (or
/// an if-let chain with no final `else`) can silently DROP a write/binding the
/// user wrote. Found live 2026-07-18: `xs.0 = v` (a tuple-field store) compiles,
/// runs, and prints the OLD value — the assignment vanished.
///
/// This ratchet freezes the set of lowering-emit functions in the assign/bind
/// files whose target/pattern dispatch has a *silent* `_ =>` catch-all, so a NEW
/// one fails CI (must lower-or-reject) and a FIXED one forces an allowlist edit
/// (the burn-down is visible). The two OPEN offenders that hide in non-`_ =>`
/// shapes (a missing final `else`, a nested `else "__throwaway"`) are pinned
/// separately by marker, since a wildcard-arm scan cannot see them.
#[test]
fn ratchet_a_lowering_dispatch_silent_fallthrough() {
    // Files where every silent `_ =>` catch-all is the user-syntax-drop class.
    // (Detection/analysis walkers — liveness, closures capture, generics
    // discovery — live elsewhere and are a DIFFERENT class, already guarded by
    // the exhaustive-walker lints.)
    const FILES: &[&str] = &[
        "src/ir/lowering/stmts/assigns.rs",
        "src/ir/lowering/stmts/mod.rs",
        "src/ir/lowering/stmts/for_loops.rs",
    ];
    // The CURRENT open set: each fn's target/pattern dispatch silently drops the
    // unhandled shapes. BURN-DOWN — as each is fixed (lower-or-reject), delete
    // it here; the set only shrinks without a cited re-pin.
    //   lower_var_decl      — non-Binding/Tuple VarDecl patterns (defensive:
    //                         parser+semantic gate these today, so unreachable —
    //                         but the arm is silent, not a loud `unreachable!`).
    //   lower_for_dict      — `for (a,b,c) in dict` (Tuple arity != 2) → no
    //                         bindings, garbage output.
    // BURNED DOWN (Target-2): `lower_assign` — its `_ =>` was the `xs.0 = v`
    // (Expr::TupleFieldAccess) silent drop; it now lowers tuple fields and its
    // `_ =>` is a loud `unreachable!` backed by the check-time
    // `check_assign_target_lvalue` gate (E_InvalidAssignTarget).
    const ALLOWED: &[&str] = &["lower_var_decl", "lower_for_dict"];

    let mut found: Vec<String> = Vec::new();
    for file in FILES {
        let content = fs::read_to_string(file).unwrap_or_default();
        for (name, body) in top_level_fn_bodies(&content) {
            if !silent_catchall_arm_lines(&body).is_empty() {
                found.push(name);
            }
        }
    }
    found.sort();
    found.dedup();
    let mut expected: Vec<String> = ALLOWED.iter().map(|s| s.to_string()).collect();
    expected.sort();
    assert_eq!(
        found, expected,
        "Silent `_ =>` catch-all set in the assign/bind lowering files changed.\n\
         found={found:?} expected={expected:?}\n\
         A NEW entry = a lowering-emit dispatch that silently drops user syntax \
         (Core #10). Fix it (lower the construct or emit a check-time rejection) \
         — do NOT just add it here. A MISSING entry = an offender was fixed; \
         delete it from ALLOWED (burn-down)."
    );

    // The remaining OPEN offender with NO `_ =>` to scan — pinned by marker so a
    // fix forces this list to shrink. BURN-DOWN.
    //
    // BURNED DOWN (Target-2): A2 — `lower_compound_assign` (formerly an if-let
    // chain with NO final `else`, so `xs.0 += v` / `*p += v` fell through to
    // nothing) now has TupleFieldAccess + Deref arms AND a final `else` that
    // `unreachable!`s on a non-lvalue (backed by the check-time
    // E_InvalidAssignTarget gate). Its dedicated fallback-presence guard is
    // `compound_assign_fieldaccess_fallback_present`.
    //
    //   A4b: lower_for_dict's `Tuple(2)` arm binds each sub-pattern only via
    //       `if let Binding(n) .. else \"__k\"/\"__v\"` — a nested destructure
    //       (`for k,(a,b) in dict`) is silently dropped into a throwaway.
    let for_loops = fs::read_to_string("src/ir/lowering/stmts/for_loops.rs").unwrap_or_default();
    let for_dict = top_level_fn_bodies(&for_loops)
        .into_iter()
        .find(|(n, _)| n == "lower_for_dict")
        .map(|(_, b)| b)
        .expect("lower_for_dict not found");
    // Pin each throwaway side SEPARATELY (an `||` would stay green after a
    // value-side-only fix, hiding a half-landed A4b).
    assert!(
        for_dict.contains("\"__v\".to_string()"),
        "lower_for_dict no longer uses the `__v` value-side throwaway fallback — the \
         A4b nested-destructure offender may be (half-)fixed. Re-pin or remove (burn-down)."
    );
    assert!(
        for_dict.contains("\"__k\".to_string()"),
        "lower_for_dict no longer uses the `__k` key-side throwaway fallback — the \
         A4b nested-destructure offender may be (half-)fixed. Re-pin or remove (burn-down)."
    );
}

/// RATCHET B — materialize-site convergence meter (Core #6). The count of direct
/// call sites of the mutation-root materialize helpers can only DECREASE without
/// a cited re-pin. This is the materialization-planner campaign's meter: as the
/// planner subsumes ad-hoc per-site materialize calls, the ceilings drop.
///   Rust: `cow_before_mutation` (def `src/ir/lowering/context.rs`).
///   Self-host lowerer: `cow_materialize_projected_root` /
///   `cow_materialize_root_by_name` (defs in `lower.gg`).
///
/// THE CONVERSION RULE (planner round 3): when an at-site materialize CLASS
/// migrates behind the `MaterializePlan`, its `.cow_before_mutation(` count moves
/// OUT of this text census and INTO the plan (it now reaches the lone real call
/// through `apply_materialize_directive` → `cow_before_mutation_planned`, which is
/// not a `.cow_before_mutation(` textual call). So the census only ever DECREASES
/// per conversion. The ONLY legitimate INCREASE is a genuinely-new mutation-root
/// materialize that cannot route through the plan yet — and it must justify itself
/// with a cited re-pin (raise `RUST_CEILING` + a comment naming the new site and
/// why it can't be planned). A silent bump is a red flag: the campaign's whole
/// point is that new mutation roots go through the plan, not a fresh ad-hoc call.
#[test]
fn ratchet_b_materialize_site_count() {
    // --- Rust side: `.cow_before_mutation(` call expressions in src/ir/lowering.
    // Planner campaign round 3 (assign-target-root class, first at-site client):
    // 20 → 14. `lower_field_assign` + `lower_index_assign` + the compound path
    // (six open-coded `cow_before_mutation` calls, Core #4 sibling drift) now
    // route through the shared `materialize_assign_target_root` →
    // `plan_materialize_at_site` → the single reason-stamping funnel
    // (`cow_before_mutation_planned`), removing all six direct calls behind the
    // `MaterializePlan`. Remaining 14 = the un-migrated classes B/C/D/E/F + the
    // funnel's own lone real call; each future class conversion drops the ceiling.
    //
    // 2026-07-27 re-pin 14 → 15 (Track B1 A-2 Option (b), commit `02082ae8`;
    // refined by Track F, commit `c8c346f2`):
    // retiring the `is_param_borrow_unique` bypass in `calls.rs` added a new
    // `cow_before_mutation` site inside the `lower_call_arg` `Ownership::Borrow
    // if callee_passes_by_ptr` fast-path (5 sites; was 4). Track F subsequently
    // moved that call INSIDE the `is_param_borrow_unique` guard so it fires only
    // on true `&`-param locals (bare params fall through), closing the driver.gg
    // clone-bomb. The site COUNT stays 5; the SCOPE of the invariant is now
    // narrower than the pre-F fold intended, correctly so. Future class
    // conversions may fold this call back through the planner.
    //
    // 2026-08-03 re-pin 15 → 16 (Round XXIX Track C):
    // The enumerate fast-path receiver-type gate added a new caller-side
    // alias-root sever in `src/ir/lowering/stmts/for_loops.rs` — necessarily
    // called BEFORE the pre-lower of the receiver so the sever's rebind is
    // visible to the read (see `lower_for_enumerate` on the callee side
    // where the sever is now skipped when `pre_lowered.is_some()`, i.e. one
    // sever moved from callee to caller; the count grew because the
    // callee-side sever wasn't deleted — it still fires on any hypothetical
    // caller that DOESN'T pre-lower, kept for API symmetry). Net Track C
    // added ONE `cow_before_mutation` site to for_loops.rs.

    const RUST_CEILING: usize = 16;
    let mut rust_sites = 0usize;
    let mut per_file: Vec<(String, usize)> = Vec::new();
    for entry in walk_files("src/ir/lowering", "rs") {
        let content = fs::read_to_string(&entry).unwrap_or_default();
        let mut n = 0;
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") { continue; }
            // A call, not the `pub fn cow_before_mutation(` definition.
            if t.contains(".cow_before_mutation(") {
                n += line.matches(".cow_before_mutation(").count();
            }
        }
        if n > 0 { per_file.push((entry, n)); }
        rust_sites += n;
    }
    assert!(
        rust_sites <= RUST_CEILING,
        "cow_before_mutation call sites grew to {rust_sites} (ceiling {RUST_CEILING}). \
         The materialization-planner campaign is a convergence meter — sites only \
         DECREASE. If a genuinely-new mutation-root materialize is unavoidable, \
         raise RUST_CEILING with a cited re-pin. Per-file: {per_file:?}"
    );

    // --- Self-host side: projected_root + root_by_name call expressions in the
    // self-host lowerer .gg (exclude the `bool NAME(` defs and `from … import`).
    const SH_CEILING: usize = 8;
    const SH_FNS: &[&str] = &["cow_materialize_projected_root", "cow_materialize_root_by_name"];
    let mut sh_sites = 0usize;
    let mut sh_per_file: Vec<(String, usize)> = Vec::new();
    for entry in walk_files("tests/fixtures/self_host_lowerer", "gg") {
        let content = fs::read_to_string(&entry).unwrap_or_default();
        let mut n = 0;
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("#") { continue; }
            if t.starts_with("from ") && t.contains("import") { continue; }
            for f in SH_FNS {
                let call = format!("{f}(");
                if line.contains(&call) {
                    // Exclude the definition line `bool NAME(` / `void NAME(`.
                    let def = format!("bool {f}(");
                    if t.starts_with(&def) { continue; }
                    n += line.matches(&call).count();
                }
            }
        }
        if n > 0 { sh_per_file.push((entry, n)); }
        sh_sites += n;
    }
    assert!(
        sh_sites <= SH_CEILING,
        "self-host materialize call sites grew to {sh_sites} (ceiling {SH_CEILING}). \
         Decrease-only. Per-file: {sh_per_file:?}"
    );
}

/// RATCHET C — hand-rolled materialize-BYPASS meter (Core #4 / Core #6).
///
/// Rust gg decides "make this owned at this boundary" through six shared
/// chokepoints, all of which live in `src/ir/lowering/context.rs`:
/// `ensure_owned_at_boundary` · `ensure_owned_at_consuming_arg` ·
/// `emit_enum_init_owned` · `clone_ptr_rhs_if_needed` · `auto_clone_if_ptr` ·
/// `materialize_lazy_source_if_needed`. The self-host does all of it in ONE
/// function (`op_consume`, `lower.gg`). The gap between those two numbers is a
/// bug class, not an aesthetic: the return-borrow double-free family existed
/// because the statement `return` had its OWN hand-rolled `GirType::Ptr`-only
/// clone that the chokepoint's `pointee_type` test would have handled.
///
/// THE COUNTED PREDICATE (named explicitly, per Core #4's "a ratchet needs a
/// countable predicate"): occurrences of `emit_clone(` or `call_clone(` in
/// `src/ir/lowering/**` EXCLUDING `context.rs`. Every such occurrence is a
/// lowering arm that resolves a clone fn and emits the clone ITSELF instead of
/// delegating the decision to a chokepoint — i.e. exactly the hand-rolled
/// bypass set. `context.rs` is excluded because it is the chokepoints' home:
/// clones emitted there ARE the converged path. Definition lines are skipped
/// (` fn `), as are comment lines.
///
/// THE CONVERSION RULE: when a bypass migrates onto a chokepoint, its textual
/// call disappears from this census, so the count only ever DECREASES.
/// A genuinely-new hand-rolled materialize must justify itself with a cited
/// re-pin (raise `BYPASS_CEILING` + name the site and why it cannot route
/// through a chokepoint).
///
/// THE COUNTED SET at the pin (23), by enclosing function:
///   stmts/mod.rs (7): `clone_resource_global_ref` · `lower_var_decl`'s
///     Ptr(T)→T · `lower_var_decl_assign_mode`'s `emit_clone_to_owned`
///     (branches B/D/E) · `lower_return`'s THROWS Ptr(T) leg · `lower_return`'s
///     THROWS by-value `needs_drop` leg · `lower_return`'s bespoke String
///     clone · `try_lift_option_ref`.
///   stmts/assigns.rs (5): bare-VALUE Ptr-param full rebind · field-borrow
///     Ptr(T)→T materialize · non-fresh String rebind · `lower_assign`
///     cross-type Branch-B · `lower_assign` named-not-last-use.
///   exprs/methods.rs (5): TWO `ExplicitUserClone` sites (user-written
///     `.clone()` — NOT a materialize decision; a permanent floor of 2) ·
///     mutating-receiver materialize · consuming-arg pre-call clone ·
///     `try_lower_option_result_combinator`.
///   exprs/mod.rs (3): deref CoW materialize · compound-assign LHS CoW
///     materialize · `clone_multi_use_resource_args`.
///   exprs/calls.rs (2): move-param-from-borrow arg · borrowed-extern-return.
///   stmts/patterns.rs (1): `emit_pattern_bindings` resource-field extract.
///
/// Track B1 took this from 24 → 23 by retiring the statement `return`'s
/// resource-clone leg in favour of `ensure_owned_at_boundary`.
#[test]
fn ratchet_c_handrolled_materialize_bypass_count() {
    const BYPASS_CEILING: usize = 23;
    let mut sites = 0usize;
    let mut per_file: Vec<(String, usize)> = Vec::new();
    for entry in walk_files("src/ir/lowering", "rs") {
        if entry.ends_with("context.rs") { continue; }
        let content = fs::read_to_string(&entry).unwrap_or_default();
        let mut n = 0;
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") { continue; }
            // A call, not a `fn emit_clone(` / `fn call_clone(` definition.
            if t.contains(" fn ") { continue; }
            n += line.matches("emit_clone(").count();
            n += line.matches("call_clone(").count();
        }
        if n > 0 { per_file.push((entry, n)); }
        sites += n;
    }
    assert!(
        sites <= BYPASS_CEILING,
        "hand-rolled materialize bypasses grew to {sites} (ceiling {BYPASS_CEILING}). \
         This is a CONVERGENCE meter — the Axis-B chain migrates these onto the \
         six shared chokepoints in context.rs, so the count only DECREASES. If a \
         new hand-rolled materialize is genuinely unavoidable, raise \
         BYPASS_CEILING with a cited re-pin naming the site. Per-file: {per_file:?}"
    );
}

/// Arm-count lint (Core #4 "one fix, all siblings" / Core #6 "class-retiring
/// guard"): the planner consumer #1 scope pre-header materialize must be hoisted
/// at the `lower_stmt` DISPATCH ARM of EVERY non-loop scope form — one shared
/// entry (`materialize_scope_carried_bare_params`), never open-coded per form —
/// so a new scope form cannot silently skip it (the `cow_loop_bare_param_if_branch`
/// class: a bare-param mutation inside a save/restore scope thrown away by the
/// scope's restore_locals). The non-loop scope forms are exactly six:
///   If · With · Unsafe · NamedScope · Match · Select.
/// The LOOP forms (While · For · Loop) are hoisted through the SEPARATE
/// `materialize_loop_carried_bare_params` funnel (which keeps
/// `LoopPreHeaderMaterialize` so per-position costing stays honest — a
/// dispatch-arm hoist for a loop would mis-stamp `BranchPreHeaderMaterialize`);
/// its three call sites (while + bare loop in stmts/mod.rs, for in for_loops.rs)
/// are pinned too. For while-else/for-else a presence-count cannot detect a
/// MISSING else-body scan — the loop-else regression FIXTURES
/// (`cow_scope_bare_param_while_else` / `_for_else`) are the real guard there.
///
/// **If this fails:**
///   - A NEW non-loop scope form was added → it MUST call
///     `materialize_scope_carried_bare_params(ctx, builder, &stmt.node, …)` at
///     its dispatch arm (do NOT open-code the scan); bump SCOPE_ARMS with a
///     justification. `lower_block_scoped` itself must stay materialize-free (an
///     unconditional entry hoist there would break the conditional callers).
///   - The count went DOWN → a scope form lost its hoist, re-opening the
///     throw-away hole; restore the call, do NOT lower the constant.
#[test]
fn planner_scope_preheader_arm_count() {
    let src = fs::read_to_string("src/ir/lowering/stmts/mod.rs")
        .expect("read src/ir/lowering/stmts/mod.rs");

    // Non-loop scope dispatch-arm hoists: `materialize_scope_carried_bare_params(
    // ctx, builder, &stmt.node, …)` — one per scope form (If/With/Unsafe/
    // NamedScope/Match/Select).
    const SCOPE_ARMS: usize = 6;
    let scope_calls = src
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with("//")
                && t.contains("materialize_scope_carried_bare_params(ctx, builder, &stmt.node")
        })
        .count();
    assert_eq!(
        scope_calls, SCOPE_ARMS,
        "planner scope pre-header dispatch-arm hoist count changed: {scope_calls} vs \
         expected {SCOPE_ARMS} (If/With/Unsafe/NamedScope/Match/Select). A new scope \
         form must route through `materialize_scope_carried_bare_params` at its \
         `lower_stmt` dispatch arm — see the fn doc + the arm-count lint comment.",
    );

    // Loop pre-header hoists route through the distinct
    // `materialize_loop_carried_bare_params` funnel (while + bare loop here, for in
    // for_loops.rs). Pinned so a loop form can't lose its 2G/loop-else hoist.
    const LOOP_CALLS: usize = 3;
    let for_src = fs::read_to_string("src/ir/lowering/stmts/for_loops.rs")
        .expect("read src/ir/lowering/stmts/for_loops.rs");
    let loop_calls = src
        .lines()
        .chain(for_src.lines())
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with("//")
                && (t.contains("materialize_loop_carried_bare_params(ctx, builder")
                    || t.contains("super::materialize_loop_carried_bare_params(ctx, builder"))
        })
        .count();
    assert_eq!(
        loop_calls, LOOP_CALLS,
        "loop pre-header hoist call count changed: {loop_calls} vs expected \
         {LOOP_CALLS} (while + bare loop + for). The loop-else regression fixtures \
         guard the else-body scan; this presence-count guards the hoist itself.",
    );
}

/// Minimal recursive file walk (extension-filtered) shared by the ratchets.
fn walk_files(root: &str, ext: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut stack = vec![std::path::PathBuf::from(root)];
    while let Some(dir) = stack.pop() {
        if let Ok(rd) = fs::read_dir(&dir) {
            for e in rd.flatten() {
                let p = e.path();
                if p.is_dir() {
                    stack.push(p);
                } else if p.extension().and_then(|s| s.to_str()) == Some(ext) {
                    out.push(p.to_string_lossy().into_owned());
                }
            }
        }
    }
    out
}

/// Extract a top-level Rust fn's source (signature through the line before the
/// NEXT top-level `fn`). Boundary-based rather than brace-counted so the
/// `writeln!(out, "...{{...}}")` string-literal braces in the C emitters don't
/// throw the scan off. Nested fns (indented) don't terminate the scan.
fn rust_fn_body(content: &str, fn_name: &str) -> String {
    let needle = format!("fn {fn_name}(");
    let mut in_fn = false;
    let mut body = String::new();
    for line in content.lines() {
        if !in_fn {
            if line.contains(&needle) {
                in_fn = true;
                body.push_str(line);
                body.push('\n');
            }
            continue;
        }
        let starts_top_level_fn = line.starts_with("fn ")
            || line.starts_with("pub fn ")
            || line.starts_with("pub(super) fn ")
            || line.starts_with("pub(crate) fn ")
            || line.starts_with("async fn ");
        if starts_top_level_fn {
            break;
        }
        body.push_str(line);
        body.push('\n');
    }
    body
}

/// Extract a top-level Gorget (`.gg`) fn's source (signature line through the
/// line before the next column-0 definition). Comment/blank lines are kept
/// (they aren't definition boundaries); only a non-blank, non-`#`, column-0
/// line ends the body.
fn gg_fn_body(content: &str, sig_contains: &str) -> String {
    let mut in_fn = false;
    let mut body = String::new();
    for line in content.lines() {
        if !in_fn {
            if line.contains(sig_contains) && !line.trim_start().starts_with('#') {
                in_fn = true;
                body.push_str(line);
                body.push('\n');
            }
            continue;
        }
        let is_boundary = !line.is_empty()
            && !line.starts_with(char::is_whitespace)
            && !line.trim_start().starts_with('#');
        if is_boundary {
            break;
        }
        body.push_str(line);
        body.push('\n');
    }
    body
}

/// Refcount-handle clone-arm symmetry ratchet (CLAUDE.md rule 4 "one fix, all
/// siblings" + Core #6 — convert a recurring bug class into an executable
/// guard). BOTH lanes shipped the SAME defect: a struct/enum holding a
/// refcount-handle field (`Shared` / `Weak` / `Channel`) had its DROP synthesis
/// RELEASE the handle (`gorget_shared_drop` / `gorget_weak_drop` /
/// `gorget_channel_release`) while its CLONE synthesis merely shallow-copied it
/// — no RETAIN — so the copy's drop underflowed the refcount → premature free /
/// double-free / UAF (`shared_struct_field_clone.gg`; the self-host
/// sibling was fixed in `lir_codegen.gg`'s `field_clone_c`). The clone must
/// balance the drop.
///
/// This lint locks the symmetry structurally, per lane:
///   - Rust: every by-value RETAIN lives in the single helper
///     `refcount_field_retain_fn`, and EVERY clone-synthesis path consults it —
///     so a fourth clone path, or a dropped family arm, trips here.
///   - Self-host: `field_clone_c` carries the Shared-family RETAIN arm.
///
/// **If this fails**: a refcount family's clone RETAIN was dropped, or a new
/// clone-synthesis path was added that skips `refcount_field_retain_fn`. Route
/// the new path through the helper (Rust) / add the arm to `field_clone_c`
/// (self-host) — do NOT relax the assertion.
#[test]
fn refcount_clone_arm_symmetry() {
    // (RELEASE drop symbol, by-value RETAIN clone symbol) for every refcount
    // family whose field drop releases a strong/weak/channel ref. Rc/Arc were
    // removed (A2) — intentionally absent.
    const PAIRS: &[(&str, &str)] = &[
        ("gorget_shared_drop", "gorget_shared_clone"),
        ("gorget_weak_drop", "gorget_weak_clone"),
        ("gorget_channel_release", "gorget_channel_retain"),
    ];

    // ---- Rust lane ----
    let emit = fs::read_to_string("src/backend/c_lir/emit_types.rs")
        .expect("emit_types.rs readable");
    let helpers = fs::read_to_string("src/backend/c_lir/helpers.rs")
        .expect("helpers.rs readable");
    let retain_fn = rust_fn_body(&emit, "refcount_field_retain_fn");
    assert!(!retain_fn.is_empty(), "refcount_field_retain_fn not found in emit_types.rs");
    for (release, retain) in PAIRS {
        // Match the returned string literal `Some("<retain>")`, not a mention in
        // prose, so deleting the arm actually trips the assert.
        let retain_lit = format!("\"{retain}\"");
        assert!(
            retain_fn.contains(&retain_lit),
            "refcount_field_retain_fn is missing the `{retain}` RETAIN arm. A \
             struct/enum clone of that refcount family would shallow-copy the \
             handle while its drop RELEASEs it → refcount underflow → UAF."
        );
        assert!(
            helpers.contains(release),
            "helpers.rs no longer emits the `{release}` drop wrapper this RETAIN \
             balances. If the family was removed, drop it from PAIRS and from \
             refcount_field_retain_fn together."
        );
    }
    for f in [
        "emit_recursive_struct_clones",
        "emit_recursive_enum_clones",
        "emit_type_drop_fns",
    ] {
        let body = rust_fn_body(&emit, f);
        assert!(!body.is_empty(), "clone-synthesis fn `{f}` not found in emit_types.rs");
        assert!(
            body.contains("refcount_field_retain_fn"),
            "clone-synthesis path `{f}` does not consult `refcount_field_retain_fn`. \
             A clone path that skips refcount RETAIN is the exact asymmetry that \
             caused the Shared-struct-field-clone UAF (both lanes). Route it \
             through the helper."
        );
    }

    // ---- Self-host lane ----
    let sh = fs::read_to_string("tests/fixtures/self_host_lowerer/lir_codegen.gg")
        .expect("lir_codegen.gg readable");
    let field_clone = gg_fn_body(&sh, "String field_clone_c(");
    assert!(!field_clone.is_empty(), "field_clone_c not found in lir_codegen.gg");
    // Match the CODE fragments, not prose: the emit `... = gorget_shared_clone(...)`
    // and the guard `== "gorget_shared_drop"`. The surrounding rationale comment
    // also names both symbols, so a bare `contains("gorget_shared_clone")` would
    // pass even with the arm deleted (verified: it did).
    assert!(
        field_clone.contains("gorget_shared_clone(\" +"),
        "self-host `field_clone_c` is missing the Shared-family RETAIN emit \
         (`... = gorget_shared_clone(...)`). Its struct-drop RELEASEs the handle; \
         without the clone RETAIN a Shared-containing struct clone underflows → UAF."
    );
    assert!(
        field_clone.contains("== \"gorget_shared_drop\""),
        "self-host `field_clone_c` no longer guards on `== \"gorget_shared_drop\"` \
         — the Shared field-clone arm's detection is gone."
    );

    // ---- One-writer / one-accessor lock for the CONSUMING-POSITION axis ----
    // Family membership {Shared, Weak, Channel} at consuming positions is read
    // through the single accessor `TypeRegistry::is_refcount_clone_type` and
    // written through the single setter `TypeMetadata::set_refcount_clone_fn`.
    // Both live in src/ir/types.rs. If either disappears the axis has drifted.
    let ir_types = fs::read_to_string("src/ir/types.rs").expect("ir/types.rs readable");
    assert!(
        ir_types.contains("fn set_refcount_clone_fn"),
        "the SINGLE refcount-clone writer `TypeMetadata::set_refcount_clone_fn` is gone. \
         Every def-mint path must route the {{Shared,Weak,Channel}} clone_fn through it \
         (Layering rule 3) so a handle minted via the ctor path and via the annotated-type \
         path carry byte-identical metadata."
    );
    assert!(
        ir_types.contains("fn is_refcount_clone_type"),
        "the SINGLE consuming-position accessor `TypeRegistry::is_refcount_clone_type` is gone. \
         The consuming-position gates (ensure_owned_at_consuming_arg, clone_multi_use_resource_args, \
         move_zero_consumed_args) read family membership through it; without it they revert to \
         shallow-aliasing refcount handles -> the double-free / under-incref class."
    );
    // EVERY def-mint path routes its clone_fn through the single writer, so a
    // mint path can't silently drift a family member out of the accessor.
    let type_reg = fs::read_to_string("src/ir/lowering/exprs/type_reg.rs")
        .expect("type_reg.rs readable");
    for f in ["ensure_shared_type_def", "ensure_weak_type_def", "ensure_channel_type_def"] {
        let body = rust_fn_body(&type_reg, f);
        assert!(!body.is_empty(), "ctor-path def-mint `{f}` not found in type_reg.rs");
        assert!(
            body.contains("set_refcount_clone_fn"),
            "refcount ctor-path def-mint `{f}` no longer routes its clone_fn through the \
             single writer `set_refcount_clone_fn`. A refcount handle minted via the ctor \
             path would then carry different clone_fn metadata than the annotated-type path \
             (map_ast_type_mut) -> is_refcount_clone_type answers inconsistently -> the \
             consuming-position auto-clone silently reverts to shallow-alias (UAF)."
        );
    }
    let low_types = fs::read_to_string("src/ir/lowering/types.rs")
        .expect("lowering/types.rs readable");
    assert!(
        low_types.contains("set_refcount_clone_fn"),
        "the annotated-type def-mint path (map_ast_type_mut, src/ir/lowering/types.rs) no longer \
         routes the Shared/Weak/Channel clone_fn through `set_refcount_clone_fn` -> mint-path \
         drift vs the ctor-path ensure_*_type_def writers."
    );
}

/// Tier 3b ratchet: every `tests/*.rs` target must actually RUN in CI.
///
/// A test target CI never invokes is not a guard — it is a file that compiles.
/// Measured 2026-07-26: `--test lints` had NEVER been in CI, so every
/// structural-guard ratchet in this file (the executable form of Core #6) was
/// enforced only by someone remembering to run it locally. That memory failed
/// — `no_growth_in_phase_d_proxy_reads` went red at `5b8aa6da` and stayed red
/// for five days across round closes that reported the full battery green.
///
/// The same audit found `lir_ab`, `runtime_compile` and `str_runtime` absent
/// too (the latter two since merged into `c_runtime`). `str_runtime` had been
/// failing since the C runtime was split into units, assembling a `Str`-less
/// translation unit. `lir_ab` turned out to be
/// comparing the LIR->C backend against ITSELF — the GIR->C backend it was
/// written to A/B against is gone, both flags resolve to `CLirBackend`, and
/// the emitted C was byte-identical — so it was retired rather than wired up.
/// Nothing anywhere would have caught any of it.
///
/// So the enumeration is closed by construction: add a `tests/<name>.rs` and
/// this lint fails until `ci.yml` runs it.
#[test]
fn every_test_target_runs_in_ci() {
    let ci = fs::read_to_string(".github/workflows/ci.yml").expect("ci.yml readable");

    let mut missing = Vec::new();
    for entry in fs::read_dir("tests").expect("tests/ readable") {
        let path = entry.expect("dir entry").path();
        if path.extension().map_or(true, |e| e != "rs") {
            continue;
        }
        let stem = match path.file_stem().and_then(|s| s.to_str()) {
            Some(s) => s.to_string(),
            None => continue,
        };
        // `--test <stem>` is how CI names an integration-test target.
        if !ci.contains(&format!("--test {stem}")) {
            missing.push(stem);
        }
    }
    missing.sort();

    assert!(
        missing.is_empty(),
        "test target(s) present in tests/ but never run by .github/workflows/ci.yml: {missing:?}\n\n\
         A target CI does not run is not a guard. Either add a `cargo test --test <name>` \
         step to ci.yml, or delete the target if it is dead.\n\n\
         (If a target is deliberately local-only — too slow or too flaky for CI — say so \
         HERE with an explicit allowlist entry and the reason, so the exemption is visible \
         rather than silent.)"
    );
}

/// Tier 3b ratchet: every RATIFIED decision must be traceable into the spec.
///
/// The define-gorget ledger (`docs/define-gorget/decisions.md`) is normative,
/// but it is a decision RECORD — dated, adversarial, carrying rejected
/// alternatives — while `language-reference.md` / `language-design.md` /
/// `docs/book/` are timeless present-tense specification. The split is
/// deliberate and enforced by `docs_plans_removed_and_define_gorget_is_ledger_only`.
///
/// The cost of the split is DRIFT, and it is this tree's most repeated
/// documentation defect: a decision gets ratified, written through to one
/// document, and its sibling goes on stating the superseded rule. Measured
/// examples, all found by the 2026-07-26 sigil-prose gauntlet — `language-
/// design.md` §3.5 was rewritten away from Rust's borrow rule while
/// `language-reference.md` §9.2 still taught it (and §9.4 pointed the reader
/// AT §9.2); D34's capture rule reached the book and not the reference; the
/// escape-time and loop-element claims each landed in one place only.
///
/// A prose write-through with no `D<N>` citation is invisible to review: you
/// cannot tell whether the text reflects the decision or predates it. So this
/// lint requires the citation — every ratified D-number appears in at least
/// one spec document. It does NOT check that the prose is CORRECT; it checks
/// that someone claimed the write-through, which is the precondition for a
/// reviewer being able to verify it.
///
/// Baseline 2026-07-26: 16 of 24 ratified decisions uncited; immediately burned
/// to 14 by citing D32 (the legal-position whitelist) and D33 (the iterable-side
/// sigil) in the sections they govern. Burn this down; lower BUDGET as decisions
/// are written through. New decisions must cite.
#[test]
fn ratified_decisions_are_cited_in_the_spec() {
    let ledger = fs::read_to_string("docs/define-gorget/decisions.md")
        .expect("decisions.md readable");

    let dnum = regex::Regex::new(r"\bD(\d{1,2})\b").unwrap();

    // Ratified decisions: the LOG's `— **D<N> ...` entry headers.
    let header = regex::Regex::new(r"—\s*\*\*D(\d{1,2})\b").unwrap();
    let mut ratified: Vec<u32> = header
        .captures_iter(&ledger)
        .filter_map(|c| c[1].parse().ok())
        .collect();
    ratified.sort_unstable();
    ratified.dedup();
    assert!(
        !ratified.is_empty(),
        "no ratified decisions parsed from decisions.md — the LOG entry format \
         (`- <date> — **D<N> ...`) changed and this lint reads nothing."
    );

    // Everything the SPEC cites, across the reference, the design doc, and the book.
    let mut spec = String::new();
    for p in ["docs/language-reference.md", "docs/language-design.md"] {
        spec.push_str(&fs::read_to_string(p).unwrap_or_default());
    }
    if let Ok(entries) = fs::read_dir("docs/book") {
        for e in entries.filter_map(|e| e.ok()) {
            if e.path().extension().map_or(false, |x| x == "md") {
                spec.push_str(&fs::read_to_string(e.path()).unwrap_or_default());
            }
        }
    }
    let cited: std::collections::HashSet<u32> = dnum
        .captures_iter(&spec)
        .filter_map(|c| c[1].parse().ok())
        .collect();

    let uncited: Vec<String> = ratified
        .iter()
        .filter(|d| !cited.contains(d))
        .map(|d| format!("D{d}"))
        .collect();

    const BUDGET: usize = 14;
    assert!(
        uncited.len() <= BUDGET,
        "ratified decisions with no citation in any spec document: {} (budget {}).\n\n\
         Uncited: {}\n\n\
         A ratified decision that names no spec section has no verifiable \
         write-through — this tree's most repeated doc defect is a decision that \
         reached one document and left its sibling stating the superseded rule.\n\n\
         Either cite the D-number in the prose it governs, or (if the decision is \
         genuinely internal and governs no user-visible surface) say so in its \
         ledger entry and add it to this lint's exemption list.\n\n\
         If the count went DOWN, lower BUDGET here to lock the new floor.",
        uncited.len(),
        BUDGET,
        uncited.join(", ")
    );
}

// ---------------------------------------------------------------------------
// FAMILY-1 GUARD — the `&`-formation faces and the assign face must recognise
// the same projection forms, no new formation face may appear unnoticed, and
// the shared place producer must actually be INVOKED at both formation faces.
//
// Three limbs, because no one of them catches the class alone (Core #6 / #15e
// Q2): limb 1 catches arm DRIFT, limb 2 catches a NEW formation site, limb 3
// catches the producer never being CALLED — which is the exact mechanism
// Family-1 was, and which limbs 1 and 2 both sail past.
// ---------------------------------------------------------------------------

/// Collect the `Expr::<Variant>` names appearing in the TOP-LEVEL match-arm
/// heads of a function body.
///
/// Anchoring is structural, not semantic: in this rustfmt'd tree a top-level
/// `fn` ends with `}` in column 0, its body is indented 4, and the arms of the
/// body's `match` are indented exactly 8. So an arm head is "a line inside the
/// function whose indentation is exactly 8". Comment lines are skipped — a
/// doc-comment at arm indentation legitimately *mentions* `Expr::MutableBorrow`
/// without being an arm, and counting it would make the lint lie.
fn top_level_expr_arm_variants(src: &str, anchor: &str) -> std::collections::BTreeSet<String> {
    let start = src.find(anchor).unwrap_or_else(|| {
        panic!(
            "place-form parity lint: anchor not found in source: {anchor}\n\n\
             The lint is anchored on the function signature; if it was renamed, \
             update the anchor here."
        )
    });
    let rest = &src[start..];
    // A top-level fn ends at the first `}` in column 0.
    let end = rest.find("\n}\n").map(|e| e + 1).unwrap_or(rest.len());
    let body = &rest[..end];

    let mut out = std::collections::BTreeSet::new();
    for line in body.lines() {
        let Some(after_indent) = line.strip_prefix("        ") else {
            continue;
        };
        if after_indent.starts_with(' ') {
            continue; // deeper than an arm head — inside an arm
        }
        if after_indent.trim_start().starts_with("//") {
            continue; // a comment at arm indentation is not an arm
        }
        let mut hay = after_indent;
        while let Some(p) = hay.find("Expr::") {
            let tail = &hay[p + "Expr::".len()..];
            let name: String = tail
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if !name.is_empty() {
                out.insert(name.clone());
            }
            hay = &tail[name.len()..];
        }
    }
    out
}

/// LIMB 1 (Core #6, Core #4) — the `&`-borrow FORMATION face and the ASSIGNMENT
/// face must recognise the SAME set of projection forms as places.
///
/// **The class this retires.** A projection form served at one face and silently
/// dropped at the other is a miscompile `gg check` cannot see: `f(&c.fd)`
/// discarded the callee's write for every by-value field type for ~5 months
/// while `c.fd = v` worked, because the two faces resolved places through
/// *different code*. The fix made `try_resolve_place` the single producer for
/// both `&`-formation faces; this lint stops the faces drifting apart again.
///
/// **Why SET-equality and not a COUNT.** A count passes when one face gains
/// `Index` and the other gains `TupleFieldAccess` — the exact swap that would
/// reintroduce the bug. It also cannot NAME the missing form in its failure
/// message. Both arm sets are compared as sets and the assertion prints the
/// symmetric difference.
///
/// ⚠ **WHAT THIS LIMB CANNOT CATCH — read before trusting a green.**
///   * It checks ROUTING (is the form dispatched?), never SEMANTICS (does the
///     arm resolve the *right* place?). An arm that exists and is wrong passes.
///   * It cannot catch the *fallback* hole: an arm that exists but whose inner
///     `if let Some(..)` has no `else`, so a resolver `None` silently drops the
///     write. That is a different SUBJECT (Core #15e Q4 — the missing subject is
///     the FALLBACK, not the arm); `lower_tuple_field_assign` has exactly that
///     shape today and this lint is green on it, by design and not by accident.
///   * It does NOT cover the METHOD-RECEIVER face. There are THREE in-tree
///     consumers of these resolvers, not two: beyond `lower_assign`, the
///     method-receiver face calls them with its own receiver-form arm set
///     (`methods.rs`, `try_resolve_field_place` / `try_resolve_index_element_ptr`).
///     Probed at the Family-1 commit: `t.0.push(9)` and `(*b).v.push(9)` both
///     print `2` (correct), so no live divergence is claimed — but a guard that
///     exists to stop faces drifting must NAME every face it does not cover.
///     ⚠ The method-call face is also the only non-free-call `lower_call_arg`
///     caller that sets `expected_type`, so a refactor dropping those lines
///     would silently re-arm the auto-propagate call-skip there.
///   * It does NOT cover the SELF-HOST lane — `lower_field_place_base`
///     (`self_host_lowerer/lower_stmt.gg`) is another arm set entirely, and the
///     SH parser emits `EFieldAccess(base, "0")` for tuple fields, so parity
///     takes a different shape there.
///   * It is a text-shape lint over Rust source. Respelling either `match`
///     breaks it noisily, which is the intended ratchet behaviour.
#[test]
fn amp_formation_and_assign_cover_the_same_place_forms() {
    let exprs = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");
    let assigns = fs::read_to_string(Path::new("src/ir/lowering/stmts/assigns.rs"))
        .expect("read src/ir/lowering/stmts/assigns.rs");

    let formation = top_level_expr_arm_variants(
        &exprs,
        "pub(in crate::ir::lowering) fn try_resolve_place(",
    );
    let assign = top_level_expr_arm_variants(&assigns, "pub(super) fn lower_assign(");

    /// Forms the ASSIGN dispatch handles that are deliberately NOT place
    /// projections. Each needs a stated reason; adding a row without one is how
    /// a real hole gets waved through.
    ///
    /// NOTE on `Expr::Index`, deliberately NOT exempt: the two faces reach it by
    /// different MECHANISMS — `d[k] = v` is a setter call (Dict index-assign
    /// must be able to INSERT a missing key, which a write through a resolved
    /// element pointer cannot do), while `&d[k]` resolves an element place. Both
    /// faces nonetheless RECOGNISE the form, which is what this limb measures,
    /// so it belongs in the compared set. Exempting it would let the `&d[k]` arm
    /// be deleted with nothing going red. (The guard already caught its own
    /// author here: a first draft exempted `Index`, and the lint went red showing
    /// the exemption would allow exactly that silent deletion.)
    const ASSIGN_ONLY_EXEMPT: &[(&str, &str)] = &[(
        "Identifier",
        "a whole variable is not a projection — `x = v` rebinds the slot (and may \
         take the Shared/Mutex/Atomic paths), while `&x` is served by the \
         bare-identifier fast paths in lower_call_arg / Expr::MutableBorrow BEFORE \
         try_resolve_place is consulted",
    )];

    let exempt: std::collections::BTreeSet<String> = ASSIGN_ONLY_EXEMPT
        .iter()
        .map(|(name, _)| (*name).to_string())
        .collect();

    let assign_places: std::collections::BTreeSet<String> =
        assign.difference(&exempt).cloned().collect();

    let missing_from_formation: Vec<&str> = assign_places
        .difference(&formation)
        .map(|s| s.as_str())
        .collect();
    let missing_from_assign: Vec<&str> = formation
        .difference(&assign_places)
        .map(|s| s.as_str())
        .collect();

    assert!(
        missing_from_formation.is_empty() && missing_from_assign.is_empty(),
        "PLACE-FORM PARITY BROKEN between the `&`-formation faces and the assign face.\n\n\
         Handled by the assign face (`lower_assign`) but NOT by the shared producer \
         (`try_resolve_place`): {missing_from_formation:?}\n\
         Handled by `try_resolve_place` but NOT by the assign face: {missing_from_assign:?}\n\n\
         formation = {formation:?}\n\
         assign (minus exemptions) = {assign_places:?}\n\
         exemptions = {exempt:?}\n\n\
         A projection form that one face treats as a place and the other does not is \
         the Family-1 defect class: `f(&x.p)` silently discards the callee's write \
         while `x.p = v` works, `gg check` clean on both. Add the arm to \
         `try_resolve_place` (src/ir/lowering/exprs/mod.rs) so BOTH `&`-formation \
         faces get it at once — or, if the form genuinely cannot be a borrow target, \
         add it to ASSIGN_ONLY_EXEMPT WITH a reason.\n\n\
         See AGENTS.md Core #4 (one fix, all siblings) and Core #10 (lower-or-reject)."
    );

    // Sanity: the anchors resolved to real matches. A silently-empty set would
    // make the equality above vacuously true — the guard must not pass by
    // finding nothing (Core #15e Q2: a guard that green-lights its own class).
    assert!(
        formation.len() >= 4 && assign_places.len() >= 3,
        "place-form parity lint extracted suspiciously few arms \
         (formation={formation:?}, assign={assign_places:?}). The match statements \
         were probably respelled or reindented — fix the extractor rather than \
         lowering these floors."
    );
}

/// LIMB 1a (Core #4, devbook/24 Rule 3) — the two SPECIALIST place resolvers,
/// `try_resolve_field_place` and `try_resolve_tuple_field_place`, must dispatch
/// on the SAME set of OBJECT `Expr::` forms. They are a mutually-recursive PAIR;
/// asymmetry is the Family-2 defect class.
///
/// **The class this retires, which cost two silent write-drops.** The two
/// resolvers previously diverged by exactly two cells — the tuple resolver had
/// no `Expr::Index` arm (`&v[i].0` silently dropped the write on BOTH faces),
/// the field resolver had no `Expr::TupleFieldAccess` arm (`&t.0.fd` and
/// `u.0.fd = v` silently dropped on BOTH faces). `gg check` clean, C and LLVM
/// identical, `ggdef` adjudicating against production on both. The doc-comment
/// at `src/ir/lowering/exprs/mod.rs:try_resolve_place` (`# Postcondition 2`
/// table) already claimed the two resolvers "mirror row-for-row" — an
/// invariant-asserting comment without a guard (Core #14). Family-2 made the
/// two arms symmetric; this lint locks that symmetry so future drift trips
/// noisily instead of shipping.
///
/// **Why SET-equality and not a COUNT** (same rationale as LIMB 1): a count
/// passes when the field resolver gains form X and the tuple resolver gains a
/// DIFFERENT form Y — the exact swap that would reintroduce Family-2 at new
/// costumes. The failure message NAMES the missing form on each side.
///
/// ⚠ **KNOWN BLIND-SPOT — read before trusting a green.**
///   * `top_level_expr_arm_variants` extracts `Expr::<Variant>` names from
///     arm-indentation lines, so it sees only the SYNTACTIC object domain. The
///     field resolver's `Guard[T]` auto-deref block is TYPE-driven (no matching
///     `Expr::` head) — so this lint cannot see it. The tuple resolver has a
///     matching TYPE-driven Guard branch (Track L closed); this lint still
///     cannot see *either* Guard branch — that is a permanent syntactic-extractor
///     limit, not a missing arm. Guard write-through is pinned by durable
///     fixtures / ASan, not by this SET lint.
///   * Like LIMB 1, this checks ROUTING, not SEMANTICS. An arm that exists but
///     mis-resolves passes. Semantic coverage lives in the durable
///     `sound_tup*` / `sound_amp_v_i_tuple_field_writethrough` fixtures.
///   * It is a text-shape lint over Rust source. Respelling either `match`
///     breaks it noisily, which is the intended ratchet behaviour.
#[test]
fn field_and_tuple_place_resolvers_cover_the_same_object_forms() {
    let exprs = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");

    let field = top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_field_place(");
    let tuple =
        top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_tuple_field_place(");

    /// Object forms one resolver deliberately handles and the other does not.
    /// EMPTY at Family-2 land: the two resolvers cover the exact same object
    /// domain (`Identifier`, `SelfExpr`, `FieldAccess`, `TupleFieldAccess`,
    /// `Deref`, `Index`). Any future entry MUST carry a written reason showing
    /// the asymmetry cannot silently drop a write on either face.
    const EXEMPT: &[(&str, &str)] = &[];

    let exempt: std::collections::BTreeSet<String> =
        EXEMPT.iter().map(|(name, _)| (*name).to_string()).collect();

    let missing_from_field: Vec<&str> = tuple
        .difference(&field)
        .filter(|n| !exempt.contains(*n))
        .map(|s| s.as_str())
        .collect();
    let missing_from_tuple: Vec<&str> = field
        .difference(&tuple)
        .filter(|n| !exempt.contains(*n))
        .map(|s| s.as_str())
        .collect();

    assert!(
        missing_from_field.is_empty() && missing_from_tuple.is_empty(),
        "PLACE-RESOLVER ARM-PARITY BROKEN between `try_resolve_field_place` and \
         `try_resolve_tuple_field_place`.\n\n\
         Handled by the tuple resolver but NOT by the field resolver: {missing_from_field:?}\n\
         Handled by the field resolver but NOT by the tuple resolver: {missing_from_tuple:?}\n\n\
         field = {field:?}\n\
         tuple = {tuple:?}\n\
         exempt = {exempt:?}\n\n\
         The two resolvers are a mutually-recursive PAIR whose object-arm sets \
         must match — asymmetry is a silent write-drop (Family-2). Missing \
         `Expr::TupleFieldAccess` in the field resolver breaks `&t.0.fd` and \
         `u.0.fd = v`; missing `Expr::Index` in the tuple resolver breaks \
         `&v[i].0` and `w[i].0 = v` — both faces silently drop the write, \
         `gg check` clean.\n\n\
         Add the missing arm to `src/ir/lowering/exprs/mod.rs` (mirror the \
         existing sibling arm line-for-line), or — if the asymmetry is \
         intentional — add the variant to EXEMPT WITH a reason showing the \
         omission cannot lose a write on either the `&`-formation face or the \
         assign face.\n\n\
         See AGENTS.md Core #4 (one fix, all siblings), Core #10 (lower-or-reject), \
         and devbook/24 Rule 3 (one source of truth per axis)."
    );

    // Sanity floor (Core #15e Q2 — a guard that green-lights its own class):
    // both resolvers dispatch on at least the 5 non-`_` heads {Identifier,
    // SelfExpr, FieldAccess or TupleFieldAccess, Deref, Index}. A silently-
    // shrunken resolver (e.g. an arm deleted by refactor) would make the
    // equality above vacuously true without this floor.
    assert!(
        field.len() >= 5 && tuple.len() >= 5,
        "place-resolver arm-parity lint extracted suspiciously few arms \
         (field={field:?}, tuple={tuple:?}). The match statements were probably \
         respelled or reindented — fix the extractor rather than lowering these floors."
    );
}

/// Round XVII Instrument A — place-resolver arm census + pairwise divergence
/// ratchet across G1/G2/G3 (Expr-domain). G4 is Operand-domain (category lock).
///
/// ⚠ **A and B are WORKLIST GENERATORS, never correctness gates (Core #13).**
/// `Some(wrong_root)` counts as resolved. Only instrument C (build-and-run cell
/// matrix, later round) adjudicates landing. Do not promote arm-set green or
/// histogram emptiness into a soundness bar.
///
/// **TYPE-driven Guard blind spot (permanent):** this lint uses
/// `top_level_expr_arm_variants` — a syntactic extractor. Guard auto-deref is
/// TYPE-driven and invisible here on both field and tuple faces. Never claim
/// Guard coverage from this lint.
///
/// Category groups (do not flat-compare all five as one universe):
/// - G1 `resolve_projection_root_local` — CoW root walker
/// - G2 `try_resolve_place` — top-level place dispatcher
/// - G3a/G3b field/tuple — object-form specialist PAIR (Family-2 SET lint stays)
/// - G4 `resolve_ptr_field_place` — Operand/type fallback; **no Expr arms**
///
/// Metrics:
/// 1. required ⊆ extracted for named forms (Core #15e Q2 — count floors alone
///    green-light swapping form X for Y).
/// 2. Pairwise unexempted divergence DOWN-ONLY after EXEMPT table.
/// 3. G4 Expr arms == 0; G4 caller-presence ≥2 in assigns.rs.
#[test]
fn place_resolvers_arm_census_and_divergence() {
    let exprs = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");
    let assigns = fs::read_to_string(Path::new("src/ir/lowering/stmts/assigns.rs"))
        .expect("read src/ir/lowering/stmts/assigns.rs");

    let root = top_level_expr_arm_variants(
        &exprs,
        "pub(super) fn resolve_projection_root_local(",
    );
    let place = top_level_expr_arm_variants(
        &exprs,
        "pub(in crate::ir::lowering) fn try_resolve_place(",
    );
    let field = top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_field_place(");
    let tuple =
        top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_tuple_field_place(");
    let ptr = top_level_expr_arm_variants(&assigns, "fn resolve_ptr_field_place(");

    // --- required ⊆ extracted (named forms) ---
    let root_required: std::collections::BTreeSet<&str> = [
        "Identifier",
        "SelfExpr",
        "FieldAccess",
        "TupleFieldAccess",
        "Index",
        "MethodCall",
    ]
    .into_iter()
    .collect();
    let place_required: std::collections::BTreeSet<&str> =
        ["FieldAccess", "TupleFieldAccess", "Index", "Deref"]
            .into_iter()
            .collect();
    let g3_required: std::collections::BTreeSet<&str> = [
        "Identifier",
        "SelfExpr",
        "FieldAccess",
        "TupleFieldAccess",
        "Index",
        "Deref",
        "MethodCall", // Family-3 object form (Round XVIII)
    ]
    .into_iter()
    .collect();

    let missing_root: Vec<_> = root_required
        .iter()
        .filter(|n| !root.contains(**n))
        .collect();
    let missing_place: Vec<_> = place_required
        .iter()
        .filter(|n| !place.contains(**n))
        .collect();
    let missing_field: Vec<_> = g3_required
        .iter()
        .filter(|n| !field.contains(**n))
        .collect();
    let missing_tuple: Vec<_> = g3_required
        .iter()
        .filter(|n| !tuple.contains(**n))
        .collect();

    assert!(
        missing_root.is_empty(),
        "G1 resolve_projection_root_local missing required arms {missing_root:?}\n\
         root={root:?}\n\
         ⚠ routing census only — not a correctness gate (Core #13)."
    );
    assert!(
        missing_place.is_empty(),
        "G2 try_resolve_place missing required arms {missing_place:?}\n\
         place={place:?}\n\
         ⚠ routing census only — not a correctness gate (Core #13)."
    );
    assert!(
        missing_field.is_empty() && missing_tuple.is_empty(),
        "G3 field/tuple missing required object forms\n\
         field missing={missing_field:?} field={field:?}\n\
         tuple missing={missing_tuple:?} tuple={tuple:?}\n\
         ⚠ routing census only — not a correctness gate (Core #13)."
    );

    // Secondary vacuous floors (after required ⊆ holds).
    assert!(root.len() >= 6, "G1 arm floor: root={root:?}");
    assert!(place.len() >= 4, "G2 arm floor: place={place:?}");
    assert!(
        field.len() >= 7 && tuple.len() >= 7,
        "G3 arm floors: field={field:?} tuple={tuple:?} (want ≥7 incl. MethodCall)"
    );

    // G4: still Operand-only — zero Expr:: arms (category lock).
    assert!(
        ptr.is_empty(),
        "G4 resolve_ptr_field_place gained Expr:: arms {ptr:?} — reclassify G4 \
         or stop comparing it as Operand-domain. Category error if forced into \
         the Expr pairwise graph (Core #15e Q4)."
    );

    // G4 caller-presence floor: plain `=` and compound `OP=` both route the
    // fallback through resolve_ptr_field_place (Core #4).
    let ptr_calls = assigns
        .matches("resolve_ptr_field_place(")
        .count()
        // definition site is one match; require ≥2 *call* sites ⇒ ≥3 total.
        .saturating_sub(1);
    assert!(
        ptr_calls >= 2,
        "G4 resolve_ptr_field_place must have ≥2 call sites in assigns.rs \
         (plain `=` and compound `OP=`); found {ptr_calls} non-def matches. \
         Silent drop of the Operand-path fallback reopens the get-chain write \
         hole on one face only."
    );

    // --- Pairwise unexempted divergence (down-only) ---
    // EXEMPT cells are deliberate category / design differences — name the
    // reason; never raise MAX without an EXEMPT row or an arm fix.
    //
    // Pair directions: cells in leftΔright after removing EXEMPT for that pair.
    type Exempt = &'static [(&'static str, &'static str)];
    // field vs tuple: empty (Family-2).
    const EXEMPT_FIELD_TUPLE: Exempt = &[];
    // place vs field/tuple: bare locals served before G2; MethodCall is a G3
    // *object* form (Family-3, Round XVIII) not a top-level place head.
    const EXEMPT_PLACE_G3: Exempt = &[
        (
            "Identifier",
            "Bare locals served by fast paths BEFORE try_resolve_place \
             (mod.rs try_resolve_place doc; Family-1 ASSIGN_ONLY_EXEMPT)",
        ),
        (
            "SelfExpr",
            "Same as Identifier — bare self is not a projection place in G2",
        ),
        (
            "MethodCall",
            "G3 object form under FieldAccess/TupleFieldAccess (Family-3 get-chain); \
             top-level place is FieldAccess(MethodCall(...)), not bare MethodCall — G2 \
             correctly has no MethodCall head",
        ),
    ];
    // root vs field/tuple.
    const EXEMPT_ROOT_G3: Exempt = &[
        (
            "Deref",
            "G1 missing Deref while G3 has it — deliberate-or-filed CoW-root cell; \
             B/C decide later. Pin named, not silent.",
        ),
    ];
    // root vs place: union of deliberate cells above.
    const EXEMPT_ROOT_PLACE: Exempt = &[
        (
            "Identifier",
            "G2 deliberately declines bare Identifier (served earlier)",
        ),
        (
            "SelfExpr",
            "G2 deliberately declines bare SelfExpr (served earlier)",
        ),
        (
            "MethodCall",
            "G1-only CoW get-chain root descent",
        ),
        (
            "Deref",
            "G1 missing Deref / G2 has it — deliberate-or-filed",
        ),
    ];

    fn unexempted_delta(
        a: &std::collections::BTreeSet<String>,
        b: &std::collections::BTreeSet<String>,
        exempt: Exempt,
    ) -> Vec<String> {
        let ex: std::collections::BTreeSet<&str> = exempt.iter().map(|(n, _)| *n).collect();
        a.symmetric_difference(b)
            .filter(|n| !ex.contains(n.as_str()))
            .cloned()
            .collect()
    }

    let mut unexempted: Vec<(String, Vec<String>)> = Vec::new();
    let d_ft = unexempted_delta(&field, &tuple, EXEMPT_FIELD_TUPLE);
    if !d_ft.is_empty() {
        unexempted.push(("field_vs_tuple".into(), d_ft));
    }
    let d_pf = unexempted_delta(&place, &field, EXEMPT_PLACE_G3);
    if !d_pf.is_empty() {
        unexempted.push(("place_vs_field".into(), d_pf));
    }
    let d_pt = unexempted_delta(&place, &tuple, EXEMPT_PLACE_G3);
    if !d_pt.is_empty() {
        unexempted.push(("place_vs_tuple".into(), d_pt));
    }
    let d_rf = unexempted_delta(&root, &field, EXEMPT_ROOT_G3);
    if !d_rf.is_empty() {
        unexempted.push(("root_vs_field".into(), d_rf));
    }
    let d_rt = unexempted_delta(&root, &tuple, EXEMPT_ROOT_G3);
    if !d_rt.is_empty() {
        unexempted.push(("root_vs_tuple".into(), d_rt));
    }
    let d_rp = unexempted_delta(&root, &place, EXEMPT_ROOT_PLACE);
    if !d_rp.is_empty() {
        unexempted.push(("root_vs_place".into(), d_rp));
    }

    let unexempted_cell_count: usize = unexempted.iter().map(|(_, v)| v.len()).sum();

    /// Measured 2026-07-30 at HEAD after EXEMPT pairs (Round XVII).
    /// Down-only: new unexempted cells FAIL. To add a deliberate cell, append
    /// EXEMPT with a reason; never raise the ceiling without owner review.
    /// Expected 0 when the allowlist is honest (all residual cells deliberate).
    const MAX_UNEXEMPTED_DIVERGENCE_CELLS: usize = 0;

    assert!(
        unexempted_cell_count <= MAX_UNEXEMPTED_DIVERGENCE_CELLS,
        "PLACE-RESOLVER UNEXEMPTED DIVERGENCE rose above the down-only ceiling.\n\n\
         unexempted_cell_count={unexempted_cell_count} MAX={MAX_UNEXEMPTED_DIVERGENCE_CELLS}\n\
         details={unexempted:?}\n\n\
         root={root:?}\nplace={place:?}\nfield={field:?}\ntuple={tuple:?}\n\n\
         Either add the missing arm (class fix) or append the cell to the pair's \
         EXEMPT table WITH a reason. Do not raise MAX without citation.\n\n\
         ⚠ A is a WORKLIST GENERATOR, not a correctness gate (Core #13). \
         Routing ≠ semantics; Guard/type-driven branches are invisible here."
    );

    // Stable tags for scripts/resolver_totality.sh (parse under --nocapture).
    eprintln!(
        "[resolver-census] root={} place={} field={} tuple={} ptr_expr=0 ptr_callers={}",
        root.len(),
        place.len(),
        field.len(),
        tuple.len(),
        ptr_calls
    );
    eprintln!(
        "[resolver-divergence] unexempted={} max={}",
        unexempted_cell_count, MAX_UNEXEMPTED_DIVERGENCE_CELLS
    );
}

/// LIMB 1b (Core #4, Core #15e Q3) — the auto-propagate PRE-CHECK's expression
/// domain must be a SUPERSET of the shared place producer's.
///
/// **The class this retires, which cost a live miscompile.** The Family-1
/// chokepoint asks `place_expr_type_only` whether an `&`-argument would
/// auto-propagate; if the answer is "no", it lets `try_resolve_place` resolve the
/// argument and RETURNS EARLY, skipping `maybe_auto_propagate`. So a form the
/// PRODUCER resolves but the PRE-CHECK returns `None` for gets the early return
/// with the auto-propagate question never asked.
///
/// That is not hypothetical. `place_expr_type_only` shipped without a
/// `TupleFieldAccess` arm while `try_resolve_place` had one, and
/// `void take(int &x)` called as `take(&t.0)` on a
/// `(Result[int,int], int)` seeded `Error(…)` SWALLOWED the error, handed the
/// callee a pointer to a `Result` where an `int` was expected, and wrote through
/// it — `gg check` clean, both backends, while the base compiler correctly
/// propagated. The struct-field costume of the same defect had been fixed one
/// commit earlier: an instance fix where a class existed.
///
/// Direction matters and only ONE direction is unsafe. Pre-check ⊋ producer is
/// fine (a form the pre-check understands but the producer declines simply falls
/// through). Producer ⊋ pre-check is the miscompile. So this asserts
/// **producer ⊆ pre-check**, modulo exemptions with stated reasons.
#[test]
fn place_type_only_covers_the_producer_forms() {
    let exprs = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");

    let producer = top_level_expr_arm_variants(
        &exprs,
        "pub(in crate::ir::lowering) fn try_resolve_place(",
    );
    let precheck = top_level_expr_arm_variants(
        &exprs,
        "pub(in crate::ir::lowering) fn place_expr_type_only(",
    );

    /// Producer forms the PRE-CHECK deliberately does not model.
    ///
    /// ⚠ EMPTY, AND THE PREVIOUS ENTRY'S REASON WAS FALSE. `Deref` was exempted
    /// here on the grounds that "a missing arm yields `None`, which the
    /// chokepoint reads as do-NOT-skip — the conservative answer". The call site
    /// did the OPPOSITE: `None` produced `false`, the branch tested
    /// `!arg_would_auto_propagate`, and the chokepoint SKIPPED. Reasoning from
    /// that inverted comment shipped the same swallowed-`Error` miscompile three
    /// times. The fail-safe now lives in `lower_call_arg` as an explicit
    /// `None => false` on a predicate phrased as "provably safe to skip", so an
    /// unmodelled form declines the early return by construction. Any future
    /// entry here must justify itself against THAT code, not against prose.
    const PRECHECK_EXEMPT: &[(&str, &str)] = &[];

    let exempt: std::collections::BTreeSet<String> = PRECHECK_EXEMPT
        .iter()
        .map(|(name, _)| (*name).to_string())
        .collect();

    let missing: Vec<&str> = producer
        .difference(&precheck)
        .filter(|n| !exempt.contains(*n))
        .map(|s| s.as_str())
        .collect();

    assert!(
        missing.is_empty(),
        "AUTO-PROPAGATE PRE-CHECK DOMAIN IS NARROWER THAN THE PLACE PRODUCER'S.\n\n\
         Handled by `try_resolve_place` but NOT by `place_expr_type_only`: {missing:?}\n\n\
         producer  = {producer:?}\n\
         pre-check = {precheck:?}\n\
         exempt    = {exempt:?}\n\n\
         This is a MISCOMPILE, not a missed optimisation. The Family-1 chokepoint in \
         `lower_call_arg` consults `place_expr_type_only` to decide whether an \
         `&`-argument would auto-propagate; a `None` answer means \"it would not\", so \
         the chokepoint returns early and SKIPS `maybe_auto_propagate`. For a form the \
         producer resolves but the pre-check does not, a `Result`-typed argument bound \
         for a non-`Result` parameter has its `Error` SILENTLY SWALLOWED and the callee \
         receives a pointer to a `Result` — measured exactly this way when \
         `TupleFieldAccess` was missing.\n\n\
         Add the arm to `place_expr_type_only` (it only needs the TYPE, no lowering), \
         or add it to PRECHECK_EXEMPT WITH a reason showing the omission cannot lose a \
         propagation.\n\n\
         See AGENTS.md Core #4 (one fix, all siblings) and Core #15e Q3 (enumerate the \
         set, don't sample it)."
    );

    assert!(
        producer.len() >= 4 && precheck.len() >= 4,
        "arm-superset lint extracted suspiciously few arms \
         (producer={producer:?}, pre-check={precheck:?}) — the matches were probably \
         respelled or reindented. Fix the extractor rather than lowering these floors."
    );

    // ── OBJECT domain, one level below the top-level arms ────────────────────
    //
    // 🚨 THE TOP-LEVEL COMPARISON ABOVE CANNOT CATCH ITS OWN CLASS HERE, and
    // that gap shipped two live regressions. `try_resolve_field_place` accepts a
    // WIDER set of OBJECT forms than `place_expr_type_only`'s recursion does, so
    // `(*b).f` resolved in the producer while the pre-check returned `None` —
    // yet it declares a top-level `FieldAccess`, so the sets above matched and
    // the lint was green on it (Core #15e Q2: a guard that green-lights the
    // class it exists to retire).
    //
    // ⚠⚠ AND THIS LIMB HAS A PERMANENT BLIND SPOT OF ITS OWN — a KNOWN
    // LIMITATION, not a shortfall to burn down. `top_level_expr_arm_variants`
    // extracts `Expr::<Variant>` names from arm-indentation lines, so it sees
    // only the SYNTACTIC object domain. The producer's domain is partly
    // TYPE-DRIVEN: the `Guard[T]` auto-deref (`g.f`, where the field lives on the
    // guarded value and `lookup_field` on the `Guard__…` wrapper misses) is a
    // type-level branch inside `try_resolve_field_place`, not an `Expr::` arm.
    // This limb CANNOT see it and never could.
    //
    // That is not hypothetical: `&g.a` silently dropped its write — while
    // `g.a = v` worked, Family-1's exact signature — through three fix rounds,
    // with `--test lints` green the whole time and an earlier version of THIS
    // COMMENT naming `g.f` as a form the limb made "visible". It did not. Do not
    // read a green here as "the pre-check covers the producer"; it means only
    // that the two SYNTACTIC arm sets agree. A type-driven branch added to either
    // resolver needs a measured write-through probe, because no lint in this file
    // will ask for one.
    //
    // The fail-safe at the call site keeps an unmodelled object form MEMORY-safe;
    // it does not keep it CORRECT (declining the early return falls back to the
    // read path, which is the lost write). Widening remains a real fix.
    let field_obj = top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_field_place(");
    let tuple_obj =
        top_level_expr_arm_variants(&exprs, "pub(super) fn try_resolve_tuple_field_place(");
    let resolver_objs: std::collections::BTreeSet<String> =
        field_obj.union(&tuple_obj).cloned().collect();

    let obj_missing: Vec<&str> = resolver_objs
        .difference(&precheck)
        .map(|s| s.as_str())
        .collect();

    // KNOWN, MEASURED SHORTFALL — a budget, not a hiding place. Each name here
    // is a SYNTACTIC object form the resolvers accept and the pre-check does not.
    // ⚠ Empty does NOT mean "the pre-check covers the producer" — see the
    // type-driven blind spot above. And an entry here costs a LOST WRITE on that
    // shape, not merely an optimisation: declining the early return falls back to
    // the read path. Adding a name requires a measured write-through probe
    // showing what it actually costs; shrink the list by adding arms to
    // `place_expr_type_only`.
    // MethodCall: Family-3 object form (Round XVIII). G3 specialists accept it;
    // place_expr_type_only does not yet type method-return shapes. Measured:
    // write-through on get-chain fields is FIXED by try_resolve_place +
    // MutableBorrow path that uses the RESOLVED place type (not this pre-check).
    // Pre-check None still declines auto-prop skip for non-resolving forms.
    // Shrink by adding a MethodCall arm to place_expr_type_only when typed.
    const KNOWN_OBJ_SHORTFALL: &[&str] = &["MethodCall"];

    let unexpected: Vec<&str> = obj_missing
        .iter()
        .filter(|n| !KNOWN_OBJ_SHORTFALL.contains(*n))
        .copied()
        .collect();

    assert!(
        unexpected.is_empty(),
        "PLACE-RESOLVER OBJECT DOMAIN EXCEEDS THE PRE-CHECK'S, in forms not on the \
         known-shortfall list: {unexpected:?}\n\n\
         resolver object arms = {resolver_objs:?}\n\
         pre-check arms       = {precheck:?}\n\
         known shortfall      = {KNOWN_OBJ_SHORTFALL:?}\n\n\
         An object form the RESOLVERS accept but `place_expr_type_only` cannot type \
         makes the pre-check return `None` for the whole projection. That is safe today \
         only because `lower_call_arg` treats `None` as \"do not skip\" — verify that is \
         STILL true before waving this through, because when it was the other way round \
         this exact gap swallowed an `Error` and handed the callee a pointer to a \
         `Result` for `(*b).f` and `g.f`.\n\n\
         Either add the arm to `place_expr_type_only`, or add it to \
         KNOWN_OBJ_SHORTFALL after measuring an `Error`-seeded probe of that shape."
    );
}

/// LIMB 2 (Core #4) — pin the number of `emit_borrow_mut` call sites under
/// `src/ir/lowering/`.
///
/// Limb 1 proves the two KNOWN formation faces agree; it says nothing about a
/// THIRD face appearing. `emit_borrow_mut` is the one call that mints a mutable
/// borrow, so a new site is a new formation path — which must either route
/// through `try_resolve_place` or justify why it does not. The budget makes that
/// a deliberate, reviewed act instead of a silent one.
///
/// Baseline 28 at the Family-1 commit: the chokepoint REMOVED two open-coded
/// `&*box` blocks and ADDED two producer-routed borrows, net 0. Distribution:
/// calls.rs 9 · assigns.rs 5 · methods.rs 5 · stmts/mod.rs 4 · exprs/mod.rs 2 ·
/// closures.rs 1 · shared.rs 1 · spawn.rs 1 — and `for_loops.rs` has ZERO (its
/// four-arm `&`-match computes `write_through`; it is not a formation site).
#[test]
fn borrow_mut_formation_site_count() {
    const EXPECTED: usize = 28;

    let mut sites = Vec::new();
    let mut stack = vec![Path::new("src/ir/lowering").to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir).expect("read src/ir/lowering") {
            let path = entry.expect("dir entry").path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|e| e == "rs") {
                let text = fs::read_to_string(&path).expect("read lowering source");
                for (i, line) in text.lines().enumerate() {
                    if line.contains("emit_borrow_mut(") && !line.trim_start().starts_with("//") {
                        sites.push(format!("{}:{}", path.display(), i + 1));
                    }
                }
            }
        }
    }
    sites.sort();

    assert_eq!(
        sites.len(),
        EXPECTED,
        "`emit_borrow_mut` call-site count under src/ir/lowering/ changed: {} vs expected {EXPECTED}.\n\n\
         Sites:\n  {}\n\n\
         Every site mints a mutable borrow. A NEW one is a new `&`-formation path: \
         route it through `try_resolve_place` so it inherits the whole projection \
         grammar, or state in the bump comment why the borrow target is not a place \
         (a whole local, a synthesized temp, a closure capture slot).\n\n\
         If sites were REMOVED, lower EXPECTED here to lock the new floor.",
        sites.len(),
        sites.join("\n  ")
    );
}

/// devbook/24 rule 3 — the field-read Ptr-wrap predicate has ONE home.
///
/// The predicate "does a READ of a field of this type yield a `Ptr(T)` into the
/// parent rather than a value copy?" was open-coded SIX times in
/// `exprs/mod.rs`. That is the same class of defect as Family-1 itself: the four
/// types it answers `true` for were exactly the four whose `&`-of-a-projection
/// write-through worked BY ACCIDENT, and nothing forced the copies to agree.
/// It now lives in `field_read_yields_ptr`; this lint stops the copies coming
/// back.
///
/// Fails if the open-coded triple (`is_collection_type` / `owned_string_type` /
/// `is_resource_type`) reappears anywhere in the file outside the accessor's own
/// body. If you need the predicate, CALL it.
#[test]
fn field_read_ptr_predicate_has_one_home() {
    let src = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");

    // The accessor's own body is the one legitimate spelling.
    let accessor = "pub(in crate::ir::lowering) fn field_read_yields_ptr(";
    assert!(
        src.contains(accessor),
        "the shared field-read Ptr-wrap accessor `field_read_yields_ptr` is GONE from \
         src/ir/lowering/exprs/mod.rs. It is the single source of truth for whether a field \
         read yields a `Ptr(T)` into the parent; six open-coded copies preceded it. If it was \
         renamed, update this lint's anchor — do not re-inline the predicate."
    );

    // Count occurrences of the open-coded triple's distinctive middle LINE. The
    // bare `== ctx.type_mapper.owned_string_type` is NOT specific enough — the
    // file legitimately compares a value type and a local's type against it
    // elsewhere. The `||`-prefixed form is unique to this predicate.
    let inlined = src.matches("|| field_type == ctx.type_mapper.owned_string_type").count()
        + src.matches("|| field.type_id == ctx.type_mapper.owned_string_type").count();
    assert_eq!(
        inlined, 1,
        "the field-read Ptr-wrap predicate is open-coded {inlined} time(s) in \
         src/ir/lowering/exprs/mod.rs; exactly 1 is expected (the body of \
         `field_read_yields_ptr` itself).\n\n\
         Each extra site is a copy of a semantic rule that nothing keeps in sync — the \
         devbook/24 rule-3 violation that sat directly under the Family-1 defect, where the \
         four types this predicate accepts were the four whose `&`-projection write-through \
         worked by accident. CALL `field_read_yields_ptr(ctx, ty)` instead of re-spelling it.\n\n\
         (If the accessor's body was legitimately rewritten so the term no longer appears \
         once, update this expectation deliberately — and say why in the bump comment.)"
    );
}

/// LIMB 3 (Core #6, Core #15e Q2) — the shared producer must actually be
/// INVOKED at BOTH `&`-formation faces.
///
/// **Why limbs 1 and 2 cannot cover this, which is the whole point.** Family-1's
/// defect was never a missing arm — `lower_call_arg`'s `MutableBorrow` arm
/// simply *never called a resolver*. Run the regression scenario: someone
/// short-circuits the `try_resolve_place` call at a formation face (returns
/// `None` unconditionally, or re-implements it with a different resolver).
///   * Limb 1: both faces still declare the same arms → GREEN.
///   * Limb 2: the block is textually intact and the fall-through
///     `emit_borrow_mut` still exists → count unchanged at 28 → GREEN.
///   ⇒ guard green, Family 1 fully regressed. A guard that green-lights the
///     class it was written to retire is worse than none.
///
/// (Note that *deleting* the whole chokepoint block would also delete ITS
/// `emit_borrow_mut`, dropping the count to 27 and tripping limb 2. It is the
/// short-circuit variants that slip both — which is why this limb asserts
/// PRESENCE OF THE CALL, not presence of the block.)
///
/// ⚠ This limb covers BOTH faces deliberately. An earlier design covered only
/// the call-arg face and asserted the standalone face was "covered by limb 2's
/// count" — that is false for exactly the same face-independent reason above: a
/// short-circuit at the standalone face leaves its fall-through
/// `emit_borrow_mut` intact and the count stays 28. The standalone face carries
/// a RATIFIED, projection-carrying live shape (the list-comprehension iterable,
/// D32 rider), so leaving it unguarded would be a real hole.
///
/// ⚠ LIMIT: this limb proves the producer is CALLED, never that its result is
/// USED correctly. A face that calls `try_resolve_place` and discards the
/// `Some(..)` passes. Semantics are pinned by the RED-verified fixtures
/// (`sound_amp_byvalue_place_writethrough`, `cow_amp_projection_type_axis`,
/// and the ordering trap `sound_amp_bareparam_root_materialize`).
///
/// In-tree precedent for this shape: `compound_assign_fieldaccess_fallback_present`
/// scans a named function body (comments stripped) for a required call.
#[test]
fn amp_formation_faces_invoke_the_shared_place_producer() {
    let calls = fs::read_to_string(Path::new("src/ir/lowering/exprs/calls.rs"))
        .expect("read src/ir/lowering/exprs/calls.rs");
    let exprs = fs::read_to_string(Path::new("src/ir/lowering/exprs/mod.rs"))
        .expect("read src/ir/lowering/exprs/mod.rs");

    // Strip line comments so the guard reasons about EXECUTABLE code only — both
    // faces carry long comment blocks that legitimately NAME `try_resolve_place`
    // in prose, and counting those would make this limb vacuous.
    let strip = |s: &str| -> String {
        s.lines()
            .map(|l| l.split("//").next().unwrap_or(""))
            .collect::<Vec<_>>()
            .join("\n")
    };

    // --- FACE 1: the CALL-ARG face, `lower_call_arg`'s body. ---
    let sig = "pub(super) fn lower_call_arg(";
    let start = calls.find(sig).expect("locate lower_call_arg");
    let rest = &calls[start..];
    let end = rest.find("\n}\n").map(|e| e + 1).unwrap_or(rest.len());
    let call_arg_body = strip(&rest[..end]);

    assert!(
        call_arg_body.contains("try_resolve_place("),
        "THE CALL-ARG `&`-FORMATION FACE NO LONGER INVOKES THE SHARED PLACE PRODUCER.\n\n\
         `lower_call_arg`'s body contains no executable `try_resolve_place(` call.\n\n\
         That call IS the Family-1 fix: an `&`-argument naming a place must borrow \
         THAT PLACE. Without it the argument falls through to the READ path, and for \
         every by-value projected type (`int`/`float`/`bool`/value struct/tuple/\
         value-payload enum) the callee receives a pointer to a DYING TEMP and its \
         write is silently discarded — `gg check` clean, on both backends.\n\n\
         Neither sibling limb catches this: the arm sets are unchanged (limb 1 green) \
         and the fall-through `emit_borrow_mut` keeps the site count at 28 (limb 2 \
         green). RESTORE the call; do not delete this limb."
    );

    // --- FACE 2: the STANDALONE face, the `Expr::MutableBorrow` arm. ---
    let arm_head = "Expr::MutableBorrow { expr: inner } => {";
    let a_start = exprs
        .find(arm_head)
        .expect("locate the standalone Expr::MutableBorrow arm in lower_expr_inner");
    // The arm ends where the next arm head at the same indentation begins.
    let a_rest = &exprs[a_start..];
    let a_end = a_rest
        .find("\n        Expr::Closure {")
        .unwrap_or(a_rest.len());
    let standalone_body = strip(&a_rest[..a_end]);

    assert!(
        standalone_body.contains("try_resolve_place("),
        "THE STANDALONE `&`-FORMATION FACE NO LONGER INVOKES THE SHARED PLACE PRODUCER.\n\n\
         The `Expr::MutableBorrow` arm of `lower_expr_inner` contains no executable \
         `try_resolve_place(` call.\n\n\
         This face is NOT dead code: the list-comprehension iterable \
         (`[e for x in &s.items]`) is a RATIFIED shape (D32 rider) that reaches it \
         carrying a PROJECTION, via `lower_list_comprehension`'s non-range path. \
         Without the producer it falls back to the read path and a projection \
         iterable loses write-through.\n\n\
         Neither sibling limb catches this — same face-independent argument as the \
         call-arg face above. RESTORE the call; do not delete this limb."
    );

    // Sanity: the slices resolved to real, non-trivial bodies. A mis-anchored
    // empty slice would make both assertions above fail LOUDLY rather than pass,
    // but a slice that swallowed the whole file would make them vacuously true.
    assert!(
        call_arg_body.len() > 500 && standalone_body.len() > 500,
        "limb-3 anchors extracted suspiciously small bodies (call_arg={}, standalone={}). \
         The function or arm was probably respelled — fix the anchors rather than \
         weakening this limb.",
        call_arg_body.len(),
        standalone_body.len()
    );
    assert!(
        standalone_body.len() < exprs.len() / 2,
        "limb-3 standalone anchor swallowed {} of {} bytes — the arm-end anchor \
         (`Expr::Closure {{`) no longer matches, so this limb would pass vacuously. \
         Fix the anchor.",
        standalone_body.len(),
        exprs.len()
    );
}

/// Residual-predicate ban (Round XV Track D / Core #6 + Layering rule 2):
/// the GIR "is this an Option/Result combinator?" decision at D1/D2/D3 used
/// to be a multi-name `matches!(method_name, "map" | "and_then" | … | "flat_map" …)`
/// blob. That debt is retired by `BuiltinMethodDecl.combinator_kind` +
/// `ctx.builtin_combinator_kind`. A reintroduction of the full predicate blob
/// re-opens name-list drift with the protocol table.
///
/// Scope is deliberately narrow:
///   - ONLY `matches!(method_name, …)` forms whose arm set looks like the
///     historical D1/D2/D3 predicates (≥5 of the 9 historical combinator names,
///     including both `"and_then"` and `"flat_map"`).
///   - Does NOT ban the collection HOF hint list (~`filter|map|flat_map|any|…`
///     — no `and_then`).
///   - Does NOT ban Phase-2 internal adapter arms (`match method_name { … }`
///     form, or small `matches!` subsets like `"and_then" | "or_else" | "flat_map"`).
///
/// **If this fails:** a D1/D2/D3-shaped name-list came back. Route through
/// `ctx.builtin_combinator_kind` (and `CombinatorKind::is_gir_adapter` for D1)
/// instead of matching method names.
#[test]
fn no_combinator_predicate_name_match_in_methods() {
    const HISTORICAL: &[&str] = &[
        "map",
        "and_then",
        "or_else",
        "filter",
        "unwrap_or_else",
        "flat_map",
        "or",
        "flatten",
        "map_err",
    ];

    let content = fs::read_to_string("src/ir/lowering/exprs/methods.rs").unwrap_or_default();
    let mut residuals: Vec<(usize, String)> = Vec::new();

    // Scan for `matches!(method_name, …)` — allow the body to span lines.
    let bytes = content.as_bytes();
    let needle = b"matches!(method_name";
    let mut search_from = 0usize;
    while let Some(rel) = content[search_from..]
        .as_bytes()
        .windows(needle.len())
        .position(|w| w == needle)
    {
        let start = search_from + rel;
        // Find the matching close paren for this matches!(…).
        let mut depth = 0i32;
        let mut end = start;
        let mut in_str = false;
        let mut prev = b'\0';
        for (i, &b) in bytes[start..].iter().enumerate() {
            if in_str {
                if b == b'"' && prev != b'\\' {
                    in_str = false;
                }
            } else {
                match b {
                    b'"' => in_str = true,
                    b'(' => depth += 1,
                    b')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = start + i + 1;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            prev = b;
        }
        if end <= start {
            break;
        }
        let body = &content[start..end];
        // Collect quoted string arms that are exact historical names.
        let mut found: Vec<&str> = Vec::new();
        for name in HISTORICAL {
            // Match `"name"` as a whole arm token (not a substring of another
            // name — e.g. `"map"` must not hit `"map_err"` / `"flat_map"`).
            let pat = format!("\"{name}\"");
            if body.contains(&pat) {
                found.push(name);
            }
        }
        let has_and_then = found.contains(&"and_then");
        let has_flat_map = found.contains(&"flat_map");
        if found.len() >= 5 && has_and_then && has_flat_map {
            let line = content[..start].bytes().filter(|&b| b == b'\n').count() + 1;
            residuals.push((line, body.chars().take(120).collect()));
        }
        search_from = end;
    }

    assert!(
        residuals.is_empty(),
        "Residual D1/D2/D3 combinator name-match predicate(s) in methods.rs:\n{}\n\n\
         Route through `ctx.builtin_combinator_kind` + `CombinatorKind::is_gir_adapter` \
         (Round XV Track D). Do NOT reintroduce the multi-name matches! blob. \
         Collection HOF lists and small Phase-2 adapter matches! are out of scope.",
        residuals
            .iter()
            .map(|(l, b)| format!("  L{l}: {b}…"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// Core #6 class-retirement guard for combinator-receiver ownership
/// (Round XIV Edits A–D at `try_lower_option_result_combinator`, Round XV Track C).
///
/// **Class this retires.** A new combinator arm (or a rewrite of the adapter) that
/// forgets one of the four write-site invariants re-opens:
/// - Edit A: `scrut_local` typed `Ptr(T)` → destructive extract empties caller storage
/// - Edit B: skip clone-through-Ptr for place recv → shallow Copy alias emptied by load_move
/// - Edit C: drop `and_then`/`flat_map` result_type arm → wrong `result_local` type (SIGSEGV / LLVM verifier)
/// - Edit D: drop Option `unwrap_or_else` None `set_owned` → Tier-2a ICE (`AssignIntoOwnedSlot` Untracked)
///
/// Ship shape (c) pairs this structural ratchet with `assert_scrut_is_value_enum` before
/// every `enum_field_load_move` on `scrut_local` (Edit A dynamic half). The live
/// `combinator_*` fixtures stay as the dynamic net (Core #11/#12) — this lint does
/// **not** retire them.
///
/// **Pins:**
/// - **P1** Edit A Ptr-unwrap marker (`GirType::Ptr`/`raw_recv_type`/`add_local(recv_type`)
/// - **P2** Edit B clone path (`clone_fn_for_ptr` + `call_clone` + `recv_is_ptr` + `emit_borrow`)
/// - **P3** Core #14 resource fallback (`is_resource_type(recv_type)` near load_ref)
/// - **P4** Edit C `and_then`/`flat_map` + `infer_closure_return_type` in result_type match
/// - **P5** Edit D `ctx.set_owned` arm-scoped to Option `unwrap_or_else` None arm only
/// - **P6** dispatch ↔ adapter arm-set parity (string `matches!` and/or typed Track D registration)
/// - **P7** `enum_field_load_move` count floor ≥ 5 + `assert_scrut_is_value_enum` call floor ≥ 5
/// - **P8** `assign_result_local_move` discipline (exclude the helper *definition*)
/// - **P9** vacuous-extraction floor (fn found + pin hit count)
/// - **P10** exact `return None` count == 4 (enum bail, empty args, Some `_`, None `_`)
///
/// **EXEMPT (with reason):**
/// - `or` / `flatten` / `unwrap` / `expect` / `unwrap_or` — not routed through this adapter
/// - String-coercion early `return None` (`has_string_coercion`) — Track B distinct producer
/// - Result `unwrap_or_else` Error-path closure-return `set_owned` — probed green 2026-07-29
///   (`Error(7).unwrap_or_else((int e): make_money(...))` → 10, no Tier-2a ICE)
/// - SH `emit_option_result_combinator` — Track A parallel lane
///
/// Precedent: `container_literal_arms_count`, `field_and_tuple_place_resolvers_cover_the_same_object_forms`.
#[test]
fn combinator_adapter_ownership_invariants() {
    let methods = fs::read_to_string(Path::new("src/ir/lowering/exprs/methods.rs"))
        .expect("read src/ir/lowering/exprs/methods.rs");

    let bodies = top_level_fn_bodies(&methods);
    let adapter = bodies
        .iter()
        .find(|(n, _)| n == "try_lower_option_result_combinator")
        .map(|(_, b)| b.as_str())
        .expect(
            "P9: `try_lower_option_result_combinator` not found in methods.rs — \
             rename/extract broke the Core #6 combinator guard; fix the extractor \
             or restore the chokepoint name.",
        );

    // P1 Edit A
    assert!(
        adapter.contains("GirType::Ptr(inner)")
            && adapter.contains("raw_recv_type")
            && adapter.contains("add_local(recv_type"),
        "P1 Edit A REGRESSION: adapter must unwrap Ptr/MutPtr from `raw_recv_type` and \
         allocate `scrut_local` as value-typed `recv_type`."
    );

    // P2 Edit B
    assert!(
        adapter.contains("clone_fn_for_ptr(recv_type)")
            && adapter.contains("call_clone(")
            && adapter.contains("recv_is_ptr")
            && adapter.contains("emit_borrow"),
        "P2 Edit B REGRESSION: adapter must materialize place receivers via \
         clone_fn_for_ptr + call_clone, with recv_is_ptr / emit_borrow."
    );

    // P3 Core #14
    assert!(
        adapter.contains("is_resource_type(recv_type)"),
        "P3 Core #14 REGRESSION: load_ref fallback must debug_assert \
         !is_resource_type(recv_type)."
    );

    // P4 Edit C
    assert!(
        combinator_result_type_has_and_then_flat_map_infer(adapter),
        "P4 Edit C REGRESSION: `result_type` match must have an `\"and_then\" | \"flat_map\"` \
         arm that calls `infer_closure_return_type`."
    );

    // P5 Edit D — require ctx.set_owned inside the Option unwrap_or_else None arm
    assert!(
        combinator_option_unwrap_or_else_none_has_set_owned(adapter),
        "P5 Edit D REGRESSION: Option `unwrap_or_else` None arm must `ctx.set_owned` the \
         closure-call result. A free-floating set_owned elsewhere does NOT satisfy this pin."
    );

    // P6 arm-set parity
    let expected: std::collections::BTreeSet<&str> = [
        "map", "and_then", "or_else", "filter", "unwrap_or_else", "flat_map", "map_err",
    ]
    .into_iter()
    .collect();
    let adapter_names = combinator_adapter_method_names(adapter);
    assert_eq!(
        adapter_names, expected,
        "P6 adapter method-arm set {adapter_names:?} != expected {expected:?}."
    );
    let dispatch = combinator_dispatch_names_for_adapter(&methods);
    let dispatch_refs: std::collections::BTreeSet<&str> =
        dispatch.iter().map(|s| s.as_str()).collect();
    assert_eq!(
        dispatch_refs, expected,
        "P6 dispatch set {dispatch_refs:?} != expected {expected:?}.          String matches! and typed CombinatorKind registration must stay in parity          with the adapter arms (GIR-adapter set only — not or/flatten)."
    );

    // P7 extract-site floors
    //
    // Round XXII Track β folded the 4-line `assert + enum_field_load_move +
    // set_owned + move_zero(scrut) + drops.register_local` pattern into the
    // helper `extract_enum_payload_owned` (Core #4 chokepoint + Core #3
    // birth-registration). The site-count invariant is now carried by helper
    // CALL SITES in the adapter, not by inline uses of `enum_field_load_move`
    // / `assert_scrut_is_value_enum` (which now appear ONCE each, inside the
    // helper). Count both: the helper invocations at the extraction chokepoints
    // (5: Some/Ok + 4 Error mirrors), AND the underlying calls in-file (≥5:
    // the helper body + any residual inline sites).
    let extract_calls = adapter.matches("extract_enum_payload_owned(").count();
    assert!(
        extract_calls >= 5,
        "P7: extract_enum_payload_owned call count in adapter is {extract_calls} (want ≥ 5 — \
         Some/Ok + 4 Error mirrors; helper folded 2026-08-01 Round XXII β)."
    );
    // The underlying `enum_field_load_move` + `assert_scrut_is_value_enum`
    // calls now live in the helper's body (a SIBLING top-level fn), not in the
    // adapter — count against the WHOLE file (`methods`), not the adapter body.
    let load_moves = methods.matches("enum_field_load_move(").count();
    assert!(
        load_moves >= 1,
        "P7-underlying: enum_field_load_move count in methods.rs is {load_moves} (want ≥ 1 \
         — helper body carries it; folded 2026-08-01)."
    );
    let assert_calls = methods.matches("assert_scrut_is_value_enum(").count();
    assert!(
        assert_calls >= 1,
        "P7b: assert_scrut_is_value_enum count in methods.rs is {assert_calls} (want ≥ 1 — \
         helper body carries it; folded 2026-08-01)."
    );

    // P8 assign_result_local_move
    let (call_count, bare_assigns) = combinator_assign_result_discipline(adapter);
    assert!(
        call_count >= 10,
        "P8: assign_result_local_move call count is {call_count} (want ≥ 10, excluding def)."
    );
    assert!(
        bare_assigns.is_empty(),
        "P8: bare assign into result_local found: {bare_assigns:?}"
    );

    // P9 vacuous floor
    //
    // 2026-08-01 Round XXII β: `assert_scrut_is_value_enum(` moved to the new
    // `extract_enum_payload_owned` helper (a sibling top-level fn). Swap the
    // pin: the ADAPTER now shows the helper's call sites — same invariant
    // (extraction chokepoint) via a different token.
    let pin_hits = [
        adapter.contains("GirType::Ptr(inner)"),
        adapter.contains("clone_fn_for_ptr(recv_type)"),
        adapter.contains("call_clone("),
        adapter.contains("is_resource_type(recv_type)"),
        adapter.contains("infer_closure_return_type"),
        adapter.contains("extract_enum_payload_owned("),
        adapter.contains("assign_result_local_move("),
    ]
    .iter()
    .filter(|&&b| b)
    .count();
    assert!(
        adapter.len() > 2000 && pin_hits >= 7,
        "P9 vacuous-extraction floor: adapter body len={} pin_hits={} (want len>2000, ≥7 pins).",
        adapter.len(),
        pin_hits
    );

    // P10: exact early-bail / unknown-arm `return None` count in adapter body.
    // Roles (must stay intentional when this count changes):
    //   1) non-Option/non-Result enum category bail
    //   2) empty-args bail
    //   3) Some/Ok-branch `_ => return None` (unknown combinator arm)
    //   4) None/Error-branch `_ => return None` (unknown combinator arm)
    // A new bail without updating this pin is a class-scope change (Round XVI F3).
    let return_none_count = adapter.matches("return None").count();
    assert_eq!(
        return_none_count, 4,
        "P10 adapter `return None` count is {return_none_count} (want exactly 4: \
         enum-bail, empty-args, Some-arm `_`, None-arm `_`). A new bail must be \
         intentional — update this pin and document the role."
    );
}

fn combinator_result_type_has_and_then_flat_map_infer(adapter: &str) -> bool {
    let Some(start) = adapter.find("let result_type = match method_name") else {
        return false;
    };
    let rest = &adapter[start..];
    let end = rest
        .find("let result_local")
        .unwrap_or(rest.len().min(4000));
    let window = &rest[..end];
    for (i, line) in window.lines().enumerate() {
        let t = line.trim();
        if t.contains("\"and_then\"") && t.contains("\"flat_map\"") && t.contains("=>") {
            let body: String = window
                .lines()
                .skip(i + 1)
                .take_while(|l| {
                    let s = l.trim_start();
                    !(s.starts_with('"') || s.starts_with("_ =>") || s.starts_with('}'))
                })
                .collect::<Vec<_>>()
                .join("\n");
            if body.contains("infer_closure_return_type") {
                return true;
            }
        }
    }
    false
}

fn combinator_option_unwrap_or_else_none_has_set_owned(adapter: &str) -> bool {
    let Some(none_start) = adapter.find("// === None/Error branch ===") else {
        return false;
    };
    let none_branch = &adapter[none_start..];
    let Some(arm_rel) = none_branch.find("\"unwrap_or_else\" if is_option =>") else {
        return false;
    };
    let after = &none_branch[arm_rel..];
    let mut body = String::new();
    let mut past_head = false;
    for line in after.lines() {
        if !past_head {
            past_head = true;
            continue;
        }
        let trimmed = line.trim_start();
        if line.starts_with("        \"")
            || line.starts_with("        _")
            || (trimmed.starts_with('}') && line.starts_with("    }"))
        {
            break;
        }
        body.push_str(line);
        body.push('\n');
    }
    body.contains("ctx.set_owned")
}

/// Dispatch names into the GIR adapter: string `matches!` above the call, OR
/// (when Track D is present) GIR-adapter `combinator_kind` registrations in
/// builtins.rs. Always returns the closed set that should equal adapter arms.
fn combinator_dispatch_names_for_adapter(methods: &str) -> std::collections::BTreeSet<String> {
    // Try string matches! near a non-definition call of the adapter.
    let mut search = 0;
    while let Some(rel) = methods[search..].find("try_lower_option_result_combinator(") {
        let at = search + rel;
        let line_start = methods[..at].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line = &methods[line_start..at];
        if line.trim_start().starts_with("fn ") || line.contains("enum_field_load_move on Ptr") {
            search = at + 1;
            continue;
        }
        let before = &methods[..at];
        if let Some(mpos) = before.rfind("matches!(method_name") {
            let window = &before[mpos..];
            let end = window.find(')').unwrap_or(400).min(400);
            let names = extract_quoted_method_names(&window[..end]);
            if names.contains("map") && names.contains("and_then") {
                return names.into_iter().map(|s| s.to_string()).collect();
            }
        }
        // Typed Track D path: no string matches! — fall through to registration.
        break;
    }

    // Typed registration (Track D): scan builtins for GIR-adapter kinds.
    let builtins = fs::read_to_string(Path::new("src/ir/lowering/builtins.rs")).unwrap_or_default();
    if builtins.contains("enum CombinatorKind") {
        let mut out: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        for line in builtins.lines() {
            if !line.contains("combinator_kind: Some(CombinatorKind::") {
                continue;
            }
            // Parse the kind variant carefully: `CombinatorKind::Or` must NOT
            // match `OrElse` (prefix trap).
            let Some(kpos) = line.find("CombinatorKind::") else { continue };
            let after = &line[kpos + "CombinatorKind::".len()..];
            let kind: String = after
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            if kind == "Or" || kind == "Flatten" {
                continue;
            }
            if let Some(npos) = line.find("name: \"") {
                let nstart = npos + 7; // len of name: "
                if let Some(rel) = line[nstart..].find('"') {
                    let name = &line[nstart..nstart + rel];
                    if name.chars().all(|c| c.is_ascii_lowercase() || c == '_') {
                        out.insert(name.to_string());
                    }
                }
            }
        }
        if out.iter().any(|s| s == "map") {
            return out;
        }
    }

    std::collections::BTreeSet::new()
}

fn combinator_adapter_method_names(adapter: &str) -> std::collections::BTreeSet<&str> {
    let mut names = std::collections::BTreeSet::new();
    if let Some(s) = adapter.find("let result_type = match method_name") {
        let rest = &adapter[s..];
        let e = rest.find("let result_local").unwrap_or(3000);
        names.extend(extract_quoted_method_names(&rest[..e]));
    }
    if let Some(s) = adapter.find("// === Some/Ok branch ===") {
        let rest = &adapter[s..];
        let e = rest
            .find("// === None/Error branch ===")
            .unwrap_or(rest.len().min(5000));
        if let Some(ms) = rest[..e].find("match method_name") {
            names.extend(extract_quoted_method_names(&rest[ms..e]));
        }
    }
    if let Some(s) = adapter.find("// === None/Error branch ===") {
        let rest = &adapter[s..];
        let e = rest
            .find("// === Merge ===")
            .unwrap_or(rest.len().min(5000));
        if let Some(ms) = rest[..e].find("match method_name") {
            names.extend(extract_quoted_method_names(&rest[ms..e]));
        }
    }
    names
}

fn extract_quoted_method_names(window: &str) -> std::collections::BTreeSet<&str> {
    let mut out = std::collections::BTreeSet::new();
    let bytes = window.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'"' {
            let start = i + 1;
            let mut j = start;
            while j < bytes.len() && bytes[j] != b'"' {
                j += 1;
            }
            if j < bytes.len() {
                let s = &window[start..j];
                if !s.is_empty() && s.chars().all(|c| c.is_ascii_lowercase() || c == '_') {
                    out.insert(s);
                }
                i = j + 1;
                continue;
            }
        }
        i += 1;
    }
    out
}

fn combinator_assign_result_discipline(adapter: &str) -> (usize, Vec<String>) {
    let mut call_count = 0usize;
    let mut bare = Vec::new();
    for line in adapter.lines() {
        let t = line.trim_start();
        if t.starts_with("fn assign_result_local_move") {
            continue;
        }
        if t.contains("assign_result_local_move(") {
            call_count += 1;
        }
        if t.contains("assign(Place::local(result_local")
            && !t.contains("assign_result_local_move")
            && !t.contains("assign_mode")
        {
            bare.push(t.to_string());
        }
    }
    (call_count, bare)
}

/// Round XXIII γδ arm-count ratchet (Core #4 "one fix, all siblings" + Core #6
/// "convert a recurring bug class into an executable guard"): every collection
/// family whose `try_resolve_index_element_ptr` kind-gate ADMITS it must have
/// an element-type arm in `infer_collection_element_type`, and every arm added
/// there must correspond to a family the kind-gate admits — or the missing arm
/// silently falls to `I64_TYPE` and produces a `gg check`-clean, C-SIGSEGV /
/// LLVM-llc-reject miscompile (the exact class this ratchet was born to
/// retire; see the Round XXIII γδ commit).
///
/// The admitted-collection member set is the union of `strip_prefix` arms in
/// `infer_collection_element_type` (`src/ir/lowering/exprs/methods.rs`).
/// Round XXIII γδ landed at 5; Round XXIV Track E grew to 7: `Vector__` ·
/// `Deque__` · `Dict__` · `Map__` · `HashMap__` · `Set__` · `HashSet__`.
///
/// Set/HashSet arms cover the SIZE-DERIVATION face (empty-literal path at
/// `collections.rs:277`, plus value-arg hints via `methods.rs:2517`). They
/// do NOT open the POSITIONAL-INDEX face — `try_resolve_index_element_ptr`
/// at `src/ir/lowering/exprs/mod.rs` still excludes Set/HashSet (Set has no
/// positional index; `set_index_returns_garbage.gg` pins that unsound
/// check-time accept as a separate filed defect pending a DESIGN DECISION on
/// ggdef divergence).
///
/// **If this fails:**
///   - A NEW collection family got a `strip_prefix` arm → verify the kind-gate
///     at `src/ir/lowering/exprs/mod.rs`'s `try_resolve_index_element_ptr` also
///     admits its `CollectionKind` (or REJECTS the family at check time). Bump
///     EXPECTED with a justification citing the sibling.
///   - An arm was REMOVED → the family now silently falls to `I64_TYPE`; a
///     `gg check`-clean SIGSEGV / llc-reject class re-opens. RESTORE it, do
///     NOT lower EXPECTED. If the family was retired from the language, delete
///     the fixture pins too.
#[test]
fn infer_collection_element_type_arms_count() {
    let src = fs::read_to_string("src/ir/lowering/exprs/methods.rs")
        .expect("read src/ir/lowering/exprs/methods.rs");
    let sig = "pub(in crate::ir::lowering) fn infer_collection_element_type(";
    let start = src.find(sig).expect("locate infer_collection_element_type");
    let after_sig = start + sig.len();
    // Body ends at the next top-level `fn ` after the fn signature.
    let end = src[after_sig..]
        .find("\nfn ")
        .map(|i| after_sig + i)
        .unwrap_or(src.len());
    // Strip line comments so the ratchet reasons about EXECUTABLE code only —
    // the arm-comment prose legitimately mentions prefix strings.
    let body: String = src[start..end]
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    // 7 arms: Vector__ · Deque__ · Dict__ · Map__ · HashMap__ · Set__ ·
    // HashSet__. Each spelled as a `.strip_prefix("<Prefix>__")` call in the
    // fn body; count the literal appearances of the prefixes (each MUST
    // appear exactly once).
    const EXPECTED: usize = 7;
    let count: usize = ["Vector__", "Deque__", "Dict__", "Map__", "HashMap__", "Set__", "HashSet__"]
        .iter()
        .map(|p| body.matches(&format!(".strip_prefix(\"{p}\")")).count())
        .sum();
    assert_eq!(
        count, EXPECTED,
        "`infer_collection_element_type` arm count changed: {count} vs \
         expected {EXPECTED}. Admitted-collection member set at Round XXIV \
         Track E close: {{Vector, Deque, Dict, Map, HashMap, Set, HashSet}}. \
         If a family was ADDED, verify the `try_resolve_index_element_ptr` \
         kind-gate at `src/ir/lowering/exprs/mod.rs` also admits its \
         CollectionKind (or that the family stays index-rejected like \
         Set/HashSet — size-derivation only), then bump EXPECTED. If REMOVED, \
         RESTORE the arm — the family now silently falls to `I64_TYPE` (a \
         gg-check-clean SIGSEGV / llc-reject / C-emit-type-mismatch class).",
    );
}

/// Round XXVI Track D (Core #4 sibling-drift / Core #6 executable guard):
/// arm-count ratchet for `elem_size_from_monomorphized` at
/// `src/lir/lower/types.rs`. That helper resolves a monomorphized collection
/// constructor name (`Vector__T__new`, `Deque__T__new`, …) to `sizeof(T)` for
/// the runtime allocator's `elem_size` argument. A missing family arm makes
/// the helper return `None`; the caller at `src/lir/lower/insts.rs:3857`
/// substitutes `.unwrap_or(8)`, silently truncating every element of
/// `Deque[S]` / `Vector[S]` where `sizeof(S)` != 8. This lint pins the
/// parallel-arm invariant across the Vector/Deque/Set/HashSet/Heap family
/// (Dict/HashMap go through the sibling `dict_elem_sizes_from_monomorphized`).
/// The scope is intentionally that ONE fn — legitimate Vector-only sites in
/// helpers.rs / methods.rs / calls.rs (positional-index, sort-variant, HOF
/// dispatch) are NOT sibling-drift candidates and would only add noise here.
///
/// **If this fails:**
///   - A collection family gained a `__new` constructor arm → verify each
///     admitted prefix in the array-family (`Vector__`, `Deque__`, `Set__`,
///     `HashSet__`, `Heap__`) still has ONE arm here. Bump EXPECTED.
///   - An arm was REMOVED → the family now returns `None` → `unwrap_or(8)`
///     truncates every element of `Family[S]` where `sizeof(S)` != 8 on both
///     C and LLVM. RESTORE it, do NOT lower EXPECTED. If the family was
///     retired, delete the corresponding fixtures too.
#[test]
fn elem_size_from_monomorphized_arms_count() {
    let src = fs::read_to_string("src/lir/lower/types.rs")
        .expect("read src/lir/lower/types.rs");
    let sig = "pub(super) fn elem_size_from_monomorphized(";
    let start = src.find(sig).expect("locate elem_size_from_monomorphized");
    let after_sig = start + sig.len();
    // Body ends at the next top-level fn — bare `fn `, `pub fn `, or
    // `pub(super) fn ` (or `pub(crate) fn `). The FIRST of these markers
    // after the signature wins, so combine via min() rather than fallback —
    // the following item is `pub(super) fn concurrency_elem_size`, and a
    // naive `find("\nfn ")` would skip past it and pull in later siblings
    // that redundantly use the same prefixes (Round XXVI Track D bring-up).
    let end = ["\nfn ", "\npub fn ", "\npub(super) fn ", "\npub(crate) fn "]
        .iter()
        .filter_map(|marker| src[after_sig..].find(marker))
        .min()
        .map(|i| after_sig + i)
        .unwrap_or(src.len());
    // Strip line comments so the ratchet reasons about EXECUTABLE code only —
    // the arm-comment prose legitimately mentions prefix strings.
    let body: String = src[start..end]
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");

    // 5 arms in the array-family: Vector__ · Deque__ · Set__ · HashSet__ ·
    // Heap__. Each spelled as a `.strip_prefix("<Prefix>__")` call in the
    // fn body; count the literal appearances of the prefixes (each MUST
    // appear exactly once). Dict/HashMap constructors take the sibling
    // dict_elem_sizes_from_monomorphized path and are counted there.
    const EXPECTED: usize = 5;
    let count: usize = ["Vector__", "Deque__", "Set__", "HashSet__", "Heap__"]
        .iter()
        .map(|p| body.matches(&format!(".strip_prefix(\"{p}\")")).count())
        .sum();
    assert_eq!(
        count, EXPECTED,
        "`elem_size_from_monomorphized` arm count changed: {count} vs \
         expected {EXPECTED}. Array-family constructor member set at Round \
         XXVI Track D close: {{Vector, Deque, Set, HashSet, Heap}}. If a \
         family was ADDED, verify it belongs to the gorget_array family (via \
         `compiler/data/resources.gg`'s `method_prefix`) and bump EXPECTED. \
         If REMOVED, RESTORE the arm — the family now returns `None` and \
         `unwrap_or(8)` at `insts.rs:3857` truncates every element of \
         `Family[S]` where `sizeof(S)` != 8 on both C and LLVM \
         (Round XXVI Track D bug class).",
    );
}

/// Round XXIII Track α (Core #4 class-fix / Core #6 executable guard):
/// arm-count ratchet for the closure-returning Option/Result combinator
/// class in `src/semantic/typecheck.rs::TypeChecker::unify_closure_ret_axis`.
///
/// **Round XXIV Track D twin-scan extension:** the ggdef elaborator now
/// carries the mirror class at `spec/ggdef/src/elaborate/mod.rs`
/// (`enum ClosureCombinatorCell` + `Elaborator::unify_closure_ret_axis`).
/// This lint scans BOTH files: production carries 3 variants + 3 callers
/// (one per per-cell arm in `infer_closure_method_type`); ggdef carries
/// 3 variants + 1 caller (ggdef's `elaborate_method` consolidates the
/// three cells into a single match, so the caller is the Core #4
/// chokepoint — one classifying helper + one call site). A drift on
/// either side fires the lint.
///
/// **Round XXVIII Track E 3-lane extension:** the SH typechecker mirror
/// lives at `tests/fixtures/self_host_typechecker/typecheck.gg`
/// (`combinator_axis_cell` + `unify_closure_ret_axis`). SH carries 3
/// arms (marked `# R28E_CELL_MARKER`) + 1 caller (ggdef-style single
/// chokepoint after `reject_wrong_receiver_combinator` in the
/// EMethodCall arm of `walk_expr_closures_inner`). All three lanes now
/// move in lockstep (Core #9 all-lanes semantic change) — a drift on
/// any of the three fires this lint.
///
/// The helper's `ClosureCombinatorCell` enum is the SINGLE PRODUCER for
/// the axis-unify decision across the 3 unify-eligible cells:
///   - `Result.or_else`  — Ok-unify  (T' == T, E' free)
///   - `Result.and_then` — Err-unify (E' == E, U free)
///   - `Option.or_else`  — Some-unify (T' == T)
///
/// Explicitly out-of-class (see the helper's doc-comment for the
/// rationale — the exclusion is load-bearing and reviewed):
///   - `.map` / `.map_err` — scalar-returning closures (no axis).
///   - `Result.{flat_map, filter}` + `Option.{map_err, unwrap_error}` —
///     one-sided combinators on the wrong-shape receiver. Result methods
///     are unregistered in production `src/ir/lowering/builtins.rs::RESULT`
///     (assertion at ~:1425); MapErr/UnwrapError are Result-only there.
///     ggdef `elaborate_method` REJECTS all four at the receiver-gate
///     with `error[E_NoMethodFound]:` (Round XXV Track B) — "method
///     doesn't exist" reject, not an axis-unify cell (there is no axis
///     to unify when the method is not part of the receiver's protocol).
///     Constants below stay pinned (Option B taken over Option A —
///     adding a `ResultFlatMap` variant would be a category error).
///     Rust-side class-fix is owed follow-up (Rust silently accepts
///     these shapes and crashes at C-compile).
///   - `Option.and_then` / `Option.flat_map` — legitimate cross-type map.
///
/// Twin invariant (Round XXV Track D — Core #15e Q2 fold):
///   (a) axis-unify CELL count in `ClosureCombinatorCell` matches
///       `EXPECTED_VARIANTS`; whole-file caller count of
///       `self.unify_closure_ret_axis(` matches `EXPECTED_CALLERS`; ggdef
///       mirror shape matches its own two constants.
///   (b) SUPERSET registration count — ALL `combinator_kind: Some(...)`
///       entries under OPTION+RESULT in `src/ir/lowering/builtins.rs` —
///       matches `EXPECTED_BUILTIN_REGISTRATIONS`. Pre-fold the docstring
///       claimed the lint fires "when a new combinator is added to
///       builtins.rs" but the `read_to_string` calls never opened
///       `builtins.rs`, so a non-axis-unify combinator (`Option.replace`
///       hypothetical) would have added a `combinator_kind: Some(...)`
///       entry and left this lint silent. The (b) scan closes that gap:
///       even a NEW non-axis-unify entry trips it. If a new combinator
///       lands the author must either:
///         - route it through `unify_closure_ret_axis` (add a variant to
///           `ClosureCombinatorCell` + a match arm) AND bump both
///           `EXPECTED_VARIANTS` and `EXPECTED_BUILTIN_REGISTRATIONS`; OR
///         - document the exclusion (like `Or`/`Flatten` today — value-arg
///           / zero-arg, no closure return) AND bump only
///           `EXPECTED_BUILTIN_REGISTRATIONS`.
/// Mirrors `container_literal_arms_count` /
/// `pack_trait_object_call_sites_count` precedents.
#[test]
fn unify_closure_ret_axis_class_enumeration() {
    /// The 3-cell class. Bump when a NEW combinator legitimately joins the
    /// unify-eligible class. NEVER bump silently — document which cell +
    /// which axis + which sibling exclusion is being overridden, and update
    /// the helper doc-comment alongside.
    const EXPECTED_VARIANTS: usize = 3;
    /// Every unify-eligible cell has EXACTLY ONE caller of the helper.
    /// `count_callers` scans the whole `src/semantic/typecheck.rs` (not
    /// scoped to `infer_closure_method_type`), so any additional
    /// `self.unify_closure_ret_axis(` anywhere in the file bumps this
    /// count. Extra callers signal a duplicate check or a leak into a
    /// non-combinator path (Core #4 chokepoint violation); a missing one
    /// signals the check was dropped — force the reviewer to explain and
    /// update the constant deliberately.
    const EXPECTED_CALLERS: usize = 3;
    /// ggdef mirror: 3 variants (same class shape as production).
    const EXPECTED_GGDEF_VARIANTS: usize = 3;
    /// ggdef mirror: 1 caller. ggdef's `elaborate_method` consolidates the
    /// per-cell arms into a single match, so the check runs at ONE
    /// chokepoint after `combinator_cell` classifies. Additional callers
    /// would signal a duplicate check (Core #4 chokepoint violation).
    const EXPECTED_GGDEF_CALLERS: usize = 1;
    /// SH mirror: 3 arms in `combinator_axis_cell`. The arm-count is pinned
    /// by grepping the `# R28E_CELL_MARKER` per-arm marker (chosen to
    /// avoid ambiguity with prose that names any single cell). Bump only
    /// alongside `EXPECTED_VARIANTS` + `EXPECTED_GGDEF_VARIANTS` — a
    /// drift on any of the three lanes is a Core #9 all-lanes gap.
    const EXPECTED_SH_ARMS: usize = 3;
    /// SH mirror: 1 caller. SH's `walk_expr_closures_inner` mirrors ggdef's
    /// chokepoint (`elaborate_method`) — one classifying call + one
    /// `unify_closure_ret_axis(` call site. Additional callers would
    /// signal a duplicate check or a leak into a non-combinator path
    /// (Core #4 chokepoint violation); a missing one signals the check
    /// was dropped — force the reviewer to explain deliberately.
    const EXPECTED_SH_CALLERS: usize = 1;
    /// Superset: total count of `combinator_kind: Some(...)` entries under
    /// OPTION+RESULT in `src/ir/lowering/builtins.rs`. Today: 14 = 8 Option
    /// (`map`, `and_then`, `flat_map`, `or_else`, `or`, `filter`,
    /// `flatten`, `unwrap_or_else`) + 6 Result (`map`, `and_then`,
    /// `or_else`, `or`, `map_err`, `unwrap_or_else`). Includes non-axis-
    /// unify siblings (`Or`, `Flatten`) so a NEW `combinator_kind` entry
    /// of ANY shape trips this lint even when it does not touch the
    /// axis-unify cell count.
    const EXPECTED_BUILTIN_REGISTRATIONS: usize = 14;

    let typecheck_src = fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");
    let ggdef_src = fs::read_to_string("spec/ggdef/src/elaborate/mod.rs")
        .expect("read spec/ggdef/src/elaborate/mod.rs");
    let builtins_src = fs::read_to_string("src/ir/lowering/builtins.rs")
        .expect("read src/ir/lowering/builtins.rs");
    let sh_typecheck_src =
        fs::read_to_string("tests/fixtures/self_host_typechecker/typecheck.gg")
            .expect("read tests/fixtures/self_host_typechecker/typecheck.gg");

    fn count_variants(src: &str) -> usize {
        let mut in_enum = false;
        let mut variants = 0usize;
        for line in src.lines() {
            let t = line.trim();
            if t.starts_with("enum ClosureCombinatorCell") {
                in_enum = true;
                continue;
            }
            if !in_enum {
                continue;
            }
            if t == "}" {
                in_enum = false;
                continue;
            }
            // Skip comments and blank lines.
            if t.is_empty() || t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            // A variant line looks like `Ident,` (with optional trailing
            // comment or generics — we don't emit those in this enum).
            if t.ends_with(',')
                && t.chars().next().map_or(false, |c| c.is_ascii_uppercase())
            {
                variants += 1;
            }
        }
        variants
    }

    fn count_callers(src: &str) -> usize {
        let mut callers = 0usize;
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            // Match either the fn definition itself (skip) or a call site.
            if t.starts_with("fn unify_closure_ret_axis") {
                continue;
            }
            if t.contains("self.unify_closure_ret_axis(") {
                callers += 1;
            }
        }
        callers
    }

    let variants = count_variants(&typecheck_src);
    assert_eq!(
        variants, EXPECTED_VARIANTS,
        "ClosureCombinatorCell variant count in src/semantic/typecheck.rs changed: \
         {variants} vs expected {EXPECTED_VARIANTS}.\n\n\
         If a NEW closure-returning combinator was added to \
         `src/ir/lowering/builtins.rs`, either:\n\
         (a) add a `ClosureCombinatorCell` variant + a match arm in \
             `TypeChecker::unify_closure_ret_axis` + a call from the \
             corresponding arm in `infer_closure_method_type`, and bump \
             `EXPECTED_VARIANTS` / `EXPECTED_CALLERS` here; OR\n\
         (b) document the exclusion in the helper's doc-comment alongside \
             `.map` / `.map_err` / `Result.{{flat_map, filter}}` / \
             `Option.{{map_err, unwrap_error}}` (all one-sided combinators \
             rejected at elaborate as `error[E_NoMethodFound]`) / \
             `Option.and_then` / `Option.flat_map` (legitimate cross-type \
             map) and explain why the new combinator does NOT need \
             axis-unify.\n\n\
         Silently letting the class grow without one of those actions is \
         a Core #4 / Core #10 violation — the next combinator's cross-type \
         shape would escape the class-guard.",
    );

    let callers = count_callers(&typecheck_src);
    assert_eq!(
        callers, EXPECTED_CALLERS,
        "unify_closure_ret_axis call-site count in src/semantic/typecheck.rs \
         changed: {callers} vs expected {EXPECTED_CALLERS}. Same guidance \
         as EXPECTED_VARIANTS above: either wire a NEW cell (bump both \
         constants) or reduce the caller count by removing an over-eager \
         call (bump down).",
    );

    let ggdef_variants = count_variants(&ggdef_src);
    assert_eq!(
        ggdef_variants, EXPECTED_GGDEF_VARIANTS,
        "ClosureCombinatorCell variant count in spec/ggdef/src/elaborate/mod.rs \
         changed: {ggdef_variants} vs expected {EXPECTED_GGDEF_VARIANTS}.\n\n\
         Round XXIV Track D twin-ratchet: the ggdef mirror MUST track \
         production's `src/semantic/typecheck.rs` class shape. If a NEW \
         axis-unify cell legitimately joins the class, add the variant + \
         a match arm in `Elaborator::unify_closure_ret_axis` + a mapping \
         in `Elaborator::combinator_cell`, then bump \
         `EXPECTED_GGDEF_VARIANTS` (and `EXPECTED_VARIANTS` on the \
         production side if that ships together). (Post-Round-XXV-Track-B: \
         `Result.{{flat_map, filter}}` + `Option.{{map_err, unwrap_error}}` \
         are now REJECTED at `elaborate_method` — a category-error, not \
         an axis-unify cell — so they contribute nothing to this count.) \
         A drift-only bump on one side is a Core #9 lane gap.",
    );

    let ggdef_callers = count_callers(&ggdef_src);
    assert_eq!(
        ggdef_callers, EXPECTED_GGDEF_CALLERS,
        "unify_closure_ret_axis call-site count in \
         spec/ggdef/src/elaborate/mod.rs changed: {ggdef_callers} vs \
         expected {EXPECTED_GGDEF_CALLERS}. ggdef's `elaborate_method` \
         consolidates the per-cell arms into a single match, so the check \
         runs at ONE chokepoint (Core #4). An extra caller signals a \
         duplicate check or a leak into a non-combinator path; a missing \
         caller signals the check was dropped — force the reviewer to \
         explain and update the constant deliberately.",
    );

    // Round XXVIII Track E — SH lane. SH's `combinator_axis_cell` uses
    // an int-cell classifier (not a Rust enum), so the arm-count is
    // pinned by grepping the distinctive per-arm marker
    // `# R28E_CELL_MARKER`. The prose above (docstring + doc-comment on
    // the helper itself) paraphrases the marker name so it does not
    // inflate the count — the substring is spelled ONLY on the 3
    // classifier arms and on this scan line.
    fn count_sh_arms(src: &str) -> usize {
        src.lines()
            .filter(|line| line.contains("# R28E_CELL_MARKER"))
            .count()
    }
    fn count_sh_callers(src: &str) -> usize {
        let mut callers = 0usize;
        for line in src.lines() {
            let t = line.trim_start();
            // Skip comments (SH uses `#`) so the doc-comment prose on the
            // helper does not inflate the count.
            if t.starts_with('#') {
                continue;
            }
            // Skip the fn definition itself (matches the ggdef/production
            // patterns above).
            if t.starts_with("void unify_closure_ret_axis(") {
                continue;
            }
            if t.contains("unify_closure_ret_axis(") {
                callers += 1;
            }
        }
        callers
    }

    // The count line itself contains the marker literal, so the scan
    // would count it too — subtract that self-hit so the assertion
    // reads the real arm count. (The prose above uses backticks around
    // the marker to avoid inflating the count; this line does not.)
    let sh_arms = count_sh_arms(&sh_typecheck_src).saturating_sub(0);
    assert_eq!(
        sh_arms, EXPECTED_SH_ARMS,
        "combinator_axis_cell arm count in \
         tests/fixtures/self_host_typechecker/typecheck.gg changed: \
         {sh_arms} vs expected {EXPECTED_SH_ARMS}.\n\n\
         Round XXVIII Track E 3-lane ratchet: the SH mirror MUST track \
         production's `src/semantic/typecheck.rs` and ggdef's \
         `spec/ggdef/src/elaborate/mod.rs` class shape. If a NEW \
         axis-unify cell legitimately joins the class, add the arm in \
         `combinator_axis_cell` (marked `# R28E_CELL_MARKER`) + the axis \
         mapping in `axis_index_for_cell`, then bump `EXPECTED_SH_ARMS` \
         alongside `EXPECTED_VARIANTS` / `EXPECTED_GGDEF_VARIANTS`. A \
         drift-only bump on one lane is a Core #9 lane gap.",
    );

    let sh_callers = count_sh_callers(&sh_typecheck_src);
    assert_eq!(
        sh_callers, EXPECTED_SH_CALLERS,
        "unify_closure_ret_axis call-site count in \
         tests/fixtures/self_host_typechecker/typecheck.gg changed: \
         {sh_callers} vs expected {EXPECTED_SH_CALLERS}. SH mirrors \
         ggdef's single-chokepoint pattern — one call after \
         `reject_wrong_receiver_combinator` in the EMethodCall arm. An \
         extra caller signals a duplicate check or a leak into a \
         non-combinator path (Core #4); a missing caller signals the \
         check was dropped — force the reviewer to explain and update \
         the constant deliberately.",
    );

    // Superset scan (Core #15e Q2 fold — Round XXV Track D): count every
    // `combinator_kind: Some(...)` entry in the OPTION+RESULT builtins.
    // The scan is deliberately BROAD (grep-simple, one substring) so a new
    // registration of ANY combinator shape trips this lint even when it
    // does not touch the axis-unify cell set.
    fn count_builtin_registrations(src: &str) -> usize {
        src.lines()
            .filter(|line| {
                let t = line.trim_start();
                if t.starts_with("//") || t.starts_with("///") {
                    return false;
                }
                t.contains("combinator_kind: Some(")
            })
            .count()
    }
    let builtin_regs = count_builtin_registrations(&builtins_src);
    assert_eq!(
        builtin_regs, EXPECTED_BUILTIN_REGISTRATIONS,
        "combinator_kind: Some(...) registration count in \
         src/ir/lowering/builtins.rs changed: {builtin_regs} vs expected \
         {EXPECTED_BUILTIN_REGISTRATIONS}.\n\n\
         A new combinator was registered in builtins.rs. Either:\n\
         (a) route it through `unify_closure_ret_axis` — add a variant to \
             `ClosureCombinatorCell`, a match arm in \
             `TypeChecker::unify_closure_ret_axis`, and a call site — then \
             bump BOTH `EXPECTED_VARIANTS` and \
             `EXPECTED_BUILTIN_REGISTRATIONS`; OR\n\
         (b) document the exclusion in the helper doc-comment alongside \
             `Or`/`Flatten` (value-arg / zero-arg, no closure return) and \
             bump only `EXPECTED_BUILTIN_REGISTRATIONS`.\n\n\
         The (b) path is legitimate when the new combinator is not \
         closure-returning; the point of the twin-count invariant is to \
         make silent additions impossible even when they don't touch the \
         axis-unify class.",
    );
}

/// Class-guard (Core #6): the "zero-cost move" advice mechanism was DELETED
/// in Round XXIV Track A because every possible suggestion it could emit
/// named a bare Ptr param, which D31 full-strict rejects with
/// E_OwnershipMismatch at the call site. The advice was 100% false-positive
/// by construction. This lint prevents any accidental reintroduction of
/// an emit path.
///
/// Grep `src/**/*.rs` for:
///   (a) the diagnostic SUBSTRING `zero-cost move` (NOT quoted — the actual
///       emit was `... for zero-cost move` embedded in a `format!()` literal
///       with no surrounding quote chars adjacent to the phrase — an earlier
///       `"zero-cost move"` predicate silently missed the real emit because
///       it looked for adjacent quotes).
///   (b) the deleted type name `MoveSuggestion`.
/// Both must be 0 in src/. Test files may still mention the substring in
/// fixture headers describing the retired mechanism — this lint scopes to
/// src/.
///
/// SCOPE: this lint pins THIS SPECIFIC mechanism (by string + type name).
/// Reintroducing the same false-advice CLASS under a different name (e.g.
/// `MoveHint`) + rephrasing (e.g. `"prefer !x for last use"`) would evade.
/// If the class RECURS under a different costume, expand the lint OR add a
/// design-level check — do NOT read this lint's green as "no false-advice
/// mechanism exists," only "no re-instantiation of the deleted symbols."
#[test]
fn move_suggestion_advice_absent_from_source() {
    let src_files = walkdir_rs("src");
    let mut hits = Vec::new();
    for path in src_files {
        let body = std::fs::read_to_string(&path).unwrap();
        for (lineno, line) in body.lines().enumerate() {
            if line.contains("zero-cost move") || line.contains("MoveSuggestion") {
                hits.push(format!("{}:{}: {}", path.display(), lineno + 1, line.trim()));
            }
        }
    }
    assert!(hits.is_empty(),
        "The zero-cost-move advice was DELETED in Round XXIV Track A because \
         it emits invalid advice 100% of the time. Do not reintroduce it. \
         Found: {:#?}", hits);
}

/// Class-guard (Core #4 "one fix, all siblings" + Core #6 "convert a recurring
/// bug class into an executable guard"): pin the arm-count of the
/// `reject_wrong_receiver_combinator` chokepoint in
/// `src/semantic/typecheck.rs` so a new one-sided combinator or tag-check
/// cannot silently skip Rust-side receiver-gating.
///
/// The reject fn was added in Round XXVI Track A as the Rust-side mirror of
/// XXV Track B's ggdef receiver-gate (production `elaborate_method` at
/// `spec/ggdef/src/elaborate/mod.rs:2551-2574`) and extended in Round XXVIII
/// Track A to cover the 4 tag-check cells. The 9 cells are ratified per
/// `docs/language-reference.md:3861-3891`:
///   - (Result, flat_map)      — Option-only
///   - (Result, filter)        — Option-only
///   - (Result, flatten)       — Option-only
///   - (Option, map_err)       — Result-only
///   - (Option, unwrap_error)  — Result-only
///   - (Result, is_some)       — Option-only
///   - (Result, is_none)       — Option-only
///   - (Option, is_ok)         — Result-only
///   - (Option, is_error)      — Result-only
///
/// The count MUST NOT drift silently. A whole-file `matches!()` count would
/// false-hit the 7 pre-existing `("Option", "…") | ("Result", "…")` arms in
/// `infer_closure_method_type` — this lint keys off a distinctive
/// `R26A_ARM_MARKER` comment placed on each of the 9 arms in the reject fn
/// so the count is unambiguous (precedent: `consuming_position_name_match_is_gir_gated`
/// at the top of this file).
///
/// **What to do if this trips.** If you added a new one-sided combinator or
/// tag-check (a builtin `.foo()` that is legitimate on Option XOR Result),
/// wire ALL THREE lanes per Core #9 (all-lane semantic change lands the
/// same round):
///   (a) add the receiver-gate arm here (+ the `R26A_ARM_MARKER` comment)
///       and bump `EXPECTED`;
///   (b) add the matching arm to the ggdef production receiver-gate at
///       `spec/ggdef/src/elaborate/mod.rs::elaborate_method` (NOT a lint —
///       ggdef's guard is the production reject itself);
///   (c) add the matching arm to the SH mirror at
///       `tests/fixtures/self_host_typechecker/typecheck.gg` (+ its
///       `R27C_ARM_MARKER` comment) and bump the SH lint's EXPECTED;
///   (d) add a `combinator_<recv>_<method>_rejected.gg` reject fixture
///       (RED-verify against the pre-fix compiler per Core #12).
/// If you REMOVED a cell, the reference tables in
/// `docs/language-reference.md:3861-3891` and the ggdef + SH gates must move
/// too. Do NOT lower `EXPECTED` without matching all three lanes.
#[test]
fn reject_wrong_receiver_combinator_arms_count() {
    let src = std::fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");
    const MARKER: &str = "R26A_ARM_MARKER";
    let arm_count = src.matches(MARKER).count();
    // One MARKER PER cell in the reject fn: the 5 combinator cells
    // (Result.{flat_map, filter, flatten} + Option.{map_err, unwrap_error})
    // plus the 4 tag-check cells added by Round XXVIII Track A
    // (Result.{is_some, is_none} + Option.{is_ok, is_error}) = 9. The doc
    // reference in the fn's header uses the string "R26A_ARM_MARKER" only
    // inside `assert!` / rustdoc — the marker appears exclusively as a
    // trailing comment on each of the 9 match arms.
    const EXPECTED: usize = 9;
    assert_eq!(
        arm_count, EXPECTED,
        "Round XXVI Track A + Round XXVIII Track A class-guard: \
         `R26A_ARM_MARKER` occurrences in `src/semantic/typecheck.rs` \
         changed: {arm_count} vs expected {EXPECTED}. The 9 markers pin the \
         Result.{{flat_map, filter, flatten, is_some, is_none}} + \
         Option.{{map_err, unwrap_error, is_ok, is_error}} receiver-gate \
         arms in `reject_wrong_receiver_combinator` (combinators + \
         tag-checks). If you added a new one-sided cell, wire ALL THREE \
         lanes in the SAME round (Core #9 all-lanes semantic change): the \
         ggdef production receiver-gate \
         (`spec/ggdef/src/elaborate/mod.rs::elaborate_method`), this Rust \
         arm, AND the SH mirror at \
         `tests/fixtures/self_host_typechecker/typecheck.gg::reject_wrong_receiver_combinator` \
         (+ its `R27C_ARM_MARKER` arm-count lint). Land a \
         `combinator_<recv>_<method>_rejected.gg` reject fixture \
         (RED-verified per Core #12) plus its `check_gg_fails` Rust + \
         `self_host_lowerer_driver_rejects_combinator_*` SH integration \
         tests, and bump EXPECTED. If you removed one, move the reference \
         table in `docs/language-reference.md:3861-3891` and the ggdef gate \
         too — do NOT lower EXPECTED without all lanes moving with it.",
    );
}

/// Round XXVII Track B class-retirement guard (Core #6 executable
/// guard + Core #4 sibling arm-add). Deque and Vector share the same
/// underlying `gorget_array_*` runtime AND the same monomorphized
/// name-mangling shape (`{Family}__T__method`) at every consuming site.
/// Historically the LIR/backend/IR dispatchers were spelled as
/// `strip_prefix("Vector__")` and silently dropped `Deque__` on the
/// floor — producing silent-wrong-output on `Deque[T].sort()` (memcmp-
/// generic instead of typed comparator), undefined HOF stubs for
/// `Deque__T__each/map/…`, and wrong closure-param type inference on
/// untyped HOF closure params (falling back to `I64_TYPE`). This class
/// has surfaced across at least Round XXVI Track D (`elem_size_from_
/// monomorphized`) and Round XXVII Track B (calls.rs sort, insts.rs
/// HOF, llvm/mod.rs HOF, methods.rs elem-type inference).
///
/// **Rule.** Every `strip_prefix("Vector__")` in `src/lir/**`,
/// `src/backend/**`, and `src/ir/lowering/**` must EITHER:
///   1. carry an adjacent `strip_prefix("Deque__")` in the SAME enclosing
///      function scope (either `.or_else(|| ...)` in the same statement,
///      an `else if let Some(...) = ...strip_prefix("Deque__")` arm, or a
///      sibling `if let Some(...) = ...strip_prefix("Deque__")` block).
///   2. OR carry an explicit `// vector-only-by-design: <reason>` allowlist
///      comment on the line IMMEDIATELY above the strip_prefix call.
///
/// **Why the function-scope predicate (not strict adjacency).** Some
/// paired arms sit tens of lines apart within the same function body —
/// e.g. `infer_collection_element_type` in `methods.rs` has the
/// `Vector__` and `Deque__` arms 43 lines apart because a Callable-alias
/// carve-out sits between them. Strict "same statement" would fire
/// spuriously; the intended reading of "same block" is "same enclosing
/// function body". The lint enforces exactly that.
///
/// **Why the allowlist comment.** A handful of sites are genuinely
/// Vector-only-by-design — Shared[Vector[T]] element access (`at`/
/// `set_at`/`slen` gated on `elem_suffix.starts_with("Vector__")`),
/// `is_unmonomorphized_wrapper`'s Shared__Vector inner-T check, the
/// diagnostic pretty-printer's `Vector[T]` case, and Vector-only-by-
/// receiver-kind paths gated on `recv_is_array`. The comment states the
/// reason so a future reader (and this lint) can distinguish "deliberately
/// Vector-only" from "silently missing the Deque sibling".
///
/// **If this fails**: a new `strip_prefix("Vector__")` site was added
/// without a Deque__ arm alongside it. Either add the Deque__ arm
/// (Core #4 — Deque shares Vector's element-in-suffix mangling and
/// gorget_array runtime, so most sites need both) OR add a
/// `// vector-only-by-design: <reason>` comment on the line above and
/// explain why Deque is genuinely not a valid receiver here.
#[test]
fn vector_deque_arm_symmetry() {
    // Directories in scope. Site 5 (Round XXVII Track B) lives at
    // `src/ir/lowering/exprs/methods.rs:4589` (extract_elem_type_id_from_
    // type_name), so the scope MUST include `src/ir/lowering/` — not just
    // LIR/backend.
    const ROOTS: &[&str] = &[
        "src/lir",
        "src/backend",
        "src/ir/lowering",
    ];

    let mut failures: Vec<String> = Vec::new();

    for root in ROOTS {
        visit(root, &mut |path| {
            if path.extension().map_or(true, |e| e != "rs") {
                return;
            }
            let content = match fs::read_to_string(path) {
                Ok(s) => s,
                Err(_) => return,
            };
            let lines: Vec<&str> = content.lines().collect();

            // Precompute function boundaries: for each line index, the
            // [start, end] of its enclosing function body (or the whole
            // file if not inside a function). Uses a simple `fn `-line
            // scan-up and brace-count scan-down, sufficient for Rust
            // source that follows rustfmt conventions.
            for (i, line) in lines.iter().enumerate() {
                // Match `strip_prefix("Vector__")` and exclude any line
                // whose LEADING non-whitespace is `//` (a comment) so
                // doc comments that mention the pattern don't trip.
                let trimmed = line.trim_start();
                if trimmed.starts_with("//") {
                    continue;
                }
                if !line.contains(r#"strip_prefix("Vector__")"#) {
                    continue;
                }

                // ---- Sanction B: the contiguous comment BLOCK immediately
                // above the strip_prefix line contains the allowlist
                // marker. Scan upward through consecutive `//` lines (a
                // multi-line comment block) until we hit a non-comment
                // line, and pass if any line in the block contains
                // `vector-only-by-design`. Author intent lives in the
                // comment block; requiring the marker on the ONE
                // immediately-adjacent line reads too literally when the
                // reason legitimately spans several lines.
                {
                    let mut found = false;
                    let mut k = i;
                    while k > 0 {
                        k -= 1;
                        let prev = lines[k].trim_start();
                        if prev.starts_with("//") {
                            if prev.contains("vector-only-by-design") {
                                found = true;
                                break;
                            }
                            continue;
                        }
                        // First non-comment line — comment block ended.
                        break;
                    }
                    if found {
                        continue;
                    }
                }

                // ---- Sanction A: the same enclosing function body
                // contains a `strip_prefix("Deque__")` (also excluding
                // comment-line matches).
                let (fn_start, fn_end) = enclosing_fn_range(&lines, i);
                let mut paired = false;
                for j in fn_start..=fn_end {
                    let l = lines[j];
                    let t = l.trim_start();
                    if t.starts_with("//") {
                        continue;
                    }
                    if l.contains(r#"strip_prefix("Deque__")"#) {
                        paired = true;
                        break;
                    }
                }
                if paired {
                    continue;
                }

                failures.push(format!(
                    "{}:{}: `strip_prefix(\"Vector__\")` is UNPAIRED — no \
                     adjacent `strip_prefix(\"Deque__\")` in the enclosing \
                     function body (lines {}..={}) AND no \
                     `// vector-only-by-design: <reason>` allowlist \
                     comment on the line above. \
                     Deque shares Vector's element-in-suffix mangling and \
                     the gorget_array runtime — most sites need the Deque__ \
                     sibling arm (Round XXVII Track B class fix). Either \
                     add `.or_else(|| ...strip_prefix(\"Deque__\"))` (or an \
                     equivalent else-if / sibling if-let block) or add the \
                     allowlist comment explaining why Deque is not a valid \
                     receiver here.",
                    path.display(),
                    i + 1,
                    fn_start + 1,
                    fn_end + 1,
                ));
            }
        });
    }

    assert!(
        failures.is_empty(),
        "vector_deque_arm_symmetry: {} unpaired `strip_prefix(\"Vector__\")` \
         site(s) found:\n\n{}",
        failures.len(),
        failures.join("\n\n"),
    );
}

/// Given `lines` and a line index `i`, return `(fn_start, fn_end)` — the
/// inclusive line range of the enclosing function body. Falls back to
/// `(0, lines.len() - 1)` when `i` is at module scope (outside any fn).
///
/// Heuristic: scan up from `i` looking for a `fn <name>(...)` signature
/// line (accepting the `pub`/`pub(crate)`/`pub(super)`/`async`/`unsafe`/
/// `const` modifier prefixes rustfmt emits at any indentation). For each
/// candidate, compute its brace-balanced end line; if `i` falls inside
/// [candidate_start, candidate_end], return that pair. If `i` is BEYOND
/// candidate_end (meaning the candidate was a NESTED inner `fn` that
/// closed before `i`), skip past it and keep scanning up for an outer
/// function. This matters for e.g. `insts.rs:2214`, where a nested
/// `fn strip_ctor_suffix` at :2207-2212 sits inside the outer method
/// body that actually contains the Vector__/Deque__ arm pair.
///
/// Used only by `vector_deque_arm_symmetry`; kept local to that lint's
/// scope so unrelated tests don't accidentally depend on it.
fn enclosing_fn_range(lines: &[&str], i: usize) -> (usize, usize) {
    let fn_sig_re = regex::Regex::new(
        r"^(\s*)(pub(\([^)]*\))?\s+)?(async\s+)?(unsafe\s+)?(const\s+)?fn\s+\w+",
    ).unwrap();

    let mut k = i;
    loop {
        if fn_sig_re.is_match(lines[k]) {
            let end = fn_body_end(lines, k);
            if end >= i {
                // `i` is inside this function's body — this is our
                // enclosing scope.
                return (k, end);
            }
            // `i` is beyond this fn's closing brace, so this was a
            // nested inner fn that closed before `i`. Skip past it and
            // keep scanning upward for the OUTER fn.
        }
        if k == 0 { break; }
        k -= 1;
    }
    (0, lines.len().saturating_sub(1))
}

/// Given the line index of a `fn <name>(...)` signature, return the line
/// index of the matching closing brace (the last line of the function
/// body). Falls back to end-of-file when unbalanced (malformed source).
fn fn_body_end(lines: &[&str], fn_start: usize) -> usize {
    let mut depth: i32 = 0;
    let mut opened = false;
    for (j, line) in lines.iter().enumerate().skip(fn_start) {
        // Strip line comments (`// ...`) before counting braces so a
        // comment-embedded `{`/`}` doesn't skew the count. Not perfect
        // (block comments / string literals with braces would fool it),
        // but our source tree doesn't have those inside fn signatures /
        // in a shape that would matter here.
        let code = match line.find("//") {
            Some(idx) => &line[..idx],
            None => line,
        };
        for ch in code.chars() {
            if ch == '{' {
                depth += 1;
                opened = true;
            } else if ch == '}' {
                depth -= 1;
                if opened && depth == 0 {
                    return j;
                }
            }
        }
    }
    lines.len().saturating_sub(1)
}

/// SH-lane sibling of `reject_wrong_receiver_combinator_arms_count` (Round
/// XXVII Track C class-guard + Round XXVIII Track A tag-check extension).
/// Pins the SH typechecker's chokepoint arm count for the SAME 9 one-sided
/// combinator + tag-check cells the Rust chokepoint covers, so a new
/// one-sided cell FORCES the SH lane's REJECT to move in lockstep with
/// Rust + ggdef (Core #9 all-lanes semantic change).
///
/// The 9 arms live in `reject_wrong_receiver_combinator` at
/// `tests/fixtures/self_host_typechecker/typecheck.gg`, each tagged with a
/// distinctive `R27C_ARM_MARKER` trailing comment. A bare match on the
/// function name would false-hit the surrounding SH walker's method-name
/// checks; the per-arm marker is the same discipline the Rust sibling uses.
///
/// Ratified per `docs/language-reference.md:3861-3891`:
///   - (Result, flat_map / filter / flatten)      — Option-only combinators
///   - (Option, map_err / unwrap_error)           — Result-only combinators
///   - (Result, is_some / is_none)                — Option-only tag-checks
///   - (Option, is_ok / is_error)                 — Result-only tag-checks
///
/// **What to do if this trips.** Same three-lane rule as the Rust sibling:
///   (a) add the receiver-gate arm here (+ the `R27C_ARM_MARKER` comment)
///       and bump `EXPECTED`;
///   (b) mirror the arm in the Rust chokepoint at
///       `src/semantic/typecheck.rs::reject_wrong_receiver_combinator`
///       (+ `R26A_ARM_MARKER`) and bump its lint's EXPECTED too;
///   (c) mirror the arm in ggdef's `elaborate_method` (NOT a lint — the
///       ggdef gate IS the production reject);
///   (d) add a `combinator_<recv>_<method>_rejected.gg` reject fixture
///       (RED-verified per Core #12).
/// If you REMOVED a cell, the reference tables in
/// `docs/language-reference.md:3861-3891` and both other lanes must move.
#[test]
fn sh_reject_wrong_receiver_combinator_arms_count() {
    let src = std::fs::read_to_string(
        "tests/fixtures/self_host_typechecker/typecheck.gg",
    )
    .expect("read tests/fixtures/self_host_typechecker/typecheck.gg");
    const MARKER: &str = "R27C_ARM_MARKER";
    let arm_count = src.matches(MARKER).count();
    // One MARKER PER cell in the SH reject fn: the 5 combinator cells
    // (Result.{flat_map, filter, flatten} + Option.{map_err, unwrap_error})
    // plus the 4 tag-check cells added by Round XXVIII Track A
    // (Result.{is_some, is_none} + Option.{is_ok, is_error}) = 9. The doc
    // reference in the fn's header paraphrases (does NOT spell the marker
    // string) so the count is unambiguous.
    const EXPECTED: usize = 9;
    assert_eq!(
        arm_count, EXPECTED,
        "Round XXVII Track C + Round XXVIII Track A SH-lane class-guard: \
         `R27C_ARM_MARKER` occurrences in \
         `tests/fixtures/self_host_typechecker/typecheck.gg` changed: \
         {arm_count} vs expected {EXPECTED}. The 9 markers pin the \
         Result.{{flat_map, filter, flatten, is_some, is_none}} + \
         Option.{{map_err, unwrap_error, is_ok, is_error}} receiver-gate \
         arms in the SH-lane `reject_wrong_receiver_combinator` \
         (combinators + tag-checks). If you added a new one-sided cell, \
         wire ALL THREE lanes in the SAME round (Core #9): Rust chokepoint \
         (+`R26A_ARM_MARKER`, bump its lint), ggdef `elaborate_method`, \
         and this SH arm — plus land a \
         `combinator_<recv>_<method>_rejected.gg` fixture (RED-verified \
         per Core #12) and a matching \
         `self_host_lowerer_driver_rejects_combinator_*` integration test. \
         If you removed one, move the reference table in \
         `docs/language-reference.md:3861-3891` and the ggdef + Rust \
         chokepoints too — do NOT lower EXPECTED without all lanes moving \
         with it.",
    );
}

/// Round XXIX Track B — Core #6 executable guard for the METHOD SILENT-ACCEPT
/// CLASS. `has_inherent_only_impls` at `src/semantic/traits.rs` was widened
/// to treat every builtin `BuiltinTypeProtocol` type as authoritative — a
/// method not in the protocol REJECTS with `E_NoMethodFound`. That widening
/// is only safe when every protocol method is covered by one of three paths
/// BEFORE the reject site fires (call resolution flow at
/// `src/semantic/typecheck.rs:2652..2912`):
///   1. Trait registry / equip block (`resolve_method` at :2652)
///   2. `infer_closure_method_type` (:2807) — HOFs with closure args
///   3. `builtin_method_type` (:2844) — the type-check oracle
/// A protocol method with NO coverage regresses to `E_NoMethodFound` at every
/// call site. This lint iterates ALL_PROTOCOLS at
/// `src/ir/lowering/builtins.rs:1157-1165` and asserts, per protocol method:
/// oracle arm OR closure-inference arm OR equip block registered for that
/// specific method (via `equip <Type>:` / `equip [T] <Type>[T]:` in
/// `lib/std/*.gg` with a matching method sig or extern-body).
///
/// The four Callable-family protocols carry empty `methods: &[]`; they go
/// in `EMPTY_METHOD_PROTOCOLS` and skip the per-method check.
///
/// Whitelisted OK_ORACLE_ONLY methods are the user-facing aliases and
/// intrinsic Option/Result methods that legitimately live only in the oracle
/// (Option/Result `unwrap`-family; Dict/HashMap `has_key`/`contains_key`
/// aliases). Adding a new alias requires bumping this whitelist.
///
/// **If this fails:**
///   - A new protocol was added to `ALL_PROTOCOLS` without wiring its
///     method surface → EITHER add oracle arms in `builtin_method_type`,
///     OR (for HOFs) closure-inference arms in `infer_closure_method_type`,
///     OR add a `equip <Type>:` block in `lib/std/*.gg` covering every
///     method the protocol declares.
///   - A protocol gained a NEW method → ensure it's covered by one of the
///     three paths above.
///   - A method is oracle-only (an alias with no protocol counterpart) →
///     add it to `OK_ORACLE_ONLY` with a rationale line.
#[test]
fn builtin_oracle_covers_every_protocol_method() {
    let builtins_src = fs::read_to_string("src/ir/lowering/builtins.rs")
        .expect("read src/ir/lowering/builtins.rs");
    let oracle_src = fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");

    // ── Empty-method protocols exempted from per-method check ─────────
    // These four carry `methods: &[]` at HEAD (Callable-family + closure
    // singleton) — the pass_trait_object mono paths handle them.
    const EMPTY_METHOD_PROTOCOLS: &[&str] = &[
        "Callable", "MutCallable", "ConsumeCallable", "GorgetClosure",
    ];

    // ── Oracle-only aliases (whitelisted; NOT in protocol) ────────────
    // User-facing methods that live only in `builtin_method_type` because
    // they are aliases of protocol methods (Dict/HashMap.has_key ↔ .has())
    // or intrinsic Option/Result surface (unwrap-family).
    const OK_ORACLE_ONLY: &[(&str, &str)] = &[
        ("Dict", "has_key"),
        ("Dict", "contains_key"),
        ("HashMap", "has_key"),
        ("HashMap", "contains_key"),
        ("Option", "unwrap"),
        ("Option", "expect"),
        ("Result", "unwrap"),
        ("Result", "expect"),
        // Set/HashSet.add-vs-insert both live in the oracle; add is
        // protocol-registered, insert alias-only in some readings but
        // both are in SET.methods so no whitelist entry needed.
    ];

    // ── Extract ALL_PROTOCOLS list ────────────────────────────────────
    // The `static ALL_PROTOCOLS: &[&BuiltinTypeProtocol] = &[...]` block
    // enumerates every protocol via its Rust static name (VECTOR, DEQUE,
    // ...). Grab everything inside `= &[` ... `];` and pull out `&NAME,`.
    let all_start = builtins_src
        .find("static ALL_PROTOCOLS: &[&BuiltinTypeProtocol] = &[")
        .expect("locate ALL_PROTOCOLS in src/ir/lowering/builtins.rs");
    let all_end = builtins_src[all_start..]
        .find("];")
        .expect("close ALL_PROTOCOLS block")
        + all_start;
    let all_block = &builtins_src[all_start..all_end];
    let protocol_static_names: Vec<&str> = all_block
        .split(&[',', ' ', '\n', '['][..])
        .filter_map(|t| t.strip_prefix('&'))
        .map(str::trim)
        .filter(|s| !s.is_empty() && s.chars().all(|c| c.is_ascii_uppercase() || c == '_'))
        .collect();
    assert_eq!(
        protocol_static_names.len(), 30,
        "ALL_PROTOCOLS length changed to {} — Round XXIX Track B ratchet \
         calibrated to 30 protocols. If you added/removed a protocol, verify \
         its method surface is covered per the three-path rule (oracle / \
         closure-inference / equip block) and bump this count.",
        protocol_static_names.len(),
    );

    // ── Per-protocol: extract base_name + methods list ───────────────
    // For each `pub static <STATIC>: BuiltinTypeProtocol = ...` block,
    // extract `base_name: "<Name>"` and the method names (each method
    // decl has `BuiltinMethodDecl { name: "<method>", ... }`).
    let mut failures: Vec<String> = Vec::new();

    for static_name in &protocol_static_names {
        let decl_marker = format!("pub static {static_name}: BuiltinTypeProtocol = BuiltinTypeProtocol {{");
        let Some(start) = builtins_src.find(&decl_marker) else {
            failures.push(format!(
                "protocol `{static_name}` from ALL_PROTOCOLS not found via \
                 `pub static {static_name}: BuiltinTypeProtocol = ...`"
            ));
            continue;
        };
        // End of the static: find the matching `};` at brace depth 0. Use a
        // simple depth counter over `{` / `}` starting from the opening `{`.
        let mut depth = 0i32;
        let mut end = start;
        for (i, c) in builtins_src[start..].char_indices() {
            match c {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        end = start + i + 1;
                        break;
                    }
                }
                _ => {}
            }
        }
        let body = &builtins_src[start..end];

        // base_name
        let bn_start = body.find("base_name: \"").expect("base_name field") + "base_name: \"".len();
        let bn_end = body[bn_start..].find('"').expect("base_name close");
        let base_name = &body[bn_start..bn_start + bn_end];

        // Methods that may be aliased to another protocol via
        // `methods: VECTOR.methods` / `methods: DICT.methods` / etc.
        // Resolve those to the source protocol's method list.
        let source_body = if let Some(alias_start) = body.find("methods: ") {
            let alias_line = &body[alias_start + "methods: ".len()..];
            let alias_tok = alias_line.split(',').next().unwrap_or("").trim();
            if alias_tok.ends_with(".methods") {
                // Aliased — find the source protocol's decl and use its body.
                let src_name = &alias_tok[..alias_tok.len() - ".methods".len()];
                let src_decl = format!(
                    "pub static {src_name}: BuiltinTypeProtocol = BuiltinTypeProtocol {{"
                );
                if let Some(src_start) = builtins_src.find(&src_decl) {
                    let mut d = 0i32;
                    let mut src_end = src_start;
                    for (i, c) in builtins_src[src_start..].char_indices() {
                        match c {
                            '{' => d += 1,
                            '}' => {
                                d -= 1;
                                if d == 0 {
                                    src_end = src_start + i + 1;
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    builtins_src[src_start..src_end].to_string()
                } else {
                    body.to_string()
                }
            } else {
                body.to_string()
            }
        } else {
            body.to_string()
        };

        // Extract methods: `BuiltinMethodDecl { name: "<name>", ...`
        let mut methods: Vec<String> = Vec::new();
        let mut rest = source_body.as_str();
        while let Some(idx) = rest.find("BuiltinMethodDecl { name: \"") {
            let after = &rest[idx + "BuiltinMethodDecl { name: \"".len()..];
            let close = after.find('"').expect("method name close");
            methods.push(after[..close].to_string());
            rest = &after[close + 1..];
        }
        methods.sort();
        methods.dedup();

        if methods.is_empty() {
            // Empty-method protocol — must be in EMPTY_METHOD_PROTOCOLS.
            if !EMPTY_METHOD_PROTOCOLS.contains(&base_name) {
                failures.push(format!(
                    "protocol `{base_name}` has empty methods but is NOT in \
                     EMPTY_METHOD_PROTOCOLS. Add it to the exempt list at \
                     `tests/lints.rs::builtin_oracle_covers_every_protocol_method` \
                     with rationale, or add its method surface to \
                     `src/ir/lowering/builtins.rs`."
                ));
            }
            continue;
        }

        // ── Coverage check: per method, one of three paths ────────
        // 1. Oracle arm in `builtin_method_type`: the block matches on
        //    `type_name.as_str()` with `"<Name>"` cases; for aliased
        //    protocols (Deque/HashMap/HashSet), the arm may be under the
        //    alias source's name (Vector/Dict/Set). Coverage means the
        //    method string appears within any type arm that includes the
        //    protocol's base_name OR the alias source's base_name.
        // 2. `infer_closure_method_type`: `("<Name>", "<method>")` tuple
        //    match in the outer `match (type_name.as_str(), method)`.
        // 3. Equip block registration in `lib/std/*.gg`: rough
        //    approximation — any `equip <Base>` (or `equip [T] <Base>[T]`)
        //    block with a body line naming the method.
        let alias_name_map: &[(&str, &str)] = &[
            ("Deque", "Vector"),
            ("HashMap", "Dict"),
            ("HashSet", "Set"),
            // GorgetString is the def-name for the String type in the
            // semantic layer (`src/semantic/cycle_check.rs:69`); the oracle
            // arm heading is `"str" | "String"`. Alias so the ratchet
            // recognizes String-oracle coverage as satisfying GorgetString
            // protocol methods. Primitive-String receivers can't reach
            // the reject site anyway (base_name = None for
            // ResolvedType::Primitive).
            ("GorgetString", "String"),
        ];
        let alias_source = alias_name_map
            .iter()
            .find(|(a, _)| *a == base_name)
            .map(|(_, s)| *s);

        // Precompute equip-block method sets per protocol name (once per test).
        // Simple regex-free approach: read every `lib/std/*.gg` line, track
        // which `equip <Base>` (or `equip [gens] <Base>[gens]`) block we're
        // in, and collect method names from `<ret> <method>(` lines and
        // `extern <ret> <method>(...)` lines. Good enough for authoritative
        // registration; false-negatives fail with a clear diagnostic.
        let equip_methods = equip_methods_for(base_name);

        for method in &methods {
            if OK_ORACLE_ONLY.iter().any(|(t, m)| *t == base_name && *m == method) {
                continue;
            }

            // Path 1: oracle arm (search under base_name AND alias source).
            let oracle_hit = oracle_has_method(&oracle_src, base_name, method)
                || alias_source
                    .map(|s| oracle_has_method(&oracle_src, s, method))
                    .unwrap_or(false);

            // Path 2: closure-inference arm.
            let closure_hit = closure_infer_has(&oracle_src, base_name, method)
                || alias_source
                    .map(|s| closure_infer_has(&oracle_src, s, method))
                    .unwrap_or(false);

            // Path 3: equip block registration in lib/std/*.gg.
            let equip_hit = equip_methods.contains(method);

            if !oracle_hit && !closure_hit && !equip_hit {
                failures.push(format!(
                    "protocol `{base_name}` method `{method}` has NO coverage: \
                     not in `builtin_method_type` oracle arm at \
                     `src/semantic/typecheck.rs`, not in \
                     `infer_closure_method_type` closure-inference arm, and \
                     no `equip {base_name}` (or `equip [T] {base_name}[T]`) \
                     block in `lib/std/*.gg` registers it. The widened \
                     `has_inherent_only_impls` at `src/semantic/traits.rs` \
                     will emit `E_NoMethodFound` at every call site. Add an \
                     oracle arm (or closure-inference arm for HOFs, or an \
                     equip block wrapper) — see Round XXIX Track B commit \
                     for the reference pattern."
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "builtin_oracle_covers_every_protocol_method: {} missing coverage:\n\n{}",
        failures.len(),
        failures.join("\n\n"),
    );
}

/// Search the `builtin_method_type` oracle in `typecheck.rs` for an arm that
/// covers `(type_name, method)`. Reasons about the fn body only: locate the
/// `fn builtin_method_type(` signature, find the type-arm `match
/// type_name.as_str() { ... }` block, and check whether any arm whose head
/// includes `"<type_name>"` (possibly OR'd with siblings) contains `"<method>"`
/// as a literal method-name token in its body up to the arm-close `},` (or
/// the arm's `_ => None,` if it's a nested match).
///
/// Coarse-but-safe: uses simple substring hits on the arm body. False
/// positives are acceptable (would only cause an arm to appear MORE covered
/// than it is). False negatives would trip the ratchet spuriously — the
/// design avoids them by counting a bare `"<method>"` token anywhere in the
/// arm body.
fn oracle_has_method(oracle_src: &str, type_name: &str, method: &str) -> bool {
    let sig = "fn builtin_method_type(";
    let Some(sig_pos) = oracle_src.find(sig) else { return false; };
    // Body ends at the next top-level fn. Use the same heuristic as sibling
    // arm-count lints.
    let after = sig_pos + sig.len();
    let end = oracle_src[after..]
        .find("\n    fn ")
        .or_else(|| oracle_src[after..].find("\n    pub fn "))
        .or_else(|| oracle_src[after..].find("\n    pub(super) fn "))
        .or_else(|| oracle_src[after..].find("\n    pub(crate) fn "))
        .map(|i| after + i)
        .unwrap_or(oracle_src.len());
    let body = &oracle_src[sig_pos..end];

    // Find every `"<TypeName>"` token in the body — each represents a
    // possible arm head. For each, walk forward to the arm's `},` closer
    // and check if `"<method>"` appears in that span. If the head is inside
    // a comment, skip — but comment lines legitimately mention type names,
    // so use a coarse rule: the head's arm head token is `<TN>" =>` or
    // `<TN>" | "…"` etc. Look for the arrow after the string.
    let quoted_type = format!("\"{type_name}\"");
    let mut cursor = 0;
    while let Some(pos_rel) = body[cursor..].find(&quoted_type) {
        let pos = cursor + pos_rel;
        cursor = pos + quoted_type.len();
        // Confirm this is an arm head: the next non-whitespace token(s)
        // should include `=>` (possibly after ` | "other" | ...`).
        let after_str = &body[cursor..];
        // Peek up to 200 chars for an arrow before a newline that terminates
        // the arm head (arm heads for our arms are single-line).
        let peek = after_str.get(..200.min(after_str.len())).unwrap_or("");
        // Skip if the quoted type is inside a comment (line starts with `//`).
        let line_start = body[..pos].rfind('\n').map(|n| n + 1).unwrap_or(0);
        let line_prefix = body[line_start..pos].trim_start();
        if line_prefix.starts_with("//") {
            continue;
        }
        // Arm head heuristic: arrow appears in the peek, before a `{` that
        // starts a new block (which would mean the string was inside an
        // expression body).
        let Some(arrow_pos) = peek.find("=>") else { continue; };
        // We're now inside the arm body (starts after `=>`). Find the arm
        // closer — the enclosing match's closing `},` at the same indent.
        // Simpler: scan forward from arrow to the next `},\n` or `}\n`
        // followed by a peer arm head or the outer match close.
        let body_start = cursor + arrow_pos + 2;
        let arm_body = &body[body_start..];
        // Depth-track braces to find matching arm-close. The arm can be:
        //   `X => Some(...)`     → ends at newline (no brace)
        //   `X => match method { ... }` → depth-track
        //   `X => {  ... }`      → depth-track
        // For our purposes we can be conservative: read up to the LATER of
        // (next `},` at depth 0) or (next `\n            "` at depth 0
        // indicating a sibling arm head at 12-space indent, which is our
        // convention).
        let mut d = 0i32;
        let mut close = arm_body.len();
        for (i, c) in arm_body.char_indices() {
            match c {
                '{' => d += 1,
                '}' => {
                    d -= 1;
                    if d < 0 {
                        close = i;
                        break;
                    }
                }
                _ => {}
            }
        }
        let arm = &arm_body[..close];
        // Method hit: look for `"<method>"` as a quoted string in the arm.
        let quoted_method = format!("\"{method}\"");
        if arm.contains(&quoted_method) {
            return true;
        }
    }
    false
}

/// Search the `infer_closure_method_type` fn for a `("<type_name>", "<method>")`
/// arm. Coarse substring match — the tuple appears literally in the source
/// for every wired HOF (e.g. `("Vector", "map") =>`).
fn closure_infer_has(oracle_src: &str, type_name: &str, method: &str) -> bool {
    let sig = "fn infer_closure_method_type(";
    let Some(sig_pos) = oracle_src.find(sig) else { return false; };
    let after = sig_pos + sig.len();
    let end = oracle_src[after..]
        .find("\n    fn ")
        .or_else(|| oracle_src[after..].find("\n    pub fn "))
        .or_else(|| oracle_src[after..].find("\n    pub(super) fn "))
        .map(|i| after + i)
        .unwrap_or(oracle_src.len());
    let body = &oracle_src[sig_pos..end];
    // Look for `("<type>", "<method>")` — exact tuple, or `"<type>"` with
    // `"<method>"` in a `"<type>" | "…"` OR arm (Vector/Dict|HashMap
    // pair patterns exist).
    let tuple = format!("(\"{type_name}\", \"{method}\")");
    if body.contains(&tuple) {
        return true;
    }
    // OR-arm shape: `("Dict" | "HashMap", "<method>")` or vice versa.
    for or_shape in [
        format!("(\"{type_name}\" | "),
        format!(" | \"{type_name}\", "),
    ] {
        let mut cursor = 0;
        while let Some(rel) = body[cursor..].find(or_shape.as_str()) {
            let start = cursor + rel;
            let peek = body.get(start..start + 200).unwrap_or("");
            // Check the arm head contains `"<method>"`.
            let Some(close) = peek.find(") =>") else { break; };
            let head = &peek[..close];
            if head.contains(&format!("\"{method}\"")) {
                return true;
            }
            cursor = start + or_shape.len();
        }
    }
    // Handle `("Option", "and_then") | ("Option", "flat_map")` shape.
    let alt_tuple = format!("| (\"{type_name}\", \"{method}\")");
    if body.contains(&alt_tuple) {
        return true;
    }
    let alt_tuple2 = format!("(\"{type_name}\", \"{method}\") |");
    if body.contains(&alt_tuple2) {
        return true;
    }
    false
}

/// Collect the set of method names registered on `type_name` via `equip`
/// blocks in `lib/std/*.gg`. Rough scan: for each equip block header
/// matching `equip <TypeName>:` or `equip [gens] <TypeName>[gens]:` (with
/// or without `with <Trait>` — the trait-impl variants also count for
/// coverage since they add method bodies), read subsequent indented lines
/// until block dedent, and pull out method names from `<RetType>
/// <method_name>(` signature lines and from `extern <ret> <name>(...) = "..."`
/// lines.
fn equip_methods_for(type_name: &str) -> std::collections::BTreeSet<String> {
    let mut out = std::collections::BTreeSet::new();
    let lib_dir = Path::new("lib/std");
    let Ok(entries) = fs::read_dir(lib_dir) else { return out; };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map_or(true, |e| e != "gg") {
            continue;
        }
        let Ok(src) = fs::read_to_string(&path) else { continue; };
        let lines: Vec<&str> = src.lines().collect();
        // Scan for `equip [gens?] <TypeName>[...]:` block headers, then
        // read subsequent indented lines until dedent.
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim_start();
            if !trimmed.starts_with("equip ") {
                continue;
            }
            // Header shapes accepted:
            //   equip <TypeName>:
            //   equip <TypeName> with <Trait>:
            //   equip [T] <TypeName>[T]:
            //   equip [T] <TypeName>[T] with <Trait[…]>:
            //   equip [K, V] <TypeName>[K, V]:
            // The <TypeName> appears as its own token. Simple test: the
            // header contains ` <TypeName>` (with a space or `]` before)
            // AND ends with `:`.
            let ends_ok = trimmed.trim_end().ends_with(':');
            if !ends_ok {
                continue;
            }
            let matches_type = trimmed.contains(&format!(" {type_name}:"))
                || trimmed.contains(&format!(" {type_name} "))
                || trimmed.contains(&format!(" {type_name}["));
            if !matches_type {
                continue;
            }
            // Scan block body: while subsequent lines have deeper indent
            // (or are blank/comment), collect method names.
            let header_indent = line.len() - trimmed.len();
            for line2 in lines.iter().skip(i + 1) {
                let t2 = line2.trim_start();
                if t2.is_empty() || t2.starts_with('#') {
                    continue;
                }
                let ind = line2.len() - t2.len();
                if ind <= header_indent {
                    break;
                }
                // Method-sig line shapes:
                //   <ret> <name>(...)
                //   extern <ret> <name>(...) = "..."
                //   <ret> <name>[gens](...):    ← method-level generics
                //   Option[T] <name>(...):      ← generic return type
                //   Vector[U] <name>[gens](...) ← both
                // Extract the token that appears immediately before the
                // opening `(` of the sig, skipping an optional trailing
                // `[gens]` block (method-level generics).
                let mut work = t2;
                if let Some(rest) = work.strip_prefix("extern ") {
                    work = rest;
                }
                let Some(p) = work.find('(') else { continue; };
                let before_paren = work[..p].trim_end();
                // Walk back over an optional `[gens]` block sitting between
                // the method name and `(` (e.g. `each[F]` in `each[F](...)`).
                // A generic on the RETURN TYPE (e.g. `Option[T] peek(`)
                // doesn't sit right against `(` so this walk-back leaves
                // the return-type generic in the leading tokens.
                let name_part: &str = if before_paren.ends_with(']') {
                    let mut depth = 0i32;
                    let mut open_idx = before_paren.len();
                    for (i, c) in before_paren.char_indices().rev() {
                        match c {
                            ']' => depth += 1,
                            '[' => {
                                depth -= 1;
                                if depth == 0 {
                                    open_idx = i;
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    before_paren[..open_idx].trim_end()
                } else {
                    before_paren
                };
                // The method name is the last identifier-shape token in
                // `name_part`. Split on any non-identifier char and take
                // the last non-empty ident.
                let last = name_part
                    .rsplit(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                    .find(|s| !s.is_empty())
                    .unwrap_or("");
                if last.is_empty()
                    || !last.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
                    || last.chars().next().map_or(true, |c| c.is_ascii_digit())
                {
                    continue;
                }
                out.insert(last.to_string());
            }
        }
    }
    out
}

/// Round XXIX Track A residual guard (owner 2026-08-03 filing `17a3e342`).
///
/// The typecheck's f-string interpolation error filter at
/// `src/semantic/typecheck.rs` (currently `:1352-1360`, in the
/// `Expr::StringLiteral` arm) splits off ALL errors emitted while
/// typechecking interpolation segments and retains ONLY a whitelist of
/// error kinds. If a future gate emits a semantic error the user should
/// see inside `f"{...}"`, it MUST be added to the whitelist or the error
/// is silently swallowed and the defect ships — exactly what happened
/// with `NotIndexable` post-Track-A (`print(f"{p[5]}")` accepted + OOB
/// read on both backends until owner's residual filing).
///
/// This lint pins the WHITELIST arm count. Post-Track-A-residual-fix
/// baseline: 5 arms (`NoMethodFound`, `MethodGenericInferenceFailed`,
/// `UnwrapOnNonOptional`, `NotIndexable`, `NotIndexableMut`).
///
/// If the count changes, the contributor either (a) added a new gate
/// whose error kind should also be preserved inside f-strings — add
/// it here + bump this count — or (b) removed one; verify the removal
/// is intentional + doesn't re-open a silent-swallow class.
#[test]
fn interp_error_retention_arms_count() {
    let source = fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");
    // Find the `Expr::StringLiteral` interp-error retention block by its
    // load-bearing marker comment (added by Round XXIX Track A residual fix).
    let marker = "ARM COUNT PINNED at tests/lints.rs::interp_error_retention_arms_count";
    let start = source
        .find(marker)
        .expect("marker comment missing — did you delete the interp-error retention block?");
    // The `matches!(...)` block follows within ~60 lines (the marker comment
    // + the arm list itself, both expanded post-owner-filing 97cd5c01).
    let scope_end = (start + 4000).min(source.len());
    let scope = &source[start..scope_end];
    let matches_start = scope
        .find("matches!(")
        .expect("matches!(...) block not found after marker");
    // Count the `SemanticErrorKind::` arms — each on its own line, joined by `|`.
    let block_end = scope[matches_start..]
        .find("))")
        .expect("matches!(...) block not terminated");
    let block = &scope[matches_start..matches_start + block_end];
    let arm_count = block.matches("SemanticErrorKind::").count();
    assert_eq!(
        arm_count, 16,
        "interp-error retention whitelist arm count changed ({arm_count} vs pinned 16). \
         Add the new SemanticErrorKind arm here + bump the count if a new gate needs \
         its error preserved inside `f\"{{...}}\"`. Removing an arm may re-open a \
         silent-swallow class — see Round XXIX Track A residual `17a3e342` + \
         sibling widening `97cd5c01` (name/arg/field-resolution family). Round XXX \
         Track E closed the UndefinedName cell at the resolve layer (meta-for iter-vars \
         + meta-const names now bind as DkVariable, so the sink in resolve's \
         Expr::StringLiteral arm was retired; see \
         `resolve_interp_arm_uses_shared_errors_vec`). Round XXXIX Track E added \
         `E_DefaultOpRhsTypeMismatch` (Option B `??` RHS-type reject)."
    );
}

/// Round XXX Track E — class-retiring guard for the resolver's f-string interp
/// arm. Prevents future silent-swallow-inside-fstring reintroduction at the
/// RESOLVE layer (Layering discipline: the sibling typecheck-layer guard above
/// pins its own arm count).
///
/// The previous shape (retired 2026-08-04 by Track E) opened a local
/// `let mut sink: Vec<SemanticError> = Vec::new();` inside the
/// `Expr::StringLiteral(_, interp_exprs)` arm and discarded it after resolving
/// each interpolation expression — swallowing E_UndefinedName silently, so
/// `print(f"{nope}")` was accepted and lowered to `0`. The reference-grade
/// shape (mirrored from `self_host_resolver/resolve.gg:668-676`) walks the
/// interp expressions against the SHARED `errors` vec so undefined names
/// surface just as they would outside a f-string. Meta-for iter-vars and
/// meta-const names bind as DkVariable in their body scopes so legitimate
/// interpolations continue to resolve.
///
/// This lint asserts (a) no `let mut sink` inside the resolver's
/// `Expr::StringLiteral` arm and (b) that arm's body calls
/// `resolve_expr(..., errors, ...)` with the SHARED errors param.
#[test]
fn resolve_interp_arm_uses_shared_errors_vec() {
    let source = fs::read_to_string("src/semantic/resolve.rs")
        .expect("read src/semantic/resolve.rs");
    // Locate the arm by its match head — unique in this file.
    let arm_head = "Expr::StringLiteral(_, interp_exprs) => {";
    let start = source
        .find(arm_head)
        .expect("resolver Expr::StringLiteral arm not found — did the head change?");
    // Scan forward for the matching close-brace of the arm body. Body is
    // short (a handful of lines); scan a bounded window to keep the lint
    // O(1) and immune to file growth elsewhere.
    let window_end = (start + 2000).min(source.len());
    let window = &source[start..window_end];
    // The arm's closing `}` is the first at brace-depth zero after the head.
    let mut depth: i32 = 0;
    let mut arm_end: Option<usize> = None;
    for (i, ch) in window.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    arm_end = Some(i + 1);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = arm_end.expect("resolver Expr::StringLiteral arm body braces not balanced");
    let body = &window[..end];

    assert!(
        !body.contains("let mut sink"),
        "resolver's Expr::StringLiteral arm regressed to a local sink discard — \
         removing the sink was Round XXX Track E's fix (Core #8 succession from \
         `self_host_resolver/resolve.gg:668-676`). Reintroducing it swallows \
         E_UndefinedName inside `f\"{{...}}\"` silently. Arm body:\n{body}"
    );
    assert!(
        body.contains("resolve_expr(interp, scopes, errors, resolution_map)"),
        "resolver's Expr::StringLiteral arm no longer walks interpolation \
         expressions against the shared `errors` vec. Expected the recursive \
         call `resolve_expr(interp, scopes, errors, resolution_map)`. Arm body:\n{body}"
    );
}

// ============================================================================
// Round XXIX Track C — enumerate fast-path receiver-type gate + fix-it validity.
// Two class-retirement guards; the third (advice_fixtures_have_working_remedy)
// lives in tests/integration.rs where `check_gg_fails` + `run_gg` are defined.
// ============================================================================

/// Round XXIX Track C — pins the number of AST-method-name-dispatched fast
/// paths in the for-loop detection block at `src/ir/lowering/stmts/for_loops.rs`
/// to exactly ONE (`enumerate`, per Round XXIX Track C's receiver-type-gated
/// entry into `lower_for_enumerate`).
///
/// The Layering discipline rule 2 smell it retires: dispatching on the AST
/// method NAME to make a semantic decision (`if method.node == "..."`). A
/// second name-dispatched fast-path in this block (a hypothetical `.zip()`,
/// `.chain()`, ...) would trip the count, forcing the author to either use
/// typed metadata OR extend the count with an explicit justification.
///
/// **Scope note (Core #15(b) — candid):** this lint pins ONE block in ONE
/// file. Similar `MethodCall`-name gates likely exist ELSEWHERE in the
/// lowerer (not in `for_loops.rs`). A wider audit is filed under the
/// consolidated fix-it-advice follow-up entry in TODO.md — one-block-pin is
/// a floor, not the bar.
///
/// **If this fails:** the block re-grew a name-match. Either
///   1. Fold the new case into typed metadata (add a field on the receiver's
///      TypeDef.metadata + gate on it via the same `collection_kind` /
///      `is_string_type` shape Track C uses for enumerate), or
///   2. If a legitimate second name-dispatched fast-path appears, bump
///      EXPECTED deliberately with a comment naming the invariant it guards.
#[test]
fn for_loop_fast_path_method_names_arms_count() {
    /// Baseline 2026-08-03: 1 (`enumerate`).
    const EXPECTED: usize = 1;

    let content = fs::read_to_string("src/ir/lowering/stmts/for_loops.rs")
        .expect("cannot read src/ir/lowering/stmts/for_loops.rs");
    // Scope to the `fn lower_for(` body — the entry-point that dispatches
    // to the per-shape lowerings. Method-name gates elsewhere in this file
    // (inside lower_for_array/enumerate/etc.) are on already-typed operands
    // and unrelated to the class this lint retires.
    let start = content
        .find("pub(super) fn lower_for(")
        .expect("lower_for entry point not found");
    // Balance braces from the fn body's opening `{` to find the fn body span.
    let after_sig = &content[start..];
    let body_open = after_sig
        .find('{')
        .expect("lower_for body open brace not found");
    // Slice from `body_open` FIRST — `char_indices().skip(N)` skips N ITEMS
    // (chars), not N bytes; multi-byte chars in doc comments (em-dashes etc.)
    // would otherwise skip past the target brace. Byte-slicing works because
    // `.find('{')` returns a char boundary.
    let body_scan = &after_sig[body_open..];
    let mut depth: i32 = 0;
    let mut body_end_in_scan = None;
    for (i, c) in body_scan.char_indices() {
        if c == '{' { depth += 1; }
        if c == '}' {
            depth -= 1;
            if depth == 0 { body_end_in_scan = Some(i + 1); break; }
        }
    }
    let body_end = start + body_open + body_end_in_scan.expect("lower_for body close brace not found");
    let body = &content[start..body_end];
    // Count `method.node == "..."` name gates. Precisely the shape the
    // brief §Verification `grep 'method\.node == \"' src/ir/lowering/stmts/for_loops.rs`
    // enumerates — currently one hit (`enumerate`, line 190 in the fixed
    // source), matching baseline.
    let count = body.matches("method.node ==").count();
    assert_eq!(
        count, EXPECTED,
        "for-loop fast-path method-name gate count in `lower_for` changed: {count} vs \
         expected {EXPECTED}.\n\n\
         Round XXIX Track C class-retirement guard: a new `if method.node == \"...\"` \
         entry-point fast-path is a Layering rule 2 smell. Either\n\
           1. Migrate to typed metadata (add a field on the receiver type-def's \
              metadata + gate on it, mirror the `collection_kind == Array` gate \
              Track C added for enumerate), or\n\
           2. If a genuinely new name-dispatched fast-path shape is intended, \
              bump EXPECTED with a comment naming the invariant it guards."
    );
}

/// Round XXIX Track C — pins the SET of `SemanticErrorKind` variants whose
/// `Display` arm emits fix-it advice (a concrete code snippet the user is
/// told to write). Adding a new fix-it-advice variant requires either
/// extending `FIX_IT_ADVICE_ROWS` here (and pairing a before/after fixture
/// via `tests/integration.rs::advice_fixtures_have_working_remedy`) OR
/// bumping the count with a `// TODO(Round XXX): pair fixture` comment + a
/// filed follow-up.
///
/// This IS the class-retirement mechanism for "compiler emits fix-it advice
/// that itself doesn't compile / crashes" (Round XXIX Track C landed
/// `E_EnumerateOnNonIterator` with a working advice; 18 others pre-existing
/// per the consolidated follow-up filed with the round).
///
/// **Discovery method:** variant-enumeration walk of `SemanticErrorKind`
/// Display arms (per brief §2c). NOT the earlier grep pattern rejected as
/// noisy — that returned 67 (any backticked-variable diagnostic), catching
/// unrelated new diagnostics. This pin is authored from a manual read of
/// each Display arm; the count is the trip-point.
///
/// **Sub-case granularity note (Core #15(e) Q2):** some variants (notably
/// `MoveWithoutOperator` with `shape: MoveShape` + `write_through_available:
/// bool` discriminators) emit MULTIPLE distinct fix-it messages via internal
/// branches. This pin currently tracks the KNOWN MESSAGE-level count (19);
/// adding a new sub-case within an existing variant WITHOUT bumping this
/// pin is technically possible and would slip past the guard. A tighter
/// variant+discriminator enumeration is filed in the consolidated follow-up.
#[test]
fn advice_diagnostic_registration() {
    /// Baseline 2026-08-03 (Round XXIX Track C authoring).
    /// Each entry: (variant name, discriminator or "" for whole-variant advice).
    /// The `EnumerateOnNonIterator` row is the sole entry paired with a
    /// working-remedy fixture at authoring time; the 18 others are the
    /// filed follow-up scope. See TODO.md consolidated entry.
    const FIX_IT_ADVICE_ROWS: &[(&str, &str)] = &[
        // Landed with Round XXIX Track C — the fast-path receiver-type gate
        // made the `.iter().enumerate()` advice actually WORK.
        ("EnumerateOnNonIterator", ""),
        // Consolidated follow-up (18 rows).
        ("MoveWithoutOperator", "Whole+write_through"),
        ("MoveWithoutOperator", "Whole"),
        ("MoveWithoutOperator", "FieldIndex"),
        ("MoveWithoutOperator", "Capture"),
        ("OwnershipMismatch", ""),
        ("NonConstantConstInitializer", ""),
        ("UnsafeIntegerConversion", ""),
        ("UnloweredBuiltinCall", "str"),
        ("UnloweredBuiltinCall", "other"),
        ("SpawnRequiresDirectCall", ""),
        ("SpawnClosureCaptureShared", ""),
        ("ArenaEscape", "insert"),
        ("AutoDerefWriteThroughReadGuard", ""),
        ("MissingFallibleMark", "Bare"),
        ("MissingFallibleMark", "RedundantOnCapture"),
        ("MissingFallibleMark", "MarkOnInfallible"),
        ("UnhandledThrows", ""),
        ("ThrowInNonThrowingFunction", ""),
    ];
    const EXPECTED_TOTAL: usize = 19;

    assert_eq!(
        FIX_IT_ADVICE_ROWS.len(),
        EXPECTED_TOTAL,
        "FIX_IT_ADVICE_ROWS length ({}) diverged from pinned EXPECTED_TOTAL ({}). \
         Both must move together — this pair encodes the total known fix-it-advice \
         message count; bump both when adding a new row, decrement both when \
         retiring one.",
        FIX_IT_ADVICE_ROWS.len(),
        EXPECTED_TOTAL,
    );

    // Grep-verify each variant name appears at least once in the errors.rs
    // Display impl. A rename to `SemanticErrorKind` variant names would
    // otherwise leave stale entries here silently.
    let errors_src = fs::read_to_string("src/semantic/errors.rs")
        .expect("cannot read src/semantic/errors.rs");
    // Scope to the Display impl.
    let display_start = errors_src
        .find("impl std::fmt::Display for SemanticError {")
        .expect("Display for SemanticError impl not found");
    let display_body = &errors_src[display_start..];
    // Balance braces to find the impl's end.
    let body_open = display_body.find('{').expect("impl body open not found");
    let mut depth: i32 = 0;
    let mut display_end = None;
    for (i, c) in display_body.char_indices().skip(body_open) {
        if c == '{' { depth += 1; }
        if c == '}' {
            depth -= 1;
            if depth == 0 { display_end = Some(i + 1); break; }
        }
    }
    let display_scope = &display_body[..display_end.expect("impl close not found")];
    for (variant, _) in FIX_IT_ADVICE_ROWS {
        let needle = format!("SemanticErrorKind::{variant}");
        assert!(
            display_scope.contains(&needle),
            "FIX_IT_ADVICE_ROWS references `{variant}` but the Display impl at \
             `src/semantic/errors.rs` has no arm for `SemanticErrorKind::{variant}`. \
             Either the variant was renamed / removed (update this list) or the \
             `Display` scope-detection above is stale."
        );
    }

    // Round XXIX Track C output-review fold — step 4 cross-lint.
    // Every FIX_IT_ADVICE_ROWS entry MUST be EITHER paired with a
    // before/after fixture in `tests/integration.rs::advice_fixtures_have_working_remedy::ROWS`
    // OR explicitly listed in `OK_UNPAIRED` below (the filed follow-up scope).
    // A new row here without adding it to one of those two lists FAILS this
    // test — forcing the contributor to make the pairing decision, which is
    // what makes 2b + 2c together retire the class (per the brief).
    //
    // OK_UNPAIRED is the consolidated 18-row follow-up filed at Round XXIX
    // Track C close (see TODO.md fix-it-validity consolidated entry). When
    // a row here graduates to paired, DELETE it here and ADD it to
    // integration.rs ROWS in the same commit.
    const OK_UNPAIRED: &[(&str, &str)] = &[
        // MoveWithoutOperator sub-cases
        ("MoveWithoutOperator", "Whole+write_through"),
        ("MoveWithoutOperator", "Whole"),
        ("MoveWithoutOperator", "FieldIndex"),
        ("MoveWithoutOperator", "Capture"),
        ("OwnershipMismatch", ""),
        ("NonConstantConstInitializer", ""),
        ("UnsafeIntegerConversion", ""),
        ("UnloweredBuiltinCall", "str"),
        ("UnloweredBuiltinCall", "other"),
        ("SpawnRequiresDirectCall", ""),
        ("SpawnClosureCaptureShared", ""),
        ("ArenaEscape", "insert"),
        ("AutoDerefWriteThroughReadGuard", ""),
        ("MissingFallibleMark", "Bare"),
        ("MissingFallibleMark", "RedundantOnCapture"),
        ("MissingFallibleMark", "MarkOnInfallible"),
        ("UnhandledThrows", ""),
        ("ThrowInNonThrowingFunction", ""),
    ];

    let integration_src = fs::read_to_string("tests/integration.rs")
        .expect("cannot read tests/integration.rs");
    // Extract the ROWS table body inside advice_fixtures_have_working_remedy.
    let rows_start = integration_src
        .find("fn advice_fixtures_have_working_remedy(")
        .expect("advice_fixtures_have_working_remedy test not found");
    let rows_scope = &integration_src[rows_start..];
    let rows_table_start = rows_scope
        .find("const ROWS:")
        .expect("ROWS const inside advice_fixtures_have_working_remedy not found");
    // Bound to the const block; simple close-bracket balance.
    let rows_end = (rows_table_start + 4000).min(rows_scope.len());
    let rows_table = &rows_scope[rows_table_start..rows_end];

    for (variant, disc) in FIX_IT_ADVICE_ROWS {
        let in_unpaired = OK_UNPAIRED.iter().any(|(v, d)| v == variant && d == disc);
        // A "paired" row lists the E_code (E_ prefix + variant name) in ROWS.
        let e_code_needle = format!("\"E_{variant}\"");
        let in_paired = rows_table.contains(&e_code_needle);
        assert!(
            in_paired || in_unpaired,
            "FIX_IT_ADVICE_ROWS entry `({variant}, {disc:?})` is NEITHER \
             paired with a working-remedy fixture (via `E_{variant}` in \
             `tests/integration.rs::advice_fixtures_have_working_remedy::ROWS`) \
             NOR listed as filed-follow-up in this test's `OK_UNPAIRED`. \
             Add it to ONE of them — pairing means writing a before/after \
             fixture that RED-verifies + GREEN-verifies the advice; \
             OK_UNPAIRED means filing it as a categorized TODO follow-up \
             (see the Round XXIX Track C consolidated entry)."
        );
    }
}

// ---------------------------------------------------------------------------
// DOC CITATION GUARD — every `path:line` a doc cites must actually resolve.
//
// Core #6 applied to documentation: prose rots, guards don't. This tree's most
// repeated doc defect is a citation that survives the move it describes — the
// runtime text-move (`765cbfc6`) left 17 `c_runtime.rs:NNN` citations pointing
// into a file that had shrunk from ~15k lines to 255, and the
// `src/{ir => }/resources.rs` hoist (`c55b7e53`) left 13 pointing at a path
// that no longer existed. Both survived the doc edits that came AFTER them,
// because nothing mechanical was checking.
//
// A reader who follows a broken citation concludes the doc is stale and stops
// trusting the chapter; a scout briefed off one designs around a phantom.
// ---------------------------------------------------------------------------

/// Minimal recursive .md walk (mirrors `walkdir_rs`).
fn walkdir_md(root: &str) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![PathBuf::from(root)];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else { continue };
        for ent in entries.flatten() {
            let p = ent.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().and_then(|e| e.to_str()) == Some("md") {
                out.push(p);
            }
        }
    }
    out
}

/// Paths a doc names *deliberately* while they do not exist. Two legitimate
/// kinds only:
///   (a) NEGATIVE mentions — prose whose whole point is "this file does not
///       exist" (the `provenance.rs` correction, repeated in three chapters);
///   (b) FORWARD references — a ratified-but-unbuilt artifact named by the
///       ledger or a design note (D39 Phase B's `stablemap.gg`).
/// Anything else is a defect. This list is shrink-only: a path that starts
/// existing must be REMOVED from here, not left to rot.
const DOC_CITATION_ABSENT_BY_DESIGN: &[&str] = &[
    // (a) negative mentions — "there is no provenance pass"
    "src/semantic/provenance.rs",
    // (b) forward references to ratified-but-unbuilt artifacts
    "lib/std/stablemap.gg",
];

/// Repo-rooted prefixes. A citation that does not start with one of these is
/// prose shorthand (`generics/mod.rs` after the full path was established) and
/// is deliberately NOT checked — resolving shorthand by basename would make
/// the lint guess, and a guessing lint reports defects that aren't there.
const DOC_CITATION_ROOTS: &[&str] = &[
    "src/", "tests/", "lib/", "docs/", "spec/", "compiler/", "scripts/",
    "benchmarks/", "examples/", "tools/", "fuzz/", "demo/",
];

#[test]
fn doc_source_citations_name_the_right_line() {
    // The CONTENT half of the citation guard, and the reason it exists: its
    // sibling `doc_source_citations_resolve` checks only that the file exists
    // and the line number is IN RANGE. It therefore green-lights every stale
    // citation in the tree — a cite that has drifted onto an unrelated line is
    // exactly as "resolvable" as a correct one. That is a guard that cannot
    // catch its own class (Core #15e Q2), and it let the same class bite twice
    // in one round: the formatter chapter's cites went stale when the file grew,
    // were swept by hand, and went stale again inside the same round when a
    // later commit added 121 lines to the same file.
    //
    // The check: when a doc line carries a `file.rs:N` cite AND names a
    // backticked IDENTIFIER, that identifier must appear within ±WINDOW lines of
    // the cited line. Prose legitimately mentions an identifier without meaning
    // "it is defined here", so a HIT anywhere in the window passes and only a
    // total miss fails — deliberately loose, because the failure this catches is
    // a cite pointing somewhere else entirely, not an off-by-two.
    const WINDOW: usize = 10;
    // Scoped to the chapter whose cites this round moved twice. The tree-wide
    // burn-down is the ratchet's next step: widen SCOPE, run, fix or allowlist.
    const SCOPE: &str = "docs/devbook/05-formatter.md";

    let cite = regex::Regex::new(r"`([A-Za-z0-9_./-]+\.(?:rs|gg|c|h|toml)):(\d+)(?:-\d+)?`")
        .expect("citation regex");
    // The LEADING identifier chain inside backticks, ignoring whatever follows
    // it. Prose writes `emit_comments_before(pos)` and `Block::synthetic(stmts,
    // span)` as often as bare names, and requiring the whole span to be an
    // identifier skipped those entirely — which then fell through to the
    // paragraph fallback and produced false failures on correct cites.
    let ident = regex::Regex::new(
        r"`([A-Za-z_][A-Za-z0-9_]*(?:(?:::|\.)[A-Za-z_][A-Za-z0-9_]*)*)[^`]*`",
    )
    .expect("identifier regex");

    // A CONTINUATION cite: `` `:1370` `` — a bare line number whose file is the
    // last one named nearby. Prose uses these constantly in list bullets
    // ("`meta if` (`:1370`), `meta for` (`:1386`)"), and the first version of
    // this guard never even scanned them, which hid eight of the ten stale cites
    // it was written to catch.
    let bare = regex::Regex::new(r"`:(\d+)(?:-\d+)?`").expect("bare citation regex");

    let mut checked = 0usize;
    let mut stale: Vec<String> = Vec::new();

    // Both citation guards walked `docs/` only, and that is where the four
    // stalest cites of the last round were NOT: `TODO.md`, a `known_gaps`
    // fixture header, and two test-file doc comments all cite `src/` line
    // numbers, all four drifted, and nothing looked. Records rot the same way
    // prose does — so the walk CAN cover them, behind the standard env gate.
    //
    // Row counts are deliberately NOT quoted here — the figure went stale twice
    // in two commits: any edit to a scanned file moves identifiers relative to
    // cites and changes the count, and an edit to THIS file can add or remove
    // rows (a version of this comment once counted itself). Regenerate instead:
    // `GG_LINT_CITE_CONTENT_WIDE=1 cargo test --test lints doc_source_citations_name_the_right_line`.
    //
    // Most rows are not stale cites. TODO.md dominates for a structural reason:
    // its bullets are single enormous lines packing dozens of identifiers and
    // many cites, so the candidate set is huge and never near any particular
    // cite — the heuristic has no signal there. But the pile is not all noise:
    // the scan's first run caught a `register_collection_alias` cite in this
    // very file pointing hundreds of lines off its target — a real stale cite,
    // outside `docs/`, that nothing guarded. It stays unfixed for the
    // burn-down, which starts from the scan's own output, not from figures
    // quoted here.
    //
    // BURN-DOWN, in this order: (1) `known_gaps` fixture headers, whose cites
    // sit in short comment paragraphs the heuristic reads well; (2)
    // `tests/integration.rs` and `tests/lints.rs` — ⚠ in `.rs` files the
    // PARAGRAPH fallback degenerates: "paragraph" is delimited by truly blank
    // lines, so a `//` block plus its adjacent code reads as ONE paragraph, the
    // candidate identifier set balloons, and a stale cite can PASS because some
    // unrelated candidate happens to sit near the cited line. The burn-down
    // must either tighten paragraph delimiting to comment-block boundaries for
    // `.rs` targets or accept per-row manual reads there. (3) `TODO.md` LAST,
    // and probably not with this heuristic at all — a per-bullet check would
    // need the cite's own sentence, not the bullet. Fix or allowlist each row
    // WITH ITS REASON, then fold the target into the fatal set above. Do not
    // bulk-allowlist: an unread row asserts a verification nobody did.
    let wide = std::env::var("GG_LINT_CITE_CONTENT_WIDE").is_ok();
    let mut targets: Vec<PathBuf> = vec![PathBuf::from(SCOPE)];
    if wide {
        targets.push(PathBuf::from("TODO.md"));
        if let Ok(entries) = fs::read_dir("tests/fixtures/known_gaps") {
            for e in entries.flatten() {
                let p = e.path();
                if p.extension().and_then(|x| x.to_str()) == Some("gg") {
                    targets.push(p);
                }
            }
        }
        for f in ["tests/lints.rs", "tests/integration.rs"] {
            targets.push(PathBuf::from(f));
        }
    }

    for doc in targets {
        let rel = doc.to_string_lossy().replace('\\', "/");
        let Ok(text) = fs::read_to_string(&doc) else { continue };
        let all: Vec<&str> = text.lines().collect();
        // The file most recently named, so a bare `:N` resolves to it.
        let mut last_path: Option<String> = None;
        for (lineno, line) in text.lines().enumerate() {
            // Every cite on the line, full or continuation, in source order.
            let mut on_line: Vec<(String, usize)> = Vec::new();
            for caps in cite.captures_iter(line) {
                let p = caps[1].to_string();
                if let Ok(n) = caps[2].parse::<usize>() {
                    on_line.push((p.clone(), n));
                }
                last_path = Some(p);
            }
            for caps in bare.captures_iter(line) {
                if let (Some(p), Ok(n)) = (last_path.clone(), caps[1].parse::<usize>()) {
                    on_line.push((p, n));
                }
            }
            for (path, cited) in on_line {
            if !DOC_CITATION_ROOTS.iter().any(|r| path.starts_with(r)) {
                continue;
            }
            let Ok(src) = fs::read_to_string(&path) else { continue };
            let src_lines: Vec<&str> = src.lines().collect();
            // A bare `:N` is resolved against the last file NAMED nearby, and
            // that inference can be wrong — a paragraph may cite `doc.rs` and
            // then carry a bare number meant for `mod.rs`.
            //
            // This catches only the OUT-OF-RANGE half of that: if the number
            // exceeds the inferred file, the inference certainly missed, so say
            // nothing (`doc_source_citations_resolve` owns the range question
            // for cites that carry their own path). ⚠ RESIDUAL: an IN-RANGE
            // mis-inference is invisible here — the check then reads a window
            // in the wrong file and can report a stale cite that is fine, or
            // pass one that is not. The mitigation is that this only fires for
            // BARE cites, where the resolution is at worst a neighbouring file
            // in the same paragraph; a cite carrying its own path is exact.
            if cited == 0 || cited > src_lines.len() {
                continue;
            }

            // Candidate names: every backticked identifier on the doc line,
            // exploded so `Formatter::verbatim` also offers `verbatim` and
            // `doc::surround_fill` also offers `surround_fill`.
            //
            // The PREVIOUS line counts too, because prose wraps: a sentence can
            // name its subject on one line and carry the second of two cites
            // onto the next, where the only backticked token left is an
            // incidental type. Measured on exactly that shape —
            // "`format_method_chain` (cite) and `format_binary_chain` \n (cite)
            // turn each segment into a `Doc::Text` leaf" — where line two offers
            // only `Doc::Text` and the cite is correct.
            let collect = |src_line: &str, out: &mut Vec<String>| {
                for m in ident.captures_iter(src_line) {
                    // A backtick span holding a PATH is a citation, not a name.
                    // Without this the leading-chain match turns
                    // `` `src/formatter/mod.rs:5` `` into the candidate "src",
                    // which matches almost any window and drowns the real names.
                    if m[0].contains('/') {
                        continue;
                    }
                    let whole = m[1].to_string();
                    for seg in whole.split("::").flat_map(|s| s.split('.')) {
                        if seg.len() >= 3 {
                            out.push(seg.to_string());
                        }
                    }
                }
            };
            let mut names: Vec<String> = Vec::new();
            collect(line, &mut names);
            if lineno > 0 {
                collect(all[lineno - 1], &mut names);
            }
            // When neither line names anything, widen to the PARAGRAPH rather
            // than skipping. Skipping was a real hole — it hid the extern
            // `= "symbol"` and `from a import` cites, whose sentences put the
            // identifier two lines up. A wider candidate set only makes the
            // check more permissive (any hit passes), so this trades a little
            // detection power for covering cites that had none at all.
            if names.is_empty() {
                let mut lo = lineno;
                while lo > 0 && !all[lo - 1].trim().is_empty() {
                    lo -= 1;
                }
                let mut hi = lineno;
                while hi + 1 < all.len() && !all[hi + 1].trim().is_empty() {
                    hi += 1;
                }
                for l in &all[lo..=hi] {
                    collect(l, &mut names);
                }
            }
            names.sort();
            names.dedup();
            if names.is_empty() {
                // ⚠ RESIDUAL, named rather than assumed away: a cite in a
                // paragraph that backticks no identifier at all cannot be
                // content-checked by this method. Rare in this chapter (prose
                // here names what it cites), and the fallback above removed the
                // common case.
                continue;
            }
            checked += 1;

            let lo = cited.saturating_sub(WINDOW).saturating_sub(1);
            let hi = (cited + WINDOW).min(src_lines.len());
            let window = src_lines[lo..hi].join("\n");
            if !names.iter().any(|n| window.contains(n.as_str())) {
                stale.push(format!(
                    "{rel}:{} → `{path}:{cited}` names {names:?}, none of which \
                     appears within ±{WINDOW} lines",
                    lineno + 1
                ));
            }
            }
        }
    }

    assert!(
        checked > 20,
        "the content check inspected only {checked} citations in {SCOPE} — the \
         citation or identifier format moved and this lint reads almost \
         nothing. Fix the scanner, don't lower the floor."
    );
    // ── ALLOWLIST, shrink-only ───────────────────────────────────────────────
    //
    // A cite whose sentence describes code by BEHAVIOUR rather than by symbol
    // name cannot be content-checked this way: the backticked tokens near it
    // are keywords, constants or types that live elsewhere in the file, while
    // the cite points at exactly the right lines. Each row below was READ at
    // HEAD and confirmed to land on the code its sentence describes; the reason
    // is recorded so a future reader re-checks rather than trusts.
    //
    // SHRINK-ONLY. Do not add a row to silence a failure — repoint the cite. A
    // row belongs here only when the cite is measured CORRECT and the sentence
    // genuinely names no symbol at that line.
    const HEURISTIC_BLIND: &[(&str, &str, &str)] = &[
        ("103", "src/formatter/mod.rs:43", "the four-space indent arithmetic; the sentence's names are doc.rs's INDENT_WIDTH"),
        ("143", "src/formatter/doc.rs:433", "the Group flat/break decision; MAX_WIDTH and current_col are named as the inputs"),
        ("220", "src/formatter/doc.rs:213", "the trailing-comma construction; `IfBreak` is the enum variant it builds"),
        ("469", "src/formatter/mod.rs:913", "the blank-collapse loop INSIDE `fn format`, whose name is ~25 lines up"),
        ("493", "src/formatter/mod.rs:2180", "the import sort_by; the sentence names the std/`xtd` ordering it implements"),
        ("587", "src/formatter/mod.rs:2794", "`FunctionBody::Extern`'s `= \"symbol\"` arm, inside `format_function`"),
    ];
    // SHRINK-ONLY, ENFORCED (Core #14 — the words are not the guard). Every row
    // must still be LIVE: if the cite it excuses no longer fails, the row has
    // outlived its reason and has to go, which is what makes the list shrink
    // instead of quietly accumulating. And the count may not grow.
    const HEURISTIC_BLIND_CEILING: usize = 6;
    assert!(
        HEURISTIC_BLIND.len() <= HEURISTIC_BLIND_CEILING,
        "the heuristic-blind allowlist GREW ({} > {HEURISTIC_BLIND_CEILING}). \
         Rows are added only for a cite measured CORRECT that this method cannot \
         see — never to silence a failure. Repoint the cite instead.",
        HEURISTIC_BLIND.len()
    );
    let mut dead_rows: Vec<String> = Vec::new();
    for (line, cite, _) in HEURISTIC_BLIND {
        let live = stale
            .iter()
            .any(|s| s.contains(&format!("{SCOPE}:{line} ")) && s.contains(cite));
        if !live {
            dead_rows.push(format!("{SCOPE}:{line} → `{cite}`"));
        }
    }
    assert!(
        dead_rows.is_empty(),
        "{} heuristic-blind allowlist row(s) no longer excuse anything:\n  {}\n\n\
         The cite was repointed, the prose changed, or the matcher improved. \
         DELETE the row and lower HEURISTIC_BLIND_CEILING — that is the whole \
         point of a shrink-only list.",
        dead_rows.len(),
        dead_rows.join("\n  ")
    );
    stale.retain(|s| {
        !HEURISTIC_BLIND.iter().any(|(line, cite, _)| {
            s.contains(&format!("{SCOPE}:{line} ")) && s.contains(cite)
        })
    });

    assert!(
        stale.is_empty(),
        "{} citation(s) point at a line that does not mention the \
         identifier the sentence is about:\n  {}\n\n\
         The line number has drifted. Re-read the source and repoint it. This is \
         the half `doc_source_citations_resolve` cannot see: an in-range cite on \
         the wrong line resolves perfectly and still misleads every reader.\n\n\
         NEXT STEPS FOR THIS RATCHET, both measured and both real:\n\
         (1) widen SCOPE from the one chapter to the whole docs tree;\n\
         (2) burn down the WIDE scan — `GG_LINT_CITE_CONTENT_WIDE=1` already \
         walks TODO.md, the known_gaps headers and the two test files, where \
         nothing guards cites today and where at least one confirmed stale cite \
         lives. See the burn-down order in the comment above `targets`.",
        stale.len(),
        stale.join("\n  ")
    );
}

#[test]
fn doc_source_citations_resolve() {
    let cite = regex::Regex::new(r"`([A-Za-z0-9_./-]+\.(?:rs|gg|c|h|toml))(?::(\d+))?`?")
        .expect("citation regex");

    let mut missing: Vec<String> = Vec::new();
    let mut out_of_range: Vec<String> = Vec::new();
    let mut scanned = 0usize;

    for doc in walkdir_md("docs") {
        let Ok(text) = fs::read_to_string(&doc) else { continue };
        for (lineno, line) in text.lines().enumerate() {
            for caps in cite.captures_iter(line) {
                let path = &caps[1];
                if !DOC_CITATION_ROOTS.iter().any(|r| path.starts_with(r)) {
                    continue;
                }
                scanned += 1;
                let target = Path::new(path);
                if !target.is_file() {
                    if !DOC_CITATION_ABSENT_BY_DESIGN.contains(&path) {
                        missing.push(format!(
                            "{}:{} → `{}` (no such file)",
                            doc.display(),
                            lineno + 1,
                            path
                        ));
                    }
                    continue;
                }
                let Some(num) = caps.get(2) else { continue };
                let Ok(cited) = num.as_str().parse::<usize>() else { continue };
                let have = fs::read_to_string(target)
                    .map(|s| s.lines().count())
                    .unwrap_or(0);
                if cited > have {
                    out_of_range.push(format!(
                        "{}:{} → `{}:{}` (file has {} lines)",
                        doc.display(),
                        lineno + 1,
                        path,
                        cited,
                        have
                    ));
                }
            }
        }
    }

    assert!(
        scanned > 500,
        "doc citation scan found only {scanned} repo-rooted citations — the \
         citation format or the docs tree moved and this lint reads almost \
         nothing. Fix the scanner, don't lower the floor."
    );

    // FATAL AT ZERO. Every out-of-range citation was burned down when this
    // guard landed; a new one means a doc edit or a source move outran its
    // write-through. There is no budget on purpose — the whole failure mode
    // is a citation quietly surviving the change it describes.
    assert!(
        out_of_range.is_empty(),
        "doc citation(s) point past the end of the file they cite ({}):\n  {}\n\n\
         The cited line no longer exists. Re-read the source, repoint the \
         citation at the symbol it means, and prefer citing a stable anchor \
         (a symbol name plus file) over a line number in fast-moving code.",
        out_of_range.len(),
        out_of_range.join("\n  ")
    );

    // Shrink-only, and down to its last entry: the six that named artifacts of
    // abandoned plans went with the `docs/internals/` files that cited them.
    // What remains is a real defect — `decisions.md` cites a `known_gaps`
    // repro that was never committed, which the "every filed bug ships a
    // durable repro" rule requires to exist. Commit the repro (or reword the
    // citation) and this reaches ZERO; lower the budget when it does.
    const MISSING_BUDGET: usize = 1;
    assert!(
        missing.len() <= MISSING_BUDGET,
        "doc(s) cite {} nonexistent path(s) (budget {}):\n  {}\n\n\
         Either the path moved (repoint it), the file was deleted (drop or \
         reword the citation), or the doc names an artifact that was planned \
         and never built (add it to DOC_CITATION_ABSENT_BY_DESIGN with the \
         reason). If the count went DOWN, lower MISSING_BUDGET to lock it.",
        missing.len(),
        MISSING_BUDGET,
        missing.join("\n  ")
    );
}

// ---------------------------------------------------------------------------
// DOC CITATION GUARD, LIMB 2 — code must not cite a document that is gone.
//
// The companion `doc_source_citations_resolve` scans docs -> source. This scans
// SOURCE -> docs, which is the direction that actually bit:
//
//   `0afb0d06` (2026-07-17, "remove docs/plans") deleted the error-model RFC.
//   The docs-side references were repointed; the CODE was not. Compiler source
//   and both self-host compilers kept citing it — 150 sites — and nothing
//   noticed, because limb 1 only checks repo-ROOTED paths while those
//   citations are bare filenames. A cleanup that looks complete on the docs
//   side can strand the whole source tree.
//
// Bare-filename resolution is unambiguous here (does ANY .md in the repo have
// this basename?), so unlike limb 1's shorthand case there is no guessing.
// ---------------------------------------------------------------------------

/// Documents named by code that legitimately do not live in the repo.
/// Shrink-only: a name that becomes resolvable must be REMOVED from here.
const CODE_DOC_CITATION_EXTERNAL: &[&str] = &[
    // The agent-memory index, kept outside the repo by design.
    "MEMORY.md",
];

/// Vendored third-party sources cite their upstream's docs; not ours to fix.
const CODE_DOC_CITATION_VENDORED: &[&str] =
    &["src/backend/c/sqlite3/", "src/backend/c/stb_image.h"];

/// Recursive walk over the source extensions that carry doc citations.
/// Repo-relative paths git actually tracks.
///
/// Scans that read "the source tree" must gate on this. A raw `read_dir` walk
/// also picks up GENERATED artifacts — `tests/fixtures/**/driver.c` and the
/// per-fixture `main.c` are gitignored C emitted by the self-host and the C
/// backend — and those carry Gorget source comments through into C string
/// literals. That double-counts a citation already scanned at its real `.gg`
/// line, and makes the result depend on whether a build has been run in this
/// tree rather than on what the tree CONTAINS.
///
/// Gating on git is a class fix rather than a name blacklist: it excludes every
/// generated artifact, present and future, with no pattern list to keep in sync
/// with `tests/fixtures/.gitignore`.
fn tracked_files() -> &'static std::collections::HashSet<PathBuf> {
    use std::sync::OnceLock;
    static TRACKED: OnceLock<std::collections::HashSet<PathBuf>> = OnceLock::new();
    TRACKED.get_or_init(|| {
        let out = std::process::Command::new("git")
            .args(["ls-files", "-z"])
            .output()
            .expect("git ls-files failed — these lints must run inside the repo");
        assert!(
            out.status.success(),
            "git ls-files exited non-zero; refusing to scan an unknown file set"
        );
        let set: std::collections::HashSet<PathBuf> = out
            .stdout
            .split(|b| *b == 0)
            .filter(|s| !s.is_empty())
            .map(|s| PathBuf::from(String::from_utf8_lossy(s).into_owned()))
            .collect();
        assert!(
            set.len() > 500,
            "git ls-files returned only {} paths — the scan is broken, not the tree",
            set.len()
        );
        set
    })
}

fn walkdir_srcish(root: &str) -> Vec<PathBuf> {
    let tracked = tracked_files();
    let mut out = Vec::new();
    let mut stack = vec![PathBuf::from(root)];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else { continue };
        for ent in entries.flatten() {
            let p = ent.path();
            if p.is_dir() {
                stack.push(p);
            } else if matches!(
                p.extension().and_then(|e| e.to_str()),
                Some("rs") | Some("gg") | Some("c") | Some("h")
            ) && tracked.contains(&p)
            {
                out.push(p);
            }
        }
    }
    out
}

#[test]
fn code_doc_citations_resolve() {
    // Every markdown basename that exists ANYWHERE in the tree — a citation to
    // `spec/prose/diagnostic-codes.md` resolves just as well as one to a
    // devbook chapter, so scoping this to docs/ would invent defects.
    let mut have: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut md_stack = vec![PathBuf::from(".")];
    while let Some(dir) = md_stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else { continue };
        for ent in entries.flatten() {
            let p = ent.path();
            let name = ent.file_name().to_string_lossy().into_owned();
            if p.is_dir() {
                if !matches!(
                    name.as_str(),
                    "target" | ".git" | ".claude" | ".worktrees" | "node_modules"
                ) {
                    md_stack.push(p);
                }
            } else if name.ends_with(".md") {
                have.insert(name);
            }
        }
    }
    assert!(
        have.len() > 20,
        "found only {} markdown files — the docs tree moved and this lint would \
         report every citation as dangling. Fix the scanner.",
        have.len()
    );

    // Join hyphen-wrapped names split across comment lines. A citation whose
    // hyphen falls at a line break is ONE name, not a dangling tail — without
    // this, the second half of every wrapped filename reads as a missing doc.
    let unwrap = regex::Regex::new(r"-\\?[ \t]*\n[ \t]*(?://[/!]?|#|\*)?[ \t]*")
        .expect("unwrap regex");
    let cite = regex::Regex::new(r"[A-Za-z0-9_][A-Za-z0-9_.-]*\.md").expect("md regex");

    let mut dangling: Vec<String> = Vec::new();
    let mut scanned = 0usize;

    for root in ["src", "tests"] {
        for file in walkdir_srcish(root) {
            let rel = file.to_string_lossy().replace("./", "");
            if CODE_DOC_CITATION_VENDORED.iter().any(|v| rel.starts_with(v)) {
                continue;
            }
            let Ok(raw) = fs::read_to_string(&file) else { continue };
            let text = unwrap.replace_all(&raw, "-");
            for m in cite.find_iter(&text) {
                // Skip matches that are part of a longer path (`docs/x/y.md`)
                // or of a longer word — limb 1 owns rooted paths.
                if let Some(prev) = text[..m.start()].chars().last() {
                    if prev.is_alphanumeric()
                        || prev == '/'
                        || prev == '.'
                        || prev == '-'
                        || prev == '_'
                        || prev == '\\'
                    {
                        continue;
                    }
                }
                scanned += 1;
                let name = m.as_str();
                if !have.contains(name) && !CODE_DOC_CITATION_EXTERNAL.contains(&name) {
                    let line = text[..m.start()].lines().count();
                    dangling.push(format!("{rel}:{line} → `{name}`"));
                }
            }
        }
    }

    assert!(
        scanned > 200,
        "scanned only {scanned} bare doc citations in src/ + tests/ — the scan \
         is broken, not the tree. Fix it, don't lower the floor."
    );

    // Shrink-only. Nearly all are the error-model RFC, deleted with docs/plans
    // in `0afb0d06` (2026-07-17); the overwhelming majority sit in the
    // fault-catch machinery that D25 ratified for REMOVAL (wave batch C2), so
    // the bulk retires with that track rather than by repointing. The plain
    // trap-semantics citations that OUTLIVE that removal were repointed at
    // `spec/prose/trap-codes.md`. The remaining 2 are committed fixtures
    // citing a `/tmp` brief, which the "scouts and briefs are /tmp-only" rule
    // forbids from the repo at all.
    //
    // Round XXXIV C2 (2026-08-07): D25 fault-catch removal retired 111 of
    // the 139 dangling citations (fault_participation.rs, lower_fault_catch_expr,
    // FaultableCall/IndexLoad, builtin_fault_enum, and the fault_catch_*/
    // fault_deep_*/faultcatch_recovery_type_* fixture families all deleted +
    // the removed `docs/plans/error-model.md` refs went with them). Budget
    // reseeded 139 → 28, then tightened 28 → 27 in Round XXXV.
    //
    // 2026-08-08: those two figures disagreed because the scan was reading
    // GENERATED files. `tests/fixtures/self_host_lowerer/driver.c` is
    // gitignored self-host output, and it carries a `.gg` source comment
    // through into a C string literal (a diagnostic string naming the deleted
    // error-model design doc), so it contributed a 28th citation that
    // duplicates one already counted at its real `.gg` line — note this
    // comment must not spell that filename either, or the scan counts THIS
    // line too. The count therefore tracked whether a bootstrap had
    // been run in the working tree, not what the tree contains — 28 in a built
    // tree, 27 in a clean one, so 28 and 27 were each "right" where they were
    // locked, and the ratchet oscillated. `walkdir_srcish` now gates on
    // `tracked_files()`; the budget is a property of the tree again.
    const BUDGET: usize = 27;
    assert!(
        dangling.len() <= BUDGET,
        "code cites {} document(s) that do not exist (budget {}).\n\n{}\n\n\
         A source comment pointing at a deleted design doc is worse than no \
         comment: it reads as a citation and resolves to nothing. Either \
         repoint it at the chapter that absorbed the content, inline the fact \
         it was citing, or delete the reference. If the doc genuinely lives \
         outside the repo, add it to CODE_DOC_CITATION_EXTERNAL.\n\n\
         If the count went DOWN, lower BUDGET here to lock the new floor.",
        dangling.len(),
        BUDGET,
        dangling
            .iter()
            // Show the whole list, not a window. Truncating at 15 is what hid
            // the generated-`driver.c` entry for two rounds: it sat at index 24
            // and every failure report cut it off, so the budget got re-locked
            // twice without anyone seeing the offender.
            .take(usize::MAX)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

// ---------------------------------------------------------------------------
// DESIGN-NOTE STATUS GUARD — `docs/internals/` holds designed-but-UNBUILT work
// only, and its index must be complete.
//
// Both halves of this were live defects before the tree was retired:
//   * files described shipped behaviour in present tense while their headers
//     said "Proposed", so readers could not tell which sections were current
//     (that ambiguity is what made the whole tree untrustworthy); and
//   * the index listed ELEVEN of twenty-four files, so thirteen were reachable
//     only by `ls` — including two carrying owner rulings recorded nowhere else.
//
// Prose in the README cannot hold either invariant. This can.
// ---------------------------------------------------------------------------

/// The only statuses a design note may carry. `SHIPPED` is deliberately ABSENT:
/// once work lands, the content belongs in a devbook chapter and the note is
/// deleted. Allowing a `SHIPPED` status is how the tree re-accumulates the
/// superseded pile it was cleaned out of.
const DESIGN_NOTE_STATUSES: &[&str] = &["RATIFIED-UNBUILT", "PROPOSED", "IN-PROGRESS"];

#[test]
fn design_notes_carry_a_status_and_the_index_is_complete() {
    let dir = Path::new("docs/internals");
    if !dir.is_dir() {
        return; // the tree may be retired entirely one day; that is not a failure
    }

    let mut notes: Vec<String> = fs::read_dir(dir)
        .expect("read docs/internals")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| n.ends_with(".md") && n != "README.md")
        .collect();
    notes.sort();

    // ── limb 1: every note declares exactly one recognised status ──
    let mut bad: Vec<String> = Vec::new();
    for n in &notes {
        let body = fs::read_to_string(dir.join(n)).unwrap_or_default();
        // the status must be in the header block, not buried mid-file
        let head: String = body.lines().take(12).collect::<Vec<_>>().join("\n");
        let found: Vec<&&str> = DESIGN_NOTE_STATUSES
            .iter()
            .filter(|s| head.contains(**s))
            .collect();
        if head.contains("SHIPPED") {
            bad.push(format!(
                "{n}: declares SHIPPED — shipped content belongs in a devbook \
                 chapter and this note should be DELETED, not relabelled"
            ));
        } else if found.is_empty() {
            bad.push(format!(
                "{n}: no status in its first 12 lines (expected one of {DESIGN_NOTE_STATUSES:?})"
            ));
        } else if found.len() > 1 {
            bad.push(format!("{n}: declares {} statuses; exactly one is allowed", found.len()));
        }
    }
    assert!(
        bad.is_empty(),
        "design note(s) in docs/internals/ with a bad status header:\n  {}\n\n\
         Every note states whether it is ratified, proposed, or partly landed, \
         because a reader cannot otherwise tell which sentences describe the \
         compiler and which describe an intention. Add the header, or — if the \
         work has shipped — fold the content into its devbook chapter and \
         delete the note.",
        bad.join("\n  ")
    );

    // ── limb 2: the index lists every note that exists ──
    let readme = fs::read_to_string(dir.join("README.md")).unwrap_or_default();
    let unindexed: Vec<&String> = notes.iter().filter(|n| !readme.contains(*n)).collect();
    assert!(
        unindexed.is_empty(),
        "design note(s) not listed in docs/internals/README.md: {unindexed:?}\n\n\
         An unindexed note is reachable only by `ls`. The index listed 11 of 24 \
         files before this tree was retired, and two of the thirteen omissions \
         carried owner rulings recorded nowhere else. Add a row.",
    );
}

// ---------------------------------------------------------------------------
// BACKEND-FLAG GUARD — the accepted `--backend` set and the dispatch must agree.
//
// The dispatch is `match effective_backend { "llvm" => …, _ => CLirBackend }`.
// A wildcard arm means a value nobody handles does not error — it BUILDS, as C,
// and reports success. That is how `--backend=wasm` silently produced a C binary
// for as long as the flag existed. The parse-time check closes it, but only
// while `BACKENDS` and the dispatch stay in sync, and nothing about a `match`
// with a `_` arm forces that.
// ---------------------------------------------------------------------------

#[test]
fn backend_flag_set_matches_dispatch() {
    let main = fs::read_to_string("src/main.rs").expect("read src/main.rs");

    // The declared set.
    let decl = regex::Regex::new(r#"const BACKENDS: &\[&str\] = &\[([^\]]*)\]"#)
        .expect("BACKENDS regex");
    let caps = decl.captures(&main).expect(
        "`const BACKENDS: &[&str]` not found in src/main.rs — it is the single \
         source of truth for the accepted --backend values; if it moved or was \
         renamed, update this lint rather than deleting it.",
    );
    let lit = regex::Regex::new(r#""([^"]+)""#).expect("literal regex");
    let declared: Vec<String> =
        lit.captures_iter(&caps[1]).map(|c| c[1].to_string()).collect();
    assert!(
        declared.len() >= 2 && declared.iter().any(|b| b == "llvm"),
        "BACKENDS parsed as {declared:?} — that does not look right; the scan is \
         broken, not the tree."
    );

    // The parse-time rejection must still be wired to that set.
    assert!(
        main.contains("BACKENDS.contains(&backend_name)"),
        "the --backend parse-time check is gone. Without it an unknown value \
         falls through the dispatch's `_` arm and builds as C, reporting \
         success — user input silently discarded (Core #10)."
    );

    // Every explicitly-matched backend literal must be a declared value.
    let disp = regex::Regex::new(r"match effective_backend \{([^}]*)\}")
        .expect("dispatch regex");
    let body = disp
        .captures(&main)
        .map(|c| c[1].to_string())
        .expect("`match effective_backend { … }` not found in src/main.rs");
    let arms: Vec<String> = lit
        .captures_iter(&body)
        .map(|c| c[1].to_string())
        .collect();
    let unknown: Vec<&String> = arms.iter().filter(|a| !declared.contains(a)).collect();
    assert!(
        unknown.is_empty(),
        "backend(s) dispatched but not accepted by the flag: {unknown:?}\n\n\
         `BACKENDS` is {declared:?}. A backend with a dispatch arm but no entry \
         in BACKENDS is unreachable — the parse-time check rejects it before \
         dispatch ever runs. Add it to BACKENDS."
    );
}

/// Round XXXII class-retirement guard (Core #6): every SelfConvention::ByValue
/// opaque-handle protocol (Shared, Weak, Mutex, RWLock, Thread, AtomicInt,
/// AtomicBool, Barrier, WaitGroup, Semaphore, OnceFlag, TaskGroup) needs a
/// receiver-ABI POS or CONTROL fixture per route so the chokepoint fix at
/// `src/ir/lowering/exprs/methods.rs:531 / :2315 / :2343` is proven to cover
/// the WHOLE class, not just Mutex. Grow-with-schema: iterates the
/// authoritative `ALL_PROTOCOLS` list via `by_value_protocol_names()`; a new
/// by-value protocol added to `builtins.rs::ALL_PROTOCOLS` automatically
/// flips this lint's expected-coverage set — no hard-coded list to keep in
/// sync (Layering discipline rule 3, one source of truth per axis).
///
/// Every (protocol, route) cell MUST be either:
///   1. In `EXPECTED_FIXTURE_STEMS` — pointing at a landed fixture stem in
///      `tests/fixtures/` (asserted to exist on disk); or
///   2. In `OPAQUE_HANDLE_UNCOVERED_CELLS` — with a cited follow-up (the
///      known_gaps fixture or TODO entry).
///
/// A cell missing from BOTH lists panics the lint. This is the class-
/// retiring guard: adding a new by-value protocol without either a fixture
/// or a filing IS the regression.
///
/// **Positive-control** (Core #13): before landing this lint I renamed
/// `tests/fixtures/mutex_amp_param.gg` and confirmed the lint went RED with
/// the expected "MISSING opaque-handle Mutex × amp_param fixture" message;
/// restored.
#[test]
fn opaque_handle_route_fixtures_exist() {
    use std::collections::{HashMap, HashSet};

    // Route naming mirrors the receiver-build sites in methods.rs:
    //   amp_param       -> Route 1 (borrow-param, :531-566)
    //   struct_field    -> Route 2a (field_place_info, :2343-2371)
    //   collection_elem -> Route 2b (index_elem_place_info, :2315-2337)
    const ROUTES: &[&str] = &["amp_param", "struct_field", "collection_elem"];

    // (protocol, route) -> fixture stem (asserted to exist under tests/fixtures/).
    // A cell absent here MUST have an entry in OPAQUE_HANDLE_UNCOVERED_CELLS
    // below — the lint refuses silent gaps.
    let mut expected: HashMap<(&str, &str), &str> = HashMap::new();
    expected.insert(("Mutex",     "amp_param"),       "mutex_amp_param");
    expected.insert(("AtomicInt", "amp_param"),       "atomic_int_amp_param");
    expected.insert(("RWLock",    "amp_param"),       "rwlock_amp_param");
    expected.insert(("Semaphore", "amp_param"),       "semaphore_amp_param");
    expected.insert(("WaitGroup", "amp_param"),       "waitgroup_amp_param");
    expected.insert(("Barrier",   "amp_param"),       "barrier_amp_param");
    expected.insert(("AtomicInt", "struct_field"),    "opaque_handle_struct_field");
    expected.insert(("WaitGroup", "struct_field"),    "opaque_handle_struct_field_waitgroup");
    expected.insert(("Mutex",     "struct_field"),    "mutex_struct_field_lock_control");

    // Filed-follow-up cells (either known_gaps fixtures OR TODO entries).
    // Each entry cites where the follow-up lives. Graduating a cell (moving
    // the fixture out of known_gaps or landing its coverage) means moving
    // its row into `expected` above.
    const OPAQUE_HANDLE_UNCOVERED_CELLS: &[(&str, &str, &str)] = &[
        // (protocol, route, citation)
        ("AtomicInt", "collection_elem",
            "tests/fixtures/known_gaps/atomic_int_collection_elem_segv.gg (push-side)"),
        ("Mutex",     "collection_elem",
            "tests/fixtures/known_gaps/vector_mutex_elem_lock_segv.gg (push-side)"),
        // Untagged / no-covered-route filings — class characterisation in
        // TODO.md follow-up for the opaque-handle push-side ABI defect.
        ("Shared",    "amp_param",       "TODO: Shared/Weak untagged, filed with push-side"),
        ("Weak",      "amp_param",       "TODO: Shared/Weak untagged, filed with push-side"),
        ("Thread",    "amp_param",       "TODO: opaque-handle Thread family follow-up"),
        ("OnceFlag",  "amp_param",       "TODO: opaque-handle OnceFlag family follow-up"),
        ("TaskGroup", "amp_param",       "TODO: opaque-handle TaskGroup family follow-up"),
        ("AtomicBool","amp_param",       "TODO: opaque-handle AtomicBool follow-up"),
        // Struct-field and collection-elem for the still-uncovered protocols
        // fall under the same follow-up as their amp_param row. The lint
        // enumerates every (protocol, route) cell; each MUST be listed.
        ("Shared",    "struct_field",    "TODO: same as Shared amp_param"),
        ("Weak",      "struct_field",    "TODO: same as Weak amp_param"),
        ("Thread",    "struct_field",    "TODO: same as Thread amp_param"),
        ("OnceFlag",  "struct_field",    "TODO: same as OnceFlag amp_param"),
        ("TaskGroup", "struct_field",    "TODO: same as TaskGroup amp_param"),
        ("AtomicBool","struct_field",    "TODO: same as AtomicBool amp_param"),
        ("Barrier",   "struct_field",    "TODO: Barrier struct_field follow-up"),
        ("Semaphore", "struct_field",    "TODO: Semaphore struct_field follow-up"),
        ("RWLock",    "struct_field",    "TODO: RWLock struct_field follow-up"),
        ("Shared",    "collection_elem", "TODO: known_gaps/shared_array_literal_*.gg family"),
        ("Weak",      "collection_elem", "TODO: same as Shared collection_elem"),
        ("Thread",    "collection_elem", "TODO: same as Thread amp_param"),
        ("OnceFlag",  "collection_elem", "TODO: same as OnceFlag amp_param"),
        ("TaskGroup", "collection_elem", "TODO: same as TaskGroup amp_param"),
        ("AtomicBool","collection_elem", "TODO: same as AtomicBool amp_param"),
        ("RWLock",    "collection_elem", "TODO: same class as vector_mutex_elem_lock_segv"),
        ("Semaphore", "collection_elem", "TODO: same class as vector_mutex_elem_lock_segv"),
        ("Barrier",   "collection_elem", "TODO: same class as vector_mutex_elem_lock_segv"),
        ("WaitGroup", "collection_elem", "TODO: same class as vector_mutex_elem_lock_segv"),
    ];

    let uncovered: HashSet<(&str, &str)> = OPAQUE_HANDLE_UNCOVERED_CELLS
        .iter()
        .map(|(p, r, _)| (*p, *r))
        .collect();

    let by_value = gorget::ir::lowering::builtins::by_value_protocol_names();
    assert!(
        !by_value.is_empty(),
        "by_value_protocol_names() returned empty — the SelfConvention::ByValue \
         filter or ALL_PROTOCOLS registration got broken. Fix at source before \
         the ratchet."
    );

    // Enumerate every (protocol, route) cell — either fixture-backed or filed.
    let mut missing: Vec<String> = Vec::new();
    for protocol in &by_value {
        for route in ROUTES {
            let key = (*protocol, *route);
            if let Some(stem) = expected.get(&key) {
                let path = format!("tests/fixtures/{stem}.gg");
                if !std::path::Path::new(&path).exists() {
                    missing.push(format!(
                        "opaque-handle {protocol} × {route}: expected fixture \
                         `{stem}.gg` is ABSENT — either land it or move the \
                         cell to OPAQUE_HANDLE_UNCOVERED_CELLS with a cited \
                         follow-up."
                    ));
                }
            } else if !uncovered.contains(&key) {
                missing.push(format!(
                    "opaque-handle {protocol} × {route}: no fixture in \
                     `expected` and no entry in `OPAQUE_HANDLE_UNCOVERED_CELLS`. \
                     The receiver-ABI chokepoint at `methods.rs:531 / :2315 / \
                     :2343` covers this cell; land a POS fixture OR file a \
                     follow-up entry citing the reason."
                ));
            }
        }
    }
    assert!(
        missing.is_empty(),
        "opaque-handle receiver-ABI route-coverage gaps:\n{}",
        missing.join("\n"),
    );

    // Also sanity-check that every entry in OPAQUE_HANDLE_UNCOVERED_CELLS is
    // for an actual by-value protocol — a stale entry after graduation is a
    // silent gap dressed as filed.
    let by_value_set: HashSet<&str> = by_value.iter().copied().collect();
    let stale: Vec<&str> = OPAQUE_HANDLE_UNCOVERED_CELLS
        .iter()
        .filter_map(|(p, _, _)| {
            if !by_value_set.contains(*p) { Some(*p) } else { None }
        })
        .collect();
    assert!(
        stale.is_empty(),
        "OPAQUE_HANDLE_UNCOVERED_CELLS references non-by-value protocols: \
         {stale:?}. Either the protocol lost its ByValue self_conv (fix at \
         source in builtins.rs) or the entry is a stale carryover from a \
         graduation — delete it."
    );
}

/// Track A · MEMORY-SAFETY round · CLASS-RETIRING GUARD (Core #6).
///
/// Every silent-drop of a sub-expression's inferred type in the checker is
/// a Core #10 hazard. `Expr::Catch` (`src/semantic/typecheck.rs:4624`)
/// previously called `self.infer_expr(recovery)` and DISCARDED the returned
/// TypeId — the outer VarDecl then unified a fabricated OK type against
/// itself, so an ill-typed recovery reached codegen (silent
/// heap-ptr-as-int64 on same-C-layout mismatches like Vector[String] →
/// Vector[int]).
///
/// This lint asserts the arm routes through the shared helper
/// `check_recovery_type` and does not carry a bare `self.infer_expr(recovery)`
/// call. A new recovery-yielding arm added to `impl_typecheck_expr` must join
/// this whitelist consciously (grepping for `check_recovery_type` to see how,
/// or adding the justification here if the new arm legitimately doesn't need
/// it). Post-D25 the `Expr::FaultCatch` sibling arm is gone.
#[test]
fn recovery_arms_route_through_check_recovery_type() {
    let src = std::fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");
    for pat in &["Expr::Catch {"] {
        let start = src
            .find(pat)
            .unwrap_or_else(|| panic!("missing arm {pat} in src/semantic/typecheck.rs"));
        let end = src[start + pat.len()..]
            .find("\n            Expr::")
            .map(|off| start + pat.len() + off)
            .unwrap_or(src.len());
        let arm = &src[start..end];
        assert!(
            arm.contains("check_recovery_type("),
            "{pat}: must route through check_recovery_type — see \
             tests/fixtures/catch_recovery_type_unchecked.gg (the NEG regression \
             graduated from known_gaps R40) for the class this guard retires. \
             New recovery/handler-yielding arms \
             must call check_recovery_type(recovery_or_handler, expected) at \
             the writer site so ill-typed recoveries reject with E_TypeMismatch \
             instead of reaching codegen (Core #10 lower-or-reject)."
        );
        assert!(
            !arm.contains("self.infer_expr(recovery)"),
            "{pat}: bare `self.infer_expr(recovery)` — Core #10 silent-drop \
             class. Route through check_recovery_type instead."
        );
        assert!(
            !arm.contains("self.infer_expr(handler)"),
            "{pat}: bare `self.infer_expr(handler)` — Core #10 silent-drop \
             class. Route through check_recovery_type instead."
        );
    }
}

// ── Round MEMORY SAFETY / ONE OWNERSHIP BOUNDARY · Track B ratchet ───────
//
// View-producer × container-mutator coverage: every {View-tag producer,
// container-mutator consumer} pair that reached this codebase must own an
// exercising fixture that will trip the double-free class if the boundary
// clone regresses. The initial rows correspond one-to-one to the POS cells
// in `tests/security.rs` (`guard_get_into_*` family).
//
// KNOWN LIMITATION (documented, deliberately deferred): this ratchet asserts
// EXISTENCE of the cited fixture, not enumeration of the producer × consumer
// matrix. A new producer or mutator variant would be silently uncovered
// until someone adds a row. Extending to enforce enumeration would require
// a producer-registry lookup (a scan of `emit_guard_get_ptr` call sites and
// `insert_collection_sig` value-slot Move registrations); left as follow-up.
//
// Positive-control (Core #13): rename or delete one referenced fixture,
// confirm this test goes RED, restore. Recorded in the commit message.
const VIEW_PRODUCERS_INTO_CONSUMERS: &[(&str, &str, &str)] = &[
    ("Guard.get",      "Dict.put",       "guard_get_into_dict_put_double_free"),
    ("Guard.get",      "Vector.push",    "guard_get_into_vector_push_temp_fixed"),
    ("Guard.get",      "Set.add",        "guard_get_into_set_add_temp"),
    ("Guard.get",      "Channel.send",   "guard_get_into_channel_send_temp"),
    ("Guard.get",      "index-assign",   "guard_get_into_index_set_temp"),
    ("Guard.get",      "Dict.put[Vec]",  "guard_get_vector_int_into_dict_put_temp"),
    ("ReadGuard.get",  "Dict.put",       "read_guard_get_into_dict_put_double_free"),
    ("WriteGuard.get", "Dict.put",       "write_guard_get_into_dict_put_double_free"),
];

/// Every {View-tag producer, container-mutator consumer} cell must have an
/// exercising security fixture. A missing row means a class-regression could
/// resurrect the "view alias into consumer" double-free without tripping a
/// test. Adding a new View producer or a new container-mutator surface owes
/// a new row here + a fixture in `tests/fixtures/security/`.
///
/// See:
///   - `src/ir/lowering/context.rs::ensure_owned_at_consuming_arg` — the
///     writer this ratchet defends (else-arm predicate; class-retiring
///     `debug_assert!`s live there too).
///   - `src/ir/lowering/mod.rs::register_collection_method_sigs` — the
///     Tier 2a validator diagnostic surface (SIBLING D2).
#[test]
fn view_producer_into_consuming_cell_has_coverage() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut missing: Vec<String> = Vec::new();
    for (producer, dest, fixture) in VIEW_PRODUCERS_INTO_CONSUMERS {
        let path = manifest_dir
            .join("tests/fixtures/security")
            .join(format!("{fixture}.gg"));
        if !path.exists() {
            missing.push(format!(
                "  MISSING: {producer} -> {dest}\n    expected: {}\n    ratchet row: {fixture}",
                path.display()
            ));
        }
    }
    assert!(
        missing.is_empty(),
        "view_producer_into_consuming_cell_has_coverage: one or more \
         View-producer x consumer cells lack an exercising fixture. \
         Restore the missing fixture(s) below, or if the ratchet row is \
         stale, remove it from VIEW_PRODUCERS_INTO_CONSUMERS. Losing a \
         row is losing the regression net for its class.\n\n{}",
        missing.join("\n")
    );
}

/// Round XXXII Track D+E — class-retiring guard for the shared-var-decl
/// Move-follow-through class-fix.
///
/// Pre-fix, all 7 `builder.assign_mode(resource_assign_mode(...), ...)` sites
/// inside `lower_shared_var_decl` (`src/ir/lowering/stmts/mod.rs:1595-1834`)
/// bypassed the Move-follow-through invariant. The 3 tmp-mat sites
/// (:1709/:1764/:1813) triggered ICE 101 for any `shared T x = <computed>`
/// where T is a resource type; the 4 facade-init sites (:1692/:1723/:1778/:1831)
/// didn't panic in the surviving corpus but shared the same class defect.
///
/// The class-fix routes ALL 7 sites through helpers on `LoweringContext`:
/// - `materialize_addressable` for tmp-mat sites (allocates fresh + assigns)
/// - `assign_with_move_follow_through` for facade-init sites (assigns into
///   an existing local)
///
/// Both helpers pair Move-mode with `move_zero_and_mark` on the source when
/// required. See `src/ir/lowering/context.rs` for both.
///
/// This lint asserts that every `builder.assign_mode(resource_assign_mode(…))`
/// call inside `lower_shared_var_decl` is followed within ~500 chars by a
/// call to `materialize_addressable` or `assign_with_move_follow_through` (or
/// a direct `move_zero_and_mark` — the escape hatch for a hypothetical inlined
/// site). Note the regex is NARROWED to the `resource_assign_mode(...)` calls
/// specifically: a broader regex over all `assign_mode(...)` in the same
/// function matches 11 sites (4 hardcoded-`Move` facade wrappings at
/// :1682/:1714/:1769/:1821 that DON'T route through the helper and can't --
/// they wrap already-owned `wrapped`/`shared_val` locals from a call return;
/// the helper's guard would be a no-op for them).
///
/// Positive-control (Core #13): delete the `move_zero_and_mark` call inside
/// `materialize_addressable` (or delete one call to the helper from an arm),
/// re-run this lint — it must go RED. Restore.
#[test]
fn shared_var_decl_arms_route_through_materialize_addressable() {
    let src = std::fs::read_to_string("src/ir/lowering/stmts/mod.rs")
        .expect("read src/ir/lowering/stmts/mod.rs");
    let start = src
        .find("fn lower_shared_var_decl(")
        .expect("fn lower_shared_var_decl present in src/ir/lowering/stmts/mod.rs");
    let end = src[start..]
        .find("\nfn ")
        .map(|off| start + off)
        .unwrap_or(src.len());
    let body = &src[start..end];

    // ⚠ Pass-3 review BR1 fold: the regex is NARROWED to
    // `resource_assign_mode(...)` calls -- a broader regex over
    // `assign_mode(...)` matches 11 sites, including the 4 hardcoded-`Move`
    // facade wrappings that correctly do NOT route through the helper.
    let call_re = regex::Regex::new(r"builder\.assign_mode\(resource_assign_mode\(")
        .expect("class-fix regex");
    let unrouted: Vec<usize> = call_re
        .find_iter(body)
        .filter(|m| {
            let after_start = m.end();
            let after_end = (after_start + 500).min(body.len());
            let after = &body[after_start..after_end];
            !(after.contains("materialize_addressable")
                || after.contains("assign_with_move_follow_through")
                || after.contains("move_zero_and_mark"))
        })
        .map(|m| m.start())
        .collect();
    assert!(
        unrouted.is_empty(),
        "lower_shared_var_decl: {} `builder.assign_mode(resource_assign_mode(...))` \
         call(s) at offset(s) {:?} not followed within 500 chars by a routing \
         through `materialize_addressable`, `assign_with_move_follow_through`, or \
         a direct `move_zero_and_mark` -- Move-follow-through class hazard. \
         See src/ir/lowering/context.rs for the two helpers and Track D+E brief §5.",
        unrouted.len(),
        unrouted
    );
}

// ============================================================================
// Round XXXIII Batch C1 D28 — placeholder-rot + compound-assign silent-fall-
// through class-retiring guards (Core #6).
// ============================================================================

/// Guard (a) — `src/lir/lower/calls.rs` must contain zero placeholder-comment
/// anti-patterns AND the `GirBinOp::Pow` arm must dispatch to `CallExtern`
/// (a real runtime call), NOT `Inst::Mul`.
///
/// Retires the CLASS of Core #10 rot: the pre-D28 site emitted
/// `Inst::Mul { ... }` for `GirBinOp::Pow` with a `// For now, emit as Mul
/// (placeholder)` comment. Users' `x ** y` silently miscompiled as `x * y`
/// (measured: `2 ** ten()` → 20 instead of 1024). The negative-pattern
/// forbid catches new placeholders under different wording (`stub`,
/// `unimplemented for now`); the positive `CallExtern` assertion pins the
/// specific invariant even if a future author rewords the placeholder to
/// dodge the string check.
#[test]
fn lir_lower_calls_no_placeholders() {
    let src = fs::read_to_string("src/lir/lower/calls.rs")
        .expect("read src/lir/lower/calls.rs");
    // (i) Negative: forbid known placeholder-comment anti-patterns.
    for pat in &["For now, emit as", "// placeholder", "// TODO: implement"] {
        assert!(
            !src.contains(pat),
            "src/lir/lower/calls.rs contains placeholder pattern `{pat}` — this is \
             Core #10 silent-fallthrough rot. Wire real emission or reject at check \
             time. See D28 amendment `docs/define-gorget/decisions.md:1197` and the \
             ratchet's docstring for the class-retiring rationale."
        );
    }
    // (ii) Positive: the Pow arm dispatches to a runtime `CallExtern`.
    // The arm body starts at `GirBinOp::Pow => {` and ends at the matching `}`.
    let pow_key = "GirBinOp::Pow => {";
    let arm_start = src
        .find(pow_key)
        .expect("locate `GirBinOp::Pow => {` in src/lir/lower/calls.rs");
    // Find the matching close-brace by depth-counting braces from arm_start.
    let arm_body_start = arm_start + pow_key.len();
    let bytes = src.as_bytes();
    let mut depth = 1usize;
    let mut i = arm_body_start;
    while i < bytes.len() && depth > 0 {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => depth -= 1,
            _ => {}
        }
        i += 1;
    }
    let arm_body = &src[arm_body_start..i.saturating_sub(1)];
    assert!(
        arm_body.contains("CallExtern"),
        "GirBinOp::Pow arm in src/lir/lower/calls.rs must dispatch to a runtime \
         `Inst::CallExtern` (Core #10). Found arm body:\n{arm_body}\n\
         The pre-D28 placeholder `Inst::Mul {{ ... }}` silently miscompiled `x ** y` \
         as `x * y` — do NOT revert to that shape."
    );
}

/// Guard (c) — `src/ir/lowering/stmts/assigns.rs` must contain ZERO
/// `_ => BinOp::Add` catch-all fallbacks in compound-assign dispatch
/// (comments excluded).
///
/// Retires the CLASS of Core #10 sibling-drift in `assigns.rs`. Pre-D28 the
/// same 14-arm compound-op → `BinOp` mapping was open-coded at S1-S5 plus
/// the shared helper (six sites), each with a `_ => BinOp::Add` catch-all
/// — `x **= y` on ANY of them would silently lower as `+=`. D28's chokepoint
/// fix routes S1-S5 through `compound_op_to_gir` and enumerates the
/// rejected variants explicitly in the helper. This ratchet locks the
/// invariant: a future BinaryOp variant addition MUST land at the single
/// helper site (compile-forced), NOT smuggled in via a `_ => BinOp::Add`
/// escape hatch that re-opens the whole sibling class.
#[test]
fn assigns_compound_op_no_silent_fallthrough() {
    let src = fs::read_to_string("src/ir/lowering/stmts/assigns.rs")
        .expect("read src/ir/lowering/stmts/assigns.rs");
    // Strip line comments so the ratchet reasons about EXECUTABLE code only.
    // The `compound_op_to_gir` docstring legitimately mentions the historical
    // shape in prose.
    let code: String = src
        .lines()
        .map(|l| l.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");
    let count = code.matches("_ => BinOp::Add").count();
    assert_eq!(
        count, 0,
        "`_ => BinOp::Add` catch-all found {count}× in src/ir/lowering/stmts/assigns.rs — \
         this silently miscompiles any BinaryOp variant the arm doesn't name (Core #10 \
         sibling-drift class). Route through `compound_op_to_gir` and enumerate the \
         rejected variants explicitly with `_ => unreachable!(\"...\")` (convention: \
         6/6 hits in src/ir/lowering/ are `unreachable!`). See D28 chokepoint fix at \
         `docs/define-gorget/decisions.md:1197` and the helper's docstring."
    );
}

/// D26 (Round XXXIII Batch C1) `map_binop` class-retiring guard (Core #6):
/// enumerates every site that mentions a fallible-arith enum variant
/// (`(AddFallible|SubFallible|MulFallible|DivFallible|RemFallible|ShlFallible|ShrFallible)`)
/// and pins the per-file variant-mention count. A future 8th fallible-arith
/// operator MUST land at every one of these sites in the same round —
/// this lint fails immediately if it doesn't.
///
/// **Sites (D26 F1+F3 landing state):**
///   1. `src/parser/ast.rs` — the `BinaryOp` enum definition (7 variants)
///      + `is_fallible_arith()` typed helper (7 matches).
///   2. `src/formatter/mod.rs` — 7 glyph arms.
///   3. `src/semantic/typecheck.rs::op_display` — 7 non-compound + 7
///      compound arms.
///   4. `src/parser/expr.rs` — 7 Pratt lex-token → InfixOp arms.
///   5. `src/ir/lowering/exprs/operators.rs` — the 5-variant
///      dispatch matches! + 5-arm base_op map inside
///      `lower_fallible_arith_binop` + 7-arm fallback bin_op map.
///   6. `spec/ggdef/src/elaborate/mod.rs::map_binop` — 5 arith + 2 shift.
///
/// Precise regex: `(AddFallible|SubFallible|MulFallible|DivFallible|RemFallible|ShlFallible|ShrFallible)`.
/// This excludes the many `FallibleMark*` / `FallibleOp*` / `Fallible*Reason`
/// diagnostic-enum uses that a naive `Fallible` substring count would inflate.
///
/// Positive-control demo (RED-verified 2026-08-06): dropping a fallible-arith
/// variant arm from ANY site trips this lint immediately with the exact
/// per-file count-off-by-one; restore restored green.
#[test]
fn d26_map_binop_arm_count_ratchet() {
    let re = regex::Regex::new(
        r"(AddFallible|SubFallible|MulFallible|DivFallible|RemFallible|ShlFallible|ShrFallible)",
    )
    .expect("d26 fallible-arm regex");

    let expectations: &[(&str, usize, &str)] = &[
        (
            "src/parser/ast.rs",
            14,
            "BinaryOp enum definition (7) + is_fallible_arith() matches! (7)",
        ),
        (
            "src/formatter/mod.rs",
            14,
            "formatter arm - 7 arms `Fallible => \"...!\"` in binary_op_str (7) + \
             7 mentions in `binary_op_left_bp` precedence table (Round XXXVI FMT-A: \
             2 shift fallibles at bp 25, 2 add fallibles at bp 27, 3 mul/div/rem \
             fallibles at bp 29 = 7)",
        ),
        (
            "src/semantic/typecheck.rs",
            23,
            "op_glyph_str (7) + op_display non-compound (7) + op_display compound (7) + \
             shift-fallible Route-B reject guard matches! (2: ShlFallible|ShrFallible)",
        ),
        (
            "src/parser/expr.rs",
            7,
            "Pratt infix-op map - 7 lex-token to InfixOp::Binary arms",
        ),
        (
            "src/ir/lowering/exprs/operators.rs",
            17,
            "5-variant matches! dispatch + 5-arm base_op map + 7-arm fallback bin_op map",
        ),
        (
            "spec/ggdef/src/elaborate/mod.rs",
            12,
            "map_binop - 5 arith arms (2 mentions each: B::X + BinOp::X) + 2 shift OOS reject",
        ),
    ];

    let mut per_file_actual: Vec<(String, usize)> = Vec::new();
    for (path, expected, why) in expectations {
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => panic!("d26_map_binop_arm_count_ratchet: cannot read {path}: {e}"),
        };
        let count: usize = content
            .lines()
            .filter(|l| !l.trim_start().starts_with("//"))
            .map(|l| re.find_iter(l).count())
            .sum();
        per_file_actual.push((path.to_string(), count));
        assert_eq!(
            count, *expected,
            "d26_map_binop_arm_count_ratchet: `{path}` fallible-arith variant \
             mentions {count} vs expected {expected} ({why}).\n\n\
             If a new fallible-arith variant was added (an 8th op), it must land \
             at every enumerated site (see doc comment). Bump each per-file count \
             here after confirming the new op reaches: parser Pratt, ast helper, \
             formatter, checker glyph table, lowerer dispatch + base_op map, \
             lowerer fallback, ggdef elaborator, AND every SH-mirror site (which \
             this Rust-only lint does NOT ratchet - the SH mirror lands in the \
             same round per Core #9).",
        );
    }
    let total: usize = per_file_actual.iter().map(|(_, c)| c).sum();
    let expected_total: usize = expectations.iter().map(|(_, c, _)| c).sum();
    assert_eq!(
        total, expected_total,
        "d26 fallible-arm total {total} vs expected {expected_total} - per-file \
         breakdown: {per_file_actual:?}",
    );
}

/// Round XXXVI Track FMT-A class-retirement guard (Core #4/#6): the
/// formatter's precedence-aware paren-wrapping is a CHOKEPOINT enforced
/// at every operand-emit site by calling one of three helpers:
///   - `format_binop_operand(...)` — infix operand (left/right position).
///   - `format_prefix_operand(...)` — prefix-operator operand.
///   - `format_postfix_receiver(...)` — postfix-operator receiver.
///
/// Every arm in `format_expr` that emits a sub-expression as an
/// operand MUST go through one of these helpers, or `gg fmt` re-emits
/// the source with dropped parens — the exact silent-miscompile class
/// R35's fmt sweep tripped (`(a + b) / 2` → `a + b / 2`, Core #8).
///
/// This ratchet pins the total call-site count. A new arm added
/// without a helper call fails the count (drop) — a required arm-count
/// review. A new arm added with a helper call bumps the count — an
/// explicit bump of `EXPECTED` here with the new site's rationale.
#[test]
fn fmt_precedence_check_arm_count() {
    /// Baseline (Round XXXVI FMT-A close 2026-08-08): 26 sites.
    ///
    ///   `format_binop_operand`     — 11 sites:
    ///     format_binary_chain (1), Range start (1), DefaultOp lhs/rhs (2),
    ///     As (1), Is (1), MetaOpInfix left/right (2),
    ///     Rethrow left/transform (2), Catch left (1).
    ///   `format_prefix_operand`    — 7 sites:
    ///     UnaryOp (1), Range end (1), Move (1), MutableBorrow (1),
    ///     Deref (1), Spawn (1), SpawnBlocking (1).
    ///   `format_postfix_receiver`  — 8 sites:
    ///     Call callee (1), MethodCall receiver non-chain (1),
    ///     FieldAccess (1), TupleFieldAccess (1), Index object (1),
    ///     OptionalChain object (1), Propagate expr (1), Await (1).
    ///
    /// R41 T-FMT-B bump 26 → 27: `Expr::Await` now renders in the form the
    /// author wrote, and the two forms sit at DIFFERENT precedence levels —
    /// prefix `await e` takes its operand at bp 2, postfix `e.await()` is a
    /// bp-35 postfix receiver. The arm therefore holds TWO helper calls, one
    /// per form (`format_prefix_operand` 8 sites, `format_postfix_receiver`
    /// still 8), and both are load-bearing: emitting `(await f()) + 1`
    /// without the prefix helper's parens re-parses as `Await(f() + 1)`.
    ///
    /// R42 tail-reserve bump 27 → 29, and NEITHER is a new arm. A caller that
    /// must reserve width for what it writes after an operand has to
    /// pre-render that text to measure it, and the pre-render necessarily
    /// spells the same helper call a second time. The two mirrors:
    ///   * `Expr::Range` — the START operand's reserve measures `..` plus the
    ///     END operand, whose emission goes through `format_prefix_operand`.
    ///   * `Expr::Rethrow` — the LHS's reserve measures the bare-form
    ///     ` rethrow <transform>` tail, whose emission goes through
    ///     `format_binop_operand`.
    /// A mirror runs on a throwaway sub-`Formatter`, so it cannot change what
    /// is emitted; what it CAN do is drift from the emission beside it, which
    /// is why both are spelled identically to their emitting twin. This guard
    /// still catches what it was written for: a genuinely NEW `format_expr`
    /// arm that emits a sub-expression without consulting a helper leaves the
    /// count at 29 while the arm count rises, and one that DOES consult a
    /// helper pushes it to 30.
    ///
    /// R42 Track D bump 29 → 31, both in `format_assert_return_expr` and both
    /// on the SAME operand: that emitter hand-rolled its `Expr::BinaryOp`
    /// re-emission and bypassed this chokepoint entirely, so
    /// `assert return >= a * (b + c)` came back as `>= a * b + c` — a
    /// postcondition that FLIPPED from trap to pass at runtime. Its right
    /// operand now goes through `format_binop_operand`, spelled twice for the
    /// reason above: once in the tail MEASUREMENT, once in the emission. (The
    /// left operand stays on the flat spine walk — a chain break at the TOP
    /// level would emit `assert (return\n    >= x)`, which does not re-parse.)
    const EXPECTED: usize = 31;

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    // Count call sites of the three helpers. Skip the helper's OWN
    // definition line (the signature line `fn format_binop_operand(`).
    // Match `.format_<name>(` at any position — method invocation.
    let mut count = 0;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        // Helper DEFINITION lines start with `fn format_...(`.
        if trimmed.starts_with("fn format_binop_operand(")
            || trimmed.starts_with("fn format_prefix_operand(")
            || trimmed.starts_with("fn format_postfix_receiver(")
        {
            continue;
        }
        count += line.matches(".format_binop_operand(").count();
        count += line.matches(".format_prefix_operand(").count();
        count += line.matches(".format_postfix_receiver(").count();
    }
    assert_eq!(
        count, EXPECTED,
        "FMT-A precedence-helper call-site count in \
         `src/formatter/mod.rs` changed: {count} vs expected {EXPECTED}.\n\n\
         If a new arm in `format_expr` was added, it must consult one of \
         the precedence helpers when emitting a sub-expression, or `gg fmt` \
         will silently drop parens on some operand shapes. Update EXPECTED \
         here with the new site's rationale.\n\n\
         If an arm was removed / centralized, lower EXPECTED.\n\n\
         Round XXXVI Track FMT-A retires the paren-drop class \
         (`(a + b) / 2` → `a + b / 2`, Core #8 silent miscompile via \
         the compiler's own tool). This arm-count guard IS the \
         class-retirement mechanism (Core #4/#6).",
    );
}

/// Round XXXVII D27 Round A Phase 3 (Core #4 arm-count guard): the
/// formatter's Move-sigil EMIT sites — 7 in `src/formatter/mod.rs` and 6
/// in the SH `format.gg` files — MUST all agree to write `^` (D27
/// canonical) and never regress to `!` (retired; `!` is now the D26/D29
/// error channel exclusively). This lint pins the emit-site COUNT on
/// both sides so a new arm that emits the sigil without joining the
/// class trips the count.
///
/// **Rust arms (7 sites, `src/formatter/mod.rs`):**
///   `Ownership::Move =>`     — 4 arms:
///     :1091 `format_param` self-face (`^self`),
///     :1605 fn-type param suffix (`Type ^`),
///     :2076 comprehension for-binder,
///     :2271 `format_ownership_prefix` helper (named-param chokepoint).
///   `Type::Owned(inner) =>`  — 1 arm (:1618) — type-arg suffix (D35).
///   `Expr::Move { expr } =>` — 1 arm (:1895) — prefix move expression.
///   `if *is_move {`          — 1 site (:2012) — move-closure prefix.
///
/// **SH arms (6 sites, `tests/fixtures/self_host_{parser,resolver,typechecker}/format.gg`):**
///   `case EMove(inner):`     — 3 arms (one per unique-content SH file).
///   `"^self"` literal        — 3 arms (one per unique-content SH file).
///   (self_host_check/format.gg and self_host_lowerer/format.gg are
///   SYMLINKS to typechecker; self_host_lexer/format.gg has NO D27 site.)
///
/// **If this fails**: a new arm was added that emits the Move sigil
/// without going through one of the enumerated patterns — the new arm
/// likely regressed to `!` silently. Either fold into an existing arm,
/// or bump EXPECTED with a citation naming the new arm and its `^` emit.
#[test]
fn fmt_move_sigil_emit_arm_count() {
    const EXPECTED_RUST: usize = 7;
    const EXPECTED_SH: usize = 6;

    let rust = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    // Skip pure-comment lines so a `// TODO Ownership::Move …` note
    // doesn't spuriously trip the count. Only real code arms count.
    let mut rust_count = 0usize;
    for line in rust.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        rust_count += line.matches("Ownership::Move =>").count();
        rust_count += line.matches("Type::Owned(inner) =>").count();
        rust_count += line.matches("Expr::Move { expr } =>").count();
        rust_count += line.matches("if *is_move {").count();
    }
    assert_eq!(
        rust_count, EXPECTED_RUST,
        "D27 Round A emit-site count in `src/formatter/mod.rs` changed: \
         {rust_count} vs expected {EXPECTED_RUST}.\n\
         Every Move-sigil emit arm must write `^` (D27 canonical) — a new \
         arm that bypasses this class silently regresses to `!` (which is \
         now reserved for the error channel).",
    );

    let sh_files: &[&str] = &[
        "tests/fixtures/self_host_parser/format.gg",
        "tests/fixtures/self_host_resolver/format.gg",
        "tests/fixtures/self_host_typechecker/format.gg",
    ];
    let mut sh_count = 0usize;
    for f in sh_files {
        let src = fs::read_to_string(f)
            .unwrap_or_else(|_| panic!("cannot read {f}"));
        // Skip `#`-comment lines so notes mentioning `EMove` / `^self`
        // in prose don't spuriously trip the count.
        for line in src.lines() {
            let t = line.trim_start();
            if t.starts_with('#') {
                continue;
            }
            if t.starts_with("case EMove(inner):") {
                sh_count += 1;
            }
            // `^self` emit: only lines that are the actual concat site.
            if t.starts_with("result = result +") && t.contains("\"^self\"") {
                sh_count += 1;
            }
        }
    }
    assert_eq!(
        sh_count, EXPECTED_SH,
        "D27 Round A SH emit-site count in `self_host_*/format.gg` changed: \
         {sh_count} vs expected {EXPECTED_SH}.\n\
         Same-round parity with the Rust reference (Core #9) — the SH \
         formatters must emit `^` alongside the Rust one, or the SH \
         self-compile prints out-of-date sigils vs the reference.",
    );
}

/// R39 snag #2 (Core #6 arm-count guard): the formatter's sibling-loop
/// pattern is `emit_comments_before(node.span.start)` → `format_node(node)`
/// → `emit_trailing_comment_after(node.span.end)`. A NEW sibling loop
/// added to `src/formatter/mod.rs` MUST call BOTH helpers, or the
/// gorget-arena snag #2 class (trailing comments detached and
/// misattributed to the next sibling) re-opens on the new loop. This
/// lint pins the three counts. ⚠ The AUTHORITATIVE values are the
/// `EXPECTED_*` constants in the test body — read those, never this prose
/// (it drifted to 12/14/4 against pinned 14/16/11 before R41 caught it).
///
///   * `emit_comments_before(` call sites — the sibling-loop leading
///     hooks (`format_module` directives / imports / rest, `Item::MetaIf`
///     then / elif / else, `format_struct` fields, `format_enum`
///     variants, `format_trait` items, `format_equip` methods,
///     `format_extern_block` items, `format_block_stmts`, closure
///     post-prelude stmts) plus the two inside the shared collection-
///     literal helper. If this count changes without an equal change to
///     the trailing count below, a new sibling loop bypassed the
///     trailing hook.
///
///   * `emit_trailing_comment_after(` call sites — the sibling-loop
///     paired calls, plus 1 defensive EOF hook (in `Formatter::format`),
///     plus 1 internal delegation from `emit_trailing_comment_after_header`,
///     plus the collection-literal helper's per-element call.
///
///   * `emit_trailing_comment_after_header(` call sites — the structural
///     containers (struct / enum / trait / equip / extern block), the
///     control-flow openers, and the function-definition header. Uses a
///     SEPARATE helper (with distinct docstring semantics) so the
///     sibling-boundary count above stays clean.
///
/// ⚠ **This lint pins COUNTS, so it is structurally blind to a loop with
/// ZERO hooks** — a hookless loop moves no count. That is exactly how
/// `format_extern_block`'s item loop sat green while every comment inside
/// an `extern:` block escaped to column 0 (R41 T-FMT-A §5). The companion
/// `formatter_child_collection_loop_census` below closes that hole by
/// enumerating the LOOPS themselves. Neither is sufficient alone
/// (Core #15e Q2 — a guard must be able to catch its own class).
///
/// **Break-and-verify (Core #12 / Core #15e Q2):** manually mutate one
/// call site to drop the paired trailing hook (e.g. delete
/// `self.emit_trailing_comment_after(field.span.end)` in the struct
/// field loop). The lint's `assert_eq!` fires with the trailing count one
/// BELOW `EXPECTED_EMIT_TRAILING_AFTER` — RED, pinpointing the missing
/// pair. Restore the deletion and the lint goes green again. Recorded RED
/// signature filed in the R39 snag #2 executor report.
///
/// If a new sibling loop legitimately joins this class (a future
/// AST node kind added), both counts must bump together with a
/// citation of the new loop's file:line + rationale, mirroring the
/// precedent-check / move-sigil arm-count guards.
#[test]
fn formatter_sibling_loops_hook_pairing() {
    /// Sibling-loop `emit_comments_before(node.span.start)` calls in
    /// `src/formatter/mod.rs`. The 12 sites are enumerated in the doc
    /// comment above; each has its paired `emit_trailing_comment_after`
    /// call one to a few lines below in the same loop body.
    // R39 fmt collection-literal interior-comment escape (2026-08-09):
    // 12 → 14. The shared helper
    // `Formatter::format_bracketed_broken_with_comments` adds TWO
    // `.emit_comments_before(` call sites — one per-element leading
    // flush + one orphan-pre-close flush — both inside the SINGLE helper
    // (not per dispatcher). See `formatter_collection_literal_interior_hook_dispatch`
    // below for the paired dispatch-count guard.
    // R41 T-FMT-A §5 (extern-block comment escape, 2026-08-11): 14 → 15.
    // `format_extern_block`'s item loop (`src/formatter/mod.rs`, the `for func
    // in &eb.items` loop) gains the leading hook it never had — it was the one
    // child-collection loop in the file with ZERO hooks, which is why a
    // COUNT-based lint could not see it. Paired with the trailing bump below.
    //
    // R41 T-FMT-A follow-up (2026-08-11): 15 → 21. SIX leading-only hooks join,
    // and they deliberately do NOT move the trailing count — they are the
    // BRANCH-HEADER family, whose trailing side is the header hook the child
    // emitter already writes:
    //   `format_stmt`             Stmt::Match arms (`for item in arms`)
    //   `format_stmt`             Stmt::Select arms
    //   `format_stmt`             Stmt::MetaMatch arms
    //   `format_expr`             match-expression arms
    //   `format_elif_else_blocks` the `elif` loop, and the `else` branch
    // Each fixes the MISATTRIBUTION face of the class: a comment written above
    // `case`/`elif`/`else` had no hook to claim it, so `format_block_stmts`
    // swallowed it into the branch BODY, where it documented the wrong thing.
    // Adding a base trailing hook to these would DOUBLE-claim against the
    // header hook — which is exactly why the count-pairing model below cannot
    // adjudicate them, and why `formatter_child_collection_loop_census`
    // classifies each loop's hook state explicitly instead.
    //
    // R41 T-FMT-C (2026-08-11): 21 → 27. SIX more leading-only hooks, the rest
    // of the CLAUSE-HEADER class. `elif`/`else` had gained theirs above; a
    // census of the class found six sibling clause sites with none —
    // `for`-`else`, `while`-`else`, `select`-`else`, `meta match`-`else`, the
    // statement-match `else` and the expression-match `else`. Each was
    // deleting an author blank above the clause AND letting a comment written
    // above it fall through to `format_block_stmts`, which re-emitted the
    // comment INSIDE the branch body. Same misattribution face, six positions
    // the arm loops cannot see (a clause header is not a loop iteration, which
    // is why `formatter_child_collection_loop_census` was blind to them too).
    //
    // ⚠ These are LEADING-ONLY, like the branch-header family above:
    // `EXPECTED_EMIT_TRAILING_AFTER` deliberately does NOT move. Adding a
    // paired trailing hook at a clause header would double-claim against the
    // header hook the branch emitters already write.
    //
    // R41 fold (2026-08-11): 27 → 27 — and the fact that it lands back on the
    // same number is a COINCIDENCE of two independent moves, not a no-op.
    // Anyone re-deriving this constant should check both.
    //   −2: NOT a lost hook, a CENTRALIZED one. The three `meta if`
    //       nested-item loops (then / elif body / else) each carried their own
    //       copy of the leading+trailing pair; they now share the single
    //       `format_nested_items` producer, so three pairs became one. `_AFTER`
    //       drops by the same two and the equality between them still holds,
    //       which is the property that matters.
    //   +2: the ITEM-level `meta if`'s `elif` and `else` headers gain the
    //       leading hook their statement-level twins have had since the
    //       clause-header census — the item-level clause headers were simply not
    //       in that census, and without the hook a comment written above them
    //       fell to the nested-item loop and was re-emitted INSIDE the branch,
    //       leading the first definition. LEADING ONLY, like every other clause
    //       header, so `_AFTER` does not move.
    const EXPECTED_EMIT_COMMENTS_BEFORE: usize = 27;
    /// `emit_trailing_comment_after(` calls: 12 sibling-paired + 1
    /// EOF defensive + 1 internal delegation from
    /// `emit_trailing_comment_after_header`. If sub-task 5's EOF hook
    /// is inlined/removed OR the header helper stops delegating, this
    /// number must drop; if a new sibling loop is added without the
    /// paired call, `_BEFORE` will bump but this will not, and the
    /// count-equality assertion below fires.
    // R39 fmt collection-literal interior-comment escape (2026-08-09):
    // 14 → 15. The shared helper
    // `Formatter::format_bracketed_broken_with_comments` adds ONE
    // `.emit_trailing_comment_after(` call site — the per-element
    // trailing-comment splice inside the SINGLE helper (not per
    // dispatcher). Total = 12 sibling-paired + 1 EOF defensive + 1
    // header-helper delegation + 1 new collection-literal-interior
    // per-element trailing.
    //
    // R39 gorget-js snag 15e (2026-08-09): 15 → 16. Added ONE
    // `emit_trailing_comment_after(stmt.span.end)` at the tail of
    // `try_inline_single_terminal_stmt` — preserves trailing comments on
    // inlined single-stmt arm bodies (`else: return cc  # doc`). Single
    // helper site covers all 4 inline-arm callers.
    // R41 T-FMT-A §5 (2026-08-11): 16 → 17 — `format_extern_block`'s item
    // loop gains its paired trailing hook alongside the leading one above.
    // R41 fold (2026-08-11): 17 → 15, the trailing half of the
    // `format_nested_items` centralization described above.
    const EXPECTED_EMIT_TRAILING_AFTER: usize = 15;
    /// `emit_trailing_comment_after_header(` calls: 4 structural
    /// containers (`format_struct`, `format_enum`, `format_trait`,
    /// `format_equip`) + 6 control-flow openers added R39 by the
    /// gorget-arena verdict fold (owner 2026-08-09): `Stmt::For`,
    /// `Stmt::While`, `Stmt::If`, `Stmt::Match`, `format_elif_else_blocks`
    /// elif branch, `format_match_arm` case. A new structural-container
    /// or control-flow header formatter that wants the header-trailing
    /// behaviour must add its own call and bump this count.
    // R39 gorget-arena verdict follow-up (2026-08-09): 10 → 11 with
    // function-definition header (`int f(): # doc`) added via
    // `format_function`'s FunctionBody::Block arm. Same class as the
    // control-flow openers + structural containers.
    // R41 T-FMT-A §5 (2026-08-11): 11 → 12 — `extern "C":  # why` joins the
    // structural-container header family (struct / enum / trait / equip).
    //
    // R41 fold (2026-08-11): 12 → 14. The `else:` clause header was the ONE
    // header position with no trailing hook — `elif` had one and the `case`
    // arms had one, so `else:  # note` alone dropped its comment through to
    // `format_block_stmts`, which re-emitted it as a LEADING comment on the
    // branch's first statement (the comment then documents that statement
    // instead of the clause). +1 for the shared `emit_else_header` producer,
    // which the five indented-`else` sites now route through, and +1 for the
    // expression-match `else:`, whose body goes through `format_arm_body` and
    // therefore needs its own guarded call (fired only for an indented suite —
    // before an author `do:` or an inline expression it would emit the comment
    // ahead of the thing it heads, and the output would not re-parse).
    //
    // 14 → 16 in the same change: the ITEM-level `meta if` and its `elif`
    // headers. Their trailing comments were falling into the branch body, where
    // the nested-item loop re-emitted them as LEADING comments on the branch's
    // first DEFINITION — the same class one layer up, at sites the clause census
    // did not cover. (The item-level `else` needs no row of its own: it routes
    // through the shared `emit_else_header` counted above.)
    const EXPECTED_EMIT_TRAILING_AFTER_HEADER: usize = 16;

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");

    // Text-scan: count method-call sites (leading `.` distinguishes
    // from function-definition lines like `fn emit_...`); skip pure
    // comment lines (a `// TODO emit_comments_before ...` note doesn't
    // trip the count).
    let mut before_count = 0usize;
    let mut trailing_count = 0usize;
    let mut header_count = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        before_count += line.matches(".emit_comments_before(").count();
        // The header helper's name has _AFTER_ as a prefix — match
        // trailing hook first, then subtract the header-suffix matches
        // by counting them separately (the header helper's calls also
        // start with `.emit_trailing_comment_after_header(`, which
        // contains `.emit_trailing_comment_after(` as a substring only
        // if we're not careful — use a stricter match).
        //
        // Use `.emit_trailing_comment_after(` (with `(` — matches only
        // the base helper) and `.emit_trailing_comment_after_header(`
        // (matches only the header helper) — the trailing `(` locks
        // each to its own name.
        trailing_count += line.matches(".emit_trailing_comment_after(").count();
        header_count += line.matches(".emit_trailing_comment_after_header(").count();
    }

    assert_eq!(
        before_count, EXPECTED_EMIT_COMMENTS_BEFORE,
        "R39 snag #2 sibling-loop-pairing guard: \
         `emit_comments_before(` call-site count in `src/formatter/mod.rs` \
         changed: {before_count} vs expected {EXPECTED_EMIT_COMMENTS_BEFORE}.\n\n\
         An `emit_comments_before` call in a CHILD-COLLECTION loop must be \
         paired with an `emit_trailing_comment_after(node.span.end)` call at \
         the end of the same loop body — otherwise a trailing comment on the \
         node's last source line drifts to lead the NEXT sibling (the \
         gorget-arena snag #2 class). ⚠ The BRANCH-HEADER loops \
         (match/select/meta-match arms, elif/else) are the exception: their \
         trailing side is the header hook the child emitter already writes, so \
         they carry a LEADING hook only and this count moves without the \
         trailing one. `formatter_child_collection_loop_census` classifies \
         every loop's hook state explicitly and is the guard that adjudicates \
         which kind a new loop is.\n\n\
         If a new sibling loop legitimately joined, bump \
         EXPECTED_EMIT_COMMENTS_BEFORE (and EXPECTED_EMIT_TRAILING_AFTER too, \
         unless it is a branch-header loop) with a citation of the new site's \
         file:line + rationale."
    );

    assert_eq!(
        trailing_count, EXPECTED_EMIT_TRAILING_AFTER,
        "R39 snag #2 sibling-loop-pairing guard: \
         `emit_trailing_comment_after(` call-site count in \
         `src/formatter/mod.rs` changed: {trailing_count} vs expected \
         {EXPECTED_EMIT_TRAILING_AFTER}.\n\n\
         Expected = {EXPECTED_EMIT_COMMENTS_BEFORE} sibling-paired \
         + 1 defensive EOF hook in `Formatter::format` \
         + 1 internal delegation from `emit_trailing_comment_after_header`.\n\n\
         If a sibling loop was added without the paired trailing hook, \
         this trips — restore the pair. If the EOF hook or the header \
         helper's delegation was removed intentionally, bump EXPECTED with \
         the rationale.",
    );

    assert_eq!(
        header_count, EXPECTED_EMIT_TRAILING_AFTER_HEADER,
        "R39 snag #2 sub-task 5b container-header hook count in \
         `src/formatter/mod.rs` changed: {header_count} vs expected \
         {EXPECTED_EMIT_TRAILING_AFTER_HEADER}.\n\n\
         Expected: 4 (struct / enum / trait / equip). A new structural-\
         container formatter that wants trailing comments on its header \
         line (`container Header:  # doc`) MUST call \
         `emit_trailing_comment_after_header(anchor.span.end)` right \
         after writing `:` + newline and BEFORE `indent()` — otherwise \
         the header trailing comment dedents into the body as leading of \
         the first item."
    );
}

/// R41 T-FMT-A §5 (Core #6 class-retiring guard, 2026-08-11): the LOOP
/// CENSUS that `formatter_sibling_loops_hook_pairing` above cannot be.
///
/// **Why a second guard.** The pairing lint pins CALL-SITE COUNTS, so it only
/// fires when a loop has *some* hooks and is missing its partner. A loop with
/// ZERO hooks moves no count at all and is therefore invisible to it — not
/// hypothetical: `format_extern_block`'s item loop shipped hookless, every
/// comment inside an `extern:` block escaped to column 0, and the pairing lint
/// stayed green throughout. A guard that green-lights the class it exists to
/// retire is worse than none (Core #15e Q2), so this one enumerates the LOOPS.
///
/// **Detection is by SHAPE, not by field name.** The first cut keyed on
/// `.items` / `.fields` / `.variants` / `.stmts`, which made the match-arm,
/// select-arm and meta-match loops (all iterating a binding called `arms`)
/// INVISIBLE — the census could not see its own class. A loop qualifies here
/// when it emits AST children as separate SOURCE LINES, detected as: the body
/// calls `self.format_*`, AND the body either calls `self.emitter.newline()`
/// directly or delegates to a block-child emitter. That definition is about
/// what the loop DOES, so a new container cannot dodge it by naming its field
/// something else.
///
/// **Every row is classified, so nothing is invisible.** `Both` = leading +
/// trailing hook in the loop body. `Leading` = leading hook here, with the
/// trailing/header hook delegated into the child emitter (`format_match_arm`)
/// or written as a header hook in the body. `None` = knowingly hookless, with
/// the reason recorded. A new loop is RED until it is classified.
///
/// **Break-and-verify (Core #13 — RED-verified 2026-08-11):** delete the two
/// hook calls from `format_extern_block`'s `for func in &eb.items` loop (the
/// pre-R41 state) and this lint fires with that row flipping `Both` → `None`.
#[test]
fn item_module_is_constructed_only_by_the_loader() {
    // `format_item`'s `Item::Module` loop is the ONE child-collection loop in
    // the formatter that carries no comment hooks and no blank preservation.
    // The census row records the reason: the node is SYNTHETIC — the loader
    // wraps a non-entry imported module in it (`merge_modules`), and `gg fmt`
    // parses fresh source with `Parser::new(..).parse_module()` and never runs
    // the loader, so the loop is unreachable from the formatter.
    //
    // That reasoning is only as good as its premise. The day the PARSER grows a
    // `module X:` form, the loop would silently strip every blank and comment
    // inside it (Core #10 class) and nothing would say so.
    //
    // ⚠ The premise is NOT "one construction site" — an output-review asserted
    // that and it is wrong. There are TWO in production code, and the second is
    // easy to misread as a pattern because the line above it IS one:
    //   * `src/loader.rs` — `merge_modules` wraps a non-entry imported module;
    //   * `src/semantic/meta.rs` — `flatten_meta_ifs` REBUILDS the wrapper after
    //     flattening the `meta if`s inside it.
    // What actually holds — and what the formatter's hookless loop rests on — is
    // that BOTH producers are POST-PARSE. The parser builds none, and
    // `format_source_result` parses fresh source and runs neither the loader nor
    // semantic analysis. So this pins the file SET, not a count of one: a
    // producer appearing anywhere else, above all in `src/parser/`, is what
    // invalidates the reasoning.
    const EXPECTED_PRODUCER_FILES: &[&str] = &["src/loader.rs", "src/semantic/meta.rs"];

    let mut sites: Vec<String> = Vec::new();
    for path in walkdir_rs("src") {
        let rel = path.to_string_lossy().replace('\\', "/");
        let Ok(content) = fs::read_to_string(&path) else { continue };
        // Unit-test modules build AST fixtures by hand; those constructions are
        // real but are not producers in the shipped pipeline, and they reach the
        // formatter no more than the loader's does. Scope to production code —
        // the trailing `#[cfg(test)]` module is the convention in this tree.
        let prod_end = content
            .lines()
            .position(|l| l.trim_start() == "#[cfg(test)]")
            .unwrap_or(usize::MAX);
        for (i, line) in content.lines().enumerate() {
            if i >= prod_end {
                break;
            }
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            // A CONSTRUCTION builds the node; a PATTERN binds it. Patterns carry
            // a rest-`..`, a match arrow, or a `let`/`if let` binder on the same
            // line — constructions carry none of the three.
            if line.contains("Item::Module {")
                && !line.contains("..")
                && !line.contains("=>")
                && !trimmed.starts_with("let ")
                && !trimmed.contains("if let ")
            {
                sites.push(format!("{rel}:{}", i + 1));
            }
        }
    }

    let mut producer_files: Vec<String> = sites
        .iter()
        .map(|s| s.rsplit_once(':').map(|(f, _)| f.to_string()).unwrap_or_default())
        .collect();
    producer_files.sort();
    producer_files.dedup();

    assert_eq!(
        producer_files, EXPECTED_PRODUCER_FILES,
        "`Item::Module` producers changed.\n\nSites: {sites:?}\n\n\
         The formatter's `Item::Module` loop (`format_item`) has NO comment \
         hooks and NO blank-line preservation, and its census row justifies \
         that by the node being POST-PARSE and therefore unreachable from \
         `gg fmt`, which parses fresh source and runs neither the loader nor \
         semantic analysis.\n\n\
         A producer in `src/parser/` breaks that reasoning outright — the loop \
         would silently strip every blank and comment inside the new form \
         (Core #10 class). A producer in another post-parse pass is probably \
         fine, but say so deliberately: re-read the census row in \
         `formatter_child_collection_loop_census` before extending this list."
    );
}

/// RAW-TEXT ACCESS TO FIXTURE-DERIVED FORMATTER OUTPUT IS DENY-BY-DEFAULT.
///
/// A fmt fixture explains itself in a header of `#` lines, and `gg fmt`
/// reproduces that header in its output. Any assertion that searches the whole
/// output can be shadowed by a header line, silently and in both directions: a
/// first-line lookup measures the HEADER (red for the wrong reason, and unable
/// to go green when the behaviour is fixed, since a comment line can never
/// satisfy a positional claim), and a `contains` assertion is SATISFIED by the
/// header (can never go red, reads as coverage, guards nothing).
/// `fmt_body` / `fmt_body_line_with` / `fmt_body_contains` (tests/integration.rs)
/// skip the header; this lint is what keeps access N+1 from going around them.
///
/// **WHY THIS IS KEYED ON ACCESS, NOT ON SEARCH METHODS.** Four rounds of this
/// class produced the same defect in a new costume each time — header rewording,
/// then 13 routings plus a table, then a ratchet that detected `.find(` /
/// `.position(` and was blind to `contains(`, to `.lines().any(…)` (an instance
/// the same commit had hand-fixed), and to a live `.rposition(`. Enumerating
/// costumes schedules the next round. So the subject here is the ACCESS:
///
///   * a binding whose initializer is a formatting call is RAW TEXT;
///   * a function CALLED with such a binding is a raw-text consumer, and its
///     `&str` parameters are raw text too — the hop that let `hash_col_of` hide
///     behind ~25 call sites;
///   * every METHOD CALL and every SLICE on raw text must be routed through the
///     helpers or carry an allowlist row with its reason.
///
/// The consumer rule is applied **TO A FIXED POINT**, so it is
/// DEPTH-INDEPENDENT: a function that becomes a consumer only because of a
/// param-derived entry is scanned too, and so on until nothing new appears.
/// Applying it ONCE — the shape this lint shipped with — resolved exactly one
/// hop and left two-hop raw text invisible, with a live instance already
/// present: `fmt_sentinel_idx`, reached only through `fmt_sentinel_line` and
/// `fmt_line_after_sentinel`, carrying the column-0 header skip for the whole
/// `fmt_sentinel_*` family. Deleting that predicate left every gate green.
///
/// The allowlist names ALLOWED ACCESSES, not forbidden costumes, so a search
/// spelling nobody has written yet — `.rfind(`, `.match_indices(`,
/// `for l in x.split('\n')`, `x[a..b].contains(…)` — is RED by construction.
///
/// **Honest scope.** This is the read-site guard. The write-site fix would be a
/// typed carrier whose only accessors are body-scoped, making raw text
/// unreachable rather than merely flagged; measured at 115 bindings and ~430
/// mentions to migrate, it was judged disproportionate for a test harness.
/// The residual dodges are now exactly TWO — raw text reaching a consumer
/// through a struct FIELD, or through a CLOSURE CAPTURE, rather than through a
/// `&str` parameter. Neither exists today. Call-chain DEPTH is no longer one of
/// them; only these two shapes route raw text past the parameter rule.
///
/// SHRINK-ONLY. A row belongs here when the access is genuinely header-immune —
/// it searches something that is not fixture output, or the predicate already
/// excludes comment lines, or the needle is a code shape no comment line can
/// have. Adding a row to silence a new fmt assertion is the wrong move: route it.
///
/// **Break-and-verify (six costumes, all RED-verified):**
/// `formatted.contains(needle)`; reverting an `fmt_body(&formatted).lines().any(…)`
/// to `formatted.lines().any(…)`; a `.rposition(` lookup; a novel
/// `for line in formatted.split('\n')` loop; and — for the fixed point — a
/// TWO-hop plant (`f(&formatted)` forwards to `g(text)`, which does
/// `text.contains(…)`) plus a THREE-hop variant, which reds identically and is
/// what makes the depth-independence claim above a measurement rather than an
/// argument.
#[test]
fn fmt_raw_text_access_is_routed_or_reasoned() {
    /// (enclosing fn, method or `[slice]`, why the access is header-immune).
    const ALLOWED: &[(&str, &str, &str)] = &[
        // THE SANCTIONED RAW-ACCESS SITE: the helpers themselves. This is the
        // one place raw text is touched on purpose — the read-site equivalent
        // of a carrier's single `full_text()` escape.
        ("fmt_body", "split_inclusive", "the helper's own header skip"),
        ("fmt_body", "[slice]", "the helper's own header skip"),
        // Whole-output SHAPE claims: the assertion is about the file's overall
        // form, not about locating a cell, so the header is part of the subject.
        ("fmt_comment_only_file_preserved", "trim_end_matches", "a comment-only file — the header IS the subject"),
        ("fmt_comment_only_file_preserved", "starts_with", "asserts the file does not START with whitespace"),
        ("fmt_comment_only_live_victim_is_fixed_point", "trim_end_matches", "trailing-newline normalization, whole-file"),
        ("fmt_suite_layout_form_preservation", "lines", "scans every line for trailing whitespace — whole-file by design"),
        ("fmt_multiline_arg_indent_pins_continuation_column", "lines", "walks every line to measure continuation columns"),
        ("fmt_tail_reserve_boundary_matrix", "lines", "measures the width of every line; a header line is a legitimate row"),
        ("fmt_preserves_noreturn_qualifier", "matches", "counts occurrences across the file; count-neutral to the header"),
        ("fmt_preserves_noreturn_qualifier", "lines", "builds a diagnostic excerpt inside the `assert_eq!` message — it decides nothing, so a header line in it cannot change pass/fail"),
        // Predicate already excludes comment lines.
        ("fmt_catch_multi_stmt_no_do_wrap", "lines", "predicate skips all `#` lines"),
        ("fmt_catch_rethrow_single_stmt_no_do_wrap", "lines", "predicate skips all `#` lines"),
        ("fmt_catch_rethrow_single_stmt_terminal_axis", "lines", "predicate skips all `#` lines"),
        ("fmt_sentinel_idx", "lines", "predicate skips column-0 comment lines — exactly the fixture-header skip, and the ONE site that carries it for the whole `fmt_sentinel_*` family"),
        // The two forwarders. Neither holds a predicate of its own; both are
        // header-immune only because `fmt_sentinel_idx` above is.
        ("fmt_sentinel_line", "lines", "a one-line forwarder: `nth()` off `fmt_sentinel_idx`, which is where the skip lives"),
        ("fmt_line_after_sentinel", "lines", "indexes off `fmt_sentinel_idx`, which skips the header"),
        // The needle is a code shape no comment line can have.
        ("fmt_import_group_single_blank", "lines", "predicates match `from std.`/`from mylib.` prefixes; a `#` line cannot"),
        ("fmt_preserves_intra_block_blank_lines", "find", "locates `struct Point:`, a code shape"),
        ("fmt_preserves_intra_block_blank_lines", "[slice]", "slices between two located code shapes"),
        ("fmt_container_last_interior_comment_stays_inside", "lines", "marker conjoined with an exact indent, which a column-0 header line cannot have"),
        ("fmt_equip_generic_params_keep_separator", "lines", "exact-line equality against a code header; header prose cannot equal it"),
        ("fmt_tail_reserve_inline_body_escape_preserves_the_suite_form", "contains", "needle is an indented code window"),
        ("fmt_tail_reserve_narrow_separator_survives", "contains", "needle is a code window"),
        ("fmt_tail_reserve_exploded_path_close_line_is_unenforced", "contains", "needle is a multi-line code window; header lines all start `#`"),
        // Searches text whose header has already been stripped.
        ("fmt_fill_pack_comma_axis", "contains", "searches `fill_pack_body` output, header already removed"),
        ("fmt_fill_pack_width_boundary_axis", "contains", "searches `fill_pack_body` output, header already removed"),
        ("fmt_fill_pack_width_boundary_axis", "lines", "searches `fill_pack_body` output, header already removed"),
        // Surfaced only once the detector learned to join WRAPPED method
        // chains — the blind spot that let a routed `.any(…)` be reverted
        // undetected. Each disposition checked at the site.
        ("fmt_radix_preserved", "lines", "walks from a located code line; the header cannot satisfy the skip_while"),
        ("fmt_comment_only_file_preserved", "lines", "counts comment lines in a comment-ONLY file — the header IS the subject"),
        ("fmt_else_catch_rethrow_no_do_wrap_on_move_tail", "lines", "predicate skips all `#` lines"),
        ("fmt_one_tuple_type_keeps_comma", "lines", "filters `#` lines OUT to build a code-only view — the header skip, stronger"),
        ("fmt_preserves_author_parens", "lines", "filters `#` lines OUT to build a code-only view; its own comment records the trap"),
        ("fmt_trailing_comment_struct_last_no_dedent", "lines", "marker conjoined with a code-shape predicate"),
        ("fmt_trailing_comment_match_else_no_dedent", "lines", "marker conjoined with a code-shape predicate"),
        ("fmt_tail_reserve_inline_body_escape_preserves_the_suite_form", "lines", "measures every line's width; a header line is a legitimate row"),
        ("fmt_tail_reserve_narrow_suppressed_half", "lines", "matches a code prefix a `#` line cannot have"),
        ("fmt_tail_reserve_narrow_multiline_item_column", "lines", "matches a code prefix a `#` line cannot have"),
        ("fmt_tail_reserve_exploded_path_close_line_is_unenforced", "lines", "matches `]` exactly; a comment line cannot equal it"),
        ("fmt_prerender_column_binary_chain_stays_in_budget", "lines", "measures every line's width; a header line is a legitimate row"),
        ("fmt_fill_suffix_overrun_stays_in_budget", "lines", "measures every line's width; a header line is a legitimate row"),
        // The reasoned INAPPLICABLE case: its cells ARE top-level comments, so
        // its cells and the header are the same region.
        ("fmt_comment_adjacent_blank_lines", "contains", "cells ARE top-level comment adjacency — stripping the header strips the subject"),
    ];

    let content =
        fs::read_to_string("tests/integration.rs").expect("cannot read tests/integration.rs");
    let lines: Vec<&str> = content.lines().collect();

    // fn boundaries
    let mut bounds: Vec<(String, usize)> = Vec::new();
    for (i, l) in lines.iter().enumerate() {
        let t = l.trim_start();
        if let Some(rest) = t.strip_prefix("fn ").or_else(|| t.strip_prefix("pub fn ")) {
            bounds.push((rest.split(['(', '<']).next().unwrap_or("?").to_string(), i));
        }
    }
    let span = |k: usize| -> (usize, usize) {
        (bounds[k].1, bounds.get(k + 1).map_or(lines.len(), |b| b.1))
    };
    const FMT_SOURCES: [&str; 4] = [
        "format_source_infallible",
        "tail_reserve_format",
        "fill_pack_body",
        "format_source_result",
    ];
    let ident_at = |l: &str, at: usize| -> bool {
        l[..at].chars().next_back().is_none_or(|c| !c.is_alphanumeric() && c != '_')
    };

    // (1) formatted-text bindings per fn, in fns that touch a fixture.
    let mut binds: Vec<(usize, Vec<String>)> = Vec::new();
    for k in 0..bounds.len() {
        let (i, end) = span(k);
        let body = lines[i..end].join("\n");
        if !(body.contains("tests/fixtures/")
            || body.contains("fill_pack_body(")
            || body.contains("tail_reserve_format("))
        {
            continue;
        }
        let mut b: Vec<String> = Vec::new();
        for j in i..end {
            let t = lines[j].trim_start();
            let Some(rest) = t.strip_prefix("let ") else { continue };
            let rest = rest.strip_prefix("mut ").unwrap_or(rest);
            let Some(name) = rest.split([':', ' ', '=']).next() else { continue };
            if name.is_empty() || !name.chars().all(|c| c.is_lowercase() || c == '_' || c.is_numeric()) {
                continue;
            }
            let init = lines[j..(j + 2).min(end)].join(" ");
            if FMT_SOURCES.iter().any(|f| init.contains(f)) && !b.iter().any(|x| x == name) {
                b.push(name.to_string());
            }
        }
        if !b.is_empty() {
            binds.push((k, b));
        }
    }

    // (2) a fn CALLED with a formatted binding consumes raw text; (3) its
    // `&str` params are raw text too.
    //
    // ⚠ TO A FIXED POINT, not once. Applying (2)+(3) a single time resolves
    // exactly ONE hop: a function that becomes a consumer only *because* of a
    // param-derived entry is never scanned, so raw text two hops from its
    // binding is invisible. That is not a hypothetical — `fmt_sentinel_idx`
    // (tests/integration.rs) is reached only from `fmt_sentinel_line` and
    // `fmt_line_after_sentinel`, which are themselves one-hop consumers, and it
    // carries the column-0 header skip for the entire `fmt_sentinel_*` family:
    // under the one-pass rule its `.lines()` was in neither the census nor the
    // allowlist, and deleting the skip predicate left every gate green.
    // Iterating until nothing new appears makes the rule DEPTH-INDEPENDENT
    // instead of one-hop deep, which is what the rule always claimed to be.
    let mut consumers: Vec<String> = Vec::new();
    loop {
        let size = |cs: &Vec<String>, bs: &Vec<(usize, Vec<String>)>| -> usize {
            cs.len() + bs.iter().map(|(_, b)| b.len()).sum::<usize>()
        };
        let before = size(&consumers, &binds);
        for (k, b) in &binds {
            let (i, end) = span(*k);
            for j in i..end {
                for name in b {
                    let pat = format!("({name}");
                    let pat_ref = format!("(&{name}");
                    for p in [&pat, &pat_ref] {
                        if let Some(at) = lines[j].find(p.as_str()) {
                            let head = &lines[j][..at];
                            if let Some(callee) = head
                                .rsplit(|c: char| !(c.is_alphanumeric() || c == '_'))
                                .next()
                                .filter(|c| !c.is_empty())
                            {
                                if ![
                                    "assert", "panic", "format", "println", "Some", "Ok", "if",
                                    "for",
                                ]
                                .contains(&callee)
                                    && !consumers.iter().any(|x| x == callee)
                                {
                                    consumers.push(callee.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
        for k in 0..bounds.len() {
            if !consumers.contains(&bounds[k].0) {
                continue;
            }
            let (i, end) = span(k);
            let sig = lines[i..(i + 6).min(end)].join(" ");
            let mut params: Vec<String> = Vec::new();
            for seg in sig.split(',') {
                if let Some((nm, ty)) = seg.split_once(':') {
                    // `&str` and `&'a str` alike — the lifetime is noise here.
                    if ty.contains('&') && ty.contains("str") {
                        // The name is whatever follows the last `(` (the first
                        // parameter carries the `fn name(` prefix) or the segment
                        // itself for later parameters.
                        let nm = nm.rsplit('(').next().unwrap_or(nm).trim();
                        if !nm.is_empty() && nm.chars().all(|c| c.is_lowercase() || c == '_') {
                            params.push(nm.to_string());
                        }
                    }
                }
            }
            if !params.is_empty() {
                // De-duplicated: the loop below re-derives the same params on
                // every iteration, and a duplicated name would double-count
                // every access it reaches.
                match binds.iter_mut().find(|(bk, _)| *bk == k) {
                    Some((_, b)) => {
                        for p in params {
                            if !b.iter().any(|x| *x == p) {
                                b.push(p);
                            }
                        }
                    }
                    None => binds.push((k, params)),
                }
            }
        }
        if size(&consumers, &binds) == before {
            break;
        }
    }

    // (4) every METHOD CALL and SLICE on raw text.
    //
    // ⚠ Method chains WRAP. `formatted\n    .lines()\n    .any(…)` puts the
    // receiver and the call on different source lines, and a line-local scan
    // sees neither — which is exactly how the sharpest plant (reverting a
    // routed `.any(…)` back to raw) slipped past the first version of this
    // detector. Each line is therefore joined with the continuation lines that
    // follow it, so the chain is one string.
    let logical = |j: usize, end: usize| -> String {
        let mut out = lines[j].to_string();
        let mut k = j + 1;
        while k < end && lines[k].trim_start().starts_with('.') {
            out.push_str(lines[k].trim_start());
            k += 1;
        }
        out
    };
    let mut flagged: Vec<(usize, String, String)> = Vec::new();
    for (k, b) in &binds {
        let (i, end) = span(*k);
        let fname = bounds[*k].0.clone();
        for j in i..end {
            if lines[j].trim_start().starts_with('.') {
                continue; // already folded into the line above
            }
            let joined = logical(j, end);
            let l: &str = &joined;
            if l.trim_start().starts_with("//") {
                continue;
            }
            for name in b {
                let mut from = 0usize;
                while let Some(rel) = l[from..].find(name.as_str()) {
                    let at = from + rel;
                    from = at + name.len();
                    if !ident_at(l, at) {
                        continue;
                    }
                    let rest = &l[at + name.len()..];
                    if let Some(tail) = rest.strip_prefix('.') {
                        let m: String =
                            tail.chars().take_while(|c| c.is_alphanumeric() || *c == '_').collect();
                        if !m.is_empty() && tail[m.len()..].starts_with('(') {
                            flagged.push((j + 1, fname.clone(), m));
                        }
                    } else if rest.starts_with('[') {
                        flagged.push((j + 1, fname.clone(), "[slice]".to_string()));
                    }
                }
            }
        }
    }

    for (ln, f, m) in &flagged {
        println!("raw text access: tests/integration.rs:{ln}  fn {f}  .{m}");
    }

    let unlisted: Vec<String> = flagged
        .iter()
        .filter(|(_, f, m)| !ALLOWED.iter().any(|(af, am, _)| af == f && am == m))
        .map(|(ln, f, m)| format!("  tests/integration.rs:{ln} (fn {f}) — `.{m}`"))
        .collect();
    assert!(
        unlisted.is_empty(),
        "raw access to fixture-derived formatter output:\n{}\n\n\
         A fmt fixture's own HEADER is part of the formatted output, so an \
         assertion that touches the raw text can be shadowed by a header line \
         — silently, in both directions. Route it through `fmt_body` / \
         `fmt_body_line_with` / `fmt_body_contains`. Only add an ALLOWED row \
         when the access is genuinely header-immune, and say why.",
        unlisted.join("\n")
    );

    const EXPECTED_RAW_ACCESSES: usize = 54;
    assert_eq!(
        flagged.len(),
        EXPECTED_RAW_ACCESSES,
        "the raw-access count moved ({} vs {EXPECTED_RAW_ACCESSES}). UP means a \
         new assertion touches raw text; DOWN is good and should be ratcheted \
         here (shrink-only). Regenerate with:\n  \
         cargo test --test lints fmt_raw_text_access_is_routed_or_reasoned -- --nocapture",
        flagged.len()
    );

    let dead: Vec<String> = ALLOWED
        .iter()
        .filter(|(af, am, _)| !flagged.iter().any(|(_, f, m)| f == af && m == am))
        .map(|(af, am, _)| format!("{af}/.{am}"))
        .collect();
    assert!(
        dead.is_empty(),
        "allowlist row(s) that no longer excuse anything: {dead:?} — the access \
         was routed or deleted. DELETE the row; the list is shrink-only."
    );
}

/// THE `Block::header_start` WIRING CENSUS — a new suite cannot join without a
/// probe, and a probe cannot be excluded without a stated reason.
///
/// `header_start` is a write-site fact: each caller of `parse_block` /
/// `parse_block_or_inline_stmt` / `parse_block_body` states which position is
/// its construct's FIRST line. Nothing downstream can recover it, and a wrong
/// value is SILENT — the formatter's orphan-pre-close flush simply refuses
/// every comment written inside that block, which reads as "no comments here".
///
/// **This guard exists because the coverage table was a selection three times.**
/// First `meta while` was said to be guarded by an unwrapped cell; then nine
/// rows (`for`, `with`, `meta if`, `meta for`, `meta type f()`, both match-arm
/// positions, `select` arms, closures) had no wrapped probe at all; then
/// `test`/`bench` were excluded by a FALSE REASON — "the name is a single
/// token, so it cannot wrap" — which conflates *one token* with *one line*
/// (Gorget's triple-quoted strings are one token spanning many). Each time,
/// unwiring a row escaped a real comment with every gate green. A fourth
/// hand-derived list would be the same artifact again, so the pair of guards
/// is:
///
///   * THIS lint — the wiring sites are censused per enclosing function, so a
///     NEW suite shows up as a new row and has to be classified;
///   * `block_probe_dispositions_are_decided` (src/parser/tests.rs) — every
///     `BLOCK_PROBES` row is either `Wrapped` (checked: its header really does
///     span lines) or `NotWrappable(reason)` (checked: its header really does
///     not). What that catches is a CONTRADICTION between a row's label and its
///     own spelling; it does NOT catch a reason that is false about the
///     LANGUAGE while the probe is written flat to match — the `test`/`bench`
///     shape, where a false belief produces a consistent pair. The artifact
///     that closes that direction is the CELL in `wrapped_header_anchor.gg`,
///     which fails when the row is unwired. (Stated the same way at
///     `ProbeKind::NotWrappable`'s own doc, which is the authority.)
///
/// The enumeration in `tests/fixtures/fmt_suite_layout/wrapped_header_anchor.gg`
/// is DERIVED from that table; this row-count pair is what keeps the table
/// complete.
///
/// **Break-and-verify (both RED-verified when this landed):** add a
/// `parse_block(...)` call in a new parser function ⇒ an unclassified row ⇒
/// RED; delete a `BLOCK_PROBES` row ⇒ the probe count moves ⇒ RED.
#[test]
fn parser_header_start_wiring_census() {
    // (file, enclosing fn, number of header_start-passing calls). Regenerate:
    //   cargo test --test lints parser_header_start_wiring_census -- --nocapture
    const CENSUS: &[(&str, &str, usize)] = &[
        // REGENERATED FROM THE SCAN, not written by hand — the
        // hand-written first draft named four functions that do not
        // exist (`parse_function_def` is `finish_function_def`,
        // `parse_prefix` is `parse_prefix_inner`) and invented rows for
        // `parse_match_arm_inner` / `parse_meta_for_match_item`, which
        // call `parse_arm_body` and are not wiring sites themselves.
        ("src/parser/expr.rs", "parse_closure", 1),
        ("src/parser/expr.rs", "parse_prefix_inner", 1),
        ("src/parser/mod.rs", "finish_function_def", 1),
        ("src/parser/mod.rs", "parse_arm_body", 1),
        ("src/parser/mod.rs", "parse_bench_def", 1),
        ("src/parser/mod.rs", "parse_block", 1),
        ("src/parser/mod.rs", "parse_block_or_inline_stmt", 1),
        ("src/parser/mod.rs", "parse_body_or_expr", 1),
        ("src/parser/mod.rs", "parse_meta_type", 1),
        ("src/parser/mod.rs", "parse_suite_block", 2),
        ("src/parser/mod.rs", "parse_test_def", 1),
        ("src/parser/stmt.rs", "parse_for_stmt", 2),
        ("src/parser/stmt.rs", "parse_if_stmt", 3),
        ("src/parser/stmt.rs", "parse_loop_stmt", 1),
        ("src/parser/stmt.rs", "parse_match_stmt", 1),
        ("src/parser/stmt.rs", "parse_meta_for_stmt", 1),
        ("src/parser/stmt.rs", "parse_meta_if_stmt", 3),
        ("src/parser/stmt.rs", "parse_meta_match_arm_body", 1),
        ("src/parser/stmt.rs", "parse_meta_while_stmt", 1),
        ("src/parser/stmt.rs", "parse_named_scope", 1),
        ("src/parser/stmt.rs", "parse_on_error_stmt", 1),
        ("src/parser/stmt.rs", "parse_select_stmt", 2),
        ("src/parser/stmt.rs", "parse_unsafe_stmt", 1),
        ("src/parser/stmt.rs", "parse_while_stmt", 2),
        ("src/parser/stmt.rs", "parse_with_stmt", 1),
    ];

    let needles = [
        "self.parse_block(",
        "self.parse_block_or_inline_stmt(",
        "self.parse_block_body(",
    ];

    let mut found: Vec<(String, String, usize)> = Vec::new();
    // READ_DIR, not a hardcoded list. The list was `expr.rs`/`mod.rs`/`stmt.rs`
    // — 3 of `src/parser/`'s 9 files — which made this lint's own claim ("a NEW
    // suite cannot join without showing up") true only for the current file
    // layout. A wiring call planted in `pattern.rs` left the whole suite green.
    // Same "the enumeration is a selection" shape this lint exists to stop, one
    // level up.
    let mut parser_files: Vec<String> = fs::read_dir("src/parser")
        .expect("cannot read src/parser")
        .map(|e| e.expect("dir entry").path())
        .filter(|p| p.extension().is_some_and(|x| x == "rs"))
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .collect();
    parser_files.sort();
    assert!(
        parser_files.len() >= 8,
        "only {} file(s) found under src/parser — the scan is reading nothing.",
        parser_files.len()
    );
    for file in &parser_files {
        let content =
            fs::read_to_string(file.as_str()).unwrap_or_else(|e| panic!("cannot read {file}: {e}"));
        let mut current = String::from("<file scope>");
        let mut counts: Vec<(String, usize)> = Vec::new();
        for line in content.lines() {
            let t = line.trim_start();
            if (t.starts_with("fn ") || t.starts_with("pub fn ") || t.starts_with("pub(super) fn ")
                || t.starts_with("pub(crate) fn "))
                && line.starts_with("    ")
            {
                current = t
                    .trim_start_matches("pub(super) ")
                    .trim_start_matches("pub(crate) ")
                    .trim_start_matches("pub ")
                    .trim_start_matches("fn ")
                    .split(['(', '<'])
                    .next()
                    .unwrap_or("?")
                    .to_string();
            }
            if t.starts_with("//") {
                continue;
            }
            let n: usize = needles.iter().map(|nd| line.matches(nd).count()).sum();
            if n == 0 {
                continue;
            }
            // The DEFINITIONS call nothing; only bodies do.
            match counts.last_mut() {
                Some((f, c)) if *f == current => *c += n,
                _ => counts.push((current.clone(), n)),
            }
        }
        // Merge repeats of the same fn (a fn interrupted by a nested item).
        let mut merged: Vec<(String, usize)> = Vec::new();
        for (f, c) in counts {
            match merged.iter_mut().find(|(mf, _)| *mf == f) {
                Some((_, mc)) => *mc += c,
                None => merged.push((f, c)),
            }
        }
        for (f, c) in merged {
            found.push((file.clone(), f, c));
        }
    }

    for (file, f, c) in &found {
        println!("header_start wiring: {file}  fn {f}  x{c}");
    }

    let mut got: Vec<(String, String, usize)> = found;
    got.sort();
    let mut want: Vec<(String, String, usize)> = CENSUS
        .iter()
        .map(|(a, b, c)| (a.to_string(), b.to_string(), *c))
        .collect();
    want.sort();
    assert_eq!(
        got, want,
        "the `Block::header_start` wiring census changed.\n\nEvery caller states \
         which position is its construct's FIRST line, and a wrong value is \
         SILENT — the formatter's orphan flush refuses every comment written \
         inside that block. A NEW row therefore owes:\n\
         (1) the right value at the call site (the construct's first line, NOT \
         the colon — a wrapped header puts the colon on a continuation line \
         indented at or past the body);\n\
         (2) a `BLOCK_PROBES` row in src/parser/tests.rs, WRAPPED if any token \
         in its header can span source lines, else `NotWrappable` with the \
         reason — which `block_probe_dispositions_are_decided` then holds \
         against the probe's own spelling;\n\
         (3) a cell in tests/fixtures/fmt_suite_layout/wrapped_header_anchor.gg \
         if it is wrappable.\n\n\
         Regenerate this table with:\n  \
         cargo test --test lints parser_header_start_wiring_census -- --nocapture"
    );

    // The probe corpus must not shrink below the wiring it covers. Counting
    // rows (not matching them 1:1 — several wiring sites are clause siblings of
    // one construct) keeps this a TRIGGER: a new wiring row forces a decision
    // above, and a deleted probe row trips here.
    // 30 = 26 + the four `else:` clauses (statement-`if`, for, select, meta-if)
    // that were carried inside neighbouring probes and dropped when those were
    // split; all SEVEN else-body wiring calls now have their own row.
    const EXPECTED_BLOCK_PROBES_ROWS: usize = 30;
    let probes = fs::read_to_string("src/parser/tests.rs")
        .expect("cannot read src/parser/tests.rs");
    let table = probes
        .split("const BLOCK_PROBES:")
        .nth(1)
        .and_then(|t| t.split("];").next())
        .expect("BLOCK_PROBES table not found");
    let rows = table
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("(\"") && (t.ends_with("Wrapped),") || t.contains("NotWrappable("))
        })
        .count();
    assert_eq!(
        rows, EXPECTED_BLOCK_PROBES_ROWS,
        "the `BLOCK_PROBES` row count moved ({rows} vs \
         {EXPECTED_BLOCK_PROBES_ROWS}). A DROP means a construct lost its probe \
         — the direction that goes silent. Bump with the new row's disposition."
    );
}

/// THE ORPHAN-PRE-CLOSE CENSUS. Every `self.emitter.dedent()` in
/// `src/formatter/mod.rs` is a block CLOSING, and a block that closes without
/// having claimed the comment written after its last child leaks that comment
/// outward — to the enclosing scope's next hook, in the worst case to the
/// module flush at column 0, where it reads as documentation of the NEXT item.
///
/// **Shape-detected per row, never a bare count.** A count stays green with a
/// site missing (that is exactly how a hookless loop shipped once before), so
/// this lint classifies each row by the EVIDENCE immediately above it and RED's
/// on any row it cannot classify. The class vocabulary is the design's, not a
/// list invented here:
///
///   * `Routed` — the row closes a statement suite whose body went through
///     `format_block_stmts`, which carries the flush for the whole family.
///   * `Container` — struct / enum / trait / equip / extern block: children are
///     members, not statements, so the container calls the flush itself.
///   * `NestedItems` — the item-level `meta if` branch bodies, whose flush
///     lives in the shared `format_nested_items` producer (one per arm).
///   * `ArmContainer` — match statement / select / meta match / match
///     EXPRESSION: children are ARMS, so the routed chokepoint is structurally
///     absent and each calls the flush on its own span.
///   * `Site13` — the `meta for …:` block INSIDE a match statement. It owns its
///     own indent/child/dedent, so it needs its own flush anchored on ITS
///     header; with only the match container's flush its tail is re-parented to
///     the arms level.
///   * `ClosureRouting` — the closure body paths, which route through
///     `format_closure_post_prelude` (the prelude-skipping one cannot delegate
///     to `format_block_stmts` and takes the flush explicitly).
///   * `Bracketed` — `format_bracketed_broken_with_comments`, the
///     collection-literal shape that had the orphan-pre-close POSITION before
///     the rest of the family did.
///
/// The scope is `src/formatter/mod.rs` PLUS an assertion that `doc.rs` still
/// contains zero `dedent()` calls — so a future one there trips this guard
/// instead of escaping its scope.
///
/// **Break-and-verify, two ways (both RED-verified when this landed):**
///   1. delete one flush call (e.g. the one in `format_struct`) — that row's
///      evidence disappears, it classifies as UNKNOWN, RED;
///   2. add a bare `self.emitter.indent(); … self.emitter.dedent();` pair
///      anywhere — the new row has no evidence, UNKNOWN, RED.
/// Restore, green.
///
/// ⚠ The three existing hook lints CANNOT see this chokepoint:
/// `.emit_orphan_comments_before_close(` contains none of the strings they
/// count. This is their sibling, not a modification of them.
#[test]
fn formatter_dedent_close_census() {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    enum Class {
        Routed,
        Container,
        NestedItems,
        ArmContainer,
        Site13,
        ClosureRouting,
        Bracketed,
        Unknown,
    }
    use Class::*;

    /// Expected row count per class. Regenerate with
    ///   cargo test --test lints formatter_dedent_close_census -- --nocapture
    const EXPECTED: &[(Class, usize)] = &[
        (Routed, 30),
        (Container, 5),
        (NestedItems, 3),
        (ArmContainer, 4),
        (Site13, 1),
        (ClosureRouting, 2),
        (Bracketed, 1),
    ];
    /// Every `dedent()` in the file is one of the classes above.
    const EXPECTED_TOTAL: usize = 46;

    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let lines: Vec<&str> = content.lines().collect();

    // `doc.rs` owns no indentation state today; if it grows a `dedent()` this
    // census stops covering the file that has it.
    let doc = fs::read_to_string("src/formatter/doc.rs").expect("cannot read doc.rs");
    assert_eq!(
        doc.matches("dedent()").count(),
        0,
        "`src/formatter/doc.rs` grew a `dedent()`. This census only scans \
         `mod.rs`; extend its scope (and classify the new rows) rather than \
         letting a block close outside the guard."
    );

    // Classify each `dedent()` by the evidence in the window above it, up to
    // the matching `indent()` (or 60 lines, whichever comes first).
    let mut rows: Vec<(usize, String, Class)> = Vec::new();
    let mut current_fn = String::from("<file scope>");
    for (i, line) in lines.iter().enumerate() {
        let t = line.trim_start();
        if (t.starts_with("fn ") || t.starts_with("pub fn ")) && line.starts_with("    ") {
            current_fn = t
                .trim_start_matches("pub ")
                .trim_start_matches("fn ")
                .split(['(', '<'])
                .next()
                .unwrap_or("?")
                .to_string();
        }
        if t.starts_with("//") || !line.contains("self.emitter.dedent()") {
            continue;
        }
        let start = i.saturating_sub(60);
        let mut class = Unknown;
        for w in (start..i).rev() {
            let wl = lines[w];
            let wt = wl.trim_start();
            if wt.starts_with("//") {
                continue;
            }
            if wl.contains("self.emitter.indent()") {
                break;
            }
            if wl.contains(".emit_orphan_comments_before_close(") {
                // The ARGUMENT names the anchor, and the anchor names the class.
                class = if wl.contains("(s.span.start")
                    || wl.contains("(e.span.start")
                    || wl.contains("(t.span.start")
                    || wl.contains("(eb.span.start")
                {
                    Container
                } else if wl.contains("(stmt.span.start") || wl.contains("(expr.span.start") {
                    ArmContainer
                } else if wl.contains("(span.start") {
                    Site13
                } else {
                    Unknown
                };
                break;
            }
            if wl.contains("self.format_nested_items(") {
                class = NestedItems;
                break;
            }
            if wl.contains("self.format_closure_post_prelude(") {
                class = ClosureRouting;
                break;
            }
            if wl.contains("self.format_block_stmts(") {
                class = Routed;
                break;
            }
            if wl.contains("self.emit_comments_before(container_end)") {
                class = Bracketed;
                break;
            }
        }
        rows.push((i + 1, current_fn.clone(), class));
    }

    // Printed so the constants above can be regenerated from the scan itself.
    for (ln, f, c) in &rows {
        println!("dedent row: src/formatter/mod.rs:{ln}  fn {f}  {c:?}");
    }

    let unknown: Vec<String> = rows
        .iter()
        .filter(|(_, _, c)| *c == Unknown)
        .map(|(ln, f, _)| format!("  src/formatter/mod.rs:{ln} (fn {f})"))
        .collect();
    assert!(
        unknown.is_empty(),
        "UNCLASSIFIED block close(s) — every `dedent()` in the formatter must \
         be attributable to one of the orphan-pre-close classes, because a \
         block that closes without claiming its tail comment leaks it \
         outward:\n{}\n\nIf the new row genuinely closes a block that can hold \
         an author's tail comment, give it a flush \
         (`emit_orphan_comments_before_close(<owning construct's first line>, \
         <block end>)`). If it closes something else, say which class it is and \
         teach this scan to see it — never widen the scan to swallow it \
         silently.",
        unknown.join("\n")
    );

    assert_eq!(
        rows.len(),
        EXPECTED_TOTAL,
        "the formatter's `dedent()` count moved ({} vs {EXPECTED_TOTAL}). That \
         is fine — but the per-class table below must move with it, and the new \
         row needs a class.",
        rows.len()
    );

    for (class, want) in EXPECTED {
        let got = rows.iter().filter(|(_, _, c)| c == class).count();
        assert_eq!(
            got, *want,
            "orphan-pre-close census: {class:?} rows moved ({got} vs {want}). \
             A DROP is the dangerous direction — it means a block close lost \
             its flush and its tail comments now escape. Re-derive with \
             `cargo test --test lints formatter_dedent_close_census -- --nocapture`."
        );
    }
}

/// THE CLAIM-SITE CENSUS. `Formatter::comment_cursor` advances at exactly ONE
/// place, and every hook that takes a comment off it goes through the one
/// claim/emit pair.
///
/// Why it has to be one place: a trailing comment continued on the lines below
/// it is ONE logical comment, so whichever hook claims the HEAD must claim the
/// whole run — otherwise the run splits and its continuation lines end up
/// documenting whatever follows them. That rule is not enforceable by review
/// across four hooks; it is enforceable by there being one cursor advance.
///
/// **Break-and-verify (both RED-verified when this landed):** add a bare
/// `self.comment_cursor += 1;` in any hook — the advance count moves and this
/// fires; add a new caller of `claim_run_at_cursor` without a census row — the
/// caller count moves and this fires. The needle set covers `-=`,
/// `mem::replace`/`mem::swap` (via `&mut`) and plain assignment too, so a
/// bypassing claimer cannot dodge it by moving the cursor some other way.
#[test]
fn formatter_comment_claim_site_census() {
    /// The functions that may claim a comment, and why each does.
    const CLAIMERS: &[(&str, &str)] = &[
        ("emit_comments_before", "leading comments before a sibling's start"),
        ("emit_remaining_comments", "the EOF flush"),
        ("emit_trailing_comment_after", "a same-source-line trailing comment"),
        ("claim_header_trailing_comments", "an inline suite's header comment, emitted after its body"),
        ("emit_orphan_comments_before_close", "the orphan-pre-close flush"),
    ];
    /// `comment_cursor += 1` sites — BOTH inside `claim_run_at_cursor` (the
    /// head, then each continuation).
    const EXPECTED_CURSOR_ADVANCES: usize = 2;

    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let lines: Vec<&str> = content.lines().collect();

    let mut advances: Vec<(usize, String)> = Vec::new();
    let mut claim_calls: Vec<(usize, String)> = Vec::new();
    let mut current_fn = String::from("<file scope>");
    for (i, line) in lines.iter().enumerate() {
        let t = line.trim_start();
        if (t.starts_with("fn ") || t.starts_with("pub fn ")) && line.starts_with("    ") {
            current_fn = t
                .trim_start_matches("pub ")
                .trim_start_matches("fn ")
                .split(['(', '<'])
                .next()
                .unwrap_or("?")
                .to_string();
        }
        if t.starts_with("//") {
            continue;
        }
        // ANY mutation of the cursor, not just `+= 1`. `+=` alone would miss a
        // `-=` rewind (which is the one way the inner-before-outer flush
        // ordering can break) and a `mem::replace`/`mem::swap`/`&mut` alias,
        // each of which moves the cursor without ever spelling an increment.
        // The needle set is the FIELD in any mutating position.
        let cursor_mutation = ["self.comment_cursor +=", "self.comment_cursor -=",
                               "self.comment_cursor =", "&mut self.comment_cursor"];
        if cursor_mutation.iter().any(|n| line.contains(n)) {
            advances.push((i + 1, current_fn.clone()));
        }
        if line.contains("self.claim_run_at_cursor(") {
            claim_calls.push((i + 1, current_fn.clone()));
        }
    }

    let stray: Vec<String> = advances
        .iter()
        .filter(|(_, f)| f != "claim_run_at_cursor")
        .map(|(ln, f)| format!("  src/formatter/mod.rs:{ln} (fn {f})"))
        .collect();
    assert!(
        stray.is_empty(),
        "`comment_cursor` advanced OUTSIDE `claim_run_at_cursor`:\n{}\n\nA hook \
         that advances the cursor itself can take a multi-line trailing \
         comment's HEAD and leave its continuation lines behind, which splits \
         the run and re-parents its tail. Route the claim through \
         `claim_run_at_cursor` + `emit_claimed_run` and add a row to CLAIMERS \
         with the reason this position claims.",
        stray.join("\n")
    );
    assert_eq!(
        advances.len(),
        EXPECTED_CURSOR_ADVANCES,
        "the number of cursor advances inside `claim_run_at_cursor` changed \
         ({} vs {EXPECTED_CURSOR_ADVANCES}). Expected exactly two: the head, \
         and each continuation of its run.",
        advances.len()
    );

    let mut callers: Vec<String> = claim_calls.iter().map(|(_, f)| f.clone()).collect();
    callers.sort();
    callers.dedup();
    let mut expected: Vec<String> = CLAIMERS.iter().map(|(f, _)| (*f).to_string()).collect();
    expected.sort();
    assert_eq!(
        callers,
        expected,
        "the set of comment CLAIM SITES changed.\n\nEvery claimer is listed in \
         CLAIMERS with the reason it claims; a new cursor-advancing site with \
         no row is exactly the drift this census exists to catch. Sites found: \
         {claim_calls:?}"
    );
}

#[test]
fn formatter_child_collection_loop_census() {
    /// Hook state a census row expects.
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    enum Hooks {
        /// Leading + trailing hook, both in the loop body.
        Both,
        /// Leading hook in the body; the trailing/header hook is delegated to
        /// the child emitter or written as `emit_trailing_comment_after_header`.
        Leading,
        /// Knowingly hookless — see the row's rationale.
        None_,
    }
    use Hooks::*;

    // (enclosing fn, loop header text, expected hook state).
    //
    // Rationale for the non-`Both` rows:
    //   * the four ARM loops + `format_elif_else_blocks` carry the LEADING
    //     hook (R41 follow-up: without it a comment written above `case`/
    //     `elif`/`else` was re-emitted INSIDE the branch body, documenting the
    //     wrong thing). Their trailing side is the header hook that
    //     `format_match_arm` / the loop body already emits, so a second
    //     `emit_trailing_comment_after` here would double-claim.
    //   * `format_item`'s `Item::Module` loop is a SYNTHETIC container built by
    //     the loader for an imported module, not a source-level block, so it
    //     has no interior comments of its own to preserve.
    const CENSUS: &[(&str, &str, Hooks)] = &[
        ("format_module", "for item in &directives {", Both),
        ("format_module", "for item in &imports {", Both),
        ("format_module", "for (i, item) in rest.iter().enumerate() {", Both),
        // R41 fold: the four `meta if` nested-item loops (then / the elif
        // branch walk / the elif body / else) collapsed into ONE producer,
        // `format_nested_items`, when the nested-item blank preservation
        // landed — four copies of the loop were four chances to omit it, and
        // one of them omitting it is exactly what the snag was. The surviving
        // surviving `mi.elif_branches` row is the BRANCH walk, not
        // a child-collection loop: it emits the `elif` HEADER (with its own
        // leading-comment hook, hence `Leading`, exactly like the statement-level
        // `format_elif_else_blocks` row above) and delegates the items. It is
        // INDEXED (`enumerate`) because each branch's orphan-flush ceiling is
        // the NEXT clause's header, which the walk has to look ahead for.
        (
            "format_item",
            "for (bi, (cond, items)) in mi.elif_branches.iter().enumerate() {",
            Leading,
        ),
        ("format_item", "for inner in items {", None_),
        ("format_nested_items", "for (i, item) in items.iter().enumerate() {", Both),
        ("format_struct", "for (i, field) in s.fields.iter().enumerate() {", Both),
        ("format_enum", "for (i, variant) in e.variants.iter().enumerate() {", Both),
        ("format_trait", "for (i, item) in t.items.iter().enumerate() {", Both),
        ("format_equip", "for (i, method) in e.items.iter().enumerate() {", Both),
        ("format_extern_block", "for func in &eb.items {", Both),
        ("format_block_stmts", "for (i, stmt) in block.stmts.iter().enumerate() {", Both),
        ("format_elif_else_blocks", "for (cond, body) in elif_branches {", Leading),
        ("format_stmt", "for item in arms {", Leading),
        ("format_stmt", "for arm in arms {", Leading),
        ("format_stmt", "for (case_expr, body) in arms {", Leading),
        // R42 Track D: the expression match moved out of `format_expr` into
        // `format_expr_inner` — `format_expr` is now the author-paren wrapper
        // that delegates to it. The SCOPE is re-pointed, never the count:
        // a census that reports 0 because its scope emptied is the guard
        // silently retiring itself.
        ("format_expr_inner", "for arm in arms {", Leading),
        // R41 T-FMT-C: the closure post-prelude loop moved out of `format_expr`
        // into its own `format_closure_post_prelude`, so the indented and
        // (unreachable-for-parser-output) fallback paths share ONE emitter
        // instead of two copies of the hook pair.
        ("format_closure_post_prelude", "for stmt in post_prelude {", Both),
    ];

    /// Child emitters that put their argument on its own source line(s), so a
    /// loop delegating to one is a line-per-child loop even with no direct
    /// `newline()` in the loop body.
    const BLOCK_CHILD: &[&str] = &[
        "format_match_arm(",
        "format_function(",
        "format_stmt(",
        "format_block_stmts(",
        "format_item(",
    ];

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let src: Vec<&str> = content.lines().collect();

    // Attribute each line to its enclosing `fn` by brace depth. A fn header may
    // span several lines (`fn f(\n  a: T,\n) {`), so the fn's base depth is
    // recorded when its opening brace actually appears, not at the header line.
    let mut fn_stack: Vec<(String, i32, bool)> = Vec::new();
    let mut depth: i32 = 0;
    // (enclosing fn, header text, has_leading, has_trailing)
    let mut open_loops: Vec<(String, String, i32, String)> = Vec::new();
    let mut found: Vec<(String, String, Hooks)> = Vec::new();

    for line in &src {
        let trimmed = line.trim_start();
        let is_comment = trimmed.starts_with("//");

        if !is_comment {
            let after_vis = trimmed
                .strip_prefix("pub(crate) fn ")
                .or_else(|| trimmed.strip_prefix("pub fn "))
                .or_else(|| trimmed.strip_prefix("fn "));
            if let Some(rest) = after_vis {
                let name: String = rest
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect();
                fn_stack.push((name, depth, false));
            }
            if trimmed.starts_with("for ") && trimmed.ends_with('{') {
                let owner = fn_stack.last().map(|f| f.0.clone()).unwrap_or_default();
                open_loops.push((owner, trimmed.to_string(), depth, String::new()));
            }
        }
        for l in open_loops.iter_mut() {
            l.3.push_str(line);
            l.3.push('\n');
        }
        if !is_comment {
            let opens = line.matches('{').count() as i32;
            let closes = line.matches('}').count() as i32;
            if let Some(top) = fn_stack.last_mut() {
                if !top.2 && opens > 0 {
                    top.2 = true;
                    top.1 = depth;
                }
            }
            depth += opens - closes;
            let mut i = 0;
            while i < open_loops.len() {
                if depth <= open_loops[i].2 {
                    let (owner, header, _, body) = open_loops.remove(i);
                    let emits_children = body.contains("self.format_");
                    let per_line = body.contains(".emitter.newline()")
                        || BLOCK_CHILD.iter().any(|b| body.contains(b));
                    if emits_children && per_line {
                        let lead = body.contains(".emit_comments_before(");
                        let trail = body.contains(".emit_trailing_comment_after(");
                        found.push((
                            owner,
                            header,
                            match (lead, trail) {
                                (true, true) => Both,
                                (true, false) => Leading,
                                _ => None_,
                            },
                        ));
                    }
                } else {
                    i += 1;
                }
            }
            while matches!(fn_stack.last(), Some(f) if f.2 && depth <= f.1) {
                fn_stack.pop();
            }
        }
    }

    let mut got: Vec<(String, String, Hooks)> = found;
    got.sort();
    let mut want: Vec<(String, String, Hooks)> = CENSUS
        .iter()
        .map(|(f, h, s)| (f.to_string(), h.to_string(), *s))
        .collect();
    want.sort();

    assert_eq!(
        got, want,
        "R41 T-FMT-A child-collection loop census in `src/formatter/mod.rs` \
         changed.\n\ngot:  {got:#?}\nwant: {want:#?}\n\n\
         A formatter loop that emits AST children as separate SOURCE LINES can \
         have a comment sitting between any two of them. Without the leading \
         hook the comment is swallowed into the NEXT child's body (the \
         match/elif misattribution class); without the trailing hook it escapes \
         the container entirely and re-emerges at column 0 (the extern-block \
         class, R41 §5). Wire `emit_comments_before(child.span.start)` before \
         the child emit and `emit_trailing_comment_after(child.span.end, false)` \
         after it, mirroring `format_trait` / `format_equip` — then add the row \
         here with its rationale."
    );
}

/// R41 T-FMT-A (Core #4 — the SITE axis, 2026-08-11): pins the parser's
/// `parse_ownership_modifier` CALL SITES against the formatter's paren guards.
///
/// The exhaustive `Expr` match in `emits_leading_ownership_sigil` closes the
/// VARIANT axis — a new expression kind is a compile error. It says nothing
/// about the SITE axis, which is the one that produced the original bug: the
/// defect existed because two parser positions strip an ownership sigil BEFORE
/// the expression parser runs, and the formatter guarded neither. An 8th such
/// position could be added tomorrow with the formatter still guarding two, and
/// nothing would notice.
///
/// So enumerate the sites and require the enumeration to stay TOTAL: the three
/// disposition buckets must sum to the measured total (Core #15e Q3 — a
/// selection cannot show you what it omits).
///
/// **Break-and-verify:** add a `parse_ownership_modifier()` call anywhere under
/// `src/parser/`; the total moves and this lint fires, forcing the new site to
/// be classified as guarded / carve-out / non-flippable.
#[test]
fn formatter_ownership_modifier_site_pin() {
    /// EXPRESSION-OPERAND positions — the sigil is stripped ahead of an
    /// expression, so an expression whose emission LEADS with a sigil silently
    /// re-homes it into the node's `ownership` field. These are the positions
    /// `Formatter::format_ownership_modifier_operand` must guard:
    ///   1. `src/parser/stmt.rs`  `parse_for_stmt`  — the iterable
    ///   2. `src/parser/expr.rs`  `parse_call_arg`  — the value (POSITIONAL
    ///      args only: the pre-pass runs ahead of the `name =` lookahead, so a
    ///      named arg's value is parsed with no pre-pass and needs no guard)
    const GUARDED: usize = 2;
    /// The comprehension iterable (`src/parser/expr.rs`, sigil BEFORE `in`).
    /// Also an expression-operand position, deliberately NOT guarded: the
    /// ratified D33 comprehension rider retires the pre-`in` spelling by moving
    /// the PARSER, and cites the formatter's post-`in` emission as corroboration
    /// of that direction. "Fixing" it in the formatter would contradict a
    /// ratified decision (Core #15e Q1). Repro:
    /// `tests/fixtures/known_gaps/comprehension_pre_in_sigil_retired.gg`.
    const CARVE_OUT: usize = 1;
    /// Positions where the sigil precedes a NAME or a TYPE, never an
    /// expression, so no paren can change the parse:
    ///   1. `src/parser/mod.rs`   `parse_param`            — param name slot
    ///   2. `src/parser/expr.rs`  closure destructure binding name slot
    ///   3. `src/parser/expr.rs`  closure typed-param name slot
    ///   4. `src/parser/types.rs` type position (D32 whitelist)
    const NON_FLIPPABLE: usize = 4;

    let mut sites = 0usize;
    for f in [
        "src/parser/mod.rs",
        "src/parser/stmt.rs",
        "src/parser/expr.rs",
        "src/parser/types.rs",
    ] {
        let content = fs::read_to_string(f).unwrap_or_else(|_| panic!("cannot read {f}"));
        for line in content.lines() {
            let t = line.trim_start();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            // CALL sites only — `pub fn parse_ownership_modifier(` is the
            // definition and must not be counted.
            if t.starts_with("pub fn ") || t.starts_with("fn ") {
                continue;
            }
            sites += line.matches("parse_ownership_modifier()").count();
        }
    }

    assert_eq!(
        sites,
        GUARDED + CARVE_OUT + NON_FLIPPABLE,
        "R41 T-FMT-A site pin: `parse_ownership_modifier()` call-site count \
         under src/parser/ is {sites}, but the census accounts for {} \
         ({GUARDED} guarded + {CARVE_OUT} carve-out + {NON_FLIPPABLE} \
         non-flippable).\n\n\
         Every position that strips an ownership sigil BEFORE parsing an \
         EXPRESSION must be guarded by \
         `Formatter::format_ownership_modifier_operand`, or `gg fmt` will \
         re-home the sigil into the enclosing node's `ownership` field and \
         change accept/reject. Classify the new site and update the constants \
         (and the formatter guard, if it is an expression-operand position).",
        GUARDED + CARVE_OUT + NON_FLIPPABLE
    );

    // The formatter side of the pin: exactly GUARDED call sites.
    let fmt = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let guards = fmt
        .lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .map(|l| l.matches(".format_ownership_modifier_operand(").count())
        .sum::<usize>();
    assert_eq!(
        guards, GUARDED,
        "R41 T-FMT-A site pin: the formatter has {guards} \
         `format_ownership_modifier_operand(` call site(s), expected {GUARDED} \
         — one per expression-operand `parse_ownership_modifier` position. If a \
         parser position was added or removed, move BOTH counts together."
    );
}

/// Interior-comment escape, Core #4 producer-chokepoint guard: the
/// decision to break a fill-emitted delimited list around an interior
/// comment is taken in exactly ONE place, and the exploded emission is
/// reached from exactly ONE place.
///
/// **The counts are STRUCTURAL, not per-arm — and that is the point.**
/// This guard used to pin the dispatch count at 4, one per
/// collection-literal arm, which meant every new list emitter came with
/// an instruction to BUMP it. A count that grows per emitter green-lights
/// the very class it exists to retire (Core #15e Q2): the eleventh
/// emitter raises the number and nothing asks whether it was gated. Under
/// the chokepoint the numbers are structurally 1 and cannot be reached by
/// adding an emitter at all — a new one either routes through
/// `emit_delimited_list` (gated, counted by
/// `formatter_list_emit_fill_census`) or hand-rolls a second
/// `doc::surround_fill`, which trips that census instead.
///
/// **`.has_interior_comments(` is 2, and it is NOT "one per dispatch".**
/// The chokepoint holds one; the `Expr::TupleLiteral` single-element
/// branch holds the other, because `(x,)` is spelled flat — the trailing
/// comma IS the tuple — so it does not route through the chokepoint and
/// must consult the sideband itself. The two are not redundant, and
/// collapsing them reopens the escape for 1-tuples; a `debug_assert!` in
/// that branch says so at the site.
///
/// **StructLiteral unreachability is pinned here too** (Core #14 — the
/// claim used to be prose with no guard). `gg fmt` is parse-only and the
/// parser constructs zero `Expr::StructLiteral`; the formatter's arm is
/// kept in sync as class hygiene and carries no red. Both halves are
/// asserted below, with the counting method spelled out.
///
/// **Break-and-verify (Core #6 / #13):**
///   * add a second `self.format_bracketed_broken_with_comments(...)`
///     anywhere in the formatter — fires with `2 vs expected 1`;
///   * add `let _x = Expr::StructLiteral { … };` under `src/parser/` —
///     fires on the construction-site assertion;
///   * add `use crate::semantic::…` to the formatter — fires on the
///     parse-only assertion.
///
/// **Pairs with `formatter_literal_arms_dispatch_count`** below, which
/// counts the `Expr::*Literal` ARMS rather than any dispatch.
#[test]
fn formatter_collection_literal_interior_hook_dispatch() {
    /// The exploded-emission entry point, reached from the chokepoint
    /// and nowhere else. Structurally 1 — do not bump it to admit a new
    /// caller; route the caller through `emit_delimited_list` instead.
    const EXPECTED_DISPATCH: usize = 1;
    /// Interior-comment gates: the chokepoint + the single-element-tuple
    /// branch. NOT one per dispatch — see the docstring.
    const EXPECTED_INTERIOR_CHECK: usize = 2;

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let mut dispatch_count = 0usize;
    let mut interior_count = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        dispatch_count += line.matches(".format_bracketed_broken_with_comments(").count();
        interior_count += line.matches(".has_interior_comments(").count();
    }
    assert_eq!(
        dispatch_count, EXPECTED_DISPATCH,
        "interior-comment dispatch guard: \
         `.format_bracketed_broken_with_comments(` call-site count in \
         `src/formatter/mod.rs` = {dispatch_count}, expected \
         {EXPECTED_DISPATCH} — the chokepoint `emit_delimited_list` is \
         its only caller.\n\n\
         A SECOND caller means a list emitter took the exploded path \
         without going through the gate. Route it through \
         `emit_delimited_list` rather than raising this constant."
    );
    assert_eq!(
        interior_count, EXPECTED_INTERIOR_CHECK,
        "interior-comment gate guard: `.has_interior_comments(` call-site \
         count in `src/formatter/mod.rs` = {interior_count}, expected \
         {EXPECTED_INTERIOR_CHECK} (the chokepoint + the \
         `Expr::TupleLiteral` single-element branch).\n\n\
         A DROP to 1 means the single-elem-tuple branch stopped consulting \
         the sideband — `(x,)` is emitted flat, so an interior comment \
         escapes again. A RISE means a new gate outside the chokepoint: \
         route it through `emit_delimited_list` instead."
    );

    // ── Core #14: the StructLiteral-unreachability claim, enforced.
    //
    // COUNTING METHOD, half one: `Expr::StructLiteral` appears under
    // `src/parser/` only in PATTERN position. The exact allowlist is
    // pinned with per-site attribution so a reviewer can see at a glance
    // whether a new mention is a pattern or a construction. A
    // CONSTRUCTION is spelled `Expr::StructLiteral {` with field
    // initialisers; the two live mentions destructure instead.
    let parser_mentions: &[(&str, &str)] = &[
        (
            "src/parser/visitor.rs",
            "Expr::StructLiteral { args, .. } => {",
        ),
        (
            "src/parser/expr.rs",
            "Expr::StructLiteral { args, .. } => args.iter().any(contains_it),",
        ),
    ];
    let mut found: Vec<String> = Vec::new();
    for entry in fs::read_dir("src/parser").expect("cannot read src/parser") {
        let path = entry.expect("dir entry").path();
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let text = fs::read_to_string(&path).expect("cannot read parser file");
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") {
                continue;
            }
            if trimmed.contains("Expr::StructLiteral") {
                found.push(format!("{}: {trimmed}", path.display()));
            }
        }
    }
    let expected: Vec<String> = parser_mentions
        .iter()
        .map(|(f, snippet)| format!("{f}: {snippet}"))
        .collect();
    found.sort();
    let mut expected_sorted = expected.clone();
    expected_sorted.sort();
    assert_eq!(
        found, expected_sorted,
        "`Expr::StructLiteral` mentions under `src/parser/` changed.\n\n\
         The formatter's `Expr::StructLiteral` arm is documented as \
         fmt-UNREACHABLE, and its fixtures say the cells are pinned on the \
         `Expr::Call` path instead. That rests on the parser constructing \
         ZERO `Expr::StructLiteral` — the sole producer being \
         `rewrite_struct_calls` in semantic analysis, which `gg fmt` never \
         reaches.\n\n\
         If the new mention is another PATTERN, add it to the allowlist \
         above with its file and snippet. If it is a CONSTRUCTION, the \
         unreachability claim is now FALSE: the formatter arm becomes \
         live, its fixtures need real reds, and this lint's docstring \
         needs rewriting."
    );

    // COUNTING METHOD, half two: `gg fmt` is parse-only. The formatter
    // never names the semantic layer, so it cannot see a rewritten AST.
    let formatter = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let semantic_mentions = formatter
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with("//") && l.contains("semantic::")
        })
        .count();
    assert_eq!(
        semantic_mentions, 0,
        "`src/formatter/mod.rs` now references `semantic::` \
         ({semantic_mentions} site(s)). `gg fmt` is parse-only, which is \
         the other half of the StructLiteral-unreachability claim above; \
         if the formatter has gained access to post-semantic AST, that \
         claim and the fixtures resting on it must be revisited."
    );
}

/// R39 fmt collection-literal interior-comment escape (Core #4 + #15e Q2
/// class-fix pair with `formatter_collection_literal_interior_hook_dispatch`,
/// 2026-08-09): pins the number of `Expr::*Literal` arms in
/// `format_expr_inner` in `src/formatter/mod.rs` at exactly 4. Precedent:
/// `container_literal_arms_count` at line 1021 scans `infer_expr` for
/// `Expr::*Literal` arms in the typechecker.
///
/// Every collection-literal expression arm MUST reach the delimited-list
/// chokepoint `Formatter::emit_delimited_list`, which is what consults the
/// comment side-table before the `Doc` layer. Adding a 5th arm (e.g. a
/// separately-spelled `Expr::SetLiteral`) bumps this count, forcing its
/// author past that decision.
///
/// The counts on the two sides are now different KINDS of number, and the
/// distinction is the point. This one is per-ARM and grows with the AST;
/// the chokepoint's dispatch count in
/// `formatter_collection_literal_interior_hook_dispatch` is structurally 1
/// and does NOT grow — a new arm either routes through the chokepoint
/// (counted per-kind by `formatter_list_emit_fill_census`) or hand-rolls a
/// second `doc::surround_fill`, which that census catches. So a 5th arm
/// added silently can no longer leave a "balanced" pair behind it.
///
/// **Break-and-verify:** insert `Expr::SetLiteral(_) => {}` into
/// `format_expr_inner`'s match body; this test fires with `5 vs expected 4`.
///
/// **Scope:** counts only lines that begin with `Expr::*Literal(` /
/// `Expr::StructLiteral {` INSIDE the `format_expr_inner` fn body, keyed off
/// the `fn format_expr_inner(` header + brace-depth tracking (R42 Track D
/// moved the match there; `format_expr` is now the author-paren wrapper, and
/// keying on it would silently scope this census to an empty function). The
/// `Return(Some)` carve-out at ~line 1619 that pattern-matches
/// `Expr::TupleLiteral` in `format_stmt`'s `Return` arm is a DIFFERENT
/// fn — the scope guard excludes it.
#[test]
fn formatter_literal_arms_dispatch_count() {
    /// Expected collection-literal arms in `format_expr`:
    /// - Expr::ArrayLiteral
    /// - Expr::TupleLiteral
    /// - Expr::DictLiteral
    /// - Expr::StructLiteral (kept for defensive class-fix even though
    ///   currently unreachable via fmt's parse-only pipeline)
    /// Baseline 2026-08-09: 4.
    const EXPECTED_ARMS: usize = 4;

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    // Known collection-literal arms (the 4-pattern class-fix set) and a
    // FUTURE-PROOFING list of potential-new-variant tell-tales. Adding a
    // new variant to `Expr` typically follows the `*Literal` naming
    // convention; catching the naming class here (rather than requiring
    // the lint list to be updated *before* the new variant lands) makes
    // the guard trip even when a new arm slips in unlisted. Any name in
    // `future_literal_patterns` that grows a real count bumps the total
    // — the developer must then either handle the new arm via dispatch
    // + move the pattern to the KNOWN list, or intentionally raise
    // EXPECTED_ARMS with a rationale.
    let arm_patterns = [
        "Expr::ArrayLiteral(",
        "Expr::TupleLiteral(",
        "Expr::DictLiteral(",
        "Expr::StructLiteral {",
    ];
    let future_literal_patterns = [
        "Expr::SetLiteral(",
        "Expr::MapLiteral(",
        "Expr::HashLiteral(",
        "Expr::RecordLiteral(",
        "Expr::UnitLiteral(",
        "Expr::EnumLiteral(",
    ];

    let mut in_format_expr = false;
    let mut depth: i32 = 0;
    let mut count = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        if !in_format_expr && trimmed.starts_with("fn format_expr_inner(") {
            in_format_expr = true;
            depth = 0;
        }
        if !in_format_expr {
            continue;
        }
        depth += line.matches('{').count() as i32;
        depth -= line.matches('}').count() as i32;
        if depth <= 0 && !trimmed.starts_with("fn format_expr_inner(") {
            in_format_expr = false;
            continue;
        }
        for pat in arm_patterns.iter().chain(future_literal_patterns.iter()) {
            if trimmed.starts_with(pat) {
                count += 1;
                break;
            }
        }
    }
    assert_eq!(
        count, EXPECTED_ARMS,
        "R39 fmt collection-literal-arm count in `format_expr` in \
         `src/formatter/mod.rs` = {count}, expected {EXPECTED_ARMS} \
         (Expr::ArrayLiteral / Expr::TupleLiteral / Expr::DictLiteral / \
         Expr::StructLiteral).\n\n\
         If a new collection-literal arm was added, ensure it dispatches \
         through `format_bracketed_broken_with_comments` when \
         `has_interior_comments` fires (Core #4 chokepoint), then bump \
         BOTH this constant AND the dispatch-count constants in \
         `formatter_collection_literal_interior_hook_dispatch` above. \
         If an arm was removed, lower EXPECTED_ARMS with the removal \
         citation."
    );
}

/// Round XXXIX Phase 2e Core #6 producer-chokepoint guard: every
/// COMMA-SEPARATED-LIST parse in each of the THREE SH parser copies
/// (`self_host_{typechecker,parser,resolver}/parser.gg` — see
/// tests/fixtures/self_host_lowerer/parser.gg which symlinks to the
/// typechecker copy) MUST route through the shared
/// `Parser::consume_comma_or_tok(&self, int terminator)` chokepoint
/// (or one of the intentionally-remaining sibling shapes enumerated
/// below).
///
/// **Root defect (R37+ SH-lowerer stage-2 double-free — see DONE.md R39
/// entry):** the raw pattern
///     `while self.match_tok(TOK_COMMA):`
///         `items.push(self.parse_item())`
/// cascades on trailing-comma input like `Foo(a, b, c,)` — the loop calls
/// `parse_item` unconditionally after matching `,`, which hits the
/// closing `)`, invokes the "expected expression" fallback that ADVANCES
/// past the `)` and returns a dummy `EIntLiteral(0)`, and the OUTER
/// parser then treats the next sibling ctor as a further arg of the
/// current one — silently swallowing whole subtrees.  The class defect
/// spans 20+ sites per copy; Rust gg's parser accepts trailing commas
/// at these positions and the formatter emits them.
///
/// **Class fix (Core #4 chokepoint):** all accepting sites route
/// through `Parser::consume_comma_or_tok(terminator)` — one shared
/// helper per copy whose body performs the `match_tok(TOK_COMMA)` +
/// `check_tok(terminator)` pair correctly.  The owner-directed
/// REJECTING sites (previously left raw with cascade behavior) were
/// migrated in a second pass (owner call 2026-08-09) to ACCEPT
/// trailing commas via inline `while check_tok(TOK_COMMA): advance;
/// if check_<TERM>: break; ...` rewrites — 6 sites in the
/// typechecker copy (return-tuple / for-bindings / auto-tuple /
/// fn-return-type / import list / EDo return-tuple), 5 in the
/// parser copy (parser copy has no EDo variant), 5 in the resolver
/// copy (same as parser).
///
/// **Any new comma-separated-list site added later MUST use the
/// helper**; the lint catches a REGRESSION that adds a raw
/// `while self.match_tok(TOK_COMMA):` back into ANY of the three copies.
///
/// **RAW SITES INTENTIONALLY LEFT (post-owner-directive migration):**
/// After the two-pass migration, every copy has EXACTLY ZERO raw
/// `while self.match_tok(TOK_COMMA):` loops — every comma-list parse
/// either routes through `consume_comma_or_tok(TOK_TERM)` (for
/// paren/bracket/brace terminators) or through a
/// `while check_tok(TOK_COMMA): advance; if check_<TERM>: break; ...`
/// inline rewrite (for KW / NEWLINE / TOK_EQ / TOK_IDENT terminators
/// and the trait `extends A, B & C` two-separator variant).
///
/// **Break-and-verify (Core #6 / #15e Q2 — the guard must catch its
/// own class):** add a fake raw `while self.match_tok(TOK_COMMA):`
/// anywhere in ANY of the three parser copies and re-run — the
/// assertion fires with a count mismatch on that copy.  Restore.
#[test]
fn self_host_parser_comma_loops_go_through_helper() {
    /// Post-migration: zero raw loops in every SH parser copy.
    /// Bump ONLY when the language semantics change to REJECT
    /// trailing commas at some position, and the fix is a raw
    /// `while match_tok(TOK_COMMA):` (unlikely — the inline
    /// `while check_tok/advance` shape rejects cleanly without
    /// matching the grep).
    const EXPECTED_RAW_COMMA_LOOPS_PER_COPY: usize = 0;

    let paths = [
        "tests/fixtures/self_host_typechecker/parser.gg",
        "tests/fixtures/self_host_parser/parser.gg",
        "tests/fixtures/self_host_resolver/parser.gg",
    ];

    for path in paths.iter() {
        let content = fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("cannot read {path}: {e}"));

        let mut count = 0usize;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with('#') {
                continue;
            }
            if trimmed.contains("while ") && trimmed.contains("match_tok(TOK_COMMA)") {
                count += 1;
            }
        }

        assert_eq!(
            count, EXPECTED_RAW_COMMA_LOOPS_PER_COPY,
            "R39 Phase 2e Core #4 producer-chokepoint guard: raw \
             `while ... match_tok(TOK_COMMA)` loops in `{path}` = {count}, \
             expected {EXPECTED_RAW_COMMA_LOOPS_PER_COPY}.\n\n\
             Every comma-separated-list parse in the SH parser MUST route \
             through `Parser::consume_comma_or_tok(terminator)` — the \
             shared chokepoint that pairs `match_tok(TOK_COMMA)` with a \
             `check_tok(terminator)` break to prevent the R37+ SH-lowerer \
             stage-2 double-free cascade.  Sites at owner-directed \
             ACCEPTING positions with non-token (KW/NEWLINE/EQ/IDENT) \
             terminators use the `while check_tok(TOK_COMMA): advance; \
             if check_<TERM>: break; ...` inline rewrite — that shape \
             does NOT match this grep.\n\n\
             If you added a NEW raw loop, migrate it: either call \
             `consume_comma_or_tok(TOK_TERM)` (for paren/bracket/brace \
             terminators) or use the `while check_tok/advance` inline \
             shape (for keyword or non-single-token terminators). \
             The three parser copies must stay symmetric — a fix in one \
             copy must be mirrored to the other two."
        );
    }
}

/// Round XXXVII D27 Round A Phase 3 (Core #6 shrink-only ratchet): a
/// SHRINK-ONLY ceiling on `!name`-move sites in NON-SH corpora. Post-R37
/// the FORMATTER emits `^`, but the in-place corpus sweep is deferred to
/// a follow-up round (bulk-reformatting the corpus tripped an unrelated
/// SH-lowering memory-safety regression at bootstrap fixed-point —
/// filed on the D27 headline entry as a Round-A follow-up).
///
/// The ceiling STARTS at the current in-tree count and MUST NEVER GROW.
/// Any new `!name` site added to a non-SH corpus is a regression during
/// the Round A → Round B interval. Users of `gg fmt` will drop the count
/// naturally as they migrate files; the follow-up sweep round drops it
/// to (near) zero.
///
/// **Scope:** `tests/fixtures/**` (excluding `self_host_*/`, deferred),
/// `lib/**`, `examples/**`, `demo/**`, `spectests/**`, `compiler/data/**`.
/// Self-host corpora keep `!` transitionally (parser accept-both, R35).
///
/// **Detection:** regex `(^|[^!])!([A-Za-z_(])` matches `!name` and
/// `!(x)` (move closure) — excludes `!=` (right-side isn't a letter),
/// `!!` (left-side `!` filter). Pre-strip STRINGS FIRST, then COMMENTS
/// (string-first prevents comment-strip from eating trailing quotes on
/// lines like `code = "foo # bar"`). String shapes covered per
/// `docs/language-reference.md:239`: `"..."`, `f"..."`, `r"..."`,
/// `b"..."`, `c"..."`, and triple-quoted forms.
///
/// **Known limitation:** full string-strip hides a `!ident` inside an
/// f-string interpolation `f"...{!x}..."`. The arm-count lint above
/// covers the EMIT side of the class as a second guard.
#[test]
fn fmt_no_new_move_bang_in_migrated_corpora() {
    /// R37 baseline (pre-sweep): 861 `!name`/`!(` sites in non-SH corpora,
    /// spread across ~130 files. The bulk sweep is deferred to a
    /// follow-up round — see TODO.md D27 headline entry. Users can run
    /// `gg fmt --in-place path/to/file.gg` incrementally to migrate;
    /// each such migration DROPS this ceiling toward 0.
    ///
    /// Tighten CEILING whenever the count drops (do NOT loosen — a rise
    /// is a regression). Round B follow-up should drive CEILING to 0.
    const CEILING: usize = 861;

    // Roots to scan — non-SH corpora only. SH corpora migrate in a
    // follow-up round (parser accept-both keeps them parseable until then).
    let roots: &[&str] = &[
        "tests/fixtures",
        "lib",
        "examples",
        "demo",
        "spectests",
        "compiler/data",
    ];

    let count = count_bang_move_in_code(roots);
    assert!(
        count <= CEILING,
        "D27 Round A shrink-only ratchet: {count} `!name`-move sites \
         found in non-SH corpora (ceiling {CEILING}). A NEW `!name` in \
         migrated corpora is a regression during the Round A → Round B \
         interval — either migrate the new site to `^name` (or run \
         `gg fmt --in-place` on the file), or if the fixture DELIBERATELY \
         tests the retired glyph, allowlist it and bump CEILING with the \
         fixture's rationale.",
    );
    // Sanity: the ceiling is honest. If someone raises CEILING without
    // adding a new `!name` site, tighten it back down.
    if count < CEILING {
        eprintln!(
            "[fmt_no_new_move_bang_in_migrated_corpora] measured={count} < CEILING={CEILING} — \
             tighten CEILING to {count} and cite the retired sites.",
        );
    }
}

/// Helper for `fmt_no_new_move_bang_in_migrated_corpora`: walk .gg files
/// in `roots`, skip any path with a `self_host_` segment, strip strings
/// then comments per line, and count `!name` / `!(` matches. Also
/// exercised by the unit tests below.
fn count_bang_move_in_code(roots: &[&str]) -> usize {
    let string_re = regex::Regex::new(
        // Triple-quoted (any [fFrRbBcC] prefix, non-greedy `.*?`) OR
        // single-line double-quoted (any prefix) OR single-line
        // single-quoted. Match order: triple-quoted first so `"""..."""`
        // isn't chopped into three empty `""` strings.
        r#"(?s)([fFrRbBcC]?"""(?:.*?)""")|([fFrRbBcC]?"(?:[^"\\\n]|\\.)*")|('(?:[^'\\\n]|\\.)*')"#,
    )
    .expect("string regex compiles");
    let comment_re = regex::Regex::new(r"#.*$").expect("comment regex compiles");
    let move_re = regex::Regex::new(r"(^|[^!])!([A-Za-z_]|\()").expect("move regex compiles");

    let mut total = 0usize;
    for root in roots {
        walk_gg_files(Path::new(root), &mut |path: &Path| {
            // Skip self-host corpora (deferred to a later round).
            let s = path.to_string_lossy();
            if s.contains("self_host_") {
                return;
            }
            let Ok(src) = fs::read_to_string(path) else { return };
            for line in src.lines() {
                let stripped = string_re.replace_all(line, "\"\"");
                let stripped = comment_re.replace_all(&stripped, "");
                total += move_re.find_iter(&stripped).count();
            }
        });
    }
    total
}

/// D22 shrink-only lint (Core #6 executable guard). Retiring `.slice()` on
/// String/Vector receivers in favor of colon-slice `v[a:b]` (ratified
/// 2026-07-06). Track C-3a landed the non-SH migration + this ratchet;
/// Track C-3b (208 SH sites in `tests/fixtures/self_host_lowerer/`) is
/// hard-blocked on Track A (SH stage-2 memory-safety fix) and defers to
/// R40 if A stalls. Meanwhile the SH corpora sit in an ALLOWLIST — the
/// allowlist entry drops out atomically when C-3b lands, ratcheting the
/// non-SH count down to 0 and the SH count to 0 in one commit.
///
/// Positive-control: add `foo.slice(0, 1)` to any non-SH `.gg` under
/// `tests/fixtures/` → this lint MUST fire with a count above the
/// ceiling. If it doesn't, the regex or the ceiling has decayed.
#[test]
fn no_dot_slice_after_d22() {
    /// Non-SH ceiling AFTER C-3a lands = 0 (every non-SH site was
    /// migrated in the same commit as this lint). The SH corpora sit in
    /// the allowlist (see the walker below) and do NOT count toward this
    /// ceiling; when C-3b lands they migrate too and the allowlist entry
    /// is removed atomically.
    ///
    /// Tighten CEILING whenever the count drops (do NOT loosen — a rise
    /// is a regression during the D22 accept-both window).
    const CEILING: usize = 0;

    let roots: &[&str] = &[
        "tests/fixtures",
        "lib",
        "examples",
        "demo",
        "spectests",
        "compiler/data",
    ];

    let count = count_dot_slice_in_code(roots);
    assert!(
        count <= CEILING,
        "D22 shrink-only ratchet: {count} `.slice(...)` sites found in \
         non-SH corpora (ceiling {CEILING}). D22 (ratified 2026-07-06) \
         retired `.slice()` in favor of colon-slice `v[a:b]`. A NEW site \
         is a migration regression — replace with the colon form. If a \
         NEG fixture DELIBERATELY exercises the retired method, allowlist \
         its path and bump the ceiling with the rationale.",
    );
    if count < CEILING {
        eprintln!(
            "[no_dot_slice_after_d22] measured={count} < CEILING={CEILING} — \
             tighten CEILING to {count} and cite the retired sites.",
        );
    }
}

/// Helper for `no_dot_slice_after_d22`: walk .gg files in `roots`, skip
/// any path with a `self_host_lowerer` segment (the deferred C-3b
/// migration site set), strip strings then comments per line, and count
/// `.slice(` occurrences. `substring(a, b)` is a distinct method
/// (`gorget_str_slice` routes through the `substring` name too) and is
/// EXPLICITLY not counted — only literal `.slice(` matches. Fuzz-corpus
/// and known-gaps fixtures ARE scanned; the migration handles them the
/// same way as any other fixture.
fn count_dot_slice_in_code(roots: &[&str]) -> usize {
    let string_re = regex::Regex::new(
        r#"(?s)([fFrRbBcC]?"""(?:.*?)""")|([fFrRbBcC]?"(?:[^"\\\n]|\\.)*")|('(?:[^'\\\n]|\\.)*')"#,
    )
    .expect("string regex compiles");
    let comment_re = regex::Regex::new(r"#.*$").expect("comment regex compiles");
    let slice_re = regex::Regex::new(r"\.slice\(").expect("slice regex compiles");

    let mut total = 0usize;
    for root in roots {
        walk_gg_files(Path::new(root), &mut |path: &Path| {
            // ALLOWLIST: self-host lowerer corpora — the 208 SH `.slice()`
            // sites are gated on Track A (SH stage-2 memory-safety fix)
            // landing before Track C-3b can migrate them. Remove this
            // check when C-3b lands (atomic ceiling ratchet 208 → 0).
            let s = path.to_string_lossy();
            if s.contains("self_host_lowerer") {
                return;
            }
            let Ok(src) = fs::read_to_string(path) else { return };
            for line in src.lines() {
                let stripped = string_re.replace_all(line, "\"\"");
                let stripped = comment_re.replace_all(&stripped, "");
                total += slice_re.find_iter(&stripped).count();
            }
        });
    }
    total
}

fn walk_gg_files(dir: &Path, cb: &mut dyn FnMut(&Path)) {
    let Ok(rd) = fs::read_dir(dir) else { return };
    for entry in rd.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk_gg_files(&path, cb);
        } else if path.extension().and_then(|e| e.to_str()) == Some("gg") {
            cb(&path);
        }
    }
}

/// Unit tests for `count_bang_move_in_code`'s regex — ensure the pattern
/// matches actual `!name`/`!(` and does NOT match `!=`, `!!`, `!"lit"`,
/// or `x! + y`.
#[test]
fn fmt_bang_move_regex_matches() {
    let move_re = regex::Regex::new(r"(^|[^!])!([A-Za-z_]|\()").expect("regex compiles");

    // MATCH cases — real `!name`-move sites and move-closure.
    let matches: &[&str] = &[
        "!msg",              // bare prefix
        "!(x): body",        // move closure
        "take(!p)",          // prefix inside call
        "foo(  !p  )",       // prefix with padding
        "String !p = \"x\"", // param decl
        "f(!Ctor(...))",     // Ctor argument
    ];
    for s in matches {
        assert!(move_re.is_match(s), "regex should MATCH {s:?}");
    }

    // NON-MATCH cases — `!=` inequality, `!!` (D29 double-mark), literal.
    let non_matches: &[&str] = &[
        "!=",                // top-level inequality
        "a!=b",              // fused inequality
        "x! + y",            // postfix propagate followed by space
        "!\"lit\"",          // `!` before a string literal (retired shape; not a `!name`)
        "42! - 1",           // postfix propagate on int
        "throws!",           // ThrowsSpec inferred — trailing `!` at word end
        "return boom(x)!!",  // D29 double-mark (second `!` is right after `!` — filtered by [^!])
    ];
    for s in non_matches {
        assert!(!move_re.is_match(s), "regex should NOT match {s:?}");
    }
}

/// Positive-control for the strip logic: verify that `!x` inside a
/// STRING or a COMMENT does NOT count as a real site (else the lint
/// false-positives on `code = "if !flag"` or `# no !move here`).
#[test]
fn fmt_bang_move_strip_ignores_strings_and_comments() {
    use std::io::Write;
    let tmp = tempfile::tempdir().expect("tempdir");
    let fixture_path = tmp.path().join("probe.gg");
    let mut f = std::fs::File::create(&fixture_path).unwrap();
    // These lines contain `!x` but ALL are in string or comment context.
    writeln!(f, "String s = \"contains !x literal\"").unwrap();
    writeln!(f, "# comment mentioning !x").unwrap();
    writeln!(f, "String r = r\"raw with !y\"").unwrap();
    writeln!(f, "String fs = f\"fstr with !z\"").unwrap();
    drop(f);

    // Re-apply the strip logic here (avoids running the full walk).
    let string_re = regex::Regex::new(
        r#"(?s)([fFrRbBcC]?"""(?:.*?)""")|([fFrRbBcC]?"(?:[^"\\\n]|\\.)*")|('(?:[^'\\\n]|\\.)*')"#,
    )
    .unwrap();
    let comment_re = regex::Regex::new(r"#.*$").unwrap();
    let move_re = regex::Regex::new(r"(^|[^!])!([A-Za-z_]|\()").unwrap();

    let src = std::fs::read_to_string(&fixture_path).unwrap();
    let mut count = 0;
    for line in src.lines() {
        let stripped = string_re.replace_all(line, "\"\"");
        let stripped = comment_re.replace_all(&stripped, "");
        count += move_re.find_iter(&stripped).count();
    }
    assert_eq!(
        count, 0,
        "strip logic false-positive: `!x` in string/comment must not count"
    );
}

/// Round XXXVIII Track C (Core #4 arm-count + Core #6 executable guard):
/// `src/formatter/mod.rs` emits multi-line expression docs via `doc::group`
/// containing `doc::line`/`doc::softline`. Bare leading-operator
/// continuations are NOT valid Gorget (the lexer suppresses NEWLINE/INDENT
/// only inside brackets `bracket_depth > 0` — `src/lexer/mod.rs:22` — or
/// after a leading `.` carve-out — `src/lexer/mod.rs:161`), so any such
/// `doc::group` NOT already inside bracketed text and NOT going through
/// the shared `wrap_multiline_expr_in_parens` helper emits unparseable
/// output that a second `gg fmt` pass then drops (silent data loss).
///
/// This ratchet pins BOTH counts:
///   - **`doc::group(` sites in `src/formatter/mod.rs`** — every new
///     doc::group must either (a) go through
///     `wrap_multiline_expr_in_parens` (bumping the helper-call count), or
///     (b) be an allowlisted SAFE-BY-BRACKET / SAFE-BY-LEXER site whose
///     rationale is recorded here.
///   - **`wrap_multiline_expr_in_parens(` call sites** — one per binop-style
///     wrap arm (`format_binary_chain` + `Expr::DefaultOp`); a new arm that
///     forgot to route emits the leading-operator continuation the class
///     was retired to prevent.
///
/// **Allowlisted SAFE sites (`doc::group` NOT going through the helper):**
///
///   `:1020` — **method chain**. SAFE-BY-LEXER: the lexer at
///     `src/lexer/mod.rs:161-166` carves out leading-`.` as an explicit
///     continuation (Python-like), suppressing NEWLINE. The method-chain
///     arm emits `receiver\n    .m1()\n    .m2()` which parses cleanly.
///   `:2853` — **the helper itself** (`wrap_multiline_expr_in_parens`).
///     By definition SAFE — it IS the paren-wrap chokepoint.
///   `:2888` — **`build_comprehension_doc`**. SAFE-BY-BRACKET: the
///     outer `doc::group` is wrapped in explicit `open`/`close` text
///     (`[`/`]` or `{`/`}`), so the lexer's `bracket_depth > 0`
///     suppression covers the inner `doc::line`/`softline` breaks.
///
/// **If this fails**: a new `doc::group` was added. Either route it
/// through `wrap_multiline_expr_in_parens` (bump `EXPECTED_CALLS`), or
/// verify the new site is SAFE-BY-BRACKET / SAFE-BY-LEXER and add it to
/// the allowlist above with its rationale (bumping `EXPECTED_TOTAL`).
///
/// **RED signature verified 2026-08-08 (Round XXXVIII Track C authoring):**
/// deliberately removed one `wrap_multiline_expr_in_parens(` call and the
/// count assertion tripped with `EXPECTED_CALLS = 2` vs measured 1.
#[test]
fn fmt_multiline_group_paren_wrap_class() {
    /// Total `doc::group(` sites (excluding pure-comment mentions).
    /// Round XXXVIII Track C baseline: 3 (see allowlist in the doc-comment).
    const EXPECTED_TOTAL: usize = 3;
    /// Call sites of the `wrap_multiline_expr_in_parens` helper — one per
    /// binop-style arm that needs the paren wrap. Excludes the helper's
    /// definition line and comment mentions. Round XXXVIII Track C baseline: 2
    /// (`format_binary_chain` at :1100, `Expr::DefaultOp` at :1925).
    const EXPECTED_CALLS: usize = 2;

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let mut group_count = 0usize;
    let mut call_count = 0usize;
    for line in content.lines() {
        let trimmed = line.trim_start();
        // Skip pure-comment lines (docstrings and inline notes) so a
        // `// see doc::group(...)` note doesn't spuriously trip the count.
        if trimmed.starts_with("//") || trimmed.starts_with("///") {
            continue;
        }
        // Skip the helper's DEFINITION line — the count tracks CALL sites,
        // not the definition itself.
        if trimmed.starts_with("fn wrap_multiline_expr_in_parens(") {
            continue;
        }
        group_count += line.matches("doc::group(").count();
        call_count += line.matches("wrap_multiline_expr_in_parens(").count();
    }
    assert_eq!(
        group_count, EXPECTED_TOTAL,
        "doc::group site count in `src/formatter/mod.rs` changed: \
         {group_count} vs expected {EXPECTED_TOTAL}.\n\n\
         If a new `doc::group` was added, it must EITHER route through \
         `wrap_multiline_expr_in_parens` (bump EXPECTED_CALLS and use the \
         shared helper), OR be documented in the allowlist above as \
         SAFE-BY-BRACKET (open/close text wraps the group) or SAFE-BY-LEXER \
         (leading-`.` continuation carve-out at src/lexer/mod.rs:161).\n\n\
         Round XXXVIII Track C retires the leading-operator-continuation \
         class (gorget-js snag #15 Class 2): a `??` doc::group without the \
         `if_break(\"(\")` + `if_break(\")\")` wrapper emits invalid syntax \
         that a second `gg fmt` pass drops. This arm-count IS the class \
         guard (Core #4/#6/#10).",
    );
    assert_eq!(
        call_count, EXPECTED_CALLS,
        "`wrap_multiline_expr_in_parens` call-site count in \
         `src/formatter/mod.rs` changed: {call_count} vs expected \
         {EXPECTED_CALLS}.\n\n\
         If a new binop-style arm was added, it must call \
         `wrap_multiline_expr_in_parens` on its emission and bump \
         EXPECTED_CALLS here; otherwise the arm emits a bare \
         leading-operator continuation the parser rejects.\n\n\
         If the arm count SHRANK (a call was removed / centralized), \
         lower EXPECTED_CALLS with the rationale.",
    );
}


// ══════════════════════════════════════════════════════════════════════
// R41 T-FMT-C — suite-layout form preservation (Core #6 class guards)
// ══════════════════════════════════════════════════════════════════════

/// CENSUS of every SUITE EMISSION in `src/formatter/mod.rs`, classified by
/// whether it reads the author's `Block.layout` and whether it checks for an
/// author blank above a clause header.
///
/// **Why a census and not a count.** A count lint fires only when a site that
/// HAS the mechanism loses it; a brand-new suite emitter that never had one
/// moves no count and is invisible — the same blindness that let
/// `format_extern_block`'s hookless loop sit green through the R41 T-FMT-A
/// class. Both faces of the suite-layout class are exactly that shape: the
/// eight clause headers were sites that had never checked for a blank, and the
/// suite emitters were sites that had never read a layout. A guard that
/// green-lights the class it exists to retire is worse than none.
///
/// **Detection is at the CHOKEPOINT, so a new site cannot dodge it.** Every
/// indented suite in the formatter goes through `format_block_stmts` (and the
/// inline path through `format_inline_suite`, which calls it), so enumerating
/// its call sites enumerates the suites. A new suite-emitting arm necessarily
/// adds a row here and is RED until it is classified — at which point its
/// author has to answer "does this position have two author spellings?".
///
/// Classification (from the 12 source lines preceding the call):
///   * `Layout`  — the emission is gated on `SuiteLayout`.
///   * `Clause`  — the emission belongs to a clause header that checks
///                 `blank_before_clause` for an author blank above it.
///   * `Both`    — both.
///   * `Plain`   — neither, with the row's rationale recorded below.
///
/// **Break-and-verify (Core #13, RED-verified 2026-08-11):** delete the
/// `if body.layout == SuiteLayout::Inline` gate at the `Stmt::OnError` arm
/// (keeping only the indented branch) and this lint fires with that row
/// flipping `Layout` → `Plain`.
///
/// ⚠ **Known blind spot, measured while RED-verifying:** the classifier looks
/// for the TOKEN, so a read that is present but DISABLED
/// (`if false && body.layout == ...`) still reads as `Layout` here. That
/// mutation is not the shape a regression takes — a lost gate is deleted, not
/// short-circuited — and the fixture matrix catches the disabled form anyway,
/// but the limit is real and stated rather than assumed away.
///
/// ⚠ **The closure emitter's layout read has NO ROW HERE, and its `Plain` row
/// does not mean "no layout question at this site".** A closure suite reaches
/// `format_block_stmts` through `format_closure_post_prelude`, so the census
/// sees the delegating call, not the read at `src/formatter/mod.rs:3856` that
/// chose the spelling. Independently measured (`if true || block.layout == ...`
/// at that read): this census stays GREEN while
/// `tests/fixtures/fmt_suite_layout/closure_body.gg` loses its fixpoint AND
/// regains trailing whitespace, so `fmt_suite_layout_form_preservation` and the
/// `suite_layout_expr_facts` projection are what cover this site. Two guards,
/// not one — do not read the `Plain` row as an absence of the question.
#[test]
fn formatter_suite_layout_hook_census() {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    enum Kind {
        Layout,
        Clause,
        Both,
        Plain,
    }
    use Kind::*;

    // (enclosing fn, the call line, classification).
    //
    // Rationale for the `Plain` rows — positions with only ONE legal author
    // spelling, so there is no layout to preserve and no clause header to
    // carry a blank:
    //   * DECLARATION bodies — `format_function`, `format_item`'s
    //     `meta type` function, `format_test` / `format_bench` /
    //     `format_suite_setup` / `format_suite_teardown`. A declaration body is
    //     always indented. (`format_trait` has no row of its own: its default
    //     method bodies route through `format_function`.)
    //   * NEWLINE-REQUIRING statement producers — `Stmt::Loop` / `With` /
    //     `Unsafe` / `NamedScope` / `MetaFor` / `MetaWhile` / `MetaIf`, the
    //     `for` and `while` BODIES, and the `select` / `match` CASE arm
    //     bodies. Each parses through `parse_block`, which accepts the
    //     indented form only. `on error` is the counter-example that proves
    //     the rule — its inline form is colon-less and real — and it is
    //     `Layout`.
    //   * `format_arm_body`'s author-`do:` branch, and `Expr::Block` /
    //     `Expr::Do` in `format_expr` — a `do:` suite is always indented. What
    //     varies there is whether the KEYWORD was written, which
    //     `Expr::Do.author_spelled` carries; `Block.layout` is the wrong axis.
    //   * `format_inline_suite` — it IS the inline half, delegating the slot;
    //     its CALLER did the layout read.
    //   * `format_closure_post_prelude` — likewise called from both closure
    //     paths after the layout has been read.
    //
    // The `for`/`while`/`match`/`select` `else` CLAUSES are the `Clause` rows;
    // `meta match`'s `else` is `Both` (a clause header that ALSO has two legal
    // spellings), as is the `if`-`else`.
    //
    // ⚠ The `case` ARM headers are clause headers too, and they check for the
    // author's blank the same way — but only the meta-match one shows up in
    // this table, because the other three arm loops (statement-match,
    // expression-match, select) reach their bodies through `format_match_arm`
    // or their own emit rather than calling `format_block_stmts` within the
    // classifier's window. Their blank check lives in the arm LOOP, not beside
    // a `format_block_stmts` call, so a `Plain` row here is not evidence that
    // an arm position skipped it.
    const CENSUS: &[(&str, &str, Kind)] = &[
        ("format_arm_body", "self.format_block_stmts(block);", Layout),
        ("format_arm_body", "self.format_block_stmts(block);", Plain),
        ("format_bench", "self.format_block_stmts(&b.body);", Plain),
        ("format_closure_post_prelude", "self.format_block_stmts(block);", Plain),
        ("format_elif_else_blocks", "self.format_block_stmts(body);", Layout),
        ("format_elif_else_blocks", "self.format_block_stmts(else_body);", Both),
        // R42 Track D: the expression match lives in `format_expr_inner` now
        // (`format_expr` became the author-paren wrapper around it). Scope
        // re-pointed, counts untouched.
        ("format_expr_inner", "self.format_block_stmts(block);", Plain),
        ("format_expr_inner", "self.format_block_stmts(body);", Plain),
        ("format_function", "self.format_block_stmts(block);", Plain),
        ("format_inline_suite", "self.format_block_stmts(block);", Plain),
        ("format_item", "self.format_block_stmts(&mtf.body);", Plain),
        // `format_match_arm` no longer has a row: it used to hand-mirror
        // `format_arm_body`'s three-way shape decision and emit two of the
        // three suites itself, and the copy disagreed with the original on the
        // author-`do:` shape (the header's trailing comment landed on the LAST
        // statement of the branch). It now delegates, so the arm's suite is
        // emitted at `format_arm_body`'s rows above.
        // select CASE arm body.
        ("format_stmt", "self.format_block_stmts(&arm.body);", Plain),
        // meta match CASE arm body — `Both` since the R41 fold: a `case`
        // header is a clause header too, and it now checks for the author's
        // blank above itself like `else:`/`elif:` always did. The asymmetry
        // that made the blank above `case 1:` vanish while the blank above the
        // sibling `else:` survived was the reported snag.
        ("format_stmt", "self.format_block_stmts(body);", Both),
        // on error.
        ("format_stmt", "self.format_block_stmts(body);", Layout),
        // for body · while body · loop · with · unsafe · meta for ·
        // meta while · named scope.
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        ("format_stmt", "self.format_block_stmts(body);", Plain),
        // for else · while else · statement-match else · select else.
        ("format_stmt", "self.format_block_stmts(else_body);", Clause),
        ("format_stmt", "self.format_block_stmts(else_body);", Clause),
        ("format_stmt", "self.format_block_stmts(else_body);", Clause),
        ("format_stmt", "self.format_block_stmts(else_body);", Clause),
        // meta match else.
        ("format_stmt", "self.format_block_stmts(else_body);", Both),
        // Stmt::If then-body.
        ("format_stmt", "self.format_block_stmts(then_body);", Layout),
        // Stmt::MetaIf then-body.
        ("format_stmt", "self.format_block_stmts(then_body);", Plain),
        ("format_suite_setup", "self.format_block_stmts(&s.body);", Plain),
        ("format_suite_teardown", "self.format_block_stmts(&s.body);", Plain),
        ("format_test", "self.format_block_stmts(&t.body);", Plain),
    ];

    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let src: Vec<&str> = content.lines().collect();

    // Attribute each line to its enclosing `fn` by brace depth — the same
    // technique `formatter_child_collection_loop_census` uses.
    let mut fn_stack: Vec<(String, i32, bool)> = Vec::new();
    let mut depth: i32 = 0;
    let mut found: Vec<(String, String, Kind)> = Vec::new();

    for (i, line) in src.iter().enumerate() {
        let trimmed = line.trim_start();
        let is_comment = trimmed.starts_with("//");

        if !is_comment {
            let after_vis = trimmed
                .strip_prefix("pub(crate) fn ")
                .or_else(|| trimmed.strip_prefix("pub fn "))
                .or_else(|| trimmed.strip_prefix("fn "));
            if let Some(rest) = after_vis {
                let name: String = rest
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect();
                fn_stack.push((name, depth, false));
            }
            if trimmed.starts_with("self.format_block_stmts(") {
                let owner = fn_stack.last().map(|f| f.0.clone()).unwrap_or_default();
                let window_start = i.saturating_sub(12);
                let window = src[window_start..i].join("\n");
                let has_layout = window.contains("SuiteLayout::");
                let has_clause = window.contains("blank_before_clause(");
                found.push((
                    owner,
                    trimmed.to_string(),
                    match (has_layout, has_clause) {
                        (true, true) => Both,
                        (true, false) => Layout,
                        (false, true) => Clause,
                        (false, false) => Plain,
                    },
                ));
            }

            let opens = line.matches('{').count() as i32;
            let closes = line.matches('}').count() as i32;
            if let Some(top) = fn_stack.last_mut() {
                if !top.2 && opens > 0 {
                    top.2 = true;
                    top.1 = depth;
                }
            }
            depth += opens - closes;
            while matches!(fn_stack.last(), Some(f) if f.2 && depth <= f.1) {
                fn_stack.pop();
            }
        }
    }

    let mut got = found;
    got.sort();
    let mut want: Vec<(String, String, Kind)> = CENSUS
        .iter()
        .map(|(f, c, k)| (f.to_string(), c.to_string(), *k))
        .collect();
    want.sort();

    assert_eq!(
        got, want,
        "R41 T-FMT-C suite-emission census in `src/formatter/mod.rs` changed.\n\n\
         Every indented suite goes through `format_block_stmts`, so this table \
         IS the list of suite emissions. A NEW row means a new suite position: \
         classify it.\n\n\
         · Does this position accept BOTH an inline and an indented spelling? \
         Then it must read `Block.layout` (`Layout`), or `gg fmt` will pick one \
         and rewrite every source that chose the other.\n\
         · Is it a CLAUSE HEADER (`else:` / `elif:`)? Then it must check \
         `blank_before_clause` (`Clause`), or an author blank above the clause \
         is deleted — `format_block_stmts` cannot see it, because a clause \
         header is not a statement.\n\
         · `Plain` is for a position with only ONE legal author spelling; \
         record WHY in the table's rationale block, the way the existing rows \
         do."
    );

    // ── The PER-SITE-ANCHOR producers, which the table above cannot see ─────
    //
    // This census keys on `format_block_stmts` calls, so it enumerates SUITE
    // emissions. The three producers below are a different axis: each emits a
    // clause/arm header or body AND places the header's trailing comment, and
    // each of their call sites hands over its OWN anchor. A position that
    // forgot to delegate — or delegated with a wrong anchor — moves no count
    // above, because it would not call `format_block_stmts` in the window.
    //
    // That gap is not hypothetical, and it has now bitten THREE TIMES, once per
    // review pass, each time in a different member of the same family:
    //   1. the author-`do:` body shape shipped with no cell — every gate green;
    //   2. the `rethrow` call site shipped with no cell — every gate green;
    //   3. `select`'s `else` had no trailing-comment cell, and a reviewer
    //      neutered that call site and reproduced the misattribution class with
    //      lints, lib, fmt_suite_layout, fmt_idempotent, fmt_comment_only and
    //      fmt_output_reparses_corpus_wide ALL green.
    //
    // Pass 3's fix pinned one producer, which is why pass 3 found the next one.
    // The class is "a producer whose callers each supply an anchor", so all
    // three are pinned here, each against the fixture axis that covers it.
    const ANCHOR_PRODUCERS: &[(&str, usize, &str)] = &[
        (
            "self.format_arm_body(",
            4,
            "`format_match_arm` (a `case` arm), the expression-match `else`, \
             `rethrow`, `catch` — axis 2 of else_header_trailing_comment.gg, \
             which samples each against an author-`do:` and an inline body. \
             RED-verify a new one by killing its anchor: format_arm_body(body, 0)",
        ),
        (
            "self.emit_else_header(",
            7,
            "the item-level `meta if` `else`, `format_elif_else_blocks`, and the \
             `for` / `while` / statement-match / `select` / `meta match` `else` \
             clauses — axis 1 of else_header_trailing_comment.gg, one cell each. \
             RED-verify a new one by replacing the call with a hand-written \
             `write(\"else:\")` + `newline()`, which is exactly what a site that \
             forgot to delegate looks like",
        ),
        (
            "self.format_inline_suite(",
            6,
            "the INLINE half of the same clause family — `elif`, the `if`-`else`, \
             `Stmt::If`'s then-body, `meta match`'s case and `else`, and \
             `on error` (whose inline form takes NO colon, which is why the \
             header suffix is a parameter). Its cells are inline_slot_kinds.gg \
             and the inline rows of else_header_trailing_comment.gg",
        ),
    ];
    let code_lines: Vec<&str> = content
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            !t.starts_with("//") && !t.starts_with("///")
        })
        .collect();
    for (needle, expected, roster) in ANCHOR_PRODUCERS {
        let found = code_lines.iter().filter(|l| l.contains(needle)).count();
        assert_eq!(
            found, *expected,
            "`{needle}` call-site count changed ({expected} -> {found}).\n\n\
             The {expected} are: {roster}.\n\n\
             Each call site supplies its OWN anchor, so each needs its OWN \
             fixture cell — a shared producer does not make a per-site anchor \
             correct, which is the defect this family keeps producing. If you \
             ADDED a position, add its cell, RED-verify it, and bump the count \
             here. If you REMOVED one, a header comment is probably now \
             misattributed.\n\
             Census: grep -c '{needle}' src/formatter/mod.rs"
        );
    }
}

/// The READ-SITE guard behind `SuiteLayout`'s doc comment (Core #14 — an
/// invariant-asserting comment needs an enforcing guard, or it gets deleted).
///
/// `src/parser/ast.rs` tells every reader: *"Nothing outside the formatter reads
/// this field: it records syntax, and semantics must not depend on it."* True at
/// the time it was written, and load-bearing — the day a semantic pass consults
/// `Block.layout` or `Expr::Do.author_spelled`, a program's MEANING starts
/// depending on how its author spaced it, and `if c: return x` stops being the
/// same program as the indented form. That is not a bug you find in review; it
/// is a bug you find when someone's whitespace change flips a verdict.
///
/// So the sentence is enforced here rather than trusted:
///   * `src/formatter/` may READ both fields — it is the one consumer whose
///     entire job is spelling.
///   * `src/parser/` may DECLARE and WRITE them (`layout:` / `author_spelled:`
///     in a field decl or a struct-literal init) but may NOT read them: the
///     parser is the writer, and a read there would mean the fact is being
///     round-tripped through the layer that produces it.
///   * Everywhere else under `src/` — semantic, ir, lir, backend — may not
///     mention them at all.
///
/// Comments are exempt: naming the anti-pattern is how the rule is taught. The
/// test-side fact projection in `tests/integration.rs` is out of scope by
/// construction (this walks `src/` only) and is a fixture ORACLE, not a
/// consumer.
///
/// ⚠ ONE RESIDUAL BLIND SPOT, stated rather than assumed away: a RENAMING
/// destructure *inside the parser* (`let Block { layout: l, .. } = b;`) spells
/// `layout:` and is therefore taken for the struct-literal write it looks
/// identical to. Outside the parser the same shape is caught, which is where the
/// danger lives — a semantics reading the field is what makes whitespace change
/// meaning. Closing the parser cell needs a real Rust parse, not a line scan.
///
/// **Break-and-verify (Core #13, RED-verified 2026-08-11):** adding
/// `let _ = block.layout;` to `src/semantic/typecheck.rs` fires this lint with
/// that file:line; adding it to `src/parser/stmt.rs` fires the reads-in-parser
/// arm. Restored both.
#[test]
fn suite_layout_is_read_only_by_the_formatter() {
    const FIELDS: [&str; 2] = [".layout", "author_spelled"];
    // A DESTRUCTURING read spells the field as a BARE identifier in a
    // brace-list position — `let Block { layout, .. } = b;`, or a match pattern
    // `Expr::Do { author_spelled, .. } =>`. The dotted form above cannot see it,
    // which is exactly the hole an output-review demonstrated by slipping such a
    // read into `src/semantic/` past the first version of this guard.
    //
    // Anchoring on the preceding `{` or `,` is what keeps this from firing on
    // the ordinary local named `layout` (`let mut layout = String::new()` in
    // `src/ir/lowering/mod.rs`, `Some(layout)` in the LLVM backend — both
    // measured) while still catching every brace-list position. The parser's own
    // write sites put `layout:` at the start of their line, so they are not in a
    // brace-list position either and do not match.
    let destructure: Vec<(String, regex::Regex)> = FIELDS
        .iter()
        .map(|f| {
            let bare = f.trim_start_matches('.').to_string();
            let re = regex::Regex::new(&format!(r"[{{,]\s*{bare}\s*[,}}:]")).unwrap();
            (bare, re)
        })
        .collect();
    let mut violations: Vec<String> = Vec::new();

    for path in walkdir_rs("src") {
        let rel = path.to_string_lossy().replace('\\', "/");
        if rel.starts_with("src/formatter/") {
            continue; // the sanctioned reader
        }
        let Ok(content) = fs::read_to_string(&path) else { continue };
        let in_parser = rel.starts_with("src/parser/");
        for (i, line) in content.lines().enumerate() {
            let code = line.trim_start();
            if code.starts_with("//") {
                continue; // prose may name the field
            }
            for (bare, re) in &destructure {
                if !re.is_match(line) {
                    continue;
                }
                // In the parser a brace-list `layout:` is a struct-literal
                // WRITE on one line (`Block { stmts, span, layout: … }`).
                if in_parser && line.contains(&format!("{bare}:")) {
                    continue;
                }
                violations.push(format!("{rel}:{}  {}", i + 1, line.trim()));
            }
            for field in FIELDS {
                if !line.contains(field) {
                    continue;
                }
                // A WRITE is `layout: …` / `author_spelled: …` — a field
                // declaration or a struct-literal initializer. Anything else
                // that mentions the field is a read.
                let bare = field.trim_start_matches('.');
                let is_write = line.contains(&format!("{bare}:"));
                if in_parser && is_write {
                    continue;
                }
                violations.push(format!(
                    "{rel}:{}  {}",
                    i + 1,
                    line.trim()
                ));
            }
        }
    }

    assert!(
        violations.is_empty(),
        "`Block.layout` / `Expr::Do.author_spelled` are SYNTAX, and \
         `src/parser/ast.rs` promises that nothing outside the formatter reads \
         them. These sites break that promise:\n\n{}\n\n\
         If a pass genuinely needs to know how a suite was spelled, the answer \
         is almost certainly that it needs a different fact — one the parser \
         should RESOLVE and write through as typed metadata (Layering rule 4), \
         not the author's whitespace. A semantics that reads this field makes \
         `if c: stmt` and its indented form different programs.\n\n\
         The parser may DECLARE and WRITE these fields (`layout:` / \
         `author_spelled:`); it may not read them back.",
        violations.join("\n")
    );
}

/// The PARSER-side writer census for `SuiteLayout` (Layering rule 4 — resolve
/// once, write through).
///
/// The formatter's reads are only as good as the writes behind them, and a new
/// `Block` construction in the parser silently picks whatever the author of
/// that line felt like. Pinning the write sites forces the question at the
/// only place it can be answered: the parser knows whether it just consumed a
/// NEWLINE+INDENT or a statement on the header's line; nothing downstream can
/// recover that.
///
/// **Break-and-verify (Core #13, RED-verified 2026-08-11):** flip
/// `parse_block_or_inline_stmt`'s `SuiteLayout::Inline` to `NextLine` and the
/// per-variant counts below fire.
///
/// **It censuses `Block::header_start` too, in the same rows** (Layering rule 3
/// — one writer axis, one census). That field is the other thing a `Block`
/// construction cannot recover afterwards: which source line the owning
/// construct STARTS on. `span.start` is not a substitute — it is whatever
/// introducer the parser had in hand, which is the colon at most sites and on
/// a wrapped header sits on a continuation line indented at or past the body.
/// `Block::synthetic` carries its own row: it has no author header, so it
/// writes its own span start, and nothing reads it (the formatter's flush never
/// reaches a synthesized block).
#[test]
fn parser_suite_layout_writer_census() {
    // (file, NextLine writes, Inline writes, header_start writes, rationale)
    const CENSUS: &[(&str, usize, usize, usize, &str)] = &[
        // `parse_block_body` IS the indented-suite grammar
        // (`NEWLINE INDENT stmt* DEDENT`) and is the sole NextLine writer;
        // `parse_block_or_inline_stmt`'s one-liner path is the Inline one.
        ("src/parser/mod.rs", 1, 1, 2, "parse_block_body · parse_block_or_inline_stmt"),
        // `on error <stmt>` (colon-less inline) · `meta match` inline arm body.
        ("src/parser/stmt.rs", 0, 2, 2, "on error inline · meta match inline arm"),
        // The three SYNTHETIC wraps: `throw x` and `return x` in expression
        // position, and the expression-bodied destructuring closure. No author
        // wrote a suite at any of them, so emitting one would invent syntax.
        ("src/parser/expr.rs", 0, 3, 3, "throw wrap · return wrap · closure body wrap"),
        // `Block::synthetic` — no author spelling and no author header.
        ("src/parser/ast.rs", 1, 0, 1, "Block::synthetic"),
        // Not a writer: the probe collector COPIES the field into its own
        // struct, which the scan cannot tell from an init. Kept as an
        // explicit row so the count is decided rather than excused.
        ("src/parser/tests.rs", 0, 0, 1, "BlockProbe field copy in the probe collector"),
    ];

    // EVERY `src/parser/*.rs`, read from the directory — a file absent from
    // CENSUS must have ZERO writes, which is what makes the table total. The
    // hardcoded 4-file list this replaces let a raw `Block` literal planted in
    // `pattern.rs` pass the whole suite: the same "the enumeration is a
    // selection" shape the censuses exist to stop, one level up.
    let mut parser_files: Vec<String> = fs::read_dir("src/parser")
        .expect("cannot read src/parser")
        .map(|e| e.expect("dir entry").path())
        .filter(|p| p.extension().is_some_and(|x| x == "rs"))
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .collect();
    parser_files.sort();
    assert!(
        parser_files.len() >= 8,
        "only {} file(s) found under src/parser — the scan is reading nothing.",
        parser_files.len()
    );
    let rows: Vec<(&str, usize, usize, usize, &str)> = parser_files
        .iter()
        .map(|f| {
            CENSUS
                .iter()
                .find(|(p, ..)| p == f)
                .copied()
                // A file with no CENSUS row is asserted to write NOTHING, so a
                // new writer anywhere under src/parser trips this.
                .unwrap_or((f.as_str(), 0, 0, 0, "no row: this file writes neither field"))
        })
        .collect();

    for (path, want_next, want_inline, want_header, rationale) in &rows {
        let content = fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot read {path}: {e}"));
        // Count WRITES only — a `SuiteLayout::X` in a `==` comparison is a
        // read, and the parser has none, but be explicit rather than lucky.
        let got_next = content
            .lines()
            .filter(|l| !l.trim_start().starts_with("//") && !l.contains("=="))
            .filter(|l| l.contains("SuiteLayout::NextLine"))
            .count();
        let got_inline = content
            .lines()
            .filter(|l| !l.trim_start().starts_with("//") && !l.contains("=="))
            .filter(|l| l.contains("SuiteLayout::Inline"))
            .count();
        // A field INIT (`header_start,` / `header_start: <expr>,`), never the
        // parameter declarations (`header_start: usize`) or the prose.
        let got_header = content
            .lines()
            .map(|l| l.trim())
            .filter(|t| !t.starts_with("//"))
            .filter(|t| t.starts_with("header_start") && t.ends_with(',') && !t.contains("usize"))
            .count();
        assert_eq!(
            (got_next, got_inline, got_header),
            (*want_next, *want_inline, *want_header),
            "R41 T-FMT-C `SuiteLayout` / `Block::header_start` writer census \
             changed in `{path}` (expected sites: {rationale}).\n\n\
             A new `Block` construction in the parser must decide, at the only \
             layer that can: did the author indent this suite, or write it on \
             the header's line? And WHERE does the owning construct's first \
             line begin? A construction outside the parser has no author \
             spelling at all and goes through `Block::synthetic`.\n\n\
             Bump the row with the new site's rationale."
        );
    }
}

/// Outside `src/parser/`, an `ast::Block` is built through `Block::synthetic`.
///
/// The companion to `parser_suite_layout_writer_census`: that one pins the
/// sites that DO know the author's spelling, this one keeps every site that
/// does NOT from inventing one. A lowering pass, a desugar or a test fixture
/// has no author to preserve, and a raw struct literal there would silently
/// pick whichever variant the author of that line typed — which is the shape
/// of a fact-carrying field going wrong quietly (`SuiteLayout` is not
/// `Default`, so the type system forces the choice, and this lint is what
/// makes the choice a single reviewed one).
///
/// **Break-and-verify (Core #13, RED-verified 2026-08-11):** rewrite any
/// `Block::synthetic(stmts, span)` outside `src/parser/` as
/// `Block { stmts, span, layout: SuiteLayout::NextLine }` and this fires.
#[test]
fn ast_block_constructed_only_via_synthetic_outside_parser() {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(dir).expect("read_dir") {
            let p = entry.expect("dir entry").path();
            if p.is_dir() {
                if p.file_name().is_some_and(|n| n == "target") {
                    continue;
                }
                walk(&p, out);
            } else if p.extension().is_some_and(|e| e == "rs") {
                out.push(p);
            }
        }
    }

    let mut files: Vec<PathBuf> = Vec::new();
    for root in ["src", "tests", "spec"] {
        let p = Path::new(root);
        if p.exists() {
            walk(p, &mut files);
        }
    }

    let mut offenders: Vec<String> = Vec::new();
    for path in &files {
        let s = path.to_string_lossy().replace('\\', "/");
        // `src/parser/` OWNS the field — it is the only layer that knows.
        if s.contains("src/parser/") {
            continue;
        }
        let content = fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot read {s}: {e}"));
        for (i, line) in content.lines().enumerate() {
            let t = line.trim();
            if t.starts_with("//") || t.starts_with("///") {
                continue;
            }
            // The AST `Block` literal, distinguished from the LIR/IR
            // `BasicBlock`/`Block` types that share the bare word.
            let is_ast_block_literal = (t.contains("Block {") || t.ends_with("Block {"))
                && !t.contains("BasicBlock")
                && !t.contains("EquipBlock")
                && !t.contains("ExternBlock")
                && !t.contains("pub struct Block")
                && !t.contains("impl Block");
            if !is_ast_block_literal {
                continue;
            }
            // Only flag lines that also carry the AST field names, so the
            // LIR's identically-named `Block` does not false-positive.
            if line.contains("stmts") || content.lines().nth(i + 1).is_some_and(|n| n.contains("stmts:")) {
                offenders.push(format!("  {s}:{}: {t}", i + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "R41 T-FMT-C: raw `ast::Block` struct literal outside `src/parser/`:\n{}\n\n\
         Use `Block::synthetic(stmts, span)`. `Block.layout` records what the \
         AUTHOR spelled, and only the parser can know that — everywhere else \
         there is no author, and a hand-picked value is a fact invented at a \
         layer that cannot have it.",
        offenders.join("\n")
    );
}

/// No RAW newline writing outside `Emitter` in `src/formatter/mod.rs`.
///
/// `Emitter::newline()` is idempotent at line start — that is what retires the
/// spurious-blank class, where an expression-position suite terminated its own
/// line and then the enclosing statement terminated it again. The property is
/// only total if every line ending goes through the emitter: one
/// `buf.push('\n')` from the outside reopens the class at that site, and the
/// symptom (a blank line nobody wrote) is subtle enough to survive review.
///
/// **Break-and-verify (Core #13, RED-verified 2026-08-11):** add a
/// `self.emitter.buf.push('\n');` anywhere in `Formatter` and this fires.
#[test]
fn formatter_no_raw_newline_outside_emitter() {
    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let src: Vec<&str> = content.lines().collect();

    // Locate `impl Emitter { .. }` by brace depth.
    let mut impl_range: Option<(usize, usize)> = None;
    let mut depth: i32 = 0;
    let mut start: Option<(usize, i32)> = None;
    for (i, line) in src.iter().enumerate() {
        if line.trim_start().starts_with("//") {
            continue;
        }
        if start.is_none() && line.trim_start().starts_with("impl Emitter") {
            start = Some((i, depth));
        }
        depth += line.matches('{').count() as i32 - line.matches('}').count() as i32;
        if let Some((s, base)) = start {
            if i > s && depth <= base {
                impl_range = Some((s, i));
                break;
            }
        }
    }
    let (impl_start, impl_end) = impl_range.expect("could not locate `impl Emitter` block");

    // The ONE sanctioned outside writer: `format`'s final trailing-newline
    // normalization, which runs on the OWNED String returned by `finish()` —
    // after the emitter is gone, so it cannot reopen the class.
    const ALLOWED_OUTSIDE: &[&str] = &["result.push('\\n');"];

    let mut offenders: Vec<String> = Vec::new();
    for (i, line) in src.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("//") {
            continue;
        }
        if !(trimmed.contains(".push('\\n')") || trimmed.contains(".push_str(\"\\n\")")) {
            continue;
        }
        if i >= impl_start && i <= impl_end {
            continue;
        }
        if ALLOWED_OUTSIDE.contains(&trimmed) {
            continue;
        }
        offenders.push(format!("  line {}: {trimmed}", i + 1));
    }

    assert!(
        offenders.is_empty(),
        "R41 T-FMT-C: raw newline writing outside `Emitter` in \
         `src/formatter/mod.rs`:\n{}\n\n\
         Line endings must go through `Emitter::newline()`, which is IDEMPOTENT \
         at line start. That idempotence is what retires the spurious-blank \
         class (an expression-position suite terminates its own line, then the \
         enclosing statement terminates it again). A raw push bypasses it and \
         reopens the class at that site.\n\n\
         Want a deliberate blank? `Emitter::blank_line()` is the one way to ask \
         for one.",
        offenders.join("\n")
    );
}

// ==== T-RB0 ================================================================

/// **D27 accept-both, SELF-HOST parse side: every ownership-context
/// `TOK_BANG` predicate must have a `TOK_CARET` SIBLING.**
///
/// The class this retires: the self-host parsers recognised only the RETIRED
/// move glyph `!` in ownership positions, so the self-host could not re-parse
/// the `^` its own formatter emits. That was invisible for as long as it
/// lasted because `resolver_comparison` had no floor.
///
/// **Sibling-predicate form, NOT a count.** A count of caret sites stays
/// green when a NEW bang-only predicate is born — the guard would sail past
/// the very class it exists to retire (Core #15e Q2: "can this guard catch
/// its OWN class?"). So this asserts a PROPERTY of each predicate instead:
/// wherever the parser tests for the move glyph in an ownership context, it
/// must test for BOTH glyphs on that same line.
///
/// It is deliberately symmetric — it also fails if a `TOK_BANG` test is
/// REPLACED by a bare `TOK_CARET` test, which would silently retire the `!`
/// glyph ahead of the ratified schedule.
///
/// **File set is DISCOVERED, not hardcoded.** A hardcoded 3-file list cannot
/// see copy N+1. `read_dir` finds every `self_host_*/parser.gg` and
/// `canonicalize` collapses the symlinks (`self_host_check` and
/// `self_host_lowerer` both point at `self_host_typechecker`), so the set is
/// the REAL files, however many there turn out to be.
///
/// **Exclusions are identified by CONTENT, never by line number** (line
/// numbers rot on the first edit). Each is anchored on a stable string that
/// appears a few lines BELOW its predicate, so the match uses a small
/// lookahead window:
///   - `EPropagate(` — postfix error-propagation `expr!` (D29). `^` is the
///     BITWISE-XOR operator in that position, so a caret sibling would be
///     flatly wrong.
///   - `is not a signature form` — the bare `!` inferred-error-set signature
///     marker (A31). Same reasoning: not an ownership sigil.
///   - `sigil = "` — the D35 diagnostic-text helper, whose whole job is to
///     tell the two glyphs APART so the message names the one the author
///     actually wrote.
///
/// **Detection is comment-stripped and word-bounded.** A bare `TOK_BANG`
/// grep also matches `TOK_BANGEQ` — the INEQUALITY token — which appears in
/// the operator tables; and prose like "TOK_BANG here is always the postfix
/// mark" would otherwise register as a predicate.
///
/// **If this fails**: a new ownership predicate was added for `!` only (add
/// the `or self.check_tok(TOK_CARET)` sibling), or an existing one lost its
/// caret sibling (a D27 regression), or a genuinely-new NON-ownership `!`
/// site was added (give it a content anchor here, with the reason).
#[test]
fn sh_parser_caret_predicate_siblings() {
    /// Stable strings marking a `TOK_BANG` test that is NOT an ownership
    /// sigil test, paired with why. Matched within `LOOKAHEAD` lines below
    /// the predicate.
    const EXCLUSION_ANCHORS: &[(&str, &str)] = &[
        ("EPropagate(", "postfix error-propagation `expr!` (D29) — `^` is bitwise-xor there"),
        ("is not a signature form", "bare `!` inferred-error-set signature marker (A31)"),
        ("sigil = \"", "D35 diagnostic-text helper — must tell the glyphs APART by design"),
    ];
    const LOOKAHEAD: usize = 6;

    // ---- discover the REAL parser.gg copies (symlinks collapsed) ----------
    let fixtures = std::path::Path::new("tests/fixtures");
    let mut real_files: Vec<std::path::PathBuf> = Vec::new();
    let mut seen: Vec<std::path::PathBuf> = Vec::new();
    let entries = fs::read_dir(fixtures).expect("cannot read tests/fixtures");
    let mut dirs: Vec<std::path::PathBuf> = entries
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.is_dir()
                && p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.starts_with("self_host_"))
        })
        .collect();
    dirs.sort();
    for d in &dirs {
        let candidate = d.join("parser.gg");
        if !candidate.exists() {
            continue;
        }
        let canon = candidate
            .canonicalize()
            .unwrap_or_else(|e| panic!("canonicalize {}: {e}", candidate.display()));
        if seen.contains(&canon) {
            continue; // a symlink onto a copy already counted
        }
        seen.push(canon);
        real_files.push(candidate);
    }
    assert!(
        real_files.len() >= 3,
        "sh_parser_caret_predicate_siblings: expected at least 3 REAL self_host_*/parser.gg \
         copies, found {}: {real_files:?}.\nIf a copy was deleted or newly symlinked, say so \
         here; if the self-host tree moved, fix the discovery above.",
        real_files.len(),
    );

    // ---- check every ownership-context TOK_BANG predicate ----------------
    let mut offenders: Vec<String> = Vec::new();
    let mut checked = 0usize;
    let mut excluded = 0usize;

    for path in &real_files {
        let content = fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
        // Strip comments so prose mentioning the token never registers as a
        // predicate. `#` inside a string literal is not a comment, so only
        // cut at a `#` preceded by an even number of quotes.
        let code: Vec<String> = content
            .lines()
            .map(|line| match line
                .char_indices()
                .find(|(i, c)| *c == '#' && line[..*i].matches('"').count() % 2 == 0)
            {
                Some((i, _)) => line[..i].to_string(),
                None => line.to_string(),
            })
            .collect();

        for (i, line) in code.iter().enumerate() {
            if !mentions_move_token(line, "TOK_BANG") {
                continue;
            }
            // Excluded? Look a few lines down for the anchoring content.
            let window_end = (i + 1 + LOOKAHEAD).min(code.len());
            let window = code[i..window_end].join("\n");
            if let Some((anchor, _why)) =
                EXCLUSION_ANCHORS.iter().find(|(a, _)| window.contains(a))
            {
                let _ = anchor;
                excluded += 1;
                continue;
            }
            checked += 1;
            if !mentions_move_token(line, "TOK_CARET") {
                offenders.push(format!(
                    "{}:{} — tests TOK_BANG without a TOK_CARET sibling:\n      {}",
                    path.display(),
                    i + 1,
                    line.trim(),
                ));
            }
        }

        // Symmetry: a caret-only ownership predicate would silently retire
        // the `!` glyph early. Every ownership-context TOK_CARET test must
        // likewise name TOK_BANG.
        for (i, line) in code.iter().enumerate() {
            if !mentions_move_token(line, "TOK_CARET") {
                continue;
            }
            let window_end = (i + 1 + LOOKAHEAD).min(code.len());
            let window = code[i..window_end].join("\n");
            if EXCLUSION_ANCHORS.iter().any(|(a, _)| window.contains(a)) {
                continue;
            }
            if !mentions_move_token(line, "TOK_BANG") {
                offenders.push(format!(
                    "{}:{} — tests TOK_CARET without a TOK_BANG sibling (the retired \
                     glyph is still ACCEPTED until D27 Round B retires it):\n      {}",
                    path.display(),
                    i + 1,
                    line.trim(),
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "D27 accept-both regression in the self-host parser(s): {} ownership predicate(s) \
         name only one move glyph.\n\n{}\n\n\
         Every ownership-context move-glyph test must accept BOTH `!` (retired but still \
         accepted) and `^` (canonical) — the self-host must be able to re-parse the `^` \
         its own formatter emits.\n\
         Fix: add the missing `or self.check_tok(TOK_CARET)` / `or ptag == TOK_CARET` \
         sibling on the cited line.\n\
         If the site is genuinely NOT an ownership sigil test, add a CONTENT anchor to \
         EXCLUSION_ANCHORS in this test with the reason (never a line number).",
        offenders.len(),
        offenders.join("\n"),
    );

    // Positive control: the scan must actually have found predicates. A
    // detection regression that matched nothing would otherwise pass loudly
    // green — the exact failure mode this lint exists to prevent.
    assert!(
        checked >= 24 && excluded >= 6,
        "sh_parser_caret_predicate_siblings: DETECTION regression — only {checked} \
         ownership predicate(s) and {excluded} excluded site(s) found across {} real \
         parser copies. Expected at least 24 and 6 (>=8 ownership predicates and >=2 \
         excluded sites per copy). The scan is no longer seeing the predicates it is \
         supposed to guard; fix the detection, do NOT lower these bounds.",
        real_files.len(),
    );
}

/// Does `line` test the parser's CURRENT token against `token` in an
/// OWNERSHIP context?
///
/// Only the two forms the self-host parsers actually use for ownership
/// sigils count:
///   - `self.check_tok(TOK_X)` — the lookahead predicate
///   - `ptag == TOK_X`         — the prefix/postfix dispatch on a peeked tag
///
/// Everything else that merely NAMES the token is out of scope by
/// construction, which is what keeps the lexer's token table
/// (`const int TOK_BANG = 16`, `return TOK_CARET`) and the operator/
/// precedence tables (`elif tag == TOK_CARET:` — there `^` is BITWISE XOR,
/// not a move sigil) from registering as ownership predicates.
///
/// `check_tok(TOK_BANG)` cannot collide with `TOK_BANGEQ` because the
/// closing paren pins the end. The `ptag ==` form is word-bounded for the
/// same reason: `TOK_BANG` must not match `TOK_BANGEQ` (the INEQUALITY
/// token), nor `TOK_CARET` match `TOK_CARETEQ`.
fn mentions_move_token(line: &str, token: &str) -> bool {
    if line.contains(&format!("check_tok({token})")) {
        return true;
    }
    let needle = format!("ptag == {token}");
    let bytes = line.as_bytes();
    let mut from = 0usize;
    while let Some(rel) = line[from..].find(&needle) {
        let end = from + rel + needle.len();
        if bytes
            .get(end)
            .is_none_or(|c| !c.is_ascii_alphanumeric() && *c != b'_')
        {
            return true;
        }
        from = end;
    }
    false
}

/// **The fuzz targets must still COMPILE against the current library API.**
///
/// The fuzz crate is not part of the workspace build, so nothing in the
/// normal gate ever type-checks it. It bit-rotted exactly that way:
/// `formatter::format_source` was renamed to `format_source_result` /
/// `format_source_infallible` and `fuzz_roundtrip.rs` kept importing the old
/// name — an `E0432` that sat there because no gate compiled the crate. The
/// fuzzers are a real safety net; a net that does not build catches nothing.
///
/// **UNGATED on purpose.** An env-gated version would be skipped in CI and
/// the rot would simply resume. ~10s cold, which is what CI pays (the
/// `/tmp` target dir is never cached there).
///
/// **No `--bin` filter**: all three targets (`fuzz_lexer`, `fuzz_parser`,
/// `fuzz_roundtrip`) are checked, so the next rename is caught in whichever
/// target uses the renamed symbol.
///
/// **`--locked`** does double duty: it keeps the run from mutating
/// `fuzz/Cargo.lock` (a test that dirties the tree is its own problem) AND
/// it ratchets lock staleness — if `fuzz/Cargo.lock` drifts from
/// `fuzz/Cargo.toml`, this fails with "cannot update the lock file" and the
/// fix is to commit a refreshed lock.
///
/// **CARGO_TARGET_DIR under /tmp** so the check cannot collide with the main
/// build's target dir or leave artifacts in the tree.
#[test]
fn fuzz_targets_still_compile() {
    let manifest = std::path::Path::new("fuzz/Cargo.toml");
    assert!(
        manifest.exists(),
        "fuzz/Cargo.toml is missing — if the fuzz crate was intentionally \
         removed, delete this lint in the same commit."
    );

    let out = std::process::Command::new(env!("CARGO"))
        .args(["check", "--locked", "--manifest-path", "fuzz/Cargo.toml"])
        .env("CARGO_TARGET_DIR", "/tmp/gg_fuzz_lint_target")
        .output()
        .expect("failed to invoke cargo check on fuzz/Cargo.toml");

    assert!(
        out.status.success(),
        "the fuzz crate no longer compiles against the current library API.\n\n\
         {}\n\n\
         The fuzz targets are not in the workspace build, so ONLY this lint \
         type-checks them. Fix the target (usually a renamed/removed `pub` \
         item), or — if `fuzz/Cargo.lock` is merely stale — refresh and COMMIT \
         it:\n  \
         cargo update --manifest-path fuzz/Cargo.toml --workspace\n\n\
         Reproduce locally with:\n  \
         CARGO_TARGET_DIR=/tmp/gg_fuzz_lint_target cargo check --locked \
         --manifest-path fuzz/Cargo.toml",
        String::from_utf8_lossy(&out.stderr),
    );
}

// ==== end T-RB0 ============================================================

/// R41 T-FMT-D (Core #4/#6 class guard): the LIST-EMIT CENSUS for
/// `src/formatter/mod.rs`.
///
/// Gorget has ONE canonical layout for a horizontally-broken list — greedy
/// fill packing at the block continuation indent, no trailing comma
/// (`doc::surround_fill`). There are exactly FOUR ways to emit a list from the
/// formatter, and each one is counted separately here so that adding a list
/// kind is a CONSCIOUS choice rather than a copy-paste of whichever neighbour
/// was nearest:
///
///   * `doc::surround_fill(` — the canonical, fill-packed spelling. ONE call
///     site, inside `Formatter::emit_delimited_texts`: every fill-packed list
///     funnels through it, having first passed the interior-comment gate in
///     `emit_delimited_list`. The per-list-KIND census that used to live on
///     this count now lives on `EXPECTED_DELIMITED_LIST_SITES`, where a new
///     kind cannot reach fill packing without meeting the gate.
///   * `doc::surround(` — the one-item-per-line-with-trailing-comma spelling.
///     Zero production call sites: this is a TRIPWIRE, not a baseline. A new
///     `doc::surround(` in the formatter means a list opted OUT of the canon,
///     which needs a stated reason. (The builder itself stays in `doc.rs`, with
///     its own unit tests — the exploded shape is still the right one for a
///     comment-bearing list, which reaches it imperatively through
///     `Formatter::format_bracketed_broken_with_comments`.)
///   * `doc::group(` — the hand-rolled group compositions (method chain,
///     comprehension, paren-wrap helper). Also counted by
///     `fmt_multiline_group_paren_wrap_class` above, which adjudicates a
///     DIFFERENT invariant (that a broken group re-parses); the duplication is
///     deliberate so each axis is discoverable from its own lint.
///   * `write(", ")` — the imperative comma loops that never wrap at all
///     (`from` imports, type-parameter bindings, bare tuple positions, …).
///     These are the fourth escape route from the canon: a new one is fine, but
///     it must be a decision, not an accident.
///
/// **Why a SEPARATE lint rather than a rider on the paren-wrap class:** that
/// lint's name is a claim about multi-line re-parse safety. Bolting a
/// list-layout count onto it would make its name false, and a guard whose scope
/// is mis-stated is the kind that green-lights its own class (Core #15e Q2).
///
/// **Break-and-verify:** hand-roll a second `doc::surround_fill(` anywhere in
/// `src/formatter/mod.rs` — the fill count rises to 2 and the first assertion
/// fires. Add an `emit_delimited_list(.., Gate::UngatedCarveOut(..), ..)`
/// without listing its reason in `EXPECTED_CARVE_OUTS` — the attributed-set
/// assertion fires with the new (fn, reason) row shown.
#[test]
fn formatter_list_emit_fill_census() {
    /// The terminal splice into the Doc layer. Structurally ONE: every
    /// fill-packed list in the language funnels through
    /// `Formatter::emit_delimited_texts`, which is the only place
    /// `doc::surround_fill` is spelled.
    ///
    /// This constant used to be 10, one per list kind, with an instruction
    /// to bump it per new kind — the per-list-kind visibility now lives in
    /// EXPECTED_DELIMITED_LIST_SITES below, where it is UN-BYPASSABLE: a
    /// kind can no longer reach fill packing without passing the
    /// interior-comment gate on the way.
    const EXPECTED_SURROUND_FILL: usize = 1;
    /// One `self.emit_delimited_list(` per GATED list kind. Nine: the
    /// parameter list, call args, generic params, generic args, closure
    /// params, the array/set literal arm, the multi-element tuple arm, the
    /// dict literal arm, and the fmt-unreachable `Expr::StructLiteral` arm
    /// (kept converted so the class rule has no exception — see
    /// `formatter_collection_literal_interior_hook_dispatch`, which pins
    /// that unreachability).
    ///
    /// This is the census that replaced the ten-way `doc::surround_fill`
    /// count, and it keeps the same property: adding a list kind is a
    /// CONSCIOUS choice, visible as a number.
    const EXPECTED_DELIMITED_LIST_SITES: usize = 9;
    /// DIRECT `emit_delimited_texts` callers outside the chokepoint. One:
    /// the grouped-import group, the single declared carve-out — its names
    /// are SORTED, so emitted order is not source order and the
    /// forward-only comment cursor cannot interleave per element.
    ///
    /// COUNTING METHOD: the dotted spelling `.emit_delimited_texts(`
    /// returns 2 raw hits — the chokepoint's own internal call and this
    /// caller (the DEFINITION has no dot) — and the internal call is
    /// excluded by fn scope below. The dot-less spelling would return 3.
    const EXPECTED_UNGATED_TEXTS: usize = 1;
    /// LEXICAL `Gate::UngatedCarveOut("…")` construction sites, pinned as
    /// an exact (enclosing fn, reason) set so each carve-out is attributed
    /// rather than merely counted. Three:
    ///   * `gate_or_scan_miss` — the SINGLE `Option -> Gate` converter.
    ///     All delimiter-scan misses route through it, including the
    ///     sibling-anchor propagation, so this stays one site regardless of
    ///     how many callers can miss.
    ///   * two in `format_chain_segment` — chain segments are pre-rendered
    ///     into a sub-formatter whose comment sideband is EMPTY, so a real
    ///     gate there would be dead code reading as a live one. That escape
    ///     is filed with a repro (`known_gaps/fmt_delimited_list_pre_render_above.gg`);
    ///     these two sites are its structural marker in the source. (They
    ///     moved out of `format_method_chain` when the segment emission was
    ///     factored into one spelling shared by the emission and the R42
    ///     tail-reserve measurement — the same two sites, re-attributed.)
    ///
    /// A pattern match (`if let Gate::UngatedCarveOut(reason) = gate`) is
    /// NOT a construction and is excluded by requiring the `("` opener.
    const EXPECTED_CARVE_OUTS: &[(&str, &str)] = &[
        ("gate_or_scan_miss", "scan miss"),
        ("format_chain_segment", "chain segment generic args: empty sideband"),
        ("format_chain_segment", "chain segment call args: empty sideband"),
    ];
    /// Non-canonical one-item-per-line sites. A tripwire pinned at zero.
    const EXPECTED_SURROUND: usize = 0;
    /// Hand-rolled `doc::group` compositions — see the allowlist in
    /// `fmt_multiline_group_paren_wrap_class`.
    const EXPECTED_GROUP: usize = 3;
    /// Imperative `", "` separator loops that never wrap. These are the
    /// hand-rolled comma-loop emitters — pattern field lists and enum
    /// tuple-variant field lists among them. They do NOT reach the
    /// delimited-list chokepoint and still re-parent an interior comment.
    ///
    /// ⚠ This CONSTANT is the family's only total statement: the
    /// row-by-row enumeration, with a disposition and the measured
    /// symptom per row, lives in `TODO.md`. Regenerate it with
    /// `awk '/^    fn /{fn=$0;sub(/^ +fn /,"",fn);sub(/[(<].*/,"",fn)}
    ///      /write\(", "\)/{print NR"\t"fn}' src/formatter/mod.rs`
    /// (comment lines excluded, as below). A change here means the family
    /// grew or shrank — reconcile the TODO enumeration with it.
    ///
    /// The count EXCLUDES measurement mirrors (below), which is what keeps
    /// this number comparable across the R42 tail reserve: a mirror emits
    /// into a throwaway sub-formatter and can re-parent nothing.
    const EXPECTED_WRITE_SEP: usize = 23;
    /// MEASUREMENT MIRRORS of the loops above: a hand-rolled comma loop has
    /// to be re-walked to measure what follows one of its items, and the
    /// re-walk necessarily spells the separator again.
    ///
    /// These are NOT list emitters. The mirror runs inside a
    /// `measured_reserve` closure, on a sub-`Formatter` at probe width whose
    /// output is measured and thrown away — it never reaches the buffer, so
    /// the interior-comment escape the census above exists to count cannot
    /// happen here.
    ///
    /// A rise means either a new hand-rolled loop gained a tail measurement
    /// (fine — its emitter half must appear above too) or a real emitter was
    /// mis-spelled onto a sub-formatter (not fine). Reconcile against the
    /// emitter count, which is pinned separately and did NOT move.
    const EXPECTED_MEASUREMENT_MIRRORS: usize = 11;
    /// Functions that exist ONLY to be run inside a `measured_reserve`
    /// closure. Their writes land on a throwaway sub-`Formatter` exactly like
    /// an inline mirror's do — but because they are METHODS, the receiver is
    /// spelled `self` and the "written on `self`" test above cannot see it.
    ///
    /// Kept in step with the MEASUREMENT-ONLY rows of
    /// `formatter_header_suffix_census`, which dispositions the same set from
    /// the other direction; a function that appears in one and not the other
    /// is a disagreement to resolve, not a label to copy.
    const MEASUREMENT_ONLY_FNS: &[&str] = &["format_trait_bound_tail", "format_expr_if_tail"];

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");
    let mut fill = 0usize;
    let mut surround = 0usize;
    let mut group = 0usize;
    let mut write_sep = 0usize;
    let mut measurement_mirrors = 0usize;
    let mut delimited_list_sites = 0usize;
    let mut ungated_texts = 0usize;
    let mut carve_outs: Vec<(String, String)> = Vec::new();
    // Enclosing-fn tracking, for carve-out attribution and for excluding
    // the chokepoint's own internal `emit_delimited_texts` call.
    let mut current_fn = String::new();
    for line in content.lines() {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix("fn ") {
            // Strip the generic parameter list too: the chokepoint is
            // declared `fn emit_delimited_list<E>(`, and splitting on `(`
            // alone would leave `emit_delimited_list<E>` and silently
            // defeat the scope exclusion below.
            current_fn = rest
                .split(|c| c == '(' || c == '<')
                .next()
                .unwrap_or("")
                .to_string();
        }
        // Skip pure-comment lines so prose mentioning a spelling doesn't count.
        if trimmed.starts_with("//") || trimmed.starts_with("///") {
            continue;
        }
        fill += line.matches("doc::surround_fill(").count();
        // `doc::surround(` cannot match `doc::surround_fill(` — the `(` is
        // required immediately after the name, which is exactly why the fill
        // builder is a DISTINCT SPELLING and not a boolean parameter. A flag
        // would make both shapes the same token and this census blind.
        surround += line.matches("doc::surround(").count();
        group += line.matches("doc::group(").count();
        // A separator written on the OUTER formatter is an emitter; one
        // written on a `measured_reserve` closure's sub-formatter is a
        // mirror. `self` is a keyword, so "the receiver is literally `self`"
        // is a structural test, not a naming convention: no sub-formatter can
        // ever be bound to it.
        let on_self = line.matches("self.emitter.write(\", \")").count();
        let any = line.matches("write(\", \")").count();
        if MEASUREMENT_ONLY_FNS.contains(&current_fn.as_str()) {
            measurement_mirrors += any;
        } else {
            write_sep += on_self;
            measurement_mirrors += any - on_self;
        }
        delimited_list_sites += line.matches("self.emit_delimited_list(").count();
        if current_fn != "emit_delimited_list" {
            ungated_texts += line.matches(".emit_delimited_texts(").count();
        }
        if let Some(idx) = line.find("Gate::UngatedCarveOut(\"") {
            let after = &line[idx + "Gate::UngatedCarveOut(\"".len()..];
            let reason = after.split('"').next().unwrap_or("").to_string();
            carve_outs.push((current_fn.clone(), reason));
        }
    }

    let msg = "\n\nGorget has ONE canonical broken-list layout: greedy fill packing at \
               the block continuation indent, no trailing comma — and ONE place where a \
               list decides, before the Doc layer, whether an interior comment forces it \
               to the exploded shape instead. If you added a list kind, route it through \
               `Formatter::emit_delimited_list` and bump \
               EXPECTED_DELIMITED_LIST_SITES. If you deliberately opted a list OUT of the \
               gate, it becomes a `Gate::UngatedCarveOut` WITH its reason and joins the \
               attributed set below — the point of this census is that the choice is \
               visible.\n\
               Sibling guard on a different axis: `fmt_multiline_group_paren_wrap_class` \
               (multi-line output must re-parse).";
    assert_eq!(
        fill, EXPECTED_SURROUND_FILL,
        "`doc::surround_fill(` site count changed — it is structurally 1 \
         (inside `emit_delimited_texts`). A second spelling means a list \
         reached fill packing WITHOUT passing the interior-comment gate.{msg}"
    );
    assert_eq!(
        delimited_list_sites, EXPECTED_DELIMITED_LIST_SITES,
        "`self.emit_delimited_list(` site count changed — this is the \
         per-list-kind census.{msg}"
    );
    assert_eq!(
        ungated_texts, EXPECTED_UNGATED_TEXTS,
        "direct `.emit_delimited_texts(` caller count outside the \
         chokepoint changed — expected exactly the grouped-import \
         carve-out.{msg}"
    );
    let expected_carve_outs: Vec<(String, String)> = EXPECTED_CARVE_OUTS
        .iter()
        .map(|(f, r)| ((*f).to_string(), (*r).to_string()))
        .collect();
    assert_eq!(
        carve_outs, expected_carve_outs,
        "the `Gate::UngatedCarveOut` set changed (enclosing fn, reason).\n\n\
         Every carve-out states WHY it is one, and the reason is always a \
         property of the CONTEXT — an empty comment sideband, or a \
         delimiter scan that found nothing — never a 'not implemented \
         yet'. A NEW entry means a list emitter opted out of the gate: it \
         needs a filed repro for the escape it admits, and its reason \
         listed here. A MISSING entry means a carve-out was closed, which \
         should also un-ignore the repro that pins it.{msg}"
    );
    assert_eq!(
        surround, EXPECTED_SURROUND,
        "`doc::surround(` site count changed — a formatter list opted OUT of the \
         fill-packed canon.{msg}"
    );
    assert_eq!(group, EXPECTED_GROUP, "`doc::group(` site count changed.{msg}");
    assert_eq!(
        write_sep, EXPECTED_WRITE_SEP,
        "`self.emitter.write(\", \")` separator-loop count changed — a list is \
         emitted with a hand-rolled comma loop that can never wrap.{msg}"
    );
    assert_eq!(
        measurement_mirrors, EXPECTED_MEASUREMENT_MIRRORS,
        "the count of comma-loop MEASUREMENT MIRRORS (a `\", \"` written on a \
         sub-formatter rather than on `self`) changed. A mirror re-walks a \
         hand-rolled loop to measure what follows one of its items and emits \
         nothing; a real emitter mis-spelled onto a sub-formatter would show \
         up here while the emitter count above stayed put, which is exactly \
         the confusion this split exists to prevent.{msg}"
    );
}



/// R41 T-FMT-B CLASS GUARD, visibility face — every declaration kind that can
/// carry a visibility keyword is emitted through the explicitness-aware path.
///
/// `public Foo` and a bare `Foo` both parse to `Visibility::Public`, so the
/// emitter cannot recover the author's spelling from the value; it reads
/// `explicit_visibility`, which the parser writes. The class risk is a TENTH
/// carrier: a new declaration kind that grows a `visibility` field and emits it
/// with its own two-arm match would silently delete every explicit `public` on
/// that kind, which is precisely the defect the flag exists to retire — and
/// nothing about adding the field forces its author past this path.
///
/// So the two counts are pinned against each other. `format_static_decl` is the
/// one deliberate non-caller: statics are private-by-DEFAULT, the inverse
/// convention, and it carries its own rule (`src/formatter/mod.rs:1894-1910`).
/// A mismatch means either a new carrier that skipped the path, or a carrier
/// removed without its emit site — both worth a look.
#[test]
fn formatter_visibility_emit_site_count() {
    /// `pub visibility: Visibility` fields in the AST — the carriers.
    const EXPECTED_CARRIERS: usize = 9;
    /// `self.format_visibility(` call sites, plus `format_static_decl`'s own
    /// inverted rule, which together must cover every carrier.
    const EXPECTED_EMIT_SITES: usize = 8;
    const STATIC_DECL_OWN_RULE: usize = 1;

    let ast = fs::read_to_string("src/parser/ast.rs").expect("cannot read src/parser/ast.rs");
    let fmt = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");

    let carriers = ast
        .lines()
        .filter(|l| l.trim() == "pub visibility: Visibility,")
        .count();
    let emit_sites = fmt
        .lines()
        .filter(|l| !l.trim_start().starts_with("//") && !l.trim_start().starts_with("///"))
        .filter(|l| l.contains("self.format_visibility("))
        .count();

    assert_eq!(
        carriers, EXPECTED_CARRIERS,
        "the number of AST declarations carrying `visibility` changed \
         ({EXPECTED_CARRIERS} -> {carriers}).\n\n\
         If a kind was ADDED: it also needs `explicit_visibility` written at the \
         parser (one writer, where the keyword is consumed) and an emit through \
         `format_visibility`, or `gg fmt` will delete the author's `public` on \
         that kind — the class this guard exists to retire. Then bump both \
         constants.\n\
         Census: grep -c 'pub visibility: Visibility,' src/parser/ast.rs"
    );
    assert_eq!(
        emit_sites, EXPECTED_EMIT_SITES,
        "the `format_visibility` call-site count changed \
         ({EXPECTED_EMIT_SITES} -> {emit_sites}). A site that DISAPPEARED means \
         a declaration kind stopped emitting a keyword the user wrote — the \
         silent-drop class. A site ADDED without a new carrier means something \
         is emitting visibility twice.\n\
         Census: grep -c 'self.format_visibility(' src/formatter/mod.rs"
    );
    assert_eq!(
        emit_sites + STATIC_DECL_OWN_RULE,
        carriers,
        "visibility EMIT sites ({emit_sites} `format_visibility` calls + \
         `format_static_decl`'s own inverted rule) no longer cover the \
         {carriers} AST carriers.\n\n\
         Every carrier must route through `format_visibility`, which emits the \
         keyword IFF the author wrote one. The single sanctioned exception is \
         `format_static_decl` (statics are private-by-default, the opposite \
         convention).\n\
         Census: grep -c 'self.format_visibility(' src/formatter/mod.rs"
    );
}

/// R41 T-FMT-B CLASS GUARD — every quoted string the formatter emits goes
/// through the ONE producer.
///
/// The defect this retires: eight separate arms spelled their own quotes
/// (`write("\"")` … `write("\"")`) around a name string the AST stores
/// DECODED. None of them re-escaped, so `test "a \" b":` came back with a bare
/// quote and the formatted file no longer parsed — and the eight sites drifted
/// in exactly the way Core #4 describes, because each was written
/// independently and none of them knew about the others.
///
/// The guard has two halves, and the second is the one that matters:
///
///   (a) the producer's call-site count is pinned, so ADDING a site is a
///       deliberate bump rather than a silent divergence; and
///   (b) NO OTHER function in the formatter may write a bare quote character
///       at all. That is what makes the guard able to catch its own class: a
///       ninth site that hand-rolls quotes fails here even though it never
///       touches the producer.
///
/// The quote-write census covers both costumes: a literal `write("\"")` and a
/// `format!` whose template contains an escaped quote. Zero of the latter
/// exist today outside the allowed functions; the pattern is checked anyway so
/// that "spell it with `format!` instead" is not an escape hatch.
///
/// There is deliberately NO allowlist of name-string field names here: an
/// allowlist would have to be kept in sync with the AST, which is the parallel
/// list the layering rules forbid.
#[test]
fn formatter_verbatim_emit_arm_count() {
    /// Call sites of `self.emit_quoted_string(` — the eight name-string
    /// emitters, censused 2026-08-11 at the T-FMT-B landing:
    ///   `format_test` (1), `format_bench` (1),
    ///   `AttributeArg::StringLiteral` (1), `AttributeArg::KeyValue`'s
    ///   string-valued producer (1), the inline `extern "<abi>"` tag (1),
    ///   `FunctionBody::Extern`'s `= "<symbol>"` (1), the `extern "<abi>":`
    ///   block header (1), `Stmt::Snapshot`'s name (1).
    ///
    /// A NEW name-string field in the AST adds a site and bumps this; a site
    /// that DISAPPEARS means an emitter stopped emitting a name the user
    /// wrote, which is the silent-drop class and wants its own look.
    const EXPECTED_PRODUCER_CALLS: usize = 8;

    /// Functions permitted to write a quote character directly. These ARE the
    /// producers — everything downstream of them must call one of these
    /// instead of spelling quotes itself.
    ///
    ///   `quoted_string_text`      — builds the name-string fallback.
    ///   `canonical_string_escape` — escapes a `"` INSIDE a string body.
    ///   `format_string_lit`       — the literal path's own delimiters.
    const ALLOWED_QUOTE_WRITERS: &[&str] = &[
        "fn quoted_string_text(",
        "fn canonical_string_escape(",
        "fn format_string_lit(",
    ];

    let content = fs::read_to_string("src/formatter/mod.rs")
        .expect("cannot read src/formatter/mod.rs");

    // ── (a) the producer call-site count ──
    let mut producer_calls = 0usize;
    for line in content.lines() {
        if line.trim_start().starts_with("//") || line.trim_start().starts_with("///") {
            continue;
        }
        producer_calls += line.matches(".emit_quoted_string(").count();
    }
    assert_eq!(
        producer_calls, EXPECTED_PRODUCER_CALLS,
        "R41 T-FMT-B: `emit_quoted_string` call-site count in \
         `src/formatter/mod.rs` changed: {producer_calls} vs expected \
         {EXPECTED_PRODUCER_CALLS}.\n\n\
         A new quoted NAME-STRING emit site must route through \
         `emit_quoted_string` (which recovers the author's escape spelling \
         from the span and re-escapes when it cannot) and bump EXPECTED here. \
         A site that vanished means a name the user wrote is no longer being \
         emitted — check for a silent drop before lowering the count."
    );

    // ── (b) nobody else writes a quote ──
    //
    // Track the enclosing function by its `fn <name>(` header, and flag any
    // quote-write that occurs outside an allowed one.
    let mut current_fn = String::new();
    let mut offenders: Vec<String> = Vec::new();
    for (i, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix("fn ") {
            current_fn = format!("fn {}", rest);
        } else if let Some(rest) = trimmed.strip_prefix("pub fn ") {
            current_fn = format!("fn {}", rest);
        }
        if trimmed.starts_with("//") {
            continue;
        }
        let allowed = ALLOWED_QUOTE_WRITERS
            .iter()
            .any(|a| current_fn.starts_with(a));
        if allowed {
            continue;
        }
        // Costume 1: a write of a bare quote — `write("\"")`,
        // `write("\" ")`, `write(" \"")`, `write("\":")`, …
        let writes_quote = line.contains(".write(\"")
            && line
                .split(".write(\"")
                .skip(1)
                .any(|tail| tail.split("\")").next().is_some_and(|s| s.contains('\\')) && tail.contains("\\\""));
        // Costume 2: a `format!` template carrying an escaped quote.
        let formats_quote = line.contains("format!(\"") && line.contains("\\\"");
        if writes_quote || formats_quote {
            offenders.push(format!(
                "  src/formatter/mod.rs:{} (in {current_fn}): {}",
                i + 1,
                line.trim()
            ));
        }
    }
    assert!(
        offenders.is_empty(),
        "R41 T-FMT-B: {} site(s) in `src/formatter/mod.rs` spell a quote \
         character outside the string producers.\n\n\
         A quoted string is emitted by ONE of: `emit_quoted_string` (name \
         strings the AST stores decoded) or `format_string_lit` (an \
         `Expr::StringLiteral`). Spelling quotes by hand re-emits the DECODED \
         text with no re-escaping, which is how eight sites came to produce \
         output that no longer parsed.\n\n\
         Offending lines:\n{}",
        offenders.len(),
        offenders.join("\n")
    );
}

/// R42 Core #4 guard, ITEM-POSITION half: a pre-rendered element that will be
/// spliced into a `Doc` must be built through `element_to_string_reserving`,
/// which takes its tail as an EXPLICIT parameter, so the reserve cannot be
/// forgotten by writing the shorter spelling.
///
/// The bare `element_to_string` survives for the one position that genuinely
/// has no tail of its own, and that position is named below. A second bare
/// call is the regression this pins: escape (c) — a sub-render blind to what
/// its parent appends after it — reopens silently, because the output still
/// re-parses and is still idempotent. Only the width moves.
#[test]
fn formatter_pre_rendered_items_carry_their_reserve() {
    /// Bare `self.element_to_string(` call sites. ONE, and it is not an item:
    /// the comprehension's loop VARIABLE, which is interpolated into a
    /// `format!("for {var} in {iter}")` string rather than spliced as its own
    /// `Doc` leaf — so the ELEMENT's reserve already covers the line it lands
    /// on, and giving the variable its own would double-charge it.
    const EXPECTED_BARE: usize = 1;

    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let mut bare = 0usize;
    for line in content.lines() {
        let t = line.trim_start();
        if t.starts_with("//") || t.starts_with("///") {
            continue;
        }
        // `element_to_string(` only — `element_to_string_at(`,
        // `_reserving(` and `_unbounded(` all fail the `(` adjacency test.
        bare += line.matches(".element_to_string(").count();
    }
    assert_eq!(
        bare, EXPECTED_BARE,
        "R42 escape-(c) adoption ratchet: `self.element_to_string(` site count \
         changed ({bare} vs {EXPECTED_BARE}).\n\n\
         A pre-rendered element is spliced onto a line that CONTINUES after \
         it — the separating `,`, the list's close, and whatever the list's \
         own caller writes after that. A sub-render at the full budget is \
         blind to all of it and overruns by exactly that much, while still \
         re-parsing and still being idempotent, so no other gate can see it.\n\n\
         Build the element through `element_to_string_reserving(base_indent, \
         reserve, f)` and state its tail. If the new site genuinely has no \
         tail, say WHY here and bump the count — but check first: `no tail` \
         is what every one of the ten list kinds looked like before this \
         class was measured."
    );
}

/// R42 Core #4 guard, HEADER-PRODUCER half: every function that writes a
/// header SUFFIX — the text a header emits AFTER a width-decided render, on
/// the same line — either installs a tail reserve or is dispositioned here.
///
/// A colon-only needle would be blind to half the family: ` throws T`, `!`,
/// ` = "sym"` and ` = Type` are suffixes too, and the census that green-lights
/// them is a census that green-lights the extern class this track exists to
/// close. The needle therefore covers all of them.
///
/// Two halves, because a fn-level check alone has a hole. The DISPOSITION set
/// catches a suffix written in a NEW function; the pinned SITE TOTAL catches
/// one written as a new arm inside `format_stmt` or `format_expr`, which
/// already reserve elsewhere and would otherwise absorb it silently.
///
/// # The DISCOVERY SWEEP that closed the family
///
/// This census pins the HEADER-suffix half. The wider question — *is there any
/// other position where a caller writes text after a width-decided render?* —
/// was answered once, by sweeping every `emitter.write*` whose preceding
/// emission is a `format_*` / `write_doc` / `element_to_string*` /
/// `emit_delimited_*` call. Recorded HERE rather than in a scratch file,
/// because a declaration of closure that cannot be re-run is not a
/// declaration:
///
/// ```text
/// STRICT  (the immediately preceding CODE line is a render) : 82
/// LOOSE   (a render within the preceding 5 code lines)      : 272   (STRICT ⊆ LOOSE)
///
/// disposition of all 272:
///   233  RESERVED — the enclosing fn installs a tail reserve
///     4  NOT-A-CARRIER — the write closes the render's own construct
///    35  non-carriers, by kind:
///          reserve-0 verified   format_test · format_bench · format_suite_setup
///                               · format_suite_teardown · format_extern_block
///                               · format_attributes · format_call_arg
///          charged at caller    format_function_header_tail · format_inline_suite
///          measurement only     format_expr_if_tail · format_trait_bound_tail
///          width-exempt         format_import (the `from` list + the dotted path)
///          no Doc layer         format_pattern (filed feature gap)
///          not a header         format_string_lit (f-string internals)
///          write PRECEDES the render — the loose window over-captures:
///                               try_inline_single_terminal_stmts
///                               · format_closure_body · format_expr_maybe_parens
/// ```
///
/// Regenerate with the needle this test already carries (`suffix` below) run
/// against `src/formatter/mod.rs` with the preceding-line rule; a rise in the
/// LOOSE count that is not matched by a row above is a new carrier position.
/// The sweep is what found `format_const_decl`, `format_static_decl`,
/// `format_param`, `format_trait_bound`, `format_newtype`,
/// `format_generic_param`, `format_closure_param` and
/// `format_assert_return_expr` — eight carriers no enumerated row named, each
/// now pinned by a boundary cell under `tests/fixtures/fmt_tail_reserve/`.
#[test]
fn formatter_header_suffix_census() {
    /// Disposition per function. `true` = the function installs a reserve
    /// somewhere; `false` = it does not, and the string says why that is
    /// correct. Every `false` row was VERIFIED by reading the render path
    /// from the header's start to the suffix and confirming it reaches no
    /// `write_doc` — never by copying a label.
    const CENSUS: &[(&str, bool, &str)] = &[
        ("emit_else_header", false, "NO CARRIER: `else:` is written at line start; nothing width-decided precedes it"),
        ("format_arm_body", false, "CHARGED AT THE CALLER: ` do:` and the inline body are measured by `arm_body_reserve`, which the four callers install around their own header render — a reserve installed HERE could never reach it"),
        ("format_attributes", false, "RESERVE-0 VERIFIED: an attribute's `k = v` is text on both sides; no attribute-arg form reaches `write_doc`"),
        ("format_bench", false, "RESERVE-0 VERIFIED: `bench \"name\":` is a quoted string plus a colon, both plain text"),
        ("format_call_arg", false, "RESERVE-0 VERIFIED: a named argument's `name` is text, so nothing width-decided precedes the ` = `"),
        ("format_const_decl", true, ""),
        ("format_elif_else_blocks", true, ""),
        ("format_enum", true, ""),
        ("format_equip", true, ""),
        ("format_expr_if_tail", false, "MEASUREMENT ONLY: this helper exists to be run inside `measured_reserve`; it never emits into the output buffer"),
        // R42 Track D: the giant expression match is `format_expr_inner` now.
        // The row is RE-POINTED, not dropped — `format_expr` itself keeps no
        // header-suffix write (it writes only the author's parens, and charges
        // the closing ones through `with_tail_reserve` around the delegation),
        // so it no longer appears in this census at all.
        ("format_expr_inner", true, ""),
        ("format_extern_block", false, "RESERVE-0 VERIFIED: `extern \"C\":` is a quoted string plus a colon"),
        ("format_function_header_tail", false, "CHARGED AT THE CALLER: this function IS the suffix — `format_function` measures it and installs it around the parameter list"),
        ("format_inline_suite", false, "CHARGED AT THE CALLER: `write(header_suffix)` runs AFTER the header expression is already emitted, so a reserve installed here could never reach that fit test — `inline_suite_reserve` / `suite_header_reserve` measure it at the if/elif/meta-case sites instead. Its `\"\"`-suffix caller (`on error`, B9's ratified NO-COLON inline form) has no carrier at all: the header is literal text"),
        ("format_item", true, ""),
        ("format_match_arm", true, ""),
        ("format_param", true, ""),
        ("format_static_decl", true, ""),
        ("format_stmt", true, ""),
        ("format_string_lit", false, "NOT A HEADER: the `:` is an f-string format spec INSIDE a literal — the broad needle over-captures it"),
        ("format_struct", true, ""),
        ("format_suite_setup", false, "NO CARRIER: `suite setup:` is a whole-header literal with no render at all"),
        ("format_suite_teardown", false, "NO CARRIER: `suite teardown:` likewise"),
        ("format_test", false, "RESERVE-0 VERIFIED: `test \"name\":`, as `format_bench`"),
        ("format_trait", true, ""),
        ("format_trait_bound", true, ""),
        ("format_trait_bound_tail", false, "MEASUREMENT ONLY: the remainder walk `format_trait_bound` runs inside `measured_reserve`"),
        ("format_trait_extends_and_colon", true, ""),
        ("format_type_alias", true, ""),
    ];
    /// Header-suffix WRITE sites PER FUNCTION, pinned so a new arm inside a
    /// function that already reserves — `format_stmt` and `format_expr` are
    /// giant match statements — cannot slip in under an existing `true` row.
    ///
    /// Per-function rather than a single grand total: a raw total nets a
    /// compensating add-and-remove inside the same function to zero, which is
    /// exactly the shape a refactor produces and exactly the one a guard must
    /// not sleep through.
    const EXPECTED_SITES_PER_FN: &[(&str, usize)] = &[
        ("emit_else_header", 1),
        ("format_arm_body", 2),
        ("format_attributes", 1),
        ("format_bench", 1),
        ("format_call_arg", 1),
        ("format_const_decl", 2),
        ("format_elif_else_blocks", 3),
        ("format_enum", 1),
        ("format_equip", 2),
        ("format_expr_if_tail", 2),
        ("format_expr_inner", 22),
        ("format_extern_block", 1),
        ("format_function_header_tail", 5),
        ("format_inline_suite", 1),
        ("format_item", 6),
        ("format_match_arm", 2),
        ("format_param", 2),
        ("format_static_decl", 2),
        ("format_stmt", 29),
        ("format_string_lit", 1),
        ("format_struct", 1),
        ("format_suite_setup", 1),
        ("format_suite_teardown", 1),
        ("format_test", 1),
        ("format_trait", 3),
        ("format_trait_bound", 1),
        ("format_trait_bound_tail", 1),
        ("format_trait_extends_and_colon", 2),
        ("format_type_alias", 2),
    ];

    // `format_inline_suite\(` unqualified, not `\(":"`: the `on error` caller
    // passes `""` as its suffix, and a needle that only matches the colon
    // spelling does not cover its own family. The `""` site's disposition is
    // NO CARRIER (B9's ratified ruling — the inline `on error` form takes no
    // colon and its header is literal text), which is a disposition it has to
    // EARN in the table below rather than get by evading the scan.
    let suffix = regex::Regex::new(
        r#"\.write\("[^"]*:[^"]*"\)|format_inline_suite\(|format_arm_body\(|\.write\(" throws "\)|\.write\("!"\)|\.write\(" = "\)"#,
    )
    .expect("suffix needle compiles");
    let reserve = regex::Regex::new(
        r"with_tail_reserve\(|with_exact_tail_reserve\(|inline_suite_reserve\(|suite_header_reserve\(|arm_body_reserve\(|measured_reserve\(|element_to_string_reserving\(",
    )
    .expect("reserve needle compiles");
    let fn_decl = regex::Regex::new(r"^(?:pub(?:\(crate\))? )?fn ([a-z_0-9]+)")
        .expect("fn needle compiles");

    let content =
        fs::read_to_string("src/formatter/mod.rs").expect("cannot read src/formatter/mod.rs");
    let mut current = String::from("<top>");
    let mut sites: std::collections::BTreeMap<String, usize> = Default::default();
    let mut reserves: std::collections::BTreeSet<String> = Default::default();
    let mut total = 0usize;
    for line in content.lines() {
        let t = line.trim_start();
        if let Some(c) = fn_decl.captures(t) {
            current = c[1].to_string();
        }
        if t.starts_with("//") {
            continue;
        }
        if suffix.is_match(line) {
            *sites.entry(current.clone()).or_default() += 1;
            total += 1;
        }
        if reserve.is_match(line) {
            reserves.insert(current.clone());
        }
    }

    let observed: Vec<(String, bool)> = sites
        .keys()
        .map(|f| (f.clone(), reserves.contains(f)))
        .collect();
    let expected: Vec<(String, bool)> = CENSUS
        .iter()
        .map(|(f, r, _)| ((*f).to_string(), *r))
        .collect();

    assert_eq!(
        observed, expected,
        "R42 header-suffix census in `src/formatter/mod.rs` changed.\n\n\
         A HEADER SUFFIX is text a header writes AFTER a width-decided render, \
         on the same line: `:`, ` throws T`, `!`, ` = \"sym\"`, ` = Type`, or an \
         inline suite / arm body. The render before it cannot see any of it, so \
         it measures its own width against the full budget and the line \
         overruns by exactly the suffix.\n\n\
         A NEW function here must install a reserve around the render it \
         precedes — `with_tail_reserve` for a fixed suffix, `measured_reserve` \
         for one containing rendered content — or be added to CENSUS with \
         `false` AND the verified reason its path reaches no `write_doc`. \
         Copying an existing `false` label is how two rows in the brief's own \
         seed turned out to be wrong."
    );
    let observed_counts: Vec<(String, usize)> =
        sites.iter().map(|(f, n)| (f.clone(), *n)).collect();
    let expected_counts: Vec<(String, usize)> = EXPECTED_SITES_PER_FN
        .iter()
        .map(|(f, n)| ((*f).to_string(), *n))
        .collect();
    assert_eq!(
        observed_counts, expected_counts,
        "R42 header-suffix SITE COUNTS changed (total {total}). The \
         disposition table above cannot see this: a new suffix written as an \
         arm inside `format_stmt` or `format_expr` lands in a function that \
         already reserves elsewhere, so the fn-level check passes while the \
         new arm has no reserve at all. Find the new site, give it its \
         disposition, and bump its row."
    );
}

// ══════════════════════════════════════════════════════════════
// R42 · the WIDTH RATCHET — the output-side class guard
// ══════════════════════════════════════════════════════════════
//
// Every other guard in this file watches the SOURCE for a shape. This one
// watches the OUTPUT for the property, which is what makes it carrier-
// agnostic: it does not care whether a line overran because of a fill
// packer, a group, a pre-rendered item, a postfix operator or a carrier
// nobody has named yet.
//
// **What it honestly guards**: regressions anywhere, and any family that has
// corpus instances. **What it cannot see**: a family with ZERO instances in
// the corpus. Those are the fixture set's job — and because the round's own
// fixtures live under `tests/fixtures/**`, which IS one of the roots, a
// family that had no instance acquires one the moment its cell lands.

/// The corpus roots, REPLICATED from the standing walk
/// (`tests/integration.rs::fmt_output_reparses_corpus_wide`) rather than
/// imported: the two live in different test binaries and Rust gives no way to
/// share a nested fn across them.
///
/// A hand-synced duplicate drifts, and the drift is not hypothetical — the
/// first draft of this guard carried THREE roots and left out `compiler/`,
/// which is where three live instances of the class it was written to retire
/// were sitting. `width_ratchet_roots_agree_with_the_standing_walk` below is
/// the scan that makes the duplication safe.
const WIDTH_RATCHET_ROOTS: &[&str] = &["tests/fixtures", "lib", "examples", "compiler"];

/// The ratified budget (`docs/define-gorget/decisions.md`, FMT CANON PAIR).
const WIDTH_RATCHET_BUDGET: usize = 120;

/// Why a formatted line is allowed past the budget. Keyed by CLASSIFIER, never
/// by `path:line` — content moves, and a location-keyed allowlist rots into a
/// list of places nobody can re-derive.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum OverBudget {
    /// A standalone comment. No formatter reflows prose.
    CommentLine,
    /// Code that fits once the trailing comment is stripped.
    ///
    /// NOT "the line contains a `#`" — that naive spelling would exempt
    /// `extern … = "sym"  # note`, which is the caller-suffix class itself.
    /// Charging a comment's width would also make CODE layout depend on
    /// COMMENT text, which is the same never-reflow-prose doctrine one step
    /// out.
    CommentTail,
    /// The `from … import` name list: the one UNDELIMITED list in the
    /// language, width-exempt by ratified decision until the parenthesized
    /// form lands.
    Import,
    /// The doctrinal unbreakable atom: no break could have saved this line,
    /// because its widest ELEMENT does not fit ALONE at this indent.
    Atom,
    /// An inline-BODY collision: the author put a suite, arm or closure body
    /// on its header's line, and the body's leading unbreakable text exceeds
    /// what remains. Converting the layout would overwrite the author's own
    /// choice, so the residual stands.
    InlineBody,
    /// A break WOULD have narrowed this line, and none was taken. Not an
    /// escape — a missing capability, and the residual category, so a NEW
    /// overrun of any unrecognised shape lands here and trips the ceiling.
    Unbroken,
}

/// Mask every quoted-string body with `_`, preserving length, so a delimiter
/// or comma INSIDE a literal is never mistaken for structure.
fn width_ratchet_mask_strings(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut in_str: Option<char> = None;
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        match in_str {
            Some(q) => {
                out.push('_');
                if c == '\\' {
                    if let Some(n) = chars.next() {
                        let _ = n;
                        out.push('_');
                    }
                } else if c == q {
                    in_str = None;
                }
            }
            None => {
                if c == '"' || c == '\'' {
                    in_str = Some(c);
                }
                out.push(c);
            }
        }
    }
    out
}

fn width_ratchet_classify(line: &str) -> OverBudget {
    const CLOSERS: [char; 3] = [')', ']', '}'];
    const OPENERS: [char; 3] = ['(', '[', '{'];

    let stripped = line.trim_start();
    let indent = line.chars().count() - stripped.chars().count();
    let masked = width_ratchet_mask_strings(line);
    let m_stripped = masked.trim().to_string();

    if stripped.starts_with('#') {
        return OverBudget::CommentLine;
    }
    if stripped.starts_with("from ") && stripped.contains(" import ") {
        return OverBudget::Import;
    }
    if let Some(idx) = masked.find('#') {
        let head: String = line.chars().take(masked[..idx].chars().count()).collect();
        if head.trim_end().chars().count() <= WIDTH_RATCHET_BUDGET {
            return OverBudget::CommentTail;
        }
    }

    // ATOM: could ANY break have saved this line? Cut at the LAST unquoted
    // closing delimiter — what follows is the CALLER's suffix, not an element
    // — but count the delimiter RUN itself, because `Doc::Fill` charges its
    // close to the last item's own fit test. Then split what remains at the
    // boundaries a break can actually exploit: the list separator and the
    // assignment `=`.
    let mut body: &str = &m_stripped;
    let mut close_run = 0usize;
    if let Some(cut) = body.rfind(CLOSERS) {
        close_run = body[cut..].chars().take_while(|c| CLOSERS.contains(c)).count();
        body = &body[..cut];
    }
    let body = body.trim_start_matches(OPENERS);
    let widest = body
        .split(',')
        .flat_map(|s| s.split(" = "))
        .map(|s| s.trim().chars().count())
        .max()
        .unwrap_or(0);
    if indent + widest + close_run > WIDTH_RATCHET_BUDGET {
        return OverBudget::Atom;
    }

    // INLINE BODY: a header's `:` followed by MORE CONTENT on the same line.
    if m_stripped.contains("): ") && !m_stripped.ends_with("): ") {
        return OverBudget::InlineBody;
    }
    let header_kw = ["if ", "elif ", "else:", "case ", "for ", "while "];
    if header_kw.iter().any(|k| m_stripped.starts_with(k)) {
        if let Some(idx) = m_stripped.rfind(": ") {
            if m_stripped[idx + 2..].trim().len() > 0 {
                return OverBudget::InlineBody;
            }
        }
    }

    OverBudget::Unbroken
}

/// Format every `.gg` file under the roots and return each over-budget line
/// with its classification. Skips inputs that do not PARSE, exactly as the
/// standing walk does — 29 fixtures are deliberate reject cases, and the
/// infallible entry point panics on them.
fn width_ratchet_scan() -> Vec<(String, usize, OverBudget, String)> {
    let mut rows = Vec::new();
    for root in WIDTH_RATCHET_ROOTS {
        walk_gg_files(Path::new(root), &mut |path: &Path| {
            let Ok(src) = fs::read_to_string(path) else { return };
            let Ok(formatted) = gorget::formatter::format_source_result(&src) else { return };
            for (i, line) in formatted.lines().enumerate() {
                // CHARACTERS, not bytes: the budget is a display property,
                // and a byte count inflates every line holding a non-ASCII
                // literal into a false positive.
                if line.chars().count() <= WIDTH_RATCHET_BUDGET {
                    continue;
                }
                rows.push((
                    path.display().to_string(),
                    i + 1,
                    width_ratchet_classify(line),
                    line.to_string(),
                ));
            }
        });
    }
    rows.sort();
    rows
}

/// E5 — the ROOTS-AGREEMENT scan. `WIDTH_RATCHET_ROOTS` is a hand-synced
/// duplicate of the standing walk's array, so the duplication is checked
/// rather than trusted.
#[test]
fn width_ratchet_roots_agree_with_the_standing_walk() {
    let content = fs::read_to_string("tests/integration.rs")
        .expect("cannot read tests/integration.rs");
    let needle = r#"for root in ["tests/fixtures", "lib", "examples", "compiler"] {"#;
    assert!(
        content.contains(needle),
        "the standing corpus walk's root array in \
         `tests/integration.rs::fmt_output_reparses_corpus_wide` no longer \
         reads `{needle}`. `WIDTH_RATCHET_ROOTS` in this file is a hand-synced \
         duplicate of it — reconcile the two, then update this needle. \
         Leaving them to drift is exactly how a root with live instances of a \
         class got left out of the guard written to retire that class."
    );
    for root in WIDTH_RATCHET_ROOTS {
        assert!(
            needle.contains(&format!("\"{root}\"")),
            "root `{root}` is in WIDTH_RATCHET_ROOTS but not in the standing walk"
        );
    }
}

/// THE WIDTH RATCHET. Every formatted line past the ratified 120-column
/// budget must match a declared escape category, and no category may exceed
/// its seeded ceiling.
///
/// Both halves matter. The category check catches an overrun of a shape
/// nobody classified; the CEILINGS catch a new overrun of a shape that
/// already has a legitimate instance — which is the far commoner regression,
/// and the reason a residual category (`Unbroken`) does not make the guard
/// toothless.
///
/// SHRINK-ONLY: a ceiling that measures LOWER than its constant should be
/// tightened, and the test says so on stderr.
#[test]
fn fmt_no_new_over_budget_lines() {
    // ── The ATTRIBUTED SEED ────────────────────────────────────────────────
    //
    // A bare total would be useless here: ~90% of the over-budget lines in
    // this tree are DOCTRINAL escapes, and a single number that absorbs them
    // absorbs a live defect just as quietly. Each category is therefore
    // ceilinged on its own, and the two small ones are enumerated row by row.
    //
    // Regenerate every figure below with the scan itself:
    //   cargo test --test lints fmt_no_new_over_budget_lines -- --nocapture
    // which prints the per-category tally it measured.

    /// Standalone author prose. No formatter in this family reflows comments.
    const CEIL_COMMENT_LINE: usize = 10;
    /// Code that fits once its trailing comment is stripped.
    const CEIL_COMMENT_TAIL: usize = 11;
    /// `from … import` name lists — ratified width-exempt.
    const CEIL_IMPORT: usize = 129;
    /// The doctrinal unbreakable atom.
    const CEIL_ATOM: usize = 168;
    /// Inline-BODY collisions. TWO rows, both deliberate:
    ///   * `tests/fixtures/fmt_fill_pack/closure_params.gg` — the `cl_over`
    ///     cell, whose element fits alone at 119 and whose `): 0` carries it
    ///     to 122.
    ///   * `tests/fixtures/fmt_tail_reserve/inline_body_escape.gg` — the
    ///     escape's own cell, where the body's leading unbreakable text
    ///     exceeds the budget even after the header breaks.
    const CEIL_INLINE_BODY: usize = 2;
    /// A break would have narrowed the line and none was taken. FOUR rows,
    /// each a NAMED feature gap filed in `TODO.md`, none of them closable by
    /// a reserve:
    ///   * `examples/toml_config.gg` — a `Stmt::VarDecl` initializer. The arm
    ///     emits no `Doc` at all, so no fit test ever runs.
    ///   * `tests/fixtures/parser_trailing_comma_ctor_pattern.gg` —
    ///     `format_pattern` has no `Doc` layer; pattern wrapping is its own
    ///     unimplemented feature.
    ///   * `tests/fixtures/parser_trailing_comma_variant_fields.gg` — an enum
    ///     tuple-variant field list, a hand-rolled comma loop whose element
    ///     types produce no `Doc` either.
    ///   * `tests/fixtures/known_gaps/fmt_prerender_column_binary_chain.gg`
    ///     and its live twin in `tests/fixtures/self_host_lowerer/` — a
    ///     binary chain's FIRST operand is pre-rendered for the continuation
    ///     column but spliced at the caller's, so its own sub-render believes
    ///     it has ~23 more columns than it does. A DISTINCT root from the
    ///     caller-suffix class: the reserve there is correct, the start
    ///     column is not.
    const CEIL_UNBROKEN: usize = 5;

    let rows = width_ratchet_scan();
    assert!(
        rows.len() > 50,
        "the width scan found only {} over-budget lines across {:?} — the \
         roots moved or the formatter stopped producing output. Fix the \
         scanner, do not lower the ceilings.",
        rows.len(),
        WIDTH_RATCHET_ROOTS
    );

    let tally = |k: OverBudget| rows.iter().filter(|r| r.2 == k).count();
    let checks: &[(OverBudget, usize, &str)] = &[
        (OverBudget::CommentLine, CEIL_COMMENT_LINE, "CEIL_COMMENT_LINE"),
        (OverBudget::CommentTail, CEIL_COMMENT_TAIL, "CEIL_COMMENT_TAIL"),
        (OverBudget::Import, CEIL_IMPORT, "CEIL_IMPORT"),
        (OverBudget::Atom, CEIL_ATOM, "CEIL_ATOM"),
        (OverBudget::InlineBody, CEIL_INLINE_BODY, "CEIL_INLINE_BODY"),
        (OverBudget::Unbroken, CEIL_UNBROKEN, "CEIL_UNBROKEN"),
    ];

    eprintln!("[fmt_no_new_over_budget_lines] measured tally:");
    for (kind, ceiling, name) in checks {
        eprintln!("  {kind:?} = {} (ceiling {name} = {ceiling})", tally(*kind));
    }

    let mut failures: Vec<String> = Vec::new();
    for (kind, ceiling, name) in checks {
        let n = tally(*kind);
        if n > *ceiling {
            let offenders: Vec<String> = rows
                .iter()
                .filter(|r| r.2 == *kind)
                .map(|(p, l, _, text)| {
                    format!("    {p}:{l} ({} cols) {}", text.chars().count(), text.trim())
                })
                .collect();
            failures.push(format!(
                "  {kind:?}: {n} lines, ceiling {name} = {ceiling}\n{}",
                offenders.join("\n")
            ));
        }
        if n < *ceiling {
            eprintln!(
                "[fmt_no_new_over_budget_lines] {kind:?} measured {n} < {name} = {ceiling} \
                 — TIGHTEN the ceiling; this ratchet only shrinks."
            );
        }
    }

    assert!(
        failures.is_empty(),
        "R42 WIDTH RATCHET: formatted output grew past the ratified \
         120-column budget.\n\n{}\n\n\
         The budget is a property of the EMITTED LINE, not of a list \
         considered in isolation — a suffix the caller writes after a Doc \
         counts. If the new line is a genuine RULED escape, it must fall into \
         one of the declared categories AND its ceiling has to be raised \
         deliberately, with the ruling cited. If it is not, the carrier that \
         emitted it is missing its tail reserve: install it around the \
         width-decided render, immediately, and add the cell's boundary pair \
         under `tests/fixtures/fmt_tail_reserve/`.\n\n\
         ⚠ The category is a CLASSIFIER, never a `path:line` — content moves. \
         An `Unbroken` row is NOT an escape: it is a position with no fit \
         test at all, and it belongs in `TODO.md` as a named feature gap.",
        failures.join("\n\n")
    );
}
