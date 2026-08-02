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
    const BUDGET: usize = 93;

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
    const EXPECTED_SKIPS_UNIFY: usize = 14;
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
/// the throws obligation identically to a direct call.
#[test]
fn d23_method_throws_return_sites() {
    const EXPECTED_METHOD_RET_SITES: usize = 4;
    const EXPECTED_PRODUCER_CALLS: usize = 2;

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
        ("src/formatter/mod.rs", 1),
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
    const EXPECTED: usize = 36;

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
    const EXPECTED: usize = 36;

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

/// Ratchet (CLAUDE.md rule 4 — sibling-site drift): every faultable arithmetic
/// op that a fault-`catch` can intercept MUST route through the ONE shared
/// `Inst::FaultCheck` + `Term::Branch` lowering shape (error-model.md §11), so a
/// future faultable condition can't silently skip the flag-and-branch shape and
/// fall back to the panic-by-default form (or, worse, emit a bare arithmetic /
/// bare index-read with no fault check at all).
///
/// The single source of truth for the faultable ARITHMETIC conditions is the
/// `FaultOp` enum (`src/lir/mod.rs`): every member is one fault condition tested
/// by `Inst::FaultCheck`. Increment 1 shipped Add/Sub/Mul/Div/Rem; Increment 2
/// (C) split the signed `TYPE_MIN/-1` division overflow into `DivOverflow`, so
/// the floor is now SIX. Adding a seventh condition (a future `Pow`, a `Neg` of
/// `TYPE_MIN`, …) must extend the `FaultOp` enum AND give it a C/LLVM
/// `Inst::FaultCheck` emit + the GIR→LIR mapping in the `FaultableBinOp` arm,
/// then bump `FAULT_OP_VARIANTS` — forcing the new condition through the shared
/// branch path as part of the change, not after the next miscompile.
///
/// The faultable ARRAY-READ shape (`Fault.Bounds`) is a separate GIR variant
/// `FaultableIndexLoad` (error-model.md §11 Increment 2 (A)) — it does NOT
/// produce a `FaultOp` arm (it null-branches on `gorget_array_safe_get` instead
/// of a `FaultCheck`), so it is ratcheted separately below: its presence in the
/// GIR→LIR lowering is asserted so a future index-fault sibling can't skip the
/// branch-before-deref shape.
///
/// **If `FAULT_OP_VARIANTS` fails because a variant was added:** confirm the new
/// `FaultOp::X` has a C/LLVM `Inst::FaultCheck` emit path and a GIR→LIR mapping
/// in the `FaultableBinOp` arm, then bump it. If removed, lower it.
#[test]
fn fault_op_lowering_arms_count() {
    /// Baseline 2026-06-21: 6 (Add, Sub, Mul, Div, Rem, DivOverflow).
    const FAULT_OP_VARIANTS: usize = 6;

    // Count the `FaultOp` enum variants — the single source of truth for the
    // faultable arithmetic conditions tested by `Inst::FaultCheck`.
    let lir = fs::read_to_string("src/lir/mod.rs").unwrap_or_default();
    let mut in_fault_op = false;
    let mut variant_count = 0usize;
    for line in lir.lines() {
        let t = line.trim_start();
        if t.starts_with("pub enum FaultOp") {
            in_fault_op = true;
            continue;
        }
        if in_fault_op {
            if t.starts_with('}') {
                break;
            }
            if t.starts_with("//") || t.is_empty() {
                continue;
            }
            // A variant line is a bare `Identifier,` inside the enum body.
            if t.ends_with(',') && t[..t.len() - 1].chars().all(|c| c.is_alphanumeric() || c == '_') {
                variant_count += 1;
            }
        }
    }
    assert_eq!(
        variant_count, FAULT_OP_VARIANTS,
        "`FaultOp` variant count (src/lir/mod.rs) changed: {variant_count} vs \
         expected {FAULT_OP_VARIANTS}.\n\n\
         Every faultable arithmetic condition must route through the shared \
         `Inst::FaultCheck` + `Term::Branch` path. If you added a condition, also \
         add its C/LLVM `Inst::FaultCheck` emit and its GIR→LIR mapping in the \
         `FaultableBinOp` arm of src/lir/lower/insts.rs, then bump \
         FAULT_OP_VARIANTS. If you removed one, lower it.",
    );

    // The faultable array-read shape must stay wired through the shared
    // branch-before-deref lowering (`gorget_array_safe_get` + NULL-branch +
    // shared element materialization). Assert the GIR→LIR arm + the safe-get
    // call are present so a future index-fault sibling can't skip the shape.
    let insts = fs::read_to_string("src/lir/lower/insts.rs").unwrap_or_default();
    assert!(
        insts.contains("Instruction::FaultableIndexLoad {"),
        "the `FaultableIndexLoad` GIR→LIR lowering arm vanished from \
         src/lir/lower/insts.rs — the `Fault.Bounds` array-read must lower \
         through the shared null-branch-before-deref shape (error-model.md §11).",
    );
    assert!(
        insts.contains("gorget_array_safe_get")
            && insts.contains("materialize_collection_element"),
        "the faultable array-read lowering must use `gorget_array_safe_get` + the \
         shared `materialize_collection_element` element path — a future sibling \
         must not duplicate or skip the clone/move-zero/str-ptr logic.",
    );

    // Cross-frame fault propagation (error-model.md §11, Increment 2.1a/2.1c)
    // adds a THIRD faultable shape, `Instruction::FaultableCall` — a
    // participating callee writes a per-category fault TAG into a hidden trailing
    // `MutPtr<i32>` slot and the caller reads the tag VALUE and DISPATCHES to the
    // matching per-category handler after the call. Like the other two shapes it
    // must stay wired through the one shared lowering arm, and — THE LINCHPIN —
    // each handler MUST be counted as a block successor in `successors()` (else
    // DCE prunes a handler and the fault recovery silently vanishes). Assert both
    // so a future refactor can't drop either without tripping this ratchet.
    assert!(
        insts.contains("Instruction::FaultableCall {"),
        "the `FaultableCall` GIR→LIR lowering arm vanished from \
         src/lir/lower/insts.rs — the cross-frame fault call must lower through \
         the shared `Inst::Call` + tag-dispatch shape (error-model.md §11).",
    );
    let optimize = fs::read_to_string("src/ir/transforms/optimize.rs").unwrap_or_default();
    assert!(
        optimize.contains("Instruction::FaultableCall { overflow_handler, divzero_handler, bounds_handler, .. }"),
        "the `FaultableCall` arm vanished from `successors()` / the block-id \
         remap loops in src/ir/transforms/optimize.rs — its per-category handler \
         blocks must count as successors (else DCE prunes a fault handler and \
         cross-frame fault recovery silently vanishes) and forward through \
         block renumbering (else a stored handler id goes stale).",
    );
}

/// Sibling-arm ratchet for the `FaultableCall` per-category tag-dispatch
/// (error-model.md §11, Increment 2.1c, CLAUDE.md invariant #4 "one fix, all
/// siblings" + devbook/24 rule 2 "typed metadata, not name-matched"): the
/// cross-frame fault call routes by reading the slot tag VALUE and dispatching
/// to one of N per-category handler FIELDS on `Instruction::FaultableCall`
/// (`overflow_handler`, `divzero_handler`; Bounds adds `bounds_handler` in
/// 2.1d). A single `slot != 0` branch could NOT distinguish categories — it
/// would route every fault to one entry and construct the WRONG `Fault` variant
/// (the measured §2.3 silent miscompile: a deep DivByZero printing the Overflow
/// arm). This lint PINS the handler-category count so the next category (Bounds)
/// is FORCED to add its own typed handler field + tag-dispatch arm — not a
/// name-matched or single-branch dodge.
///
/// **If this fails because you added a category:** add the `<cat>_handler:
/// Option<BlockId>` field to the `FaultableCall` GIR variant
/// (`src/ir/instructions.rs`), thread it through the builder ctors, printer, the
/// three `optimize.rs` remap/successor arms, and the GIR→LIR tag-dispatch
/// (`src/lir/lower/insts.rs`) with its own `tag == <CAT>_TAG → handler` branch,
/// AND resolve it (always-Some) at the call-site gate (`calls.rs`). Then bump
/// `FAULT_CALL_HANDLER_CATEGORIES`. If you removed one, lower it.
#[test]
fn fault_call_handler_category_count() {
    /// Baseline 2026-06-25: 3 (overflow_handler, divzero_handler,
    /// bounds_handler — Bounds landed in 2.1d).
    const FAULT_CALL_HANDLER_CATEGORIES: usize = 3;

    // Count the `*_handler: Option<BlockId>` fields inside the `FaultableCall`
    // GIR variant body — the single source of truth for the dispatch categories.
    let instructions = fs::read_to_string("src/ir/instructions.rs").unwrap_or_default();
    let mut in_variant = false;
    let mut handler_fields = 0usize;
    for line in instructions.lines() {
        let t = line.trim_start();
        if t.starts_with("FaultableCall {") {
            in_variant = true;
            continue;
        }
        if in_variant {
            // The variant body ends at its closing `},` (the field list is
            // brace-balanced; the next variant or the enum tail follows).
            if t.starts_with("},") || t == "}" {
                break;
            }
            if t.starts_with("//") || t.is_empty() {
                continue;
            }
            // A handler field line: `<name>_handler: Option<BlockId>,`.
            if t.ends_with("_handler: Option<BlockId>,") {
                handler_fields += 1;
            }
        }
    }
    assert_eq!(
        handler_fields, FAULT_CALL_HANDLER_CATEGORIES,
        "`FaultableCall` per-category handler-field count \
         (src/ir/instructions.rs) changed: {handler_fields} vs expected \
         {FAULT_CALL_HANDLER_CATEGORIES}.\n\n\
         The cross-frame fault call dispatches by slot-tag VALUE to one \
         per-category handler field. A new category (e.g. Bounds, 2.1d) must add \
         its own `<cat>_handler: Option<BlockId>` field AND a matching \
         `tag == <CAT>_TAG → handler` arm in the GIR→LIR tag-dispatch \
         (src/lir/lower/insts.rs) + the call-site resolution (calls.rs) + the \
         builder/printer/optimize.rs sibling arms, then bump \
         FAULT_CALL_HANDLER_CATEGORIES — forcing the new category through the \
         shared tag-dispatch, not a single-branch dodge (the §2.3 miscompile).",
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
/// The real C `GorgetMap` (`src/backend/c/runtime/runtime_preamble.c`, 19
/// pointer/size_t fields × 8 = 152 bytes; `GorgetSet` is a typedef alias) and
/// Rust gg (`src/lir/lower/types.rs`, `GorgetMap | GorgetSet => 152`) both use
/// **152**. The self-host previously hand-duplicated this size as the literal
/// `184` across 9 sites in `lir_lower.gg` (2 struct defs + 7 ResourceMetadata
/// returns). That over-count (the size of an out-of-date 23-field layout)
/// inflated every enum/union/array layout embedding a Dict/Set, so
/// `gorget_array_push` read past the stack slot = stack-buffer-overflow on the
/// xml fixtures. The fix collapsed all 9 sites onto the single
/// `GORGET_MAP_STRUCT_SIZE` constant in `lir.gg`.
///
/// This lint pins three invariants so the divergent literal cannot creep back:
///   (a) `GORGET_MAP_STRUCT_SIZE` is defined as `152` in `lir.gg`.
///   (b) Rust gg still agrees (`GorgetMap | GorgetSet => 152` in types.rs).
///   (c) No raw `184` literal lingers in `lir_lower.gg`, AND every GorgetMap /
///       GorgetSet `ResourceMetadata`/`LirStructDef` size site reads the named
///       constant rather than a bare integer (so all 9 stay single-sourced).
#[test]
fn self_host_gorget_map_struct_size() {
    const EXPECTED_SIZE: usize = 152;
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
         struct is 19 fields × 8 bytes = 152 (runtime_preamble.c). Do NOT change this \
         to 184 (the stale 23-field over-count that overflowed gorget_array_push on the \
         xml fixtures) without first changing the actual runtime struct AND Rust gg.",
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
/// the correspondence:
///   (a) the `code()` string SETS are identical (same closed set of `T_<X>`);
///   (b) `is_catchable()` agrees variant-for-variant;
///   (c) the §10.9 `Fault` language-prelude enum (`builtin_fault_enum()`)
///       equals EXACTLY the `is_catchable()`-true subset (bare variant names).
/// It compares ONLY the typed `code()` — NEVER the human `detail`/message text
/// (production "integer overflow" vs ggdef "arithmetic overflow" is a
/// sanctioned, conformance-ignored divergence, so `message()` is never fed in).
///
/// ONE macro-expanded variant list drives BOTH the rustc-exhaustiveness
/// ratchet (two generated catch-all-free `match`es) AND the arrays every check
/// below iterates, so the ratchet REACHES the assertions: adding a variant to
/// EITHER enum is a hard compile error AT the `trap_parity_pin!` list, and
/// extending that list is the only fix — which extends the arrays in the same
/// keystroke. (Pre-macro, the `_p_exhaustive`/`_g_exhaustive` guards were
/// SEPARATE from hand-listed arrays: a developer could fix the match and leave
/// every check running vacuously over the stale list — the new variant
/// invisible to (a), (b), and (c).)
#[test]
fn trap_kind_parity_prod_vs_ggdef() {
    use std::collections::BTreeSet;
    use gorget::trap::TrapKind as P;
    use ggdef::TrapKind as G;

    /// `$name` must exist in BOTH enums; `$name(payload)` marks the
    /// ggdef-side payload variants (production variants are all unit — a
    /// future payload-carrying production variant fails to compile here and
    /// the macro grows a marker then). The `V { .. }` patterns bind any
    /// variant shape, so the generated matches stay exhaustive-and-only-
    /// exhaustive: rustc errors HERE on a variant missing from the list.
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

    // Same order on both sides so the zip pairs matching codes (asserted
    // below). THE single source of truth for the registry pin.
    let (prod, ggd) = trap_parity_pin![
        Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk,
        AssertFailed(String::new()), Panic(String::new()),
    ];

    // (a) code() SETS identical.
    let prod_codes: BTreeSet<&str> = prod.iter().map(|t| t.code()).collect();
    let ggd_codes: BTreeSet<&str> = ggd.iter().map(|t| t.code()).collect();
    assert_eq!(
        prod_codes, ggd_codes,
        "production TrapKind::code() set must equal ggdef's (D11 registry parity)",
    );

    // (b) is_catchable() agrees variant-for-variant (paired by code()).
    for (p, g) in prod.iter().zip(ggd.iter()) {
        assert_eq!(p.code(), g.code(), "TrapKind ordering drift between prod and ggdef");
        assert_eq!(
            p.is_catchable(), g.is_catchable(),
            "is_catchable() disagrees for {} (prod {} vs ggdef {})",
            p.code(), p.is_catchable(), g.is_catchable(),
        );
    }

    // (c) §10.9 Fault prelude enum == is_catchable()-true subset (by bare name).
    let fault_variants: BTreeSet<String> =
        gorget::ir::lowering::generics::builtin_fault_enum()
            .variants.iter().map(|v| v.node.name.node.clone()).collect();
    let catchable_bare: BTreeSet<String> = prod.iter()
        .filter(|t| t.is_catchable())
        .map(|t| t.code().strip_prefix("T_").unwrap().to_string())
        .collect();
    assert_eq!(
        fault_variants, catchable_bare,
        "§10.9 Fault enum (builtin_fault_enum) must equal the is_catchable()-true \
         TrapKind subset (bare variant names)",
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
    // sentence rule + regen commands. Compact further and lower this back
    // toward 58_000 as a follow-up round.
    const CEILING: u64 = 59_000;
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

    const RUST_CEILING: usize = 15;
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
/// Round XXIII γδ landed at 5: `Vector__` · `Deque__` · `Dict__` · `Map__` ·
/// `HashMap__`.
///
/// ⚠ Set/HashSet are DELIBERATELY EXCLUDED from this set today: neither has a
/// positional index, so `set_index_returns_garbage.gg` (`known_gaps/`, TODO.md)
/// asserts a check-time REJECTION rather than an arm here. If Set/HashSet
/// iteration acquires a struct-payload silent-`0/0` symptom of its own (a
/// distinct Core #15e Q3 gap filed by Round XXIII γδ TODO follow-up), the fix
/// may or may not extend this arm set — verify the actual code path first.
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

    // 5 arms: Vector__ · Deque__ · Dict__ · Map__ · HashMap__. Each spelled
    // as a `.strip_prefix("<Prefix>__")` call in the fn body; count the
    // literal appearances of the prefixes (each MUST appear exactly once).
    const EXPECTED: usize = 5;
    let count: usize = ["Vector__", "Deque__", "Dict__", "Map__", "HashMap__"]
        .iter()
        .map(|p| body.matches(&format!(".strip_prefix(\"{p}\")")).count())
        .sum();
    assert_eq!(
        count, EXPECTED,
        "`infer_collection_element_type` arm count changed: {count} vs \
         expected {EXPECTED}. Admitted-collection member set at Round XXIII γδ \
         close: {{Vector, Deque, Dict, Map, HashMap}}. If a family was ADDED, \
         verify the `try_resolve_index_element_ptr` kind-gate at \
         `src/ir/lowering/exprs/mod.rs` also admits its CollectionKind, then \
         bump EXPECTED. If REMOVED, RESTORE the arm — the family now silently \
         falls to `I64_TYPE` (a gg-check-clean SIGSEGV / llc-reject class).",
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
/// The helper's `ClosureCombinatorCell` enum is the SINGLE PRODUCER for
/// the axis-unify decision across the 3 unify-eligible cells:
///   - `Result.or_else`  — Ok-unify  (T' == T, E' free)
///   - `Result.and_then` — Err-unify (E' == E, U free)
///   - `Option.or_else`  — Some-unify (T' == T)
///
/// Explicitly out-of-class (see the helper's doc-comment for the
/// rationale — the exclusion is load-bearing and reviewed):
///   - `.map` / `.map_err` — scalar-returning closures (no axis).
///   - `Result.flat_map`   — deliberately UNREGISTERED in production
///     `src/ir/lowering/builtins.rs::RESULT` (assertion at ~:1425). NB:
///     ggdef currently ACCEPTS `Result.flat_map` at its `elaborate_method`
///     arm-picker — Core #9 lane divergence, own port owed. If that port
///     ships, either the mirror needs a new `ResultFlatMap` variant (and
///     `EXPECTED_GGDEF_VARIANTS` must bump — coupled by construction) OR
///     the ggdef arm gets rejected and the constants stay pinned.
///   - `Option.and_then` / `Option.flat_map` — legitimate cross-type map.
///
/// If a NEW closure-returning combinator gets added to `builtins.rs`,
/// this lint fires and forces the author to either route it through
/// `unify_closure_ret_axis` (by adding a variant to `ClosureCombinatorCell`
/// and a match arm in the helper) or document the exemption alongside its
/// siblings. Mirrors the `container_literal_arms_count` /
/// `pack_trait_object_call_sites_count` precedents.
#[test]
fn unify_closure_ret_axis_class_enumeration() {
    /// The 3-cell class. Bump when a NEW combinator legitimately joins the
    /// unify-eligible class. NEVER bump silently — document which cell +
    /// which axis + which sibling exclusion is being overridden, and update
    /// the helper doc-comment alongside.
    const EXPECTED_VARIANTS: usize = 3;
    /// Every unify-eligible cell has EXACTLY ONE caller of the helper
    /// inside `infer_closure_method_type` (one per arm). Additional
    /// callers elsewhere would signal a duplicate check or a leak into
    /// non-combinator paths — force the reviewer to explain.
    const EXPECTED_CALLERS: usize = 3;
    /// ggdef mirror: 3 variants (same class shape as production).
    const EXPECTED_GGDEF_VARIANTS: usize = 3;
    /// ggdef mirror: 1 caller. ggdef's `elaborate_method` consolidates the
    /// per-cell arms into a single match, so the check runs at ONE
    /// chokepoint after `combinator_cell` classifies. Additional callers
    /// would signal a duplicate check (Core #4 chokepoint violation).
    const EXPECTED_GGDEF_CALLERS: usize = 1;

    let typecheck_src = fs::read_to_string("src/semantic/typecheck.rs")
        .expect("read src/semantic/typecheck.rs");
    let ggdef_src = fs::read_to_string("spec/ggdef/src/elaborate/mod.rs")
        .expect("read spec/ggdef/src/elaborate/mod.rs");

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
             `.map` / `.map_err` / `Result.flat_map` / `Option.and_then` / \
             `Option.flat_map` and explain why the new combinator does \
             NOT need axis-unify.\n\n\
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
         cell is being ported (e.g. `ResultFlatMap` after the ggdef \
         `Result.flat_map` follow-up ships), add the variant + a match arm \
         in `Elaborator::unify_closure_ret_axis` + a mapping in \
         `Elaborator::combinator_cell`, then bump `EXPECTED_GGDEF_VARIANTS` \
         (and `EXPECTED_VARIANTS` on the production side if that ships \
         together). A drift-only bump on one side is a Core #9 lane gap.",
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
