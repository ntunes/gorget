# Executor brief: RV-A — field-access soundness (typed wrapper flag + deref-reject staging + self-host mirror)

> **Status:** v5 — pass-5 folded (R1 BLOCKING: the GuardAccept row's ABSENT-field behavior was
> unspecified — the permissive reading ships `guard.nonexistent` → garbage-0, reopening the
> exact Core-#8 class for guards; the table now has the GuardAccept row: present → ACCEPT,
> absent → E_NoFieldFound (the inner-resolution prober already yields it), + the
> `fieldaccess_guard_missing_field_reject.gg` NEG. R2: two stale `is_deref_wrapper`-as-field
> mentions reworded (the predicate is `deref_wrapper_kind.is_some()`). R3: the ggdef-lane
> claim corrected to the two-mechanism reality — corpus_b + spec_conformance_ggdef FILTER;
> converter_agreement + coverage_histogram read everything but TOLERATE reject fixtures
> (uncounted/bucketed); conclusion unchanged, M4's ggdef gate is the backstop. R4: M1 now
> states the bool→Option adaptation + the per-registry variant maps.) Pass-4 folded (R1 should-fix: ONE field, not two — `deref_wrapper_kind:
> Option<DerefWrapperKind>`, default `None` in BOTH DefInfo ctors (scope.rs:216/:293); `None`
> = not-a-wrapper (subsumes the leg-1 bool via `.is_some()`); no bare-enum default landmine,
> one source of truth per axis (rule 3), M2 now zero-judgment. R2: the stale "verify which
> applies" ggdef-exclusion remnant reconciled to R6's answer (non-spectest placement, no
> EXCLUDE). R3: the `Box[int].x` primitive-inner NEG added to M3.)
> Pass-3 folded (2 BLOCKING, both pre-resolutions: R1 the first table row is
> **{Box} ONLY** — §9.4 (language-design.md:1707-1712) names Box as its sole deref example;
> Weak's design access is `.upgrade()` (§9.2), so E_DerefCoercionUnimplemented would LIE for
> it; Shared/Weak/Mutex/RWLock all → E_NoFieldFound (consistent with M3 as written; promoting
> Shared later = a §9.4 doc change + enum reseed, owner call). R2 the second distinction is a
> **3-WAY typed enum** `DerefWrapperKind { GuardAccept, DerefTarget, NonDerefContainer }` on
> DefInfo, seeded once at the two registration sites (the allowed compute_drop_taint-precedent
> name-match), read via accessor at the reject site — one boolean collapses Box-vs-Mutex and
> forces a consumer name-match. Plus the mechanical note: the `definitely_absent` boolean arm
> (typecheck.rs:2822 emits outside the match) must restructure to a 3-valued result or inline
> emits to carry the second code.) Pass-2 folded (SIX: R1 the partition is GUARDS-vs-CONTAINERS — pass-2's
> 8-wrapper probe matrix shows Mutex/RWLock DIRECT field access is ALSO garbage-0 (the green
> fixture only touches the guards) → reject = {Box, Shared, Weak, Mutex, RWLock}, accept =
> {Guard, ReadGuard, WriteGuard}; blast radius of the wider set must be RE-MEASURED by the
> executor, not assumed. R2 the ledger clarification had silently NO-OPPED (pre-assertion
> str.replace) — now genuinely recorded at decisions.md with the corrected partition. R3 the
> reject site needs a 3-WAY DIAGNOSTIC TABLE (below) and the M2 leg is PROTOTYPE-FIRST. R4
> guard POS controls + container NEGs added to M3. R5 §9.4 = the DESIGN-DOC section
> (language-design.md:1708 — the reference's §9.4 is a different topic); bump
> diagnostic-codes.md's "93 E_ codes" count to 94. R6 fixture placement pre-stated: ALL new
> fixtures live in tests/fixtures/ — NEVER spectests/run/ (`reject_*` there gets pulled into
> the ggdef lane); no corpus EXCLUDE needed — every ggdef lane filters by prefix/dir and
> skips fieldaccess_*/reject_* in tests/fixtures/.) Pass-1 folded (4 reservations: R1 BLOCKING — the Option-C reject is NARROWED
> to the verified-broken subset (Box; probe Shared/Weak) because guard/lock field access WORKS
> (green fixtures `guard_struct_field`/`guard_rwlock_field` — rejecting them would break
> working features; ledger scope-clarification recorded); R2 BLOCKING — the METHOD auto-deref
> reject is SCOPED OUT (un-prototyped resolution-path work; cc-fails loudly today, no silent
> garbage; deferred to the deref-backend track per the scout's original filing); R3 — the new
> diagnostic is SPECIFIED: new SemanticErrorKind + `E_DerefCoercionUnimplemented` + registry
> row + message "field `<f>` exists on `<Inner>` but deref coercion (§9.4) is not yet
> implemented for `<Wrapper>`" — NOT a reused E_NoFieldFound (which would lie); R4 — M2
> SUPERSEDES the patch's Option-B inner-check accept-branch (only the typed-flag retirement
> survives from that hunk). Plus: the new negatives are explicitly EXCLUDED from the ggdef
> conformance lane, not comment-noted.) Awaiting the next fresh pass. **Scout basis (read both FIRST):**
> `docs/plans/define-gorget/scouts/scout-rva-fieldaccess.md` + the proven patch
> `docs/plans/define-gorget/scouts/patches/rva_proto.patch` (189 lines, 5 files; lib 1107/0;
> wrapper/box/field/deref 94/0/1; self-host RTPrimitive reject 0 FPs across 1547 fixtures).
> ⚠ The patch implements Option B; the OWNER RULED Option C on 2026-07-16 (ledger LOG) — M2
> below EXTENDS the patch to the ruled scope. **Model policy:** executor + brief-reviews Opus;
> output-review on Fable.

## Objective (three legs)

1. **Typed wrapper metadata (rule 2 + rule 3):** retire the `is_field_deref_wrapper`
   NAME-match (`typecheck.rs:189`, `:~2803`) with **ONE typed field**
   `deref_wrapper_kind: Option<DerefWrapperKind>` (default `None` in both DefInfo ctors,
   scope.rs:216/:293 — `None` = not a wrapper; `.is_some()` gives the leg-1 predicate;
   the `Some(kind)` carries the 3-way split) seeded at BOTH
   registries — `BUILTIN_GENERIC_TYPES` (resolve.rs:19) AND the builtin-module `.gg` structs
   (Box in collections.gg; RWLock/ReadGuard/WriteGuard in sync.gg), the struct site gated on
   `is_builtin_module(path)` — so a USER `struct Guard` gets flag=false and stops escaping
   `E_NoFieldFound` (`g.y` printing garbage 0 today, measured). One allowed seed name-match at
   registration (the `compute_drop_taint` precedent); every downstream read is the typed flag.
2. **OWNER-RULED STAGING (Option C + the ledger SCOPE CLARIFICATION, decisions.md
   2026-07-16 — genuinely recorded, verified in-file):** the partition is
   **GUARDS-vs-CONTAINERS** (pass-2's full 8-wrapper probe matrix):
   **ACCEPT (works today, green fixtures): Guard, ReadGuard, WriteGuard.**
   **REJECT (direct field access = silent garbage-0, all measured): Box, Shared, Weak,
   Mutex, RWLock.** Wrapper METHOD auto-deref is OUT OF SCOPE (cc-fails loudly; the
   deref-backend track owns it). The reject needs a SECOND typed distinction beyond
   the is-a-wrapper predicate (`deref_wrapper_kind.is_some()`, true for all 8) — the SAME `deref_wrapper_kind` field's `Some(kind)` values (Box → DerefTarget;
   Guard/ReadGuard/WriteGuard → GuardAccept; Shared/Weak/Mutex/RWLock → NonDerefContainer —
   NOT a second field; one axis, one source of truth), read via a typed accessor at the
   reject site — never name-matched at the consumer.
   **THE 3-WAY DIAGNOSTIC TABLE (R3 — the reject site keys on
   (field-present-on-inner × wrapper-is-§9.4-deref-target)):**
   | case | code | example |
   |---|---|---|
   | `DerefWrapperKind::GuardAccept` + field PRESENT on inner | ACCEPT (works today — green fixtures) | `guard.x` |
   | `DerefWrapperKind::GuardAccept` + field ABSENT on inner | `E_NoFieldFound` (pass-5: never silently accept into error_id/garbage — the prober already resolves the inner) | `guard.nonexistent` |
   | present on inner AND `DerefWrapperKind::DerefTarget` (**{Box} only** — §9.4's sole named target; pre-resolved pass-3) | `E_DerefCoercionUnimplemented` — "field `x` exists on `Point` but deref coercion (design-doc §9.4) is not yet implemented for `Box`" | `Box[P].x` |
   | absent on inner | `E_NoFieldFound` (the §9.4 message would lie) | `Box[P].nonexistent` |
   | primitive inner | `E_NoFieldFound` | `Box[int].x` |
   | `DerefWrapperKind::NonDerefContainer` ({Shared, Weak, Mutex, RWLock} — Weak accesses via `.upgrade()` §9.2, never deref; the others via .lock()/.read()/.get()) | `E_NoFieldFound` even when the field exists on the inner | `Shared[P].x`, `Weak[P].x`, `Mutex[P].x`, `RWLock[C].port` |
   **The M2 leg is PROTOTYPE-FIRST:** before wiring fixtures, the executor implements the
   table at the reject site (`typecheck.rs:2801-2831` definitely_absent arm), keeps the
   patch's inner-resolution block (patch lines ~103-125) as the field-present prober but
   flips its outcome from accept to the table's codes — NOTE the current arm computes a
   BOOLEAN `definitely_absent` and emits outside the match (typecheck.rs:2822); carrying the
   second code requires restructuring to a 3-valued result or inline emits (pass-3) —, RE-MEASURES the blast radius of the
   5-container reject across the full targeted battery (the "1 fixture" figure was
   Box-only), and reports the measured count before proceeding. Registry: new row + bump the header count BY ONE from whatever it currently reads
   (`spec/prose/diagnostic-codes.md:12` — the curation track moved it 93→95 in the same
   round; never hardcode the target number, read-then-increment). This REVERSES the prior `check_gg_ok` staging: flip `fieldaccess_box_field_ok` to a
   reject fixture (keep the `#[ignore]`d run-twin asserting the CORRECT future `7` — cite the
   deref-backend TODO entry), and `Box[P].nonexistent` rejects via the same deref-aware path.
   Blast radius measured: 1 fixture + zero real corpus sites; re-verify with the full battery.
3. **Self-host mirror (scoped):** the `DkNoFieldFound`→`E_NoFieldFound` reject in
   **infer.gg's EFieldAccess arm** (NOT typecheck.gg — RV-D's zone) for **RTPrimitive
   receivers only** (`s.data` on String — 0 false positives measured across 1547 fixtures).
   The struct-receiver reject is DEFERRED (unsound without a complete field registry — 45 FPs
   measured; prerequisite filed). Both-lane fixture: `reject_field_on_string.gg` green on
   Rust + self-host now.

## Milestones

M1 — apply the proven patch's STRUCTURE adapted to the Option field (the patch carries an
`is_deref_wrapper: bool` — convert to `deref_wrapper_kind: Option<DerefWrapperKind>` per
Objective 1); seed the variant map at BOTH registries (BUILTIN_GENERIC_TYPES: Box→DerefTarget,
Shared/Weak/Mutex→NonDerefContainer, Guard→GuardAccept; the builtin-module struct site:
RWLock→NonDerefContainer, ReadGuard/WriteGuard→GuardAccept, Box→DerefTarget); verify the
two-registry seeding empirically (`from std.collections
import Box` was the mid-scout miss — probe it). M2 — the Option-C extension: the deref-access
reject + the fixture flip + the "not yet implemented" message naming §9.4 and the filed backend
track. M3 — fixtures (ALL in tests/fixtures/, NEVER spectests/run/ — R6): NEGs: user-`Guard.y` + `fieldaccess_guard_missing_field_reject.gg` (builtin-guard ABSENT
field → E_NoFieldFound, pass-5) +
`Box[P].nonexistent` + `Box[P].x` (staged, E_DerefCoercionUnimplemented) + `Box[int].x` (primitive-inner →
E_NoFieldFound — the DerefTarget+primitive arm, pass-4) + `Shared[P].x` + `Weak[P].x` +
`Mutex[P].x` + `RWLock[C].port` (E_NoFieldFound per the table) + `reject_field_on_string`
both-lane. POS controls (REQUIRED — they pin the guards-accept invariant): the existing
`guard_struct_field.gg` + `guard_rwlock_field.gg` stay green (name them in the gate list),
plus a real-field-on-user-struct-named-Guard POS. ggdef: absent-field is NOT in ggdef's check-time repertoire (scout-verified —
`field_ty` returns Unknown; eval IllFormed only) — the new negatives stay out of the ggdef conformance lane by PLACEMENT (pass-2 R6 + pass-5
correction: tests/fixtures/ only; corpus_b/b1 + spec_conformance_ggdef FILTER by prefix/dir,
while converter_agreement + coverage_histogram read everything but TOLERATE reject fixtures
— frontend-error/no-pair = uncounted/bucketed; NO corpus EXCLUDE needed; M4's full ggdef
gate is the backstop); the
alignment rides the axis-extension track. New fixtures not gitignore-hidden; fmt-idempotent;
 M4 — gates
(FOREGROUND): `cargo test --lib` · wrapper/box/field/deref + field-access filters C AND LLVM ·
self-host driver rebuild + reject/accept lanes + `type_comparison` (≤85 baseline, print
counts) · `cargo test -p ggdef` (insurance) · `cargo test --test lints`. Bootstrap: the
self-host half adds an RTPrimitive-only reject measured at 0 FPs — bootstrap is the PARENT's
gate; you run build + targeted.

## Out of scope

The deref-coercion BACKEND (§9.4 implementation — its own filed track; it flips the staging);
the self-host struct-receiver reject (blocked on the field-registry prerequisite); user
`struct Box` link error + `Shared[P].x` codegen hole + tuple `.0` parsing (all filed);
RV-D's typecheck.gg zone.

## Process contract

Standard (worktree pwd verify; no stash; explicit staging; /tmp checkpoints per milestone;
Edit-tool-only; retry transient cargo errors). Commit when green
(`fix(semantic): RV-A — typed deref_wrapper_kind metadata (user structs stop escaping
E_NoFieldFound) + owner-ruled deref-access staging reject + self-host String-receiver mirror`),
trailers: Co-Authored-By Claude Opus + Claude-Session. Report NEW bugs (file-don't-fix).

## Acceptance

User-named wrapper structs reject absent fields (no more garbage-0); wrapper deref access
rejects with the §9.4-naming message; `s.data` rejects on BOTH lanes; the name-match is
retired at every consumer (grep proves it); zero over-rejection (the POS control + full
targeted filters); lib/lints/ggdef/type_comparison green.
