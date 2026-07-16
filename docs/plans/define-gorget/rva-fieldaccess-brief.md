# Executor brief: RV-A — field-access soundness (typed wrapper flag + deref-reject staging + self-host mirror)

> **Status:** v2 — pass-2 folded (SIX: R1 the partition is GUARDS-vs-CONTAINERS — pass-2's
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

1. **Typed wrapper flag (rule 2):** retire the `is_field_deref_wrapper` NAME-match
   (`typecheck.rs:189`, `:~2803`) with a typed `is_deref_wrapper` flag seeded at BOTH
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
   `is_deref_wrapper` (which marks all 8) — a typed guards-vs-containers bit set at the same
   registration sites, never name-matched at the consumer.
   **THE 3-WAY DIAGNOSTIC TABLE (R3 — the reject site keys on
   (field-present-on-inner × wrapper-is-§9.4-deref-target)):**
   | case | code | example |
   |---|---|---|
   | present on inner AND wrapper is a §9.4 target (Box; Shared/Weak per the design doc — VERIFY which the doc promises) | `E_DerefCoercionUnimplemented` — "field `x` exists on `Point` but deref coercion (design-doc §9.4) is not yet implemented for `Box`" | `Box[P].x` |
   | absent on inner | `E_NoFieldFound` (the §9.4 message would lie) | `Box[P].nonexistent` |
   | primitive inner | `E_NoFieldFound` | `Box[int].x` |
   | wrapper NOT a §9.4 target (Mutex/RWLock — access via .lock()/.read(); auto-deref not promised) | `E_NoFieldFound` | `Mutex[P].x`, `RWLock[C].port` |
   **The M2 leg is PROTOTYPE-FIRST:** before wiring fixtures, the executor implements the
   table at the reject site (`typecheck.rs:2801-2831` definitely_absent arm), keeps the
   patch's inner-resolution block (patch lines ~103-125) as the field-present prober but
   flips its outcome from accept to the table's codes, RE-MEASURES the blast radius of the
   5-container reject across the full targeted battery (the "1 fixture" figure was
   Box-only), and reports the measured count before proceeding. Registry: new row +
   bump the "93 E_ codes" header count to 94 (`spec/prose/diagnostic-codes.md:12`). This REVERSES the prior `check_gg_ok` staging: flip `fieldaccess_box_field_ok` to a
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

M1 — apply the proven patch; verify the two-registry seeding empirically (`from std.collections
import Box` was the mid-scout miss — probe it). M2 — the Option-C extension: the deref-access
reject + the fixture flip + the "not yet implemented" message naming §9.4 and the filed backend
track. M3 — fixtures (ALL in tests/fixtures/, NEVER spectests/run/ — R6): NEGs: user-`Guard.y` +
`Box[P].nonexistent` + `Box[P].x` (staged, E_DerefCoercionUnimplemented) + `Shared[P].x` +
`Mutex[P].x` + `RWLock[C].port` (E_NoFieldFound per the table) + `reject_field_on_string`
both-lane. POS controls (REQUIRED — they pin the guards-accept invariant): the existing
`guard_struct_field.gg` + `guard_rwlock_field.gg` stay green (name them in the gate list),
plus a real-field-on-user-struct-named-Guard POS. ggdef: absent-field is NOT in ggdef's check-time repertoire (scout-verified —
`field_ty` returns Unknown; eval IllFormed only) — the new negatives are EXPLICITLY EXCLUDED from the ggdef conformance lane (the corpus_b/b1
EXCLUDE mechanism or non-spectest placement — verify which applies); the alignment rides the
axis-extension track. New fixtures not gitignore-hidden; fmt-idempotent;
if any lands in tests/fixtures top-level, check the corpus_b/b1 EXCLUDE question (reject
fixtures with CheckFails provenance are handled — verify, don't assume). M4 — gates
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
(`fix(semantic): RV-A — typed is_deref_wrapper flag (user structs stop escaping
E_NoFieldFound) + owner-ruled deref-access staging reject + self-host String-receiver mirror`),
trailers: Co-Authored-By Claude Opus + Claude-Session. Report NEW bugs (file-don't-fix).

## Acceptance

User-named wrapper structs reject absent fields (no more garbage-0); wrapper deref access
rejects with the §9.4-naming message; `s.data` rejects on BOTH lanes; the name-match is
retired at every consumer (grep proves it); zero over-rejection (the POS control + full
targeted filters); lib/lints/ggdef/type_comparison green.
