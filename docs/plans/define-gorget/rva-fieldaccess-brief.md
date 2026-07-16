# Executor brief: RV-A — field-access soundness (typed wrapper flag + deref-reject staging + self-host mirror)

> **Status:** v1 — pass-1 folded (4 reservations: R1 BLOCKING — the Option-C reject is NARROWED
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
2. **OWNER-RULED STAGING (Option C, ledger 2026-07-16 + the pass-1 SCOPE CLARIFICATION):**
   wrapper FIELD access REJECTS **only for the verified-broken subset**: Box (measured
   garbage-0) + Shared/Weak IF the executor's probes show them broken (probe first; accept if
   they work). Guard/RWLock/ReadGuard/WriteGuard field access WORKS (green fixtures
   `guard_struct_field`/`guard_rwlock_field`) and STAYS ACCEPTED — do not touch it. Wrapper
   METHOD auto-deref is OUT OF SCOPE (cc-fails loudly; the deref-backend track owns it). The
   reject needs a SECOND typed distinction — `is_deref_wrapper` (all 8 names, objective 1)
   cannot double as "deref unimplemented"; add a separate deref_implemented-style flag or an
   explicit broken-subset check at the reject site, typed, never name-matched at the consumer.
   Diagnostic: NEW SemanticErrorKind + `E_DerefCoercionUnimplemented` + a
   `spec/prose/diagnostic-codes.md` registry row; message: "field `<f>` exists on `<Inner>`
   but deref coercion (§9.4) is not yet implemented for `<Wrapper>`" (never reuse
   E_NoFieldFound — the field DOES exist on the inner type). **This M2 SUPERSEDES the proto
   patch's Option-B inner-check accept-branch** (typecheck.rs:2803-2827 hunk): only the
   typed-flag rule-2 retirement survives from that hunk; the deref-aware inner resolution is
   reused to produce the new diagnostic, not to accept. This REVERSES the prior `check_gg_ok` staging: flip `fieldaccess_box_field_ok` to a
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
track. M3 — fixtures: user-`Guard.y` NEG + `Box[P].nonexistent` NEG + `Box[P].x` NEG (staged) +
`reject_field_on_string` both-lane NEG + a POS control (real field on a user struct named
Guard). ggdef: absent-field is NOT in ggdef's check-time repertoire (scout-verified —
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
