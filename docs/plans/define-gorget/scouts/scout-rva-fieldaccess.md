# RV-A Scout Report — field-access wrapper carve-out + self-host absent-field reject

Scout, worktree `agent-a980f6da761a2502c`. Prototype `/tmp/recover_rva_proto.patch`
(5 files, +78/-7, applies clean). Self-host driver binaries: `/tmp/rva/sh_tc_driver3`.
**Lands nothing.**

## VERDICT
Both halves confirmed and prototyped end-to-end. Recommendation:
- **Production (#1): typed `DefInfo.is_deref_wrapper` flag + deref-aware inner-field check.**
  Fixes bug #1 (user struct escaping) AND closes `Box[P].nonexistent` (the TODO's "absorb
  Strategy-2B wrapper entry"). Keeps `Box[P].x` accepted (honors the staged `#[ignore]`d intent
  + design-doc 9.4). Measured: cargo test --lib 1107/0; wrapper/box/field/deref integration
  94/0/1 (the 1 ignored is the pre-existing `fieldaccess_box_field_ok` run test). ZERO regressions.
- **Self-host (#5): RTPrimitive-only reject.** Closes the reported `s.data`-on-String hole,
  SOUND (0 false positives across all 1547 fixtures). The struct-absent reject is DEFERRED —
  the self-host field table is incomplete (45 false positives measured); it needs a complete
  per-struct field registry (filed).

## Premise table (all verified against current source)
| Premise (from brief/TODO) | Status | Evidence |
|---|---|---|
| `is_field_deref_wrapper` name-matches type names (typecheck.rs:189, :2803) | TRUE | read; one fn, one caller |
| User `struct Guard: int x` + `g.y` escapes E_NoFieldFound, prints garbage 0 | TRUE (measured) | `gg run` → `0`, exit 0; control `struct Foo` rejects |
| Deref-aware close needs inner-type resolution (`Box[P].nonexistent`) | TRUE | prototyped; rejects after fix |
| Self-host infer.gg EFieldAccess returns NO_TYPE without diagnosing | TRUE | infer.gg:251-263 |
| `s.data` on String REJECTED by Rust, ACCEPTED by self-host | TRUE (measured) | driver: no DIAG; Rust: E_NoFieldFound |
| lower_expr.gg:4732 emits int64-0 placeholder `[bug]` for unknown field | TRUE | read; but rejection belongs at typecheck, not lowerer |
| No NoFieldFound anywhere in self_host_typechecker/ | TRUE | grep: only DkUndefinedName etc.; added DkNoFieldFound |

## Production half (#1) — MEASURED probes
- `struct Guard: int x` + `g.y` → **prints `0`** (silent miscompile; the exact class the
  2026-07-13 FieldAccess fix closed for non-wrappers, reopened by the name-match).
- `struct Foo` + `f.y` (control) → correctly **rejects** `error[E_NoFieldFound]`.
- **`Box[Point].x` auto-deref → prints `0` (garbage), NOT 3.** Even the "legit" deref-field read
  is broken. Explicit `*b` then `.x` → correctly prints `3`/`4`.
- `Box[Point].nonexistent` → prints `0` (silent-accept-garbage; the staged Strategy-2B hole).
- `Box[String].len()` (doc 9.4 method auto-deref) → **fails to COMPILE** (C error). The whole
  "Deref Coercion" feature (§9.4) is broadly unimplemented, not just field access.
- User `struct Box` (legit fields) → **link error** (`undefined reference to Box`) — a SEPARATE
  name-match hole in lowering (BUILTIN_GENERIC_TYPES). User `struct Shared/Mutex/Guard/Weak` +
  legit `.x` → work (3/4); only the ABSENT-field access escapes.

## The seed mechanism (the crux for a TYPED flag, not name-match)
The 8 wrapper names come from TWO registries — both must be seeded:
1. `BUILTIN_GENERIC_TYPES` (resolve.rs:19), registered `DefKind::Import`, `Span::dummy()` —
   covers Box, Shared, Weak, Mutex, Guard.
2. `.gg` struct decls in builtin modules — Box (collections.gg), RWLock/ReadGuard/WriteGuard
   (sync.gg). **Box has BOTH registrations**; `from std.collections import Box` resolves to the
   struct def, so seeding only the Import def MISSED it (caught empirically — see prototype note).
The prototype seeds both: the Import loop (inline) + the struct-define site guarded by
`is_builtin_module(FileModule path)` so a USER `struct Guard` (real span, non-builtin scope) gets
flag=false and no longer escapes. This is the ONE allowed seed name-match (mirrors
`compute_drop_taint` seeding from `equip Drop`); every downstream read is the typed flag.
Layering rule 2 satisfied.

## Minimal vs deref-aware — RECOMMENDATION with cost
Three shapes measured:
- **Option A (minimal typed flag, wrappers stay permissive):** fixes bug #1 only. LEAVES
  `Box[P].nonexistent` accept→0 (Core #8 known defect) and does NOT satisfy the TODO's "absorb
  Strategy-2B wrapper entry / close `.nonexistent`". Not recommended alone.
- **Option B (typed flag + deref-aware inner check) — RECOMMENDED.** ~15 lines over A. Resolves
  the wrapper's inner type (Generic targs[0]), checks the field on the inner struct: rejects
  `Box[P].nonexistent` + `Box[int].x` (primitive inner), accepts `Box[P].x`, rejects user
  `Guard.y` (flag=false path). Closes the TODO scope. Keeps `Box[P].x` accepted — matching the
  DELIBERATE staged intent (`fieldaccess_box_field_ok` is a `check_gg_ok` + `#[ignore]`d `7`
  run test) and design-doc §9.4. Cost: DefInfo field + seed (both sites) + rewritten
  definitely-absent arm. Measured clean (see gates).
- **Option C (reject ALL wrapper auto-deref field access):** simplest (delete carve-out, no
  flag). Rejects `Box[P].x` too. Reference-grade-simplest by Core #8 (no accept→garbage), blast
  radius exactly 1 fixture (`fieldaccess_box_field_ok`). BUT reverses a documented staged design
  decision (§9.4 Deref Coercion is INTENDED; the `#[ignore]`d `7` says the team wants `b.x`→7).
  **DESIGN CALL for the owner** — only if auto-deref field access is abandoned in favor of the
  `*b`/`.get()` idiom. Not the scout's call to make unilaterally.

**Recommend Option B.** It satisfies the TODO scope, honors the design intent, retires the
name-match with a typed flag, and closes the two check-time holes. The remaining runtime gap
(`Box[P].x`→0; and method auto-deref `boxed.len()` not compiling) is the genuinely-separate
Strategy-2B-BACKEND work (IR/C/LLVM deref-field-read) — file, don't fold into RV-A.

## Self-host half (#5) — MEASURED
- Self-host driver on `s.data` (String) → **no DIAG (accepted)**; on `f.y` (struct) → **no DIAG**.
  Rust rejects both. Confirmed lane divergence.
- Home: **infer.gg EFieldAccess arm** (infer.gg:251) — the mirror of Rust's `infer_expr`
  FieldAccess. `sexpr.span` gives the field-access span; `ctx` is a `&`-ref with the diagnostics
  sink. **Avoids the RV-D typecheck.gg zone entirely** — principled and zone-clean.
- DiagKind: added `DkNoFieldFound` → `E_NoFieldFound` (diagnostic.gg enum + the short-name map
  :129 + the code map `diag_kind_code` :185; both are EXHAUSTIVE matches → both need the arm).
- **RTPrimitive arm is SOUND: 0 false positives across all 1547 fixtures** (only the 2 legit
  `fieldaccess_{int,string}_field_reject` fire). Single DIAG, no dupes (infer not double-visiting).
- **RTDefined (struct) arm is UNSOUND as written: 45 false positives** (datetime, derive_ordinal,
  httpserver_*, p2p_*, test_math3d `Mat4.m00`, newtype `.0`, import_type_alias …). Root cause:
  `variant_field_types.contains(key)` is NOT a reliable "absent" signal — it's populated lazily
  during the walk at typecheck.gg:3004 and only when the field type resolves, so it MISSES
  generic-struct / imported / equip-defined fields. The self-host DefInfo has NO field list and
  there is NO complete per-struct field registry. A sound struct reject needs one built first
  (populated for ALL structs before inference). **Deferred / filed.**
- Blast radius on the self-host SOURCE: the modified Rust gg (Option B) rebuilds the self-host
  driver clean — the self-host source has NO wrapper-`.field` auto-deref, so the deref-aware
  reject adds zero new rejections there.

## Fixture plan (invariant #9 — both-lane)
- `reject_field_on_string.gg` (`s.data`): Rust reject + self-host reject (RTPrimitive) →
  **both-lane green now.** The clean both-lane deliverable for this track.
- `fieldaccess_box_field_ok.gg`: UNCHANGED under Option B (still `check_gg_ok`; the `#[ignore]`d
  `7` run test stays ignored — the runtime read is the separate backend gap).
- NEW production negatives: `fieldaccess_wrapper_missing_field_reject.gg` (`Box[P].nonexistent`)
  + `fieldaccess_user_guard_missing_field_reject.gg` (`struct Guard: int x; g.y`) — Rust/C/LLVM
  lanes reject. NOT self-host both-lane (needs the struct registry); NOT ggdef (see below).
- User-`struct Guard` absent-field both-lane: **blocked on the self-host struct registry** —
  keep production-lane only until that follow-up lands.
- **ggdef:** elaborates field access but `field_ty` (elaborate/mod.rs:531) returns `Ty::Unknown`
  for an absent field — NO check-time reject; only an `IllFormed("no field")` halt at EVAL
  (eval.rs:892/935). So `E_NoFieldFound` is NOT in ggdef's check-time repertoire; a four-lane
  reject fixture would need ggdef alignment (make `field_ty` reject → emit E_NoFieldFound).
  Optional follow-up, not required for the C/LLVM/self-host lanes.

## Zone map
- Production: `src/semantic/scope.rs` (DefInfo field), `src/semantic/resolve.rs` (seed, 2 sites),
  `src/semantic/typecheck.rs` (:189 delete `is_field_deref_wrapper`, :2803 read flag +
  deref-aware). No overlap with other RV tracks.
- Self-host: `diagnostic.gg` (⚠ shares the DiagKind enum with the RV-G/DkDoubleMove wave —
  append-only, low risk), `infer.gg` EFieldAccess arm. **Stays OUT of typecheck.gg** (RV-D's
  SAFETY-WALK zone) — no conflict.

## Executor plan + gates
1. Production Option B: add `DefInfo.is_deref_wrapper` (init false, 2 ctors); seed at
   resolve.rs BUILTIN_GENERIC_TYPES loop (Box/Shared/Weak/Mutex/Guard) + struct-define site
   guarded by `is_builtin_module` (Box/Shared/Weak/Mutex/Guard/RWLock/ReadGuard/WriteGuard);
   rewrite typecheck.rs definitely-absent arm to read the flag + resolve inner + check field;
   **DELETE `is_field_deref_wrapper`** (proto keeps it `#[allow(dead_code)]`).
   Gates: `cargo test --lib`; integration `fieldaccess box_ shared mutex rwlock guard deref weak`
   (expect 94/0/1). Add the 2 production negative fixtures.
2. Self-host: add `DkNoFieldFound` (enum + 2 exhaustive-match arms + code map); infer.gg
   RTPrimitive reject ONLY. Gates: rebuild driver; `type_comparison`; `spec_conformance_selfhost`;
   corpus scan = 2 no-field-found DIAGs (the legit rejects). Add `reject_field_on_string.gg`.
3. Parent: full integration sweep + parity regen.

## NEW bugs (FILE, don't fix here)
1. **Box auto-deref field READ returns 0** (`Box[P].x`→0 not 7) — the Strategy-2B BACKEND gap
   (IR/C/LLVM deref-field-load). The `#[ignore]`d `fieldaccess_box_field_ok` run test tracks it.
2. **Box METHOD auto-deref fails to COMPILE** (`Box[String].len()`, doc §9.4's own example) — the
   Deref Coercion feature is unimplemented for methods too, not just fields.
3. **User `struct Box` → link error** — BUILTIN_GENERIC_TYPES name-match in lowering breaks a user
   struct named Box (constructor mangling collision). The broader name-match class (Core #4); the
   typed `is_deref_wrapper` flag only fixes the FieldAccess reader.
4. **`Shared[Point].x` → broken C link** (`undefined reference to Shared`) — under Option B this
   now rejects at typecheck (good), but the underlying Shared-as-bare-type codegen hole remains.
5. **Self-host has no complete per-struct field registry** — blocks a sound self-host struct-absent
   reject; `variant_field_types` is incomplete (45 false positives). Needs a StructFieldInfo mirror
   populated before inference.
6. **Self-host parses newtype/tuple `.0` as EFieldAccess "0"** (over-rejected in the naive struct
   version: `Meters.0`, `UserId.0`) — relates to TODO tuple-named-field facet (ii).
