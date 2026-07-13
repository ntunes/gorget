# Wave brief — FieldAccess soundness fix (reject bogus field on fieldless receivers)

> **Track #17** (the CallArg prerequisite, root-caused 2026-07-13). Fixes a
> TYPECHECKER soundness hole: `Expr::FieldAccess` on a FIELDLESS receiver silently
> ACCEPTS the bogus access (returns the wildcard `error_id`) → the backend faithfully
> emits UNCOMPILABLE / miscompiled C. Core #8: the language must REJECT this.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-cbackend-coalesce-bug.md` (the
> "backend coalesce bug" was a RED HERRING — root cause is the typechecker; fix
> PROTOTYPED + validated). **Fix patch:** `scouts/patches/fieldaccess-reject-fix.patch`
> (~74 lines, 1 file). **Minimal repro:** `scouts/patches/fieldaccess-fieldless-repro.gg`.
>
> **Status:** v1 — pass-1 (Opus, fresh) SIGN OFF, minor notes folded. Pass-1 applied
> the fix patch, BUILT + RAN the over-rejection gate (`cargo test --lib` 1107/0;
> field/struct/box/generic/enum/shared/deref slice 312/0; `self_host_runtime`/`_diff`
> 2/0) and **exhaustively verified carve-out COMPLETENESS** — every late-resolving
> field path enumerated + covered, with the clincher that Gorget has NO user-definable
> `Deref` trait (closed builtin deref set → the name-match carve-out CAN be + IS
> complete); `Rc`/`Arc`/`Cell` are not real types; `AtomicInt`/`Bool` are real structs
> with normal field checking (no false-reject); bare-type-param `T.field` resolves to
> `Var` → not rejected (tested). Folded the 3 minor notes: ENUMS named in the reject
> set + an `enum.field` negative fixture; a generic-fn bare-type-param positive guard;
> the typed-flag-vs-name-match layering acknowledgment (→ Strategy-2B follow-up).
> Awaiting passes 2-3 (fresh, confirming — the ≥3-brief convention; carve-out
> completeness independently re-derived).

---

## 0. Scope (decided)

**Ship the TARGETED fix:** reject `E_NoFieldFound` for a named field access on a
receiver that DEFINITELY has no such field — primitives (`int`/`String`/`bool`/…),
builtin generics (`Vector`/`Dict`/`Set`/`Channel`/`Future`/`Task`/`TaskGroup` —
placeholder defs, absent from `struct_fields`), **enums** (variants, no fields — use
`match`), and user structs missing the field — while CARVING OUT
smart-pointer / guard wrappers (`Box`/`Shared`/`Mutex`/`RWLock`/`Weak`/`ReadGuard`/
`WriteGuard`/`Guard`) whose field resolves LATE (through a deref), and unresolved
inference vars (`Var`) / already-`error_id` receivers. **Follow-ups FILED (not this
track):** (i) bogus-field-on-wrapper (`Box[T].nonexistent` still accepted — the
deref-aware Strategy-2B); (ii) named-field access on tuples/arrays/slices (left
permissive here to minimize blast radius).

## 1. Root cause (verified, cited)

`Expr::FieldAccess { object, field }` (`src/semantic/typecheck.rs:2649`) reports
`NoFieldFound` ONLY in the `ResolvedType::Defined` (concrete user-struct) branch
(`:2685`). The `Generic` branch (`:2723` — the auto-deref carve-out for builtin
generics) does NOT, and primitives hit neither — all fall through to
`self.types.error_id` (`:2767`). `error_id` "silently accepts any downstream
parameter type" (documented `mod.rs:397-410`), so `v.value` on a `Vector[Inner]`
typechecks with 0 errors → the backend emits `slot(GorgetArray) = int32_t 0` →
`error: incompatible types: GorgetArray from int32_t`. `struct_fields` holds only
user `Item::Struct` defs (`resolve.rs:556-591`), so builtin `Vector`/`Dict`/`Set`
(placeholder defs, `scope.rs:245`) are absent → their field accesses fall straight
through. **Minimal repro (NOT size-sensitive):** a 12-line program with
`count(v.value)` (`.value` on a `Vector`) — `gg check` → 0 errors (the bug); `gg run`
→ the cc error (`scouts/patches/fieldaccess-fieldless-repro.gg`).

**The existing analogue to mirror:** `Deref`-on-non-Box already reports `DerefNonBox`
with an `error_id`/`Var` suppression guard (`typecheck.rs:2948-2963`). The fix
applies the same shape to FieldAccess.

## 2. The fix (write-site; scout-prototyped — `fieldaccess-reject-fix.patch`)

In `typecheck.rs` FieldAccess: unify ALL "absent field" reporting at one site and
replace the `error_id` fallthrough (`:2767`) with a DEFINITELY-ABSENT check that
reports `NoFieldFound` for:
- primitives (no fields);
- builtin generics `Vector`/`Dict`/`Set`/`Channel`/`Future`/`Task`/`TaskGroup`
  (placeholder defs — no user fields);
- enums (variants, no fields — pass-1 verified `enum.value` correctly rejects;
  enums use `match`, no in-repo reliance);
- user structs missing the named field (the existing `:2685` case, now unified);
and CARVES OUT (returns the permissive `error_id`/late-resolve, as today):
- smart-pointer / guard wrappers whose field resolves through a deref
  (`Box`/`Shared`/`Mutex`/`RWLock`/`Weak`/`ReadGuard`/`WriteGuard`/`Guard`);
- unresolved inference vars (`ResolvedType::Var`) and already-`error_id` receivers
  (suppress cascade — mirror the DerefNonBox guard `:2948-2963`).

The carve-out is a NAME-MATCH (`is_field_deref_wrapper`) — a layering smell in the
abstract, but CONSISTENT with the pervasive existing precedent in the same file
(`unify` name-matches `Mutex`/`Shared`/`RWLock` at `:1061`/`:1078`; `Deref` matches
`Box` at `:2944`; the wrapper family is name-matched across `cycle_check.rs`/
`resolve.rs`/`safety/*`) — and it CAN be complete because Gorget has NO user-definable
`Deref` trait (deref-coercion is a CLOSED compiler-builtin set — pass-1 clincher). The
reference-grade typed-flag version (a `DefInfo.is_deref_wrapper` read instead of the
name-list) folds into the already-filed Strategy-2B follow-up; NOT this track.

## 3. THE KEY RISK the gauntlet must hammer — carve-out COMPLETENESS

The ONLY real risk is a FALSE-REJECT: a receiver whose field genuinely resolves LATE
(through a deref / auto-deref / trait / meta path) that the carve-out MISSES → the
fix wrongly rejects valid code. The reviewers MUST:
1. **Enumerate EVERY type/path whose FieldAccess resolves late or indirectly** — the
   smart-pointer/guard family (confirm the list is exhaustive: is there a `Rc`/`Arc`/
   `Cell`/`Ref`/newtype/`Owned` analogue?), auto-deref chains, trait-provided fields
   (if any), meta/comptime types, generic type params (`T.field` where `T:` a bound?),
   associated/projected types. Confirm the carve-out (or the `Var`/`error_id`
   suppression) covers each — cite where each is handled.
2. **Confirm the builtin-generic set is right** — is rejecting `Vector.field`/
   `Dict.field`/`Set.field` correct (they truly have no user-visible fields), and are
   there builtin generics WITH user-visible fields (e.g. does any builtin expose a
   field via a method-like accessor) that would be wrongly rejected?
3. **The `Var`/incomplete-inference guard is load-bearing** — a field access on a
   not-yet-resolved receiver must NOT be rejected (it may resolve to a struct). Verify
   the suppression matches DerefNonBox's.

## 4. Fixtures + gates

**Fixtures:** wire `scouts/patches/fieldaccess-fieldless-repro.gg` as a NEGATIVE
(`assert E_NoFieldFound` on `Vector[T].value`) + add negatives for `int.foo` /
`String.foo` (primitive), `struct.nonexistent` (single error), and **`enum.field`**
(the new enum-reject surface — pin it, pass-1 note) + POSITIVE guards that
`Box[T].field` / `shared T.field` / `struct.field` STILL typecheck, PLUS a
**generic-fn bare-type-param** guard `foo[T](T val): val.x` (the subtlest carve-out
path — a `T.field` resolves to `Var` → must NOT reject; pass-1 tested it passes +
runs). These are the false-reject tripwires — the carve-out regression net.

**Gates (this is a RUST production-compiler change — it changes what `gg` ACCEPTS, so
it is bootstrap-gated in the sense that the self-host SOURCE + lib + all fixtures must
still typecheck):**
1. `cargo build` + `cargo test --lib` (scout: 1107/0).
2. **Full `cargo test --test integration -- --test-threads=4`** (`GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=120`, quiet box) — the fix must not over-reject ANY in-repo
   fixture / self-host / lib program. This is THE over-rejection gate.
3. **`self_host_bootstrap_fixed_point` GREEN** + **`self_host_runtime` / `_diff`**
   (scout: 2/0 — the self-host source has no bogus field access the fix rejects).
4. **`GG_BACKEND=llvm` integration sweep** (the fix is backend-agnostic — typecheck —
   but confirm no LLVM-lane surprise).
5. `cargo test --test lints`.
**Out-of-repo note:** gorget-js / arena / gglox / gconf are compiled by Rust `gg` too;
if any has a bogus field access this now rejects, it surfaces in the deferred
coordination round — NOT a gate here, but note it in the landing.

## 5. Worktree + playbook preamble (non-negotiable)

Standard preamble (verify `pwd` + `git rev-parse --show-toplevel` inside the worktree;
NEVER touch `/workspace/gorget` or `/workspace/gorget-1`; no `/workspace/gorget/...`
absolute paths). `isolation: "worktree"`, `model: "opus"`; `git merge --ff-only
gorget-1` on entry; stage EXPLICITLY by file name (never `git add -a`/`.`/`commit -a`);
NEVER `git stash`; checkpoint the durable patch after the fix; run the full sweep + LLVM
FOREGROUND with generous timeouts (the box must be quiet — do NOT run review agents
concurrently, per the contention lesson). On an Edit-tool desync, re-Read + retry.

## 6. Definition of done

- [ ] `Expr::FieldAccess` rejects a named field on a definitely-fieldless receiver
      (primitive / builtin generic / missing struct field) with `E_NoFieldFound`;
      smart-pointer/guard wrappers + `Var`/`error_id` receivers carved out (still OK).
- [ ] Negative fixtures (`Vector[T].value`, `int.foo`, `String.foo`,
      `struct.nonexistent`) reject; POSITIVE guards (`Box[T].field`, `shared T.field`,
      `struct.field`) still typecheck. The carve-out regression net is in place.
- [ ] **Full integration sweep GREEN** (C) + **`GG_BACKEND=llvm` GREEN** + **bootstrap
      fixed-point GREEN** + `self_host_runtime`/`_diff` — NO over-rejection of any
      in-repo program.
- [ ] Carve-out completeness verified by the gauntlet (§3) — every late-resolving
      field path enumerated + covered.
- [ ] Follow-ups filed: bogus-field-on-wrapper (deref-aware Strategy-2B) + tuple/
      array/slice named-field.
- [ ] Landing note flags the out-of-repo coordination-round exposure.

## 7. Non-goals

- **No CallArg work** (that's the next track, unblocked BY this — its proto's 7
  `.value`-on-`Vector[CallArg]` sites become correct typecheck errors this fix
  produces, fixed to `callarg_values(...)`).
- **No wrapper-bogus-field reject** (`Box[T].nonexistent`) / **no tuple-array-slice
  field reject** — filed follow-ups.
- Any NEW gap the fix hits → fixture + sharp TODO, never a reshape to dodge it.
