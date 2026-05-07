# Structural Guards

How Gorget makes soundness invariants statically enforceable, and the discipline for adding new ones.

> **Status:** Foundational. Cite this in PRs that introduce or migrate structural guards.
> **Builds on:** `layering-discipline.md`, `unified-resource-model.md`.
> **Companion in `AGENTS.md`:** *No name matching*, *Layering discipline*, *Debugging heuristic — fix complexity as a wrong-layer signal*.

---

## The bar

Gorget's goal is to be a reference implementation of a most-correct, most-robust compiler. That goal is hollow without enforcement: a soundness invariant that's documented but unchecked is a regression waiting to happen.

The bar is therefore:

> **Every soundness invariant is a writer-side static guard. Every guard is fatal once migrated. Every bug we fix leaves a permanent counterexample and a validator that locks the class shut.**

Phase C of the unified resource model demonstrated this end-to-end: four shallow-copy-of-resource shapes (Assign-Copy, FieldLoad, IndexLoad, EnumFieldLoad, Call/CallExtern args) were each migrated from "let downstream lowering paper over it" to "any GIR module containing a violation halts the build." The lowering passes can no longer silently emit the unsound shape, *anywhere*, in any user program or stdlib module. Phase C is this document's template.

---

## The five principles

### 1. Every invariant has a writer-side validator

Not "we have a test that covers this case" — *"we fail the build if the invariant is ever violated, in any input, anywhere in the codebase or any user program."*

The validator runs over the IR (or LIR) module structure after lowering. It checks the same property a human reviewer would check by reading the lowering code — but mechanically, on every program, every time. Phase C's `validate_resource_moves` / `validate_resource_field_reads` / `validate_resource_call_args` are the template: a few hundred lines of structural walking, no codegen, no runtime cost.

If a property is *important enough that violating it is a bug*, it's important enough to write a validator for.

### 2. Every bug leaves a permanent counterexample

A user-reported bug ships two artefacts:

- **(a) A fixture** capturing the user-facing symptom (`tests/fixtures/<name>.gg` + `.expected`, wired into the integration suite).
- **(b) A validator** capturing the *structural* violation that produced the symptom — when applicable.

Without (a), nobody notices when the surface behaviour regresses. Without (b), the next regression has the same shape and produces the same symptom in a different fixture, with no early signal. The validator is what makes the class permanently closed; the fixture is what makes the class observable in user terms.

Not every bug has a structural framing — some are genuinely ad-hoc. But before declaring a bug "ad-hoc", apply the *fix-complexity heuristic* (`AGENTS.md` Layering discipline): if the fix you're sketching is intrinsically complex, you're patching a symptom, and the structural framing exists upstream.

### 3. No silent skips

When a lowering pass takes a conservative shortcut to dodge a known issue (the snag #24 root cause was `recursive_drop_structs` skipping Option/Result fields entirely to avoid a double-free with shallow-copy patterns), the shortcut leaves two things behind:

- A **self-documenting marker** in the data structure (`__clone_only:`, `__inline_option_drop:`, `__drop_then_clone:`) so a human reading the table sees "this is deliberately incomplete."
- A **validator** that treats the marker as a violation, gated behind an env var until the migration is complete.

The marker is the evidence; the validator is what makes the evidence load-bearing. A skip without a marker is invisible; a marker without a validator is decorative. Both together is what the discipline requires.

### 4. The migration framework is a primitive

Phase C established a standard pattern. Any new structural guard ships with it:

```
(1) Write the validator.
(2) Gate behind GG_VALIDATE_<NAME> env var → log per-class counts to a file.
    Build does not panic; sweeping reveals the migration size.
(3) File a TODO entry tracking the burn-down by class.
(4) Migrate lowering sites one class at a time. Each commit drops one
    class's count to zero and demonstrates the integration sweep stays
    green. Migrations are independent — different agents can pick up
    different classes in parallel.
(5) Promote the validator to fatal (panic on any violation, no env var
    needed) once the count is zero. The class is permanently closed.
(6) Move the TODO entry to DONE.md with the commit chain.
```

Every step is concrete, observable, parallelisable. New guards take days, not weeks, because the pattern is reusable.

### 5. Layering discipline becomes enforcement

The four rules of `layering-discipline.md` (lossless on invariants, typed metadata not name-matched, one source of truth per axis, resolve once / write through) are the *design principles*. Structural guards are how those principles are *enforced*.

When `layering-discipline.md` says "if a downstream pass reconstructs information from names, the boundary upstream was drawn wrong", a structural guard is what makes that statement load-bearing. Without the guard, "was drawn wrong" is editorial; with it, the build fails until the boundary is fixed.

The same applies to `AGENTS.md`'s *Debugging heuristic — fix complexity as a wrong-layer signal*: a structural guard at the upstream write site is precisely what makes a downstream complex fix unnecessary. The guard catches the bug at the layer where it was introduced.

---

## What's already in place

These structural guards exist today and are load-bearing:

| Guard | Class | Site | Status |
|-------|-------|------|--------|
| `validate_resource_moves` | Assign-Copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_field_reads` | FieldLoad shallow-copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_index_reads` | IndexLoad shallow-copy of resource | `src/ir/validate.rs` | Fatal |
| `validate_resource_enum_reads` | EnumFieldLoad shallow-copy of resource payload | `src/ir/validate.rs` | Fatal |
| `validate_resource_call_args` | Call / CallExtern resource arg | `src/ir/validate.rs` | Fatal |
| Cycle check (snag #21) | Unbounded recursive type at typecheck | `src/semantic/cycle_check.rs` | Fatal |
| Cross-module type collision (snag #20) | Two modules publish same TypeDef name | `src/semantic/scope.rs` | Fatal |
| GIR module validation | Generic LIR-shape soundness | `src/ir/lowering/mod.rs:1505` | Fatal |
| `register_signatures_recursive` ExternBlock arm (snag #12) | Extern functions get FunctionInfo / def.type_id | `src/semantic/typecheck.rs` | Fatal |
| `validate_move_follow_through` (Tier 1b) | Move-mode assign of drop-registered source without follow-through MoveZero | `src/ir/validate.rs` | Fatal |
| `validate_box_inner_type` (Tier 1d) | Regular `Box[T]` StructDef missing typed inner-type metadata | `src/lir/validate.rs` | Fatal |

Plus the migration framework itself — `GG_VALIDATE_*` env gate, per-class file logging, the gate-→-zero-→-promote pattern from Phase C — is reusable as-is.

---

## Backlog

Tiered by maturity. Tier 1 = invariants we know are violated today, with concrete burn-downs. Tier 2 = invariants we should have but haven't designed validators for. Tier 3 = discipline meta-invariants policing the design rules.

### Tier 1 — invariants with known violations today

#### 1a. Drop completeness *(addresses snag #24)*

**Invariant.** Every droppable field of every type T must be reachable through T's emitted drop function. If a field's type has `drop_strategy ≠ None`, the field appears in `module.recursive_drop_structs[T]` (or `recursive_drop_enums[T]`) with a non-skip drop entry.

**Validator sketch.** Walk every struct/enum LIR StructDef. For each field whose type has a non-None drop strategy, check the recursive-drop table contains a corresponding non-`__clone_only:` entry. The `__clone_only:` markers self-incriminate.

**Why it matters.** Snag #24's class. A struct field of type `Option[Box[Resource]]` silently leaks the box at scope exit because `lir/lower/mod.rs:412-423` records the field as `__clone_only:` (skip drop). The validator turns this leak from "discovered when a user runs ASAN" into "the build halts the moment the lowering pass produces an incomplete drop table".

**Burn-down (in order).**
1. Build validator + env gate. Initial sweep gives the migration size.
2. Inline-drop scheme for Option/Result struct fields (prototype documented in TODO snag #24).
3. CoW clone-on-consume for enum constructors with resource args (the deeper bug snag #24 exposed).
4. Match-scrutinee staging migration so the resulting Borrow shape doesn't trip Phase C's read-site validators.
5. Promote.

**Estimate.** 1-2 sessions for the validator, ~3-5 sessions for the migration.

#### 1b. Move follow-through *(SHIPPED 2026-05-07; commit `3e49a03a`)*

Validator at `src/ir/validate.rs:1642` (`validate_move_follow_through`); fatal panic block at `src/ir/lowering/mod.rs:1619`. The dominant violation class — f-string interp segment temps emitting `Move`-mode assigns of drop-registered sources without follow-through — was closed in commit `1d3ccd5b` at `src/ir/lowering/exprs/calls.rs:1517` and `:1533`. Sweep of record (`c_emit_comparison`, 1066 fixtures): 0 violations. See DONE.md for the full chain.

#### 1c. Type-metadata coherence at registration

**Invariant.** Whenever a TypeDef is registered with `copy_semantics: Trivial`, none of its fields/variant-payloads has `copy_semantics: Resource` or `drop_strategy ≠ None`. Said differently: the upgrade pass (`upgrade_types_from_fields`) is unnecessary because metadata is correct at the construction site.

**Validator sketch.** After GIR lowering completes, for each TypeDef walk fields/variant payloads. If any payload is droppable, the type's `drop_strategy` must be non-None and `copy_semantics` must be Resource.

**Why it matters.** The snag #24 root cause includes a timing class: `make_option_type_def` registers Options with `..Default::default()` (DropStrategy::None), and the global `upgrade_types_from_fields` pass that fixes this only runs once at module-start. Lazy registrations during expression lowering miss the upgrade. Promoting *coherence-at-construction* to a hard invariant eliminates the timing class entirely.

**Burn-down.**
1. Validator + gate.
2. Migrate `make_option_type_def` / `make_result_type_def` / `make_wrapper_type_def` to take the type registry and compute correct metadata from the inner type.
3. Delete the post-hoc `upgrade_types_from_fields` pass (or keep it as a defence-in-depth assertion that the construction sites are correct).
4. Promote.

**Estimate.** 1-2 sessions. Migration is mechanical once the validator pinpoints the construction sites.

#### 1d. Box-inner-type completeness *(SHIPPED 2026-05-07; commit `bfb6bb67`, defense-in-depth tests `095ff22f`)*

Validator at `src/lir/validate.rs:764` (`validate_box_inner_type`); fatal at `src/lir/lower/types.rs:615` (LIR module-exit, unconditional) AND under `assert_module_valid` per-pass via the `VALIDATORS` registry at `src/lir/validate.rs:74`. Both wirings active: per-pass under debug + `GG_VALIDATE_PASSES`, plus unconditional release-build fatal. 7 unit tests cover well-formed Boxes, trait-box skip, missing-inner detection, mismatched-suffix detection, non-Box ignore, unexpected-field-shape branch, and full-pipeline `assert_module_valid` integration. See DONE.md for the full chain.

### Tier 2 — invariants we should have, designed but not yet built

#### 2a. CoW consume-site discipline *(deeper class than #1a)*

**Invariant.** At every consuming position (push / put / insert / send / `v[i] = x` / enum-init / struct-init / Box.new / function arg with `Ownership::Move`), the IR mode of the source matches its typed `LocalOwnership` state per the rules in `AGENTS.md`'s *Ownership at Consuming Positions*:

| Source state | Required IR shape |
|--------------|-------------------|
| Owns AND dead at this call | Move (transfer ownership) |
| Borrow OR owned but live past this call | Clone (deep copy) |
| Static literal | Runtime `*_materialize` |

**Validator sketch.** For each `Inst::Call` / `Inst::CallExtern` / `Inst::EnumInit` / `Inst::StructInit` / push / put / insert / send / index-store, check the source operand's `LocalOwnership` against the IR mode. Mismatch is a violation.

**Why it matters.** This is the *deepest* layer of the snag #24 class. Today the IR emits `enum_init Node::VarDecl { copy _3 }` even when `_3 = decls` is owned and live past the call (read at scope exit). The "owned but live → clone" rule isn't enforced at the enum-constructor consume site. The skip in `recursive_drop_structs` was masking this; once drops fire correctly, the aliasing produces use-after-free.

**Burn-down.**
1. Validator + gate. Initial sweep likely produces the largest migration of any Tier 1/2 item.
2. Migrate consume sites one at a time (push first, then put, then enum-init, etc.) — the migration framework handles this.
3. Promote.

**Estimate.** 5-10 sessions. Worth the investment; this is the load-bearing invariant for the entire CoW system.

#### 2b. Match-scrutinee discipline

**Invariant.** Every match scrutinee staging assign of a resource-typed scrutinee uses `AssignMode::Borrow` or `AssignMode::Move`, never `AssignMode::Copy`. Phase C covers most of this; the Option/Result-of-resource case (which the snag #24 attempts surfaced) is the remaining gap.

**Validator sketch.** Walk `Inst::Assign` instructions whose dst is the result of `stage_match_scrutinee`. For resource scrutinee types, mode must not be Copy.

**Why it matters.** When snag #24's drop-completeness fix promotes Option/Result-of-resource to Resource, the existing match-scrutinee staging emits Copy mode (because today's classifier doesn't see Option as a resource) and trips Phase C's validator. Closing this gap unblocks the snag #24 migration and locks the rule that match never consumes its scrutinee.

**Burn-down.** Validator + a single migration commit + promote. ~1 session.

#### 2c. Drop-tracking pre-rebind correctness *(snag #23 class)*

**Invariant.** When a value flows into a heap-allocating consumer (`Box.new`, `gorget_string_clone_to_owned`, `gorget_array_clone`, etc.), the source's drop registration is retired *before* any subsequent drop emission targets the source — i.e., the source slot is move-zeroed AND `drops.mark_moved` is set, not just `drops.unregister`.

**Validator sketch.** Pattern-match the IR for `_x = call <heap_alloc_fn>(copy _y)` followed eventually by `drop _y` in the same control-flow region without an intervening MoveZero. Heap-alloc fns are recognised structurally (the LIR knows which functions are heap-allocating consumers).

**Why it matters.** Snag #23's Box.new segfault. The fix shipped (round 10's `4ebefe44`); the validator locks the class so a future heap-allocating consumer can't forget the move-zero step.

**Burn-down.** ~1 session.

#### 2d. Sidecar absence

**Invariant.** For each typed metadata field on a TypeDef / StructDef / FunctionInfo, no parallel `HashMap<TypeId, X>` or `HashMap<String, X>` sidecar exists in the codebase tracking the same fact.

**Validator sketch.** Static analysis at lint level (or a Rust test that grep-walks the source tree). Each typed field has a registered name; the validator catches new sidecars by scanning for `HashMap` declarations whose value type matches the typed field's value type AND whose key type matches the field's owner.

**Why it matters.** *Layering discipline rule 3*: one source of truth per axis. Sidecars accumulate quietly; the validator catches them at introduction time. This is the discipline meta-rule with the highest payoff because parallel sidecars are how multi-step inconsistencies enter the codebase.

**Burn-down.** Designing the static check is the hard part. Once written, eliminating the existing sidecars is a sequence of typed-metadata-migration commits.

### Tier 3 — discipline meta-invariants

#### 3a. No name-matching at consumer boundaries

**Rule.** `name.starts_with("Box__")` / `.starts_with("Vector__")` / `.starts_with("Option__")` / similar at non-registrar sites is a violation. The legitimate registrar sites (LIR registrars, the C-emit boundary contract layer) form an explicit allowlist.

**Validator sketch.** Lint pass over the compiler source tree (Rust). Match `&str::starts_with` calls whose argument is a string literal matching a known mangled-type prefix; flag anything outside the allowlist.

**Why it matters.** *Layering discipline rule 2*: typed metadata, not name-matched. The rule has been violated quietly multiple times across the project's history; each violation produced a snag (snag #13's helper-emission gap, snag #20's collision detector gap, snag #24's skip class). Promoting the rule to a lint catches the next instance at PR time.

**Burn-down.** One-off lint pass. Allowlist starts small; grows as legitimate registrar sites are discovered.

#### 3b. Phase D state coherence

**Rule.** `LocalOwnership` is the source of truth for ownership and borrow tracking. Any consumer reading `drops.is_registered`, `is_named_local`, `is_owned_local`, etc. as proxies for ownership is a discipline violation that should migrate to the typed accessor.

**Validator sketch.** Same shape as 3a: a lint pass identifying remaining proxy reads.

**Why it matters.** Phase D4 / D4.5 work has been migrating these proxies for months. The endpoint is "Phase D is the only ownership signal." A validator locks the rule once the migration is complete.

**Burn-down.** Already in progress under existing Phase D TODOs. The validator is the closing artefact.

---

## How to add a structural guard

Concrete checklist for shipping a new validator. Cite this section in the commit message that introduces the validator.

1. **Name the invariant in one sentence.** If you can't, the framing isn't crisp enough; refine before writing code.
2. **Identify the writer site.** Where in the lowering pipeline does the violating shape get produced? The validator runs *after* this site, on the resulting IR.
3. **Identify the reader site that depends on the invariant.** What downstream code assumes the property holds? If nothing assumes it, the property may not be load-bearing — reconsider.
4. **Implement the validator.** Pure structural walk of the IR / LIR module. No codegen, no runtime cost. Returns a Vec of typed warnings, each carrying enough context (function, block, instruction index, type names) for the user to locate the violation.
5. **Wire the env gate.** `GG_VALIDATE_<NAME>=<log-path>` writes per-class violation counts and details to `<log-path>`. Build does not panic at this stage. Default unset; opt-in.
6. **File a TODO entry** under "Phase X extension" or a similar umbrella. Track the burn-down by class, with a sweep-of-record showing initial counts. Reference the validator's source file and the env var name.
7. **Migrate.** One class at a time; each commit drops a class's count to zero and demonstrates the integration sweep stays green. Different agents can run different migrations in parallel — classes are independent.
8. **Promote to fatal.** Replace the env-gate write with an unconditional panic (matching Phase C's pattern at `src/ir/lowering/mod.rs:1505-1586`). The class is permanently closed.
9. **Move the TODO entry to `DONE.md`** with the commit chain, and add a row to the *What's already in place* table at the top of this document.

The expected timeline for a typical guard is days to ~2 weeks, depending on migration size. Validators with small initial sweeps (single-digit violations) often ship in one session including the migration.

---

## Relationship to other docs

- **`layering-discipline.md`** describes *the design principles* (what each layer can and can't do, where information must cross). This document describes *the enforcement mechanism* that makes those principles load-bearing.
- **`unified-resource-model.md`** describes *the resource model* (Owned / Borrowed / View / MaybeOwned / etc.) and Phase A/B/C/D migrations. Phase C is the worked example this document generalises.
- **`copy-on-write.md`** describes *the consume-site rules* (Tier 2a above). When 2a's validator ships, this doc and copy-on-write.md form a closed loop: copy-on-write states the rule, 2a enforces it.
- **`AGENTS.md`** carries the operational discipline (no name matching, fix-complexity-as-wrong-layer-signal, don't redesign around compiler gaps). This document is *why* those rules exist as enforcement, not just style.

---

## Why this is worth doing

Most compilers ship correctness as an aspiration: the implementers know the invariants, code review catches the egregious violations, fuzzers catch the rest, and users file the remainder as bugs.

The reference-implementation bar is different: invariants are *machine-checked at every build*. A bug becomes a permanent counterexample within a few hours of being reported, with a structural guard locking the class shut. New contributors don't need to learn the invariants — the build fails when they violate them.

Phase C demonstrated this is achievable at modest cost (a few hundred lines of validator per class, plus migration time amortised across multiple sessions and contributors). Generalising the discipline to every soundness invariant in the compiler is the program of work this document scopes.
