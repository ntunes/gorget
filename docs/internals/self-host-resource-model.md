# Self-Host Layering Roadmap

> **Status:** Proposed (2026-05-10). Companion to `unified-resource-model.md` — same phase taxonomy applied to the self-host implementation in `tests/fixtures/self_host_*/`.
> **Builds on:** `layering-discipline.md` (principles, language-agnostic), `structural-guards.md` (methodology, language-agnostic), `unified-resource-model.md` (Rust impl, parallel work).
> **Cite this in PRs that:** touch self-host IR layer boundaries, add self-host validators, or migrate name-prefix routing to typed reads.

This document is the self-host counterpart to `unified-resource-model.md`. Same phases (A, D, C, Tier E), same migration framework, same writer-side-guard discipline — applied to the Gorget-implemented self-host compiler rather than the Rust-implemented one.

It also sets the rule that distinguishes self-host work from a typical bootstrap port: **the self-host implementation is a forcing function for fixing Gorget itself, not a place to work around Gorget's gaps.**

---

## 0. The premise

The self-host has three simultaneous roles. The third is load-bearing for this document:

1. **Stress test.** Self-host exercises every corner of the language across ~30k lines of `.gg`. When something's awkward, the language is awkward; when self-host crashes, the compiler has a bug.
2. **Regression net.** `*_comparison` tests + `self_host_bootstrap_fixed_point` lock the implementation against silent drift.
3. **Reference-grade demonstration of idiomatic Gorget.** Self-host is the canonical answer to *"how would you write this kind of code?"* New contributors read it; users learn the language from it; the compiler's claim to be a robust, elegant, expressive language stands or falls on what self-host looks like.

When (1) finds a gap — a pattern that *should* compile cleanly but doesn't, or compiles to wrong code, or compiles to code that needs a workaround — the response is always:

> **Fix the gap in Gorget first. Then write the self-host code the right way.**

NOT: file the gap, work around it in self-host. NOT: file the gap, ship the workaround indefinitely. A workaround is at most a bridge until the next commit closes it. The gap itself is the priority artefact; the self-host code waiting on the fix is the second.

This inverts the usual bootstrap order. In a typical compiler project, the implementation language is THE compiler; the bootstrap exists to validate it. Here, self-host being correct/robust/idiomatic is also a goal, so a gap that forces self-host to be ugly is a goal regression, not a TODO. Every workaround we discover is *evidence of a Gorget bug to fix*, not a constraint to design around.

**Choose the most-elegant, correct, robust, fast long-term shape, always.** If the elegant shape doesn't compile today, the path is: file the gap → fix Gorget → revisit. Never: pick the second-most-elegant shape because the elegant one trips a compiler bug. The whole point of self-host being a forcing function is that those bugs surface here first; conceding to them is conceding the showcase role.

The audit trail goes both ways. Every entry on the *Gorget-side gaps surfaced* log (§7) names:
- The self-host workaround it produced (file, line, shape).
- The Gorget-side fix that retired the workaround (commit).
- The self-host re-implementation that replaced the workaround with idiomatic code (commit).

When the third column is filled in, the entry moves to `DONE.md`. While only the first two are filled, it lives in `TODO.md` as an active obligation.

---

## 1. The recurring failure pattern (self-host edition)

The Rust roadmap (`unified-resource-model.md` §1) catalogues a class of bugs caused by lookup-table drift, single-source-of-truth violations, and type-system gaps. Self-host has its own analogue — a class of bugs surfaced *in* self-host code (workarounds we wrote, not bugs in user programs):

| Date | Symptom | Self-host workaround | Gorget gap |
|------|---------|---------------------|------------|
| 2026-05-09 | Stage-1 segfault on `module.items.get(i).unwrap()` at one specific call site | Wrapper `get_item_at(v, i)` (`tests/fixtures/self_host_typechecker/typecheck.gg:39`) preserved at one site | Self-host emission of inline get-then-unwrap on a Vector struct field, where the value subsequently passes into a function that mutably borrows the parent struct, produces buggy C |
| 2026-05-09 | `Option[int].unwrap_or(default)` skips None-check in self-host emission | Wrapper `sr_lookup` using `if .contains: .get.unwrap` (`tests/fixtures/self_host_lowerer/lir_lower.gg:35`) | Self-host's lowering of `Option.unwrap_or` doesn't emit the None-tag check; produces a NULL deref when key is absent |
| 2026-05-09 | Nested `Vector[Vector[T]].get(i).unwrap().push(x)` silently breaks downstream codegen | Rebuild-and-set workaround in `lir_ssa.gg:60-79` | Self-host's emit of get-then-mutate-chain on nested Vectors emits incorrect type info for downstream expressions |
| 2026-05-09 | `(method_call())` in boolean context parses as tuple-start | Drop the redundant parens at the call site | Parser's prefix handling for `(` in `if X and (...)` ambiguates with single-element-tuple syntax |

These are **Gorget bugs**, not self-host bugs. The workarounds in self-host are temporary. Each row's terminal state is the workaround's removal in a commit that follows the corresponding Gorget-side fix.

(Patterns 1-3 above are roughly mirror-images of the Rust patterns in `unified-resource-model.md` §1: lookup-table drift becomes "emit-path drift in self-host's lower.gg", single-source-of-truth becomes "self-host's IR carries fewer typed invariants than the Rust IR", type-system gaps become "self-host's CoW emit can't yet handle X without producing buggy C".)

---

## 2. What's already shipped (the baseline)

The 2026-04 through 2026-05 sessions retired roughly the same generation of debt that Phase A's preliminary work retired on the Rust side: parallel-vector storage that should have been `Dict`, dead/wrapper functions, stale workaround comments. Concrete commits:

- StructRegistry parallel-vec → `Dict[String, int]` (commit `8d944ddc`).
- Enum registry parallel-vec → `Dict[String, Vector[String]]` (`7b715d97`).
- Module statics triple-vec → `Dict[String, GirStaticInfo]` (`03e85ad8`).
- Scope-table parallel `is_import / is_trait / is_dummy` flags retired (`1e9989bb`).
- Vector-dedup workarounds → `Dict[String, bool]` in `lir_codegen.gg` (`33dd9f1a`).
- Three rounds of dead-code + 1-line-wrapper retirement (`020abdfc`, `a2a1700b`, plus the typed-accessor inline series).

Net **~600 lines of vestigial scaffolding gone**. This is the cleanup *before* the typed-metadata work can start — same shape as Phase A's preliminary `RuntimeFn` enum and `Inst::CollectionCtor` shipped before the full `ResourceMetadata` schema.

The starting state for this roadmap is therefore *clean enough that the typed-metadata work isn't fighting concurrent debt cleanup*. The phases below build on it.

---

## 3. Phase A — Unified resource metadata (self-host)

> **Rust counterpart:** `unified-resource-model.md` §3.

### 3.1 The starting state

`GirTypeInfo` (`tests/fixtures/self_host_lowerer/gir.gg:153`) carries today only `{ name, fields, variants, is_enum }`. There is one flat `Dict[String, bool] resource_types` field on `GirModule`, populated at type-registration. No `drop_strategy`, `copy_semantics`, `collection_kind`, `clone_fn`, or per-type runtime metadata.

The self-host accordingly has 74 `name.starts_with("X__")` sites across `lower.gg` (29) and `lir_lower.gg` (45), each independently re-deriving "is this a Vector? a Dict? a Box? a Mutex?" from the mangled name. Pure Rule 2 violations (per `layering-discipline.md`). Adding a new resource means touching every site.

### 3.2 The typed schema

The Rust schema (`pub struct ResourceMetadata { runtime_name, size, align, lir_type, drop_fn, clone_fn, ... }`) translates to a Gorget struct on `GirTypeInfo` with **real Gorget enums** for the categorical axes — never int-coded. Int-coding would be a Rule 2 violation in miniature (semantic state in a primitive pun) and the discipline says: *if the elegant shape doesn't compile, we fix Gorget first*. The shape:

```
enum CopySemantics:
    CsTrivial
    CsResource
    CsRefCounted

enum CollectionKind:
    CkNotCollection
    CkVector
    CkDict
    CkSet
    CkDeque
    CkHeap

enum BoxKind:
    BkNotBox
    BkRegularBox
    BkTraitBox

struct GirResourceMetadata:
    String runtime_name        # "GorgetString", "GorgetArray", ...
    int size_bytes             # C ABI byte size
    int lir_type               # LirType the GtNamed lowers to
    Option[String] drop_fn     # runtime symbol; None for non-droppable
    Option[String] clone_fn    # runtime symbol; None for trivial
    Option[String] materialize_fn  # CoW view→owned; None if no view distinction
    CopySemantics copy_semantics
    CollectionKind collection_kind
    BoxKind box_kind
    bool opaque_handle         # Mutex / Channel / Thread / ...
```

`GirTypeInfo` gains `Option[GirResourceMetadata] resource_meta`. Populated at type-registration time from the same source the Rust impl uses.

If self-host's emit of any of these patterns trips a codegen bug — `Option[String]` field reads, enum-discriminant matches at high call-frequency, struct-with-many-enum-fields construction — that bug gets a TODO entry and a Gorget-side fix before this Phase A migration proceeds. No conceding to int-coding "for now."

(Implementation note: `meta` is a Gorget keyword, so match-arm bindings use `rmeta` or similar — `case Some(rmeta): ...`. This is by-design language convention, not a codegen bug.)

### 3.3 Migration plan

1. Define `GirResourceMetadata` struct + populate at every type-registration site (already a small set: `register_type`, `lookup_or_register_named`, `register_ptr`, the prelude pre-registration in `lower.gg`'s startup).
2. Add a single accessor `Option[GirResourceMetadata] resource_meta_for(GirModule &gmod, String name)` reading via `type_infos`.
3. Migrate the 29 sites in `lower.gg` and 45 in `lir_lower.gg` — one site or one site-family per commit. Each commit drops a category's `starts_with` count to zero and demonstrates the integration sweep stays green.
4. Promote: extend the Rust-side `tests/lints.rs` ratchet to also scan `tests/fixtures/self_host_*/*.gg` with the same `MANGLED_PREFIXES` budget. Initial budget = 74; each migration commit lowers it.

### 3.4 Gorget gaps to expect (and fix, not work around)

Likely surfaces:
- `Option[GirResourceMetadata]` field on a struct read from a `Vector[GirTypeInfo]` element via `dict.get(name).unwrap()` may hit one of the `Option<…>` codegen edges we've already filed (e.g., the `module.items.get(i).unwrap()` regression class).
- Replacing 74 name-prefix dispatches with typed match-on-int may surface match-arm codegen issues we haven't seen at scale.
- The typed `GirResourceMetadata` struct propagating through `&gmod` borrow paths exercises CoW patterns the lir_ssa.gg `pending_phis` workaround already documents as fragile.

Each surface = a Gorget bug to fix in `src/`, not a workaround to ship in `tests/fixtures/`.

#### 3.4.1 The Dict.get → Option[V] gap (closed 2026-05-10)

**Status: SHIPPED.** All three changes below landed together; bootstrap_fixed_point passes; the typed accessor `resource_meta_for(&gmod, name)` is the live consumer. Section retained for historical context.

The first prediction came true. Phase A.4's first consumer migration —
`map_gir_type`'s `GtNamed` arm in `lir_lower.gg` — needs to call
`resource_meta_for(&gmod, name)` which is just `gmod.resource_metadata.get(name)`.
That single call exposed three layered gaps in self-host's collection
support; closing them is the dependency for **every** Phase A consumer
migration that reads from the populated Dict.

**Workaround in tree (until the gap closes):** the consumer at
`lir_lower.gg:412` calls `build_resource_metadata(name)` directly,
re-deriving the metadata on each read instead of going through the
populated Dict. The populate pass at `lower_gir_to_lir` runs but its
output isn't read. Future consumer migrations follow the same pattern.

**The three coordinated changes needed:**

1. **`lower.gg::infer_method_return_type` returns `Option__V` for
   collection getters.** Currently the `get/unwrap/pop/last/first/remove`
   arm at lines 864–914 explicitly returns the bare element type (with
   an in-line comment at 891-894 acknowledging this as a workaround for
   `dict.get(NULL, k)` segfaults that happen when the binding is typed
   as I64 fallback). The fix: split unwrap from the rest, and have the
   `get/pop/etc.` branches return `lookup_or_register_named(&gmod,
   "Option__" + elem_norm)` where `elem_norm` maps `"Str"` to
   `"GorgetString"` and leaves primitives in their C-typedef form
   (`int64_t`, `double`).

2. **`lower.gg::EMethodCall` unwrap handler recognises C-typedef inner
   names.** At line 1693, `prim_name_to_type(inner_name_str)` only
   matches surface forms (`"int"`, `"float"`); the monomorphised type
   names use C-typedefs (`Option__int64_t`, `Option__double`). Without
   this, unwrap on `Option__int64_t` falls through to
   `lookup_or_register_named(&gmod, "int64_t")` which registers
   `int64_t` as a *named* GIR type, breaking downstream codegen. Add a
   parallel mapping table keyed on `"int64_t"` → `I64_TYPE`,
   `"int32_t"` → `I32_TYPE`, …, `"double"` → `F64_TYPE`,
   `"Str"` → `lookup_or_register_named(&gmod, "GorgetString")`, before
   the `lookup_or_register_named` fallback.

3. **LIR lift port** to `tests/fixtures/self_host_lowerer/lir_lower.gg`
   (~250 lines mirroring `src/lir/lower/lifts.rs::emit_void_ptr_option_wrap`):
   - Helpers `is_collection_void_return(name) -> bool`,
     `is_consuming_collection_method(name) -> bool`,
     `resource_clone_fn(payload_ty, &m) -> String`.
   - Function
     `int emit_void_ptr_option_wrap(emit_name, dst_local, opt_sid, lir_args, &f, bb, &local_to_slot, &m)`
     that emits the 4-block diamond: entry (raw_ptr call, memset
     dst slot, NullPtr+Cmp, branch) → some_bb (reload raw_ptr, build
     payload via clone/deref/aliased-ptr, IEnumInit Some) → none_bb
     (IEnumInit None) → merge_bb (returned to caller).
   - Signature change of `lower_instruction` from `void` to `int`,
     returning the (possibly new) bb to continue emitting into.
   - Lift dispatch in the GICallExtern arm just before the regular
     ICallExtern emit, gated on `is_collection_void_return(call_name)`
     AND `slot.enum_kind == EK_OPTION`.
   - Caller-loop update at `lir_lower_function` to thread the returned
     bb back into `lir_bb`.

**Why all three are needed simultaneously.** The Tier 1 lift (#3) fires
conditionally on `slot.enum_kind == EK_OPTION` — without #1, the slot
type arrives as bare `V` and the lift's discriminator misses. Without
#2, even when #1 ships, `unwrap()` on `Option__int64_t` returns a value
typed as a synthesized `int64_t` named type, which the codegen treats
as opaque and corrupts downstream arithmetic. Verified in the 2026-05-10
probe: #1 + #2 alone (without #3) produced the correct `Option[V]` type
flow but stage-1's `gorget_str_to_cstr(path)` then mismatched its arg
expectation because the Vector.get's val_type flipped from Ptr-aliased-
as-Str to actual Str struct. #1 + #2 + #3 needed in the same commit.

**Probe outcome 2026-05-10 (reverted; bridge restored).** Stage-0 → stage-1
bootstrap PASSED with all three changes. Stage-1 → stage-2 cc FAILED
with `aggregate value used where an integer was expected` at synthetic
`memset(__v3798, (int)__v0, ...)` calls inside `infer___infer_expr_type`
and similar lift firings. The lift's `IIConst(zero_byte, LT_I32, 0)` dst
gets substituted to `__v0` (the function's first param, an aggregate
`__gg_SpannedExpr`) by the SSA pass's `apply_value_substitutions`. Root
cause not pinned in the probe window — `value_subst.put` only fires from
`process_block`'s `ISlotLoad` arm at `lir_ssa.gg:194`, but no SlotLoad
in the lift has dst = zero_byte's id, so the substitution path is
unclear.

**Hypotheses for the SSA value-subst issue (to investigate next):**
- (a) `lir_fn_next_value` not properly monotonic across nested `&f`
  borrows in stage-0's compilation of `emit_void_ptr_option_wrap`.
  Check by adding a debug print of `f.next_value` on entry/exit of
  `emit_void_ptr_option_wrap` and at each `lir_fn_next_value` call.
- (b) SSA's `read_variable` entry-block fallback at `lir_ssa.gg:222-256`
  allocates new value ids for zero-init constants of promotable slots
  with no def. If that allocation collides with the lift's IConst dst
  id (because the SSA pass runs *after* lir_lower has populated
  `next_value`, the new IDs should be higher — but check whether
  `compute_predecessors` ran on a stale block list).
- (c) The SSA pass treating my new blocks (some_bb / none_bb / merge_bb)
  as predecessors of subsequent code that wasn't anticipated by
  `compute_predecessors`. Worth verifying that `compute_predecessors`
  is called *after* all `lower_instruction` calls have completed.

**Re-attempt protocol when ready.** Add `eprintln`-style debug
instrumentation to `lir_ssa.gg::process_block` (line 194) and
`apply_value_substitutions` to log every `value_subst.put` and
`sub_val(zero_byte, ...)` substitution. Run stage-1 on the smallest
fixture that triggers the lift inside `infer_expr_type` to capture the
substitution chain. The diff for #1 + #2 + #3 is preserved in this
session's local history — see TODO.md "Self-host showcase blockers"
for the precise file:line citations.

**Self-host re-impl (after the fix lands).** Flip
`build_resource_metadata(name)` → `resource_meta_for(&gmod, name)` at
every Phase A consumer site (one-line per site) and delete the bridge
calls + comments. The populate pass becomes the sole source of truth.

---

## 4. Phase D — Local-state consolidation (self-host)

> **Rust counterpart:** `unified-resource-model.md` §6.

### 4.1 The starting state

`GirLocal` (`gir.gg:122`) carries `{ type_id, name_hint }`. The `Operand` enum carries `OpCopy / OpMove / OpConst*` — no `Clone` or `Borrow` mode tag. Self-host has *no* per-local ownership state at the IR level; the closest analogue is the Rust-side `Local.ownership: OwnershipState` field that Phase D promotes from sidecar maps to a typed struct field.

Self-host doesn't have the sidecar maps either — it just doesn't track local ownership at all. That's a gap, not a difference: the typed information Phase C-style validators need to *check* against doesn't exist for the validator to *read*.

### 4.2 The typed schema

Real Gorget enums, same discipline as §3.2:

```
enum LocalOwnership:
    LoOwned
    LoBorrowed
    LoView
    LoParam
    LoMaybeOwned

enum BorrowOrigin:
    BoNone
    BoParam
    BoCollectionElement
    BoField
    BoRuntimeView

struct GirLocal:
    int type_id
    Option[String] name_hint
    LocalOwnership ownership
    BorrowOrigin borrow_origin
```

`Operand` extends with `OpClone(int local)` and `OpBorrow(int local)` to match the Rust-side `AssignMode { Copy, Move, Clone, Borrow }`. Adding two variants to a widely-used enum is the kind of change most likely to surface a self-host emit issue (the existing `Operand` enum is matched-on at hundreds of sites). If it does, fix the bug; don't sneak the new modes in as a sidecar map.

### 4.3 Migration plan

1. Extend `GirLocal` with the new fields, defaulting `ownership = Owned`. Compile-clean baseline.
2. Walk `lower.gg`'s emit sites and populate `ownership` correctly at every `add_local`. Param locals → `Param`. Locals bound to `.get(i)` results → `Borrowed` with `borrow_origin = CollectionElement`. Etc.
3. Walk read sites and tag operands with the right mode. `OpCopy` only for trivial types; resource reads must be `OpMove`, `OpClone`, or `OpBorrow`.
4. Phase C's validators (§5) read these fields — the migration is what populates them.

### 4.4 Gorget gaps to expect

The 5-variant int-coded ownership (and similar for borrow_origin) is a workaround for self-host's match-on-enum-variant codegen quirks (we've documented several this session). The clean shape is a real enum:

```
enum LocalOwnership:
    LoOwned
    LoBorrowed
    LoView
    LoParam
    LoMaybeOwned
```

If self-host's enum-field-on-struct-read codegen has issues that force the int-coded fallback, *that's a Gorget bug to fix*. Not a self-host design choice. Same for the `Operand::OpBorrow` extension — adding an enum variant should Just Work; if it doesn't, fix the compiler.

---

## 5. Phase C — Strict move/clone validation (self-host)

> **Rust counterpart:** `unified-resource-model.md` §7 + `structural-guards.md` Tier 1c/2a.

### 5.1 Validator design (`validate.gg`)

A new file `tests/fixtures/self_host_lowerer/validate.gg` holds the structural walks. Each validator is a pure tree walk over the GIR or LIR module, returning a `Vector[String]` of violations with location context. No codegen, no runtime cost.

Initial validators (mirror of Rust's):
- `validate_resource_moves` — every `OpCopy` of a resource-typed local is a violation.
- `validate_resource_field_reads` — `IFieldLoad` from a resource struct in `OpCopy` mode is a violation.
- `validate_resource_call_args` — Call/CallExtern arg mode must match source ownership per the *Ownership at Consuming Positions* table in `AGENTS.md`.

### 5.2 Migration framework on self-host

Validators run inline during lowering, gated on `GG_VALIDATE_<NAME>=<log-path>` env vars — identical to the Rust side. While a class's count is non-zero the gate writes per-class violations to the log file; the build does not panic. Once the count reaches zero, the validator promotes to fatal (unconditional panic) — same step (5) of the Rust framework's `gate-→-zero-→-promote` pattern.

No separate `gg validate` subcommand. The end state is *always-on, fatal-on-violation* — a subcommand that runs the validators "explicitly" would imply they're optional or cosmetic. They're neither: once a class is closed, the build halts on the first violation in any program. The env gate is the bridge between *implementing the validator* and *promoting it to fatal*; once promoted, the gate is removed and the check is unconditional.

This matches `structural-guards.md`'s framework verbatim:
1. Write the validator.
2. Gate behind `GG_VALIDATE_<NAME>=<log-path>` → log per-class counts; build doesn't panic.
3. File a TODO with the burn-down by class.
4. Migrate one class at a time; each commit drops a class's count to zero.
5. **Promote to fatal** once the count is zero. The gate is removed; the class is permanently closed.
6. Move the TODO to `DONE.md`.

### 5.3 Per-class burn-down

Same shape as the Rust Phase C: each violation class (Assign-Copy, FieldLoad, IndexLoad, EnumFieldLoad, Call args) is a separate validator + separate burn-down. Migrations are independent and parallelisable.

### 5.4 Gorget gaps to expect

The validator file is itself a substantial `.gg` module that exercises `Vector[String]` accumulation, struct-field walks across `LirFunction`/`LirBlock`/`LirInst`, and Dict-backed name tables. If any of those patterns surface a codegen bug in self-host, the bug gets fixed in `src/` first.

---

## 6. Tier E — Residual self-host hygiene

> **Rust counterpart:** `unified-resource-model.md` §8.

Items independent of A/D/C, runnable in parallel:

- **6.1 Lints ratchet covers self-host.** Extend `tests/lints.rs` to scan `tests/fixtures/self_host_*/*.gg` with the same `MANGLED_PREFIXES` set. Initial budget includes the 74 sites Phase A migrates to zero; ratchet tightens as Phase A commits land.
- **6.2 Validator-runs-after-every-pass on self-host.** Self-host's lowering pipeline (`parser → resolver → typechecker → GIR lower → LIR lower → SSA → codegen`) gets a validator hook between each pass, gated on `GG_VALIDATE_PASSES=1`. Same pattern as Rust's `assert_module_valid`.
- **6.3 Drop-flag dataflow init.** Mirror of the Rust Tier E drop-flag work — relevant if self-host's emit ever needs explicit drop flags (currently it relies on Gorget's emit-side drop tracking).
- **6.4 Critical-edge splitting + post-SSA invariants.** Self-host's `lir_ssa.gg` has the load-bearing `pending_phis` workaround precisely because critical edges aren't split. Once Phase D ships, splitting may become tractable and the workaround may retire.

---

## 7. Gorget-side gaps surfaced (running log)

Every gap that surfaces during self-host migration work goes here. Format: surface symptom · self-host workaround (commit) · Gorget fix (commit) · self-host re-implementation (commit). Entries with all three columns filled move to `DONE.md`.

The session 2026-05-09 → 2026-05-10 entries (currently in `TODO.md`) seed this log:

| Symptom | Workaround | Gorget fix | Self-host re-impl |
|---------|-----------|------------|-------------------|
| `module.items.get(i).unwrap()` inline crashes stage-1 at one specific call site | `get_item_at` wrapper preserved at typecheck.gg:1404 + resolve.gg analogues (`3e2c1e82`, `4d0e9f53`) | (open) | (open) |
| `Option[int].unwrap_or(default)` skips None-check in self-host emit | `sr_lookup` helper (`8d944ddc`) | (open) | (open) |
| Nested `Vec[Vec[T]].get(i).unwrap().push(x)` silently breaks downstream codegen | Rebuild-and-set in lir_ssa.gg:60-79 | (open) | (open) |
| `(method_call())` in boolean context parses as tuple-start | Drop redundant parens at call site | (open) | (open) |
| Reading a struct field's Vector[T] from `&self` previously triggered spurious memset | (long fixed) | (long fixed) | comment scrub `1e9989bb` |
| `Dict[String, _]` state-loss claim across many parallel-Vector workarounds | (long fixed by Dict insertion-order fix 2026-05-08) | (long fixed) | the cleanup series 2026-05-09 |

The bottom two rows are the model: gap closes, self-host re-implements, the workaround disappears entirely. The top four are still open obligations.

---

## 8. Costs and counter-arguments

**"Self-host should be a passive port, not a forcing function."** That trades the third role (showcase) against the first two (stress test, regression net). Compilers that take this trade-off ship implementations whose bootstrap is functional but uninspiring; the language's claims of elegance and robustness become aspirational. Gorget's bar is higher: self-host is *also* the demonstration. Trading the third role away is trading the demonstration away.

**"Fixing every gap before re-implementing is slow."** The alternative is permanent workarounds. A workaround's true cost isn't the lines it adds — it's the false signal it gives ("self-host can't express this idiomatically because Gorget doesn't let it") and the inertia it accumulates ("we've always done it this way"). The slow fix is faster than the slow accumulation.

**"Some gaps are too expensive to fix immediately."** Then the gap entry stays open and the workaround stays in self-host *as a bridge*, not as a permanent design. The audit log (§7) is the discipline: workarounds without an open Gorget-fix obligation are violations of the discipline, not pragmatism.

**"This duplicates work between Rust and self-host."** Yes, somewhat. Phase A's typed schema gets defined twice (Rust struct + Gorget struct). Phase C's validators get written twice. The duplication is the cost of having the third role; the alternative is conceding self-host's status as a reference implementation.

---

## 9. See also

- `layering-discipline.md` — the four rules (lossless on invariants, typed metadata not name-matched, one source of truth per axis, resolve once / write through). Apply identically to self-host.
- `structural-guards.md` — the migration framework, the bar, the steps. Apply identically; Tier 3a's lint extension to self-host is item 6.1.
- `unified-resource-model.md` — the Rust implementation. Each phase here cross-refs the corresponding section there.
- `AGENTS.md` *Self-host as the elegance showcase* — the operational rule that this document formalises into a roadmap.
- `AGENTS.md` *Don't redesign around compiler gaps* — the rule that §0's "fix the gap, don't work around it" is the strong form of.
