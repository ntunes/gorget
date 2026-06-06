# Layering Discipline

How the Gorget compiler's IR layers (AST → GIR → LIR → backend) relate, and what's allowed to cross each boundary.

> **Status:** Foundational. Cite this in PRs that touch IR layer boundaries.
> **Builds on:** `unified-resource-model.md`, `ownership-ir.md`, `lir-design.md`.
> **Related rule (specific case of Rule 2):** "No name matching" in `AGENTS.md`.

---

## The principle

Layered architecture works when each layer does two jobs cleanly:

1. **Resolves abstractions** that the previous layer expressed in higher-level form. Generics get monomorphised at the AST → GIR boundary; methods get dispatched at the GIR → LIR boundary; LIR gets emitted as C symbols at the LIR → backend boundary. Once an abstraction is resolved, the syntactic detail goes away — that's the *good* kind of dropping.
2. **Carries semantic invariants forward** so the next layer can act on them without reverse-engineering. Ownership, drop strategy, view-vs-owned, ABI shape, copy semantics, borrow provenance: these are *facts about the program* that don't change just because the representation got lower. They must accumulate across layers, not erode.

Layering breaks down when (1) and (2) get conflated — when a lowering pass drops a semantic invariant *as if* it were a syntactic detail, and downstream code ends up reverse-engineering the answer from names, sentinel values, or shape heuristics.

This document names the four rules that keep (1) and (2) separate, and the litmus test for when a boundary has been drawn wrong.

---

## The four rules

### Rule 1 — Lossless on invariants, lossy on syntax

Each layer **may** add information (control flow, SSA, value-numbering, block parameters) and **may** resolve abstractions (generics → monomorphised, methods → direct calls, traits → vtable lookups, AST shape → instruction sequences). Each layer **may not** drop semantic invariants:

- Ownership state of every local (Owned / Borrowed / View / MaybeOwned)
- Borrow origin (Param / CollectionElement / Field / RuntimeView)
- Drop strategy (None / Trivial / Recursive / Custom)
- Copy semantics (Trivial / Resource / RefCounted)
- View-vs-owned discriminator (Phase B of the unified resource model)
- ABI kind (ByValue / ByPtr / ByMutPtr)
- Read mode at every read site (Copy / Move / Clone / Borrow)

Invariants accumulate; abstractions evaporate. If a downstream layer needs to know an invariant the upstream layer dropped, the upstream layer was wrong to drop it.

### Rule 2 — Typed metadata, not name-matched

When a fact crosses a boundary, it crosses as a typed field on a struct. Not as a name prefix, not as a sentinel value, not as a runtime-symbol convention.

The "No name matching" rule in `AGENTS.md` is this rule applied at the runtime-symbol boundary. It generalises to every boundary: between AST and GIR, between GIR and LIR, between LIR and backend. *Any* time the answer to "what does this mean?" is computed by string-matching, the upstream metadata is missing.

The C-emit boundary is the principled exception: the runtime symbol *is* the contract with the runtime, so the C backend has to spell `gorget_str_trim`. But even there, *which* symbol gets spelled must be driven by a typed registry — never by `if name == "..."` in the backend code.

### Rule 3 — One source of truth per axis

For each kind of information, exactly one piece of metadata, at exactly one location, read through one accessor.

Counter-example pattern (smell): the same fact represented in two or three places that have to stay in sync.

- `LocalOwnershipState` (lowering context, 7 variants) **and** `Local.ownership: OwnershipState` (post-lowering, 3 variants) **and** six sidecar `FxHashMap<LocalId, …>` maps in `FunctionState`. All three describe the same axis (per-local ownership). All three have drifted at one point or another. Phase D of the unified-resource-model proposal collapses them into one typed field on `Local`.
- 16 parallel name-based lookup tables for resource metadata (`clone_fn_for_ptr`, `infer_drop_strategy`, `elem_drop_fn_for_*`, …). All describe the same axis (per-type resource semantics). Phase A collapses them into one `ResourceMetadata` accessor.

When two pieces of state are answering the same question, they will eventually disagree. Pick one and delete the other.

### Rule 4 — Resolve once, write through

When a lowering pass *does* resolve an abstraction (method dispatch picks a target, generic monomorphisation picks a TypeId, trait picks a vtable slot), the result writes into the typed metadata of the next layer. The next layer doesn't redo the work and doesn't get to disagree.

Counter-example: the C backend re-deriving "is this collection ordered?" by checking `name.starts_with("Dict__")`. The GIR already knew (the type's `CollectionKind` field). It just didn't propagate. Backend gets it wrong silently when a new collection type is added.

Right shape: GIR computes `CollectionKind`, writes it onto the LIR slot or the relevant instruction's typed field, backend reads it through a typed accessor. The decision is made once, at the layer that has the source-of-truth context.

---

## The litmus test

> **If a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong.**

Mechanically checkable. Walk every place a lower layer asks:

- `if name.starts_with("...")`
- `match name.as_str() { ... }`
- `if size == 0` (when `size == 0` is being used as a sentinel for "view", not as an actual size)
- `if local_type == int64_t && context_says_X` (encoding semantic state in a primitive type pun)

Each one is evidence that an upstream layer dropped a typed invariant. The fix is upstream: add the field, write it at the source, read it at the consumer. Never patch the symptom by adding another name match.

---

## Where Gorget gets this right today

- `TypeMetadata.drop_strategy` on `TypeDef` — typed, single source of truth (partial; Phase A finishes it).
- `Local.ownership: OwnershipState` field — typed, persists post-lowering (partial; Phase D extends it).
- `Instruction::Borrow { dst, place }` — origin captured at emission time (Rule 4 emission half is right; Phase D adds the persistence half).
- `Inst::SlotStore { is_move }` — typed flag on the instruction, not derived from receiver naming. Good shape.
- `AssignMode { Copy, Move, Clone, Borrow }` on `Instruction::Assign` — typed mode tag, not inferred at the consumer.

## Where Gorget gets this wrong today

- The 16 parallel name-based lookup tables for resource metadata (catalogued in `unified-resource-model.md` §3.1). Phase A.
- The seven sidecar `FxHashMap<LocalId, …>` maps in `LoweringContext` for ownership state (catalogued in `unified-resource-model.md` §6.1). Phase D.
- C backend's `name.starts_with("Vector__") || name == "GorgetArray"` checks for collection dispatch. Fixed downstream once `CollectionKind` is read instead.
- `is_view_returning_string_runtime` style name lists (the example in `AGENTS.md`'s "No name matching" section). Fixed by `BuiltinMethodDecl.returns_view: bool` at the typed declaration.
- LIR `Slot { ty, name }` discards `Local.ownership` at the GIR → LIR boundary. Phase D restores it via `Slot.origin: Option<BorrowOrigin>`.

---

## How to apply this when adding a new layer, instruction, or type

**Adding a new IR layer.** Before writing the `lower_to_X` function, list the invariants the new layer must carry forward from the previous layer. For each one, declare a typed field on the new layer's structs. Write the lowering as a translation that *populates* those fields — never as a translation that hopes downstream can recover the invariant from shape.

**Adding a new instruction.** Ask: what semantic facts does a downstream consumer need to know about this instruction beyond its operands? Each one becomes a typed field on the instruction. If you find yourself writing `// downstream code can tell this is an X by checking ...` in the instruction's docstring, the field is missing.

**Adding a new resource type, builtin, or runtime fn.** Adding it should touch exactly one declaration site (the `ResourceMetadata` table, the `BuiltinMethodDecl` registry, the `extern fn` block with typed attributes). If adding it requires updating multiple lists in multiple files to "stay in sync", the metadata is fragmented across consumers — fix the fragmentation first, then add the type.

**Refactoring an existing layer.** Every time you delete a sidecar map or a name-based lookup, replace it with a typed field on the layer's primary struct. The replacement is the test that the boundary has been drawn correctly: if you can't write the typed field because "the lowering doesn't know that yet," the lowering needs to learn — that's the whole point.

---

## Costs and counter-arguments

**"This makes structs bigger."** Yes. A `Local` with full ownership state is ~16 bytes larger than a `Local` with just `type_id + name_hint`. Compile-time only — no runtime impact. The cost-benefit is decisively on the side of typed metadata: the cost is bounded (linear in IR size), the cost of fragmentation is unbounded (every new resource type or builtin risks silently breaking a sidecar that wasn't updated).

**"This makes lowering more verbose."** Slightly — every lowering site has to populate every invariant. But the verbosity is at the source-of-truth site, where the context to populate it correctly already exists. Verbosity at the source is far cheaper than reverse-engineering at every consumer.

**"What if I genuinely don't know the invariant at lowering time?"** Then it isn't an invariant — it's a fact that gets *computed later*. Fine: the field is `Option<T>`, populated by the pass that computes it. The discipline isn't "everything must be known up front" — it's "once known, it propagates as a typed field."

**"What about layer-bridging concerns like spans?"** Spans are an invariant (every IR node has a source location, modulo compiler-generated nodes). They propagate via `BasicBlock.span_map` already. Same pattern.

---

## See also

- `AGENTS.md` § "No name matching" — Rule 2 applied at the runtime-symbol boundary.
- `unified-resource-model.md` — Phase A (Rule 3 on the type axis), Phase D (Rule 3 on the local axis), Phase C (Rule 4 enforced via validator).
- `ownership-ir.md` — current state of the GIR ownership invariants.
- `lir-design.md` — LIR layer's invariants and what it adds vs resolves.
