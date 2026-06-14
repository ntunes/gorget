# 24. Layering discipline

**Subsystem:** the discipline that governs how information crosses the
compiler's IR layer boundaries (`.gg → AST → GIR → LIR → BIR → backend`). It is
not code in one file — it is a *contract* that every lowering pass and every
backend obeys, enforced partly by the type system (BIR is a newtype over LIR, so
backends cannot receive unlowered ops; see [Ch. 16](16-bir.md)), partly by LIR
validators (`src/lir/validate.rs`), and partly by review. The canonical
four-rule summary lives in `AGENTS.md` (symlinked as `CLAUDE.md`); this chapter
is the *why*, with each rule's enforcement re-derived from current source.

The pipeline has five representations. Each one is allowed to *resolve* the
abstractions the previous layer expressed (the lossy-on-syntax direction) and is
required to *carry forward* the program's semantic invariants (the
lossless-on-invariants direction). Layering breaks when those two get conflated
— when a pass drops an invariant *as if* it were syntax, and a downstream
consumer reverse-engineers it from a name, a sentinel, or a shape.

## The two jobs of a layer

A layered compiler works when each boundary does two things cleanly:

1. **Resolve abstractions.** Generics monomorphise at AST → GIR; methods/traits
   dispatch to concrete callees at GIR → LIR (`src/lir/lower/calls.rs`); canonical
   high-level LIR ops expand to primitives at LIR → BIR (`src/bir/lower.rs`); BIR
   emits concrete C symbols or LLVM IR at the backend. Once an abstraction is
   resolved, the syntactic form that expressed it goes away. That is the *good*
   kind of dropping.

2. **Carry semantic invariants forward.** Ownership, borrow origin, drop
   strategy, copy semantics, view-vs-owned, ABI shape, read mode — these are
   *facts about the program*, not syntax. They do not stop being true because the
   representation got lower. They must accumulate, not erode.

The slogan: **invariants accumulate; abstractions evaporate.** If a downstream
layer needs an invariant the upstream layer dropped, the upstream layer was
wrong to drop it.

## The four rules

### Rule 1 — Lossless on invariants, lossy on syntax

A layer **may** add information (control flow, SSA value-numbering, block
parameters) and **may** resolve abstractions. It **may not** drop a semantic
invariant. The invariants Gorget tracks, and where they live as typed fields:

| Invariant | Typed home (current source) |
|---|---|
| Per-local ownership / borrow state | `Local.ownership: LocalOwnership`, `src/ir/mod.rs:670` (enum at `:450`) |
| Borrow origin (param / collection-elem / field / view / alias) | `enum BorrowOrigin`, `src/ir/mod.rs:582` |
| Move-vs-clone at a slot store | `Inst::SlotStore { …, is_move: bool }`, `src/lir/mod.rs:552` |
| View-vs-fresh result of a builtin | `BuiltinMethodDecl.returns_view` / `.returns_fresh`, `src/ir/lowering/builtins.rs:71,79` |
| Box inner type for drop/alloc codegen | `StructDef.box_inner_type: Option<String>`, `src/lir/mod.rs:1541` |
| Receiver convention (by-ptr vs by-value) | `BuiltinMethodDecl.self_conv: SelfConvention`, `src/ir/lowering/builtins.rs:65` |

`Local.ownership` is explicit about *not* defaulting: the `LocalOwnership::Untracked`
variant (it is the `#[default]`) means "no decision recorded yet" and readers must
**not** treat it as `Owned` — the doc-comment at `src/ir/mod.rs:441-458` spells out
exactly the silent-mis-drop bug that collapsing the distinction would reintroduce.
That is Rule 1 in miniature: a third state (`Untracked`) is itself an invariant, and
folding it into a default *loses* information.

### Rule 2 — Typed metadata, not name-matched

When a fact crosses a boundary it crosses as a **typed field on a struct** —
never as a name prefix, a sentinel value, or a runtime-symbol convention.

The "No name matching" rule in `CLAUDE.md` is this rule applied at the
runtime-symbol boundary, and it generalises to every boundary. *Any* time the
answer to "what does this mean?" is computed by string-matching an identifier,
the upstream metadata is missing one layer up.

The C-emit boundary is the **one principled exception**: the runtime symbol *is*
the contract with the runtime, so the C backend has to spell `gorget_str_trim`.
But even there, *which* symbol gets spelled is driven by a typed registry, not by
`if name == "..."` in the backend. The collection/method dispatch in
`src/lir/lower/insts.rs:470-480` reads `self.runtime_callees` (a map populated at
protocol-registration time) and a typed `self_by_ptr` flag — its own comment at
`:478` says *"no name-prefix tests here."* The same registry-driven spelling
recurs at `src/lir/lower/insts.rs:639` (*"driven by `runtime_callees.self_by_ptr`,
not name-prefix tests"*).

### Rule 3 — One source of truth per axis

For each kind of information, exactly one piece of metadata, at exactly one
location, read through one accessor. The smell is the same fact stored in two or
three places that must be kept in sync — they *will* eventually disagree.

History bears this out. Per-local ownership was once split across a
lowering-context enum, a post-lowering field, and several sidecar
`FxHashMap<LocalId, …>` maps; the D6 work lifted it onto a single
`Local.ownership` field, and the doc-comment at `src/ir/mod.rs:666-667` records
the consolidation: *"Source of truth at the GIR/LIR boundary (D6: lifted from
`func_state.local_ownership` directly onto Local)."* When you find two pieces of
state answering the same question, pick one and delete the other.

### Rule 4 — Resolve once, write through

When a pass *resolves* an abstraction — method dispatch picks a callee, generic
monomorphisation picks a `TypeId`, a Box registration knows its inner type — the
result writes into the next layer's typed metadata. The downstream layer reads
it; it does not redo the work and does not get to disagree.

The cleanest live example is `StructDef.box_inner_type`. The LIR lowering knows
the inner type at Box-struct registration and writes it onto the typed field
(`src/lir/mod.rs:1541`). The C backend *reads* it to emit the matching
`Box__<inner>__drop` wrapper — `src/backend/c_lir/emit_types.rs:1403-1409` walks
`module.structs`, takes `sd.box_inner_type`, and emits `void Box__{inner}__drop`
without ever re-deriving the inner type from the `Box__` name prefix (the comment
and read loop live at `:1398-1409`, the emit statement at `:1416`). Resolve once
(at registration), write through (the field), read once (at emit).

## The litmus test

> **If a downstream pass reconstructs information from names, sentinel values, or
> shape heuristics, the boundary upstream was drawn wrong.**

It is mechanically checkable. Walk every place a lower layer does any of:

- `if name.starts_with("Vector__")` / `match name.as_str() { … }`
- `if size == 0` *when `0` is being used as a "this is a view" sentinel* rather
  than as an actual size (the cap=0 view discriminator is legitimately a
  sentinel at the *runtime ABI* — see [Ch. 11](11-copy-on-write.md) — but a
  *compiler pass* deciding semantics off it would be a violation)
- `if local_type == int64_t && context_says_X` (encoding semantic state in a
  primitive type pun)

Each is evidence an upstream layer dropped a typed invariant. The fix is always
upstream — add the field, write it at the source, read it at the consumer.
**Never** patch the symptom by adding another name match. The pattern is so
load-bearing that LIR ships *validators* for it: `validate_box_inner_type` and
its inverse `validate_box_inner_type_consistency` (`src/lir/validate.rs:788,855`)
assert that the typed `box_inner_type` field and the `Box__`-prefixed name agree
in *both* directions, so a future regression that re-introduces name-derivation
fails the LIR validation pass rather than miscompiling silently.

## The debugging heuristic — fix complexity is a signal of the wrong layer

When you have localized a bug and the fix you are sketching is *intrinsically
complex* — save/restore around branches, phi insertion at merges, scope-tracking
name maps, manual SSA repair — **stop.** That complexity is almost always a tell
that you are patching a *symptom*. Real bugs in a well-layered compiler are
usually a one-line oversight at a **write** site, not a multi-case rule at the
**read** site.

The procedure:

1. Trace the data the buggy site is *reading*. Where was it last written?
2. Look at the writer. Did it respect all the typed metadata available, or did it
   default / hardcode / collapse cases the upstream had distinguished?
3. Writer was lossy → fix at the source; the downstream "complex fix" evaporates.
4. Writer was faithful → trace one more layer up and repeat.

Every layer hop *without* finding the bug should make you **more** suspicious of
your diagnosis, not less.

## Worked examples

### Snag #17 — the `self_conv` flag (a Rule-4 write-site bug masquerading as a Rule-1 read-site fix)

**Symptom.** Chained `text.substring(...)` corrupted a later `parse_float(text)`.
It *looked* like the CoW materialize-alias machinery was rebinding a variable
across a control-flow merge, and the candidate fixes ran 50+ lines: save/restore
the rebind across branches, repair the merge. That is exactly the "complex fix at
the read site" tell.

**Real bug.** The writer was lossy. When the GIR registered a builtin method's
signature, it derived the receiver's pointer kind from the method's
`self_conv` — and `resolve_builtin_method_return_type` was treating
*every* method as a mutable borrow. `substring`/`slice` are
`SelfConvention::Borrow` (immutable), but they were being registered as
`MutPtr`. The doc-comment that now sits at the fixed site,
`src/ir/lowering/context.rs:733-746`, narrates the whole failure: a method
registered as `MutPtr` makes `lower_method_call`'s `needs_mut` check fire
`cow_before_mutation`, which materializes (clones + rebinds the variable name)
the receiver — *"and the rebind then leaked across control-flow merges, causing
later reads of the same name to come from a local that was only initialized in
one branch."*

**Fix.** Read the typed `self_conv` and register the self-param's pointer kind
faithfully — `Borrow → Ptr`, `MutBorrow → MutPtr`, `ByValue → the type`,
`Static → no self param` (`src/ir/lowering/context.rs:751-760`). With the writer
faithful, the bogus materialization never triggers and the entire "rebind across
merge" read-site fix is never-taken code. A five-line change at the writer
dissolved a fifty-line change at the reader. The `self_conv` flag is the single
source of truth for receiver convention; it is read identically when populating
`runtime_callees.self_by_ptr` (`src/ir/lowering/context.rs:770`,
`:628`, `:647`).

### Snag #13 — Box inner-type metadata (a Rule-2 / Rule-4 fix)

**Symptom.** A `Box`-recursive enum linked against an undefined
`__gorget_box_alloc_<T>` symbol. The tempting fix was to scan the recursive-drop
tables for `Box__X__drop` entries and parse the inner type back out of the name —
i.e. name-matching, a Rule-2 violation.

**Real bug.** The `StructDef` for `Box[T]` had the inner-type information at
registration time but did not expose it to the C backend. The fix added the
typed field `StructDef.box_inner_type: Option<String>`
(`src/lir/mod.rs:1541`), set it at every Box-struct registration, and had the C
emitter read it (`src/backend/c_lir/emit_types.rs:1404`) instead of fishing the
inner type out of the `Box__` prefix. The field's doc-comment states the
contract directly: it is read *"to emit the matching `__gorget_box_alloc_<inner>`
/ `_free_<inner>` helpers and the `Box__<inner>__drop` wrapper without re-deriving
the inner from the `Box__` name prefix"* (`src/lir/mod.rs:1535-1540`). Trait-object
boxes deliberately leave it `None` and carry their own typed discriminator
(`is_trait_box`, `src/lir/mod.rs` adjacent), so the two Box shapes are
distinguished by *typed fields*, not by parsing names.

### Fix C — `ReadMode::Borrow` honoured for strings only (a Rule-1 invariant dropped at a layer boundary)

**Symptom.** `for x in vec:` over a recursive-drop user struct deep-cloned the
element every iteration via `{Type}__clone`, even when the body only read through
it. On the self-host self-compile this was a ~3.26-billion-clone clone-bomb —
the slow compile (~421s) was the *symptom*, not the disease.

**Real bug.** The GIR producer (`lower_for_array`, `src/ir/lowering/stmts/for_loops.rs`)
set the typed invariant correctly: `index_load_borrow` emits `read:
ReadMode::Borrow` — "this element is a view, don't clone it." The LIR
collection-element lowering (`src/lir/lower/insts.rs:1063-1100`) then honoured
that mode **only for strings**: the borrow branch was gated on `clone_fn_name ==
"gorget_string_clone_to_owned"` (`insts.rs:1066-1071`), so a recursive-drop
struct element fell through to the `{Type}__clone` arm
(`insts.rs:1083-1100`) regardless of the `Borrow` mode upstream had set. The
read-mode invariant — a Rule-1 fact (read mode, named in "The two jobs of a
layer") — was silently dropped at the GIR→LIR boundary because the consumer's
branch was narrower than the invariant it was meant to carry.

**Fix.** Bind the for-element as a `Ptr(elem)` borrow alias at the *producer*
(`for_loops.rs`, gated on typed `TypeDefKind` + `is_resource_type` +
`!is_collection_type`, no name matching) rather than re-deriving the borrow intent
in the LIR reader. Body reads auto-deref the `Ptr`; the owning boundaries clone
through it via `ensure_owned_at_boundary` / `ensure_owned_at_consuming_arg`, the
same apparatus that makes a borrowed `Vector[T]` param safe. The full mechanism,
the enum extension (`build_enum_recv_ptr`), and the soundness argument are in
[Chapter 11](11-copy-on-write.md) — "For-loop elements: borrow the element, don't
clone it." The cleaner follow-up generalizes the `insts.rs:1083` branch to honour
`ReadMode::Borrow` for any recursive-drop element, so the LIR reader stops being
the place the invariant is (under-)interpreted (TODO).

Both Snags #17/#13 reduce to the same lesson, and so does Fix C: the bug was a
missing or mis-read typed field — or, here, a typed mode honoured too narrowly —
one layer up, and the "obvious" fix at the consumer was complexity that the
correct write-site fix erased.

## How to apply this when extending the compiler

- **Adding an IR layer.** Before writing `lower_to_X`, list the invariants the
  new layer must carry from the previous one and declare a typed field for each.
  Write the lowering as a translation that *populates* those fields, never one
  that hopes the consumer can recover the invariant from shape. BIR is the model:
  it is a newtype wrapper whose construction *guarantees* the canonical-op
  expansion happened, so a backend cannot even be handed unlowered LIR
  (`src/bir/mod.rs`, [Ch. 16](16-bir.md)).

- **Adding an instruction.** Ask what semantic facts a downstream consumer needs
  beyond the operands. Each becomes a typed field. If you catch yourself writing
  `// downstream can tell this is an X by checking …`, the field is missing.

- **Adding a resource type, builtin, or runtime fn.** Adding it should touch
  exactly one declaration site — the `BuiltinTypeProtocol`/`BuiltinMethodDecl`
  table (`src/ir/lowering/builtins.rs`). If it requires updating multiple lists
  in multiple files "to stay in sync," the metadata is fragmented across
  consumers; fix the fragmentation first.

- **Refactoring a layer.** Every time you delete a sidecar map or a name-based
  lookup, replace it with a typed field. If you *can't* write the field because
  "the lowering doesn't know that yet," then the lowering needs to learn — that
  is the point, not an obstacle.

"What if I genuinely don't know the invariant at lowering time?" Then it is not
an invariant — it is a fact computed later, and the field is `Option<T>`
populated by the pass that computes it (exactly the shape of `box_inner_type`).
The discipline is not "everything must be known up front"; it is "once known, it
propagates as a typed field."

## In the self-host

Not applicable as a *standalone* subsystem: layering discipline is a contract
over the whole pipeline, not a pass with its own fixtures. The self-host
frontend (`tests/fixtures/self_host_*`) is held to the same discipline in spirit
(see `AGENTS.md` § "Self-host as the elegance showcase" — defensive
name-matching workarounds are technical debt to retire). The discipline has no
`*_comparison` test of its own; it is verified by the LIR validators
(`src/lir/validate.rs`) on the Rust path and by review on both.

The self-host reaches *all the way down* — it is not a frontend-only port. The
`self_host_lowerer` fixtures implement the LIR layer (`lir.gg`, GIR → LIR
lowering in `lir_lower.gg`, SSA construction in `lir_ssa.gg`, drop elaboration in
`drop_elab.gg`) and a full LIR → C backend (`lir_codegen.gg`, ~5.3k lines:
`emit_structs`/`emit_externs`/`emit_box_allocators_from_lir`). It is exercised by
`c_emit_comparison` and `self_host_bootstrap_fixed_point`. So the rules'
downstream half *is* covered in the self-host:

- **Rule 4 (resolve once, write through)** is exercised through the GIR → LIR
  lowering, which resolves the canonical-op abstractions and writes the resulting
  primitives forward. The self-host folds those canonical-op helpers directly
  into the lowerer (`lir_lower.gg:2208` *"Canonical-op helpers"*) rather than
  carving out a *separate* BIR newtype layer — there is no `bir_*.gg`. That is
  the one downstream layer the self-host genuinely lacks: the BIR newtype that
  makes Rule 1 unforgeable for backends (see [Ch. 16](16-bir.md)) has no
  self-host analogue, though the canonical-op *expansion* it guards does.

- **The C-emit symbol-spelling exception (Rule 2's principled carve-out)** is
  exercised directly: `lir_codegen.gg` spells `gorget_*` runtime symbols and
  `__gorget_box_alloc_<inner>` itself (the allocator-emit prefix at
  `lir_codegen.gg:741`, with the matching constructor spellings at `:3959`,
  `:3980`, `:4600`) — the same name-*is*-the-contract boundary the Rust C backend
  spells.

And the discipline cuts the other way too: the self-host LIR lowering is itself a
live instance of the very litmus-test pattern this chapter warns about. It still
routes collection/Box dispatch off name-prefix tests —
`lir_lower.gg:244` (`name.starts_with("Vector__")`) and `:291`
(`name.starts_with("Box__")`) — exactly the `if name.starts_with(…)` smell from
[the litmus test](#the-litmus-test). The `no_growth_in_self_host_name_prefix_routing`
ratchet (see [Ch. 27](27-comparison-bootstrap.md)) budgets and freezes these so
they cannot proliferate while the self-host is migrated toward typed dispatch.

## See also

- `CLAUDE.md` / `AGENTS.md` § "Layering discipline" and § "No name matching" —
  the canonical four-rule summary and the Rule-2-at-the-runtime-symbol-boundary
  specialization.
- [Ch. 16 — BIR](16-bir.md) — the layer whose newtype construction makes Rule 1
  *unforgeable* for backends.
- [Ch. 12 — GIR lowering](12-gir-lowering.md) and [Ch. 13 — ownership in the
  IR](13-ownership-in-ir.md) — where the GIR invariants (`LocalOwnership`,
  `BorrowOrigin`) are written.
- [Ch. 11 — copy-on-write](11-copy-on-write.md) — the cap=0 view discriminator,
  the one place a sentinel is legitimate (at the runtime ABI, not in a pass).
