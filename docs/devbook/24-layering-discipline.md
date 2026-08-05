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
| Why a compiler-inserted clone happened | `Instruction::Call.reason: Option<ImplicitCloneReason>`, `src/ir/instructions.rs` (GIR-only — see below) |

`Local.ownership` is explicit about *not* defaulting: the `LocalOwnership::Untracked`
variant (it is the `#[default]`) means "no decision recorded yet" and readers must
**not** treat it as `Owned` — the doc-comment at `src/ir/mod.rs:441-458` spells out
exactly the silent-mis-drop bug that collapsing the distinction would reintroduce.
That is Rule 1 in miniature: a third state (`Untracked`) is itself an invariant, and
folding it into a default *loses* information.

The `MaterializeReason` on a clone call is the same rule seen from the *accumulation*
side. When the lowering inserts an implicit clone at an ownership boundary — a
consuming push, a struct-field init from a borrow, a return of a borrowed value, a
CoW materialization — it already *knows* which boundary demanded it. Historically that
"why" lived only in a side-car diagnostic (`ImplicitCloneWarning`) keyed to no
instruction, so nothing downstream could recover it: given a clone `Call`, the reason
was gone. The fix is Rule 1's positive direction — carry the fact as a typed field on
the instruction that embodies it (`Instruction::Call.reason: Option<ImplicitCloneReason>`),
set once at the producer through the single `emit_clone` / `call_clone` chokepoint, and
guard it with an env-gated ratchet (`GG_VALIDATE_CLONE_REASONS`, always-on strict in
debug builds) that fails on any clone `Call` left untagged. The invariant now accumulates
*at GIR*: every compiler-emitted clone names its boundary. It deliberately does **not**
yet survive GIR→LIR — `Instruction::Call` is destructured with `..` when it lowers, and
`Inst::Call` carries no reason field — because no LIR consumer needs it today. Threading
it to LIR (so a future materialization planner can read a directive at the layer it costs
against) is the follow-up; when a consumer arrives, Rule 1 says the reason must reach it
as a typed field, never be reconstructed from the callee name.

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

The same rule governs wrapper disposition. `DerefWrapperKind`
(`src/semantic/scope.rs`) is the **SSoT** for BOTH field-access AND
method-dispatch wrapper dispositions — read once at the checker to decide
the D36 face split, written through to the IR lowering via
`MethodResolution.auto_deref` (the extended value type on
`method_resolutions`, D36 Q2). There is no parallel sidecar; the two
downstream reads (field access + method dispatch) consult the same typed
channel.

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

### A receiver convention honoured too loosely (Rule 4, and the heuristic)

A builtin method declares how it takes its receiver: `self_conv:
SelfConvention` records `Borrow` (immutable, by pointer), `MutBorrow` (mutable,
by pointer), `ByValue` (consumed), or `Static` (no self). That is the single
source of truth for the receiver convention, written once on the
`BuiltinMethodDecl` (`src/ir/lowering/builtins.rs:65`).

A return-type resolver that ignores `self_conv` and assumes *every* call mutates
its receiver is the canonical write-site lossiness. An immutable view-returning
method like `substring` — declared `Borrow` — is then modeled as if it
*consumed* its receiver, triggering a copy-on-write materialization of a value
that was never mutated; the corruption surfaces far downstream when the
materialization's rebind leaks across a control-flow merge. The tempting fix
lives at that read site — save and restore the rebind across the branch, repair
the merge — and its very complexity is the diagnosis: the value should never have
been materialized at all. Reading the typed `self_conv` faithfully at the
resolver (`Borrow → Ptr`, `MutBorrow → MutPtr`, `ByValue → the type`, `Static →
no self`) is a few lines at the *writer*, after which the materialization never
fires and the whole read-site fix is never-taken code. The same `self_conv` flag
drives `runtime_callees.self_by_ptr` — one source of truth, read everywhere the
receiver convention matters. (Full case study: the contributor playbook,
[Ch. 29](29-contributor-playbook.md).)

### Box inner-type metadata (Rules 2 & 4)

The C backend emitting code for a `Box[T]` needs the inner type `T` twice over:
to spell the `__gorget_box_alloc_<inner>` / `_free_<inner>` helpers and the
`Box__<inner>__drop` wrapper. The inner type is *known* at LIR Box-struct
registration, so it is a resolved fact — and the temptation, if the layer
boundary drops it, is to recover it at the backend by parsing it back out of the
`Box__<inner>`-prefixed name. That is the litmus-test smell exactly: a downstream
pass reconstructing meaning from a name, which means the metadata is missing one
layer up.

The disciplined shape is Rule 4 applied at the runtime-symbol boundary. Resolve
once: the inner type is captured at registration and written through onto a typed
field, `StructDef.box_inner_type: Option<String>` (`src/lir/mod.rs:1544`). Read
once: the C emitter reads that field (`src/backend/c_lir/emit_types.rs`) and
never touches the name. Trait-object boxes deliberately leave `box_inner_type`
`None` and carry their own typed discriminator, `is_trait_box`
(`src/lir/mod.rs:1549`), so the two Box shapes are told apart by *typed fields*,
never by parsing identifiers. (Full case study: the contributor playbook,
[Ch. 29](29-contributor-playbook.md).)

### A read mode honoured too narrowly (Rule 1)

The invariant: a `for`-loop element's *read mode* is a layer fact. When the body
only reads through the element, the GIR producer records that it is a view, and
no consumer downstream is licensed to deep-clone it. The cost of dropping that
fact hides one layer below the surface — a hot self-compile loop walking a large
collection that pays a `{Type}__clone` on every element looks like a slow
compile, while the disease is the dropped invariant.

The GIR producer (`lower_for_array`, `src/ir/lowering/stmts/for_loops.rs`) sets
the invariant faithfully: `index_load_borrow` emits `read: ReadMode::Borrow` —
"this element is a view, don't clone it." A consumer carries the invariant
correctly only if its branch is *as wide as* the invariant. The LIR
collection-element lowering (`src/lir/lower/insts.rs:1063-1100`) once honoured
the borrow mode **only for strings** — its borrow branch is gated on
`clone_fn_name == "gorget_string_clone_to_owned"` (`insts.rs:1066-1071`), so a
recursive-drop struct element falls through to the `{Type}__clone` arm
(`insts.rs:1083-1100`) regardless of the `Borrow` mode upstream set. A Rule-1
fact (read mode, named in "The two jobs of a layer") is silently dropped at the
GIR→LIR boundary whenever the consumer's branch is narrower than the invariant it
is meant to carry.

The disciplined repair is at the producer, not the narrow reader: bind the
for-element as a `Ptr(elem)` borrow alias in `for_loops.rs`, gated on typed
`TypeDefKind` + `is_resource_type` + `!is_collection_type` (no name matching).
Body reads auto-deref the `Ptr`; the owning boundaries clone through it via
`ensure_owned_at_boundary` / `ensure_owned_at_consuming_arg`, the same apparatus
that makes a borrowed `Vector[T]` param safe. The full mechanism, the enum
extension (`build_enum_recv_ptr`), and the soundness argument are in
[Chapter 11](11-copy-on-write.md) — "For-loop elements: borrow the element, don't
clone it." The cleaner generalization widens the `insts.rs:1083` branch to honour
`ReadMode::Borrow` for *any* recursive-drop element, so the LIR reader stops being
the place the invariant is (under-)interpreted at all.

### Trait-method symbols: agree on the name, don't reconstruct it (Rules 2 & 4)

**Symptom.** A trait-impl method call can miscompile at the very bottom of the
pipeline: the emitted C either fails to link (`undefined reference to
X__method`) or assigns a call's result into the wrong type
(`"Str/GorgetArray from int"` — the classic C *implicit-int* fallout of calling
a symbol the compiler never declared).

**Real bug.** Two sites disagreed on the *name* of one function. The definition
side emits an own-vtable trait method's **body** under the trait-prefixed
mangling `Trait_for_Type__method` (`lower.gg`'s `lower_equip_block`, called with
the `did_split` flag at `lower.gg:3376`). But the call side —
`EMethodCall` in `lower_expr.gg` — reconstructed the symbol *from the receiver
type alone*, building the bare `recv_type_name + "__" + mname`
(`lower_expr.gg:1422` binds `recv_type_name`). The trait name isn't visible at
the call site, so the two spellings could never agree: the body lived at
`Trait_for_Type__method`, the call referenced `Type__method`, and nothing
defined the latter. That is Rule 2 and Rule 4 in one: a semantic fact (which
function this call resolves to) was being *reconstructed from a name* at the
read site instead of written through from the resolver — and the litmus test
fires verbatim, "a downstream pass reconstructs information from names → the
boundary upstream was drawn wrong."

**The tell.** A name-match workaround had already grown downstream to paper over
the symptom: dead-code elimination in `lir_codegen.gg:1228-1235` special-cases
trait dispatch, matching a bare `target_name` against any `*_for_Type__method`
key (`cand.ends_with("_for_" + target_name)`) so the trait-impl function isn't
pruned as unreachable. Its own comment states the disagreement plainly —
*"Self-host emits the bare form at LIR but the function is registered under the
trait-prefixed form."* That a *reachability* pass had to suffix-match names to
keep a called function alive was the signal the call/definition boundary was
drawn wrong: DCE was compensating for a symbol the call site had spelled but no
definition owned.

**Fix.** Make the definition and the call site agree on **one** name via the
registry, not by parallel reconstruction. (1) **Write-through at registration**
(`lower.gg:2826`, fed by a `pre_trait_defs` pre-scan at `:2774`): the fn-sig
registration is gated on the *same* own-vtable predicate the body-emit uses, so
an own-vtable method registers **only** `Trait_for_Type__method` and **drops**
the spurious bare `Type__method` entry; inherited/derive/unregistered methods
keep the bare entry (their bodies genuinely use it), and non-overridden default
methods register the trait-prefixed sig. After this, `fn_sigs` is the single
source of truth and the bare name truly has no entry. (2) **Redirect at the call
site** (`lower_expr.gg:1444`): try the bare name first; only if `fn_sigs` has no
such key, suffix-search its keys for the `*_for_<recv>__<method>` form and
redirect both `full_name` and `sig_lookup_name` to it. The call site no longer
*invents* a symbol — it looks one up. Both halves mirror Rust gg exactly: the
call-site redirect is `src/ir/lowering/exprs/methods.rs:298-326` (bare-first,
then the `_for_<name>__<method>` suffix search), and the own-vtable registration
split is `src/ir/lowering/traits.rs:269-344` (`register_trait_equip_sigs`). The
call site no longer *invents* a symbol — it looks one up. The DCE suffix-match in
`lir_codegen.gg` survives as a defensive backstop, but it is no longer
load-bearing once the symbol the call site spells is the symbol the registry
holds.

### A backend re-deriving a canonical value (Rule 3, at the backend boundary)

The same lesson has a backend-specific shape: **a backend that re-derives a value
the canonical post-optimization table already holds is a layering smell — read
the table.** By the time a backend runs, the LIR layer has computed and stored the
facts the backend needs (sizes, pointee types, ABI classifications) into
canonical fields. A backend that reconstructs one of those — from a local scan,
a field-sum, a name — is redoing a resolved decision it is not licensed to
disagree with (Rule 3, one source of truth; Rule 4, resolve once / write
through). Two live instances, both in the LLVM backend, both where the
re-derivation *got it wrong*:

- **Move-out field null-zero sized by a fragile scan, not the pointee table.** A
  drop-elaboration move-out zeroes the moved-from struct/enum field by storing
  `Null` into a pointer. The LLVM `Store` handler sized that zero by scanning the
  module for an `Inst::FieldPtr` whose `dst` was the store pointer
  (`src/backend/llvm/mod.rs:3601`, `dest_field_ty`) — but a move-out store's `ptr`
  is a `Cast`/byte-`getelementptr` result, *not* a `FieldPtr` dst, so the scan
  returned `None` and the fallback emitted an 8-byte `store ptr null` (only the
  enum tag). The moved-from field's heap-`String` pointer at offset 8 survived
  the partial zero and was *also* copied into the destination → both copies
  dropped at scope exit → double-free. The C backend never had the bug because it
  sizes this from the canonical `func.pointee_types` table (the C oracle is at
  `src/backend/c_lir/mod.rs:2729-2737`). The fix reads the *same* table: when the
  value is `Null` and `func.pointee_types[ptr]` is a `Struct(sid)`, emit a
  full-struct `memset` (`src/backend/llvm/mod.rs:3640-3652`), making the two
  backends byte-size-identical at the site. It matches `Struct(sid)` *only* —
  genuine `Ptr`/`PtrTo` fields stay on the 8-byte path, because over-zeroing a
  pointer field would itself diverge from C.

- **Cover-struct size taken as a field-sum, not the runtime ABI size.** A *cover
  struct* declares a small field that stands in for a larger runtime layout
  (`struct File: int handle` is an 8-byte gorget-visible cover for the 16-byte
  runtime `GorgetFile`; `struct TlsSocket: int _handle` covers a 24-byte
  handle). Layout queries (`is_small_aggregate`, `sizeof_lir_type`) read
  `computed_c_size`, so if that field holds the 8-byte field-sum, every
  downstream ABI decision is wrong: a register-return where the runtime returns
  by `sret` (callee reads garbage → SIGSEGV), or a `File f = !x` move-out that
  memcpy's 8 bytes into a 16-byte slot (upper half uninitialized → corrupt
  handle). The fix is at the *one* canonical write — `compute_struct_sizes`
  (`src/lir/mod.rs:1852-1884`) sets `computed_c_size =
  field_sum.max(opaque_runtime_size(name))`. `opaque_runtime_size` returns `None`
  for ordinary user structs (no change) and `Some(==field_sum)` for already-agreeing
  singletons (no-op), so only the genuine cover-struct divergence is corrected —
  and because it's at the canonical write, it flips every downstream ABI decision
  *consistently on both backends* at once. (Chapter 19 documents the matching
  `%File`/`%GorgetFile` LLVM struct override; once `computed_c_size` is correct
  the `needs_sret` path follows it.)

The tell in both cases is identical to the index/trait examples above: the
backend is reconstructing a fact (a field size, a struct size) instead of reading
the typed table one layer up. The disciplined fix is never "make the backend's
scan smarter" — it is "read the canonical field," and if the canonical field is
itself wrong, fix it at its single write site so *both* backends inherit the
correction.

All of these examples reduce to the same lesson: the bug is a missing or
mis-read typed field — a typed mode honoured too narrowly, a resolved symbol
reconstructed instead of written through, or a backend re-deriving a canonical
size instead of reading the table — one layer up, and the "obvious" fix at the
consumer (a save/restore, a name-prefix parse, a DCE suffix-match, a smarter
scan) is complexity that the correct write-site fix erases.

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

*"This makes the structs bigger."* It does. Carrying full ownership state on a
`Local` costs on the order of a couple of machine words over a bare
`type_id + name_hint` — compile-time only, with no runtime footprint. The
trade is decisively worth it because the two costs are of different kinds: the
memory cost is **bounded**, linear in IR size, while the cost of fragmentation
is **unbounded** — every new resource type or builtin is another chance to
silently miss a sidecar that nobody remembered to update.

*"This makes lowering more verbose."* Slightly, since each lowering site must
populate every invariant. But the verbosity lands at the source-of-truth site,
which is exactly where the context needed to populate it correctly already
exists. Paying it once at the writer is far cheaper than reverse-engineering
the same fact at every consumer — which is the read-side complexity the
debugging heuristic above teaches you to distrust.

*"What about layer-bridging concerns like spans?"* Spans are an invariant too —
every IR node has a source location, modulo compiler-generated nodes — and they
already propagate this way, through `BasicBlock.span_map` (`src/lir/ssa.rs:189`).
Same pattern, no exception needed.

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
