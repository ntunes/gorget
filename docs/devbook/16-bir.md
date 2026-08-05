# 16. BIR: backend-agnostic synthesis & validation

**Subsystem:** BIR — *Backend IR* — the final layer before machine-code
emission (`.gg → AST → GIR → LIR → BIR → backend`). It lives in `src/bir/`
(~6,250 LOC across four files: `mod.rs`, `lower.rs`, `synth.rs`, `validate.rs`).
BIR is not a new data structure; it is a **newtype wrapper over `LirModule`**
(`BirModule(LirModule)`, `src/bir/mod.rs:72`) whose construction guarantees that
every *canonical* high-level LIR op has been expanded into primitives. Backends
take `&BirModule`, so the Rust type system makes it *impossible* for a backend to
receive unlowered LIR. The lowering pass (`lower.rs`) does the expansion, the
synthesis pass (`synth.rs`) emits the shared helper functions some expansions
delegate to, and the validator (`validate.rs`) asserts the invariant held.

> **Doc-comment warning.** The module-level docstrings in `mod.rs:20-26` and
> `validate.rs:23-27` still describe "Step 0 — trivial passthrough, accepts every
> instruction, empty allowlist." That is **stale**. The shipped validator has
> eleven canonical-op rejection arms (`validate.rs:54-104`) and `lower.rs` is
> ~4,058 lines of real expansions. This chapter describes the shipped reality and
> cites the live code, not those docstrings.

## Why a layer, not just a pass

The lift plan (the former `lir-backend-lift-plan.md`, now folded into this
chapter and Chapter 14) measured "how dumb the
backend is" by counting name-based dispatch sites (`name == "X"`,
`name.starts_with("Y")`). The two backends (`c_lir`, `llvm`) historically carried
thousands of lines of *semantic* decisions: HOF loop generation, enum
construction, sret wrapping, sort trampolines. Those decisions belong upstream,
once, in a backend-agnostic place — the four-line layering rule (Ch. 24) says
information crosses boundaries as resolved primitives, not reconstructed by the
consumer.

BIR enforces that contract **at the type level**. The mechanism is the newtype:

- `BirModule` is constructed *only* through `BirModule::from_lir`
  (`mod.rs:77`), which runs `lower::lower_lir_to_bir` then
  `validate::assert_primitives_only` and returns `Result<Self, BirError>`.
- The `Backend` trait's emit entry point takes `&BirModule`
  (`src/backend/mod.rs:382`: `fn generate(&self, module: &crate::bir::BirModule)`).
- A backend reads the underlying LIR through `BirModule::as_lir(&self)`
  (`mod.rs:85`) for its 1:1 translation. There is no way to hand a backend a
  `LirModule` directly through `generate`.

`BirModule` deliberately derives neither `Debug` nor `Clone` (`mod.rs:69-72`) —
`LirModule` doesn't either, and inspection should go through `as_lir()`.
`as_lir_mut()` (`mod.rs:92`) exists for the few post-construction passes that
mutate module-level metadata (see [Pipeline placement](#pipeline-placement));
`into_lir()` (`mod.rs:98`) unwraps and is documented "use sparingly — the whole
point of the newtype is to preserve the invariant."

### The shapes this layer deliberately is not

Four alternatives were weighed and set aside, recorded here so they are not
re-proposed on their surface appeal:

- **MLIR-style dialects.** Gorget is one language, not a compiler framework;
  dialect infrastructure buys extensibility the project has no use for.
- **Sea of Nodes.** V8 abandoned it — harder to debug, slower to compile, and
  it discards the block structure the backends and the validators rely on.
- **Stack-based bytecode.** The targets are LLVM and native code, not a VM.
- **Same-datatype phases policed by runtime validators (the SIL model).** This
  works for teams with the headcount to maintain deep invariant-checking
  infrastructure. Here, a type-level error is dramatically cheaper than a
  runtime validator error — which is exactly why BIR is a newtype whose
  *construction* proves the invariant, rather than a flag on `LirModule` that a
  pass is trusted to check.

The name follows the same logic. "BIR" describes a **role** — this is what
backends eat — rather than its content, so adding a stage beneath it later
(a machine-specific IR, say) would not shift its meaning. The alternatives all
collided with established names or described content that could drift: `CIR`
(overloaded with GHC's Core), `MIR` (rustc), `TIR` (Zig), `PIR` (Parrot), plus
`EIR` and `FIR`, whose expansions are ambiguous.

## The canonical ops

A *canonical op* is a high-level `Inst` variant that LIR is allowed to carry but
no backend is allowed to see. The single source of truth for "what is canonical"
is the validator's rejection list. Eleven variants are rejected today
(`validate.rs:54-104`):

| Op | What it abstracts |
|---|---|
| `SizeOf` | `sizeof(T)` as an `I64` constant |
| `EnumInit` | tag-store + payload-field-stores into an enum slot |
| `EnumCheck` | tag-load + compare against a variant tag |
| `EnumExtract` | payload-field-load out of an enum |
| `StructInit` | per-field stores into a struct slot |
| `CowClone` | copy-on-write clone of a String (today) |
| `TraitCall` | dynamic vtable dispatch through a `*_TraitObj` |
| `HofExpand` | a higher-order method (`each`/`map`/`fold`/`sort_by`/…) |
| `AddressOf` | take the address of an SSA value (spill + `SlotAddr`) |
| `BoxAlloc` | heap-allocate and initialize a `Box[T]` |
| `CollectionCtor` | construct a Vector/Dict/Set/HashMap/etc. |

Each gets a match arm returning `BirError::UnloweredCanonicalOp { fn_name,
block_id, opcode }` (`mod.rs:40`, `validate.rs:54`). Everything else falls through
the catch-all `_ => Ok(())` (`validate.rs:111`) — *primitive by default*. This is
the key maintenance property: adding a new **primitive** needs zero validator
changes; adding a new **canonical op** needs exactly one arm here plus one
expansion in `lower.rs`. A canonical op "graduates" out of existence by deleting
its `Inst` variant once it's no longer emitted — at which point its validator arm
and expansion both stop compiling, which is the intended forcing function.

`validate::assert_primitives_only` (`validate.rs:36`) is a flat triple loop over
`functions → blocks → insts`, returning the first offender. It is an **instance**
of the structural-guard framework — a cheap, always-on invariant assertion that
turns "a backend silently mishandled an unlowered op" into a loud, located error.
For the framework itself (the `assert_module_valid` family, when guards run, the
debugging philosophy) see Ch. 25; BIR's validator is one specialized member.

## The lowering pass

`lower::lower_lir_to_bir(module: LirModule) -> Result<LirModule, BirError>`
(`lower.rs:41`) is by-value-in / by-value-out: it owns the module, rewrites every
function in place, appends any synthesized helpers, and returns the rewritten
module for the validator to confirm.

### Per-function expansion

`expand_func` (`lower.rs:80`) walks one function. Its structure carries several
deliberate decisions worth citing:

- **Fast path.** `func_needs_expansion` (`lower.rs:3892`) scans for any of the
  eleven canonical ops *plus* `CallRuntime` — twelve `matches!` arms in all
  (`lower.rs:3895-3909`). `CallRuntime` is not one of the validator's eleven
  rejection arms, but the pass rewrites it (`CallRuntime → CallExtern`,
  `lower.rs:840-850`), so it has to trigger the rebuild too. If none of the
  twelve are present the whole rebuild is skipped (`lower.rs:87`). Walking
  without cloning is O(n); rebuilding allocates.
- **Index-based block iteration** (`while bb_idx < func.blocks.len()`,
  `lower.rs:105`) rather than an iterator, for two reasons stated in the code:
  some expansions call `func.add_slot(...)` (needs `&mut func.slots` while a block
  borrow would be outstanding), and HOF expansion *appends new blocks* (check /
  body / done) that must themselves be processed on later iterations — a `done`
  block can contain a nested `HofExpand` (`lower.rs:99-103`).
- **ValueId counter shadowing.** The pass shadows `func.next_value_raw()` into a
  local `next` (`lower.rs:94`) because rewriting block insts holds `&mut func`,
  conflicting with `func.next_value()`. It writes back via `set_next_value_raw`
  at the end (`lower.rs:900`). Fresh values come from the local `alloc_value`
  (`lower.rs:3917`).
- **Parallel span maps.** Each block carries a `span_map` parallel to its insts.
  For a 1-to-N expansion, all N emitted insts inherit the source inst's span: the
  arm pushes insts, then the tail of the loop pads `new_spans` by
  `new_insts.len() - pre_push_len` copies of `current_src_span`
  (`lower.rs:880-891`). This keeps trace attribution pointing back at the
  originating source line.

### The simple expansions (1-to-N inside a block)

Most canonical ops expand to a short inline sequence of primitives:

- **`SizeOf { dst, ty }`** → `IConst { dst, ty: I64, value: c_sizeof_lir_type(ty,
  structs) }` (`lower.rs:129`). The size comes from the shared LIR table
  `crate::lir::lower::types::c_sizeof_lir_type` — *one* source of truth for layout,
  shared with the backends rather than duplicated.
- **`EnumInit { target, struct_id, variant_tag, fields }`** → `FieldPtr`(field 0,
  the tag) + `IConst`(tag) + `Store`, then for each payload field a `FieldPtr` +
  `Store` (`lower.rs:133-169`). The tag is `I32` at field 0.
- **`EnumCheck`** → `FieldPtr`(tag) + `Load`(I32) + `IConst` + `Cmp Eq`
  (`lower.rs:170`).
- **`EnumExtract`** → `FieldPtr`(payload) + `Load` (`lower.rs:193`).
- **`StructInit { target, struct_id, fields }`** → per-field `FieldPtr` + `Store`
  (`lower.rs:203`).
- **`CowClone { dst, src, ty }`** → `CallExtern "gorget_string_copy_cow"(src)`
  with a `Ptr` arg ABI (`lower.rs:266`). Only String is wired today; the match on
  the struct name is the extension point for other CoW types.
- **`AddressOf { dst, value, ty }`** → `func.add_slot(ty)` then `SlotStore`
  (spill the SSA value) + `SlotAddr { dst, slot }` (take its address)
  (`lower.rs:287-301`). This is the expansion that motivates the
  `func.add_slot` borrow note above.
- **`BoxAlloc { dst, inner_ty, value }`** → `IConst`(sizeof) +
  `CallExtern "__gorget_alloc"(size)` (result is the heap ptr `dst`) + `Store
  { ptr: dst, value }` (`lower.rs:819-838`).
- **`CollectionCtor { kind, with_capacity, str_keyed, args, … }`** → a single
  `CallExtern` whose runtime name is chosen by the `(kind, with_capacity,
  str_keyed)` triple (`lower.rs:852-876`) — e.g. `(Vector, false, _) =>
  "gorget_array_new"`, `(Dict, _, true) => "gorget_dict_new_str"`. The original
  args (key/val sizes, capacity) pass through verbatim.

**Why every aggregate field-write is a plain `Store`.** `EnumInit`, `StructInit`,
`BoxAlloc`, and `AddressOf` all deliberately emit `Inst::Store` even for aggregate
payloads rather than choosing `Store`-vs-`Memcpy` here. The comment at
`lower.rs:150-158` (and again at `lower.rs:206-208`, `lower.rs:835-837`,
`lower.rs:291-293`) explains the reasoning: each backend's `Store` handler already
dispatches on `val_types` to pick scalar-store vs aggregate-memcpy. Routing
through `Store` keeps the single source of truth at the `val_types` layer instead
of re-deciding aggregate-ness in the lowering pass — a layering-discipline call
(Ch. 24): don't reconstruct what's already typed one layer down.

One more passthrough sits next to these: `CallRuntime { dst, callee, args,
arg_abis }` → `CallExtern` using `callee.c_name()` (`lower.rs:840-850`).
`CallRuntime` is a typed-callee form of `CallExtern`; it's rewritten to the
name-based form because the backends still pattern-match runtime calls on `name`
(a later lift step folds them into enum-aware dispatch).

### `TraitCall` — expansion via synthesis

`TraitCall` does *not* expand inline. Instead the arm (`lower.rs:220-265`) calls
`pool.get_or_emit_trait_helper(...)` to obtain (or reuse) a synthesized dispatch
helper, then rewrites the site into a plain `Call { func: helper, args: [object,
…args] }`. The motivation (`lower.rs:230-244`): the helper has a *typed
signature*, so the backend's normal `Call` coercion handles aggregate args (e.g.
`Str`-by-value) without the arg-ABI ambiguity an inline `CallPtr` would carry.
`param_tys` arrives carrying the method's concrete LIR types as resolved at emit
time from the VTable FnPtr — not the opaque `void*` the extern decl carries.

### `HofExpand` — the big one

`HofExpand` covers every higher-order collection method and is the bulk of
`lower.rs`. The single arm (`lower.rs:302`) dispatches on `hof_op` into two
families:

1. **Sort family** (`SortBy` / `SortedBy` / `SortByKey` / `SortedByKey`,
   `lower.rs:314-393`) — delegates to a synthesized `sort_impl` function (see
   [Synthesis](#module-level-synthesis)) and rewrites the site to a `Call`. The
   `Sorted*` (returning) variants additionally inline `clone → &clone → Call →
   load` so the in-place and returning forms share one synth body
   (`lower.rs:348-384`); the in-place forms are a direct `Call(impl, [coll,
   closure])` (`lower.rs:385-391`).

2. **Loop family** (`Each`, `Any`, `All`, `Fold`, `Reduce`, `Count`, `Find`,
   `FindIndex`, `Filter`, `Map`, `FlatMap`, and the `Dict*` / `Set*` variants,
   `lower.rs:394-414`) — these expand *in place* into a control-flow skeleton of
   new blocks. The arm captures the remainder of the current block (and its
   parallel spans) into `remaining` (`lower.rs:415-422`), saves the original
   terminator, installs `new_insts` into the current block, then calls a
   per-op `expand_*` helper (`expand_each` `lower.rs:1113`, `expand_fold`
   `lower.rs:1344`, `expand_map` `lower.rs:2153`, `expand_filter`
   `lower.rs:1998`, `expand_dict_each` `lower.rs:2764`, `expand_set_filter`
   `lower.rs:3751`, etc.). Each helper builds a `check_bb` / `body_bb` / `done_bb`
   triple; the shared scaffold is `HofLoopCtx` (`lower.rs:921`), which emits the
   length check and per-element `ElemPtr` + optional `Load` up to the
   `CallClosure`, leaving terminators to the variant. The captured `remaining`
   insts move into `done_bb` so control resumes correctly after the loop.

The closure is dispatched exclusively through `Inst::CallClosure` with the
caller-supplied `closure_arg_abis` and `closure_ret_ty` carried on the
`HofExpand`. The body never inspects closure env layout — this is the
opaque-closure invariant (below).

### Appending synthesized functions

After all functions are expanded, `lower_lir_to_bir` splices the synthesis pool's
output onto the module tail (`lower.rs:60-75`):

```rust
let synth_fns = pool.finish();
// ... extend module.functions ...
for i in start..module.functions.len() {
    crate::lir::types::compute_function_value_types_at(&mut module, i);
}
```

`value_types` is recomputed for *only* the newly-appended functions (`lower.rs:70-74`)
so the validator and backends see fully-populated metadata. A `debug_assert`
(`lower.rs:63-67`) guards that nothing added functions outside the pool.

## Module-level synthesis

`src/bir/synth.rs` (~1,981 LOC) emits new `LirFunction` definitions that several
canonical-op expansions delegate to, so backends never reinvent them per type.
Two families exist today: the **sort** helpers and the **trait-dispatch**
helpers. The design (folded from the former `bir-module-synthesis-plan.md`,
"Option C", landed 2026-04-24) replaced a per-backend TLS `qsort` trampoline with
one primitive-LIR mergesort emitted once per shape.

### The `SynthPool`

`SynthPool` (`synth.rs:102`) holds two dedup caches and the accumulating
`new_fns` vector, seeded with `base_func_count` so it can assign stable `FuncId`s
(`FuncId(base_func_count + new_fns.len())`, e.g. `synth.rs:225`). `finish()`
(`synth.rs:299`) returns the vector.

- **Sort dedup key** `SynthKey` (`synth.rs:44`): `(op, element_ty,
  closure_arg_abis, closure_ret_ty)` — all mangled to strings. The closure's
  `__call` *function name* is deliberately **not** in the key (`synth.rs:36-43`):
  two different closures with the same ABI shape share one body.
- **Trait dedup key** `TraitHelperKey` (`synth.rs:92`): `(trait_obj_struct,
  method_idx, sig_mangle)`, keyed on resolved IDs so dedup is invariant under the
  trait's name.

`get_or_emit_sort_impl` / `get_or_emit_sort_by_key_impl` (`synth.rs:128`,
`synth.rs:160`) funnel into `emit_or_reuse_sort` (`synth.rs:183`): cache lookup,
else build a `fn(arr: Ptr, cl: Ptr) -> Void` (`synth.rs:208-211`) and fill its body
with `emit_sort_impl_body` (`synth.rs:412`). `get_or_emit_trait_helper`
(`synth.rs:246`) builds `fn(self: Ptr, …user_params) -> ret_ty` and fills it with
`emit_trait_helper_body` (`synth.rs:1264`) — three `FieldPtr`+`Load` pairs: one
for the vtable (field 1, `synth.rs:1309-1320`), one for the method fn-ptr
(`method_idx`, `synth.rs:1322-1334`), and one for `self->data` (field 0,
`synth.rs:1336-1348`); the data pointer is prepended as the call's first arg
(`synth.rs:1352`), then a `CallPtr` (`FieldPtr`+`Load`×3 + `CallPtr`, matching
the doc comment at `synth.rs:235`).

### The sort body

`emit_sort_impl_body` (`synth.rs:412`) emits an **iterative bottom-up stable
mergesort** as ~18 basic blocks (`synth.rs:424-441`): entry / alloc /
pass-check / run-check / merge-loop / drain / copy-back / next-width / free /
done. Stability and O(n log n) worst-case are why mergesort was chosen over
heapsort (unstable) or quicksort (recursion); the rationale and rejected
alternatives are in the synthesis plan. The aux buffer is raw bytes via
`CallExtern "malloc"` (`synth.rs:508-512`) and elements move via `Memcpy`, so one
body serves scalar and aggregate element types uniformly. For `*ByKey` variants
the comparator extracts a key per element before comparing.

### The opaque-closure invariant

Synthesized bodies talk to the closure **only** through `Inst::CallClosure` —
they never read `FieldPtr` on the closure pointer, never depend on env layout
(`synth.rs:9-17`). This is exactly what makes the `closure_call`-name-free dedup
key sound: two captures of the same closure type with different env values share
one synth fn and pass distinct closure pointers at runtime. The invariant is
*enforced*, not just documented: the unit test
`sort_by_synthesis_end_to_end_and_opaque_closure_invariant` (`synth.rs:1691`)
walks the emitted synth body and asserts no `FieldPtr` uses the closure param as
base (`synth.rs:1825`).

### Namespace guardrail

All synth fns use the reserved prefix `__gg_synth_` (`SYNTH_PREFIX`,
`synth.rs:34`). Monomorphization never produces names in this namespace, and
`assert_no_synth_prefix` (`synth.rs:306`) panics at pass entry
(`lower.rs:45`) if any existing function already carries it — a defensive check
that something upstream isn't squatting the synthesis namespace.

## Pipeline placement

`BirModule::from_lir` is wired at three sites in `src/main.rs`: the C-source dump
path (`main.rs:658`), the main build path (`main.rs:705`), and the run path
(`main.rs:1467`). All three share the same surrounding order, and the order is
load-bearing:

```
LIR lower → SSA → wire-collection-bridges → promote-runtime-calls
          → compute pointee/value/origin types ("compute-types-pre-bir")
          → BirModule::from_lir              ← canonical ops expanded + validated
          → split_critical_edges
          → optimize_module                  ← runs POST-BIR
          → recompute types ("compute-types-post-bir")
          → backend.generate(&bir_module)
```

(`main.rs:654-668`, `:699-717`, `:1467-1477`.) Two design points:

- **Types are computed pre-BIR** (so `from_lir`'s synthesis can fall back through
  them) and **again post-BIR** (so the optimizer and backend see types for the
  primitives the expansions emitted and for the appended synth fns).
- **`lir::optimize::optimize_module` runs *after* BIR**, deliberately
  (`main.rs:662`, `:709-711`): synthesized mergesort/HOF bodies want
  DCE/fold/CSE/inlining as primitive LIR, and drop-elaboration / the optimizer
  see the *expanded* primitive shape rather than the opaque high-level op. The
  optimizer mutates through `bir_module.as_lir_mut()` — one of the sanctioned
  uses of mutable access on an already-validated module.

After `from_lir`, the LIR-level structural guard `assert_module_valid` is also run
(`main.rs:661`, tag `"bir-lowering"`) — that is the *general* LIR validator
(Ch. 25), distinct from BIR's primitives-only check which already ran inside
`from_lir`.

## In the self-host

**The self-host has no separate BIR stage, but backend codegen *is*
self-hosted.** The self-host frontend (`tests/fixtures/self_host_*`) covers the
lexer, parser, resolver, type checker, a GIR lowerer, *and* a LIR→C backend
(`tests/fixtures/self_host_lowerer/lir_codegen.gg`, ~5,265 lines; its header
calls itself "Phase 4 of the self-host LIR backend", `lir_codegen.gg:7`). What it
does **not** have is a dedicated lower/synth/validate layer between LIR and
emission: instead of expanding canonical ops in a separate pass and validating
the result, it expands them **inline during C emission**. The C-emit `match`
carries arms for the same canonical ops Rust's `lower.rs` handles —
`IBoxAlloc` (`lir_codegen.gg:3042`), `IAddressOf` (`:3058`), `IStructInit`
(`:3073`), `IEnumInit` (`:3088`) — each writing the equivalent primitive C
directly. So there is no `BirModule` newtype, no `SynthPool`, and no
primitives-only validator; the canonical-op expansion is fused into codegen
rather than separated into the BIR layer Rust uses. The synthesis-pool design
(the once-per-shape mergesort / trait-helper emission) is explicitly out of scope
for the self-host (per the former `bir-module-synthesis-plan.md` §8, now folded
into this chapter).

To read the current C-emission parity, run:

```
cargo test --test integration c_emit_comparison -- --nocapture
```

and read the printed matched-fn counts (`tests/integration.rs:13549`). That test
diffs the self-host's `lir_codegen.gg` emission against Rust's. It is
**diagnostic-always-pass** — a green run asserts nothing about parity; only the
printed counts do. C-emit is the largest remaining self-host parity gap. Because
the self-host fuses expansion into emission rather than routing through a BIR
layer, any canonical-op divergence surfaces in this codegen comparison, not in a
BIR-specific comparison test — there is none.

**Porting BIR to the self-host is an architectural-fidelity gap, not a
correctness one.** The fusion is a *deliberate* simplification: it produces
identical C, so it costs the self-host **zero runtime parity** — there is no
fixture the BIR layer would make the self-host compile *correctly* that the fused
path compiles wrong. What the self-host gives up is the *property* BIR exists for:
the newtype that makes "the backend never sees an unlowered canonical op"
unforgeable at the type level ([above](#why-a-layer-not-just-a-pass)), the
primitives-only validator as a structural guard ([Ch. 25](25-structural-guards.md)),
and the once-per-shape synthesis pool. Adding a `bir.gg` layer to the self-host —
a newtype over its LIR, an expansion pass mirroring `lower.rs`, and a
primitives-only validator — would make the self-host's pipeline shape match
Rust's, reinforcing the self-host's role as the language's elegance showcase
([Ch. 26](26-self-host-frontend.md); `CLAUDE.md` § "Self-host as the elegance
showcase"). Because it buys no north-star parity (the metric is *runtime* parity
with Rust), it is a future *owner-funded* fidelity/showcase item, prioritized as
elegance rather than against the parity backlog.
