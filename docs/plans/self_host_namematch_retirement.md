# Self-host name-match retirement — typed-home design for the two remaining SHIP-GATE predicates

> **Scope:** evaluate typed-home options for the two remaining SHIP-GATE name-match
> predicates left after cleanup-7 (commit `27d02a10`) and sequence the impl vs the
> in-flight expected_type port (brief-8 / `docs/plans/self_host_ssa_cleanup.md` §7).
> Sole deliverable. No compiler-code edits in this pass.

## 0. Background

Cleanup-7 retired `is_collection_getter_method` cleanly by reusing the existing
typed discriminator `option_ref_payload_of(&gmod, ret_tid) >= 0` at
`tests/fixtures/self_host_lowerer/lower.gg:4957`. That worked because the typed
home already existed and was populated end-to-end by the lowerer itself
(`record_option_ref_payload` at `lower.gg:2848`, `:3659`).

Two predicates remain:

| # | Predicate                  | Def                          | Call site                                  | Drives                                       |
|---|----------------------------|------------------------------|--------------------------------------------|----------------------------------------------|
| 1 | `is_string_view_method`    | `lower.gg:462-473`           | `lower.gg:4972` (EMethodCall result-tag)  | `LoView` tag → `op_consume` materialises    |
| 2 | `is_owning_mutator_arg`    | `lower.gg:513-540`           | `lower.gg:4899` (EMethodCall arg-loop)    | `CkCallArgOwning` → `OpMove`/`OpClone`      |

Both predicates live in `EMethodCall`'s arms inside `lower_expr`
(`lower.gg:4363` onward).

## 1. Current state — per-predicate inventory

### 1.1 `is_string_view_method`

**Body (`lower.gg:462-473`):** open-coded `if method == "slice" or method == "substring"
or method == "byte_slice" or method == "char_at": return true` × 5 lines covering
{slice, substring, byte_slice, char_at, trim, trim_left, trim_right, strip, lstrip,
rstrip, removeprefix, removesuffix, str, as_str} — 14 method names (matches Rust's 14
`returns_view: true` rows at `src/ir/lowering/builtins.rs:692-716`).

**Sole call site (`lower.gg:4972`):**
```
elif recv_is_string and is_string_view_method(mname):
    mdst = add_local_with(&ctx, ret_tid, NO_NAME, LoView(), BoNone())
```
Gated by `recv_is_string` (`recv_type_name == "String" or "GorgetString" or "Str"`,
`:4956`) — so user methods on non-String types that happen to share names
(`my_buf.trim()`) don't trip the view tagging. Decision drives result-local
ownership = `LoView` so `op_consume` clones the view at any owning consume
position rather than moving the dangling alias by value.

**Rust analog:** `BuiltinMethodDecl { …, returns_view: true, … }` at
`src/ir/lowering/builtins.rs:692-716`, accessed via
`Ctx::builtin_returns_view` (`src/ir/lowering/context.rs:770-779`), called from
`src/ir/lowering/exprs/methods.rs:2595` — `ctx.builtin_returns_view(&type_name,
method_name) && ctx.type_mapper.is_string_type(ret_type)`. **The Rust check is
keyed on the typed `BuiltinMethodDecl.returns_view` field, NOT a name table.**

**Nearby typed metadata:**
- `RuntimeFn` (`compiler/data/schema.gg:153-162`) has `returns_fresh: bool` —
  the closest sibling — but NO `returns_view`. `returns_fresh` is also not
  consumed in self-host today (data-only; consumed in Rust's `tag_ownership.rs`).
- `option_ref_payload_of` — typed shape check on return type. Doesn't apply
  (view-returning String methods return plain `Str`, not `Option[Ref[T]]`).
- `resource_meta_for(&gmod, recv_type_name)` returns a `ResourceMeta` exposing
  `collection_kind`. Doesn't apply for String view-vs-owned (orthogonal axis).

### 1.2 `is_owning_mutator_arg`

**Body (`lower.gg:513-540`):** `match kind` over `CollectionKind` (5 arms +
`CkNotCollection`):
- `CkVector` / `CkDeque`: `push` ⇒ idx==0; `set` / `insert` ⇒ idx==1
- `CkHeap`: `push` ⇒ idx==0
- `CkDict`: `put` / `set` ⇒ idx==0 or idx==1
- `CkSet`: `add` / `insert` ⇒ idx==0

**Sole call site (`lower.gg:4899`):**
```
if is_owning_mutator_arg(_recv_ck, mname, _ma_idx):
    ma_kind = CkCallArgOwning()
```
The `CkCallArgOwning` promotion is what flips `op_consume` from a wrong
`OpBorrow` (default for runtime mutators not in `fn_move_params`) to the correct
`OpMove`/`OpClone`. Without this, `gorget_array_push`'s value arg shallow-aliases
the source local and both free the same heap.

**Rust analog:** `consuming_positions_by_name: Vec<usize>` at
`src/ir/lowering/exprs/methods.rs:1851-1861` — **also a name-keyed `match`**
(`"push" | "add" | "extend" | "send" | "push_back" | "push_front" =>
vec![0], "put" | "set" | "insert" => …`). Rust then filters via a TYPED
ABI check at `:1877-1885` — `ctx.fn_param_abis.get(…).get(idx+1) == Some(ParamABI::ByPtr)`
disqualifies pass-by-pointer params. Plus a name-keyed gate
`is_string_builder_method = type_name == "GorgetString"` to exclude
`GorgetString.push`/`push_line`/`push_char` (StringBuilder appends READ the arg).

**The self-host's `CollectionKind` gate is more typed than Rust's first
filter.** Rust passes through any method named `set`/`insert` on any type
and relies on the ABI filter; self-host gates on `_recv_ck` so a user struct
method named `set` is never even a candidate. This is a **strict improvement**
over the Rust shape, not a divergence.

**Nearby typed metadata:**
- `CollectionKind` (`gir.gg`) — already used; this is the typed key.
- `fn_move_params: Dict[String, Vector[bool]]` (`lower.gg:8806`) — populated
  from `fdef.params[i].ownership == 2` (the user's `!` sigil). Built-in
  runtime mutators (`gorget_array_push` etc.) are NOT registered here — that
  gap is precisely what `is_owning_mutator_arg` closes.
- `RuntimeFn` (`schema.gg:153`) — has per-`RuntimeParam` `abi`, no per-param
  consume-flag analog of Rust's `consuming_positions`.

## 2. Typed-home options — per predicate

LOC ballparks are gross order-of-magnitude only; risk is qualitative against the
SHIP-GATE goal (retiring the name-match without regressing behaviour).

### 2.1 Options table — `is_string_view_method`

| Opt | Sketch                                                                                   | LOC   | Risk     | Semantic improvement      |
|-----|------------------------------------------------------------------------------------------|-------|----------|---------------------------|
| A   | Reuse existing typed signal                                                              | n/a   | n/a      | **Not available.** No analog of `option_ref_payload_of` for String-view shape exists. `Str` views and owned `Str` have the same `ret_tid`; the view-vs-owned distinction is method-keyed by construction (Rust mirrors via `BuiltinMethodDecl.returns_view`). |
| B   | Module-init Dict `view_method_names: Dict[String, bool]` on `GirModule`                  | ~25   | LOW      | **COSMETIC.** Single consumer (`lower.gg:4972`); same data; just renames "list of strings in a function" to "list of strings in a Dict." Fails CLAUDE.md "No name matching" litmus (single-consumer + name-keyed = identical to predicate). |
| C   | Add `bool returns_view` to `RuntimeFn` in `schema.gg`; populate per-entry in `resources.gg` (e.g. `gorget_str_slice` ⇒ true); query via `runtime_fn_returns_view(c_name)` after `map_to_runtime` resolves the C name | ~50 + cross-language + SCHEMA_VERSION bump | MEDIUM-HIGH (schema bump cascades to Rust mirror `src/ir/resource_schema.rs`; coordinate landing) | **REFERENCE-GRADE for the runtime-fn axis.** Reads typed at the source of truth. But: GIR-lowering site at `:4972` doesn't yet know the runtime C name — `map_to_runtime` runs in `lir_lower.gg`. Either lift the resolution to `lower.gg` (~30 LOC of plumbing) or query by method-name in `lower.gg` and runtime-C-name in `lir_lower.gg` from a SHARED registry (then it's really Option F dressed up). |
| D   | Port full `BuiltinMethodDecl` infrastructure (Rust `src/ir/lowering/builtins.rs`) into self-host: per-type method tables, `protocol_for_mangled_name`, `builtin_returns_view`, … | ~300-500 | HIGH (broad surface — see Rust file's 916 lines; touches receiver-type → method-list dispatch across many call sites) | **REFERENCE-GRADE end-state.** Subsumes Option C and any future per-method typed signal (returns_view, returns_fresh, is_mutating, self_conv, runtime_callee). Out of scope for an incremental cleanup. |
| E   | Refactor the GIR site to consult the LIR-level String-method dispatch at `lir_lower.gg:1276` | ~20  | LOW behaviour, HIGH layering | **LAYER VIOLATION.** GIR result-tagging would now depend on LIR symbol mapping. Violates "Layering discipline — Lossless on invariants, lossy on syntax" and "Resolve once, write through." Reject. |
| F   | Self-host-only Dict constant `STRING_VIEW_METHODS: Dict[String, bool]` (or `Set[String]`) populated at module init | ~15 | LOW | **COSMETIC, NO RUST PRECEDENT.** Rust does NOT have a `returns_view_by_name` constant table — it uses `BuiltinMethodDecl.returns_view`. Inventing one would put the data in a self-host structure that Rust doesn't mirror. Equivalent to Option B with a different shape. Per CLAUDE.md "No name matching" — a single-consumer name-keyed table is renaming, not retirement. |

**Per-predicate verdict — `is_string_view_method`:** there is **no clean
typed home short of Option D** (full BuiltinMethodDecl port). Option C is the
narrow incremental path but requires a SCHEMA_VERSION bump and a cross-language
coordinate land — non-incremental. Options A/B/E/F are cosmetic or layer-wrong.

### 2.2 Options table — `is_owning_mutator_arg`

| Opt | Sketch                                                                                   | LOC   | Risk     | Semantic improvement      |
|-----|------------------------------------------------------------------------------------------|-------|----------|---------------------------|
| A   | Reuse existing typed signal                                                              | n/a   | n/a      | **Not available.** No analog of `option_ref_payload_of` for "consumes value at idx N." The closest sibling — `fn_move_params` — is keyed on user-declared `!` sigils on user functions; runtime mutators have no such declaration. |
| B   | Module-init Dict on `GirModule`: `runtime_consuming_positions: Dict[String, Vector[int]]` keyed by mangled method name + collection kind, populated once at GIR lowering start | ~40 | LOW-MEDIUM | **MARGINAL.** Two consumers possible (this site + a future `OpClone`-materialization arm) BUT the data is the same; just relocated. Same litmus risk as B for #1. |
| C   | Add `Vector[int] consuming_positions` to `RuntimeFn`; populate per-entry in `resources.gg`; query in `lower.gg` after `map_to_runtime` resolves the C name | ~80 + cross-language + SCHEMA_VERSION bump | MEDIUM-HIGH | **REFERENCE-GRADE for the runtime-fn axis.** Same plumbing problem as Option C for #1 — GIR site doesn't know C name yet. |
| D   | Full `BuiltinMethodDecl` port — `is_mutating` + `params` together imply consuming positions per receiver type | ~300-500 | HIGH | **REFERENCE-GRADE end-state.** Subsumes Option C. Out of scope for incremental. |
| E   | Refactor to LIR-level — defer the decision to lir_lower's `needs_ptr_arg`. | ~30 | LOW behaviour, HIGH layering | **LAYER VIOLATION** + behaviour issue: `op_consume`'s OpMove/OpClone decision MUST happen at GIR lowering (the LIR-level pass already assumes operands are typed). Reject. |
| F   | Self-host-only constant `Dict[(CollectionKind, String), Vector[int]]` (or two parallel tables) populated at module init from a static list | ~30  | LOW | **PRAGMATIC MIDDLE GROUND.** Direct port of Rust's `consuming_positions_by_name` at `methods.rs:1851` — Rust uses a `match` literal-expr; the self-host's analog is a Dict populated once. **Both still name-matching at heart** — the data is the same. **But:** Rust gets this past CLAUDE.md "No name matching" because it then filters via `ParamABI::ByPtr` (typed). The self-host already does better than Rust here: the `CollectionKind` gate IS the typed filter. So Option F as a self-host port would be: keep the `CollectionKind` gate AS the source of truth (already typed), and move the inner per-(kind, method) consuming-position lookup into a constant. This is **cleaner than the current open-coded `match` only because** the data table is uniform and grep-greppable; it's not a SEMANTIC improvement. |

**Per-predicate verdict — `is_owning_mutator_arg`:** the current code is
already MORE typed than Rust at this site (CollectionKind gate vs Rust's
name-keyed match + ABI filter). The name-match inside (`mname == "push"`,
`mname == "set"`, etc.) is the residual name-set, and it CANNOT be replaced
by a typed signal short of:
- Option C — schema bump (out of scope), or
- Option D — full BuiltinMethodDecl port (out of scope), or
- Option F — same-shape constant table (cosmetic).

## 3. Sequencing vs brief-8 (expected_type port)

**Brief-8 (`/tmp/impl-brief-8.md`) STEP 2.3** explicitly targets free-function
`lower_call`'s call-arg loop at `lower.gg:5499-5512` — that loop lives inside
`int lower_call(...)` at `:5458`. It handles `ECall(EIdentifier, args)` paths.

Our predicates are at `:4899` and `:4972`, both inside the `EMethodCall`
arm of `lower_expr` (`:4363+`). Rust splits the two paths likewise:
- `src/ir/lowering/exprs/calls.rs` = free-function `lower_call` (brief-8 target)
- `src/ir/lowering/exprs/methods.rs` = method calls (our predicates' analogs)

**Subsumption analysis:**

| Predicate                  | Brief-8 effect? | Why                                                                                                                                                                                                                                                                                                                                                                                            |
|----------------------------|-----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `is_string_view_method`    | NO              | The call site is RESULT-tagging (`mdst = add_local_with(LoView())`) — about the LOCAL's ownership tag, NOT the dest type. Brief-8 propagates `expected_type` into RHS expression lowering; it doesn't add the LoView local-ownership tag.                                                                                                                                                       |
| `is_owning_mutator_arg`    | NO              | The call site is in EMethodCall's value-arg consume loop. Brief-8 STEP 2.3 ports the free-function `lower_call` arg loop — explicitly NOT EMethodCall. Even if brief-8 expanded to EMethodCall args, `expected_type` affects RHS type-tagging (variant ctors, bare None), NOT the `ConsumeKind` (OpMove/OpClone/OpBorrow) decision. They're orthogonal axes.                                     |

**Surprise / interaction:** none, in either direction. Brief-8 makes neither
predicate easier nor harder; the two efforts are on disjoint code paths.

**However:** brief-8 may add new EMethodCall-adjacent typed metadata as a
byproduct (e.g., a clearer `LowerCtx.expected_type` field that a follow-up
could use for STRING_TYPE detection — `recv_is_string` already exists). Not a
subsumption, just a small future-proofing data point.

## 4. Recommendations

### 4.1 Per predicate

| #   | Predicate                  | Recommendation       | One-line rationale                                                                                                                                                                                            |
|-----|----------------------------|----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1   | `is_string_view_method`    | **DEFER**            | No clean typed home exists short of Option C (SCHEMA_VERSION bump) or Option D (BuiltinMethodDecl port); user already FORGAVE this name-match (drop_emission.md `:649-651`); cosmetic options B/F fail litmus. |
| 2   | `is_owning_mutator_arg`    | **DEFER** (preferred) **OR** PROCEED-NOW with Option F (acceptable) | Current `CollectionKind`-gated shape is already strictly MORE typed than Rust; remaining name-match is unavoidable without Option C/D; an Option F port is cosmetic and adds a constant table whose only justification is uniformity. |

### 4.2 Joint recommendation + sequencing

**Both predicates converge on the same end-state: Option D (BuiltinMethodDecl
port).** They do not diverge — each is shadowed by the same Rust infrastructure
(`BuiltinMethodDecl` carries `returns_view`, `is_mutating`, `params`,
`runtime_callee`, `self_conv` — all the typed signals these predicates
synthesise from method names).

**Recommended sequencing:**

1. **Land brief-8** (expected_type port). This is the largest in-flight cleanup
   on `lower.gg`; let it stabilise the GIR-lowering hot path before touching
   anything else.
2. **DEFER both predicates** until a separate **BuiltinMethodDecl port effort**
   is scoped. That effort should be planned as:
   - Schema design — extend `RuntimeFn`, OR add a new `BuiltinMethodDecl`
     type, OR a per-collection-type protocol table; pick after surveying ALL
     consumers (`returns_fresh` is already in `RuntimeFn` and unused in
     self-host — there are multiple sleeping fields here to wake up at once).
   - SCHEMA_VERSION bump + Rust mirror update.
   - Port `protocol_for_mangled_name` to self-host (it exists at
     `src/ir/lowering/builtins.rs` — receiver-type → method-list dispatch).
   - Migrate both predicates' call sites to read through `builtin_returns_view`
     / `builtin_consuming_positions` accessors.
   - Migrate the LIR-level String-method dispatch at `lir_lower.gg:1276` AS
     A SECOND CONSUMER (its `match method` is the OTHER current name-match in
     the area — fits the "multiple consumers" litmus for justifying a typed
     home).
3. **Reject Option F** for `is_owning_mutator_arg` as an incremental cleanup
   in the meantime. It would add ~30 LOC of cosmetic data movement and freeze
   the wrong shape (self-host-only constant with no Rust counterpart) into the
   tree, making the eventual Option D port harder.

### 4.3 If the user wants a SHIP-GATE green flag despite the deferrals

The SHIP-GATE block at `docs/plans/drop_emission.md:643-661` lists these as
"typed-signal cleanups." The user already explicitly FORGAVE
`is_string_view_method` (`:649-651`: *"the `is_string_view_method` name-match is
FORGIVEN for now since the patch fixes the truncation; a FOLLOW-UP agent removes
the name-matching and makes the code reference-grade on a subsequent pass"*).
By the same logic, `is_owning_mutator_arg`'s `CollectionKind`-gated shape is
already typed at the receiver-kind level, and the inner method-name set is
unavoidable until Option C/D ships. Both can be marked **DEFERRED to a future
BuiltinMethodDecl-port effort** in `TODO.md` without keeping the SHIP-GATE
block open indefinitely.

## 5. Out of scope (explicit)

- Convergence drift (`__gg_R`/`__gg_W`).
- NEXT BLOCKER #4 (`type_category_for_name`'s `unwrap_or` Some-arm) —
  brief-8's scope clarification confirms it's unrelated.
- The LIR-level String-method `match method` dispatch at `lir_lower.gg:1276` —
  acknowledged as a parallel name-match in a different layer; would be a
  natural SECOND consumer for Option D and is one of the reasons Option D
  is the right end-state, but porting it is its own scoped effort.
- Schema design (SCHEMA_VERSION bump + Rust mirror coordination) — listed
  here as the gating step for Option C/D, not designed.

## 6. Litmus-test summary (per CLAUDE.md)

| Litmus                                                                                                  | `is_string_view_method`                                                                  | `is_owning_mutator_arg`                                                                                          |
|---------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| Single consumer or multiple?                                                                            | Single (one call site)                                                                   | Single (one call site)                                                                                           |
| Source of truth elsewhere?                                                                              | YES — Rust's `BuiltinMethodDecl.returns_view`                                            | YES — Rust's `consuming_positions_by_name` + ABI filter                                                          |
| Existing typed home in self-host?                                                                       | NO                                                                                       | PARTIAL — `CollectionKind` gate is typed; inner method-set is not                                                |
| Single-consumer name-keyed Dict = cosmetic?                                                             | YES (Option B/F = cosmetic)                                                              | YES (Option F = cosmetic)                                                                                        |
| Reference-grade end-state                                                                               | Option D                                                                                 | Option D                                                                                                         |
| Conclusion                                                                                              | DEFER to BuiltinMethodDecl port                                                          | DEFER to BuiltinMethodDecl port (or accept current CollectionKind-gated shape as good-enough until then)         |

The recurring theme: both predicates are tiny enough that a typed-home
*just for them* is a rename, not a retirement. The clean fix is to port the
infrastructure that has the right shape (Option D) and wire BOTH at once —
plus the LIR-level String-method dispatch — so the typed home has the
multiple-consumers + source-of-truth shape that justifies its existence
per Layering rule 3.
