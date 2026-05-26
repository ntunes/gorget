# Sound borrow-by-default for collection `.get()` — `Option[Ref[T]]` representation

**Status (2026-05-26):** PLAN — not yet implemented. Supersedes the shallow-value-copy
borrow-by-default attempt (uncommitted in `tests/fixtures/self_host_lowerer/lower.gg`, saved as
`/tmp/bug3b_fork_a_edits.diff`; see memory `project_selfhost_bug3b_fork_a_status`). This is the
foundational fix the user chose (fork-A option 3) after the shallow-copy approach fixed bug #3b's
OOM but destabilised the self-host bootstrap.

**One-line goal:** make collection `.get()`/`.first()`/`.last()` in the self-host return
`Option[Ref[T]]` (an Option whose Some payload is a *pointer* to the element), matching the language
spec — so reads/mutations go *through the pointer* at zero cost and an owned bind clones the pointee,
instead of the unsound shallow value-copy that corrupts aggregate reads.

**Reference-grade, not Rust-mirror (user directive 2026-05-26).** The Rust compiler is a *reference
for the semantics*, not a template to copy line-for-line. Where Rust does something ugly — notably
its `strip_prefix("Ref__")` string-matching to recover the pointee type
(`src/ir/lowering/exprs/methods.rs:612-615`) — the self-host should do it *right*: thread the typed
`GtPtr` payload id through (Decision D3), never reconstruct meaning from a mangled-name substring
(CLAUDE.md "No name matching"). Treat the `Ref__`-recognition fallbacks in this plan as bridges to
delete once the typed path is threaded, not the destination. And: this work may expose genuine
Gorget *compiler* bugs (CoW/borrow gaps) — when it does, fix them in the Rust compiler too (the
JS-interpreter-snag pattern), don't paper over them in self-host. Self-host is the showcase; it
reads like `docs/book/`.

---

## 1. Why (rationale + spec grounding)

The language spec already mandates this representation:

- `docs/language-design.md:353` — *"Indexing a collection (`v[i]`, `dict[key]`) returns a
  **reference to the element in-place, not a copy**. … the value stays as a **pointer** internally,
  and methods/field access resolve through it at zero cost."*
- `docs/language-design.md:326-328` — borrowed values *"cannot be stored in any structure that
  escapes the callee's frame. This prevents **shallow-copy bugs where the stored value shares the
  caller's heap allocations**."* ← this is exactly the bug the shallow-copy attempt hit.
- `docs/book/11-ownership.md:248-305` — CoW: collection reads borrow (a pointer); the clone happens
  only at the first mutation through an alias, or when an owned copy is demanded.

The Rust reference compiler implements this with `Option[Ref[T]]` (the `__gg_Option__Ref__<T>`
structs visible in `tests/fixtures/self_host_lowerer/driver.c`, whose `Some_0` field is a `void*`):

- `src/ir/lowering/exprs/methods.rs:~1987-1991` — for borrowing methods (`get`/`first`/`last`) it
  builds `Option__Ref__{elem}` and registers it with a `GirType::Ptr(inner_type)` payload via
  `ensure_option_type_registered`.
- `src/ir/lowering/types.rs:~1166-1183` — `make_option_type_def` builds the Some variant's `_0`
  field directly from the passed `type_id` (which is the `Ptr` type for borrowing gets).
- `src/semantic/types.rs` — `ResolvedType::Ref(TypeId)` is a first-class source-level type; the IR
  lowers it to `GirType::Ptr(inner)`.

The self-host currently types `.get()` as `Option[T]` (value payload) and the prior attempt stored
a pointer into that value-sized field (or shallow-memcpy'd the element) — representationally
unsound: aggregate-field reads through the alias corrupt (`ti.variants` reads empty →
`lookup_ctor_field_type` falls back to `int64_t` → bootstrap crash). `Option[Ref[T]]` removes the
shortcut: the payload IS a pointer, reads deref, owned binds clone the pointee.

**This also retires two existing workarounds** (do as part of this work — see "Self-host as the
elegance showcase" in CLAUDE.md): the `feedback_nested_vector_get_set` "rebuild inner Vector"
dodge (the pointer makes `coll.get(i).unwrap().push(x)` mutate the real element in place), and the
materialize/writeback scaffolding the shallow-copy attempt added.

---

## 2. Semantics (the contract this must produce)

`Ref[T]` ≡ `GtPtr(T)` in GIR (no new GirType variant; reuse `GtPtr`). `.get(i)` →
`Option[Ref[T]]` whose Some payload is the **element's address** (the `void*` from
`gorget_array_get`/`gorget_map_get`), NOT a copy.

| Surface shape | Lowering | Clone? |
|---|---|---|
| `coll.get(i).unwrap().field` (read) | deref ptr, read field (existing `GtPtr` auto-deref) | no |
| `coll.get(i).unwrap().push(x)` (mutate inner) | push **through the pointer** → mutates the real element in place | no (no writeback needed — it IS the element) |
| `T x = coll.get(i).unwrap()` (owned named bind) | deref + `T__clone` → independent owned `x` | yes (one element) |
| `coll.get(i).unwrap()` consumed at an owning position (`set`/return/field-init/`!`) | clone the pointee (a-5 / op_consume on a `GtPtr`-to-resource → `decide_ptr_consume` → `OpClone`) | yes |
| `for x in coll` (existing for-element path) | unchanged — already borrows via `BoCollectionElement` | no |

Invariant: **only the owner frees.** The `Option[Ref[T]]` and the unwrapped `Ref[T]` are
borrows (`LoBorrowed`), never dropped. The collection remains the sole owner; owned binds clone
into a new owner that IS dropped. No shallow copy ever shares a heap buffer with the collection.

**Out of scope (trust, like the rest of the self-host):** detecting `coll.push(y)` while a
`coll.get(i)` ref is live (use-after-realloc). The self-host compiles only known-valid Gorget
(Rust's borrow-checker validated it upstream); it has no safety pass and this plan does not add
one. If a self-host source site does this, fix the *source*, not add checking.

---

## 3. Design touchpoints (phased)

Anchors below are HEAD-relative `tests/fixtures/self_host_lowerer/` line numbers (the execution
agent starts from committed `HEAD`, NOT the dirty tree — Decision D4 / §0). They drift as edits
land.

**Keystone correction (review pass 1, RES-1):** the downstream deref/dispatch/consume machinery
keys on the GIR type being `GtPtr(T)`. But the self-host reconstructs the unwrapped/destructured
payload type by **string-slicing the Option name** — `inner = recv_tn.slice(8,...)` → `"Ref__T"`,
then `lookup_or_register_named(&gmod, "Ref__T")` (`lower.gg:~4350-4378`) → a `GtNamed("Ref__T")`,
**NOT** a `GtPtr(T)`. So `GtPtr`-ness does NOT arise automatically; it must be CONSTRUCTED at every
inner-type-reconstruction site, exactly as Rust does: `src/ir/lowering/exprs/methods.rs:612-615`
special-cases `inner_name.strip_prefix("Ref__") → GirType::Ptr(pointee)`. The self-host has zero
`Ref__` handling today (confirmed). Porting this `strip_prefix("Ref__") → GtPtr(inner)` recognition
into the unwrap branch AND the match-destructure inner-type derivation is the **core of the change**,
not Phase 0 plumbing.

Once the payload local is genuinely `GtPtr(T)`, these DO already work (verified pass 1):
`EFieldAccess` auto-derefs a `GtPtr` base (`lower.gg` EFieldAccess Ptr-unwrap); `local_type_name`
/`type_id_to_base_name` deref `GtPtr(inner)→T` for method dispatch (`lower.gg:~3085`);
`op_consume`/`decide_ptr_consume` clone a `GtPtr`-to-resource at owning positions
(`lower.gg:~1406-1467`); `OpBorrow` of a `LT_PTR` slot passes the pointer value, not `&slot`
(`lir_lower.gg:~2835`); `emit_void_ptr_option_wrap`'s `payload_is_ptr` branch stores the element
pointer (`lir_lower.gg:2134-2144`); and the C backend already emits `void* Some_0` +
`__field_read_` returning the pointer for `LT_PTR` payload fields (`lir_codegen.gg:~90/564/3679/2367`,
matching `driver.c`'s `__gg_Option__Ref__*`). So Phase 0 should pass — but the GtPtr-ness must be
born first (Phases 1+3).

### §0 — Starting state (RES-7): verify a CLEAN baseline before touching anything

The repo working tree is currently DIRTY (`lower.gg` carries the ~174-line shallow-copy attempt,
saved at `/tmp/bug3b_fork_a_edits.diff`). This plan builds the `Option[Ref[T]]` approach **from
committed `HEAD`**, not on top of that attempt. A true `isolation: "worktree"` checkout off `HEAD`
is clean — BUT memory `feedback_multi_agent_worktree_isolation` records that subagents in THIS
project sometimes run in `/workspace/gorget-1` directly. So the executing agent MUST, as its first
action: run `pwd` + `git rev-parse --show-toplevel` (confirm it's inside its worktree), then
`git diff HEAD --stat -- tests/fixtures/self_host_lowerer/lower.gg`; if non-empty, STOP and report
(do not `git checkout` the parent's uncommitted work without confirmation). The HEAD baseline has
bug #3b (the OOM) present and no `Ref__` handling — that is the correct, expected starting point.

### Phase 0 — Prereq spike: prove the C backend emits pointer enum payloads (DE-RISK FIRST)

Before any wiring, confirm the self-host's `lir_codegen.gg` can emit an `Option__Ref__T` whose
`Some_0` is a pointer field, that `IEnumInit` stores a pointer into it, and that the
`__field_read_Option__Ref__T_Some_0` helper returns the pointer (not a deref'd value). Rust emits
exactly this in `driver.c` (`__gg_Option__Ref__GirTypeInfo` with `Some_0` a `void*`, read via
`*(void**)`), so the C *shape* is proven — the question is whether the self-host's struct-gen +
field-read emission already handle a `GtPtr`/`LT_PTR` enum field. Hand-craft a tiny fixture
(`Vector[SomeStruct]`, `auto r = v.get(0)`; `match r: case Some(p): print(p.field)`) compiled by
the **self-host** and inspect the emitted C. **If the C backend can't emit it, that becomes Phase 1
and the rest blocks on it.**

### Phase 1 — `.get()` result typing → `Option[Ref[T]]` (RE-ANCHORED per review pass 2)

**Corrected mental model (pass-2 RES-1): the type is born in the typechecker and STRIPPED in GIR
lowering — the fix is to stop stripping, not to add typing at the getter tag site.** The self-host
typechecker ALREADY types `.get()`/`.first()`/`.last()` as `Option[Ref[T]]`
(`tests/fixtures/self_host_typechecker/infer.gg:386-410`, explicit `RTRef` wrap). For a built-in
getter, `ret_tid` flows into `lower.gg` via the typechecker side-table (`lookup_expr_gir_type`,
`lower.gg:~4637` → `~2761` `resolved_to_gir_type`) BEFORE the `is_collection_getter_method` tag site
(`~4660`) runs — so editing ~4660 is too late / wrong. The Ref is erased in two arms:
  - `resolved_to_c_name` `RTRef` arm (`lower.gg:2679-2683`): collapses `Ref[T]`→pointee name, so
    `Option[Ref[T]]` mangles to `Option__T`.
  - `resolved_to_gir_type` `RTRef` arm (`lower.gg:2731-2738`): returns `resolved_to_gir_type(inner)`,
    dropping the Ref → bare pointee GIR type.

**The Phase-1 edit:** in these two arms, STOP stripping `RTRef` — instead mangle to `Ref__<pointee>`
(c-name) and register `GtPtr(pointee)` (gir type, via `register_ptr`, `gir.gg:430`), mirroring Rust
(`src/ir/lowering/`'s Ref→Ptr lowering). Then `Option[Ref[T]]` naturally mangles to `Option__Ref__T`
and the Some payload GIR type is `GtPtr(T)`. (Caveat RESOLVED by pass-3 R3 — the flip is provably safe, no scoping guard needed: `RTRef` is
produced ONLY at `infer.gg:398,405` (the `.get()` getter); `&`/`!` params go through
`ast_type_to_resolved` (`types.gg:254`, no `TRef`/`RTRef` — sigils are separate ownership metadata,
not a type wrapper) and their C names/ABI form on a *separate* path (`lower.gg:~6906-6914`,
`map_ast_type` + `GtMutPtr` keyed on `p.ownership==1`). Un-stripping the `RTRef` arms cannot touch
`&T` params.)
- **`pop`/`remove`** keep returning `Option[T]` (owned) — their typechecker types are NOT `RTRef`
  (they transfer ownership out), so they're unaffected by the un-stripping. Confirm in infer.gg.
- Tag the dst `LoBorrowed`; the *type* (`GtPtr`) now carries the borrow (Decision D2).

### Phase 2 — Option-of-ptr struct generation

- **`lir_lower.gg` Pass 3 (~953-981).** `Some_0` for `Option__Ref__<inner>` must be `LT_PTR`. Note
  (pass-5 #3): `resolve_field_lir_type("Ref__T")` ALREADY returns `LT_PTR` via the
  unregistered-name fallback (`lir_lower.gg:~639-668`) — so the field lands as a pointer with no
  `Ref__`-infix recognition needed. The actual requirement is the *inverse*: ensure `Ref__T` is
  NEVER registered as a struct (which would shadow the fallback) and never enters the drop/resource
  tables (RES-5). So Phase 2 here is mostly a CONFIRM-and-guard, not new recognition logic.
- **`emit_void_ptr_option_wrap` (`lir_lower.gg:2090`).** Confirm the `payload_is_ptr` branch
  (`:2134-2144`) fires (payload field is `LT_PTR`) → stores `raw_in_some` (the element pointer)
  directly via `IEnumInit`. Likely **no change** — just needs Phase 2's struct to report a ptr
  field. Verify the consuming-method (`pop`/`remove`) path is untouched.

### Phase 3 — `.unwrap()` and `match` destructure yield `Ref[T]` (a real `GtPtr`)

This is the keystone (RES-1/RES-2).

**Preferred mechanism — typed, not name-matched (user directive) — REQUIRES NET-NEW TYPED STORAGE
(pass-4 R-A).** Honest scoping: the typed lookup is NOT free today. There is no GIR-level typed
payload-field id to read back — `GirFieldInfo` is `{String name, String type_name}` (`gir.gg:248`,
strings only), and the typed `GtPtr`/`LT_PTR` `Some_0` field is built only at the LIR layer in
Pass 3 (`lir_lower.gg:944-981`), itself reconstructed from the mangled name and absent from
`gmod.type_infos` by name. So the reference-grade path must **add the typed channel**:

> **Phase 2b (net-new) — thread the payload `GtPtr` id (populate site corrected per pass-5).** The
> `Option__Ref__T` GIR id is born in the **RTGeneric arm** of `resolved_to_gir_type`
> (`lower.gg:2724-2730`): it mangles the arg to a string via `resolved_to_c_name` and returns
> `lookup_or_register_named("Option__Ref__T")` — it does NOT recurse into the arg, so the
> previously-named `RTRef`-arm edit is DEAD for this case. **Populate there:** in the RTGeneric arm,
> when `def.name == "Option"` (or Result) and the single arg is `RTRef(inner)`, ALSO compute
> `ptr_tid = register_ptr(&gmod, resolved_to_gir_type(inner))` and record
> `option_ref_payload.put(option_tid, ptr_tid)` — a typed side table on `GirModule`
> (`Dict[int,int]`, Option GIR id → payload `GtPtr` id; single source of truth, one accessor,
> layering rule 3). The Option id is stable/name-deduped (`gir.gg:423-428` `named_types`), so the
> SAME id appears at registration and at every read site (verified pass-5).
> **Read sites:** unwrap has `recv.type_id` (`lower.gg:~4330`) → table lookup → mint payload as that
> `GtPtr`; svardecl just matches `GtPtr(inner)` on `val_loc.type_id` directly (no table needed — it
> consumes an already-minted `GtPtr`). Match needs the id THREADED: `lookup_ctor_field_type` takes
> only the enum NAME today (`lower.gg:~6373`), so pass the scrutinee's `scr_tid` (`~6398`) down to it
> (or look the payload up before calling) so the Some payload binds as the table's `GtPtr` rather
> than re-parsing. **Zero string inspection at the read sites once threaded.**

**Bridge mechanism (transitional, flagged for deletion — narrow scope after pass-5).** With Phase 2b
threaded, the only site left without a typed Option id in hand is the `infer_method_return_type`
*heuristic fallback* (it builds a type from a NAME before any registered id exists). There, teach the
shared leaf `resolve_field_gir_type` (`lower.gg:2988-3003`) to map a `Ref__`-prefixed name →
`register_ptr(pointee)`, marked `# BRIDGE: typed via option_ref_payload once on the typechecker
path`. This is name-matching — a bridge, NOT the destination. (Rust uses exactly this string-match at
`methods.rs:612-615` — the ugliness we improve on per the reference-grade directive, not the bar we
match.) Known name-deriving sites to audit (pass-1 + pass-2); each either gets the typed thread
(2b) or, if heuristic-only, the marked bridge:

- **unwrap branch (`lower.gg:~4350-4378`):** `inner_tid = lookup_or_register_named(inner_name_str)`
  where `inner_name_str` can be `"Ref__T"` → make it `register_ptr(pointee)` (mirror
  `src/ir/lowering/exprs/methods.rs:612-615` `strip_prefix("Ref__")→Ptr`).
- **`infer_method_return_type` (pass-2 RES-2 + pass-3 R1) — TWO more sites:**
  - getter arm (`lower.gg:~3389-3424`) lumps `get, pop, last, first, remove` into ONE arm emitting
    value `Option__<elem>`. **SPLIT it, do NOT flip it (pass-3 R1):** `get`/`first`/`last`/`safe_get`
    → `Option__Ref__<elem>`; `pop`/`remove` → keep value `Option__<elem>` (they transfer ownership
    out — used 22× in self-host; a wholesale flip aliases→double-frees them). NB the typechecker
    (`self_host_typechecker/infer.gg:387,405`) types ONLY `.get()` as `RTRef` — `first`/`last`/
    `safe_get` have no infer arm and fall through to THIS path-3 fallback (and aren't exercised by
    the current bootstrap, so this split is for parity/robustness, but the pop/remove hazard is real).
  - `unwrap` arm (`lower.gg:~3327-3387`) slices `inner = recv_name.slice(8,...)` = `"Ref__T"` →
    needs `Ref__`→`GtPtr` recognition. This is the inference path for `auto x =
    coll.get(i).unwrap()`.
- **match-destructure:** `lookup_ctor_field_type` (`lower.gg:~6373` Option arm, calls
  `resolve_field_gir_type` at the leaf) — fixing the leaf covers it; verify the Some payload for an
  `Option__Ref__T` scrutinee resolves to `GtPtr(T)` so `match coll.get(i): case Some(p):` binds `p`
  as a pointer.
- **`emit_payload_read_mode` (`lower.gg:6057-6097`) — RES-2 guard (placement per pass-3 R2):** a
  `GtPtr` payload must NEVER be clone-extracted. `dst_tname = type_id_to_name(dst_type)` collapses
  `GtPtr(T)`→`"T"` (`lower.gg:~2876`), so `resolve_payload_clone_fn("T")`→`T__clone` and — for the
  direct `match coll.get(i)` case where the scrutinee is an owned Option temp (`scrutinee_nonowning`
  false → `borrow_only` false) — the clone arm (`~6091`) fires and clone-extracts a *pointer*. Insert
  the guard at the TOP, **right after the `GICallExtern` field-read emit and BEFORE both the
  `borrow_only`/`is_deep_clone_fn` elision and the clone arm**: if `dst_type` matches `GtPtr(_)`,
  return the field-read dst directly (it holds the pointer). **Do NOT remove the `borrow_only`
  elision (D5 / §Phase-3 phasing note) — the GtPtr guard is ADDITIVE; for-element still needs
  `borrow_only`.**

**Phasing note — the for-element path (pass-2 RES-3, a real conflict).** The `for x in coll` path
(`lower.gg:~6571-6598`) is INDEPENDENT: it hardcodes `opt_name = "Option__" + elem_name` (VALUE
payload, not `Option__Ref__`) and relies on `emit_payload_read_mode(..., borrow_only=true)` +
the `borrow_only and is_deep_clone_fn` elision (`lower.gg:~6088`) to avoid the O(tree²) clone bomb.
A `GtPtr`-only guard does NOT cover it (its payload is value-typed `Option__T`). **Therefore: KEEP
the `borrow_only` elision for the value-payload for-element path; add the GtPtr guard as a separate
arm for the `Option[Ref]` path.** (Recommendation: do NOT migrate for-element to `Option[Ref]` in
this cluster — it works and is proven; unifying it is a separate follow-up. Decision D5.)

Once these mint a real `GtPtr(T)`, the downstream (field-access auto-deref, method dispatch,
`op_consume`→clone-at-owning) fires correctly.

### Phase 4 — owned bind clones the pointee; retire the live dodges

- **`decide_svardecl_emission` (`lower.gg:738`) + SVarDecl branches (~5560-5660) — RES-3.**
  Correction to the prior draft: HEAD's `decide_svardecl_emission` has **no `BoCollectionElement`
  carve-out** (that was only in the dirty tree); it routes on `source_is_borrow_alias`. For
  `T x = opt.unwrap()` with a `GtPtr(T)` `LoBorrowed` source, `source_tname =
  type_id_to_name(GtPtr(T)) = "T"` → `source_is_resource = true`, `source_is_borrow_alias = true` →
  **Branch C fires → `BorrowAlias()`** today, which would alias the collection's heap into an
  "owned" `x` (double-free). Insert a new branch **before Branch C**: detect a `GtPtr`-to-resource
  source *from the GIR type* (match `GtPtr(inner)`, NOT the name — the name collapses) bound to an
  owned local → `CloneAndMove(clone_fn_of_pointee)` (deref+clone). Verify `CloneAndMove`'s
  `clone_fn(OpBorrow(val))` works when `val` is a `GtPtr` (clone fns take `const T*`; `OpBorrow` of a
  `LT_PTR` slot passes the pointer — matches).
- **Retire `emit_chain_writeback` / `need_chain_writeback` (RES-4) — these are LIVE in HEAD**
  (`lower.gg:~4516-4571, 4646-4647`; `emit_chain_writeback` at `~5751`). They implement the
  `coll.get(i).unwrap().mut_method()` → `coll.set(idx, !recv)` dodge. With `Option[Ref]` the
  mutation already goes through the pointer in place; the writeback would set the element to a
  *pointer into itself* (recv is now `GtPtr`) → corruption. Remove the chain-writeback detection +
  emission. This also retires the `feedback_nested_vector_get_set` workaround.
- **Note (RES-4):** the `materialize_collection_borrow`/`mutation_root_local`/4-hook machinery the
  earlier draft listed for removal does **NOT exist in HEAD** (dirty-tree only) — starting clean
  (D4/§0) means there is nothing to remove there; do not chase it.

### Phase 5 — type-name plumbing (the regression-prone seam)

- **`resolve_field_gir_type` / `resolve_field_lir_type` / `type_id_to_name`.** Ensure a pointer
  payload round-trips: a `GtPtr(elem)` payload must format to a name (`Ref__<elem>` or `Ptr__<elem>`)
  that resolves back to a `GtPtr`. The cleanest is to **avoid the string round-trip** for the
  Option-of-ptr payload by registering the Option struct directly from the GIR `GtPtr` type id
  (Decision D3) rather than reconstructing from a mangled name. Decide and document.
- **`type_infos` population (`lower.gg:~8557/8572/8940` + mono path ~9100) — anchor corrected per
  RES-6.** These build `GirTypeInfo.variants[].fields[].type_name` from the AST as strings. They do
  NOT need to change for *user* enums (those keep value payloads). They are only the regression
  *victim* (a corrupted read produced bad data) — once `.get()` returns a real pointer, the reads
  that populate them stay correct. Confirm no Option-of-ptr leaks into user-enum field-type strings.

- **Drop-table guard (RES-5).** `Option__Ref__T` and `Ref__T` must NEVER be droppable and must
  never enter `resource_types` / `type_runtime_map` / the optionlike resource set — else Pass 3's
  unconditional `drop_fn_for_type(inner,...)` (`lir_lower.gg:~953-981`, inner=`"Ref__T"`) would emit
  a drop that frees the pointee → double-free (the Option[Ref]/Ref is a borrow, §2 invariant: never
  dropped). Today this holds *accidentally* (`drop_fn_for_type` returns "" for `Ref__T`). Make it
  explicit: assert `Ref__`-prefixed type names resolve to no clone/drop fn, and ensure D3's
  "register from GIR id" does not register the `GtPtr` payload as a resource. Add a guard + a
  comment so a future edit can't silently make `Ref__T` droppable.

---

## 4. Decisions to lock (reviewers: challenge these)

- **D1 — Naming.** Use `Option__Ref__<elem>` (Rust parity, matches `driver.c`) vs reuse
  `Option__<elem>` with a ptr payload. *Recommendation:* `Option__Ref__<elem>` — keeps the
  owned-Option (`pop`/`remove`) and borrowed-Option (`get`) types distinct, matches the reference,
  and avoids retrofitting every `Option__<elem>` consumer.
- **D2 — Do we still need `BoCollectionElement`?** Once the *type* (`GtPtr`) carries the borrow,
  the ownership tag may be redundant for clone-vs-borrow routing. *Recommendation:* keep the
  `LoBorrowed` tag (drop-suppression), drop reliance on `BoCollectionElement` for the get path;
  re-evaluate whether the for-element path still needs it.
- **D3 — Register Option-of-ptr from GIR type id, not mangled name.** *Recommendation:* yes — pass
  the `GtPtr` payload type id through to the Option struct registration so Pass 3 / type-name
  plumbing never has to parse `Ref__`/`Ptr__` out of a string (kills the Phase-5 seam and a class
  of name-matching per CLAUDE.md "No name matching"). **Strengthened (RES-1 + pass-4 R-A):** the
  read sites (unwrap, match-destructure, svardecl) currently derive the payload type from the Option
  NAME and there is NO GIR-level typed payload-field id to read back (pass-4 R-A). The reference-grade
  resolution is **Phase 2b**: a typed `option_ref_payload: Dict[int,int]` side table (Option type id
  → payload `GtPtr` id), populated at Option-of-ref registration, read at every read site — zero
  name parsing. The `Ref__`-string bridge is transitional only, marked for deletion. This is the
  "thread the typed id all the way through" path, now scoped as concrete net-new work rather than
  hand-waved as "harder."
- **D4 — Revert the shallow-copy attempt entirely and build fresh from committed baseline.**
  *Recommendation:* yes. The execution agent works in a worktree off committed `HEAD` (which does
  NOT contain the uncommitted shallow-copy edits), so it naturally starts clean. The diff is saved
  for reference only. (See §0 for the dirty-tree verification step.)
- **D5 — Do NOT migrate the for-element (`for x in coll`) path to `Option[Ref]` in this cluster
  (pass-2 RES-3).** It's independent (hardcodes value-payload `Option__T` + `borrow_only` elision)
  and proven. Keep the `borrow_only`/`is_deep_clone_fn` elision for it; add the `GtPtr` guard as a
  separate arm. Unifying for-element onto `Option[Ref]` is a clean follow-up, not a prerequisite.

---

## 5. Risks / blockers (reviewers: add any missed)

1. **C backend pointer-enum-payload support (Phase 0).** Review pass 1 verified the self-host's
   `lir_codegen.gg` DOES emit `void* Some_0` (`field_type_str`/`c_type_name` LT_PTR→void*,
   `~90/564`), `__field_read_` returns the pointer (`~3679`), and read-type inference yields the
   field's `LT_PTR` (`~2367`) — so Phase 0 should pass. Still run the spike to confirm end-to-end,
   but this is no longer the top risk.
1b. **TOP RISK — minting `GtPtr` at the reconstruction sites (Phase 3, RES-1/2).** The actual core
   of the change: unwrap + match-destructure currently produce `GtNamed("Ref__T")`, not `GtPtr(T)`,
   and `emit_payload_read_mode` would clone a (name-collapsed) `GtPtr` payload. These three edits
   are the load-bearing work; if any is missed, the bootstrap stays broken in the same way.
2. **Pass-3 / GIR-registration ordering.** The Option-of-ptr struct must be registered with its
   ptr payload *before* Pass 3 reads it. D3 (register from GIR id) mitigates.
3. **`pop`/`remove` and other Option producers** must keep value payloads — only `get`/`first`/
   `last`/`safe_get` flip to `Ref`. A blanket change would break ownership-transfer-out.
4. **Method dispatch on a `Ref[T]` receiver.** `coll.get(i).unwrap().method(args)` — does method
   resolution see through `GtPtr(T)` to dispatch `T`'s method? The EFieldAccess path auto-derefs;
   confirm the method-call path does too (it likely already does for field-loaded `GtPtr` locals).
5. **Interaction with `lowerer_comparison`.** Today the self-host emits `Option__T` (Ref stripped)
   where Rust emits `Option__Ref__T` — so this change moves MANY lines toward Rust parity at once
   (pass-2 RES-7). Expect a large comparison delta; verify it's uniformly toward-Rust, not a mix of
   improvements + new divergences.
6. **Field-write writeback Ptr-guard (pass-2 RES-6).** The EFieldAccess writeback for `t.field.push(x)`
   (`need_writeback`/`emit_field_write_from_local`, `lower.gg:~4498/4644/5712`) already skips when
   `recv_is_ptr_borrow` (`~4526`). Under `Option[Ref]`, `coll.get(i).unwrap().field.push(x)` makes the
   unwrap a `GtPtr`, so the existing Ptr-guard SHOULD cover it — but confirm `recv_is_ptr_borrow`
   triggers when the field base roots in a `GtPtr` chain (it keys on the receiver local's ownership +
   `is_ptr_type`).
7. **`&`-param RTRef collision — RESOLVED, non-issue (pass-3 R3).** Traced: `RTRef` is produced
   only by the `.get()` getter (`infer.gg:398,405`); `&`/`!` params never carry `RTRef` (separate
   `types.gg:254` path, sigils as ownership metadata) and their names/ABI form on a separate path
   (`lower.gg:~6906`). Un-stripping the `RTRef` arms is safe; no scoping guard required. (Kept here
   as a documented dead risk so a future reader doesn't re-raise it.)

---

## 6. Validation (parent drives integration; agent does build + unit)

Per CLAUDE.md multi-agent rules, the execution agent runs `cargo build` + `cargo test --lib` +
targeted checks only; the parent (this conversation) drives the full integration sweep and the
self-host bootstrap loop. Gates, in order:

1. `gg check` clean on the self-host driver.
2. Phase-0 spike: hand-crafted fixture emits + runs correctly under self-host codegen.
3. Stage-0 rebuild → stage-1 self-emit completes (no OOM — bug #3b stays fixed) → `cc -O0` →
   **stage-2 runs to completion** (no `int64_t`/`unknown field` crash) → **fixed point**
   (stage-1 emit ≡ stage-2 emit).
4. `cargo test --test integration --release self_host_bootstrap -- --test-threads=1` green.
5. `self_host_bootstrap_fixed_point` green.
6. `lowerer_comparison` green (parity held/improved).
7. `cargo test --lib --release` (~1059 baseline) + full `cargo test --test integration` (parent).
8. ASan pass on the stage-2 emit (no leaks/double-frees) — `Option[Ref[T]]`/unwrapped `Ref` must
   never be dropped; owned-bind clones must be.

Validation cycle ≈ 25-30 min (≈9 min stage-0 build + ≈10 min emit + cc + ≈10 min stage-2 run).
Always `stdbuf -oL` on `--emit-c`; ≥2400s timeouts.

---

## 7. Phasing into commits

Each phase should leave the tree `gg check`-clean; the *bootstrap* may stay red until the cluster
lands (ship as one squashed commit at the end, per the drop-emission ship discipline — partial
states crash). Suggested commit boundaries: Phase 0 (spike, possibly throwaway) · Phase 1+2
(get→Option[Ref], struct-gen) · Phase 3 (unwrap→Ref) · Phase 4 (owned-bind clone + retire
materialize dodge) · Phase 5 (plumbing cleanup) · final (squash + ship-gate).

---

## 8. Open questions for review

- Is there a self-host site that does `coll.push(y)` while holding a `coll.get(i)` ref (the
  use-after-realloc the spec forbids but we don't check)? If so it must be fixed in source first.
- Does `Dict.get(k)` (str-keyed) return a value pointer with the same lifetime semantics as
  `Vector.get(i)`? (Runtime: `gorget_map_get` returns `m->values + idx*val_size` — inline, yes.)
- Should `v[i]` index-read share the exact same `Option[Ref]`-less path (it returns a bare ref, not
  an Option)? Confirm `lower_index_assign`/index-read already use the pointer path and don't regress.

---

## Phase 6 — R1 fix: deref `GtPtr(primitive)` payloads at value-consuming positions

**Found by post-implementation review (2026-05-26).** Phases 1-5 left a hole: `int key =
vec.get(i).unwrap()` (the most common getter shape — `Vector[int]`/`Vector[bool]`, hundreds of
sites: `lir_ssa.gg:66/486/492/537`, `driver.gg:93/97`, …). `.get()` types as `Option[Ref[int]]`,
unwrap yields `GtPtr(int)`, and value-binding it into an `int` slot `OpCopy`s the 8-byte POINTER,
not `*ptr` → `key` holds the element's address → garbage downstream. `gg check` is clean (types
line up) but stage-2 would miscompile.

**Reference (Rust `methods.rs:610-731`):** unwrap of `Option__Ref__T` returns `Ptr(T)` UNIFORMLY
(even primitives); the `inner_is_resource` gate (`:647`) only chooses Move-vs-Copy of the borrow
operand for drop-tracking. The deref of a `Ptr(primitive)` happens at the CONSUMER (var-decl copies
through the pointer). The self-host's `decide_ptr_consume` instead `OpCopy`s the pointer bits — the
bug.

**Rule (gate on `is_resource(pointee)`):** a `GtPtr(inner)` extracted from an `Option[Ref[inner]]`
is, at a VALUE-consuming position:
- **inner is a resource** (String/Vector/Dict/user struct-or-enum-with-resource) → keep the
  `GtPtr` borrow (current Phase 3/4 behavior: field-access auto-derefs; owned-bind deref+clones via
  Branch C-pre; consume clones the pointee). NO CHANGE.
- **inner is Copy/primitive** (int/float/bool/byte/…/POD value struct) → **deref-load** `*ptr` into
  a value-typed local. A primitive borrow is semantically pointless (book ch11: Copy types copy);
  copying the pointer is the bug.

**Sites (apply the SAME gate consistently — `is_resource_type_name(pointee, &gmod)`):**
1. **`emit_payload_read_mode` GtPtr guard (`lower.gg:~6113`).** Currently `case GtPtr(_): return dst`
   (returns the pointer) for ALL pointees. Split: resource pointee → `return dst` (pointer);
   non-resource pointee → emit a deref load (`GIDeref`/`ILoad` of `*dst` into a fresh value-typed
   local of the pointee type, `LoOwned`) and return THAT. This is the single load-bearing fix — all
   value-consuming paths (svardecl bind, call-arg, unwrap_or merge, match-destructure) route their
   payload through here, so derefing primitives here fixes them at once. Keep passing `dst_type =
   GtPtr(inner)` so the guard still sees the pointer source.
2. **`infer_method_return_type` unwrap arm (`lower.gg:~3406`) — REFINEMENT, not the load-bearing fix
   (review pass 2).** The plain-unwrap *lowering* (`lower.gg:~4500-4549`) does NOT consult this
   function — it returns the value local directly and `decide_svardecl_emission` reads the local's
   real type. Gate it anyway (resource → `GtPtr`; primitive → value pointee type) so type-inference
   consumers (`auto x = …unwrap()` via fallback) agree with the SSA — but know the unwrap fix is #1,
   not this.
3. **match-destructure (`lower_ctor_pattern`, `lower.gg:~6376/6392/6394`) — CORRECTED (review pass 2,
   B2).** `fty` is used for BOTH the `emit_payload_read_mode` `dst_type` arg AND the bound local's
   type, so it can't be gated one way for both. Required shape: KEEP `fty = scr_ref_payload`
   (`GtPtr`) as the `dst_type` passed into `emit_payload_read_mode` (so the #1 guard fires and
   derefs primitives), but type the BOUND local from the RETURNED `field_val`'s actual type
   (`ctx.locals.get(field_val).type_id`), NOT `fty` — i.e. `add_local_inheriting(&ctx,
   ctx.locals.get(field_val).type_id, name, field_val)`. Then resource → `bound: GtPtr`, primitive →
   `bound: value`, matching what #1 returns. NOTE: no primitive-pointee `match coll.get(i): case
   Some(p):` site exists in the current self-host (all `match …get` chain `.unwrap()` first), so this
   primitive case is NOT exercised by the bootstrap — but implement it correctly anyway (don't ship
   an unsound instruction); the resource-match case IS exercised and keeps `bound: GtPtr`.
4. **`unwrap_or` (`lower.gg:~4509-4532`) — REQUIRED EDIT, exercised (review pass 2 B1, mechanism
   CORRECTED review pass 3).** NOT mere sanity: `sr.get(name).unwrap_or(-1)` (`sr: Dict[String,int]`)
   is load-bearing throughout `lir_lower.gg` (StructRegistry id lookups: `:528,536,543,580,595,665,696,708…`).
   ⚠ **Do NOT gate the shared `inner_tid`** (declared `lower.gg:4468`, set `=ref_payload_tid` at
   `4500-4501`). It is the SAME variable used at the field-read calls (`emit_payload_read` at `~4524`
   unwrap_or-Some AND `~4535` plain-unwrap) — those MUST keep `dst_type = GtPtr(inner)` so the site-#1
   guard fires and emits the `GIDeref`. Gating `inner_tid` would type the field-read dst as a value
   slot (the helper writes the pointer into it → bug reintroduced) AND stop the guard from matching →
   no deref → breaks the dominant plain-unwrap path too. FIX: keep `inner_tid = GtPtr` unconditionally;
   introduce a SEPARATE slot-type variable for `uo_dst` only — at `~4513`, `int uo_slot_tid =
   primitive_pointee ? value_pointee_tid : inner_tid` (resource → `GtPtr`; primitive → value pointee
   type), `add_local(&ctx, uo_slot_tid, …)`. Then the Some arm stores the site-#1-deref'd value
   (`~4525`) and the None arm the value default `-1` (`~4529`) into a value-typed `uo_dst`. (Pointee
   name: `type_id_to_name(ref_payload_tid)` already collapses `GtPtr(inner)`→inner name,
   `lower.gg:~2935`; value pointee tid: destructure `GtPtr(inner)` off `gmod.type_table`.)
5. **PLiteral sub-pattern (`lower.gg:~6400-6416`) — refinement, unexercised (review pass 2).** The
   `case Some(0):`-style literal path computes `lit_fty` via `lookup_ctor_field_type`→the `Ref__`
   bridge→`GtPtr`, would deref via site #1, but `GICmp` at `~6413` still passes `lit_fty`(=GtPtr) as
   the operand type. No primitive-pointee literal-match on a getter exists today (all chain
   `.unwrap()` first). Gate it for completeness OR leave a `# TODO` + fixture per "don't redesign
   around gaps"; not a blocker.

**R2 follow-up (refinement, verify-after-green, not a blocker):** the return-type priority
(`lower.gg:~4732`) is `fn_sigs` → typechecker side-table (populates `option_ref_payload`) →
`infer_method_return_type` (populates). If a collection getter ever resolves via `fn_sigs`, neither
populate site runs and the channel is empty. Not exercised today (getters are runtime methods, not
in `fn_sigs`). After the bootstrap is green, add an assert/guard or confirm no getter resolves via
`fn_sigs`.

**Validation:** `gg check` clean → then the parent's stage-0 → stage-1 emit → cc → **stage-2 runs +
fixed point** (this is the gate R1 specifically threatens) → bootstrap/comparison → ASan.
