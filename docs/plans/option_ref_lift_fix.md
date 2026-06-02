# Brief — self-host `Option[Ref[T]] → Option[T]` lift conversion (heap_basic peek residual)

FIDELITY round (1:1:1:1 cadence). Self-host-side only (Rust gg already correct).
Re-verified by RUNNING the code at `a6c161fb` (not source-reading). ⚠ Needs ≥3 fresh
sequential reviews before the executor launches.

## Bug (re-verified by emit-C + cc + run + diff vs Rust)
`heap_basic` line 1 = `0` (self-host) vs `5` (Rust); everything else MATCHes. The
culprit is `Heap.peek()` (`lib/std/heap.gg:33-36`):
```
Option[T] peek(self):
    if self.heap_data.len() == 0:
        return None
    return self.heap_data.get(0)     # <- Option[Ref[T]] returned where Option[T] expected
```
`.get()` returns `Option[Ref[int]]` (a borrow); the fn returns `Option[int]` (value),
so the compiler must LIFT: deref the Ref and rebuild an owned `Option[int]`. The
self-host BUILDS the `Option[Ref[int]]` correctly (emitted C `peek` bb3-6: `__s9` =
`Option__Ref__int64_t{tag, Some_0=void*}`), then in the merge block **discards it and
hardcodes None**:
```c
__bb6:
    __v27 = __s9;                          // the built Option[Ref[int]] — DISCARDED
    __s0 = (Option__int64_t){.tag = 1};    // <- BUG: hardcoded None (lir_codegen.gg:2860/2909 default)
    return __v28;                          // returns None → unwrap() reads zeroed payload → prints 0
```
(`pop()` works — it does `.get(0).unwrap()`, extracting immediately; the gap is ONLY
the bare `return coll.get(i)` where the fn returns `Option[T]` by value.)

## Root cause
The self-host has **no** `Option[Ref[T]] → Option[T]` lift conversion. When `SReturn`
assigns the `Option[Ref[int]]` value into the `Option[int]` return slot (local 0), the
types mismatch and the C backend falls back to the Option-field None-default
(`lir_codegen.gg:2848-2860` / `:2909`, "Option fields: emit `{.tag = 1}`"). Per the
layering discipline (downstream defaulting on a type mismatch = the lowering upstream
dropped the conversion), the fix belongs in the **lowering**, not the codegen.

## Rust reference (the exact behavior to mirror)
`try_lift_option_ref` — `src/ir/lowering/stmts/mod.rs:2934-3065`. Called at:
- **return site** `:1755-1767` (`if let Some(converted) = try_lift_option_ref(ctx, builder, &operand, src_ty, ret_type, …)`),
- **var-decl/assign site** `:541-555` (same helper).

The helper (read it in full — this is the spec):
1. Operand must be a bare Copy/Move of a whole local (no projections); src type name
   starts with `Option__Ref__`, dst name starts with `Option__` but NOT `Option__Ref__`.
   Otherwise return None (caller passes the operand through untouched).
2. Extract inner type from `Option__Ref__<inner>` (`resolve_mangled_type`).
3. Branch on the tag (field 0): `tag == 0` → Some, else → None.
4. **Some**: extract the Ref payload as a `Ptr(inner)` local (assigning the whole
   `Option[Ref]` source to a `Ptr(inner)` local triggers enum-payload-extract →
   FieldPtr+Load). Then: resource pointee → `clone_fn(ptr)` → owned T; non-resource
   pointee (primitives/value structs, e.g. `int`) → `*ptr` (deref). Then
   `enum_init(dst, "Some", [owned_payload])` → merge slot.
5. **None**: `enum_init(dst, "None", [])` → merge slot.
6. Merge block → return `Copy(merge)`. (Move-into-merge when dst is a resource type;
   Copy otherwise.)

## Fix (self-host) — port + wire at SReturn + RETIRE the workaround it enabled
**Edit 1 — port `try_lift_option_ref` into `lower.gg`.** Mirror the Rust helper. REUSE
the self-host's existing primitives (do NOT reinvent):
- **Detect the shape + inner type via the typed channel:** `option_ref_payload_of(&gmod,
  tid)` (defined `gir.gg:524`; a side-channel `Dict` lookup — returns the `GtPtr(inner)`
  payload id `>= 0` ONLY for Option tids RECORDED by a borrowing-getter lowering,
  `record_option_ref_payload` at `lower.gg:2923-2930`/`:3782-3793`). This recorded-ness
  is what makes the gate SOUND (no structural `Ref__`-name re-parse, no false-positive on
  a hand-built `Option__Ref__T` that bypassed a getter; CLAUDE.md "No name matching").
  Use it to gate (`>= 0`) AND to get the inner `Ptr(inner)` type. The dst (return-place)
  must be `Option__T` (name starts `Option__`, NOT `Option__Ref__`) — mirror Rust
  `:2966-2968`.
- **Enum construction is `GICallExtern(dst, "Some"/"None", args)`, NOT `IEnumInit`** —
  the self-host GIR has no `IEnumInit` instruction; bare `Some`/`None` ctor calls route
  through the LIR `try_lower_prelude_variant` rewrite one layer down (template: the
  throws-Ok-wrap at `lower.gg:6283`, the bare `None` at `:4266`/`:5348`, the
  `unwrap_or` builder at `:4894-4912`).
- **Structure the tag-branch on the EXISTING `unwrap_or` template (`lower.gg:4894-4912`)
  — do NOT invent an extract-then-GIDeref split (the self-host has no Rust-style
  `try_enum_payload_extract`; the only helper is `emit_payload_read`, which ALREADY
  applies its own `GtPtr` deref/keep guard).** Mirror the template exactly:
  - `scr_ptr = match_scrutinee_ptr(&ctx, &gmod, val)` (how the unwrap path gets the
    Option scrutinee pointer); `merge = add_local(&ctx, dst_type, NO_NAME)` (dst_type =
    the `Option__T` return-place type); `tag = emit_tag_read(&ctx, scr_ptr, src_name,
    &gmod)`; `GICmp(cmp, CMP_EQ, I64_TYPE, OpCopy(tag), OpConstI64(0))`; `GTBranch(cmp,
    some_bb, none_bb)` with a `merge_bb`.
  - **Some arm — delegate clone-vs-copy to `op_consume` (ONE source of truth; mirror the
    template at `:4904-4905` exactly — do NOT re-gate).** `payload = emit_payload_read(&ctx,
    scr_ptr, src_name, "Some", 0, ptr_tid, &gmod)` where **`ptr_tid` is the `GtPtr(inner)`
    payload tid from `option_ref_payload_of` — NOT the value `inner` tid** (only the
    `GtPtr` tid makes `emit_payload_read`'s guard fire: `lower.gg:6965-6968` → resource
    pointee returns the bare ptr, primitive pointee `GIDeref`s to a value). Then build the
    Some Option and let `op_consume` decide the clone: `GICallExtern(some_res, "Some",
    [op_consume(&ctx, &gmod, payload, CkCallArgOwning())])`. `op_consume` routes a
    `GtPtr(resource)` payload → `OpClone` (deep clone) and a primitive value → `OpCopy`
    (`decide_ptr_consume`, `lower.gg:~1434`) — the SAME `is_resource` decision
    `emit_payload_read` used, so there is no second predicate to keep in sync. ⚠ Do NOT add
    a manual `resolve_payload_clone_fn` gate + `GICallExtern(clone_fn, [OpBorrow])` (it
    works for the common case but re-derives the resource/primitive split via a predicate
    that can desync from `emit_payload_read`'s internal `is_resource_type_name` for a
    resource-with-drop-but-no-clone pointee → corruption; CLAUDE.md "one source of truth").
    And do NOT add a 2nd `GIDeref` (double-derefs `int` → garbage; `emit_payload_read`
    already deref'd the primitive).
  - **None arm:** `GICallExtern(none_res, "None", [])`.
  - **⚠ Store each arm's result into `merge` via `op_consume(&ctx, &gmod, <res>,
    CkAssign())` (as the template does at `:4905`/`:4909`) — this AUTO-SELECTS Move for an
    owned resource Option, mirroring Rust's `assign_mode(Move) when is_resource_type(dst)`
    (`stmts/mod.rs:3038-3046`). A plain `GIAssign(merge, OpCopy(res))` shallow-aliases the
    freshly-built resource Option → double-free; heap_basic (`Option__int64_t`,
    non-resource) would mask this, but Edit 3's `Option__ResourceMetadata` (resource)
    detonates step 4.**
  - `merge_bb`: the lift's result is `merge`.

**Edit 2 — call the lift at the `SReturn` site.** Inject AFTER the throws-Ok-wrap (ends
`lower.gg:6284`) and BEFORE the `op_consume` at `lower.gg:6285` (NOT before the
`GIAssign(0)` at `:6298` — `op_consume` already consumes `val` at 6285). **Reassign
`val`** to the lifted local so the existing `op_consume(val, CkReturn())` + `GIAssign(0,
ret_op)` + move-zero/excl logic (`:6285-6301`) all see the corrected `Option__T` value.
Gate: return place (`ctx.locals.get(0)`) type name starts `Option__` (non-Ref) AND
`option_ref_payload_of(val_type) >= 0`. ⚠ **No-op fallback (mirror Rust):** when the
shapes don't match, leave `val` untouched — do NOT regress `return Some(x)` / `return x`
/ `return coll.pop()` (owned `Option[T]`) / nested-Option returns.

**Edit 3 — RETIRE the workaround this fix exists to kill** (CLAUDE.md "a fix is
incomplete until the dodge it enabled in self-host is gone"). `resource_meta_for`
(`lir_lower.gg:416-426`) currently returns `Some(gmod.resource_metadata.get(name)
.unwrap().clone())` with a comment citing exactly this bug ("the self-host lowerer leaves
the Ref payload un-deref'd at the return → callers see None → 1238 FATAL
validate_resource_moves violations. See TODO.md 'Option[Ref] return auto-deref'"). After
Edit 1+2, rewrite ONLY line 426 (the `if …contains(name):` branch) to the idiomatic
`return gmod.resource_metadata.get(name)` — **leave the lazy-populate path `:427-434`
untouched** (it already returns plain non-Ref `Option`s, unaffected by the lift). The lift
now does the deref+clone — `ResourceMetadata` is a RESOURCE type, so this exercises the
lift's CLONE branch in the bootstrap (a stronger test than heap_basic's primitive-deref
branch; behaviorally identical — both yield an owned clone). Move the `TODO.md`
"Option[Ref] return auto-deref" item to `DONE.md`.

⚠ **SEQUENCE (de-risk — retiring the workaround makes the bootstrap DEPEND on the lift):**
(1) land Edit 1+2; (2) confirm heap_basic MATCH + `fixed_point` GREEN **with the
workaround still present** (pure regression — proves the lift didn't break anything and
the no-op fallback is sound); (3) THEN apply Edit 3; (4) re-confirm `fixed_point` GREEN
(now the driver's own `resource_meta_for` exercises the lift's resource-clone branch — the
real validation). If step 4 detonates (bootstrap FATAL), the lift's resource-clone branch
has a bug — fix it, do NOT revert to the workaround.

NOT NEEDED for heap_basic: the var-decl / SAssign call site (Rust `:555`) — `peek()`'s
only lift site is the bare `return`. Log it as an optional mirror-completeness follow-up.

## Scope / expected outcome
heap_basic: WRONG-OUTPUT → MATCH (**+1 parity → 247**). The TODO notes this is a GENERAL
gap ("likely affects other `return coll.get(i)` patterns") — re-measure the full
diagnostic; the fix may MATCH additional fixtures (report the delta). Do NOT reshape
`lib/std/heap.gg` or the fixture to dodge it.

## Gate (self-host-dir only — no `src/`, but verify broadly; follow the SEQUENCE above)
1. `cargo build` + `cargo test --lib` green.
2. **After Edit 1+2 (workaround STILL present):** force-rebuild the self-host driver;
   emit-C `heap_basic` → the `peek` merge block emits the deref+rebuild (NOT `{.tag =
   1}`) → cc → run → stdout MATCHes `gg run` (line 1 = `5`). Then `self_host_runtime`
   lock-in ≥246/0 + `self_host_bootstrap_fixed_point` GREEN (REGRESSION check — the lift
   is dormant in driver code here, so this proves the no-op fallback + the new code path
   don't break the driver).
3. **After Edit 3 (workaround retired):** force-rebuild the driver; re-run
   `self_host_bootstrap_fixed_point` — MUST be GREEN. This is the LOAD-BEARING gate:
   `resource_meta_for` now returns a bare `.get()`, so the driver compiling ITSELF
   exercises the lift's resource-clone branch. A wrong lift → bootstrap FATAL (the TODO
   cites 1238 validate_resource_moves violations). Fix the lift; never revert to the dodge.
4. **FULL `cargo test --test integration -- --test-threads=4`** — `lowerer_comparison` /
   `c_emit_comparison` unchanged-or-better, all sync/heap/collection fixtures green.
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → re-measure parity (expect ≥247; 246
   `.out` committed now); report which fixtures moved.
6. Re-seed snapshots additively (`GG_REGEN_RUNTIME_SNAPSHOT=1`), confirm only NEW `.out`
   files (zero existing modified).

## Files (stage by name only — never `-a`)
`tests/fixtures/self_host_lowerer/lower.gg` (Edit 1 port + Edit 2 SReturn call),
`tests/fixtures/self_host_lowerer/lir_lower.gg` (Edit 3 workaround retirement),
`TODO.md` + `DONE.md` (move the "Option[Ref] return auto-deref" item). NO `src/`.
Reuse existing GIR primitives (`GICallExtern("Some"/"None")`, `emit_payload_read`,
`GTBranch`) — `lir_codegen.gg` should NOT need changes (flag if it does).
