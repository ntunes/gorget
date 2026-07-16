# Executor brief: CoW Track 1B — self-host value index-element field write-through

> **Status:** v3 — ✅ PASS 4 SIGNED OFF CLEAN (2026-07-16; gauntlet 5→4→2→0) — **EXECUTING**.
> Pass-4 confirmed the correctness crux at the LIR level (GtPtr dst skips the aggregate
> copy-out; plain ISlotStore of the raw pointer) and that BOTH compound legs (read + write-back)
> route through the new arm. Pass-3 folded (2 reservations: the ELEMENT-KIND mandate — dst is
> `GtPtr(elem)`+`LoBorrowed` for BOTH value-struct and resource elements, do NOT replicate the
> read path's 3-way split whose struct→by-value `else` arm IS the bug; the Out-of-scope
> bootstrap contradiction reconciled + chunk-verbiage dropped for the monolithic test).
> Pass-2 folded: `collection_kind` gate mandate on the shared resolver; read/receiver blast +
> executor-run bootstrap; statics via the `lower_index_assign` idiom; ggdef local/static split.
> Pass-1 folded: fixture scoped; `gorget_array_get_ptr`/untrack framing corrected; statics in
> scope; lint deferral explicit. Awaiting the next fresh pass (pass-3 verified all other axes
> clean, incl. CkVector/CkDeque completeness, the OpCopy-if-ptr idiom for both base kinds, and
> the fixture's expected values against the inline test). **Campaign:** `cow-writethrough-materialize-closed-set.md` (v3) — this is the
> campaign's designated first PR. **Wave-0 basis:** `cow-wave0-measure.md` — gap B re-verified
> broken this session (self-host only; Rust GIR is correct).
> **Model policy:** executor + brief-reviews Opus; output-review before integration on Fable.

## Objective

`v[0].x = 88` on a Vector element (value struct) SILENTLY LOSES the write in the self-host
lowerer: the driver reads the element via `gorget_array_get` (a VALUE COPY), writes the field
into the copy, and the store dies — reading back gives the stale value. Rust gg is correct
(R39-T1: element Ptr + deref-store). Fix the self-host to match, and promote the Rust-only
inline regression to a three-lane corpus fixture.

Spec: `docs/language-design.md` §3.1 — an owned place (`v[i].f`) is an unbroken chain to the
owner → **write-through**. Expected output derivation per the campaign's principle 1: adjudicate
the new fixture with **ggdef first** (if the vector-index field-store shape is in ggdef's
subset, the ggdef verdict IS the expected stdout; if out-of-subset, note that in the fixture
comment and derive from §3.1 — never from what a compiler happens to print).

## Root cause (scout-verified 2026-07-16 + plan's code map)

`lower_place_base` (`tests/fixtures/self_host_lowerer/lower_stmt.gg:1514`) special-cases only
Identifier/static/Deref bases; an **Index** base falls through to `lower_expr` → value copy →
the subsequent field write targets the copy. Wave-0 measured: driver emits `gorget_array_get`
where Rust GIR emits the `get_ptr` + deref-store shape.

## Fix shape (mirror Rust R39-T1 — fix the CLASS at the place resolver)

**Mechanism (pass-1-corrected — the campaign plan's `gorget_array_get_ptr` is an ASPIRATIONAL
name from a comment at `src/ir/lowering/exprs/mod.rs:2543`; no such symbol exists anywhere):**
Rust's Index arm (`exprs/mod.rs:2553-2595`) emits `Inst::ElemPtr` (inline `base + idx*elem_size`).
The self-host building block is `gorget_array_get` (returns a pointer INTO the buffer for ALL
element types — `runtime_array.c:31-36`) with the dst typed **`GtPtr(elem)`** and tagged
**`LoBorrowed`** — the shape the resource-element READ arm uses at `lower_expr.gg:4971-4973`.

**⚠ ELEMENT-KIND MANDATE (pass-3, the subtle one): the new EIndex arm types the dst
`GtPtr(elem)` + `LoBorrowed` for BOTH value-struct AND resource elements — do NOT replicate the
read path's scalar/resource/struct 3-way split.** The read path at `lower_expr.gg:4969-4975`
gives `GtPtr` ONLY to the `eix_is_resource` arm; a plain value struct like `Point` takes the
`:4975` `else` arm — a BARE-typed dst, which `lir_lower.gg:~4616-4634` turns into the
`*(StructT*)` aggregate COPY-OUT. That value-copy arm IS this bug (`Vector[Point]` is exactly
the fixture's element). The dst slot TYPE is what decides pointer-vs-copy at the C level
(`GtPtr` dst → plain `ISlotStore` of the raw pointer; bare aggregate dst → copy-out). Rust's
own comment says it outright (`exprs/mod.rs:2540-2543`): "lower_index_access returns a value
COPY for VALUE-type elements … **Force the element Ptr(T) here for BOTH element kinds**."
Scalar elements are not this arm's business (a field store needs a struct-ish element; scalar
receivers keep the existing fall-through — Copy semantics are correct for them). There is NO self-host
untrack pass to call (Rust's `untrack_transient_element_refs_in_range`, `context.rs:2543`, has
no twin) — the `LoBorrowed` tag IS the untrack-equivalent: it keeps the element Ptr out of
drop-tracking. **The real risk is mis-tagging the dst `LoOwned` → the buffer element gets
drop-registered → double-free — this is what the ASan gate is for.**

**⚠ MANDATE (pass-2, HIGH): the EIndex arm MUST gate on the collection kind and PRESERVE the
`lower_expr` fall-through for every non-array base.** `lower_place_base` is SHARED — it is
called by the field-READ arm (`lower_expr.gg:4582`), the method-RECEIVER arm (`:3289`), and the
hasher arm (`:3932`) for `x[i].field`/`x[i].method()` on ALL collection kinds. A Dict `d[k].f`
read works today precisely BECAUSE it falls through to `lower_expr` → `gorget_map_get`. An
unconditional `gorget_array_get` in the new arm would call an ARRAY getter on a MAP →
UB/crash, regressing working Dict/Set reads and colliding with Track 1C. Dispatch exactly as
the existing read/index-assign paths do — via `resource_meta_for(...).collection_kind`
(see the getter dispatch at `lower_expr.gg:~4840-4920` and `lower_index_assign`,
`lower_stmt.gg:1818-1846`): take the pointer path for **CkVector/CkDeque array-kind** bases
ONLY; everything else (Dict/Set/user-Index) falls through unchanged.

**Blast radius is WIDER than stores — this is intended, verify it (pass-2):** the new arm
reroutes index-element field-READS and method-RECEIVERS (incl. `m[0].push()`, the nested-Vector
mutation shape) from value-copy to element-pointer. That is the one-resolver design the
campaign wants, and the pointer-base read path (`lower_expr.gg:4587-4725`) already handles
`GtPtr` bases (it's the `&self`/`&param` path). Executor duties: (a) verify index-element reads
and method-receiver shapes still pass (targeted probes); (b) an emitted-GIR-shape change on
reads is EXPECTED, not a bug — note it, don't fight it; (c) because the fixture alone has no
read/receiver-only coverage, run `self_host_bootstrap_fixed_point` YOURSELF, FOREGROUND, with
`GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600` (a single monolithic test — no chunking;
it self-exercises every read shape) — this track is an exception to the
bootstrap-is-parent's-job default; the parent still runs the full sweeps.
(d) If nested-Vector `get→push→set` shapes start passing, REPORT it — it may retire a filed
workaround (memory: nested-vector get-push-set) — do not silently absorb it.

**Static bases are in scope — use the SELF-HOST's own proven idiom (pass-2 corrects the pass-1
Rust framing):** do NOT mirror Rust's GlobalRef materialization. The self-host already writes
static array elements correctly via `lower_index_assign` (`lower_stmt.gg:1769-1868`): it lowers
the collection base via **`lower_place_base` itself** (`:1770`), which returns a `GtMutPtr` for
statics through `__global_ref__` (`:1519-1529`), then consumes it with the `is_ptr_type(base) →
OpCopy` idiom (`:1857-1859`). The new EIndex arm must lower ITS collection base the same way —
recursively via `lower_place_base`, NEVER via `lower_expr` (a static through `lower_expr`
yields a value slot whose address-of is a pointer-to-pointer). Proof this works:
`static_vector_index_store.gg` / `static_vector_index_compound.gg` pass today. Verify
`PTS[0].x = 99` / `PTS[1].x += 100` end-to-end on the driver; if statics still turn out
structurally larger (report the evidence), scope the fixture to locals and file the static
case — do not silently drop it.

Do it in the shared place-base path (`lower_place_base`), NOT per-statement-kind — Track 1C
(Dict/Set) and 2F (nested `&`) will extend the same resolver; leave the EIndex arm shaped so a
Map/Set kind can be added without copying the logic (Core #4). The place-resolver
`CollectionKind`-exhaustiveness arm-count lint is DEFERRED to campaign Wave 3 by plan — noted
here so the class-guard is visibly tracked, not forgotten.

Primary file: `tests/fixtures/self_host_lowerer/lower_stmt.gg` (place base + field write,
~1514–1638; the EIndex fall-through to `lower_expr` is at `:1550-1552`). Keep the fix in
`lower_stmt.gg` where feasible; if you must touch `lower_expr.gg` (shared with future tracks
1A-SH/2D/2E), FLAG it in your report for the parent's integration sweep. Do NOT touch
`lower_loops.gg` (Track 1A's zone) or the typechecker dir (two other tracks are active there —
coarse-kind split in `typecheck.gg`, tuple-DefId in `parser.gg`/`resolve.gg`; stay out).

## Milestones

1. **M1 — the fix** in `lower_place_base` (+ helpers). Checkpoint
   `git diff > /tmp/recover_cow1b_m1.patch`.
2. **M2 — corpus fixture promotion, SCOPED (pass-1: promoting the FULL inline-test body would
   MISMATCH the self-host lane and RAISE the parity WRONG count — the opposite of the campaign
   goal):** create `tests/fixtures/cow_value_index_field_writethrough.gg` (+ `.expected`)
   containing ONLY the M1-covered shapes from the inline Rust test
   `rust_value_index_element_field_writethrough` (`tests/integration.rs:21130`, body at
   `:21169-21197`): the plain (`v[0].x = 88`, `PTS[0].x = 99`) and compound (`v[1].y += 5`,
   `PTS[1].x += 100`) single-level index-element field stores. The NESTED (`ns[0].inner.val = 99`)
   and value-field-METHOD-receiver (`hs[0].c.bump()`) shapes route through
   `lower_place_base(EFieldAccess-base)` — NOT fixed by the EIndex arm — so they STAY in the
   inline Rust test: **KEEP the inline twin** (its comment already documents why it's not
   corpus), and file the two residual shapes as the follow-up they are (they belong to the
   nested-place class, Track 2F's neighborhood). **ggdef adjudication is SPLIT (pass-2): the
   LOCAL shapes (`v[0].x = 88`, `v[1].y += 5`) are IN-subset** (`navigate_write` handles
   Vector-Index + Struct-Field, `spec/ggdef/src/eval.rs:931-949`) — **adjudicate them with a
   statics-stripped ggdef probe** (expected write-through: `88`, `45`); **the STATIC lines are
   OUT-of-subset** (`Item::StaticDecl` is not elaborated, `elaborate/mod.rs:62-119`) — derive
   them from §3.1 prose with an explicit out-of-subset note in the fixture comment. Do NOT run
   the mixed fixture through ggdef and conclude the shape is out-of-subset — only the statics
   are. Wire the standard three-lane integration tests (C + LLVM; the self-host lane
   auto-enrolls via the runtime-parity corpus). Verify new files are not gitignore-hidden
   (`git status` must show them).
3. **M3 — negative-space probes** (cheap, in-worktree, not committed unless they find something):
   multilevel `v[i].inner.f = x` (expected: still broken — confirm + report, it's 2F's class),
   `v[i].f.push(y)` (projected chain through the element), compound `v[i].f += 1` — confirm
   write-through lands or report the residual shape (report, don't scope-creep the fix).
4. **M4 — gates (FOREGROUND, generous timeouts; chunk >600s gates by test name)**:
   rebuild the self-host lowerer driver (`GG_BUILD_TIMEOUT_SECS=600`) · the new fixture green on
   all three lanes · `self_host_runtime` targeted run · read/receiver probes per the blast-radius
   duties (Dict `d[k].f` read still works; `v[i].m()` receivers; `m[0].push()`) ·
   **`self_host_bootstrap_fixed_point` FOREGROUND** (this track's explicit exception — see
   blast-radius section) · **ASan** on the new fixture + the existing multilevel `cow_index_*`
   fixtures (the #1 risk is mis-tagging the element-Ptr dst `LoOwned` → drop-registered buffer
   element → DOUBLE-FREE; a leak-check needs a positive control before trusting "clean") ·
   `cargo test --lib` · `lower_comparison`/`type_comparison` diagnostic runs (print the counts) ·
   targeted `cow_*` + `static_vector_index_*` integration filters.

## Out of scope

- Dict/Set element field stores (Track 1C), `for x in &coll` (Track 1A), nested-`&` (2F),
  plain-`self` (2E). If your fix naturally generalizes, leave the seam clean and NOTE it.
- Any Rust-side lowering change (Rust is correct here).
- The full C/LLVM sweeps + parity regen: the PARENT runs these (combined with the other
  in-flight tracks). **EXCEPTION carved out for THIS track (pass-2/3): you DO run
  `self_host_bootstrap_fixed_point` yourself** — it is the only coverage for the read/receiver
  reroute (see blast-radius section). It is a SINGLE monolithic test (stage-2==3==4): run it
  FOREGROUND with `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600` — no chunking applies.
  Everything else beyond build + targeted + lib + that one bootstrap test stays the parent's.

## Process contract (non-negotiable)

Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree.
NEVER touch `/workspace/gorget` (main) or `/workspace/gorget-1`; worktree-relative paths only
(your worktree nests UNDER the main checkout — an absolute `/workspace/gorget/...` path writes
into MAIN). NEVER `git stash`; checkpoint to /tmp after every milestone. Stage by EXPLICIT file
name only. On an Edit-tool desync, re-Read and retry the Edit tool — never a shell heredoc with
an absolute path. Commit when green (`fix(self-host): ...`). Report any NEW pre-existing bug
(file:line + repro) — file-don't-fix.

## Acceptance

`v[0].x = 88; print(v[0].x)` prints `88` through the self-host lowerer; the promoted fixture is
green on C + LLVM + self-host runtime lanes with a ggdef-adjudicated (or explicitly
prose-derived) expected file; ASan clean on the new + multilevel index fixtures; lib green;
comparisons not regressed; zero changes outside the lowerer dir + the fixture/test wiring.
