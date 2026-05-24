# Drop Emission Completion Plan

**Status:** v3.1 — this is the STRATEGIC plan + historical record. **The LIVE next-session
execution plan is [`drop_emission_next_session.md`](drop_emission_next_session.md)** (written
2026-05-24 after implementation began). Empirical work since v3 corrected two of v3's own claims
(Phase C.1 and the reorder) — see the v3.1 note in "Revision history" and the running log in
[`consumer_audit.md`](consumer_audit.md). Use those two docs for current state; this file for
the phase taxonomy + rationale.
**Goal:** Ship full architectural drop emission to self-host so `self_host_bootstrap` + `self_host_bootstrap_fixed_point` pass with real drop emission, NOT labels-only.
**Estimated effort:** 15-40 hours over 2-4 sessions.

> **What's TRUE as of v3.1 (2026-05-24, after implementation began — supersedes the v3 TL;DR):**
> 1. **Phase D — CONFIRMED + SHIPPED (WIP).** Emit `GIDropIfAlive` **unconditionally**, no
>    `maybe_moved` gate, no drop queue — emit directly at scope-pop. Un-disabled this session;
>    drops fire correctly (string/array/map free climbing to Rust parity).
> 2. **Phase C.3 (cluster b) — SHIPPED** (`d2efd716`): one allocator + post-construction
>    fn-ptr stores driven by `drop_strategy`/`recursive_drop_*`, generalizing
>    `emit_dict_ctor_wiring`. **NOTE:** C.3/(b) is correct but does NOT reduce the OOM on its
>    own (it's inert until cluster (a) emits the frees that invoke elem_drop — see #4).
> 3. **Phase C.1 — v3 WAS WRONG; the skip removal IS correct (REVIVED + validated).** v3
>    cancelled C.1 ("the `__imported_type__` skip is correct"). FALSE for whole-program
>    compilation: the bootstrap preamble carries only RUNTIME drops, not user-type
>    `<Type>__drop`, so the skip zeroed all 267 user-type drops. Removing both skip sites in
>    `populate_drop_metadata` → **0→267 `__drop` defs, 0 double-defines** (the `fn_exists()`
>    guard handles dedup). C.1 = remove the skip.
> 4. **The OOM is closed by cluster (a), NOT (b).** v3 said "(b) ships first, kills the 13 GB
>    OOM" — DISPROVEN by measurement. The OOM is the total ABSENCE of scope-exit drop emission
>    (cluster a); un-disabling it (Phase D) took stage-1 from 14.4 GB → 1 MB. (b) makes (a)'s
>    frees *deep* but reduces nothing alone. OOM + the return-corruption SIGSEGV are ONE root
>    cause: cluster (a). The clusters are coupled, not independently OOM-relevant.

## TL;DR (for picking up cold)

1. The self-host has been "labels-only" since forever — IR tags ownership correctly, but emission ignores tags and leaks everything. Bootstrap passed pre-A.2 (commit `f15a45c6`) because both stages produced the same leaky C (byte-equal).

2. Path A series (C.1 through E.1) is the work of making the self-host emit CORRECT C. We've shipped 5/6. E.1 has failed 9 times because D.1's "consumer audit" was incomplete.

3. This plan replaces "try E.1 again with fingers crossed" with "exhaustively enumerate all consumer sites, fix them in batches, then ship E.1."

4. Last known-green bootstrap: commit `f15a45c6` (A.1, 2026-05-22 16:03). Last broken commit: `2e544e84` (A.2). Everything since has been whack-a-mole on the cascading consumer bugs.

## Background

### What "drop emission" actually means

Three coupled pieces, each with a writer site and a consumer:

1. **Per-type drop fns** (`<UserType>__drop(void* self)`) — emitted in stage1.c by `emit_struct_drops` / `emit_enum_drops` in lir_codegen.gg
2. **Per-collection elem_drop** — `gorget_array_new_drop(sizeof(T), T__drop)` at construction time, so `gorget_array_set` etc. can drop overwritten elements
3. **Per-scope drop calls** — `GIDropIfAlive(local_id)` emitted at scope exits in lower.gg, lowered to `<type>__drop(&slot)` C calls

All three must ship together. Today self-host has #1 partially (runtime types only, not user types), #2 barely (1-2 call sites), and #3 not at all (no-op stubs).

### Why E.1 is the keystone

Without `lower_return`'s `MoveZero` on the returned local, the function-exit drop fires on the local that the returned struct's interior pointers alias. The caller dereferences dangling pointers → SIGSEGV. This is the canonical "return-corruption" backtrace seen in all 9 prior E.1 attempts:

```
str_alloc_copy ← gorget_string_clone_to_owned ← loader___type_mentions_iter
  ← function_mentions_iter ← ast_mentions_std_iter_need
  ← should_auto_load_std_iter ← load_imports ← main
```

### Why prior attempts failed

The 6-commit plan (C.1 → A.1 → A.2 → B.1 → D.1 → E.1) assumes D.1 exhaustively finds all LIR consumers that need updating for Ptr-typed locals. **D.1 missed sites.** Each E.1 attempt was sunk by a different unaudited consumer:

| Attempt | Sunk by |
|---|---|
| Phase 4 retry 1-3 | Class 1-7 aliasing bugs |
| Phase 4 retry 4-7 | Class 8 field-read aliasing |
| Phase 3.5 | Ptr-aggregate-store at LIR |
| COMMIT 3 step 1 (GIMoveZero) | LoBorrowed/LoView propagation gaps |
| Today's COMMIT 1 attempt | OpBorrow → ISlotLoad (value, not address) |

The pattern is consistent: shipping E.1's emission triggers calls to drop/clone fns from sites that lower the operand wrong. Each fix landed but a new gap surfaces.

### What this plan does differently

Instead of "ship E.1 and debug the next cascade":

1. **Phase A**: Enumerate ALL OpBorrow / OpClone / OpMove consumers, audit each against Rust's equivalent. Produce a checklist of `(site, current_behavior, expected_behavior, fix_description)`.
2. **Phase B**: Land audit fixes in batches BEFORE attempting E.1. Each batch validates independently (lib + lowerer_comparison).
3. **Phase C**: Register user-type drops + elem_drops.
4. **Phase D**: Wire scope-exit emission.
5. **Phase E**: Ship E.1.
6. **Phase F**: Confirm bootstrap + fixed_point.

Phases A-B are the work that should have been D.1. Without them, we're guessing.

## Current state inventory (as of 2026-05-24)

### Working baseline (HEAD = `b78044c1`)

- ✅ `cargo build --release` clean
- ✅ `cargo test --lib --release` 1059/1061 (2 pre-existing fails are baseline)
- ✅ `cargo test --test integration --release lowerer_comparison` 1/1 (~63s)
- ✅ Individual fixture compilation: parser.gg, loader.gg, traits.gg all work
- ⚠️ `self_host_bootstrap` fragile — OOMs on driver.gg in elaborate_drops (passes when memory available, fails when constrained)
- ❌ `self_host_bootstrap_fixed_point` will not converge without Phase D + E

### Path A series status

| Phase | Commit | Status | What it does |
|---|---|---|---|
| C.1 | 67e17357 | ✅ | BorrowOrigin enum + tracking helpers |
| A.1 | f15a45c6 | ✅ | GIFieldLoad variant (stub) — last green bootstrap |
| A.2 | 2e544e84 | ✅ | LIR FieldLoad dispatch + EFieldAccess Ptr-wrap |
| B.1 | 84e60ae8 | ✅ | lower_var_decl_assign_mode 7-branch tree |
| D.1 | 79d718d5 | ✅ (INCOMPLETE AUDIT) | LIR consumer audit — missed sites |
| Phase 2.3 | bac24e49 | ✅ | op_consume GtPtr → decide_ptr_consume |
| Phase 2.3.5 | 58da31e6 | ✅ | unwrap Ptr/MutPtr at .clone() dst type |
| lex_emit fix | 7fcbe65e | ✅ (today) | skip writeback for Ptr-borrow receivers |
| Drop fwd-decls | b78044c1 | ✅ (today) | enum drop/clone fwd decls + clone return type |
| E.1 | — | ❌ | 9 attempts, all reverted |

### Known unaudited consumer sites (discovered today, partial list)

| Site | File:Line | Problem |
|---|---|---|
| OpBorrow lowering | lir_lower.gg:2482 | Emits `ISlotLoad`, not `ISlotAddr`. Gives value, not address. |
| match_scrutinee_ptr | lower.gg:5829-5836 | Uses `op_consume(scrutinee, CkAssign())` — forces clone for resource params. Should keep scrutinee as borrow. |
| `__field_read_TYPE_FIELD` codegen | lir_codegen.gg:3528 | Casts `base_val` to `(Type*)`. Works only when base_val is a pointer; broken when it's a struct value. |
| populate_drop_metadata | lir_lower.gg:3439, 3475 | Skips `__imported_type__` types. Means 0 user-type drops in stage1.c. |
| is_droppable_type | lower.gg:887-927 | Excludes user types from drop registration. |
| Vector[T] construction | lir_lower.gg / lir_codegen.gg | Uses `gorget_array_new` without elem_drop for user types. |

This is a partial list. Phase A's job is to make it exhaustive.

## Phase A: Hybrid audit — static enumeration + dynamic probe

**Goal:** Enumerate the *runtime-reachable* cascading consumer bugs E.1 will trigger. Static reading alone misses subtle aliasing; dynamic probing alone gives backtraces but no breadth.

**v2 change:** v1's "read-only audit" was naive. The 9 prior E.1 attempts already proved that. Pure static reading would have false positives (sites that look broken but aren't reached) and false negatives (runtime aliasing that doesn't show in code shape). Replaced with a hybrid.

### A.1 — Static enumeration (candidate set, ~2-4h)

```bash
grep -n "OpBorrow\|OpClone\|OpMove" tests/fixtures/self_host_lowerer/lir_lower.gg
grep -n "match_scrutinee\|lower_pattern\|emit_tag_read\|emit_payload_read" tests/fixtures/self_host_lowerer/lower.gg
grep -n "(__gg_[A-Z][A-Za-z_]+ \*)" tests/fixtures/self_host_lowerer/lir_codegen.gg
```

For each match: walk the call chain. Categorize as (i) call-arg consumer, (ii) slot-store consumer, (iii) tag/field read consumer. Cross-reference against Rust equivalent. **Produces a CANDIDATE list of sites that MIGHT need updating, not a definitive fix list.**

### A.2 — Dynamic probe (the load-bearing step, ~4-6h)

**v2 (post-review) addition: staged probe — smaller fixtures first to drain cheap cascades, driver.gg last.**

The naive "run stage-1 on driver.gg and collect SIGSEGVs" risks stalling on the elaborate_drops OOM (~88s, ~13 GB) before reaching the cascade sites. The OOM and the SIGSEGVs are partly the same underlying problem — both stem from missing drops — but they're different failure modes that may compete for which fires first. Staged probing lets cheap cascades surface on small inputs before we burn cycles on driver.gg.

**On a throwaway branch:**
1. Ship Phases C + D + E together (drops emit, scope-exit fires, lower_return MoveZero) on the throwaway branch, accepting this will SIGSEGV and/or OOM somewhere
2. Build stage-1 (cc clean expected; if not, the unrelated cc-emission bug surfaced first — fix that)

**Then probe in stages:**
3. **A.2.a — Probe parser.gg first** (~174 KB, compiles cleanly today in ~60s, ~3 GB peak). Cascades that surface here are CHEAP — small input, low memory, fast turnaround. Stub past each SIGSEGV, re-run, collect backtraces. Continue until parser.gg runs to completion.
4. **A.2.b — Probe loader.gg next** (~similar size, compiles cleanly today). Captures any cascades parser.gg didn't trigger. Same stub-and-continue approach.
5. **A.2.c — Probe driver.gg last** (full transitive module, ~500 functions, ~13 GB memory pressure today). At this point most cheap cascades have been drained. Bump `GG_STAGE1_TIMEOUT_SECS=600` and ensure ≥16 GB RAM available. If the OOM still fires before reaching the cascade sites, the drops we shipped in C+D+E aren't actually closing the leak — diagnose that as a higher-priority blocker before continuing the probe.

**Stub-past-SIGSEGV technique:**
- Identify the crashing function from the backtrace
- Wrap the problematic OpClone/OpBorrow/Token-cast site in a temporary guard (`if (slot is broken-shape) skip drop`)
- Re-run, collect the NEXT crash backtrace
- Stubs are throwaway — they get replaced by real fixes in Phase B

**Output**: a list of every reachable cascade site with its specific backtrace, in `docs/plans/cascade_probe_results.md`. Each entry includes:
- Crash signature (top of stack)
- Fixture that surfaced it (parser.gg / loader.gg / driver.gg)
- Function name + GIR-level pattern triggered
- Whether the static audit (A.1) flagged it
- Concrete fix description

**Fixture-staging rationale (for future readers):**
- Cheap cascades (small-input, low-memory) get fixed first → reduces driver.gg's leak surface area before A.2.c
- driver.gg's OOM may resolve on its own as B.x fixes land — many of the elaborate_drops leaks are downstream of the same consumer bugs the probe hunts
- If A.2.c still OOMs after all of A.2.a + A.2.b's findings are fixed in B, that's a NEW signal — the drops we shipped have an additional bug not exposed by smaller inputs. Tag as separate work.

### A.3 — Cross-reference Rust impl for each cascade

For every cascade site found in A.2, find the Rust impl equivalent (`src/ir/lowering/*.rs`, `src/lir/lower/*.rs`). Document what Rust does differently and the exact Gorget-port shape.

### A.4 — Produce the checklist

Output table format (in `docs/plans/consumer_audit.md`):

| ID | Site | Cascade signature | Rust does | Fix | Static-found? |
|---|---|---|---|---|---|
| C-01 | `lir_lower.gg:2482` (OpBorrow lowering) | `error: cannot convert to pointer type` at `((Token*)val)->tag` | Emits ISlotAddr when slot is struct | Switch to ISlotAddr for non-scalar slots | ✓ |
| C-02 | `match_scrutinee_ptr` (forced OpClone on params) | Same as C-01 (upstream cause) | Uses CkMatchPtr → OpBorrow | Replace CkAssign with CkMatchPtr | ✓ |
| ... | ... | ... | ... | ... | ... |

**Static-found** column tracks how well A.1 predicted A.2's findings. If most cascades are marked ✗ (static missed them), this signals the static approach was wrong all along.

### A.5 — Identify dependency DAG

Some fixes interact. Document order. Group into batches that can land independently.

**Estimated effort:** 9-18 hours total (A.1: 2-4h, A.2: 6-12h — staged a/b/c with ~88s rebuilds per stub iteration is likely the long pole, A.3-5: 1-2h).
**Validation:** Throwaway branch produces stage-1 that runs to completion (with guards).
**Exit criterion:** Stage-1 reaches `print(generate_c(...))` on driver.gg without SIGSEGV (the guards being in place is fine — we'll remove them in Phase B).

### A.6 — Decision gate: scope check

If A.2 finds >15 distinct cascade sites OR >5 sites require restructuring (not just patching): trigger the abandon-and-rewrite path (see "When to abandon and rewrite" below). The cascading count is the leading indicator of structural drift severity.

## Phase B: Consumer audit fixes (batched)

**Goal:** Land each audit fix as a single commit. Validate after each.

**v2 change:** v1 claimed `lowerer_comparison` validates per-batch. **This is false** — that test only counts `fn ` declarations (`tests/integration.rs:13389-13392`), it's blind to drop emission. v2 uses a richer per-batch validation.

### B.x (one per batch from Phase A.5)

Each batch:
1. Make the code change matching the audit's "Fix" column
2. **Remove the corresponding A.2 guard** from the throwaway branch's instrumentation (the guard is "fix this site or skip it")
3. `cargo build --release` clean
4. `cargo test --lib --release` 1059/1061 (catches gross breakage but not drop-shape changes)
5. `cargo test --test integration --release lowerer_comparison` 1/1 (catches function-count regressions; blind to drops)
6. **(New)** Manual stage-1 rebuild:
   - cc clean (no new errors)
   - Stage-1 on traits.gg: 1011 lines, exit 0 (no regression)
   - Stage-1 on parser.gg: stays exit-0 (no new SIGSEGVs introduced)
7. **(New, per-batch)** Diff the generated drop bodies vs Rust gg's driver.c:
   ```bash
   diff <(grep "^void.*__drop\b" /tmp/stage1_body.c | sort) <(grep "^void.*__drop\b" tests/fixtures/self_host_lowerer/driver.c | sort)
   ```
   Check for spurious drops added / drops missing. NOT a hard equality gate (some shape differences are expected) but a CHANGE log to inspect.
8. Commit with clear message citing the audit table ID

If any batch fails validation: roll it back. Continue with other independent batches. File the failed batch as a follow-up.

**Estimated effort:** 2-4 hours per batch. Batch count is unknown until Phase A.2 completes — could be 3-10 batches. **Total: 6-40 hours.**
**Validation gates:** as above. Note: the strongest gate is the manual stage-1 runs, not lowerer_comparison.
**Exit criterion:** All A.2 guards removed; stage-1 runs cleanly on driver.gg without SIGSEGV.

## Phase C: User-type drop + elem_drop registration

**Goal:** Make `<UserType>__drop` definitions emit in stage1.c body; wire elem_drop on user-type collections.

### C.1 — Remove __imported_type__ skip  **REVIVED + VALIDATED (v3.1, was wrongly cancelled in v3)**

v3 cancelled this, claiming the `__imported_type__` skip (`lir_lower.gg:3439, 3475`) is correct
because "imported types' drops come from the Rust preamble as static inline." **That is FALSE
for whole-program compilation (the bootstrap).** The bootstrap preamble is
`rust_c[..first "\ntypedef struct __gg_"]` — it contains only RUNTIME-type drops
(`gorget_string_free` etc.), NOT user-type `<Type>__drop`. driver.gg imports every module, so
the skip tagged EVERY user type `__imported_type__` and `populate_drop_metadata` skipped all of
them → `recursive_drop_structs` empty → **0 `__drop` definitions** (Rust emits 212+).

**Fix (validated this session):** remove BOTH skip sites in `populate_drop_metadata`. Result:
`__drop` defs **0 → 267**, clone_inplace 0 → 89, **0 redefinition errors** — the `fn_exists()`
guard in `emit_struct_drops`/`emit_type_drop_fns` (`lir_codegen.gg` ~4592) is the PROPER
double-define guard (vs. a blanket skip). The "generator works" observation in v3 was true but
irrelevant — the generator iterates `recursive_drop_structs.keys()`, which the skip kept empty.
**This is part of the cluster-(a) atomic change** (it unmasks the match-scrutinee + move-zero
cascades — see consumer_audit.md). Already in WIP commit `1614ac2a`.

### C.2 — Include user types in is_droppable_type

`tests/fixtures/self_host_lowerer/lower.gg:901-927` — `is_droppable_type` currently returns
true only for `GtNamed` with `resource_meta_for(name)=Some` (runtime types). Flip to also
return true for user resource types (struct/enum with resource fields, via
`gmod.resource_types`). **COUPLES WITH CLUSTER (a):** registering user types for drop is only
safe once (a-1..a-5) ensure borrows aren't registered and moved-out slots are zeroed —
otherwise it double-frees. Ship c-2 *inside* the atomic cluster-(a) batch, not before.

### C.3 — Per-element drop fn-pointer wiring (v3: generalize the existing pattern)

**NOT** `gorget_array_new_drop`. Rust uses ONE allocator (`gorget_array_new` /
`gorget_map_new` / `gorget_set_new`) then wires the element drop/clone/materialize fn-pointers
into the collection struct *after* construction via `SlotAddr + ElemPtr(byte_offset) +
Store(NamedFuncAddr)`, driven by `drop_strategy` metadata (`src/lir/lower/insts.rs:1880-1906`,
`elem_drop_fn_for_type` at `src/lir/lower/types.rs:103-128`). Fixed offsets: GorgetArray
elem_drop=40/elem_clone=48/elem_materialize=56; GorgetMap
val_drop=104/val_clone=112/key_drop=120/key_clone=128/val_materialize=136; GorgetSet
key_drop=120/key_clone=128.

The self-host **already implements this pattern** for Dict/Set *keys* in
`emit_dict_ctor_wiring` (`lir_codegen.gg:1246-1248`: `dv.key_drop = (..)key_ty__drop`).
**This is cluster (b) — shipped as groundwork in `d2efd716` (NOT independently OOM-relevant; the
OOM is closed by cluster (a) — see the v3.1 correction below):**
- **b-1 (D1):** generalize the store pattern to Vector elements (`lir_lower.gg:2839` ctor).
- **b-2 (D2):** add the symmetric `val_drop`/`val_clone` wiring for Dict values
  (`lir_lower.gg:2800` ctor).
- **b-3 (D3):** extend Set element coverage (`lir_lower.gg:2825` ctor).
- **min-2 (F2):** recursive box-drop for enum Box-payload variants (`lir_codegen.gg:4609`) —
  add a typed `box_inner_type` field (Snag #13 shape), don't name-match.

Drive every spelling from the typed drop table (`recursive_drop_structs/enums` /
`drop_strategy`), never a name prefix. **SHIPPED** (`d2efd716`). **Correction (v3.1):** this
does NOT kill the OOM on its own — `elem_drop` is inert until cluster (a) emits the scope-exit
frees that invoke it. The OOM (14.4 GB → 1 MB) is closed by cluster (a)'s Phase D, not by (b).
(b) makes (a)'s frees *deep* (e.g. freeing a `Vector[Vector[int]]` also frees the inner arrays).

### C.4 — Validate

- `lowerer_comparison` 1/1
- `grep -c "void.*__drop\b" stage1.c` > 50 (currently 0)
- `grep -c "gorget_array_new_drop" stage1.c` > 20 (currently 1-2)
- Manual stage-1 compile: NO regressions on hello.gg, traits.gg

**Estimated effort:** 1-2 hours.
**Risk:** Untested user-type drop fns may have generation bugs. Surface area: ~200 types in driver.gg.

## Phase D: Real drop emission at scope exits

**Goal:** Replace no-op stubs in `emit_scope_drops`, `emit_drops_for_early_exit`, `flush_drop_queue` with real `GIDropIfAlive` emission.

**v3 change — THE QUEUE WAS A MISTAKE.** v2 said "Decision is forced: queue + flush" because
it assumed emission must be *gated on `maybe_moved`*, and `maybe_moved` isn't populated until
after the inline emit calls. But the audit established (from Rust source + the self-host's own
`lower.gg:767-779` docstring + `structural-guards.md:112-120`) that emission is
**unconditional** — `GIDropIfAlive` is emitted for every droppable local regardless of
`maybe_moved`; the runtime memcmp guard + LIR `drop_elab` handle the actual skip. Rust
`drops.rs:504-505,540-541` literally `let _ = entry.maybe_moved;` at the emit site. There is
**no drop queue** in Rust — the scope's `entries` vec IS the queue, walked in reverse at
scope-pop / early-exit.

Remove the `maybe_moved` dependency and the *only* reason for queue+flush evaporates. Emit
**directly at scope-pop**, which is the shape `pop_drop_scope` (`lower.gg:851`) already
supports. **Retire `DropEmission` / `ctx.drop_queue` / `flush_drop_queue` entirely** (dead
code once D.1/D.2 emit directly).

### D.1 — emit_scope_drops body (direct, unconditional)

`tests/fixtures/self_host_lowerer/lower.gg:838-843` — replace the no-op with direct LIFO
emission, NO `maybe_moved` consultation:

```gorget
void emit_scope_drops(LowerCtx &ctx, Vector[DropEntry] entries):
    int i = entries.len() - 1
    while i >= 0:
        DropEntry entry = entries.get(i).unwrap()
        emit(&ctx, GIDropIfAlive(entry.local_id))   # unconditional; drop_elab elides
        i = i - 1
```

(`is_droppable_type` already gated *registration*, so `entries` only holds droppable locals.
The borrow-vs-owner correctness lives at the registration gate (a-1) + slot-kind-aware drop
lowering (a-2/a-3), NOT here.)

### D.2 — emit_drops_for_early_exit body (same, with exclude)

`tests/fixtures/self_host_lowerer/lower.gg:1051-1071`. Walk scope stack innermost→target,
emit `GIDropIfAlive(entry.local_id)` per entry, skip `exclude_local` (the returned local).
Mirrors Rust `emit_early_exit_drops` (`drops.rs:369-383`).

### D.3 — flush_drop_queue: DELETE

`tests/fixtures/self_host_lowerer/lower.gg:1096-1105` + its call sites (6630, 6861) + the
`DropEmission` struct (121-126) + the `ctx.drop_queue` field. All dead once D.1/D.2 emit
directly. (Rust has no analogue — confirms this is vestigial self-host scaffolding.)

**Also delete the stale/contradicting docstrings** (review Edit C — CLAUDE.md "elegance
showcase" rule 3, these are false historical records): the queue-design docstrings at
`lower.gg:80-120` + `164-172`; the "self-host doesn't have drop_elab" claim at
`lower.gg:811-818` (FALSE — `drop_elab.gg` is wired at `driver.gg:79`); the "0 such
definitions today"/TODO in `is_droppable_type` at `lower.gg:887-900`; and the "when self-host
grows one" comment at `lir_lower.gg:3164`. A fix is incomplete until these fossils are pruned.

**Borrow-ordering note — ship a gap fixture, not just prose (review Edit A):** Rust's
`emit_scope_drops_ordered` (`drops.rs:414-508`) topo-sorts drops so borrowers drop before
their sources; plain LIFO is Rust's `!has_constraints` fast path. Start with plain reverse-LIFO
(matching `entries` order), but per "Don't redesign around compiler gaps" the deferral must
leave a **wired artifact**: ship a `#[ignore]`'d fixture exercising a
borrower-whose-source-drops-first case, with expected output reflecting correct ordering.
Promote to topo only if that fixture or the probe surfaces a use-after-free — not a bare prose
deferral that lets a latent UAF ride along invisibly.

### D.4 — Validate

- `cargo test --lib --release` 1059/1061
- `cargo test --test integration --release lowerer_comparison` 1/1 (**note**: this stays 1/1 because the test counts `fn ` declarations and drops don't change that count — see lowerer_comparison limitation above)
- Stage-1 build clean
- Stage-1 on traits.gg: 1011 lines, exit 0
- Stage-1 on parser.gg: produces output AND we can manually inspect emitted drops:
  ```bash
  grep -c "gorget_array_free\|gorget_string_free\|<UserType>__drop(" /tmp/stage1_body.c
  ```
  Should jump from 0 → 1000+.
- driver.gg memory profile: re-run the per-pass diag instrumentation from today's session. elaborate_drops's +10.76 GB delta should drop dramatically (the drops being emitted are exactly what closes the leak).

**Estimated effort:** 4-8 hours.
**Risk:** Will surface remaining E.1-related bugs. Expected — D's drops fire, exposing return-corruption that E.1 must close.

**Decision: D+E ship atomically.** Intermediate state (D without E) crashes the bootstrap; no value shipping it independently.

## Phase E: lower_return MoveZero (the keystone)

**Goal:** Port Rust's `lower_return` faithfully. NOT just MoveZero detection — Rust's `lower_return` is ~250 lines with 6-7 distinct concerns. Port them all upfront; trim what's unused after empirically confirming.

**v2 change:** v1's "may or may not be needed depending on what tests fail" is the same reactive whack-a-mole pattern v1's Phase A was supposed to eliminate. v2 ports all concerns upfront.

### E.1 — Port lower_return from Rust faithfully

`src/ir/lowering/stmts/mod.rs:1509-1750` is the reference. Six distinct concerns to port:

1. **`owning_param_returned`** (Rust 1536-1547): when returning a `!`-sigil resource param identifier, MoveZero the param slot. Without this, function-exit `DropIfAlive { *v }` frees data the caller's return value still aliases.

2. **`clone_resource_global_ref`** (Rust 1552): when returning a MODULE_GLOBAL_STRING (or other module-global resource), clone the global so the caller gets an independent allocation. Rare path but real.

3. **`is_explicit_result_variant`** detection (Rust 1518-1568, Snag #36): when the return expr is already `Ok(...)` / `Error(...)` / `Some(...)` / `None`, skip the auto-wrap-in-Result step. Without this, throws functions double-wrap.

4. **`maybe_auto_propagate`** (Rust 1569-1576): for throws functions returning a Result, unwrap so the Ok-wrap below works on the inner value.

5. **`returned_local`** detection + MoveZero (Rust 1578-1584, 1604-1608, 1674-1676): identify the local being moved into return slot 0; MoveZero it. **This is the headline keystone.**

6. **Ok-wrap for non-explicit return** (Rust 1612-1672): wrap value in Result.Ok variant.
   **Confirm-then-port (review Edit D):** the throws-fn return type is already widened to
   `Result__T__E` (`lower.gg:6501-6508`) but the value is NOT wrapped today — so either
   self-host fixtures always write explicit `Ok(...)`/`Error(...)` (concern 6 is moot now) or
   bare `return x` in a throws fn mis-compiles. Before porting: grep self-host throws-fns for
   bare `return <expr>` (non-`Ok`/`Error`). Port concern 6 regardless (for completeness), but
   if no bare returns exist, mark it untested-by-fixture and add a fixture returning a bare
   value from a throws fn (Snag #36 shape) so the port has a regression net.

7. **Ptr(T) → T auto-clone** (Rust 1717-1740): when operand is Ptr(T) but return type is T, clone-through-deref to materialize an owned T.

Self-host's `lower.gg::case SReturn(opt_expr)` (line ~5291) currently does only the bare minimum: lower expr → emit GIAssign(0, val) → drops → terminator.

The full port involves adding all 7 concerns. Reference each Rust line range; cite the matching Gorget code path.

### E.2 — Validate

- `cargo test --lib --release` 1059/1061
- `cargo test --test integration --release lowerer_comparison` 1/1 (function count preserved)
- `cargo test --test integration --release self_host_bootstrap -- --test-threads=1` **PASSES**
- `cargo test --test integration --release self_host_bootstrap_fixed_point -- --test-threads=1` **PASSES**
- driver.gg memory profile: instrument elaborate_drops again, peak memory should drop from ~13 GB to <3 GB

**Estimated effort:** 4-8 hours (v1 said 1-2h — that was wrong, Rust's lower_return is ~250 lines with 6-7 concerns).
**Risk:** Phase A.2 dynamic probe should have surfaced any cascades; if not, ONE more cascade may emerge here. If so: file as sub-batch in B, fix, retry E.2.

## Phase F: Lock in + cleanup

### F.1 — Tighten fixed_point N=5 back to N=2

`bootstrap_fixed_point` allows 5 cascade generations to converge. With correct drop emission, should converge in ≤2. File as task #10.

### F.2 — Retire ad-hoc Fixes A/B/C/D

Once Phase E is solid, audit the accumulated workarounds in lower.gg. Each "Fix A/B/C/D" comment + the `add_local_inheriting` plumbing should be auditable against the Rust impl. Retire any that are obviated by the proper machinery. Cite "Don't redesign around compiler gaps" / "Self-host as elegance showcase" rules from CLAUDE.md.

### F.3 — Documentation

- Update `docs/internals/copy-on-write.md` if any of the seven materialization points shifted
- Move `project_rust_machinery_port_plan.md` from "in progress" to "complete" status
- Archive this plan to `docs/plans/archive/` or rename

### F.4 — Optional: ship a snag-style fixture

A small fixture that exercises the return-corruption pattern Rust gg handles and self-host now does too. Wired into the integration sweep.

**Estimated effort:** 2-4 hours.

## Risks

### R-01: New cascade discoveries during Phase E

History suggests every E.1 attempt surfaces ≥1 new unaudited consumer. Mitigation: Phase A's exhaustive audit. Residual risk: ~30% the audit still misses something.

### R-02: Audit underestimates scope

Phase A might miss site categories we haven't identified. Mitigation: include `lower.gg` writer sites in addition to LIR consumer sites; audit method-call patterns separately from operand lowering.

### R-03: Phase B fixes interact unexpectedly

Two fixes that both look correct in isolation may conflict. Mitigation: small batches, validate after each, easy revert.

### R-04: Test flakiness from OOM

`self_host_bootstrap` is memory-fragile. After Phase E + drop emission, this should resolve (the leak source is closed). If it doesn't resolve, the leak isn't fully closed — return to Phase A.

### R-05: lowerer_comparison gives false confidence (was: "breaks during Phase D")

**v2 correction**: lowerer_comparison only counts `fn ` declarations (`tests/integration.rs:13389-13392`) — it does NOT see GIDropIfAlive instructions or any sub-function shape. Drop emission does NOT change function counts, so the test stays 1/1 even when drops are completely broken. **The real risk is the opposite of v1's claim**: the test gives false confidence that Phases C-D-E are working when they aren't.

Mitigation: rely on manual stage-1 rebuilds + grep-diff against Rust gg's driver.c emissions (per Phase B.x.7 + Phase D.4 validation gates). Treat lowerer_comparison as a function-count regression guard only.

## Rollback strategy

### Per-phase rollback

- Phase A is doc-only, no rollback needed.
- Phase B batches each commit independently; revert is `git revert <batch_commit>`.
- Phase C, D, E ship as separate commits; revert each independently.

### Worst-case rollback

If everything goes wrong: `git reset --hard f15a45c6` returns to the last known-green bootstrap. We lose:
- The architectural progress since A.2
- Today's parser.gg fix (real correctness fix; would need to be re-applied on top)
- Forward-decl prep work

We gain:
- Bootstrap + fixed_point pass (in the labels-only sense)
- Time to think

## Resource estimate (v2)

| Phase | v1 estimate | v2 estimate | Notes |
|---|---|---|---|
| A. Hybrid audit | 4-8h | 9-18h | Staged probe a/b/c with ~88s rebuilds per stub iteration is likely the long pole |
| B. Audit fixes | 6-20h | 6-40h | Batch count unknown until A.2 completes |
| C. User-type drop reg | 1-2h | 1-2h | Unchanged; generator works |
| D. Scope-exit emission | 4-8h | 4-8h | Queue+flush is forced (not direct-emit) |
| E. lower_return MoveZero | 1-2h | 4-8h | v1 underestimated; ~7 concerns to port |
| F. Cleanup | 2-4h | 2-4h | Unchanged |
| **Total** | **18-44h** | **26-80h** | 3-7 focused sessions |

v2 is wider because:
- Phase A added the dynamic-probe step (real cost not optional)
- Phase B is unbounded — depends on cascade count from A.2
- Phase E porting 7 Rust concerns instead of 1

**Rewrite threshold reset:** if the plan ends up costing >60h, the lowerer rewrite (1-2 weeks = ~80h) becomes economically competitive. The decision-gate is in Phase A.6.

## Reading order for next session

If picking this up cold:

1. **This plan, top to bottom.** (~15 min)
2. **DONE.md** entries from A.1 onward — the empirical numbers and what each commit did. (~15 min)
3. **TODO.md** top entries — most recent status. (~5 min)
4. **`docs/internals/copy-on-write.md`** Phase 3 — the seven materialization points. (~10 min)
5. **`docs/internals/clone-emission-at-calls.md`** — the canonical decision tree. (~10 min)
6. **`docs/internals/layering-discipline.md`** — Especially "fix complexity as a signal of wrong layer". (~10 min)
7. **`src/ir/lowering/stmts/mod.rs:1509-1750`** — the E.1 blueprint. All 7 concerns (owning_param_returned, clone_resource_global_ref, is_explicit_result_variant, maybe_auto_propagate, Tier 1c, Ok-wrap, Ptr→T auto-clone). (~15 min)
8. **`src/ir/lowering/drops.rs:107-508`** — the drop scope management blueprint. (~10 min)

Total: ~85 min to be cold-ready on Phase A.

## Decision points to surface before starting

**v2: the original v1 decision points 1 and 2 are now resolved in the phase bodies — kept here only as decision-archive. Active decisions are 3 and 4.**

1. ~~Direct emit vs queue+flush for D.1.~~ **DECIDED**: queue+flush (forced by lower.gg:82-88 docstring — `maybe_moved` is universally false at inline-emit time). See Phase D body.

2. ~~Phase D + E atomic vs separate.~~ **DECIDED**: atomic. Intermediate state crashes the bootstrap; no value shipping independently. See Phase D body.

3. **Phase F.2 ad-hoc workaround retirement timing.** Active decision. Recommendation: pause F.2 until F.1 and F.3 are done; retiring workarounds prematurely risks breakage. Re-evaluate after Phase E is locked in.

4. **Test fragility threshold.** Active monitoring. If `self_host_bootstrap` stays OOM-fragile even after Phase E, that's a signal Phase E didn't close the elaborate_drops leak source. Where to look next: lower_module's IR construction, lir_lower's slot allocation. May trigger a new diagnostic phase.

## When to abandon and rewrite

If Phase A surfaces >15 unaudited sites OR Phase B uncovers structural issues that require redesigning the IR shape: stop incremental work, write the rewrite plan, switch to the lowerer rewrite path (see this session's transcript for the rewrite proposal sketch).

Rewrite threshold: if total estimate balloons past 60 hours, the rewrite (1-2 weeks) becomes more economical.

## Authorship

Written by Claude during 2026-05-24 session after diagnosing the cascading-bug pattern that broke 9 prior E.1 attempts. Based on:
- Full context of today's session (lex_emit fix, COMMIT 1 attempt, OpBorrow cascade)
- Git log + DONE.md historical record (last green = f15a45c6, broke at 2e544e84)
- Rust impl reading (`src/ir/lowering/stmts/mod.rs`, `src/ir/lowering/drops.rs`, `src/lir/lower/insts.rs`)
- Memory files (`project_rust_machinery_port_plan.md`, `project_rust_borrow_machinery_depth.md`)

v2 revised after reviewer agent feedback flagged three load-bearing flaws (see Revision history below).

If picking up cold, start with §"Reading order for next session" above.

## Revision history

### v3.1 — 2026-05-24 (post implementation — corrects two v3 claims; live plan moved out)

Implementation began (WIP commit `1614ac2a`). Empirical results corrected v3:
- **Phase C.1 UN-cancelled.** v3 cancelled C.1 ("the `__imported_type__` skip is correct").
  WRONG for whole-program compilation — the skip zeroed all user-type drops (0 `__drop` defs).
  Removing both skip sites → 0→267 defs, 0 double-defines (`fn_exists` guard). C.1 = remove the
  skip; it's part of the cluster-(a) atomic change.
- **"(b) kills the OOM" DISPROVEN.** The OOM is the total absence of scope-exit drop emission
  (cluster a); Phase D un-disable took stage-1 14.4 GB → 1 MB. (b) is inert until (a) emits the
  frees. OOM + return-corruption SIGSEGV are one root cause (cluster a); the clusters are
  coupled, not independently OOM-relevant.
- **Confirmed/shipped:** Phase D unconditional emission (WIP); cluster (b) C.3 (`d2efd716`);
  a-7 match-scrutinee borrow (cascade 2780→0); user-type drops generate (267). Remaining:
  set/push consume-via-pointer ABI + the move-zero-at-all-consume-sites loop → see the LIVE
  execution plan **[`drop_emission_next_session.md`](drop_emission_next_session.md)** and the
  empirical log **[`consumer_audit.md`](consumer_audit.md)**.
- This file is now the strategic/historical record; the v3 TL;DR was replaced by a v3.1 banner
  at the top. The v3 entry below is preserved as the record of what v3 (wrongly) decided.

### v3 — 2026-05-24 (post Phase A static audit)

Phase A static audit completed (`docs/plans/consumer_audit.md`): 3 parallel worktree agents
(self-host LIR, self-host lower.gg, Rust reference) + parent doc synthesis. The audit
ground-truthed v2's phase designs against current code + Rust source and found **four
load-bearing errors in v2**:

- **Phase D inverted the soundness contract.** v2 gated `GIDropIfAlive` on `maybe_moved`
  (queue+flush). Rust emits unconditionally and discards `maybe_moved` at the emit site
  (`drops.rs:504-505,540-541`); gating reintroduces the Snag #30 double-free
  (`structural-guards.md:112-120`). There is no drop queue in Rust. **Rewrote Phase D** to
  direct unconditional emission; retired `DropEmission`/`drop_queue`/`flush_drop_queue`. The
  self-host's own `lower.gg:767-779` docstring already stated the correct contract; v2
  inherited the contradicting `lower.gg:82-94`/line-169 comment.
- **Phase C.3 misframed + name-matching violation.** Rust uses one allocator + post-ctor
  fn-ptr stores driven by `drop_strategy`, not `gorget_array_new_drop`. **Reframed C.3** as
  "generalize the existing `emit_dict_ctor_wiring` key-drop pattern to Vector elems + Dict
  values."
- **Phase C.1 wrong.** The `__imported_type__` skip is correct (imported drops come from the
  Rust preamble). **Cancelled C.1.**
- **The cascade is two separable clusters.** v2 treated it as one monolithic risk. Audit
  split it: (a) double-free [L2/L3 `&slot` on Ptr slots + C2 IMoveSlot no-op + B1 missing
  borrow flag + E.1] must ship ATOMICALLY (root cause of all 9 E.1 failures — half-shipped);
  (b) leak/OOM [D1/D2/D3 missing collection elem/val-drop wiring] is INDEPENDENT. **Reordered:
  ship (b) first** — it kills the 13 GB driver.gg OOM [⚠️ **v3.1: this claim was DISPROVEN —
  (b) does NOT reduce the OOM; cluster (a)'s Phase D does. See the v3.1 entry above.**],
  resolving R-04 and the entire reason
  v2 built the elaborate staged a/b/c probe. The A.2 dynamic probe demotes from discovery to
  validation. **A.6 gate verdict: do NOT trigger rewrite** (~13 sites, 0-1 restructuring).

### v2.1.1 — 2026-05-24 (post third-pass review estimate bump)

Reviewer ground-truthed v2.1 against the code (`GG_STAGE1_TIMEOUT_SECS` confirmed real at integration.rs:13800). Verdict: "ship it" — no substantive concerns. One trivia accepted: Phase A.2's sub-estimate of 4-6h was stale after the staged a/b/c restructure since each fixture rebuild is ~88s + N stub iterations. Bumped A.2 to 6-12h, A total to 9-18h, plan total to 26-80h.

**Note**: the 80h upper bound now brushes against the 60h rewrite-threshold. If Phase A.2 surfaces near the higher end of its band, treat that as a strong rewrite signal independent of A.6's >15-sites criterion.

### v2.1 — 2026-05-24 (post second-pass review)

Reviewer agent ground-truthed v2 against the actual code and confirmed all empirical claims hold. Surfaced one substantive concern + two pieces of stale doc-debt:

- **Probe vs OOM ordering** (substantive). The A.2 dynamic probe could stall on driver.gg's elaborate_drops OOM (~88s, ~13 GB) before reaching the SIGSEGV cascade sites it's hunting. The OOM and SIGSEGV are different failure modes that compete for which fires first. **Updated Phase A.2 to a staged probe**: parser.gg first (cheap, drains early cascades) → loader.gg next → driver.gg last with bumped resources. If driver.gg still OOMs after A.2.a/b's findings are fixed in B, that signals a separate bug beyond the cascade set.

- **R-05 was now false**. v1 claimed "lowerer_comparison breaks during Phase D because drop emission changes GIR shape." v2's own Q1 finding established that lowerer_comparison only counts `fn ` declarations — it CAN'T see drops at all. R-05's risk was the opposite of what v1 wrote. **Rewrote R-05** as "lowerer_comparison gives false confidence."

- **Decision points 1 & 2 stale**. Both were resolved in v2's phase bodies (queue+flush forced by Q5; D+E atomic by Phase D body) but the trailing "Decision points to surface" section still listed them as open. **Updated to archive the resolved ones and keep only the active decisions (3, 4)**.

- Minor: Reading order's E.1 reference widened from `stmts/mod.rs:1509-1600` to `:1509-1750` to cover all 7 concerns Phase E now ports.

### v2 — 2026-05-24 (this revision)

Reviewer agent surfaced five questions, three load-bearing. Empirically verified each and updated the plan:

- **Q1: `lowerer_comparison` is blind to drops.** Confirmed: `tests/integration.rs:13389-13392` only counts `fn ` declarations. v1's per-batch validation claim was false security. **Updated Phase B** to add manual stage-1 build + run-on-fixtures + diff-against-rust-drops as the real gate. lowerer_comparison stays in the chain but acknowledged as a function-count check only.

- **Q2: Pure static audit cannot find runtime cascades.** Reviewer's "ship D+E on throwaway branch, stub past each SIGSEGV" framing is sharper. **Rewrote Phase A** as a hybrid: static enumeration (A.1) gives candidates; dynamic probe (A.2) with SIGSEGV-stubbing collects the reachable cascade set. The static-only v1 would have had false positives and false negatives.

- **Q3: populate_drop_metadata generator works (confirmed).** Today's COMMIT 1 attempt empirically emitted `__gg_Token Token__clone(void* __p)` correctly. Rust gg emits 206 user-type drop bodies in driver.c with correct shape. Phase C estimate stands. **No plan change** — reviewer's concern was valid but resolved.

- **Q4: E.1 underestimated (1-2h → 4-8h).** Reviewer correctly counted 5+ distinct concerns in Rust's `lower_return`: owning_param_returned MoveZero, clone_resource_global_ref, is_explicit_result_variant detection, maybe_auto_propagate, Tier 1c Move-for-fresh-Result, Ok-wrap, Ptr(T)→T auto-clone. v1's "may or may not be needed depending on what tests fail" was the same reactive whack-a-mole pattern v1's Phase A was supposed to eliminate. **Phase E now ports all 7 concerns upfront.**

- **Q5: `maybe_moved` is NOT populated at scope-pop time.** Empirically verified in lower.gg:82-94 docstring — explicit statement that `wire_liveness_into_modes` runs AFTER inline emit calls. v1's "decision point: direct-emit vs queue+flush" was already decided by the existing DropEmission infrastructure. **Updated Phase D** to use queue+flush definitively, no "decision" framing.

Resource estimate revised from 18-44h to 23-72h.

### v1 — 2026-05-24

Initial draft, written same day. Three load-bearing flaws (above) discovered when reviewed.
