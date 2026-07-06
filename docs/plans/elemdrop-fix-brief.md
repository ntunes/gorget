# EXECUTOR BRIEF: collection-element destructor fix (P1 drop-lost + P2 field-leak), BOTH compilers

> **STATUS: v4 — pass 3 folded (R1 stale remnant, R2 emit-lane mechanics, R3 persistent ASan helper). pass 2 (Opus) EMPIRICALLY REFUTED the "both compilers identically broken"
> premise by running the pristine self-host driver: THE SELF-HOST IS ALREADY CORRECT (the
> 2026-06-22 fadb2259 fold routes every equip-Drop type into resource_types -> type_drop_fns ->
> __gorget_dtor_{T} wiring; the scout source-read a STALE comment at lir_lower.gg:5514-5517
> predating the fold). Phase S re-scoped accordingly. Passes: 1 (5) + 2 (3) + 3 (3) folded;
> 0 clean of >=3 — pass 4 pending. Executor: not launched.**
> Scout artifacts: /tmp/recover_elemdrop/ (findings, measured prototype patch, ready fixture).
> This brief was drafted by the scout that prototyped and MEASURED the Rust fix end-to-end
> (probe matrix, ASan, bootstrap 446s PASS); reviews verify, executor lands.

## Operational rules
Worktree preamble verbatim (CLAUDE.md): pwd + git rev-parse first, inside YOUR worktree; never
touch /workspace/gorget or /workspace/gorget-1; worktree-RELATIVE paths; never git stash
(checkpoint via git diff > /tmp/elemdrop_exec_state.patch); Edit-desync -> re-Read + retry;
non-Edit writes -> check main status and STOP on surprises. Zones: src/lir/lower/insts.rs,
tests/fixtures/drop_collection_custom_elem*.gg (NEW), tests/integration.rs (your test fns only),
tests/lints.rs (the arm-count ratchet, additive). NEVER: TODO.md, docs/**, spec/** (concurrent
ggdef track), tests/fixtures/self_host_lowerer/** (Phase S changes NO self-host source). TWO PHASES, commit each: Phase R (Rust fix, from the proven patch) then
Phase S (self-host regression LOCK — no self-host source changes; see SELF-HOST STATUS). Gates
FOREGROUND, teed to /tmp/elemdrop_*_$RANDOM.log; no bootstrap gate needed (no self-host source
is touched). Commit
messages: fix(drop): ... Full integration sweep + runtime_diff = parent's job.

## SELF-HOST STATUS (corrected by review pass 2 — empirical)
The self-host is ALREADY CORRECT for both P1 and P2: pristine driver emits AND wires
`__gorget_dtor_Noisy` for fieldless custom-Drop (elem_drop/val_drop/key_drop all wired across
Vector/Dict/Set), and the composite frees String fields (no P2 leak; the self-host ASan lane is
already clean). Mechanism: lower.gg:3629 (fadb2259) folds every `equip T with Drop` into
resource_types -> lir_lower.gg:5505 block records it in type_drop_fns -> lc_collection_drop_fn
returns `__gorget_dtor_{T}`. Evidence C files: /tmp/elemdrop_probe/selfhost_{pvt,str,full}.c.
INVARIANT-#8 BOOKKEEPING: Rust is the broken side; the self-host is the reference. Fix Rust
toward it (which the proven Phase-R patch does — both then emit `__gorget_dtor_{T}`).

**Phase S (re-scoped): LOCK the self-host's correct behavior, change NO self-host source.**
Add a targeted self-host regression test **modeled EXACTLY on `assert_box_deref_asan_clean`
(tests/integration.rs:20652)**: run the driver with `--emit-c --runtime-dir=<runtime_dir>`
(NOT `--lir-c`, which emits body-only non-runnable C), grep the emitted C for the
`.elem_drop`/`.val_drop`/`.key_drop` → `__gorget_dtor_Noisy` wiring, then cc + run + assert
stdout (and ASan-clean, since that lane already sanitizes). No bootstrap gate needed (no
self-host source changes). The
reader-alignment refactor (migrate lc_collection_drop_fn/clone_fn + emit_dict_ctor_wiring:1935
from recursive_drop_* to type_drop_fns — behavior-preserving, architectural) is DEFERRED OUT of
this track: filed as a LOW self-host-elegance TODO item, gated on byte-identical driver output +
bootstrap if ever done.

## READY-MADE FIXTURE (saved /tmp/scout_elemdrop_fixture.gg)
tests/fixtures/drop_collection_custom_elem.gg — temp/named-move × Vector/Dict-value/Set-key,
Noisy{int id}+custom Drop+@derive(Equatable,Hashable). Expected stdout (from FIXED compiler,
ASan clean):
  drop 0            (hash_of temp — note: fires early; executor may drop the hash_of line if noise)
  A: vector temp
  B: vector named-move
  C: dict value named-move
  D: set key named-move
  done
  drop 40
  drop 30
  drop 20
  drop 10
PRE-FIX this printed ZERO of the numeric drops after "done" (all 4 element drops lost).
Also recommend a SEPARATE fixture with a String field + heap string (leak regression, needs ASan
in the self_host_emit_cc lane or a `--sanitize` gate) since normal integration run doesn't ASan.

## DROP ORDER NOTES (for expected outputs)
- Scope-exit locals drop LIFO by declaration.
- Named local MOVED into a collection (dead after) => does not double-drop (MoveZero).
- `Noisy y = v[i]` CLONES (owned dest, non-Copy elem) => independent drop, no double-free.
- v.pop().unwrap() MOVES the element out => not dropped by the vector.

## BACKENDS
Fix is in shared LIR (src/lir/lower/insts.rs) => BOTH C and LLVM backends get it.
Verified LLVM: probe_vec_temp/named_move/set_key all fire correct drops under --backend=llvm.

============================================================
# DRAFT EXECUTOR BRIEF
============================================================

## Goal
Fix the CLASS: a droppable struct/enum used as a COLLECTION ELEMENT must fire its
correct destructor when the collection drops. Two coupled defects:
  (P1) Custom-drop type with ONLY trivial fields (int/float/bool/ptr): elem_drop NOT
       wired at all => custom drop() silently LOST (fd/lock-leak class).
  (P2) Custom-drop type WITH droppable fields: elem_drop wired to `{T}__drop`
       (USER BODY ONLY) instead of the composite `__gorget_dtor_{T}` => fields LEAK
       (ASan-confirmed).
Fix RUST (the broken side); the self-host is ALREADY CORRECT and is the reference (pass-2
empirical verdict — see SELF-HOST STATUS). Phase S locks it with a regression test.

## Root cause (verified file:line)
- Element drop/clone wiring: `infer_fn_ptr_stores_from_types`, src/lir/lower/insts.rs:2346.
  Old gate per family: `elem_drop_fn_for_type` (Trivial-only, src/lir/lower/types.rs:103)
  OR `recursive_drop_structs/enums.contains_key` (populated only for NON-empty field lists,
  src/lir/lower/mod.rs:616). Neither covers Custom-with-trivial-fields; and the name used
  (`{T}__drop`) is the user body, not the composite.
- The CORRECT unified map already exists: `type_drop_fns` (src/lir/lower/mod.rs:734), keyed by
  type name, `drop_fn_name` = `__gorget_dtor_{name}` (Custom) / `{name}__drop` (Recursive),
  and it RECORDS Custom-with-trivial-fields (skip guard at mod.rs:717 excludes Custom). It's
  threaded as `self.type_drop_fns` (mod.rs:103), directly usable in insts.rs.

## Files
1. src/lir/lower/insts.rs — `infer_fn_ptr_stores_from_types` (Vector/Deque, Dict/HashMap val+key,
   Set/HashSet). ALREADY PROTOTYPED — patch at /tmp/scout_elemdrop_proto.patch (git apply in wt).
   Route the else-if branches through `self.type_drop_fns.get(t).drop_fn_name` (drop) and
   `self.type_drop_fns.contains_key(t)` + `{t}__clone_inplace` (clone). 21 ins / 26 del.
2. tests/fixtures/drop_collection_custom_elem.gg (NEW) + assert in tests/integration.rs (model
   on drop_collections at integration.rs:5338 using run_gg). Ready-made fixture at
   /tmp/scout_elemdrop_fixture.gg; exact expected stdout captured (see fixture section above).
3. tests/fixtures/drop_collection_custom_elem_leak.gg (NEW) — String-field + HEAP string
   (`"aa"+"bb"`) element; the P2 assertion uses the RUST `gg build --sanitize` path in Phase R.
   (The self-host lane is ALREADY ASan-clean for this shape — pass-2 verified; the Phase-S
   regression test locks it.) Normal integration does NOT ASan.
4. SELF-HOST: per "SELF-HOST STATUS" above — NO source changes; ONE new regression test in
   tests/integration.rs locking the driver's correct elem/val/key_drop wiring + runtime output
   for the new fixture (zone: your test fns only).

## Fixture battery (temp / named-move / named-clone × Vector / Dict-value / Dict-key / Set)
Use Noisy{int id} + `equip Drop` (print) + `@derive(Equatable,Hashable)` (needed for Set/Dict-key).
ALSO (pass-1 R4): Dict `.remove()` and Set `.remove()` move-out shapes under ASan (the pop-out
double-drop check extended to the map/set families).
Expected drop counts (measured on the fixed compiler, all ASan-clean):
  - Vector temp / named-move: element fires once at collection drop.
  - Vector named-clone (local live after push): TWO drops (local's own + collection's clone).
  - Dict[str,Noisy] value, Set[Noisy] key, Dict[Noisy,int] key: element fires at collection drop.
  - Enum with custom Drop + trivial payload as Vector element: fires once (edge — include it).
Derive exact expected stdout by RUNNING the fixed compiler (trustworthy now) AND eyeballing order
(scope-exit LIFO; moved-in locals don't double-drop; index-read clones; pop moves out).

## Gates (executor)
- cargo build; cargo test --lib (expect 1101/0).
- cargo test --test integration -- --test-threads=4 drop  (35/35) + the new fixtures.
- Targeted: collection|leak|set_|dict|vector|cow_ slice (use GG_BUILD_TIMEOUT_SECS=600 — DEBUG
  self-host tests time out at the default 120s under load, NOT a regression).
- ASan (`gg build --sanitize`) on the leak fixture: MUST be clean (proves P2).
- Parent runs full integration + self_host_bootstrap_fixed_point + GG_RUNTIME_DIFF=1 sweep.

## Additional deliverable (pass-1 R3, Core #4 rule 3)
An arm-count ratchet in tests/lints.rs (model: `container_literal_arms_count`,
tests/lints.rs:725 — the CLAUDE.md-cited pattern): the collection families in
`infer_fn_ptr_stores_from_types` must route element drop wiring through `type_drop_fns` — budget
the count of legacy `elem_drop_fn_for_type`/`recursive_drop_*.contains` gates in that function at
its post-fix value so a new sibling can't reintroduce the hole. (tests/lints.rs is shared with the
define-gorget track's B2 increment — coordinate via the orchestrator if both are in flight.)

## Non-goals
- Do NOT touch the clone-vs-borrow decision at assignment/consuming positions (works correctly).
- Do NOT rename {T}__drop or __gorget_dtor_{T} conventions.
- Do NOT expand type_drop_fns semantics beyond recording fieldless-custom (self-host) — the Rust
  side already records them; only the READER changes there. (Self-host: no change at all.)

## Bigger-than-it-looks flags
- P2 (field leak) is a SECOND, ASan-only bug; without a sanitized gate it passes silently.
  The gate is a PERSISTENT integration test (Core #6 — not a one-shot manual check), on the
  RUST path, modeled on `assert_box_deref_asan_clean`: `gg build --sanitize <leak_fixture> -o
  <bin>` → run with `ASAN_OPTIONS=detect_leaks=1:abort_on_error=0:exitcode=99` → assert exit 0
  AND no `LeakSanitizer`/`AddressSanitizer`/`SUMMARY:` on stderr. (The self-host ASan lane
  cannot lock a RUST P2 regression — the self-host is already clean either way.)
- Self-host is ALREADY CORRECT (pass-2); Phase S is a regression LOCK only — no source change,
  no DEBUG populate budget.
- The TODO entry was ALREADY corrected by the orchestrator (2026-07-06) — the executor does NOT
  touch TODO.md (zone rule).

## FINAL GATE RESULT
self_host_bootstrap_fixed_point (release, with Rust fix applied): PASS (1 passed / 0 failed, 446s).
=> Rust fix does NOT regress the self-host bootstrap. All scout gates green.

## STATUS: SCOUT COMPLETE 2026-07-06
Rust fix prototyped + fully measured. Self-host fix scoped (not yet applied — brief covers it).
Checkpoints: /tmp/scout_elemdrop_findings.md (this), /tmp/scout_elemdrop_proto.patch (Rust diff),
/tmp/scout_elemdrop_fixture.gg (ready fixture), /tmp/scout_elemdrop/insts.fixed.rs (backup).
