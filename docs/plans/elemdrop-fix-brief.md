# EXECUTOR BRIEF: collection-element destructor fix (P1 drop-lost + P2 field-leak), BOTH compilers

> **STATUS: v1 (scout draft, adopted by orchestrator 2026-07-06) — review passes cleared: 0 of >=3
> (update per pass). Executor: not launched.**
> Scout artifacts: /tmp/recover_elemdrop/ (findings, measured prototype patch, ready fixture).
> This brief was drafted by the scout that prototyped and MEASURED the Rust fix end-to-end
> (probe matrix, ASan, bootstrap 446s PASS); reviews verify, executor lands.

## Operational rules
Worktree preamble verbatim (CLAUDE.md): pwd + git rev-parse first, inside YOUR worktree; never
touch /workspace/gorget or /workspace/gorget-1; worktree-RELATIVE paths; never git stash
(checkpoint via git diff > /tmp/elemdrop_exec_state.patch); Edit-desync -> re-Read + retry;
non-Edit writes -> check main status and STOP on surprises. Zones: src/lir/lower/insts.rs,
tests/fixtures/drop_collection_custom_elem*.gg (NEW), tests/integration.rs (your test fns only),
tests/fixtures/self_host_lowerer/{lir_lower,lir_codegen}.gg. NEVER: TODO.md, docs/**, spec/**
(concurrent ggdef track). TWO PHASES, commit each: Phase R (Rust fix, from the proven patch) then
Phase S (self-host mirror). Gates FOREGROUND, teed to /tmp/elemdrop_*_$RANDOM.log; Phase-S final
gate = self_host_bootstrap_fixed_point (release, GG_BUILD_TIMEOUT_SECS=600, foreground). Commit
messages: fix(drop): ... Full integration sweep + runtime_diff = parent's job.

## SELF-HOST FIX SHAPE (mirror, for the brief)
Rust fix was a pure READER change because Rust type_drop_fns already records Custom-fieldless.
Self-host needs TWO changes:
1. lir_lower.gg:5505 — also record fieldless custom-Drop structs in type_drop_fns with
   drop_fn_name={T}__drop (no fields => user body is the whole drop; __gorget_dtor_{T} is NOT
   emitted for fieldless so must NOT be the name). Verify emit_type_drop_fns/emit_struct_drops
   don't double-define {T}__drop (fn_exists guard should cover it).
2. lir_codegen.gg lc_collection_drop_fn (1970) + lc_collection_clone_fn (2016): replace the
   `recursive_drop_structs.contains OR recursive_drop_enums.contains` gate (lines 1999 / 2044)
   with `m.type_drop_fns.contains(type_name)` and use
   `m.type_drop_fns.get(type_name).unwrap().drop_fn_name` for the drop name (clone stays
   type_name+"__clone_inplace"). This fixes BOTH the missing-elem-drop and the user-body-only
   field-leak in the self-host.

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
Fix BOTH the Rust gg AND the self-host (invariant #8 — both are identically broken).

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
   (`"aa"+"bb"`) element; wire it into an ASan lane (the self_host_emit_cc lane already ASans, or
   add a `--sanitize` assertion) to lock P2. Normal integration does NOT ASan.
4. SELF-HOST (see "SELF-HOST FIX SHAPE" above):
   - tests/fixtures/self_host_lowerer/lir_lower.gg:5505 — also record fieldless custom-Drop
     structs in type_drop_fns (drop_fn_name = `{T}__drop`).
   - tests/fixtures/self_host_lowerer/lir_codegen.gg:1970 (lc_collection_drop_fn) + :2016
     (lc_collection_clone_fn) — replace the recursive_drop_* gate with `m.type_drop_fns` lookup;
     drop name from `type_drop_fns.get(t).unwrap().drop_fn_name`.
   - Verify emit_type_drop_fns / emit_struct_drops don't double-define `{T}__drop` for the newly
     recorded fieldless types (fn_exists guard should already cover it — CONFIRM).

## Fixture battery (temp / named-move / named-clone × Vector / Dict-value / Dict-key / Set)
Use Noisy{int id} + `equip Drop` (print) + `@derive(Equatable,Hashable)` (needed for Set/Dict-key).
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

## Non-goals
- Do NOT touch the clone-vs-borrow decision at assignment/consuming positions (works correctly).
- Do NOT rename {T}__drop or __gorget_dtor_{T} conventions.
- Do NOT expand type_drop_fns semantics beyond recording fieldless-custom (self-host) — the Rust
  side already records them; only the READER changes there.

## Bigger-than-it-looks flags
- P2 (field leak) is a SECOND, ASan-only bug unmasked by the same wiring; without an ASan lane it
  passes silently. Ensure a sanitized assertion exists.
- Self-host needs a populate change too (not a pure reader change like Rust) — budget DEBUG time.
- The TODO entry's diagnosis (named-local vs temp) is WRONG; the real axis is field triviality.
  Update/replace the TODO entry when landing.

## FINAL GATE RESULT
self_host_bootstrap_fixed_point (release, with Rust fix applied): PASS (1 passed / 0 failed, 446s).
=> Rust fix does NOT regress the self-host bootstrap. All scout gates green.

## STATUS: SCOUT COMPLETE 2026-07-06
Rust fix prototyped + fully measured. Self-host fix scoped (not yet applied — brief covers it).
Checkpoints: /tmp/scout_elemdrop_findings.md (this), /tmp/scout_elemdrop_proto.patch (Rust diff),
/tmp/scout_elemdrop_fixture.gg (ready fixture), /tmp/scout_elemdrop/insts.fixed.rs (backup).
