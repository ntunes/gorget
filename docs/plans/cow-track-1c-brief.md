# Executor brief: CoW Track 1C — Dict `d[k].field = x` write-through (both compilers)

> **Status:** v4 — pass-4 folded (M2b's Rust prediction was FALSIFIED by measurement: HashMap
> Rust-side prints 0 — the READ/STORE is broken by the filed element-typing bug, NOT a
> gate-dropped write printing 1; the self-host read shares the mechanism so BOTH-0 symmetric
> is a plausible+acceptable outcome; the STOP trigger is re-scoped to SAFETY only; a HashMap
> probe joins the ASan gate). Pass-3 folded (1 substantive: the self-host producer CANNOT be Dict-scoped —
> no CkHashMap exists; `resource_meta_for` maps `HashMap__`→CkDict (lir_lower.gg:318) and the
> self-host resolves HashMap value types (lower_types.gg:2648), so the `case CkDict()` arm
> writes through HashMap-of-struct while scoped Rust drops the write — a LATENT divergence,
> zero corpus sites. Disposition: DOCUMENT + PROBE + hand to the HashMap track, which closes
> it by fixing Rust — see M2b below. Do NOT add a CkHashMap variant or a name-check here.)
> Pass-2 SIGNED OFF (fold verified coherent+necessary: HashMap falls back to the identical old path, provably not-worse; the two-EXCLUDE mandate confirmed complete across all 7 ggdef test files; N1 comment-wording nit folded). Pass-1 folded (1 blocking: the patch's `Map` gate arm ADMITTED HashMap while
> HashMap-of-struct element typing is broken upstream (pre-existing, now filed HIGH) → the gate
> is SCOPED to `Array|OrderedMap` (Dict only, matching what is proven); the HashMap track owns
> flipping the arm + the fixture shape. Also: ggdef sub-suite count corrected to 7 test files;
> ASan is executor-mandatory — pass-1 could not run it). Awaiting the next fresh pass. **Scout basis (read both FIRST):** `docs/plans/cow-track-1c-scout.md` (measured
> matrix, runtime-layout notes, sibling grep) + the PROVEN patch
> `docs/plans/define-gorget/scouts/patches/cow1c_proto.patch` (both compilers × both backends,
> targeted 183/0, ASan+UBSan clean). **Campaign:** `cow-writethrough-materialize-closed-set.md`
> (v3 — the 1B architecture correction routes self-host work at `lower_field_place_base`).
> **Model policy:** executor + brief-reviews Opus; output-review on Fable.

## Objective

`d[k].field = x` / `d[k].field += n` on a Dict value-struct element silently loses the write on
BOTH compilers (wave-0 gap C, re-measured broken by the scout). Fix it at the two proven
write-place producers, close the live double-eval hazard, and pin with a three-lane fixture.
Set is OUT of scope (no index surface; the checker gap `Set[int] s[0]`-passes-check is filed
separately — do not fix it here).

## Mechanism (scout-proven — the patch is the spec; highlights)

- **Rust:** extend the Index-arm gate in `try_resolve_field_place` at
  `src/ir/lowering/exprs/mod.rs:2582` from `Array` to **`Array|OrderedMap` ONLY** (pass-1: do
  NOT admit `Map` — HashMap-of-struct element typing is broken upstream, filed HIGH; a `Map`
  arm would silently route HashMap field-writes into wrong output. Extend the gate's existing
  unsupported-bases comment (which lists Set) to include HashMap/`Map`, citing the filed
  entry by DURABLE PHRASE — "HashMap-of-struct element typing, methods.rs:3859" — never a
  TODO line number). No new runtime
  symbol: `gorget_map_get` (`runtime_map.c:322`) already returns a pointer into the value slot;
  LIR `IndexLoad` already routes Dict→`gorget_map_get`; `materialize_collection_element`
  returns the raw pointer for a `Ptr(T)` dst. Pure GIR place resolution — C and LLVM inherit.
- **Double-eval MANDATE:** the arm must use the scout's **type-only pre-check**
  (`index_base_kind_type_only`/`place_expr_type_only`) to resolve the collection kind WITHOUT
  lowering, returning `None` before `lower_expr(coll)` for unsupported bases — measured:
  `make()[0].x = 99` calls `make()` once post-fix (twice today).
- **Self-host:** ONE arm in `lower_field_place_base` (`lower_stmt.gg:1606`): the
  `fpb_is_array`-style dispatch gains `case CkDict(): fpb_getter = "gorget_map_get"` (typed
  collection_kind dispatch — never name-matching). Do NOT touch the shared `lower_place_base`
  (the 1B receiver-hijack lesson).
- Nested `d[k].inner.f` becomes a bonus-fix on Rust (recursion) but stays broken on the
  self-host — that asymmetry is 2F's class: REPORT it in the fixture comment, do not chase it.

## Milestones

1. **M1** — adopt the proven patch (`git apply --check` first; re-read hunks on drift), THEN
   **remove the `Map` arm from the gate** (the patch predates the pass-1 scoping — the landed
   gate is `Array|OrderedMap` only, with the one-line comment citing the filed HashMap entry).
   Checkpoint `/tmp/recover_cow1c_exec_1.patch`.
2. **M2 — fixture**: `tests/fixtures/cow_dict_index_field_writethrough.gg` (+ runtime snapshot)
   covering plain / compound / String-key shapes (scout's probes: 99 / 41 / 99). Expected
   output is **prose-derived from §3.1 with an explicit out-of-subset note** in the fixture
   comment (ggdef's `navigate_write` has no Map arm — scout re-verified from source). Wire the
   standard `run_gg` test (self-host lane auto-enrolls). **MANDATORY (the 1B corpus lesson):
   add the fixture to the `EXCLUDE` lists in BOTH `spec/ggdef/tests/corpus_b.rs:~37` AND
   `corpus_b1.rs:~35`** with the documented out-of-subset reason (Dict write-place not
   elaborable), mirroring the 1B fixture's entries. Verify not gitignore-hidden; run `gg fmt`
   idempotence on it.
2b. **M2b — the HashMap divergence probe + documentation (pass-4-corrected):** run
   `HashMap[int, Point] h; h[0] = Point(1,2); h[0].x = 99; print(h[0].x)` through BOTH
   compilers post-fix and record the outputs. **Measured reality (pass-4): Rust prints 0 —
   the HashMap-of-struct READ/STORE is itself broken by the filed element-typing bug
   (methods.rs:3859), independent of the scoped gate** (a store-then-read with NO field
   write also prints 0 while the Dict control prints the field). The self-host read shares
   the `index_value_type_name` mechanism, so **"both print 0" is a plausible and ACCEPTABLE
   outcome** — as is a genuine asymmetry (self-host writes through via CkDict). Record
   WHATEVER prints, in the FIXTURE COMMENT (never a fixture assertion — no behavior is
   pinned), attributed accurately (broken read/store, NOT "gate-dropped write"), citing the
   HashMap TODO entry that owns convergence by fixing Rust. **STOP only if the probe CRASHES
   or trips ASan on either compiler** — a surprising print value is data to record, not a
   stop condition.
3. **M3 — double-eval regression test**: a side-effecting-base probe wired as a test (the
   scout's `make()[0].x` shape — assert the producer runs once). If the natural home is an
   inline Rust test, that's fine; name it so the eval-order class is greppable.
4. **M4 — gates (FOREGROUND; chunk >600s gates by test name):** `cargo build` ·
   `cargo test --lib` · targeted `cow_/dict_/hashmap_/index_` filters on **C AND LLVM**
   (scout baseline: 183/0) · self-host driver rebuild (GG_BUILD_TIMEOUT_SECS=600) + the three
   Dict probes + the 1B array fixture (no regression) · `self_host_runtime` targeted ·
   **ASan+UBSan** on the new fixture + dict-of-vector/nested-array regressions + the M2b
   HashMap-of-struct probe (the CkDict arm now fires for HashMap on the self-host) ·
   **FULL `cargo test -p ggdef`** (all 7 test files — corpus_b/b1 brick without the EXCLUDE
   entries) · `lower_comparison`/`type_comparison` counts. The bootstrap + full sweeps +
   parity are the PARENT's (this fix's blast is the write-place producers only — no receiver
   rerouting, unlike 1B).

## Out of scope

Set (filed checker gap); nested self-host shapes (2F); `for x in &coll` (1A); the shared
`lower_place_base`; spectests/.

## Process contract (non-negotiable)

Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside your worktree. NEVER touch
`/workspace/gorget` or `/workspace/gorget-1`; worktree-relative paths only (worktrees nest
UNDER main). NEVER `git stash`; checkpoint to /tmp per milestone. Stage by EXPLICIT file name.
Edit-tool desync → re-Read + retry, never a heredoc with an absolute path. Transient cargo
errors under contention: retry. Commit when green
(`fix(cow): 1C — Dict value-element field write-through (Index place arm Array|OrderedMap +
type-only pre-check kills double-eval)`), trailers: Co-Authored-By Claude Opus + the Claude-Session line.
Report any NEW pre-existing bug (file-don't-fix).

## Acceptance

Dict field stores write through on C + LLVM + self-host (99/41/99); `make()` runs once; 1B
fixture unregressed; full ggdef suite green (corpus EXCLUDEs in place); ASan/UBSan clean;
zero changes outside the two producers + fixture/test wiring + the two EXCLUDE lists.
