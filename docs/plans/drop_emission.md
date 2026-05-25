# Drop Emission — Self-Host Plan (unified)

**Status (2026-05-25, HANDOVER):** IN PROGRESS — cc-clean, **not yet bootstrapping.** WIP is
**COMMITTED** on branch `gorget-1` (2 ahead of `main`@`087b5a13`: `758ed737` = code WIP,
`63c327f7` = handover docs; squash at merge-to-main). Stage-1 now emits the full stage-2 C (**616363 lines, ~567s** — use a ≥2400s
timeout; the earlier "hang" was a 600s-timeout artifact on a loaded box, NOT a bug). The live
blocker has moved into **stage-2 lowering**: an **`EBinaryOp` infinite recursion** (see NEXT BUG).
Fixed + committed this run: **bug #1** (None/`NO_NAME` lowered as 8-byte `OpConstUnit()` →
add_local drop-clone over-read; now a typed `IEnumInit(tag=1)` None) and **bug #2** (`EArrayLiteral`
push receiver `OpMove`→`OpBorrow`). **SHIP-GATE (no name-matching):** before this cluster ships
green, the consuming-position name-match (`is_owning_mutator_arg`) MUST become a typed signal —
see guardrail #5; the self-host is to be BETTER than Rust here, not a mirror.

This is the single authoritative plan for self-host drop emission. (It absorbed the former
`consumer_audit.md` empirical log and `drop_emission_completion.md` strategic plan — both deleted
2026-05-25; their durable content is folded into "History" and "Deferred optimizations" below.)

**Goal:** ship full architectural drop emission to the self-host frontend so
`self_host_bootstrap` + `self_host_bootstrap_fixed_point` pass with REAL drops (not labels-only),
at parity with the Rust reference compiler (`tests/fixtures/self_host_lowerer/driver.c`).

**Guiding principle (from the user):** *Only owners free their resources. Collections, `Option`,
and `Result` own their contents and free them recursively on drop (see `docs/book/11-ownership.md`).
Make it correct first, optimize later — zero EVERY move for now; don't port Rust's selective
zeroing yet.* Invariant everywhere: **every `OpMove` of a resource pairs with a `GIMoveZero` of
its source; consuming from a non-owner (borrow/view) CLONES; a field/element that is DROPPED must
also be CLONED (drop-without-clone on a shallow copy = double-free).**

---

## Shipped this run — verified present in the tree; do NOT redo

(All confirmed by a fresh review agent against the live working tree, 2026-05-25.)

1. **STEP 1 / Option C** — collection-mutator value/key args own, typed via `collection_kind`:
   `collection_kind_of` + `is_owning_mutator_arg` (`lower.gg`), used at the method-call value-arg
   loop.
2. **OpClone-at-by-pointer materialization** (the deferred "Site 2"), `lir_lower.gg` OpClone
   need_ptr branch: clone → fresh temp slot → pass its address.
3. **a-5 clone-on-borrow** — `op_consume` `LoBorrowed`/`LoView` at a consume position → `OpClone`
   (`lower.gg`).
4. **Option/Result-of-resource = clone-needing WITHOUT an ABI flip** — new ABI-inert
   `GirModule.optionlike_resource_types` (`gir.gg`), populated by `register_optionlike_resource`,
   read by `is_resource_type_name`; clone/drop fns generated via `lir_lower` Pass 3 +
   `drop_fn_for_type`'s 4th `optionlike` param.
5. **Prelude variant ctors `Some`/`Ok`/`Error` own their payload** — registered in
   `fn_move_params` after the Fix-D loop (prelude variants aren't in `m.items`, so Fix-D missed
   them; without this `Some(x)` borrowed its payload → alias). Fixed UAF #1 (`parse_equip_item`).
   Use **typed** `Vector[bool]` locals for the flag vectors (a bare `[true]` literal gets
   elem_size 8 → 1-byte-bool push overflow).
6. **Choice A: Option params by-pointer** (Rust-parity) — both param-wrap sites use
   `is_resource_type_name`; the temporary `is_byref_resource_param` (choice B) is DELETED.
7. **Variant-ctor result typing** — `lower_call` types `Some(x)` as `Option__<inner>` using the
   LOWERED ARG's local type (`first_arg_type`), gated to non-primitive inners (a `Box.new()` arg
   currently mistyped as I64 keeps legacy typing — an orthogonal gap).
8. **Box[T] field deep-clone** (THE `meta_expand` double-free fix) — `field_clone_c`
   (`lir_codegen.gg`) emits Rust's `field = __gorget_box_alloc_<I>(*(<I>*)field);
   <I>__clone_inplace(field);` at all 4 field-clone sites; `box_alloc_for_field_type` generates
   the box-inner allocators the ICall scan misses (e.g. `SpannedPattern`). Box detected via the
   typed `type_runtime_map == "Box"`.

---

## Validation loop (ASan is the tool — one run = alloc site + BOTH free stacks)

**Turnaround budget (so you don't mistake slow for hung):** ~9 min stage-0 build + ~567s emit +
~30s cc/ASan-build + ASan run = **~25–50 min to first signal**, more on a loaded box. Run ONE heavy
thing at a time. Paths below are the MAIN worktree (`/workspace/gorget-1`) with its prebuilt
`driver`; a fresh worktree agent must build stage-0 first (the `gg build` line does this).

```bash
cd /workspace/gorget-1
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg   # rebuild stage-0 (~9 min)
OUT=/tmp/s1.c
timeout 2400 stdbuf -oL ./tests/fixtures/self_host_lowerer/driver \
    tests/fixtures/self_host_lowerer/driver.gg lib --emit-c > "$OUT"                              # emit ~567s — USE ≥2400s: 600s gave a FALSE "hang" on a loaded box; stdbuf -oL MANDATORY
python3 -c "r=open('tests/fixtures/self_host_lowerer/driver.c').read();i=r.find('\ntypedef struct __gg_');open('/tmp/s1full.c','w').write(r[:i]+'\n'+open('$OUT').read())"
cc -O0 -w -o /tmp/s1bin /tmp/s1full.c -lm -lpthread                                               # plain cc: ABI/type/link errors
cc -O0 -g -fsanitize=address -w -o /tmp/s1asan /tmp/s1full.c -lm -lpthread                        # ASan build
ASAN_OPTIONS=abort_on_error=0:detect_leaks=0 timeout 3000 /tmp/s1asan \
    tests/fixtures/self_host_lowerer/driver.gg lib --emit-c > /tmp/stage2.c 2>/tmp/asan.out       # ASan is ~2-3x slower; bug #3 crashes early (~80s) but give headroom
```

Supporting tools: `--emit-gir` / `--emit-lir` IR dumps (trace a value to its write site —
`awk '/fn @<MangledName>\(/{f=1} f{print} f&&/^}/{exit}' /tmp/gir.txt`), and the drop-count
grep-diff against the Rust reference to watch parity climb:

```bash
RUST=tests/fixtures/self_host_lowerer/driver.c
for p in '__drop(' 'gorget_string_free(' 'gorget_array_free(' 'gorget_map_free('; do
  printf '%-22s rust=%s self=%s\n' "$p" "$(grep -cF "$p" $RUST)" "$(grep -cF "$p" $OUT)"; done
# Rust targets (approx, drift as self-host source grows): __drop 2358 · string_free 5340 · array_free 544 · map_free 102
```

---

## NEXT BUG — bug #3: `EBinaryOp` infinite recursion in stage-2 lowering (START HERE)

(Bugs #1 + #2 are FIXED + committed in `758ed737` — see Status. The old add_local/`NO_NAME`
overflow WAS bug #1; do not re-chase it.)

Running the **ASan stage-2 binary** (the self-host compiled by itself) on the source
stack-overflows in `lower_expr`:
```
#0 lower___lower_expr  /tmp/s1full.c:199931      (fn prologue — huge ~148KB frame overflows the stack)
#1..#N lower___lower_expr /tmp/s1full.c:211580   (the SAME call site, every frame — the lhs recursion)
```
Source: `lower_expr`'s `case EBinaryOp(lhs_box, op, rhs_box)` (lower.gg:~3859) → `lower_expr(*lhs_box)`.
It never bottoms out. While lowering **`derive___field_write_lines`**.

**Observed in the 2026-05-25 ASan session (regenerate to re-confirm — these are gdb observations,
not re-runnable facts; the `/tmp` file no longer exists):**
- The deref looked correct: `__s270` = lhs `Box__SpannedExpr`; `memcpy(&__s344, __s270, 192)` = a
  `*box` (192 ≈ sizeof `SpannedExpr`, unverified by exact size).
- `Expr__clone` case 9 (EBinaryOp) DEEP-clones correctly — `__gorget_box_alloc_SpannedExpr(*box)`
  (fresh box + copy) then `SpannedExpr__clone_inplace`. So **`Expr__clone` is not the culprit.**
- The recursing node looked the SAME at every depth (identical operator-`String` `.alloc` ptr) ⇒
  two AST nodes **sharing one `Box[SpannedExpr]`** (a shared-box alias), which presents in gdb
  exactly like a "cycle."

**LEADING HYPOTHESIS — a drop-emission WIP change created a shared-box alias (NOT the parser).**
Strong evidence this is the WIP, not upstream: (a) the WIP touched ZERO parser/AST files
(`git diff --stat f15a45c6..HEAD -- '*parser*' '*ast*'` = empty); (b) the **labels-only** baseline
`f15a45c6` has a *structurally identical* `lower_expr` EBinaryOp arm recursing on
`lower_expr(*lhs_box)` and it **bootstrapped fine** — same parser, same recursion. If the parser
built a cyclic AST, labels-only would have stack-overflowed identically. It didn't. So the cycle is
a **runtime aliasing bug introduced by this WIP's clone/move/drop path** — most likely a
`Box[SpannedExpr]` field consumed WITHOUT a clone or `GIMoveZero`, leaving two nodes pointing at one
box. This is the SAME failure mode as UAF #1, the array-literal clone, and the add_local over-read
(all looked like other things first; all were consume/move/clone decisions). The earlier "built
upstream / pre-existing / orthogonal" framing was WRONG — it contradicted the labels-only evidence.

**NEXT — cheapest decisive experiment FIRST (do this before any gdb pointer-chasing):** reproduce
with labels-only and see if bug #3 disappears. Either `git stash`/revert the WIP's `lower.gg` +
`lir_*.gg` deltas (keep gir.gg if needed to compile), OR build the self-host at `f15a45c6`, emit,
ASan-run. If the EBinaryOp recursion does NOT reproduce labels-only → confirmed it's the WIP; then
**bisect which WIP change introduces it** (prime suspects: a-5 clone-on-`LoBorrowed`, the
prelude-variant-owning change, or a missing `GIMoveZero` on a moved `Box[SpannedExpr]` field at a
consume/construction site). Only if it DOES reproduce labels-only is the parser/transform in play
(secondary). To re-observe the alias: regenerate the stage-2 C (loop below), build a `-g` driver
(`gg build … --emit-c-lir` > a `.c`, then `cc -O0 -g`), run, `gdb -p <pid>` the hung process; the
durable code anchors are `lower.gg:~3859` (EBinaryOp arm) and fn `derive___field_write_lines` — NOT
`/tmp` line numbers (the generated file is regenerated each run, so any `/tmp/s1full.c:NNNNN` is
non-reproducible).

---

## Then: STEP 2 loop → ship

Re-run the validation loop after each fix; each remaining fault is another unclean
move/borrow/clone or a slot/arg mismatch — apply the invariants. When stage-1 emits a full
stage-2 and runs to exit 0, run the gates and **ship the whole cluster as ONE squashed commit**
(partial states crash — that is why the historical E.1 attempts failed):

```bash
cargo test --test integration --release self_host_bootstrap -- --test-threads=1
cargo test --test integration --release self_host_bootstrap_fixed_point -- --test-threads=1
cargo test --lib --release            # ~1059/1061 baseline
cargo test --test integration --release lowerer_comparison
```
On ship, copy the "Deferred optimizations" list below into `TODO.md` so it outlives this doc.

---

## Invariants & guardrails (do not violate)

1. **Only owners free.** Owned AND dead-at-this-call → MOVE (+ `GIMoveZero` source); borrow/view,
   OR owned-but-live-past → CLONE. A field/element that is DROPPED must also be CLONED.
2. **Make it work first; zero every move (for now).** Do NOT port Rust's selective-zeroing yet;
   `drop_elab` elides the redundant runtime check.
3. **Runtime ABI is pointer-correct** for collection-mutator value args (`needs_ptr_arg` on the
   mapped runtime name) AND, now, for Option/Result params (choice A). An `OpMove` on a
   struct-value slot is address-taken cleanly; the only trap is a value source whose slot is
   `LT_PTR` (`.get()`-derived) — that needs CLONE.
4. **Drop emission is unconditional:** `GIDropIfAlive` for every droppable owned local at scope
   exit, never gated on `maybe_moved`; `drop_elab` handles elision.
5. **No name-matching for routing** beyond the documented contracts (the `Box__`/`Option__`/
   `Result__` runtime mangling and prelude-variant id `Some`/`None`/`Ok`/`Error` — these STAY,
   they're fixed language contracts). **SHIP-GATE (user directive 2026-05-25):** the
   `push/put/set/insert/add/send` consuming-method **name-match** (`is_owning_mutator_arg`, STEP
   1/Option C) is a TEMPORARY scaffold — it MUST be replaced by a typed per-method /
   per-`collection_kind` consuming signal **before the cluster ships green**. The self-host is to
   be BETTER than the Rust impl (whose name-match removal is a separate TODO), not a mirror of its
   smell. The swap is output-NEUTRAL (same consume decisions → no `bootstrap_fixed_point` churn),
   so: make it WORK first (drop emission green), then de-smell as the final pre-ship cleanup — do
   NOT let it crystallize. ("Output-neutral" is the expectation, NOT a proof — VERIFY it by running
   `bootstrap_fixed_point` after the swap; the name-match is already `collection_kind`-gated
   (`lower.gg:~488`) so a typed signal keyed on the same kind *should* agree, but confirm. Note the
   analogous Rust name-match was genuinely UNSOUND on `ByPtr` borrow-params — see DONE.md `07649296`
   / `c2810559` — the self-host dodges that only via the kind gate.)
6. **`stdbuf -oL`** on every `--emit-*` run, or block-buffering masquerades as a hang.

---

## Deferred optimizations — DO NOT FORGET (correct first, then these)

File these into `TODO.md` when the cluster ships green:
1. **MoveZero elision** — we emit `GIMoveZero` for EVERY `OpMove`; let `drop_elab`'s dataflow
   drive selective elision (skip the zero where the slot is provably dead) → fewer instructions,
   faster emit.
2. **Conservative clone where Rust would move** — tighten last-use analysis so more consume sites
   move instead of clone, toward Rust's stage-1 clone-call parity. Track via the drop-count
   harness.
3. **Clone-emission layer** — Rust emits the clone inline at GIR; the self-host labels `OpClone`
   at GIR and materializes at LIR. Functionally equivalent; reconcile (self-host-as-showcase) or
   keep the split, but note it can cause GIR-shape diffs vs Rust.
4. **Perf: the ~510s emit** — profile (per `feedback_perf_hunt_playbook`); suspect O(n²)
   drop-type resolution / block-instruction append as drop count grows.
5. **`fixed_point` N back to 2 + retire Phase-F.2 band-aids** — `add_local_inheriting` /
   `inherit_borrow_from` were workarounds; audit-and-retire once real move+clone+drop is in, then
   tighten `bootstrap_fixed_point` from N=5 to N=2.
6. **Retire stale docstrings** — e.g. `lower.gg`'s `decide_operand_at_consuming_arg` still says
   "dead code in this commit. No caller exists" — it IS called now. Sweep for similar.

---

## History (concise)

The "labels-only" self-host bootstrapped cleanly at `f15a45c6` (2026-05-22). The drop-emission
push since then: a Path-A series (C.1 BorrowOrigin, A.1/A.2 Ptr-typed field-load dsts, B.1
SVarDecl 7-branch, D.1 LIR Ptr-aggregate-store, Phase 2.3 clone-through-Ptr) shipped as discrete
commits; the keystone "E.1" (scope-exit drop emission + `lower_return` MoveZero) failed ~9 times
because the coupled double-free cluster was shipped in partial states. The breakthrough was
treating it as ONE atomic cluster and driving each fault to root with ASan: the OOM (14.4 GB → 1
MB), the pointer-cast cascade (2780 → 0), user-type drops (0 → hundreds), the return-path and
enum-variant-ctor double-frees, and finally (2026-05-25) the prelude-variant-owning UAF, choice-A
Option ABI, variant-ctor typing, and the Box-field deep-clone that closed the `meta_expand`
double-free. Remaining: the `lower_function` `add_local` slot/arg mismatch above. The full
blow-by-blow is in `git log` and `DONE.md`.
