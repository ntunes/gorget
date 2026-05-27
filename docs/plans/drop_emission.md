# Drop Emission — Self-Host Plan (unified)

> **SUPERSEDED 2026-05-27 — READ "## NEXT STEPS" FIRST.** s1bin now BUILDS CLEAN and no longer segfaults
> (Option[Ref] Phases 1-6 + Layers 1-6 + Gap A `bdc5b537` + Layer 9 `dac39a64`). bug #3b's `generate_c`
> `.get()`-clone is **RESOLVED** (`compute_reachable_fns` borrows via `gorget_array_safe_get`). **The live
> blocker is now a RESIDUAL clone-accumulation OOM in the LOWERING phase (`lower_module`), NOT generate_c
> and NOT the lexer.** The "Live blocker = bug #3b … generate_c" wording in the 2026-05-26 status below is
> STALE — see "## NEXT STEPS" for the current state, first-diagnostic, and repro.

**Status (2026-05-26):** IN PROGRESS — cc-clean, **not yet bootstrapping.** WIP is **COMMITTED** on
branch `gorget-1`. **Branch state (verified 2026-05-26): `main` is at `7cc7a101` and ALREADY contains
the drop-emission cluster UN-SQUASHED (`758ed737` → `19f90339` → `7cc7a101`); `gorget-1` is a few docs-only commits ahead (this session's status/handover doc commits).** ⚠ The cluster landed on `main` before bootstrap is
green — so the "squash the cluster as ONE commit at merge-to-main" ship plan (NEXT STEPS / guardrails)
is now describing a state that partially already happened; reconcile next session (the squash, if still
wanted, would be a `main` history rewrite, not a side-branch merge). Flag to the user. **Live blocker = bug #3b** (a
heap clone-OOM in `lir_codegen.gg::generate_c` — `while`-loop `.get().unwrap()` deep-clones whole
`LirFunction`s; ~11 GB). It is the SAME `.get()`-aggregate-clone class as bug #3, whose lower-phase
instance (for-element / discovery walkers) is **FIXED + verified in `7cc7a101`** (CoW borrow
for-element + read-only match-destructure → `lower_module` completes, `lowerer_comparison` green). See
NEXT BUG (STATUS) + NEXT STEPS for the bug #3b fork (A reference-grade `.get()`-borrow + clone-on-mutation
port vs B surgical borrow-accessor). Also committed: `19f90339` (Box/MutPtr clone-through, orthogonal).
Earlier this run: **bug #1** (None/`NO_NAME` 8-byte `OpConstUnit()` → typed `IEnumInit(tag=1)`) and
**bug #2** (`EArrayLiteral` push receiver `OpMove`→`OpBorrow`), both committed in `758ed737`. **SHIP-GATE (no name-matching):** before this cluster ships
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
9. **CoW borrow for-element + read-only match-destructure** (`7cc7a101`, bug #3 lower-phase) —
   `for x in coll` and non-owning `match` destructures bind the element as a BORROW, not a clone.
   Both clone sites suppressed for for-elements: `coll.get()`→`Option[T]` wrap
   (`emit_void_ptr_option_wrap`, `lir_lower.gg:2090`) and `emit_payload_read`, keyed on the typed
   `BoCollectionElement(coll≥0)` tag (`gir_local_is_for_element`); String/Closure keep eager clone
   (value-slot consume ABI). Read-only walkers 7–19 clones→0–1; `lower_module` completes;
   `lowerer_comparison` green. (Was the lower-phase half of bug #3; bug #3b is the codegen half.)

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
grep-diff against the Rust-compiled stage-0 reference (`driver.c` = the C the Rust `gg` emits for
the self-host frontend — NOT a separate "Rust compiler"; it's the parity target for drop counts)
to watch parity climb:

```bash
RUST=tests/fixtures/self_host_lowerer/driver.c
for p in '__drop(' 'gorget_string_free(' 'gorget_array_free(' 'gorget_map_free('; do
  printf '%-22s rust=%s self=%s\n' "$p" "$(grep -cF "$p" $RUST)" "$(grep -cF "$p" $OUT)"; done
# Rust targets (approx, drift as self-host source grows): __drop 2358 · string_free 5340 · array_free 544 · map_free 102
```

---

## bug #3 (lower-phase clone-OOM) — FIXED `7cc7a101`; (2026-05-27) live blocker is a RESIDUAL lower-phase OOM in `lower_module` — see "## NEXT STEPS"

> **⚠ THIS "START HERE" BLOCK IS 2026-05-26 AND ITS "live blocker = generate_c" IS STALE. The current
> START-HERE is "## NEXT STEPS" (2026-05-27):** generate_c's `.get()`-clone is RESOLVED (Option[Ref] →
> `compute_reachable_fns` uses `gorget_array_safe_get`); Gap A (`bdc5b537`) + Layer 9 (`dac39a64`) landed;
> s1bin builds clean + no segfault. The live blocker is now a RESIDUAL clone-accumulation OOM in the
> LOWERING phase (`lower_module`), NOT generate_c. Read the rest of THIS block only for the (resolved)
> bug #3 record.

> **STATUS (2026-05-26, end of session — SUPERSEDED, see banner above):**
> - **bug #3 (the lower-phase clone-OOM described in this section) is FIXED + VERIFIED in `7cc7a101`**
>   ("CoW borrow for-element + read-only match-destructure"). `for x in coll` and non-owning `match`
>   destructures now bind the element as a BORROW, not a clone (the producer was *two* clones —
>   `coll.get()`→`Option[T]` wrap at `lir_lower.gg:2090` AND `emit_payload_read` — both skipped for
>   for-elements via the typed `BoCollectionElement(coll≥0)` tag). Read-only walkers: 7–19 clones → 0–1;
>   `lower_module` now runs to completion (was OOM 14.5 GB). `lowerer_comparison` GREEN (re-verified);
>   `cargo test --lib` 1059/2-pre-existing-fail. Committed on `gorget-1`.
> - **LIVE BLOCKER is now bug #3b** — the SAME `.get()`-aggregate-clone class at a different idiom
>   (manual `while` loop). With bug #3 fixed, stage-1 runs through lower→lir-lower→ssa→drop-elab and
>   OOMs (~11 GB) in **`lir_codegen.gg::generate_c`**: `compute_reachable_fns` (`lir_codegen.gg:902`)
>   and `emit_func_forward_decls` (`:1414/1419`) do `m.functions.get(i).unwrap().name` in `while`
>   loops — each `.get().unwrap()` deep-clones the WHOLE `LirFunction` to read one field. (Body loop
>   `:5193` has the same `.get().unwrap()` clone.) See "## NEXT STEPS" for the fix fork.
> - The section below (DIAGNOSIS CORRECTED block + the superseded A.2/recursion analysis) is the
>   RECORD of how bug #3 was diagnosed; it's resolved — read it only for context.

(Bugs #1 + #2 are FIXED + committed in `758ed737` — see Status. The old add_local/`NO_NAME`
overflow WAS bug #1; do not re-chase it.)

> **DIAGNOSIS CORRECTED (2026-05-26, STEP A executed in a worktree + independently verified) —
> everything below this block about "A.2 root / unbounded `lower_expr` recursion / ~13k-deep `+`
> chain / complete the A.2 recovery" is REFUTED. Read this; ignore the rest of the section except as
> a record of how we got here.**
>
> **What it actually is:** a **heap allocation blow-up**, NOT a stack/recursion problem. At the tip,
> `lower_expr` recursion depth stays **< 100** (~30k calls), while RSS climbs monotonically to
> **~14.5 GB** driven by **`SpannedExpr__clone` called 10M+ times**, then OOM-SIGKILL. The driver is
> the read-only generic-discovery walkers **`discover_generic_calls_{stmts,stmt,expr}`**
> (`lower.gg:7860` / `7805` / `7762`), which take their AST args **BY VALUE**
> (`Vector[Stmt] stmts`, `Stmt stmt`, `SpannedExpr sexpr`) and deep-recurse (`*callee_box`, `*lhs`,
> `*rhs`, `*inner`, `*cond`, …). Under the **clone-on-consume regime** installed *after* A.2
> (`bac24e49` Phase 2.3 clone-through-deref / a-5), the `for stmt in stmts` element-extract and the
> by-value recursive args now deep-clone the whole subtree at every descent — verified: the emitted C
> for each walker contains 7 `Stmt__clone`/`SpannedExpr__clone`/`Expr__clone` calls. `lower_module`
> runs these over every function body (+ a transitive fixpoint re-walk), so across the self-host's
> ~1000+ functions the per-descent clone compounds to O(tree²)+ → 10M+ clones → OOM.
>
> **Why the earlier framing was wrong (verified):** (1) the `git bisect` first-bad `2e544e84` (A.2)
> is a DIFFERENT, since-fixed bug — at A.2 stage-1 **hangs in `loader___load_imports`** (loader.gg:560)
> and **never reaches lowering** (zero AST clones; `*box` deref is a shallow `memcpy(192)`). A.2
> predates the clone-on-consume regime (`bac24e49` is newer), so it categorically cannot be the
> clone-OOM. The bisect found *first-breakage*, exactly as the (now-removed) STEP A.0 gate warned.
> (2) "Unbounded recursion / ~13k-deep chain" was a **call-count↔depth conflation** (the ring-log
> `__ri≈13k` counted total `lower_expr` calls; depth was <100) **plus an ASan artifact** — my
> stack-overflow signal came from running an **ASan-instrumented** stage-2 (inflated frames hit the
> 8MB stack first), whereas real `self_host_bootstrap` compiles stage-2 with **plain `cc -O0`**, where
> the failure is the heap-OOM. NOT a cycle, NOT a UAF, NOT corruption, NOT the `Box` ctor.
>
> **FIX (verified-correct target): make the read-only walkers BORROW their AST args** (CoW-default-
> borrow — they never mutate). Change `discover_generic_calls_{stmts,stmt,expr}` to take `&` params,
> and ensure the `for`-element extract over a borrowed collection + the `*box` recursive derefs
> propagate Ptr aliases at zero cost (mirror `lower_for`'s `OpBorrow(coll_local)` path, NOT
> `emit_payload_read`'s auto-clone). Reconcile with the clone-on-consume arms so the `&`-param +
> for-element path does NOT route through `OpClone` (that path is correct only at genuine *owning*
> consume positions; a read-only walk is not one). See "## NEXT STEPS" below for the worked plan.
>
> **On the committed `Box`/`MutPtr` fix `19f90339`:** it changed the `GtMutPtr`(`&`/`!`-param) consume
> arm; the walkers use BARE (`GtPtr`) params, so it is likely NOT the direct driver of this OOM (the
> driver is the bare-by-value-resource + for-element clone from `bac24e49`/a-5). Keep it; do NOT
> assume it's implicated without measuring. (The handover agent grouped it with the clone regime by
> commit-proximity, not evidence.)
>
> Anchors: `lower.gg:7860/7805/7762` (the three walkers); their call/recursion sites `7778-7850 self-recursion + 8827/8830 lower_module calls`;
> `lower_module` body-walk + transitive fixpoint (~`8704`+); the for-element consume decision
> (`op_consume`/`emit_payload_read` vs `lower_for`'s `OpBorrow`); `bac24e49` (clone-on-consume regime).

> **CORRECTION (2026-05-25, second session):** an earlier draft of this section root-caused bug #3 to
> `Box(x)` shallow-aliasing its payload (a missing `fn_move_params` entry + `GtMutPtr→OpCopy` consume).
> That was **WRONG** — it's been DISPROVEN. The `Box`/`MutPtr` clone fix was implemented and **verified
> in the emitted C** (`Box(lhs)` now emits `SpannedExpr__clone(...)` before the alloc), yet
> `self_host_bootstrap` **fails identically** (`status=None` signal-kill at step 8). The `Box`/`MutPtr`
> change is correct-by-spec (matches Rust's `ensure_owned_at_consuming_arg` + language-ref §9.6) but is
> **orthogonal to bug #3**. It is **COMMITTED as `19f90339`** (lower.gg op_consume +
> decide_operand_at_consuming_arg GtMutPtr arms → clone-through; "Box" in fn_move_params) — KEPT
> (lowerer_comparison-green); reconcile with the recovery fix's clone path (NEXT STEPS STEP B). Do NOT
> re-chase the Box-ctor mechanism.

### (superseded — REFUTED, kept as diagnostic record) the A.2 / unbounded-recursion analysis

*Everything from here until "## NEXT STEPS" was the working theory before STEP A was executed; it is
DISPROVEN (see the DIAGNOSIS-CORRECTED block above + STATUS at top). Read only for the audit trail —
the bug was a heap clone-OOM (lower-phase FIXED `7cc7a101`; codegen-phase = bug #3b), not unbounded
recursion and not A.2.*

**CONFIRMED NATURE (three independent probes, 2026-05-25):** `lower_expr`'s `case EBinaryOp(lhs_box,…)`
(`lower.gg:3881`, recursion `lower_expr(*lhs_box)` at `lower.gg:3928`) recurses **without bound**.
1. **ASan stack-overflow** in `lower___lower_expr`, every frame the same lhs-recursion call site.
2. **Unlimited-stack run → OOM SIGKILL** (137), NOT completion and NOT a clean SIGSEGV. So it is
   **genuinely unbounded** (a finite-but-deep `+` chain would have completed; the ~148KB frame alone is
   not the cause).
3. **Chain dump** (inject a stack-depth guard at `lower_expr` entry; at >6MB growth dump the `lhs_box`
   chain): nodes march by a **regular 256-byte stride** through the heap (`…9840 → …9740 → …9640 → …`),
   all `tag=9` (EBinaryOp), then a `tag=6` node with garbage `opdata` — and ASan flags a
   **heap-buffer-OVERFLOW** (reading past an allocation), **not** a use-after-free.

**INTERPRETATION (REVISED 2026-05-26 — supersedes the corruption framing; that was wrong):** it is
**NOT a cycle, NOT a UAF, NOT a corrupt/mis-stored box.** Definitive probes (all on the stage-2 binary,
guard fires at a stack-growth threshold then inspects):
- Cycle detector (visited-set over the `lhs_box` chain, mirroring the real recursion): **no repeat**.
- Heap-`lhs_box` ring-log of the last 40 `lower_expr` entries at the 7.5MB-deep point: all `tag=9
  op='+'`, all **distinct** addresses increasing by exactly 0xd0 (208B = one `SpannedExpr` box),
  `REPEATS=0`. Call counter ~13,080.
- 512MB ASan quarantine: still stack-overflow, NOT a UAF → memory is **live**, not freed.
- Unlimited stack → OOM SIGKILL at 75s → genuinely unbounded depth.

⇒ `lower_expr` is descending a genuinely **~13,000+-deep `+` `EBinaryOp` chain** of consecutive
freshly-bump-allocated heap boxes, while handling `derive___field_write_lines` (whose SOURCE has only
~30 `+`). So an **over-built / unboundedly-deep `+` chain** is generated from a finite input — NOT
corruption. The ASan "allocated by" stack for these boxes was a deep `Expr__clone ↔ SpannedExpr__clone`
recursion, so the chain is fabricated by a CLONE (or the parser's Pratt loop). The earlier
"256-stride into a string / box-holds-String-ptr / heap-buffer-overflow" signals were artifacts of the
*blind* dump walking PAST the chain into adjacent memory — ignore them. (`Expr__clone` case 9 and the
parser's box construction both look structurally correct on inspection.)

**BISECT RESULT (2026-05-26):** `git bisect` (good=f15a45c6, bad=758ed737, test=`self_host_bootstrap`
exact) → **first bad commit `2e544e84` = "wire GIFieldLoad with Ptr-typed dst for resource fields
(A.2)"**. A.2's own message ADMITS it: *"Bootstrap MAY regress until B.1 + E.1 land — downstream
consumers (SVarDecl, op_consume) don't yet handle Ptr-typed source locals from field reads… bootstrap
reactivation is later commits' job."* **That recovery was never completed** → bug #3. A.2 makes
`EFieldAccess` on a **resource** field emit `GIFieldLoad` into a **Ptr-typed (aliasing) dst**
(`lower.gg` EFieldAccess named-field path: `dst_type_id = register_ptr(field_type)` for resource fields,
`LoBorrowed` + `BoField` origin; `lir_lower.gg` GIFieldLoad dispatch → `ISlotStore` of the field
pointer). `lower_expr` itself does `match sexpr.expr:` (an `Expr`-typed resource field read, now a
Ptr-alias) which flows into the EBinaryOp/box-deref/clone path where a downstream consumer mishandles it,
fabricating the unbounded `+` chain. **Caveat:** bootstrap fails throughout [A.2..758ed737] for evolving
reasons, so the bisect found first-BREAKAGE (=A.2, the first drop CODE commit; f15a45c6=A.1 added only
the unused instruction). A.2's documented breakage IS the incomplete Ptr-typed-field port — the root
B.1/D.1/E.1 were meant to fully recover but didn't. **FIX = complete the A.2 recovery** (handle
Ptr-typed field-read source locals — `LoBorrowed`/`BoField`, slot `LT_PTR_TO_BASE+sid` — correctly where
they flow into consume/clone/recursion: materialize/deref so they don't alias-then-explode), OR revert
A.2's resource-field Ptr-dst to a value-load if the alias benefit isn't yet needed.

**FORK RESOLVED (2026-05-26) — it's LOWER-TIME.** A shallow-stack probe (walk the `lhs_box` chain only
when `current_fn=="derive___field_write_lines"` and stack-growth < 1MB, i.e. before recursion deepens)
reports a max parsed `+`-chain depth of **50** (`terminator tag=6`) — matching the ~30-50 source `+`.
The 13k-deep chain NEVER appears at shallow stack. So the **parser builds a correct ~50-deep chain, and
LOWERING fabricates the 13,000+-deep one** (consistent with the ASan box-allocation stack being a deep
`Expr__clone`/`SpannedExpr__clone` recursion). I.e. a **lower-time clone-and-re-lower explosion** of the
`+` chain → unbounded `lower_expr` recursion → OOM/stack-overflow.

---

## NEXT STEPS — bug #3b clone-OOM CLEARED (2026-05-27 pm): s1bin now exits 0; new blocker is a SCALE-dependent self-host large-String truncation

> **Read this first.** The bug #3b clone-accumulation OOM (`lower_module` → `lower_gir_to_lir` →
> `drop_elab` → `generate_c`, the whole cascade) is **FIXED**. s1bin previously SIGKILLed at ~13 GB; it
> now **exits 0 with a ~5 GB peak**. The fix was a series of CoW-contract restorations (reads must
> borrow, not deep-clone) at the writer sites, found by capturing the REAL gdb OOM backtraces
> (`gorget_array_reserve` alloc-fail / `LirBlock__clone` / `LirFunction__clone`) — NOT the misleading
> batch-mode lexer artifact. Commits on `gorget-1`: `2041d255` (lir_lower in-place push/term) +
> `30f882ec` (drop_elab + lir_codegen borrows + nested-collection elem-drop wiring).

**What was fixed (all CoW reads-must-borrow at the writer; output-neutral — `lowerer_comparison` GREEN, `cargo test --lib` 1060/1062 with 2 pre-existing `lir::validate` panic-assert fails):**
1. **`lir_lower.gg` `lower_operand`/`lower_instruction` LirBlock-clone (THE original bug #3b residual).**
   Real backtrace: `gorget_array_reserve(fail) ← gorget_array_clone ← LirBlock__clone ←
   lower_operand ← lower_instruction ← lir_lower_function ← lower_gir_to_lir`. The get-mutate-set idiom
   `LirBlock blk = f.blocks.get(bb).unwrap(); blk.insts.push(x); f.blocks.set(bb, blk)` value-bound the
   block → deep-cloned its whole growing `insts` vector on EVERY instruction → O(n²)/function → ~13 GB.
   Fix: `lir_push_inst`/`lir_set_term` in `lir.gg` mutate the block IN PLACE via the `.get()` borrow
   (`f.blocks.get(bb).unwrap().insts.push(x)`), mirroring Rust `block_mut(bb).push_inst()`
   (`src/lir/lower/mod.rs:1480/1490`). All 79 sites converted.
2. **`drop_elab.gg` `forward_dataflow` LirBlock-clone** (per-worklist-visit) → borrow the block into
   `compute_transfer`/`term_successors`.
3. **`lir_codegen.gg` (generate_c) read-only block/inst/fn scans** — `compute_reachable_fns`,
   `collect_hashable_key_types`, `collect_func_addr_targets`, `fn_exists`, the main function-emit loop,
   and the per-block C emitter — all value-bound `LirFunction`/`LirBlock`/`LirInst` (deep clones, leaked)
   → converted to BORROW chains (`m.functions.get(fi).unwrap().blocks.get(bi).unwrap()...`). lir_codegen
   does ZERO block mutation, so these are safely borrows.
4. **`drop_elab` `Vector[Vector[int]]` leak** — `forward_dataflow`'s state arrays leaked the inner vectors
   on every `.set()` (displaced element) and at function exit, because `__gorget_array_new_sized_T`
   typed-literal ctors emitted `gorget_array_new` with `elem_drop = NULL`. Fix: wire `elem_drop`+
   `elem_clone` for **NESTED-COLLECTION elements only** (Vector/Dict/Set inner) in the
   `__gorget_array_new_sized_` branch of `emit_call_extern_with` (lir_codegen.gg ~:3715). **Scoped
   to collections deliberately** — wiring String/user-struct elements DOUBLE-FREES the get-mutate-set
   idiom (self-host lacks Rust's `index_borrow_sources` clone-on-mutation; a GLOBAL wiring crashed in
   `compute_reachable_fns` freeing a still-aliased LirInst String — verified, reverted). Two other
   reverted dead-ends: `assign_inner_state` (in-place nested `outer.get(i).push()` SIGSEGVs in s1bin —
   the `feedback_nested_vector_get_set` gap).

**NEW LIVE BLOCKER (2026-05-27 pm) — SCALE-dependent self-host large-String truncation in `generate_c` output.**
With the OOM gone, s1bin runs to completion (exit 0) but emits a **TRUNCATED** stage-2: **59415 lines /
1,633,116 bytes** (deterministic, byte-identical across runs), cut **mid-token** inside `emit_function`
for `Parser__parse_match_stmt` (3407 of 3997 functions emitted), ending `…__v236 = gorget_array_new(sizeof(`.
**This is NOT the clone-OOM** (exit 0, empty stderr, ~5 GB peak — no SIGKILL/panic) and **NOT my borrow
conversions** (they're output-neutral: `lowerer_comparison` GREEN, and s1bin emits **clean, complete**
output for a SMALL input — `_self_host_e2e_preamble.gg` → 830 lines ending in a proper `}`). It is a
**pre-existing self-host runtime String/StrBuf scaling bug, newly EXPOSED** because s1bin now gets far
enough to assemble the ~610K-line / multi-MB output String. Matches the MEMORY note "stage-1's runtime
`body.slice()` / `index_of` mishandles 9 MB strings". Pipeline: `driver.gg:103 print(generate_c(&lir))`
— `generate_c` (`lir_codegen.gg:5120`) builds the whole C as ONE `String` (StrBuf `body_buf.s` then
`out + body`), and either the StrBuf concat or the final `print` truncates past ~1.6 MB in s1bin.

**FIRST DIAGNOSTIC (fresh session):** repro = `cc -O0 -g` the assembled stage-1, run on `driver.gg lib
--lir-c` (NO ulimit needed — it's not OOM), confirm exit 0 + 1,633,116-byte truncation. Then bisect the
String path: (a) is `generate_c`'s returned `String` already truncated (StrBuf concat / `String + String`
bug at scale), or (b) does `print` (`fwrite` of `msg.data`/`msg.len`) truncate a complete String? Add a
`gorget_print_err(int_to_str(generate_c(&lir).len()))` probe in `driver.gg` main to see if the String's
own `.len` is ~1.6 M (print bug) or the full ~20 MB (StrBuf-assembly bug). **Sharper localization:** the output String is grown by `sb_push`
(`lir_codegen.gg:46`), which calls `gorget_string_append_buf(&buf, rhs, rhs.byte_len())`. The C runtime
`gorget_string_append_buf` (`size_t len`, correct 64-bit realloc) is NOT the bug. **The suspect is the
call lowering**: the emitted C passes `gorget_str_to_cstr(rhs)` (a NUL-terminated C string) as `data` and
copies `rhs.byte_len()` bytes — if `gorget_str_to_cstr` or `byte_len()` mis-handles `rhs` at scale, or if
the cumulative `buf.s` realloc path in s1bin is miscompiled, the append silently truncates. The cut is
mid-token (not a hard byte cap), pointing at a per-append failure once `buf.s` is multi-MB. Check
`byte_len()` (`int` return — fine at 1.6 M but verify the self-host's impl) and whether `gorget_str_to_cstr`
allocates a fresh cstr each append (an O(n²) cstr churn that could itself OOM/corrupt at scale). The
thin-pointer String redesign (`project_thin_pointer_string`) is the broader context.

**Then — validate (in order):** once the String truncation is fixed, s1bin should emit the full ~610K
lines → `self_host_bootstrap` (exact) → `self_host_bootstrap_fixed_point` → `lowerer_comparison` →
`cargo test --lib` → full integration (parent). Then SHIP-GATE (`is_owning_mutator_arg` name-match →
typed signal) + squash, per below.

---

## NEXT STEPS — (HISTORICAL, superseded by the section above) bug #3b RE-LOCALIZED (2026-05-27 am): `generate_c` clone FIXED; live OOM is now in the LOWERING phase (`lower_module`)

> **Read this first — the old framing below this section ("(superseded)…") is stale.** bug #3b's
> ORIGINAL site — `m.functions.get(i).unwrap().name` cloning a whole `LirFunction` in `generate_c` — is
> **RESOLVED.** The Option[Ref] borrow-by-default work (Phases 1-6, on `gorget-1`) made `.get()` return
> `Option[Ref[T]]` (a `GtPtr` payload). **Verified in the emitted stage-1 C:** `compute_reachable_fns`
> now lowers `m.functions.get(i)` to `gorget_array_safe_get` (a **borrow pointer**), NOT
> `gorget_array_clone` — only small `gorget_string_clone_to_owned` calls remain. No whole-`LirFunction`
> clone in `generate_c`.

**DONE since this doc was written (all on `gorget-1`; squash at green — see `## History`):**
- Option[Ref] (Phases 1-6) + Layers 1-6 (`c96efd58` → `6186b436`) — full record in `option_ref_borrow.md`.
- **Gap A** / bare nullary enum-variant resolution (`bdc5b537`): fixed the `push(IDropGuardClose)`
  208-vs-8-byte stack overflow (a bare nullary `Inst` variant used as a value now lowers via
  `lower_nullary_variant_ident` → `lower_call`, like the with-parens form).
- **Layer 9** / `Dict.put` borrowed-String-key UAF (`dac39a64`): `decide_svardecl_emission` Branch A now
  excludes `GtPtr`/`GtMutPtr` sources (typed, mirrors Rust `src/ir/lowering/stmts/mod.rs:1140-1141`), so
  `String x = coll.get(i).unwrap()` clones via Branch C-pre instead of a name-collapse `BorrowAlias`.
  3-pass brief review + a diff review (ASan repro) signed off.
- **Result:** s1bin now **builds clean** (`cc` exit 0, schema 12/12, no `(Option*)`/unknown-field errors)
  and **no longer segfaults**.

**LIVE BLOCKER — a clone-ACCUMULATION OOM in the LOWERING phase (`lower_module`), the bug #3 class.**
s1bin OOMs (system `SIGKILL`/137 at ~11 GB in the canonical run; `gorget: panic: allocation failed` under
`ulimit -v 3000000`). **It is in LOWERING, not parse — VERIFIED:** the truncated s1bin output is **758
`gir_liveness_diff` warnings** (`/tmp/v12-stage2.c`, all but one line), which `diag_warn` emits
**per-function inside `lower_module`** (lower.gg:7250 / :7485). The driver pipeline (driver.gg) is
`parse_source`(48) → `resolve_module`(59) → `type_check_module`(60) → **`lower_module`(61)** → … →
`generate_c`(later). So emitting 758 warnings PROVES s1bin completed parse + resolve + typecheck and was
executing `lower_module` when memory ran out (the 3 GB cap dies at 758; a larger run reaches the full ~797
warnings before the panic). The accumulation builds through `lower_module` (and possibly the later
`lower_gir_to_lir` / `generate_c`).

**⚠ The earlier "lexer / `StringLiteral.lex_segments` clone" framing was WRONG — do NOT chase it.** A gdb
backtrace through `lex_emit → StringLiteral__clone → gorget_array_clone` was a **batch-mode artifact**:
`gdb -batch -ex run -ex bt` stops at the FIRST breakpoint hit — an early `lex_segments` clone during
`load_imports`' parse that SUCCEEDED — then exits, never reaching the real alloc-failure in lowering. The
`lex_emit` clone is at most a minor wart (≈9k string literals × tiny `lex_segments` ≈ single-digit MB —
numerically cannot be the ~11 GB consumer). **The real OOM backtrace was never captured; getting it is
step 1 below.**

**Why s1bin but not the driver:** the Rust-gg-compiled driver lowers the SAME program and emits all 612963
lines without OOM. So s1bin's `lower_module` (compiled from stage-1 C) accumulates/clones where the
driver's (compiled by Rust gg) moves/borrows/frees. This is the **residual of bug #3** (the lower-phase
clone-bomb): the for-element + non-owning-match borrow fix made the DRIVER's `lower_module` complete, but a
residual clone/retain in self-compiled `lower_module` still grows to ~11 GB. Same "stage-1 mis-compiles fn
X → s1bin's X is memory-pathological" fidelity class as Layers 1-6.

**PRE-EXISTING, NOT a Layer-9 regression — VERIFIED.** Pre-Layer-9 s1bin (`/tmp/v11-bin`) dies at the SAME
758-warning lowering point under the 3 GB cap (`allocation failed`); cycle 8 only reached its (now-fixed)
UAF because it ran under full system memory. Clearing the earlier crashes (IDropGuardClose, Layer 9 UAF)
just let s1bin run far enough into lowering to exhaust memory.

**FIRST DIAGNOSTIC (fresh session):**
1. **Repro (canonical — do NOT hand-roll cwd/paths; a wrong cwd faked a `schema.gg`-drop earlier, see TODO
   `find_project_root_for`):** rebuild driver (`cd tests/fixtures/self_host_lowerer && GG_BUILD_TIMEOUT_SECS=900
   ../../../target/release/gg build driver.gg`), then `cargo test --test integration self_host_bootstrap`
   or `/tmp/cycle9.sh` (mirrors it: repo-root cwd, ABSOLUTE `driver.gg`+`lib` paths, `--lir-c`, preamble
   `\ntypedef struct __gg_`, `cc -O0 -w -lm -lpthread`). Fast fail: `cc -O0 -g` the assembled C, run under
   `ulimit -v 3000000`.
2. **Get the REAL OOM backtrace** (the artifact this handoff LACKS — do not skip): under the cap, gdb must
   `continue` PAST the many SUCCESSFUL `gorget_array_reserve`/`gorget_array_clone` calls to the FAILING one
   — break on the `fprintf(…"allocation failed")` / the `gorget_array_reserve` alloc-fail (v12-full.c:~2275)
   and let it run to THAT hit, then `bt`. (Do NOT `bt` on the first clone — that's the artifact that misled
   this handoff.) That stack names the `lower_module`/`lower_gir_to_lir` site allocating the dominant buffer.
3. **Bisect by memory, not guesswork:** the per-function `gir_liveness_diff` warnings are a FREE progress
   meter — note RSS at warning N to see whether memory grows LINEARLY across `lower_module`'s function loop
   (a per-function clone/retain) or JUMPS (one big structure). `__gorget_array_clone_count` (runtime global)
   tracks clone growth; distinguish churn (freed) vs leak (retained).
4. **Re-open the lower-phase anchors** (the live targets): the discovery walkers + `lower_module` body-walk +
   transitive generic fixpoint, and `op_consume`/`decide_ptr_consume`. **Diff vs the driver:** which
   `lower_module`/GIR structure does s1bin clone/retain that Rust moves/borrows?
5. **Rust parity:** `src/ir/lowering/` discovery/lowering passes borrow their AST and don't retain
   per-function deep copies.

**LIKELY ROOT (bug #3 residual — CONFIRM via the real backtrace; do NOT pre-commit a site):** a
per-function clone or retained-copy in self-compiled `lower_module` (or `lower_gir_to_lir`) that the
Rust-compiled driver does as a move/borrow. The OLD fork (A) ("make `.get()` borrow + port
`index_borrow_sources`") and fork (B) ("surgical generate_c accessor") are **superseded** — generate_c is
fixed and the live OOM is in `lower_module`. **One residual generate_c clone for LATER:** `LirFunction
cur_func = m.functions.get(cur).unwrap()` (lir_codegen.gg:~1020) still binds a VALUE → a whole-`LirFunction`
clone per worklist entry; harmless now (generate_c unreached) but a latent next-blocker once lowering is
fixed — borrow it (`&cur_func` / bind the borrow). Fix at the writer (CLAUDE.md "Don't redesign around
compiler gaps"); validate via the canonical bootstrap.

**Then — validate (in order):** `self_host_bootstrap` (exact) green → `self_host_bootstrap_fixed_point`
green → `lowerer_comparison` parity → `cargo test --lib --release` (baseline per CLAUDE.md ~1027; ~1059
observed) → full `cargo test --test integration` (parent drives). NB: stage-2 is built with plain
`cc -O0` (NO ASan) — judge the REAL failure mode there (ASan inflates frames and masked a heap-OOM as a
stack-overflow this session). Re-run the drop-count grep-diff vs `driver.c`.

**Then — SHIP-GATE + squash.** Before green-ship, replace the consuming-position name-match
(`is_owning_mutator_arg`) with a typed signal (guardrail #5), then squash the whole cluster as ONE
commit (partial states crash). Fold "Deferred optimizations" into `TODO.md`.

**Anchors (LOWERING clone-OOM — the live blocker):** `lower_module` (lower.gg, the per-function lowering
loop that emits the `gir_liveness_diff` warnings at :7250/:7485) + the discovery walkers + the transitive
generic fixpoint (~lower.gg:8704+); `op_consume`/`decide_ptr_consume` consume decisions (lower.gg:1312+/
1533+); then `lower_gir_to_lir` (lir_lower.gg). The dominant ~11 GB consumer is one of these — pin it via
the REAL OOM backtrace (FIRST DIAGNOSTIC step 2) + the per-warning RSS meter (step 3). Diff vs the
Rust-compiled driver's `lower_module` (which completes). **Repro artifacts (this session, `/tmp`):**
`cycle9.sh` (canonical cycle), `v12-full.c` (assembled stage-1 C), `v12-g` (`-O0 -g` debug build), `v12-bin`
(the OOMing s1bin), `v12-stage2.c` (the 758-warning truncated output = proof OOM is in lowering); `v11-bin`
= pre-Layer-9 s1bin (OOMs identically under cap → proves pre-existing). NOTE: `v12-gdb2.log`'s lexer
backtrace is the MISLEADING batch-mode artifact (first clone during parse, not the OOM) — ignore it.
**generate_c `.get()` borrow — NOW FIXED via Option[Ref]** (`compute_reachable_fns` uses
`gorget_array_safe_get`); the old walker/fixpoint anchors below are kept only as historical record.

**(historical) In-tree change `19f90339` (`Box`-owning + `GtMutPtr`-to-resource clone-through):**
committed; correct-by-spec (Rust `ensure_owned_at_consuming_arg` + lang-ref §9.6). Keep it; reconcile only
if a future `OpClone`-materialization arm overlaps. **(historical, STALE) old generate_c anchors:** the
three walkers `lower.gg:7860/7805/7762`; `lower_module` body-walk + transitive fixpoint (~`8704`+);
`emit_payload_read` auto-clone vs `lower_for`'s `OpBorrow(coll_local)`; `bac24e49` (Phase 2.3 clone-on-
consume regime). (first-bad `2e544e84`=A.2 = a since-fixed `load_imports` hang.)

---

### (superseded) earlier Box-ctor analysis — kept for context, DISPROVEN above

**Symptom:** the stage-2 binary (self-host compiled by itself) stack-overflows in `lower_expr`'s
`case EBinaryOp(lhs_box, …)` (`lower.gg:3881`) → `lower_expr(*lhs_box)`, which never bottoms out,
while lowering **`derive___field_write_lines`** (whose body is a deep left-leaning `+`
String-concat chain, parsed via the Pratt loop in `parser.gg:~1765`). gdb saw the same node at every
depth (identical operator-`String` `.alloc` ptr) — a self-referential `Box[SpannedExpr]`.

**CONFIRMED it's the WIP, not the parser (the decisive A/B):**
- `self_host_typechecker/` (the symlinked INPUT: `parser.gg`/`ast.gg`/`derive.gg`/…) is
  **byte-identical** `f15a45c6..HEAD` (`git diff --stat` empty). So the stage-2 parser builds the
  exact same AST in both. Only the 6 real lowerer files differ.
- `self_host_bootstrap` (step 8 *runs* the compiled stage-2 binary on driver.gg): **PASSES at
  `f15a45c6`** (both it and `_fixed_point`), **FAILS at HEAD** (`status=None`, signal-kill = stack
  overflow). Same input, only the lowerer differs ⇒ the recursion is a WIP runtime-aliasing bug.

**ROOT CAUSE (two coupled gaps), found by diffing the self-host-emitted C for
`Parser__parse_expr_bp_with_lhs` at `f15a45c6` vs HEAD:**

`parse_expr_bp_with_lhs(&self, int min_bp, SpannedExpr !lhs)` repeatedly does
`lhs = SpannedExpr(EBinaryOp(Box(lhs), op, Box(rhs)), Span(lhs.span.start, rhs.span.end))`.
`Box(x)` is lowered as `GORGET_ALLOC(sizeof(SpannedExpr)); memcpy(box, src, sizeof)` — a **shallow**
copy (it duplicates the struct bytes, *including the inner `Box`/`String` pointers*, but does NOT
deep-clone them). At `f15a45c6` that was harmless (labels-only → nothing freed `src`). The WIP turned
on **drop emission** (the new `gorget_string_free`/`gorget_array_free`/`__drop` calls visible in the
HEAD body diff), so the owned source is now dropped — freeing the inner boxes the shallow copy still
aliases. Dangling alias + deterministic same-size allocator reuse → the self-referential box. This is
exactly the plan invariant: **"a field/element that is DROPPED must also be CLONED — drop-without-
clone on a shallow copy = double-free."**

Why `Box(x)` shallow-copies instead of clone/move:
1. **`Box` is missing from `fn_move_params`.** `Some`/`Ok`/`Error` were registered as owning prelude
   ctors (`lower.gg:8514-8518`, item #5 — fixed the `parse_equip_item` UAF), but `Box` — the sibling
   prelude ctor with identical heap-payload-ownership semantics — was missed. So
   `classify_call_arg(&gmod, "Box", 0)` misses → `CkCallArgBorrow` → `op_consume` returns `OpBorrow`
   (a non-consume early-out: clone/move never considered). `Box(x)` routes through `lower_call`'s
   generic arg loop (`lower.gg:5219/5226`), so registering it there *does* take effect.
2. **`!`-move params are typed `GtMutPtr`, indistinguishable from `&`-borrow params.** Even with (1)
   fixed, consuming `lhs` (a `!`-param) hits `decide_operand_at_consuming_arg`'s
   `case GtMutPtr(_): return OpCopy` (`lower.gg:~1426`) — a shallow pointer-copy — because
   `lower.gg:6770-6771` (functions) and `7046-7047` (methods) collapse BOTH `&` (ownership==1) and
   `!` (ownership==2) to `GtMutPtr(inner)`. A `!`-param is **callee-owned** and must clone/move on
   consume; only `&` is a borrow-alias (`OpCopy`). NB the `rhs` operand is a by-value owned local
   (`GtNamed`, `LoOwned`) — gap (1) alone fixes *that* arm (→ `OpClone`/`OpMove`); gap (2) is needed
   for the `!lhs` arm.

**THE FIX (both parts; correctness-first per the guiding principle — clone unless owned-and-dead):**
- Part 1: register `Box` owning — `Vector[bool] box_mv = [true]; fn_move_params.put("Box", !box_mv)`
  beside `Some`/`Ok`/`Error` (`lower.gg:~8518`). Use a **typed** `Vector[bool]` local, not a bare
  `[true]` (elem_size-8 push-overflow trap — see the item-#5 docstring at `lower.gg:8507-8512`).
- Part 2: make `!`-move params distinguishable from `&`-borrow so consuming a `!`-param clones.
  **Recommended (reference-grade):** type `!` (ownership==2) as `GtPtr(inner)` (which already routes
  through `decide_operand_at_consuming_arg`'s `case GtPtr(inner) → decide_ptr_consume → OpClone` for
  resource inners), keeping `&` (ownership==1) as `GtMutPtr`. Touches the TWO param-typing sites
  (`lower.gg:6770-6771` + `7046-7047`). **ABI caveat — verify before shipping:** confirm a `!`-param
  typed `GtPtr` still address-takes/mutates correctly and that nothing downstream keys on `GtMutPtr`
  to recognise a `!`-param (grep `GtMutPtr` consumers). Alternative if GtPtr ripples: a distinct
  ownership tag (`LoOwnedParam` for `!`, retain `LoParam`/borrow for `&`) consulted in the
  `GtMutPtr` arm of `decide_operand_at_consuming_arg`. Both are output-affecting (more clones) — run
  the full bootstrap to verify green and watch for new double-frees elsewhere.

**Reproduction harness used (cheap, no bisect):** emit the self-host's own C for the offending fn at
each revision and diff — `driver driver.gg lib --lir-c > s2.c` (~580s at HEAD; faster labels-only),
extract `Parser__parse_expr_bp_with_lhs`, normalize SSA names (`sed -E 's/__v[0-9]+/__v/g; …'`),
diff bodies. The box-of-`lhs` site is `GORGET_ALLOC(sizeof(__gg_SpannedExpr)); memcpy(…)`; the WIP's
new drops are the `*_free`/`__drop` lines absent at `f15a45c6`. (A minimal stand-alone fixture did
NOT reproduce — its smaller types gave the `!lhs` slot a raw `LT_PTR` instead of the real fn's
typed `LT_PTR_TO_BASE+sid`, so it emitted a different `sizeof(void*)` box. Use the real fn.)
Durable anchors: `lower.gg:3881` (EBinaryOp arm), `parser.gg:~1765` (Pratt loop), fn
`Parser__parse_expr_bp_with_lhs` / `derive___field_write_lines`.

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
4. **Perf: the ~567s emit** — profile (per `feedback_perf_hunt_playbook`); suspect O(n²)
   drop-type resolution / block-instruction append as drop count grows.
5. **`fixed_point` N back to 2 + retire Phase-F.2 band-aids** — `add_local_inheriting` /
   `inherit_borrow_from` were workarounds; audit-and-retire once real move+clone+drop is in, then
   tighten `bootstrap_fixed_point` from N=5 to N=2.
6. **Retire stale docstrings** — e.g. `lower.gg`'s `decide_operand_at_consuming_arg` still says
   "dead code in this commit. No caller exists" — it IS called now. Sweep for similar.
7. **Restore named-local clarity in the borrow-chain read scans** — bug #3b's fix turned clean
   `LirFunction func = m.functions.get(fi).unwrap()` loops in `lir_codegen.gg`/`drop_elab.gg` into
   verbose repeated `m.functions.get(fi).unwrap().blocks.get(bi).unwrap().insts.get(ii).unwrap()`
   chains (a value-bind would deep-clone). This is forced ONLY by the absent `Ref[T]`/`MutRef[T]`
   borrow-local (language-reference.md:912, "(planned)"). When `Ref[T]` ships, revisit these sites
   to restore readability (self-host-as-showcase). Output-review reservation #2, 2026-05-27.

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
double-free. bug #3 (lower-phase heap clone-OOM — for-element/discovery-walker `.get()` clones, NOT
recursion/A.2) is FIXED in `7cc7a101`. THEN (2026-05-27): Option[Ref] (Phases 1-6) + Layers 1-6 + Gap A
(`bdc5b537`) + Layer 9 (`dac39a64`) landed — s1bin now BUILDS CLEAN and no longer segfaults; bug #3b's
`generate_c` `.get()`-clone is RESOLVED (`compute_reachable_fns` borrows via `gorget_array_safe_get`). The
**live blocker is now a residual lower-phase clone-accumulation OOM in `lower_module`** (NOT generate_c,
NOT the lexer) — see "## NEXT STEPS" above. The full blow-by-blow is in `git log` and `DONE.md`.
