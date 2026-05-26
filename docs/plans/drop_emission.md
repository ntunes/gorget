# Drop Emission — Self-Host Plan (unified)

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

## bug #3 (lower-phase clone-OOM) — FIXED `7cc7a101`; live blocker is now bug #3b (codegen-phase `.get()` clone)

> **STATUS (2026-05-26, end of session) — START HERE:**
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

## NEXT STEPS — bug #3b: the `.get().unwrap()` whole-`LirFunction` clone in `generate_c`

**DONE this session (verified, `7cc7a101`):** the lower-phase clone-OOM (bug #3) is fixed — for-element
+ non-owning-match destructures now borrow (see STATUS under NEXT BUG). `lower_module` completes;
`lowerer_comparison` green.

**bug #3b (live blocker):** the SAME `.get()`-aggregate-clone class, now in `lir_codegen.gg::generate_c`.
`compute_reachable_fns` (`lir_codegen.gg:902`), `emit_func_forward_decls` (`:1414/1419`), and the body
loop (`:5193`) iterate with `m.functions.get(i).unwrap().name` in `while` loops — each `.get().unwrap()`
deep-clones the WHOLE `LirFunction` (all blocks/instructions) just to read one field → OOM ~11 GB.
The for-element fix doesn't cover the manual `while`-loop `.get()` idiom. **Repro:** build stage-0, run
stage-1 on `driver.gg --emit-c`, `cc -O0`, run → OOM-SIGKILL ~11 GB in `generate_c` (it dies in the
reachability/fwd-decl setup, BEFORE the function-emission loop). Confirm with `print`-trace bisecting.

**FORK — pick one next session:**

- **(A) reference-grade — close the whole class.** Make `.get()` (and the other aggregate reads:
  Option-wrap, payload-read) **borrow by default** (CoW: "collection element reads propagate Ptr aliases
  at zero cost") and **port Rust's clone-on-mutation detection** (`index_borrow_sources`,
  `src/semantic/safety/check_expr.rs:358`) into the self-host safety pass so the get-mutate-set idiom
  (`t = coll.get(i); t.field.push(x); coll.set(i,t)`) clones on the MUTATION, not the read. This kills
  bug #3b AND every future `.get()`-clone instance in one stroke. **Caveat (verified by the impl agent):**
  a naive general-`.get()`-borrow WITHOUT the detection **double-frees** get-mutate-set (e.g.
  `traits.gg:append_builtin_method`). So this is substantial + UAF-risky and MUST land with the
  detection machinery, not before. This is the project-directive ("reference-grade over surgical")
  answer.

- **(B) surgical stopgap — borrow-iterate the codegen `while` loops only.** Add a `LirModule`
  borrow-accessor (e.g. `function_names() -> Vector[String]`, or a by-index field-borrow) so
  `compute_reachable_fns` / `emit_func_forward_decls` / the `:5193` body loop read names/fields without
  cloning whole `LirFunction`s. Unblocks bootstrap fast, but it's **whack-a-mole** — other
  `while … .get().unwrap()` aggregate reads will keep biting until (A) lands. Treat as a bridge, not the
  fix.

**Then — validate (in order):** `self_host_bootstrap` (exact) green → `self_host_bootstrap_fixed_point`
green → `lowerer_comparison` parity → `cargo test --lib --release` (baseline per CLAUDE.md ~1027; ~1059
observed) → full `cargo test --test integration` (parent drives). NB: stage-2 is built with plain
`cc -O0` (NO ASan) — judge the REAL failure mode there (ASan inflates frames and masked a heap-OOM as a
stack-overflow this session). Re-run the drop-count grep-diff vs `driver.c`.

**Then — SHIP-GATE + squash.** Before green-ship, replace the consuming-position name-match
(`is_owning_mutator_arg`) with a typed signal (guardrail #5), then squash the whole cluster as ONE
commit (partial states crash). Fold "Deferred optimizations" into `TODO.md`.

**In-tree change `19f90339` (`Box`-owning + `GtMutPtr`-to-resource clone-through):** committed; correct-
by-spec (Rust `ensure_owned_at_consuming_arg` + lang-ref §9.6); crash byte-identical with/without it. It
touches the `GtMutPtr` (`&`/`!`-param) consume arm — a DIFFERENT param shape than the BARE (`GtPtr`)
walkers this OOM is about — so likely not the driver. Keep it; if STEP B's for-element borrow fix and
this fix's `OpClone`-materialization arms overlap, reconcile (don't double-handle), but do not assume
`19f90339` is implicated without a clone-count measurement.

**Anchors:** the three walkers `lower.gg:7860/7805/7762` + their call/recursion sites `7778-7850 self-recursion + 8827/8830 lower_module calls`;
`lower_module` body-walk + transitive fixpoint (~`8704`+); the for-element consume decision
(`emit_payload_read` auto-clone vs `lower_for`'s `OpBorrow(coll_local)`); `op_consume`/
`decide_operand_at_consuming_arg`/`decide_ptr_consume`; `bac24e49` (Phase 2.3 clone-on-consume regime,
post-A.2). (NOT bug-#3-relevant but historical: first-bad `2e544e84`=A.2 is a since-fixed
`load_imports` hang.) Rust read-only-walk parity: `src/ir/lowering/` discovery passes borrow their AST.

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
recursion/A.2) is FIXED in `7cc7a101`; the live blocker is now **bug #3b** (the same `.get()`-clone
class at `generate_c`'s `while`-loop idiom) — see the NEXT BUG STATUS + NEXT STEPS sections above. The
full blow-by-blow is in `git log` and `DONE.md`.
