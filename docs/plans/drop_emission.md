# Drop Emission — Self-Host Plan (unified)

**Status (2026-05-25, HANDOVER):** IN PROGRESS — cc-clean, **not yet bootstrapping.** WIP is
**COMMITTED** on branch `gorget-1` (ahead of `main`@`087b5a13`; `758ed737` = code save-point +
handover-doc commits; squash at merge-to-main). Stage-1 now emits the full stage-2 C (**616363 lines, ~567s** — use a ≥2400s
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

## NEXT BUG — bug #3: unbounded `lower_expr` recursion — lower-time clone explosion of a finite `+` chain (A.2 Ptr-typed field-read incomplete recovery)

(Bugs #1 + #2 are FIXED + committed in `758ed737` — see Status. The old add_local/`NO_NAME`
overflow WAS bug #1; do not re-chase it.)

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

## NEXT STEPS — complete the A.2 Ptr-typed-field-read recovery (reference-grade) [chosen 2026-05-26]

**Decision (user, 2026-05-26):** pursue the reference-grade completion of the Rust-machinery port
(`project_rust_machinery_port_plan`), NOT a revert of A.2. A.2's resource-field `GIFieldLoad`→Ptr-dst
is the intended design (it eliminates the silent `field.push(...)` writeback-bug class); the defect is
that the *recovery* A.2 deferred to "B.1 + E.1" — making every downstream consumer handle a Ptr-typed
field-read source local (`LoBorrowed`, `BoField(base,fi)`, slot `LT_PTR_TO_BASE+sid`) — was never
finished. So **finish it**, mirroring Rust's `FieldLoad` consume path (`src/lir/lower/insts.rs:800-869`
+ how `ensure_owned_at_consuming_arg` / the GIR consume sites treat a `BorrowedPtr`/field-origin source).

**STEP A — CONFIRM the A.2-era failure IS today's bug #3, THEN pinpoint the mishandling consumer.**
*Caveat the whole plan rests on (be honest):* the bisect found first-BREAKAGE = A.2, and bootstrap fails
throughout [A.2..758ed737] for evolving reasons. We have NOT verified that the crash *signature* at A.2
(the ~13k-deep `+`-chain OOM/stack-overflow in `lower_expr` on `field_write_lines`) is the SAME as
today's at 758ed737 — bug #3 could have shifted/emerged at a later drop-emission commit, in which case
STEP B is aimed one layer off. **A.0 (do this first):** check out `2e544e84`, run the documented
self-host harness, and confirm the identical signature (run the stage-2 binary on `driver.gg`; expect a
`lower_expr` lhs-recursion stack-overflow lowering `derive___field_write_lines`). If A.2's failure is a
DIFFERENT crash (e.g. a compile/link failure of stage-2, or a non-`field_write_lines` site), bisect
*within* [A.2..758ed737] for the commit that introduces THIS signature before proceeding.
**A.1 (then) — pinpoint the consumer (cheap, no rebuild):** the runaway is `lower_expr` lowering
`field_write_lines`' `+`-concat chain (parsed ~50 deep; lowering fabricates ~13k via clone-recursion).
Use the documented harness (emit `--lir-c` ~580s → splice `driver.c` preamble → `cc -O0 -g
[-fsanitize=address]` → run on `driver.gg`; inject probes directly into the generated `/tmp/*.c`, no
stage-0 rebuild). Trace where a Ptr-typed field-read result (`sexpr.expr` is an `Expr` = resource → A.2
makes it a Ptr-alias; likewise any `.field` of a resource) flows into a consume/clone site that (a)
treats the Ptr-alias as an owned value, or (b) re-clones + re-lowers it, compounding depth. Prime
suspects: `op_consume`/`decide_operand_at_consuming_arg` on a `LoBorrowed`+ptr-to-struct source (a-5
clone-on-borrow → `OpClone` that deep-clones the whole subchain per recursion level); the `*lhs_box`
box-deref reading through a Ptr-aliased `Expr`; the `GIFieldLoad`→`match` interaction. **Confirm the
single site with evidence — don't guess, and don't start STEP B until A.1 names it.**

**STEP B — fix at the consumer(s) STEP A.1 identified, Rust-parity.** *Contingent on A.1's finding — the
direction below is the expected default (matches Rust + §9.6), not a pre-committed patch; adjust to the
actual site.* A Ptr-typed field-read result is a BORROW into the base; consuming it at an owning position
must **materialize once** (deref + clone to a fresh owned value), and the recursion/match must **deref
the pointer, not re-clone-and-re-lower**. Likely needs: (1) reading/lowering through a Ptr-typed
`sexpr.expr` derefs (no per-level re-clone); (2) `op_consume` on a `LoBorrowed`+`LT_PTR_TO_BASE+sid`
source at a consume position clones the POINTEE exactly once (must not compound); (3) no
clone-then-re-lower loop. Follow `docs/internals/layering-discipline.md` (fix at the write/producer site
if a downstream pass is reconstructing from a Ptr it should have been handed materialized). This likely
overlaps the `decide_ptr_consume` / `OpClone`-materialization paths the committed `Box`/`MutPtr` change
(`19f90339`) touched — reconcile, don't double-handle.

**STEP C — validate (in order):** `self_host_bootstrap` (exact) green → `self_host_bootstrap_fixed_point`
green → `lowerer_comparison` fn-count parity → `cargo test --lib --release` (~1059 baseline) → full
`cargo test --test integration` (parent drives). Re-run the drop-count grep-diff vs `driver.c` to watch
parity hold.

**STEP D — SHIP-GATE + squash.** Before green-ship, replace the consuming-position name-match
(`is_owning_mutator_arg`) with a typed signal (guardrail #5), then squash the whole cluster as ONE
commit (partial states crash). Fold "Deferred optimizations" into `TODO.md`.

**Orthogonal in-tree change:** the `Box`-owning + `GtMutPtr`-to-resource clone-through fix
(`lower.gg` `op_consume` + `decide_operand_at_consuming_arg` + `fn_move_params.put("Box")`) is committed
separately; it is correct-by-spec (Rust `ensure_owned_at_consuming_arg` + lang-ref §9.6) and
**verified orthogonal to bug #3** (crash byte-identical with/without it). Keep it; it may interact with
STEP B's clone path — reconcile there.

**Anchors:** first-bad `2e544e84` (A.2); `lower.gg` EFieldAccess named-field path (the `GIFieldLoad`
+ `register_ptr` + `set_field_borrow` emission), `lower_expr` `case EBinaryOp` (3881) + `match
sexpr.expr`, `op_consume`/`decide_operand_at_consuming_arg`/`decide_ptr_consume`; `lir_lower.gg`
`GIFieldLoad` dispatch + `OpClone` materialization; fn `derive___field_write_lines` (derive.gg:160-187);
Rust `src/lir/lower/insts.rs:800-869`, `src/ir/lowering/context.rs` `ensure_owned_at_consuming_arg`.

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
double-free. Remaining (the live blocker): bug #3 — the A.2 Ptr-typed field-read incomplete
recovery (unbounded `lower_expr` clone-explosion of `field_write_lines`' `+` chain); see the NEXT BUG
+ NEXT STEPS sections above. The full
blow-by-blow is in `git log` and `DONE.md`.
