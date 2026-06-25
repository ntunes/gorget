# Error-model Inc-2.1d scout — Bounds cross-frame fault tag

**Status:** DESIGN/FEASIBILITY SCOUT + END-TO-END PROTOTYPE-MEASURED (C **and**
LLVM, compiles + runs + caught + binding + uncaught-panic + cross-category
re-panic + RESOURCE-element + ASan/UBSan-clean), 2026-06-25, on `gorget-1` tip
`5d6e9261` (worktree). Throwaway prototype — REVERTED (tree clean), not
integrated. The prototype diff lives at
`scratchpad/inc21d_proto.diff` (parent's scratch) and is reproduced in §3.

Builds directly on the 2.1c landing (`d49e3cea`, DONE.md), which shipped the
per-category tag-dispatch on `FaultableCall` with `bounds_handler` deliberately
omitted. The 2.1c scout (`docs/plans/error-model-inc21c-scout.md` §1.3, §0.6/0.7)
characterized Bounds as "a distinct mechanism" and deferred it here. This scout
**measured that the distinct mechanism is already 95% in place** — the LOCAL
`FaultableIndexLoad` + `gorget_array_safe_get` + NULL-branch shape (Inc-2 /
`a447c726`) is reused UNCHANGED in the callee; 2.1d is the SAME shape as 2.1c
(a third category threaded through the identical sites), NOT a new instruction.

Grounded in `error-model.md` §11.1 (Bounds via `gorget_array_safe_get`+NULL-branch),
§11.2 (branch-before-deref CFG, handler-bb constructs the `Fault` variant), the
2.1c scout §1.3 + §0.6/0.7 (the tag-dispatch design 2.1d extends), DONE.md
Inc-2.1c, and `docs/devbook/24-layering-discipline.md` (the resource-element
materialization is a rule-1 write-site win — see §4).

---

## 0. VERDICT

- **2.1d is ONE coherent increment — do it now. NO 2.1d-i / 2.1d-ii split.** The
  resource-element subtlety the 2.1c scout flagged (`Vector[String]` element →
  `Ptr(T)` borrow vs the arith scalar return) **DISSOLVES** at the callee return
  boundary: the callee's declared return type is the OWNED element type (`String`,
  not `Ptr(String)`), so the EXISTING return-boundary `ensure_owned_at_boundary`
  materialization clones the borrow to an owned value BEFORE the cross-frame
  return — the cross-frame ABI returns an owned `Str` on the no-fault path and a
  zeroed sentinel on the fault path. No new unification, no new drop-gate. **MEASURED
  ASan/UBSan-clean** on `Vector[String]` deep bounds AND a drop-bearing callee
  (§2). So the int and resource cases ship together.
- **The mechanism = 2.1c's, extended by exactly one category.** A participating
  callee's `setup_fault_return_scope` gains a third return block
  (`bounds_return_bb`); its body's `v[i]` lowers — via the UNCHANGED
  `bounds_handler_for` gate (`methods.rs:3420`) — to a `FaultableIndexLoad`
  routing to that block; `fill_fault_return_block` writes tag 3 and returns the
  sentinel (the SAME shape it uses for Overflow/DivByZero, just `panic_msg =
  "index out of bounds"`). The caller's `FaultableCall` gains a `bounds_handler`
  field + a `tag == BOUNDS_TAG → bounds_handler` arm in the tag-switch
  (`insts.rs:686`). The lint baseline goes 2→3 (it ALREADY documents this exact
  bump). ~120-line mechanical diff across 11 files (§3).
- **ONE genuinely new design item the 2.1c machinery lacks: a `bounds_panic`
  block on `FaultScope` for the uncaught-CATEGORY re-panic.** `FaultScope` has
  `div_overflow_panic`/`div_zero_panic` but no Bounds analogue. When a
  participating callee can raise Bounds but the catching scope catches only an
  ARITH category (or vice-versa), the gate must resolve
  `bounds_handler.unwrap_or(bounds_panic)` so the `FaultableCall` carries an
  ALWAYS-Some bounds handler that re-panics "index out of bounds" — mirroring
  2.1c's §3 cross-category re-panic. **Without this, a deep Bounds caught only by
  `catch Fault.Overflow:` would be SILENTLY SWALLOWED (Core-#8 miscompile).
  MEASURED: with the fix it re-panics, exit 1, both backends (§2).** This is the
  load-bearing correctness addition (add the field to `FaultScope`, populate it
  in BOTH the participating-callee scope AND the local-catch scope).

**Recommendation: scope 2.1d = Bounds cross-frame, int + resource elements
together, incl. the `bounds_panic` re-panic block. One executor pass.**

---

## 1. PREMISE VERIFICATION (every cite re-verified this session against tip `5d6e9261`)

### 1.1 The 2.1c machinery to extend — CONFIRMED LANDED

- **`FaultableCall`** (`src/ir/instructions.rs:371-392`): now carries
  `overflow_handler` + `divzero_handler: Option<BlockId>`; the doc-comment (`:369`)
  ALREADY says "Bounds adds a third `bounds_handler` category in 2.1d". ✅
- **GIR→LIR tag-switch** (`src/lir/lower/insts.rs:686-746`): loads the slot ONCE,
  chains per-category `emit_tag_branch` arms (`==0` continue, `==OVERFLOW_TAG →
  overflow_handler`, `==DIVZERO_TAG → divzero_handler`) via
  `resolve_variant_ordinal("Fault",…)+1` (typed, no magic literal). Clean
  `if let Some(h) = …` chaining — a bounds arm is a 3-line addition. ✅
- **The gate** (`src/ir/lowering/exprs/calls.rs:1376-1474`): resolves
  `(fault_overflow_handler, fault_divzero_handler)` as
  `s.{overflow,divzero}_handler.unwrap_or(s.div_{overflow,zero}_panic)`
  (ALWAYS-Some); slot-alloc predicate `is_some() || is_some()` (`:1443`); threads
  both into `builder.fault_call{,_void}`. ✅
- **`setup_fault_return_scope`** (`src/ir/lowering/functions.rs:65-111`): builds
  `overflow_return_bb` + `divzero_return_bb`, returns `(BlockId, BlockId)`, sets
  `overflow_handler/divzero_handler: Some(...)` and **`bounds_handler: None`**
  (`:106` — the explicit 2.1d hole, comment "Bounds deep propagation is 2.1d").
  ✅ — confirmed `bounds_handler` is `None`, must become `Some(bounds_return_bb)`.
- **`fill_fault_return_block`** (`functions.rs:128-183`): already
  parameterized by `(variant: &str, panic_msg: &str)` — the tag is
  `resolve_variant_tag("Fault", variant)+1` (typed). **Bounds reuses it verbatim**
  with `("Bounds", "index out of bounds")`. The 3 fill-call sites
  (`functions.rs:1046/1140/1157`) each call it for Overflow + DivByZero; add a
  third call. ✅
- **`participates_in_fault` flag** (`functions.rs:1186`,
  `func.participates_in_fault = fault_return_bbs.is_some()`); participation set
  `compute_participating_fault_fns` (`src/ir/lowering/mod.rs:919`). ✅
- **Lint `fault_call_handler_category_count`** (`tests/lints.rs:2748-2793`):
  baseline `FAULT_CALL_HANDLER_CATEGORIES = 2`; the doc-comment (`:2740-2746`) +
  the assert message ENUMERATE the exact 2.1d Bounds steps. **It FIRES** the
  moment a `bounds_handler` field is added (MEASURED: "3 vs expected 2") — bump
  to 3. ✅

### 1.2 The Bounds check mechanism — the LOCAL path is reused UNCHANGED in the callee

How a LOCAL `v[i] catch Fault.Bounds:` works today (verified, file:line):
- The catch's `lower_fault_catch_expr` (`src/ir/lowering/exprs/mod.rs:3601-3663`)
  sets `FaultScope.bounds_handler = Some(bounds_entry)`.
- The index read's `bounds_handler_for(ctx)` gate
  (`src/ir/lowering/exprs/methods.rs:3420/3476-3478`) reads
  `ctx.func_state.fault_scope?.bounds_handler`; when `Some` AND the base is an
  array (typed `collection_kind == Array` gate, `:3422-3424`, not a name check),
  it emits `builder.index_load_faultable(…, handler)` → GIR
  `Instruction::FaultableIndexLoad { …, fault_handler }`
  (`instructions.rs:250-256`).
- The GIR→LIR split (`src/lir/lower/insts.rs:1222-1272`) calls
  `gorget_array_safe_get` (`runtime_array.c:41`, returns NULL on OOB; signed
  index so negatives are OOB), tests NULL, `Term::Branch` BEFORE any deref, and
  materializes the element in the continuation via
  `materialize_collection_element` (shared with the plain `IndexLoad` array path).

**The cross-frame version reuses ALL of the above UNCHANGED.** The only
difference is WHAT `bounds_handler` points at: in the LOCAL case it is a local
handler-entry block; in the CROSS-FRAME callee it is `bounds_return_bb` (the
fault-return block that writes tag 3 + early-exit-drops + returns the sentinel).
The `FaultableIndexLoad` lowering doesn't care which — it just `Term::Branch`es
to `block_map[fault_handler]`. **This is why 2.1d is small: the callee-side index
read is already implemented; we just point `bounds_handler` at a return block.** ✅

### 1.3 The NEW participation category — uncaught INDEX READS

2.1c's participation (`src/ir/lowering/fault_participation.rs`) detects uncaught
ARITHMETIC ops via `is_faultable_arith` (`:57-62`, Add/Sub/Mul/Div/Rem) +
`pattern_catches_arith` (`:45-52`, Overflow/DivByZero/binding). 2.1d adds the
index-read analogue:
- `pattern_catches_arith → pattern_catches_fault`: add `"Bounds"` to the variant
  match (binding already returns `true`).
- `UncaughtArithDetector → UncaughtFaultDetector`: add an `Expr::Index { object,
  index }` arm alongside the `Expr::BinaryOp` arm — depth-0 index read ⇒ `found =
  true`. (`Expr::Index` AST variant `src/parser/ast.rs:550-553`.)
- 3 call-site/comment renames (`function_has_uncaught_arith →
  …_fault`, the `DeepCatchCalleeCollector`'s `pattern_catches_arith` call).

**Soundness of over-approximation:** the AST detector can't tell an array index
from a dict/string index (those have no `safe_*` variant and never lower to a
`FaultableIndexLoad`). Over-flagging a non-array index is HARMLESS — it only adds
an unused fault slot + a dead `bounds_return_bb` (DCE'd), exactly as 2.1c
over-flags a function whose arith is never deep-caught. The GIR
`bounds_handler_for` gate narrows the ACTUAL faultable lowering to ARRAY element
reads at the type-resolved site, so correctness never depends on the AST detector
being precise. ✅

### 1.4 The resource-element `Ptr(T)` subtlety — DISSOLVES at the callee return boundary

The 2.1c scout flagged: a bounds-faulting index read on a resource-element array
returns a `Ptr(T)` borrow (`methods.rs:3407-3409`, `register_ptr_type` for a
resource elem) vs the arithmetic scalar return; `ensure_owned_at_boundary`
handles the LOCAL case (`exprs/mod.rs:3674`) but "the cross-frame callee returns a
sentinel … so the result-type unification differs … needs its own
drop-correctness fixture."

**MEASURED: the cross-frame case needs NOTHING extra.** Trace of the emitted C for
`String getx(Vector[String] xs, int i): xs[i]` (the resource callee, §2):
```c
Str getx(const void* __p0, int64_t __p1, void* __p2) {   // __p2 = fault slot
__bb0:
    __v5 = gorget_array_safe_get(__v0, __v1);
    if (__v5 == NULL) goto __bb1;       // OOB → fault-return block
    else goto __bb4;                    // in-bounds → continuation
__bb1:                                  // fault-return: CATCHING caller?
    if (__p2 != NULL) goto __bb2; else goto __bb3;
__bb2:  *(int32_t*)__fault_slot = 3;    // Bounds TAG (ordinal 2 + 1)
        return *(Str*)&__s0;            // sentinel _0 (zeroed) — never read
__bb3:  gorget_panic_at(..., "index out of bounds");   // NULL slot → panic
__bb4:  __v9 = gorget_string_clone_to_owned(__v5);      // Ptr→OWNED materialize
        memcpy(&__s0, &__s5, sizeof(Str));
        return *(Str*)&__s0;            // owned Str, caller takes ownership
}
```
The callee's DECLARED return type is `String` (owned), not `Ptr(String)`. The
EXISTING return-boundary materialization (`gorget_string_clone_to_owned` at
`__bb4`) clones the `Ptr(String)` element to an owned `Str` BEFORE the
cross-frame return — the same Ptr→owned clone the local
`ensure_owned_at_boundary` does, but here it is at the callee's `return xs[i]`,
which is a `lower_return` boundary that already owns. So the cross-frame ABI is
uniform: an owned `Str` on the no-fault path, a zeroed sentinel on the fault
path. **This is a devbook/24 rule-1 win: the materialization is at the WRITE site
(callee return), not reconstructed at the caller's call boundary.**

**MEASURED ASan/UBSan-clean** (`gg build --sanitize`, `ASAN_OPTIONS=detect_leaks=1`):
- `Vector[String]` deep bounds: OOB→"missing", in-bounds→"bob", exit 0, no
  leak/double-free.
- A drop-bearing callee (a live `String guard` local across the OOB read): the
  early-exit drops run on the fault path → `guard` dropped exactly once, clean. ✅

### 1.5 Tag value + dispatch — CONFIRMED

`Fault` variants `Overflow`(0)/`DivByZero`(1)/`Bounds`(2)
(`src/ir/lowering/generics/substitute.rs:332/336/340`, semantic twin
`src/semantic/resolve.rs:178`). Tag = ordinal + 1 ⇒ **Bounds = 3** (0 reserved
for "no fault"). The emitted C writes `*(int32_t*)__fault_slot = 3` (§1.4), and
the tag-switch computes `bounds_tag = resolve_variant_ordinal("Fault","Bounds")+1
= 3`. Matches the registry, no magic literal. ✅

---

## 2. THE PROTOTYPE — MEASURED, both backends (REVERTED, not integrated)

Throwaway edits across 11 files (the §3 diff), built clean (`cargo build` 0
errors, `cargo test --lib` 1084/0, `cargo test --test lints` 29/0 after the 2→3
bump), MEASURED, then `git checkout --` reverted (tree clean, 0 diff lines).

| Case | Fixture (shape) | C stdout / exit | LLVM stdout / exit | Verdict |
|---|---|---|---|---|
| Deep bounds caught | `getx(xs,99) catch Fault.Bounds: 999` | `999` / 0 | `999` / 0 | ✅ caught |
| Deep bounds in-bounds | `getx(xs,1) catch Fault.Bounds: 999` | `20` / 0 | `20` / 0 | ✅ no fault |
| Deep bounds binding | `catch f: match f` Bounds arm → 7 | `7` / 0 | `7` / 0 | ✅ tag selects Bounds arm |
| Deep bounds UNCAUGHT | no catch → panic | `42`+`index out of bounds` / 1 | same / 1 | ✅ panic-by-default |
| **Mixed callee, Bounds-only catch** | callee has `xs[i] + a*b`; catch Bounds; 1st OOB, 2nd overflow | `5`+`integer overflow` / 1 | same / 1 | ✅ **cross-category re-panic** |
| **Bounds in Overflow-only catch** | callee OOB; `catch Fault.Overflow:` only | `index out of bounds` / 1 | same / 1 | ✅ **NOT swallowed** |
| **Resource element** | `Vector[String]` deep bounds | `missing`,`bob` / 0 | same / 0 | ✅ + ASan/UBSan-clean |
| Drop-bearing callee | live `String guard` across OOB | `fallback`,`guard-alive:alice` / 0 | — | ✅ ASan/UBSan-clean, 1 drop |
| Local mixed (regression) | `(xs[i]+a*b) catch Fault.Bounds:` | `14`,`-1`,`integer overflow` / 1 | same / 1 | ✅ local path intact |
| Full `fault_` suite | 34 fixtures | 34/0 | 34/0 | ✅ no regression |
| `vector_`/`array_`/`index_`/`bounds_check` | 57 fixtures | 57/0 | — | ✅ no regression |
| `catch_`/`throws_`/`result_` | 76 fixtures | 76/0 | — | ✅ no regression |

The two **bold** rows are the load-bearing correctness proofs: a Bounds fault a
scope doesn't catch must RE-PANIC, not silently fall through. The `bounds_panic`
field + always-Some gate resolution (§0, §3) makes this work uniformly on both
backends.

**Self-host / bootstrap impact: NONE.** No self-host source uses `(expr) catch
Fault.X:` over a call (the 10 `catch Fault.` matches in `self_host_*` are all
COMMENTS in the self-host's own lowering code, verified). So
`compute_participating_fault_fns` hits its empty-set fast path
(`fault_participation.rs:182-187`) for the self-host → the participation set is
empty → signatures unchanged → `bootstrap_fixed_point` untouched (new GIR field
defaults). The new deep-bounds fixtures register as not-yet-at-parity in the
diagnostic `self_host_runtime_diff` (honest, never excluding a self-host failure).

---

## 3. THE EXACT EXTENSION SITES (the prototype diff, ~120 lines / 11 files)

The full diff is at `scratchpad/inc21d_proto.diff`. Site-by-site:

1. **`src/ir/instructions.rs`** (`FaultableCall`, after `divzero_handler:` ~`:391`):
   add `bounds_handler: Option<BlockId>,` field + doc-comment.

2. **`src/ir/builder.rs`** (`fault_call` `:393`, `fault_call_void` `:414`): add a
   `bounds_handler: Option<BlockId>` param to BOTH ctors + thread into the
   `Instruction::FaultableCall { … bounds_handler }`.

3. **`src/ir/lowering/context.rs`** (`FaultScope`, after `div_zero_panic` `:328`):
   add `pub bounds_panic: BlockId,` field + doc-comment.

4. **`src/ir/lowering/functions.rs`** (`setup_fault_return_scope` `:65`):
   - return type `(BlockId, BlockId) → (BlockId, BlockId, BlockId)`;
   - add `let bounds_return_bb = builder.new_block();` + a `bounds_panic` block
     (`gorget_panic("index out of bounds")` + `unreachable`, mirroring the
     `div_*_panic` blocks);
   - `bounds_handler: None → Some(bounds_return_bb)`; add `bounds_panic` to the
     `FaultScope { … }`; return the triple.
   - the **3 fill-call sites** (`:1046/1140/1157`): destructure
     `(overflow_bb, divzero_bb, bounds_bb)`; add
     `fill_fault_return_block(ctx, b, bounds_bb, slot, …, "Bounds", "index out of bounds");`.

5. **`src/ir/lowering/exprs/mod.rs`** (`lower_fault_catch_expr` `:3629`): add a
   `bounds_panic` block (same shape) + `bounds_panic` to the LOCAL
   `FaultScope { … }` (`:3662`). (The local `bounds_entry` already exists.)

6. **`src/ir/lowering/exprs/calls.rs`** (the gate `:1377`):
   - tuple → triple: add `Some(s.bounds_handler.unwrap_or(s.bounds_panic))` and a
     `None` to the no-scope arm;
   - slot-alloc predicate (`:1443`): `|| fault_bounds_handler.is_some()`;
   - both `builder.fault_call{,_void}` calls (`:1466/1469`): add
     `fault_bounds_handler`.

7. **`src/ir/lowering/fault_participation.rs`** (§1.3):
   `pattern_catches_arith → pattern_catches_fault` (+`"Bounds"`);
   `UncaughtArithDetector → UncaughtFaultDetector` (+ the `Expr::Index` arm);
   `function_has_uncaught_arith → …_fault`; the 2 module-doc/comment updates.

8. **`src/ir/printer.rs`** (`:517`): add `bounds_handler` to the destructure +
   `if let Some(h) = bounds_handler { write!(out, " bounds->bb{}", h.0) }`.

9. **`src/ir/transforms/optimize.rs`** — the **3** block-id arms (the 2.1c scout's
   FOLD-2, all re-verified): `:1884` (`resolved[]` remap), `:2043` (`remap[]`
   renumber), `:2089` (`successors()`). Each: add `bounds_handler` to the
   destructure + `if let Some(h) = bounds_handler { … }` (mirroring the two
   existing per-category lines). **The successor arm (`:2089`) is THE LINCHPIN** —
   omit it and DCE prunes `bounds_return_bb` → bounds recovery vanishes.

10. **`src/lir/lower/insts.rs`** (the tag-switch `:686`): add `bounds_handler` to
    the destructure; compute `bounds_tag = resolve_variant_ordinal("Fault","Bounds")+1`;
    add `if let Some(h) = bounds_handler { cur = emit_tag_branch(self, cur,
    slot_val, bounds_tag, *h); }` after the divzero arm.

11. **`tests/lints.rs`**: bump `FAULT_CALL_HANDLER_CATEGORIES` 2→3 (`:2751`); update
    the exact-string match in `fault_op_lowering_arms_count` (`:2718`) to
    `"Instruction::FaultableCall { overflow_handler, divzero_handler, bounds_handler, .. }"`.

**`..`-eliding sites that need NO change** (re-verified — they don't name the
handler fields): `tag_ownership.rs:147`, `liveness.rs:324/403`,
`validate.rs:380/429/733/792/1539/2513/2979`, `shared_async.rs:623`, the other
`optimize.rs` `..` arms (`:466/559/1701/1741/2252/2410/2532/2579`),
`sim/dispatch.rs:86/1158`. (The sim's `FaultableCall { …, .. }` arm at `:1158`
delegates to the plain `Call` arm — handler-agnostic, correct.)

---

## 4. THE RESOURCE-ELEMENT ISSUE — characterized + RESOLVED (no split)

See §1.4 for the full trace. Summary: the 2.1c scout's worry was that a
resource-element index read yields `Ptr(T)`, so the cross-frame result-type
unification would differ from the int case. **It does not**, because:

1. The callee's declared return type is the OWNED element (`String`), so its
   `return xs[i]` goes through the normal return boundary
   (`ensure_owned_at_boundary`/`lower_return`), which clones the `Ptr(String)`
   to an owned `Str` (`gorget_string_clone_to_owned`) BEFORE returning.
2. The cross-frame ABI therefore returns an owned `Str` (caller takes ownership,
   `needs_drop` registers it at the call site, `calls.rs:1470`) on the no-fault
   path, and a zeroed sentinel `_0` on the fault path (never read —
   branch-before-read).
3. The drop accounting is the SAME as 2.1c's: `fill_fault_return_block` runs
   `emit_early_exit_drops` on the fault path; the caller registers the owned
   return for drop on the no-fault path. No double-free (the OOB path returns the
   zeroed sentinel, never the element), no leak.

**MEASURED ASan/UBSan-clean** on `Vector[String]` deep bounds + a drop-bearing
callee (§2). **No 2.1d-i/2.1d-ii split is warranted** — int and resource elements
ship as one increment.

(The one thing 2.1d adds that 2.1c lacks is the `bounds_panic` block, §0 — but
that is the cross-CATEGORY re-panic, orthogonal to the resource subtlety. It is
required regardless of element type.)

---

## 5. EXECUTOR BRIEF OUTLINE

**Scope: 2.1d = Bounds cross-frame propagation, int + resource elements, incl.
the `bounds_panic` cross-category re-panic block. One pass, ~120-line mechanical
diff (§3). LIFT THE PROTOTYPE DIRECTLY** (it compiled clean + measured green on
both backends; the executor re-implements under full discipline + adds the
fixtures/snapshots).

Sub-slices:
1. **2.1d-a — callee tag-write + `bounds_panic`** (`functions.rs`, `context.rs`,
   `exprs/mod.rs`): the third return block + `setup_fault_return_scope` triple +
   the 3 fill-call sites + the `bounds_panic` field populated in BOTH FaultScope
   construction sites (participating-callee AND local-catch). (§3 items 3,4,5.)
2. **2.1d-b — participation** (`fault_participation.rs`):
   `pattern_catches_fault` + the `Expr::Index` detector arm + the renames.
   (§3 item 7.)
3. **2.1d-c — the tag-dispatch** (`instructions.rs`, `builder.rs`, `calls.rs`,
   `printer.rs`, the 3 `optimize.rs` arms, `insts.rs`): the `bounds_handler`
   field + gate resolution + tag-switch arm. (§3 items 1,2,6,8,9,10.)
4. **2.1d-d — fixtures + lock-in + lint bump** (§3 item 11):
   - `fault_deep_catch_bounds.gg` → `getx(xs,99) catch Fault.Bounds: 999` → `999`.
   - `fault_deep_catch_bounds_binding.gg` → `catch f: match f` Bounds arm → `7`
     (the tag-dispatch regression guard).
   - `fault_deep_uncaught_bounds_panic.gg` → deep_catcher prints `42`, then an
     uncaught deep OOB panics `index out of bounds`, exit 1 (`run_gg_panics_with_stdout`).
   - `fault_deep_catch_bounds_resource.gg` → `Vector[String]` deep bounds →
     `missing`/`bob`; run under ASan/UBSan (the resource drop-gate).
   - `fault_deep_catch_bounds_drop.gg` → a Drop-bearing local live across the OOB
     in the callee → ASan/UBSan-clean, dropped once.
   - **A mixed-callee fixture** (the Core-#8 cross-category guard): a callee with
     BOTH `v[i]` and `a*b`, caught by `catch Fault.Bounds:` ONLY → the OOB is
     caught, the overflow RE-PANICS "integer overflow", exit 1. **AND** the
     inverse: a Bounds caught only by `catch Fault.Overflow:` → re-panics "index
     out of bounds", exit 1 (this is the SWALLOW guard — the one the
     `bounds_panic` field exists for).
   - Snapshot the C stdout into `tests/fixtures/runtime_snapshots/`; each runs
     under `GG_BACKEND=llvm` too (parity).
   - Bump `FAULT_CALL_HANDLER_CATEGORIES` 2→3 + the `fault_op_lowering_arms_count`
     match-string (§3 item 11).

**Discriminant registry: NONE new** — `Fault.Bounds` (ord 2 → tag 3) already
exists (`generics/substitute.rs:340`, `resolve.rs:178`); 2.1d only READS it.

### Gate battery (parent runs the full sweep)
- `cargo build` + `cargo test --lib` (≥1084/0) — executor self-gates.
- `cargo test --test lints` (29/0 after the 2→3 bump).
- `cargo test --test integration fault_` BOTH backends (C + `GG_BACKEND=llvm`) —
  the ~34-fixture fault prefix + the new deep-bounds fixtures (≥40/40 each).
- **Drop-gate ASan/UBSan-clean** on `fault_deep_catch_bounds_resource` +
  `_drop` (`gg build --sanitize`, `ASAN_OPTIONS=detect_leaks=1`), BOTH backends.
- `self_host_bootstrap_fixed_point` GREEN (zero self-host impact, but PROVE it).
- Regression slice: `vector_`/`array_`/`index_`/`bounds_check`/`catch_`/`throws_`/
  `result_` (measured 0-regress in the prototype).
- Full `cargo test --test integration -- --test-threads=4` (C) + a
  `GG_BACKEND=llvm` fault-prefix sweep — **parent's job, not the executor's.**

---

## 6. REFERENCE-GRADE / BOTH-BACKENDS CONCERNS (Core invariant #8)

- **The cross-category re-panic is the reference-grade linchpin** (the
  `bounds_panic` block). Without it, a deep Bounds caught only by an arith catch
  is SILENTLY SWALLOWED → reads the sentinel → garbage. "Both backends agree on
  the garbage" is NOT a pass — it is the exact phrasing the gate must trip. The
  output-review's acceptance bar is *re-panic with "index out of bounds", exit 1*,
  not "C == LLVM". MEASURED green with the fix (§2, both bold rows).
- **Panic-by-default must match across C/LLVM.** MEASURED: uncaught deep bounds
  panics `index out of bounds`, exit 1, IDENTICAL on both backends (the LLVM path
  inherits the shared GIR `panic_bb` NULL-slot arm). ✅
- **The resource-element materialization is drop-correct on both backends.**
  MEASURED ASan/UBSan-clean (no leak/double-free) on `Vector[String]` deep bounds
  + the drop-bearing callee, both backends. The Ptr→owned clone is at the callee
  return boundary (shared GIR), so neither backend special-cases it. ✅
- **The negative-index semantics carry across frames.** `gorget_array_safe_get`
  treats `index < 0` as OOB (signed index), so a deep `getx(xs, -1) catch
  Fault.Bounds:` is a catchable Bounds across frames — matching the LOCAL
  `fault_catch_bounds_negidx.gg` behavior. (Add a `fault_deep_catch_bounds_negidx`
  fixture if the executor wants belt-and-suspenders; the prototype didn't measure
  it but the mechanism is identical to the positive-OOB case.)

---

## 7. CITE MAP (re-verified 2026-06-25, tip `5d6e9261`)

| Structure | file:line |
|---|---|
| `FaultableCall` (overflow/divzero; bounds doc-comment) | `src/ir/instructions.rs:371-392` (`:369` "Bounds … 2.1d") |
| GIR→LIR tag-switch (per-category `emit_tag_branch` chain) | `src/lir/lower/insts.rs:686-746` |
| call-site gate (resolve `unwrap_or(panic)`, ALWAYS-Some) | `src/ir/lowering/exprs/calls.rs:1376-1474` (`:1443` slot-alloc) |
| `setup_fault_return_scope` (bounds_handler: None ← the hole) | `src/ir/lowering/functions.rs:65-111` (`:106`) |
| `fill_fault_return_block` (already `(variant, panic_msg)`) | `functions.rs:128-183` |
| 3 fill-call sites + participates flag | `functions.rs:1046 / 1143 / 1160 / 1186` |
| `FaultScope` (div_overflow/zero_panic; NO bounds_panic) | `src/ir/lowering/context.rs:308-329` |
| local `lower_fault_catch_expr` (bounds_entry + panic blocks) | `src/ir/lowering/exprs/mod.rs:3601-3676` |
| `bounds_handler_for` gate + faultable-array index read | `src/ir/lowering/exprs/methods.rs:3412-3436 / 3476-3478` |
| `FaultableIndexLoad` (GIR) | `src/ir/instructions.rs:250-256` |
| `FaultableIndexLoad` GIR→LIR (safe-get + NULL-branch) | `src/lir/lower/insts.rs:1222-1272` |
| `gorget_array_safe_get` / panicking `gorget_array_get` | `src/backend/c/runtime/runtime_array.c:41` / panic-print `:33` |
| participation (`pattern_catches_arith`, `is_faultable_arith`, detectors) | `src/ir/lowering/fault_participation.rs:45 / 57 / 68 / 110 / 168 / 211` |
| `Expr::Index` AST variant | `src/parser/ast.rs:550-553` |
| `Fault` variants (Overflow/DivByZero/Bounds = 0/1/2 → tags 1/2/3) | `src/ir/lowering/generics/substitute.rs:332/336/340` (sem twin `resolve.rs:178`) |
| builder `fault_call` / `fault_call_void` | `src/ir/builder.rs:393 / 414` |
| printer `FaultableCall` | `src/ir/printer.rs:517-535` |
| 3 optimize.rs block-id arms (remap/renumber/successors) | `src/ir/transforms/optimize.rs:1884 / 2043 / 2089` |
| sim `FaultableCall` (`..`, delegates to Call) | `src/sim/dispatch.rs:1158` |
| lint `fault_call_handler_category_count` (baseline 2 → 3) | `tests/lints.rs:2748-2793` (`:2751`) |
| lint `fault_op_lowering_arms_count` (exact-string match) | `tests/lints.rs:2710-2724` (`:2718`) |

---

## 8. DOCS THE DESIGN RESTS ON

`error-model.md` §11.1 (Bounds via `gorget_array_safe_get`+NULL-branch, the
fault membership), §11.2 (branch-before-deref CFG, handler-bb constructs the
`Fault` variant — the bounds-return block realizes this for the cross-frame
case); `error-model-inc21c-scout.md` §1.3 (the Bounds mechanism + the
resource-element `Ptr(T)` flag this scout RESOLVES) + §0.6/0.7 (the tag-dispatch
2.1d extends, FOLD-2's three optimize.rs arms, FOLD-5/6 the typed-tag/always-Some
discipline); DONE.md Inc-2.1c `d49e3cea` + Inc-2 `a447c726` (the local Bounds
mechanism); `docs/devbook/24-layering-discipline.md` (rule 1 — the resource Ptr→owned
materialization is at the callee WRITE site, not the caller READ site; rule 2 —
the tag is `resolve_variant_ordinal+1`, the participation flag is typed, no
name-matching; rule 4 — the lint forces the third category through the shared
dispatch).
