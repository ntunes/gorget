# Parity Triage Scout — 2026-06-25

Scout of three WRONG-OUTPUT self-host parity bugs from the handover backlog
(TODO "4d", `string_error_handling`, `shared_stale_match`). Every yield below is
**end-to-end measured** (self-host emit-c → cc → run → diff vs `gg run` oracle,
plus full-corpus `self_host_runtime_diff` before/after on an identical 1300-fixture
corpus). Base: gorget-1 tip `e02627ae`.

Measurement harness used per-fixture: `driver F lib --emit-c --runtime-dir=<abs>`
→ `cc -O0 -w -o bin file.c -lm -lpthread` → run (mirrors `self_host_emit_cc_run`,
`tests/integration.rs:17515`). Driver force-rebuilt via
`gg build tests/fixtures/self_host_lowerer/driver.gg` (GG_BUILD_TIMEOUT_SECS=600).

---

## RANKED RECOMMENDATION

1. **★ SHIP — Candidate A (Dict.keys()/values() element-type erasure).**
   CONFIRMED +1 MATCH end-to-end (756→757), zero regressions (CC-FAIL/CRASH lists
   byte-identical before/after). One-site, reference-grade fix in
   `infer_method_return_type`; gates all green (lock-in 0-regressed,
   fixed_point ok). Brief an executor on the prototype below.
2. **Candidate B (`unwrap_error`→0) — REAL but DEEPER, own scout.** Not a mirage,
   but multi-site: `unwrap_error` is entirely ABSENT from `infer_method_return_type`
   (falls to I64 default → prints `0`), AND the codegen offset is self-admittedly
   "approximate — works for simple scalar payloads" (`lir_codegen.gg:4261`), AND
   the Result struct's distinct error-payload layout for a `String` error needs
   verifying. ≥3 sites + a layout question. Not in scope for a narrow flip today.
3. **Candidate C (`shared_stale_match`) — MIRAGE for a narrow fix; it's the
   already-filed sync-spawn re-architecture (TODO line 235).** The divergence is
   fully explained by "self-host `spawn worker(&x)` runs SYNCHRONOUSLY inline
   instead of deferring to `.await()`". No lowering one-liner; it's a Task/scheduler
   change. Already tracked. Do NOT chase here.

A confirmed +1 (A) beats two "should-works". Ship A.

---

## Candidate A — Dict.keys()/values() element-type erasure (TODO "4d") ✅ CONFIRMED +1

### Bug (both outputs measured)
The TODO framed this as "`Dict.get(k).unwrap()` in a condition inside
`for k in Dict.keys()` drops the branch." **The real root cause is narrower and
more general:** ANY `for k in <Dict>.keys()` (or `.values()`) drops the ENTIRE
loop in the self-host, independent of the body.

Repro `tests/fixtures/dk3.gg` (created this scout):
```
out size: 1   ← oracle (gg run)
out size: 0   ← self-host (WRONG)
```
Isolation variants (all RUN):
| variant | shape | self-host |
|---|---|---|
| B | `for k in ks` (ks a Vector local) | MATCH (2/2) |
| C | `Vector[int] ks = a.keys(); for k in ks` | MATCH (2/2) |
| A | `for k in a.keys()` (iterate the call directly) | **DROPPED (0 vs 2)** |

Emitted C for variant A contains the smoking gun:
`/* [lower_fail] SFor: could not resolve element type for GorgetArray */`
— the loop body, the `get`, the condition, and the `push` are all absent.

### Root cause (post-split file:line)
`infer_method_return_type` (`lower_types.gg:1921`) returns the **bare
`GorgetArray`** for `keys`/`values`, erasing the element type. Then
`lower_for` (`lower_loops.gg:174`) classifies it `CkVector` and routes to
`lower_for_vector` (`lower_loops.gg:224-228`), whose first line is:
```
String elem_name = collection_element_type(coll_tn)   # coll_tn = "GorgetArray"
if elem_name == "":
    lower_fail("SFor: could not resolve element type for " + coll_tn)   # ← drops loop
    return
```
`collection_element_type("GorgetArray")` (`lower_types.gg:2166`) returns `""`
because there is no `Vector__<T>` prefix to slice. The element type was lost one
layer up at the return-type-inference site — a textbook "fix at the write site"
case (CLAUDE.md invariant #1): the loop reader was faithful; the inference WRITER
was lossy.

### Rust reference (the oracle)
`src/ir/lowering/builtins.rs:393-406` — `keys` returns `Vector__{ctx.elem_name}`
(the dict KEY type), `values` returns `Vector__{ctx.val_name}` (the dict VALUE
type), falling back to `GorgetArray` only when the typed lookup fails. The
self-host's `items` arm (`lower_types.gg:1880-1920`) ALREADY does the
`Dict__K__V`-name split to recover K and V for a tuple — this fix mirrors that
exact, in-tree machinery for the single types.

### Prototype diff (`tests/fixtures/self_host_lowerer/lower_types.gg`, the `keys`/`values` arm)
Replaces the one-line `return lookup_or_register_named(&gmod, "GorgetArray")`
with the receiver-`Dict__K__V` split (mirroring the adjacent `items` arm), returning
`Vector__K` for `keys` and `Vector__V` for `values`; keeps the bare `GorgetArray`
fallback when the receiver isn't a recognized Dict/HashMap/GorgetMap. +38 lines,
one site. (Full diff is the committed working-tree change to `lower_types.gg`.)

### Measured flip
- `dk3.gg`: WRONG (`out size: 0`) → **MATCH (`out size: 1`)**.
- `for k in a.keys(): out.push(k)` (int keys): 0 → 2 ✓
- `for v in a.values()` (string-keyed, sum): MATCH ✓
- `for k in a.keys()` (string keys, concat): MATCH ✓
- existing `dict_keys_values.gg` fixture: still MATCH (regression-checked) ✓

### Full-corpus PARITY delta (identical 1300-fixture corpus, dk3 present both runs)
```
BEFORE: MATCH 756  WRONG 89  CC-FAIL 194  CRASH 33  DRIVER-FAIL 1  → 756/1073 = 70.5%
AFTER:  MATCH 757  WRONG 88  CC-FAIL 194  CRASH 33  DRIVER-FAIL 1  → 757/1073 = 70.5%
```
WRONG→MATCH diff = **exactly {dk3}**, new WRONG regressions = **{} (none)**.
CC-FAIL list and CRASH list are **byte-identical** before/after (no
WRONG→CRASH/CC-FAIL hidden moves). Clean isolated +1.

### Gate battery (all green WITH the fix in place)
- `self_host_runtime` lock-in: **0 regressed, ok** (65.8s).
- `self_host_runtime_diff`: 757/1073 (+1).
- `self_host_bootstrap_fixed_point`: **ok** (321s) — self-host still reproduces
  itself byte-identically; the driver self-compiles `.keys()`/`.values()` loops,
  so re-convergence is the load-bearing validation that the fix is internally
  consistent.

### Executor brief (recommended)
- Land the `lower_types.gg` `keys`/`values` arm fix + commit `tests/fixtures/dk3.gg`
  (oracle `out size: 1`).
- Add `dk3` to the `self_host_runtime` lock-in passing set (regen snapshot).
- Note: `lower_types.gg` is a regular file in `self_host_lowerer` (NOT symlinked) —
  the runtime-diff/bootstrap path only needs this one dir. The OTHER self-host
  dirs (typechecker/resolver/parser/lexer) have independent `lower_types.gg`
  copies but are not exercised by the runtime path; touching them is unnecessary
  for parity but worth a one-line note in the commit so a future audit doesn't
  flag drift.
- Gate battery: lock-in + fixed_point (lowering change) + parity re-confirm.

---

## Candidate B — `string_error_handling` `unwrap_error()`→0 (REAL, deeper)

### Bug (both outputs measured)
```
oracle:    -5: negative: -5     abc: not a number: abc     empty: empty input
self-host: -5: 0                abc: 0                      empty: 0
```
`match r: case Error(e): ...` arms work fine (the loop section matches). It is
specifically `Result[int, String].unwrap_error()` (used in the f-strings at
`string_error_handling.gg:10/13/16`) that yields `0`.

### Root cause (post-split — why it's deeper than A)
Three separate gaps, not one inference miss:
1. **Type inference: `unwrap_error`/`unwrap_err` is ABSENT from
   `infer_method_return_type`** (`lower_types.gg` — only `unwrap` is handled at
   `:1981`). It falls through to the I64 default → the destination local is
   int-typed (size 8) → `0` prints. Fixing this alone needs the `Result__OK__ERR`
   name-split to return the SECOND (error) type — analogous to A's K/V split but
   for the error slot.
2. **Codegen offset is self-admittedly approximate.** `lir_codegen.gg:4258-4263`
   `__result_unwrap_error` does `memcpy(&dst, src + 8, sizeof(dst))` with the
   comment *"This is approximate — works for simple scalar payloads."* A `String`
   (16-byte GorgetString) error payload is not a simple scalar; the offset and
   size both need to be correct once the dst is String-typed.
3. **Result struct layout for an `int` OK + `String` ERR** must actually store the
   error payload at the assumed offset 8 — unverified. The Ok and Error payloads
   may overlap (tagged union) or be laid out distinctly; the fix must agree with
   the runtime `Result` layout the self-host emits.

Reference: Rust `unwrap_error` lowering + the typed `Result` payload split
(`src/semantic/typecheck.rs:5579+` for keys/values shows the pattern; the
Result error-payload type is carried typed in Rust, not name-split). This is
≥3 sites + a layout question → **own scout, not a narrow flip.** Not chased here
to avoid a partial fix that prints garbage instead of `0`.

---

## Candidate C — `shared_stale_match` (MIRAGE for a narrow fix — known sync-spawn re-arch)

### Bug (both outputs measured)
```
oracle:    stale match fired \n 1
self-host: updated           \n 1
```
`shared_stale_match.gg`: `shared int x = 0; spawn worker(&x); int val = x;
t.await(); match val: case 0: "stale match fired" else: "updated"; print(x)`.
Oracle: `val` snapshots `0` BEFORE the spawned worker runs (worker runs at
`.await()`), so `case 0` fires; `x` is `1` after. Self-host: `val` reads `1`,
so `else` fires.

### Root cause — already filed (TODO line 235), NOT a lowering one-liner
The second line (`print(x)`) is `1` on BOTH — the worker DOES run. The ONLY
divergence is the snapshot `int val = x` reads `1` instead of `0`, which is
EXACTLY the symptom of "self-host `spawn worker(&x)` runs the task SYNCHRONOUSLY
(inline) instead of deferring to `.await()`" — the already-tracked bug at
TODO line 235. The fix is "Task spawn must enqueue, not call inline (deferred to
await under scheduler=single)" — a scheduler/async-runtime change, not a match-
scrutinee or shared-cell lowering fix. The "match scrutinee reads wrong generation"
framing in the backlog is a red herring: the scrutinee is faithful; the spawn
timing is wrong. **Do not chase as a narrow parity flip.** It rides along when the
sync-spawn bug is fixed.

---

## Artifacts
- New fixture: `tests/fixtures/dk3.gg` (candidate A repro, oracle `out size: 1`).
- Working-tree change: `tests/fixtures/self_host_lowerer/lower_types.gg`
  (candidate A prototype fix — `keys`/`values` arm).
- Logs (scratch, ephemeral): `/tmp/parity_BEFORE_*.log`, `/tmp/parity_AFTER_*.log`,
  `/tmp/lockin_*.log`, `/tmp/fixedpoint_*.log`.
