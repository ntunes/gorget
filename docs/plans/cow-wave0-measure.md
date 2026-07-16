# CoW write-through/materialize — Wave-0 measurement report

**Scout:** wave-0 measurement. Binaries: worktree `target/release/gg` (C + `--backend=llvm`),
`target/release/ggdef` (definition oracle). Raw log: `/tmp/cow_wave0_measure.log`.
Probe sources: `/tmp/cow_wave0/*.gg`.

**Judges:** C backend · LLVM backend · ggdef (phase-0 subset — records ElabError / eval-error when out of subset).

## Spec grounding (derivation basis)

- language-design §3.1 "one rule": write reaches a real owner through an **unbroken `&` chain** → write-through; chain hits an immutable binding (bare local/param/alias, `for x in coll` element) → **materialize** a private copy *there*, owner untouched.
- prose/02: Borrow positions (bare params, reads, receivers, **match binds, for-vars**) materialize-on-write; **`self` is a bare binding (D2)** — plain-`self` write materializes; `&self` writes through.
- prose/03: `&` = WriteThrough (lands on owner, materialize no-op); `!` = Move.
- prose/04 + D4: types with a transitive custom `Drop` are `drop_tainted`; elaboration **REJECTS all six implicit-copy positions INCLUDING materialize-on-write** for a tainted **live place** (`E_MoveWithoutOperator`; fix-it = `!`/`.clone()`/`&`). Fresh temps still move.
- decisions D2: plain-`self` mutation = uniform CoW (materialize); both compilers wrong today (write through).

## ggdef subset boundaries hit (important — several probes are out-of-subset)

- **`for x in &coll` / `for x in !coll`**: NOT in ggdef (elab error "`for &`/`for !` iteration is Increment B2"). Bare `for x in coll` IS supported.
- **Struct patterns in `match`** (`case Point(x,y)` on a struct value): ggdef does not match them ("no match arm matched … non-exhaustive"). Only enum-variant patterns are in-subset.
- **Dict index write-place** (`d[0].x=`): ggdef eval-error "write projection Index(0) on Dict". Out of subset.
- **`.get_or()`** and similar collection methods: out of phase-0 subset.

So the ggdef oracle is authoritative only for: **bare-for control, plain-`self` (E), drop-taint param/self (T)**. For A / A-alias / C / F / match, expected is prose-derived (out-of-subset noted).

## Measurement table

| Probe | Derived expected (spec cite) | C | LLVM | ggdef | Verdict |
|-------|------------------------------|---|------|-------|---------|
| **A** `for c in &a` (owned root) | `101` — unbroken `&` to owner → write-through (§3.1) | `1` | `1` | out-of-subset (for-`&` = B2) | **BROKEN-AS-FILED** (gap A both backends: `&` mode ignored, degrades to bare materialize) |
| **A-alias** `b=a; for c in &b`; print a then b | `1` then `101` — `&`-chain ends at bare `b` → materialize AT b; a untouched (§3.1) | `1` / `1` | `1` / `1` | out-of-subset | **BROKEN** (a=1 correct; **b=1 WRONG**, want 101 — write lost). NOT the Core#8 write-to-both: `a` is correctly untouched, the write is simply dropped |
| **A-bare** `for c in a` | `1` — bare element = Borrow → materialize (§3.1) | `1` | `1` | `1` | **CORRECT** (all 3 agree) |
| **C** `Dict[int,Point] d; d[0].x=99` | `99` — owned Dict place writes through | `1` | `1` | out-of-subset (Dict write-proj) | **BROKEN-AS-FILED** (gap C both backends: write lost) |
| **E** plain-`self` `bump` mutates self.n | `1` — D2 plain-`self` materializes, caller untouched | `101` | `101` | **`1`** (oracle) | **BROKEN-AS-FILED** (gap E both compilers write through; ggdef adjudicates `1`) |
| **F** snag53 nested `&o.inner.raw[k]=v` | `=1+2` (fixture `.expected`) — nested `&` aliases live sub-object | `` (empty) | `` (empty) | out-of-subset (`.get_or`) | **BROKEN-AS-FILED** (gap F both backends: nested write lost) |
| **match-bind** `case Point(x,y): x=x+100`; print p.x | p.x `1` — match binds are Borrow, mutate materializes | p.x=`1` | p.x=`1` | out-of-subset (struct pattern) | **CoW CORRECT** (materialize holds) — but see discovered defect below |
| **T-param** bare `Res r` (custom Drop) mutate `r.id` | **REJECT** `E_MoveWithoutOperator` — drop-tainted materialize is an implicit copy (D4/prose04) | REJECT (E_MoveWithoutOperator) | REJECT (same) | REJECT (same) | **ALREADY-CORRECT** (all 3 reject — bare-param materialize drop-taint gate is DONE) |
| **T-self** bare `self` (custom Drop) mutate `self.id` | **REJECT** `E_MoveWithoutOperator` — D2 would materialize; a materialize of a tainted live place is rejected (D4) | RUNS: `after 99` + `drop 99` (**WRITE-THROUGH**) | same | **REJECT** (oracle) | **WORSE-THAN-FILED** — both compilers write through (D2 gap E) AND skip the D4 taint gate; ggdef rejects. See T disposition below |
| **T-self control** same but `&self` write-through | `after 99` + `drop 99` (write-through, one drop) | `after 99`/`drop 99` | same | same | **CORRECT** (all 3 agree — the reject fix-it path works) |

## Drop-taint × materialize (review rider 3 / Track 2T — the decision is effectively pre-answered)

- **Bare param + tainted (T-param): already rejected** by production (both backends) AND ggdef, with the exact `E_MoveWithoutOperator` message + `!`/`.clone()` fix-it. The sixth-position (materialize) drop-taint gate is **already live for bare params**.
- **Bare self + tainted (T-self): production WRITES THROUGH** (no materialize at all → no clone → one drop, no double-drop *today*). But that is the D2 gap E, not correctness. **ggdef REJECTS** (`E_MoveWithoutOperator` at materialize-on-write). So the definition's disposition is unambiguous: **reject drop-tainted plain-`self` mutation** — same family as bare param.
- **Consequence for Track 2E:** when 2E makes plain-`self` materialize, it MUST route drop-tainted receivers through the existing `E_MoveWithoutOperator` reject (as bare-param already does) — otherwise a naive "materialize all plain-self" would turn today's single-drop write-through into a **silent clone → double-drop**. The reviewer's recommended disposition (reject) is confirmed by both ggdef and the existing bare-param path. **2T is a small, well-scoped gate, not an open design question** — the owner ruling should ratify "reject", matching what ggdef + bare-param already do. The `&self` fix-it (T-self control) is proven correct on all three judges.

## Discovered defect (tangential to CoW — must be filed)

**Struct-value `match` patterns (undocumented shape) mis-bind AND skip arity checking — both backends (Core#8).**
- **Rotated-by-one bindings:** `Point(1,2)` + `case Point(x,y)` → x=`2`, y=`1` (want 1,2). `Trip(10,20,30)` + `case Trip(a,b,c)` → a=`20`, b=`30`, c=`10` (want 10,20,30). Consistent: binding[i] ← field[(i+1) mod n].
- **No arity check:** `case Point(a)` (1 binding, 2-field struct) passes `gg check` with "OK: no semantic errors".
- Together these smell like struct-value patterns routing through the **enum-variant payload projection path** (a phantom discriminant/tag slot → the +1 offset AND the missing arity gate). Struct-value positional patterns are NOT in the language-reference §8 pattern list (documented patterns are enum-variant / tuple), so this may be an **unintended acceptance of an undocumented shape** rather than a supported feature. ggdef doesn't match struct patterns at all (out of subset), so no oracle.
- The **CoW/materialize aspect is correct** (`p.x` unchanged after the bound-name mutation → materialize holds); the field↔binding mapping + arity gate are the bug. **Both backends identical → reference miscompile, needs fixing in BOTH (or an explicit reject of the shape).** Not in the CoW closed set → does NOT reorder the CoW waves. **Surfaced for the parent to file to TODO.md** (I fix nothing per mandate; did not edit TODO.md to avoid contaminating the shared tree). Not currently exercised by any fixture.

## Wave-1 ordering assessment

No CoW gap is already-fixed; the plan's **1B → 1C → 1A(+I)** order stands. Notes:
- Gap **A** confirmed as "`&` mode bit ignored → for-`&` degrades to bare materialize" on both backends — matches the plan's "mode bit on iterable" fix framing. A-alias confirms the fix must materialize-at-`b` then write into b's copy (NOT write-through to `a`) — the derivation locks expected `1`/`101`.
- Gap **C** (Dict) and **F** (snag53 nested `&`) both confirmed broken both backends — no reorder.
- Gap **E** is the only one with a live ggdef oracle (`1`); it stays wave-2 and is **coupled to 2T** (drop-taint) exactly as the plan says — the T-self measurement proves 2E cannot land without the taint gate.

## Backend parity

C and LLVM **agree on every probe** (same value, same reject, same empty). No backend-parity divergence found — all gaps are shared-GIR-level, as the plan's file map predicts.

## Probe B (self-host value `v[i].field=`) — measured via self-host lowerer driver

Program: `Vector[Point] v = [...]; v[0].x = 88; print(v[0].x)`. Compared Rust `gg build --emit-gir`
vs the self-host `self_host_lowerer/driver.gg` GIR (built this session).

- **Rust (correct, write-through):** `_13 = index_load _1, const 0` → `_13: *Point`; store `_13.*.0 = const 88` derefs through the element pointer; read-back `index_load` → 88.
- **Self-host (broken, write lost):** `_10 = call_extern @gorget_array_get(borrow _8, 0)` → `_10: Point` **value copy** (not `*Point`); `@__field_write_Point_x(borrow _10, 88)` writes into the throwaway `_10`; the later `gorget_array_get` re-reads the element → **stale `10`**, printed instead of `88`.

**Verdict B: BROKEN-AS-FILED** (self-host only; Rust correct). R39-T1's premise ("self-host still miscompiles this shape") **re-verified this session**. Root is exactly as the plan states: self-host `lower_place_base` falls through the `Index` base of a field-store to a value `gorget_array_get` instead of forcing the element `Ptr` (`lower_stmt.gg` `lower_place_base` / `lower_field_write`, Track 1B). This is what keeps it out of the corpus (`self_host_runtime_diff` would count a permanent WRONG) — the inline Rust-only test carries the correct expected output until 1B lands.

## Wave-1 ordering — final

Order **1B → 1C → 1A(+I)** stands, unchanged. Every wave-1 gap (A, B, C) confirmed broken this session; none already-fixed; C+LLVM agree throughout; ggdef out-of-subset for A/C (for-`&`, Dict write-proj) so their expected output is prose-derived and locked by §3.1 (A: 101; A-alias: 1/101; C: 99). Gap E stays wave-2 with a live ggdef oracle (`1`) and is correctly coupled to 2T.
