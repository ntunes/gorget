# CoW Track 1C scout — Dict `d[k].field = x` write-through (BOTH compilers)

**Verdict: 1C is a SMALL, well-scoped fix, prototyped end-to-end and PASSING on
both compilers × both backends. Set is out of scope (Dict-only). The double-eval
hazard is LIVE and the fix must (and my prototype does) close it with a type-only
pre-check.** Nested `d[k].inner.f` (2F) stays out of 1C.

Proto patch: `/tmp/recover_cow1c_proto.patch` (Rust `exprs/mod.rs` + self-host
`lower_stmt.gg`; 2 files). Probes: `/tmp/cow1c/*.gg`.

---

## 1. Premise table (file:line, re-verified this session)

| Premise | Status | Evidence |
|---|---|---|
| Rust `try_resolve_field_place` Index arm gated Array-only | TRUE | `src/ir/lowering/exprs/mod.rs:2582-2589` `is_array … == CollectionKind::Array; if !is_array return None` |
| Non-array collections fall through to `None` (Dict/Set unsupported) | TRUE | same arm comment + measured broken |
| Runtime pointer-getter for Map exists | TRUE — **`gorget_map_get` returns a pointer INTO the value slot** | `runtime_map.c:322-336`: `return (char*)m->values + idx*m->val_size` |
| LIR `IndexLoad` already routes Dict→`gorget_map_get` | TRUE | `src/lir/lower/insts.rs:1081,1148-1183` (`is_dict` branch) |
| `materialize_collection_element` returns raw ptr for a `Ptr(T)` dst (no clone) | TRUE | `insts.rs:1874-1884` (early return, stores `ptr_val`) — works for Array AND Map |
| `infer_collection_element_type` maps `Dict__K__V`→V | TRUE | `exprs/methods.rs:3837+` (`Dict__`/`Map__` → value name) |
| `MapGet` sig = `(Ptr, VoidElem)→Ptr` (key by pointer, LIR handles ABI) | TRUE | `src/lir/runtime.rs:415` |
| Self-host 1B producer `lower_field_place_base` is the 1C extension point | TRUE | `lower_stmt.gg:1576-1660`; commit `565392d8` diff confirms "Track 1C (Dict/Set) extend THIS producer" |
| Self-host Dict value-type helper `index_value_type_name` | TRUE | `lower_types.gg:2641` (Dict/HashMap → value name) |
| ggdef out-of-subset for Dict write-place | TRUE (re-verified from source) | `spec/ggdef/src/eval.rs:923 navigate_write` has arms for Struct/Vector/Tuple/Enum only → a Map value + Index write-proj hits `IllFormed`. Matches wave-0. |
| Set has no field-store shape | TRUE | `Set[Point]` REJECTED at check (elements must be Hashable); `Set[int] s[0]` passes check but FAILS C-compile (separate checker gap — filed below). `gorget_map_get` returns sentinel `(void*)1` in set-mode (`runtime_map.c:330`). |

## 2. Measured broken matrix — pre-fix (current tree, this session)

Program shape: `Dict[K,Point] d; d[k]=Point(1,2); <op>; print(d[k].x)` → expected 99 / 41.

| Probe | Expected (spec) | C (pre) | LLVM (pre) | ggdef |
|---|---|---|---|---|
| plain `d[0].x = 99` | 99 (§3.1 owned place WT) | **1** | **1** | out-of-subset |
| compound `d[0].x += 40` | 41 | **1** | **1** | out-of-subset |
| strkey `d["a"].x = 99` | 99 | **1** | **1** | out-of-subset |
| nested `d[0].inner.v = 77` | 77 (§3.1; = gap 2F) | **5** | **5** | out-of-subset |
| **double-eval** `make()[0].x=99` | `make` called ONCE | **make called TWICE** | (same) | n/a |
| Set `s[0]` (int) | — | passes check, **C-compile FAILS** | (same) | — |
| Set `Set[Point]` | — | **rejected at check** (Hashable) | (same) | — |

C≡LLVM on every probe (shared-GIR). Expected = §3.1-prose-derived (out-of-subset).

## 3. Prototype — measured before/after, all judges

### Rust (`exprs/mod.rs`): extend the Index-arm gate to `Array | OrderedMap | Map`, preceded by a **type-only pre-check** (`index_base_kind_type_only` → `place_expr_type_only`) that resolves the collection kind from the base's static type WITHOUT lowering, returning `None` before `lower_expr(coll)` for unsupported/unresolvable bases.

| Probe | C (post) | LLVM (post) | ASan+UBSan |
|---|---|---|---|
| plain | **99** | **99** | clean (rc=0, 0 reports) |
| compound | **41** | **41** | clean |
| strkey | **99** | **99** | clean |
| nested | **77** ✓ bonus | **77** | clean |
| `make()[0].x` | **make once** | — | — |
| nested-arr `m[0][0].x` (regression) | **55** | **55** | — |
| dict-of-vec `d[0][0].x` (mixed nest) | **66** | **66** | — |

### Self-host (`lower_stmt.gg` `lower_field_place_base`): add a `case CkDict(): fpb_getter = "gorget_map_get"` arm (swapped the `bool fpb_is_array` flag for a `String fpb_getter`; emit uses `fpb_getter`).

| Probe (via `driver … --emit-c` → cc → run) | Self-host (post) | ASan |
|---|---|---|
| plain | **99** | clean |
| compound | **41** | clean |
| strkey | **99** | clean |
| nested | **5** (still broken — 2F, single-level `EIndex` only) | — |
| **regression** array 1B fixture `cow_value_index_field_writethrough` | `99/3/88` = Rust oracle | — |
| **regression** `test_dict_int_keys`, `test_dict_str_values` | = Rust oracle | — |

### Gates run this session (Rust prototype)
- `cargo test --lib` → **1107/0**.
- `cargo test --test integration -- cow_ dict_ hashmap_ index_` C → **183/0/2ign**; LLVM → **183/0/2ign**.
- ASan+UBSan on plain/strkey/compound (Rust C AND self-host C) → clean.

## 4. Runtime-layout notes (the model)

`gorget_map_get(&map, key) → void*` points **directly into the map's value slot**
(`values + idx*val_size`). Identical role to `gorget_array_get`'s pointer into the
element buffer. So a `d[k].field = x` place resolves EXACTLY like an array's:
`index_load(coll, key, Ptr(V))` → LIR `is_dict` → `gorget_map_get` → `materialize_
collection_element` (Ptr-dst → raw ptr, no clone) → the pointer-deref walk appends
`Deref + Field(idx)` → the store writes through into the map buffer. The key is
passed via `A::VoidElem` (LIR spills+addresses it) — unchanged from the working
Dict READ/index-assign paths. **No new runtime symbol, no backend change** — pure
GIR-layer place resolution; C and LLVM inherit it identically.

## 5. Double-eval hazard — CONFIRMED LIVE; closed by the fix

`make()[0].x = 99` (side-effecting Dict producer) calls `make()` **TWICE** today:
the Index arm lowers `coll=make()` (effect #1), returns `None` (not array), then
the caller's fallback (`lower_field_assign` `assigns.rs:678-700`) re-lowers the
whole `make()[0]` (effect #2). My `index_base_kind_type_only` pre-check resolves
the kind type-only and returns `None` **before** `lower_expr(coll)` for
unresolvable/side-effecting bases → the fallback lowers `coll` exactly once
(measured: "make called" once). For SUPPORTED kinds the arm lowers once and
returns `Some` (no fallback). The pre-check recurses through pure place spines
(`Identifier`/`self`/`FieldAccess`/nested `Index`, incl. globals) so nested and
mixed chains keep single-eval; only genuinely side-effecting producers stop early.

## 6. Sibling grep (Core #4) — the enumerated place-resolution class

Grep `CollectionKind::Array | is_array` across `src/ir/lowering/`. The
**write-through-place** members of the class (what 1C touches):

| Site | Role | 1C disposition |
|---|---|---|
| `exprs/mod.rs:2582` (`try_resolve_field_place` Index arm) | field-store place: `d[k].field=` / `+=` | **FIXED (this track — the one Rust producer)** |
| SH `lower_stmt.gg:1606` (`lower_field_place_base`) | self-host mirror | **FIXED (the one SH producer)** |

Already-Dict-correct (NOT Array-only, no change needed): `lower_index_assign`
(`assigns.rs:1027` `is_vector` — Dict→`gorget_map_put` dispatched separately),
`lower_index_compound_assign` (`assigns.rs:1559` + SH `lower_stmt.gg:2027`),
`lower_index_access` read (`methods.rs:3757` + LIR `is_dict`). The remaining
`is_array` sites (`methods.rs` `get/first/last/pop/sort/zip`, `for_loops.rs:233`)
are **method-dispatch**, not field-place — out of the CoW write-through class.
**Net: exactly two producers, one per compiler — both fixed. No third sibling.**

## 7. Executor plan + gates

**Rust** (`src/ir/lowering/exprs/mod.rs`): the two new type-only helpers
(`place_expr_type_only`, `index_base_kind_type_only`) above `try_resolve_field_place`
+ the Index-arm rewrite (gate on the pre-check kind ∈ {Array,OrderedMap,Map};
drop the old post-lowering `is_array` gate). Diff in the patch (~90 lines).

**Self-host** (`tests/fixtures/self_host_lowerer/lower_stmt.gg`
`lower_field_place_base`, ~:1603-1657): `bool fpb_is_array` → `String fpb_getter`;
add `case CkDict(): fpb_getter = "gorget_map_get"`; emit uses `fpb_getter`.
(Nested `EFieldAccess(EIndex)` is NOT handled here — that's 2F; leave it.)

**Fixture** — mirror the 1B pattern exactly:
- `tests/fixtures/cow_dict_index_field_writethrough.gg` covering plain +
  compound + String-key, all single-level, on a LOCAL Dict.
  Expected stdout = **§3.1-prose-derived** (99 / 41 / 99) with an explicit
  **out-of-subset note** in the fixture/ test comment
  (`navigate_write` has no Map arm — NOT ggdef-adjudicated, unlike 1B's local
  Vector half).
- `#[test] fn cow_dict_index_field_writethrough()` using `run_gg(...)` (covers
  C by default + LLVM under the LLVM sweep), + a `runtime_snapshots/…​.out`
  snapshot to auto-enroll the self-host lane (integration.rs ~:21452 is the
  template).
- **ggdef-lane / corpus caveat (the 1B lesson, EXTENDED):** because this fixture
  is OUT of ggdef's subset, add its filename to the `EXCLUDE` lists in BOTH
  `spec/ggdef/tests/corpus_b.rs:37` AND `spec/ggdef/tests/corpus_b1.rs:35`
  (documented reason: "Dict index write-place outside phase-0 subset —
  `navigate_write` eval.rs:923 has no Map arm; expected is §3.1-prose-derived").
  The 1B fixture is already in both lists — follow it. **Skipping this bricks
  corpus_b/b1** (they harvest inline expectations and run every non-excluded
  fixture through ggdef, which `IllFormed`s on the Dict write-proj).

**Gates (per campaign plan §Verification + the expectation-adding rider):**
```
cargo test --lib
cargo test --test integration -- cow_ dict_ hashmap_ index_ --test-threads=4          # C
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release -- cow_ dict_ hashmap_ index_ --test-threads=4
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
cargo test -p ggdef -- --test-threads=4          # MANDATORY (fixture-adding ⇒ ggdef-lane event; proves the EXCLUDE lands)
cargo test --test spec_conformance -- --test-threads=4
# ASan on the new fixture (Rust C + self-host C), per box_deref_asan mechanism (integration.rs:22134)
```
Wave-1 CLOSE (parent): full C + full LLVM integration + ggdef + spec_conformance
+ parity regen (`self_host_runtime_diff` — expect WRONG↓). The new snapshot
auto-enrolls the self-host runtime lane.

## 8. Zone note (for zone-briefing)

- Rust hunks: `src/ir/lowering/exprs/mod.rs` — new helpers inserted just BEFORE
  `try_resolve_field_place` (was ~:2485) and the Index arm (~:2537-2595). No
  overlap with the tuple-DefId `SVarDecl` work (that's in `lower_*.gg`, not
  Rust exprs).
- Self-host hunks: `tests/fixtures/self_host_lowerer/lower_stmt.gg`
  `lower_field_place_base` **~:1603-1657 only**. The queued tuple-DefId
  `SVarDecl` construction work is elsewhere in `lower*.gg` — DISJOINT from this
  field-place producer. Zone `lower_stmt.gg place/field/index` is shared with 1B
  (landed), 2F (later) — serialize 2F after 1C.

## 9. NEW bugs discovered (file — do NOT fix here)

1. **Set indexing accepted by the checker, un-lowerable (both backends).**
   `Set[int] s; print(s[0])` passes `gg check` ("OK: no semantic errors") but the
   emitted C FAILS to compile (unresolved). A Set element is its key — `s[k]` has
   no meaning; the typechecker should REJECT Set subscript. Undocumented-shape
   acceptance, latent. (Tangential to CoW; not in the closed set.)
2. **Nested-through-Dict divergence Rust vs self-host (pre-existing 2F, newly
   visible).** After 1C, Rust `d[0].inner.v = x` WRITES THROUGH (77, via
   `try_resolve_field_place` recursion over `EFieldAccess(EIndex)`), but the
   self-host `lower_field_place_base` handles only a single-level `EIndex` → still
   5. This is the filed 2F gap (snag #53 family); 1C does not close it and the two
   compilers diverge on that shape until 2F. Do NOT add a nested Dict fixture in
   1C (it would fail the self-host lane). Flag for 2F: extend
   `lower_field_place_base` to recurse `EFieldAccess` bases (Rust already does).
