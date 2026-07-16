# CoW campaign — Track 1A scout (the wave-1 closer) — FINAL

Scout worktree: `/workspace/gorget/.claude/worktrees/agent-a9f9be614ab0b1d8a` (main clean; only 2 self-host `.gg` edits).
Binaries: `target/release/gg` (Rust C+LLVM), `/tmp/cow1a/driver_base` (self-host baseline), `/tmp/cow1a/driver_proto` (self-host + 1A prototype).
Probes: `/tmp/cow1a/probe_*.gg`. Prototype patch: **`/tmp/cow1a_proto.patch`** (182 lines, self-host only).

---

## HEADLINE (read first)

1. **1A IS A BOTH-COMPILERS SEMANTIC FLIP — NOT a self-host-only catch-up like 1B/1C.**
   Baseline self-host MATCHES Rust on every probe (both print the WRONG `1`/`2`). So a
   self-host-only 1A would make self-host print `101` while Rust stays `1` → they DIVERGE →
   `runtime_diff` counts a NEW WRONG. **A self-host-only landing is a parity REGRESSION → NO-GO.**
   1A must land Rust `for_loops.rs` + self-host `lower_loops.gg` + a ggdef out-of-subset note in
   ONE round, pinned by cross-lane fixtures (invariant #9).

2. **BRIEF PREMISE ERROR:** the brief says "extend the SAME producer they built on
   (`lower_field_place_base`, `lower_stmt.gg:1578`)". WRONG for 1A. 1A lives in the **for-loop
   lowering** (`lower_for`/`lower_for_vector`, `lower_loops.gg` / `for_loops.rs`). `lower_field_place_base`
   is UNTOUCHED. The only tie to 1B is that the write-through element uses the same
   `gorget_array_get → GtPtr(elem)` pointer. (The PLAN is right — gap-A files "for_loops.rs; SH
   lower_loops.gg"; the brief mis-cited it.)

3. **NEW GAP DISCOVERED — "A2": Rust's BARE `for c in v` over a RESOURCE-struct element WRITES
   THROUGH on `c.field = x`** (prints `101`), violating §3.1 ("a `for x in coll` element is an
   immutable binding → materialize"). The self-host is CORRECT here (materializes → `1`). This is
   an UNPINNED spec violation (the only resource-for-element fixture,
   `cow_direct_for_element_resource_struct`, is READ-only). It shares the for_loops.rs element-binding
   code with gap A and should be fixed in the same reference-grade restructure.

4. **`for x in &coll` is OUT of the ggdef subset** ("for-`&` = Increment B2"). No ggdef fixture;
   expected output is prose-derived (§3.1). Already filed in **TODO.md lines 28-39** with the exact
   correct diagnosis and marked "own scout" (= this scout).

---

## Premise table (claim → verdict, CURRENT source)

| Claim | Verdict | Evidence |
|---|---|---|
| Gap A live (`for c in &a` write-through lost) | **LIVE** | Rust C+LLVM + self-host all print `1`/`2`; want `101`/`102` |
| Gap A is "Both" compilers | **CONFIRMED** | self-host baseline == Rust (both `1`/`2`) |
| Bare `for c in a` = materialize (`1`) | **CORRECT** both lanes | value-struct: Rust+SH `1` |
| Alias-root `b=a; for c in &b` → `1`/`101` | **BROKEN** both | Rust+SH `1`/`1` (a OK, b write lost) |
| Brief: extend `lower_field_place_base` | **STALE/WRONG** | 1A is `lower_for_vector` (`lower_loops.gg:237`) |
| `for x in &coll` in ggdef subset | **FALSE** (Increment B2) | wave-0 report L19; expected prose-derived |
| SH parses `&` iterable as EMutableBorrow | **CONFIRMED** | parser.gg `parse_for_stmt:3306` (`OWN_BORROW`) + `parse_prefix:2540` → EMutableBorrow on the iterable |
| Rust ignores the `&` mode | **CONFIRMED** | `for_loops.rs:170-193` auto-derefs the Ptr iterable to a value borrow; `iter_source_coll` only set for `Expr::Identifier` |
| Rust behavior = value-vs-resource split, not bare-vs-`&` | **CONFIRMED** | `for_loops.rs:487-539` `is_recursive_struct` gate: resource→Ptr-alias (no materialize guard), value→value-copy |

---

## MEASURED yield (compile → run → diff; NOT source-read)

| probe | baseline SH | **proto SH** | Rust (unchanged) | derived expected (§3.1) |
|---|---|---|---|---|
| `for c in &a` value-struct, owned root | `1`/`2` | **`101`/`102` ✓** | `1`/`2` ✗ | `101`/`102` |
| `for c in a` value-struct, bare (control) | `1` | `1` ✓ | `1` ✓ | `1` |
| `b=a; for c in &b` alias-root | `1`/`1` | **`1`/`101` ✓** | `1`/`1` ✗ | `1`/`101` |
| `for c in &v` resource-struct, owned root | `1` | **`101` ✓** | `101` ✓ (by accident) | `101` |
| `for c in v` resource-struct, bare, mutate | `1` ✓ | `1` ✓ | **`101` ✗ (gap A2)** | `1` |

The proto self-host hits the **reference-grade** shape uniformly: bare → materialize; `&` →
write-through; across BOTH value and resource elements. Rust matches the reference on only 2 of 5
cells.

**Regression / clone safety (self-host):**
- 4 existing bare-for fixtures (`cow_direct_for_element_resource_struct`, `auto_struct_vector`,
  `cow_field_of_for_element_read`, `cow_loop_borrow_propagation`) — emitted-and-run output
  **byte-identical** between base and proto drivers.
- **Clone-neutral:** `array_clone` count identical base vs proto (7 alias / 6 owned-root) — the
  change swaps the element BINDING (materialize-copy → write-through ptr), adds/removes zero clones.
- `cargo test --lib`: **1107 passed / 0 failed** (confirms no Rust source touched).
- The self-host compiler source uses NO `for x in &coll` (grep) → bootstrap fixed-point INERT.

---

## Prototype design (self-host, my zone) — `/tmp/cow1a_proto.patch`

Producer: **`lower_for` + `lower_for_vector`** (`lower_loops.gg`).
1. `lower_for` entry: strip `EMutableBorrow(inner)` iterable → `for_write_through=true`, iterate
   `inner`. New `bool write_through` param on `lower_for_vector`.
2. `lower_for_vector`: when `write_through && !is_enumerate && elem_tid >= PRIM_COUNT`, bind the loop
   var as `gorget_array_get(coll, idx)` typed **GtPtr(elem), LoBorrowed, NO drop reg** (the exact 1B
   element-ptr shape). A body `x.field = v` then routes
   `lower_field_write → lower_field_place_base(EIdentifier x) → lower_place_base → nl_get(x)` = the
   GtPtr; `lower_field_write` passes a ptr base as `OpCopy` → `((T*)ptr)->field = v` writes THROUGH
   (`lower_stmt.gg:1739-1741`). Bare path unchanged. Typed gates only (no name-matching): the
   CkVector/CkDeque `collection_kind` already resolved upstream; `elem_tid >= PRIM_COUNT` excludes
   scalars.

**Sibling enumeration (Core #4) — `lower_for_vector` has 9 call sites:** 1 in `lower_loops.gg`
(statement-for) + **8 in `lower_expr.gg`** (list-comprehension `[x for x in v]`). The prototype
threads `write_through` through all 9; the 8 comprehension sites pass `false` (comprehension-over-`&`
= **Gap I**, deferred). This is exactly why the plan mandates ONE shared iterable-mode helper — the
reference-grade fix should extract `for_iterable_mode(iter_expr) -> (stripped, write_through)` and
feed BOTH the stmt-for and comprehension paths, not two parallel strips.

---

## Cross-lane analysis (Core #9) — why this is bigger than the brief

- Baseline self-host == Rust on ALL probes → self-host-only landing DIVERGES → parity WORSE.
- **Rust lane is bigger than "add `&` write-through".** Rust's `lower_for_array` element binding is
  driven by value-vs-resource (`is_recursive_struct`), IGNORING the `&` mode. Reference-grade Rust
  fix = **restructure `lower_for_array` so the element binding is MODE-driven** (bare → a
  materialize-capable borrow; `&` → MutPtr write-through place), UNIFORM over value/resource. That
  single restructure fixes gap A (value `&`) AND gap A2 (resource bare over-write-through).
- **Alias-root is the HARD cross-lane case.** The self-host passes it via a PRE-EXISTING **eager
  copy** of `Vector[Cell] b = a` (7 `array_clone`s; a CoW pessimization but correct output — b is
  independent, so writing through b's buffer never touches a). **Rust does NOT eager-copy** (`b=a`
  GIR has 0 clones — a lazy borrow-alias). So Rust's `&` write-through element mutation MUST route
  through the alias-sever (`cow_before_mutation` Case 2 / collection materialize) to materialize b
  BEFORE writing, or it writes `101`/`101` into the SHARED buffer (Core #8 write-to-both). The
  executor must verify the Rust alias-root severs; the self-host's eager-copy masks the sever path
  there (a separate note: self-host `b=a` eager-clone is a latent CoW-pessimization worth a TODO,
  but NOT blocking — output is correct).
- ggdef: `for x in &coll` out-of-subset (Increment B2) → explicit note + filed subset gap; NO ggdef
  fixture; expected = prose-derived (§3.1).

---

## GO / NO-GO

- **Self-host prototype: GO.** Reference-grade, no regression, clone-neutral. Patch is a sound base.
- **1A landing: GO — but MANDATORY both-lane** (Rust `for_loops.rs` + self-host `lower_loops.gg` +
  ggdef note), pinned by cross-lane fixtures. **Self-host-only 1A: NO-GO** (parity regression).
- **Fold gap A2 into the same round** — the reference-grade Rust restructure fixes it for free, and
  a `cow_for_bare_resource_elem_materialize.gg` fixture (expected `1`) forces it in (RED on Rust today).

---

## Executor brief recommendations

**Scope:** both-lane track. Rust `for_loops.rs` (`lower_for` + `lower_for_array`, mode-driven
restructure) + self-host `lower_loops.gg` (my patch, productionized) + ggdef out-of-subset note.
Serialize on `lower_loops.gg` / `lower_expr.gg` per the plan's file-zone table.

**Fixtures (the value/resource × bare/& matrix + alias-root; expected = §3.1-derived, out-of-subset noted):**
1. `cow_for_amp_vector_field_writethrough.gg` — value struct, `&`, owned → `101`/`102` (gap A core).
2. `cow_for_amp_vector_alias_root.gg` — `b=a; for c in &b` → `1` then `101` (the HARD case; verify Rust severs).
3. `cow_for_bare_vector_control.gg` — value struct, bare → `1` (materialize control).
4. `cow_for_bare_resource_elem_materialize.gg` — resource struct, bare, mutate `c.field` → `1` (pins gap A2; RED on Rust today).
5. `cow_for_amp_resource_elem_writethrough.gg` — resource struct, `&` → `101`.

**Rust lane:** mode-drive `lower_for_array`'s element binding; route the `&` element mutation through
the alias-sever so the alias-root gives `1`/`101` not `101`/`101`; verify no existing corpus /
self-host fixture relies on bare-for-resource-elem write-through before flipping gap A2 (grep for
`for <x> in <vec>:` followed by `<x>.field =` / mutating method).

**Self-host lane:** start from `/tmp/cow1a_proto.patch`; PRODUCTIONIZE by extracting the shared
iterable-mode helper feeding both stmt-for AND the 8 comprehension callers (absorbs Gap I). Handle
non-owned roots (`for c in &self.field`, `&v[i]`, static) via the 1B ptr-or-borrow base idiom (the
prototype only covers an owned-local root). Add a Deque probe (`gorget_array_get` shared → likely
works).

**ggdef lane:** out-of-subset note (Increment B2) + filed subset gap; no ggdef fixture.

**Gates (fixture-ADDING ⇒ ggdef-lane event, Batch-A/1B lesson):** full C + full LLVM integration,
`cargo test -p ggdef`, `spec_conformance`, `self_host_bootstrap_fixed_point`, ASan on the new
write-through fixtures (the element-ptr no-drop path is the #1 ASan risk — mirror 1B's gate), parity
regen (expect WRONG drop; the value-`&` + resource-bare cells both flip once BOTH lanes land).

**Surprises / smells (reports, not downgrades):**
- Gap A2 (Rust resource bare-for over-write-through) — file HIGH; fold into 1A round.
- Self-host `Vector[Cell] b = a` eager-clones (7 array_clones) — a CoW pessimization (should be
  lazy borrow-alias); correct output but worth a separate TODO (do NOT block 1A on it).
- `lower_for_vector` 9 call sites → the shared-mode-helper refactor is the reference-grade shape.
