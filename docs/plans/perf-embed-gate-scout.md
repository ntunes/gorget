# Confirming/Prototyping Scout — "gate the GG_IMPL embeds behind the relocatable build" (TODO Perf-fix, Option c)

**Verdict: ⛔ REFUTED / NO-GO as scoped.** Fresh end-to-end measurement shows the
721 KB embeds are **NOT** the sweep-doubling cost — gating them saves
**≈0** (≈0.7 s out of a ~50 s self-host self-compile = ~1.3 %, within noise;
≈0 on the Rust-gg side). And the only gate that would actually skip the embed
*evaluation* (a top-level `meta if`) is **un-flattenable by the self-host**, so it
would **break `self_host_bootstrap_fixed_point`**. Two independent kill shots.

The owner's real symptom (sweep ~300 s → ~780 s) is real, but its cause is **not**
the embeds. See "What the cost actually is" below.

---

## 1. Fresh measurement (regenerated this session — do not trust the dated "30→70 s")

Environment: release `gg` (`cargo build --release`, `target/release/gg`), C backend,
`cc` = system default. Wall-clock + peak RSS via a tiny
`resource.getrusage(RUSAGE_CHILDREN)` python wrapper (no `/usr/bin/time` on this box).

**Quick experiment harness:** stub every `embed_file("…")` → `""` in a copy of
`driver.gg` (90 call sites replaced), compile the stubbed copy, diff timings.

| Stage (what runs)                                    | WITH embeds | WITHOUT embeds |   Δ        |
|------------------------------------------------------|------------:|---------------:|-----------:|
| Rust gg front-half  `gg build driver.gg --emit-c-lir`|     3.26 s  |        3.29 s  |  ~0 (noise)|
| `cc` compiling the 26 MB emitted body C              |     8.65 s  |        8.76 s  |  ~0 (noise)|
| Rust gg **full** `gg build driver.gg`                |   12.2 s    |       11.7 s   | ~0.5 s     |
| **Self-host self-compile** `driver F lib --emit-c-lir`|   50.15 s  |       49.48 s  | ~0.7 s (1.3%)|
| Peak RSS, Rust full build                            |   904 MB    |       904 MB   |  0         |
| Peak RSS, self-host self-compile                     |   275 MB    |       272 MB   |  ~0        |
| Emitted body-C size                                  |  27.0 MB    |       26.1 MB  | 0.9 MB (3%)|

**Commands (reproducible):**
```bash
# build seed
cargo build --release
GG=target/release/gg ; D=tests/fixtures/self_host_lowerer/driver.gg ; LIB=$PWD/lib

# WITH embeds — Rust gg full + front-half
$GG build $D -o /tmp/driver_with                       # ~12 s, 904 MB
$GG build $D --emit-c-lir > /tmp/with.clir             # ~3.3 s ; 27 MB

# WITHOUT embeds — stub embed_file("…") -> "" in a fixture-dir copy, then:
$GG build $DRIVER_NOEMBED -o /tmp/driver_noembed       # ~11.7 s, 904 MB
$GG build $DRIVER_NOEMBED --emit-c-lir > /tmp/no.clir  # ~3.3 s ; 26 MB

# self-host self-compile (the ~10×/sweep cost), WITH vs WITHOUT embeds:
/tmp/driver_with   $D            $LIB --emit-c-lir > /tmp/sh_with.clir   # 50.15 s
/tmp/driver_noembed $DRIVER_NE   $LIB --emit-c-lir > /tmp/sh_no.clir     # 49.48 s
```

**Honest current cost of the embeds: ≈0.7 s per self-compile, ≈0.5 s per Rust build, 0 RSS, 3 % of emitted C.** Not the sweep regression.

---

## What the cost actually is (the embeds are exonerated)

A `gg build driver.gg` (~12 s, Rust gg) decomposes as:
- **~3.3 s** Gorget front-half (lex/parse/sema/lower/emit C). Embeds: no effect.
- **~8.7 s** `cc` compiling the **26 MB** emitted C. Embeds are ~0.9 MB of that
  (~3 %); removing them changes cc time by ~0.1 s.

The **self-host self-compile is ~50 s** — 15× slower than Rust gg for the *same*
front-half — because the self-host compiler is itself slow (it's a young compiler
compiling its own ~large source). The embeds contribute ~1.3 % of that. The sweep
runs this self-compile ~10× (the `*_comparison` + `self_host_bootstrap*` family,
including the multi-stage `self_host_bootstrap_fixed_point`), so the self-compile
*is* a big chunk of the sweep — but the **embeds are a rounding error inside it.**

**Why the original scout's premise was wrong:** "per-character escaping of 721 KB"
sounds expensive, but (a) escaping 721 KB is microseconds-to-low-ms of string work,
dwarfed by the multi-second lower/SSA/drop-elab passes over the *whole* self-host
source, and (b) the dominant downstream cost is `cc` on a 26 MB C file, where the
embeds are ~3 %. The estimate was asserted, not compiled-and-run-and-diffed — the
exact trap the project's "scout yield estimates MUST be end-to-end-verified" rule
warns about.

---

## 2. The gate mechanism (what it would be) + 3. PROTOTYPE RESULTS

### Mechanism (Rust gg: works)
`embed_file` is a meta-builtin (`src/semantic/meta.rs:1086`) evaluated at the
`MetaConst` level in Phase 1 (`meta.rs:476-478` → `process_meta_item:606`). Phase 1
does **not** descend into `Item::MetaIf` (`process_meta_item`'s `_ => {}` arm), and
Phase 1.5 `flatten_meta_ifs` (`meta.rs:865`) calls `process_meta_item` **only on the
winning branch** (`:882-884`). So wrapping the embed consts in a top-level
`meta if feature("relocatable"): … else: …` means the off-branch's `embed_file`
calls are **never evaluated**. `feature("…")` (`meta.rs:1033`) reads `--feature`
CLI flags (`src/main.rs:336 parse_features`). Gate point in `driver.gg`: wrap the
runtime const block (lines **58–119**) and the lib const block (lines **196–223**),
with empty-string fallback consts in the `else` branch so the unconditional builders
(`build_embedded_runtime` :125, `build_embedded_lib` :226) stay defined.

### PROTOTYPE — Rust gg (PASS)
`docs/plans/scratch-perf-embed-gate/meta_if_embed_probe.gg`:
```
meta if feature("relocatable"):
    meta String GREETING = embed_file("greeting.txt")
else:
    meta String GREETING = "disk-fallback"
String get_greeting(): return GREETING
void main(): print(get_greeting())
```
```
$ target/release/gg run probe.gg                       → disk-fallback   # else; embed NOT evaluated
$ target/release/gg run probe.gg --feature relocatable → hello-from-embed # then; embed evaluated
```
Confirms: off-branch never touches `greeting.txt` (file need not even exist). The
mechanism is correct on the Rust gg.

### PROTOTYPE — self-host (⛔ BLOCKER)
```
$ /tmp/driver_with probe.gg lib --emit-c
/* [bug] EIdentifier: in fn 'get_greeting': unknown identifier 'GREETING'
   — returning OpConstI64(0) placeholder (WRONG) */
```
**The self-host does not flatten top-level `meta if`.** `tests/fixtures/self_host_lowerer/meta.gg`
handles `IMetaConst`/`IMetaType`/`IMetaTypeFunc` and statement-level subst only —
there is **no `Item`-level `IMetaIf` flattening pass**. A const declared inside a
top-level `meta if` is therefore never defined → the self-compiled binary is broken.

This is **already a known, separately-filed gap**: TODO.md "Case-B META-EXPANSION
CLUSTER … start IMetaIf flatten (highest yield) … mirror Rust `meta.rs:860`". It is
its own deep scout→brief→execute item, *not* a free pre-req of this perf fix.

### `self_host_bootstrap_fixed_point` impact
`driver.gg` IS the program the fixed-point test self-compiles. Gating its embeds
behind a top-level `meta if` would make the self-host emit `OpConstI64(0)` for every
`RT_*`/`LIB_*` reference inside the builders → `build_embedded_runtime`/`_lib`
return junk → fixed-point diverges / the staged binaries are broken. **The gate as
scoped BREAKS the load-bearing self-host gate.** Hard blocker.

---

## 4. Sweep-delta estimate

- Self-compiles of `driver.gg` per sweep: ~**10** (the `serial(self_host_lowerer_driver)`
  family — `lowerer_comparison`, `c_emit_comparison`, `self_host_bootstrap`,
  `self_host_bootstrap_fixed_point` which alone runs stage1→2→3, plus the lint/guard
  tests that reuse the cached stage-0 binary; the stage-0 *build* is cached once via
  `build_gg_dir_cached`, so the per-sweep cost is dominated by the self-host *runs*).
- Per-self-compile embed saving (measured): **~0.7 s**.
- **Best-case sweep delta ≈ 10 × 0.7 s ≈ 7 s** out of ~780 s = **~0.9 %.**

The TODO's "~halves the sweep" is off by ~2 orders of magnitude. There is no
"~halve" hiding here.

---

## 5. Precise edit plan for the executor

**There is no edit to make. Do not implement Option (c).** It buys ~0.9 % at best,
and is blocked on the self-host `IMetaIf` gap (which would break
`self_host_bootstrap_fixed_point`). The correct executor action is to **retire the
TODO "Perf-fix" item** (or rewrite it to point at the real cost), citing this scout.

If, after the self-host gains `meta if` flattening (the separate Case-B item), one
*still* wanted the gate for binary-size / cleanliness reasons (not speed), the edit
would be: wrap `driver.gg:58-119` and `:196-223` in
`meta if feature("relocatable"): … else: <empty-string fallback consts> …`, keep the
builders unconditional, and gate it behind a new `self_host_embed_gate` test that
diffs the `--emit-c-lir` output WITH `--feature relocatable` against the committed
relocatable snapshot AND confirms the non-relocatable build still
fixed-point-converges. But this is a **size/cleanliness** change, not a perf win.

---

## 6. Risks/blockers + the real lead

**Blockers that kill Option (c):**
1. **~0 saving** — the embeds are 3 % of emitted C and ~1.3 % of self-compile time.
2. **Self-host can't flatten top-level `meta if`** (`meta.gg` has no `IMetaIf` pass)
   → gating breaks `self_host_bootstrap_fixed_point`. Gated on the separate Case-B
   "IMetaIf flatten" item.
3. `embed_file(cond ? path : "")` is inexpressible (arg must be a string *literal*),
   so an "`embed_file`-internal feature gate" (option b) isn't clean either and would
   need parallel Rust + self-host changes.

**Where the real perf is (the actual lead for a follow-up perf scout):**
- The sweep's self-host cost is the **self-host self-compile at ~50 s × ~10**. To
  move the sweep you must make the **self-host compiler faster** (profile its
  lower/SSA/drop-elab passes over its own source) and/or shrink the **26 MB emitted
  C** that `cc` then chews on for ~8.7 s. MEMORY angle (per project rule): the Rust
  build peaks at **904 MB RSS** — worth a `--clones=stats` look, but that's a
  different investigation than the embeds.
- A cheaper sweep lever that needs *zero* compiler work: the stage-0 driver binary is
  already cached (`build_gg_dir_cached`); the multi-stage `fixed_point` re-runs the
  ~50 s self-compile per stage. If convergence is provable at stage-2 in practice,
  capping `MAX_GEN` lower (or caching stage-N C across the bootstrap tests) would save
  real wall-clock — but that's a harness change, not the embed gate. (Flagging only;
  not scoped here.)

**Re-verify-before-quoting note:** the dated "30→70 s" / "char-by-char escape is the
cost" / "~halves the sweep" lines in TODO.md are all REFUTED by the fresh
compile-and-run measurement above. They should not enter any brief.
