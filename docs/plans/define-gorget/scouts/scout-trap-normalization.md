# SCOUT REPORT — Trap Normalization (D11) + Throws Totality (D23)
_Read-only scout. Checkpointed incrementally. PID-based path: /tmp/scout_trap_norm_946.md_

Status: COMPLETE (2026-07-10). Compiler built cold; all citations verified; end-to-end measured on C/LLVM/ggdef.

## Sections
1. Premise verification table
2. End-to-end measurement table
3. Runtime-library trap census + proposed TrapKind mapping
4. Design proposal
5. Recommended slicing
6. Owner design questions

---
## SECTION 2 — END-TO-END MEASUREMENT (the blast-radius baseline)

Built compiler cold (`cargo build`, exit 0) + `cargo build -p ggdef`. 8 trapping programs, three evaluators. Commands: C = `gg build <f> && ./bin`; LLVM = `gg build --backend=llvm <f> && ./bin`; GGDEF = `ggdef run <f>`.

| Program (trap class) | C backend (stderr / exit) | LLVM backend | ggdef |
|---|---|---|---|
| bounds `v[5]` | `gorget: panic: index out of bounds: index 5, length 3` / **1** | identical / **1** | `ggdef: trap: index out of bounds` / **101** |
| neg index `v[-1]` | `gorget: panic: index out of bounds: index 18446744073709551615, length 3` / **1** | identical / **1** | `ggdef: trap: index out of bounds` / **101** |
| unwrap None | `<path>:3:11: called \`unwrap()\` on a \`None\` value` / **1** | identical / **1** | `ggdef: trap: called \`unwrap()\` on a \`None\` value` / **101** |
| unwrap Error | `<path>:4:11: called \`unwrap()\` on a \`Error\` value` / **1** | identical / **1** | `ggdef: trap: called \`unwrap()\` on a \`Error\` value` / **101** |
| unwrap_error on Ok | `<path>:4:11: called \`unwrap_error()\` on a \`Ok\` value` / **1** | (assumed identical) | **GAP**: `elaboration error … .unwrap_error() outside phase-0 subset` / **2** |
| overflow `a+1` | `<path>:3:17: integer overflow` / **1** | identical / **1** | `ggdef: trap: arithmetic overflow` / **101** |
| div by zero | `<path>:3:16: division by zero` / **1** | identical / **1** | `ggdef: trap: division by zero` / **101** |
| assert(false) | `<unknown>:0:0: assertion failed` / **1** | identical / **1** | **GAP**: `elaboration error … statement \`assert\` outside phase-0 subset` / **2** |
| panic("boom") | `<path>:2:11: boom` / **1** | identical / **1** | **GAP**: `elaboration error … unresolved callee \`panic\` (Increment B2)` / **2** |

### Key measured findings (CORRECTIONS to the proposal's "surveyed" claims)
1. **C and LLVM are already at FULL PARITY** — byte-identical stderr, identical exit=1, on all reachable classes. The "three exit codes 1/134/139" claim is STALE for reachable compiler-emitted traps: **every reachable trap exits 1.** 139=SIGSEGV is stack-overflow-guard (out of v1, by design). 134=SIGABRT: NOT reproduced by any straightforward program — see finding 4.
2. **Three stderr formats CONFIRMED on production**, and they split by EMIT PATH not trap class:
   - `gorget: panic: <msg>` — NO location — for BOUNDS (top-level `v[i]` routes to the runtime helper `gorget_array_get`, `runtime_array.c:33`, which `fprintf`s directly; the compiler never sees it as a `gorget_panic`).
   - `<file>:<line>:<col>: <msg>` — WITH location — for overflow / divzero / user-panic / unwrap (all go through the GIR `gorget_panic(msg)` → rewritten to `gorget_panic_at` at the C/LLVM boundary).
   - `<unknown>:0:0: <msg>` — for ASSERT: assert lowers to `gorget_panic` too, but the assert lowering does NOT carry a span, so the boundary rewrite emits `<unknown>:0:0`. (Confirmed premise: location threading is missing on the assert path.)
3. **`<detail>` text DIVERGES between ggdef and production** even where both trap: production `integer overflow` vs ggdef `arithmetic overflow`; production `index out of bounds: index 5, length 3` vs ggdef `index out of bounds`. ⇒ conformance must compare the **T_ code + exit ONLY**, never the detail string (design Q1).
4. **ggdef models only 5 of the 8 trap classes.** MISSING: `assert` (AssertFailed), `panic()` (Panic), `unwrap_error` (UnwrapErrorOnOk) — all three hit `elaboration error … outside phase-0 subset` (exit 2). This is a REAL SCOPE EXPANSION for the definition side: to gen fixtures for those three codes, ggdef elaboration must first learn `assert`, `panic()`, and `.unwrap_error()`. Not a "just split the enum" job.
5. **The `abort()`/exit-134 sites are LATENT, in the c_lir backend (not the runtime):** `emit_hof.rs:156` (`unwrap_error on Ok`), `emit_types.rs:246` (`<unknown>:0:0: unwrap_err on Ok`), `mod.rs:3122` (bounds), `:3131` (divzero), `:3137` (generic panic) — inline checks that `fprintf(...); abort();` (exit 134) with YET ANOTHER message format. The common lowering routes through `gorget_panic` (exit 1), so I could not reach them with plain programs, but they are live code in the blast radius — a fourth format + the 134 exit. D11 must fold these too.

---
## SECTION 1 — PREMISE VERIFICATION TABLE

| # | Premise (from brief) | Verdict | Evidence (file:line) |
|---|---|---|---|
| 1 | `panic_normal.c` renders `%s:%d:%d: %s\n` + exit(1); `gorget_panic` → `<unknown>:0:0` | **CONFIRMED** | `src/backend/c/runtime/panic_normal.c:3-8` |
| 1 | LLVM shares `@gorget_panic_at`; rewrite threads loc | **CONFIRMED** | `src/backend/llvm/mod.rs:1500-1506` (declare), `:4507-4528` (rewrite w/ `loc.0/1/2`) |
| 1 | Compiler-emit trap kinds: bounds/unwrap/overflow/divzero/assert/panic | **CONFIRMED + REFINED** | overflow/divzero NULL-slot panic `functions.rs:88-107`; div/overflow/bounds fault-scope panic `exprs/mod.rs:3785-3805`; assert `stmts/mod.rs:2550-2561`; user panic `exprs/calls.rs:565-575`; unwrap guard `lir/lower/insts.rs:3603-3638` (variant words None/Error/Ok at `:4195-4247`) |
| 1 | C emit-boundary rewrites `gorget_panic`→`gorget_panic_at` | **CONFIRMED** | `src/backend/c_lir/emit_call_extern.rs:49-67` |
| 2 | Scattered runtime-lib `fprintf(stderr,"gorget: panic:…"); exit(1)` | **CONFIRMED** | census below (Section 3); ~60 sites across bytes/channel/array/string/alloc/shared |
| 2 | Any `abort()`/exit(134) in runtime | **CORRECTED** | NO `abort()` in `runtime/*.c`. `abort()` lives in **c_lir backend emit** (`emit_hof.rs:156`, `emit_types.rs:246`, `mod.rs:3122/3131/3137`) — latent, exit 134, could not reproduce via plain programs (all reachable traps → exit 1) |
| 3 | ggdef `EXIT_TRAP=101`, `Fault`={Overflow,DivByZero,Bounds,Panic(String)}, `exit_code()`, unwrap→`Fault::Panic`, PROVISIONAL | **CONFIRMED** | `spec/ggdef/src/eval.rs:40` (PROVISIONAL), `:45` (101), `:53-59` (Fault), `:81-90` (exit_code), `:1207` (unwrap Panic) |
| 4 | frontmatter `Expect{exit,stdout}` no `trap:`; `gen_frontmatter` writes block; `adjudicate` code-not-compared, 187 fixtures all exit-0 nonzero-branch dormant | **CONFIRMED** | `spec/ggdef/src/frontmatter.rs:44-48` (Expect), `spec/ggdef/src/lib.rs:146-152` (`render_expect_block_from`), `tests/spec_conformance.rs:230-253` (adjudicate; comment `:220-229`), 187 run fixtures, 0 with `trap:` |
| 4 | ggdef-side lane compares exit+stdout | **CONFIRMED** | `spec/ggdef/tests/spec_conformance_ggdef.rs:92-93` (`got_exit == fm.expect.exit`) |
| 5 | unwrap fix landed — production traps on unwrap None/Error | **CONFIRMED** | `lir/lower/insts.rs:3603-3638` `emit_unwrap_panic_guard`; measured: unwrap None/Error/unwrap_error-on-Ok all trap w/ exit 1 + correct message |
| 6 | D23: checker emits unhandled-throws; any message contains "Result["? | **CONFIRMED LEAK (D23 VIOLATED today)** | non-throws use of a throws call → bare `E_TypeMismatch: expected int, found Result[int, String]` (measured, `d23_nothrows`); operand position leaks it TWICE. No dedicated diagnostic exists |
| 6 | tests/lints.rs E_ ratchet to mirror | **CONFIRMED** | `tests/lints.rs` name-prefix/sidecar ratchets (`:178`,`:344`); ggdef import ratchet `:4815` (FORBIDDEN=ir/semantic/lir/bir/backend, budget 0 fatal) |
| 6 | smith ggdef verdict lane (fuzz-tier home) | **CONFIRMED** | `tests/smith/main.rs:8-48` (ggdef verdict lane, SPEC-DIVERGE), `generator.rs:789` (`generate(seed,tier)`, only tier 0 impl, knob plumbed) |
| 6 | reference §10.1 has totality sentence | **CONFIRMED ABSENT** | `docs/language-reference.md:2406-2418` — only "auto-propagate"; no totality sentence |

---
## SECTION 3 — RUNTIME-LIBRARY TRAP CENSUS + PROPOSED TrapKind MAPPING

All are `fprintf(stderr, "gorget: panic: …"); exit(1)` (exit 1, NO location) unless noted. Grouped by proposed disposition.

### 3a. BOUNDS-shaped (→ `T_Bounds`, the flagship; today the reachable `v[i]` path)
| Site | Message |
|---|---|
| `runtime_array.c:33,81,94` | `index out of bounds: index N, length M` (get / get_ref / set — **the measured top-level `v[5]` path**) |
| `runtime_array.c:156,173,380,445` | swap_remove / swap / insert / vector-slice out of bounds |
| `runtime_string.c:756,773` | string byte_slice / byte index out of bounds |
| `shared_runtime.c:99,107` | shared array index out of bounds |
| `bytes_runtime.c:83,97,156,168,178,192,202,214` + `bytes_f32_runtime.c:5-83` (16 sites) | bytes read/write offset out of bounds |

### 3b. GENERIC user-visible panics (→ `T_Panic`)
| Site | Message | Note |
|---|---|---|
| `runtime_array.c:144` | `pop from empty array` | shape-panic |
| `runtime_string.c:304,328` | `format error` | |
| `channel_runtime.c:51,62,91,164` | `send on closed channel` | phase-3 (channels); fold or defer |
| `channel_runtime.c:118,141,252,286` | `recv on closed empty channel` | phase-3 |
| `bytes_runtime.c:141,145` | cannot open / short read `/dev/urandom` | environmental |
| `runtime_file.c:10,142,163` | cannot open file (read/write) | **D17 says these become `throws IoError`, not traps** — coordinate |
| `runtime_path.c:171` | cannot open directory | D17-class |
| `runtime_error.c:12,22-23` | try-stack overflow / `Unhandled error:` | error-runtime internals |

### 3c. OOM / RESOURCE-EXHAUSTION (→ explicitly OUTSIDE v1 per D11; leave as-is, file follow-up)
| Site | Message |
|---|---|
| `runtime_array.c:298-335` | array capacity overflow / allocation failed (6 sites) |
| `runtime_string_extended.c` (12 sites) | out of memory |
| `runtime_string_array.c:15` | out of memory |
| `runtime_arena_alloc.c:32,34,57,59` · `runtime_pool_alloc.c:84,94` · `runtime_tracking_alloc.c:60` · `runtime_tlsf_alloc.c:259-357` | allocator allocation failed |

### 3d. INVARIANT/DEBUG asserts (→ `T_Panic` or leave; internal "can't happen")
`runtime_array.c:259` (`gorget_array_free` invariant), `runtime_string.c:126` (`gorget_string_free` invariant).

### 3e. NOT traps (exclude): `_exit(127)` child-exec-failed (`process_runtime.c:86`, `process_spawn_runtime.c:63`); metal/sdl/hot_reload GPU/platform stderr; alloc-report/clone-stats diagnostics.

### 3f. c_lir BACKEND inline checks (latent exit-134, FOURTH format — must fold)
| Site | Emits |
|---|---|
| `c_lir/mod.rs:3122` | `{f}:{ln}:{cl}: index out of bounds` + `abort()` → **T_Bounds** |
| `c_lir/mod.rs:3131` | `{f}:{ln}:{cl}: division by zero` + `abort()` → **T_DivByZero** |
| `c_lir/mod.rs:3137` | generic `{f}:{ln}:{cl}: {msg}` + `abort()` → **T_Panic** |
| `c_lir/emit_hof.rs:156` | `{f}:{ln}:{cl}: unwrap_error on Ok` + `abort()` → **T_UnwrapErrorOnOk** |
| `c_lir/emit_types.rs:246` | `<unknown>:0:0: unwrap_err on Ok` + `abort()` → **T_UnwrapErrorOnOk** |

### Census verdict
The **hidden blast radius is bounds** (3a — ~30 sites across 5 runtime files, PLUS the c_lir inline path 3f). Everything in 3b/3d folds to `T_Panic` mechanically. 3c (OOM) is explicitly **outside v1** — leave with a filed follow-up (matches D11 "OOM stays outside v1"). 3f's `abort()`/134 sites are the true "three exit codes" source and MUST be redirected to the new trap entry (kills the 134 + the fourth format in one move). **Cross-track coordination flag:** `runtime_file.c`/`runtime_path.c` (3b) are D17's `throws IoError` targets — do NOT normalize them as `T_Panic`; let D17 convert them to thrown errors. The brief should carve them out.

---
## SECTION 4 — DESIGN PROPOSAL (grounded in cited docs)

### 4.1 The `TrapKind` registry
**Shape** (mirrors `SemanticErrorKind::code()`, `spec/prose/diagnostic-codes.md:8-33`): a closed Rust enum, `code()` an exhaustive catch-all-free `match` → `T_<VariantName>`, so rustc exhaustiveness IS the ratchet.
```rust
pub enum TrapKind { Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk, AssertFailed, Panic }
impl TrapKind {
    pub fn code(&self) -> &'static str { match self { Overflow => "T_Overflow", … Panic => "T_Panic" } } // no `_`
    pub fn is_catchable(&self) -> bool { matches!(self, Overflow|DivByZero|Bounds) } // the §10.9 Fault subset
}
```
**Home (production): a NEW crate-root `src/trap.rs` (`gorget::trap`).** Rationale: read by BOTH `src/ir/lowering/*` AND `src/backend/{c_lir,llvm}` with no layering inversion (backends already depend on `ir`; a root module avoids ir↔backend coupling either direction). Pure data — no compiler decisions — so it does not violate the ggdef fence conceptually.

**ggdef: DUPLICATE, pinned by a parity lint** (recommended over sharing). The import ratchet (`tests/lints.rs:4815`, FORBIDDEN=ir/semantic/lir/bir/backend, budget 0) does NOT forbid `gorget::trap` — so sharing is *mechanically* possible — but standing-rule 5 (ggdef shares lexer+parser+AST+span ONLY, RFC §2.4) and the definitional-independence principle argue for ggdef owning its own `TrapKind` as part of THE DEFINITION, with production matching it. This matches the existing pattern (ggdef re-derives everything; corpus/converter parity is lint-enforced, never shared). A `tests/lints.rs` parity lint asserts the two variant lists + `code()` strings are identical AND that `is_catchable()` ⇔ the prose `Fault` subset. → **Design Q3** (owner picks share-`gorget::trap` vs duplicate+lint).

**§10.9 `Fault` re-founding:** the prelude `Fault` enum (Overflow/DivByZero/Bounds) is UNCHANGED (`resolve_variant_tag("Fault",…)`, `functions.rs:151` still works). It IS the `is_catchable()` subset by construction. The parity lint pins `Fault variants == TrapKind::is_catchable() variants`.

### 4.2 Emit-site plan (how the code reaches the runtime)
**New runtime entry:** `gorget_trap_at(const char* code, const char* detail, const char* file, int line, int col)` renders exactly `trap[%s]: %s at %s:%d:%d\n` + `exit(101)`. The **code string is threaded as data from the single Rust registry** (`TrapKind::code()`), so the runtime is a dumb formatter — NO C-side name table to hand-sync (satisfies layering rule 2; the only "spelled symbol" at the C-emit boundary is the fixed `gorget_trap_at`, which is the sanctioned exception). A bare `gorget_trap(code, detail)` wrapper (→ `<unknown>:0:0`) covers runtime-internal callers, exactly as `gorget_panic`/`gorget_panic_at` pair today.

**Threading:** compiler-emit sites (overflow/divzero/user-panic/unwrap/assert) already carry `loc` at the C/LLVM `CallExtern` rewrite (`emit_call_extern.rs:49-67`, `llvm/mod.rs:4507-4528`). Change: those sites pass the `TrapKind::code()` string as a new leading arg; the boundary rewrite emits `gorget_trap_at` instead of `gorget_panic_at`. The GIR emit helpers (`functions.rs`, `exprs/mod.rs`, `stmts/mod.rs`, `insts.rs::emit_unwrap_panic_guard`) get the `TrapKind` at construction (they already know which trap they are).
- **Assert** (`stmts/mod.rs:2550`): passes `T_AssertFailed`; the `<detail>` keeps the expression text (`generate_assert_static_msg`). BUT the assert path today has NO span (→ `<unknown>:0:0`) — threading a span here is part of the slice (→ Design Q4).
- **Bounds** is the heavy one: the reachable top-level path is the **runtime helper** `gorget_array_get` (`runtime_array.c:33`), which has NO span. Two options: (b1) thread file/line/col into the index call (hot-path cost, wide), or (b2) redirect the already-existing c_lir inline bounds check (`mod.rs:3122`) to `gorget_trap_at(T_Bounds, detail, loc)` and make it the live path. **Recommend v1 = accept `<unknown>:0:0` for helper-raised traps** (bounds/string-index/shared/bytes) — uniform format + `T_Bounds` + exit 101 everywhere, location threaded only where the compiler already has it; file "thread location into runtime-helper traps" as a follow-up. Conformance normalizes ` at file:line:col` OUT anyway (RFC §4:215), so `<unknown>` costs nothing in conformance and matches today's already-shipping assert behavior. → **Design Q2**.
- **panic_test.c setjmp path** (`panic_test.c:33-45`): the test-harness `gorget_panic_at` also longjmps on `__gorget_in_test`. The new `gorget_trap_at` must mirror this (same setjmp/cleanup) so `#[test]` assertion-failure capture keeps working. → **Design Q5** (does test-mode need the code? Recommend: format identically, code carried, longjmp path unchanged).

### 4.3 spectests wiring
- **Reader** (`frontmatter.rs`): add `Expect.trap: Option<String>` (the `T_` code), parsed from `#   trap: T_Bounds`.
- **Writer** (`lib.rs:146` `render_expect_block_from`): when `Outcome::Trap`, also emit `#   trap: {kind.code()}`. This needs `Outcome::Trap` to carry the `TrapKind` (today it carries `Fault`; ggdef's `Fault` SPLITS into the 8-variant TrapKind — `eval.rs:53`). Expectations still flow FROM ggdef (`ggdef -- gen`), never a backend.
- **Adjudicate** (`spec_conformance.rs:230`): tighten the dormant `exit!=0` branch → require exit==101 AND (if `expect.trap` present) the observed stderr's `trap[T_X]` code == `expect.trap`, with ` at file:line:col` stripped before compare. **Compare code + exit ONLY, never `<detail>`** (measured divergence: production `integer overflow` vs ggdef `arithmetic overflow`; `index 5, length 3` vs bare — Design Q1). Parse `T_X` out of `trap[...]:` via a small regex; do NOT string-match message bodies.
- **New fixtures:** ≥1 per catchable code (T_Overflow/T_DivByZero/T_Bounds, exit 101) — these finally EXERCISE the dormant nonzero branch. **BLOCKER:** ggdef cannot yet gen T_AssertFailed / T_Panic / T_UnwrapErrorOnOk (see 4.5) — those fixtures wait on ggdef support or use `adjudicator: prose`.

### 4.4 Prose + reference write-through
- **NEW `spec/prose/trap-codes.md`** mirroring `diagnostic-codes.md` (source-of-truth note → `TrapKind::code()`; naming scheme `T_<Variant>`; the exit-101 rule; the catchable subset; ` at file:line:col` normalization).
- **§10.9** (`language-reference.md:2568-2583`): Fault re-founded as the catchable SUBSET of the trap vocabulary (add a sentence: the full trap set is `T_*`; `Fault` names exactly the three catchable ones).
- **§10.1** (`:2406`): add the D23 totality sentence.
- Also sweep: `language-design.md` panic/fault sections, `docs/book/` chapters describing panic output, `docs/devbook/` error-model doc (referenced as `error-model.md §11` in `functions.rs:126`).

### 4.5 ggdef prerequisite work (SCOPE CORRECTION — measured)
ggdef models only 5/8 classes. Before the definition side can gen fixtures for all codes, ggdef elaboration must learn: **`assert` statement** (→ AssertFailed), **`panic()` call** (→ Panic), **`.unwrap_error()`** (→ UnwrapErrorOnOk). Plus `Fault`→`TrapKind` split + unwrap → UnwrapNone/UnwrapError. This is real interpreter work, NOT "rename the enum". It belongs in T1.

### 4.6 D23 enforcement (measured NOT satisfied)
D23 needs THREE pieces, and the first is real work (the ratchet alone would fail today):
1. **A dedicated diagnostic** replacing the leaking `E_TypeMismatch` (measured: `expected int, found Result[int, String]`). Add `E_UnhandledThrows` (or reshape the throws-call-in-non-Result-position path) whose message says *"this call throws `String`; declare `throws String` or handle it (catch / rethrow / Result capture)"* and NEVER prints `Result[`. Intercept where the auto-propagation type mismatch is raised (`typecheck.rs:5378,5414-5444` region) for a throws/Result callee in a non-throws context.
2. **A `tests/lints.rs` ratchet** asserting no unhandled-throws diagnostic Display contains `Result[` (mirrors the name-prefix ratchets). Guards against regression once (1) lands.
3. **reference §10.1 totality sentence** + **smith throws-in-every-expression-position tier** (`generator.rs` tier knob is plumbed; add the tier; leaks = SPEC-DIVERGE via the existing verdict lane `main.rs:8-48`).
D23 is **DISJOINT** from D11: different files (typechecker + smith + reference vs runtime/backend/ggdef/spectests), no shared code. It can run as its own PARALLEL track.

---
## SECTION 5 — RECOMMENDED SLICING (dependency order + size/risk)

I **largely CONFIRM** the working hypothesis (T1 definition / T2 production / T3 D23), with two corrections: (a) T1 must include the ggdef `assert`/`panic`/`unwrap_error` elaboration work (measured gap), which makes T1 bigger than "spec infra"; (b) split T2 because the bounds/runtime-helper unification is heavy enough to risk overloading one agent.

### Dependency order
```
T1 (DEFINITION) ───────────────► must land FIRST (defines the format T2/T3 match)
   │
   ├──► T2a (PRODUCTION registry + compiler-emit reroute)  ── depends on T1's code() names
   │        └──► T2b (runtime-lib census fold + bounds + c_lir abort() reroute) ── depends on T2a
   │
   └──► T3 (D23 enforcement) ── PARALLEL, only needs the reference file (light coupling)
```
**T1 lands first** — it is the single source of the `T_` names, the `trap:` frontmatter contract, and the ggdef oracle every other lane is diffed against. Expectations flow FROM ggdef, so the format cannot be defined by production. T2 and T3 are disjoint in file-zone and can run in parallel AFTER T1 (T3 can even start before T1 since it shares no files — only the reference §10.1 edit touches a doc T1 also edits: coordinate the one file).

### The slices
| Slice | Scope | Files (disjoint zones) | Size | Risk |
|---|---|---|---|---|
| **T1 — DEFINITION** | ggdef `Fault`→8-variant `TrapKind` split (`code()`,`is_catchable()`); ADD ggdef elaboration for `assert`/`panic()`/`.unwrap_error()`; `trap:` frontmatter reader+writer; `adjudicate` tightening (exit 101 + code, detail-normalized); NEW `spec/prose/trap-codes.md`; the T_Overflow/DivByZero/Bounds fixtures; §10.9 re-founding sentence | `spec/ggdef/src/{eval,frontmatter,lib,elaborate/*}.rs`, `tests/spec_conformance.rs`, `spec/ggdef/tests/spec_conformance_ggdef.rs`, `spec/prose/trap-codes.md`, `spectests/run/trap_*.gg`, `docs/language-reference.md §10.9` | **L** (the ggdef elaboration for assert/panic/unwrap_error is the swing; without it, M) | Med — ggdef changes are well-fenced + fast to test; the elaboration additions are new surface |
| **T2a — PROD registry + emit** | NEW `src/trap.rs`; reroute the ~10 compiler-emit `gorget_panic` sites to carry `TrapKind::code()`; new `gorget_trap_at`/`gorget_trap` runtime + C/LLVM boundary rewrite; parity lint (Fault↔TrapKind); keep exit→101 | `src/trap.rs`, `src/ir/lowering/{functions,stmts,exprs/mod,exprs/calls}.rs`, `src/lir/lower/insts.rs`, `src/backend/c_lir/emit_call_extern.rs`, `src/backend/llvm/mod.rs`, `src/backend/c/runtime/panic_normal.c`+`panic_test.c`, `tests/lints.rs` | **M** | Med — touches both backends symmetrically; the pattern already exists (panic→panic_at); parity lint contains it |
| **T2b — runtime-lib fold + bounds** | Convert census 3a/3b/3d sites to `gorget_trap`/`gorget_trap_at`; redirect c_lir `abort()` sites (3f) → `gorget_trap_at` (kills 134 + 4th format); bounds path decision (Q2); CARVE OUT D17's file/path sites; leave 3c OOM w/ filed follow-up | `src/backend/c/runtime/{runtime_array,runtime_string,shared_runtime,bytes_runtime,bytes_f32_runtime,channel_runtime}.c`, `src/backend/c_lir/{mod,emit_hof,emit_types}.rs` | **M–L** | **Higher** — many sites; bounds spans runtime+backend; channel sites are phase-3-adjacent; needs the Q2 ruling before start |
| **T3 — D23** | NEW `E_UnhandledThrows` diagnostic (no `Result[`); intercept the throws-in-non-Result-position mismatch; `tests/lints.rs` no-`Result[` ratchet; smith throws-in-every-position tier; reference §10.1 sentence; negative fixtures | `src/semantic/{errors,typecheck}.rs`, `tests/lints.rs`, `tests/smith/{generator,main}.rs`, `docs/language-reference.md §10.1`, `tests/fixtures/*` | **M** | Med — the diagnostic interception point needs care (must catch operand-position leak too, which today errors TWICE) |

### Flags
- **T2b is the one at risk of being too big for one agent** — if the owner wants real locations on bounds (Q2 = thread span), split bounds into its own slice. If Q2 = accept `<unknown>` in v1, T2b stays one agent.
- **T1's ggdef elaboration additions** (assert/panic/unwrap_error) could be pre-factored as a tiny T0 prerequisite if the owner wants T1 kept to "spec infra only" — but they're small and cohesive with T1.
- **One shared file, `docs/language-reference.md`:** T1 edits §10.9, T3 edits §10.1 — different sections, but sequence them or merge-coordinate to avoid a conflict.

---
## SECTION 6 — OWNER DESIGN QUESTIONS (each with a recommendation)

**Q1 — Does conformance compare the `<detail>` string, or only `T_` code + exit?**
Measured divergence: production `integer overflow` vs ggdef `arithmetic overflow`; production `index out of bounds: index 5, length 3` vs ggdef bare `index out of bounds`. **Recommend: compare `T_` code + exit 101 ONLY; `<detail>` is human-facing, host/impl-specific, and normalized out** (like ` at file:line:col`). Rationale: detail carries runtime values (index N, len M) and impl-chosen wording — pinning it manufactures false MISMATCHes and freezes wording. (One-line: the CODE is the contract, the detail is UX.)

**Q2 — Do runtime-helper-raised traps (bounds/string-index/shared/bytes) get real `file:line:col` in v1, or accept `<unknown>:0:0`?**
The reachable `v[i]` path raises inside `gorget_array_get`, which has no span. **Recommend: accept `<unknown>:0:0` for helper-raised traps in v1**, thread location only where the compiler already has it (overflow/divzero/panic/unwrap), file "thread span into runtime-helper traps" as a follow-up. Rationale: conformance normalizes location out anyway; assert ALREADY ships `<unknown>:0:0`; threading spans into the index hot-path is wide + perf-sensitive. (If you want bounds to have real locations — it IS the flagship trap for LLM-debuggability — say so and bounds becomes its own slice via the c_lir inline-check reroute.)

**Q3 — ggdef `TrapKind`: DUPLICATE (parity-lint) or SHARE `gorget::trap`?**
The import ratchet permits `gorget::trap` (not in FORBIDDEN), so sharing is possible. **Recommend: DUPLICATE + parity lint.** Rationale: ggdef is THE DEFINITION and should own its trap vocabulary independently (production matches ggdef, not vice-versa — same principle as "expectations flow from ggdef"); matches the existing re-derive-not-share pattern; keeps standing-rule 5's shared surface at lexer+parser+AST+span. The lint is the sanctioned anti-drift mechanism (not hand-sync). (Share only if you'd rather have literally one enum and are willing to widen ggdef's shared surface.)

**Q4 — Does the `assert` `<detail>` keep the expression text, and do we thread a span to it now?**
Today: `<unknown>:0:0: assertion failed` (or the generated expr message). **Recommend: keep the expression text in `<detail>` (it's the useful part), and thread the assert span as part of T2a** (assert already has the AST node; it's a small fix, and unlike bounds it's a single compiler-emit site, not a runtime helper). So assert gets real locations even under Q2's "helpers stay `<unknown>`".

**Q5 — Does the test-harness setjmp path (`panic_test.c`) also normalize / carry the code?**
`#[test]` assertion capture longjmps through `gorget_panic_at`. **Recommend: `gorget_trap_at` mirrors the setjmp/cleanup path unchanged; format identically; the code rides but the test runner keeps matching on the captured message as today.** Rationale: no behavior change to test capture, just the new symbol + format.

**Q6 — Fold the channel-send/recv-on-closed sites (census 3b) now, or defer with phase-3?**
Channels are phase-3 (RFC §2.6). **Recommend: fold their FORMAT now to `T_Panic` (cheap, uniform) but do NOT design catchability** — they stay uncaught panics like everything else in v1. Rationale: uniform stderr is the whole point of D11; leaving 4 channel sites on the old format re-opens the rule-2 violation. (OOM/allocator 3c genuinely stays outside v1 per D11 — leave those.)

**Q7 — D17 coordination: `runtime_file.c`/`runtime_path.c` panics.**
These are D17's `throws IoError` targets (file/dir open failure). **Recommend: T2b explicitly CARVES THEM OUT** — do not normalize them to `T_Panic`; they become thrown errors under D17, not traps. Flag both tracks so neither double-owns them.

**Q8 — Sequencing vs D17/D12/D13 tracks touching the same runtime files.**
T2b edits `runtime_array.c`, `runtime_string.c` etc.; other ratified tracks (D12 enforcement, D13 allocators) may touch adjacent runtime code. **Recommend: run T2b's runtime edits in a dedicated worktree AFTER T1, and brief it on the exact carve-out list (Q7) + which files sibling tracks own.** Low risk if zones are declared.
