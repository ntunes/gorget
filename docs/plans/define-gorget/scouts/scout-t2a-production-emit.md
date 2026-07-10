# SCOUT REPORT — T2a (PRODUCTION side of trap normalization, D11)

PID 17551. Worktree `agent-a8155b47d2b20cef1`. Read-only + end-to-end MEASURED. Prototype reverted; worktree + main clean.
Status: COMPLETE.

## HEADLINE (read this first)
The prior whole-track scout's central premise is **WRONG, and I measured it**: overflow/divzero do **NOT** route through `gorget_panic`. In BOTH backends they are emitted **INLINE** (`__builtin_add_overflow(...) { fprintf; exit(1) }` in C; `@llvm.sadd.with.overflow` + `fprintf; exit(1)` in LLVM). The prior scout could not tell because the inline path and the `gorget_panic_at` path produce **byte-identical** stderr. This makes T2a materially BIGGER than the prior scout scoped: T2a must reroute the inline arithmetic emit in BOTH backends, not just the `gorget_panic` rewrite.

Three more load-bearing results, all measured/verified this session:
1. **The self-host lane needs its OWN reroute.** A Rust-only T2a (C+LLVM+C-runtime) leaves the self-host lane emitting `gorget: integer overflow` + exit 1 → still MISMATCH. MEASURED end-to-end by building the self-host driver and running `trap_overflow` through it.
2. **T2a flips 7 of the 8 trap fixtures, not 8.** `trap_bounds` is a runtime-library path (`gorget_array_get`), which is **T2b**, not T2a. So floors go 187 → 194 on T2a (not 195); the last +1 (bounds) lands with T2b.
3. **`shift out of range` is a compiler-emit trap with NO `T_` code** — ggdef doesn't model shift, so the registry has no variant for it. A real gap T2a must rule on (owner Q).

---

## SECTION 1 — PREMISE VERIFICATION

| # | Premise (from brief) | Verdict | Evidence (file:line) |
|---|---|---|---|
| 1 | overflow/divzero NULL-slot panic at `functions.rs:88-107` routes through `gorget_panic` | **CORRECTED — those blocks are DEAD for `main`** | `functions.rs:88-107` DO call `gorget_panic("integer overflow"/"division by zero"/"index out of bounds")`, BUT the comment at `functions.rs:80-83` says they are "reached only when a NULL slot is passed… DCE'd" — the **fault-scope participating-callee** path, NOT `main`. Top-level `m+1` uses the INLINE `Inst::Add{overflow:Trap}` emit (measured, see §2). |
| 1 | fault-scope div `exprs/mod.rs:3785-3805` | CONFIRMED present but same story — fault-scope-only | `exprs/mod.rs` fault machinery; not the reachable top-level path |
| 1 | assert `stmts/mod.rs:2550-2561` → `gorget_panic` | **CONFIRMED** | `stmts/mod.rs:2550` (`call_extern("gorget_panic",[msg_op])`) and `:2557-2561` (static-msg path). No span carried → renders `<unknown>:0:0`. |
| 1 | user panic `calls.rs:573-577` → `gorget_panic` | **CONFIRMED** | `calls.rs:573-577` (`call_void("gorget_panic",[msg_op])` + `unreachable`) |
| 1 | unwrap guard `insts.rs:3603-3638`, variant words `:4195-4247` → `gorget_panic` | **CONFIRMED** | `insts.rs:3603-3638` `emit_unwrap_panic_guard` builds `"called \`{method}()\` on a \`{variant_word}\` value"` → `CallExtern{name:"gorget_panic"}`. Callers `:4195-4203` and `:4240-4247` set `variant_word` None/Error/Ok. |
| 1 | **overflow/divzero (reachable top-level path)** | **NEW — INLINE in BOTH backends, not gorget_panic** | C: `c_lir/mod.rs:2456-2542` (`Inst::Add/Sub/Mul` overflow, `Inst::Div/Rem/Mod` div0 + `TYPE_MIN/-1`) each `fprintf(stderr,"%s:%d:%d: …"); exit(1)`. LLVM: `emit_overflow_check` `llvm/mod.rs:7250-7323` (`:7316` `exit(1)`), div checks `:3768/3803/3849/6899/6904`. MEASURED §2. |
| 1 | **bounds (top-level `a[5]`)** | **RUNTIME HELPER — this is T2b, not T2a** | `runtime_array.c:31-36` `gorget_array_get` → `fprintf(stderr,"gorget: panic: index out of bounds: index %zu, length %zu\n"); exit(1)`. No span, different format. Confirmed via `--emit-c-lir`. |
| 2 | `panic_normal.c` renders `%s:%d:%d: %s` + `exit(1)`; `gorget_panic`→`<unknown>` | **CONFIRMED** | `panic_normal.c:3-9` (exact) |
| 2 | C rewrite `gorget_panic`→`gorget_panic_at` | **CONFIRMED** | `c_lir/emit_call_extern.rs:45-68` (name-match `name=="gorget_panic" && args.len()==1`, emits `gorget_panic_at("{f}",{ln},{cl},msg)`) |
| 2 | LLVM rewrite + decl | **CONFIRMED** | `llvm/mod.rs:4507-4530` (rewrite), `:1506` (`declare void @gorget_panic_at(ptr,i32,i32,ptr)`) |
| 3 | self-host has its own emit; needs own reroute? | **CONFIRMED + MEASURED — YES** | self-host inline arith `lir_codegen.gg:4412-4470` emits `"gorget: integer overflow\n"`+`exit(1)` (its OWN format, no span); `gorget_panic` sites `lower_stmt.gg:1441`(assert), `lower_closures.gg:93`, `lower_expr.gg:3525/7412/7714`(unwrap/panic). NO `gorget_trap`/`gorget_panic_at` anywhere in self-host. MEASURED §2d. |
| 4 | Fault prelude enum + is_catchable subset | **CONFIRMED** | `builtin_fault_enum()` `ir/lowering/generics/substitute.rs:323-347` = exactly `{Overflow,DivByZero,Bounds}` (3 unit variants). Registered `semantic/resolve.rs:185`. ggdef `is_catchable()`=same three (`eval.rs:106-108`). |
| 4 | parity-lint mechanically possible | **CONFIRMED** | `ggdef` is a `[dev-dependencies]` of the root package (`Cargo.toml:28,32`) → `tests/lints.rs` can `use ggdef::TrapKind` AND `use gorget::trap::TrapKind`, cross-check `code()`/`is_catchable()`. Precedent `SemanticErrorKind::code()` `semantic/errors.rs:127`. |
| 5 | compiler-emit exits 1 today | **CONFIRMED** | inline arith `exit(1)`; `gorget_panic_at` `exit(1)`; bounds helper `exit(1)`. |
| 5 | latent `abort()`/134 sites in c_lir | **CONFIRMED, latent (fault-scope path)** | `c_lir/mod.rs:3122` bounds, `:3131` divzero, `:3137` generic — `abort()` (134); `emit_hof.rs:156`, `emit_types.rs:246` unwrap_err-on-Ok `abort()`. Unreached by top-level programs (T2b). |

**Registry to mirror EXACTLY (ggdef, LANDED):** `spec/ggdef/src/eval.rs:64-124` — 8 variants `Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk, AssertFailed(String), Panic(String)`; `code()`→`T_<Variant>` (`:89-100`), exhaustive no-`_`; `is_catchable()`=`Overflow|DivByZero|Bounds` (`:106-108`). Render (production): `trap[T_X]: <detail> at <file>:<line>:<col>` + exit **101** (`spec/prose/trap-codes.md` Rendering). ggdef itself renders WITHOUT the ` at …` suffix (`main.rs:120`) — fine, conformance strips it.

**Conformance adjudication (LANDED):** `tests/spec_conformance.rs:249-256` — trap fixture MATCHes iff `exit==101` AND stderr **contains** `trap[<code>]` AND stdout is a prefix. Detail + ` at file:line:col` NOT compared. So production only needs `trap[T_X]` on stderr + exit 101. Floors `C=LLVM=SELFHOST=187` (`:73-79`), `MIN_FIXTURES=195` (8 trap fixtures above the floor).

---

## SECTION 2 — END-TO-END MEASUREMENT (prototype + self-host probe)

Compiler built cold (`cargo build`, exit 0). Prototype = throwaway worktree edits, since reverted.

### 2a. Baseline (current production, C backend) — `int r = m + 1`
```
$ gg build ovf.gg -o ovf_bin && ./ovf_bin
ovf.gg:4:17: integer overflow          (stderr)
EXIT=1
```
`--emit-c-lir` proves the path is INLINE, not `gorget_panic`:
```
2840:  if (__builtin_add_overflow((int64_t)__v3,(int64_t)__v4,&__v5)) { fprintf(stderr,"…/ovf.gg:4:17: integer overflow\n"); exit(1); }
```

### 2b. Prototype reroute (C backend, ONE class = overflow)
Three throwaway edits: (i) `gorget_trap_at(code,detail,file,line,col)`→`fprintf(stderr,"trap[%s]: %s at %s:%d:%d\n",…); exit(101)` into `panic_normal.c`; (ii) forward-decl in `runtime_string.c`; (iii) rerouted `Inst::Add` overflow (`c_lir/mod.rs:2459`) to `gorget_trap_at("T_Overflow","integer overflow","{f}",{ln},{cl})`. `cargo build` (9s), then:
```
$ ./ovf_bin2
trap[T_Overflow]: integer overflow at …/ovf.gg:4:17     (stderr)
EXIT=101
```
**Before → after: `ovf.gg:4:17: integer overflow` / exit 1  →  `trap[T_Overflow]: integer overflow at ovf.gg:4:17` / exit 101.**

### 2c. Conformance fixture flip (C lane), replicating `adjudicate`
```
$ gg build spectests/run/trap_overflow.gg -o fix_ovf && ./fix_ovf
stdout=[computing]  exit=101
stderr=[trap[T_Overflow]: integer overflow at …/spectests/run/trap_overflow.gg:18:17]
VERDICT: MATCH (flipped from MISMATCH)   # exit==101 ∧ stderr⊇"trap[T_Overflow]" ∧ stdout prefix "computing\n"
```
**Proven end-to-end: the T2a design flips the C lane MISMATCH→MATCH.**

### 2d. Self-host lane MEASUREMENT (Premise 3 — the load-bearing unknown)
Built the self-host driver in-worktree (`gg build driver.gg`, ~1 min, exit 0). Ran the SAME fixture through the self-host lane against the (prototype-modified) runtime:
```
$ driver spectests/run/trap_overflow.gg lib --emit-c --runtime-dir=$PWD/src/backend/c/runtime > ovf.c
# self-host-emitted C, line 1524:
  if (__builtin_add_overflow((int64_t)__v5,(int64_t)__v7,&__v10)) { fprintf(stderr,"gorget: integer overflow\n"); exit(1); }
$ cc -O0 -w -o ovf_bin ovf.c -lm -lpthread && ./ovf_bin
stdout=[computing]  exit=1
stderr=[gorget: integer overflow]
SELF-HOST VERDICT: MISMATCH (needs its own reroute)
```
`gorget_trap_at` is DECLARED+DEFINED in the linked runtime (self-host C lines 882/1414) yet the self-host's own inline emit (`lir_codegen.gg:4412`) is untouched by the Rust-only change → keeps `gorget: integer overflow` + exit 1. **CONCLUSION (measured): the self-host lane does NOT ride the Rust reroute; it needs its own emit reroute mirroring the Rust change — exactly like the unwrap-panic fix that had to touch both self-host emit routes.**

### 2e. Which of the 8 fixtures does T2a flip? (emit-path census)
| Fixture | Trap class | Reachable emit path | T2a or T2b |
|---|---|---|---|
| trap_overflow | Overflow | INLINE arith (both backends) | **T2a** |
| trap_divbyzero | DivByZero | INLINE arith (`Inst::Div` div0) | **T2a** |
| trap_unwrap_none | UnwrapNone | `gorget_panic` guard | **T2a** |
| trap_unwrap_error | UnwrapError | `gorget_panic` guard | **T2a** |
| trap_unwrap_error_on_ok | UnwrapErrorOnOk | `gorget_panic` guard (reachable path: `<path>:L:C: called \`unwrap_error()\` on a \`Ok\` value` / exit 1 — abort()/134 sites are latent) | **T2a** |
| trap_assert | AssertFailed | `gorget_panic` (`<unknown>:0:0`) | **T2a** |
| trap_panic | Panic | `gorget_panic` | **T2a** |
| **trap_bounds** | Bounds | **runtime helper `gorget_array_get`** (`gorget: panic: …` / exit 1) | **T2b** |

**T2a flips 7/8; `trap_bounds` waits for T2b.**

---

## SECTION 3 — DESIGN PROPOSAL

### 3.1 The production registry — `src/trap.rs` (`gorget::trap`)
Mirror ggdef `eval.rs:64-124` EXACTLY: closed enum, exhaustive no-`_` `code()`→`T_<Variant>`, `is_catchable()`=`Overflow|DivByZero|Bounds`. Home = crate-root `src/trap.rs` (pure data, read by both `ir/lowering/*` and `backend/{c_lir,llvm}`, no layering inversion). ggdef keeps its own copy (it IS the definition); the parity lint pins them (§3.5). Optional `detail()` accessor so wording lives beside the code (owner Q-B).
```rust
pub enum TrapKind { Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk, AssertFailed, Panic }
impl TrapKind {
    pub fn code(&self) -> &'static str { /* exhaustive, no `_` — rustc IS the ratchet */ }
    pub fn is_catchable(&self) -> bool { matches!(self, Overflow | DivByZero | Bounds) }
}
```
(Production variants are payload-free — detail is formed at the emit site, unlike ggdef's `AssertFailed(String)`/`Panic(String)`.)

### 3.2 The runtime entry — `gorget_trap_at` (proven §2b)
Add to `panic_normal.c` (+ forward-decl beside `runtime_string.c:458`, + LLVM `declare`, + `panic_test.c` mirror):
```c
static inline void gorget_trap_at(const char* code, const char* detail,
                                  const char* file, int line, int col) {
    fprintf(stderr, "trap[%s]: %s at %s:%d:%d\n", code, detail, file, line, col);
    exit(101);
}
```
The `T_` code is passed AS DATA from `gorget::trap::TrapKind::code()`; the runtime is a dumb formatter, no C-side code table (layering rule 2; the single sanctioned spelled symbol is `gorget_trap_at`). T2a needs no `<unknown>` wrapper — every T2a site has a span.

### 3.3 Emit-site reroute — TWO mechanisms (not one)
**(A) Inline arithmetic (overflow/divzero) — the part the prior scout missed.** Change each inline `fprintf; exit(1)` to `gorget_trap_at(CODE, DETAIL, file,line,col)`, CODE from the statically-known backend arm.
- C `c_lir/mod.rs`: `Inst::Add :2459`, `Sub :2469`, `Mul :2479` → `T_Overflow`; `Inst::Div` div0 `:2504/:2510`, `TYPE_MIN/-1 :2505` → `T_DivByZero`/`T_Overflow`; `Inst::Rem :2534/:2535/:2540`, `Inst::Mod :2567/:2576` → same. (+ shift `:2648/:2655` — owner Q-A.)
- LLVM `llvm/mod.rs`: `emit_overflow_check :7250-7323` → `T_Overflow`; `emit_div_overflow_trap :7331…` → `T_Overflow`; div0 checks `:3768/:3803/:3849/:6899` → `T_DivByZero`. Replace each `fprintf; call void @exit(i32 1)` with `call void @gorget_trap_at(...)`.

**(B) `gorget_panic`-carried sites (assert/user-panic/unwrap) — reuse the existing rewrite pattern.** Introduce a NEW 2-arg extern `gorget_trap` the emit sites call with the code as a leading `Constant::Str`:
- assert `stmts/mod.rs:2550,2557` → `[Str(TrapKind::AssertFailed.code()), msg]`
- user panic `calls.rs:575` → `[Str(TrapKind::Panic.code()), msg]`
- unwrap guard `insts.rs:3630-3635` → map `(is_unwrap_err, variant_word)`→code (None→UnwrapNone, Error→UnwrapError, Ok→UnwrapErrorOnOk), pass `[Str(code), msg]`.
Then the backend rewrites (C `emit_call_extern.rs:49`, LLVM `mod.rs:4507`) grow a sibling branch: `name=="gorget_trap" && args.len()==2` → emit `gorget_trap_at(arg0, arg1, file,line,col)`. **Leave the 1-arg `gorget_panic` branch UNTOUCHED** — keeps runtime-internal `gorget_panic` callers (T2b: `str index out of bounds`, `format error`, …) on the old path. This is the clean T2a/T2b seam.

Both mechanisms converge on the single `gorget_trap_at`. Assert gets a real span if given one (Q-D) — single compiler-emit site, unlike bounds.

### 3.4 `panic_test.c` mirror
Add a `gorget_trap_at` to `panic_test.c` mirroring the setjmp/longjmp+cleanup so `#[test]` assertion capture keeps working; format identically, code rides along, longjmp path unchanged.

### 3.5 Parity lint (`tests/lints.rs`)
Two mechanical assertions (ggdef is a dev-dep):
1. **prod ↔ ggdef code sets equal:** collect `code()` over an explicit variant list of each of `gorget::trap::TrapKind` and `ggdef::TrapKind`; assert SETS equal. (Each `code()` is rustc-exhaustive; the lint catches a variant present in one enum but not the other.)
2. **is_catchable ⇔ Fault prelude:** assert the variant names where `gorget::trap::TrapKind::is_catchable()` is true equal `builtin_fault_enum().variants` (`{Overflow,DivByZero,Bounds}`) — call `pub fn builtin_fault_enum()` directly, compare names. No message-text matching.

### 3.6 exit-101
Falls out of `gorget_trap_at`. Every T2a-rerouted site stops using `exit(1)`/`@exit(i32 1)`. T2b later flips the runtime-lib helpers + the latent `abort()`/134.

---

## SECTION 4 — RECOMMENDED SLICING

### T2a SPLITS into two agent-sized slices (the measurement forces this)
```
T1 (LANDED) ──► T2a-rust      (C+LLVM+registry+runtime+lint)   ── flips C & LLVM floors 187→194
             └► T2a-selfhost  (self-host .gg emit reroute)      ── flips self-host floor 187→194
                    (disjoint file zones; both depend only on T1)
             ──► T2b (bounds runtime-lib fold + latent abort()/134) ── flips all three 194→195
```

| Slice | Scope | Files | Size | Risk |
|---|---|---|---|---|
| **T2a-rust** | `src/trap.rs`; INLINE arith reroute in C (`c_lir/mod.rs:2456-2655`) + LLVM (`llvm/mod.rs:7250-7323,3768-3854,6885-6904`); new 2-arg `gorget_trap` extern + rewrite branch (`emit_call_extern.rs`, `llvm/mod.rs:4507`); reroute assert/panic/unwrap (`stmts/mod.rs:2550`, `calls.rs:575`, `insts.rs:3630`); `gorget_trap_at` in `panic_normal.c`(+`runtime_string.c` fwd-decl, LLVM declare, `panic_test.c` mirror); parity lint | ~9 files | **M–L** | Med. Inline arith = many small symmetric sites across both backends (mechanical but wide); the `gorget_panic` half reuses a proven pattern; parity lint contains drift. **Bigger than the prior scout's "M".** |
| **T2a-selfhost** | Mirror in `.gg`: inline arith `lir_codegen.gg:4412-4470` → `gorget_trap_at` with `T_Overflow`/`T_DivByZero` + span; `gorget_panic` emit sites (`lower_stmt.gg:1441`, `lower_closures.gg:93`, `lower_expr.gg:3525/7412/7714`) → carry code; self-host CallExtern C-emit (`lir_codegen.gg:2734`) grows the `gorget_trap` case | 4 `.gg` files | **M** | Med. Self-host dialect; needs driver rebuild + `bootstrap_fixed_point`. Guarded by `*_comparison` + spec_conformance self-host lane. |
| **T2b** | bounds runtime-lib fold (`runtime_array.c` + string/shared/bytes) → `gorget_trap`/`gorget_trap_at`; latent `abort()`/134 reroute (`c_lir/mod.rs:3122/3131/3137`, `emit_hof.rs:156`, `emit_types.rs:246`); carve out D17 file/path sites; OOM stays out of v1 | runtime `.c` + c_lir | **M–L** | Higher (per prior scout). Flips the last fixture (bounds) on all lanes. |

**Floor bumps:** T2a-rust → `C_MATCH_FLOOR=194, LLVM_MATCH_FLOOR=194` (same commit). T2a-selfhost → `SELFHOST_MATCH_FLOOR=194`. T2b → all three `=195 (=MIN_FIXTURES)`. **Never bump a floor before its lane's reroute lands.**

**Shared-file coupling (SEQUENCE, don't parallelize T2a-rust with T2b):** both edit `c_lir/mod.rs` (T2a `:2456-2655`; T2b `:3122-3137`) and `llvm/mod.rs`, different regions. Run T2a-rust first, T2b after. **T2a-rust ∥ T2a-selfhost is safe** (Rust `src/` vs `.gg` fixtures — disjoint).

### Sizing verdict
- **T2a-rust is one solid agent**, upper end. If a reviewer wants it smaller, split "inline-arith reroute (both backends)" from "gorget_panic→gorget_trap rewrite + registry + lint" — independent.
- **T2a-selfhost is a separate agent** (different codebase + driver-rebuild/bootstrap gate). Do NOT fold into T2a-rust.

---

## SECTION 5 — OWNER DESIGN QUESTIONS

**Q-A (NEW, blocking for T2a) — `shift out of range` has no `T_` code.** C traps it (`c_lir/mod.rs:2648/2655`, `fprintf; exit(1)`); LLVM does NOT check it (`llvm/mod.rs:3986-3995` raw `shl`/`ashr`/`lshr` — silent UB); ggdef doesn't model shift (no `Shl`/`Shr` in `ggc.rs` BinOp) → outside the conformance corpus. Two defects: a missing code AND a C-vs-LLVM parity bug. **Recommend: map shift-out-of-range → `T_Overflow`** (arithmetic-domain; no registry/ggdef change) and, in T2b, ADD the missing LLVM shift range-check so both backends trap identically. (Alternative: a new `T_ShiftOutOfRange` variant — forces a T1/ggdef change for a trap the definition doesn't model. Not worth it.)

**Q-B — `<detail>` wording per class.** Conformance ignores detail, but it's user-facing. **Recommend: keep production's existing strings** (`integer overflow`, `division by zero`, `index out of bounds`, `` called `unwrap()` on a `None` value ``, …) on a `TrapKind::detail()` beside `code()`. Divergence from ggdef detail is explicitly allowed (trap-codes.md Rendering).

**Q-C — unwrap detail keeps the variant word?** Today `` called `unwrap_error()` on a `Ok` value ``. **Recommend: keep it** — only the code + exit + `trap[...]` wrapper change.

**Q-D — assert span.** Today `<unknown>:0:0`. **Recommend: thread the assert span in T2a-rust** — single compiler-emit site (`stmts/mod.rs`), AST node has it; unlike bounds this is cheap → assert gets real locations.

**Q-E — bounds location (T2b, restated).** Reachable bounds is the runtime helper `gorget_array_get`, no span. **Recommend: accept `<unknown>:0:0` for helper-raised bounds in v1** (conformance normalizes location out; matches assert's shipping behavior); file "thread span into runtime-helper traps" as follow-up.

**Q-F — `gorget_trap` extern vs first-class `Inst::Trap(TrapKind)`.** The proposal reuses extern-call + name-match-rewrite (one new spelled symbol `gorget_trap`). Cleaner-but-bigger: a typed `Inst::Trap(TrapKind)` through LIR/BIR, no name-match. **Recommend: extern-call for T2a** (matches the proven `gorget_panic`→`gorget_panic_at` machinery, contained); note `Inst::Trap` as a future consolidation.

---

## Appendix — commands (all run this session, in-worktree)
- `cargo build` (exit 0; +9s incremental after prototype).
- Baseline `gg build ovf.gg && ./ovf_bin` → `ovf.gg:4:17: integer overflow` / exit 1; `--emit-c-lir | grep add_overflow` → inline `exit(1)` (gen line 2840).
- Prototype: edits to `panic_normal.c`+`runtime_string.c`+`c_lir/mod.rs:2459`; rebuild; `./ovf_bin2` → `trap[T_Overflow]: integer overflow at …:4:17` / exit 101.
- Fixture `gg build spectests/run/trap_overflow.gg && ./fix_ovf` → exit 101, stderr⊇`trap[T_Overflow]`, stdout `computing` → **MATCH**.
- Self-host `gg build driver.gg`; `driver trap_overflow.gg lib --emit-c --runtime-dir=$PWD/src/backend/c/runtime`; self-host C line 1524 = `gorget: integer overflow`+`exit(1)`; `cc … && ./ovf_bin` → exit 1, stderr `gorget: integer overflow` → **MISMATCH** (self-host needs own reroute).
- Prototype reverted (`git checkout --`); `git status` clean in worktree AND main.
