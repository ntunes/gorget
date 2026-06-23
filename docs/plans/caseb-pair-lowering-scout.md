# Scout — Case-B pairs: (a)-class registration + reference-grade runtime-lowering

**Status:** THROWAWAY PROTOTYPE, fresh measurement on `gorget-1` tip
`df05ac2a` (2026-06-23). Pairs each (a)-class resolver-registration fix
(re-derived from `b242f98b`) with its reference-grade runtime-lowering pass so
the fixture actually FLIPS to byte-exact MATCH. **DO NOT INTEGRATE — this branch
is a measurement vehicle.**

Grounded in `docs/language-reference.md` (the `??` default operator §"Operators",
`field_value`/`field_set` reflection, aliased imports) and the Rust references
`src/ir/lowering/exprs/calls.rs:525` (panic), `src/ir/lowering/exprs/mod.rs:772`
(`Expr::DefaultOp`), `src/semantic/rewrite.rs:42` + `:810` (alias rename +
field_value rewrite), `src/semantic/meta.rs:2507` / `:3296` (field_value
meta-rewrite + the delayed-meta-for engine).

---

## 0. Headline

| Pair | Registration half | Lowering half (the FLIP) | Per-fixture result |
|---|---|---|---|
| **1. panic** | A.2 `panic`∈`is_builtin` (1 line) | `panic`→`gorget_panic` call-lowering **+ the `??` (DefaultOp) lowering** the fixture also needs | **panic_builtin FLIPS ✅** + 2 sibling flips |
| **2. field_access** | A.5 reflection carve-out (~25 lines) | `field_value`/`field_set`→`.field` rewrite | **DEFERRED — blocked by the missing `meta for … in fields(T)` delayed-meta engine, NOT by the rewrite.** Reference-grade target spelled out in §3. |
| **3. import_alias** | A.4 `ImportStmt.aliases` + parser `as` + `collect_import` (medium) | `rename_import_aliases` Pass-1.5 AST rewrite (full reference-grade) | **import_alias FLIPS ✅** |

**Runtime parity delta (MEASURED, `GG_RUNTIME_DIFF=1 … self_host_runtime_diff`):
731 → 735 MATCH = +4, ZERO regressions.** The 4 flips:

| fixture | how it flipped |
|---|---|
| `panic_builtin` (TARGET) | panic call-lowering + `??` lowering |
| `import_alias` (TARGET) | A.4 + `rename_import_aliases` |
| `snag43c_default_op_non_copy` (sibling) | `??` lowering — exercises `o ?? JsValue.Undefined` with a **non-Copy resource** rhs |
| `snag44_closure_throw_diagnosed` (sibling) | `??` lowering — exercises `x ?? throw E(...)` (divergent rhs gate) |

The `??` lowering was the high-leverage find: it generalises beyond panic to the
WHOLE DefaultOp family (Some/Ok unwrap, `panic`/`throw`/`return` divergent rhs,
non-Copy resource defaults), which is why one pass flipped 4 fixtures.

**No-regression: CLEAN.**
- `self_host_bootstrap_fixed_point` **GREEN** (285s; the driver self-compiles its
  OWN new `??`-lowering / alias-rename / panic arm / parser `as`-consumption /
  format `as`-rendering and re-converges byte-identical — the load-bearing gate).
- `cargo test --lib` **1084/0**.
- All 8 `*_comparison` byte-identical to the prior-scout HEAD baseline:
  type 1181/42/68/0, check 1162/61/68/0, c_emit 1031 matched / 149 mism,
  lowerer 1067 matched / 114 mism / 5 (pre-existing negatives),
  resolver 1276/15/0, parser 1270/21/0.

---

## 1. Pair 1 — `panic` (A.2) + `panic`→`gorget_panic` **+ the `??` lowering**

### Re-verified failure
Baseline `panic_builtin.gg` CC-FAILs: `__v6 = (Option__int64_t)__v7 +
(Option__int64_t)__v8` — *conversion to non-scalar type*. TWO miscompiles:
1. `panic(msg)` lowered as an ordinary value-returning call (`__v = panic(__msg)`).
2. `some_val ?? panic(...)` — **the `??` operator is completely unimplemented**:
   the parser models it `EBinaryOp(lhs, "??", rhs)` (parser.gg:838), and the
   lowerer falls through to `map_binop("??")` whose `else` defaults to **OP_ADD**
   (lower_types.gg:2436-2441, `diag_bug` "unknown operator '??' … OP_ADD (WRONG)").

### Registration half (re-applied from `b242f98b`)
`resolve.gg:60-61` — `if name == "panic": return true` in `is_builtin`. Mirrors
Rust `resolve.rs:2109`.

### Lowering half — reference-grade

**(a) panic call-lowering** (`lower_expr.gg:4347`, new arm in `lower_call`,
before the `Str`/`len` arms). Mirrors Rust `calls.rs:525-531`:
```
elif fname == "panic" and args.len() == 1:
    int panic_msg = lower_expr(&ctx, args.get(0).unwrap(), &gmod)
    emit(GICallExtern(-1, "gorget_panic", [op_consume(panic_msg, CkFormatArg())]))
    # noreturn: jump to a fresh dead block (self-host has no GTUnreachable;
    # GTJump(dead_bb) is the faithful equivalent — same idiom as the assert
    # lowering, lower_stmt.gg:1036)
    int dead_bb = new_block(&ctx); set_terminator(GTJump(dead_bb)); switch_to(dead_bb)
    int unit = add_local(UNIT_TYPE); emit(GIAssign(unit, OpConstUnit())); result = unit
```
The runtime `gorget_panic(const char*)` is already declared + registered cstr-arg
(lir_codegen.gg:2012 `runtime_arg_is_cstr`), so the String message converts via
`gorget_str_to_cstr(...)` at C-emit. Emitted C: `gorget_panic(gorget_str_to_cstr(__v10))`.

**(b) the `??` / DefaultOp lowering** (`lower_expr.gg:704`, new arm in the
`EBinaryOp` case, beside `and`/`or`/`in`). Mirrors Rust `Expr::DefaultOp`
(`mod.rs:772-953`), built on the self-host's OWN tag-read/payload-read vocabulary
(the `unwrap_or` template at `lower_expr.gg:1502-1520` fused with the divergent-arm
gate from `lower_match_expr`, `lower_match.gg:1451-1457`):
- Classify lhs off the typed `enum_category` channel (Some/Ok = success variant;
  `.ok_type` = inner T) — NO name re-parse.
- Result slot typed inner-T (not Option/Result) — both arms store a T value.
- `emit_tag_read` + `GICmp(==0)` + `GTBranch(some_bb, else_bb)`.
- Some/Ok arm: `emit_payload_read` → `op_consume(CkAssign)` (auto Move/clone for
  resource T) → store to the T slot.
- None/Error arm: `lower_expr(rhs)`; **gate** on `block_terms.get(current_block)`:
  only `GIAssign + GTJump(merge)` when `GTNone()` (rhs didn't diverge). A divergent
  rhs (`panic`/`throw`/`return`) contributes NO merge edge — exactly the
  `lower_match_expr` divergent-arm gate.

### Proven (byte-exact)
```
$ /tmp/sh_driver panic_builtin.gg lib --emit-c … | cc … && ./a.out
a=99
b=42                  ← MATCHES gg run, byte-exact
```
Emitted C for `main`'s `??`: a real tag-branch — `__bb1` (Some) reads `Some_0`
→ merge; `__bb2` (None) `gorget_panic(...)` → dead block (never merges).
**Before:** `(Option__int64_t)__v7 + (Option__int64_t)__v8` (CC-FAIL).

### Flips
`panic_builtin` (target) + `snag43c_default_op_non_copy` (resource-rhs `??`) +
`snag44_closure_throw_diagnosed` (throw-rhs `??`). **+3 net for this pair.**

### Honest edge — a SEPARATE pre-existing gap surfaced (NOT a regression)
`dop_throw_rhs.gg` (`o ?? throw "no value"` inside a `throws String` fn, with the
None/throw path ACTUALLY taken at runtime) went baseline-FAIL→still-MISMATCH
(garbage OP_ADD → "runs but wrong"). Root cause is **NOT** the `??` lowering: the
self-host's `throw` in EXPRESSION/value position is lowered as a no-op (the `??`
None-arm's `__bb2` just `goto merge` with no Error-return; the merge then builds an
`Ok` with an uninitialised payload). The emitted C even stubs `#define Error(x)
((int64_t)0)`. My `??` gate behaves correctly — it defers to whatever terminator
the rhs sets; `throw`-in-value-position simply doesn't set one. `snag44` flips
because its throw path is never taken at runtime. **Follow-up:** lower a bare
`throw E` / `throw msg` in value position to a divergent `GTReturn(Error(payload))`
(Rust `lower_throw`, `stmts/mod.rs:2244`, threaded through the expression path) —
this would flip `dop_throw_rhs` and harden the `??`/throw family. NOT a regression
(baseline equally broken), NOT in scope here.

---

## 2. Pair 3 — aliased imports (A.4) + `rename_import_aliases` Pass-1.5 (reference-grade, FULL rewrite)

### Re-verified failure
Baseline `import_alias.gg` CC-FAILs: `undefined reference to 'msin'`. With the A.4
registration half alone (resolver registers `msin`), the LOWERER's
`map_runtime_name` maps the SURFACE call name — `sin`→`gorget_sin`, but `msin`
maps to itself → undefined symbol. The fixture stays red until the alias is
rewritten back to its original BEFORE lowering.

### Registration half (re-applied from `b242f98b`)
- `ast.gg:255` — `Vector[String] aliases` on `ImportStmt` (parallel to `names`;
  "" = bare).
- `parser.gg:3975/3986/3997` — `parse_import_alias()` consumes optional
  `as <ident>` after each imported name; `aliases` parallel to `names`.
- `resolve.gg:281-292` — `collect_import` registers each name under its
  locally-visible spelling (alias when present). Mirrors Rust `rebind_alias`
  (`resolve.rs:234-241`).

### Lowering half — reference-grade FULL AST-walk rewrite (NOT a `map_runtime_name` shortcut)
New pass `rename_import_aliases(&m)` in **`meta.gg`** (after the existing
`subst_*` infra it mirrors). Faithful port of Rust's `rewrite_import_aliases`
(`rewrite.rs:42-124`, wired at `mod.rs:248` after resolve / before lowering):
- `collect_import_aliases(m) -> Dict[String,String]` — scans `IImport` items,
  maps each non-empty `alias → original` surface name.
- `rename_aliases_expr` — recursive identifier-rename walk (`EIdentifier`, `ECall`
  callee, and through every sub-expression form `subst_expr` covers) — renames
  `msin`→`sin`.
- `rename_aliases_stmts` — full statement walk (SExpr/SReturn/SVarDecl/SAssign/
  SCompoundAssign/SIf/SWhile/SFor/SMatch), mirroring `subst_stmts`.
- `rename_import_aliases(&m)` — rebuilds every IFunction/IEquip/ITrait body with the
  renamed statements (the same item-reconstruction template as
  `apply_collect_target_rewrites`).

Wired into **`driver.gg`** at all 3 entry sites, right after
`apply_collect_target_rewrites(&m)` (Rust's after-resolve / before-lower order).
Also: **`format.gg`** `format_import` now renders `as <alias>` for round-trip
fidelity (the bootstrap formats then re-parses).

**Scope note (honest):** the identifier-axis (call callees / value refs) is the
full coverage `import_alias.gg` needs and the full coverage `subst_expr` provides.
Rust's `rename_type`/`rename_function` also rename aliased TYPE names in
signatures; that axis is unexercised by any current fixture and is a trivial
extension of the same walk (add `rename_aliases_type` + thread through param/return
types) — spelled out here so the integration version covers it.

### Proven (byte-exact)
```
$ /tmp/sh_driver import_alias.gg … | cc … && ./a.out
0.000000
1.000000
3                     ← MATCHES gg run, byte-exact
```
Emitted C: `gorget_sin(__v1)`, `gorget_cos(__v11)`, `gorget_abs(__v23)` — the
aliases were renamed to surface names before `map_runtime_name`; bare `abs` works
too. **No `msin`/`mcos` anywhere.** Before: `undefined reference to 'msin'`.

### Multi-copy
`self_host_resolver/` (independent 4th resolver copy) + `self_host_parser/`
(independent ast/parser) were NOT touched — they keep the 2-field `ImportStmt` and
stay internally consistent; their comparisons are unaffected (resolver 1276/15,
parser 1270/21 — byte-identical to baseline). **For the resolver_comparison
`import_alias` win, the integration version must ALSO apply A.4 to
`self_host_resolver/{ast,parser,resolve}.gg`** (lowering files live only in
`self_host_lowerer`).

---

## 3. Pair 2 — field_access (A.5) + `field_value`/`field_set` rewrite — **DEFERRED-AS-TOO-DEEP**

### Re-verified failure + ROOT CAUSE (the rewrite is NOT the blocker)
Baseline `field_access.gg` CC-FAILs: `undefined reference to 'field_value'`. But
the deeper, load-bearing finding: **the self-host does not expand `meta for fname,
ftype in fields(T)` in STATEMENT bodies AT ALL.** The monomorphised generic
bodies come out EMPTY:
```c
Str to_debug__Point(__gg_Point __p0) { … __v4 = __s0; return __v4; }   // returns "" — the whole meta-for body dropped
int64_t sum_int_fields__Point(__gg_Point __p0) { … __v1 = 0LL; return __v1; }   // returns 0
void zero_int_fields__Point(void* __p0) { … return; }   // no-op
```
The self-host's `expand_meta_types` (meta.gg:654) handles meta CONSTS, top-level
`meta if`, and `meta for` in MATCH arms — but has NO per-generic-instantiation
**delayed-meta evaluation** (`fields(T)` reflection + `meta for`/`meta if`
statement-body unrolling). So the `field_value`→`.field` rewrite alone flips
NOTHING: the generic functions stay wrong, and BOTH fixtures that use `field_value`
(`field_access`, `trait_default_meta`) ALSO use `meta for fields(T)` (verified —
there is no direct-only `field_value` fixture).

### Reference-grade target (precisely spelled out)
Two pieces, in order:

1. **The delayed-meta-for engine** — port Rust's `evaluate_delayed_meta_block`
   (`meta.rs:3300-3470`) + `eval_delayed_expr` + `eval_delayed_meta_range` + the
   **`fields(T)` builtin** (`meta.rs:2990`, returns a `MetaValue::List` of
   `[name, type]` pairs off the struct's `TypeInfo`). This runs during generic
   monomorphisation: for each struct field it binds `fname`/`ftype` into a child
   meta-env, `substitute_block`s the loop body, recursively evaluates nested
   `meta if ftype == "int":` per field, and splices the unrolled statements. This
   is the substantial piece — a multi-hundred-line meta-engine addition, NOT a
   localised lowering pass. It is the genuine "too deep for the scout window"
   case the brief anticipated.

2. **The `field_value`/`field_set`/`make_variant` rewrite** — port Rust's
   `rewrite.rs:810` (direct literal `field_value(p,"x")` → `p.x`) AND
   `meta.rs:2507`/`:2146` (the post-substitution rewrite that fires AFTER the
   meta-loop var `fname` is substituted to a string literal, inside the engine of
   piece 1). Both are ~25-line AST rewrites. In the self-host (no dedicated
   `rewrite.rs` pass), the reference-grade home is a new pre-lowering AST rewrite
   alongside `rename_import_aliases` (the same `subst`/walk infra), PLUS the
   post-substitution hook inside the piece-1 engine. **Do not prototype a
   field_value-only shortcut** — it would be dead without piece 1 and would flip
   no fixture (verified). The A.5 registration half (already correct) is the only
   shippable part of this pair until piece 1 lands.

**Recommendation:** field_access is gated on the delayed-meta engine. Build piece 1
as its own scout+brief cycle (it unblocks the whole `meta for fields(T)` reflection
family, not just one fixture); the field_value rewrite (piece 2) rides on top.

---

## 4. No-regression evidence (all on the FINAL combined code, this session)

- `self_host_bootstrap_fixed_point`: **GREEN** (`GG_BUILD_TIMEOUT_SECS=600`,
  release, `--test-threads=1`): `test result: ok. 1 passed` (285.68s). The
  bootstrap compiles the driver's OWN source — now containing the `??` lowering,
  panic arm, `rename_import_aliases`, `as`-parsing, 3-field `ImportStmt`, and the
  `as`-rendering formatter — and re-converges byte-identical.
- `cargo test --lib`: **1084 passed, 0 failed**.
- All 8 `*_comparison`: **byte-identical to the `b242f98b` HEAD baseline** (see §0).
- `GG_RUNTIME_DIFF=1 … self_host_runtime_diff`: **731 → 735 MATCH (+4)**,
  fail-set diff shows **exactly 4 flips, 0 new failures** (regression-free).

### Re-runnable commands
- Build driver: `GG_BUILD_TIMEOUT_SECS=600 gg build
  tests/fixtures/self_host_lowerer/driver.gg -o /tmp/sh_driver`.
- Per-fixture: `/tmp/sh_driver F.gg lib --emit-c
  --runtime-dir=$PWD/src/backend/c/runtime > F.c; cc -O0 -w -o F F.c -lm
  -lpthread; ./F </dev/null` vs `gg run F.gg`.
- Parity: `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test
  integration --release self_host_runtime_diff -- --nocapture` → read the
  `MATCH : N` line.

---

## 5. Copies touched

| file | change | symlinked into |
|---|---|---|
| `self_host_typechecker/resolve.gg` | A.2 panic + A.5 carve-out + A.4 register | lowerer/, check/ (×3) |
| `self_host_typechecker/parser.gg` | A.4 `as`-consumption | lowerer/, check/ (×3) |
| `self_host_typechecker/ast.gg` | A.4 `ImportStmt.aliases` | lowerer/, check/ (×3) |
| `self_host_typechecker/meta.gg` | `rename_import_aliases` pass | lowerer/, check/ (×3) |
| `self_host_typechecker/format.gg` | `format_import` `as <alias>` | lowerer/, check/ (×3) |
| `self_host_lowerer/lower_expr.gg` | panic arm + `??`/DefaultOp lowering | (real file, lowerer-only) |
| `self_host_lowerer/driver.gg` | `rename_import_aliases(&m)` ×3 sites | (real file, lowerer-only) |

NOT touched: `self_host_resolver/`, `self_host_parser/` (independent copies — keep
2-field `ImportStmt`; integration version must apply A.4 there for the
resolver_comparison `import_alias` win).

---

## 6. Bottom line for the orchestrator

1. **2 of 3 target pairs FLIP, +4 net runtime parity (731→735), 0 regressions.**
   panic_builtin + import_alias land their full reg+lowering pairs reference-grade;
   the `??`/DefaultOp lowering is the high-leverage win (4 flips, whole family).
2. **field_access is correctly DEFERRED** — its blocker is the missing
   delayed-meta-for-fields engine, NOT the field_value rewrite (the rewrite alone
   flips nothing; verified). Reference-grade target spelled out in §3 (build the
   meta engine as its own cycle).
3. **One pre-existing gap surfaced, filed not dodged:** `throw` in value position
   (`dop_throw_rhs`) is lowered as a no-op → the `??`/throw-rhs path runs-but-wrong
   when actually taken. NOT a regression, NOT caused by the `??` lowering. Follow-up
   = lower value-position `throw` to a divergent `GTReturn(Error(...))`.
4. **All gates green** (fixed_point, lib, all comparisons byte-identical). The
   integration version must additionally apply A.4 to `self_host_resolver` (for the
   resolver_comparison win) and extend `rename_aliases_*` to the type axis.
