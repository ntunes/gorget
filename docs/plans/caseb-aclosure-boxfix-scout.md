# Case-B "A_closure" Box-fix scout — cracking the latent `Box__T._0`-vs-`void*` lowering bug

**Scout date:** 2026-06-24 · **Base tip:** gorget-1 `2cf266fe` · **Prototype commit:** see `PROTOTYPE(scout)` on this branch — **DO NOT INTEGRATE**.

All numbers below were regenerated THIS session from the cited commands on the current tip.

---

## TL;DR / verdict — SHIPPABLE

- **Root cause FOUND and FIXED.** The blocker was NOT in `lir_lower.gg`'s construction-site/typedef as the prior scout hypothesized (`docs/plans/caseb-aclosure-scout.md` §4). It is one layer up, in `lower.gg`'s **generic-struct monomorphization-record loop**: `Box` is a registered `struct Box[T]: pass` template (`lib/std/collections.gg:18`), and the mono-record loop's skip-guard only excludes `is_builtin_collection_base` — which **intentionally omits `Box`** (per its own lint contract). So when `Box` is de-keyworded, `Box[SpannedType]` reaches that loop, `Box__SpannedType` gets registered in `type_infos` as a user struct → its ctor's dst slot types as the struct → `try_lower_user_struct_ctor` emits `((Box__SpannedType*)x)->_0 = …` against the `typedef void*` → stage-1 cc fails.
- **The fix is a 1-condition WRITE-site skip** at the mono-record loop (`lower.gg:3279`), reading the **TYPED `box_kind` channel** (`BkRegularBox`/`BkTraitBox`) — NOT a `Box`-name string match. It **mirrors Rust gg exactly** (`src/ir/lowering/types.rs:699`, which intercepts `"Box"` in the SAME match arm as the collection bases, BEFORE the generic-template path, and `return`s).
- **De-keyword + Box-fix → `self_host_bootstrap_fixed_point` GREEN** (re-converges, 281s, stage-2==stage-3==stage-4) + **stage-1 cc GREEN** (0 errors). **PARITY-NEUTRAL: 754/1069 = 70.5%** (= baseline). **Lock-in net 727/0.** Comparisons: parser **+1**, resolver **+1**, type/check/c_emit/lowerer flat, 0 crashes.
- **The `s`/`n` binding is RESTORED** (the β-flip-prereq value): snag51's `enum Box` is now well-typed `__gg_Box` with `->A_0`/`->B_0` extraction (was MANGLED `* self` under the keyword). snag51 stays a FAILING bucket on the SEPARATE closure-call ABI mirage (a distinct, named, filed follow-up — §5).

**Recommendation: LAND de-keyword + Box-fix together as one increment.** It is small (7 files: 6 de-keyword + 1 fix), reference-grade (mirrors Rust + reads typed metadata), bootstrap-green, parity-neutral, and lint-clean. The β-flip prerequisite is delivered.

---

## 1. Reproduce — stage-1 cc RED (de-keyword applied, current tip)

The de-keyword (per `caseb-aclosure-scout.md` §2; re-applied surgically because the prior prototype's `parser.gg` predated Track-α slice-4c at `85217064`) reproduces the exact RED:

```
/tmp/s1_dekw.c:54774:35: error: request for member '_0' in something not a structure or union
    ((Box__SpannedType *)(__v652))->_0 = (void*)&__v651;
/tmp/s1_dekw.c:59593:35: error: ... ((Box__SpannedExpr *)(__v324))->_0 = (void*)&__v322;
/tmp/s1_dekw.c:59650:35: error: ... ((Box__SpannedExpr *)(__v357))->_0 = (void*)&__v355;
```
`typedef void* Box__SpannedType;` (stage1.c:5627) — a `._0` struct-init written onto a `void*`.

**The 3 sites** are the SOURCE of the self-host's own `parser.gg`, which constructs boxed enum-variant payloads with explicit type args:
- `parser.gg:1879` — `err_type = Some(Box[SpannedType](rt))`  → `Box__SpannedType`
- `parser.gg:2130` — `EImplicitClosure(Box[SpannedExpr](val))` → `Box__SpannedExpr`
- `parser.gg:2136` — `EImplicitClosure(Box[SpannedExpr](val))` → `Box__SpannedExpr`

Manual repro (the §7 procedure): `driver … lib --lir-c` → splice preamble → `cc` → exit 1, the 3 `._0` errors.

---

## 2. Root cause — PRECISELY ISOLATED (instrumented, both paths)

The prior scout's locus (`lir_lower.gg:643-656` construction-vs-typedef) is a SYMPTOM, not the disease. I instrumented `lower_call` (the GICall func_name + ret_type), `lir_lower`'s GICall arm (the dst slot type), and `lower_module`'s `struct_templates`. The probes (run on BOTH the keyword and de-keyword full driver self-compiles) decisively isolate the divergence:

### 2.1 The GICall func_name is IDENTICAL in both paths
Both keyword and de-keyword parse `Box[SpannedType](rt)` to `ECall(EIdentifier("Box"), [rt], targs=[SpannedType])` (keyword via `parser.gg:2716-2723`, identifier via `parse_expr_bp_with_lhs:1782` — structurally identical), then `lower_call` mangles `call_name = "Box" + "__" + "SpannedType" = "Box__SpannedType"` (`lower_expr.gg:4708-4711`). Probe (IDENTICAL both builds):
```
SCOUT-BOX-CALL fname=Box targs=1 call_name=Box__SpannedType   # ×1
SCOUT-BOX-CALL fname=Box targs=1 call_name=Box__SpannedExpr   # ×2
SCOUT-BOX-CALL fname=Box targs=0 call_name=Box                # ×123 (bare Box(elem) — these box-alloc fine)
```
So `lower_call` is NOT the divergence; the prior scout's "func_name == "Box" dispatch (lir_lower:3372) doesn't fire" is true for BOTH builds — the bare `Box(elem)` calls (targs=0) hit it; the explicit `Box[T](x)` calls (targs=1) never do, in either build.

### 2.2 The dst SLOT TYPE diverges → that's what flips `try_lower_user_struct_ctor`
Probe in `lir_lower`'s GICall arm (`func=Box__SpannedType`, sid=90):
```
KEYWORD:     SCOUT-LIR-BOX func=Box__SpannedType sid=90 slotty=3       STRUCTBASE+sid=100090
DE-KEYWORD:  SCOUT-LIR-BOX func=Box__SpannedType sid=90 slotty=100090  STRUCTBASE+sid=100090
```
- **Keyword:** dst `slotty=3` (= `I32_TYPE`, a scalar). `try_lower_user_struct_ctor`'s guard `slot_ty != LT_STRUCT_BASE + sid` (`lir_lower.gg:2835`) is TRUE (3 ≠ 100090) → returns false → falls through → box-alloc. **Correct.**
- **De-keyword:** dst `slotty=100090` (= the `Box__SpannedType` struct type). Guard PASSES → `IStructInit` → `._0` write. **cc-fail.**

### 2.3 The slot type comes from `ret_type`, which comes from `type_infos.contains("Box__SpannedType")`
Probe in `lower_call` right before the GICall emit:
```
KEYWORD:     SCOUT-RETTYPE call_name=Box__SpannedType ret_type=4    type_infos=0 is_type_ctor=0
DE-KEYWORD:  SCOUT-RETTYPE call_name=Box__SpannedType ret_type=161  type_infos=1 is_type_ctor=0
```
- **Keyword:** `type_infos.contains("Box__SpannedType") = 0` (FALSE) → ret_type stays I64(4), the dst slot ends up scalar → guard fails → box-alloc.
- **De-keyword:** `type_infos.contains("Box__SpannedType") = 1` (TRUE) → `lower_expr.gg:5142` sets `ret_type = lookup_or_register_named("Box__SpannedType")` (the struct type 161) → dst slot is the Box struct → guard passes → `._0`.

### 2.4 The terminal cause: `struct_templates.contains("Box")` flips with the keyword
`Box__SpannedType` lands in `type_infos` ONLY via the generic-struct mono-record loop (`lower.gg:3265-3330`), gated on `struct_templates.contains(mrinst.base_name)` (`:3278`). Probe after `struct_templates` is built (`lower.gg:3124-3131`):
```
KEYWORD:     SCOUT-STRUCT-TEMPLATES Box=0
DE-KEYWORD:  SCOUT-STRUCT-TEMPLATES Box=1
```
**This is the terminal divergence.** When `Box` lexes as a KEYWORD, `struct Box[T]: pass` (`lib/std/collections.gg:18`) fails to parse as a struct decl (`Box` isn't a valid struct-name identifier) → `struct_templates` lacks `Box` → the mono loop skips `Box[SpannedType]` → no `type_infos` entry → scalar slot → box-alloc. De-keyworded, `struct Box[T]` registers as a template → the mono loop runs → `Box__SpannedType` registered as a user struct → the whole `._0` chain fires.

**The construction-vs-typedef divergence the prior scout saw at `lir_lower.gg` is a faithful downstream consequence of this one upstream WRITE-site oversight (Layering rule 1: the writer was lossy — it let a runtime box type into the user-struct mono path).**

---

## 3. The fix — WRITE-site skip at the mono-record loop, mirroring Rust + typed metadata

### Rust reference (`src/ir/lowering/types.rs:699`) — THE shape to mirror
```rust
// Collection types: all resolve to GorgetArray/GorgetMap/etc. but need
// a registered TypeId so fields referencing them don't get UNIT_TYPE.
"Vector" | "Dict" | "HashMap" | "Set" | "HashSet" | "Box" => {
    register_collection_alias(mapper, registry, &name.node, generic_args, &mangled);
    return;                                // <-- intercepts Box BEFORE the generic-template loop
}
```
Rust groups `"Box"` IN THE SAME match arm as the collection bases and `return`s — `Box[SpannedType]` is NEVER monomorphized as a user struct. The self-host's mono-record-loop guard (`lower.gg:3276`) only checks `is_builtin_collection_base`, which omits `Box`.

### Why not "just add Box to `is_builtin_collection_base`"
That helper is the GorgetArray/Map/Set-backed set, enforced by `tests/lints.rs::collection_base_names_single_source` (line 2681: *"Box excluded"*; the lint REJECTS any name not in its `EXPECTED_BASES`). Adding `Box` there would FAIL the lint. The self-host design comment (`lower.gg:2280-2281`) is also explicit: *"`Box` is intentionally EXCLUDED here: the self-host registers `Box__<inner>` via lir_lower's BkRegularBox arm, not the mono-record loop."* — which is EXACTLY the property the fix enforces.

### The prototyped fix (`lower.gg`, mono-record loop `:3279`)
A SEPARATE typed skip, reading the `box_kind` channel — the same `BkRegularBox`/`BkTraitBox` discriminator `lir_lower.gg:413/643` already use (`resource_meta_for("Box")` → `BkTraitBox`; the mono `Box__<inner>` → `BkRegularBox`). No `Box`-name string match (Layering rule 2 / "No name matching"):
```gorget
        if is_builtin_collection_base(mrinst.base_name):
            continue
        # Box ... is a runtime box-alloc type, NOT a user struct ... [full comment cites
        # types.rs:699, the lint, lir_lower:413/643, and the ._0 cc-fail it prevents]
        bool mrinst_is_box = false
        match resource_meta_for(&gmod, mrinst.base_name):
            case Some(mrinst_rm):
                match mrinst_rm.box_kind:
                    case BkRegularBox():
                        mrinst_is_box = true
                    case BkTraitBox():
                        mrinst_is_box = true
                    case BkNotBox():
                        pass
            case None:
                pass
        if mrinst_is_box:
            continue
```
(+ `from gir import BoxKind, BkNotBox, BkRegularBox, BkTraitBox`.)

**Result:** `Box[SpannedType]` skips the mono loop → no `type_infos` entry → ctor result types scalar → `try_lower_user_struct_ctor` guard fails → box-alloc. The emit is now byte-for-byte the box-alloc shape (`__v652 = __gorget_box_alloc_SpannedType(__v651);`), identical to the keyword baseline.

---

## 4. MEASURED — bootstrap GREEN, parity-neutral (all regenerated THIS session, tip `2cf266fe`)

### Stage-1 cc — GREEN
`driver … lib --lir-c` (de-keyword + Box-fix) → **0 `Box__.*->_0` writes**, **3 box-alloc ctor sites present**; spliced stage1.c → `cc` **exit 0, 0 errors**.

### Bootstrap fixed-point — GREEN (the BLOCKER gate)
```
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
→ test result: ok. 1 passed; 0 failed; finished in 281.48s        # RE-CONVERGES (stage-2==3==4)
```

### Parity — NEUTRAL, 754/1069 = 70.5% (= baseline)
```
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture
→ MATCH 754 · WRONG 90 · CC-FAIL 194 · CRASH 31 · DRIVER-FAIL 0
→ PARITY = 754/1069 = 70.5%
```
No fixture flips MATCH↔non-MATCH. snag51 stays in a failing bucket (CRASH in the harness / clean-CC-but-empty-output by hand — the §5 closure mirage). The Box-fix is parity-neutral by design: it makes the de-keyworded `Box[T]` lowering CORRECT, which the keyword was masking; no program's *output* changes.

### Lock-in regression net — 727/0
```
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime -- --nocapture --test-threads=1
→ passing set : 727 · regressed : 0
```
Every committed snapshot byte-matches, incl. all `box_*`/`mutex_*`/`guard_*`/`import_type_alias_box`.

### Comparison tests (diagnostic; baseline vs fix, both regenerated this session)
| test | baseline (`2cf266fe`) | de-keyword+Box-fix | delta |
|------|----------------------|--------------------|-------|
| `lexer_comparison` | 0 mismatch / 0 crash | 0 mismatch / 0 crash | 0 |
| `parser_comparison` | 1270/1294 | **1271/1294** | **+1** |
| `resolver_comparison` | 1275/1294 | **1276/1294** | **+1** |
| `type_comparison` | 1222/1294 (1180 exact + 42 superset) | 1222/1294 | 0 |
| `check_comparison` | — | 1221/1294 (1160 exact + 61 superset), 0 crash | (flat) |
| `c_emit_comparison` | 1045/1294, 0 self-host crash | 1045/1294, 0 self-host crash | 0 |
| `lowerer_comparison` | 1071 matched, 5 crash | 1071 matched, 5 crash | 0 |

parser/resolver each **+1** (the de-keyworded `Box`/identifier fixture now agrees with Rust). The 5 `lowerer` "crashes" are PRE-EXISTING negative `*_error.gg` fixtures (`break_outside_loop_error`, …) — identical in baseline, none Box-related. c_emit/type/check FLAT → the Box-fix is C-emit-neutral (no new mismatches).

### s/n binding RESTORED (the β-flip-prereq value)
snag51's `enum Box: A(String) B(int)` now emits well-typed `struct __gg_Box { … Str A_0; int64_t B_0; }`, well-typed `Box__drop`/`Box__clone` (was MANGLED `* self` under the keyword), and `main`'s match does `((__gg_Box *)…)->tag` + `->A_0` (`s`) + `->B_0` (`n`) extraction. **The bindings are genuinely BOUND.**

---

## 5. Downstream blocker for snag51 (the mirage — SEPARATE, filed)

Even with de-keyword + Box-fix, snag51 does NOT reach MATCH. Its `mk` closures (`auto mk = (): match …`) emit a **`void`-returning** `__Closure_N__call` body that computes the tail value but `return;`s without propagating it, so `main` prints EMPTY (oracle prints the full menu incl. `Enum match: A('from-match-arm')`). This is the **closure-call ABI gap** — INDEPENDENT of the keyword/Box work. (Note: the Box-fix actually IMPROVED snag51 from a hard CC-FAIL to clean-CC-but-empty; it now mirages on the ABI gap only.)

**Filed follow-up:** *"self-host closure-call ABI: non-void closure bodies emit `void`-returning `__Closure_N__call`, dropping the tail value (snag51 `mk()` family)."* — the gate that unblocks snag51 → MATCH and likely a broader `.map(it…)` / IIFE-closure class. **NOT in scope here.**

---

## 6. Reference-grade gate (Core invariant #8) — PASS

- **De-keyword half** is reference-grade: matches Rust (`Box` = identifier, `token.rs:316-317`), makes the names genuinely bound, IMPROVES comparison parity (+1/+1).
- **Box-fix half** is a REAL lowering-correctness fix, not a dodge: the language SHOULD lower `Box[T]` enum payloads as box-alloc runtime values, consistently with the `typedef void*` and with Rust. It is a WRITE-site fix at the source (the mono-record loop let a runtime box type into the user-struct path), reading TYPED metadata (`box_kind`), not a read-site `._0` patch or a name-match band-aid. It mirrors Rust's `types.rs:699` exactly and respects the self-host's own design contract (`lower.gg:2280` + the `collection_base_names_single_source` lint).
- It does NOT re-keyword `Box` to dodge the bug (forbidden by "Don't redesign around compiler gaps").
- The bootstrap (Core #7) is GREEN. The lock-in net is 0-regressed.

**Executor's definition of done (MET by this prototype):** 6 files de-keyworded + `lower.gg` Box-fix → `self_host_bootstrap_fixed_point` GREEN, `self_host_runtime` 727/0, parser/resolver `+1`, type/check/c_emit/lowerer flat (0 crashes), full parity 754/1069 neutral. snag51 stays failed on §5 (separately filed).

---

## 7. Complete file/symlink site-list

### Source files edited (7 — stage by EXACT name)
**De-keyword (6 independent copies; per `caseb-aclosure-scout.md` §2):**
1. `tests/fixtures/self_host_parser/lexer.gg` — remove `KwBox..KwRwLock` from the `Keyword` enum + the 8 `keyword_from_str` arms.
2. `tests/fixtures/self_host_parser/parser.gg` — remove the 8 `case Kw{Box,Rc,Arc,Weak,Cell,RefCell,Mutex,RwLock}:` arms in `keyword_tag`.
3. `tests/fixtures/self_host_resolver/lexer.gg` — same as (1).
4. `tests/fixtures/self_host_resolver/parser.gg` — same as (2).
5. `tests/fixtures/self_host_typechecker/lexer.gg` — same as (1).
6. `tests/fixtures/self_host_typechecker/parser.gg` — same as (2).

**Box-fix (1 real file):**
7. `tests/fixtures/self_host_lowerer/lower.gg` — `+from gir import BoxKind, BkNotBox, BkRegularBox, BkTraitBox`; the typed `box_kind` skip at the mono-record loop (`:3279`).

### Symlink topology (inherit the edits — do NOT edit directly)
- `self_host_check/lexer.gg` → `../self_host_typechecker/lexer.gg`
- `self_host_check/parser.gg` → `../self_host_typechecker/parser.gg`
- `self_host_lowerer/lexer.gg` → `../self_host_typechecker/lexer.gg`
- `self_host_lowerer/parser.gg` → `../self_host_typechecker/parser.gg`
- `self_host_lowerer/ast.gg` → `../self_host_typechecker/ast.gg`

So editing the 3 `typechecker` copies propagates to `check` + `lowerer`. `self_host_lexer/` is ALREADY correct (the reference) — leave it.

### NOT needed (dead, per `caseb-aclosure-scout.md` §2.C)
The `KW_BOX..KW_RWLOCK` int consts (`parser.gg:124-126,153-157`), `extract_name`'s keyword arms (`:1094-1110`), and the `parse_type`/`parse_prefix`/`skip_type_lookahead` keyword intercepts (`:1368`, `:2685`, `:3397`) become dead (`keyword_tag` never returns `KW_BOX`). Inert; the executor MAY delete for cleanliness but it is not required for correctness/measurement. The prototype leaves them (minimal diff).

---

## 8. Decomposition — NOT needed

The increment is small (7 files, +34/−99 lines) and atomic: the de-keyword and the Box-fix MUST land together (de-keyword alone regresses the bootstrap; Box-fix alone is a no-op until `Box` de-keywords). One commit. The snag51 closure-call ABI mirage (§5) is the only deferred item, and it is a genuinely distinct, named, filed follow-up — not a decomposition of this work.

---

## 9. Commands to regenerate every number (after a force-rebuild)

```bash
# driver (self-compiles the de-keyworded lexer + the Box-fixed lowerer)
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg

# bootstrap (the BLOCKER gate) — expect GREEN, ~281s
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture

# parity (north-star) — expect 754/1069
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture

# regression net (build-breaking) — expect passing 727, regressed 0
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime -- --nocapture --test-threads=1

# comparisons (diagnostic; counts only) — expect parser 1271, resolver 1276, 0 crashes
for t in lexer_comparison parser_comparison resolver_comparison type_comparison check_comparison c_emit_comparison lowerer_comparison; do
  GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release $t -- --nocapture --test-threads=1
done

# manual stage-1 repro (to debug without the full bootstrap):
./tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.gg lib --lir-c > /tmp/s1body.c
PE=$(grep -n $'\ntypedef struct __gg_' tests/fixtures/self_host_lowerer/driver.c | head -1 | cut -d: -f1)
head -n $((PE-1)) tests/fixtures/self_host_lowerer/driver.c > /tmp/pre.c
cat /tmp/pre.c /tmp/s1body.c > /tmp/s1.c && cc -O0 -w -o /tmp/s1 /tmp/s1.c -lm -lpthread   # expect exit 0
grep -c 'Box__.*->_0' /tmp/s1body.c   # expect 0
```
