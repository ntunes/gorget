# Parity Triage Scout — 2026-06-25b

**Scope:** Triage the three run-confirmed candidates (A `unwrap_error`, B
range-slice routing, C `newtype_field_access`/`fstring_format`). RUN-confirm
each, prototype the narrowest fix, MEASURE full-corpus parity before/after.

**Tip:** gorget-1 `5d6e9261` (code tip; docs on top). Driver force-rebuilt
(`GG_BUILD_TIMEOUT_SECS=600`) before every measurement.

**Measurement harness:** `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo
test --test integration --release self_host_runtime_diff -- --nocapture`
(reads `PARITY = MATCH/(...)`). Both BEFORE and AFTER measured on the SAME
current tree (the corpus grew to 1078 fixtures since the DONE.md
758/1072 — apples-to-apples requires re-measuring BEFORE on this tree).

---

## 🥇 RANKED RECOMMENDATION

**BRIEF AN EXECUTOR ON CANDIDATE A (`Result.unwrap_error()`).** It is a
**CONFIRMED, MEASURED, non-mirage +2 MATCH** with ZERO regressions — and the
prototype is already complete and gated. The last round filed A as "real but
deeper (≥3 sites + layout question)"; this round CRACKED it with a
self-contained clone-on-borrow fix (no call-ABI or drop-tracking rework).

- **A:** ✅ CONFIRMED +2 (`string_error_handling` + bonus `result_string_string`). Prototype done, gated below.
- **B:** ❌ MIRAGE — all 3 cited fixtures (`string_indexing`, `str_codepoint_index`, `cow_lazy_w3c_named_bind`) already MATCH on the current self-host. No residual. Do NOT chase.
- **C `newtype_field_access`:** ❌ MIRAGE — already MATCHes on the current self-host.
- **C `fstring_format`:** ⛔ CONFIRMED WRONG but DEEP — the ENTIRE f-string format-spec lowering (`:x`/`:X`/`:o`/`:b`/`:08d`/`:.2f`/`:.3e`) is missing in the self-host (every spec ignored). Not a bounded single-line fix; own scout if attacked.

---

## Candidate A — `Result.unwrap_error()` returns 0 → reference-grade fix

### Confirmed bug (both outputs measured)

`string_error_handling.gg`, `r2.unwrap_error()` on a `Result[int, String]`
Error variant:

```
ORACLE:    -5: negative: -5   |   abc: not a number: abc   |   empty: empty input
SELF-HOST: -5: 0              |   abc: 0                    |   empty: 0
```

### Root cause — TWO bugs at two layers

1. **Type inference** (`lower_types.gg`): `infer_method_return_type` has an
   `unwrap` arm (returns `uec.ok_type` from the typed enum-category channel)
   but **NO `unwrap_error`/`unwrap_err` arm** → falls through to the `I64`
   default → `r.unwrap_error()` typed `int64_t` → the f-string formats it
   `%lld` (`-5: %lld` template) and the Result-payload `Str` is never read.

2. **Codegen** (`lir_codegen.gg:~4258`, `emit_option_result_combinator`):
   `__result_unwrap_error` read at hardcoded **offset 8**, which is the
   `Ok_0` slot (Result layout `{ int32_t tag; T Ok_0; E Error_0; }` — the
   error payload is the THIRD field, not the second). The cited comment
   admitted it: *"approximate — works for simple scalar payloads."* On an
   Error variant the `Ok_0` slot is zero → prints `0`.

### Rust reference (the oracle)

- `src/semantic/typecheck.rs:5714` — `"unwrap_error" => Some(val_type())` (returns the Err type).
- `src/ir/lowering/builtins.rs:842` — `unwrap_error` decl, `self_conv: SelfConvention::Borrow` (BORROWING getter; the source Result stays live).
- `src/backend/c_lir/emit_types.rs:232` and `emit_hof.rs:154` — read `__src.{err_field}` (the named `Error_0` field via struct cast), NOT an offset.
- The Rust-emitted C for this fixture takes the ADDRESS of `Error_0` (`&((Result*)x)->Error_0`), reads a VIEW copy, and POISONS the Result tag to `2` (`tag = 2`) so the Result's drop `switch(tag)` misses `case 1` — a **move-out** of the error string.

### Why the naive offset fix double-frees (and how the prototype solves it)

Reading `Error_0` by value (the naive `((Result*)src)->Error_0`) and the
tag-poison BOTH FAIL in the self-host because **the self-host passes the
Result BY VALUE to the combinator** (`__v20 = __s11; ... (&__v20)`), so
poisoning reaches only a throwaway copy — the live local `__s11` is still
dropped, double-freeing the moved-out `Error_0` (measured: `double free or
corruption (fasttop)`). Rust avoids this by passing a POINTER to the live
local; reproducing that in the self-host is a deep call-ABI/drop-tracking
change (the "deeper" the last round flagged).

**The reference-grade self-contained fix:** `unwrap_error` is a BORROWING
getter (Rust `self_conv: Borrow`) — the source Result stays live and is
dropped normally, so a **resource (String) error payload must be CLONED** to
an independent owner. The Result keeps its own `Error_0` (freed by its drop);
the result owns a separate copy (freed once). No double-free, no need to
reach the live local, no tag-poison. Scalar Err payloads copy by value (no
clone). This is equally correct as Rust's move-out, and matches the
self-host's by-value call ABI without rework.

### Prototype diff (LANDED in this worktree, gated below)

**`tests/fixtures/self_host_lowerer/lower_types.gg`** — new arm in
`infer_method_return_type`, after the `unwrap` block (~line 2052):

```gorget
    if method == "unwrap_error" or method == "unwrap_err":
        if recv >= 0 and recv < ctx.locals.len():
            int uerr_tid = ctx.locals.get(recv).unwrap().type_id
            if uerr_tid >= 0 and uerr_tid < gmod.type_table.len():
                GirType uerr_gt0 = gmod.type_table.get(uerr_tid).unwrap()
                match uerr_gt0:
                    case GtPtr(uerr_inner):
                        uerr_tid = uerr_inner
                    case GtMutPtr(uerr_inner2):
                        uerr_tid = uerr_inner2
                    else:
                        pass
            EnumCategory uerr_ec = enum_category_of(&gmod, uerr_tid)
            if uerr_ec.category == ENUM_CAT_RESULT:
                return uerr_ec.err_type
        return I64_TYPE
```

**`tests/fixtures/self_host_lowerer/lir_codegen.gg`** — rewrite the
`__result_unwrap_error` arm in `emit_option_result_combinator` (~line 4258):

```gorget
        case "__result_unwrap_error":
            if dst >= 0:
                if src_ty >= LT_STRUCT_BASE:
                    String res_cn = c_type_name(src_ty, &sn)
                    String rc_cast = "((" + res_cn + "*)" + src + ")"
                    int rc_sid = src_ty - LT_STRUCT_BASE
                    if rc_sid >= 0 and rc_sid < m.structs.len():
                        LirStructDef rc_def = m.structs.get(rc_sid).unwrap()
                        if rc_def.fields.len() >= 3:
                            String rc_efty = field_type_str(rc_def.fields.get(2).unwrap().ty, &sn)
                            if rc_efty == "Str" or rc_efty == "GorgetString":
                                return v(dst) + " = gorget_string_clone_to_owned(&" + rc_cast + "->Error_0);"
                    return v(dst) + " = " + rc_cast + "->Error_0;"
                return "memcpy(&" + v(dst) + ", (char*)" + src + " + 8, sizeof(" + v(dst) + "));"
            return ";"
```

Notes on scoping: ONLY `__result_unwrap_error` is touched. `__option_unwrap`
(offset 8 = `Some_0` ✓), `__result_unwrap`/`__result_expect` (offset 8 =
`Ok_0` ✓) are CORRECT at offset 8 and left untouched — `unwrap_error` is the
sole combinator where offset 8 is the wrong field. The String-detection uses
a TYPED field-type comparison (`field_type_str(...) == "Str"`), NOT a
name-prefix — the name-prefix ratchet stays flat.

### Measured fixture flip + full-corpus parity delta

| | BEFORE (reverted) | AFTER (prototype) |
|---|---|---|
| PARITY | **760/1078 = 70.5%** | **762/1078 = 70.7%** |
| MATCH | 760 | 762 (**+2**) |
| WRONG-OUTPUT | 87 | 85 (−2) |
| CC-FAIL | 195 | **195 (identical set)** |
| CRASH | 36 | **36 (identical set)** |

**Flips (WRONG-OUTPUT → MATCH):** `string_error_handling` (the candidate) +
`result_string_string` (BONUS — a `Result[String, String]` fixture that
exercises String-Err `unwrap_error` AND a borrowed-param Ok; the clone path
handles String-Err even when Ok is also a String).

**Regressions: ZERO.** No fixture newly entered WRONG/CC-FAIL/CRASH; the
CC-FAIL and CRASH sets are byte-identical BEFORE vs AFTER. Both flipped
fixtures independently re-verified MATCH vs the `gg run` oracle.

### Lints impact

`cargo test --test lints` → **29 passed; 0 failed.** The fix adds no
`starts_with("PREFIX__")` routing (codegen uses typed field-type comparison;
type-inference uses exact `method == "unwrap_error"`), so the self-host
name-prefix ratchet (budget 75) and `count_name_prefix_sites_self_host` stay
flat. `no_growth_in_name_prefix_routing` and all sibling ratchets green.

---

## Candidate B — range-slice `s[a..b]` routing — MIRAGE (no residual)

RUN-confirmed all three cited fixtures already MATCH on the current self-host
(`gg run` oracle == self-host emit/cc/run, byte-identical):

- `string_indexing` — MATCH (`h`/`o`/`el`/`he`/.../`caf`/`o`).
- `str_codepoint_index` — MATCH (multi-byte `café`/CJK/negative/for-in).
- `cow_lazy_w3c_named_bind` — MATCH (`t = hello`/`c = e`/`s = hello`/`v.len() = 3`).

The string range-index → `gorget_str_slice` routing is already correct.
Confirms the prior scout's note. Do NOT chase.

---

## Candidate C — singles

- **`newtype_field_access`** — MIRAGE. RUN-confirmed MATCH (`3.140000` / `42`). The newtype `.0` field access already works. Do NOT chase.
- **`fstring_format`** — CONFIRMED WRONG but DEEP. The whole f-string format-spec feature is unimplemented in the self-host:

```
ORACLE:    hex: ff | HEX: FF | oct: 377 | bin: 11111111 | padded: 00000042 | fixed: 3.14 | sci: 3.142e+00 | neg hex: ffff...ffd6
SELF-HOST: hex: 255| HEX: 255| oct: 255 | bin: 255      | padded: 42       | fixed: 3.141593 | sci: 3.141593 | neg hex: -42
```

Every conversion/width/precision spec (`:x` `:X` `:o` `:b` `:#b` `:08d`
`:06x` `:.2f` `:.3e` `:.3E`) is ignored — the self-host emits the default
`%lld`/`%f`. This is a feature-port (the f-string format-spec lowering /
`gorget_string_format` spec handling), not a bounded triage win. Own scout if
attacked; not recommended as a quick win.

---

## Gate battery (for the executor / integration)

The fix touches self-host lowering (`lir_codegen.gg`), so the full battery:

1. `cargo test --test lints` — **29/0** (ratchet flat) ✅ measured.
2. `self_host_runtime` (snapshot lock-in) — must stay green; regen the 2 new MATCH snapshots (`string_error_handling`, `result_string_string`) via `GG_REGEN_RUNTIME_SNAPSHOT=1 … self_host_runtime -- --nocapture --test-threads=1`, commit the `.out` files so the flips are pinned (Core #6).
3. `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — load-bearing canary; the driver self-compiles its own `unwrap_error` call sites and must re-converge.
4. Parity re-confirm: `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → expect 762/1078.
5. Full `cargo test --test integration` (owner-required at round close).

Files touched (2; both `self_host_lowerer/`-only, NOT symlinked into the
other self-host dirs — `lower_types.gg` and `lir_codegen.gg` are independent
copies, so a single edit each):
- `tests/fixtures/self_host_lowerer/lower_types.gg`
- `tests/fixtures/self_host_lowerer/lir_codegen.gg`
