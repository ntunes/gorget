# FIDELITY brief — borrowed-String (`Ptr(GorgetString)`) printing/formatting (self-host)

## Goal
`print(x.strfield)` and `f"{x.strfield}"` — where `x.strfield` is a **String
field of a struct** (or a String bound from an enum payload via `match`/`is`) —
print the GorgetString **pointer as an int** instead of the string. Fix both
formatters to handle a borrowed String. Targets `match_expr_block_arms`
(WRONG→MATCH, confirmed) + the broad inline-struct-String-print class; **+N**
(authoritative count from runtime_diff).

## ⚠ Corrected diagnosis (the TODO was WRONG — re-verified by RUNNING)
TODO framed this as "bug (1): struct-with-String enum-payload binding mis-lowers
in `lower_match_stmt`." **RUNNING disproves that:** a PLAIN read with no match/is
already fails —
```
struct Item: String desc
void main():
    Item x = Item("hello")
    print(f"got: {x.desc}")   # oracle "got: hello"; self "got: 281474750871032"
    print(x.desc)             # oracle "hello";      self "281474750871032"
```
So the root is NOT pattern-binding; the match/`is` cases are just instances of a
**general borrowed-String formatting gap**. Discriminating tests:
- `String v = x.desc; print(v)` → ✅ (SVarDecl's declared type forces a value).
- `print(x.n)` (int field) → ✅ (int formatting is fine).
- `print(x.desc)` / `f"{x.desc}"` (String field, INLINE) → ❌ (the bug).

## Root cause (precise, writer-site)
`EFieldAccess` on a **resource** field (String/Vector/struct) correctly types its
result local as `Ptr(<field type>)` — a borrow alias, NOT a copy (lower.gg
~5520-5524: `if field_is_resource: dst_type_id = register_ptr(...)`). So `x.desc`
is a local typed `Ptr(GorgetString)`. That is correct (read = borrow).

Both formatters then mis-classify it because they use an **exact** id check:
- print: `if val_type == gs_tid:` (lower.gg ~6123) — misses `Ptr(GorgetString)`.
- f-string: `elif aty == gs_tid:` (lower.gg ~4345) — misses `Ptr(GorgetString)`.

`gs_tid` is the bare `GorgetString` value id; `Ptr(GorgetString)` is a different
id, so the check fails → falls to `%lld` → prints the pointer. (`is_string_type_id`
at lower.gg:3906 already recurses through `GtPtr`/`GtMutPtr`, but the formatters
don't call it.)

## The fix (both formatters, lower.gg only)
When the format arg is a **pointer-to-String**, deref it to a NON-OWNING value
`GorgetString` local, then format via the existing `%s`/`.data` value path:
```
if is_ptr_to_string_type_id(val_type, &gmod):
    int sval = add_local_with(&ctx, gs_tid, NO_NAME, LoBorrowed(), BoNone())
    emit(&ctx, GIDeref(sval, val, gs_tid))
    val = sval; val_type = gs_tid
```
`LoBorrowed` is load-bearing: the deref'd value aliases the original's heap
buffer; marking it non-owning means drop-elab won't free it (no double-free —
the field's owner frees it). Mirror the box-deref precedent at lower.gg:5475-5476
(`add_local_inheriting` + `GIDeref`).

### ⚠ GUARD PRECISION (refinement over the first prototype)
The prototype used `is_string_type_id(val_type) and val_type != gs_tid`. That is
slightly too loose: `is_string_type_id` ALSO matches a VALUE String typed under a
`"String"`/`"Str"` GtNamed id distinct from `gs_tid` — deref'ing a *value* as if
it were a pointer would be wrong. Tighten to a precise **pointer-to-string**
predicate (new helper near `is_string_type_id`):
```
bool is_ptr_to_string_type_id(int tid, GirModule &gmod):
    match gmod.type_table.get(tid).unwrap():
        case GtPtr(inner):    return is_string_type_id(inner, &gmod)
        case GtMutPtr(inner): return is_string_type_id(inner, &gmod)
        else:                 return false
```
(Reviewers: confirm whether value-String-under-`"String"`/`"Str"`-alias-id
actually occurs in the corpus — if it provably never does, the looser guard is
equivalent; the precise guard is correct either way and strictly safer.)

## Scope / what this does NOT fix
- `match_expr_diverging_arm` stays DIFF — its remaining line is **bug (2):
  `s.data` (the `.data` field on a `GorgetString`)** → lower.gg:5528-5537
  emits the `[bug] unknown field 'data'` I64(0) placeholder (the field isn't in
  GorgetString's `type_infos`). SEPARATE root, separate follow-up — do NOT bundle.
- Do NOT touch `EFieldAccess`'s `Ptr`-typed result (correct for the borrow model).
- Do NOT reshape fixtures.

## File zone
ONLY `tests/fixtures/self_host_lowerer/lower.gg` (the new helper + the two
formatter call sites ~4342 and ~6109).

## Gates (force-rebuild driver: `rm tests/fixtures/self_host_lowerer/driver{,.c}`)
- `match_expr_block_arms` MATCH; the 3 repros MATCH; `match_expr_diverging_arm`
  still DIFF only on the `.data` line (bug 2).
- `runtime_diff` parity ≥ 287 (target +N); no MATCH→worse.
- `self_host_runtime` regressed=0 (then regen → new passing set).
- `lowerer_comparison` ≥954, `c_emit_comparison` ≥883 (a value-position/format
  fix; fn-counts shouldn't move).
- `bootstrap_fixed_point` GREEN — ⚠ does the DRIVER itself print/format a borrowed
  String? If yes, fixed_point reconverging is a real neutrality signal; if the
  driver always binds strings to typed locals first, it's a pure guard.
