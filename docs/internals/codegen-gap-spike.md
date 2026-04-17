# Codegen-Gap Spike Memo — `equip` on Primitive-Typed Receivers

Author: opus-4.7 session 2026-04-17.
Status: **read-only investigation**. No code changed.

## The gap in one sentence

`equip <PrimitiveType>: …` (most importantly `equip String:`) is parsed and
accepted by the semantic phase, but **every equip-lowering site in the Rust
backend filters on `Type::Named`** — so the method body is never lowered
into a GIR function. At the call site the method resolves, then falls
through to `map_monomorphized_to_runtime`, which for `String` rewrites
`GorgetString__user_method` to the nonexistent runtime function
`gorget_str_user_method`. The call discards the aggregate return value
and main ends up with garbage in the `Result[T, E]` slot.

This is what has been blocking:
- `equip String with Writer:` (Phase 3)
- Split-equip supertrait satisfaction (`equip ParseErr with Displayable`
  + `equip ParseErr with Error` — the Error equip rejects ParseErr as
  "missing `display`" because the other equip was for `String` receiver
  in the Error trait's derived helpers)
- Any future stdlib method on a built-in receiver that returns
  `Result[T, IoError]` / `Option[T]` / other aggregates.

## Reproducer (minimal)

```gorget
from std.io import IoError

equip String:
    Result[int, IoError] my_len(self):
        return Ok(self.len())

int main():
    String s = "hello"
    Result[int, IoError] r = s.my_len()
    match r:
        case Ok(n): print(n)      # expected: 5
        case Error(e): print(e.display())
    return 0
```

Observed: C compile error
```
error: incompatible types when assigning to type
  '__gg_Result__int64_t__IoError' from type 'int32_t' {aka 'int'}
```

GIR dump shows the method as an **extern with placeholder i32 return type**:
```
extern fn @GorgetString__my_len(...) -> i32
```
— no body.

## Root cause

Parse-tree representation: `equip String:` → `EquipBlock { type_:
Type::Primitive(PrimitiveType::StringType) }`. Same for `equip int:`,
`equip bool:`, etc.

Every downstream pass that looks at equip blocks filters on
`Type::Named { name, generic_args }`:

| File | Line | What it does |
|------|------|--------------|
| `src/ir/lowering/mod.rs` | 814–818 | Pre-scan: skip generic-argumented Named types |
| `src/ir/lowering/mod.rs` | 820–848 | Pre-scan: register fn_sigs for non-generic equip methods |
| `src/ir/lowering/mod.rs` | 1091–1101 | Lower equip method bodies to GIR |
| `src/ir/lowering/mod.rs` | 2239–2249 | `populate_trivial_getter_methods` |
| `src/ir/lowering/mod.rs` | 2274–2280 | `populate_gir_equip_methods` |
| `src/ir/lowering/traits.rs` | 1223–1236 | `extract_type_name` helper |

All five sites have the pattern:
```rust
if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
    // … register/lower …
}
```

`Type::Primitive(StringType)` skips every one silently.

## Workaround that exists today (undocumented)

Users can write `equip GorgetString:` instead of `equip String:` —
`GorgetString` is a bare identifier so it parses as `Type::Named`, all
paths work, and the call dispatch already mangles `String` → `GorgetString`
so `s.my_len()` on a `String` receiver resolves to `GorgetString__my_len`.

Verified in the spike: the reproducer above prints `5` if you change
the first line of the equip block from `equip String:` to
`equip GorgetString:`. No other changes needed.

This is not a fix — it's a leak. The design doc and every tutorial
shows `equip String:`. Users will hit the broken form first.

## Fix scope

**Option A — surgical: normalize `Type::Primitive` at the equip sites.**

Add a small helper `extract_equip_type_mangled(&Type) -> Option<String>`
that returns the correct mangled name for both Named and Primitive
types (`StringType` → `"GorgetString"`, `Int` → `"int64_t"`,
`Bool` → `"bool"`, etc.). Call it at all five equip-filter sites instead
of the raw pattern match.

- **Lines changed**: ~30. Five sites + one helper + possibly one helper
  in `traits.rs::extract_type_name`.
- **No new files, no public API changes.**
- **Pure additive behavior**: previously-silently-dropped equip blocks
  now produce correct functions. No existing code path changes semantics.
- **Tests needed**: one fixture exercising `equip String: … Result …`,
  one exercising `equip int:`. Plus the existing
  `duplicate_impl_error.gg` fixture (which happens to use `equip int
  with Sortable:`) should gain a positive twin once the duplicate-error
  lint path is verified to still fire.

**Option B — parser normalization**: rewrite `Type::Primitive(StringType)`
to `Type::Named { name: "GorgetString" }` when it appears in `equip`'s
type-slot. 1-line parser change. **Rejected**: couples the parser to a
specific downstream C name. Violates layering.

**Option C — semantic error**: reject `equip String:`, point users to
`equip GorgetString:`. **Rejected**: ergonomic regression; requires
documenting an alternative name that leaks backend details.

**Option A is the only candidate. Scope is ~30 lines in one file plus
a helper.**

## Risk to parallel efforts

### Self-host LIR backend

Self-host's equip-lowering in
`tests/fixtures/self_host_lowerer/lower.gg::lower_equip_block` already
handles `TPrimitive("String") → "GorgetString"` correctly via
`type_to_c_name` + `prim_to_c_name` (lowered via `match stype.ty:
case TPrimitive(name): …`). It *already does what Option A proposes for
Rust.*

**Net effect of landing Option A**: Rust aligns with self-host.
Divergence between the two goes **down**, not up. The self-host parser
test score and fn-count match should only improve, if anything — some
of the 275 remaining self-host failures may actually be cases where
Rust's lowerer silently dropped an equip block that self-host would
have lowered.

### LLVM backend

Unaffected. LLVM consumes LIR. With Option A, LIR gets one additional
`fn @GorgetString__user_method` per previously-dropped equip block.
LLVM emits that function like any other. No dispatch path changes.

### C backend

Unaffected — the call site already mangles `String` → `GorgetString`
for method dispatch. Once the function exists in `func_index`, the
LIR-lowerer picks the "registered function" branch at `insts.rs:399`
instead of falling through to the `map_monomorphized_to_runtime`
remapping, so `gorget_str_*` fallback stops firing on user equip
methods.

### Test suite

Grep for existing callers:
```
$ grep -rn "^equip [a-z]\+ " tests/fixtures lib
tests/fixtures/duplicate_impl_error.gg:4: equip int with Sortable:  (negative test — expects duplicate-impl error)
tests/fixtures/duplicate_impl_error.gg:8: equip int with Sortable:  (the duplicate)
```

Zero fixtures today actually *use* an `equip <Primitive>:` body, because
the feature has never worked. Adding the fix therefore:
- Cannot regress any existing test.
- Should fire the duplicate-impl semantic check on the
  `duplicate_impl_error.gg` fixture exactly as it does today (the lint
  lives in the semantic phase, before lowering, so it's unaffected by
  lowering-path changes).

## Recommendation

Land Option A whenever the self-host effort stabilizes, OR sooner if
self-host authors confirm they're not mid-refactor on
`lower_equip_block`.

The fix is:
- Small (30 lines).
- Directionally neutral (closes divergence with self-host, does not
  introduce new divergence).
- Strictly additive (silently-dropped input becomes correctly-lowered
  output).

Not landing it means Phase 3 (Writer/Reader impls on String/File/stdout)
stays stuck on wrapper-struct workarounds, and every code sample in
`docs/book/10-errors.md` / `19-stdlib.md` that writes `equip String
with Writer:` has to be rewritten to use a user struct — which users
will copy-paste and then wonder why their own `equip String` doesn't
compile.

## Out-of-scope observations (flagged in TODO if not already)

- **The `map_monomorphized_to_runtime` rule for `GorgetString__*` is
  overly aggressive**: it remaps *any* `GorgetString__method` to
  `gorget_str_method`, including user equip methods. This is today
  guarded by the `func_index` check at `insts.rs:399` (registered
  functions skip the remap), but a future refactor that bypasses that
  check would reintroduce this gap. Worth a note in the lift-tracking
  TODO.

- **`is_dict_hof` / `original_name` were load-bearing dead code** I
  removed this session. If the self-host C backend mirrors those
  branches, it should similarly prune them — cross-reference when the
  self-host C backend catches up.

- **DictIter Ptr-ABI panic** at `emit_types.rs:1645` when constructing a
  user Iterator struct from `Dict.keys()` is a *separate* gap with its
  own root cause (scalar passed where Ptr ABI expected). Not covered
  by Option A. Logged in TODO.
