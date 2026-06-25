# Scout: `Option[T] G = None` static-init miscompile — REAL bug is `const`, not `static`

**Date:** 2026-06-25
**Base:** my worktree branch at gorget-1 tip `5d6e9261` (`git merge --ff-only gorget-1` = already up to date)
**Verdict:** the bug as briefed (a `static`/`public static` zero-init) **does NOT reproduce** on the
current tip — the `static` path is correct. The REAL live miscompile is in the **`const`** path:
a module-level `const` whose initializer is not compile-time-foldable (any enum/struct constructor,
including `None`) is silently lowered to an `I64(0)` placeholder. Recommended fix = **reject** it at
typecheck (Core invariant #8 — make the language reject the ill-formed program), prototyped + measured
below.

---

## 1. Repro — measured by RUNNING (not source-reading)

Option ordinals: `enum Option[T]: Some(T) / None` ⇒ **`Some` = ordinal 0, `None` = ordinal 1**
(`src/parser/tests.rs:783`, prelude). So a zeroed tag reads as `Some` — the danger the brief described
is real, but it only manifests on the `const` path.

Test program (`match G` with `f"some:{x}"` / `"none"` arms):

| Declaration form                          | Output      | Verdict |
|-------------------------------------------|-------------|---------|
| `Option[int] G = None` (bare top-level)   | `none`      | ✅ correct |
| `static Option[int] G = None`             | `none`      | ✅ correct |
| `public static Option[int] G = None`      | `none`      | ✅ correct |
| `private static Option[int] G = None`     | `none`      | ✅ correct |
| **`const Option[int] G = None`**          | **`some:0`** | ❌ **BUG** |
| **`public const Option[int] G = None`**   | **`some:0`** | ❌ **BUG** |
| **`const Option[int] G = Some(42)`**      | **`some:0`** | ❌ **BUG** (payload lost too) |
| **`const Color C = Color.Blue()`** (ord 2) | **(empty)**  | ❌ **BUG** (match falls through) |
| **`const Color C = Color.Green()`** (ord 1) | **(empty)** | ❌ **BUG** |
| `const Option None`, read via `.is_some()` | `is_none`  | ✅ (no `match` to fold) |

**The brief's premise is wrong about which keyword triggers it.** `static`/`public static` are CORRECT
(they emit a runtime `__gg_static_init_G()` that writes the proper tag — see §2). The live bug is
`const`, and it is **broader than None**: ANY non-foldable `const` initializer (Option, user enum at any
ordinal, struct literal) miscompiles. I confirmed the `static` path is correct on BOTH the current tip
and the parent of the most-recent enum-related commit (`847e767b^` = `69786c3c`) — so this was never a
`static` regression.

---

## 2. Root cause (file:line)

### Why `static` is correct (the template)
`static`/`public static` parse to `Item::StaticDecl` and go through `lower_static_decl`
(`src/ir/lowering/mod.rs:2402`). Each is registered in `global_names`
(`src/ir/lowering/mod.rs:1272`) and gets a **runtime initializer function**. Generated C for
`static Option[int] G = None`:

```c
__gg_Option__int64_t __lir_g0 = {0}; // G        // decl zero-init (tag 0) ...
...
__gg_Option__int64_t __gg_static_init_G(void) {
    ...
    __v4 = (void*)&((__gg_Option__int64_t *)(__v0))->tag;
    __v5 = (int32_t)1LL;          // <-- writes None's tag = 1
    *(int32_t*)(__v4) = __v5;
    ...
}
...
__lir_g0 = __gg_static_init_G();  // ... overwritten at startup, BEFORE main
```

The `{0}` decl is harmless because the runtime init runs first and writes tag 1. **Correct.**

### Why `const` is broken (the bug)
`const` parses to `Item::ConstDecl`. The only handling is a compile-time fold:
`eval_const_expr(&const_def.value.node, ...)` at `src/ir/lowering/mod.rs:548`, result stashed in
`ctx.module_constants`. **`eval_const_expr` (`src/ir/lowering/mod.rs:3737-3802`) has NO arm for enum/struct
constructors** — `None`, `Some(..)`, `Color.Blue()`, `Path`, `Call`, struct literals all fall to the
terminal `_ => None`. So:

- `module_constants` gets **no `G` entry** (fold failed), AND
- `ConstDecl` is **never** registered in `global_names` (only `StaticDecl` is, line 1272).

At every use site, `Expr::Identifier("G")` is lowered in `src/ir/lowering/exprs/mod.rs:171-188`. It checks
locals → `module_constants` (miss) → `global_names` (miss) → `fn_sigs` (miss) →
`resolve_enum_variant_typed` (miss, `G` isn't a variant) → **terminal `else`**:

```rust
// src/ir/lowering/exprs/mod.rs:185-188
} else {
    // Could be a function name or unknown — produce a constant placeholder
    Operand::Constant(Constant::I64(0))
}
```

So `G` becomes the literal `0`. `match` then reads the "Option" tag from `0` ⇒ ordinal 0 ⇒ `Some` arm,
payload `0` ⇒ prints `some:0`. For `const Color C = Color.Blue()` the scrutinee is `0` ⇒ ordinal 0 =
`Red`, but the C-emit / match-lowering produces no matching `Color.Red()` arm path here so it prints
nothing. **The `I64(0)` placeholder is the symptom; the producer (`const` decl) never resolved or
rejected the non-foldable value.**

### Layering read (devbook/24)
This is a **write-site** bug per devbook/24 + Core invariant #1. The read site
(`exprs/mod.rs:187`) collapses a distinction — "valid foldable const" vs. "ill-formed non-foldable
const" — that the producer (the `const` decl) never resolved. The principled fix lives at the producer
(typecheck), not the read site. The `I64(0)` fallback is a legitimate catch-all for genuinely-unknown
identifiers (it should stay), so patching IT to special-case enums would be the wrong layer.

---

## 3. True scope

**Not None-only, and not `static` at all.** The bug is: **every module-level `const` whose initializer is
not compile-time-foldable** by `eval_const_expr` — i.e. any enum constructor (`None`, `Some(x)`,
`Color.Blue()` at ANY ordinal), `Path` variant, or struct literal. Foldable forms (int/float/bool/string
literals, references to other consts, arithmetic over those) are unaffected and correct.

The docs back this: module-level `const` means **"Compile-time constant, inlined at use sites"**
(`docs/language-reference.md:896`, `:912`); every documented example is a primitive
(`const int MAX_SIZE = 1024`, `const float PI = …`). There is NO documented support for a `const`
holding an enum/struct value — that's what `static` is for ("runtime value with global lifetime",
`docs/language-reference.md:897`).

**No existing fixture/self-host uses a non-foldable module-level `const`.** I tallied every module-level
`const` RHS shape across `tests/fixtures/` — all are int (`588×`), `-int` (`6×`), string (`3×`), `true`,
or float (`N.N`). Zero enum/struct/constructor RHS. So rejecting them breaks nothing.

---

## 4. Recommended fix — REJECT at typecheck (Core invariant #8)

Two directions were considered:
- **(A) Reject** — a non-foldable module-level `const` is ill-formed; emit a typecheck error pointing the
  user at `static`. Matches the documented `const` semantics ("inlined at use sites"), is backend-agnostic,
  and is the Core-invariant-#8 move (make the language reject the bad program rather than miscompile it).
- **(B) Promote to a runtime global** (route non-foldable `const` through the `static` machinery). Rejected:
  it conflates `const` (inlined, immutable, public-by-default) with `static` (one mutable runtime instance,
  private-by-default) — two distinct documented concepts — and silently changes the meaning of `const`.

**Direction A is recommended and prototyped.** The fix is a producer-site check in the typechecker's
`Item::ConstDecl` arm + a foldability predicate mirroring `eval_const_expr`'s capabilities.

### Minimal diff (prototyped + measured — 2 files, +50 lines)

`src/semantic/errors.rs` — new error kind + Display:
```rust
/// A module-level `const` initializer is not a compile-time constant ...
NonConstantConstInitializer { name: String },
// Display:
SemanticErrorKind::NonConstantConstInitializer { name } => {
    write!(f, "`const {name}` initializer is not a compile-time constant; `const` values are inlined at every use site. Use `static {name}` for a runtime-initialized global")
}
```

`src/semantic/typecheck.rs` — predicate (free fn near `equip_generic_names`) + check in the
`Item::ConstDecl` arm (~line 6985):
```rust
fn expr_is_const_foldable(expr: &Expr) -> bool {
    match expr {
        Expr::IntLiteral(_) | Expr::FloatLiteral(_) | Expr::BoolLiteral(_) => true,
        Expr::StringLiteral(lit, _) => {
            use crate::lexer::token::StringSegment;
            lit.segments.len() == 1
                && matches!(lit.segments.first(), Some(StringSegment::Literal(_)))
        }
        Expr::Identifier(_) => true,           // reference to another const/meta
        Expr::UnaryOp { operand, .. } => expr_is_const_foldable(&operand.node),
        Expr::BinaryOp { left, right, .. } =>
            expr_is_const_foldable(&left.node) && expr_is_const_foldable(&right.node),
        _ => false,
    }
}
// in Item::ConstDecl(c) =>, right after `let value_ty = checker.infer_expr(&c.value);`:
if !expr_is_const_foldable(&c.value.node) {
    checker.error(
        SemanticErrorKind::NonConstantConstInitializer { name: c.name.node.clone() },
        c.value.span,
    );
}
```

The predicate is deliberately a **superset-safe mirror** of `eval_const_expr`: it allows exactly the
forms that fold (numeric/bool literals, non-interpolated strings, const-name references, arithmetic/unary
over those) and rejects everything else. `Expr::Identifier` is allowed because a const-name reference is
resolved later by `eval_const_expr` (an undefined name is already caught by resolution). Keep the two in
sync if `eval_const_expr` grows arms (e.g. if enum-const-folding is ever genuinely added, relax both).

### Measured results
```
$ ./target/debug/gg run const_opt.gg     # const Option[int] G = None
error: `const G` initializer is not a compile-time constant; `const` values are inlined
       at every use site. Use `static G` for a runtime-initialized global
  ┌─ const_opt.gg:1:23
1 │ const Option[int] G = None
  │                       ^^^^
1 semantic error(s) found
```
- `const Some(42)`, `const Color.Blue()`, `const Color.Green()` → all REJECTED with the precise span.
- `static`/`public static Option None` → still `none` (correct, untouched).
- Legit consts (`const int A=5`, `const float P=3.14`, `const String S="hi"`, `const int B=A*2+1`,
  `const bool F=true`) → still build+run, print `5 3.140000 hi 11 true`.
- `meta int M = 1024` → unaffected (different item kind), prints `1024`.
- **`cargo build`** clean. **`cargo test --lib`** = **1084 passed / 0 failed**.
- Targeted integration `const_ static_ enum option_` = **81 passed / 0 failed**;
  `match_ pattern derive_ assert_ drop_` = **103 passed / 0 failed**.
- **LLVM backend** rejects identically (typecheck is pre-backend) and builds legit consts.

---

## 5. Fixtures to add (executor)

Negative (rejection) fixtures — expected `gg check` error (the harness's negative-test convention):
1. `const_enum_initializer_error.gg` — `const Option[int] G = None` + a `match G`; expect
   `NonConstantConstInitializer` ("not a compile-time constant … Use `static G`").
2. `const_enum_user_variant_error.gg` — `const Color C = Color.Blue()` (non-zero ordinal); same error.
   Pins the "generalizes beyond Option / beyond ordinal 0" property.

Positive (still-works) guard — to prevent over-rejection regressions:
3. Extend/keep `const_declarations.gg` (already covers int/float/bool/String) and add a
   `const int B = A * 2 + 1` const-reference-arithmetic line; expected stdout unchanged.

Companion positive guard that the *correct* path works:
4. `static_option_none_match.gg` — `static Option[int] G = None` + `match G` → expected stdout `none`.
   (Locks in that the fix does NOT touch the working `static` path; pairs with the negative const fixture.)

Wire the negative fixtures through the existing `*_error.gg` harness convention (e.g. alongside
`const_assign_error.gg`, `assignment_to_const_nested_error.gg`).

---

## 6. Self-host parallel follow-up (do NOT fix this round — filed)

The self-host has the **same `const`-enum gap**, matching Rust (so parity is preserved either way until
both are fixed). Verified by source-reading the self-host lowerer (cheap; no `const Option` fixture exists
to run):

- The self-host's static-init handling (`tests/fixtures/self_host_lowerer/lower.gg:2966-3011`) correctly
  records a `static Option[T] X = None` in `gmod.none_decls` and emits a typed `None` (IEnumInit tag 1)
  at the EIdentifier fallback (`lower_expr.gg:633`). This MIRRORS Rust's correct `static` path.
- BUT the **`IConstDecl` arm** (`lower.gg:2937-2962`) only folds int/float/bool/string and `pass`es
  (drops) any non-foldable value — it does NOT populate `none_decls`. A `const Option = None` would then
  hit the EIdentifier unknown-fallback placeholder, exactly like Rust's `I64(0)`. **Same bug, same
  layer.**

**Follow-up (parallel, after the Rust fix lands):** mirror the Rust reject in the self-host's `IConstDecl`
lowering arm — emit the self-host's diagnostic (or, minimally, route a non-foldable `const` enum to the
same `none_decls`/typed-`None` path the `static` arm uses) so the self-host rejects/handles it the same
way Rust does. This is the filed "LATENT silent-None class (self-host)" item; keep it in `TODO.md`.
The self-host `static` path is already correct, so this is `const`-only.

---

## 7. Blast radius + gate battery (for the executor)

**Blast radius:** small + surgical. Touches only `src/semantic/{errors.rs,typecheck.rs}`. Adds a new
diagnostic on a previously-silently-miscompiled construct; the `_ => false` predicate rejects exactly the
non-foldable forms `eval_const_expr` already couldn't fold, so no foldable const changes behavior. No
backend/LIR/GIR change. No fixture currently uses a non-foldable module-level `const`, so no existing
green test flips (verified by RHS-shape tally across `tests/fixtures/`).

**Watch-outs for the reviewer:**
- Keep `expr_is_const_foldable` a faithful mirror of `eval_const_expr`'s arms; a drift that's stricter
  than `eval_const_expr` would falsely reject a legit const. (Current predicate is a safe superset:
  allows `Identifier`, which `eval_const_expr` may still fail to resolve — but that's caught by
  resolution/used-before-def, not a miscompile.)
- `meta const` / `MetaConst` is a DIFFERENT item kind — confirm it's untouched (it is; `meta int M=…`
  measured fine).
- The `I64(0)` fallback at `exprs/mod.rs:187` is a legit catch-all (function names, etc.) — do NOT touch
  it; the fix is at the producer.

**Gate battery:**
1. `cargo build` (done — clean).
2. `cargo test --lib` (done — 1084/0).
3. Targeted integration: `cargo test --test integration -- --test-threads=4 const_ static_ enum option_ match_ pattern derive_` (done — green).
4. Add the 4 fixtures (§5) and run them on BOTH backends:
   `GG_BACKEND=llvm cargo test --test integration --release <new fixture names>`.
5. Parent drives the full `cargo test --test integration` sweep + `GG_BACKEND=llvm` sweep + a
   `tests/lints.rs` check that no new completed-status entry was added to `TODO.md`.
6. File the self-host follow-up (§6) in `TODO.md` as pending work.

---

## Appendix — exact prototype diff (in this worktree, NOT pushed)

```
 src/semantic/errors.rs    |  9 +++++++++
 src/semantic/typecheck.rs | 41 +++++++++++++++++++++++++++++++++++++++++
 2 files changed, 50 insertions(+)
```
(Throwaway scout prototype — for the executor to reproduce/refine, not to merge as-is. Only this scout
doc is committed on the scout branch.)
