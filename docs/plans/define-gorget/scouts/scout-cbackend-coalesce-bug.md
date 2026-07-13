# Scout — the "latent C-backend coalesce bug" is a TYPECHECKER hole (not coalescing)

**Date:** 2026-07-13 · **Mode:** read-only investigation + prototype (worktree `agent-acf83322f9a3c4b15`, off `gorget-1`).
**TL;DR:** The prior scout's finger at `coalesce_assign_exact` (`src/backend/c_lir/mod.rs:2109`) is a **red herring**. The C backend is faithfully lowering a program the **typechecker wrongly accepted**. Root cause: `Expr::FieldAccess` (`src/semantic/typecheck.rs:2649`) returns the wildcard `error_id` — instead of reporting `NoFieldFound` — for a **field access on a type that has no such field** whenever the receiver is a *builtin generic* (`Vector`/`Dict`/`Set`/…) or a *primitive* (`int`/`String`/`bool`/…). `error_id` unifies with any downstream type, so the bogus access typechecks and the C backend emits `__sN(GorgetArray) = __vK(int32_t 0)` → `error: incompatible types … GorgetArray from int32_t`. Fixed at the write site (the typechecker); prototype confirmed on a **minimal 12-line repro**, the full CallArg-proto driver, `cargo test --lib` (1107/0), and the baseline self-host driver.

---

## Section 1 — Repro confirmation (full + MINIMIZED)

### 1a. Full repro (confirmed)
Applied `patches/callarg-normalization-proto.patch` (16 self-host `.gg` files) and built the driver:

```
$ ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg
driver.c:538856: error: incompatible types when assigning to type 'GorgetArray' from type 'int32_t' {aka 'int'}
 538856 |     #define __v9331 __coal75
        |                     ^~~~~~~~
 ... (7 identical errors) ...
C compiler exited with: exit status: 1
```

7 errors, exactly as the prior scout reported. **The gcc location is misleading**: it points at the `#define __vNNN __coalK` line because the coalescing macro *expands* `__vNNN` at the real assignment site and gcc attributes a macro-expansion type error to the macro definition. The prior scout's "In function `eval_meta_int_v2` (4×)" attribution is also a gcc artifact — **all 7 sites are in `lower_expr.gg`** (see §3d), not eval_meta.

The **real** assignment (traced from the macro, `driver.c`):
```c
531503:    GorgetArray __s2962 = {0};   // slot declared GorgetArray (Vector[SpannedExpr])
560952:    __v9331 = (int32_t)0LL;       // value 9331 = int 0
560953:    __s2962 = __v9331;            // ← GorgetArray = int32_t  (the cc error)
560954:    __v9332 = &__s2962;           // &slot passed as `margs.value`
```
`__s2962` is the materialization slot for the arg `margs.value` (6th arg of `try_lower_set_hof`, which takes `Vector[SpannedExpr] margs`). The LIR stores an **`int 0`** into a `GorgetArray` slot. The Gorget typechecker reported **0 semantic errors** for this program — that is the bug.

### 1b. MINIMIZED repro (12 lines, tiny function — NOT size-sensitive)
`patches/fieldaccess-fieldless-repro.gg`:
```gorget
struct Inner:
    int value

int count(Vector[Inner] xs):
    return xs.len()

int main():
    Vector[Inner] v = []
    v.push(Inner(5))
    v.push(Inner(9))
    int n = count(v.value)   # .value on a Vector[Inner] — Vector has no `.value` field
    print(n)
    return 0
```
```
$ ./target/release/gg check  repro1.gg   →  OK: no semantic errors        # (baseline: WRONG — should reject)
$ ./target/release/gg run    repro1.gg
repro1.c:2987:12: error: incompatible types when assigning to type 'GorgetArray' from type 'int32_t'
 2987 |     __s8 = __v13;      // __s8 GorgetArray, __v13 = (int32_t)0LL
```
**Identical error in a 4-instruction `main`.** The "perturbation-sensitivity / only-in-huge-functions" the prior scout observed was an illusion: the trigger is the *expression* `X.value` on a fieldless type, which the mechanical `.value`-insertion pass of the CallArg patch injected at 7 spots that happen to sit inside the two largest functions. Coalescing (huge functions) only changes *where gcc prints the error*, not *whether it occurs*.

### 1c. Probe matrix (baseline compiler, all should reject — none did)
| expression | receiver | baseline verdict | correct |
|---|---|---|---|
| `v.value` | `Vector[Inner]` | **accepted** (→ miscompile) | reject |
| `s.nonexistent_field` | `String` | **accepted** | reject |
| `x.nonexistent_field` | `int` | **accepted** | reject |
| `i.nonexistent_field` | `Inner` (user struct) | rejected ✓ | reject |
| `b.x` | `Box[Point]` (valid) | accepted ✓ | accept |

So: **concrete user structs already do field-existence checking; builtin generics and primitives do not.**

---

## Section 2 — Root cause (the precise defect, cited)

`src/semantic/typecheck.rs`, `Expr::FieldAccess` arm (~`:2649`). After peeling `Ref`, it handles two cases and otherwise returns `error_id`:

- **`ResolvedType::Defined(did)`** (concrete user struct, `:2676`): looks up `struct_fields.get(&did)`; if the field is absent it **reports `NoFieldFound`** (`:2685` in baseline). ✓ This is why `Inner.nonexistent_field` is rejected.
- **`ResolvedType::Generic(did, targs)`** (generic-struct instantiation, `:2723`): if the field is found, substitutes generic params and returns its type; **a missing field is deliberately NOT reported** (comment `:2714-2722`) because several wrappers auto-deref (`Box`/`Guard`/`Shared`/…). Falls through.
- **Everything else** (primitives; builtin generics whose `did` is not in `struct_fields`; enums; tuples…) → straight to the fallthrough.
- **Fallthrough (`:2767` baseline): `self.types.error_id`.** ← THE HOLE.

Two facts make this fatal:
1. **`struct_fields` holds only user `Item::Struct` defs** (`src/semantic/resolve.rs:556-591`). `Vector`/`Dict`/`Set` are builtin placeholder defs (`src/semantic/scope.rs:245`) **not** in `struct_fields`. So `v.value` enters the `Generic` branch, finds `struct_fields.get(&vector_did) == None`, skips the whole block, and hits `error_id`.
2. **`error_id` unifies with any concrete type** (documented at `src/semantic/mod.rs:397-410`: "returns `error_id`, which silently accepts any downstream parameter type"). So `count(v.value)` — `count` wants `Vector[Inner]` — typechecks. The IR then materializes an arg slot typed from the *callee's* `Vector[SpannedExpr]` param (→ `GorgetArray`) but lowers the `error_id` field-read as a zero (`int 0`), producing `slot(GorgetArray) = int32_t 0`.

This is a **known, partially-documented hole** (the `mod.rs:397-410` note + the `:2714-2722` "Strategy 2B follow-up" comment already name it). The exact analogue was already fixed for the `Deref` operator (`typecheck.rs:2948-2963`): *"Any OTHER concretely-resolved type is not deref-able. Without an error here the type checker returned inner_type unchanged (a silent no-op) and the IR lowering emitted a garbage pointer dereference that segfaults."* — same disease, same write-site fix.

**Layering verdict (per CLAUDE.md "fix at the write site"):** the C backend and `coalesce_assign_exact` are correct. `coalesce_assign_exact` groups strictly by the exact decl-ctype string, so it never merges an `int32_t` value with a `GorgetArray` value — the two `__coalK` slots are distinct and correctly typed. The mistyped store originates one layer up (the IR faithfully lowers a wildcard the typechecker minted). The fix is in the typechecker.

---

## Section 3 — The write-site fix (prototyped + confirmed)

### 3a. The change (`src/semantic/typecheck.rs`, ~74 lines; patch: `patches/fieldaccess-reject-fix.patch`)
1. Removed the `Defined` branch's inline `else { report NoFieldFound }` so all "absent field" reporting happens at **one** site.
2. Replaced the `error_id` fallthrough with a principled *definitely-absent* check that reports `NoFieldFound` for fieldless receivers and **suppresses** it for the legitimate "resolved late" cases:
```rust
let resolved_rt = self.types.get(resolved).clone();
let definitely_absent = match &resolved_rt {
    ResolvedType::Primitive(_) => true,                       // int/String/bool/… have no fields
    ResolvedType::Defined(did) | ResolvedType::Generic(did, _) => {
        let name = self.scopes.get_def(*did).name.clone();
        if is_field_deref_wrapper(&name) { false }            // Box/Shared/Mutex/RWLock/Weak/ReadGuard/WriteGuard/Guard
        else if let Some(sfi) = self.struct_fields.get(did) {
            !sfi.fields.iter().any(|(n, _)| n == &field.node)  // known field list → absent iff not present
        } else { true }                                       // builtin generic/opaque (Vector/Dict/Set/enum) → absent
    }
    _ => false,                                               // Var/Error/Tuple/… : conservative, no report
};
if definitely_absent { self.error(NoFieldFound{ field, type_ }, expr.span); }
self.types.error_id
```
Plus a small free-fn `is_field_deref_wrapper(name)` mirroring the existing name-based wrapper recognition (`unify`'s `Mutex`/`Shared`/`RWLock`, `:1061`; `Box` deref, `:2944`). The wrapper carve-out preserves `Box[Point].x` (which today *also* rides the `error_id` hole — see §1c). Deref-aware rejection of a *bogus* field on a wrapper (`box.nonexistent`) is intentionally left as the documented **Strategy 2B follow-up**.

Design is grounded in `docs/language-design.md` (static typing / "reject the invalid program"), the CLAUDE.md Core-#8 reference-grade gate, and the existing `Deref`-non-Box precedent.

### 3b. Confirmation — MEASURED (fixed `./target/release/gg`, rebuilt in 29s)
```
minimal repro  count(v.value)   →  error[E_NoFieldFound]: no field `value` found on type `Vector[Inner]`   (1 error)  ✓ now REJECTED
String.foo / int.foo            →  error[E_NoFieldFound]                                                     ✓ REJECTED
struct.nonexistent_field        →  1 error (single report, no double-report)                                ✓
Box[Point].x                    →  OK: no semantic errors                                                    ✓ preserved
shared Point.x                  →  OK: no semantic errors                                                    ✓ preserved
```
Diagnostics are reference-grade: `` no field `value` found on type `Vector[CallArg]` ``.

### 3c. Full CallArg-proto driver
- With the fix, `gg check` on the **unmodified proto patch** reports **exactly 7** `` no field `value` found on type `Vector[CallArg]` `` errors — the silent miscompile is now 7 precise typecheck errors, matching the 7 cc errors 1:1. (Proof the fix catches the real class.)
- Those 7 are genuine **patch defects**: the mechanical `.value`-insertion over-applied `.value` to `Vector[CallArg]`-typed args that should use the existing `callarg_values(...)` helper (`ast.gg:46`). Sites (all `lower_expr.gg`): `3963`/`3979`/`3998` (`margs.value` → `try_lower_{vector,dict,set}_hof`), `5946`/`5956`/`5964` (`spawn_args.value`), `6017` (`sb_args.value`).
- After correcting those 7 sites (`X.value` → `callarg_values(X)`), the driver `gg check`s clean (0 errors) and **`gg build` succeeds — `Built: …/driver`, EXIT=0, zero `GorgetArray from int32_t` errors**. So the two-part story holds: compiler *rejects* the bad program; the corrected program *compiles*.

### 3d. Baseline unaffected — MEASURED
- **`cargo test --lib`: `1107 passed; 0 failed`.**
- **Baseline `self_host_lowerer/driver.gg` (patch reverted) builds CLEAN** with the fixed compiler: `Built: …/driver`, EXIT=0, 0 cc errors. No false-rejects across the entire ~40-file self-host frontend — the strongest available signal that the carve-outs are complete.
- Targeted integration slice (`field_ struct_ box_ generic_ enum_`): **167 passed; 0 failed; 3 ignored** (23s).
- `self_host_runtime` + `self_host_runtime_diff` (baseline fixtures, fixed compiler — builds AND runs the driver, output diffed): **2 passed; 0 failed** (60s). The self-host frontend still builds and produces byte-correct output.

---

## Section 4 — Size / risk / regression test

**Size:** ~74 lines, one file (`src/semantic/typecheck.rs`), no new IR/backend surface. LOW.

**Risk:** LOW–MEDIUM. The one real risk is a *false-reject* — a legitimate field access whose type genuinely resolves late through a mechanism other than the carved-out wrappers. Mitigations already measured: `cargo test --lib` 1107/0; the baseline self-host (huge real corpus, incl. Box/shared/guard usage) builds clean. Residual watch-items for the executor's full sweep:
- Any auto-deref wrapper not in `is_field_deref_wrapper` would false-reject — verify the set against `src/lir/types.rs` + `lib/std/sync.gg` (I included `Box`, `Shared`, `Mutex`, `RWLock`, `Weak`, `ReadGuard`, `WriteGuard`, `Guard`).
- Enum values reaching field access now reject (they have no `struct_fields` entry) — correct, but confirm no fixture reads a field off an enum by relying on the old silent `error_id`.
- Run the **full** `cargo test --test integration` + `self_host_bootstrap_fixed_point` + `GG_BACKEND=llvm` sweep before landing (parent's job).

**Regression test recommendation (Core #6 — turn the bug into a guard):**
- Persist the minimal repro as a **negative fixture** that must be *rejected*: `patches/fieldaccess-fieldless-repro.gg` is saved. Wire an integration/`gg check` negative test asserting `E_NoFieldFound` on `Vector[T].value`, plus positive guards that `Box[T].field` and `shared T.field` still pass (the over-reject hole).
- Add a self-host mirror if/when the self-host typechecker (`self_host_typechecker/typecheck.gg`) grows the same field-existence check for builtin generics/primitives (it currently mirrors the struct-only check).

---

## Section 5 — Owner design questions

1. **Scope of the reject now.** The prototype rejects bogus field access on **primitives + builtin generics + missing struct fields**, and *carves out* smart-pointer/guard wrappers (`Box`/guards/`Shared`) where a bogus field (`box.nonexistent`) still slips through (the documented Strategy-2B deref-aware follow-up). Ship the targeted fix now and file the wrapper deref-awareness as a follow-up (recommended — unblocks CallArg immediately, LOW risk), or hold for the full deref-aware version (larger, needs Box/guard inner-type resolution at typecheck)?
2. **`Tuple`/`Array`/`Slice` named-field access** (`t.foo` on a tuple) is currently left as `_ => false` (not rejected) to keep the prototype's blast radius minimal — tuples use `.0`/`.1` (a separate `TupleFieldAccess` node). Extend the reject to these too (clearly correct, marginally larger), or leave for the follow-up?
3. **The prior scout's brief is wrong about the locus.** It names `coalesce_assign_exact` as the root cause and lists "fix the backend coalescing bug" as the executor's blocking prerequisite. This scout supersedes that: the fix is in the **typechecker**, and the CallArg proto patch additionally has 7 genuine `.value`-on-`Vector[CallArg]` defects to correct (§3c). Confirm the CallArg executor brief should be updated to (a) land this typechecker fix and (b) fix the 7 patch sites with `callarg_values(...)`.

---

### Command log (worktree, `--release`)
```
git apply patches/callarg-normalization-proto.patch
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg   # → 7 cc "GorgetArray from int32_t"
./target/release/gg run  patches/fieldaccess-fieldless-repro.gg                                   # → same error in a tiny main
# --- apply fix to src/semantic/typecheck.rs, cargo build --release (29s) ---
./target/release/gg check patches/fieldaccess-fieldless-repro.gg   # → E_NoFieldFound (rejected) ✓
cargo test --lib                                                    # → 1107 passed; 0 failed ✓
git checkout -- tests/fixtures/ ; ./target/release/gg build .../driver.gg   # baseline → Built, EXIT=0, 0 cc errors ✓
git apply patches/callarg-normalization-proto.patch ; ./target/release/gg check .../driver.gg     # → exactly 7 E_NoFieldFound ✓
perl -i -pe 's/\bmargs\.value\b/callarg_values(margs)/g; ...'      # fix 7 sites
./target/release/gg build .../driver.gg                            # → Built, EXIT=0, 0 cc errors ✓
```
