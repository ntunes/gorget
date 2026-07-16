# A32 — HOF effect polymorphism (throws/Result): scout + design draft

**Status:** ✅ DESIGN RATIFIED 2026-07-16 — owner accepted forks **A1 B1 C1 D1 E1 F1 G1**.  
**Normative:** `docs/plans/define-gorget/decisions.md` LOG (“A32 BASIC DESIGN RATIFIED”).  
**⚠ A1 REFINED same day — the A1×E1 COMPOSITION PIN (ratified; LOG + evidence
`scout-a32-a1xe1-composition.md`):** inferred rethrows fires only through opt-in latent
callable params spelled `U(T)!` (A31's bare-`!` at the type position); unannotated function
types are concretely infallible EVERYWHERE (E1 uniform); HOF bodies mark latent invocations
`f(x)!`; pre-D27 param-position spelling is `Callable[U(T)!]` only; Result/Option combinator
callbacks stay unmarked per D1. Read this file's §3.2/§3.3 declaration-model text through
that pin — "HOF decls stay success-typed" means success-typed WITH the `!` latency marker on
the callback param, not bare `U(T)`.  
This file remains evidence + worked examples; do not re-litigate ratified forks.  
**Date:** 2026-07-16  
**Scope:** Basic syntax + semantics for “HOF fallible iff callback fallible.”  
**Out of scope:** Implementation; full `throws × async` effect algebra; D29 implementation.

Related: D23 (throws totality), D29 (fallible-use mark `!`, always-mark, Result-returning calls),
A31 (inferred error sets), D17 (stdlib fallibility).

---

## 0. TL;DR

**Rule (agreed in spirit):** a higher-order function is fallible exactly when a
**function argument it invokes** is fallible.

**Today (measured):** Gorget has **no** HOF effect polymorphism.

- Function types carry **no** throws bit (`ResolvedType::Function` = params + return only).
- Closures **clear** `current_function_throws` — `throw` / prop inside a closure is
  `ThrowInNonThrowingFunction` (Snag #44); “throws on closures not yet supported.”
- `infer_closure_method_type` only reads the callback’s **success return type** `U`
  (`extract_fn_return_type`); it never makes `map`/`filter`/`fold` throw.
- Builtin HOF rows (`Vector.map`, etc.) have no throws metadata.

**Design-now goal:** pin syntax + semantics so D29, A31, callable types, and D17
combinators don’t calcify dual APIs (`try_map`) or wrong function types.

**Implement-later:** typechecker + mono after D29 call-sites (separate track).

---

## 1. Scout — current substrate (`file:line`)

### 1.1 Function types have no effect

`src/semantic/types.rs:32-37` — `ResolvedType::Function { params, param_ownerships, return_type }`  
No `throws` / error-type field. AST function types resolve the same (`types.rs:498-512`).

`FunctionInfo` on **named** functions does carry throws (`resolve.rs:65-75`: `throws: bool`,
`throws_type_id: Option<TypeId>`). That metadata is **not** part of first-class function
values / closure types used as HOF arguments.

### 1.2 Closures are non-throwing frames

`typecheck.rs:3348-3374` (Snag #44): entering a closure body saves/restores
`current_function_throws` and forces it to **false** (and clears `current_fn_throws_type_id`).
Comment states explicit `throws` on closures is **not yet supported**.

Consequence: a callback cannot legally call `f()!` / `throw` today even if the enclosing
function is `throws E`. A32 requires **throwing callables** as a prerequisite surface.

### 1.3 HOF return typing ignores fallibility

`typecheck.rs:6254+` — `infer_closure_method_type`:

| Receiver.method | Callback shape assumed | HOF result type |
|---|---|---|
| `Vector.map` | `(T) -> U` | `Vector[U]` |
| `Vector.filter` | `(T) -> bool` | `Vector[T]` |
| `Vector.fold` | init + `(U,T) -> U` | `U` |
| `Option.map` | `(T) -> U` | `Option[U]` |
| `Result.map` | `(T) -> U` | `Result[U,E]` (E from receiver) |
| `Result.and_then` | `(T) -> Result[U,E]` | return of callback |
| Iterator `map`/`filter`/`fold`/`collect` | via `try_iterator_adapter_type` | adapter types |

**No path** sets “this method call throws E because the closure throws E.”

### 1.4 Builtin protocol

`src/ir/lowering/builtins.rs` — Vector/Dict/Set/Option/Result HOF methods registered with
`runtime_callee: None` (lowered specially); signatures are success-typed only.

### 1.5 Corpus pressure

- In-repo **throws decls** concentrated in fixtures (D29 census ~179); **lib/self-host: 0**.
- No fixture found that passes a `throws` callback into `map`/`filter` (would be rejected
  or mis-modeled today).
- D17 will create real pressure: `paths.map(read_file)`-shaped code once fs is fallible.

### 1.6 D29 interaction (ratified, not implemented)

- Mandatory `!` on fallible Call/MethodCall (throws **or** declared-`Result` return).
- Always-mark + disposition on marked expr.
- HOF call sites must eventually participate: `xs.map(f)!` when the instantiation is fallible.

---

## 2. Worked examples (target semantics)

Notation: post-D29 marks shown. Closures may need a throws spelling (Fork C).

### 2.1 Infallible callback — HOF non-fallible

```gorget
Vector[int] ys = xs.map((int x): x + 1)    # no ! on map
```

### 2.2 Throws callback — HOF fallible

```gorget
int parse_one(String s) throws ParseError: ...

Vector[int] ys = xs.map((String s) throws ParseError: parse_one(s)!)!
#                         ^^^^^^^^^^^^^^^^ callback fallible
#                                                              ^ mark on map (fallible HOF use)
```

Inside a monomorphized/checked `map` body (conceptual):

```gorget
# compiler-internal / elaborated
U u = f(elem)!     # callback invocation is a fallible call
```

### 2.3 Result-returning callback — same rule (D29 item 3)

```gorget
Result[int, ParseError] parse_r(String s): ...

Vector[int] ys = xs.map((String s): parse_r(s)!)!
# map peels to Vector[int], error ParseError — fallible HOF
```

### 2.4 Nested HOFs

```gorget
zs = xss.map((Vector[String] row) throws E: row.map(parse_one)!)!
# outer map fallible because body calls fallible inner map
```

### 2.5 Multi-callback (`fold`)

```gorget
int acc = xs.fold(0, (int a, int x) throws E: combine(a, x)!)!
```

If only one of two callbacks were fallible (unusual API), effect = union of **invoked**
function-args’ effects (Fork B for typing of E).

### 2.6 User-defined HOF

```gorget
U apply[T, U](T x, U(T) f): f(x)    # today: no throws on f(x)

# target: if f is fallible, apply is fallible; call site:
y = apply(x, parse_one)! 
```

Same rule for free functions and methods — not only builtins.

### 2.7 Explicit handling at HOF boundary

```gorget
Vector[int] ys = xs.map(parse_one)! catch (ParseError e):
    return default_vec()
```

---

## 3. Ratified model (forks A1–G1)

### 3.1 Core rule

> When typechecking a call `hof(..., f, ...)` (or method), if the callee **invokes** a
> parameter `f` that has function type with error effect `E` (throws E or returns
> `Result[_, E]`), then the **hof call** has error effect `E` (or the join of such
> effects — Fork B). Otherwise the hof call is non-fallible.
>
> D23/D29 apply: the hof call is typed as its success type `T` in every position;
> use requires `!` and a disposition when fallible.

### 3.2 Declaration model — **default R1: inferred rethrows**

HOF **source** declarations stay success-typed; effect is **computed** from arguments
at each call (Swift `rethrows` style, without a keyword if inference is enough):

```gorget
# Stdlib / user — no throws on map itself
Vector[U] map[T, U](self, U(T) f):
    ...
```

At a call site, if `f` is fallible with `E`, then `map` is treated as `throws E` for
that instantiation only.

**Alternative R2:** explicit effect parameters in the type system / syntax
(`effect E`, Midori). More power, more surface — see Fork A.

### 3.3 Callable / function types must carry effect (prerequisite)

Without this, inference has nothing to read.

**Proposed elaboration (not necessarily user syntax):**

```text
Function { params, return_type, error: None | Some(E) }
```

- `error: None` — infallible callable  
- `error: Some(E)` — throws E **or** returns Result[T,E] peeled at call (same fallible use)

User-facing spellings (Fork C):

| Callable kind | Possible spelling |
|---|---|
| Named fn | already `throws E` / `Result[T,E]` return |
| Closure | `(T x) throws E: ...` or inferred from body |
| Function type | `int(String) throws ParseError` or `Callable[...]` form |

### 3.4 D29 mark placement

| Site | Mark? |
|---|---|
| Infallible `xs.map(f)` | no |
| Fallible `xs.map(f)` | `xs.map(f)!` |
| Callback body calling fallible work | `g(x)!` inside callback (callback must be throws frame) |
| Result capture of HOF | `Result[Vector[U],E] r = xs.map(f)!` |

### 3.5 Result.map vs Vector.map (subtle)

`Result[T,E].map((T)->U)` today transforms **success**; it does **not** mean the callback
throws. Under A32:

- If callback is infallible: keep today’s `Result[U,E]` (receiver’s E).  
- If callback is fallible with `E2`: need a rule (Fork D):  
  - **D-join:** error becomes join(E, E2) / require E==E2, or  
  - **D-reject:** Result combinators stay data-only; only “collection HOFs” rethrow  

**Default lean:** collection/iterator HOFs rethrow; **Result/Option combinators** that
already encode failure in the **receiver** stay data-plane unless callback is fallible,
in which case **require same E** or reject until A31 union (simplest v1: same E only).

### 3.6 Non-goals (v1 design)

- Full `async` effect polymorphism (forward-compat sentence only).  
- Permanent `try_map` / `try_filter` dual stdlib.  
- Faults as effects (D24/D25).  
- Deep nested-Result sugar.  
- Implementing A31 unions (may **reference** A31 for multi-E later).

### 3.7 Doctrine

- **No permanent try_map.** Interim: loops or Result-as-data.  
- **D29 impl does not include A32 impl.**  
- **D17 fallible combinators blocked on A32 impl**; `read_file` etc. not blocked.  
- Async: “same polymorphism should later apply to async callables; v1 = error effect only.”

---

## 4. OWNER FORKS — ✅ RATIFIED A1 B1 C1 D1 E1 F1 G1 (2026-07-16)

Historical options kept for the derivation record; **bold = chosen.**

### Fork A — Declaration model → **A1**

| Option | Meaning |
|---|---|
| **A1 ✅** | **Inferred rethrows:** HOF decls stay without `throws`; effect from callback at each call |
| A2 | Explicit effect param syntax in surface language |
| A3 | Keyword `rethrows` on HOF (Swift) — middle ground |

### Fork B — Multiple fallible callbacks / error type join → **B1**

| Option | Meaning |
|---|---|
| **B1 ✅** | All invoked fallible callbacks must share the **same** `E`; else type error with fix-it |
| B2 | A31-style inferred union of error types (later widening) |
| B3 | HOF always erases to a single named error via `From` obligations |

### Fork C — Closure / function-type throws syntax → **C1**

| Option | Meaning |
|---|---|
| **C1 ✅** | `(params) throws E: body` on closures; function types carry effect; `Callable[…]` safe form |
| C2 | Infer closure throws only from body (sugar later) |
| C3 | Only named functions as fallible callbacks — rejected as too weak |

Prerequisite: closures may be throws frames (lift Snag #44 when annotated).

### Fork D — Result/Option combinators vs collection HOFs → **D1**

| Option | Meaning |
|---|---|
| **D1 ✅** | Collections/iter/user HOFs **rethrow**; Result/Option combinators data-plane; same-E if callback fallible |
| D2 | Uniform rethrow on all HOFs including `Result.map` |
| D3 | Only user HOFs rethrow — rejected (forces try_map) |

### Fork E — Throwing callback vs infallible function type → **E1**

| Option | Meaning |
|---|---|
| **E1 ✅** | **Reject** silent coerce of fallible callable → infallible function type |
| E2 | Coerce by wrapping — rejected |

### Fork F — Trait methods / equip defaults → **F1**

| Option | Meaning |
|---|---|
| **F1 ✅** | Same rethrows rule (including defaults) |
| F2 | Traits must always write explicit throws |

### Fork G — Async → **G1**

| Option | Meaning |
|---|---|
| **G1 ✅** | v1 = error effect only; async composition is forward-compat |
| G2 | Design throws×async together now |

---

## 5. Implications if defaults (A1,B1,C1,D1,E1,F1,G1) are ratified

| Area | Impact |
|---|---|
| **D29** | Brief cites A32: `map(f)!` when f fallible; no assumption “methods never throw from callbacks.” D29 **impl** still doesn’t build A32. |
| **A31** | Multi-E HOFs wait on A31 (B2); v1 same-E (B1) unblocks A32 without A31. |
| **Function types** | Must gain an error/effect slot — design couples to D29 callable notes. |
| **Closures** | Must support throws frames (syntax C1) — Snag #44 becomes conditional. |
| **D17** | `read_file` etc. unblocked; **fallible stdlib combinators** wait on A32 **impl**. |
| **Stdlib** | Single `map`/`filter`/`fold`; no `try_*` duals. |
| **ggdef** | Model rethrows on HOF calls when effect polymorphism lands; until then loud reject or subset. |

---

## 6. Implementation sketch (not this pass)

1. Extend function/closure types with optional error type.  
2. Infer closure throws from annotation and/or body (C1/C2).  
3. At HOF call: compute effect from invoked fn-args; attach to call like throws callee.  
4. D29: require `!` on fallible HOF calls.  
5. Body checking of generic HOFs: check under assumed `f` effect (or mono after instantiation).  
6. Fixtures: infallible map; throws callback map; Result callback; reject throwing→infallible coerce; fold; user HOF.  
7. Both compilers + ggdef within subset.

---

## 7. Ratification

**Done 2026-07-16:** owner accepted **A1 B1 C1 D1 E1 F1 G1**.  
Recorded in `decisions.md` LOG (“A32 BASIC DESIGN RATIFIED”).  
Implementation brief only when that track is scheduled (after D29 call-sites; not opened
in the design session).

---

## 8. Open non-fork notes (non-blocking)

- Exact function-type grammar disambiguation in param position (D29 packet: prefer
  `Callable[...]` brackets) — align with C1 when spelling is written into reference.  
- Whether `filter` predicates that throw are allowed (yes under rule; rare).  
- Iterator lazy adapters: effect when? On `map` construction vs `collect` — **lean:**
  effect at the adapter call that registers the callback, and again if terminal ops
  invoke it; may need a sub-scout at impl time for lazy iter. Flag as **impl detail**.  
- Terminology (`throws` vs `fails`) — orthogonal; design uses current keywords.
