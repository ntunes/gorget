# Follow the value: a construct from source to machine code

*Verified against commit `7d3350a0` (branch `worktree-agent-a3fe15a62bbab46c4`).*

Every other chapter in this book cuts the compiler **horizontally** — one
subsystem at a time, frontend to backend. This chapter cuts it **vertically**:
it picks one tiny program, fixes on one construct inside it, and walks that
construct through *every* layer the pipeline drags it through, naming the exact
function and `file:line` that touches it at each hop. The point is to see the
whole pipeline at once, end to end, so the per-subsystem chapters have a spine
to hang off of.

The construct is a collection literal bound to a typed local:

```gorget
void main():
    Vector[int] v = [1, 2, 3]
    print(v.len())
```

We follow the literal `[1, 2, 3]` (and the local `v` it binds to). It is the
right choice for a spine because it is *small* — three integers and a bracket —
yet it exercises the load-bearing machinery the whole language is built on: the
**copy-on-write consuming-position contract** (CLAUDE.md → "Ownership at
Consuming Positions"), which decides per element whether the value is *moved*
or *cloned* into the collection. That decision is the heart of Gorget's "no
lifetimes" ownership model, and you can watch it being made in a single
lowering function.

> **How to read along.** Each hop below cites the handler that processes
> `[1, 2, 3]` at that stage. You can reproduce the intermediate forms yourself:
> `gg parse f.gg` dumps the AST, `gg build --emit-gir f.gg` dumps GIR,
> `gg build --emit-lir f.gg` dumps LIR, and `gg build --emit-c-lir f.gg` dumps
> the generated C (`src/main.rs:560,581,637`; see
> [Chapter 1](01-pipeline-and-driver.md)). The `file:line`s here are the *code
> that produces* those dumps, re-derived per the freshness rule
> ([Chapter 0](00-how-to-read.md)).

---

## Hop 0 — Source bytes → tokens (the lexer)

The Logos-based lexer (`src/lexer/`, [Chapter 3](03-lexer.md)) turns the line
`    Vector[int] v = [1, 2, 3]` into a stream of `Spanned<Token>`. For our
literal the relevant tokens are `[`, three integers, two commas, and `]`:

- `[` matches `#[token("[")] LBracket` (`src/lexer/token.rs:103-104`) at the raw
  level, then maps to `Token::LBracket` (`src/lexer/mod.rs:438`).
- Each integer `1` / `2` / `3` matches the integer regex `#[regex(r"[0-9][0-9_]*")]
  IntLiteral` (`src/lexer/token.rs:23-24`), and the lexer parses the slice into
  a value: `Ok(n) => Token::IntLiteral(n)` (`src/lexer/mod.rs:376`, via
  `parse_int_literal` dispatched at `:390`).

Note the integer's *value* is decoded here, at the lexer, not deferred — the
token carries the `i64`. The lexer is also where indentation becomes structure:
the four-space indent under `void main():` is tracked into the block grammar
(Chapter 3), which is why the parser never sees raw whitespace.

## Hop 1 — Tokens → AST (the parser)

The recursive-descent + Pratt parser (`src/parser/`,
[Chapter 4](04-parser-ast.md)) consumes those tokens. Entry is
`Parser::new(src).parse_module()` (`src/parser/mod.rs:60,464`); our literal is
parsed by the prefix handler for `[`:

- `parse_array_or_comprehension` (`src/parser/expr.rs:1580`) skips the `[`,
  parses the first element, checks for a `for` (comprehension) — there is none —
  then loops on commas collecting the rest, and finally produces
  `Expr::ArrayLiteral(items)` (`src/parser/expr.rs:1612`). The AST node is
  `Expr::ArrayLiteral(Vec<Spanned<Expr>>)` (`src/parser/ast.rs:667`).
- Each integer element is parsed by the literal prefix arm:
  `Token::IntLiteral(n) => Expr::IntLiteral(n)` (`src/parser/expr.rs:387-389`).

One subtlety the parser does **not** resolve, and the comments in the lowerer
later lean on: `Expr::ArrayLiteral` is the AST node for *both* `[a, b, c]` and
the set literal `{a, b, c}` — "semantic analysis distinguishes set vs array by
context" (`src/parser/expr.rs:1735`, and the duplicate construction at `:1745`).
The bracket-vs-brace distinction is deliberately *not* carried as a separate AST
node; it is recovered downstream from the declared type. Keep that in mind for
Hop 5.

The whole `Vector[int] v = …` line parses to a `Stmt::VarDecl` whose initializer
is this `ArrayLiteral`; the declared type `Vector[int]` is parsed as a type
annotation on the statement.

## Hop 2 — Name resolution (Pass 1–2)

Resolution (`src/semantic/resolve.rs`, [Chapter 7](07-name-resolution.md))
binds every identifier to a `DefId`. For our literal there is *nothing to
resolve* — integer literals carry no names — so the `ArrayLiteral` arm simply
recurses into its elements:

```rust
Expr::ArrayLiteral(elements) | Expr::TupleLiteral(elements) => {
    for elem in elements { resolve_expr(elem, …); }
}
```

(`src/semantic/resolve.rs:1635-1639`). The interesting resolution on this line
happens on the *binding*: the `Stmt::VarDecl` arm (`src/semantic/resolve.rs:1018`)
introduces `v` into the current scope so the later `v.len()` resolves to it. The
literal itself passes through untouched — a good illustration of "lossless on
invariants, lossy on syntax" ([Chapter 24](24-layering-discipline.md)): the
resolver adds binding information and leaves the literal's shape alone.

## Hop 3 — Type inference & checking (Pass 4)

Typecheck (`src/semantic/typecheck.rs`, [Chapter 9](09-type-checking.md)) is
where `[1, 2, 3]` acquires a type. The `ArrayLiteral` arm:

```rust
Expr::ArrayLiteral(elements) => {
    if elements.is_empty() { return self.types.error_id; }
    let first_type = self.infer_expr(&elements[0]);
    for elem in &elements[1..] {
        let et = self.infer_expr(elem);
        self.unify(first_type, et, elem.span);
    }
    self.types.insert(ResolvedType::Array(first_type, elements.len()))
}
```

(`src/semantic/typecheck.rs:2483-2493`). The first element types as `int`, the
other two are unified against it (a heterogeneous `[1, "x"]` would fail here),
and the literal is recorded as `ResolvedType::Array(int, 3)`. Separately, the
var-decl checker unifies that array type against the *declared* `Vector[int]`;
this is the point at which the literal is known to be filling a `Vector`, not a
fixed-size array or a `Set` — the fact Hop 5 reads back out. (The sibling
`TupleLiteral` arm at `:2495` shows how a declared `decl_type_hint` is threaded
per-element; the array arm relies on the surrounding var-decl's expected type
rather than per-element hints.)

## Hop 4 — The frontend boundary

That is the end of the AST-level pipeline. The full Pass 0–5 chain
(`analyze_with_source_dir`, `src/semantic/mod.rs:96`; the table is in
[Chapter 1](01-pipeline-and-driver.md)) has now run: meta/derive, resolution,
traits, typecheck, and the safety/borrow check (Pass 5,
[Chapter 10](10-ownership-safety.md)). The safety checker is where the
*consuming-position* policy is *validated* — it would reject a move of a value
that is still live past this point — but the policy is *applied* (turned into
move-vs-clone instructions) in the next hop, GIR lowering. The product handed
forward is the type-checked AST plus an `AnalysisResult`
(`src/semantic/mod.rs:43`).

## Hop 5 — GIR lowering: the literal becomes calls (and the CoW contract fires)

This is the hop that earns the example. GIR lowering (`src/ir/lowering/`,
[Chapter 12](12-gir-lowering.md)) lowers `Expr::ArrayLiteral` in
`lower_array_literal` (`src/ir/lowering/exprs/collections.rs:13`). Its
one-line docstring *is* the lowering:

> *"Lower `[e1, e2, ...]` to `gorget_array_new(sizeof(elem))` + N
> `gorget_array_push` calls."* (`collections.rs:12`)

Walk it in order:

1. **Set-vs-array disambiguation.** First it reads the surrounding declared type
   (`ctx.func_state.expected_type`) and asks `collection_kind` whether the
   target is a `Set`/`OrderedSet`; if so it diverts to
   `lower_set_literal_from_array` (`collections.rs:27-35`). This is exactly the
   "context decides" the parser deferred at Hop 1 — and it reads a *typed*
   `collection_kind`, not the `[` vs `{` syntax (the syntax is gone). Our target
   is `Vector[int]` → `CollectionKind::Array`, so we fall through.

2. **Create the buffer.** `gorget_array_new` is emitted with the element size as
   a `Constant::SizeOf(etype)` operand (`collections.rs:70-74`). The element type
   `etype` is inferred from the first lowered element (`collections.rs:66-68`).
   The fresh array local is then tagged `Owned` (`ctx.set_owned`,
   `collections.rs:79`) and registered for drop (`collections.rs:80`) — the
   literal *owns* a fresh allocation, and that ownership fact is written onto the
   `Local` so downstream sinks (a `return`, a struct field init) don't
   clone-then-leak it (the comment at `collections.rs:75-78` spells out the leak
   this prevents).

3. **Push each element — and here the CoW contract is made concrete.** For each
   element the lowerer computes an `elem_mode` (`collections.rs:88-106`):
   `Copy` for non-resource types (primitives like our `int`), and for resource
   types, `Move` when the source is owned-and-dead at this site, else `Copy`.
   This is the consuming-position decision table from CLAUDE.md, in code: *"Owns
   AND dead at this call → move; Borrow, or owned-but-live → clone."* For
   `[1, 2, 3]` the elements are `int` primitives, so each takes the `Copy`
   branch — there is nothing to move or clone, and no `MoveZero` is emitted. But
   the *same function* handles `Vector[String] xs = [a, b]`, and there the
   `Move` branch fires `move_zero_and_mark` (`collections.rs:118-127,142-150`) so
   the source slot is zeroed and the scope-exit drop doesn't double-free a buffer
   the array now owns. Each element is stored into a temp, borrowed, and the temp
   reference + a mutable borrow of the array are passed to `gorget_array_push`
   (`collections.rs:128-134` for the first element, `:152-158` for the rest).

So the GIR for `[1, 2, 3]` is one `gorget_array_new` `CallExtern` and three
`gorget_array_push` `CallExtern`s, with the array carried as an `Owned` local.
The instruction vocabulary (`Instruction`, `CallExtern`, `Borrow`/`BorrowMut`,
the per-read `AssignMode`) is described in [Chapter 12](12-gir-lowering.md); the
ownership fields on the `Local` (`LocalOwnership::Owned`, `SlotKind`) in
[Chapter 13](13-ownership-in-ir.md). The binding `v` is a `Stmt::VarDecl`,
lowered by `lower_var_decl` (`src/ir/lowering/stmts/mod.rs:372`), which binds the
array operand into `v`'s slot (the assign-mode tree is `lower_var_decl_assign_mode`,
`stmts/mod.rs:1081`).

> **Why this is the right spine.** Nothing about `[1, 2, 3]` *looks* like it
> touches ownership — it is three integers. But the lowering function that
> handles it is identical to the one that handles `[someString, otherString]`,
> and reading it you see the entire move-vs-clone machine in one place. That is
> the lesson: the consuming-position contract is not a special case bolted onto
> "hard" types; it is the default path every collection literal takes, and
> primitives just happen to take its cheapest branch.

## Hop 6 — GIR → LIR & SSA

LIR lowering (`src/lir/lower/mod.rs`, [Chapter 14](14-lir-ssa.md)) consumes the GIR
and produces SSA-form `LirModule`. The four `CallExtern` calls become LIR
`Inst::CallExtern { dst, name, args, arg_abis }` (`src/lir/mod.rs:856-861`),
emitted by the GIR→LIR instruction lowering in `src/lir/lower/insts.rs` (the
`CallExtern` push sites at `insts.rs:85,93,…`). Two things are added at this
boundary, both *typed metadata written through* so the backend never re-derives
them:

- **SSA.** Each call's result is a fresh `ValueId`; `construct_ssa` (per
  function, driven from `lower_module`, see [Chapter 1](01-pipeline-and-driver.md))
  builds the def/use graph and inserts phis at merges. Our straight-line `main`
  has no merges, so the array value flows linearly from `gorget_array_new`'s
  `dst` through the three pushes into `v`.
- **ABI tags.** Each argument gets an `AbiKind` in `arg_abis`
  (`src/lir/mod.rs:860`), resolved from the runtime function's signature by
  `RuntimeFn::resolve_lir_sig` (`src/lir/runtime.rs:611`). This is what tells the
  backend that the array self-argument of `gorget_array_push` is passed *by
  pointer* and the element is passed *by pointer* — facts the backend reads off
  the tag rather than guessing from the name.

## Hop 7 — BIR

Between LIR and the backend sits BIR (`src/bir/`, [Chapter 16](16-bir.md)), a
newtype over `LirModule` whose constructor `BirModule::from_lir`
(`src/bir/mod.rs:77`) expands the ten *canonical* high-level LIR ops into
primitives and then asserts none survive. Our construct is unaffected at the op
level — `gorget_array_new`/`gorget_array_push` are already plain `CallExtern`s,
not canonical ops like `CollectionCtor` or `CowClone`. (Had we written
`v.map(...)`, *that* would be a `HofExpand` canonical op expanded here.) BIR's
relevance to our trace is the *guarantee* it provides: the backend takes a
`&BirModule` and so is structurally incapable of seeing an un-expanded op — the
literal arrives at codegen as primitives only.

## Hop 8 — LIR → C (the backend)

The C backend (`src/backend/c_lir/`, [Chapter 17](17-c-backend.md)) is a
deliberately *dumb* 1:1 translator — "no semantic decisions … all type
coercions, drop calls, vtable dispatch are already explicit in LIR"
(`src/backend/c_lir/mod.rs:1-5`). It walks the BIR and emits one `.c`. Our four
calls go through the `Inst::CallExtern` arm, `emit_call_extern`
(`src/backend/c_lir/emit_call_extern.rs:6`), which spells the runtime symbol
(`gorget_array_new`, `gorget_array_push`) and marshals each argument according
to its `arg_abis` tag — reading `arg_abis.get(i)` and emitting `&` for
by-pointer args (`emit_call_extern.rs:704-714`). This is the one sanctioned
place name-spelling is allowed: at the C-emit boundary the runtime symbol name
*is* the contract with the runtime (CLAUDE.md → "No name matching", the
exception clause).

The emitted C calls land on the runtime functions defined in
`src/backend/c/c_runtime.rs`:

```c
static inline GorgetArray gorget_array_new(size_t elem_size) { … }      // c_runtime.rs:5233
static inline void gorget_array_push(GorgetArray* arr, const void* elem) { … } // c_runtime.rs:5244
```

So the generated C for `Vector[int] v = [1, 2, 3]` is, in essence: allocate a
`GorgetArray` sized for `int`, then push pointers to three `int` temporaries
into it, then bind it to `v`. The signatures confirm the lowering's call shape
exactly — `gorget_array_new` takes the `sizeof(int)` the lowerer emitted as
`Constant::SizeOf`, and `gorget_array_push` takes the array *by pointer* and the
element *by pointer*, matching the `BorrowMut`/`Borrow` the lowerer produced at
Hop 5. The runtime, ABI, and drop contract those calls obey are
[Chapter 18](18-runtime-abi.md).

## The whole trace on one page

| # | Stage | Construct's form | Handler (`file:line`) |
|---|-------|------------------|------------------------|
| 0 | Lexer | `[`, `IntLiteral(1)`, … | `src/lexer/token.rs:23,103`; `src/lexer/mod.rs:376,438` |
| 1 | Parser | `Expr::ArrayLiteral([IntLiteral …])` | `src/parser/expr.rs:1580,1612`; `:387`; `src/parser/ast.rs:667` |
| 2 | Resolve | (recurse into elements; bind `v`) | `src/semantic/resolve.rs:1635`; `:1018` |
| 3 | Typecheck | `ResolvedType::Array(int, 3)`, unified vs declared `Vector[int]` | `src/semantic/typecheck.rs:2483` |
| 4 | Safety | move/clone *validated* | `src/semantic/safety/`; entry `src/semantic/mod.rs:337` |
| 5 | GIR | `gorget_array_new` + 3× `gorget_array_push`; array `Owned`; per-elem move/clone | `src/ir/lowering/exprs/collections.rs:13` |
| 6 | LIR/SSA | 4× `Inst::CallExtern` w/ `arg_abis`, SSA `ValueId`s | `src/lir/lower/insts.rs:85`; `src/lir/mod.rs:856`; `src/lir/runtime.rs:611` |
| 7 | BIR | unchanged (already primitive) | `src/bir/mod.rs:77` |
| 8 | C backend | `gorget_array_new(...)` / `gorget_array_push(&arr, &e)` | `src/backend/c_lir/emit_call_extern.rs:6`; runtime `src/backend/c/runtime/runtime_array.c:4`, `:15` |

## What the trace teaches

Three things are worth carrying away from this one walk:

1. **Abstractions evaporate, invariants accumulate.** The set-vs-array
   distinction (a syntactic `[` vs `{`) is *resolved* by typecheck and *consumed*
   at Hop 5 from a typed `collection_kind`, never re-read from syntax. The
   *ownership* of the literal's buffer, by contrast, is *born* at Hop 5 and
   carried forward as a typed `Owned` fact on the `Local` all the way to the
   backend's drop emission. This is layering discipline made visible
   ([Chapter 24](24-layering-discipline.md)).

2. **The consuming-position contract is the common path, not a corner case.**
   The move-vs-clone decision lives in `lower_array_literal`'s `elem_mode`
   closure and runs for *every* element of *every* collection literal. Our
   `int`s take its cheapest branch (`Copy`, no `MoveZero`); a `String` element
   would take the `Move`-or-clone branch. Same code, same contract.

3. **Each hop adds, none reconstructs.** No stage downstream of the parser
   re-derives "this is an array literal" from a name or shape; each reads the
   typed fact the previous stage wrote (`collection_kind`, `LocalOwnership`,
   `arg_abis`). When you go to *fix* something on a path like this, the place to
   fix is the **writer** of the fact the buggy reader consumed — which is exactly
   the heuristic the [contributor playbook](29-contributor-playbook.md) makes
   into a discipline.

## Two more constructs, in brief

The same vertical cut is worth doing for two other constructs that stress
different machinery; each is a one-paragraph sketch here, with the per-subsystem
chapter as the deep dive.

- **An f-string with interpolation** (`f"x = {v.len()}"`) is the cross-cutting
  case: the lexer's `lex_scan_string` carries the interpolation segments
  (Chapter 3), the parser sub-parses each `{…}` into a real expression, the
  resolver and typecheck handle the embedded `v.len()` like any other call, and
  GIR lowering desugars the whole thing into runtime string-concatenation calls.
  Its parity quirk drives several self-host mismatches — the self-host parser
  pre-parses interpolation segments differently from Rust (see `MEMORY.md`'s
  resolver-gap note and [Chapter 26](26-self-host-frontend.md)).

- **A `match` on an enum** exercises the `EnumInit`/`EnumCheck`/`EnumExtract`
  canonical LIR ops — the very ops that Hop 7 (BIR) expands into primitives. A
  `match` lowers to a `Switch` terminator (`src/ir/instructions.rs:384`) over the
  enum's discriminant plus `EnumExtract`s for the bound payloads; BIR then
  expands those to tag-loads and field-loads ([Chapter 16](16-bir.md)). Where our
  array literal sailed through BIR untouched, a `match` is the construct that
  makes the BIR expansion earn its keep.

---

*Walkthrough chapter. Verified against `7d3350a0`. Every `file:line` traces the
handler that processes `[1, 2, 3]` (or `v`) at that stage; re-derive any that
have drifted via the `--emit-{gir,lir,c-lir}` dumps named at the top.*
