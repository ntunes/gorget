# WRONG cells in detail — 13 defects, ranked by how ordinary the code is

(This is the `REPORT.md` deliverable; the filename differs because the harness blocked a file
literally named `REPORT.md`.)

Every repro below was built with `gg build` and **run**; return codes were read from plain
commands. Expected values were hand-derived from the docs before running. Where a string value
is the subject it is built at runtime via `String mk(String a, String b): return a + b`.
LLVM results come from `gg build --backend=llvm`. Garbage values differ per run (they are
uninitialised pointer reads); one observed sample is quoted.

Legend: **NEW** = no matching TODO.md entry or `known_gaps/` fixture found.
**FILED** = already recorded. **COSTUME** = a new face of a filed root.

---

## 1. `match` used as an EXPRESSION ignores guards entirely — NEW, silently wrong

Rank 1: chapter 3, both features taught in the same chapter, wrong for **every** input, no
diagnostic, both backends.

```gorget
void main():
    int score = 85
    String g = match score:
        case s if s >= 90: "A"
        case s if s >= 80: "B"
        else: "F"
    print(g)
```

* Expected `B`. Actual **`A`**. With `score = 10` (no guard true) it still prints `A` not `F`.
* The **identical guards as a match STATEMENT are correct** (`stmt B`). A match *expression*
  with plain literal patterns is also correct (`case 2: "two"` -> `two`). It is specifically
  *guards inside a match expression* that are dropped — the first arm always wins.
* Same on **LLVM** (`LLVM-MISMATCH`).
* Cells: `match_expr_guard`, `match_expr_guard_none_match`, `pair_grade_c_match_expr`.
  Probe: `probe/p18.gg` (five cases, statement vs expression side by side).
* Found by the **spelling-equivalence pair** `pair:grade` — the if/elif and match-statement
  spellings both print `B`, so the divergence names the defect. No single-spelling cell in this
  sweep would have caught it.
* Duplicate check: grepped TODO.md for `match.*expression.*guard`, `guard.*match expr`,
  `match-expression`. Only hit is a ggdef match-exhaustiveness note (~line 1133), unrelated.

---

## 2. `x += coll.get(i).unwrap()` adds the Option's POINTER — NEW, silently wrong

Rank 2: chapter 5, four lines, no diagnostic on C; the LLVM backend proves the mechanism.

```gorget
void main():
    Vector[int] v = [1, 2, 3]
    int total = 0
    total += v.get(0).unwrap()
    print(f"{total}")
```

* Expected `1`. Actual **`187650764399264`**.
* Characterised in `probe/p17.gg` — the axis is the **compound-assign operator**, not the loop:

| spelling | result |
|---|---|
| `a = a + v.get(0).unwrap()` | **1 — correct** |
| `b += v.get(0).unwrap()` | garbage |
| `c -= v.get(0).unwrap()` | garbage |
| `d *= v.get(1).unwrap()` | garbage |
| `e += m.get("k").unwrap()` (Dict receiver) | garbage |
| `f += o.unwrap()` (plain `Option[int]` local) | **7 — correct** |
| `int x = v.get(i).unwrap()` then `total += x` | **correct** |

* **LLVM refuses to compile the same source**, and its message is the diagnosis:
  `'%v67' defined with type 'ptr' but expected 'i64'` inside
  `call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %v77, i64 %v67)` — the Option's pointer is
  being fed to the add. The C backend does the same thing silently.
* Cells: `vec_compound_add_get_unwrap`, `dict_compound_add_get_unwrap`, `pair_sum_c_getunwrap`.
* Found by the pair `pair:sum_ints` — for-in, while+index, `fold` and `iter().sum()` all print 15.
* **Discriminator vs the filed family.** TODO.md has a large `.get().unwrap()` compound-assign
  family (lines ~402, ~1222, ~1225, ~1227), but every entry there is about the get-chain as the
  **LHS write-through target** (`coll.get(i).unwrap().field += v` dropping the write, or
  double-evaluating the base, or UAF-ing on the write-back). This one is the opposite side: the
  get-chain is on the **RHS**, the destination is a plain `int` local, and the defect is a
  **wrong READ**, not a lost write.

---

## 3. `Vector[Struct].sort(cmp)` is a silent no-op on C and invalid IR on LLVM — NEW

Rank 3: chapter 5+6, "sort a list of records by a field" is a top-10 exercise everywhere.

```gorget
struct Person:
    int age

void main():
    Vector[Person] ps = Vector[Person]()
    ps.push(Person(30))
    ps.push(Person(25))
    ps.push(Person(40))
    ps.sort((a, b): a.age - b.age)
    for p in ps:
        print(f"{p.age}")
```

* Expected `25 / 30 / 40`. Actual **`30 / 25 / 40`** — returned untouched. No warning, no error,
  exit 0. `.sorted(cmp)` behaves identically.
* On **LLVM the same source fails to build**: `llc: error: 'ret i64 %v6' defined with type
  'ptr' but expected 'i64'` — invalid IR from the comparator thunk. Two lanes, two faces, one
  root; per Core #8 that is >=2 bugs, not "benign".
* `Vector[int].sort((a, b): b - a)` **is correct** (`5,4,3,1,1`), so the axis is the element type.
* Cells: `sort_structs_by_field`, `vec_sort_structs_comparator`, `vec_sorted_structs_comparator`.
  Probe: `probe/p13.gg`.
* Duplicate check: TODO.md ~571 mentions `SortedBy` / "no `gorget_array_sort_by` runtime", but
  that entry is scoped to the **self-host lane**. This is Rust `gg` on C and LLVM.

---

## 4. Reassigning a function-typed variable is a no-op — NEW, silently wrong

Rank 4: chapter 4, the Book's own example verbatim (`docs/book/04-functions.md`, "Function Types").

```gorget
void main():
    int(int, int) op = (a, b): a + b
    print(f"{op(3, 4)}")
    op = (a, b): a * b
    print(f"{op(3, 4)}")
```

* Expected `7 / 12` (the book prints exactly these). Actual **`7 / 7`** — the second call still
  runs the *first* closure. No diagnostic. Same on **LLVM**.
* Cells: `func_type_value`, `func_closure_reassign`.
* Duplicate check: TODO.md ~1028 mentions "a reassignment (`f = (...)`) MISSES the channel", but
  that is a **self-host** return-type-inference note ("no regression — just unaddressed"), not
  this Rust-lane wrong value.

---

## 5. `auto v = [10, 20, 30]` infers `int[3]`, not `Vector[int]` — NEW, one root, four faces

Rank 5: chapter 5, and it directly contradicts the book.

`docs/book/05-collections.md`, "Arrays vs Vectors":
> Use `auto` with a literal to get a `Vector[T]` (dynamic, supports `push`/`pop`/`len`).
> `auto dynamic = [1, 2, 3]   # Vector[int] — growable`

Proof (`probe/p2.gg`): passing it to a `String` parameter reports
`type mismatch: expected 'String', found 'int[3]'`.

The runtime value really *is* growable — `probe/p3.gg` pushes 50 elements onto `auto v = [1,2,3]`
and gets `len 53`, sum `1231` (correct). So the static type spelling disagrees with the runtime
representation. Four consequences:

**(a) `+` concatenation rejected** — Book Ch5 verbatim (`auto c = a + b`):
`auto a = [1,2,3]; auto b = [4,5]; auto c = a + b`
-> `error[E_TypeMismatch]: expected 'int[3]', found 'int[2]'`.
With `Vector[int]` destinations it is correct (`vec_concat_typed` passes).

**(b) Slicing yields GARBAGE, silently** (`probe/p14.gg`):
`auto v = [1,2,3,4,5]; auto mid = v[1:4]; print(f"{mid}")` -> prints `281474292334296`.
`mid` is typed as the element type `int`, so this **compiles and runs**. With
`Vector[int] v = [...]` the same slice is correct (`vec_slice_typed` passes).

**(c) `index_of` after a bound `contains` returns the wrong answer** (`probe/p6.gg`):
```gorget
auto v = [10, 20, 30]
bool has = v.contains(20)
print(f"{has}")                    # true
match v.index_of(20):
    case Some(i): print(f"{i}")    # prints 0 — should be 1
    case None:    print("missing")
```
And `v.index_of(99)` on the same receiver prints `0` instead of `missing` — it claims to find an
element that is not there. Narrowed in `probe/p5.gg` / `p7.gg` / `p8.gg`: `index_of` alone is
correct; a **discarded** `v.contains(20)` is harmless; a *different* receiver is harmless;
`is_empty()` does not trigger it; and with `Vector[int] v = [...]` it is correct
(`vec_index_of_after_contains` passes). Trigger = auto-literal receiver + `contains` result bound
to a local + `index_of` inline as the match scrutinee. Same on **LLVM**.

**(d) Ragged nested literals rejected** — see defect 6, same expected-type-propagation root.

Cells: `vec_concat`, `vec_slice`, `vec_contains_index_of` (+ passing companions
`vec_concat_typed`, `vec_slice_typed`, `vec_index_of_after_contains`).

---

## 6. A ragged nested vector literal is rejected — NEW

Rank 6: chapter 5; the flatten / grouped-data exercise is written no other way.

`Vector[Vector[int]] nested = [[1, 2], [3], [4, 5]]`
-> `error[E_TypeMismatch]: type mismatch: expected 'int[2]', found 'int[1]'`

* The **declared** type is `Vector[Vector[int]]`, so the inner literals should be vectors, not
  length-checked arrays. Equal-length rows are accepted (`vec_nested` passes with
  `[[1,2,3],[4,5,6]]`), and building the same ragged structure with `push` works
  (`vec_nested_ragged_built` passes). Only the ragged *literal* fails.
* Cells: `vec_nested_ragged`, `ex_flatten_nested`.

---

## 7. `for j in 0..(n - 1)` does not parse — NEW

Rank 7: chapter 3; every hand-written bubble sort and windowed loop.

`for j in 0..(n - 1):` -> `error: expected ',', found '-'`

* The bare form `for j in 0..n - 1:` parses and runs correctly (`probe/p12.gg` has both side by
  side). The parser appears to read `(` after `..` as the start of a tuple, so the `-` looks like
  a missing comma. Parenthesising for clarity is the instinctive spelling.
* A parenthesised *start* is fine — `for j in (i + 1)..v.len():` works (`ex_two_sum` passes).
* Cells: `loop_range_paren_endpoint`, `sort_bubble_manual`.

---

## 8. `sort_by_key` on `Vector[String]` fails at LINK — NEW

Rank 8: chapter 5, Book verbatim (`words.sort_by_key((s): s.len())`).

```
/usr/bin/ld: in function `__Closure_0__call':
  undefined reference to `int64_t__len'
```

* Compiles clean, fails at link. The closure parameter is typed `int64_t` regardless of the
  receiver's element type, so `.len()` mangles to `int64_t__len`.
* `Vector[int].sort_by_key((x): x)` **works** (`probe/p15.gg` -> `1,2,3`), so the axis is the
  element type.
* Cells: `sort_by_key_length`, `vec_sort_by_key_strings`.
* Related but distinct: TODO.md ~576 lists `sort_by_key`/`sorted_by_key` HOF-lowering as a
  **self-host** gap; this is a Rust-lane link failure.

---

## 9. `type(x)` does not parse — NEW

Rank 9: documented in **both** `docs/book/05-collections.md` and `docs/language-reference.md` S15.

`print(type(x))` -> `error: expected expression, found 'type'`

Not "no such function" — a **parse** error: `type` is a reserved word (used for error-set
aliases, TODO.md ~139) and cannot appear in expression position. Cell: `misc_type_builtin`.

---

## 10. `.iter()` needs an undocumented import — NEW

Rank 10: chapter 5. The chapter presents `.iter()` as available on every collection and never
mentions an import.

`print(f"{v.iter().count()}")` -> `error[E_NoMethodFound]: no method 'iter' found on type 'Vector[int]'`

Adding `from std.iter import Iterator` fixes it (`probe/p9.gg` -> `OK: no semantic errors`).
**Inconsistent:** `for i, x in v.iter().enumerate():` resolves *without* the import
(`vec_enumerate_method` passes). Cells: `vec_iter_terminals`, `misc_iter_no_import`.

---

## 11. `auto v = []` then push a runtime String -> raw pointer — FILED

`tests/fixtures/known_gaps/auto_empty_literal_push_string_raw_pointer.gg`.

```gorget
String mk(String a, String b): return a + b

void main():
    auto v = []
    v.push(mk("al", "pha"))
    print(v.get(0).unwrap())      # 187650945262240
```

Same via `v[0]`. Same on **LLVM**. What this sweep adds is the pair context: the *same* build
written three other ways is correct — `Vector[String] v = Vector[String]()`,
`Vector[String] v = []` (typed destination, empty **literal**), and a comprehension. So the
trigger is precisely `auto` + empty literal, not the empty literal itself.
Cells: `vec_empty_auto_push_string`, `vec_empty_auto_push_string_subscript`,
`pair_buildvec_c_auto_empty_literal`.

---

## 12. Borrow/move markers leak into `Option` payload types — FILED root, 3 NEW COSTUMES

Filed as HIGH at TODO.md ~248 (found 2026-08-22), from the `docs/language-design.md:2609` idiom.
The filed faces are `??` RHS mismatch, `unwrap().unwrap()` rejection, and an ICE.

**The filed face, in beginner clothes.** Book Ch5 verbatim:
`int a = ages.get("Alice") ?? 0` ->
`error[E_DefaultOpRhsTypeMismatch]: right-hand side of '??' must be 'int &' (unwrapped) ... but got 'int'`
This kills the **word-frequency counter** — the canonical map exercise in three curricula
(`dict_count_words`).

**Three costumes not enumerated in the filed entry.** All three are `match` on a method call used
**inline as the scrutinee**, and all three are repaired by binding the `Option` to a local first:

| receiver / call | payload type bound | cell | passing companion |
|---|---|---|---|
| `Dict[String,int].get(k)` | `int &` | `dict_put_get` | `dict_get_bound_first` |
| `Vector[int].pop()` | `int !` | `vec_pop` | `vec_pop_typed` |
| `v.iter().find(pred)` | `<error>` | `opt_vector_find` | `probe/p11.gg` |

Each reports `E_NonPrintableInterpolation: cannot interpolate ... of type '<X>' in string`.
`Vector[int].get(i)` does **not** exhibit it (`vec_get_out_of_bounds` passes), so it is
per-method, not universal. Workarounds that all work: `unwrap_or`, `unwrap`, and binding to
`Option[int]` first (`probe/p10.gg`).

---

## 13. `enumerate(c)` and `zip(a, b)` as free functions do not exist — FILED

TODO.md ~286 ("the Book teaches two functions that DO NOT EXIST"), filed 2026-08-17.
`error[E_UndefinedName]: undefined name 'enumerate'` / `undefined name 'zip'; did you mean 'Div'?`
The real spellings are `c.iter().enumerate()` / `a.iter().zip(b.iter())`.
Cells: `vec_enumerate`, `vec_enumerate_free_fn`, `vec_zip`.

---

# UNSURE (2)

**`f"{42:<5d}"` silently drops the alignment spec.** Prints `[42]`, not `[42   ]`. The documented
format grammar is `[#][0][width][.precision][type]` — alignment is not in it, so the intended
answer is genuinely unknown. But an unrecognised spec being **silently dropped** rather than
rejected is the shape Core #10 warns about. Width alone works (`f"{42:5d}"` -> `[   42]`).
Cell: `misc_format_align`.

**`String.to_int()` does not exist.** `docs/language-reference.md:1917` says *"Use
`String.to_int()` for parsing"*; the method is not found. The book teaches `std.conv.parse_int`,
which works correctly including the failure path (`misc_parse_int` passes). Per AGENTS.md a
reference-vs-code conflict is an open question, not doc-wins. Cell: `misc_string_to_int_method`.

---

# REJECTED-AND-THAT-IS-RIGHT (8) — with diagnostic-quality notes

All eight rejections are **correct**. What varies is whether the message helps.

| shape | first message the user sees | verdict |
|---|---|---|
| `!flag` for `not flag` | `E_MoveInOperandPosition` — explains the sigil, lists where it IS valid, says "read the place directly" | **excellent** (does not name `not` as the fix) |
| `a && b` | `E_TypeMismatch: expected bool, found int` **then** `E_AmpInOperandPosition` (equally good text) | good message, **wrong one on top**; neither names `and` |
| `"ab" * 3` | `E_TypeMismatch` **then** `E_UnsupportedOperator: '*' is not defined for String — String supports +/+= (concatenation) only` | good, but no pointer to `.repeat(n)`; confusing first error |
| `"x = %d" % x` | same shape as above for `%` | good, but no pointer to f-strings |
| `range(5)` | `E_UndefinedName: undefined name 'range'` | **incomplete** — the most common Python carry-over, no hint at `0..5`, though the compiler emits did-you-mean elsewhere |
| `v.append(3)` | `E_NoMethodFound: no method 'append' found on type 'Vector[int]'` | **incomplete** — no "did you mean `push`" |
| `void main() { ... }` | `error: expected type, found '{'` | **poor** — never mentions indentation-based blocks |
| `int x = 5;` | `error: unexpected character: ';'` (one per semicolon) | terse but adequate |

Two cross-cutting diagnostic findings from the WORKS/WRONG cells:

* **Spans point at end-of-file.** Several `E_NoMethodFound` / `E_NotIndexable` errors on `Vector`
  receivers cite a blank line at EOF (`cells/vec_iter_terminals.gg:9:1`) instead of the offending
  call, and repeat the same error 3-4 times.
* **The bounds trap has no source location.** `trap[T_Bounds]: index out of bounds: index 10,
  length 3 at <unknown>:0:0`, where `trap[T_DivByZero]` and `trap[T_Overflow]` both carry a real
  `file:line:col`.

---

# Lane and sanitizer summary

* Every silently-wrong defect that builds on both backends is **wrong on both** (defects 1, 4,
  5c, 11) — no backend-specific carve-outs.
* Two defects are **worse on LLVM**: `Vector[Struct].sort(cmp)` (defect 3) and
  `+= .get().unwrap()` (defect 2) produce invalid IR that `llc` rejects, where the C backend
  emits a silently wrong program. In both cases the LLVM error names the mechanism (`ptr` where
  `i64` is expected), which should shorten the fix.
* **ASan/UBSan: clean across all 354 cells** (`gg build --sanitize`, `detect_leaks=0`). The only
  non-zero exits are the three cells that are *meant* to trap. Note that ASan is silent on every
  raw-pointer-printed-as-integer cell — it is a type confusion, not an invalid access, so the
  sanitizer is structurally blind to it (Core #13).
