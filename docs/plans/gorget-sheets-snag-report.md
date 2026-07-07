# Gorget-sheets snag report (filed 2026-07-07)

Upstream bugs and language/tooling gaps discovered while building
[gorget-sheets](file://../../.worktrees/gorget-sheets) (spreadsheet TUI + formula
engine). Each item has a minimal repro under `tests/fixtures/known_gaps/` and a
`TODO.md` entry. **Re-run repros in the current tree before acting** — do not
trust this document's symptom descriptions without regenerating.

**Reporter context:** gorget-sheets dogfoods idiomatic Gorget (`match`, `Result`,
`String` slices, `Dict.get`, `&` borrows, multi-module layout under `src/`).
Workarounds live in gorget-sheets `AGENTS.md` §7–8 until these close.

---

## Snag #53 — nested struct field mutation through `&outer.inner` is a silent no-op

**Severity:** High (correctness — mutations appear to run but do not persist)

**Symptom:** `void set(Outer &o): o.inner.raw[k] = v` returns without error; a
subsequent read shows the dict unchanged. Same pattern blocked gorget-sheets TUI
commits: `st.view.sheet.raw[addr] = text` and `sheet_set(&st.view.sheet, …)`
never persisted; flattening `AppState` to `st.sheet.raw[addr] = text` worked.

**Reference-grade:** Taking `&nested.field` for mutation should alias the live
sub-object (same as Rust `&mut outer.inner`).

**Repro:** `tests/fixtures/known_gaps/snag53_nested_struct_field_mut.gg`

```bash
target/debug/gg run tests/fixtures/known_gaps/snag53_nested_struct_field_mut.gg
# got: (empty line)   expected: =1+2
```

**Workaround (gorget-sheets):** Flat `AppState` with `Sheet` as a direct field;
mutate `st.sheet.raw[…]` only through `&AppState`, never `&st.view.sheet`.

---

## Snag #54 — `Result` local + branch assign + `return out` returns wrong variant

**Severity:** High (correctness — silent wrong numeric result)

**Symptom:** A function assigns `Result[T,E] out` in `if`/`else` branches, then
`return out`. Callers observe the wrong variant (e.g. `Ok(0.0)` for a non-empty
literal cell). Replacing with **early `return` per branch** fixes it.

**Repro:** `tests/fixtures/known_gaps/snag54_result_out_fallthrough.gg`

```bash
target/debug/gg run tests/fixtures/known_gaps/snag54_result_out_fallthrough.gg
# got: 0.000000   expected: 3
```

**Workaround:** Early `return` from each branch in `sheet_get_value` (gorget-sheets
`src/engine/sheet.gg`).

**Investigate:** LIR lowering for `Result` phi/merge at function exit; likely
drops or default-initializes the result slot.

---

## Snag #55 — `Dict.get_or` inside a callee mis-reads (empty) while caller `get_or` works

**Severity:** High (correctness)

**Symptom:** In `main`, `raw.get_or("A1", "")` prints the stored value; the same
`get_or` inside `cell_value(raw, addr)` sees empty and takes the error path.
`Option.get` + `unwrap` in the callee works.

**Repro:** `tests/fixtures/known_gaps/snag55_dict_get_or_in_callee.gg`

```bash
target/debug/gg run tests/fixtures/known_gaps/snag55_dict_get_or_in_callee.gg
# got: 3 then empty   expected: 3 then 3
```

**Workaround:** Use `sh.raw.get(addr)` + `Option` handling in callees
(`sheet_cell_display`, `sheet_get_value`).

---

## Snag #56 — `.contains()` on module-level `String` constant mis-lowers (CC-FAIL)

**Severity:** Medium (compile failure)

**Symptom:** `String SHEET_WS = " \t"` then `SHEET_WS.contains(ch)` fails C
compile (`str__contains` argument types wrong). Literal receiver `" \t".contains(ch)`
compiles and runs.

**Repro:** `tests/fixtures/known_gaps/snag56_module_string_contains.gg`

```bash
target/debug/gg run tests/fixtures/known_gaps/snag56_module_string_contains.gg
# C compile error on str__contains
```

**Workaround:** Use string-literal receivers or explicit `ch == " " or ch == "\t"`.

---

## Snag #57 — import path model: file-relative, no package `src/` root

**Severity:** Medium (tooling / UX — blocks natural multi-module layouts)

**Symptom (not a single-runtime bug):**

1. **Same-directory rule:** Inside `src/engine/sheet.gg`, `from engine.errors import …`
   resolves to `src/engine/engine/errors.gg` (missing). Same-package imports must be
   `from errors import …` (relative to the importing file's directory).

2. **Entry-relative rule:** Only modules reachable from the **entry file's directory**
   import. Programs under `tests/fixtures/` cannot import `src/engine/…` unless the
   entry is also under `src/` (gorget-sheets runs all eval harnesses from `src/eval_*.gg`).

3. **No `src_roots` in `gorget.toml`:** Unlike Cargo-style layouts, there is no manifest
   field to declare `src/` as a package root for stable `engine.sheet` imports.

**Reference-grade:** Document clearly in `docs/language-reference.md` + `docs/book/`,
and/or add `src_roots` / package-root imports so `from engine.sheet import …` works
from any entry when `gorget.toml` declares `src/`.

**Workaround (gorget-sheets):** All modules under `src/`; entry points and CLI fixtures
as `src/*.gg`; same-dir imports inside `term/` and `engine/`.

---

## Snag #58 — cross-module `int` bindings need `public` despite visibility spec ambiguity

**Severity:** Low (ergonomics / spec drift)

**Symptom:** `from codes import KEY_ENTER` fails with `E_PrivateImport` unless
`codes.gg` spells `public int KEY_ENTER = 260`. `language-reference.md` states most
items are public by default; unclear if module-level `int` globals count.

**Repro:** `tests/fixtures/known_gaps/snag52b/decode.gg` + `codes.gg` (without
`public` → check error; with `public` → OK).

**Workaround:** Export key codes as `public int` or duplicate literals at use sites
(gorget-sheets `term/keysource.gg` uses literal `match` arms for script tokens).

---

## Process note

When fixing any snag above:

1. Run the repro fixture end-to-end (`gg run`, diff stdout).
2. Add a non-`known_gaps` regression fixture if the fix is reference-grade.
3. Remove the matching workaround from gorget-sheets `AGENTS.md` §8.
4. Move the `TODO.md` entry to `DONE.md` with commit pointer.