# D29 READABILITY PAGES — the full post-wave surface

**Currency write-through 2026-07-16.** Normative: `decisions.md` LOG (D29 formal +
catch-attachment pin). Signatures keep **`throws E`**; call sites carry postfix **`!`**.
Earlier drafts that rewrote signatures as `! E` are superseded.

Renders representative Gorget in the complete post-wave surface: **D27** `^` (move sigil,
replacing prefix `!`) · **D22** `v[a:b]` colon-slices (replacing `.slice()`) · **D28** `**`
power + `**!` fallible · **D26** `+! -! *! …` fallible operators · **D29** postfix `!`
fallible-use marker on calls. Before = today's surface; After = post-wave (ratified).

These feed the owner's readability census. The point D29 argues: today the propagation is
*invisible* (auto-prop happens with no mark); after, every place a call can fail is visible
at the call, exactly as the language's own "sigils mark effects" principle demands. Keywords
still state contracts (`throws E`); sigils mark flow (`!` at use sites).

---

## Page 1 — Book error example (`docs/book/10-errors.md`, "Auto-Propagation")

### BEFORE (today — propagation is invisible)

```gorget
Config load_config(String path) throws String:
    String content = read_file(path)      # if this throws, we throw too — but you can't SEE it
    Config cfg = parse_config(content)     # same here — looks like a normal call
    return cfg

int parse_port(String input) throws String:
    if input.is_empty():
        throw "empty input"
    Option[int] n = int.parse(input)
    match n:
        case Some(val):
            if val < 1 or val > 65535:
                throw f"port out of range: {val}"
            return val
        case None:
            throw f"not a number: {input}"
```

### AFTER (D29 — every fallible call carries `!`; contract stays `throws`)

```gorget
Config load_config(String path) throws String:
    String content = read_file(path)!      # `!` — this call may abort load_config
    Config cfg = parse_config(content)!    # visible at a glance
    return cfg

int parse_port(String input) throws String:
    if input.is_empty():
        throw "empty input"
    Option[int] n = int.parse(input)
    match n:
        case Some(val):
            if val < 1 or val > 65535:
                throw f"port out of range: {val}"
            return val
        case None:
            throw f"not a number: {input}"
```

What changed: a `!` after each throws call (`read_file(path)!`, `parse_config(content)!`).
Signatures still say `throws String`. `int.parse` is captured into an `Option` (not a throws
call), so no mark. `throw` is unchanged. The happy path still reads straight-line — you've
added one glyph per fallible edge, and gained the ability to see, in `load_config`, that it
has exactly two places it can fail.

Handled form (same mark; disposition attaches):

```gorget
void main():
    Config cfg = load_config("app.toml")! catch (String e):
        print(f"config error: {e}")
        return
    # …
```

---

## Page 2 — Real self-host excerpt (move sigil + slices), then throws-augmented

Source: `tests/fixtures/self_host_lowerer/lexer.gg` + `loader.gg` (real self-host code).
The self-host uses NO throws today, so this page shows D27 + D22 on genuine code, then a
D29-augmented variant of the loader to show how error propagation would read.

### BEFORE (today) — move `!`, `.slice()`

```gorget
void lex_emit(&self, Token !tok, int start, int end):
    self.lex_tokens.push(SpannedToken(!tok, start + self.lex_base_offset,
                                      end + self.lex_base_offset))

String resolve_std(String module_path):
    return "std." + module_path.slice(8, module_path.len())

String parent_dir(String path):
    int last_slash = find_last(path, "/")
    return path.slice(0, last_slash)
```

### AFTER (D27 `^` + D22 `[a:b]`)

```gorget
void lex_emit(&self, Token ^tok, int start, int end):
    self.lex_tokens.push(SpannedToken(^tok, start + self.lex_base_offset,
                                      end + self.lex_base_offset))

String resolve_std(String module_path):
    return "std." + module_path[8:]

String parent_dir(String path):
    int last_slash = find_last(path, "/")
    return path[:last_slash]
```

`Token !tok` → `Token ^tok` (move sigil, param + call site); `.slice(8, len())` → `[8:]`;
`.slice(0, last_slash)` → `[:last_slash]`. Freeing `!` from the move role is precisely what
lets D29 claim `!` for the error channel without ambiguity.

### AFTER + D29 — a fallible loader path (illustrative)

```gorget
String load_module_source(String module_path) throws LoadError:
    String resolved = resolve_import(module_path)!     # may throw LoadError
    String source = read_file(resolved)!               # may throw LoadError
    return source[:source.len()]                        # (D22 slice; no failure here)
```

Every hop that can abort `load_module_source` is marked; the signature still declares
`throws LoadError`. Compare to the invisible-auto-prop version where `resolve_import(...)`
and `read_file(...)` looked identical to infallible calls.

---

## Page 3 — Dense synthetic sample (all of D22/D26/D27/D28/D29 together)

A small expression evaluator — the densest realistic mix.

### BEFORE (today)

```gorget
int eval_pow(String expr, int base, int exp) throws EvalError:
    Vector[int] digits = parse_digits(expr)            # invisible propagation
    int head = digits.slice(0, 1).get(0).unwrap()
    Vector[int] rest = digits.slice(1, digits.len())
    int acc = pow(base, exp)                            # float-only free-fn today
    for int d in rest:
        acc = checked_add(acc, d)                       # verbose fallible add
    return acc

Vector[int] parse_digits(String s) throws EvalError:
    if s.is_empty():
        throw EvalError.Empty()
    return to_digits(!s)                                 # move + invisible propagation
```

### AFTER (D22 `[a:b]` + D26 `+!` + D27 `^` + D28 `**` + D29 `!`)

```gorget
int eval_pow(String expr, int base, int exp) throws EvalError:
    Vector[int] digits = parse_digits(expr)!           # visible fallible use
    int head = digits[0:1].get(0).unwrap()             # D22 slice
    Vector[int] rest = digits[1:]                       # D22 slice-to-end
    int acc = base ** exp                               # D28 power operator, checked
    for int d in rest:
        acc = acc +! d                                  # D26 fallible add (Overflow → channel)
    return acc

Vector[int] parse_digits(String s) throws EvalError:
    if s.is_empty():
        throw EvalError.Empty()
    return to_digits(^s)!                               # D27 move + D29 fallible-use mark
```

Reading the AFTER top-to-bottom, the fallible surface is legible without leaving the line:
`parse_digits(expr)!` can fail; `base ** exp` is a checked power (traps, not in the
signature); `acc +! d` is a fallible add that feeds the `EvalError` channel; `to_digits(^s)!`
moves `s` in and marks the fallible call. Each sigil says one thing:
`^` = the source dies here · `!` (postfix on a call) = this use is fallible ·
`!` (on an operator, `+!`/`**!`) = this arithmetic can fail into the error channel ·
`[a:b]` = a slice · `throws E` = the function's error contract.

### The one corner to keep in mind

A throws call compared directly for inequality needs a space so `!` doesn't fuse with `!=`:

```gorget
if compute()! != sentinel:        # `compute()! != x` — one space; mark then compare
    ...
# `compute()!=sentinel` would parse as `compute() != sentinel` (bare call, no mark)
# → E_UnhandledThrows with a fix-it. `gg fmt` inserts the space automatically.
```
