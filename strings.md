# String Redesign — UTF-8 by Default

## Overview

Gorget's string types are redesigned around three types with clear roles:

| Type | C representation | Copy? | Encoding | Purpose |
|------|-----------------|-------|----------|---------|
| `str` | `Str { const char* data, size_t len }` | Yes (view) | UTF-8 guaranteed | Primary string type — reading text |
| `String` | `String { char* data, size_t len, size_t cap, ... }` | No (owned) | UTF-8 guaranteed | Owned mutable strings — building/storing text |
| `cstr` | `const char*` | Yes (pointer) | Raw bytes, null-terminated | C interop only |

**Key changes from current design:**
- `str` becomes a **fat pointer** (pointer + byte length) instead of bare `const char*`
- `str` operations use **codepoint indexing** (Python semantics)
- The old `const char*` semantics move to `cstr` for C FFI
- UTF-8 is validated at system boundaries (FFI, file I/O, network)

## Type Details

### `str` — UTF-8 immutable view

```c
typedef struct {
    const char* data;
    size_t len;           // byte length (NOT codepoint count)
} Str;
```

- **Copy**: bitwise copy of two words — no ownership, no freeing
- **Never owns memory**: points into a String's buffer, static data, or stack data
- **UTF-8 guaranteed**: all operations preserve validity by construction
- **Two words**: efficient to pass by value
- **Not null-terminated**: can represent substrings without copying

The internal `len` field stores *byte* length for O(1) byte operations. User-facing `.len()`
returns *codepoint* count (O(n)) — Python semantics.

### `String` — UTF-8 owned heap string

```c
typedef struct {
    char* data;
    size_t len;           // byte length
    size_t cap;           // allocated capacity
} String;
```

(Allocator field will be added per `alloc.md` plan.)

- **Non-Copy**: owns its buffer, freed via RAII `gorget_string_free()`
- **Mutable**: supports append, insert, replace operations
- **UTF-8 guaranteed**: mutation operations preserve validity
- **Coerces to `str`** automatically: `(Str){ .data = s.data, .len = s.len }`

### `cstr` — raw C string

```c
const char*
```

- **Copy**: just a pointer
- **Null-terminated**: standard C convention
- **No encoding guarantee**: raw bytes, no UTF-8 validation
- **For C interop only**: passing to/from extern C functions

## String Literals

String literals produce `str` values at zero cost:

```
str greeting = "hello"
# compiles to: Str greeting = { "hello", 5 };
# no heap allocation — data lives in the read-only segment
```

The compiler knows the byte length at compile time and embeds it directly.

For `String`:
```
String greeting = "hello"
# compiles to: String greeting = gorget_string_new("hello", 5, __gorget_current_alloc);
# heap allocation — copies the literal into owned buffer
```

For `cstr`:
```
cstr raw = c"hello"           # explicit cstr literal with c prefix
# compiles to: const char* raw = "hello";
```

Note: `c"..."` prefix creates a `cstr` literal. Bare `"..."` always produces `str`.

## Coercion Paths

```
String → str      automatic: extract { .data, .len }
str → String      explicit: String(s) constructor (allocates, copies)
cstr → str        explicit: str.from_cstr(p) (calls strlen, validates UTF-8)
str → cstr        explicit: s.to_cstr() (copies + null-terminates)
String → cstr     explicit: s.to_cstr() (returns .data if no embedded nulls)
"literal" → str   automatic at compile time (zero cost)
"literal" → String  automatic when assigned to String variable (allocates)
c"literal" → cstr  automatic at compile time (zero cost)
```

The `str → cstr` direction requires care: `str` may contain embedded null bytes (valid
in UTF-8 for U+0000) and is not null-terminated. `to_cstr()` appends a null terminator,
allocating if needed.

## Operations — Codepoint Indexing (Python Semantics)

All user-facing operations on `str` and `String` use **codepoint indexing**. A codepoint is
a single Unicode character (U+0000 to U+10FFFF), encoded as 1-4 bytes in UTF-8.

### Length

```
str s = "café"
s.len()                  # → 4 (codepoints: c, a, f, é)
s.byte_len()             # → 5 (bytes: 63 61 66 C3 A9)
```

`len()` counts codepoints by walking the UTF-8 sequence — O(n) in byte length.
`byte_len()` returns the stored byte length — O(1).

### Indexing

```
str s = "café"
s[0]                     # → "c" (str view of first codepoint)
s[3]                     # → "é" (str view of fourth codepoint)
s[4]                     # → runtime error: index out of bounds
s[-1]                    # → "é" (negative indexing from end, Python-style)
```

`s[i]` returns a `str` view of the ith codepoint — O(n) to find the byte offset.
The returned `str` points into the original string's buffer (no allocation).

### Slicing

```
str s = "café au lait"
s[0..4]                  # → "café" (str view, codepoint range)
s[5..7]                  # → "au"
s[5..]                   # → "au lait" (to end)
s[..4]                   # → "café" (from start)
```

Codepoint-based ranges. Returns `str` views — no allocation.

### Iteration

```
for ch in "café":
    print(ch)            # prints: c, a, f, é (each is a str)
```

Iterates over codepoints, yielding `str` values (single-codepoint views).

```
for b in "café".bytes():
    print(b)             # prints: 99, 97, 102, 195, 169 (each is uint8)
```

Byte-level iteration via `.bytes()`.

```
for cp in "café".codepoints():
    print(cp)            # prints: 99, 97, 102, 233 (each is int, Unicode codepoint value)
```

Codepoint integer values via `.codepoints()`.

### String Methods

| Method | Behavior | Returns |
|--------|----------|---------|
| `s.len()` | Codepoint count (O(n)) | `int` |
| `s.byte_len()` | Byte count (O(1)) | `int` |
| `s.is_empty()` | `s.byte_len() == 0` (O(1)) | `bool` |
| `s.contains(needle)` | Byte-level search (O(n*m)) | `bool` |
| `s.starts_with(prefix)` | Byte-level prefix check | `bool` |
| `s.ends_with(suffix)` | Byte-level suffix check | `bool` |
| `s.index_of(needle)` | First occurrence (codepoint index) | `int` (-1 if not found) |
| `s.count(needle)` | Count occurrences | `int` |
| `s.split(delim)` | Split by delimiter | `Vector[str]` |
| `s.join(parts)` | Join with separator | `String` |
| `s.trim()` | Strip Unicode whitespace | `str` |
| `s.strip(chars)` | Strip given characters | `str` |
| `s.upper()` | Unicode uppercase | `String` |
| `s.lower()` | Unicode lowercase | `String` |
| `s.replace(old, new)` | Replace all occurrences | `String` |
| `s.repeat(n)` | Repeat n times | `String` |
| `s.removeprefix(pre)` | Remove prefix if present | `str` |
| `s.removesuffix(suf)` | Remove suffix if present | `str` |
| `s.chars()` | Codepoints as str views | `Vector[str]` |
| `s.bytes()` | Raw UTF-8 bytes | `Vector[uint8]` |
| `s.codepoints()` | Codepoint integer values | `Vector[int]` |
| `s.to_cstr()` | Null-terminated copy | `cstr` (allocates) |
| `s.hash()` | Hash (byte-level) | `int` |

**Return type rule**: methods that return a contiguous view of the original return `str`
(no allocation). Methods that produce new/modified text return `String` (allocates).

### Comparisons

```
s1 == s2                 # byte-level comparison (valid because both are UTF-8)
s1 < s2                  # lexicographic by codepoint value
```

Byte-level comparison is correct for UTF-8 equality (identical bytes = identical codepoints).
Ordering is by codepoint value (Unicode scalar order), which byte-level lexicographic
comparison produces correctly for UTF-8.

### String Building

```
String builder = String()
builder.append("hello")
builder.append(" ")
builder.append("world")
str result = builder              # coerce to str view
```

Or via concatenation:
```
str a = "hello"
str b = " world"
String combined = a + b           # str + str → String (allocates)
```

## String Interpolation

```
str name = "world"
String greeting = "hello {name}"  # → String via gorget_string_format
print("hello {name}")             # → direct printf (no intermediate allocation)
```

Interpolation always produces `String` when assigned. `print()` uses printf directly.

## UTF-8 Validation

Validation happens at **system boundaries** — where bytes enter the str/String world:

| Boundary | Validation |
|----------|-----------|
| String literals | Compile-time (lexer reads UTF-8 source) |
| `cstr → str` conversion | Runtime: `str.from_cstr()` validates, returns `Result[str, Error]` |
| File I/O | `File.read()` validates, returns `Result[String, Error]` |
| Network I/O | Validate on receive |
| Byte buffer → String | `String.from_bytes()` validates, returns `Result[String, Error]` |

Once inside `str`/`String`, validity is maintained by construction — all operations
produce valid UTF-8 from valid UTF-8 inputs.

## Unicode Scope

### Included (Phase 1)
- UTF-8 encoding/decoding
- Codepoint iteration and indexing
- Byte-level operations (contains, starts_with, etc. — correct for UTF-8)
- ASCII case conversion (a-z ↔ A-Z)
- ASCII whitespace trimming

### Included (Phase 2)
- Unicode-aware `upper()` / `lower()` (common scripts: Latin, Greek, Cyrillic)
- Unicode whitespace for `trim()` (Unicode category Zs)

### Deferred
- Full Unicode case folding (locale-dependent: Turkish İ/ı, German ß→SS, etc.)
- Grapheme cluster segmentation (combining characters, emoji sequences)
- Unicode normalization (NFC, NFD, NFKC, NFKD)
- Collation (locale-aware sorting)

These deferred features belong in a `std.unicode` library module, not built into the type.

## Impact on Existing Code

### What breaks
- Every function currently taking `const char*` for str params now receives `Str`
- `strlen(s)` calls must change to `s.len` (byte length field access)
- `strcmp(a, b)` calls must change to `memcmp` + length comparison
- String literal emission changes from bare pointer to struct init
- `s[i]` changes from byte access to codepoint access

### What stays the same
- `String` struct layout gains no new fields (allocator field is separate plan)
- String interpolation conceptually unchanged
- `+` concatenation behavior unchanged
- Borrow semantics unchanged

### Migration path
- `cstr` is added FIRST as an escape hatch — existing C interop code migrates to `cstr`
- Then `str` is changed to fat pointer
- Then operations are changed to codepoint-based

## C Runtime Functions

### New core functions

```c
// str construction
static inline Str gorget_str_from_literal(const char* data, size_t len) {
    return (Str){ .data = data, .len = len };
}

static inline Str gorget_str_from_cstr(const char* cstr) {
    return (Str){ .data = cstr, .len = strlen(cstr) };
}

// str operations
static inline size_t gorget_str_byte_len(Str s) {
    return s.len;
}

static inline size_t gorget_str_codepoint_count(Str s);    // O(n) UTF-8 walk
static inline Str gorget_str_index(Str s, int64_t i);    // ith codepoint
static inline Str gorget_str_slice(Str s, int64_t start, int64_t end);    // codepoint range
static inline bool gorget_str_eq(Str a, Str b);    // byte-level (correct for UTF-8)
static inline int gorget_str_cmp(Str a, Str b);    // lexicographic

// UTF-8 utilities
static inline int gorget_utf8_codepoint_len(unsigned char first_byte);    // 1-4 bytes
static inline int64_t gorget_utf8_decode(const char* data, size_t* advance);    // decode one codepoint
static inline bool gorget_utf8_validate(const char* data, size_t len);    // validate byte sequence
```

### Modified functions

All existing `gorget_string_*` functions that take `const char* s` as a str parameter
change to take `Str s`. Internal logic changes from `strlen()` to `s.len` for
length and from null-terminator scanning to length-bounded operations.

## Implementation Phases

### Phase S0: Add `cstr` type (escape hatch)

- Add `cstr` keyword to lexer
- Add `PrimitiveType::CStr` to type system
- `cstr` maps to `const char*` in codegen
- `cstr` is Copy, no drop
- Add `c"..."` literal syntax for cstr literals
- Add coercion: `cstr → str` (current str, temporary — will change in S2)
- **All existing tests pass unchanged** — this is purely additive

### Phase S1: Add `Str` fat pointer type to runtime

- Add `Str` typedef to C runtime
- Add `gorget_str_from_literal()`, `gorget_str_from_cstr()`
- Add `gorget_str_eq()`, `gorget_str_cmp()`
- Add UTF-8 utilities: `gorget_utf8_decode()`, `gorget_utf8_validate()`,
  `gorget_utf8_codepoint_len()`
- Add `gorget_str_codepoint_count()`, `gorget_str_index()`, `gorget_str_slice()`
- **Unit tests for all new runtime functions**
- Existing code unchanged — new functions coexist with old ones

### Phase S2: Switch `str` to fat pointer

This is the breaking change. Everything before is additive, everything after builds on this.

- Change `PrimitiveType::Str` codegen from `const char*` to `Str`
- String literal emission: `"hello"` → `(Str){ "hello", 5 }`
- Update `String → str` coercion: extract `{ .data, .len }`
- Update `str → String` coercion: pass both data and length
- Update all `gorget_string_*` runtime functions that take `const char*` str params
  to take `Str` instead
- Update `strcmp`-based comparisons to `gorget_str_eq`/`gorget_str_cmp`
- Update string method dispatch in `c_expr_methods.rs`
- Migrate any internal `const char*` usage that should be `cstr`
- **Fix all failing tests** — many will break due to type change

### Phase S3: Codepoint-based operations ✓

- Wire `s.len()` → `gorget_str_codepoint_count()` (was `strlen`) ✓
- Wire `s.byte_len()` → `s.len` field access (new method) ✓
- Wire `s[i]` → `gorget_str_index()` (codepoint-based, was byte-based) ✓
- Wire `s[i..j]` → `gorget_str_slice()` (codepoint-based, was byte-based) ✓
- Wire `for ch in s:` → UTF-8 codepoint iteration (was byte iteration) ✓
- Add `s.bytes()` method for explicit byte iteration ✓
- Add `s.codepoints()` method for codepoint integer iteration ✓
- Add `s.chars()` method for codepoint str views ✓
- **Update integration tests** that depend on string behavior ✓

### Phase S4: UTF-8 validation at boundaries ✓

- `str.from_cstr()` validates UTF-8, returns `Result[str, Error]` ✓
- `String.from_bytes()` validates UTF-8 ✓
- `File.read()` validates UTF-8 (or returns error) ✓
- Invalid UTF-8 in source code: lexer error ✓

### Phase S5: Unicode-aware operations ✓

- `upper()` / `lower()` for Latin, Greek, Cyrillic scripts ✓
- Unicode whitespace for `trim()` ✓
- Unicode-aware `contains()`, `split()`, etc. where relevant ✓

## Interaction with Other Plans

### Allocator design (`alloc.md`)
- `String` gains allocator field in a separate phase — orthogonal to this redesign
- `str.to_cstr()` allocates — uses current thread-local allocator
- New `String` from `str` uses current allocator

### Meta evaluation (`meta.md`)
- `meta str` constants work with the new fat pointer — compiler embeds byte length
- `meta if platform() == "macos"` — platform() returns `str` (fat pointer)

### GIR (`ir.md`)
- `str` in GIR: `type Str = struct { data: *u8, len: u64 }` with Copy semantics
- `cstr` in GIR: `type cstr = *u8` (bare pointer)
- String operations lower to calls to runtime functions (same as today, different signatures)
- **This redesign should land BEFORE GIR Phase 2** so the IR encodes the correct string types
  from the start

## Recommended Implementation Order (in context)

```
1. Allocator P0       (runtime refactor: malloc → macros)
2. Strings S0         (add cstr type — escape hatch)
3. Strings S1         (add Str runtime functions)
4. Strings S2         (switch str to fat pointer — breaking change)
5. Strings S3         (codepoint-based operations)
6. Strings S4         (UTF-8 validation at boundaries)
7. Meta M0–M3         (keyword, constants, assertions, conditional compilation)
8. GIR Phase 0        (data structures and tooling)
9. GIR Phase 1        (parallel pipeline)
10. Strings S5        (Unicode-aware operations — can happen in parallel with GIR)
...
```

Strings S0–S4 must land before GIR Phase 2 to avoid encoding the old string representation
into the IR.
