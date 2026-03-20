# Meta Programming

Gorget's `meta` system runs code at compile time. Where other languages need macros,
code generators, or build scripts, Gorget uses `meta` — typed, checked, and part of
the language.

---

## Meta Constants

Compile-time values, inlined everywhere they're used:

```gorget
meta int MAX_CONNECTIONS = 1024
meta int BUFFER_SIZE = MAX_CONNECTIONS * 64
meta String VERSION = "2.1.0"
meta float PI = 3.14159265358979
meta bool DEBUG = false
```

Constants compose freely — `BUFFER_SIZE` uses `MAX_CONNECTIONS` in its definition.
They're evaluated during compilation and replaced with their values in the output.

---

## Meta Assertions

Check invariants at compile time:

```gorget
meta assert MAX_CONNECTIONS > 0, "MAX_CONNECTIONS must be positive"
meta assert BUFFER_SIZE <= 1048576, "buffer exceeds 1 MB"
```

Failures are compile errors, not runtime errors. Use these to catch configuration
mistakes before the program ever runs.

---

## Meta Log

Print messages during compilation:

```gorget
meta int PAGE_SIZE = 4096
meta int PAGES = 16

meta log "page size:", PAGE_SIZE, "pages:", PAGES
# Compiler output: [meta] page size: 4096 pages: 16
```

Inside generic functions, `meta log` runs at monomorphization time — once per
type instantiation:

```gorget
void process[T]():
    meta log "processing type:", typename(T)
```

---

## Conditional Compilation

### Meta If

Choose code paths at compile time:

```gorget
meta if platform() == "linux":
    print("running on Linux")
elif platform() == "macos":
    print("running on macOS")
else:
    print("unknown platform")
```

Only the matching branch is compiled. The others are discarded entirely — they
don't even need to type-check.

### Feature Flags

Enable features from the command line:

```gorget
meta if feature("debug_logging"):
    void log(String msg):
        print(f"[DEBUG] {msg}")
else:
    void log(String msg):
        pass
```

```bash
gg build app.gg --feature debug_logging
```

### Conditional Type Aliases

```gorget
meta bool ORDERED = true
meta type Map = Dict if ORDERED else HashMap
meta type Index = int32 if MAX_ENTITIES <= 2147483647 else int64
```

---

## Type Predicates

Inside generic functions, test type properties with `is`:

```gorget
void describe[T]():
    meta if T is int:
        print("integer type")
    elif T is float:
        print("floating point")
    elif T is str:
        print("string type")
    else:
        print("other type")
```

Available predicates:

| Predicate | Matches |
|-----------|---------|
| `T is int` | Any integer type |
| `T is float` | Any float type |
| `T is signed` | Signed integers |
| `T is unsigned` | Unsigned integers |
| `T is numeric` | Any number |
| `T is bool` | `bool` |
| `T is str` | `str` |
| `T is char` | `char` |
| `T is int32` | Exact type match |
| `T is MyStruct` | Exact type match |

Compose with `or` and `and`:

```gorget
meta if T is signed or T is float:
    print("handles signed math")
```

---

## Reflection Builtins

### Type Information

```gorget
void info[T]():
    meta log "type:", typename(T)
    meta log "size:", sizeof(T)
    meta log "bits:", bitwidth(T)
    meta log "min:", min_val(T)
    meta log "max:", max_val(T)
```

### Struct Reflection

Inspect struct fields at compile time:

```gorget
struct Vec3:
    float x
    float y
    float z

void inspect[T]():
    meta log "fields:", field_count(T)
    meta for name in field_names(T):
        meta log "  field:", name
```

Available struct builtins:

| Builtin | Returns |
|---------|---------|
| `fields(T)` | List of (name, type) pairs |
| `field_names(T)` | List of field names |
| `field_count(T)` | Number of fields |
| `has_field(T, String)` | Whether field exists |
| `field_type(T, String)` | Type of a field |
| `field_value(val, name)` | Read field by name |
| `field_set(obj, name, value)` | Write field by name |

### Enum Reflection

```gorget
enum Color:
    Red
    Green
    Blue

void list_variants[T]():
    meta for name in variant_names(T):
        meta log "variant:", name
```

| Builtin | Returns |
|---------|---------|
| `variant_names(T)` | List of variant names |
| `variant_count(T)` | Number of variants |
| `variant_payloads(T)` | List of [name, type] pairs |
| `enum_ordinal(T, String)` | Ordinal of variant |

### Trait Checking

```gorget
meta if implements(T, "Displayable"):
    print(f"{val}")
else:
    print("(not displayable)")
```

---

## Meta For

Loop over compile-time lists:

```gorget
void print_fields[T](T val):
    meta for name in field_names(T):
        meta log "accessing:", name
        auto v = field_value(val, name)
        # use v...
```

`meta for` unrolls at compile time — each iteration becomes a separate block of
code specialized for that field/variant.

### Expanding Match Arms

```gorget
void describe_color[E](E val):
    match val:
        meta for vname, T in variant_payloads(E):
            case vname(payload):
                print(f"{vname}")
```

This generates one `case` arm per variant, without writing them by hand.

---

## Platform Builtins

Available at module level (no generic context needed):

| Builtin | Returns |
|---------|---------|
| `platform()` | `"linux"`, `"macos"`, `"windows"`, `"freebsd"` |
| `arch()` | `"x86_64"`, `"aarch64"` |
| `arch_word_bits()` | `64` or `32` |
| `debug()` | `true` in debug builds |
| `sizeof(Type)` | Size in bytes |
| `alignof(Type)` | Alignment in bytes |
| `typename(Type)` | Type name as string |
| `embed_file(String)` | File contents embedded at compile time |

### Embedding Files

```gorget
meta String SCHEMA = embed_file("schema.sql")
meta String LICENSE = embed_file("../LICENSE")
```

The file contents become a compile-time string constant. The path is relative
to the source file.

---

## Summary

| Feature | Syntax | Evaluated |
|---------|--------|-----------|
| Constant | `meta int X = expr` | Compile time |
| Assertion | `meta assert cond, msg` | Compile time |
| Log | `meta log exprs` | Compile time |
| Conditional | `meta if cond:` | Compile time |
| Type alias | `meta type T = ...` | Compile time |
| Type predicate | `T is numeric` | Monomorphization time |
| Struct reflection | `field_names(T)` | Monomorphization time |
| Enum reflection | `variant_names(T)` | Monomorphization time |
| Loop | `meta for x in list:` | Unrolled at compile time |
| Feature flag | `feature("name")` | Compile time, set via CLI |
| File embedding | `embed_file("path")` | Compile time |
