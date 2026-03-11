# Gorget Language Reference

This document is the formal reference for the Gorget programming language. It describes the lexical structure, grammar, type system, and semantic rules that define the language.

For motivation, examples, and design rationale, see [language-design.md](language-design.md).

---

## 1. Notation

Grammar rules use a modified EBNF:

| Notation      | Meaning                                      |
|---------------|----------------------------------------------|
| `rule = ...;` | Production rule                              |
| `A B`         | Sequence (A followed by B)                   |
| `A | B`       | Alternative (A or B)                         |
| `[ A ]`       | Optional (zero or one A)                     |
| `{ A }`       | Repetition (zero or more A)                  |
| `( A )`       | Grouping                                     |
| `"text"`      | Terminal keyword or symbol                   |
| `UPPER`       | Token produced by the lexer                  |
| *italic*      | Prose description                            |

---

## 2. Source Files

A Gorget source file uses the `.gg` extension and is encoded as UTF-8. A source file contains a sequence of top-level items:

```ebnf
module = { item } ;
```

### 2.1 Indentation

Gorget uses indentation to delimit blocks, like Python. The lexer converts leading whitespace into `INDENT` and `DEDENT` tokens.

**Rules:**
- Blocks are opened by a `:` at the end of a line, followed by an increase in indentation.
- The indentation level must be consistent within a block.
- A decrease in indentation emits one or more `DEDENT` tokens.
- Tabs and spaces must not be mixed. Four spaces is the canonical indent.
- Expressions inside paired brackets (`()`, `[]`, `{}`) suppress newline and indentation processing, allowing multi-line expressions.
- A leading `.` on a new line is treated as continuation of the previous expression (method chaining).

### 2.2 Comments

```ebnf
comment     = "#" *any-char-except-newline* NEWLINE ;
doc_comment = "#/" *any-char-except-newline* NEWLINE ;
```

- `#` begins a line comment; the rest of the line is ignored.
- `#/` begins a documentation comment, attached to the next item.
- There are no block comments.

### 2.3 Semicolons

Gorget has no semicolons. Newlines terminate statements.

---

## 3. Lexical Structure

### 3.1 Keywords

Keywords are reserved and cannot be used as identifiers.

**Type keywords:**

```
int    int8   int16  int32  int64
uint   uint8  uint16 uint32 uint64
float  float32 float64
bool   char   str    String void   auto
```

**Declaration keywords:**

```
const  struct  enum  trait  equip  public  static  type  newtype  directive
```

**Control flow keywords:**

```
if  elif  else  for  while  loop  in  match  case
break  continue  return  do  pass
```

**Logical operator keywords:**

```
and  or  not  is
```

**Literal keywords:**

```
true  false  None  Some  Ok  Error
```

**Error handling keywords:**

```
throw  throws  raw  catch
```

**Import keywords:**

```
import  from
```

**Resource/scope keywords:**

```
with  as  via
```

**Generic/constraint keywords:**

```
where  extends  live  outlives
```

**Concurrency keywords:**

```
async  await  spawn  shared
```

**Safety keywords:**

```
unsafe  extern
```

**Self keywords:**

```
self  Self
```

**Smart pointer keywords:**

```
Box  Rc  Arc  Weak  Cell  RefCell  Mutex  RwLock
```

**Ownership keywords:**

```
move  mutable
```

**Testing keywords:**

```
test  suite  assert
```

**Special identifiers:**

```
it  panic
```

### 3.2 Identifiers

```ebnf
IDENTIFIER = letter { letter | digit | "_" } ;
letter     = "a".."z" | "A".."Z" | "_" ;
digit      = "0".."9" ;
```

An identifier must not collide with a keyword, except where context allows (e.g., field names).

### 3.3 Literals

#### Integer Literals

```ebnf
INT_LITERAL  = decimal | hex | octal | binary ;
decimal      = digit { digit | "_" } ;
hex          = "0x" hex_digit { hex_digit | "_" } ;
octal        = "0o" octal_digit { octal_digit | "_" } ;
binary       = "0b" bin_digit { bin_digit | "_" } ;
```

Underscores may appear between digits for readability: `1_000_000`. The type of an unadorned integer literal is `int` (alias for `int64`).

#### Float Literals

```ebnf
FLOAT_LITERAL = digit { digit | "_" } "." digit { digit | "_" } [ exponent ] ;
exponent      = ( "e" | "E" ) [ "+" | "-" ] digit { digit } ;
```

The type of an unadorned float literal is `float` (alias for `float64`).

#### Boolean Literals

```
true   false
```

Type: `bool`.

#### Character Literals

```ebnf
CHAR_LITERAL = "'" ( char | escape ) "'" ;
escape       = "\\" ( "n" | "t" | "r" | "\\" | "'" | "0"
             | "u{" hex_digit { hex_digit } "}" ) ;
```

Type: `char`. A Unicode scalar value (4 bytes).

#### String Literals

```ebnf
STRING_LITERAL = [prefix] '"' { string_segment } '"' ;
string_segment = literal_chars | interpolation | escape ;
interpolation  = "{" expression [":" format_spec] "}" ;
format_spec    = ["#"] ["0"] [width] ["." precision] [type_char] ;
```

Interpolation is only available in **f-strings** (strings prefixed with `f`). Expressions inside `{}` are evaluated and converted to their string representation. Use `{{` and `}}` to produce literal braces. Normal strings treat `{` and `}` as literal characters.

**String kinds:**

| Prefix    | Kind       | Interpolation | Escapes |
|-----------|------------|---------------|---------|
| (none)    | Normal     | No            | Yes     |
| `f`       | Format     | Yes           | Yes     |
| `r`       | Raw        | No            | No      |
| `b`       | Byte       | No            | Yes     |
| `"""`     | Multi-line | No            | Yes     |
| `f"""`    | Multi-line format | Yes    | Yes     |
| `c`       | C string   | No            | Yes     |

Type: `String` (owned, heap-allocated). String literals that appear in borrow position are implicitly `str` (immutable string slice).

#### None Literal

```
None
```

Type: `Option[T]` for some inferred `T`.

### 3.4 Operators and Punctuation

**Arithmetic:**

| Symbol | Name            |
|--------|-----------------|
| `+`    | Addition / String concat / Vector concat |
| `-`    | Subtraction / Negation |
| `*`    | Multiplication / Dereference |
| `/`    | Division        |
| `%`    | Remainder (sign follows dividend) |
| `mod`  | Modulo (sign follows divisor) |

**Bitwise:**

| Symbol | Name            |
|--------|-----------------|
| `&`    | Bitwise AND     |
| `\|`   | Bitwise OR      |
| `^`    | Bitwise XOR     |
| `~`    | Bitwise NOT     |
| `<<`   | Left shift      |
| `>>`   | Right shift     |

**Comparison:**

| Symbol | Name               |
|--------|--------------------|
| `==`   | Equal              |
| `!=`   | Not equal          |
| `<`    | Less than          |
| `>`    | Greater than       |
| `<=`   | Less than or equal |
| `>=`   | Greater than or equal |

**Logical:**

| Keyword | Name        |
|---------|-------------|
| `and`   | Logical AND |
| `or`    | Logical OR  |
| `not`   | Logical NOT |

**Ownership:**

| Symbol/Keyword | Name           | Meaning                |
|----------------|----------------|------------------------|
| (bare)         | Immutable borrow | Read-only access       |
| `&` or `mutable` | Mutable borrow | Read+write access    |
| `!` or `move`  | Move           | Ownership transfer   |

Both operator and keyword forms are equivalent and may be used interchangeably.

**Range:**

| Symbol | Name               |
|--------|--------------------|
| `..`   | Exclusive range    |
| `..=`  | Inclusive range     |

**Optional:**

| Symbol | Name              |
|--------|-------------------|
| `?.`   | Optional chaining |
| `??`   | Default operator  |

**Assignment:**

| Symbol | Name               |
|--------|--------------------|
| `=`    | Assignment         |
| `+=`   | Add-assign         |
| `-=`   | Subtract-assign    |
| `*=`   | Multiply-assign    |
| `/=`   | Divide-assign      |
| `%=`   | Remainder-assign   |
| `&=`   | Bitwise AND-assign |
| `\|=`  | Bitwise OR-assign  |
| `^=`   | Bitwise XOR-assign |
| `<<=`  | Left shift-assign  |
| `>>=`  | Right shift-assign |

**Delimiters:**

| Symbol | Name          |
|--------|---------------|
| `(`  `)` | Parentheses |
| `[`  `]` | Brackets    |
| `{`  `}` | Braces      |
| `:`    | Colon (block opener, type annotation) |
| `,`    | Comma         |
| `.`    | Dot (field access, method call) |
| `@`    | At (attribute prefix) |
| `\|`   | Pipe (or-pattern separator) |
| `_`    | Underscore (wildcard) |

---

## 4. Types

### 4.1 Primitive Types

| Type      | Size    | Description                     |
|-----------|---------|---------------------------------|
| `int`     | 64-bit  | Signed integer (alias for `int64`) |
| `int8`    | 8-bit   | Signed integer                  |
| `int16`   | 16-bit  | Signed integer                  |
| `int32`   | 32-bit  | Signed integer                  |
| `int64`   | 64-bit  | Signed integer                  |
| `uint`    | 64-bit  | Unsigned integer (alias for `uint64`) |
| `uint8`   | 8-bit   | Unsigned integer                |
| `uint16`  | 16-bit  | Unsigned integer                |
| `uint32`  | 32-bit  | Unsigned integer                |
| `uint64`  | 64-bit  | Unsigned integer                |
| `float`   | 64-bit  | Floating-point (alias for `float64`) |
| `float32` | 32-bit  | Floating-point                  |
| `float64` | 64-bit  | Floating-point                  |
| `bool`    | 1 byte  | Boolean (`true` or `false`)     |
| `char`    | 4 bytes | Unicode scalar value            |
| `str`     | —       | Immutable string slice (borrowed) |
| `String`  | —       | Owned, heap-allocated string    |
| `void`    | 0       | No value (unit type)            |

All primitive numeric types and `bool` and `char` are **Copy** types — they are implicitly copied on assignment and do not require `!` or `move` to transfer.

### 4.2 Compound Types

#### Tuples

```ebnf
tuple_type = "(" type "," type { "," type } ")" ;
```

A fixed-size, heterogeneous sequence. Fields are accessed by index: `.0`, `.1`, etc.

```gorget
(int, String) pair = (42, "hello")
int x = pair.0
```

**Bare tuple return types.** In function return position, parentheses are optional — the return type can be written as a comma-separated list of types directly before the function name:

```gorget
# Equivalent forms:
(str, int, bool) parse(str line): ...
str, int, bool parse(str line): ...    # bare form — preferred
```

The bare form is only valid in the return-type position of a function definition. In all other positions (variable types, function parameters, struct fields), parentheses are required.

#### Arrays

```ebnf
array_type = type "[" const_expr "]" ;
```

A fixed-size, homogeneous sequence. Size must be a compile-time constant. Use this only when you need a fixed C-level array; for dynamic lists, use `auto` with an array literal to get a `Vector[T]`.

```gorget
int[5] arr = [1, 2, 3, 4, 5]   # fixed C array
auto v = [1, 2, 3, 4, 5]       # Vector[int] — dynamic, supports push/pop/etc.
```

#### Slices

```ebnf
slice_type = type "[" "]" ;
```

A borrowed view into contiguous memory. Does not own its data.

```gorget
int[] slice = arr[1..4]
```

#### Function Types

```ebnf
function_type = type "(" [ type { "," type } ] ")" ;
```

The return type comes first, followed by parameter types in parentheses. This mirrors function declaration syntax.

```gorget
int(int, int) adder = add      # function that takes two ints, returns int
void() callback = some_func    # function that takes nothing, returns void
```

#### Callable Trait Types

Callable trait types represent callable values — closures, function references, or callable objects — with type-safe dispatch through vtables. Three variants exist, forming a coercion hierarchy:

```gorget
Callable[int(int)]      # immutable: reads captures but cannot mutate them
MutCallable[int(int)]   # mutable: may mutate captured variables
ConsumeCallable[int(int)]  # consuming: takes ownership of captures (single use)
```

**Hierarchy coercion** (upward is OK, downward is an error):
- `Callable` → `MutCallable` → `ConsumeCallable`
- A `Callable` closure can be passed where `MutCallable` or `ConsumeCallable` is expected.
- A `MutCallable` closure can be passed where `ConsumeCallable` is expected.

**Usage as parameters:**

```gorget
int apply(Callable[int(int)] f, int x):
    return f(x)

int apply_mut(MutCallable[int(int)] f, int x):
    return f(x)
```

**Usage as local variables:**

```gorget
Callable[int(int)] triple = (n): n * 3
int result = triple(4)  # 12
```

**Closure kind auto-classification:**
- No mutations to captures → `Callable`
- Assigns to captures → `MutCallable`
- Move closure (`!` or `move` prefix) → `ConsumeCallable`

Named functions and non-capturing closures are always `Callable` and coerce to any variant.

**Boxed callable trait objects:**

`Box[Callable[sig]]` creates a heap-allocated, type-erased callable — enabling heterogeneous closure collections and callback storage:

```gorget
# Heap-allocated callable via Box.new
Box[Callable[int(int)]] f = Box.new((n): n * 2)
int r = f(5)  # 10

# Auto-boxing: bare closure assigned to Box[Callable] is boxed automatically
Box[Callable[int(int)]] g = (n): n + 100
```

`Box[MutCallable[sig]]` and `Box[ConsumeCallable[sig]]` work similarly for mutable and consuming closures.

### 4.3 Named Types

```ebnf
named_type = IDENTIFIER [ "[" type { "," type } "]" ] ;
```

Named types include structs, enums, and type aliases, optionally with generic arguments in square brackets.

```gorget
Vector[int] nums = Vector.new()
Pair[String, int] entry = Pair("key", 42)
```

### 4.4 Special Types

#### Self Type

`Self` refers to the implementing type within a trait or equip block.

#### Inferred Type

`auto` requests that the compiler infer the type from the initializer expression. Only valid for local variable declarations.

```gorget
auto x = 42          # inferred as int
auto name = "hello"  # inferred as String
```

### 4.5 Smart Pointer Types

| Type              | Description                              |
|-------------------|------------------------------------------|
| `Box[T]`          | Single-owner heap allocation             |
| `Rc[T]`           | Reference-counted (single-threaded)      |
| `Arc[T]`          | Atomic reference-counted (thread-safe)   |
| `Weak[T]`         | Non-owning reference (for Rc/Arc)        |
| `Cell[T]`         | Interior mutability for Copy types       |
| `RefCell[T]`      | Interior mutability with runtime checks  |
| `Mutex[T]`        | Thread-safe interior mutability          |
| `RwLock[T]`       | Reader-writer lock                       |

### 4.6 Copy vs. Non-Copy Types

**Copy types** (implicitly copied, no `!` needed):
- All integer types (`int`, `int8`, ..., `uint64`)
- All float types (`float`, `float32`, `float64`)
- `bool`, `char`
- Tuples where all elements are Copy

**Non-Copy types** (require `!` or `move` to transfer ownership):
- `String`
- All structs
- All enums
- Collections (`Vector`, `Dict`, `HashMap`, etc.)

---

## 5. Items

Items are the top-level declarations in a module.

```ebnf
item = function_def | struct_def | enum_def | trait_def
     | equip_block | import_stmt | type_alias | newtype_def
     | const_decl | static_decl | extern_block ;
```

### 5.1 Functions

```ebnf
function_def = { attribute } [ "public" ] [ qualifiers ]
               return_type IDENTIFIER [ generic_params ]
               "(" [ param_list ] ")" [ throws_clause ]
               [ where_clause ] ( block | "=" expr NEWLINE | NEWLINE ) ;

qualifiers    = { "async" | "const" | "static" | "unsafe" } ;
return_type   = type { "," type } | "void" ;  (* bare tuple: str, int or (str, int) *)
param_list    = param { "," param } ;
param         = [ "live" [ "(" IDENTIFIER ")" ] ]
                type [ "&" | "!" | "mutable" | "move" ] IDENTIFIER [ "=" expr ]
              | "meta" IDENTIFIER ;   (* meta op parameter — see §19.23 *)
throws_clause = "throws" [ type ] ;
block         = ":" NEWLINE INDENT { statement } DEDENT ;
```

A function has:
- Zero or more **attributes** (e.g., `@test`, `@inline`)
- Optional **visibility** (`public`)
- Optional **qualifiers** (`async`, `const`, `static`, `unsafe`)
- A **return type** (or `void`)
- A **name**
- Optional **generic parameters** in `[]`
- A **parameter list** in `()`
- Optional **throws clause**
- Optional **where clause** for generic bounds
- A **body**: either an indented block, an expression body (`= expr`), or no body (declaration only, for trait methods and extern functions)

**Parameter ownership modes:**

| Declaration                      | Meaning           | Call site                      |
|----------------------------------|-------------------|--------------------------------|
| `Type name`                      | Immutable borrow  | `f(arg)`                       |
| `Type &name` or `Type mutable name` | Mutable borrow | `f(&arg)` or `f(mutable arg)` |
| `Type !name` or `Type move name`  | Move (ownership) | `f(!arg)` or `f(move arg)` |

The ownership annotation at the call site **must match** the parameter declaration. Mismatches are compile-time errors. Both operator (`&`/`!`) and keyword (`mutable`/`move`) forms are equivalent.

**Expression body shorthand:**

```gorget
int double(int x) = x * 2
```

Equivalent to a block body with `return`.

**Self parameters** (in equip blocks):

| Form                       | Meaning           |
|----------------------------|-------------------|
| `self`                     | Immutable borrow  |
| `&self` or `mutable self`  | Mutable borrow    |
| `!self` or `move self`   | Consuming (move)  |
| *(no self)*                | Static method     |

The `live` keyword on a parameter indicates that the return value borrows from that parameter's data (explicit lifetime annotation):

```gorget
str get(live Container self, int index)
```

Named borrow groups on parameters distinguish independent lifetimes when a function takes multiple borrowed inputs. The compiler uses body analysis to determine which groups flow to the return value, so moving a non-return-contributing source doesn't trigger false positives:

```gorget
str pick_first(live(a) str x, live(b) str y) where a outlives b:
    return x
```

The `where a outlives b` bound is enforced at call sites: if group `a`'s argument source is moved while group `b`'s source is still alive, the compiler emits an error.

See [section 9.6](#96-lifetime-inference-and-live-annotations) for guidance on when `live` is required vs. when inference handles it automatically.

On struct fields, `live` marks fields that hold borrowed data. The struct cannot outlive the referenced data:

```gorget
struct Parser:
    live str source
    int position
```

Named borrow groups also work on struct fields to distinguish independent lifetimes:

```gorget
struct Merger:
    live(left) str a
    live(right) str b
```

### 5.2 Structs

```ebnf
struct_def = { attribute } [ "public" ] "struct" IDENTIFIER
             [ generic_params ] ":" NEWLINE INDENT { field_def } DEDENT ;
field_def  = [ "public" ] type IDENTIFIER NEWLINE ;
```

Structs are product types with named fields. Fields are private by default.

```gorget
struct Point:
    float x
    float y

struct Pair[A, B]:
    A first
    B second
```

**Construction:** Positional arguments in declaration order.

```gorget
Point p = Point(1.0, 2.0)
Pair[int, String] pair = Pair[int, String](1, "hello")
```

### 5.3 Enums

```ebnf
enum_def = { attribute } [ "public" ] "enum" IDENTIFIER
           [ generic_params ] ":" NEWLINE INDENT { variant } DEDENT ;
variant  = IDENTIFIER [ "(" type { "," type } ")" ] NEWLINE ;
```

Enums are sum types (tagged unions). Variants can be:
- **Unit** variants: `Red`
- **Tuple** variants: `Some(T)`, `Custom(uint8, uint8, uint8)`

```gorget
enum Color:
    Red
    Green
    Blue
    Custom(uint8, uint8, uint8)

enum Option[T]:
    Some(T)
    None
```

**Construction:** User-defined enum variants are accessed via qualified syntax `EnumName.Variant(args)`. Parentheses are optional for nullary (no-payload) variants. Built-in prelude variants (`Ok`, `Error`, `Some`, `None`) are always available bare.

```gorget
Color c = Color.Red            # user enum — nullary, no parens needed
Color d = Color.Red()          # parens also accepted for nullary
Color e = Color.Custom(1,2,3)  # payload variant — parens required
Option[int] x = Some(42)      # prelude — bare OK
```

**Variant namespacing:** Variants are namespaced under their enum type to prevent name collisions when two enums share variant names. Generic enum variants (e.g., `Maybe[T].Just`) remain bare since they stay in scope.

```gorget
from gg.log import LogLevel, Logger

LogLevel lvl = LogLevel.Info
Result[int, str] err = Error("bad")    # prelude Error — unambiguous
match lvl:
    case LogLevel.Info:
        print("info")
    case LogLevel.Err:
        print("error")
```

**Dot-shorthand:** When the expected type is known from context (variable declaration, assignment, return, or function parameter), `.Variant` desugars to `EnumType.Variant`. Parentheses are optional for nullary variants:

```gorget
Color c = .Red                 # VarDecl: .Red → Color.Red
c = .Blue(42)                  # Assignment: payload needs parens
return .Green                  # Return: uses declared return type
print_color(.Green)            # Arg: uses parameter's declared type
match c:
    case .Red:                 # Pattern: nullary, no parens
        print("red")
    case .Blue(n):
        print("blue {n}")
```

**Glob import:** Use `EnumName.*` to bring a type's variants into bare scope:

```gorget
from gg.log import LogLevel.*    # imports LogLevel type + all variants bare

LogLevel lvl = Info              # bare nullary variant via glob import
match lvl:
    case Info:
        print("info")
    case Err:
        print("err")
```

### 5.4 Traits

```ebnf
trait_def  = { attribute } [ "public" ] "trait" IDENTIFIER
             [ generic_params ] [ "extends" trait_bound_list ]
             ":" NEWLINE INDENT { trait_item } DEDENT ;
trait_item = function_def | assoc_type ;
assoc_type = "type" IDENTIFIER [ ":" trait_bound_list ] NEWLINE ;
```

Traits define shared behavior. They may contain:
- **Method signatures** (no body — must be implemented)
- **Default method implementations** (with body — may be overridden)
- **Associated type declarations**

```gorget
trait Displayable:
    String to_string(self)

trait Comparable:
    int compare(self, Self other)

trait Iterator[T]:
    Option[T] next(&self)
```

**Trait inheritance:** The `extends` keyword declares supertrait requirements:

```gorget
trait Animal extends Displayable:
    str name(self)
    str sound(self)
```

### 5.5 Equip Blocks

```ebnf
equip_block = "equip" [ generic_params ] type [ "with" type ] [ "via" IDENTIFIER ]
              [ where_clause ] ":" NEWLINE INDENT { function_def | "pass" } DEDENT ;
```

Equip blocks attach methods to types. There are three forms:

**Inherent implementation** (methods directly on a type):

```gorget
equip Point:
    float distance(self, Point other):
        ...
    static Point origin():
        return Point(0.0, 0.0)
```

**Trait implementation** (satisfying a trait):

```gorget
equip Point with Displayable:
    String to_string(self):
        return "({self.x}, {self.y})"
```

**Delegation via field** (auto-forward unimplemented trait methods through a struct field):

```gorget
equip Outer with Showable via inner:
    pass
```

When `via field_name` is specified, any trait method not explicitly provided in the equip block is automatically delegated to the named field. The field's type must implement the target trait. Explicitly provided methods take priority over delegation.

```gorget
equip Wrapper with Describable via inner:
    str describe(self):
        return self.label   # explicit override
    # count() auto-forwarded to self.inner.count()
```

### 5.6 Imports

```ebnf
import_stmt = simple_import | grouped_import | from_import ;
simple_import  = "import" dotted_name NEWLINE ;
grouped_import = "import" dotted_name ".{" IDENTIFIER { "," IDENTIFIER } "}" NEWLINE ;
from_import    = "from" dotted_name "import" import_name { "," import_name } NEWLINE ;
import_name    = IDENTIFIER | IDENTIFIER ".*" ;
dotted_name    = IDENTIFIER { "." IDENTIFIER } ;
```

```gorget
import std.io
import gg.json
from std.conv import int_to_str, parse_int
from gg.log import LogLevel, Logger           # import type only (qualified variant access)
from gg.log import LogLevel.*, Logger         # import type + all variants bare (glob)
```

**Glob import (`EnumName.*`):** Imports the named type AND brings all its enum variants into bare scope. Useful when working extensively with a single enum. Glob-imported variants shadow prelude variants with the same name.

```gorget
from gg.log import LogLevel.*
LogLevel lvl = Info()        # bare — from glob import
LogLevel err = Err()         # bare — from glob import
```

### 5.7 Type Aliases

```ebnf
type_alias = "type" IDENTIFIER [ generic_params ] "=" type NEWLINE ;
```

Creates an alternative name for an existing type. The alias is interchangeable with the original.

```gorget
type Callback = int(int, int)
type StringMap[V] = Dict[String, V]
```

### 5.8 Newtypes

```ebnf
newtype_def = "newtype" IDENTIFIER "(" type ")" NEWLINE ;
```

Creates a distinct type wrapping another type. Zero runtime cost. Not interchangeable with the inner type.

```gorget
newtype Meters(float)
newtype UserId(int)
```

The inner value is accessed via `.0`.

### 5.9 Constants and Statics

```ebnf
const_decl  = [ "public" ] "const" type IDENTIFIER "=" expr NEWLINE ;
static_decl = [ "public" | "private" ] "static" type IDENTIFIER "=" expr NEWLINE ;
```

- `const`: compile-time constant. Inlined at every use site. Public by default.
- `static`: runtime value with global lifetime. One instance per program. **Private by default** — only accessible within the declaring module. Use `public static` to export.

```gorget
const int MAX_SIZE = 1024
const float PI = 3.14159265358979
static int counter = 0              # private to this module
public static int shared_counter = 0  # accessible to importers
```

### 5.10 Extern Blocks

```ebnf
extern_block = "extern" [ STRING_LITERAL ] ":" NEWLINE INDENT { function_decl } DEDENT ;
function_decl = type IDENTIFIER "(" [ param_list ] ")" NEWLINE ;
```

Declares foreign functions (FFI). The optional string specifies the ABI (default: `"C"`).

```gorget
extern "C":
    int printf(str format, ...)
    void free(RawPtr[void] ptr)
```

### 5.11 Attributes

```ebnf
attribute = "@" IDENTIFIER [ "(" attr_args ")" ] NEWLINE ;
attr_args = attr_arg { "," attr_arg } ;
attr_arg  = IDENTIFIER | STRING_LITERAL | IDENTIFIER "=" STRING_LITERAL ;
```

Attributes provide metadata to the compiler:

```gorget
@derive(Cloneable, Equatable, Hashable)
struct Point:
    float x
    float y

test "addition":
    assert add(2, 3) == 5

@inline
int fast_add(int a, int b) = a + b
```

**Derivable traits:**

- **Structs:** Equatable, Displayable, Cloneable, Hashable, Serializable, Deserializable, Default, From, TryFrom
- **Enums:** Equatable, Displayable, Cloneable, Hashable, Serializable, Deserializable

Note: `From` and `TryFrom` are only derivable for single-field structs (newtypes).

---

## 6. Statements

Statements are executed for their side effects. They appear inside function bodies and blocks.

```ebnf
statement = var_decl | expr_stmt | assign_stmt | compound_assign_stmt
          | return_stmt | throw_stmt | break_stmt | continue_stmt | pass_stmt
          | for_stmt | while_stmt | loop_stmt | if_stmt | match_stmt
          | with_stmt | unsafe_stmt | named_scope_stmt | item ;
```

### 6.1 Variable Declarations

```ebnf
var_decl = [ "const" | "shared" [ "(" shared_override ")" ] ] ( type | "auto" ) pattern "=" expr NEWLINE ;
shared_override = "rwlock" | "atomic" ;
```

Declares a new variable with an explicit type or inferred type (`auto`). Local variables are mutable by default; prefix with `const` for immutability. Note that function arguments follow the opposite convention: they are immutable borrows by default, requiring `&` for mutable access (see [Ownership](#43-ownership-rules)).

```gorget
int x = 5
const int y = 10
auto name = "gorget"
const auto pi = 3.14
```

**Shared bindings.** Prefix with `shared` to declare a binding that safely crosses concurrency boundaries (spawn/await). The compiler wraps the binding in `Shared[T]` (ARC) and optionally adds a `Mutex` or `RwLock` based on control-flow analysis of how the binding's borrows cross concurrency boundaries. User overrides (`shared(rwlock)`, `shared(atomic)`) skip CFA and use the specified primitive.

```gorget
shared int count = 0                           # CFA decides sync strategy
shared Config config = load_config()           # read-only across boundaries → ARC only
shared(rwlock) Dict[str, str] cache = Dict()   # user override: ARC + RwLock
shared(atomic) int flags = 0                   # user override: ARC + Atomic
```

See [Concurrency — Shared Bindings](#shared-bindings) for details on token semantics and CFA.

The pattern on the left side may be a simple binding or a destructuring pattern (see [Patterns](#8-patterns)).

**Bare tuple destructuring with `auto`.** When the type is `auto`, a comma-separated list of bindings can appear directly after `auto` without parentheses:

```gorget
auto (x, y) = get_pair()   # parenthesized
auto x, y = get_pair()     # bare — preferred
auto a, _, c = triple()    # wildcards allowed
```

### 6.2 Assignment

```ebnf
assign_stmt = expr "=" expr NEWLINE ;
```

Assigns a new value to a mutable variable, field, or index location.

```gorget
x = 10
point.x = 3.14
arr[0] = 42
```

Reassigning a moved variable revives it:

```gorget
String s = "hello"
String t = !s        # s is now moved
s = "world"          # s is live again
```

### 6.3 Compound Assignment

```ebnf
compound_assign_stmt = expr ( "+=" | "-=" | "*=" | "/=" | "%="
                            | "&=" | "|=" | "^=" | "<<=" | ">>=" ) expr NEWLINE ;
```

```gorget
x += 1
total *= factor
x &= 0xFF
x <<= 4
```

### 6.4 Expression Statements

```ebnf
expr_stmt = expr NEWLINE ;
```

An expression evaluated for its side effects. The value is discarded.

```gorget
print("hello")
list.push(42)
```

### 6.5 Return

```ebnf
return_stmt = "return" [ expr { "," expr } ] NEWLINE ;
```

Exits the enclosing function, optionally with a value. Must appear inside a function.

**Bare tuple return.** Returning multiple values can be written as a comma-separated list without parentheses:

```gorget
str, int parse(str line):
    return "key", 42        # bare — preferred
    # return ("key", 42)    # parenthesized — also valid
```

The list is desugared into a `TupleLiteral` and the return type must be a matching tuple type.

### 6.6 Throw

```ebnf
throw_stmt = "throw" expr NEWLINE ;
```

Raises an error. Must appear inside a function declared with `throws`.

```gorget
Record parse_line(str line) throws ParseError:
    if line.is_empty():
        throw ParseError("empty line")
    return parse(line)
```

### 6.7 Break and Continue

```ebnf
break_stmt    = "break" [ expr ] NEWLINE ;
continue_stmt = "continue" NEWLINE ;
```

- `break` exits the innermost enclosing loop. An optional expression provides the loop's value (for loop-as-expression).
- `continue` skips to the next iteration of the innermost enclosing loop.

Both are compile-time errors outside a loop.

### 6.8 Pass

```ebnf
pass_stmt = "pass" NEWLINE ;
```

A no-op statement. Used as a placeholder in empty blocks.

### 6.9 If Statement

```ebnf
if_stmt = "if" expr ":" block
          { "elif" expr ":" block }
          [ "else" ":" block ] ;
```

Conditional execution. The condition must be of type `bool`. The `elif` keyword chains additional conditions.

```gorget
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")
```

### 6.10 Match Statement

```ebnf
match_stmt = "match" expr ":" NEWLINE INDENT
             { match_item }
             [ "else" ":" block ] DEDENT ;
match_item = "case" pattern [ "if" expr ] ":" block
           | "meta" "for" IDENTIFIER { "," IDENTIFIER } "in" expr ":"
             NEWLINE INDENT "case" pattern ":" block DEDENT ;
```

Pattern matching on a value. Arms are tried in order; the first matching pattern executes. The `else` arm catches anything not matched by preceding `case` arms.

```gorget
match color:
    case Red:
        print("red")
    case Custom(r, g, b):
        print("rgb({r}, {g}, {b})")
    else:
        print("other")
```

**Guards:** A `case` arm may include an `if` guard expression:

```gorget
match value:
    case x if x > 100:
        print("large")
    case 0:
        print("zero")
    else:
        print("other")
```

### 6.11 For Loop

```ebnf
for_stmt = "for" pattern { "," pattern } "in" [ "&" | "!" | "mutable" | "move" ] expr ":" block
           [ "else" ":" block ] ;
```

Iterates over a collection or range. The optional ownership modifier before the iterable controls ownership:

| Form                               | Meaning                              |
|------------------------------------|--------------------------------------|
| `for x in coll`                     | Immutable borrow (collection intact) |
| `for x in &coll` or `for x in mutable coll` | Mutable borrow (modify in-place) |
| `for x in !coll` or `for x in move coll`   | Move (consumes collection)       |

The optional `else` block runs if the loop completes without `break` (Python-style).

```gorget
for item in collection:
    if item.matches():
        break
else:
    print("no match found")
```

**Bare tuple pattern.** When iterating over a collection of tuples, the loop variable can be a comma-separated list of bindings without parentheses:

```gorget
Vector[(int, str)] pairs = ...
for i, s in pairs:          # bare — preferred
    print("{i}: {s}")
# for (i, s) in pairs:      # parenthesized — also valid
```

### 6.12 While Loop

```ebnf
while_stmt = "while" expr ":" block [ "else" ":" block ] ;
```

Loops while the condition is `true`. Supports `else` (runs if loop exits normally without `break`).

### 6.13 Loop (Infinite)

```ebnf
loop_stmt = "loop" ":" block ;
```

Infinite loop. Exit with `break`.

### 6.14 With Statement

The `with` keyword introduces a scoped block. It has two forms that are distinguished by the presence or absence of `as`:

#### Resource management (`with expr as name:`)

```ebnf
with_stmt   = "with" with_binding { "," with_binding } ":" block ;
with_binding = expr "as" IDENTIFIER ;
```

Scoped resource management. The bound resource is automatically cleaned up (Drop called) when the block exits.

```gorget
with File.open(path) as file:
    String content = file.read_all().unwrap()
    print(content)
# file is closed here
```

For allocator-specific `with` binding semantics (scoped allocation, escape analysis), see `std.alloc` in §15.3.

#### Shared variable access (`with name:`)

```ebnf
with_shared_stmt = "with" IDENTIFIER { "," IDENTIFIER } ":" block ;
```

Acquires the synchronization primitive (Mutex, RwLock, or atomic) for a `shared` variable, making its current value available inside the block. The lock is released when the block exits. This is the **only** way to access a shared variable — the compiler rejects bare reads or writes outside a `with` block.

```gorget
shared int counter = 0

async void main():
    # ... spawn tasks that modify counter ...
    with counter:
        print(counter)      # guaranteed fresh value
        counter += 1        # safe: lock is held
    # lock released here
```

**Freshness, not atomicity.** If the block contains a yield point (`await`, `sleep`, blocking I/O), the lock is released at the yield and reacquired on resumption. The variable is auto-refreshed to the latest value after reacquisition, but any local snapshots taken before the yield may be stale. The compiler warns about common pitfalls — see §7.25 for details on stale-value and check-then-act warnings.

**How to tell the two forms apart.** The `as` keyword is the disambiguator:
- `with expr as name:` — resource management. Creates a **new** binding from an expression. The expression is evaluated once, and `name` is dropped at block exit.
- `with name:` — shared access. References an **existing** `shared` variable. Acquires its lock, auto-refreshes on yield, releases at block exit.

### 6.15 Unsafe Block

```ebnf
unsafe_stmt = "unsafe" ":" block ;
```

Opts into operations the compiler cannot verify: raw pointer dereferencing, FFI calls, mutable static access.

### 6.16 Named Scope Block

```ebnf
named_scope_stmt = IDENTIFIER ":" NEWLINE INDENT stmt* DEDENT ;
```

A **named scope** is a mid-function drop zone. Variables declared inside the block are dropped when the block exits — exactly like variables at function return, but at an earlier point. The name is documentation-only; it is not a bindable identifier.

```gorget
void main():
    data = load_data()

    workers:                                  # scope named "workers"
        Task[void] t1 = spawn process(data)
        Task[void] t2 = spawn process(data)
    # Task[void]__drop(t2), Task[void]__drop(t1) — both joined here

    cleanup:                                  # scope named "cleanup"
        File f = File.open("out.txt").unwrap()
        write_results(&f)
    # File__drop(f) — file closed here

    print("done")
```

**Thread safety via RAII.** `Task[T]`'s drop implementation joins the thread. Tasks created inside a named scope are joined before the block exits, so outer borrows remain valid for the entire scope:

```gorget
void crunch(str data):
    workers:
        # data is borrowed inside — safe because tasks are joined at scope exit
        Task[void] t1 = spawn analyse(data)
        Task[void] t2 = spawn summarise(data)
    # both threads joined here; data still lives
    print("crunch done")
```

**Comparison with `with X as y:`** — `with` manages a single resource acquired at block entry. A named scope is a general drop boundary for any number of variables; there is no acquisition step.

Variables declared *outside* the named scope and read or borrowed *inside* are perfectly valid — they outlive the scope by definition.

### 6.17 Assert

```ebnf
assert_stmt = "assert" expr [ "," expr ] NEWLINE ;
```

`assert` checks a condition and panics with a diagnostic message if it fails. **Assertions are always enabled** — they are never stripped in release builds.

```gorget
void process(Vector[int] data):
    assert data.len() > 0, "data must not be empty"
    assert is_sorted(data)
```

When the condition is a binary comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`), the compiler captures both operand values and includes them in the failure message:

```
assertion failed: left == right
  left:  42
  right: 43
```

This introspection works for all types that implement `Formatter` — primitives, strings, enums, and structs. No `assert_eq` or specialized assertion functions are needed.

If a custom message is provided (second argument), it replaces the generated diagnostic.

For expensive invariant checks that should not run in production, wrap them in a `@[debug_only]` function:

```gorget
@[debug_only]
void check_tree_invariants(Tree t):
    assert t.is_balanced()
    assert t.size() == t.count_nodes()
```

### 6.18 Assert Return (Postconditions)

```ebnf
assert_return_stmt = "assert" "return" expr NEWLINE ;
```

`assert return` declares a postcondition: a property that the function's return value must satisfy. The compiler inserts the check before every `return` statement in the function body.

```gorget
int clamp(int value, int lo, int hi):
    assert lo <= hi                    # precondition
    assert return >= lo                # postcondition
    assert return <= hi                # postcondition

    if value < lo: return lo
    if value > hi: return hi
    return value
```

At each `return` site, the compiler binds `return` to the return value and evaluates the postcondition assertions. If any postcondition fails, the function panics before returning.

**Rules:**
- `assert return` is invalid in `void` functions (compile error).
- For tuple returns, `return` refers to the whole tuple: `assert return.0 <= return.1`.
- Postconditions use the same always-on / `@[debug_only]` semantics as regular `assert`.
- `assert return` statements must appear before any non-assert statement in the function body (alongside precondition asserts).

---

## 7. Expressions

Expressions produce values. Many Gorget constructs are expressions, including `if`, `match`, `do`, and closures.

### 7.1 Operator Precedence

From lowest to highest precedence:

| Precedence | Operators / Forms            | Associativity |
|------------|------------------------------|---------------|
| 0          | `rethrow` `catch`            | Right         |
| 1          | `or`                         | Left          |
| 2          | `and`                        | Left          |
| 3          | `not`                        | Unary (prefix)|
| 4          | `\|` (bitwise OR)            | Left          |
| 5          | `^` (bitwise XOR)            | Left          |
| 6          | `&` (bitwise AND)            | Left          |
| 7          | `==` `!=` `<` `>` `<=` `>=` `is` `in` | Non-associative |
| 8          | `??`                         | Left          |
| 9          | `..` `..=`                   | Non-associative |
| 10         | `<<` `>>`                    | Left          |
| 11         | `+` `-` `+%` `-%`            | Left          |
| 12         | `*` `/` `%` `mod` `*%`       | Left          |
| 13         | Unary `-` `~` `!` `&` `*`   | Unary (prefix)|
| 14         | `?.` `.` `()` `[]`           | Left          |
| 15         | Atoms (literals, identifiers, grouped expressions) | — |

### 7.2 Literals

See [Section 3.3](#33-literals) for syntax. Literal expressions produce values of the corresponding type.

### 7.3 Identifiers and Paths

```ebnf
identifier_expr = IDENTIFIER ;
path_expr       = IDENTIFIER { "." IDENTIFIER } ;
```

An identifier resolves to a variable, function, type, or constant in scope. A path accesses a nested name (e.g., `Color.Red`).

### 7.4 Unary Operators

```ebnf
unary_expr = ( "-" | "not" | "~" | "*" ) expr ;
```

| Operator | Name        | Operand type     | Result type      |
|----------|-------------|------------------|------------------|
| `-`      | Negation    | Numeric          | Same as operand  |
| `not`    | Logical NOT | `bool`           | `bool`           |
| `~`      | Bitwise NOT | Integer          | Same as operand  |
| `*`      | Dereference | Pointer/smart ptr | Inner type      |

### 7.5 Binary Operators

```ebnf
binary_expr = expr op expr ;
op = "+" | "-" | "*" | "/" | "%" | "+%" | "-%" | "*%"
   | "==" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or" | "in"
   | "&" | "|" | "^" | "<<" | ">>" ;
```

Arithmetic operators require matching numeric types. Comparison operators produce `bool`. Logical operators require `bool` operands. Bitwise operators require matching integer types and produce the same type.

**Wrapping arithmetic operators** (`+%`, `-%`, `*%`) perform the same operation as `+`, `-`, `*` but are guaranteed to wrap on overflow rather than panic, regardless of the overflow mode. In default (checked) mode, `+` panics on overflow while `+%` wraps silently. Under `directive overflow=wrap` (or `--overflow=wrap`), all arithmetic wraps and the wrapping operators behave identically to their non-wrapping counterparts. Compound assignment forms (`+%=`, `-%=`, `*%=`) are also available.

The `+` and `+=` operators also work on strings, producing a new concatenated string:

```gorget
str greeting = "hello" + " " + "world"   # "hello world"
str s = "foo"
s += "bar"                                # "foobar"
```

For building strings from many parts, prefer collecting into a `Vector[str]` and using `join` (§15.2):

```gorget
Vector[str] parts = Vector[str]()
parts.push("hello")
parts.push("world")
str result = " ".join(parts)   # "hello world"
```

The `+` operator on Vectors produces a new concatenated Vector without modifying the originals:

```gorget
auto a = [1, 2, 3]
auto b = [4, 5]
auto c = a + b   # [1, 2, 3, 4, 5]
```

The `in` operator tests membership in a range or collection:

```gorget
if value in 1..=100:
    print("in range")
```

### 7.6 Function Calls

```ebnf
call_expr = expr [ "[" type { "," type } "]" ] "(" [ arg_list ] ")" ;
arg_list  = call_arg { "," call_arg } ;
call_arg  = [ IDENTIFIER "=" ] [ "&" | "!" | "mutable" | "move" ] expr ;
```

The optional `[...]` provides explicit generic type arguments. Arguments may use ownership annotations matching the parameter declarations. Both operator and keyword forms are accepted.

```gorget
add(1, 2)
max[int](a, b)
consume(!value)          # or: consume(move value)
modify(&data)            # or: modify(mutable data)
create_user("Alice", 30, admin = true)
```

**Named arguments:** Once a named argument appears, all subsequent arguments must also be named.

### 7.7 Method Calls

```ebnf
method_call = expr "." IDENTIFIER [ "[" type { "," type } "]" ] "(" [ arg_list ] ")" ;
```

Calls a method on a receiver. The receiver is automatically borrowed (no `&`/`!` at the call site).

```gorget
point.distance(other)
list.push(42)
name.to_upper()
```

### 7.8 Field Access

```ebnf
field_access = expr "." IDENTIFIER ;
tuple_access = expr "." INT_LITERAL ;
```

Accesses a named struct field or a positional tuple/newtype field.

```gorget
point.x
pair.0
```

### 7.9 Index Access

```ebnf
index_expr = expr "[" expr "]" ;
```

Accesses an element by index.

```gorget
arr[0]
map["key"]
```

### 7.10 Range Expressions

```ebnf
range_expr = [ expr ] ( ".." | "..=" ) [ expr ] ;
```

| Form      | Meaning                    |
|-----------|----------------------------|
| `a..b`    | Half-open: a, a+1, ..., b-1 |
| `a..=b`   | Inclusive: a, a+1, ..., b  |
| `a..`     | From a, unbounded end      |
| `..b`     | Unbounded start, up to b-1 |

### 7.11 Optional Chaining

```ebnf
optional_chain = expr "?." IDENTIFIER ;
```

Short-circuits to `None` if the receiver is `None`; otherwise accesses the field.

```gorget
auto city = user?.address?.city    # Option[String]
```

### 7.12 Default Operator

```ebnf
default_op = expr "??" expr ;
```

Unwraps the left operand if `Some`; otherwise evaluates the right operand. The right-hand side is **lazy** — it is only evaluated when the left-hand side is `None`.

```gorget
String name = user?.name ?? "anonymous"
```

### 7.13 Raw Expression

```ebnf
raw_expr = "raw" expr ;
```

Captures a potentially-throwing call as a `Result[T, E]` instead of auto-propagating:

```gorget
auto result = raw read_file(path)
match result:
    case Ok(content): print(content)
    case Error(e): print("Error: {e}")
```

### 7.14 Move Expression

```ebnf
move_expr = ( "!" | "move" ) expr ;
```

Transfers ownership of a value. The source variable becomes invalid after the move. Both `!` and `move` keyword are equivalent.

```gorget
String s2 = !s1          # s1 is invalid after this
String s3 = move s2    # equivalent keyword form
consume(!data)            # data is moved into consume
consume(move data)      # equivalent keyword form
```

### 7.15 Mutable Borrow Expression

```ebnf
mut_borrow_expr = ( "&" | "mutable" ) expr ;
```

Creates a mutable borrow of a value. The original variable remains valid but cannot be accessed while the borrow is active. Both `&` and `mutable` keyword are equivalent.

```gorget
modify(&data)          # operator form
modify(mutable data)   # keyword form
```

### 7.16 Type Cast

```ebnf
as_expr = expr "as" type ;
```

Converts between types. The following cast pairs are allowed:

| From | To | Behavior |
|---|---|---|
| `int` / `i8` / `i16` / `i32` | `float` | Exact for small values; may lose precision for large integers |
| `float` | `int` / `i8` / `i16` / `i32` | Truncates toward zero |
| `int` ↔ `i8` / `i16` / `i32` | (each other) | Narrows or widens; narrowing may truncate |
| `uint8` / `u16` / `u32` / `uint64` | `int` / `float` | Unsigned to signed/float widening |
| `int` / `float` | `uint8` / `u16` / `u32` / `uint64` | Signed to unsigned; negative values wrap |
| `char` | `int` | Unicode codepoint value |
| `int` | `char` | Codepoint to character (must be valid Unicode) |
| `int` | `uint8` | Common for byte manipulation |
| `bool` | `int` | `true` → 1, `false` → 0 |

Casts between unrelated types (e.g., `str as int`) are a compile error. Use `str.to_int()` for parsing.

```gorget
float f = 42 as float       # int → float
int n = 3.14 as int          # float → int (truncates: 3)
int code = 'A' as int        # char → int (65)
uint8 b = 255 as uint8       # int → byte
```

### 7.17 Pattern Test (`is`)

```ebnf
is_expr = expr "is" [ "not" ] pattern ;
```

Tests whether a value matches a pattern. Produces `bool`. If the pattern introduces bindings, they are available in the subsequent `then` block of an `if` statement.

```gorget
if result is Ok(value):
    use(value)

if color is not Red:
    print("not red")
```

### 7.18 If Expression

```ebnf
if_expr = "if" expr ":" expr
          { "elif" expr ":" expr }
          "else" ":" expr ;
```

Evaluates to the value of the taken branch. All branches must produce the same type.

```gorget
int abs_val = if x >= 0: x else: -x
```

### 7.19 Match Expression

```ebnf
match_expr = "match" expr ":" NEWLINE INDENT
             { "case" pattern [ "if" expr ] ":" expr NEWLINE }
             [ "else" ":" expr NEWLINE ] DEDENT ;
```

Like a match statement, but each arm produces a value.

```gorget
String label = match color:
    case Red: "red"
    case Green: "green"
    else: "other"
```

### 7.20 Do Expression

```ebnf
do_expr = "do" ":" block ;
```

An expression block. The value of the last expression in the block becomes the block's value. The last statement may be an `if` or `match` whose branches produce values.

```gorget
int result = do:
    int a = compute_a()
    int b = compute_b()
    a + b
```

### 7.21 Closures

```ebnf
closure = [ "!" | "move" ] [ "async" ] "(" [ closure_param_list ] ")" ":" ( expr | block ) ;
closure_param_list = closure_param { "," closure_param } ;
closure_param = [ type ] [ "&" | "!" | "mutable" | "move" ] IDENTIFIER ;
```

Anonymous functions that capture variables from their environment.

```gorget
auto doubled = numbers.map((x): x * 2)
auto sum = pairs.map((a, b): a + b)
auto typed = strings.map((String s): s.parse[int]())
```

**Move closures:** Prefix `!` or `move` forces all captured variables to be moved into the closure:

```gorget
auto handle = thread.spawn(!(x):          # operator form
    print("value: {x}")
)
auto handle = thread.spawn(move (x):    # keyword form
    print("value: {x}")
)
```

**Multi-line closures:**

```gorget
auto process = (int x):
    int result = x * 2
    result += 1
    result
```

**Implicit `it` closures:** For single-parameter closures, the implicit `it` parameter avoids boilerplate:

```gorget
auto doubled = numbers.map(it * 2)
auto names = users.filter(it.age >= 18).map(it.name)
```

Rules for `it`:
- Only valid inside closures with exactly one parameter
- No explicit parameter list needed when `it` is used
- `it` is always an immutable borrow
- In nested closures, `it` refers to the innermost closure's parameter

### 7.22 Comprehensions

#### List Comprehension

```ebnf
list_comp = "[" expr "for" pattern "in" [ "&" | "!" | "mutable" | "move" ] expr [ "if" expr ] "]" ;
```

```gorget
Vector[int] squares = [x * x for x in 0..10]
Vector[int] evens = [x for x in 0..100 if x % 2 == 0]
```

#### Dict Literal

```ebnf
dict_literal = "{" [ expr ":" expr { "," expr ":" expr } [ "," ] ] "}" ;
```

```gorget
auto ages = {"alice": 30, "bob": 25, "carol": 35}
Dict[str, int] empty = {}
```

Dict literals create a `Dict[K, V]` (insertion-order-preserving). Types are inferred from the first key-value pair. Empty dict literals require a type annotation.

#### Dict Subscript Access

Dicts and HashMaps support subscript read and write with `d[key]` syntax:

```gorget
auto d = {"x": 10, "y": 20}
print("{d["x"]}")      # read: 10
d["x"] = 99            # write (update)
d["z"] = 30            # write (insert)
```

Reading a missing key aborts with a `KeyError`. Use `.get_or(key, default)` for safe access.

#### Dict Comprehension

```ebnf
dict_comp = "{" expr ":" expr "for" variables "in" expr [ "if" expr ] "}" ;
```

```gorget
Dict[String, int] lengths = {s: s.len() for s in words}
```

#### Set Comprehension

```ebnf
set_comp = "{" expr "for" IDENTIFIER "in" expr [ "if" expr ] "}" ;
```

```gorget
HashSet[int] unique = {x * x for x in 1..=10}
```

### 7.23 Collection Literals

```ebnf
array_literal = "[" [ expr { "," expr } [ "," ] ] "]" ;
tuple_literal = "(" expr "," expr { "," expr } [ "," ] ")" ;
```

Array literals with an inferred type (`auto`) produce a `Vector[T]` (dynamic array). With an explicit fixed-size array type (e.g. `int[3]`), they produce a C-level fixed array.

```gorget
auto v = [1, 2, 3]       # Vector[int] — supports push, pop, len, etc.
int[3] a = [1, 2, 3]     # Fixed C array — no dynamic methods
Vector[int] w = [4, 5]   # Explicit Vector type also works
```

Vectors support concatenation with the `+` operator, which returns a new Vector without modifying the originals:

```gorget
auto combined = v + w     # [1, 2, 3, 4, 5]
```

Tuple literals produce tuple types.

### 7.24 Struct Literals

```ebnf
struct_literal = IDENTIFIER "(" [ expr { "," expr } ] ")" ;
```

Constructs a struct with positional arguments in field declaration order.

```gorget
Point p = Point(1.0, 2.0)
Person alice = Person("Alice", 30)
```

### 7.25 Await and Spawn

```ebnf
await_expr = "await" expr | expr ".await()" ;
spawn_expr = "spawn" expr ;
```

- `await` suspends until an async operation completes. Both prefix (`await expr`) and postfix (`expr.await()`) forms are supported and equivalent. Must appear inside an `async` function. Combining both on the same expression (`await expr.await()`) is a compile error.
- `spawn` launches an async task concurrently, returning a `Task[T]`.

The postfix syntax chains naturally with method calls; the prefix syntax makes suspension points more visible:

```gorget
# Prefix await — suspension point stands out
int result = await add_async(3, 4)
String data = await fetch("https://example.com")

# Postfix .await() chains naturally
String body = http.get(url).await().text()
String data = fetch("https://example.com").await()
int result = add(f().await(), g().await()).await()

# ERROR: double await
int x = await fetch().await()   # compile error: expression is awaited twice
```

#### Suspension-Point Safety

An `.await()` expression is a **suspension point** — execution may pause and resume later, potentially on a different thread. To prevent dangling references and data races, the compiler enforces that **references to local variables** may not live across `.await()` points. Parameters are exempt: when a caller directly awaits an async function, the caller is blocked and all parameters remain alive throughout.

**What can cross an `.await()`:**
- **Owned types** (`String`, structs, enums, collections) — they own their data, so the data moves with the suspended state.
- **Copy types** (`int`, `float`, `bool`, `char`) — trivially duplicated, no pointers involved.
- **Static string literals** (`str s = "hello"`) — point to program-global storage that is always valid.
- **`str` parameters** — the caller is blocked at the direct-await call site, so `str` params and any `str` derived from them remain alive across the suspension.

**What cannot cross an `.await()`:**
- **`str` derived from a local `String`** — the local variable owns the data; a borrow of it cannot outlive the variable's scope across a suspension point.
- **`&T` references to local variables** — same reasoning.

```gorget
# OK: owned int crosses await
async int compute():
    int x = 42
    some_task().await()
    return x               # fine: int is Copy

# OK: static str crosses await
async void greet():
    str msg = "hello"
    some_task().await()
    print(msg)             # fine: "hello" is a static literal

# OK: str parameter crosses await (caller is blocked, param stays alive)
async void process(str name):
    some_task().await()
    print(name)            # fine: caller is blocked, name is live

# OK: str derived from a parameter is also safe
async void process2(str data):
    str slice = get_prefix(data)
    some_task().await()
    print(slice)           # fine: data (and thus slice) is live

# ERROR: str borrowed from a local variable
async void process_local():
    String owned = String.from("hello")
    str s = owned.as_str()
    some_task().await()
    print(s)               # error: s borrows from local `owned`

# FIX: use the owned String directly
async void process_fixed():
    String owned = String.from("hello")
    some_task().await()
    print(owned)           # fine: String owns its data
```

**`spawn` supports direct function calls and closures.** The compiler checks that all captured variables and arguments are safe to send across threads:

```gorget
spawn worker(42)         # OK — direct function call
spawn obj.method()       # ERROR — method calls not supported
spawn get_fn()(x)        # ERROR — indirect call
```

**Closures can be spawned if their captures are safe** (owned or Copy types). The compiler tracks each closure's capture set and rejects closures that capture borrowed references:

```gorget
int x = 42
spawn ((): print(x))()          # OK — x is Copy (int)
spawn ((): print("hello"))()    # OK — string literal is Static

auto c = (): print(x)
spawn c()                       # OK — closure variable with Copy capture

str name = get_name()
spawn ((): print(name))()       # ERROR — name has borrowed origin

Shared[int] counter = Shared[int](0)
spawn ((): print(counter.get()))()  # OK — Shared[T] is Copy
```

**`spawn` with borrowed references is rejected.** Unlike `.await()`, `spawn` launches a fire-and-forget thread that may outlive the current function. The compiler rejects passing borrowed references (`str` params, `&T`) to spawned tasks:

```gorget
async void worker(str name):
    print(name)

void launch(str name):
    # ERROR: name is a borrowed str — thread may outlive launch()
    auto t = spawn worker(name)
    # FIX: pass an owned String instead
    auto t2 = spawn worker(String(name).as_str())   # or redesign worker to take String
```

**`spawn` is not a suspension point.** The current function continues immediately after `spawn`, so non-borrowed values remain valid:

```gorget
async void example(str s):
    auto task = spawn some_async_fn()
    print(s)               # fine: spawn doesn't suspend, s is still live
```

See [4.6 Copy vs. Non-Copy Types](#46-copy-vs-non-copy-types) for the full type classification and [4.3 Ownership Rules](#43-ownership-rules) for borrow semantics.

### 7.26 Select

```ebnf
select_stmt = "select" ":" NEWLINE INDENT { select_arm } [ "else" ":" block ] DEDENT ;
select_arm  = "case" select_op ":" block ;
select_op   = type IDENT "=" expr ".recv()" | expr ".send(" expr ")" ;
```

`select` waits on multiple channel operations simultaneously and executes the arm of whichever completes first. Each arm is either a `recv()` (receiving from a channel) or a `send()` (sending to a channel). An optional `else` arm executes if no operation is immediately ready (non-blocking select).

```gorget
Channel[int] ch1 = Channel[int]()
Channel[str] ch2 = Channel[str]()

select:
    case int val = ch1.recv():
        print(f"got int: {val}")
    case str msg = ch2.recv():
        print(f"got str: {msg}")
    else:
        print("nothing ready")
```

---

## 8. Patterns

Patterns appear in `match` arms, `case` clauses, `is` expressions, `for` loops, and variable declarations.

```ebnf
pattern = wildcard | literal_pattern | binding | constructor_pattern
        | tuple_pattern | or_pattern | rest_pattern ;
```

### 8.1 Wildcard

```ebnf
wildcard = "_" ;
```

Matches any value, binding nothing.

### 8.2 Literal Patterns

```ebnf
literal_pattern = INT_LITERAL | FLOAT_LITERAL | BOOL_LITERAL
                | CHAR_LITERAL | STRING_LITERAL ;
```

Matches if the value equals the literal.

### 8.3 Binding Patterns

```ebnf
binding = IDENTIFIER ;
```

Matches any value and binds it to the given name.

### 8.4 Constructor Patterns

```ebnf
constructor_pattern = IDENTIFIER [ "." IDENTIFIER ] "(" [ pattern { "," pattern } ] ")" ;
```

Matches an enum variant or struct and destructures its fields.

```gorget
match result:
    case Ok(value): use(value)
    case Error(e): handle(e)

match point:
    case Point(x, y): print("{x}, {y}")
```

### 8.5 Tuple Patterns

```ebnf
tuple_pattern = "(" pattern "," pattern { "," pattern } ")"
              | pattern "," pattern { "," pattern } ;   (* bare form: auto / for only *)
```

Destructures a tuple. In `auto` declarations and `for` loop headers, parentheses are optional:

```gorget
auto (x, y) = get_coordinates()    # parenthesized
auto x, y = get_coordinates()      # bare — preferred

for (a, b) in pairs:               # parenthesized
for a, b in pairs:                 # bare — preferred
```

In `match` arms, parentheses are required: `case (a, b):`

### 8.6 Or Patterns

```ebnf
or_pattern = pattern "|" pattern { "|" pattern } ;
```

Matches if any alternative matches:

```gorget
case 200 | 201 | 204:
    print("success")
```

### 8.7 Rest Pattern

```ebnf
rest_pattern = ".." ;
```

Matches remaining fields in a constructor pattern (partial destructuring).

---

## 9. Ownership and Borrowing

Gorget enforces memory safety through compile-time ownership and borrowing rules, similar to Rust.

### 9.1 Ownership Rules

1. Every value has exactly one **owner** (the variable that holds it).
2. When the owner goes out of scope, the value is dropped (freed).
3. Ownership can be **transferred** (moved) using `!` or the `move` keyword.
4. After a move, the source variable is invalid. Any use is a compile-time error (**use-after-move**).
5. A variable cannot be moved more than once (**double-move** error).
6. A variable cannot be moved inside a loop body (**move-in-loop** error).
7. **Copy types** (primitives, small value types) are implicitly copied on assignment; no `!` or `move` is needed.
8. Reassigning a moved variable revives it — the new value makes it live again.

### 9.2 Borrowing Rules

At any given point in a program, for a given value, you may have **either**:

- **Any number of immutable borrows** (bare `Type name`), OR
- **Exactly one mutable borrow** (`Type &name`)

Never both simultaneously. This is enforced at compile time.

### 9.3 Call-Site Ownership Validation

The ownership annotation on a call argument **must match** the parameter declaration:

| Parameter declares                 | Call site must use                 | Meaning |
|------------------------------------|-----------------------------------|---------|
| `String s`                         | `f(s)`                            | Immutable borrow |
| `String &s` or `String mutable s`  | `f(&s)` or `f(mutable s)`        | Mutable borrow |
| `String !s` or `String move s`   | `f(!s)` or `f(move s)`         | Move |

Mismatches produce an **OwnershipMismatch** error.

### 9.4 Same-Call Aliasing

Within a single function call's arguments, the following conflicts are detected:

| Arguments          | Error                              |
|--------------------|------------------------------------|
| `f(&x, &x)`       | Double mutable borrow              |
| `f(x, &x)`        | Immutable + mutable borrow         |
| `f(&x, !x)`       | Mutable borrow + move              |

Passing the same variable bare twice (`f(x, x)`) is allowed for Copy types.

### 9.5 Branch Merging

When control flow branches (if/else, match), ownership state is merged **conservatively**: if a variable is moved in any branch, it is treated as moved after the branch point.

```gorget
String s = "hello"
if condition:
    consume(!s)
else:
    pass
# s is treated as moved here (conservative)
print(s)  # ERROR: use after move
```

### 9.6 Lifetime Inference and `live` Annotations

The compiler automatically infers which parameters' data flows into a function's return value. Most code needs no lifetime annotations at all. The `live` keyword exists for the small number of cases where inference cannot determine the relationship on its own.

#### What the compiler infers automatically

**Single reference parameter** — when a function takes exactly one reference-type parameter and returns a reference type, the compiler assumes the return borrows from that parameter:

```gorget
str trim_prefix(str s):
    return s.byte_slice(1, s.byte_len())
```

**`self` methods** — methods returning a reference type are assumed to borrow from `self`:

```gorget
equip Holder:
    str get_name(self):
        return self.name
```

**Body analysis with multiple reference parameters** — the compiler traces return expressions through the function body to determine which parameters contribute:

```gorget
str longer(str x, str y):
    if x.len() >= y.len():
        return x
    return y
```

Here the compiler sees both `x` and `y` in return positions and records that the result may borrow from either.

**Transitive through calls** — when a return expression calls another function, the compiler uses that function's already-computed metadata to determine which arguments (and therefore which outer parameters) flow through:

```gorget
str chain(str s):
    return identity(s)
```

**Local variable aliases** — assignments from parameters to locals are traced, so returning a local that holds parameter data is correctly attributed:

```gorget
str forward(str s):
    str local = s
    return local
```

#### When `live` is required

**Trait method declarations** have no body for the compiler to analyze. For methods with `self`, the self-elision rule covers most cases automatically. For non-self methods with multiple reference parameters, `live` makes the dependency explicit:

```gorget
trait Container:
    str get(live Container self, int index)
```

**Extern FFI declarations** also have no body. With two or more reference-type parameters and no `self`, the elision rules cannot determine which parameter the return borrows from:

```gorget
extern str pick_better(live str a, str b)
```

**Multiple independent borrow sources** needing precision use named groups and `outlives` constraints. This lets the compiler reject moving one source while the other is still in use:

```gorget
str pick_first(live(a) str x, live(b) str y) where a outlives b:
    return x
```

Named groups are optional — the compiler's body analysis already determines that only `x` flows to the return. The groups add the `outlives` constraint, which is enforced at call sites.

**Struct fields holding references** use `live` to mark that the struct borrows from external data. The struct cannot outlive what its `live` fields reference:

```gorget
struct Parser:
    live str source
    int position
```

Named groups on struct fields distinguish independent lifetimes:

```gorget
struct Merger:
    live(left) str a
    live(right) str b
```

---

## 10. Error Handling

Gorget uses a `throws`/`raw`/`throw` model that desugars to `Result[T, E]`.

### 10.1 Throwing Functions

A function declared with `throws` may fail:

```gorget
Data process(str path) throws AppError:
    String content = read_file(path)    # auto-propagates errors
    return transform(content)
```

Inside a `throws` function (or any function returning `Result`), calls to other throwing or `Result`-returning functions **auto-propagate** errors — if the callee fails, the caller immediately returns the error. No explicit unwrapping is needed; the compiler inserts the unwrap automatically.

### 10.2 Throw

The `throw` keyword explicitly raises an error:

```gorget
throw ParseError("invalid input")
```

It is a compile-time error to use `throw` in a function not declared with `throws`.

### 10.3 Raw

The `raw` keyword captures a potentially-failing call as a `Result` value instead of auto-propagating:

```gorget
auto result = raw read_file(path)
match result:
    case Ok(content): use(content)
    case Error(e): handle(e)
```

### 10.4 Rethrow

The `rethrow` keyword catches an error from a throwing call, transforms it, and re-throws.

**Bare form** — replace the error without inspecting it:

```gorget
void main() throws int:
    Json doc = json_parse(input) rethrow 1
```

**Binding form** — bind the original error and transform it:

```gorget
int load_config(str path) throws ConfigError:
    str content = read_file(path) rethrow (str e): ConfigError.Io(f"loading {path}: {e}")
    return parse(content) rethrow (str e): ConfigError.Parse(e)
```

On success, the expression's value passes through unchanged. On error, the transform expression is evaluated and thrown. In the binding form, the original error is available to the transform; in the bare form, it is discarded.

It is a compile-time error to use `rethrow` in a function not declared with `throws`.

### 10.5 Catch

The `catch` keyword is the recovery counterpart to `rethrow`. Where `rethrow` transforms an error and re-throws it (staying in error land), `catch` recovers from an error by producing a fallback value (exiting error land). The overall expression always succeeds.

**Binding form** — bind the error and compute a recovery value:

```gorget
void main():
    int port = parse_port(input) catch (e): 8080
    print("using port {port}")
```

```gorget
void main():
    int x = risky() catch (e): default_value
```

On success, the expression's value passes through unchanged. On failure, the error is bound to the identifier and the recovery expression is evaluated. The recovery expression must produce the same type as the success value.

Unlike `rethrow`, `catch` does **not** require the enclosing function to declare `throws` — it fully handles the error, so nothing escapes.

### 10.6 Throws on Main

`main()` may declare `throws int`, where the thrown integer becomes the process exit code:

```gorget
void main() throws int:
    Data d = load("config.json") rethrow 1
    process(d)
    # implicit success → exit 0
```

If `main` throws, the process exits with that code. If `main` completes normally, the process exits with code 0. It is a compile-time error for `main` to throw any type other than `int`.

### 10.5 On Error

The `on error` statement registers cleanup code that runs only if the function exits via an error (thrown or auto-propagated). It has two forms:

**Block form** — for multi-line cleanup:

```gorget
File open_and_process(str path) throws str:
    File f = File.open(path)
    on error:
        f.close()
    str content = f.read_all()
    return process(content)
```

**Inline form** — for single-statement cleanup:

```gorget
File open_and_process(str path) throws str:
    File f = File.open(path)
    on error f.close()
    str content = f.read_all()
    return process(content)
```

Multiple `on error` statements execute in **reverse** (LIFO) order on error paths. They do **not** execute on normal return. This is similar to Zig's `errdefer`.

It is a compile-time error to use `on error` in a function not declared with `throws`.

### 10.6 Error Types

Error types are typically enums:

```gorget
enum AppError:
    Io(IoError)
    Parse(ParseError)
    NotFound(String)
```

---

## 11. Generics

### 11.1 Generic Parameters

```ebnf
generic_params = "[" generic_param { "," generic_param } "]" ;
generic_param  = IDENTIFIER
               | "live" IDENTIFIER
               | "const" type IDENTIFIER ;
```

Types, functions, traits, and equip blocks may be parameterized:

- **Type parameters:** `[T]`, `[T, U]`
- **Lifetime parameters:** `[live a]` (for named borrow groups on structs — see §5.1)
- **Const parameters:** `[const int N]`

```gorget
T identity[T](T x):
    return x

struct FixedArray[T, const int N]:
    T[N] data
```

### 11.2 Generic Arguments

```ebnf
generic_args = "[" type { "," type } "]" ;
```

Provided at use sites when inference cannot determine the types:

```gorget
Pair[int, String] p = Pair[int, String](1, "hello")
auto result = max[int](a, b)
```

### 11.3 Inline Trait Bounds

Trait bounds are written inline in the generic parameter list, before the parameter name. Use `&` to combine multiple bounds.

```ebnf
generic_param     = [trait_bound_list " "] IDENTIFIER
                  | "live" IDENTIFIER
                  | "const" type IDENTIFIER ;
trait_bound_list  = trait_bound { "&" trait_bound } ;
trait_bound       = IDENTIFIER [ "[" type_or_binding { "," type_or_binding } "]" ] ;
```

```gorget
void print_all[Displayable T](Vector[T] items):
    for item in items:
        print(item.to_string())

void process[Displayable & Cloneable & Comparable T](T item):
    ...
```

The `where` keyword is retained solely for `outlives` borrow-group ordering constraints:

```gorget
str pick[Displayable T](live(a) T x, live(b) T y) where a outlives b:
    return x
```

### 11.4 Monomorphization

Gorget uses monomorphization: each unique combination of generic type arguments produces a specialized copy of the generic definition at compile time. This is a zero-cost abstraction — no runtime dispatch overhead.

---

## 12. Visibility

```ebnf
visibility = "public" | "private" ;
```

Two levels:

| Level     | Keyword    | Visible to                     |
|-----------|------------|--------------------------------|
| Public    | `public`   | All modules                    |
| Private   | `private`  | Same module only               |

**Defaults:** Most items (functions, structs, enums, traits, constants) are **public by default**. Two exceptions are **private by default**:
- **`static` declarations** — mutable module-level state should be explicitly exported
- **struct fields** — internal layout is an implementation detail

Applicable to: functions, structs, struct fields, enums, traits, constants, statics.

```gorget
public struct Point:
    public float x
    public float y
    float internal_id           # private (fields default)

static int counter = 0          # private (statics default)
public static int shared = 0    # explicitly public
```

---

## 13. Method Resolution

When a method is called on a value, the compiler resolves it in this order:

1. **Inherent methods** — methods defined in `equip Type:` blocks (no trait)
2. **Trait methods** — methods from `equip Type with Trait:` blocks

If multiple traits provide a method with the same name, the implementation is ambiguous and must be disambiguated.

Self parameters are **auto-borrowed**: the compiler automatically borrows the receiver at the appropriate mode (`self` = immutable borrow, `&self` = mutable borrow, `!self` = move). No `&`/`!` annotation is needed at the method call site.

---

## 14. String Interpolation

String interpolation is available only in **f-strings** (strings prefixed with `f`). Inside an f-string, `{expression}` evaluates the expression and inserts its string representation. Normal strings do **not** support interpolation — `{` and `}` are treated as literal characters.

The interpolated expression must be of a type that is either:

- A primitive type (`int`, `float`, `bool`, `char`)
- A `String` or `str`

Using a non-printable type (struct, enum) in interpolation is a compile-time error (**NonPrintableInterpolation**) unless the type implements `Displayable`.

```gorget
int x = 42
print(f"The answer is {x}")
print(f"Math: {2 + 2}")
print(f"Escaped brace: {{literal}}")
str name = "world"
print(f"Hello, {name}!")
```

### 14.1 Format Specifiers

Interpolated expressions can include a format specifier after a `:` to control output formatting:

```gorget
{expression:spec}
```

The format spec syntax is: `[#][0][width][.precision][type]`

- `#` — alternate form (adds `0x`, `0o`, or `0b` prefix for hex, octal, binary)
- `0` — zero-pad to fill width
- `width` — minimum field width (digits)
- `.precision` — decimal places for floats
- `type` — conversion type character

**Integer format types:**

| Spec | Description | Example | Output |
|------|-------------|---------|--------|
| `d`  | Decimal     | `f"{255:d}"`  | `255`  |
| `x`  | Hex (lower) | `f"{255:x}"`  | `ff`   |
| `X`  | Hex (upper) | `f"{255:X}"`  | `FF`   |
| `o`  | Octal       | `f"{255:o}"`  | `377`  |
| `b`  | Binary      | `f"{255:b}"`  | `11111111` |

**Float format types:**

| Spec | Description | Example | Output |
|------|-------------|---------|--------|
| `f`  | Fixed-point | `f"{3.14159:.2f}"` | `3.14` |
| `e`  | Scientific (lower) | `f"{3.14159:.3e}"` | `3.142e+00` |
| `E`  | Scientific (upper) | `f"{3.14159:.3E}"` | `3.142E+00` |

**Combining specifiers:**

```gorget
int n = 42
print(f"{n:08d}")        # "00000042" — zero-padded to 8 digits
print(f"{n:06x}")        # "00002a"   — zero-padded hex
print(f"{255:#x}")       # "0xff"     — hex with 0x prefix
print(f"{255:#b}")       # "0b11111111" — binary with 0b prefix
```

### 14.2 String Concatenation

The `+` operator concatenates two strings and returns a new string. The `+=` operator appends in place. See also §7.5.

```gorget
str a = "hello"
str b = a + " world"     # "hello world"
a += "!"                  # "hello!"

# Chaining works left-to-right
str full = "a" + "b" + "c"   # "abc"
```

---

## 15. Built-in Functions

The following functions are available without import:

| Function      | Signature               | Description                     |
|---------------|-------------------------|---------------------------------|
| `print`       | `void(String)`          | Print to stdout with newline    |
| `println`     | `void(String)`          | Print to stdout with newline    |
| `len`         | `int(Measurable)`       | Length — delegates to `x.len()` |
| `range`       | `Range(int, int)`       | Create a range                  |
| `enumerate`   | `Iterator(Collection)`  | Iterate with index              |
| `zip`         | `Iterator(A, B)`        | Combine two iterators           |
| `map`         | `Iterator(Collection, fn)` | Transform elements           |
| `filter`      | `Iterator(Collection, fn)` | Filter elements              |
| `type`        | `String(any)`           | Runtime type name               |
| `panic`       | `Never(String)`         | Abort with message              |

### 15.1 Built-in Traits

The compiler automatically registers the following core traits. They cannot be redefined but any user type may implement them via equip blocks (§5.5). Implementing a built-in trait unlocks the corresponding compiler feature.

| Trait | Required Method | Returns | Compiler Feature |
|---|---|---|---|
| `Displayable` | `str display(self)` | `str` | String interpolation, `print()` |
| `Equatable` | `bool eq(self, Self other)` | `bool` | `==` and `!=` operators |
| `Hashable` | `int hash(self)` | `int` | `Dict` keys, `Set` elements |
| `Cloneable` | `Self clone(self)` | `Self` | Deep copying |
| `Drop` | `void drop(!self)` | `void` | Auto-cleanup on scope exit, `with` statement (§6.14) |
| `Iterator[T]` | `Option[T] next(&self)` | `Option[T]` | `for` loop desugaring (§6.11) |
| `Add[Out]` | `Out add(self, Self rhs)` | `Out` | `+` and `+=` operators |
| `Sub[Out]` | `Out sub(self, Self rhs)` | `Out` | `-` and `-=` operators |
| `Mul[Out]` | `Out mul(self, Self rhs)` | `Out` | `*` and `*=` operators |
| `Div[Out]` | `Out div(self, Self rhs)` | `Out` | `/` and `/=` operators |
| `Rem[Out]` | `Out rem(self, Self rhs)` | `Out` | `%` and `%=` operators |
| `Mod[Out]` | `Out mod(self, Self rhs)` | `Out` | `mod` operator |
| `Neg[Out]` | `Out neg(self)` | `Out` | Unary `-` operator |
| `Comparable` | `int compare(self, Self other)` | `int` | `<`, `>`, `<=`, `>=` operators |
| `Index[K, V]` | `V get(self, K key)` | `V` | `a[k]` read access |
| `IndexMut[K, V]` | `void set(&self, K key, V value)` | `void` | `a[k] = v` write access |
| `Iterable[T]` | `Iterator[T] iter(&self)` | `Iterator[T]` | `for` loop desugaring (§6.11) |
| `Default` | `Self default()` (static) | `Self` | Zero/default values, `@derive(Default)` |
| `From[T]` | `Self from(T value)` (static) | `Self` | Infallible type conversion, `@derive(From)` |
| `TryFrom[T]` | `Result[Self, str] try_from(T value)` (static) | `Result[Self, str]` | Fallible type conversion, `@derive(TryFrom)` |
| `Parseable` | `Option[Self] parse(str s)` (static) | `Option[Self]` | Fallible string parsing via `Type.parse(s)` |
| `Measurable` | `int len(self)` | `int` | Types with a length; enables `len(x)` free function |

#### Displayable

Enables string interpolation (`"{value}"`) and `print()` for user-defined types. Without this trait, using a non-primitive type in interpolation is a compile-time error (`NonPrintableInterpolation`).

```gorget
struct Point:
    float x
    float y

equip Point with Displayable:
    str display(self):
        return "({self.x}, {self.y})"

Point p = Point(3.0, 4.0)
print("{p}")  # prints: (3.0, 4.0)
```

#### Equatable

Enables `==` and `!=` operators for user-defined types. The `Self` parameter refers to the implementing type.

```gorget
equip Point with Equatable:
    bool eq(self, Point other):
        return self.x == other.x and self.y == other.y

if p1 == p2:
    print("equal")
```

#### Hashable

Required for types used as `Dict` keys or `Set` elements. Should return a consistent integer hash.

```gorget
equip Point with Hashable:
    int hash(self):
        return self.x * 31 + self.y

Set[Point] points = {}
points.add(Point(1.0, 2.0))
```

#### Cloneable

Enables deep copying of values. The return type `Self` resolves to the implementing type.

```gorget
equip Point with Cloneable:
    Point clone(self):
        return Point(self.x, self.y)

Point copy = p.clone()
```

#### Drop

Provides deterministic cleanup. The `drop` method is called automatically when a value goes out of scope, and is invoked by the `with` statement (§6.14). The `!self` parameter means `drop` takes ownership of the value (move semantics).

```gorget
struct Connection:
    int fd

equip Connection with Drop:
    void drop(!self):
        close_fd(self.fd)

with Connection(open_fd("db")) as conn:
    conn.query("SELECT 1")
# conn.drop() called automatically here
```

#### Iterator[T]

Enables `for` loop iteration (§6.11). The type parameter `T` is the element type. The `&self` parameter means `next` takes a mutable borrow, allowing the iterator to advance its internal state.

```gorget
struct Counter:
    int current
    int max

equip Counter with Iterator[int]:
    Option[int] next(&self):
        if self.current < self.max:
            int val = self.current
            self.current = self.current + 1
            return Some(val)
        return None()

for i in Counter(0, 5):
    print("{i}")  # prints 0 through 4
```

#### Iterable[T]

Provides the `iter` method used by `for` loop desugaring (§6.11). Types implementing `Iterable[T]` return an `Iterator[T]` that produces elements. The `&self` parameter allows creating iterators from mutable references.

```gorget
equip NumberRange with Iterable[int]:
    Iterator[int] iter(&self):
        return Counter(self.start, self.end)

for n in NumberRange(1, 5):
    print("{n}")  # prints 1 through 4
```

#### Default

Provides a default value for a type via a static `default()` method. Derivable for structs with `@derive(Default)` — each field gets its zero value (`0` for int, `""` for str, etc.).

```gorget
@derive(Default)
struct Config:
    int timeout
    str host
    bool verbose

Config c = Config.default()  # Config(0, "", false)
```

#### From[T]

Infallible type conversion. The static `from` method converts a value of type `T` into `Self`. Derivable for single-field structs (newtypes) with `@derive(From)`.

```gorget
newtype Celsius(float)

@derive(From)
newtype Fahrenheit(float)

# Manual implementation
equip Celsius with From[Fahrenheit]:
    static Celsius from(Fahrenheit f):
        return Celsius((f.0 - 32.0) * 5.0 / 9.0)

Celsius c = Celsius.from(Fahrenheit(212.0))  # Celsius(100.0)
```

#### TryFrom[T]

Fallible type conversion. Like `From[T]` but returns `Result[Self, str]` to handle conversion failures. Derivable for single-field structs (newtypes) with `@derive(TryFrom)`.

```gorget
newtype Percentage(int)

equip Percentage with TryFrom[int]:
    static Result[Percentage, str] try_from(int value):
        if value < 0 or value > 100:
            return Error("percentage must be 0-100")
        return Ok(Percentage(value))

auto result = Percentage.try_from(50)   # Ok(Percentage(50))
auto bad = Percentage.try_from(200)     # Error("percentage must be 0-100")
```

#### Parseable

Fallible string parsing. Returns `Option[Self]` — `Some(value)` on success, `None` on invalid input. Built-in for all numeric primitives (`int`, `int8`, `int16`, `int32`, `uint`, `uint8`, `uint16`, `uint32`, `float`, `float32`). Never panics.

```gorget
Option[int] n = int.parse("42")
if n is Some(val):
    print("{val}")            # 42

Option[int] bad = int.parse("hello")
if bad is None:
    print("invalid")          # invalid

# unwrap_or for default values
int port = int.parse(port_str).unwrap_or(8080)
```

User-defined types can equip `Parseable`:

```gorget
newtype Hex(int)

equip Hex with Parseable:
    static Option[Hex] parse(str s):
        Option[int] n = int.parse(s)
        if n is Some(val):
            return Some(Hex(val))
        return None
```

#### Measurable

Types that have a length. Equipping `Measurable` enables the `len(x)` free function, which delegates to `x.len()`. Built-in types (`Vector`, `Dict`, `HashMap`, `Set`, `HashSet`, `str`, `String`) have built-in `.len()` methods that work without equipping the trait. User-defined types need `equip T with Measurable` to participate.

```gorget
struct Buffer:
    int size

equip Buffer with Measurable:
    int len(self):
        return self.size

Buffer buf = Buffer(42)
print("{len(buf)}")    # 42
print("{buf.len()}")   # 42
```

#### Operator Traits

Operator traits enable user-defined types to use built-in operators. The `Out` type parameter controls the return type.

**Arithmetic (`Add`, `Sub`, `Mul`, `Div`, `Rem`, `Mod`).** Each trait enables its corresponding binary operator. `Rem` maps to `%`/`%=` (remainder — sign follows dividend). `Mod` maps to the `mod` keyword operator (modulo — sign follows divisor).

```gorget
struct Vec2:
    int x
    int y

equip Vec2 with Add[Vec2]:
    Vec2 add(self, Vec2 rhs):
        return Vec2(self.x + rhs.x, self.y + rhs.y)

Vec2 c = Vec2(1, 2) + Vec2(3, 4)  # Vec2(4, 6)
```

Compound assignment desugars to the trait method: `v += Vec2(1, 0)` becomes `v = v.add(Vec2(1, 0))`.

**Unary negation (`Neg`).** Enables the unary `-` operator.

```gorget
equip Vec2 with Neg[Vec2]:
    Vec2 neg(self):
        return Vec2(-self.x, -self.y)

Vec2 v = -Vec2(3, 4)  # Vec2(-3, -4)
```

**Comparison (`Comparable`).** Enables `<`, `>`, `<=`, `>=` via a single `compare` method that returns negative, zero, or positive `int`.

```gorget
equip Vec2 with Comparable:
    int compare(self, Vec2 other):
        int m1 = self.x * self.x + self.y * self.y
        int m2 = other.x * other.x + other.y * other.y
        if m1 < m2:
            return -1
        elif m1 > m2:
            return 1
        return 0

if a < b:
    print("a is smaller")
```

Optionally, specific comparison methods (`lt`, `gt`, `lte`, `gte`) can be defined in the same equip block. If present, they take precedence over the derived `compare` behavior for that operator.

**Indexing (`Index`, `IndexMut`).** Enable `a[k]` read and `a[k] = v` write syntax.

```gorget
struct Grid:
    int a
    int b

equip Grid with Index[int, int]:
    int get(self, int key):
        if key == 0:
            return self.a
        return self.b

equip Grid with IndexMut[int, int]:
    void set(&self, int key, int value):
        if key == 0:
            self.a = value
        else:
            self.b = value

Grid g = Grid(10, 20)
print("{g[0]}")   # 10
g[1] = 99
```

Note: `IndexMut.set` takes `&self` (mutable borrow) since it modifies the receiver.

**Dispatch priority.** Operator traits only apply to user-defined types. Built-in operations (string concatenation, vector concat, primitive arithmetic, map indexing) take precedence and are unchanged.

#### Trait Features

**Default method implementations.** Trait methods may include a body, providing a default that implementors can override (§5.4):

```gorget
trait Greetable:
    str name(self)
    str greet(self):
        return "Hello, {self.name()}!"

equip Person with Greetable:
    str name(self):
        return self.first_name
    # greet() uses the default implementation
```

**Trait inheritance.** The `extends` keyword declares supertrait requirements. A type implementing a child trait must also implement all parent traits. The child's vtable includes parent method slots.

```gorget
trait Animal extends Displayable:
    str sound(self)
```

**Delegation via field.** The `via` clause on equip blocks auto-forwards unimplemented trait methods through a struct field (§5.5):

```gorget
equip Wrapper with Displayable via inner:
    pass  # display() forwarded to self.inner.display()
```

### 15.2 Built-in Type Methods

The following methods are available on built-in types without any import.

**`str`** — String methods

| Method | Signature | Description |
|---|---|---|
| `len()` | `→ int` | Number of Unicode codepoints (O(n) UTF-8 walk) |
| `byte_len()` | `→ int` | Byte length of the UTF-8 representation (O(1)) |
| `is_empty()` | `→ bool` | True if length is zero |
| `contains(needle)` | `str → bool` | True if `needle` is a substring |
| `starts_with(prefix)` | `str → bool` | True if string starts with `prefix` |
| `ends_with(suffix)` | `str → bool` | True if string ends with `suffix` |
| `index_of(needle)` | `str → Option[int]` | Codepoint index of first occurrence (`None` if not found) |
| `count(needle)` | `str → int` | Number of non-overlapping occurrences |
| `char_at(index)` | `int → char` | Byte at byte index (panics if out of bounds; for parser/codec use) |
| `byte_slice(start, end)` | `int, int → str` | Byte-range substring view (O(1), for parser/codec use) |
| `substring(start, end)` | `int, int → str` | Codepoint-range substring view from `start` to `end` (panics if out of bounds) |
| `trim()` | `→ str` | Strip leading/trailing Unicode whitespace (view, no allocation) |
| `strip(chars?)` | `str? → str` | Strip codepoints (or whitespace) from both ends (view) |
| `lstrip(chars?)` | `str? → str` | Strip codepoints (or whitespace) from left (view) |
| `rstrip(chars?)` | `str? → str` | Strip codepoints (or whitespace) from right (view) |
| `to_upper()` | `→ String` | Unicode-aware uppercase (Latin/Greek/Cyrillic) |
| `to_lower()` | `→ String` | Unicode-aware lowercase (Latin/Greek/Cyrillic) |
| `replace(old, new)` | `str, str → String` | Replace all occurrences of `old` with `new` |
| `split(delim)` | `str → Vector[str]` | Split into parts by delimiter |
| `join(parts)` | `Vector[str] → String` | Join strings with receiver as separator |
| `repeat(n)` | `int → String` | Repeat string `n` times |
| `removeprefix(prefix)` | `str → str` | Remove `prefix` if present, otherwise return unchanged (view) |
| `removesuffix(suffix)` | `str → str` | Remove `suffix` if present, otherwise return unchanged (view) |
| `pad_left(n, char)` | `int, char → String` | Left-pad to width `n` with fill character |
| `pad_right(n, char)` | `int, char → String` | Right-pad to width `n` with fill character |
| `bytes()` | `→ Vector[uint8]` | Raw UTF-8 bytes as a vector |
| `codepoints()` | `→ Vector[int]` | Unicode codepoint values as a vector |
| `chars()` | `→ Vector[str]` | Individual characters (codepoints) as `str` views |
| `hash()` | `→ int` | Hash value |

**Unicode support scope:**
- `to_upper()`/`to_lower()` handle 1:1 simple case mappings for Latin (U+0000–024F), Greek (U+0370–03FF), and Cyrillic (U+0400–04FF). Locale-dependent mappings (e.g., Turkish İ/ı) and one-to-many mappings (e.g., ß→SS) are not yet supported.
- `trim()`/`strip()`/`lstrip()`/`rstrip()` recognize all 25 Unicode whitespace codepoints (Unicode Zs category + control chars: HT, LF, VT, FF, CR, SP, NBSP, OGHAM, EN/EM spaces, etc.).
- `index_of()` returns a **codepoint index**, not a byte offset.
- All search methods (`contains`, `starts_with`, `ends_with`, `index_of`, `count`, `replace`, `split`) are safe on non-null-terminated `str` views (from `byte_slice()`, `s[i..j]`).
- **Deferred**: grapheme cluster segmentation, Unicode normalization (NFC/NFD), locale-dependent case mappings.

**String indexing, slicing, and iteration** operate at the Unicode codepoint level:

| Operation | Result type | Description |
|---|---|---|
| `s[i]` | `str` | Returns the i-th Unicode codepoint as a string view (O(n) walk) |
| `s[i..j]` | `str` | Returns codepoint range [i, j) as a non-allocating view (O(n) walk) |
| `s[-1]` | `str` | Negative indexing counts from end |
| `for ch in s:` | yields `str` | Iterates Unicode codepoints (O(n) total, single UTF-8 pass) |

For byte-level access (useful in parsers and codecs), use `char_at(i)` (returns `char`) and `byte_slice(a, b)` (returns `str` byte-range view in O(1)).

**UTF-8 validation at system boundaries.** All `str` and `String` values are guaranteed to contain valid UTF-8. The compiler enforces this at the boundaries where external bytes enter the string world:

| Boundary | Return type | On invalid UTF-8 |
|---|---|---|
| `File.read_all()` | `Result[String, str]` | Returns `Error("invalid UTF-8 in file")` |
| `Socket.read_line()` | `Result[String, str]` | Returns `Error("invalid UTF-8 from socket")` |
| `TlsSocket.read_line()` | `Result[String, str]` | Returns `Error("invalid UTF-8 from socket")` |
| `bytes_to_str(buf)` | `Result[str, str]` | Returns `Error("invalid UTF-8 in byte buffer")` |

String literals are validated at compile time by the lexer. Internal string operations (slicing, concatenation, indexing) preserve UTF-8 validity by construction.

**`String`** — Owned mutable string methods

`String` is a heap-allocated, growable string buffer. It supports all `str` methods above (auto-coerced to `str` view) plus these mutation methods:

| Method | Signature | Description |
|---|---|---|
| `len()` | `→ int` | Number of Unicode codepoints |
| `is_empty()` | `→ bool` | True if length is zero |
| `capacity()` | `→ int` | Current allocated capacity in bytes |
| `push(s)` | `str → void` | Append a string |
| `push_char(c)` | `char → void` | Append a single character |
| `push_line(s)` | `str → void` | Append a string followed by a newline |
| `clear()` | `→ void` | Remove all content (keeps allocated capacity) |
| `str()` | `→ str` | View as immutable `str` slice |

`String` also inherits all read-only `str` methods: `contains()`, `starts_with()`, `split()`, `trim()`, etc.

**`char`** — Character methods

| Method | Signature | Description |
|---|---|---|
| `is_alpha()` | `→ bool` | True if alphabetic |
| `is_digit()` | `→ bool` | True if ASCII digit |
| `is_alphanumeric()` | `→ bool` | True if alphabetic or digit |
| `is_whitespace()` | `→ bool` | True if whitespace |
| `is_upper()` | `→ bool` | True if uppercase letter |
| `is_lower()` | `→ bool` | True if lowercase letter |
| `to_upper()` | `→ char` | Convert to uppercase |
| `to_lower()` | `→ char` | Convert to lowercase |

**`Vector[T]`** — Dynamic array

| Method | Signature | Description |
|---|---|---|
| `push(item)` | `T → void` | Append an element |
| `pop()` | `→ Option[T]` | Remove and return last element (`None` if empty) |
| `get(index)` | `int → Option[T]` | Get element at index (`None` if out of bounds) |
| `set(index, item)` | `int, T → void` | Set element at index |
| `remove(index)` | `int → Option[T]` | Remove element at index, shifting subsequent elements (`None` if out of bounds) |
| `len()` | `→ int` | Number of elements |
| `is_empty()` | `→ bool` | True if length is zero |
| `contains(item)` | `T → bool` | True if element exists (by value) |
| `index_of(item)` | `T → Option[int]` | Index of first match (`None` if not found) |
| `insert(index, item)` | `int, T → void` | Insert element at index, shifting subsequent elements |
| `extend(other)` | `Vector[T] → void` | Append all elements from another vector |
| `slice(start, end)` | `int, int → Vector[T]` | New vector from elements `[start, end)` |
| `clear()` | `→ void` | Remove all elements |
| `reserve(n)` | `int → void` | Pre-allocate capacity for at least `n` elements |
| `sort()` | `→ void` | Sort elements in place (ascending) |
| `sorted()` | `→ Vector[T]` | Return a new sorted copy |
| `reverse()` | `→ void` | Reverse elements in place |
| `any(pred)` | `(T) → bool → bool` | True if any element satisfies predicate |
| `all(pred)` | `(T) → bool → bool` | True if all elements satisfy predicate |
| `filter(pred)` | `(T) → bool → Vector[T]` | Elements satisfying predicate |
| `map(f)` | `(T) → U → Vector[U]` | Apply function to each element |
| `fold(init, f)` | `U, (U, T) → U → U` | Left fold with initial value |
| `reduce(f)` | `(T, T) → T → T` | Reduce without initial value |

**`Dict[K, V]`** — Ordered hash map (insertion-order preserving, like Python 3.7+ `dict`)

Iteration, `keys()`, `values()`, and `items()` all return entries in insertion order. Removing a key and re-inserting it places it at the end.

| Method | Signature | Description |
|---|---|---|
| `put(key, value)` | `K, V → void` | Insert or update a key-value pair |
| `get(key)` | `K → Option[V]` | Get value for key (`None` if missing) |
| `contains(key)` | `K → bool` | True if key exists |
| `remove(key)` | `K → bool` | Remove key, return whether it existed |
| `len()` | `→ int` | Number of entries |
| `is_empty()` | `→ bool` | True if length is zero |
| `clear()` | `→ void` | Remove all entries |
| `get_or(key, default)` | `K, V → V` | Get value for key, or return `default` |
| `get_or_put(key, default)` | `K, V → V` | Get value for key, or insert `default` and return it |
| `update(other)` | `Dict[K, V] → void` | Merge all entries from `other` (overwrites existing keys) |
| `keys()` | `→ Vector[K]` | All keys in insertion order |
| `values()` | `→ Vector[V]` | All values in insertion order |
| `items()` | `→ Vector[(K, V)]` | All key-value pairs in insertion order |
| `filter(pred)` | `(K, V) → bool → Dict[K, V]` | Entries satisfying predicate |
| `fold(init, f)` | `U, (U, K, V) → U → U` | Left fold over entries |

**`HashMap[K, V]`** — Unordered hash map

Same API as `Dict` but does not preserve insertion order. Use when order is irrelevant and maximum performance is desired.

| Method | Signature | Description |
|---|---|---|
| `put(key, value)` | `K, V → void` | Insert or update a key-value pair |
| `get(key)` | `K → Option[V]` | Get value for key (`None` if missing) |
| `contains(key)` | `K → bool` | True if key exists |
| `remove(key)` | `K → bool` | Remove key, return whether it existed |
| `len()` | `→ int` | Number of entries |
| `is_empty()` | `→ bool` | True if length is zero |
| `clear()` | `→ void` | Remove all entries |
| `get_or(key, default)` | `K, V → V` | Get value for key, or return `default` |
| `get_or_put(key, default)` | `K, V → V` | Get value for key, or insert `default` and return it |
| `update(other)` | `HashMap[K, V] → void` | Merge all entries from `other` |
| `keys()` | `→ Vector[K]` | All keys (unordered) |
| `values()` | `→ Vector[V]` | All values (unordered) |
| `items()` | `→ Vector[(K, V)]` | All key-value pairs (unordered) |
| `filter(pred)` | `(K, V) → bool → HashMap[K, V]` | Entries satisfying predicate |
| `fold(init, f)` | `U, (U, K, V) → U → U` | Left fold over entries |

**`Set[T]`** — Hash set

| Method | Signature | Description |
|---|---|---|
| `add(item)` | `T → void` | Insert an element |
| `contains(item)` | `T → bool` | True if element exists |
| `remove(item)` | `T → bool` | Remove element, return whether it existed |
| `len()` | `→ int` | Number of elements |
| `is_empty()` | `→ bool` | True if length is zero |
| `clear()` | `→ void` | Remove all elements |
| `union(other)` | `Set[T] → Set[T]` | New set with elements from both |
| `intersection(other)` | `Set[T] → Set[T]` | New set with elements in both |
| `difference(other)` | `Set[T] → Set[T]` | New set with elements in self but not `other` |
| `is_subset(other)` | `Set[T] → bool` | True if all elements are in `other` |
| `is_superset(other)` | `Set[T] → bool` | True if `other`'s elements are all in self |
| `filter(pred)` | `(T) → bool → Set[T]` | Elements satisfying predicate |
| `fold(init, f)` | `U, (U, T) → U → U` | Left fold over elements |

**`Option[T]`** — Optional value

| Method | Signature | Description |
|---|---|---|
| `unwrap()` | `→ T` | Extract value (panics if `None`) |
| `expect(msg)` | `str → T` | Extract value (panics with `msg` if `None`) |
| `unwrap_or(default)` | `T → T` | Extract value or return default (eager) |
| `unwrap_or_else(f)` | `() → T → T` | Extract value or compute default (lazy) |
| `is_some()` | `→ bool` | True if `Some` |
| `is_none()` | `→ bool` | True if `None` |
| `map(f)` | `(T) → U → Option[U]` | Apply function to inner value |
| `and_then(f)` | `(T) → Option[U] → Option[U]` | Flat-map |
| `or_else(f)` | `() → Option[T] → Option[T]` | Fallback if `None` |
| `flatten()` | `Option[Option[T]] → Option[T]` | Unwrap one nesting layer |

**`Result[T, E]`** — Success or error

| Method | Signature | Description |
|---|---|---|
| `unwrap()` | `→ T` | Extract value (panics if `Error`) |
| `expect(msg)` | `str → T` | Extract value (panics with `msg` if `Error`) |
| `unwrap_or(default)` | `T → T` | Extract value or return default (eager) |
| `unwrap_or_else(f)` | `(E) → T → T` | Extract value or compute default from error (lazy) |
| `is_ok()` | `→ bool` | True if `Ok` |
| `is_err()` | `→ bool` | True if `Error` |
| `map(f)` | `(T) → U → Result[U, E]` | Apply function to success value |
| `and_then(f)` | `(T) → Result[U, E] → Result[U, E]` | Flat-map on success |
| `or_else(f)` | `(E) → Result[T, F] → Result[T, F]` | Flat-map on error |

**`Box[T]`** — Heap-allocated value

| Method | Signature | Description |
|---|---|---|
| `get()` | `→ T` | Get the contained value |
| `set(value)` | `T → void` | Replace the contained value |

**`File`** — File handle

| Method | Signature | Description |
|---|---|---|
| `read_all()` | `→ Result[String, str]` | Read entire file contents (validates UTF-8) |
| `write(data)` | `str → void` | Write string to file |
| `close()` | `→ void` | Close the file handle |

### 15.3 Standard Library Modules

The following functions are available via `import`:

**`std.fs`** — File system

| Function | Signature | Description |
|---|---|---|
| `read_file` | `str(str)` | Read entire file to string |
| `write_file` | `void(str, str)` | Write string to file |
| `append_file` | `void(str, str)` | Append string to file |
| `file_exists` | `bool(str)` | Check if file exists |
| `delete_file` | `bool(str)` | Delete a file |
| `mkdir` | `bool(str)` | Create a directory |
| `rmdir` | `bool(str)` | Remove a directory |
| `rename` | `bool(str, str)` | Rename a file or directory |
| `copy_file` | `bool(str, str)` | Copy a file from source to destination |
| `file_size` | `int(str)` | Get file size in bytes |
| `is_dir` | `bool(str)` | Check if path is a directory |

**`std.path`** — Path manipulation

| Function | Signature | Description |
|---|---|---|
| `path_join` | `str(str, str)` | Join two path segments |
| `path_parent` | `str(str)` | Parent directory |
| `path_basename` | `str(str)` | File name component |
| `path_extension` | `str(str)` | File extension |
| `path_stem` | `str(str)` | File name without extension |

**`std.os`** — Operating system

| Function | Signature | Description |
|---|---|---|
| `exit` | `void(int)` | Exit with status code |
| `getenv` | `str(str)` | Get environment variable |
| `setenv` | `void(str, str)` | Set environment variable |
| `getcwd` | `str()` | Current working directory |
| `platform` | `str()` | OS name: `"macos"`, `"linux"`, `"windows"`, `"freebsd"` |
| `args` | `Vector[str]()` | CLI arguments |
| `readdir` | `Vector[str](str)` | List directory entries |

**`std.conv`** — Type conversions

| Function | Signature | Description |
|---|---|---|
| `ord` | `int(char)` | Character to integer code point |
| `chr` | `char(int)` | Integer code point to character |
| `parse_int` | `int(str)` | Parse string as integer |
| `parse_float` | `float(str)` | Parse string as float |
| `int_to_str` | `str(int)` | Integer to string |
| `float_to_str` | `str(float)` | Float to string (compact format) |
| `bool_to_str` | `str(bool)` | Bool to `"true"` or `"false"` |
| `char_to_str` | `str(char)` | Single character to string |
| `codepoint_to_str` | `str(int)` | Unicode code point to string |

> **Note:** `parse_int` and `parse_float` panic on invalid input. For fallible parsing, use the `Parseable` trait: `int.parse(s)` returns `Option[int]`, `float.parse(s)` returns `Option[float]` (see §15.1).

**`std.io`** — I/O

| Name | Signature | Description |
|---|---|---|
| `stderr` | `File` | Standard error stream |
| `stdout` | `File` | Standard output stream |
| `getchar` | `int()` | Read one byte from stdin (-1 on EOF) |
| `term_cols` | `int()` | Terminal width in columns |
| `term_rows` | `int()` | Terminal height in rows |
| `input` | `str(str)` | Print prompt, read a line from stdin |
| `readline` | `str()` | Read a line from stdin (no prompt) |

**`std.random`** — Random numbers

| Function | Signature | Description |
|---|---|---|
| `rand` | `int()` | Random integer |
| `seed` | `void(int)` | Seed the random number generator |
| `rand_range` | `int(int, int)` | Random integer in `[lo, hi)` |

**`std.time`** — Time

| Function | Signature | Description |
|---|---|---|
| `time` | `int()` | Current Unix timestamp in seconds |
| `time_ms` | `int()` | Current time in milliseconds |
| `sleep_ms` | `void(int)` | Sleep for milliseconds |

**`std.math`** — Math

| Function | Signature | Description |
|---|---|---|
| `abs` | `int(int)` / `float(float)` | Absolute value (dispatches by argument type) |
| `min` | `int(int, int)` / `float(float, float)` | Minimum (dispatches by argument type) |
| `max` | `int(int, int)` / `float(float, float)` | Maximum (dispatches by argument type) |
| `sqrt` | `float(float)` | Square root |
| `pow` | `float(float, float)` | Exponentiation |
| `floor` | `float(float)` | Round down |
| `ceil` | `float(float)` | Round up |
| `round` | `float(float)` | Round to nearest |
| `log` | `float(float)` | Natural logarithm |
| `log2` | `float(float)` | Base-2 logarithm |
| `log10` | `float(float)` | Base-10 logarithm |
| `sin` | `float(float)` | Sine |
| `cos` | `float(float)` | Cosine |
| `tan` | `float(float)` | Tangent |
| `asin` | `float(float)` | Arcsine |
| `acos` | `float(float)` | Arccosine |
| `atan` | `float(float)` | Arctangent |
| `atan2` | `float(float, float)` | Two-argument arctangent |

**`std.fmt`** — Formatting

Re-exports the `Displayable` trait and `format` builtin for discoverability. Both are available in the prelude without an explicit import.

**`std.process`** — Process execution

| Name | Signature | Description |
|---|---|---|
| `ExecResult` | struct | Result of a process: `output: str`, `errors: str`, `exit_code: int` |
| `exec` | `int(str)` | Run a shell command, return exit code |
| `exec_output` | `ExecResult(str)` | Run a command, capture stdout and exit code |
| `process_spawn` | `Result[Process, str](str, Vector[str])` | Spawn a child process with full pipe control |
| `getpid` | `int()` | Return the current process ID |
| `Process` | struct (opaque) | Handle to a child process |
| `Process.wait` | `int()` | Wait for the process to exit; returns its exit code |
| `Process.kill` | `void()` | Send SIGKILL to the process |
| `Process.pid` | `int()` | Return the child's PID |
| `Process.write_stdin` | `void(str)` | Write data to the child's stdin pipe |
| `Process.close_stdin` | `void()` | Close the child's stdin pipe (signals EOF) |
| `Process.read_stdout` | `str()` | Read all remaining stdout from the child |
| `Process.read_stderr` | `str()` | Read all remaining stderr from the child |

Use `exec` / `exec_output` for quick shell one-liners. Use `process_spawn` when you need argument safety (no shell interpretation), bidirectional pipe I/O, or fine-grained process lifecycle control.

```gorget
from std.process import process_spawn, getpid

Result[Process, str] result = process_spawn("echo", Vector[str]("hello world"))
match result:
    case Ok(p):
        str out = p.read_stdout()
        int code = p.wait()
        print(out)         # hello world
        print(code)        # 0
    case Error(msg):
        print("spawn failed: {msg}")
```

**`std.signal`** — Signal handling

| Name | Signature | Description |
|---|---|---|
| `signal_trap(int sig)` | `void` | Install a handler that sets a flag when `sig` is received |
| `signal_check(int sig)` | `bool` | Returns true if `sig` was received since last check; clears the flag |
| `signal_wait()` | `int` | Blocks until any trapped signal arrives; returns the signal number |
| `signal_ignore(int sig)` | `void` | Set `SIG_IGN` for the signal (silently discard) |
| `signal_reset(int sig)` | `void` | Restore the default handler for the signal |
| `signal_send(int pid, int sig)` | `int` | Send a signal to a process (returns 0 on success) |

**Constants:** `SIGHUP` (1), `SIGINT` (2), `SIGQUIT` (3), `SIGABRT` (6), `SIGKILL` (9), `SIGUSR1` (10), `SIGUSR2` (12), `SIGPIPE` (13), `SIGALRM` (14), `SIGTERM` (15), `SIGCHLD` (17).

```gorget
from std.signal import signal_trap, signal_check, SIGINT, SIGTERM

signal_trap(SIGINT)
signal_trap(SIGTERM)
loop:
    if signal_check(SIGINT) or signal_check(SIGTERM):
        print("shutting down gracefully")
        break
    do_work()
```

**`std.sync`** — Synchronization primitives

```gorget
from std.sync import AtomicInt, AtomicBool, CondVar, Barrier, RWLock
```

| Type | Constructor | Key Methods |
|---|---|---|
| `AtomicInt` | `AtomicInt(int)` | `load() -> int`, `store(int)`, `add(int) -> int`, `sub(int) -> int`, `compare_exchange(int, int) -> bool` |
| `AtomicBool` | `AtomicBool(bool)` | `load() -> bool`, `store(bool)`, `swap(bool) -> bool`, `compare_exchange(bool, bool) -> bool` |
| `CondVar` | `CondVar()` | `wait(Guard[T])`, `notify_one()`, `notify_all()` |
| `Barrier` | `Barrier(int n)` | `wait()` — blocks until `n` threads call `wait()` |
| `RWLock[T]` | `RWLock[T](T)` | `read() -> ReadGuard[T]`, `write() -> WriteGuard[T]` |
| `ReadGuard[T]` | from `lock.read()` | `get() -> T`; dropping releases the read lock |
| `WriteGuard[T]` | from `lock.write()` | `get() -> T`, `set(T)`; dropping releases the write lock |

All atomics use sequential consistency (`__ATOMIC_SEQ_CST`). Module-level sync primitives can be declared without `static`:

```gorget
from std.sync import AtomicInt, Barrier

AtomicInt counter = AtomicInt(0)
Barrier b = Barrier(2)
```

`CondVar.wait(g)` must be called while `g` (a `Guard[T]`) is held; it atomically releases the lock, sleeps until notified, and re-acquires before returning.

**`std.thread`** — OS threads

```gorget
from std.thread import Thread, thread_spawn, current_thread_id
```

| Name | Signature | Description |
|---|---|---|
| `thread_spawn` | `Thread[T](T())` | Spawn a zero-argument function as a new OS thread |
| `Thread[T].join` | `T()` | Block until the thread finishes; returns its result |
| `Thread[T].id` | `int()` | Return the thread's `pthread_t` as an integer |
| `current_thread_id` | `int()` | Return the calling thread's ID |

Threads are preemptive OS threads (pthreads), heavier than async tasks. `thread_spawn` currently requires bare function references — closures with captured state are not yet supported; share state via module-level variables instead.

```gorget
from std.thread import Thread, thread_spawn
from std.sync import AtomicInt

AtomicInt counter = AtomicInt(0)

void increment():
    counter.add(1)

void main():
    Thread[void] t1 = thread_spawn(increment)
    Thread[void] t2 = thread_spawn(increment)
    t1.join()
    t2.join()
    print(counter.load())   # 2
```

Contrast with `spawn`: async tasks share a thread pool cooperatively; OS threads are independent and preemptive.

**`gg.json`** — JSON parsing and serialization

| Name | Kind | Description |
|---|---|---|
| `Json` | enum | JSON value: `Null`, `Bool(bool)`, `Int(int)`, `Float(float)`, `Str(str)`, `Arr(Vector[Json])`, `Obj(Dict[str, Json])` |
| `json_parse` | `Json(str)` | Parse a JSON string into a `Json` value |
| `json_stringify` | `str(Json)` | Serialize a `Json` value to a compact JSON string |
| `json_pretty` | `str(Json)` | Serialize a `Json` value to a pretty-printed JSON string |
| `Serializer` | trait | Serialization backend: `write_bool`, `write_int`, `write_float`, `write_str`, `write_null`, `begin_struct`/`end_struct`, `begin_seq`/`end_seq`, `result` |
| `Serializable` | trait | `void serialize(self, Box[Serializer] ser)` — types implement this to be serialized (derivable via `@derive`) |
| `Deserializer` | trait | Deserialization backend: `read_bool`, `read_int`, `read_float`, `read_str`, `is_null`, `begin_struct`/`end_struct`, `begin_seq`/`end_seq` |
| `Deserializable` | trait | `void deserialize(&self, Box[Deserializer] de)` — types implement this to be deserialized (derivable via `@derive`) |

**`gg.log`** — Structured leveled logging

```gorget
from gg.log import LogLevel, Logger, log_info, log_warn
```

| Name | Kind | Description |
|---|---|---|
| `LogLevel` | enum | `Debug`, `Info`, `Warn`, `Err` |
| `Logger` | struct | Leveled logger with optional timestamps and prefix |
| `Logger.new` | `Logger(LogLevel)` | Create logger with minimum level |
| `Logger.with_prefix` | `Logger(LogLevel, str)` | Logger with prefix string |
| `Logger.with_timestamps` | `Logger(LogLevel)` | Logger with `[YYYY-MM-DD HH:MM:SS]` timestamps |
| `Logger.set_level` | `void(&self, LogLevel)` | Change minimum level at runtime |
| `Logger.debug` | `void(&self, str)` | Log at Debug level |
| `Logger.info` | `void(&self, str)` | Log at Info level |
| `Logger.warn` | `void(&self, str)` | Log at Warn level |
| `Logger.error` | `void(&self, str)` | Log at Error level |
| `log_debug` | `void(str)` | Module-level Debug (always emits) |
| `log_info` | `void(str)` | Module-level Info (always emits) |
| `log_warn` | `void(str)` | Module-level Warn (always emits) |
| `log_error` | `void(str)` | Module-level Error (always emits) |

Messages below the logger's minimum level are suppressed. Module-level convenience functions always emit (no filtering).

```gorget
from gg.log import LogLevel, Logger

void main():
    Logger log = Logger.new(LogLevel.Info())
    log.debug("skipped")     # suppressed (below Info)
    log.info("hello")        # [INFO] hello
    log.warn("caution")      # [WARN] caution
```

**`gg.toml`** — TOML parsing and serialization

| Name | Kind | Description |
|---|---|---|
| `TomlValue` | enum | TOML value: `Str(str)`, `Int(int)`, `Float(float)`, `Bool(bool)`, `DateTime(str)`, `Array(Vector[TomlValue])`, `Table(Dict[str, TomlValue])` — **`DateTime` stores raw text only; year/month/day fields are not decomposed** |
| `parse` | `TomlValue(str)` | Parse a TOML string into a `TomlValue` |
| `stringify` | `str(TomlValue)` | Serialize a `TomlValue` to a TOML string |

**`gg.xml`** — XML parsing and serialization

| Name | Kind | Description |
|---|---|---|
| `XmlNode` | enum | XML node: `Element` (tag, attributes, children), `Text(str)` |
| `xml_parse` | `XmlNode(str)` | Parse an XML string into an `XmlNode` tree |
| `xml_stringify` | `str(XmlNode)` | Serialize an `XmlNode` tree to an XML string |

**`std.encoding`** — Text encoding/decoding (URL, HTML, UTF-8, Latin-1)

*URL encoding (RFC 3986):*

| Function | Signature | Description |
|---|---|---|
| `url_encode` | `str(str)` | Percent-encode non-unreserved characters |
| `url_decode` | `Result[str, str](str)` | Decode percent-encoded string |
| `form_encode` | `str(str)` | URL-encode for `application/x-www-form-urlencoded` (space → `+`) |
| `form_decode` | `Result[str, str](str)` | Decode form-encoded string (`+` → space) |

*HTML entity escaping:*

| Function | Signature | Description |
|---|---|---|
| `html_escape` | `str(str)` | Escape `& < > " '` to named entities |
| `html_unescape` | `str(str)` | Decode named entities, `&#DDD;`, and `&#xHH;` references |

*UTF-8 utilities:*

| Function | Signature | Description |
|---|---|---|
| `utf8_len` | `int(str)` | Count Unicode codepoints (not bytes) |
| `utf8_codepoints` | `Vector[int](str)` | Extract all codepoints as integers |
| `utf8_is_valid` | `bool(str)` | Validate UTF-8 byte sequence structure |
| `utf8_char_at` | `int(str, int)` | Get nth codepoint (0-indexed); returns -1 if out of range |

*Latin-1 (ISO 8859-1):*

| Function | Signature | Description |
|---|---|---|
| `latin1_encode` | `Result[Vector[uint8], str](str)` | UTF-8 → Latin-1 bytes; fails if any codepoint > 255 |
| `latin1_decode` | `str(Vector[uint8])` | Latin-1 bytes → UTF-8 string |

```gorget
from std.encoding import url_encode, url_decode, html_escape

str enc_url = url_encode("hello world")        # "hello%20world"
auto dec_url = url_decode("hello%20world")      # Ok("hello world")
str safe = html_escape("<b>Tom & Jerry</b>")    # "&lt;b&gt;Tom &amp; Jerry&lt;/b&gt;"
```

**`gg.csv`** — RFC 4180 CSV parsing and serialization

| Function | Signature | Description |
|---|---|---|
| `parse` | `Result[Vector[Vector[str]], str](str)` | Parse CSV string into rows of fields |
| `parse_delim` | `Result[Vector[Vector[str]], str](str, str)` | Parse with custom delimiter (e.g. `"\t"` for TSV) |
| `parse_table` | `Result[CsvTable, str](str)` | Parse CSV where first row = headers |
| `parse_table_delim` | `Result[CsvTable, str](str, str)` | Parse table with custom delimiter |
| `stringify` | `str(Vector[Vector[str]])` | Serialize rows to CSV string (CRLF line endings) |
| `stringify_delim` | `str(Vector[Vector[str]], str)` | Serialize with custom delimiter |
| `stringify_table` | `str(CsvTable)` | Serialize table (headers + rows) to CSV |
| `stringify_table_delim` | `str(CsvTable, str)` | Serialize table with custom delimiter |

`CsvTable` methods: `row_count()`, `col_count()`, `headers()`, `row(int)`, `get(int, int)`, `get_named(int, str)`, `has_column(str)`, `column_index(str)`.

```gorget
from gg.csv import parse_table, stringify, CsvTable
from std.collections import Vector

auto result = parse_table("name,age\nAlice,30\nBob,25\n")
match result:
    case Ok(tbl):
        print(tbl.row_count())           # 2
        print(tbl.get_named(0, "name"))  # Alice
    else:
        print("error")
```

**`std.bytes`** — Byte manipulation

| Function | Signature | Description |
|---|---|---|
| `bytes_from_str` | `Vector[uint8](str)` | Convert string to byte vector |
| `bytes_to_str` | `Result[str, str](Vector[uint8])` | Convert byte vector to string (validates UTF-8) |
| `bytes_from_hex` | `Vector[uint8](str)` | Decode hex string to bytes |
| `bytes_to_hex` | `str(Vector[uint8])` | Encode bytes as hex string |
| `bytes_write_u32_be` | `void(Vector[uint8], int)` | Write 32-bit big-endian integer |
| `bytes_read_u32_be` | `int(Vector[uint8])` | Read 32-bit big-endian integer |
| `bytes_write_u16_be` | `void(Vector[uint8], int)` | Write 16-bit big-endian integer |
| `bytes_read_u16_be` | `int(Vector[uint8])` | Read 16-bit big-endian integer |
| `bytes_concat` | `Vector[uint8](Vector[uint8], Vector[uint8])` | Concatenate two byte vectors |
| `bytes_slice` | `Vector[uint8](Vector[uint8], int, int)` | Slice a byte vector |
| `random_bytes` | `Vector[uint8](int)` | Generate random bytes |

**`gg.crypto`** — Cryptography

| Name | Kind | Description |
|---|---|---|
| `crypto_sha256` | `str(str)` | SHA-256 hash (hex string) |
| `crypto_sha1` | `str(str)` | SHA-1 hash (hex string) |
| `crypto_hmac` | `str(str, str, str)` | HMAC (algorithm, key, message) |
| `crypto_aes_ctr_new` | `CipherContext(Vector[uint8], Vector[uint8])` | Create AES-CTR cipher context |
| `CipherContext` | struct | Cipher state with `encrypt`/`decrypt` methods |
| `BigNum` | struct | Arbitrary-precision integer for cryptographic operations |
| `crypto_bn_from_bytes` | `BigNum(Vector[uint8])` | Create BigNum from bytes |
| `crypto_bn_to_bytes` | `Vector[uint8](BigNum)` | Convert BigNum to bytes |
| `crypto_bn_mod_exp` | `BigNum(BigNum, BigNum, BigNum)` | Modular exponentiation |
| `RSAKey` | struct | RSA public key |
| `crypto_rsa_load_public` | `RSAKey(Vector[uint8], Vector[uint8])` | Load RSA public key (n, e) |
| `crypto_rsa_verify` | `bool(RSAKey, Vector[uint8], Vector[uint8])` | RSA signature verification |
| `crypto_random_bytes` | `Vector[uint8](int)` | Cryptographically secure random bytes |

**`gg.http`** — HTTP client

Supports HTTP and HTTPS (TLS). Follows redirects automatically (up to 5 hops).

```gorget
from gg.http import get, post, put, delete, patch, HttpResponse

Result[HttpResponse, str] r = get("https://api.example.com/data")
if r.is_ok():
    HttpResponse resp = r.unwrap()
    print("{resp.status_code}")   # e.g. 200
    print(resp.body_text)         # response body as string
    print(resp.headers["content-type"])
```

`HttpResponse` fields:

| Field | Type | Description |
|---|---|---|
| `status_code` | `int` | HTTP status code (200, 404, …) |
| `body_text` | `str` | Response body decoded as UTF-8 |
| `headers` | `Dict[str, str]` | Response headers (lowercase names) |

Client functions (all return `Result[HttpResponse, str]`):

| Function | Signature | Description |
|---|---|---|
| `get` | `(str url, Dict[str,str] headers={})` | GET request |
| `post` | `(str url, str body="", Dict[str,str] headers={})` | POST request |
| `put` | `(str url, str body="", Dict[str,str] headers={})` | PUT request |
| `delete` | `(str url, Dict[str,str] headers={})` | DELETE request |
| `patch` | `(str url, str body="", Dict[str,str] headers={})` | PATCH request |

Helper:

| Function | Signature | Description |
|---|---|---|
| `parse_url` | `(str url) → (str host, int port, str path, bool use_tls)` | Parse a URL into components |

---

**`gg.httpserver`** — HTTP/1.1 server

Provides a `Router`-based request dispatcher, keep-alive connection handling, TLS support, middleware pipeline, and static file serving.

```gorget
from gg.httpserver import Router, HttpRequest, HttpServerResponse
from std.net.socket import ServerSocket, Socket, server_socket_bind
from std.time import sleep

HttpServerResponse handle_hello(HttpRequest req):
    return HttpServerResponse.ok("hello, " + req.params["name"])

async void main():
    Router router = Router.new()
    router.get("/hello/:name", handle_hello)
    router.use((req, resp): resp.with_header("x-powered-by", "gorget"))

    int port = 8080
    ServerSocket srv = server_socket_bind("0.0.0.0", port).unwrap()
    while true:
        Socket conn = srv.accept().unwrap()
        spawn http_server_handle_conn_ka(conn, (req): router.dispatch(req), 30000)
```

**`HttpRequest`** fields:

| Field | Type | Description |
|---|---|---|
| `method` | `str` | `"GET"`, `"POST"`, `"PUT"`, `"DELETE"`, `"PATCH"`, … |
| `path` | `str` | URL path (e.g. `"/api/users"`) |
| `query_string` | `str` | Raw query string without leading `?` |
| `http_version` | `str` | `"HTTP/1.1"` |
| `headers` | `Dict[str, str]` | Request headers (lowercase names) |
| `body` | `str` | Request body |
| `params` | `Dict[str, str]` | URL path parameters captured by the Router |

`HttpRequest` methods:

| Method | Returns | Description |
|---|---|---|
| `query_params()` | `Dict[str, str]` | Parse `query_string` into key/value pairs |

**`HttpServerResponse`** fields:

| Field | Type | Description |
|---|---|---|
| `status_code` | `int` | HTTP status code |
| `body` | `str` | Response body |
| `content_type` | `str` | `Content-Type` header value |
| `headers` | `Dict[str, str]` | Additional response headers |

`HttpServerResponse` factory methods (static):

| Method | Description |
|---|---|
| `HttpServerResponse.ok(str body)` | 200 text/plain |
| `HttpServerResponse.html(str body)` | 200 text/html |
| `HttpServerResponse.json(str body)` | 200 application/json |
| `HttpServerResponse.not_found()` | 404 |
| `HttpServerResponse.bad_request(str msg)` | 400 |
| `HttpServerResponse.internal_error(str msg)` | 500 |
| `HttpServerResponse.redirect(str location)` | 302 with `Location` header |

Instance methods:

| Method | Returns | Description |
|---|---|---|
| `with_header(str name, str value)` | `HttpServerResponse` | Return copy with additional header |
| `status_text()` | `str` | Human-readable status (e.g. `"OK"`, `"Not Found"`) |

**`Router`** — URL-pattern dispatcher:

```gorget
Router router = Router.new()
router.get("/users/:id", handle_user)    # GET
router.post("/users", handle_create)     # POST
router.put("/users/:id", handle_update)  # PUT
router.delete("/users/:id", handle_del)  # DELETE
router.patch("/users/:id", handle_patch) # PATCH
router.use(cors_middleware)              # middleware hook

HttpServerResponse resp = router.dispatch(req)
```

`Router` registers routes for exact paths (e.g. `/hello`) and parameterized paths (e.g. `/users/:id`). Path parameters are populated into `req.params`. The router automatically handles:

- **`HEAD`** — runs the matching `GET` handler and strips the response body
- **`OPTIONS`** — returns 200 with an `Allow` header listing registered methods for the path; returns 404 if no routes match

**Middleware** hooks run in registration order after the matched handler:

```gorget
# Hook receives (request, response) and returns the (modified) response
router.use((req, resp): resp.with_header("access-control-allow-origin", "*"))
```

**Connection handlers** (low-level):

| Function | Description |
|---|---|
| `http_server_handle_conn_ka(Socket, handler, timeout_ms)` | Keep-alive HTTP connection handler (async) |
| `http_server_handle_conn_tls_ka(TlsSocket, handler, timeout_ms)` | Keep-alive HTTPS connection handler (async) |

**TLS server**:

```gorget
from gg.httpserver import HttpServer

HttpServer srv = HttpServer.new_tls("0.0.0.0", 8443, "/path/cert.pem", "/path/key.pem")
srv.serve((req): router.dispatch(req)).await()
```

**Static file serving**:

```gorget
from gg.httpserver import http_serve_file, http_mime_type

router.get("/static/:file", (req): http_serve_file("./public", req))
```

`parse_query_string(str) → Dict[str, str]` — parse a raw query string.
`http_mime_type(str path) → str` — return MIME type for a file path by extension.

---

**`std.net.socket`** — TCP sockets

| Name | Kind | Description |
|---|---|---|
| `Socket` | struct | TCP socket with `read`, `read_exact`, `write`, `write_str`, `read_line` (returns `Result[String, str]`), `set_timeout`, `close` methods |
| `socket_connect` | `Result[Socket, str](str, int)` | Connect to host:port |

**`gg.gfx`** — Canvas graphics

| Name | Kind | Description |
|---|---|---|
| `Canvas` | struct | Drawing canvas |
| `Color` | struct | RGBA color |
| `open` | `Result[Canvas, str](str, int, int)` | Open a canvas (title, width, height) |
| `close` | `void(Canvas)` | Close a canvas |
| `fill_circle` | `void(Canvas, int, int, int, Color)` | Draw filled circle |
| `draw_circle` | `void(Canvas, int, int, int, Color)` | Draw circle outline |

**`gg.sdl`** — SDL2 bindings

Low-level SDL2 bindings for window management, rendering, input handling, and audio. Provides opaque structs (`SDLWindow`, `SDLRenderer`, `SDLTexture`, `SDLFont`, `SDLEvent`), 40+ constants (`SDL_INIT_VIDEO`, `SDL_QUIT`, `SDLK_*` key codes, etc.), and functions covering:

- **Lifecycle:** `sdl_init`, `sdl_quit`, `sdl_create_window`, `sdl_create_renderer`, `sdl_destroy_window`, `sdl_destroy_renderer`
- **Rendering:** `sdl_set_draw_color`, `sdl_clear`, `sdl_present`, `sdl_draw_rect`, `sdl_fill_rect`, `sdl_draw_line`, `sdl_draw_point`
- **Textures:** `sdl_load_texture`, `sdl_draw_texture`, `sdl_draw_texture_ex`, `sdl_destroy_texture`, `sdl_texture_width`, `sdl_texture_height`
- **Text:** `sdl_load_font`, `sdl_draw_text`, `sdl_text_width`, `sdl_close_font`
- **Input:** `sdl_poll_event`, `sdl_event_type`, `sdl_event_key`, `sdl_event_mouse_x`, `sdl_event_mouse_y`, `sdl_event_mouse_button`
- **Time:** `sdl_delay`, `sdl_ticks`

**`gg.ecs`** — Entity Component System

| Name | Kind | Description |
|---|---|---|
| `EntityPool` | struct | Entity ID allocator with `create() → int`, `destroy(int)`, `is_alive(int) → bool` methods |
| `SparseSet` | struct | Sparse-set storage for component data with `add`, `remove`, `get`, `has`, `len` methods |

**`gg.ssh`** — SSH client

| Name | Kind | Description |
|---|---|---|
| `Session` | struct | SSH session with `exec(str) → CommandResult` method for remote command execution |
| `CommandResult` | struct | Command execution result (exit code, output) |
| `connect` | `Session(...)` | Establish SSH connection (host, port, username, password, crypto parameters) |

The SSH module implements the SSH-2 protocol including key exchange, encryption, and SFTP operations.

**`gg.regex`** — Regular expressions (PCRE2)

| Name | Kind | Description |
|---|---|---|
| `Regex` | struct | Compiled regular expression pattern |
| `Match` | struct | Result of a successful match (text, offsets, groups) |
| `regex_compile` | `Result[Regex, str](str)` | Compile a regex pattern |
| `regex_compile_with` | `Result[Regex, str](str, str)` | Compile with flags (`"i"`, `"m"`, `"s"`, `"x"`, `"u"`, `"U"`) |
| `regex_escape` | `String(str)` | Escape special regex characters in a string |
| `regex_is_match` | `bool(str, str)` | Convenience: compile + test (pattern, subject) |
| `regex_find` | `Option[Match](str, str)` | Convenience: compile + find first match |
| `regex_replace` | `String(str, str, str)` | Convenience: compile + replace first match |

**Regex methods:**

| Method | Signature | Description |
|---|---|---|
| `is_match` | `bool(self, str)` | Test if pattern matches anywhere in subject |
| `find` | `Option[Match](self, str)` | Find first match |
| `find_at` | `Option[Match](self, str, int)` | Find first match starting at byte offset |
| `find_all` | `Vector[Match](self, str)` | Find all non-overlapping matches |
| `replace` | `String(self, str, str)` | Replace first match |
| `replace_all` | `String(self, str, str)` | Replace all matches |
| `split` | `Vector[str](self, str)` | Split string by pattern |
| `splitn` | `Vector[str](self, str, int)` | Split with maximum number of parts |
| `fullmatch` | `Option[Match](self, str)` | Match entire string |
| `capture_count` | `int(self)` | Number of capture groups |
| `group_names` | `Vector[str](self)` | Names of named capture groups |
| `pattern_str` | `str(self)` | Original pattern string |

**Match methods:**

| Method | Signature | Description |
|---|---|---|
| `text` | `str(self)` | Full matched text |
| `start` | `int(self)` | Start byte offset |
| `end_pos` | `int(self)` | End byte offset |
| `group` | `Option[str](self, int)` | Get capture group by index (0-based) |
| `group_by_name` | `Option[str](self, str)` | Get capture group by name |
| `groups` | `Vector[str](self)` | All capture group values |
| `group_count` | `int(self)` | Number of capture groups |

Flags for `regex_compile_with`: `i` (case-insensitive), `m` (multiline), `s` (dotall), `x` (extended), `u` (Unicode), `U` (ungreedy). Requires PCRE2 (`libpcre2-8`).

**`gg.tensor`** — N-dimensional numeric tensors (NumPy-equivalent)

Gorget's tensor library provides strided N-dimensional arrays with O(1) view operations (reshape, transpose, slice). Tensors use `Shared[Vector[T]]` internally so views share the same backing buffer — mutations through one view are visible through all views sharing the same storage. Call `.contiguous()` for an independent deep copy.

```gorget
from gg.tensor import Tensor, tensor_arange, tensor_zeros_int, tensor_linspace,
    tensor_display_int, tensor_matmul_int, tensor_dot_int, tensor_eq_int
```

**Construction:**

| Name | Signature | Description |
|---|---|---|
| `tensor_arange` | `Tensor[int](int start, int stop)` | `[start, start+1, ..., stop-1]` |
| `tensor_linspace` | `Tensor[float](float start, float stop, int num)` | Evenly spaced floats |
| `tensor_zeros_int` | `Tensor[int](Vector[int] shape)` | All-zero int tensor |
| `tensor_zeros_float` | `Tensor[float](Vector[int] shape)` | All-zero float tensor |
| `tensor_ones_int` | `Tensor[int](Vector[int] shape)` | All-one int tensor |
| `tensor_ones_float` | `Tensor[float](Vector[int] shape)` | All-one float tensor |
| `tensor_full_int` | `Tensor[int](Vector[int] shape, int val)` | Fill with constant |
| `tensor_full_float` | `Tensor[float](Vector[int] shape, float val)` | Fill with constant |

**Introspection & Element Access (equip Tensor[T]):**

| Method | Signature | Description |
|---|---|---|
| `tensor_shape` | `Vector[int](self)` | Shape dimensions |
| `ndim` | `int(self)` | Number of dimensions |
| `size` | `int(self)` | Total element count |
| `flat_get` | `T(self, int i)` | Get element at flat index |
| `flat_set` | `void(&self, int i, T val)` | Set element at flat index |
| `at` | `T(self, Vector[int] indices)` | Multi-index element access |
| `set_at` | `void(&self, Vector[int] indices, T val)` | Multi-index element write |

**Arithmetic (equip Tensor[T] with Add/Sub/Mul/Div):** `a + b`, `a - b`, `a * b`, `a / b`, `-a`

**Scalar ops:** `.scalar_add(v)`, `.scalar_sub(v)`, `.scalar_mul(v)`, `.scalar_div(v)`

**Broadcasting:** `.broadcast_add(other)`, `.broadcast_sub(other)`, `.broadcast_mul(other)`, `.broadcast_div(other)` — NumPy-style shape broadcasting

**Reductions:** `.sum_all()`, `.min_all()`, `.max_all()`, `.sum_axis(axis)`, `.min_axis(axis)`, `.max_axis(axis)`. Float-specific: `tensor_mean_all(t)`, `tensor_std_all(t)`, `tensor_mean_axis(t, axis)`

**O(1) View Operations:**

| Method | Signature | Description |
|---|---|---|
| `reshape` | `Tensor[T](self, Vector[int] new_shape)` | New shape, shared storage |
| `transpose` | `Tensor[T](self)` | Swap first two dims (2D) |
| `permute` | `Tensor[T](self, Vector[int] axes)` | Reorder all dimensions |
| `tensor_slice` | `Tensor[T](self, int axis, int start, int end)` | Slice along one axis |
| `contiguous` | `Tensor[T](self)` | Force independent copy |

**Linear Algebra:**

| Name | Signature | Description |
|---|---|---|
| `tensor_matmul_int` | `Tensor[int](Tensor[int] a, Tensor[int] b)` | Matrix multiply `[m,k] @ [k,n]` |
| `tensor_matmul_float` | `Tensor[float](Tensor[float] a, Tensor[float] b)` | Float matmul |
| `tensor_dot_int` | `int(Tensor[int] a, Tensor[int] b)` | 1D dot product |
| `tensor_dot_float` | `float(Tensor[float] a, Tensor[float] b)` | 1D dot product |

**Comparisons (int):** `tensor_eq_int`, `tensor_lt_int`, `tensor_gt_int`, `tensor_le_int`, `tensor_ge_int`, `tensor_ne_int` — return `Tensor[bool]`

**Comparisons (float):** `tensor_eq_float`, `tensor_lt_float`, `tensor_gt_float`, `tensor_le_float`, `tensor_ge_float`, `tensor_ne_float` — return `Tensor[bool]`

**Display:** `tensor_display_int(t)`, `tensor_display_float(t)` — format as `Tensor(shape=[2,3], [0, 1, 2, ...])`

---

**`gg.dataframe`** — Tabular data manipulation (pandas-equivalent)

DataFrames store heterogeneous typed columns with optional null masking. Columns can be `int`, `float`, `str`, or `bool` type.

```gorget
from gg.dataframe import DataFrame, Column, df_from_columns, df_from_csv,
    col_from_ints, col_from_strs, col_from_floats, col_from_bools,
    df_group_by, df_inner_join, df_concat
```

**Construction:**

| Name | Signature | Description |
|---|---|---|
| `df_from_columns` | `DataFrame(Vector[str] names, Vector[Column] cols)` | Build from typed columns |
| `df_from_csv` | `Result[DataFrame, str](str csv_text)` | Parse CSV with type inference |
| `df_from_records` | `DataFrame(Vector[str] headers, Vector[Vector[str]] rows)` | Build from string rows |
| `col_from_ints` | `Column(Vector[int] data)` | Create int column (all present) |
| `col_from_floats` | `Column(Vector[float] data)` | Create float column (all present) |
| `col_from_strs` | `Column(Vector[str] data)` | Create str column (all present) |
| `col_from_bools` | `Column(Vector[bool] data)` | Create bool column (all present) |
| `col_ints_null` | `Column(Vector[int] data, Vector[bool] mask)` | Create nullable int column; mask[i]=false means null |
| `col_strs_null` | `Column(Vector[str] data, Vector[bool] mask)` | Create nullable str column; mask[i]=false means null |

**DataFrame methods (equip DataFrame):**

| Method | Signature | Description |
|---|---|---|
| `nrows` | `int(self)` | Row count |
| `ncols` | `int(self)` | Column count |
| `has_column` | `bool(self, str name)` | Column existence check |
| `get_column` | `Column(self, str name)` | Get a column by name |
| `column_names` | `Vector[str](self)` | All column names |
| `head` | `DataFrame(self, int n)` | First n rows |
| `tail` | `DataFrame(self, int n)` | Last n rows |
| `df_slice` | `DataFrame(self, int start, int end)` | Row slice `[start, end)` |
| `select` | `DataFrame(self, Vector[str] cols)` | Keep listed columns |
| `drop` | `DataFrame(self, str col)` | Drop one column |
| `rename` | `DataFrame(self, str old, str new)` | Rename column |
| `filter_by` | `DataFrame(self, str col, str op, str val)` | Filter rows (`"=="`, `"!="`, `"<"`, `">"`, `"<="`, `">="`) |
| `sort_by` | `DataFrame(self, str col, bool ascending)` | Sort rows |
| `add_column` | `DataFrame(self, str name, Column col)` | Append a column |
| `drop_column` | `DataFrame(self, str name)` | Remove a column |
| `apply_int` | `DataFrame(self, str col, int(int) f)` | Map function over int column |
| `apply_float` | `DataFrame(self, str col, float(float) f)` | Map function over float column |
| `apply_str` | `DataFrame(self, str col, str(str) f)` | Map function over str column |
| `fillna_int` | `DataFrame(self, str col, int val)` | Fill nulls in int column |
| `fillna_float` | `DataFrame(self, str col, float val)` | Fill nulls in float column |
| `fillna_str` | `DataFrame(self, str col, str val)` | Fill nulls in str column |
| `dropna` | `DataFrame(self)` | Remove rows with any null |
| `df_sum` | `str(self, str col)` | Column sum (as string) |
| `df_mean` | `str(self, str col)` | Column mean |
| `df_min` | `str(self, str col)` | Column minimum |
| `df_max` | `str(self, str col)` | Column maximum |
| `df_count` | `int(self, str col)` | Non-null count |
| `df_std` | `str(self, str col)` | Standard deviation |
| `describe` | `DataFrame(self)` | Summary statistics per column |
| `value_counts` | `DataFrame(self, str col)` | Count unique values |

**Column methods (equip Column):**

| Method | Signature | Description |
|---|---|---|
| `col_len` | `int(self)` | Number of rows |
| `dtype_name` | `str(self)` | `"int"`, `"float"`, `"str"`, or `"bool"` |
| `value_as_str` | `str(self, int idx)` | Get value as string (`"null"` if masked) |
| `is_null_at` | `bool(self, int idx)` | True if row is null |
| `null_count` | `int(self)` | Number of null rows |

**GroupBy & Joins:**

| Name | Signature | Description |
|---|---|---|
| `df_group_by` | `GroupBy(DataFrame df, str col)` | Group by column |
| `.agg` | `DataFrame(self, str target, str op)` | Aggregate groups (`"sum"`, `"mean"`, `"min"`, `"max"`, `"count"`) |
| `df_inner_join` | `DataFrame(DataFrame left, DataFrame right, str on)` | Inner join on key column |
| `df_concat` | `DataFrame(DataFrame a, DataFrame b)` | Vertical concat (same schema) |
| `df_to_csv` | `str(DataFrame df)` | Serialize to CSV string |

**`std.alloc`** — Memory Allocators

Six allocators provide explicit control over memory allocation strategy. All share a common vtable interface and can be composed with collections via scoped binding (`with`) or per-object direction (`alloc=`). The compiler's escape analysis prevents dangling references to allocator-scoped memory at compile time.

```gorget
from std.alloc import Arena, PoolAllocator, TlsfAllocator, TrackingAllocator, FixedBufferAllocator, FallbackAllocator
```

**Arena** — Bump allocator. Fast sequential allocation; all memory freed at once on destroy or reset.

| Name | Signature | Description |
|---|---|---|
| *constructor* | `Arena(int capacity)` | Create arena with given byte capacity |
| `bytes_used` | `int(self)` | Total bytes currently allocated |
| `reset` | `void(self)` | Free all allocations (reuse arena memory) |
| `destroy` | `void(self)` | Release the arena and all its memory |

**PoolAllocator** — Fixed-size block pool. Pre-allocated blocks with O(1) alloc/free from a free list.

| Name | Signature | Description |
|---|---|---|
| *constructor* | `PoolAllocator(int block_size, int initial_count)` | Create pool with given block size (bytes) and initial block count |
| `used_blocks` | `int(self)` | Number of currently allocated blocks |
| `free_blocks` | `int(self)` | Number of available blocks |
| `total_blocks` | `int(self)` | Total blocks (used + free) |
| `block_size` | `int(self)` | Size of each block in bytes |
| `reset` | `void(self)` | Return all blocks to the free list |
| `destroy` | `void(self)` | Release the pool and all its memory |

**TlsfAllocator** — Two-Level Segregated Fit allocator. Variable-size allocation with O(1) worst-case and automatic coalescing of freed blocks.

| Name | Signature | Description |
|---|---|---|
| *constructor* | `TlsfAllocator(int pool_size)` | Create allocator with given pool size in bytes (default 65536) |
| `bytes_used` | `int(self)` | Total bytes currently allocated |
| `peak_bytes` | `int(self)` | High-water mark of bytes allocated |
| `pool_size` | `int(self)` | Total pool capacity in bytes |
| `reset` | `void(self)` | Free all allocations and reset the pool |
| `destroy` | `void(self)` | Release the allocator and all its memory |

**TrackingAllocator** — Instrumentation wrapper. Wraps the current active allocator to record allocation statistics.

| Name | Signature | Description |
|---|---|---|
| *constructor* | `TrackingAllocator()` | Create tracker wrapping the current active allocator |
| `bytes_allocated` | `int(self)` | Cumulative bytes allocated |
| `bytes_freed` | `int(self)` | Cumulative bytes freed |
| `current_bytes` | `int(self)` | Bytes currently in use (`allocated - freed`) |
| `peak_bytes` | `int(self)` | High-water mark of concurrent bytes |
| `alloc_count` | `int(self)` | Total number of allocations |
| `free_count` | `int(self)` | Total number of frees |
| `realloc_count` | `int(self)` | Total number of reallocations |
| `report` | `void(self)` | Print allocation statistics to stderr |
| `reset` | `void(self)` | Reset all counters to zero |
| `destroy` | `void(self)` | Release the tracker |

**FixedBufferAllocator** — Stack/static-buffer bump allocator. Allocates from a caller-provided buffer of fixed size; zero heap usage after construction.

| Name | Signature | Description |
|---|---|---|
| *constructor* | `FixedBufferAllocator(int capacity)` | Create bump allocator with given byte capacity |
| `bytes_used` | `int(self)` | Bytes currently allocated from the buffer |
| `capacity` | `int(self)` | Total buffer size in bytes |
| `reset` | `void(self)` | Free all allocations (reuse buffer from start) |
| `destroy` | `void(self)` | Release the allocator |

When the buffer is exhausted, allocations return `NULL`. Pair with `FallbackAllocator` to overflow gracefully to a secondary allocator.

**FallbackAllocator** — Allocator combinator. Tries a primary allocator first; on failure falls back to a secondary allocator. Individual frees are no-ops (designed for bulk-free primaries like `FixedBufferAllocator` or `Arena`).

| Name | Signature | Description |
|---|---|---|
| *constructor* | `FallbackAllocator(primary, secondary)` | Create combinator wrapping any two allocators |
| `primary_count` | `int(self)` | Number of allocations served by the primary |
| `fallback_count` | `int(self)` | Number of allocations served by the secondary |
| `destroy` | `void(self)` | Release the combinator (does not destroy primary/secondary) |

```gorget
from std.alloc import FixedBufferAllocator, FallbackAllocator, Arena
from std.collections import Vector

void main():
    FixedBufferAllocator fba = FixedBufferAllocator(128)
    Arena overflow = Arena(65536)
    with FallbackAllocator(fba, overflow) as fb:
        Vector[int] v = Vector[int]()
        int i = 0
        while i < 100:
            v.push(i)
            i = i + 1
        print("fallback_count: {fb.fallback_count()}")
    print("done")
```

**Scoped binding (`with`)**

The `with` statement binds an allocator to a block scope. All allocations inside the block — including those from collections like `Vector`, `List`, and `Dict` — automatically use the bound allocator. On block exit, the allocator is destroyed and all its memory is freed.

```gorget
from std.alloc import Arena
from std.collections import Vector

void main():
    with Arena(4096) as a:
        Vector[int] v = Vector[int]()
        v.push(1)
        v.push(2)
        print("bytes: {a.bytes_used()}")
    # arena destroyed here; all memory freed
```

**Composable allocation (`alloc=`)**

Collections accept a named `alloc` parameter to direct their allocations to a specific allocator without scoped binding. This is useful when the allocator's lifetime extends beyond a single block.

```gorget
from std.alloc import PoolAllocator
from std.collections import Vector

void main():
    PoolAllocator pool = PoolAllocator(128, 32)
    Vector[int] v = Vector[int](alloc=pool)
    v.push(10)
    v.push(20)
    print("used: {pool.used_blocks()}")
```

The `alloc=` parameter is accepted by `Vector`, `List`, `Array`, `Dict`, `HashMap`, `Set`, `HashSet`, `Channel`, and `String` constructors.

**Escape analysis**

The compiler rejects programs where allocator-scoped data escapes the allocator's lifetime. This prevents use-after-free at compile time.

```gorget
# COMPILE ERROR: cannot assign arena-scoped value to outer variable
from std.alloc import Arena
from std.collections import Vector

void main():
    Vector[int] outer = Vector[int]()
    with Arena(4096) as a:
        outer = !Vector[int]()  # error: cannot assign arena-scoped value
```

Returning an allocator-scoped value from a function is also rejected:

```gorget
# COMPILE ERROR: cannot return arena-scoped value
from std.alloc import Arena
from std.collections import Vector

Vector[int] make_in_arena():
    with Arena(4096) as a:
        Vector[int] v = Vector[int]()
        v.push(1)
        return v  # error: cannot return arena-scoped value
```

Escape analysis applies to all allocator types, not just `Arena`.

**Nesting**

Allocator scopes nest naturally. An allocator created inside a `with` block uses the outer allocator for its own internal metadata; each scope's allocations are independent.

```gorget
from std.alloc import Arena
from std.collections import Vector

void main():
    with Arena(4096) as outer:
        Vector[int] v1 = Vector[int]()
        v1.push(10)
        with Arena(1024) as inner:
            Vector[int] v2 = Vector[int]()
            v2.push(20)
            print("inner: {v2.get(0).unwrap()}")
        print("outer: {v1.get(0).unwrap()}")
```

**When to use which allocator:**

| Allocator | Category | Best for |
|---|---|---|
| `Arena` | Bump (no individual free) | Temporary work buffers, request-scoped data, parsing |
| `PoolAllocator` | Fixed-size free-list | Object pools, ECS components, list nodes |
| `TlsfAllocator` | General-purpose O(1) | Real-time systems, embedded, latency-sensitive code |
| `TrackingAllocator` | Instrumentation wrapper | Profiling, leak detection, allocation auditing |
| `FixedBufferAllocator` | Stack/static-buffer bump | Zero-heap hot paths, embedded targets, scratch buffers |
| `FallbackAllocator` | Combinator | Primary-fast + secondary-safe overflow; composing any two allocators |

---

## 16. Compilation Model

### 16.1 Pipeline

```
.gg source → Lexer → Parser → Semantic Analysis → Code Generation → cc → Binary
```

1. **Lexer**: Tokenizes source, producing INDENT/DEDENT tokens for indentation.
2. **Parser**: Recursive descent parser produces an AST.
3. **Semantic Analysis** (5 passes):
   - Pass 1: Collect top-level definitions
   - Pass 2: Resolve names in all bodies
   - Pass 3: Build trait/impl registry
   - Pass 4: Type checking and inference
   - Pass 5: Borrow checking (ownership validation)
4. **Code Generation**: Emit C source code.
5. **C Compiler**: Compile C to native binary via `cc`.

### 16.2 Semantic Errors

| Error                        | Cause                                                |
|------------------------------|------------------------------------------------------|
| `UndefinedName`              | Name not found in any enclosing scope                |
| `DuplicateDefinition`        | Same name defined twice in the same scope            |
| `TypeMismatch`               | Expression type doesn't match expected type          |
| `WrongArgCount`              | Function called with wrong number of arguments       |
| `NotAFunction`               | Calling something that isn't callable                |
| `NotAType`                   | Using a non-type where a type is expected            |
| `NotAStruct`                 | Struct literal for something that isn't a struct     |
| `MissingTraitMethod`         | Trait impl is missing a required method              |
| `NoMethodFound`              | Method doesn't exist on type                         |
| `CannotInferType`            | Insufficient info for `auto` inference               |
| `NoFieldFound`               | Field doesn't exist on struct                        |
| `DuplicateImpl`              | Duplicate trait implementation                       |
| `MethodSignatureMismatch`    | Method signature doesn't match trait definition      |
| `BreakOutsideLoop`           | `break`/`continue` outside of loop                   |
| `ReturnOutsideFunction`      | `return` outside of function                         |
| `ThrowInNonThrowingFunction` | `throw` in function without `throws`                 |
| `UseAfterMove`               | Variable used after ownership was moved              |
| `MoveWithoutOperator`        | Non-Copy type passed without `!` or `move`         |
| `BorrowConflict`             | Borrow exclusivity violated (aliasing in call)       |
| `MoveInLoop`                 | Moving a variable inside a loop body                 |
| `DoubleMove`                 | Same variable moved more than once                   |
| `OwnershipMismatch`          | Call-site annotation doesn't match param declaration |
| `NonPrintableInterpolation`  | Non-primitive type in string interpolation            |
| `UnknownDirective`           | Unrecognized directive name                          |
| `AssignmentToConst`          | Assignment to a `const` binding (always an error)    |

### 16.3 Directives

Directives set per-file compilation options directly in source code.
They must appear at the top of the file before any other items.

```gorget
directive strip-asserts
directive overflow=wrap
```

**Syntax:**

```ebnf
directive = "directive" name [ "=" value ] ;
name      = IDENT { "-" IDENT } ;
value     = IDENT ;
```

**Available directives:**

| Directive                          | Equivalent CLI flag   | Effect                                    |
|------------------------------------|-----------------------|-------------------------------------------|
| `directive strip-asserts`          | `--strip-asserts`     | Remove all `assert` statements from build |
| `directive overflow=wrap`          | `--overflow=wrap`     | Enable wrapping arithmetic (no overflow panic) |
| `directive trace`                  | `--trace`             | Enable execution tracing for testing      |
| `directive hot-reload`             | `--hot-reload`        | Enable hot code reload mode               |
| `directive scheduler=X`           | `--scheduler=X`       | Select spawn scheduler: `pool` (default), `thread`, `inline`, `single` |

#### Scheduler Backends

The `scheduler` directive selects the runtime execution model for `spawn`:

| Value    | Model                          | Use case                              |
|----------|--------------------------------|---------------------------------------|
| `pool`   | M:N thread pool + work-stealing | Async-heavy, general purpose (default) |
| `thread` | 1:1 OS thread per spawn        | CPU-bound, FFI, simple debugging      |
| `inline` | Synchronous on caller thread   | Tests, deterministic replay, WASM     |
| `single` | N:1 cooperative event loop     | Embedded, scripts, low overhead       |

The CLI flag `--scheduler=X` overrides the source directive.

**Interaction with CLI flags:** Source directives and CLI flags are merged so
that either can enable an option. However, if the CLI explicitly contradicts a
source directive, the CLI flag prevails. This lets build systems override
per-file options without editing source code.

| Source directive          | CLI flag              | Result           |
|---------------------------|-----------------------|------------------|
| `directive strip-asserts` | *(none)*              | asserts stripped |
| *(none)*                  | `--strip-asserts`     | asserts stripped |
| `directive strip-asserts` | `--no-strip-asserts`  | asserts kept     |
| `directive overflow=wrap` | *(none)*              | wrapping         |
| *(none)*                  | `--overflow=wrap`     | wrapping         |
| `directive overflow=wrap` | `--overflow=checked`  | checked (panic)  |
| `directive trace`         | *(none)*              | tracing enabled  |
| *(none)*                  | `--trace`             | tracing enabled  |
| `directive trace`         | `--no-trace`          | tracing disabled |
| `directive scheduler=inline` | *(none)*          | inline scheduler |
| *(none)*                  | `--scheduler=thread`  | thread scheduler |
| `directive scheduler=inline` | `--scheduler=pool` | pool scheduler (CLI wins) |

---

## 17. CLI Interface

The Gorget compiler is invoked as `gg` with the following commands:

| Command            | Description                              |
|--------------------|------------------------------------------|
| `gg lex <file>`    | Tokenize and print tokens                |
| `gg parse <file>`  | Parse and print AST                      |
| `gg check <file>`  | Run semantic analysis (no code output)   |
| `gg build <file>`  | Compile to native binary                 |
| `gg run <file>`    | Compile and execute                      |
| `gg test <file>`   | Compile and run tests                    |
| `gg fmt <file>`    | Format source code (prints to stdout; use `-i`/`--in-place` to overwrite) |
| `gg report <file>` | Generate HTML report from trace file     |
| `gg init`          | Initialize a new Gorget project in the current directory |
| `gg new <name>`    | Create a new project directory with scaffolding |
| `gg add <dep>`     | Add a dependency (`--git <url>` or `--path <dir>`) |
| `gg remove <dep>`  | Remove a dependency |
| `gg`               | Launch interactive REPL for experimenting with Gorget code |

**CLI flags:**

| Flag                 | Description                                             |
|----------------------|---------------------------------------------------------|
| `--strip-asserts`    | Remove all `assert` statements                          |
| `--no-strip-asserts` | Keep asserts even if source has `directive strip-asserts`|
| `--overflow=wrap`    | Enable wrapping arithmetic (no overflow panic)          |
| `--overflow=checked` | Force checked arithmetic even if source says `wrap`     |
| `--trace`            | Enable tracing for test execution                       |
| `--no-trace`         | Disable tracing even if source has `directive trace`   |
| `--report html`      | Generate HTML report after testing (implies `--trace`) |
| `--tag <name>`       | Only run tests matching this tag (repeatable)           |
| `--exclude-tag <name>` | Skip tests with this tag (repeatable; exclusion wins) |
| `--filter <substr>`  | Only run tests whose name contains `<substr>`           |
| `--bench`            | Run benchmarks instead of tests                         |
| `--timeout <value>`  | Global test timeout (`5s`, `500ms`, or `5000` for ms)   |
| `--parallel <N>`     | Run tests across N worker processes                     |
| `--failed-only`      | Re-run only previously failed tests                     |
| `--failed-first`     | Run previously failed tests first, then the rest        |
| `--snapshot <cmd>`   | Snapshot subcommand: `save`, `diff`, `list`, `show`, `delete` |
| `--hot-reload`       | Enable hot code reload (builds host + guest shared library) |
| `--shared [-o file]` | Build as a shared library (`.dylib`/`.so`)              |
| `--show-borrows`     | Print inferred borrow analysis for all functions (diagnostic) |
| `-i` / `--in-place`  | Format file in place (for `gg fmt`)                     |

---

## 18. Testing

Gorget has a built-in test framework. Test files use `test` blocks instead of `main()`. The framework uses existing language features wherever possible — `meta for` for parameterized tests, `with` + `Drop` for fixtures, `assert` for contracts — rather than adding dedicated testing DSL.

### 18.1 Test Blocks

```gorget
test "addition works":
    assert 1 + 1 == 2

test "string equality":
    auto s = "hello"
    assert s == "hello"
```

Run with `gg test <file>`. Each test runs in isolation — assertion failures are caught and reported without terminating the process.

#### Scoped Resources

Tests use the standard `with` statement inside the test body for scoped resource management. Resources are automatically cleaned up when the `with` block exits — even if the test fails via assertion.

```gorget
test "reads file":
    with File.open("data.txt") as f:
        auto content = f.read_all().unwrap()
        assert content == "expected"

test "copies data":
    with Resource("a") as a:
        with Resource("b") as b:
            assert a.name == "a"
            assert b.name == "b"
```

If the binding's type implements the `Drop` trait, its `drop()` method is called on both the success and failure paths. This is the same `with` statement used everywhere else in the language (§6.14) — no special test syntax.

### 18.2 Suite Setup and Teardown

```gorget
suite setup:
    print("before all tests")

suite teardown:
    print("after all tests")
```

`suite setup` runs once before all tests. `suite teardown` runs once after all tests. At most one of each per file. Panics in setup/teardown are fatal (terminate the process).

### 18.3 Tag Filtering

```gorget
@tag("smoke")
test "quick check":
    assert true

@tag("slow")
test "long computation":
    assert true
```

Run only tagged tests: `gg test file.gg --tag smoke`. Multiple `--tag` flags select tests matching any tag.

Exclude tagged tests: `gg test file.gg --exclude-tag slow`. If a test's tag is both included (`--tag`) and excluded (`--exclude-tag`), exclusion wins.

### 18.4 Name Filtering

Filter tests by name substring: `gg test file.gg --filter "fibonacci"`. Only tests whose name contains the substring will run. Can be combined with `--tag` and `--exclude-tag`.

### 18.5 `@should_panic`

Tests can be marked to expect a panic:

```gorget
@should_panic
test "assert fails as expected":
    assert 1 == 2

@should_panic("left == right")
test "assert with message match":
    assert 1 == 2
```

A `@should_panic` test passes if the test body panics (e.g., via a failed assertion or runtime error). If a string argument is provided, the panic message must contain that substring for the test to pass.

### 18.5.1 `@skip`

Tests can be marked as skipped with a reason:

```gorget
@skip("not implemented yet")
test "future feature":
    assert false
```

Skipped tests are reported but not executed:

```
  test: future feature ... SKIP (not implemented yet)
```

Skipped tests count separately from passed and failed in the summary.

### 18.5.2 `@timeout`

Set a per-test timeout in milliseconds:

```gorget
@timeout("500")
test "must complete quickly":
    int result = expensive_computation()
    assert result > 0
```

If the test exceeds the timeout, it is interrupted and reported as a failure:

```
  test: must complete quickly ... FAIL: timed out after 500ms (501ms)
```

The `--timeout` CLI flag sets a global default for all tests. Per-test `@timeout` overrides the global value.

```bash
gg test file.gg --timeout 5s       # 5 seconds
gg test file.gg --timeout 500ms    # 500 milliseconds
gg test file.gg --timeout 5000     # 5000 milliseconds (bare number)
```

### 18.5.3 Parallel Execution

Run tests across multiple worker processes:

```bash
gg test file.gg --parallel 4
```

Tests are distributed using stride-based scheduling: worker `i` runs tests `i`, `i+N`, `i+2N`, etc. Each worker produces its own output. Result files are merged after all workers complete.

### 18.5.4 Re-run Strategies

Test results are persisted to `.gorget/<name>.test-results.json`. Two re-run modes use these results:

```bash
gg test file.gg --failed-only     # Only run previously failed tests
gg test file.gg --failed-first    # Run failed tests first, then the rest
```

`--failed-only` filters the test suite to only previously-failed tests. `--failed-first` reorders the test suite so previously-failed tests execute first, then the remaining tests in their original order. Both modes read the results file from the previous run.

### 18.5.5 Snapshots

The `snapshot` statement captures a named value during a test run for later comparison:

```gorget
test "config serialization":
    auto cfg = Config("prod", 8080)
    snapshot "config" cfg.to_str()

test "fibonacci sequence":
    List[int] results = List[int]()
    for i in range(10):
        results.push(fib(i))
    snapshot "fib_10" results
```

**Rules:**

- `snapshot` is only valid inside `test` blocks.
- The expression must be a primitive type (`int`, `float`, `str`, `bool`) or a type that implements `Serializable`.
- Each snapshot is identified by its name string. Names must be unique within a test block.

**CLI commands:**

```bash
gg test file.gg --snapshot save "v1"          # run tests, save all snapshots under version "v1"
gg test file.gg --snapshot diff "v1" "v2"     # compare two saved versions
gg test file.gg --snapshot list               # list all saved versions
gg test file.gg --snapshot show "v1"          # print snapshot contents
gg test file.gg --snapshot delete "v1"        # remove a saved version
```

**Storage:** Snapshots are persisted to `.gorget/snapshots/<file_stem>/<version>.json`. Each version file contains a JSON object mapping snapshot names to their serialized values.

**Diff behavior:** `--snapshot diff` exits with code 0 if the two versions are identical, or code 1 if they differ. This makes it suitable for CI pipelines — a non-zero exit fails the build when snapshots diverge unexpectedly.

### 18.6 Console Output

Test output includes a count header and per-test timing:

```
Running 3 tests...
  test: addition works ... PASS (0ms)
  test: string equality ... PASS (1ms)
  test: edge case ... FAIL: assertion failed (0ms)

2 passed, 1 failed (1ms)
```

### 18.7 Trace Output

The `--trace` flag works with `gg test` to produce a JSONL trace file (`<name>.trace.jsonl`) containing test-specific events interleaved with function-level trace events:

```jsonl
{"type":"test_start","name":"addition works"}
{"type":"call","fn":"add","args":{"a":1,"b":2},"depth":0}
{"type":"return","fn":"add","value":3,"depth":0}
{"type":"test_end","name":"addition works","status":"pass","duration_ms":0}
{"type":"test_start","name":"string equality"}
{"type":"test_end","name":"string equality","status":"pass","duration_ms":0}
```

- `test_start` — emitted before each test begins execution.
- `test_end` — emitted after each test completes, with `"status":"pass"` or `"status":"fail"` and `"duration_ms"`.
- Function-level `call`/`return`/`loop` events appear between the start and end markers for each test.

Enable with `gg test --trace <file>` or `directive trace` in the source file. `--no-trace` overrides both.

#### HTML Report

Generate a self-contained HTML report from trace data:

```bash
gg test --report html tests/test_basic.gg   # auto-enables --trace, produces .report.html
gg report test_basic.trace.jsonl             # from an existing trace file
gg report test_basic.trace.jsonl --output custom.html  # custom output path
```

**Usage:** `gg report <trace.jsonl> [--output <path>]`

The report shows a pass/fail summary with a pass-rate bar, a test table with status badges and durations, and expandable per-test function call traces. The HTML file is self-contained (inline CSS and JS) and works offline.

### 18.8 Process Testing

```gorget
from std.process import exec_output, exec

test "echo captures stdout":
    auto result = exec_output("echo hello")
    assert result.exit_code == 0
    assert result.output == "hello\n"

test "exec returns exit code":
    auto code = exec("true")
    assert code == 0
```

`ExecResult` has fields: `output: str`, `errors: str`, `exit_code: int`.

### 18.9 Coexisting with `main()`

Test blocks and `main()` can coexist in the same file. The command determines which entry point is used:

- `gg build` / `gg run` — compiles `main()`, ignores test blocks entirely (no test code in the binary)
- `gg test` — compiles the test runner, ignores `main()`

```gorget
int double(int x):
    return x * 2

test "double works":
    assert double(3) == 6

void main():
    print("{double(21)}")
```

Semantic analysis (type checking, name resolution) runs on all code regardless of command, so a broken test will be caught during `gg build`.

### 18.10 Parameterized Tests via `meta for`

Instead of dedicated parameterized test syntax, use `meta for` to generate test blocks at compile time:

```gorget
meta for name, a, b, expected in [
    ["positives", 1, 2, 3],
    ["zeros", 0, 0, 0],
    ["negatives", -1, 1, 0],
]:
    test "addition - {name}":
        assert a + b == expected
```

This expands to three independent `test` blocks. Each has its own name, pass/fail status, and timing. No special runner infrastructure is needed.

Single-parameter:

```gorget
meta for n in [1, 2, 3, 4, 5]:
    test "square of {n}":
        assert n * n > 0
```

Composes with resource bindings:

```gorget
meta for query, expected in [["SELECT 1", 1], ["SELECT 2", 2]]:
    test "query {query}":
        with make_test_db() as db:
            assert db.query_int(query) == expected
```

### 18.11 Constraints

- At most one `suite setup` and one `suite teardown` per file.

### 18.10 Benchmarks

```gorget
bench "addition":
    int x = 1 + 2

bench "string concat":
    str s = "hello" + " world"
```

Run with `gg test --bench <file>`. Each bench block is:
1. Warmed up (3 iterations).
2. Auto-calibrated — iterations double from 100 until total >= 1 second.
3. Reported — iterations and average time per iteration (auto-scaled to ns, us, ms, or s).

```
Running 2 benchmarks...

  bench: addition ... 400000000 iters, 3 ns/iter
  bench: string concat ... 64000000 iters, 19 ns/iter

2 benchmarks complete.
```

The `--filter` flag works for benchmarks: `gg test --bench --filter "sort" file.gg`.

---

## 19. Meta & Compile-Time Evaluation

The `meta` keyword marks declarations evaluated entirely at compile time, before code generation begins. Meta constructs are removed from the output — the runtime binary sees only their substituted results.

### 19.1 Meta Constants

Syntax: `meta <type> <name> = <expr>`

All meta-compatible types: `int`, `int8`–`int64`, `uint`–`uint64`, `float`, `float32`/`float64`, `bool`, `str`.

```gorget
meta int   MAX_CONNECTIONS = 1024
meta int   BUFFER_SIZE     = MAX_CONNECTIONS * 64
meta str   VERSION         = "2.1.0"
meta float PI              = 3.14159265358979
meta bool  VERBOSE         = false
```

Constants compose freely:

```gorget
meta int PAGE_SIZE = 4096
meta int PAGES     = 16
meta int POOL_SIZE = PAGE_SIZE * PAGES   # 65536
```

After evaluation, every use of `MAX_CONNECTIONS` in the file is replaced by the literal `1024` before type-checking.

### 19.2 Meta Assertions

Syntax: `meta assert <condition> [, <message>]`

Checked at compile time; a failed assertion is a compile error.

```gorget
meta assert MAX_CONNECTIONS > 0, "MAX_CONNECTIONS must be positive"
meta assert BUFFER_SIZE <= 1048576, "buffer exceeds 1 MB"
meta assert TABLE_SIZE == 1024   # no message required
```

### 19.3 Meta Log

`meta log` emits a compile-time diagnostic message to stderr. It is the soft counterpart to `meta assert`: instead of halting compilation, it prints and continues.

**Syntax:**

```gorget
meta log expr [, expr ...]
```

Each expression is evaluated as a meta expression (same rules as `meta const` and `meta if` conditions) and printed space-separated with the prefix `[meta]`.

```gorget
meta int PAGE_SIZE = 4096
meta int PAGES     = 16

meta log "page size:", PAGE_SIZE, "pages:", PAGES
# stderr: [meta] page size: 4096 pages: 16
```

Inside generic function bodies, `meta log` is evaluated at monomorphization time:

```gorget
str describe[T]():
    meta log "describe called for type:", typename(T)
    meta if typename(T) == "int":
        return "integer"
    elif typename(T) == "str":
        return "string"
    else:
        return "other"
```

When `describe[int]()` is instantiated: `stderr: [meta] describe called for type: int`.

Multiple comma-separated arguments are space-separated in the output. `meta log` accepts any expression valid in a meta context: string literals, integer/bool/float literals, meta const names, `typename(T)`, `sizeof(T)`, arithmetic, and boolean expressions.

`meta log` has no effect on the compiled program; all `meta log` statements are erased before type-checking and code generation.

### 19.4 Meta Type Aliases

**Plain alias** — `meta type <Name> = <Type>`

```gorget
meta type Num      = int
meta type IntVec   = Vector[int]
meta type Callback = int(str, int)
```

Generic args on use sites are preserved:

```gorget
meta type Pair = Vector   # generic alias
Pair[int] xs = Pair[int]()   # → Vector[int]
```

**Conditional alias** — `meta type <Name> = <Type> if <expr> else <Type>`

```gorget
meta type Map   = Dict if feature("ordered") else HashMap
meta type Index = int32 if MAX_ENTITIES <= 2147483647 else int64
```

No `elif` on single-line conditional types; use a type function for multi-branch logic (§19.5).

### 19.4 Conditional Compilation

Syntax mirrors the `if` statement, but the condition and all branches are evaluated at compile time. The losing branch is **completely removed** — its imports, type errors, and platform-specific code are never seen by the type checker.

```gorget
meta if platform() == "linux":
    from std.net import LinuxSocket as Socket
elif platform() == "macos":
    from std.net import MacSocket as Socket
else:
    meta assert false, "unsupported platform"
```

Any top-level item is valid inside a branch: functions, structs, imports, other `meta` declarations.

Feature-gated code:

```gorget
meta if feature("metrics"):
    import std.metrics
    void record_hit(str key):
        std.metrics.increment(key)
else:
    void record_hit(str key):
        pass   # no-op in non-metrics build
```

Build flags are passed on the command line: `gg build --feature metrics`.

### 19.5 Meta Type Functions

Multi-branch type computation. Syntax is identical to a regular function but prefixed `meta type` and returns a type name.

```gorget
meta type sized_int(int bits):
    if bits <= 8:
        return int8
    elif bits <= 16:
        return int16
    elif bits <= 32:
        return int32
    else:
        return int64

meta type Word     = sized_int(arch_word_bits())
meta type SmallIdx = sized_int(8)
```

Calling a type function in a type position:

```gorget
meta type Elem = sized_int(32)   # → int32
Vector[Elem] data = Vector[Elem]()
```

Type function parameters follow normal Gorget param syntax. Only meta-compatible types (`int`, `float`, `bool`, `str`) are supported as parameter types.

### 19.6 Compile-Time Function Evaluation (M7)

Any ordinary function with meta-compatible parameter and return types can be called in a meta initializer — no special annotation required. The compiler interprets the body at compile time.

```gorget
int square(int x) = x * x

int sum_range(int n):
    int total = 0
    for i in 0..n:
        total = total + i + 1
    return total

meta int FOUR   = square(2)              # 4
meta int SUM100 = sum_range(100)         # 5050
meta int NESTED = square(sum_range(3))   # 36
```

**Supported in compile-time function bodies:**

- Variable declarations (`int x = 0`)
- Assignment (`x = x + 1`)
- Compound assignment (`x += 1`)
- `if / elif / else`
- `while` loops
- `loop` (must `break` or `return`)
- `for` over integer ranges (`0..n`, `1..=n`)
- `break`, `continue`
- `return` (mandatory — bare `return` is not allowed)
- `assert`
- Nested function calls (including recursive)
- Expression-body functions (`int double(int x) = x * 2`)

**Not supported:** `match`, `throw`, `select`, `with`, generic functions, `async` functions, `unsafe` functions.

**Limits:**

- Recursion depth: 256 calls
- Iterations per loop: 100,000 total across all loops in a single invocation

Attempting to exceed either limit is a compile error.

**Types supported as parameters/return values:** `int` (and all int variants), `float` (and variants), `bool`, `str`. Structs, enums, and collections are not supported.

### 19.7 Built-In Meta Functions

These are always available in meta contexts:

| Function | Return | Description |
|----------|--------|-------------|
| `platform()` | `str` | `"linux"`, `"macos"`, `"windows"`, `"unknown"` |
| `arch()` | `str` | `"x86_64"`, `"aarch64"`, `"arm"`, `"wasm32"`, `"unknown"` |
| `arch_word_bits()` | `int` | Native word size in bits (32 or 64) |
| `feature(str)` | `bool` | True if `--feature <name>` was passed to `gg build` |
| `debug()` | `bool` | Shorthand for `feature("debug")` |
| `sizeof(Type)` | `int` | Size in bytes (primitive types and `str`, `cstr`, `String`) |
| `alignof(Type)` | `int` | Alignment in bytes (primitive types) |
| `typename(Type)` | `str` | String representation, e.g. `"int"`, `"Vector[int]"` |
| `embed_file(str)` | `str` | Read a file at compile time, embed its contents as a string |

`sizeof` and `alignof` support primitive types and the built-in string types. User-defined struct sizes are not available during Phase 0 meta evaluation (which runs before layout computation). Inside generic function bodies, `sizeof(T)` resolves the generic parameter to its concrete type at monomorphization time (see §19.12).

```gorget
meta int  INT_SIZE  = sizeof(int)          # 8
meta int  STR_SIZE  = sizeof(str)          # 16
meta int  PTR_ALIGN = alignof(cstr)        # 8
meta str  INT_NAME  = typename(int)        # "int"
meta bool IS_64     = arch_word_bits() == 64
meta bool HAS_TLS   = feature("tls")
```

**`embed_file(path)`** reads a file at compile time and inlines its contents as a string constant.
The path is resolved relative to the source file's directory. It is a compilation error if the
file does not exist:

```gorget
meta str SQL      = embed_file("queries/get_users.sql")
meta str SHADER   = embed_file("shaders/vertex.glsl")
meta str TEMPLATE = embed_file("templates/email.html")

void main():
    print(SQL)       # prints the file contents at runtime — no I/O at runtime
```

The embedded string becomes an ordinary `meta str` constant and is substituted throughout the
module like any other meta constant. Multiline files work naturally — the string preserves all
whitespace and newlines.

### 19.8 Evaluation Order and Scoping

Meta declarations are processed **top-to-bottom before any type-checking or code generation**:

1. **Phase 1 — Evaluate:** `MetaConst`, `MetaAssert`, `MetaType`, `MetaTypeFunc` are processed in order. Forward references to functions work (all items are visible), but forward references to other meta constants do not — a constant must be declared before it is used.
2. **Phase 1.5 — Flatten MetaIf:** Conditions are evaluated, winning branches are spliced in, and any meta declarations in those branches are processed. This repeats until no more `meta if` blocks remain (handles nested conditional compilation).
3. **Phase 2 — Substitute:** Every occurrence of a meta constant name in the AST is replaced with its literal value. Meta type aliases are substituted in all type annotations.
4. **Phase 3 — Remove:** All `meta` items are stripped from the module before semantic analysis.

After this pass, the rest of the compiler sees no `meta` constructs.

### 19.9 Interaction with `--feature` Flags

Feature flags are arbitrary strings passed at build time:

```
gg build --feature tls --feature metrics myapp.gg
```

Test with `feature("tls")` or `debug()` (shorthand for `feature("debug")`). There is no fixed set of features — any string is valid. The `debug` feature has no special compiler behavior beyond being testable with `debug()`.

### 19.10 Common Patterns

**Sized integer selection:**

```gorget
meta type sized_int(int bits):
    if bits <= 8:   return int8
    elif bits <= 16: return int16
    elif bits <= 32: return int32
    else:           return int64

meta type NativeInt = sized_int(arch_word_bits())
```

**Compile-time lookup table:**

```gorget
int make_crc_entry(int n):
    int v = n
    for i in 0..8:
        if v & 1 == 1:
            v = (v >> 1) ^ 0xEDB88320
        else:
            v = v >> 1
    return v

meta int CRC0 = make_crc_entry(0)
meta int CRC1 = make_crc_entry(1)
# ... etc.
```

**Configuration validation:**

```gorget
meta int MAX_WORKERS = 8
meta int QUEUE_SIZE  = 256
meta assert MAX_WORKERS >= 1, "need at least one worker"
meta assert QUEUE_SIZE > MAX_WORKERS, "queue too small"
```

**Platform-specific defaults:**

```gorget
meta str CONFIG_DIR = "/etc/myapp" if platform() == "linux" else "/Library/Application Support/myapp"
```

---

### 19.11 Delayed Meta in Generic Bodies

`meta if` and `meta for` can appear as **statements inside generic function and method bodies**. Unlike module-level `meta if` (which is evaluated before semantic analysis), these *delayed* forms are evaluated at **monomorphization time** — when the type parameters are concrete.

#### Syntax

```gorget
# meta if inside a generic function body
T clamp[Numeric T](T val, T lo, T hi):
    meta if typename(T) == "int":
        if val < lo: return lo
        if val > hi: return hi
        return val
    elif typename(T) == "float":
        if val != val: return lo   # NaN → lo
        if val < lo: return lo
        if val > hi: return hi
        return val
    else:
        return val

# meta for inside a generic function body
void emit_info[Numeric T]():
    meta for i in 0..3:
        meta if i == 0:
            print("iteration zero")
        elif i == 1:
            print("iteration one")
        else:
            print("iteration two")
```

#### Evaluation Rules

- The condition of `meta if` and the range of `meta for` are **compile-time expressions** — they follow the same rules as module-level meta expressions (see §19.7).
- `typename(T)` inside a delayed meta condition resolves `T` to its concrete type name at the monomorphization call site.
- `sizeof(T)` inside a delayed meta condition resolves `T` to its concrete type's byte size.
- The **winning branch** is spliced inline, replacing the `meta if`/`meta for` node in the AST before GIR lowering. Losing branches are discarded entirely.
- Nested `meta if` inside `meta for` bodies is supported; the loop variable is available as a meta constant for inner conditions.

#### Restrictions

- Conditions must be evaluable at compile time: they can reference `typename(T)`, `sizeof(T)`, meta constants, literals, and arithmetic — but **not** runtime variables or function calls.
- Bodies may contain any valid runtime statements (if, while, return, etc.) since the winning body is lowered as ordinary code.

```gorget
str type_label[T](T val):
    meta if typename(T) == "int":
        return "integer"
    elif typename(T) == "float":
        return "float"
    else:
        return "other"

void main():
    print(type_label[int](42))      # integer
    print(type_label[float](3.14))  # float
    print(type_label[bool](true))   # other
```

#### 19.11.1 `meta const` Inside Generic Bodies

`meta const` binds a compile-time name to a value computed at monomorphization time. It is the inline counterpart to module-level `meta TYPE name = expr`.

**Syntax:**
```
meta const name = expr
```

No type annotation — the type is inferred from the evaluated `MetaValue`.

**Scope:** the binding is visible to all subsequent statements in the **same block**. It does not escape to enclosing or sibling blocks.

**Common use — capturing intermediate meta values:**

```gorget
# Print "VariantName=ordinal" for each variant of any enum T
void print_ordinals[T]():
    meta for vname in variant_names(T):
        meta const idx = enum_ordinal(T, vname)
        print("{vname}={idx}")

# Reverse lookup: print variant names in ordinal order
void print_names[T]():
    meta for i in 0..variant_count(T):
        meta const vname = enum_from_ordinal(T, i)
        print("{vname}")
```

Without `meta const`, the same result requires nested double-loops with `meta if` conditions to match variant names against their ordinals — substantially more verbose.

**Distinction from module-level `meta`:**

| Form | Location | Type annotation |
|------|----------|----------------|
| `meta INT x = expr` | Module level | Required (`INT`, `STR`, `BOOL`, …) |
| `meta const x = expr` | Generic function/method body | None — inferred |

Module-level `meta` is evaluated during Phase 0 (before semantic analysis). `meta const` inside a generic body is evaluated at monomorphization time, after type parameters are substituted.

---

### 19.12 Delayed-Context Builtins Reference

Inside generic function bodies, method bodies, and trait default bodies, all of the following
builtins are available. They resolve generic type parameters to their concrete values at
monomorphization time.

**Type inspection:**

| Builtin | Returns | Description |
|---------|---------|-------------|
| `typename(T)` | `str` | Canonical type name; resolves `T` at monomorphization |
| `typeof(T)` | `str` | Alias for `typename(T)` |
| `sizeof(T)` | `int` | Byte size; resolves `T` at monomorphization |
| `bitwidth(T)` | `int` | Bit width of numeric types (§19.21) |
| `min_val(T)` | `int` | Minimum value for integer types (§19.21) |
| `max_val(T)` | `int` | Maximum value for integer types (§19.21) |
| `implements(T, str)` | `bool` | True if T implements the named trait (§19.22) |

**Struct reflection:**

| Builtin | Returns | Description |
|---------|---------|-------------|
| `fields(T)` | `List[[str,str]]` | `[name, type]` pairs for all fields (§19.14) |
| `field_names(T)` | `List[str]` | Field names in declaration order (§19.19) |
| `field_count(T)` | `int` | Number of fields (§19.19) |
| `has_field(T, str)` | `bool` | True if the named field exists (§19.19) |
| `field_type(T, str)` | `str` | Canonical type name of a named field (§19.19) |

**Enum reflection:**

| Builtin | Returns | Description |
|---------|---------|-------------|
| `variant_names(T)` | `List[str]` | Variant names in declaration order (§19.20) |
| `variant_count(T)` | `int` | Number of variants (§19.20) |
| `variant_payloads(T)` | `List[[str,str]]` | `[name, inner_type]` pairs (§19.20) |
| `enum_ordinal(T, str)` | `int` | Zero-based ordinal of a named variant (§19.16) |
| `enum_from_ordinal(T, int)` | `str` | Variant name at ordinal n (§19.16) |

**Comparison: Phase 0 vs delayed:**

| Builtin | In Phase 0 (module level) | In delayed meta (generic body) |
|---------|--------------------------|-------------------------------|
| `typename(int)` | `"int"` | `"int"` |
| `typename(T)` where T is generic | `"T"` (literal param name) | `"int"` / `"float"` / etc. at each call site |
| `sizeof(int)` | `8` | `8` |
| `sizeof(T)` where T is generic | error: unknown size | concrete size at each call site |

**Example — dispatching on `sizeof`:**

```gorget
str size_class[Numeric T]():
    meta if sizeof(T) == 1:
        return "byte"
    elif sizeof(T) == 4:
        return "word"
    elif sizeof(T) == 8:
        return "dword"
    else:
        return "large"

void main():
    print(size_class[int8]())   # byte
    print(size_class[int32]())  # word
    print(size_class[int]())    # dword
    print(size_class[float]())  # dword
```

---

### 19.13 Type Predicate `T is Category`

`T is X` in a delayed meta condition evaluates to `bool` at monomorphization time. The RHS `X` is interpreted as a **type category** for the common base-type names, allowing concise type-family predicates:

| Expression | Meaning |
|------------|---------|
| `T is float` | T is any floating-point (float32 or float) |
| `T is int` | T is any integer type (int8..uint64) |
| `T is signed` | T is any signed integer (int8, int16, int32, int) |
| `T is unsigned` | T is any unsigned integer (uint8..uint64) |
| `T is numeric` | T is any integer or float |
| `T is bool` | T is exactly bool |
| `T is str` | T is exactly str |
| `T is char` | T is exactly char |
| `T is float32` | T is exactly float32 (exact match, not a category) |
| `T is int8`, `T is uint64`, etc. | T is exactly that type (exact match) |
| `T is MyStruct` | T resolves to exactly MyStruct (exact match) |

**Note:** `float` and `int` denote broad categories (any floating-point / any integer), not the specific `float` (F64) or `int` (I64) types. For an exact F64 match, use `typename(T) == "float"`.

**Negation** (`T is not X`) is supported: `meta if T is not signed:`.

**Composability** — `T is X` is a normal meta boolean and composes with `and`/`or`/`not`:

```gorget
meta if T is signed or T is float:
    print("handles signed math")
```

**Example:**

```gorget
T clamp[T](T val, T lo, T hi):
    meta if T is float:
        return fmax(fmin(val, hi), lo)
    else:
        return min(max(val, lo), hi)

str classify[T]():
    meta if T is float:
        return "float"
    elif T is signed:
        return "signed"
    elif T is unsigned:
        return "unsigned"
    else:
        return "other"

void main():
    print(classify[float32]())  # float
    print(classify[int8]())     # signed
    print(classify[uint]())     # unsigned
    print(classify[bool]())     # other
```

---

### 19.14 `fields(T)` — Combined Field Iteration

`fields(T)` returns all struct fields as `(name, type)` pairs, enabling combined iteration in a
single `meta for` loop using multi-variable destructuring:

```gorget
meta for <name>, <type> in fields(T):
    <body>
```

Each iteration binds `<name>` to the field name string and `<type>` to the canonical Gorget
type name string (e.g. `"int"`, `"float"`, `"str"`, `"MyStruct"`).

**Compared to separate loops:**

```gorget
# Before fields(T): two separate mechanisms required
void old_way[T]():
    meta for fname in field_names(T):
        auto ftype = field_type(T, fname)   # repeated per-field lookup

# With fields(T): single loop, both variables available together
void new_way[T]():
    meta for fname, ftype in fields(T):
        print("{fname}:{ftype}")
```

**Composable with `T is X`:** the type string bound by `fields(T)` works as the LHS of `is`
predicates inside the loop body:

```gorget
void count_numeric_fields[T]():
    auto count = 0
    meta for fname, ftype in fields(T):
        meta if ftype is numeric:
            count += 1
    print(count)

struct Player:
    str name
    int health
    bool alive

void main():
    count_numeric_fields[Player]()   # 1 (only health is numeric)
```

**Note on variable naming:** `type` is a Gorget keyword and cannot be used as a loop variable
name. Use `ftype`, `ty`, `field_type`, or any other non-keyword identifier.

**`fields(T)` requires T to be a struct** (not an enum or primitive). For enum variants, use
`variant_names(T)` and `variant_count(T)`.

---

### 19.15 `field_value(val, fname)` — Compile-Time Field Read

`field_value(val, fname)` reads a struct field whose name is known at compile time. It is a
**source-to-source rewrite** — the compiler transforms `field_value(val, "x")` into `val.x`
(field access) before type-checking and code generation. No new runtime overhead is introduced.

**Two usage forms:**

```gorget
# Form 1 — meta-loop variable (most common)
str to_debug[T](T val):
    str out = ""
    meta for fname, ftype in fields(T):
        meta if ftype == "int":
            int v = field_value(val, fname)   # fname is substituted to "x", "y", ... per iteration
            out = out + "{fname}={v}"
    return out

# Form 2 — direct string literal
auto p = Point(10, 20)
int xval = field_value(p, "x")   # same as p.x
int yval = field_value(p, "y")   # same as p.y
```

**How it works:**

In Form 1, `fname` is a meta-loop variable that gets substituted to a string literal (`"x"`,
`"y"`, …) for each loop iteration. The compiler then rewrites `field_value(val, "x")` to `val.x`
before type-checking.

In Form 2, the second argument is already a string literal at compile time. The rewrite pass
(which runs after name resolution) converts it to the equivalent field access.

**Cannot use in `meta const`:** `field_value` produces a *runtime* value, not a compile-time
meta constant. Using it in a `meta const` definition emits a compile error:

```gorget
meta const bad = field_value(p, "x")  # error: field_value() is a runtime expression
```

### 19.16 `field_set(obj, fname, value)` — Compile-Time Field Write

`field_set(obj, fname, value)` assigns to a struct field whose name is known at compile time.
Like `field_get`, it is a **source-to-source rewrite** — the compiler transforms
`field_set(obj, "x", 42)` into `obj.x = 42` (field assignment) before type-checking.

```gorget
# Form 1 — meta-loop variable
void zero_int_fields[T](T &val):
    meta for fname, ftype in fields(T):
        meta if ftype == "int":
            field_set(val, fname, 0)

# Form 2 — direct string literal
Point p = Point(1, 2)
field_set(p, "x", 42)   # same as p.x = 42
```

`field_set` must appear as a statement (not inside an expression). It cannot be used in
`meta const` definitions.

**Generic field_value and field_set example:**

```gorget
str to_debug[T](T val):
    str out = ""
    meta for fname, ftype in fields(T):
        if out != "":
            out = out + ","
        meta if ftype == "int":
            int v = field_value(val, fname)
            out = out + "{fname}={v}"
        elif ftype == "str":
            str v = field_value(val, fname)
            out = out + "{fname}={v}"
        elif ftype == "bool":
            bool v = field_value(val, fname)
            out = out + "{fname}={v}"
    return out

# Generic int-field sum
int sum_int_fields[T](T val):
    int total = 0
    meta for fname, ftype in fields(T):
        meta if ftype == "int":
            total += field_value(val, fname)
    return total
```

---

### 19.16 `enum_ordinal` and `enum_from_ordinal` — Enum Ordinal Reflection

Two builtins map between variant names and their zero-based ordinal positions at compile time.

**`enum_ordinal(T, name)`** — ordinal of a named variant:

```gorget
meta if enum_ordinal(T, vname) == 0:
    print("first variant: {vname}")
```

**`enum_from_ordinal(T, n)`** — variant name at ordinal `n`:

```gorget
# Validated round-trip: ordinal → name → compare
meta if enum_from_ordinal(T, i) == vname:
    print("consistent at ordinal {i}")
```

**As a `meta for` range bound** — iterate only a prefix of the variants:

```gorget
# Print all variants before "Blue"
void print_before_blue[T]():
    meta for i in 0..enum_ordinal(T, "Blue"):
        meta for vname in variant_names(T):
            meta if enum_ordinal(T, vname) == i:
                print("{vname}")
```

**Printing ordinals** — combine an integer range with `variant_names` to print each variant's ordinal:

```gorget
void print_ordinals[T]():
    meta for i in 0..variant_count(T):
        meta for vname in variant_names(T):
            meta if enum_ordinal(T, vname) == i:
                meta if enum_from_ordinal(T, i) == vname:
                    print("{vname}={i}")
```

Both builtins error at compile time if the type is not an enum, the variant name is not found, or
the ordinal is out of range.

---

### 19.17 Compile-Time Loop Unrolling (`meta for`)

`meta for` inside a generic body unrolls its loop at monomorphization time. Two range forms are supported:

**Integer range:**

```gorget
meta for <var> in <start>..<end>:
    <body>
```

- `<start>` and `<end>` must be compile-time integer expressions.
- `<var>` is available as a meta constant inside nested `meta if` conditions within `<body>`.
- `<body>` is duplicated once per iteration, with `<var>` substituted in any nested meta expression.

```gorget
void emit_labels[Numeric T]():
    meta for i in 0..3:
        meta if i == 0:
            print("first")
        elif i == 1:
            print("second")
        else:
            print("third")

void main():
    emit_labels[int]()
    # → first
    # → second
    # → third
```

**List range** — iterate over a list produced by a reflection builtin:

```gorget
meta for <var> in variant_names(T):
    <body>

meta for <name>, <type> in fields(T):
    <body>

meta for <name>, <type> in variant_payloads(T):
    <body>
```

- The loop variable(s) bind to string values from the list each iteration.
- Multi-variable destructuring (`name, type`) unpacks list-of-pairs builtins (`fields`, `variant_payloads`).
- String variables substituted in constructor or pattern positions become identifiers before lowering.

The unrolled code is lowered identically to if the programmer had written each body directly.

---

### 19.18 Default Trait Method Bodies with Meta

Default method bodies in trait definitions can use `meta if`, `meta for`, and reflection builtins
that reference `Self`. When the trait is equipped onto a concrete type, the compiler evaluates the
meta expressions at that monomorphization point — substituting `Self` with the equipped type before
lowering the method body.

#### `make_variant(T, "VariantName")` — Construct an Enum Variant

`make_variant(T, "Red")` is a compile-time rewrite that produces `T.Red()`. It is only valid
inside `meta for` loops or other delayed meta contexts where the variant name is resolved to a
string literal before the body is lowered.

```gorget
trait FromStr:
    Option[Self] from_str(str s):
        meta if Self is Enum:
            meta for vname in variant_names(Self):
                if s == vname:
                    return Some(make_variant(Self, vname))
        return None()

enum Color:
    Red()
    Green()
    Blue()

equip Color with FromStr   # no body needed — uses the trait default

void main():
    auto r = Color.from_str("Red")
    match r:
        case Some(_): print("found")
        case None: print("not found")
    # → found
```

The `meta if Self is Enum` guard (§19.13) prevents the loop from expanding for non-enum types. For
each variant name produced by `variant_names(Self)`, the compiler generates an `if s == "<name>":`
branch that returns `Some(T.VariantName())`.

#### Blank `equip` Syntax

When a trait has a complete default implementation, the equip block can be written without a body:

```gorget
equip Color with FromStr      # uses all trait defaults — no colon or indented block
equip Direction with FromStr  # same
```

This is equivalent to `equip Color with FromStr: pass`.

#### `Enum` and `Struct` Type Categories

`Self is Enum` and `Self is Struct` are supported in `meta if` conditions inside default method
bodies (and generic function bodies). They check the concrete equipped type against the GIR type
registry:

| Category | Matches |
|----------|---------|
| `Enum` | User-defined enums |
| `Struct` | User-defined structs |
| `int` | All integer primitives |
| `float` | All floating-point primitives |
| `numeric` | All numeric primitives |

---

### 19.19 Additional Struct Field Builtins

These builtins complement `fields(T)` (§19.14) for targeted struct introspection. All require `T`
to be a struct and are evaluated at monomorphization time in generic bodies.

| Builtin | Signature | Description |
|---------|-----------|-------------|
| `field_names(T)` | `→ List[str]` | All field names in declaration order |
| `field_count(T)` | `→ int` | Number of fields |
| `has_field(T, "name")` | `→ bool` | True if the named field exists |
| `field_type(T, "name")` | `→ str` | Canonical Gorget type name of the named field |

**`field_names(T)`** is the lower-level companion to `fields(T)` — it returns only the name
strings without the paired type strings. Prefer `fields(T)` when both name and type are needed;
use `field_names(T)` when only names are required:

```gorget
void print_field_names[T]():
    meta for fname in field_names(T):
        print(fname)
```

**`field_count(T)`** returns the number of fields as a compile-time integer, useful in `meta if`
conditions and as a `meta for` range bound:

```gorget
void assert_two_fields[T]():
    meta assert field_count(T) == 2, "T must have exactly 2 fields"
```

**`has_field(T, "name")`** checks for the existence of a field before accessing it — useful for
adapting generic code to structs with optional fields:

```gorget
void maybe_print_id[T](T val):
    meta if has_field(T, "id"):
        print("{field_value(val, "id")}")
    else:
        print("(no id)")
```

**`field_type(T, "name")`** returns the canonical Gorget type name of a specific field. The second
argument must be a string literal or a meta-loop variable substituted to one:

```gorget
void inspect[T]():
    meta if has_field(T, "score"):
        meta const st = field_type(T, "score")
        print("score type: {st}")
```

---

### 19.20 Variant Inspection

Three builtins introspect enum variants at monomorphization time. All require `T` to be an enum.

| Builtin | Signature | Description |
|---------|-----------|-------------|
| `variant_names(T)` | `→ List[str]` | All variant names in declaration order |
| `variant_count(T)` | `→ int` | Number of variants |
| `variant_payloads(T)` | `→ List[[str, str]]` | `[variant_name, inner_type]` pairs |

#### `variant_names(T)` and `variant_count(T)`

```gorget
void print_variants[T]():
    meta if T is Enum:
        print("count: {variant_count(T)}")
        meta for vname in variant_names(T):
            print(vname)

enum Color:
    Red()
    Green()
    Blue()

void main():
    print_variants[Color]()
    # → count: 3
    # → Red
    # → Green
    # → Blue
```

`meta for vname in variant_names(T)` iterates over a list of strings. When `vname` appears in
a constructor call or match pattern, the compiler rewrites it as an identifier — `make_variant`
(§19.18) also achieves this, but `vname` in call/pattern position does so implicitly.

`variant_count(T)` is useful as a `meta for` range bound (see §19.16) or in assertions:

```gorget
meta assert variant_count(Color) == 3, "unexpected Color variant count"
```

#### `variant_payloads(T)`

`variant_payloads(T)` is the enum counterpart to `fields(T)`. It returns one `[name, type]` pair
per variant, where the inner type is the canonical Gorget name of the single payload field:

| Variant | `variant_payloads` entry |
|---------|--------------------------|
| `IntCol(TypedColumn[int])` | `["IntCol", "int"]` |
| `FloatCol(TypedColumn[float])` | `["FloatCol", "float"]` |
| `StrCol(TypedColumn[str])` | `["StrCol", "str"]` |

For unit variants or multi-field variants, the inner type string is `""`. The primary use case is
collapsing per-variant dispatch into a single `meta for` block (see §19.24).

```gorget
void list_payloads[T]():
    meta if T is Enum:
        meta for vname, vtype in variant_payloads(T):
            print("{vname} → {vtype}")
```

---

### 19.21 Numeric Type Inspection

The following builtins are available in delayed meta contexts (generic function and method bodies).
They all resolve generic type parameters to their concrete values at monomorphization time.

| Builtin | Signature | Description |
|---------|-----------|-------------|
| `typeof(T)` | `→ str` | Canonical Gorget type name — alias for `typename(T)` |
| `bitwidth(T)` | `→ int` | Bit width of a numeric type |
| `min_val(T)` | `→ int` | Minimum value for an integer type |
| `max_val(T)` | `→ int` | Maximum representable value for an integer type |

**`typeof(T)`** is a synonym for `typename(T)` that reads as a question ("what type is T?") rather
than a conversion. Both return the same string:

```gorget
meta const name = typeof(T)   # same as typename(T)
```

**`bitwidth(T)`** returns the bit width of any numeric type:

```gorget
void show_bits[T]():
    print("{bitwidth(T)}")

void main():
    show_bits[int8]()    # → 8
    show_bits[int32]()   # → 32
    show_bits[float32]() # → 32
    show_bits[float]()   # → 64
```

Supported types and their widths: `int8`/`uint8`/`bool` → 8; `int16`/`uint16` → 16;
`int32`/`uint32`/`float32` → 32; `int`/`int64`/`uint`/`uint64`/`float`/`float64` → 64.
Passing any other type is a compile-time error.

**`min_val(T)` and `max_val(T)`** accept integer types only and return the boundary value as an
`int` meta constant:

| Type | `min_val` | `max_val` |
|------|-----------|-----------|
| `int8` | −128 | 127 |
| `int16` | −32768 | 32767 |
| `int32` | −2147483648 | 2147483647 |
| `int` / `int64` | −9223372036854775808 | 9223372036854775807 |
| `uint8` | 0 | 255 |
| `uint16` | 0 | 65535 |
| `uint32` | 0 | 4294967295 |
| `uint` / `uint64` | 0 | 9223372036854775807 |

```gorget
void show_range[Signed T]():
    meta const lo = min_val(T)
    meta const hi = max_val(T)
    print("[{lo}, {hi}]")

void main():
    show_range[int8]()   # → [-128, 127]
    show_range[int32]()  # → [-2147483648, 2147483647]
```

Passing a float type to `min_val`/`max_val` is a compile-time error.

---

### 19.22 `implements(T, "Trait")` — Trait Introspection

`implements(T, "TraitName")` evaluates to `true` at monomorphization time if `T` has been
equipped with the named trait. This allows generic code to adapt its behavior based on what
interfaces the concrete type provides.

**Signature:** `implements(T, "TraitName") → bool`

- First argument: a type (may be a generic type parameter `T`, `Self`, or a concrete name).
- Second argument: a string literal — the exact trait name as declared.

```gorget
str maybe_display[T](T val):
    meta if implements(T, "Displayable"):
        return "{val}"
    else:
        return "<no display>"

void main():
    print(maybe_display[int](42))      # → 42
    print(maybe_display[bool](true))   # → true
```

`implements` is evaluated after all equip blocks in the module are registered. A type equipped
in the same file is visible. Cross-module traits are visible after import.

```gorget
void generic_combine[T](T a, T b):
    meta if implements(T, "Addable") and implements(T, "Displayable"):
        T c = a + b
        print("{c}")
    elif implements(T, "Displayable"):
        print("{a} and {b} (no add)")
    else:
        print("(opaque type)")
```

---

### 19.23 `meta op` — Compile-Time Operator Parameters

`meta op` allows a function to accept a **binary operator token as a compile-time parameter**,
eliminating the need for separate `add`, `sub`, `mul`, `div` variants of the same algorithm.

#### Declaration

A `meta op` parameter is declared using the `meta` keyword in place of a type annotation:

```gorget
T fold[Numeric T](T a, T b, meta op):
    return a meta[op] b
```

- `meta op` replaces the type annotation — operators carry no runtime type.
- The parameter name (`op` above) is arbitrary.

#### Call Site

Pass the operator token directly — no quotes, no parentheses:

```gorget
void main():
    print(fold[int](10, 3, +))   # → 13
    print(fold[int](10, 3, *))   # → 30
    print(fold[int](10, 3, -))   # → 7
    print(fold[int](10, 3, /))   # → 3
```

Supported operators: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`, `==`, `!=`, `<`, `<=`,
`>`, `>=`.

#### Usage in the Body

Inside the function body, use the operator with the `meta[op_name]` infix syntax:

```gorget
T elemwise[Numeric T](T a, T b, meta op):
    return a meta[op] b
```

`a meta[op] b` is a placeholder that the compiler expands to `a + b`, `a * b`, etc. when the
function is monomorphized. The expansion happens before IR lowering.

#### Larger Example

```gorget
Tensor[T] tensor_elemwise[Numeric T](Tensor[T] a, Tensor[T] b, meta op):
    Tensor[T] out = Tensor[T](a.shape())
    for i in 0..a.len():
        out.set(i, a.get(i) meta[op] b.get(i))
    return out

void main():
    auto a = Tensor[int]([1, 2, 3])
    auto b = Tensor[int]([10, 20, 30])
    print(tensor_elemwise[int](a, b, +))   # [11, 22, 33]
    print(tensor_elemwise[int](a, b, *))   # [10, 40, 90]
```

One `tensor_elemwise` definition replaces separate `tensor_add`, `tensor_sub`, `tensor_mul`,
`tensor_div` functions.

#### Restrictions

- `meta op` parameters are positional; they cannot have default values.
- The operator must be a binary infix operator — unary operators are not supported.
- Multiple `meta op` parameters in the same function are allowed.
- `meta op` parameters are resolved at compile time; they cannot be stored in variables or
  passed as runtime values.

---

### 19.24 `meta for` Inside Match Arms

`meta for` can appear inside a `match` arm list. Each loop iteration produces one concrete match
arm, replacing a block of identical per-variant arms with a single template.

#### Syntax

```gorget
match expr:
    meta for var [, var] in range_expr:
        case pattern: body
```

- The `meta for` header appears at the same indentation as regular `case` arms.
- Exactly **one** `case` arm follows the header — the template arm.
- The template arm is duplicated for each iteration, with meta variables substituted throughout.

#### Example — Dispatch Wrapper

```gorget
Column col_slice(Column col, int start, int end):
    match col:
        meta for vname, T in variant_payloads(Column):
            case vname(c): return vname(col_slice_inner[T](c, start, end))
```

The compiler expands this to:

```gorget
Column col_slice(Column col, int start, int end):
    match col:
        case IntCol(c):   return IntCol(col_slice_inner[int](c, start, end))
        case FloatCol(c): return FloatCol(col_slice_inner[float](c, start, end))
        case StrCol(c):   return StrCol(col_slice_inner[str](c, start, end))
        case BoolCol(c):  return BoolCol(col_slice_inner[bool](c, start, end))
```

#### Substitution in the Template Arm

Meta variables are substituted in three positions:

1. **Pattern** — `case vname(c)` → `case IntCol(c)`, `case FloatCol(c)`, etc.
2. **Callee identifier** — `return vname(...)` rewrites the callee to the concrete variant name.
3. **Type argument** — `col_slice_inner[T]` substitutes `T` with the concrete payload type.

#### With `variant_names(T)`

When no payload type is needed, `variant_names(T)` provides just the name:

```gorget
int col_len(Column col):
    match col:
        meta for vname in variant_names(Column):
            case vname(c): return c.col_len()
```

#### Nested Match

`vname` is substituted throughout the entire arm body, including nested match patterns:

```gorget
Column col_concat(Column a, Column b):
    match a:
        meta for vname, T in variant_payloads(Column):
            case vname(ca):
                match b:
                    case vname(cb):
                        return vname(col_concat_inner[T](ca, cb))
                    else:
                        return a
```

#### Exhaustiveness

When any `meta for` items are present in a match, the exhaustiveness checker defers to IR
lowering. The expanded arms are validated after expansion completes.

---

## Appendix A: Grammar Summary

This appendix collects the grammar rules from throughout the document.

```ebnf
(* ── Module ── *)
module = { item } ;

(* ── Items ── *)
item = directive | function_def | struct_def | enum_def | trait_def
     | equip_block | import_stmt | type_alias | newtype_def
     | const_decl | static_decl | extern_block
     | test_def | suite_setup | suite_teardown
     | meta_item ;

(* ── Directives ── *)
directive = "directive" IDENTIFIER { "-" IDENTIFIER } [ "=" IDENTIFIER ] ;

(* ── Functions ── *)
function_def = { attribute } [ "public" ] { qualifier }
               return_type IDENTIFIER [ generic_params ]
               "(" [ param_list ] ")" [ throws_clause ]
               [ where_clause ] ( block | "=" expr NEWLINE | NEWLINE ) ;
qualifier     = "async" | "const" | "static" | "unsafe" ;
return_type   = type { "," type } | "void" ;  (* bare tuple: str, int or (str, int) *)
param_list    = param { "," param } ;
param         = [ "live" [ "(" IDENTIFIER ")" ] ]
                type [ "&" | "!" | "mutable" | "move" ] IDENTIFIER [ "=" expr ]
              | "meta" IDENTIFIER ;   (* meta op parameter — see §19.23 *)
throws_clause = "throws" [ type ] ;
block         = ":" NEWLINE INDENT { statement } DEDENT ;

(* ── Structs ── *)
struct_def = { attribute } [ "public" ] "struct" IDENTIFIER
             [ generic_params ] ":" NEWLINE INDENT { field_def } DEDENT ;
field_def  = [ "public" ] type IDENTIFIER NEWLINE ;

(* ── Enums ── *)
enum_def = { attribute } [ "public" ] "enum" IDENTIFIER
           [ generic_params ] ":" NEWLINE INDENT { variant } DEDENT ;
variant  = IDENTIFIER [ "(" type { "," type } ")" ] NEWLINE ;

(* ── Traits ── *)
trait_def  = { attribute } [ "public" ] "trait" IDENTIFIER
             [ generic_params ] [ "extends" trait_bound_list ]
             ":" NEWLINE INDENT { trait_item } DEDENT ;
trait_item = function_def | "type" IDENTIFIER [ ":" trait_bound_list ] NEWLINE ;

(* ── Equip blocks ── *)
equip_block = "equip" [ generic_params ] type [ "with" type ] [ "via" IDENTIFIER ]
              [ where_clause ] ":" NEWLINE INDENT { function_def | "pass" } DEDENT ;

(* ── Imports ── *)
import_stmt    = simple_import | grouped_import | from_import ;
simple_import  = "import" dotted_name NEWLINE ;
grouped_import = "import" dotted_name ".{" IDENTIFIER { "," IDENTIFIER } "}" NEWLINE ;
from_import    = "from" dotted_name "import" IDENTIFIER { "," IDENTIFIER } NEWLINE ;
dotted_name    = IDENTIFIER { "." IDENTIFIER } ;

(* ── Type aliases & Newtypes ── *)
type_alias  = "type" IDENTIFIER [ generic_params ] "=" type NEWLINE ;
newtype_def = "newtype" IDENTIFIER "(" type ")" NEWLINE ;

(* ── Constants & Statics ── *)
const_decl  = [ "public" ] "const" type IDENTIFIER "=" expr NEWLINE ;
static_decl = [ "public" ] "static" type IDENTIFIER "=" expr NEWLINE ;

(* ── Extern ── *)
extern_block  = "extern" [ STRING_LITERAL ] ":" NEWLINE INDENT { function_def } DEDENT ;

(* ── Tests ── *)
test_def       = { attribute } "test" STRING_LITERAL block ;
suite_setup    = "suite" "setup" block ;
suite_teardown = "suite" "teardown" block ;

(* ── Meta items ── *)
meta_item = "meta" ( meta_const | meta_assert | meta_type | meta_type_func | meta_if ) ;

meta_const = type IDENTIFIER "=" expr NEWLINE ;

meta_assert = "assert" expr [ "," expr ] NEWLINE ;

meta_type = "type" IDENTIFIER "=" meta_type_rhs NEWLINE ;
meta_type_rhs = type [ "if" expr "else" type ]      (* plain or conditional alias *)
              | IDENTIFIER "(" [ meta_args ] ")" ;   (* type function call *)
meta_args = expr { "," expr } ;

meta_type_func = "type" IDENTIFIER "(" [ params ] ")" ":" block ;

meta_if = "if" expr ":" meta_block
          { "elif" expr ":" meta_block }
          [ "else" ":" meta_block ] ;
meta_block = NEWLINE INDENT { item } DEDENT ;

(* ── Attributes ── *)
attribute = "@" IDENTIFIER [ "(" attr_args ")" ] NEWLINE ;
attr_args = attr_arg { "," attr_arg } ;
attr_arg  = IDENTIFIER | STRING_LITERAL | IDENTIFIER "=" STRING_LITERAL ;

(* ── Generics ── *)
generic_params   = "[" generic_param { "," generic_param } "]" ;
generic_param    = [ trait_bound_list " " ] IDENTIFIER
                 | "live" IDENTIFIER
                 | "const" type IDENTIFIER ;
where_clause     = "where" where_bound { "," where_bound } ;
where_bound      = IDENTIFIER "outlives" IDENTIFIER ;
trait_bound_list = trait_bound { "&" trait_bound } ;
trait_bound      = IDENTIFIER [ "[" type_or_binding { "," type_or_binding } "]" ] ;

(* ── Types ── *)
type = primitive_type | named_type | array_type | slice_type
     | tuple_type | function_type | "Self" | "auto" ;
primitive_type = "int" | "int8" | "int16" | "int32" | "int64"
               | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
               | "float" | "float32" | "float64"
               | "bool" | "char" | "str" | "String" | "void" ;
named_type     = IDENTIFIER [ "[" type { "," type } "]" ] ;
array_type     = type "[" const_expr "]" ;
slice_type     = type "[" "]" ;
tuple_type     = "(" type "," type { "," type } ")" ;
function_type  = type "(" [ type { "," type } ] ")" ;
(* ── Statements ── *)
statement = var_decl | expr_stmt | assign_stmt | compound_assign_stmt
          | return_stmt | throw_stmt | break_stmt | continue_stmt | pass_stmt
          | for_stmt | while_stmt | loop_stmt | if_stmt | match_stmt
          | with_stmt | unsafe_stmt | item ;

var_decl            = [ "const" | "shared" [ "(" ( "rwlock" | "atomic" ) ")" ] ] ( type | "auto" ) pattern "=" expr NEWLINE ;
expr_stmt           = expr NEWLINE ;
assign_stmt         = expr "=" expr NEWLINE ;
compound_assign_stmt = expr ( "+=" | "-=" | "*=" | "/=" | "%="
                            | "&=" | "|=" | "^=" | "<<=" | ">>=" ) expr NEWLINE ;
return_stmt         = "return" [ expr { "," expr } ] NEWLINE ;  (* bare tuple: return a, b *)
throw_stmt          = "throw" expr NEWLINE ;
break_stmt          = "break" [ expr ] NEWLINE ;
continue_stmt       = "continue" NEWLINE ;
pass_stmt           = "pass" NEWLINE ;

for_stmt   = "for" pattern { "," pattern } "in" [ "&" | "!" | "mutable" | "move" ] expr ":" block [ "else" ":" block ] ;  (* bare tuple: for x, y in ... *)
while_stmt = "while" expr ":" block [ "else" ":" block ] ;
loop_stmt  = "loop" ":" block ;
if_stmt    = "if" expr ":" block { "elif" expr ":" block } [ "else" ":" block ] ;
match_stmt = "match" expr ":" NEWLINE INDENT
             { match_item } [ "else" ":" block ] DEDENT ;
match_item = "case" pattern [ "if" expr ] ":" block
           | "meta" "for" IDENTIFIER { "," IDENTIFIER } "in" expr ":"
             NEWLINE INDENT "case" pattern ":" block DEDENT ;  (* see §19.24 *)
with_stmt  = "with" with_binding { "," with_binding } ":" block
           | "with" IDENTIFIER { "," IDENTIFIER } ":" block ;  (* shared variable access *)
with_binding = expr "as" IDENTIFIER ;
unsafe_stmt = "unsafe" ":" block ;

(* ── Expressions ── *)
expr = literal | IDENTIFIER | path_expr | unary_expr | binary_expr
     | call_expr | method_call | field_access | tuple_access | index_expr
     | range_expr | optional_chain | default_op | raw_expr
     | move_expr | mut_borrow_expr | deref_expr | as_expr | is_expr
     | if_expr | match_expr | do_expr | closure | implicit_closure
     | list_comp | dict_comp | set_comp
     | array_literal | tuple_literal | dict_literal | struct_literal
     | await_expr | spawn_expr | rethrow_expr | catch_expr
     | "self" | "it" | "(" expr ")" ;
rethrow_expr = expr "rethrow" ( expr | "(" [ type ] IDENTIFIER ")" ":" expr ) ;
catch_expr   = expr "catch" "(" IDENTIFIER ")" ":" expr ;

(* ── Patterns ── *)
pattern = "_" | literal | IDENTIFIER
        | IDENTIFIER [ "." IDENTIFIER ] "(" [ pattern { "," pattern } ] ")"
        | "(" pattern "," pattern { "," pattern } ")"
        | pattern "|" pattern { "|" pattern }
        | ".." ;
```
