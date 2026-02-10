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
const  struct  enum  trait  equip  public  static  type  newtype
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
throw  throws  try
```

**Import keywords:**

```
import  from
```

**Resource/scope keywords:**

```
with  as
```

**Generic/constraint keywords:**

```
where  extends  dynamic  live  life
```

**Concurrency keywords:**

```
async  await  spawn
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
moving  mutable
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
STRING_LITERAL = '"' { string_segment } '"' ;
string_segment = literal_chars | interpolation | escape ;
interpolation  = "{" expression "}" ;
```

Interpolation: expressions inside `{}` are evaluated and converted to their string representation. Use `{{` and `}}` to produce literal braces.

**String kinds:**

| Prefix | Kind       | Interpolation | Escapes |
|--------|------------|---------------|---------|
| (none) | Normal     | Yes           | Yes     |
| `r`    | Raw        | No            | No      |
| `b`    | Byte       | No            | Yes     |

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
| `+`    | Addition        |
| `-`    | Subtraction / Negation |
| `*`    | Multiplication / Dereference |
| `/`    | Division        |
| `%`    | Modulo          |

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
| `!` or `moving`  | Move           | Ownership transfer   |

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
| `??`   | Nil coalescing    |
| `?`    | Try / early return |

**Assignment:**

| Symbol | Name               |
|--------|--------------------|
| `=`    | Assignment         |
| `+=`   | Add-assign         |
| `-=`   | Subtract-assign    |
| `*=`   | Multiply-assign    |
| `/=`   | Divide-assign      |
| `%=`   | Modulo-assign      |

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

All primitive numeric types and `bool` and `char` are **Copy** types — they are implicitly copied on assignment and do not require `!` or `moving` to transfer.

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

#### Arrays

```ebnf
array_type = type "[" const_expr "]" ;
```

A fixed-size, homogeneous sequence. Size must be a compile-time constant.

```gorget
int[5] arr = [1, 2, 3, 4, 5]
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

#### Dynamic Trait Objects

```ebnf
dynamic_type = "dynamic" IDENTIFIER ;
```

A trait object with dynamic dispatch via vtable. Used when the concrete type is not known at compile time.

```gorget
void draw(dynamic Shape shape):
    shape.render()
```

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

**Non-Copy types** (require `!` or `moving` to transfer ownership):
- `String`
- All structs
- All enums
- Collections (`Vector`, `HashMap`, etc.)

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
return_type   = type | "void" ;
param_list    = param { "," param } ;
param         = type [ "&" | "!" | "mutable" | "moving" ] IDENTIFIER [ "=" expr ] ;
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
| `Type !name` or `Type moving name`  | Move (ownership) | `f(!arg)` or `f(moving arg)` |

The ownership annotation at the call site **must match** the parameter declaration. Mismatches are compile-time errors. Both operator (`&`/`!`) and keyword (`mutable`/`moving`) forms are equivalent.

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
| `!self` or `moving self`   | Consuming (move)  |
| *(no self)*                | Static method     |

The `live` keyword on a parameter indicates that the return value borrows from that parameter's data (explicit lifetime annotation):

```gorget
str get(live Container self, int index)
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

**Construction:** Variants are constructed as bare function calls at module scope:

```gorget
Color c = Red()
Option[int] x = Some(42)
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
equip_block = "equip" [ generic_params ] type [ "with" type ]
              [ where_clause ] ":" NEWLINE INDENT { function_def } DEDENT ;
```

Equip blocks attach methods to types. There are two forms:

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

### 5.6 Imports

```ebnf
import_stmt = simple_import | grouped_import | from_import ;
simple_import  = "import" dotted_name NEWLINE ;
grouped_import = "import" dotted_name ".{" IDENTIFIER { "," IDENTIFIER } "}" NEWLINE ;
from_import    = "from" dotted_name "import" IDENTIFIER { "," IDENTIFIER } NEWLINE ;
dotted_name    = IDENTIFIER { "." IDENTIFIER } ;
```

```gorget
import std.io
import std.sync.{Arc, Mutex}
from std.fmt import Displayable, format
```

### 5.7 Type Aliases

```ebnf
type_alias = "type" IDENTIFIER [ generic_params ] "=" type NEWLINE ;
```

Creates an alternative name for an existing type. The alias is interchangeable with the original.

```gorget
type Callback = int(int, int)
type StringMap[V] = HashMap[String, V]
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
static_decl = [ "public" ] "static" type IDENTIFIER "=" expr NEWLINE ;
```

- `const`: compile-time constant. Inlined at every use site.
- `static`: runtime value with global lifetime. One instance per program.

```gorget
const int MAX_SIZE = 1024
const float PI = 3.14159265358979
static int COUNTER = 0
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
@derive(Debuggable, Cloneable, Equatable)
struct Point:
    float x
    float y

@test
void test_addition():
    assert_eq(add(2, 3), 5)

@inline
int fast_add(int a, int b) = a + b
```

---

## 6. Statements

Statements are executed for their side effects. They appear inside function bodies and blocks.

```ebnf
statement = var_decl | expr_stmt | assign_stmt | compound_assign_stmt
          | return_stmt | throw_stmt | break_stmt | continue_stmt | pass_stmt
          | for_stmt | while_stmt | loop_stmt | if_stmt | match_stmt
          | with_stmt | unsafe_stmt | item ;
```

### 6.1 Variable Declarations

```ebnf
var_decl = [ "const" ] ( type | "auto" ) pattern "=" expr NEWLINE ;
```

Declares a new variable with an explicit type or inferred type (`auto`). Variables are mutable by default; prefix with `const` for immutability.

```gorget
int x = 5
const int y = 10
auto name = "gorget"
const auto pi = 3.14
```

The pattern on the left side may be a simple binding or a destructuring pattern (see [Patterns](#8-patterns)).

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
compound_assign_stmt = expr ( "+=" | "-=" | "*=" | "/=" | "%=" ) expr NEWLINE ;
```

```gorget
x += 1
total *= factor
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
return_stmt = "return" [ expr ] NEWLINE ;
```

Exits the enclosing function, optionally with a value. Must appear inside a function.

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
             { "case" pattern [ "if" expr ] ":" block }
             [ "else" ":" block ] DEDENT ;
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
for_stmt = "for" pattern "in" [ "&" | "!" | "mutable" | "moving" ] expr ":" block
           [ "else" ":" block ] ;
```

Iterates over a collection or range. The optional ownership modifier before the iterable controls ownership:

| Form                               | Meaning                              |
|------------------------------------|--------------------------------------|
| `for x in coll`                     | Immutable borrow (collection intact) |
| `for x in &coll` or `for x in mutable coll` | Mutable borrow (modify in-place) |
| `for x in !coll` or `for x in moving coll`   | Move (consumes collection)       |

The optional `else` block runs if the loop completes without `break` (Python-style).

```gorget
for item in collection:
    if item.matches():
        break
else:
    print("no match found")
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

```ebnf
with_stmt   = "with" with_binding { "," with_binding } ":" block ;
with_binding = expr "as" IDENTIFIER ;
```

Scoped resource management. The bound resource is automatically cleaned up (Drop called) when the block exits.

```gorget
with File.open(path) as file:
    String content = file.read_all()
    print(content)
# file is closed here
```

### 6.15 Unsafe Block

```ebnf
unsafe_stmt = "unsafe" ":" block ;
```

Opts into operations the compiler cannot verify: raw pointer dereferencing, FFI calls, mutable static access.

---

## 7. Expressions

Expressions produce values. Many Gorget constructs are expressions, including `if`, `match`, `do`, and closures.

### 7.1 Operator Precedence

From lowest to highest precedence:

| Precedence | Operators / Forms            | Associativity |
|------------|------------------------------|---------------|
| 1          | `or`                         | Left          |
| 2          | `and`                        | Left          |
| 3          | `not`                        | Unary (prefix)|
| 4          | `==` `!=` `<` `>` `<=` `>=` `is` `in` | Non-associative |
| 5          | `??`                         | Left          |
| 6          | `..` `..=`                   | Non-associative |
| 7          | `+` `-`                      | Left          |
| 8          | `*` `/` `%`                  | Left          |
| 9          | Unary `-` `!` `&` `*`       | Unary (prefix)|
| 10         | `?.` `.` `()` `[]`           | Left          |
| 11         | Atoms (literals, identifiers, grouped expressions) | — |

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
unary_expr = ( "-" | "not" | "*" ) expr ;
```

| Operator | Name        | Operand type     | Result type      |
|----------|-------------|------------------|------------------|
| `-`      | Negation    | Numeric          | Same as operand  |
| `not`    | Logical NOT | `bool`           | `bool`           |
| `*`      | Dereference | Pointer/smart ptr | Inner type      |

### 7.5 Binary Operators

```ebnf
binary_expr = expr op expr ;
op = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">"
   | "<=" | ">=" | "and" | "or" | "in" ;
```

Arithmetic operators require matching numeric types. Comparison operators produce `bool`. Logical operators require `bool` operands.

The `in` operator tests membership in a range or collection:

```gorget
if value in 1..=100:
    print("in range")
```

### 7.6 Function Calls

```ebnf
call_expr = expr [ "[" type { "," type } "]" ] "(" [ arg_list ] ")" ;
arg_list  = call_arg { "," call_arg } ;
call_arg  = [ IDENTIFIER "=" ] [ "&" | "!" | "mutable" | "moving" ] expr ;
```

The optional `[...]` provides explicit generic type arguments. Arguments may use ownership annotations matching the parameter declarations. Both operator and keyword forms are accepted.

```gorget
add(1, 2)
max[int](a, b)
consume(!value)          # or: consume(moving value)
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

### 7.12 Nil Coalescing

```ebnf
nil_coalescing = expr "??" expr ;
```

Unwraps the left operand if `Some`; otherwise evaluates to the right operand.

```gorget
String name = user?.name ?? "anonymous"
```

### 7.13 Try Expression

```ebnf
try_expr = "try" expr ;
```

Captures a potentially-throwing call as a `Result[T, E]` instead of auto-propagating:

```gorget
auto result = try read_file(path)
match result:
    case Ok(content): print(content)
    case Error(e): print("Error: {e}")
```

### 7.14 Move Expression

```ebnf
move_expr = ( "!" | "moving" ) expr ;
```

Transfers ownership of a value. The source variable becomes invalid after the move. Both `!` and `moving` keyword are equivalent.

```gorget
String s2 = !s1          # s1 is invalid after this
String s3 = moving s2    # equivalent keyword form
consume(!data)            # data is moved into consume
consume(moving data)      # equivalent keyword form
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

Converts between types. Valid for numeric type conversions.

```gorget
float f = 42 as float
int n = 3.14 as int
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
closure = [ "!" | "moving" ] [ "async" ] "(" [ closure_param_list ] ")" ":" ( expr | block ) ;
closure_param_list = closure_param { "," closure_param } ;
closure_param = [ type ] [ "&" | "!" | "mutable" | "moving" ] IDENTIFIER ;
```

Anonymous functions that capture variables from their environment.

```gorget
auto doubled = numbers.map((x): x * 2)
auto sum = pairs.map((a, b): a + b)
auto typed = strings.map((String s): s.parse[int]())
```

**Move closures:** Prefix `!` or `moving` forces all captured variables to be moved into the closure:

```gorget
auto handle = thread.spawn(!(x):          # operator form
    print("value: {x}")
)
auto handle = thread.spawn(moving (x):    # keyword form
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
list_comp = "[" expr "for" pattern "in" [ "&" | "!" | "mutable" | "moving" ] expr [ "if" expr ] "]" ;
```

```gorget
Vector[int] squares = [x * x for x in 0..10]
Vector[int] evens = [x for x in 0..100 if x % 2 == 0]
```

#### Dict Comprehension

```ebnf
dict_comp = "{" expr ":" expr "for" variables "in" expr [ "if" expr ] "}" ;
```

```gorget
HashMap[String, int] lengths = {s: s.len() for s in words}
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

Array literals produce `Vector[T]` or fixed arrays depending on context. Tuple literals produce tuple types.

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
await_expr = "await" expr ;
spawn_expr = "spawn" expr ;
```

- `await` suspends until an async operation completes. Must appear inside an `async` function.
- `spawn` launches an async task concurrently.

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
tuple_pattern = "(" pattern "," pattern { "," pattern } ")" ;
```

Destructures a tuple:

```gorget
auto (x, y) = get_coordinates()
```

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
3. Ownership can be **transferred** (moved) using `!` or the `moving` keyword.
4. After a move, the source variable is invalid. Any use is a compile-time error (**use-after-move**).
5. A variable cannot be moved more than once (**double-move** error).
6. A variable cannot be moved inside a loop body (**move-in-loop** error).
7. **Copy types** (primitives, small value types) are implicitly copied on assignment; no `!` or `moving` is needed.
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
| `String !s` or `String moving s`   | `f(!s)` or `f(moving s)`         | Move |

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

---

## 10. Error Handling

Gorget uses a `throws`/`try`/`throw` model that desugars to `Result[T, E]`.

### 10.1 Throwing Functions

A function declared with `throws` may fail:

```gorget
Data process(str path) throws AppError:
    String content = read_file(path)    # auto-propagates errors
    return transform(content)
```

Inside a `throws` function, calls to other `throws` functions **auto-propagate** errors — if the callee fails, the caller immediately returns the error. No `?` operator is needed.

### 10.2 Throw

The `throw` keyword explicitly raises an error:

```gorget
throw ParseError("invalid input")
```

It is a compile-time error to use `throw` in a function not declared with `throws`.

### 10.3 Try

The `try` keyword captures a potentially-failing call as a `Result` value instead of auto-propagating:

```gorget
auto result = try read_file(path)
match result:
    case Ok(content): use(content)
    case Error(e): handle(e)
```

### 10.4 Error Types

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
               | "life" IDENTIFIER
               | "const" type IDENTIFIER ;
```

Types, functions, traits, and equip blocks may be parameterized:

- **Type parameters:** `[T]`, `[T, U]`
- **Lifetime parameters:** `[life a]` (for explicit lifetime annotation)
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

### 11.3 Where Clauses

```ebnf
where_clause = "where" where_bound { "," where_bound } ;
where_bound  = IDENTIFIER "is" trait_bound { "+" trait_bound } ;
trait_bound  = IDENTIFIER [ "[" type_or_binding { "," type_or_binding } "]" ] ;
```

Constrain generic type parameters:

```gorget
void print_all[T](Vector[T] items) where T is Displayable:
    for item in items:
        print(item.to_string())

void process[T](T item) where T is Displayable + Cloneable + Comparable:
    ...
```

### 11.4 Monomorphization

Gorget uses monomorphization: each unique combination of generic type arguments produces a specialized copy of the generic definition at compile time. This is a zero-cost abstraction — no runtime dispatch overhead.

---

## 12. Visibility

```ebnf
visibility = [ "public" ] ;
```

Two levels:

| Level     | Keyword    | Visible to                     |
|-----------|------------|--------------------------------|
| Private   | *(none)*   | Same module only               |
| Public    | `public`   | All modules                    |

Applicable to: functions, structs, struct fields, enums, traits, constants, statics.

```gorget
public struct Point:
    public float x
    public float y
    float internal_id     # private
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

Inside a normal string literal, `{expression}` evaluates the expression and inserts its string representation. The expression must be of a type that is either:

- A primitive type (`int`, `float`, `bool`, `char`)
- A `String` or `str`

Using a non-printable type (struct, enum) in interpolation is a compile-time error (**NonPrintableInterpolation**) unless the type implements `Displayable`.

```gorget
int x = 42
print("The answer is {x}")
print("Math: {2 + 2}")
print("Escaped brace: {{literal}}")
```

---

## 15. Built-in Functions

The following functions are available without import:

| Function      | Signature               | Description                     |
|---------------|-------------------------|---------------------------------|
| `print`       | `void(String)`          | Print to stdout with newline    |
| `println`     | `void(String)`          | Print to stdout with newline    |
| `len`         | `int(Collection)`       | Length of a collection          |
| `range`       | `Range(int, int)`       | Create a range                  |
| `enumerate`   | `Iterator(Collection)`  | Iterate with index              |
| `zip`         | `Iterator(A, B)`        | Combine two iterators           |
| `map`         | `Iterator(Collection, fn)` | Transform elements           |
| `filter`      | `Iterator(Collection, fn)` | Filter elements              |
| `type`        | `String(any)`           | Runtime type name               |
| `panic`       | `Never(String)`         | Abort with message              |

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
| `MoveWithoutOperator`        | Non-Copy type passed without `!` or `moving`         |
| `BorrowConflict`             | Borrow exclusivity violated (aliasing in call)       |
| `MoveInLoop`                 | Moving a variable inside a loop body                 |
| `DoubleMove`                 | Same variable moved more than once                   |
| `OwnershipMismatch`          | Call-site annotation doesn't match param declaration |
| `NonPrintableInterpolation`  | Non-primitive type in string interpolation            |

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

---

## Appendix A: Grammar Summary

This appendix collects the grammar rules from throughout the document.

```ebnf
(* ── Module ── *)
module = { item } ;

(* ── Items ── *)
item = function_def | struct_def | enum_def | trait_def
     | equip_block | import_stmt | type_alias | newtype_def
     | const_decl | static_decl | extern_block ;

(* ── Functions ── *)
function_def = { attribute } [ "public" ] { qualifier }
               return_type IDENTIFIER [ generic_params ]
               "(" [ param_list ] ")" [ throws_clause ]
               [ where_clause ] ( block | "=" expr NEWLINE | NEWLINE ) ;
qualifier     = "async" | "const" | "static" | "unsafe" ;
return_type   = type | "void" ;
param_list    = param { "," param } ;
param         = type [ "&" | "!" | "mutable" | "moving" ] IDENTIFIER [ "=" expr ] ;
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
equip_block = "equip" [ generic_params ] type [ "with" type ]
              [ where_clause ] ":" NEWLINE INDENT { function_def } DEDENT ;

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

(* ── Attributes ── *)
attribute = "@" IDENTIFIER [ "(" attr_args ")" ] NEWLINE ;
attr_args = attr_arg { "," attr_arg } ;
attr_arg  = IDENTIFIER | STRING_LITERAL | IDENTIFIER "=" STRING_LITERAL ;

(* ── Generics ── *)
generic_params = "[" generic_param { "," generic_param } "]" ;
generic_param  = IDENTIFIER | "life" IDENTIFIER | "const" type IDENTIFIER ;
where_clause   = "where" where_bound { "," where_bound } ;
where_bound    = IDENTIFIER "is" trait_bound { "+" trait_bound } ;
trait_bound_list = trait_bound { "+" trait_bound } ;
trait_bound    = IDENTIFIER [ "[" type_or_binding { "," type_or_binding } "]" ] ;

(* ── Types ── *)
type = primitive_type | named_type | array_type | slice_type
     | tuple_type | function_type | dynamic_type | "Self" | "auto" ;
primitive_type = "int" | "int8" | "int16" | "int32" | "int64"
               | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
               | "float" | "float32" | "float64"
               | "bool" | "char" | "str" | "String" | "void" ;
named_type     = IDENTIFIER [ "[" type { "," type } "]" ] ;
array_type     = type "[" const_expr "]" ;
slice_type     = type "[" "]" ;
tuple_type     = "(" type "," type { "," type } ")" ;
function_type  = type "(" [ type { "," type } ] ")" ;
dynamic_type   = "dynamic" IDENTIFIER ;

(* ── Statements ── *)
statement = var_decl | expr_stmt | assign_stmt | compound_assign_stmt
          | return_stmt | throw_stmt | break_stmt | continue_stmt | pass_stmt
          | for_stmt | while_stmt | loop_stmt | if_stmt | match_stmt
          | with_stmt | unsafe_stmt | item ;

var_decl            = [ "const" ] ( type | "auto" ) pattern "=" expr NEWLINE ;
expr_stmt           = expr NEWLINE ;
assign_stmt         = expr "=" expr NEWLINE ;
compound_assign_stmt = expr ( "+=" | "-=" | "*=" | "/=" | "%=" ) expr NEWLINE ;
return_stmt         = "return" [ expr ] NEWLINE ;
throw_stmt          = "throw" expr NEWLINE ;
break_stmt          = "break" [ expr ] NEWLINE ;
continue_stmt       = "continue" NEWLINE ;
pass_stmt           = "pass" NEWLINE ;

for_stmt   = "for" pattern "in" [ "&" | "!" | "mutable" | "moving" ] expr ":" block [ "else" ":" block ] ;
while_stmt = "while" expr ":" block [ "else" ":" block ] ;
loop_stmt  = "loop" ":" block ;
if_stmt    = "if" expr ":" block { "elif" expr ":" block } [ "else" ":" block ] ;
match_stmt = "match" expr ":" NEWLINE INDENT
             { "case" pattern [ "if" expr ] ":" block } [ "else" ":" block ] DEDENT ;
with_stmt  = "with" with_binding { "," with_binding } ":" block ;
with_binding = expr "as" IDENTIFIER ;
unsafe_stmt = "unsafe" ":" block ;

(* ── Expressions ── *)
expr = literal | IDENTIFIER | path_expr | unary_expr | binary_expr
     | call_expr | method_call | field_access | tuple_access | index_expr
     | range_expr | optional_chain | nil_coalescing | try_expr
     | move_expr | mut_borrow_expr | deref_expr | as_expr | is_expr
     | if_expr | match_expr | do_expr | closure | implicit_closure
     | list_comp | dict_comp | set_comp
     | array_literal | tuple_literal | struct_literal
     | await_expr | spawn_expr | "self" | "it" | "(" expr ")" ;

(* ── Patterns ── *)
pattern = "_" | literal | IDENTIFIER
        | IDENTIFIER [ "." IDENTIFIER ] "(" [ pattern { "," pattern } ] ")"
        | "(" pattern "," pattern { "," pattern } ")"
        | pattern "|" pattern { "|" pattern }
        | ".." ;
```
