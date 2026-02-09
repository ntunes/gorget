from std.fmt import Displayable
from std.collections import HashMap, Vector
import std.io

# Type alias
type StringList = Vector[String]
type IntPair = (int, int)

# Newtype
newtype UserId(int)

# Simple struct
struct Point:
    float x
    float y

# Generic struct
struct Pair[A, B]:
    A first
    B second

# Enum with variants
enum Option[T]:
    Some(T)
    None

enum Shape:
    Circle(float)
    Rectangle(float, float)
    Triangle(float, float, float)

# Trait definition
trait Drawable:
    void draw(self)
    float area(self)

# Trait with extends
trait Colorable extends Drawable:
    String color(self)

# Implement trait for struct
implement Displayable for Point:
    String to_string(self):
        return "({self.x}, {self.y})"

# Inherent implementation
implement Point:
    float distance(self):
        return 0.0

    Point origin():
        return Point(0.0, 0.0)

# Public function
public int add(int a, int b):
    return a + b

# Expression body function
int double(int x) = x * 2

# Generic function with where clause
void print_all[T](Vector[T] items) where T is Displayable:
    for item in items:
        print("{item}")

# Async function
async String fetch(String url):
    pass

# Function with throws
int parse_int(String s) throws ValueError:
    pass

# Main function exercising many constructs
void main():
    # Variable declarations
    int x = 5
    const int y = 10
    auto name = "vyper"
    float pi = 3.14

    # If/elif/else
    if x > 0:
        print("positive")
    elif x < 0:
        print("negative")
    else:
        print("zero")

    # For loop with range
    for i in 0..10:
        print("{i}")

    # While loop
    while x > 0:
        x -= 1

    # Compound assignment
    x += 1
    x *= 2

    # Method calls and field access
    auto p = Point(1.0, 2.0)
    auto d = p.distance()
    auto px = p.x

    # Function calls
    auto sum = add(3, 4)
    print("result: {sum}")

    # List comprehension
    Vector[int] squares = [x * x for x in 0..10]

    # Array literal
    auto nums = [1, 2, 3, 4, 5]

    # Optional chaining and nil coalescing
    auto val = a?.b ?? "default"

    # Match statement
    match x:
        case 1: print("one")
        case 2: print("two")
        else:
            print("other")

    # Move and borrow
    auto owned = !name
    auto borrowed = &x

@derive(Debuggable)
struct Config:
    String host
    int port
