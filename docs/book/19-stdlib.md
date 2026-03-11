# The Standard Library

Gorget ships with a comprehensive standard library split into two namespaces:
`std.*` for core utilities and `gg.*` for domain-specific libraries. This chapter
is a tour, not a reference — it shows what's available and how to get started.

---

## Core: `std.*`

### File System (`std.fs`)

```gorget
from std.fs import read_file, write_file, file_exists, mkdir

str content = read_file("config.toml")
write_file("output.txt", "hello world")

if file_exists("data.json"):
    print("found it")

mkdir("logs")
```

Key functions: `read_file`, `write_file`, `append_file`, `file_exists`,
`delete_file`, `mkdir`, `rmdir`, `rename`, `copy_file`, `file_size`, `is_dir`.

### Path Operations (`std.path`)

```gorget
from std.path import path_join, path_basename, path_extension

str full = path_join("/home/user", "docs/readme.md")
str base = path_basename(full)       # "readme.md"
str ext = path_extension(full)       # "md"
```

Also: `path_parent`, `path_stem`, `path_normalize`.

### OS Interface (`std.os`)

```gorget
from std.os import getenv, getcwd, args, platform, exit

str home = getenv("HOME")
str cwd = getcwd()
str os = platform()          # "linux", "macos", "windows"
Vector[str] argv = args()

if argv.len() < 2:
    print("usage: tool <file>")
    exit(1)
```

### Type Conversions (`std.conv`)

```gorget
from std.conv import int_to_str, parse_int, parse_float, ord, chr

str s = int_to_str(42)          # "42"
int n = parse_int("100")        # 100
char c = chr(65)                # 'A'
int code = ord('A')             # 65
```

### Math (`std.math`)

```gorget
from std.math import sqrt, pow, sin, cos, abs, min, max, floor, ceil

float root = sqrt(2.0)          # 1.414...
float area = pow(radius, 2.0) * 3.14159
int smaller = min(a, b)
float rounded = floor(3.7)      # 3.0
```

Also: `log`, `log2`, `log10`, `tan`, `asin`, `acos`, `atan`, `atan2`, `round`.

### Time (`std.time`)

```gorget
from std.time import time, time_ms, sleep_ms

int now = time()               # Unix timestamp (seconds)
int precise = time_ms()        # milliseconds
sleep_ms(100)                  # sleep 100ms
```

### Random (`std.random`)

```gorget
from std.random import rand, seed, rand_range

seed(42)
int n = rand()                 # random int
int die = rand_range(1, 7)     # 1..6 inclusive
```

### Process Execution (`std.process`)

```gorget
from std.process import exec, exec_output, ExecResult

int code = exec("ls -la")

ExecResult result = exec_output("git status")
print(result.output)
print(f"exit: {result.exit_code}")
```

For long-running processes:

```gorget
from std.process import process_spawn

auto proc = process_spawn("python3", ["-u", "server.py"]).unwrap()
proc.write_stdin("hello\n")
str output = proc.read_stdout()
proc.kill()
```

### I/O (`std.io`)

```gorget
from std.io import readline, input

str line = readline()              # read line from stdin
str name = input("Your name: ")   # prompt + read
```

### Collections (`std.collections`)

Beyond the prelude types (`Vector`, `Dict`):

```gorget
from std.collections import HashMap, Set, Box

HashMap[str, int] map = HashMap[str, int]()
Set[int] unique = Set[int]()
Box[int] heap_val = Box(42)
```

### Heap / Priority Queue (`std.heap`)

```gorget
from std.heap import Heap

Heap[int] h = Heap[int].new()
h.push(5)
h.push(1)
h.push(3)

Option[int] smallest = h.pop()    # Some(1)
```

Min-heap by default. Elements must implement `Comparable`.

---

## Domain Libraries: `gg.*`

### HTTP Client (`gg.http`)

```gorget
from gg.http import get, post, HttpResponse

HttpResponse resp = get("https://api.example.com/data")
print(f"{resp.status_code}")
print(resp.body_text)

HttpResponse resp2 = post("https://api.example.com/submit", body)
```

Pure Gorget implementation — no C dependencies. Supports TLS.

### HTTP Server (`gg.httpserver`)

```gorget
from gg.httpserver import Server, Request, Response

async void main():
    Server s = Server()
    s.get("/", (Request req): Response.ok("Hello!"))
    s.get("/users/:id", handle_user)
    s.listen(8080)
```

Features: routing, middleware, query string parsing, JSON body parsing, static
files, keep-alive.

### JSON (`gg.json`)

```gorget
from gg.json import json_parse, json_stringify, Json

Json doc = json_parse("{\"name\": \"Alice\", \"age\": 30}")
str name = doc["name"].as_str()
int age = doc["age"].as_int()

str output = json_stringify(doc)
```

### Serialization

For automatic JSON conversion, use `@derive`:

```gorget
@derive(Serializable, Deserializable)
struct User:
    str name
    int age

User u = User("Alice", 30)
str json = serialize(u)
User u2 = deserialize[User](json)
```

### CSV (`gg.csv`)

```gorget
from gg.csv import parse_table

Vector[Vector[str]] rows = parse_table(csv_text)
for row in rows:
    print(f"{row[0]}, {row[1]}")
```

### YAML / TOML / XML

```gorget
from gg.yaml import yaml_parse
from gg.toml import toml_parse
from gg.xml import xml_parse, xml_query
```

All parsers return structured data that can be queried and traversed.

### Database (`gg.db`, `gg.sqlite`)

```gorget
from gg.sqlite import sqlite_connect
from gg.db import Row, Param

auto db = sqlite_connect("app.db").unwrap()
db.exec_simple("CREATE TABLE users (name TEXT, age INTEGER)")
Vector[Row] rows = db.query_raw("SELECT * FROM users", Vector[Param]()).unwrap()
```

### Logging (`gg.log`)

```gorget
from gg.log import log_info, log_warn, log_error

log_info("server started on port 8080")
log_warn("disk usage above 80%")
log_error(f"connection failed: {reason}")
```

### UUID (`gg.uuid`)

```gorget
from gg.uuid import uuid4

str id = uuid4()    # "550e8400-e29b-41d4-a716-446655440000"
```

### SSH (`gg.ssh`)

```gorget
from gg.ssh import ssh_connect

auto client = ssh_connect("host", 22, "user", "key.pem").unwrap()
auto result = client.exec("ls -la").unwrap()
print(result)
```

---

## Summary

The standard library covers:

| Area | Modules |
|------|---------|
| Core I/O | `std.fs`, `std.io`, `std.path` |
| System | `std.os`, `std.process`, `std.time` |
| Data | `std.collections`, `std.heap`, `std.bytes` |
| Utilities | `std.conv`, `std.math`, `std.random`, `std.fmt` |
| Networking | `gg.http`, `gg.httpserver`, `gg.ssh`, `gg.p2p` |
| Data formats | `gg.json`, `gg.csv`, `gg.yaml`, `gg.toml`, `gg.xml` |
| Storage | `gg.db`, `gg.sqlite`, `gg.influx` |
| Applications | `gg.cli`, `gg.uuid`, `gg.log`, `gg.ecs`, `gg.gfx` |

Everything is pure Gorget unless noted. No C dependencies for the core modules.
