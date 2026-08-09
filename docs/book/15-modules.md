# Modules and Imports

As programs grow, you need to split code across files and organize it into
reusable pieces. Gorget's module system is file-based and straightforward.

---

## Importing Modules

### Simple Import

```gorget
import std.io
import xtd.json
```

This makes the module available by its full name: `std.io.readline()`,
`xtd.json.json_parse(s)`.

### From Import

Pull specific names into scope:

```gorget
from std.conv import int_to_str, parse_int
from xtd.log import LogLevel, Logger
```

Now you use `int_to_str()` directly instead of `std.conv.int_to_str()`.

### Glob Import

Import all variants of an enum:

```gorget
from xtd.log import LogLevel.*

LogLevel level = Info()      # bare variant access
```

Glob import is designed for enums — it brings variants into bare scope so you
don't need `LogLevel.Info()`.

### Grouped Import

```gorget
import std.{io, fs, conv}
```

---

## Visibility

By default, items are public. Use `private` to restrict visibility:

```gorget
private int helper_function(int x):
    return x * 2

public void api_function():    # public is the default
    print(f"{helper_function(5)}")
```

Private items are only accessible within the same module.

---

## File-Based Modules

Each `.gg` file is a module. The file name becomes the module name:

```
myproject/
    gorget.toml
    main.gg           # entry point
    config.gg         # module: config
    utils.gg          # module: utils
    lib/
        parser.gg     # module: lib.parser
```

Import by path:

```gorget
from config import Config, load_config
from lib.parser import parse
```

---

## The Standard Library

Gorget ships with two module namespaces:

### `std.*` — Core Standard Library

| Module | Purpose |
|--------|---------|
| `std.collections` | `Vector`, `Dict`, `HashMap`, `Set`, `Box` |
| `std.fs` | File I/O: `read_file`, `write_file`, `file_exists`, `mkdir` |
| `std.path` | Path manipulation: `path_join`, `path_basename`, `path_extension` |
| `std.os` | OS interface: `exit`, `getenv`, `getcwd`, `args`, `platform` |
| `std.io` | Stdin/stdout: `readline`, `input`, `getchar` |
| `std.conv` | Type conversions: `int_to_str`, `parse_int`, `ord`, `chr` |
| `std.math` | Math: `sqrt`, `sin`, `cos`, `abs`, `min`, `max` (use `**` for exponentiation) |
| `std.random` | Random: `rand`, `seed`, `rand_range` |
| `std.time` | Time: `time`, `time_ms`, `sleep_ms` |
| `std.process` | Process execution: `exec`, `exec_output`, `process_spawn` |
| `std.sync` | Synchronization: atomics, primitives |
| `std.heap` | Binary min-heap / priority queue |
| `std.bytes` | Byte manipulation |
| `std.encoding` | Character encoding |
| `std.datetime` | Date and time |
| `std.fmt` | String formatting |
| `std.term` | Terminal control |

### `xtd.*` — Domain Libraries

| Module | Purpose |
|--------|---------|
| `xtd.http` | HTTP/1.1 client with TLS support |
| `xtd.httpserver` | HTTP server with routing and middleware |
| `xtd.json` | JSON parsing and serialization |
| `xtd.csv` | CSV parsing |
| `xtd.yaml` | YAML parsing |
| `xtd.toml` | TOML parsing |
| `xtd.xml` | XML parsing and querying |
| `xtd.log` | Logging with levels |
| `xtd.db` | Generic database interface |
| `xtd.sqlite` | SQLite binding |
| `xtd.influx` | InfluxDB client |
| `xtd.ssh` | SSH client |
| `xtd.p2p` | Peer-to-peer networking |
| `xtd.uuid` | UUID generation |
| `xtd.cli` | Command-line interface builder |
| `xtd.dataframe` | DataFrame operations |
| `xtd.tensor` | Tensor operations |
| `xtd.ecs` | Entity component system |
| `xtd.gfx` | Graphics rendering |

### Prelude

Some types and functions are always available without imports:
- `print`, `assert`
- `Option[T]`, `Some`, `None`
- `Result[T, E]`, `Ok`, `Error`
- `Vector[T]`, `Dict[K, V]`
- `String`, all primitive types

---

## Package Management

### Creating a Project

```bash
gg new myproject
```

Creates `gorget.toml`:

```toml
[package]
name = "myproject"
version = "0.1.0"
```

### Adding Dependencies

```bash
gg add http-client --git https://github.com/user/http-client.git --tag v1.0
gg add utils --path ../shared/utils
```

Dependencies are recorded in `gorget.toml` and locked in `gorget.lock` for
reproducible builds.

The manifest is plain TOML rather than Gorget source (D44 — the manifest file).
It is data, not a program: `gg` must be able to read a package's name, version
and dependencies *without running anything*, and so must every other tool that
looks at your project. Build steps that need real logic belong in code, not in
the manifest.

### Removing Dependencies

```bash
gg remove http-client
```

---

## Summary

| Concept | Syntax | Example |
|---------|--------|---------|
| Import module | `import mod` | `import std.fs` |
| Import names | `from mod import names` | `from std.conv import parse_int` |
| Glob import | `from mod import Enum.*` | `from xtd.log import LogLevel.*` |
| Grouped import | `import ns.{a, b}` | `import std.{io, fs}` |
| Private item | `private` keyword | `private void helper():` |
| File = module | filename | `config.gg` = `config` module |
