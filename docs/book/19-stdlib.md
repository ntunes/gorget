# The Standard Library

Gorget's `std.*` namespace provides the core building blocks you reach for in every
program: file I/O, networking, concurrency, string manipulation, math, and system
interfaces. These modules have no external dependencies beyond libc — they're always
available and always lightweight.

For the domain-specific "batteries included" libraries (HTTP, JSON, databases,
graphics, etc.), see [Chapter 20 — The Extended Library](20-xtd.md).

For full method signatures, see the [Language Reference](../language-reference.md) §15.

---

## Built-in Functions (no import needed)

Before diving into modules, remember that several functions are available everywhere
without any import:

| Function | Description |
|----------|-------------|
| `print(x, terminator = "\n", file = stdout)` | Print `x` (Displayable) to stdout or stderr. Pass `terminator = ""` for no newline, `terminator = "\t"` for TSV, `terminator = ", "` for CSV. |
| `len(x)` | Length of any `Measurable` |
| `range(start, end)` | Create an integer range (also `0..5` syntax) |
| `enumerate(collection)` | Iterate with `(index, element)` pairs |
| `zip(a, b)` | Combine two collections element-by-element |
| `map(collection, fn)` | Transform elements |
| `filter(collection, fn)` | Filter elements |
| `type(value)` | Runtime type name as `String` |
| `panic(msg)` | Abort with message |

These work with any type that implements `Iterable`. See
[Chapter 5](05-collections.md) for usage examples.

---

## File System (`std.fs`)

```gorget
from std.fs import read_file, write_file, file_exists, mkdir

String content = read_file("config.toml")
write_file("output.txt", "hello world")

if file_exists("data.json"):
    print("found it")

mkdir("logs")
```

Key functions: `read_file`, `read_file_bytes`, `write_file`, `write_file_bytes`,
`append_file`, `file_exists`, `delete_file`, `mkdir`, `rmdir`, `rename`,
`copy_file`, `file_size`, `is_dir`, `readdir`.

## Path Operations (`std.path`)

```gorget
from std.path import path_join, path_basename, path_extension

String full = path_join("/home/user", "docs/readme.md")
String base = path_basename(full)       # "readme.md"
String ext = path_extension(full)       # "md"
```

Also: `path_parent`, `path_stem`, `path_normalize`, `path_absolute`.

## OS Interface (`std.os`)

```gorget
from std.os import getenv, getcwd, args, platform, exit

String home = getenv("HOME")
String cwd = getcwd()
String os = platform()          # "linux", "macos", "windows"
Vector[String] argv = args()

if argv.len() < 2:
    print("usage: tool <file>")
    exit(1)
```

Also: `setenv`, `mem_allocated`, `mem_freed`, `mem_live` (live allocation tracking).

## Process Execution (`std.process`)

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
String output = proc.read_stdout()
proc.kill()
```

Also: `getpid`, `wait`, `wait_timeout`, `read_all`, `read_stderr`.

## Signal Handling (`std.signal`)

```gorget
from std.signal import signal_trap, signal_check, SIGINT, SIGTERM

signal_trap(SIGINT)
signal_trap(SIGTERM)

# ... main loop ...
if signal_check(SIGINT):
    print("interrupted!")
```

Also: `signal_wait`, `signal_ignore`, `signal_reset`, `signal_send`.

Constants: `SIGHUP`, `SIGINT`, `SIGQUIT`, `SIGABRT`, `SIGKILL`, `SIGUSR1`,
`SIGUSR2`, `SIGPIPE`, `SIGALRM`, `SIGTERM`, `SIGCHLD`.

---

## I/O and Terminal

### Standard I/O (`std.io`)

```gorget
from std.io import readline, input, stderr

String line = readline()              # read line from stdin
String name = input("Your name: ")   # prompt + read
```

Also: `stdout`, `stderr` (static `File` handles), `getchar`, `stdin_eof`,
`term_cols`, `term_rows`.

### Terminal Colors (`std.term`)

```gorget
from std.term import red, green, bold, is_tty

if is_tty():
    print(bold(green("SUCCESS")) + " all tests passed")
    print(red("ERROR") + " something went wrong")
```

Colors: `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`.
Styles: `bold`, `dim`, `underline`. Also: `strip_ansi`.

---

## Text and Data

### Type Conversions (`std.conv`)

```gorget
from std.conv import int_to_str, parse_int, parse_float, ord, chr, int_to_float

String s = int_to_str(42)                  # "42"
Result[int, String] n = parse_int("100")   # Ok(100)
String c = chr(65)                         # "A"
int code = ord("A")                        # 65
float f = int_to_float(42)                 # 42.0
```

Also: `float_to_str`, `bool_to_str`, `codepoint_to_str`.

### String Formatting (`std.fmt`)

```gorget
from std.fmt import pad_left, pad_right, center, join, str_truncate

String padded = pad_left("42", 6, "0")      # "000042"
String centered = center("title", 20, "-")   # "-------title--------"
String joined = join(["a", "b", "c"], ", ")  # "a, b, c"
String short = str_truncate("hello world", 8, "...")  # "hello..."
```

Also: `repeat`.

### Bytes and Binary (`std.bytes`)

```gorget
from std.bytes import bytes_from_str, bytes_to_hex, base64_encode, base64_decode

Vector[uint8] raw = bytes_from_str("hello")
String hex = bytes_to_hex(raw)               # "68656c6c6f"
String b64 = base64_encode(raw)              # "aGVsbG8="

auto decoded = base64_decode(b64).unwrap()   # back to raw bytes
```

Also: `bytes_to_str` (with UTF-8 validation), `bytes_from_hex`,
`bytes_concat`, `bytes_slice`, `random_bytes`, big/little-endian
read/write helpers (`bytes_read_u32_be`, `bytes_write_u16_le`, etc.).

### Text Encoding (`std.encoding`)

```gorget
from std.encoding import url_encode, url_decode, html_escape

String encoded = url_encode("hello world!")   # "hello%20world%21"
String safe = html_escape("<script>alert('xss')</script>")

from std.encoding import latin1_encode, latin1_decode
auto bytes = latin1_encode("cafe").unwrap()
```

Also: `form_encode`, `form_decode`, `html_unescape`, `utf8_len`,
`utf8_codepoints`, `utf8_is_valid`, `utf8_char_at`.

---

## Math and Random

### Math (`std.math`)

```gorget
from std.math import sqrt, sin, cos, abs, min, max, floor, ceil

float root = sqrt(2.0)          # 1.414...
float area = radius ** 2.0 * 3.14159
int smaller = min(a, b)
float rounded = floor(3.7)      # 3.0
```

Also: `log`, `log2`, `log10`, `tan`, `asin`, `acos`, `atan`, `atan2`, `round`.

Constants: `PI`, `E`, `TAU`, `INFINITY`, `NAN`.

### Random (`std.random`)

```gorget
from std.random import rand, seed, rand_range

seed(42)
int n = rand()                 # random int
int die = rand_range(1, 7)     # 1..6 inclusive
```

---

## Time

### Timestamps (`std.time`)

```gorget
from std.time import time, time_ms, sleep_ms, format_time

int now = time()               # Unix timestamp (seconds)
int precise = time_ms()        # milliseconds
sleep_ms(100)                  # sleep 100ms
String formatted = format_time(now, "%Y-%m-%d %H:%M:%S")
```

For async code, use `sleep(seconds)` which suspends without blocking the thread.

### Calendar (`std.datetime`)

```gorget
from std.datetime import DateTime

DateTime now = DateTime.now()
DateTime utc = DateTime.utc_now()
print(now.to_string())           # "2026-04-12T14:30:00+01:00"

DateTime tomorrow = now.add_days(1)
int diff = tomorrow.diff_seconds(now)   # 86400

String pretty = now.format("YYYY-MM-DD")
int day = now.weekday()          # 0=Monday, 6=Sunday
```

Also: `from_epoch`, `from_epoch_utc`, `add_seconds`, `add_hours`,
`add_minutes`, `day_of_year`, `to_epoch`.

---

## Collections

### Core Types (`std.collections`)

Beyond the prelude types (`Vector`, `Dict`):

```gorget
from std.collections import HashMap, Set, HashSet, Box

HashMap[String, int] map = HashMap[String, int]()
Set[int] unique = Set[int]()
Box[int] heap_val = Box(42)
```

### Priority Queue (`std.heap`)

```gorget
from std.heap import Heap

Heap[int] h = Heap[int].new()
h.push(5)
h.push(1)
h.push(3)

Option[int] smallest = h.pop()    # Some(1)
```

Min-heap by default. Elements must implement `Comparable`.

### Lazy Iterators (`std.iter`)

Concrete state-machine iterators over `Vector[T]`, `Set[T]`, and
`Dict[K, V]` — every collection's `.iter()` returns an `Iterator[T]`
state machine that composes through method chains.

```gorget
Vector[int] v = [10, 20, 30, 40, 50]

# Terminals: .count(), .collect(), .last(), .nth(n), .any(p), .all(p),
# .find(p), .find_index(p), .for_each(f), .fold(init, f) — all live
# as Iterator[T] default-method bodies. Any adapter that `equip`s
# the trait inherits them.
Vector[int] first3 = v.iter().take(3).collect()   # [10, 20, 30]
int evens = v.iter().filter(is_even).count()      # method-level
                                                  # generics inferred
                                                  # from arg types
bool has_big = v.iter().any(is_big)
Option[int] first_match = v.iter().find(is_match)

# Vector also carries convenience wrappers that delegate to
# self.iter().method() — same shape, less typing at call sites.
int doubled_sum = v.map(double).fold(0, sum)

# Adapter chain — take / skip / map / filter / etc. are defaults on
# Iterator[T], returning the concrete adapter struct by value. No
# boxing, no trait-object dispatch. Each step fuses at the
# monomorphised layer.
for x in v.iter().take(2):
    print(x)

# Set.iter() yields a lazy SetIter[T] walking the bucket array —
# no .items() materialisation.
Set[int] s = Set[int]()
s.add(1); s.add(2); s.add(3)
for x in s.iter().take(2):
    print(x)

# Dict.iter() yields a lazy DictIter[K, V] producing (K, V) tuples.
# Closure tuple destructuring (`((K k, V v))`) binds the components as
# named locals inside iterator-chain closures.
Dict[String, int] ages = Dict[String, int]()
ages.put("Alice", 30)
ages.put("Bob", 25)
for p in ages.iter():
    print(f"{p.0}: {p.1}")
int sum_ages = ages.iter().fold(0, (int acc, (String k, int v)): acc + v)

# Direct named HOFs on Dict still take key+value as TWO closure args
# (natural shape for one-shot Dict ops). Use the iterator chain when
# composing with .filter / .map / .take / etc.:
bool any_adult = ages.any((String k, int v): v >= 18)
int via_chain = ages.iter().filter(((String k, int v)): v >= 18).count()

# collect() infers its target from the LHS binding type.
Vector[int] dups = [1, 1, 2, 3, 3, 3]
Set[int] uniq = dups.iter().collect()                  # → Set[int]
Vector[(int, int)] pairs = [(1, 10), (2, 20)]
Dict[int, int] d = pairs.iter().collect()              # → Dict[int, int]
```

Adapters: `TakeIter[Iter, T]`, `SkipIter[Iter, T]`,
`ChainIter[IterA, IterB, T]`, `MapIter[Iter, T, U, F]`,
`FilterIter[Iter, T, F]`, plus `TakeWhileIter`, `DropWhileIter`,
`FilterMapIter`, `InspectIter`, `EnumerateIter`, `ZipIter`,
`WindowsIter`, `ChunksIter`. Each generic over the source
iterator type so chains compose at monomorphisation without
virtual dispatch.

Bound-needing terminals (`min` / `max` / `contains` / `sum` /
`product` / `join`) ship as `Iterator[T]` defaults — the compiler
demand-gates emission so each only specialises for `T`s that
satisfy the bound (Comparable / Equatable / Numeric / Displayable).
Called as `v.iter().sum()` / `.min()` / `.contains(x)` /
`.join(", ")` etc.

`Dict.keys()` / `.values()` / `.items()` still return eager
`Vector[K]` / `Vector[V]` / `Vector[(K, V)]` for callers that want
the materialised form.

### Byte-shaped I/O (`std.io`)

The narrow waist for output and input:

```gorget
from std.io import Writer, IoError, write_all, write_display

struct ByteSink:
    Vector[byte] buf

equip ByteSink with Writer:
    Result[int, IoError] write(&self, Vector[byte] buf):
        self.buf.extend(buf.clone())
        return Ok(buf.len())

ByteSink s = ByteSink(Vector[byte]())
write_all[ByteSink](&s, "hello world".bytes())
write_display[ByteSink, int](&s, 42)
```

Every Writer returns `Result[int, IoError]` — pattern-match on
`IoError.NotFound` / `PermissionDenied` / `UnexpectedEof` / `Utf8Invalid(offset)`
instead of parsing strings. Short-writes are allowed; `write_all` wraps
the short-write loop.

`write` takes raw bytes (`Vector[byte]`, not `String`) because
Writer is byte-shaped — binary protocols, TLS, compression all push
arbitrary bytes, not UTF-8. Callers with a `String` source convert via
`.bytes()` at the boundary. (`byte` is a lexer-level alias for `uint8`
— same type, just clearer in signatures.)

Reader mirror: `reader_drain[R](&r)` reads-to-EOF, `read_exact[R](&r, n)`
fills at least `n` bytes.

**Writer/Reader implementors** in the stdlib: `String` (in-memory
byte buffer), `File` (disk I/O + `stdout` / `stderr` / `stdin`
handles), `Socket` (TCP), `TlsSocket` (OpenSSL). All surface failures
as `IoError` variants; short-writes / short-reads are handled
transparently by the `write_all` / `reader_drain` / `read_exact`
loops on top.

**Whole-file convenience** — `file_open(path, mode)` returns
`Result[File, IoError]`; `read_to_string(path)`, `read_all_bytes(path)`,
`write_string(path, content)`, `write_all_bytes(path, buf)` wrap the
common patterns.

**Typed-error stdout writes** — the Writer primitives work on the
`stdout` handle just like any other File:

```gorget
from std.io import stdout, stderr, IoError, write_display, write_str, write_all

Result[int, IoError] r = write_display[File, int](&stdout, 42)
Result[int, IoError] r2 = write_str[File](&stderr, "oops\n")
```

Use these when you need to handle `BrokenPipe`, `TimedOut`, etc.
explicitly; otherwise the builtin `print` stays as the ergonomic
default (panics on I/O failure, which is fine for most programs).

---

## Concurrency

### Threads (`std.thread`)

```gorget
from std.thread import thread_spawn, current_thread_id

auto handle = thread_spawn(():
    print("hello from thread")
    42
)
int result = handle.join()     # blocks until thread finishes -> 42
```

### Channels (`std.channel`)

```gorget
from std.channel import Channel

Channel[int] ch = Channel[int](10)    # buffered, capacity 10

# producer
ch.send(42)

# consumer
int val = ch.recv()
ch.close()
```

MPSC (multi-producer, single-consumer). Also: `recv_timeout`, `is_closed`, `len`.

### Synchronization Primitives (`std.sync`)

```gorget
from std.sync import AtomicInt, Barrier, WaitGroup, Semaphore, RWLock

AtomicInt counter = AtomicInt()
counter.add(1)
int val = counter.load()

WaitGroup wg = WaitGroup()
wg.add(3)
# ... spawn 3 tasks, each calls wg.done() ...
wg.wait()
```

Also: `AtomicBool`, `CondVar`, `OnceFlag`, `ReadGuard`, `WriteGuard`.

---

## Networking

### TCP (`std.net.socket`)

```gorget
from std.net.socket import socket_connect, server_socket_bind

auto conn = socket_connect("example.com", 80).unwrap()
conn.write_str("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n")
String response = conn.read_line().unwrap()
conn.close()
```

Server side:

```gorget
auto srv = server_socket_bind("0.0.0.0", 8080).unwrap()
auto client = srv.accept().unwrap()
```

Also: `read`, `read_exact`, `set_timeout`, async variants (`nb_read`, `nb_write`,
`nb_accept`).

### TLS (`std.net.tls`)

```gorget
from std.net.tls import tls_connect

auto conn = tls_connect("example.com", 443).unwrap()
conn.write_str("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
String line = conn.read_line().unwrap()
conn.close()
```

Also: `tls_server_bind` (TLS server with cert/key), `read`, `read_exact`,
`set_timeout`.

### UDP (`std.net.udp`)

```gorget
from std.net.udp import udp_bind, UdpPacket

auto sock = udp_bind("0.0.0.0", 5000).unwrap()
sock.sendto(bytes_from_str("ping"), "127.0.0.1", 5001)

UdpPacket pkt = sock.recvfrom(1024).unwrap()
print(f"from {pkt.sender.host}:{pkt.sender.port}")
```

Also: `join_multicast`, `leave_multicast`, `poll`, `set_nonblocking`.

---

## Memory Allocators (`std.alloc`)

Custom allocators for performance-critical or embedded code:

```gorget
from std.alloc import Arena, PoolAllocator, TrackingAllocator

Arena a = Arena()
# ... allocate within the arena ...
auto cp = a.checkpoint()
# ... more allocations ...
a.restore(cp)                  # free everything since checkpoint
a.destroy()
```

Six allocator types:

| Type | Use case |
|------|----------|
| `Arena` | Bulk allocate, checkpoint/restore, reset all at once |
| `PoolAllocator` | Fixed-size blocks — game entities, packet buffers |
| `TrackingAllocator` | Count allocations, track peak usage, find leaks |
| `TlsfAllocator` | Real-time — O(1) alloc/free with low fragmentation |
| `FixedBufferAllocator` | Stack-like allocation within a fixed buffer |
| `FallbackAllocator` | Try primary allocator, fall back to secondary |

---

## Summary

| Area | Modules |
|------|---------|
| File system | `std.fs`, `std.path` |
| System | `std.os`, `std.process`, `std.signal` |
| I/O & terminal | `std.io`, `std.term` |
| Time | `std.time`, `std.datetime` |
| Data structures | `std.collections`, `std.heap` |
| Text & encoding | `std.conv`, `std.fmt`, `std.bytes`, `std.encoding` |
| Math | `std.math`, `std.random` |
| Concurrency | `std.thread`, `std.sync`, `std.channel`, `std.async` |
| Networking | `std.net.socket`, `std.net.tls`, `std.net.udp` |
| Memory | `std.alloc` |
