# The Extended Library

Gorget's `xtd.*` namespace is the "batteries included" layer — domain-specific
libraries for web development, data processing, graphics, cryptography, and more.
These modules build on the core `std.*` primitives (see
[Chapter 19 — The Standard Library](19-stdlib.md)) and some use embedded C
libraries (SQLite, stb_image, SDL2, OpenSSL, PCRE2).

For full method signatures, see the [Language Reference](../language-reference.md) §15.

---

## Web

### HTTP Client (`xtd.http`)

```gorget
from xtd.http import get, post, HttpResponse

HttpResponse resp = get("https://api.example.com/data").unwrap()
print(f"{resp.status_code}")
print(resp.body_text)

HttpResponse resp2 = post("https://api.example.com/submit", body).unwrap()
```

Pure Gorget implementation — no C dependencies. Supports TLS, chunked transfer
encoding, and custom headers.

Also: `put`, `delete`, `request` (custom method/headers).

### HTTP Server (`xtd.httpserver`)

```gorget
from xtd.httpserver import HttpServer, HttpRequest, HttpServerResponse

HttpServerResponse handle(HttpRequest req):
    return HttpServerResponse.ok(f"Hello, {req.path}!")

async void main():
    auto server = HttpServer.new("0.0.0.0", 8080)
    server.serve(handle)
```

Features: routing, middleware, query string parsing, static files, keep-alive,
concurrent connections.

Response builders: `ok`, `html`, `json`, `of_json`, `not_found`, `bad_request`,
`internal_error`, `redirect`, `with_header`.

---

## Data Formats

### JSON (`xtd.json`)

```gorget
from xtd.json import json_parse, json_stringify, Json

Json doc = json_parse("{\"name\": \"Alice\", \"age\": 30}").unwrap()
String name = doc["name"].as_str()
int age = doc["age"].as_int()

String output = json_stringify(doc)
```

`Json` is a recursive enum: `Null`, `Bool`, `Int`, `Float`, `Str`, `Arr`, `Obj`.

### Serialization with `@derive`

For automatic JSON conversion:

```gorget
@derive(Serializable, Deserializable)
struct User:
    String name
    int age

User u = User("Alice", 30)
String json = serialize(u)
User u2 = deserialize[User](json)
```

### JSON Path Queries (`xtd.jsonpath`)

```gorget
from xtd.jsonpath import get, get_all, set, delete

Json doc = json_parse(data).unwrap()
Json name = get(doc, "users.0.name")
Vector[Json] ages = get_all(doc, "users.#.age")
```

Supports: dot paths, array indices, wildcards (`*`), deep scan (`..key`),
array length (`#`), filter expressions (`#(age>30)`), slices (`[0:3]`),
comparison operators (`==`, `!=`, `<`, `>`, `>=`, `<=`).

### CSV (`xtd.csv`)

```gorget
from xtd.csv import parse_table, CsvTable

CsvTable table = parse_table(csv_text).unwrap()
print(f"rows: {table.row_count()}, cols: {table.col_count()}")

for i in 0..table.row_count():
    String name = table.get_named(i, "name")
    print(name)
```

RFC 4180 compliant — handles quoted fields, embedded newlines, configurable
delimiters. Also: `parse`, `stringify`, `parse_table_delim`, `stringify_table`.

### YAML (`xtd.yaml`)

```gorget
from xtd.yaml import yaml_parse, yaml_stringify, Yaml

Yaml doc = yaml_parse(text).unwrap()
String out = yaml_stringify(doc)
```

YAML 1.2 parser. `Yaml` enum: `Null`, `Bool`, `Int`, `Float`, `Str`, `Seq`, `Map`.
Supports block/flow collections, quoted/plain scalars, comments, document markers.

### TOML (`xtd.toml`)

```gorget
from xtd.toml import toml_parse, toml_stringify, TomlValue

TomlValue config = toml_parse(text).unwrap()
String out = toml_stringify(config)
```

TOML v1.0. `TomlValue` enum: `Str`, `Int`, `Float`, `Bool`, `DateTime`, `Arr`, `Tbl`.
Supports tables, array-of-tables, inline tables, multiline strings.

### XML (`xtd.xml`)

```gorget
from xtd.xml import xml_parse, xml_stringify, XmlNode

XmlNode doc = xml_parse(text).unwrap()
String out = xml_stringify(doc)
```

`XmlNode` enum: `Element(tag, attributes, children)` or `Text(content)`.
Supports elements, attributes, text, entity references, self-closing tags, comments.

---

## Text Processing

### Regular Expressions (`xtd.regex`)

```gorget
from xtd.regex import regex_compile, regex_is_match, Regex, Match

bool ok = regex_is_match("\\d+", "abc123")   # true

Regex re = regex_compile("[a-z]+(\\d+)").unwrap()
Option[Match] m = re.find("item42")
if m is Some(match):
    print(match.text())          # "item42"
    print(match.group(1) ?? "")  # "42"

Vector[String] parts = re.split("item1-item2-item3")
String replaced = re.replace_all("cat123dog456", "***")
```

PCRE2-backed. Also: `find_all`, `fullmatch`, `splitn`, `replace`,
`capture_count`, `group_names`, `group_by_name`, `regex_escape`,
`regex_compile_with` (flags).

---

## Databases

### Database Traits (`xtd.db`)

Shared foundation for all database backends:

```gorget
from xtd.db import Row, Param, IntParam, StrParam, FromRow, Queryable

# Row provides: get(col), get_int(col), get_float(col), get_bool(col), has(col)
# Param enum: IntParam, FloatParam, StrParam, BoolParam, NullParam
```

Traits: `FromRow` (derive with `@derive(FromRow)`), `Queryable`, `DbConnection`.

### SQLite (`xtd.sqlite`)

```gorget
from xtd.sqlite import SqliteConn

auto db = SqliteConn.open("app.db").unwrap()
db.exec_simple("CREATE TABLE users (name TEXT, age INTEGER)")

db.exec("INSERT INTO users VALUES (?, ?)", [StrParam("Alice"), IntParam(30)])

Vector[Row] rows = db.query_raw("SELECT * FROM users", []).unwrap()
for row in rows:
    print(f"{row.get(\"name\")}: {row.get_int(\"age\")}")

db.close()
```

SQLite3 amalgamation compiled inline — no external dependencies.

### InfluxDB (`xtd.influx`)

```gorget
from xtd.influx import influx_connect

auto client = influx_connect(base_url, token, org, bucket).unwrap()
client.exec_simple("cpu,host=a value=0.64")
Vector[Row] rows = client.query_raw("from(bucket:\"mydb\")", []).unwrap()
```

Pure Gorget over HTTP. Implements the same `DbConnection` trait as SQLite.

---

## Cryptography and Compression

### Cryptography (`xtd.crypto`)

```gorget
from xtd.crypto import crypto_sha256, crypto_aes_gcm_encrypt, crypto_random_bytes
from std.bytes import bytes_from_str, bytes_to_hex

Vector[uint8] hash = crypto_sha256(bytes_from_str("hello"))
print(bytes_to_hex(hash))

auto key = crypto_random_bytes(32).unwrap()
auto nonce = crypto_random_bytes(12).unwrap()
auto encrypted = crypto_aes_gcm_encrypt(key, nonce, plaintext).unwrap()
```

| Category | Functions |
|----------|-----------|
| Hashes | SHA-256, SHA-1, HMAC |
| Symmetric | AES-GCM (encrypt/decrypt), AES-CTR |
| Asymmetric | RSA (verify), Ed25519 (keygen/sign/verify), X25519 (ECDH) |
| Key derivation | HKDF-SHA256 |
| Random | `crypto_random_bytes` |

### Compression (`xtd.compress`)

```gorget
from xtd.compress import zlib_compress, zlib_decompress, crc32_compute

auto compressed = zlib_compress(data).unwrap()
auto original = zlib_decompress(compressed, original_size).unwrap()
int checksum = crc32_compute(data)
```

Also: `zlib_compress_level`, `deflate_compress`, `deflate_decompress`.

---

## Networking

### SSH Client (`xtd.ssh`)

```gorget
from xtd.ssh import connect, CommandResult

auto session = connect("host.example.com", 22, "user", "password").unwrap()
CommandResult result = channel_exec(session, "ls -la").unwrap()
print(result.stdout)
print(f"exit: {result.exit_code}")
```

Pure Gorget SSH2 implementation (no libssh dependency). RFC 4253 transport,
RFC 4252 authentication, RFC 4254 channels. Supports password auth, command
execution, `~/.ssh/config` parsing.

### Peer-to-Peer (`xtd.p2p`)

```gorget
from xtd.p2p import p2p_generate_identity, p2p_encode_message, p2p_decode_message

auto keys = p2p_generate_identity()
auto id = p2p_peer_id_hex(keys.peer_id)
print(f"My peer ID: {id}")
```

Ed25519 identities, X25519 encrypted channels, UDP wire protocol with
PING/PONG, DATA, ANNOUNCE, DHT operations (FIND_NODE, STORE, FIND_VALUE),
gossip, NAT hole-punching, and relay support.

---

## Numerical Computing

### Tensors (`xtd.tensor`)

```gorget
from xtd.tensor import tensor_zeros, tensor_from, tensor_add, tensor_matmul
from xtd.tensor import reshape, transpose, tensor_display

auto a = tensor_from[int]([2, 3], [1, 2, 3, 4, 5, 6])
auto b = tensor_zeros[int]([2, 3])
auto c = tensor_add(a, b)

auto matrix = reshape(a, [3, 2])
auto t = transpose(matrix)
auto product = tensor_matmul(matrix, t)
print(tensor_display(product))
```

N-dimensional, view-based (zero-copy reshape/transpose/slice). Generic over
`int` and `float`.

| Category | Functions |
|----------|-----------|
| Creation | `tensor_zeros`, `tensor_ones`, `tensor_from`, `tensor_clone`, `tensor_arange`, `tensor_linspace` |
| Views | `reshape`, `transpose`, `permute`, `tensor_slice` |
| Arithmetic | `tensor_add`, `tensor_sub`, `tensor_mul`, `tensor_div`, `tensor_neg` |
| Scalar ops | `scalar_add`, `scalar_sub`, `scalar_mul`, `scalar_div` |
| Broadcasting | `tensor_badd`, `tensor_bsub`, `tensor_bmul` |
| Reductions | `tensor_sum`, `tensor_min`, `tensor_max`, `tensor_mean_all`, `tensor_std_all`, axis variants |
| Linear algebra | `tensor_matmul`, `tensor_dot` |
| Comparison | `tensor_eq`, `tensor_lt`, `tensor_gt`, `tensor_le`, `tensor_ge`, `tensor_ne` |

### DataFrames (`xtd.dataframe`)

```gorget
from xtd.dataframe import DataFrame, Column, df_from_csv

auto df = df_from_csv(csv_text).unwrap()
print(f"rows: {df.num_rows}, cols: {df.num_cols}")

auto filtered = df.filter_gt("age", 25)
auto sorted = df.sort_by("name", true)
auto grouped = df.group_by("department").agg("salary", "mean")
```

Pandas-inspired tabular data with typed columns (`int`, `float`, `String`,
`bool`), nullable values, CSV import/export, filtering, sorting, groupby
aggregation, joins (inner/left/right/outer/cross), column arithmetic, sampling,
median, variance.

### 3D Math (`xtd.math3d`)

```gorget
from xtd.math3d import Vec3, Mat4, radians

Vec3 pos = Vec3(1.0, 2.0, 3.0)
Vec3 dir = Vec3.forward()
float d = pos.dot(dir)
Vec3 up = pos.cross(dir).normalize()
float dist = pos.distance(Vec3.zero())
```

Pure Gorget — no FFI. Types: `Vec2`, `Vec3`, `Vec4`, `Mat4`. Operations:
add, sub, scale, dot, cross, normalize, lerp, distance. Also: `radians`,
`degrees`, `clamp_f`.

---

## Graphics and Multimedia

### 2D Graphics (`xtd.gfx`)

```gorget
from xtd.gfx import open, clear, fill_rect, present, close, Color, Canvas

Canvas c = open("My Game", 800, 600).unwrap()
clear(c, Color(0, 0, 0, 255))
fill_rect(c, 100, 100, 50, 50, Color(255, 0, 0, 255))
present(c)
close(c)
```

High-level 2D wrapper over SDL2. Also: `draw_rect`, `draw_line`, `draw_point`,
`draw_circle`.

### Low-Level Graphics

For full control over the rendering pipeline:

- **`xtd.sdl`** — SDL2 bindings: window management, input events, textures, fonts, timing
- **`xtd.gl`** — OpenGL 2.1–4.6: state management, shaders, textures, VBOs, blend modes
- **`xtd.metal`** — Apple Metal: render pipelines, buffers, textures, compute
- **`xtd.gpu`** — Platform-adaptive abstraction: Metal on macOS, OpenGL on Linux

### Image Processing (`xtd.image`)

```gorget
from xtd.image import image_load, image_write_png, image_resize

auto img = image_load("photo.jpg").unwrap()
print(f"{img.width}x{img.height}, {img.channels} channels")

auto resized = image_resize(img, 320, 240).unwrap()
image_write_png("thumb.png", resized)
```

stb_image-backed. Supports PNG, JPG, BMP, TGA, GIF. Also: `image_load_rgba`,
`image_load_from_memory`, `image_flip_vertically`, `image_write_jpg`,
`image_encode_png`.

### Audio (`xtd.audio`)

```gorget
from xtd.audio import audio_init, audio_load_wav, audio_play_channel

audio_init(44100, 2, 1024)
auto sound = audio_load_wav("click.wav").unwrap()
audio_play_channel(0, sound, 0)
```

SDL2_mixer-backed. Supports WAV loading (file and memory), music playback,
channel mixing, volume control, fade in/out, 3D positioning/panning.

---

## Game Development

### Entity Component System (`xtd.ecs`)

```gorget
from xtd.ecs import EntityPool, SparseSet, Entity

EntityPool pool = EntityPool.new()
SparseSet[Vec3] positions = SparseSet[Vec3].new()
SparseSet[float] healths = SparseSet[float].new()

Entity player = pool.create()
positions.insert(player, Vec3(0.0, 0.0, 0.0))
healths.insert(player, 100.0)

Vector[Entity] alive = query2(positions, healths)
```

Generational entity handles (ABA-safe), O(1) sparse set storage with `insert`,
`remove`, `has`, `get`, `set`, `try_get`, `each`, and `SparseSetIter`. Two-component
queries with `query2`.

---

## CLI and Utilities

### Argument Parsing (`xtd.cli`)

```gorget
from xtd.cli import CliParser

auto cli = CliParser.new("mytool", "A command-line tool")
cli.add_flag("verbose", "v", "Enable verbose output")
cli.add_option("output", "o", "Output file", "out.txt")
cli.add_positional("input", "Input file")

cli.parse(args()).unwrap()

if cli.has("verbose"):
    print("verbose mode")
String out = cli.get("output")
```

### Logging (`xtd.log`)

```gorget
from xtd.log import Logger, LogLevel, log_info, log_warn, log_error

log_info("server started on port 8080")
log_warn("disk usage above 80%")
log_error(f"connection failed: {reason}")

Logger logger = Logger.with_timestamps(LogLevel.Debug)
logger.debug("detailed trace info")
```

Also: `Logger.with_prefix` for namespaced logging, `set_level` for runtime control.

### UUID (`xtd.uuid`)

```gorget
from xtd.uuid import UUID

UUID id = UUID.v4()
print(id.to_string())     # "550e8400-e29b-41d4-a716-446655440000"

auto parsed = UUID.parse("550e8400-e29b-41d4-a716-446655440000").unwrap()
bool same = id.equals(parsed)
```

---

## Summary

| Area | Modules |
|------|---------|
| Web | `xtd.http`, `xtd.httpserver` |
| Data formats | `xtd.json`, `xtd.jsonpath`, `xtd.csv`, `xtd.yaml`, `xtd.toml`, `xtd.xml` |
| Text processing | `xtd.regex` |
| Databases | `xtd.db`, `xtd.sqlite`, `xtd.influx` |
| Cryptography | `xtd.crypto`, `xtd.compress` |
| Networking | `xtd.ssh`, `xtd.p2p` |
| Numerical | `xtd.tensor`, `xtd.dataframe`, `xtd.math3d` |
| Graphics | `xtd.gfx`, `xtd.sdl`, `xtd.gl`, `xtd.metal`, `xtd.gpu` |
| Multimedia | `xtd.audio`, `xtd.image` |
| Game dev | `xtd.ecs` |
| CLI & utilities | `xtd.cli`, `xtd.log`, `xtd.uuid` |
