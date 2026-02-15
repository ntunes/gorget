# std.ssh — SSH Client

An SSH2 client implemented in pure Gorget on top of `std.net.socket` and
`std.crypto`. Connect to remote servers, run commands, and collect output —
with automatic `~/.ssh/config` resolution.

## Import

```gorget
from std.ssh import ssh_connect, Session, CommandResult
```

## Quick Start

```gorget
from std.ssh import ssh_connect, Session, CommandResult

void main():
    Result[Session, str] res = ssh_connect("myserver", 22, "deploy", "s3cret")
    Session session = res.unwrap()

    CommandResult result = session.run("uname -a")
    print(result.stdout)
    print("exit code: {result.exit_code}")

    session.close()
```

## Types

### CommandResult

Holds the output of a remote command.

```gorget
struct CommandResult:
    str stdout
    str stderr
    int exit_code
```

### Session

An authenticated SSH connection. Created by `ssh_connect()`.

```gorget
struct Session:
    Socket sock
    Vector[uint8] session_id
    CipherContext enc_cipher
    CipherContext dec_cipher
    Vector[uint8] enc_mac_key
    Vector[uint8] dec_mac_key
    int send_seq
    int recv_seq
    bool encrypted
```

> **Note:** Session fields are internal. Interact with sessions through the
> methods below.

### SshConfig

Parsed connection parameters from `~/.ssh/config`.

```gorget
struct SshConfig:
    str hostname
    int port
    str user
```

## Free Functions

### ssh_connect

```gorget
Result[Session, str] ssh_connect(str host, int port, str user, str password)
```

Open an SSH connection, perform key exchange, and authenticate.

Before connecting, resolves `host` against `~/.ssh/config` to apply any
configured `HostName`, `Port`, or `User` overrides (see
[SSH Config](#ssh-config) below).

Returns `Error` if the TCP connection fails or authentication is rejected.

### parse_ssh_config

```gorget
SshConfig parse_ssh_config(str host, int default_port, str default_user)
```

Parse `~/.ssh/config` and return the resolved settings for `host`. Falls back
to the provided defaults if the config file is missing or contains no matching
`Host` block.

## Session Methods

| Signature | Description |
|---|---|
| `CommandResult run(&self, str cmd)` | Execute a command and return its output |
| `void close(&self)` | Send SSH disconnect and close the socket |

### run

```gorget
CommandResult run(&self, str cmd)
```

Execute `cmd` on the remote server. Opens a session channel, sends an exec
request, and collects stdout until the channel closes. Returns a
`CommandResult` with `stdout`, `stderr`, and `exit_code`.

```gorget
CommandResult r = session.run("ls -la /tmp")
print(r.stdout)
if r.exit_code != 0:
    print("command failed")
```

### close

```gorget
void close(&self)
```

Send an SSH disconnect message and close the underlying TCP socket. Always
call this when done with a session.

## SSH Config

`ssh_connect` automatically reads `~/.ssh/config` (if it exists) and applies
matching directives before connecting. This lets you use host aliases and
per-host settings just like OpenSSH.

### Supported Directives

| Directive | Effect |
|---|---|
| `Host` | Start a new host block. Matches by exact name or `*` wildcard. |
| `HostName` | Override the actual hostname to connect to. |
| `Port` | Override the port number. |
| `User` | Override the username. |

### Example Config

```
# ~/.ssh/config

Host prod
    HostName 10.0.1.50
    Port 2222
    User deploy

Host staging
    HostName staging.example.com
    User ci

Host *
    Port 22
```

With this config:

```gorget
# Connects to 10.0.1.50:2222 as "deploy"
ssh_connect("prod", 22, "", "password")

# Connects to staging.example.com:22 as "ci"
ssh_connect("staging", 22, "", "password")

# No matching block — connects to literal host
ssh_connect("other.example.com", 22, "root", "password")
```

### Override Precedence

Explicit non-default arguments to `ssh_connect` take priority over config
values:

- A `port` other than `22` overrides the config's `Port`.
- A non-empty `user` overrides the config's `User`.
- `HostName` from config always applies (pass the alias as `host`).

## Protocol Details

The implementation covers the minimum needed for interactive command execution:

| Layer | Algorithm |
|---|---|
| Key exchange | `diffie-hellman-group14-sha256` (RFC 3526 / RFC 4253) |
| Host key | `ssh-rsa` |
| Cipher | `aes128-ctr` |
| MAC | `hmac-sha2-256` |
| Authentication | Password (RFC 4252) |
| Channel | Session with exec request (RFC 4254) |

The client identifies itself as `SSH-2.0-Gorget_1.0`.

## Dependencies

`std.ssh` is built on three lower-level stdlib modules:

- **`std.bytes`** — Byte buffer helpers (hex encoding, big-endian I/O,
  concat/slice)
- **`std.net.socket`** — POSIX TCP sockets (connect, read, write)
- **`std.crypto`** — OpenSSL wrappers (SHA-256, HMAC, AES-CTR, BigNum, RSA)

Programs using `std.ssh` require OpenSSL (`libcrypto`) at link time. The
compiler auto-detects OpenSSL via `pkg-config` with a Homebrew fallback on
macOS.

## Limitations

- Password authentication only (no public key / IdentityFile support yet)
- No host key verification against `~/.ssh/known_hosts`
- No `ProxyJump` or `ProxyCommand` support
- Single channel per session (no multiplexing)
- Glob-style Host patterns (e.g., `Host *.example.com`) are not supported;
  only exact names and `*` match
