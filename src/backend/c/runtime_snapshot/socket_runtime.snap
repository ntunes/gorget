
// ── TCP Socket (std.net.socket) ─────────────────────────────
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/time.h>

typedef struct {
    int64_t fd;
} GorgetSocket;

// Connect to host:port, returning a socket fd or -1 on error
static const char* __gorget_socket_last_error = NULL;

static GorgetSocket gorget_socket_connect(const char* host, int64_t port) {
    __gorget_socket_last_error = NULL;
    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    int err = getaddrinfo(host, port_str, &hints, &res);
    if (err != 0) {
        __gorget_socket_last_error = gai_strerror(err);
        return (GorgetSocket){-1};
    }

    int fd = -1;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) continue;
        if (connect(fd, rp->ai_addr, rp->ai_addrlen) == 0) break;
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);

    if (fd < 0) {
        __gorget_socket_last_error = strerror(errno);
        return (GorgetSocket){-1};
    }
    return (GorgetSocket){fd};
}

static const char* gorget_socket_last_error(void) {
    return __gorget_socket_last_error;
}

// Read up to n bytes; returns a GorgetArray of uint8
static GorgetArray gorget_socket_read(GorgetSocket* sock, int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0 || sock->fd < 0) return arr;
    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)n);
    ssize_t got = recv(sock->fd, buf, (size_t)n, 0);
    if (got > 0) {
        arr.data = buf;
        arr.len = (size_t)got;
        arr.cap = (size_t)n;
    } else {
        GORGET_FREE(buf, 0);
    }
    return arr;
}

// Read exactly n bytes (loops until complete or error)
static GorgetArray gorget_socket_read_exact(GorgetSocket* sock, int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0 || sock->fd < 0) return arr;
    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)n);
    size_t total = 0;
    while (total < (size_t)n) {
        ssize_t got = recv(sock->fd, buf + total, (size_t)n - total, 0);
        if (got <= 0) break;
        total += (size_t)got;
    }
    if (total == (size_t)n) {
        arr.data = buf;
        arr.len = (size_t)n;
        arr.cap = (size_t)n;
    } else {
        GORGET_FREE(buf, 0);
    }
    return arr;
}

// Write all bytes (loops until complete)
static int64_t gorget_socket_write(GorgetSocket* sock, const GorgetArray* data) {
    if (sock->fd < 0 || data->len == 0) return 0;
    size_t total = 0;
    while (total < data->len) {
        ssize_t sent = send(sock->fd, (uint8_t*)data->data + total, data->len - total, 0);
        if (sent <= 0) return -1;
        total += (size_t)sent;
    }
    return (int64_t)total;
}

// Write a str (convenience for text protocols)
static int64_t gorget_socket_write_str(GorgetSocket* sock, const char* s) {
    if (sock->fd < 0 || !s) return 0;
    size_t len = strlen(s);
    size_t total = 0;
    while (total < len) {
        ssize_t sent = send(sock->fd, s + total, len - total, 0);
        if (sent <= 0) return -1;
        total += (size_t)sent;
    }
    return (int64_t)total;
}

// ── Byte-oriented Writer/Reader helpers (errno-returning) ───
//
// Mirror the File-side helpers: return bytes written/read (>= 0) on
// success, or a negative errno on failure. Used by the Writer/Reader
// equip blocks in std.io to surface a typed IoError. Short writes /
// reads are legitimate for sockets — callers loop via write_all /
// reader_drain / read_exact.
static int64_t gorget_socket_write_bytes_buf(GorgetSocket* sock, const GorgetArray* buf) {
    if (!sock || sock->fd < 0) return -9;   // EBADF
    if (!buf || !buf->data || buf->len == 0) return 0;
    ssize_t sent = send(sock->fd, buf->data, buf->len, 0);
    if (sent < 0) {
        int e = errno;
        if (e == 0) e = 5; // EIO
        return -(int64_t)e;
    }
    return (int64_t)sent;
}

static int64_t gorget_socket_read_bytes_buf(GorgetSocket* sock, GorgetArray* buf, int64_t max_bytes) {
    if (!sock || sock->fd < 0) return -9;   // EBADF
    if (!buf || max_bytes <= 0) return 0;
    size_t old_len = (size_t)buf->len;
    gorget_array_ensure_capacity(buf, old_len + (size_t)max_bytes, 1);
    ssize_t got = recv(sock->fd, (uint8_t*)buf->data + old_len, (size_t)max_bytes, 0);
    if (got < 0) {
        int e = errno;
        if (e == 0) e = 5; // EIO
        return -(int64_t)e;
    }
    if (got == 0) return 0;   // EOF (clean peer close)
    buf->len = (int64_t)(old_len + (size_t)got);
    return (int64_t)got;
}

// Read until \n, return as String (for text protocols like SSH banner)
static GorgetString gorget_socket_read_line(GorgetSocket* sock) {
    if (sock->fd < 0) return GORGET_EMPTY_STR;
    size_t cap = 256, len = 0;
    char* buf = (char*)GORGET_ALLOC(cap);
    while (1) {
        char c;
        ssize_t got = recv(sock->fd, &c, 1, 0);
        if (got <= 0) break;
        if (len + 1 >= cap) {
            cap *= 2;
            buf = (char*)GORGET_REALLOC(buf, 0, cap);
        }
        buf[len++] = c;
        if (c == '\n') break;
    }
    buf[len] = '\0';
    // Strip trailing \r\n
    while (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) {
        buf[--len] = '\0';
    }
    // Use the recv-accumulated byte count directly — going through strlen
    // (gorget_string_adopt) would silently truncate at the first embedded
    // NUL byte (binary protocols, malformed input). Mirrors commit 53a4db02.
    return str_adopt_buf(buf, len, cap, __gorget_current_alloc);
}

// Set socket timeout in milliseconds
static void gorget_socket_set_timeout(GorgetSocket* sock, int64_t ms) {
    if (sock->fd < 0) return;
    struct timeval tv;
    tv.tv_sec = ms / 1000;
    tv.tv_usec = (ms % 1000) * 1000;
    setsockopt(sock->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(sock->fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
}

// Close the socket
static void gorget_socket_close(GorgetSocket* sock) {
    if (sock->fd >= 0) {
        close(sock->fd);
        sock->fd = -1;
    }
}

/* ── Async (non-blocking) socket operations ─────────────────
 * These are used by coroutine poll functions.  They attempt the operation
 * once; if it would block (EAGAIN/EWOULDBLOCK), they register with the
 * reactor and return a sentinel to signal PENDING.
 *
 * Convention:
 *   gorget_socket_async_read   → returns GorgetArray with .data==NULL on PENDING
 *   gorget_socket_async_write  → returns -2 on PENDING
 *   gorget_socket_async_accept → returns GorgetSocket{-2} on PENDING
 *   gorget_socket_async_connect → returns -2 on PENDING, 0 on success, -1 on error
 */

#define GORGET_IO_PENDING (-2)

/* MSG_NOSIGNAL prevents SIGPIPE on broken connections (Linux).
 * macOS doesn't have it — uses SO_NOSIGPIPE per-socket instead. */
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

/* Non-blocking read: try recv once.
 * Returns array with data on success, empty array on EOF/error,
 * array with data==NULL && len==0 && cap==(size_t)-1 on WOULD_BLOCK. */
static GorgetArray gorget_socket_async_read(GorgetSocket* sock, int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0 || sock->fd < 0) return arr;
    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)n);
    ssize_t got = recv(sock->fd, buf, (size_t)n, 0);
    if (got > 0) {
        arr.data = buf;
        arr.len  = (size_t)got;
        arr.cap  = (size_t)n;
        return arr;
    }
    GORGET_FREE(buf, 0);
    if (got < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        /* Signal PENDING */
        arr.data = NULL;
        arr.len  = 0;
        arr.cap  = (size_t)-1; /* sentinel */
        return arr;
    }
    /* EOF or real error */
    return arr;
}

static int gorget_socket_async_read_is_pending(GorgetArray* a) {
    return a->data == NULL && a->cap == (size_t)-1;
}

/* Non-blocking write_str: try send once.
 * Returns bytes sent (>=0), -1 on error, GORGET_IO_PENDING on WOULD_BLOCK. */
static int64_t gorget_socket_async_write_str(GorgetSocket* sock, const char* s) {
    if (sock->fd < 0 || !s) return 0;
    size_t len = strlen(s);
    if (len == 0) return 0;
    ssize_t sent = send(sock->fd, s, len, MSG_NOSIGNAL);
    if (sent >= 0) return (int64_t)sent;
    if (errno == EAGAIN || errno == EWOULDBLOCK) return GORGET_IO_PENDING;
    return -1;
}

/* Non-blocking write (bytes): try send once. */
static int64_t gorget_socket_async_write(GorgetSocket* sock, GorgetArray data) {
    if (sock->fd < 0 || data.len == 0) return 0;
    ssize_t sent = send(sock->fd, (uint8_t*)data.data, data.len, MSG_NOSIGNAL);
    if (sent >= 0) return (int64_t)sent;
    if (errno == EAGAIN || errno == EWOULDBLOCK) return GORGET_IO_PENDING;
    return -1;
}

/* Non-blocking connect: initiate connection.
 * Returns 0 on immediate success, GORGET_IO_PENDING if in progress, -1 on error. */
static int gorget_socket_async_connect_start(const char* host, int64_t port, GorgetSocket* out) {
    __gorget_socket_last_error = NULL;
    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    int err = getaddrinfo(host, port_str, &hints, &res);
    if (err != 0) {
        __gorget_socket_last_error = gai_strerror(err);
        *out = (GorgetSocket){-1};
        return -1;
    }

    int fd = -1;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) continue;
        /* Set non-blocking before connect */
        int flags = fcntl(fd, F_GETFL, 0);
        fcntl(fd, F_SETFL, flags | O_NONBLOCK);
        int rc = connect(fd, rp->ai_addr, rp->ai_addrlen);
        if (rc == 0) {
            /* Immediate connect (e.g., loopback) */
            freeaddrinfo(res);
            *out = (GorgetSocket){fd};
            return 0;
        }
        if (errno == EINPROGRESS) {
            /* Connection in progress — wait for writable */
            freeaddrinfo(res);
            *out = (GorgetSocket){fd};
            return GORGET_IO_PENDING;
        }
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);
    __gorget_socket_last_error = strerror(errno);
    *out = (GorgetSocket){-1};
    return -1;
}

/* Check if an async connect completed.  Call after fd becomes writable.
 * Returns 0 on success, -1 on error. */
static int gorget_socket_async_connect_finish(GorgetSocket* sock) {
    int err = 0;
    socklen_t len = sizeof(err);
    if (getsockopt(sock->fd, SOL_SOCKET, SO_ERROR, &err, &len) != 0 || err != 0) {
        __gorget_socket_last_error = strerror(err ? err : errno);
        close(sock->fd);
        sock->fd = -1;
        return -1;
    }
    return 0;
}

// Set a socket to non-blocking mode (for async I/O)
static void gorget_socket_set_nonblocking(GorgetSocket* sock) {
    if (sock->fd >= 0) {
        int flags = fcntl(sock->fd, F_GETFL, 0);
        fcntl(sock->fd, F_SETFL, flags | O_NONBLOCK);
    }
}

// Set a socket back to blocking mode
static void gorget_socket_set_blocking(GorgetSocket* sock) {
    if (sock->fd >= 0) {
        int flags = fcntl(sock->fd, F_GETFL, 0);
        fcntl(sock->fd, F_SETFL, flags & ~O_NONBLOCK);
    }
}

// Server socket type — always declared alongside GorgetSocket so that the
// ServerSocket Gorget type (which is co-registered in std.net.socket) can
// be typedef'd to GorgetServerSocket even when SERVER_SOCKET_RUNTIME is not emitted.
typedef struct {
    int64_t fd;
} GorgetServerSocket;

/* Non-blocking accept: try accept once.
 * Returns GorgetSocket{fd} on success, {-1} on error, {GORGET_IO_PENDING} on WOULD_BLOCK. */
static GorgetSocket gorget_socket_async_accept(GorgetServerSocket* srv) {
    if (srv->fd < 0) {
        __gorget_socket_last_error = "server socket is closed";
        return (GorgetSocket){-1};
    }
    int fd = accept(srv->fd, NULL, NULL);
    if (fd >= 0) return (GorgetSocket){fd};
    if (errno == EAGAIN || errno == EWOULDBLOCK) return (GorgetSocket){GORGET_IO_PENDING};
    __gorget_socket_last_error = strerror(errno);
    return (GorgetSocket){-1};
}

