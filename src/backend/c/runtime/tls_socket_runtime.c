
// ── TLS Socket (std.net.tls) ────────────────────────────────
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>

typedef struct {
    int64_t fd;
    SSL_CTX* ctx;
    SSL* ssl;
} GorgetTlsSocket;

static const char* __gorget_tls_last_error = NULL;

static GorgetTlsSocket gorget_tls_connect(const char* host, int64_t port) {
    __gorget_tls_last_error = NULL;
    GorgetTlsSocket sock = {-1, NULL, NULL};

    // DNS resolution
    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);
    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    int err = getaddrinfo(host, port_str, &hints, &res);
    if (err != 0) {
        __gorget_tls_last_error = gai_strerror(err);
        return sock;
    }

    // TCP connect
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
        __gorget_tls_last_error = "TCP connection failed";
        return sock;
    }

    // SSL setup
    SSL_CTX* ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) {
        close(fd);
        __gorget_tls_last_error = "SSL_CTX_new failed";
        return sock;
    }
    SSL_CTX_set_default_verify_paths(ctx);

    SSL* ssl = SSL_new(ctx);
    if (!ssl) {
        SSL_CTX_free(ctx);
        close(fd);
        __gorget_tls_last_error = "SSL_new failed";
        return sock;
    }

    SSL_set_fd(ssl, fd);
    SSL_set_tlsext_host_name(ssl, host);  // SNI

    if (SSL_connect(ssl) != 1) {
        __gorget_tls_last_error = "TLS handshake failed";
        SSL_free(ssl);
        SSL_CTX_free(ctx);
        close(fd);
        return (GorgetTlsSocket){-1, NULL, NULL};
    }

    // Verify certificate
    long verify_result = SSL_get_verify_result(ssl);
    if (verify_result != X509_V_OK) {
        __gorget_tls_last_error = X509_verify_cert_error_string(verify_result);
        SSL_shutdown(ssl);
        SSL_free(ssl);
        SSL_CTX_free(ctx);
        close(fd);
        return (GorgetTlsSocket){-1, NULL, NULL};
    }

    return (GorgetTlsSocket){fd, ctx, ssl};
}

static const char* gorget_tls_last_error(void) {
    return __gorget_tls_last_error;
}

static GorgetArray gorget_tls_read(GorgetTlsSocket* sock, int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0 || !sock->ssl) return arr;
    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)n);
    int got = SSL_read(sock->ssl, buf, (int)n);
    if (got > 0) {
        arr.data = buf;
        arr.len = (size_t)got;
        arr.cap = (size_t)n;
    } else {
        GORGET_FREE(buf, 0);
    }
    return arr;
}

static GorgetArray gorget_tls_read_exact(GorgetTlsSocket* sock, int64_t n) {
    GorgetArray arr = gorget_array_new(sizeof(uint8_t));
    if (n <= 0 || !sock->ssl) return arr;
    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)n);
    size_t total = 0;
    while (total < (size_t)n) {
        int got = SSL_read(sock->ssl, buf + total, (int)((size_t)n - total));
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

static int64_t gorget_tls_write(GorgetTlsSocket* sock, const GorgetArray* data) {
    if (!sock->ssl || data->len == 0) return 0;
    size_t total = 0;
    while (total < data->len) {
        int sent = SSL_write(sock->ssl, (uint8_t*)data->data + total, (int)(data->len - total));
        if (sent <= 0) return -1;
        total += (size_t)sent;
    }
    return (int64_t)total;
}

static int64_t gorget_tls_write_str(GorgetTlsSocket* sock, const char* s) {
    if (!sock->ssl || !s) return 0;
    size_t len = strlen(s);
    size_t total = 0;
    while (total < len) {
        int sent = SSL_write(sock->ssl, s + total, (int)(len - total));
        if (sent <= 0) return -1;
        total += (size_t)sent;
    }
    return (int64_t)total;
}

// ── Byte-oriented TLS Writer/Reader helpers (errno-returning) ───
//
// Same contract as the socket/file versions: non-negative count on
// success, negative errno on failure. OpenSSL error codes are mapped
// to a best-fit errno (ECONNRESET for SSL_ERROR_ZERO_RETURN, EIO
// otherwise) so the Gorget-side `_errno_to_io_error` helper can
// produce a structured `IoError`.
static int64_t gorget_tls_write_bytes_buf(GorgetTlsSocket* sock, const GorgetArray* buf) {
    if (!sock || !sock->ssl) return -9;   // EBADF
    if (!buf || !buf->data || buf->len == 0) return 0;
    int sent = SSL_write(sock->ssl, buf->data, (int)buf->len);
    if (sent <= 0) {
        int ssl_err = SSL_get_error(sock->ssl, sent);
        if (ssl_err == SSL_ERROR_ZERO_RETURN) return -104;  // ECONNRESET
        int e = errno;
        if (e == 0) e = 5;   // EIO
        return -(int64_t)e;
    }
    return (int64_t)sent;
}

static int64_t gorget_tls_read_bytes_buf(GorgetTlsSocket* sock, GorgetArray* buf, int64_t max_bytes) {
    if (!sock || !sock->ssl) return -9;   // EBADF
    if (!buf || max_bytes <= 0) return 0;
    size_t old_len = (size_t)buf->len;
    gorget_array_ensure_capacity(buf, old_len + (size_t)max_bytes, 1);
    int got = SSL_read(sock->ssl, (uint8_t*)buf->data + old_len, (int)max_bytes);
    if (got < 0) {
        int ssl_err = SSL_get_error(sock->ssl, got);
        if (ssl_err == SSL_ERROR_ZERO_RETURN) return 0;   // clean close → EOF
        int e = errno;
        if (e == 0) e = 5;   // EIO
        return -(int64_t)e;
    }
    if (got == 0) return 0;   // EOF
    buf->len = (int64_t)(old_len + (size_t)got);
    return (int64_t)got;
}

static GorgetString gorget_tls_read_line(GorgetTlsSocket* sock) {
    if (!sock->ssl) return GORGET_EMPTY_STR;
    size_t cap = 256, len = 0;
    char* buf = (char*)GORGET_ALLOC(cap);
    while (1) {
        char c;
        int got = SSL_read(sock->ssl, &c, 1);
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
    // Use the SSL_read-accumulated byte count directly — going through strlen
    // (gorget_string_adopt) would silently truncate at the first embedded
    // NUL byte (binary protocols, malformed input). Mirrors commit 53a4db02.
    return str_adopt_buf(buf, len, cap, __gorget_current_alloc);
}

static void gorget_tls_close(GorgetTlsSocket* sock) {
    if (sock->ssl) {
        SSL_shutdown(sock->ssl);
        SSL_free(sock->ssl);
        sock->ssl = NULL;
    }
    if (sock->ctx) {
        SSL_CTX_free(sock->ctx);
        sock->ctx = NULL;
    }
    if (sock->fd >= 0) {
        close(sock->fd);
        sock->fd = -1;
    }
}

static void gorget_tls_set_timeout(GorgetTlsSocket* sock, int64_t ms) {
    if (sock->fd < 0) return;
    struct timeval tv;
    tv.tv_sec = ms / 1000;
    tv.tv_usec = (ms % 1000) * 1000;
    setsockopt(sock->fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(sock->fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
}

// Forward-declare GorgetTlsServerSocket so that programs importing std.net.tls
// (e.g. via xtd.http) can reference TlsServerSocket in type aliases and Result
// structs even when TLS_SERVER_RUNTIME is not emitted (no tls_server_bind call).
typedef struct {
    int64_t fd;
    SSL_CTX* ctx;
} GorgetTlsServerSocket;

