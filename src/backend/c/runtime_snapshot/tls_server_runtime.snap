
// ── TLS Server Socket (std.net.tls) ─────────────────────────
// (GorgetTlsServerSocket is defined in TLS_SOCKET_RUNTIME to ensure the type
//  is available even when TLS_SERVER_RUNTIME is not emitted.)

static const char* __gorget_tls_server_last_error = NULL;

static const char* gorget_tls_server_last_error(void) {
    return __gorget_tls_server_last_error;
}

// Bind a TLS server on host:port using the given PEM certificate and key files.
// Returns GorgetTlsServerSocket{-1, NULL} on error.
static GorgetTlsServerSocket gorget_tls_server_bind(
    const char* host, int64_t port,
    const char* cert_path, const char* key_path)
{
    __gorget_tls_server_last_error = NULL;
    GorgetTlsServerSocket srv = {-1, NULL};

    SSL_CTX* ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) {
        __gorget_tls_server_last_error = "SSL_CTX_new failed";
        return srv;
    }
    if (SSL_CTX_use_certificate_file(ctx, cert_path, SSL_FILETYPE_PEM) <= 0) {
        __gorget_tls_server_last_error = "failed to load certificate";
        SSL_CTX_free(ctx);
        return srv;
    }
    if (SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) <= 0) {
        __gorget_tls_server_last_error = "failed to load private key";
        SSL_CTX_free(ctx);
        return srv;
    }
    if (!SSL_CTX_check_private_key(ctx)) {
        __gorget_tls_server_last_error = "certificate/key mismatch";
        SSL_CTX_free(ctx);
        return srv;
    }

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);
    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    int err = getaddrinfo(*host ? host : NULL, port_str, &hints, &res);
    if (err != 0) {
        __gorget_tls_server_last_error = gai_strerror(err);
        SSL_CTX_free(ctx);
        return srv;
    }
    int fd = -1;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) continue;
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
        if (bind(fd, rp->ai_addr, rp->ai_addrlen) == 0) break;
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);
    if (fd < 0) {
        __gorget_tls_server_last_error = "bind failed";
        SSL_CTX_free(ctx);
        return srv;
    }
    if (listen(fd, 128) < 0) {
        __gorget_tls_server_last_error = "listen failed";
        close(fd);
        SSL_CTX_free(ctx);
        return srv;
    }
    srv.fd = fd;
    srv.ctx = ctx;
    return srv;
}

// Accept one TLS client connection. Returns GorgetTlsSocket with ctx=NULL
// (the server retains ownership of SSL_CTX; gorget_tls_close handles ctx==NULL).
static GorgetTlsSocket gorget_tls_server_accept(GorgetTlsServerSocket* srv) {
    __gorget_tls_server_last_error = NULL;
    GorgetTlsSocket conn = {-1, NULL, NULL};
    if (srv->fd < 0) {
        __gorget_tls_server_last_error = "server not bound";
        return conn;
    }
    int client_fd = accept(srv->fd, NULL, NULL);
    if (client_fd < 0) {
        __gorget_tls_server_last_error = strerror(errno);
        return conn;
    }
    SSL* ssl = SSL_new(srv->ctx);
    if (!ssl) {
        close(client_fd);
        __gorget_tls_server_last_error = "SSL_new failed";
        return conn;
    }
    SSL_set_fd(ssl, client_fd);
    if (SSL_accept(ssl) <= 0) {
        SSL_free(ssl);
        close(client_fd);
        __gorget_tls_server_last_error = "TLS handshake failed";
        return conn;
    }
    conn.fd = client_fd;
    conn.ctx = NULL;   /* server owns SSL_CTX */
    conn.ssl = ssl;
    return conn;
}

static void gorget_tls_server_close(GorgetTlsServerSocket* srv) {
    if (srv->ctx) {
        SSL_CTX_free(srv->ctx);
        srv->ctx = NULL;
    }
    if (srv->fd >= 0) {
        close(srv->fd);
        srv->fd = -1;
    }
}

