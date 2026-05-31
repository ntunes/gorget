
// ── TCP Server Socket (std.net.socket) ──────────────────────
// GorgetServerSocket is declared in SOCKET_RUNTIME; only functions follow here.

static const char* __gorget_server_socket_last_error = NULL;

// Bind and listen on host:port; sets SO_REUSEADDR; backlog=128.
// Returns GorgetServerSocket{-1} on error (check gorget_server_socket_last_error).
static GorgetServerSocket gorget_server_socket_bind(const char* host, int64_t port) {
    __gorget_server_socket_last_error = NULL;
    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%lld", (long long)port);

    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    int err = getaddrinfo(host, port_str, &hints, &res);
    if (err != 0) {
        __gorget_server_socket_last_error = gai_strerror(err);
        return (GorgetServerSocket){-1};
    }

    int fd = -1;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (fd < 0) continue;
        int opt = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
        if (bind(fd, rp->ai_addr, rp->ai_addrlen) == 0) break;
        close(fd);
        fd = -1;
    }
    freeaddrinfo(res);

    if (fd < 0) {
        __gorget_server_socket_last_error = strerror(errno);
        return (GorgetServerSocket){-1};
    }
    if (listen(fd, 128) != 0) {
        __gorget_server_socket_last_error = strerror(errno);
        close(fd);
        return (GorgetServerSocket){-1};
    }
    return (GorgetServerSocket){fd};
}

static const char* gorget_server_socket_last_error(void) {
    return __gorget_server_socket_last_error;
}

// Blocking accept(); returns a GorgetSocket reusing all existing read/write/close methods.
static GorgetSocket gorget_server_socket_accept(GorgetServerSocket* srv) {
    __gorget_server_socket_last_error = NULL;
    if (srv->fd < 0) {
        __gorget_server_socket_last_error = "server socket is closed";
        return (GorgetSocket){-1};
    }
    int fd = accept(srv->fd, NULL, NULL);
    if (fd < 0) {
        __gorget_server_socket_last_error = strerror(errno);
        return (GorgetSocket){-1};
    }
    return (GorgetSocket){fd};
}

static void gorget_server_socket_close(GorgetServerSocket* srv) {
    if (srv->fd >= 0) {
        close(srv->fd);
        srv->fd = -1;
    }
}

static int64_t gorget_server_socket_local_port(GorgetServerSocket* srv) {
    struct sockaddr_in sa;
    socklen_t len = sizeof(sa);
    if (getsockname(srv->fd, (struct sockaddr*)&sa, &len) == 0) {
        return (int64_t)ntohs(sa.sin_port);
    }
    return -1;
}

// Set server socket to non-blocking mode (for async accept)
static void gorget_server_socket_set_nonblocking(GorgetServerSocket* srv) {
    if (srv->fd >= 0) {
        int flags = fcntl(srv->fd, F_GETFL, 0);
        fcntl(srv->fd, F_SETFL, flags | O_NONBLOCK);
    }
}

