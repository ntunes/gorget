
// ── UDP Socket (std.net.udp) ────────────────────────────────
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <poll.h>

typedef struct {
    int64_t fd;
} GorgetUdpSocket;

typedef struct {
    Str host;
    int64_t port;
} GorgetUdpAddr;

typedef struct {
    GorgetArray data;
    GorgetUdpAddr sender;
} GorgetUdpPacket;

static const char* __gorget_udp_last_error = NULL;
static int __gorget_udp_last_errno = 0;

static const char* gorget_udp_last_error(void) {
    return __gorget_udp_last_error;
}

static int64_t gorget_udp_last_errno(void) {
    return (int64_t)__gorget_udp_last_errno;
}

static GorgetUdpSocket gorget_udp_bind(const char* addr, int64_t port) {
    __gorget_udp_last_error = NULL;
    __gorget_udp_last_errno = 0;
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        __gorget_udp_last_errno = errno;
        __gorget_udp_last_error = strerror(errno);
        return (GorgetUdpSocket){-1};
    }

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
#ifdef SO_REUSEPORT
    setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(opt));
#endif

    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons((uint16_t)port);
    if (inet_pton(AF_INET, addr, &sa.sin_addr) <= 0) {
        sa.sin_addr.s_addr = INADDR_ANY;
    }

    if (bind(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) {
        __gorget_udp_last_errno = errno;
        __gorget_udp_last_error = strerror(errno);
        close(fd);
        return (GorgetUdpSocket){-1};
    }
    return (GorgetUdpSocket){fd};
}

static int64_t gorget_udp_sendto(GorgetUdpSocket* sock, const GorgetArray* data, const char* host, int64_t port) {
    __gorget_udp_last_error = NULL;
    __gorget_udp_last_errno = 0;
    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons((uint16_t)port);
    if (inet_pton(AF_INET, host, &sa.sin_addr) <= 0) {
        // Try hostname resolution
        struct hostent* he = gethostbyname(host);
        if (!he) {
            __gorget_udp_last_errno = EHOSTUNREACH;
            __gorget_udp_last_error = "hostname resolution failed";
            return -1;
        }
        memcpy(&sa.sin_addr, he->h_addr_list[0], (size_t)he->h_length);
    }

    ssize_t sent = sendto(sock->fd, data->data, (size_t)(data->len * data->elem_size),
                          0, (struct sockaddr*)&sa, sizeof(sa));
    if (sent < 0) {
        __gorget_udp_last_errno = errno;
        __gorget_udp_last_error = strerror(errno);
        return -1;
    }
    return (int64_t)sent;
}

static GorgetUdpPacket gorget_udp_recvfrom(GorgetUdpSocket* sock, int64_t max_bytes) {
    __gorget_udp_last_error = NULL;
    __gorget_udp_last_errno = 0;
    GorgetUdpPacket pkt;
    memset(&pkt, 0, sizeof(pkt));

    uint8_t* buf = (uint8_t*)GORGET_ALLOC((size_t)max_bytes);
    if (!buf) {
        __gorget_udp_last_errno = ENOMEM;
        __gorget_udp_last_error = "out of memory";
        return pkt;
    }

    struct sockaddr_in sender_addr;
    socklen_t addr_len = sizeof(sender_addr);
    ssize_t n = recvfrom(sock->fd, buf, (size_t)max_bytes, 0,
                         (struct sockaddr*)&sender_addr, &addr_len);
    if (n < 0) {
        __gorget_udp_last_errno = errno;
        __gorget_udp_last_error = strerror(errno);
        GORGET_FREE(buf, 0);
        return pkt;
    }

    // Build data array
    pkt.data.data = buf;
    pkt.data.len = (int64_t)n;
    pkt.data.cap = max_bytes;
    pkt.data.elem_size = 1;
    pkt.data.alloc = &__gorget_global_alloc;

    // Build sender address
    static __thread char sender_host[INET_ADDRSTRLEN];
    inet_ntop(AF_INET, &sender_addr.sin_addr, sender_host, sizeof(sender_host));
    pkt.sender.host = gorget_str_from_cstr(sender_host);
    pkt.sender.port = (int64_t)ntohs(sender_addr.sin_port);

    return pkt;
}

static bool gorget_udp_poll(GorgetUdpSocket* sock, int64_t timeout_ms) {
    struct pollfd pfd;
    pfd.fd = sock->fd;
    pfd.events = POLLIN;
    pfd.revents = 0;
    int ret = poll(&pfd, 1, (int)timeout_ms);
    return ret > 0 && (pfd.revents & POLLIN);
}

static void gorget_udp_set_nonblocking(GorgetUdpSocket* sock, bool enabled) {
    int flags = fcntl(sock->fd, F_GETFL, 0);
    if (enabled) {
        fcntl(sock->fd, F_SETFL, flags | O_NONBLOCK);
    } else {
        fcntl(sock->fd, F_SETFL, flags & ~O_NONBLOCK);
    }
}

static bool gorget_udp_join_multicast(GorgetUdpSocket* sock, const char* group_addr) {
    __gorget_udp_last_error = NULL;
    __gorget_udp_last_errno = 0;
    struct ip_mreq mreq;
    memset(&mreq, 0, sizeof(mreq));
    if (inet_pton(AF_INET, group_addr, &mreq.imr_multiaddr) <= 0) {
        __gorget_udp_last_errno = EINVAL;
        __gorget_udp_last_error = "invalid multicast address";
        return false;
    }
    mreq.imr_interface.s_addr = INADDR_ANY;
    if (setsockopt(sock->fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq)) < 0) {
        __gorget_udp_last_errno = errno;
        __gorget_udp_last_error = strerror(errno);
        return false;
    }
    return true;
}

static void gorget_udp_leave_multicast(GorgetUdpSocket* sock, const char* group_addr) {
    struct ip_mreq mreq;
    memset(&mreq, 0, sizeof(mreq));
    inet_pton(AF_INET, group_addr, &mreq.imr_multiaddr);
    mreq.imr_interface.s_addr = INADDR_ANY;
    setsockopt(sock->fd, IPPROTO_IP, IP_DROP_MEMBERSHIP, &mreq, sizeof(mreq));
}

static void gorget_udp_set_multicast_loopback(GorgetUdpSocket* sock, bool enabled) {
    uint8_t val = enabled ? 1 : 0;
    setsockopt(sock->fd, IPPROTO_IP, IP_MULTICAST_LOOP, &val, sizeof(val));
}

static GorgetUdpAddr gorget_udp_local_addr(GorgetUdpSocket* sock) {
    GorgetUdpAddr addr;
    memset(&addr, 0, sizeof(addr));
    struct sockaddr_in sa;
    socklen_t len = sizeof(sa);
    if (getsockname(sock->fd, (struct sockaddr*)&sa, &len) == 0) {
        static __thread char host_buf[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &sa.sin_addr, host_buf, sizeof(host_buf));
        addr.host = gorget_str_from_cstr(host_buf);
        addr.port = (int64_t)ntohs(sa.sin_port);
    }
    return addr;
}

static void gorget_udp_close(GorgetUdpSocket* sock) {
    if (sock->fd >= 0) {
        close(sock->fd);
        sock->fd = -1;
    }
}

