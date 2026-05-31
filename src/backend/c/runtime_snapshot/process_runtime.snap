
// ── std.process runtime ──────────────────────────────────────
#include <poll.h>
#include <fcntl.h>
static inline int64_t gorget_exec(const char* cmd) {
    int status = system(cmd);
    if (status == -1) return -1;
    #ifdef _WIN32
    return (int64_t)status;
    #else
    return WIFEXITED(status) ? (int64_t)WEXITSTATUS(status) : (int64_t)-1;
    #endif
}

typedef struct ExecResult {
    Str output;
    Str errors;
    int64_t exit_code;
} ExecResult;

// Read everything available from `fd` into a growable buffer.
// Returns the malloc'd buffer (NUL-terminated, caller adopts) and writes the
// byte length (excluding the trailing NUL) into *out_len. Returns NULL with
// *out_len = 0 on read error. Used by gorget_exec_output to drain a pipe fd
// once the child has finished writing — stdout and stderr are read in the
// same shape.
static inline char* __gorget_drain_fd(int fd, size_t* out_len) {
    size_t cap = 256;
    size_t len = 0;
    char* buf = (char*)GORGET_ALLOC(cap);
    if (!buf) { *out_len = 0; return NULL; }
    while (1) {
        ssize_t n = read(fd, buf + len, cap - len - 1);
        if (n > 0) {
            len += (size_t)n;
            if (len + 1 >= cap) {
                cap *= 2;
                buf = (char*)GORGET_REALLOC(buf, 0, cap);
                if (!buf) { *out_len = 0; return NULL; }
            }
        } else if (n == 0) {
            break;  // EOF
        } else if (errno == EINTR) {
            continue;
        } else {
            break;  // genuine read error — return whatever we got
        }
    }
    buf[len] = '\0';
    *out_len = len;
    return buf;
}

// Spawn `/bin/sh -c <cmd>`, capture BOTH stdout and stderr separately into
// the returned ExecResult. Uses fork + pipe + execvp rather than popen() so
// that stderr is captured (popen("r") only opens a pipe for stdout, leaving
// stderr to leak to the parent). To avoid pipe-buffer deadlock when both
// streams produce a lot of data, the parent uses poll() to read from
// whichever fd has data ready until both EOF.
static inline ExecResult gorget_exec_output(const char* cmd) {
    ExecResult result;
    result.output = gorget_str_from_literal("", 0);
    result.errors = gorget_str_from_literal("", 0);
    result.exit_code = -1;

    int out_pipe[2], err_pipe[2];
    if (pipe(out_pipe) < 0) return result;
    if (pipe(err_pipe) < 0) {
        close(out_pipe[0]); close(out_pipe[1]);
        return result;
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        return result;
    }
    if (pid == 0) {
        // Child — wire pipes onto stdout/stderr, exec the shell.
        dup2(out_pipe[1], STDOUT_FILENO);
        dup2(err_pipe[1], STDERR_FILENO);
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        execl("/bin/sh", "sh", "-c", cmd, (char*)NULL);
        _exit(127);
    }

    // Parent — close write ends, drain both pipes concurrently via poll().
    close(out_pipe[1]);
    close(err_pipe[1]);

    // Make both fds non-blocking so a single drain loop can interleave reads
    // without ever blocking on one stream while the other is filling its
    // buffer (classic deadlock pattern with synchronous read on each fd).
    int out_fd = out_pipe[0];
    int err_fd = err_pipe[0];
    int flags;
    flags = fcntl(out_fd, F_GETFL, 0); if (flags >= 0) fcntl(out_fd, F_SETFL, flags | O_NONBLOCK);
    flags = fcntl(err_fd, F_GETFL, 0); if (flags >= 0) fcntl(err_fd, F_SETFL, flags | O_NONBLOCK);

    size_t out_cap = 256, out_len = 0;
    char* out_buf = (char*)GORGET_ALLOC(out_cap);
    size_t err_cap = 256, err_len = 0;
    char* err_buf = (char*)GORGET_ALLOC(err_cap);
    int out_open = (out_buf != NULL), err_open = (err_buf != NULL);

    while (out_open || err_open) {
        struct pollfd pfds[2];
        int nfds = 0;
        if (out_open) { pfds[nfds].fd = out_fd; pfds[nfds].events = POLLIN; nfds++; }
        if (err_open) { pfds[nfds].fd = err_fd; pfds[nfds].events = POLLIN; nfds++; }
        int pr = poll(pfds, (nfds_t)nfds, -1);
        if (pr < 0) {
            if (errno == EINTR) continue;
            break;
        }
        for (int i = 0; i < nfds; i++) {
            if (!(pfds[i].revents & (POLLIN | POLLHUP | POLLERR))) continue;
            int is_out = (pfds[i].fd == out_fd);
            char** buf = is_out ? &out_buf : &err_buf;
            size_t* len = is_out ? &out_len : &err_len;
            size_t* cap = is_out ? &out_cap : &err_cap;
            int* open_flag = is_out ? &out_open : &err_open;
            // Drain everything currently available on this fd.
            while (1) {
                if (*len + 1 >= *cap) {
                    *cap *= 2;
                    *buf = (char*)GORGET_REALLOC(*buf, 0, *cap);
                    if (!*buf) { *open_flag = 0; break; }
                }
                ssize_t n = read(pfds[i].fd, *buf + *len, *cap - *len - 1);
                if (n > 0) {
                    *len += (size_t)n;
                } else if (n == 0) {
                    *open_flag = 0;  // EOF
                    break;
                } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
                    break;  // no more data right now
                } else if (errno == EINTR) {
                    continue;
                } else {
                    *open_flag = 0;
                    break;
                }
            }
        }
    }
    if (out_buf) out_buf[out_len] = '\0';
    if (err_buf) err_buf[err_len] = '\0';
    close(out_fd);
    close(err_fd);

    int status = 0;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    result.exit_code = WIFEXITED(status) ? (int64_t)WEXITSTATUS(status) : (int64_t)-1;
    // Use the read-accumulated byte counts directly — going through strlen
    // (gorget_string_adopt) would silently truncate subprocess stdout/stderr
    // at the first embedded NUL byte (binary tools, raw protocol captures).
    // Mirrors commit 53a4db02.
    result.output = out_buf ? str_adopt_buf(out_buf, out_len, out_cap, __gorget_current_alloc)
                            : gorget_str_from_literal("", 0);
    result.errors = err_buf ? str_adopt_buf(err_buf, err_len, err_cap, __gorget_current_alloc)
                            : gorget_str_from_literal("", 0);
    return result;
}
