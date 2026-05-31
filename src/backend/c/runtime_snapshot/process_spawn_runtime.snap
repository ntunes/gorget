
// ── std.process: Process type (fork+exec+pipes) ──────────────
#include <signal.h>
#include <fcntl.h>

typedef struct GorgetProcess {
    pid_t pid;
    int   stdin_fd;   // parent write end  (-1 if not used)
    int   stdout_fd;  // parent read end   (-1 if not used)
    int   stderr_fd;  // parent read end   (-1 if not used)
} GorgetProcess;

// Spawn a child process with stdin/stdout/stderr pipes.
// Returns a Result[Process, str]:
//   Ok  → packed as GorgetProcess* (non-NULL)
//   Err → packed as NULL (caller reads gorget_process_spawn_err() for message)
static _Thread_local char __gorget_spawn_errbuf[256];

static inline GorgetProcess* gorget_process_spawn(const char* program, GorgetArray* args) {
    __gorget_spawn_errbuf[0] = '\0'; // clear stale error
    int in_pipe[2], out_pipe[2], err_pipe[2];
    if (pipe(in_pipe)  < 0 ||
        pipe(out_pipe) < 0 ||
        pipe(err_pipe) < 0) {
        snprintf(__gorget_spawn_errbuf, sizeof(__gorget_spawn_errbuf),
                 "pipe: %s", strerror(errno));
        return NULL;
    }

    // Build argv: program + elements from args vector
    int argc = args ? (int)args->len : 0;
    char** argv = (char**)GORGET_ALLOC(sizeof(char*) * (size_t)(argc + 2));
    argv[0] = (char*)program;
    for (int i = 0; i < argc; i++) {
        Str* sv = (Str*)gorget_array_get(args, i);
        size_t slen = sv->len;
        char* s = (char*)GORGET_ALLOC(slen + 1);
        if (slen > 0) memcpy(s, sv->data, slen);
        s[slen] = '\0';
        argv[i + 1] = s;
    }
    argv[argc + 1] = NULL;

    pid_t pid = fork();
    if (pid < 0) {
        snprintf(__gorget_spawn_errbuf, sizeof(__gorget_spawn_errbuf),
                 "fork: %s", strerror(errno));
        close(in_pipe[0]); close(in_pipe[1]);
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        GORGET_FREE(argv, 0);
        return NULL;
    }
    if (pid == 0) {
        // Child: wire pipes then exec
        dup2(in_pipe[0],  STDIN_FILENO);
        dup2(out_pipe[1], STDOUT_FILENO);
        dup2(err_pipe[1], STDERR_FILENO);
        close(in_pipe[0]); close(in_pipe[1]);
        close(out_pipe[0]); close(out_pipe[1]);
        close(err_pipe[0]); close(err_pipe[1]);
        execvp(argv[0], argv);
        _exit(127);
    }
    // Parent: close child-side fds
    close(in_pipe[0]);
    close(out_pipe[1]);
    close(err_pipe[1]);

    GorgetProcess* proc = (GorgetProcess*)GORGET_ALLOC(sizeof(GorgetProcess));
    proc->pid       = pid;
    proc->stdin_fd  = in_pipe[1];
    proc->stdout_fd = out_pipe[0];
    proc->stderr_fd = err_pipe[0];
    GORGET_FREE(argv, 0);
    return proc;
}

static inline const char* gorget_process_spawn_err(void) {
    return __gorget_spawn_errbuf[0] ? __gorget_spawn_errbuf : NULL;
}

static inline int64_t gorget_process_wait(GorgetProcess* proc) {
    int status = 0;
    waitpid(proc->pid, &status, 0);
    return WIFEXITED(status) ? (int64_t)WEXITSTATUS(status) : (int64_t)-1;
}

static inline void gorget_process_kill(GorgetProcess* proc) {
    kill(proc->pid, SIGTERM);
}

static inline int64_t gorget_process_pid(GorgetProcess* proc) {
    return (int64_t)proc->pid;
}

static inline void gorget_process_write_stdin(GorgetProcess* proc, const char* data) {
    if (proc->stdin_fd >= 0 && data) { size_t len = strlen(data); if (len > 0) write(proc->stdin_fd, data, len); }
}

static inline void gorget_process_close_stdin(GorgetProcess* proc) {
    if (proc->stdin_fd >= 0) { close(proc->stdin_fd); proc->stdin_fd = -1; }
}

static inline GorgetString gorget_process_read_stdout(GorgetProcess* proc) {
    if (proc->stdout_fd < 0) return gorget_string_new("");
    size_t cap = 256, len = 0;
    char* buf = (char*)GORGET_ALLOC(cap);
    ssize_t n;
    while ((n = read(proc->stdout_fd, buf + len, cap - len - 1)) > 0) {
        len += (size_t)n;
        if (len + 1 >= cap) { size_t newcap = cap * 2; buf = (char*)GORGET_REALLOC(buf, cap, newcap); cap = newcap; }
    }
    buf[len] = '\0';
    return str_adopt_buf(buf, len, cap, __gorget_current_alloc);
}

static inline GorgetString gorget_process_read_stderr(GorgetProcess* proc) {
    if (proc->stderr_fd < 0) return gorget_string_new("");
    size_t cap = 256, len = 0;
    char* buf = (char*)GORGET_ALLOC(cap);
    ssize_t n;
    while ((n = read(proc->stderr_fd, buf + len, cap - len - 1)) > 0) {
        len += (size_t)n;
        if (len + 1 >= cap) { size_t newcap = cap * 2; buf = (char*)GORGET_REALLOC(buf, cap, newcap); cap = newcap; }
    }
    buf[len] = '\0';
    return str_adopt_buf(buf, len, cap, __gorget_current_alloc);
}

// wait_timeout(ms) — poll-based wait with deadline.
// Returns exit code on child exit, or -2 on timeout (child is NOT killed).
static inline int64_t gorget_process_wait_timeout(GorgetProcess* proc, int64_t timeout_ms) {
    struct timespec start, now;
    clock_gettime(CLOCK_MONOTONIC, &start);
    for (;;) {
        int status = 0;
        pid_t r = waitpid(proc->pid, &status, WNOHANG);
        if (r > 0) return WIFEXITED(status) ? (int64_t)WEXITSTATUS(status) : (int64_t)-1;
        clock_gettime(CLOCK_MONOTONIC, &now);
        int64_t elapsed = (now.tv_sec - start.tv_sec) * 1000
                        + (now.tv_nsec - start.tv_nsec) / 1000000;
        if (elapsed >= timeout_ms) return (int64_t)-2; // timeout
        struct timespec ts = {0, 5000000}; // 5ms poll interval
        nanosleep(&ts, NULL);
    }
}

// Shared poll-based drain of stdout + stderr into buffers.
// If timeout_ms >= 0, kills child and sets timed_out on deadline.
// If timeout_ms < 0, blocks indefinitely.
#include <poll.h>
static inline ExecResult gorget_process__drain(GorgetProcess* proc, int64_t timeout_ms) {
    ExecResult result;
    int stdout_fd = proc->stdout_fd;
    int stderr_fd = proc->stderr_fd;
    // Close stdin so child sees EOF
    if (proc->stdin_fd >= 0) { close(proc->stdin_fd); proc->stdin_fd = -1; }

    size_t o_cap = 256, o_len = 0;
    char* o_buf = (char*)GORGET_ALLOC(o_cap);
    size_t e_cap = 256, e_len = 0;
    char* e_buf = (char*)GORGET_ALLOC(e_cap);

    struct timespec start;
    if (timeout_ms >= 0) clock_gettime(CLOCK_MONOTONIC, &start);
    int timed_out = 0;

    int open_fds = (stdout_fd >= 0 ? 1 : 0) + (stderr_fd >= 0 ? 1 : 0);
    while (open_fds > 0 && !timed_out) {
        int poll_timeout = -1; // block indefinitely
        if (timeout_ms >= 0) {
            struct timespec now;
            clock_gettime(CLOCK_MONOTONIC, &now);
            int64_t elapsed = (now.tv_sec - start.tv_sec) * 1000
                            + (now.tv_nsec - start.tv_nsec) / 1000000;
            int64_t remaining = timeout_ms - elapsed;
            if (remaining <= 0) { timed_out = 1; break; }
            poll_timeout = (int)(remaining < 100 ? remaining : 100);
        }

        struct pollfd fds[2];
        int nfds = 0;
        int stdout_idx = -1, stderr_idx = -1;
        if (stdout_fd >= 0) { stdout_idx = nfds; fds[nfds].fd = stdout_fd; fds[nfds].events = POLLIN; nfds++; }
        if (stderr_fd >= 0) { stderr_idx = nfds; fds[nfds].fd = stderr_fd; fds[nfds].events = POLLIN; nfds++; }
        if (nfds == 0) break;
        int r = poll(fds, (nfds_t)nfds, poll_timeout);
        if (r < 0) break;
        if (stdout_idx >= 0 && (fds[stdout_idx].revents & (POLLIN|POLLHUP))) {
            if (o_len + 1 >= o_cap) { size_t nc = o_cap * 2; o_buf = (char*)GORGET_REALLOC(o_buf, o_cap, nc); o_cap = nc; }
            ssize_t n = read(stdout_fd, o_buf + o_len, o_cap - o_len - 1);
            if (n <= 0) { close(stdout_fd); stdout_fd = -1; proc->stdout_fd = -1; open_fds--; }
            else o_len += (size_t)n;
        }
        if (stderr_idx >= 0 && (fds[stderr_idx].revents & (POLLIN|POLLHUP))) {
            if (e_len + 1 >= e_cap) { size_t nc = e_cap * 2; e_buf = (char*)GORGET_REALLOC(e_buf, e_cap, nc); e_cap = nc; }
            ssize_t n = read(stderr_fd, e_buf + e_len, e_cap - e_len - 1);
            if (n <= 0) { close(stderr_fd); stderr_fd = -1; proc->stderr_fd = -1; open_fds--; }
            else e_len += (size_t)n;
        }
    }
    o_buf[o_len] = '\0'; e_buf[e_len] = '\0';

    if (timed_out) {
        kill(proc->pid, SIGKILL);
        waitpid(proc->pid, NULL, 0);
        if (stdout_fd >= 0) close(stdout_fd);
        if (stderr_fd >= 0) close(stderr_fd);
        proc->stdout_fd = -1; proc->stderr_fd = -1;
        result.exit_code = (int64_t)-2;
    } else {
        int status = 0;
        waitpid(proc->pid, &status, 0);
        result.exit_code = WIFEXITED(status) ? (int64_t)WEXITSTATUS(status) : (int64_t)-1;
    }
    // Use the read-accumulated byte counts directly — going through strlen
    // (gorget_string_adopt) would silently truncate subprocess stdout/stderr
    // at the first embedded NUL byte (binary tools, raw protocol captures).
    // Mirrors commit 53a4db02.
    result.output = str_adopt_buf(o_buf, o_len, o_cap, __gorget_current_alloc);
    result.errors = str_adopt_buf(e_buf, e_len, e_cap, __gorget_current_alloc);
    return result;
}

// read_all() — drain stdout + stderr simultaneously, wait for exit.
static inline ExecResult gorget_process_read_all(GorgetProcess* proc) {
    return gorget_process__drain(proc, -1);
}

// read_all_timeout(ms) — drain with deadline; kills child on timeout (exit_code == -2).
static inline ExecResult gorget_process_read_all_timeout(GorgetProcess* proc, int64_t timeout_ms) {
    return gorget_process__drain(proc, timeout_ms);
}

static inline int64_t gorget_getpid(void) {
    return (int64_t)getpid();
}

// ── Signal Handling ──────────────────────────────────────────
#include <signal.h>

// Flag array: one volatile flag per signal number.
// NSIG is POSIX-defined as one more than the highest signal number.
#ifndef NSIG
#define NSIG 65
#endif
static volatile sig_atomic_t __gorget_signal_flags[NSIG];

// C-level signal handler: sets the flag for the received signal.
static void __gorget_signal_handler(int sig) {
    if (sig >= 0 && sig < NSIG) {
        __gorget_signal_flags[sig] = 1;
    }
}

// trap(sig) — install a handler that sets a flag when the signal is received.
static inline void gorget_signal_trap(int64_t sig) {
    if (sig < 0 || sig >= NSIG) return;
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = __gorget_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    sigaction((int)sig, &sa, NULL);
}

// check(sig) -> bool — returns true if signal was received since last check, clears flag.
static inline bool gorget_signal_check(int64_t sig) {
    if (sig < 0 || sig >= NSIG) return false;
    if (__gorget_signal_flags[sig]) {
        __gorget_signal_flags[sig] = 0;
        return true;
    }
    return false;
}

// wait_signal() -> int — block until any trapped signal arrives, return its number.
static inline int64_t gorget_signal_wait(void) {
    for (;;) {
        for (int i = 1; i < NSIG; i++) {
            if (__gorget_signal_flags[i]) {
                __gorget_signal_flags[i] = 0;
                return (int64_t)i;
            }
        }
        // Sleep briefly to avoid busy-waiting, then check again.
        // pause() would also work but returns -1/EINTR which is fine.
        pause();
    }
}

// ignore(sig) — set SIG_IGN for the given signal.
static inline void gorget_signal_ignore(int64_t sig) {
    if (sig < 0 || sig >= NSIG) return;
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sigaction((int)sig, &sa, NULL);
}

// reset(sig) — restore the default handler for the given signal.
static inline void gorget_signal_reset(int64_t sig) {
    if (sig < 0 || sig >= NSIG) return;
    __gorget_signal_flags[sig] = 0;
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sigaction((int)sig, &sa, NULL);
}

// send(pid, sig) — send a signal to a process.
static inline int64_t gorget_signal_send(int64_t pid, int64_t sig) {
    return (int64_t)kill((pid_t)pid, (int)sig);
}
