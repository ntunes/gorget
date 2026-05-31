
// ── I/O Reactor (timerfd/kqueue) ──
#ifdef __linux__
#include <sys/timerfd.h>
#include <sys/epoll.h>
#elif defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/event.h>
#include <sys/time.h>
#endif
#include <errno.h>

typedef struct {
    int              fd;       /* timerfd (Linux) or kqueue ident (macOS) */
    pthread_mutex_t  mtx;
    pthread_cond_t   cond;
    volatile int     woken;
} GorgetReactorWaiter;

enum {
    GORGET_REACTOR_TIMER = 0,
    GORGET_REACTOR_READ  = 1,
    GORGET_REACTOR_WRITE = 2
};

typedef struct {
    int                  fd;    /* timerfd (Linux) or kqueue ident (macOS) or socket fd */
    GorgetReactorWaiter* w;     /* NULL when is_async=1 */
    int                  is_async;
    int                  kind;  /* GORGET_REACTOR_TIMER / _READ / _WRITE */
    GorgetWaker          async_waker;
} GorgetReactorEntry;

typedef struct {
    int                  event_fd;   /* epoll fd (Linux) / kqueue fd (macOS) */
    pthread_t            thread;
    volatile int         running;
    pthread_mutex_t      mtx;
    GorgetReactorEntry*  entries;
    size_t               len, cap;
} GorgetReactor;

static GorgetReactor     __gorget_reactor;
static pthread_once_t    __gorget_reactor_once = PTHREAD_ONCE_INIT;

static void __gorget_reactor_fire_entry(GorgetReactorEntry* entry) {
    if (entry->is_async) {
        if (entry->kind == GORGET_REACTOR_TIMER) {
            close(entry->fd); /* timerfds are owned by reactor */
        }
        /* socket fds are NOT closed — owned by caller */
        entry->async_waker.wake(&entry->async_waker);
    } else {
        pthread_mutex_lock(&entry->w->mtx);
        entry->w->woken = 1;
        pthread_cond_signal(&entry->w->cond);
        pthread_mutex_unlock(&entry->w->mtx);
    }
}

static void* __gorget_reactor_thread(void* arg) {
    GorgetReactor* r = (GorgetReactor*)arg;
#ifdef __linux__
    struct epoll_event events[32];
    while (r->running) {
        int n = epoll_wait(r->event_fd, events, 32, 50 /* ms */);
        for (int i = 0; i < n; i++) {
            int fd = events[i].data.fd;
            pthread_mutex_lock(&r->mtx);
            for (size_t j = 0; j < r->len; j++) {
                if (r->entries[j].fd == fd) {
                    GorgetReactorEntry entry = r->entries[j];
                    r->entries[j] = r->entries[--r->len];
                    pthread_mutex_unlock(&r->mtx);
                    if (entry.kind == GORGET_REACTOR_TIMER) {
                        /* drain the timerfd */
                        uint64_t expirations;
                        (void)read(fd, &expirations, sizeof(expirations));
                    } else {
                        /* fd readiness: deregister from epoll (oneshot already did this
                           for EPOLLONESHOT, but explicit DEL is safe) */
                        epoll_ctl(r->event_fd, EPOLL_CTL_DEL, fd, NULL);
                    }
                    __gorget_reactor_fire_entry(&entry);
                    goto reactor_next;
                }
            }
            pthread_mutex_unlock(&r->mtx);
            reactor_next: ;
        }
    }
#elif defined(__APPLE__) || defined(__FreeBSD__)
    struct kevent events[32];
    while (r->running) {
        struct timespec timeout = { 0, 50000000 }; /* 50 ms */
        int n = kevent(r->event_fd, NULL, 0, events, 32, &timeout);
        for (int i = 0; i < n; i++) {
            uintptr_t ident = events[i].ident;
            pthread_mutex_lock(&r->mtx);
            for (size_t j = 0; j < r->len; j++) {
                if ((uintptr_t)r->entries[j].fd == ident) {
                    GorgetReactorEntry entry = r->entries[j];
                    r->entries[j] = r->entries[--r->len];
                    pthread_mutex_unlock(&r->mtx);
                    /* kqueue EV_ONESHOT auto-removes; no cleanup needed for either kind */
                    __gorget_reactor_fire_entry(&entry);
                    goto reactor_next_apple;
                }
            }
            pthread_mutex_unlock(&r->mtx);
            reactor_next_apple: ;
        }
    }
#endif
    return NULL;
}

static void __gorget_reactor_init(void) {
#if defined(__linux__)
    __gorget_reactor.event_fd = epoll_create1(EPOLL_CLOEXEC);
#elif defined(__APPLE__) || defined(__FreeBSD__)
    __gorget_reactor.event_fd = kqueue();
#else
    __gorget_reactor.event_fd = -1;
#endif
    __gorget_reactor.running  = 1;
    __gorget_reactor.len      = 0;
    __gorget_reactor.cap      = 16;
    __gorget_reactor.entries  = (GorgetReactorEntry*)GORGET_ALLOC(
        16 * sizeof(GorgetReactorEntry));
    pthread_mutex_init(&__gorget_reactor.mtx, NULL);
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__)
    pthread_create(&__gorget_reactor.thread, NULL, __gorget_reactor_thread, &__gorget_reactor);
#endif
}

/* Register a timer (ms) and block until it fires via the reactor waker protocol. */
static void gorget_reactor_sleep_ms(int64_t ms) {
    if (ms <= 0) return;
    pthread_once(&__gorget_reactor_once, __gorget_reactor_init);

    GorgetReactorWaiter waiter;
    pthread_mutex_init(&waiter.mtx, NULL);
    pthread_cond_init(&waiter.cond, NULL);
    waiter.woken = 0;

#ifdef __linux__
    int fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    if (fd >= 0) {
        struct itimerspec ts = {{0,0},{0,0}};
        ts.it_value.tv_sec  = (time_t)(ms / 1000);
        ts.it_value.tv_nsec = (long)((ms % 1000) * 1000000LL);
        timerfd_settime(fd, 0, &ts, NULL);

        struct epoll_event ev;
        ev.events  = EPOLLIN | EPOLLET;
        ev.data.fd = fd;
        epoll_ctl(__gorget_reactor.event_fd, EPOLL_CTL_ADD, fd, &ev);
        waiter.fd = fd;

        /* register */
        pthread_mutex_lock(&__gorget_reactor.mtx);
        if (__gorget_reactor.len == __gorget_reactor.cap) {
            __gorget_reactor.cap *= 2;
            __gorget_reactor.entries = (GorgetReactorEntry*)GORGET_REALLOC(
                __gorget_reactor.entries,
                (__gorget_reactor.cap / 2) * sizeof(GorgetReactorEntry),
                __gorget_reactor.cap * sizeof(GorgetReactorEntry));
        }
        __gorget_reactor.entries[__gorget_reactor.len].fd = fd;
        __gorget_reactor.entries[__gorget_reactor.len].w  = &waiter;
        __gorget_reactor.entries[__gorget_reactor.len].is_async = 0;
        __gorget_reactor.entries[__gorget_reactor.len].kind = GORGET_REACTOR_TIMER;
        __gorget_reactor.len++;
        pthread_mutex_unlock(&__gorget_reactor.mtx);

        /* block until woken */
        pthread_mutex_lock(&waiter.mtx);
        while (!waiter.woken) pthread_cond_wait(&waiter.cond, &waiter.mtx);
        pthread_mutex_unlock(&waiter.mtx);

        epoll_ctl(__gorget_reactor.event_fd, EPOLL_CTL_DEL, fd, NULL);
        close(fd);
        goto reactor_sleep_cleanup;
    }
#elif defined(__APPLE__) || defined(__FreeBSD__)
    if (__gorget_reactor.event_fd >= 0) {
        /* Use kqueue EVFILT_TIMER (ms unit) */
        static volatile int __reactor_timer_id = 0;
        int ident = __sync_fetch_and_add(&__reactor_timer_id, 1) + 100;
        struct kevent change;
        EV_SET(&change, (uintptr_t)ident, EVFILT_TIMER, EV_ADD | EV_ONESHOT, NOTE_USECONDS, ms * 1000, NULL);
        kevent(__gorget_reactor.event_fd, &change, 1, NULL, 0, NULL);
        waiter.fd = ident;

        pthread_mutex_lock(&__gorget_reactor.mtx);
        if (__gorget_reactor.len == __gorget_reactor.cap) {
            __gorget_reactor.cap *= 2;
            __gorget_reactor.entries = (GorgetReactorEntry*)GORGET_REALLOC(
                __gorget_reactor.entries,
                (__gorget_reactor.cap / 2) * sizeof(GorgetReactorEntry),
                __gorget_reactor.cap * sizeof(GorgetReactorEntry));
        }
        __gorget_reactor.entries[__gorget_reactor.len].fd = ident;
        __gorget_reactor.entries[__gorget_reactor.len].w  = &waiter;
        __gorget_reactor.entries[__gorget_reactor.len].is_async = 0;
        __gorget_reactor.entries[__gorget_reactor.len].kind = GORGET_REACTOR_TIMER;
        __gorget_reactor.len++;
        pthread_mutex_unlock(&__gorget_reactor.mtx);

        pthread_mutex_lock(&waiter.mtx);
        while (!waiter.woken) pthread_cond_wait(&waiter.cond, &waiter.mtx);
        pthread_mutex_unlock(&waiter.mtx);
        goto reactor_sleep_cleanup;
    }
#endif
    /* Fallback: nanosleep */
    {
        struct timespec ts;
        ts.tv_sec  = (time_t)(ms / 1000);
        ts.tv_nsec = (long)((ms % 1000) * 1000000LL);
        while (nanosleep(&ts, &ts) == -1 && errno == EINTR) {}
    }

reactor_sleep_cleanup:
    pthread_mutex_destroy(&waiter.mtx);
    pthread_cond_destroy(&waiter.cond);
}

/* Convenience wrapper: sleep for `seconds` (double) via the reactor. */
static void gorget_reactor_sleep_seconds(double seconds) {
    gorget_reactor_sleep_ms((int64_t)(seconds * 1000.0));
}

/* Non-blocking async sleep: register timer + fire waker when it expires.
 * Used by coroutine poll functions instead of the blocking gorget_reactor_sleep_ms. */
static void gorget_reactor_sleep_async(int64_t ms, GorgetWaker waker) {
    if (ms <= 0) { waker.wake(&waker); return; }
    pthread_once(&__gorget_reactor_once, __gorget_reactor_init);

#ifdef __linux__
    int fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    if (fd >= 0) {
        struct itimerspec ts = {{0,0},{0,0}};
        ts.it_value.tv_sec  = (time_t)(ms / 1000);
        ts.it_value.tv_nsec = (long)((ms % 1000) * 1000000LL);
        timerfd_settime(fd, 0, &ts, NULL);

        struct epoll_event ev;
        ev.events  = EPOLLIN | EPOLLET;
        ev.data.fd = fd;
        epoll_ctl(__gorget_reactor.event_fd, EPOLL_CTL_ADD, fd, &ev);

        pthread_mutex_lock(&__gorget_reactor.mtx);
        if (__gorget_reactor.len == __gorget_reactor.cap) {
            __gorget_reactor.cap *= 2;
            __gorget_reactor.entries = (GorgetReactorEntry*)GORGET_REALLOC(
                __gorget_reactor.entries,
                (__gorget_reactor.cap / 2) * sizeof(GorgetReactorEntry),
                __gorget_reactor.cap * sizeof(GorgetReactorEntry));
        }
        GorgetReactorEntry* e = &__gorget_reactor.entries[__gorget_reactor.len++];
        e->fd = fd;
        e->w  = NULL;
        e->is_async = 1;
        e->kind = GORGET_REACTOR_TIMER;
        e->async_waker = waker;
        pthread_mutex_unlock(&__gorget_reactor.mtx);
        return;
    }
#elif defined(__APPLE__) || defined(__FreeBSD__)
    if (__gorget_reactor.event_fd >= 0) {
        static volatile int __reactor_async_timer_id = 1000000;
        int ident = __sync_fetch_and_add(&__reactor_async_timer_id, 1);
        struct kevent change;
        EV_SET(&change, (uintptr_t)ident, EVFILT_TIMER, EV_ADD | EV_ONESHOT, NOTE_USECONDS, ms * 1000, NULL);
        kevent(__gorget_reactor.event_fd, &change, 1, NULL, 0, NULL);

        pthread_mutex_lock(&__gorget_reactor.mtx);
        if (__gorget_reactor.len == __gorget_reactor.cap) {
            __gorget_reactor.cap *= 2;
            __gorget_reactor.entries = (GorgetReactorEntry*)GORGET_REALLOC(
                __gorget_reactor.entries,
                (__gorget_reactor.cap / 2) * sizeof(GorgetReactorEntry),
                __gorget_reactor.cap * sizeof(GorgetReactorEntry));
        }
        GorgetReactorEntry* e = &__gorget_reactor.entries[__gorget_reactor.len++];
        e->fd = ident;
        e->w  = NULL;
        e->is_async = 1;
        e->kind = GORGET_REACTOR_TIMER;
        e->async_waker = waker;
        pthread_mutex_unlock(&__gorget_reactor.mtx);
        return;
    }
#endif
    /* Fallback: fire waker immediately (timer not available). */
    waker.wake(&waker);
}

/* ── Reactor fd-readiness registration ──────────────────────── */

/* Helper: grow entries array if needed (must hold r->mtx). */
static void __gorget_reactor_grow_if_needed(GorgetReactor* r) {
    if (r->len == r->cap) {
        r->cap *= 2;
        r->entries = (GorgetReactorEntry*)GORGET_REALLOC(
            r->entries,
            (r->cap / 2) * sizeof(GorgetReactorEntry),
            r->cap * sizeof(GorgetReactorEntry));
    }
}

/* Register interest in fd becoming readable.  Fires waker exactly once.
 * The fd is NOT owned by the reactor — caller retains ownership. */
static void gorget_reactor_wait_readable(int fd, GorgetWaker waker) {
    pthread_once(&__gorget_reactor_once, __gorget_reactor_init);
    GorgetReactor* r = &__gorget_reactor;

#ifdef __linux__
    struct epoll_event ev;
    ev.events  = EPOLLIN | EPOLLONESHOT;
    ev.data.fd = fd;
    /* Try MOD first (fd may already be registered from a previous wait) */
    if (epoll_ctl(r->event_fd, EPOLL_CTL_MOD, fd, &ev) != 0) {
        epoll_ctl(r->event_fd, EPOLL_CTL_ADD, fd, &ev);
    }
#elif defined(__APPLE__) || defined(__FreeBSD__)
    struct kevent change;
    EV_SET(&change, (uintptr_t)fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
    kevent(r->event_fd, &change, 1, NULL, 0, NULL);
#endif

    pthread_mutex_lock(&r->mtx);
    __gorget_reactor_grow_if_needed(r);
    GorgetReactorEntry* e = &r->entries[r->len++];
    e->fd          = fd;
    e->w           = NULL;
    e->is_async    = 1;
    e->kind        = GORGET_REACTOR_READ;
    e->async_waker = waker;
    pthread_mutex_unlock(&r->mtx);
}

/* Register interest in fd becoming writable.  Fires waker exactly once. */
static void gorget_reactor_wait_writable(int fd, GorgetWaker waker) {
    pthread_once(&__gorget_reactor_once, __gorget_reactor_init);
    GorgetReactor* r = &__gorget_reactor;

#ifdef __linux__
    struct epoll_event ev;
    ev.events  = EPOLLOUT | EPOLLONESHOT;
    ev.data.fd = fd;
    if (epoll_ctl(r->event_fd, EPOLL_CTL_MOD, fd, &ev) != 0) {
        epoll_ctl(r->event_fd, EPOLL_CTL_ADD, fd, &ev);
    }
#elif defined(__APPLE__) || defined(__FreeBSD__)
    struct kevent change;
    EV_SET(&change, (uintptr_t)fd, EVFILT_WRITE, EV_ADD | EV_ONESHOT, 0, 0, NULL);
    kevent(r->event_fd, &change, 1, NULL, 0, NULL);
#endif

    pthread_mutex_lock(&r->mtx);
    __gorget_reactor_grow_if_needed(r);
    GorgetReactorEntry* e = &r->entries[r->len++];
    e->fd          = fd;
    e->w           = NULL;
    e->is_async    = 1;
    e->kind        = GORGET_REACTOR_WRITE;
    e->async_waker = waker;
    pthread_mutex_unlock(&r->mtx);
}

/* Cancel all pending reactor entries for a given fd (e.g., on socket close). */
static void gorget_reactor_cancel_fd(int fd) {
    GorgetReactor* r = &__gorget_reactor;
    pthread_mutex_lock(&r->mtx);
    for (size_t j = 0; j < r->len; ) {
        if (r->entries[j].fd == fd) {
            r->entries[j] = r->entries[--r->len];
        } else {
            j++;
        }
    }
    pthread_mutex_unlock(&r->mtx);
#ifdef __linux__
    epoll_ctl(r->event_fd, EPOLL_CTL_DEL, fd, NULL);
#elif defined(__APPLE__) || defined(__FreeBSD__)
    struct kevent changes[2];
    EV_SET(&changes[0], (uintptr_t)fd, EVFILT_READ,  EV_DELETE, 0, 0, NULL);
    EV_SET(&changes[1], (uintptr_t)fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
    kevent(r->event_fd, changes, 2, NULL, 0, NULL); /* ignore errors — filter may not exist */
#endif
}
