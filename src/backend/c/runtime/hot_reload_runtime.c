
// ── Hot Reload Runtime ──────────────────────────────────────────
#include <dlfcn.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <sys/event.h>
#include <fcntl.h>
#define GORGET_DYLIB_EXT ".dylib"
#else
#define GORGET_DYLIB_EXT ".so"
#endif

typedef struct {
    void* handle;
    void* init;
    void* tick;
    void* reload;
    uint64_t state_hash;
} GorgetGuestModule;

static GorgetGuestModule gorget_hot_load(const char* path) {
    GorgetGuestModule m = {0};
    m.handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!m.handle) {
        fprintf(stderr, "[hot-reload] dlopen failed: %s\n", dlerror());
        return m;
    }
    m.init = dlsym(m.handle, "gorget_guest_init");
    m.tick = dlsym(m.handle, "gorget_guest_tick");
    m.reload = dlsym(m.handle, "gorget_guest_reload");
    uint64_t* hash_ptr = (uint64_t*)dlsym(m.handle, "GORGET_STATE_HASH");
    m.state_hash = hash_ptr ? *hash_ptr : 0;
    return m;
}

static void gorget_hot_unload(GorgetGuestModule* m) {
    if (m->handle) {
        dlclose(m->handle);
        m->handle = NULL;
    }
}

// ── File watcher (kqueue on macOS) ──────────────────────────────
#ifdef __APPLE__

typedef struct {
    int kq;
    int* fds;
    int fd_count;
} GorgetFileWatcher;

static GorgetFileWatcher gorget_hot_watch_init(const char** paths, int count) {
    GorgetFileWatcher w = {0};
    w.kq = kqueue();
    if (w.kq < 0) {
        fprintf(stderr, "[hot-reload] kqueue() failed\n");
        return w;
    }
    w.fds = (int*)GORGET_ALLOC(sizeof(int) * count);
    w.fd_count = 0;
    for (int i = 0; i < count; i++) {
        int fd = open(paths[i], O_EVTONLY);
        if (fd < 0) {
            fprintf(stderr, "[hot-reload] Cannot watch '%s'\n", paths[i]);
            continue;
        }
        struct kevent ev;
        EV_SET(&ev, fd, EVFILT_VNODE, EV_ADD | EV_CLEAR,
               NOTE_WRITE | NOTE_RENAME | NOTE_DELETE, 0, NULL);
        kevent(w.kq, &ev, 1, NULL, 0, NULL);
        w.fds[w.fd_count++] = fd;
    }
    return w;
}

static bool gorget_hot_watch_check(GorgetFileWatcher* w) {
    struct kevent ev;
    struct timespec ts = {0, 0};  // non-blocking
    int n = kevent(w->kq, NULL, 0, &ev, 1, &ts);
    return n > 0;
}

static void gorget_hot_watch_close(GorgetFileWatcher* w) {
    for (int i = 0; i < w->fd_count; i++) {
        close(w->fds[i]);
    }
    GORGET_FREE(w->fds, 0);
    close(w->kq);
}

#else
// Linux — inotify-based watcher
#include <sys/inotify.h>
#include <unistd.h>
#include <poll.h>

typedef struct {
    int ifd;       // inotify file descriptor
    int* wds;      // array of watch descriptors
    int wd_count;  // number of watches
} GorgetFileWatcher;

static GorgetFileWatcher gorget_hot_watch_init(const char** paths, int count) {
    GorgetFileWatcher w = {0};
    w.ifd = inotify_init1(IN_NONBLOCK);
    if (w.ifd < 0) {
        fprintf(stderr, "[hot-reload] inotify_init1() failed\n");
        return w;
    }
    w.wds = (int*)GORGET_ALLOC(sizeof(int) * count);
    w.wd_count = 0;
    for (int i = 0; i < count; i++) {
        int wd = inotify_add_watch(w.ifd, paths[i],
                                   IN_MODIFY | IN_MOVE_SELF | IN_DELETE_SELF);
        if (wd < 0) {
            fprintf(stderr, "[hot-reload] Cannot watch '%s'\n", paths[i]);
            continue;
        }
        w.wds[w.wd_count++] = wd;
    }
    return w;
}

static bool gorget_hot_watch_check(GorgetFileWatcher* w) {
    char buf[4096] __attribute__((aligned(__alignof__(struct inotify_event))));
    ssize_t len = read(w->ifd, buf, sizeof(buf));
    return len > 0;
}

static void gorget_hot_watch_close(GorgetFileWatcher* w) {
    for (int i = 0; i < w->wd_count; i++) {
        inotify_rm_watch(w->ifd, w->wds[i]);
    }
    GORGET_FREE(w->wds, 0);
    close(w->ifd);
}
#endif

