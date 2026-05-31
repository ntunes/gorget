
// ── Task Common ──
#include <pthread.h>

typedef struct GorgetTask {
    void (*run)(struct GorgetTask*);
    int  (*poll)(struct GorgetTask*); // stackless coroutine poll fn (NULL = non-coroutine)
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int done;
    // Coroutine scheduling state (for poll-based tasks only):
    //  0 = being polled by a worker right now
    //  1 = in the executor queue
    // -1 = returned POLL_PENDING, waiting for waker to fire
    volatile int scheduled;
    GorgetWaker parent_waker;
} GorgetTask;

#ifndef GORGET_POLL_READY
#define GORGET_POLL_READY   0
#define GORGET_POLL_PENDING 1
#endif
