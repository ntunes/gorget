
// ── Mutex[T] + Guard[T] ──
#ifndef GORGET_PTHREAD_INCLUDED
#define GORGET_PTHREAD_INCLUDED
#include <pthread.h>
#endif
typedef struct GorgetMutex {
    pthread_mutex_t lock;
    void*           data;
    size_t          data_size;
    // Waiter queue for async poll-lock (waker-based notification)
    pthread_mutex_t  wait_mtx;
    GorgetWaker*     waiters;
    int              waiter_count;
    int              waiter_cap;
} GorgetMutex;

typedef struct {
    GorgetMutex*    mutex;
    void*           ptr;
} gorget_guard_t;

static inline GorgetMutex* gorget_mutex_new(size_t size, void* init_data) {
    GorgetMutex* m = (GorgetMutex*)GORGET_ALLOC(sizeof(GorgetMutex));
    pthread_mutex_init(&m->lock, NULL);
    m->data = GORGET_ALLOC(size);
    m->data_size = size;
    memcpy(m->data, init_data, size);
    pthread_mutex_init(&m->wait_mtx, NULL);
    m->waiters = NULL;
    m->waiter_count = 0;
    m->waiter_cap = 0;
    return m;
}

// Free the mutex and its data. Does NOT free the guard — caller must release first.
static inline void gorget_mutex_free(GorgetMutex* m) {
    if (!m) return;
    pthread_mutex_destroy(&m->lock);
    pthread_mutex_destroy(&m->wait_mtx);
    if (m->waiters) GORGET_FREE(m->waiters, (size_t)m->waiter_cap * sizeof(GorgetWaker));
    if (m->data) GORGET_FREE(m->data, m->data_size);
    GORGET_FREE(m, sizeof(GorgetMutex));
}

// Blocking lock — acquires the mutex and returns a guard.
// For synchronous (non-async) contexts. Uses pthread_mutex_lock directly.
static inline gorget_guard_t gorget_mutex_lock(GorgetMutex* m) {
    pthread_mutex_lock(&m->lock);
    gorget_guard_t g;
    g.mutex = m;
    g.ptr   = m->data;
    return g;
}

// Poll-based lock for async contexts. Returns GORGET_POLL_READY (0) if the lock
// was acquired (guard filled in), or GORGET_POLL_PENDING (1) if contended (waker
// registered for notification when the lock becomes available).
//
// When a guard is released, one waiting waker is woken in FIFO order.
// The woken task should re-call gorget_mutex_poll_lock to attempt acquisition.
static inline int gorget_mutex_poll_lock(GorgetMutex* m, gorget_guard_t* out, GorgetWaker* waker) {
    int r = pthread_mutex_trylock(&m->lock);
    if (r == 0) {
        // Lock acquired — fill guard
        out->mutex = m;
        out->ptr   = m->data;
        return GORGET_POLL_READY;
    }
    // Lock contended — register waker for notification
    if (waker) {
        pthread_mutex_lock(&m->wait_mtx);
        if (m->waiter_count == m->waiter_cap) {
            int old_cap = m->waiter_cap;
            m->waiter_cap = old_cap ? old_cap * 2 : 4;
            m->waiters = (GorgetWaker*)GORGET_REALLOC(m->waiters,
                (size_t)old_cap * sizeof(GorgetWaker),
                (size_t)m->waiter_cap * sizeof(GorgetWaker));
        }
        m->waiters[m->waiter_count++] = *waker;
        pthread_mutex_unlock(&m->wait_mtx);
    }
    return GORGET_POLL_PENDING;
}

// Release the guard (unlock the mutex). Safe to call on a zeroed guard.
// Wakes one async waiter (if any) after unlocking.
static inline void gorget_guard_release(gorget_guard_t* g) {
    if (!g->mutex) return;
    GorgetMutex* m = g->mutex;
    pthread_mutex_unlock(&m->lock);
    // Wake one async waiter (FIFO order)
    pthread_mutex_lock(&m->wait_mtx);
    if (m->waiter_count > 0) {
        GorgetWaker w = m->waiters[0];
        memmove(m->waiters, m->waiters + 1, (size_t)(--m->waiter_count) * sizeof(GorgetWaker));
        pthread_mutex_unlock(&m->wait_mtx);
        w.wake(&w);
    } else {
        pthread_mutex_unlock(&m->wait_mtx);
    }
    g->mutex = NULL;
    g->ptr   = NULL;
}

// CondVar.wait(guard) bridge — defined here because it requires gorget_guard_t.
// Uses void* for the CondVar so this compiles even when SYNC_RUNTIME is absent.
// GorgetCondVar is { pthread_cond_t cond; }, so the pointer IS a pthread_cond_t*.
static inline void gorget_condvar_wait_guard(void* cv_opaque, gorget_guard_t* g) {
    pthread_cond_t* cond = (pthread_cond_t*)cv_opaque;
    pthread_cond_wait(cond, &g->mutex->lock);
}

// Guard helpers for LIR backend — generic void*-based accessors.
// gorget_guard_get returns a pointer to the guarded data (caller dereferences to T).
static inline void* gorget_guard_get(gorget_guard_t* g) { return g->ptr; }
// gorget_guard_set copies `size` bytes from `val` into the guarded data.
static inline void gorget_guard_set(gorget_guard_t* g, void* val, size_t size) { memcpy(g->ptr, val, size); }
// gorget_guard_get_ptr returns a mutable pointer to the guarded data.
static inline void* gorget_guard_get_ptr(gorget_guard_t* g) { return g->ptr; }

// gorget_mutex_lock_to: output-pointer variant for LIR backend (struct-return via pointer).
static inline void gorget_mutex_lock_to(GorgetMutex* m, gorget_guard_t* out) {
    *out = gorget_mutex_lock(m);
}
