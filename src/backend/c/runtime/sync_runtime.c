
// ── std.sync runtime ──────────────────────────────────────────
#include <pthread.h>

// ── AtomicInt ──
typedef struct { volatile int64_t __val; } GorgetAtomicInt;

static inline GorgetAtomicInt* gorget_atomic_int_new(int64_t v) {
    GorgetAtomicInt* a = (GorgetAtomicInt*)GORGET_ALLOC(sizeof(GorgetAtomicInt));
    __atomic_store_n(&a->__val, v, __ATOMIC_SEQ_CST);
    return a;
}
static inline int64_t gorget_atomic_int_load(GorgetAtomicInt* a) {
    return __atomic_load_n(&a->__val, __ATOMIC_SEQ_CST);
}
static inline void gorget_atomic_int_store(GorgetAtomicInt* a, int64_t v) {
    __atomic_store_n(&a->__val, v, __ATOMIC_SEQ_CST);
}
static inline int64_t gorget_atomic_int_add(GorgetAtomicInt* a, int64_t v) {
    return __atomic_fetch_add(&a->__val, v, __ATOMIC_SEQ_CST);
}
static inline int64_t gorget_atomic_int_sub(GorgetAtomicInt* a, int64_t v) {
    return __atomic_fetch_sub(&a->__val, v, __ATOMIC_SEQ_CST);
}
static inline int gorget_atomic_int_compare_exchange(GorgetAtomicInt* a, int64_t expected, int64_t desired) {
    return __atomic_compare_exchange_n(&a->__val, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
static inline void gorget_atomic_int_free(GorgetAtomicInt** ap) {
    GorgetAtomicInt* a = *ap;
    if (a) GORGET_FREE(a, sizeof(GorgetAtomicInt));
}

// ── AtomicBool ──
typedef struct { volatile int __val; } GorgetAtomicBool;

static inline GorgetAtomicBool* gorget_atomic_bool_new(int v) {
    GorgetAtomicBool* a = (GorgetAtomicBool*)GORGET_ALLOC(sizeof(GorgetAtomicBool));
    __atomic_store_n(&a->__val, v ? 1 : 0, __ATOMIC_SEQ_CST);
    return a;
}
static inline int gorget_atomic_bool_load(GorgetAtomicBool* a) {
    return __atomic_load_n(&a->__val, __ATOMIC_SEQ_CST);
}
static inline void gorget_atomic_bool_store(GorgetAtomicBool* a, int v) {
    __atomic_store_n(&a->__val, v ? 1 : 0, __ATOMIC_SEQ_CST);
}
static inline int gorget_atomic_bool_swap(GorgetAtomicBool* a, int v) {
    return __atomic_exchange_n(&a->__val, v ? 1 : 0, __ATOMIC_SEQ_CST);
}
static inline int gorget_atomic_bool_compare_exchange(GorgetAtomicBool* a, int expected, int desired) {
    int e = expected ? 1 : 0;
    return __atomic_compare_exchange_n(&a->__val, &e, desired ? 1 : 0, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
static inline void gorget_atomic_bool_free(GorgetAtomicBool** ap) {
    GorgetAtomicBool* a = *ap;
    if (a) GORGET_FREE(a, sizeof(GorgetAtomicBool));
}

// ── Barrier ──
// Portable implementation using mutex+condvar+generation counter.
typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t  cond;
    int             count;       // total threads required
    int             arrived;     // arrived this generation
    int             generation;  // increments each cycle
} GorgetBarrier;

static inline GorgetBarrier* gorget_barrier_new(int64_t n) {
    GorgetBarrier* b = (GorgetBarrier*)GORGET_CALLOC(1, sizeof(GorgetBarrier));
    pthread_mutex_init(&b->mtx, NULL);
    pthread_cond_init(&b->cond, NULL);
    b->count = (int)n;
    return b;
}
static inline void gorget_barrier_wait(GorgetBarrier* b) {
    int gen;
    pthread_mutex_lock(&b->mtx);
    gen = b->generation;
    b->arrived++;
    if (b->arrived == b->count) {
        b->arrived = 0;
        b->generation++;
        pthread_cond_broadcast(&b->cond);
    } else {
        while (gen == b->generation)
            pthread_cond_wait(&b->cond, &b->mtx);
    }
    pthread_mutex_unlock(&b->mtx);
}

// ── RWLock[T] + ReadGuard[T] + WriteGuard[T] ──
// Forward-declare GorgetWaker and poll constants for async poll-lock support.
// Full definitions are in ASYNC_RUNTIME; these allow compilation regardless of emission order.
#ifndef GORGET_WAKER_DEFINED
#define GORGET_WAKER_DEFINED
typedef struct GorgetWaker { void (*wake)(struct GorgetWaker*); void* data; } GorgetWaker;
#endif
#ifndef GORGET_POLL_READY
#define GORGET_POLL_READY   0
#define GORGET_POLL_PENDING 1
#endif

typedef struct GorgetRWLock {
    pthread_rwlock_t lock;
    void*            data;
    size_t           data_size;
    // Waiter queue for async poll-lock (waker-based notification)
    pthread_mutex_t  wait_mtx;
    GorgetWaker*     waiters;
    int              waiter_count;
    int              waiter_cap;
} GorgetRWLock;

typedef struct { GorgetRWLock* rwlock; void* ptr; } gorget_read_guard_t;
typedef struct { GorgetRWLock* rwlock; void* ptr; } gorget_write_guard_t;

static inline GorgetRWLock* gorget_rwlock_new(size_t size, void* init_data) {
    GorgetRWLock* rw = (GorgetRWLock*)GORGET_ALLOC(sizeof(GorgetRWLock));
    pthread_rwlock_init(&rw->lock, NULL);
    rw->data = GORGET_ALLOC(size);
    rw->data_size = size;
    memcpy(rw->data, init_data, size);
    pthread_mutex_init(&rw->wait_mtx, NULL);
    rw->waiters = NULL;
    rw->waiter_count = 0;
    rw->waiter_cap = 0;
    return rw;
}
// Free the rwlock and its data. Does NOT free the guard — caller must release first.
static inline void gorget_rwlock_free(GorgetRWLock* rw) {
    if (!rw) return;
    pthread_rwlock_destroy(&rw->lock);
    pthread_mutex_destroy(&rw->wait_mtx);
    if (rw->waiters) GORGET_FREE(rw->waiters, (size_t)rw->waiter_cap * sizeof(GorgetWaker));
    if (rw->data) GORGET_FREE(rw->data, rw->data_size);
    GORGET_FREE(rw, sizeof(GorgetRWLock));
}
// Register a waker on the rwlock's wait queue.
static inline void gorget_rwlock_register_waiter(GorgetRWLock* rw, GorgetWaker* waker) {
    if (!waker) return;
    pthread_mutex_lock(&rw->wait_mtx);
    if (rw->waiter_count == rw->waiter_cap) {
        int old_cap = rw->waiter_cap;
        rw->waiter_cap = old_cap ? old_cap * 2 : 4;
        rw->waiters = (GorgetWaker*)GORGET_REALLOC(rw->waiters,
            (size_t)old_cap * sizeof(GorgetWaker),
            (size_t)rw->waiter_cap * sizeof(GorgetWaker));
    }
    rw->waiters[rw->waiter_count++] = *waker;
    pthread_mutex_unlock(&rw->wait_mtx);
}
// Wake all waiters (both readers and writers may become unblocked).
static inline void gorget_rwlock_wake_waiters(GorgetRWLock* rw) {
    pthread_mutex_lock(&rw->wait_mtx);
    int n = rw->waiter_count;
    GorgetWaker* ws = NULL;
    if (n > 0) {
        ws = (GorgetWaker*)GORGET_ALLOC((size_t)n * sizeof(GorgetWaker));
        memcpy(ws, rw->waiters, (size_t)n * sizeof(GorgetWaker));
        rw->waiter_count = 0;
    }
    pthread_mutex_unlock(&rw->wait_mtx);
    for (int i = 0; i < n; i++) ws[i].wake(&ws[i]);
    if (ws) GORGET_FREE(ws, (size_t)n * sizeof(GorgetWaker));
}
static inline gorget_read_guard_t gorget_rwlock_read(GorgetRWLock* rw) {
    pthread_rwlock_rdlock(&rw->lock);
    gorget_read_guard_t g;
    g.rwlock = rw;
    g.ptr    = rw->data;
    return g;
}
static inline gorget_write_guard_t gorget_rwlock_write(GorgetRWLock* rw) {
    pthread_rwlock_wrlock(&rw->lock);
    gorget_write_guard_t g;
    g.rwlock = rw;
    g.ptr    = rw->data;
    return g;
}
// Poll-based read lock for async contexts.
static inline int gorget_rwlock_poll_read(GorgetRWLock* rw, gorget_read_guard_t* out, GorgetWaker* waker) {
    int r = pthread_rwlock_tryrdlock(&rw->lock);
    if (r == 0) {
        out->rwlock = rw;
        out->ptr    = rw->data;
        return GORGET_POLL_READY;
    }
    gorget_rwlock_register_waiter(rw, waker);
    return GORGET_POLL_PENDING;
}
// Poll-based write lock for async contexts.
static inline int gorget_rwlock_poll_write(GorgetRWLock* rw, gorget_write_guard_t* out, GorgetWaker* waker) {
    int r = pthread_rwlock_trywrlock(&rw->lock);
    if (r == 0) {
        out->rwlock = rw;
        out->ptr    = rw->data;
        return GORGET_POLL_READY;
    }
    gorget_rwlock_register_waiter(rw, waker);
    return GORGET_POLL_PENDING;
}
static inline void gorget_read_guard_release(gorget_read_guard_t* g) {
    if (!g->rwlock) return;
    GorgetRWLock* rw = g->rwlock;
    pthread_rwlock_unlock(&rw->lock);
    g->rwlock = NULL; g->ptr = NULL;
    gorget_rwlock_wake_waiters(rw);
}
static inline void gorget_write_guard_release(gorget_write_guard_t* g) {
    if (!g->rwlock) return;
    GorgetRWLock* rw = g->rwlock;
    pthread_rwlock_unlock(&rw->lock);
    g->rwlock = NULL; g->ptr = NULL;
    gorget_rwlock_wake_waiters(rw);
}

// ReadGuard / WriteGuard helpers for LIR backend — generic void*-based accessors.
static inline void* gorget_read_guard_get(gorget_read_guard_t* g) { return g->ptr; }
static inline void* gorget_read_guard_get_ptr(gorget_read_guard_t* g) { return g->ptr; }
static inline void* gorget_write_guard_get(gorget_write_guard_t* g) { return g->ptr; }
static inline void gorget_write_guard_set(gorget_write_guard_t* g, void* val, size_t size) { memcpy(g->ptr, val, size); }
static inline void* gorget_write_guard_get_ptr(gorget_write_guard_t* g) { return g->ptr; }

// gorget_rwlock_read_to / gorget_rwlock_write_to: output-pointer variants for LIR backend.
static inline void gorget_rwlock_read_to(GorgetRWLock* rw, gorget_read_guard_t* out) {
    *out = gorget_rwlock_read(rw);
}
static inline void gorget_rwlock_write_to(GorgetRWLock* rw, gorget_write_guard_t* out) {
    *out = gorget_rwlock_write(rw);
}

// ── CondVar ──
typedef struct { pthread_cond_t cond; } GorgetCondVar;

static inline GorgetCondVar* gorget_condvar_new(void) {
    GorgetCondVar* cv = (GorgetCondVar*)GORGET_CALLOC(1, sizeof(GorgetCondVar));
    pthread_cond_init(&cv->cond, NULL);
    return cv;
}
static inline void gorget_condvar_notify_one(GorgetCondVar* cv) {
    pthread_cond_signal(&cv->cond);
}
static inline void gorget_condvar_notify_all(GorgetCondVar* cv) {
    pthread_cond_broadcast(&cv->cond);
}

// ── WaitGroup ──
typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t  cond;
    int64_t         count;
} GorgetWaitGroup;

static inline GorgetWaitGroup* gorget_waitgroup_new(void) {
    GorgetWaitGroup* wg = (GorgetWaitGroup*)GORGET_CALLOC(1, sizeof(GorgetWaitGroup));
    pthread_mutex_init(&wg->mtx, NULL);
    pthread_cond_init(&wg->cond, NULL);
    return wg;
}
static inline void gorget_waitgroup_add(GorgetWaitGroup* wg, int64_t n) {
    pthread_mutex_lock(&wg->mtx);
    wg->count += n;
    pthread_mutex_unlock(&wg->mtx);
}
static inline void gorget_waitgroup_done(GorgetWaitGroup* wg) {
    pthread_mutex_lock(&wg->mtx);
    wg->count--;
    if (wg->count <= 0) pthread_cond_broadcast(&wg->cond);
    pthread_mutex_unlock(&wg->mtx);
}
static inline void gorget_waitgroup_wait(GorgetWaitGroup* wg) {
    pthread_mutex_lock(&wg->mtx);
    while (wg->count > 0) pthread_cond_wait(&wg->cond, &wg->mtx);
    pthread_mutex_unlock(&wg->mtx);
}
static inline void gorget_waitgroup_free(GorgetWaitGroup** wgp) {
    GorgetWaitGroup* wg = *wgp;
    if (!wg) return;
    pthread_mutex_destroy(&wg->mtx);
    pthread_cond_destroy(&wg->cond);
    GORGET_FREE(wg, sizeof(GorgetWaitGroup));
    *wgp = NULL;
}

// ── Semaphore ──
typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t  cond;
    int64_t         permits;
} GorgetSemaphore;

static inline GorgetSemaphore* gorget_semaphore_new(int64_t n) {
    GorgetSemaphore* s = (GorgetSemaphore*)GORGET_CALLOC(1, sizeof(GorgetSemaphore));
    pthread_mutex_init(&s->mtx, NULL);
    pthread_cond_init(&s->cond, NULL);
    s->permits = n;
    return s;
}
static inline void gorget_semaphore_acquire(GorgetSemaphore* s) {
    pthread_mutex_lock(&s->mtx);
    while (s->permits <= 0) pthread_cond_wait(&s->cond, &s->mtx);
    s->permits--;
    pthread_mutex_unlock(&s->mtx);
}
static inline void gorget_semaphore_release(GorgetSemaphore* s) {
    pthread_mutex_lock(&s->mtx);
    s->permits++;
    pthread_cond_signal(&s->cond);
    pthread_mutex_unlock(&s->mtx);
}
static inline int gorget_semaphore_try_acquire(GorgetSemaphore* s) {
    pthread_mutex_lock(&s->mtx);
    if (s->permits > 0) { s->permits--; pthread_mutex_unlock(&s->mtx); return 1; }
    pthread_mutex_unlock(&s->mtx);
    return 0;
}
static inline void gorget_semaphore_free(GorgetSemaphore** sp) {
    GorgetSemaphore* s = *sp;
    if (!s) return;
    pthread_mutex_destroy(&s->mtx);
    pthread_cond_destroy(&s->cond);
    GORGET_FREE(s, sizeof(GorgetSemaphore));
    *sp = NULL;
}

// ── OnceFlag ──
// Exactly-once initialization primitive.
// do_once() returns 1 for exactly one caller (the "winner"), 0 for all others.
// is_done() returns 1 after the winner has called do_once().
typedef struct {
    volatile int state; // 0=init, 1=done
} GorgetOnceFlag;

static inline GorgetOnceFlag* gorget_onceflag_new(void) {
    GorgetOnceFlag* f = (GorgetOnceFlag*)GORGET_CALLOC(1, sizeof(GorgetOnceFlag));
    f->state = 0;
    return f;
}
static inline int gorget_onceflag_do_once(GorgetOnceFlag* f) {
    int expected = 0;
    return __atomic_compare_exchange_n(&f->state, &expected, 1,
                                       0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE);
}
static inline int gorget_onceflag_is_done(GorgetOnceFlag* f) {
    return __atomic_load_n(&f->state, __ATOMIC_ACQUIRE);
}
