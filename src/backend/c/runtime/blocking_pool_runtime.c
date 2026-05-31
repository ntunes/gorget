
// ── Blocking Thread Pool ──
#ifndef GORGET_BLOCKING_POOL_MAX
#define GORGET_BLOCKING_POOL_MAX 256
#endif
#define __GORGET_BLOCKING_IDLE_SEC 10

typedef struct {
    void (*fn_ptr)(void*);
    void* arg;
    GorgetWaker waker;
} __GorgetBlockingWork;

typedef struct {
    __GorgetBlockingWork* queue;
    int queue_len, queue_cap;
    int thread_count;
    int idle_count;
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int shutdown;
} GorgetBlockingPool;

static GorgetBlockingPool __gorget_bpool;
static int __gorget_bpool_init_done = 0;
static void* __gorget_blocking_worker(void* arg);

static void __gorget_blocking_pool_init(void) {
    if (__gorget_bpool_init_done) return;
    __gorget_bpool_init_done = 1;
    __gorget_bpool.queue_cap = 16;
    __gorget_bpool.queue = (__GorgetBlockingWork*)GORGET_ALLOC(
        16 * sizeof(__GorgetBlockingWork));
    __gorget_bpool.queue_len = 0;
    __gorget_bpool.thread_count = 0;
    __gorget_bpool.idle_count = 0;
    __gorget_bpool.shutdown = 0;
    pthread_mutex_init(&__gorget_bpool.mtx, NULL);
    pthread_cond_init(&__gorget_bpool.cond, NULL);
}

static void __gorget_blocking_submit(void (*fn_ptr)(void*), void* arg, GorgetWaker waker) {
    __gorget_blocking_pool_init();
    pthread_mutex_lock(&__gorget_bpool.mtx);
    /* Enqueue work item */
    if (__gorget_bpool.queue_len == __gorget_bpool.queue_cap) {
        int old_cap = __gorget_bpool.queue_cap;
        __gorget_bpool.queue_cap *= 2;
        __gorget_bpool.queue = (__GorgetBlockingWork*)GORGET_REALLOC(
            __gorget_bpool.queue,
            (size_t)old_cap * sizeof(__GorgetBlockingWork),
            (size_t)__gorget_bpool.queue_cap * sizeof(__GorgetBlockingWork));
    }
    __gorget_bpool.queue[__gorget_bpool.queue_len++] = (__GorgetBlockingWork){fn_ptr, arg, waker};
    /* Spawn a new thread if no idle workers and below cap */
    if (__gorget_bpool.idle_count == 0 &&
        __gorget_bpool.thread_count < GORGET_BLOCKING_POOL_MAX) {
        __gorget_bpool.thread_count++;
        pthread_t th;
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&th, &attr, __gorget_blocking_worker, NULL);
        pthread_attr_destroy(&attr);
    }
    pthread_cond_signal(&__gorget_bpool.cond);
    pthread_mutex_unlock(&__gorget_bpool.mtx);
}

static void* __gorget_blocking_worker(void* arg) {
    (void)arg;
    for (;;) {
        pthread_mutex_lock(&__gorget_bpool.mtx);
        __gorget_bpool.idle_count++;

        /* Wait for work with timeout */
        struct timespec deadline;
        clock_gettime(CLOCK_REALTIME, &deadline);
        deadline.tv_sec += __GORGET_BLOCKING_IDLE_SEC;

        while (__gorget_bpool.queue_len == 0 && !__gorget_bpool.shutdown) {
            int rc = pthread_cond_timedwait(&__gorget_bpool.cond, &__gorget_bpool.mtx, &deadline);
            if (rc != 0) break; /* ETIMEDOUT or error */
        }
        if (__gorget_bpool.queue_len == 0) {
            /* Timed out or shutting down — exit this thread */
            __gorget_bpool.idle_count--;
            __gorget_bpool.thread_count--;
            pthread_mutex_unlock(&__gorget_bpool.mtx);
            return NULL;
        }
        __gorget_bpool.idle_count--;
        __GorgetBlockingWork work = __gorget_bpool.queue[0];
        memmove(__gorget_bpool.queue, __gorget_bpool.queue + 1,
                (size_t)(--__gorget_bpool.queue_len) * sizeof(__GorgetBlockingWork));
        pthread_mutex_unlock(&__gorget_bpool.mtx);

        /* Execute blocking work */
        work.fn_ptr(work.arg);
        /* Wake the parent coroutine (if any) */
        if (work.waker.wake) work.waker.wake(&work.waker);
    }
    return NULL;
}

static void __gorget_blocking_pool_shutdown(void) {
    if (!__gorget_bpool_init_done) return;
    pthread_mutex_lock(&__gorget_bpool.mtx);
    __gorget_bpool.shutdown = 1;
    pthread_cond_broadcast(&__gorget_bpool.cond);
    pthread_mutex_unlock(&__gorget_bpool.mtx);
    /* Detached threads will exit on their own — give them a moment. */
    struct timespec ts = {0, 50000000}; /* 50ms */
    nanosleep(&ts, NULL);
    GORGET_FREE(__gorget_bpool.queue,
        (size_t)__gorget_bpool.queue_cap * sizeof(__GorgetBlockingWork));
    pthread_mutex_destroy(&__gorget_bpool.mtx);
    pthread_cond_destroy(&__gorget_bpool.cond);
    __gorget_bpool_init_done = 0;
}

// Task-level adapter: submit a GorgetTask to the blocking pool.
// The task's run() is called on a blocking pool thread, then task.done is signaled.
// Parent waker (if any) is fired on completion for coroutine integration.
static void __gorget_blocking_task_run(void* arg) {
    GorgetTask* task = (GorgetTask*)arg;
    task->run(task);
    pthread_mutex_lock(&task->mtx);
    task->done = 1;
    pthread_cond_broadcast(&task->cond);
    GorgetWaker pw = task->parent_waker;
    pthread_mutex_unlock(&task->mtx);
    if (pw.wake) pw.wake(&pw);
}

static void __gorget_blocking_submit_task(GorgetTask* task) {
    __gorget_blocking_submit(
        __gorget_blocking_task_run,
        task,
        (GorgetWaker){NULL, NULL}
    );
}

#define GORGET_BLOCKING_SUBMIT(task) __gorget_blocking_submit_task(task)
