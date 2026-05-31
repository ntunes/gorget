
// ── Scheduler: Pool (M:N thread pool + work-stealing) ──

typedef struct {
    pthread_t* threads;
    int thread_count;
    GorgetTask** queue;
    int queue_len, queue_cap;
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int shutdown;
} GorgetExecutor;

static GorgetExecutor __gorget_exec;
static int __gorget_exec_init_done = 0;
// Forward declarations for mutual recursion between worker, submit, and work-stealing.
static void* __gorget_worker(void* arg);
static void __gorget_run_task_inline(GorgetTask* task);
static int __gorget_try_run_one(void);

static void __gorget_executor_init(void) {
    if (__gorget_exec_init_done) return;
    __gorget_exec_init_done = 1;
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 1) n = 4;
    __gorget_exec.thread_count = (int)n;
    __gorget_exec.threads = (pthread_t*)GORGET_ALLOC((size_t)n * sizeof(pthread_t));
    __gorget_exec.queue_cap = 64;
    __gorget_exec.queue = (GorgetTask**)GORGET_ALLOC(64 * sizeof(GorgetTask*));
    pthread_mutex_init(&__gorget_exec.mtx, NULL);
    pthread_cond_init(&__gorget_exec.cond, NULL);
    for (int i = 0; i < (int)n; i++)
        pthread_create(&__gorget_exec.threads[i], NULL, __gorget_worker, NULL);
}

static void __gorget_executor_submit(GorgetTask* task) {
    __gorget_executor_init();
    // Mark as queued before entering the executor queue.
    if (task->poll != NULL) {
        pthread_mutex_lock(&task->mtx);
        task->scheduled = 1;
        pthread_mutex_unlock(&task->mtx);
    }
    pthread_mutex_lock(&__gorget_exec.mtx);
    if (__gorget_exec.queue_len == __gorget_exec.queue_cap) {
        __gorget_exec.queue_cap *= 2;
        __gorget_exec.queue = (GorgetTask**)GORGET_REALLOC(__gorget_exec.queue,
            (size_t)(__gorget_exec.queue_cap / 2) * sizeof(GorgetTask*),
            (size_t)__gorget_exec.queue_cap * sizeof(GorgetTask*));
    }
    __gorget_exec.queue[__gorget_exec.queue_len++] = task;
    pthread_cond_signal(&__gorget_exec.cond);
    pthread_mutex_unlock(&__gorget_exec.mtx);
}

static void* __gorget_worker(void* arg) {
    (void)arg;
    for (;;) {
        pthread_mutex_lock(&__gorget_exec.mtx);
        while (__gorget_exec.queue_len == 0 && !__gorget_exec.shutdown)
            pthread_cond_wait(&__gorget_exec.cond, &__gorget_exec.mtx);
        if (__gorget_exec.shutdown && __gorget_exec.queue_len == 0) {
            pthread_mutex_unlock(&__gorget_exec.mtx);
            return NULL;
        }
        GorgetTask* task = __gorget_exec.queue[0];
        memmove(__gorget_exec.queue, __gorget_exec.queue + 1,
                (size_t)(--__gorget_exec.queue_len) * sizeof(GorgetTask*));
        pthread_mutex_unlock(&__gorget_exec.mtx);
        __gorget_run_task_inline(task);
    }
}

// Execute a single task inline (used for work-stealing while waiting).
// Handles both poll-based coroutines and run-based tasks.
static void __gorget_run_task_inline(GorgetTask* task) {
    if (task->poll != NULL) {
        pthread_mutex_lock(&task->mtx);
        task->scheduled = 0;
        pthread_mutex_unlock(&task->mtx);

        int status = task->poll(task);

        if (status == GORGET_POLL_READY) {
            pthread_mutex_lock(&task->mtx);
            task->done = 1;
            pthread_cond_broadcast(&task->cond);
            GorgetWaker pw = task->parent_waker;
            pthread_mutex_unlock(&task->mtx);
            if (pw.wake) pw.wake(&pw);
        } else {
            pthread_mutex_lock(&task->mtx);
            int waker_fired = (task->scheduled == 1);
            if (!waker_fired) task->scheduled = -1;
            pthread_mutex_unlock(&task->mtx);
            if (waker_fired) __gorget_executor_submit(task);
        }
    } else {
        task->run(task);
        pthread_mutex_lock(&task->mtx);
        task->done = 1;
        pthread_cond_broadcast(&task->cond);
        GorgetWaker pw = task->parent_waker;
        pthread_mutex_unlock(&task->mtx);
        if (pw.wake) pw.wake(&pw);
    }
}

// Try to dequeue and run one task from the executor queue.
// Returns 1 if a task was run, 0 if the queue was empty.
static int __gorget_try_run_one(void) {
    pthread_mutex_lock(&__gorget_exec.mtx);
    if (__gorget_exec.queue_len == 0) {
        pthread_mutex_unlock(&__gorget_exec.mtx);
        return 0;
    }
    GorgetTask* task = __gorget_exec.queue[0];
    memmove(__gorget_exec.queue, __gorget_exec.queue + 1,
            (size_t)(--__gorget_exec.queue_len) * sizeof(GorgetTask*));
    pthread_mutex_unlock(&__gorget_exec.mtx);
    __gorget_run_task_inline(task);
    return 1;
}

// Try to dequeue and run one POLL-BASED task (non-blocking).
// Skips run-based tasks (which may block indefinitely) to avoid deadlocking
// the select loop when a spawned task blocks on channel send.
static int __gorget_try_run_one_nonblocking(void) {
    pthread_mutex_lock(&__gorget_exec.mtx);
    for (int i = 0; i < __gorget_exec.queue_len; i++) {
        GorgetTask* task = __gorget_exec.queue[i];
        if (task->poll != NULL) {
            // Remove from queue
            memmove(__gorget_exec.queue + i, __gorget_exec.queue + i + 1,
                    (size_t)(__gorget_exec.queue_len - i - 1) * sizeof(GorgetTask*));
            __gorget_exec.queue_len--;
            pthread_mutex_unlock(&__gorget_exec.mtx);
            __gorget_run_task_inline(task);
            return 1;
        }
    }
    pthread_mutex_unlock(&__gorget_exec.mtx);
    return 0;
}

// Re-submit a task to the executor (used by future wakers for cooperative yield).
static void __gorget_executor_resubmit(GorgetTask* task) {
    pthread_mutex_lock(&task->mtx);
    task->done = 0;
    pthread_mutex_unlock(&task->mtx);
    __gorget_executor_submit(task);
}

// Waker callback used by stackless coroutines.
static void __gorget_fiber_waker_wake(GorgetWaker* w) {
    GorgetTask* task = (GorgetTask*)w->data;
    pthread_mutex_lock(&task->mtx);
    if (task->scheduled == -1) {
        task->scheduled = 0;
        pthread_mutex_unlock(&task->mtx);
        __gorget_executor_submit(task);
    } else if (task->scheduled == 0) {
        task->scheduled = 1;
        pthread_mutex_unlock(&task->mtx);
    } else {
        pthread_mutex_unlock(&task->mtx);
    }
}

// Notify the executor that the current worker is about to do blocking I/O.
// Spawns a temporary replacement worker so the pool stays at capacity.
static volatile int __gorget_blocking_active = 0;

static void __gorget_blocking_enter(void) {
    __gorget_executor_init();
    __atomic_add_fetch(&__gorget_blocking_active, 1, __ATOMIC_SEQ_CST);
    // Spawn a temporary detached worker to keep the pool at capacity.
    pthread_t tmp;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&tmp, &attr, __gorget_worker, NULL);
    pthread_attr_destroy(&attr);
}

static void __gorget_blocking_exit(void) {
    __atomic_sub_fetch(&__gorget_blocking_active, 1, __ATOMIC_SEQ_CST);
    // Signal workers to check the queue — the temp worker will exit if idle.
    pthread_mutex_lock(&__gorget_exec.mtx);
    pthread_cond_broadcast(&__gorget_exec.cond);
    pthread_mutex_unlock(&__gorget_exec.mtx);
}

static void __gorget_executor_shutdown(void) {
    if (!__gorget_exec_init_done) return;
    pthread_mutex_lock(&__gorget_exec.mtx);
    __gorget_exec.shutdown = 1;
    pthread_cond_broadcast(&__gorget_exec.cond);
    pthread_mutex_unlock(&__gorget_exec.mtx);
    for (int i = 0; i < __gorget_exec.thread_count; i++)
        pthread_join(__gorget_exec.threads[i], NULL);
    GORGET_FREE(__gorget_exec.threads, (size_t)__gorget_exec.thread_count * sizeof(pthread_t));
    GORGET_FREE(__gorget_exec.queue, (size_t)__gorget_exec.queue_cap * sizeof(GorgetTask*));
    pthread_mutex_destroy(&__gorget_exec.mtx);
    pthread_cond_destroy(&__gorget_exec.cond);
    __gorget_exec_init_done = 0;
}

// ── Worker Waker (event-driven sub-future polling) ──
typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int woken;
} __GorgetWorkerWakerCtx;

static void __gorget_worker_waker_wake(GorgetWaker* w) {
    __GorgetWorkerWakerCtx* ctx = (__GorgetWorkerWakerCtx*)w->data;
    pthread_mutex_lock(&ctx->mtx);
    ctx->woken = 1;
    pthread_cond_signal(&ctx->cond);
    pthread_mutex_unlock(&ctx->mtx);
}

// Select yield: help the executor make progress when the main thread's select
// spin-loop is waiting for data.  Only steal poll-based (non-blocking) tasks;
// run-based tasks (blocking spawns) must stay on worker threads to avoid
// deadlocking the select loop when a spawned producer blocks on channel send.
static int __gorget_select_yield(void) {
    if (!__gorget_try_run_one_nonblocking()) {
        usleep(100);   // 100 µs backoff
    }
    return 0;
}

#define GORGET_SCHEDULER_SUBMIT(task)  __gorget_executor_submit(task)
#define GORGET_SCHEDULER_WAIT(task_ptr) do { \
    for (;;) { \
        pthread_mutex_lock(&(task_ptr)->mtx); \
        if ((task_ptr)->done) { pthread_mutex_unlock(&(task_ptr)->mtx); break; } \
        pthread_mutex_unlock(&(task_ptr)->mtx); \
        if (!__gorget_try_run_one()) sched_yield(); \
    } \
} while(0)
