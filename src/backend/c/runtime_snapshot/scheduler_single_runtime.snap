
// ── Scheduler: Single (N:1 cooperative event loop) ──

static GorgetTask** __gorget_single_queue = NULL;
static int __gorget_single_queue_len = 0;
static int __gorget_single_queue_cap = 0;

static void __gorget_single_enqueue(GorgetTask* task) {
    if (__gorget_single_queue_len == __gorget_single_queue_cap) {
        int new_cap = __gorget_single_queue_cap ? __gorget_single_queue_cap * 2 : 16;
        GorgetTask** buf = (GorgetTask**)GORGET_ALLOC((size_t)new_cap * sizeof(GorgetTask*));
        if (__gorget_single_queue) {
            memcpy(buf, __gorget_single_queue, (size_t)__gorget_single_queue_len * sizeof(GorgetTask*));
            GORGET_FREE(__gorget_single_queue, (size_t)__gorget_single_queue_cap * sizeof(GorgetTask*));
        }
        __gorget_single_queue = buf;
        __gorget_single_queue_cap = new_cap;
    }
    __gorget_single_queue[__gorget_single_queue_len++] = task;
}

static int __gorget_single_try_run_one(void) {
    if (__gorget_single_queue_len == 0) return 0;
    GorgetTask* task = __gorget_single_queue[0];
    memmove(__gorget_single_queue, __gorget_single_queue + 1,
            (size_t)(--__gorget_single_queue_len) * sizeof(GorgetTask*));
    if (task->poll != NULL) {
        int status = task->poll(task);
        if (status == GORGET_POLL_READY) {
            task->done = 1;
            if (task->parent_waker.wake) task->parent_waker.wake(&task->parent_waker);
        }
        // If PENDING, the waker will re-enqueue when ready.
    } else {
        task->run(task);
        task->done = 1;
        if (task->parent_waker.wake) task->parent_waker.wake(&task->parent_waker);
    }
    return 1;
}

static void __gorget_executor_submit(GorgetTask* task) {
    __gorget_single_enqueue(task);
}

static void __gorget_executor_resubmit(GorgetTask* task) {
    task->done = 0;
    __gorget_single_enqueue(task);
}

static void __gorget_fiber_waker_wake(GorgetWaker* w) {
    GorgetTask* task = (GorgetTask*)w->data;
    __gorget_single_enqueue(task);
}

static void __gorget_executor_shutdown(void) {
    if (__gorget_single_queue) {
        GORGET_FREE(__gorget_single_queue, (size_t)__gorget_single_queue_cap * sizeof(GorgetTask*));
        __gorget_single_queue = NULL;
        __gorget_single_queue_len = 0;
        __gorget_single_queue_cap = 0;
    }
}

typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int woken;
} __GorgetWorkerWakerCtx;

static void __gorget_worker_waker_wake(GorgetWaker* w) {
    __GorgetWorkerWakerCtx* ctx = (__GorgetWorkerWakerCtx*)w->data;
    ctx->woken = 1;
}

static int __gorget_select_yield(void) {
    if (!__gorget_single_try_run_one()) { usleep(100); }
    return 0;
}

#define GORGET_SCHEDULER_SUBMIT(task) __gorget_single_enqueue(task)
#define GORGET_SCHEDULER_WAIT(task_ptr) do { \
    while (!(task_ptr)->done) { \
        if (!__gorget_single_try_run_one()) break; \
    } \
} while(0)
