
// ── Scheduler: Inline (synchronous on caller thread) ──

static void __gorget_run_task_inline_sync(GorgetTask* task) {
    if (task->poll != NULL) {
        while (task->poll(task) != GORGET_POLL_READY) { /* spin */ }
    } else {
        task->run(task);
    }
    task->done = 1;
    if (task->parent_waker.wake) task->parent_waker.wake(&task->parent_waker);
}

static void __gorget_executor_submit(GorgetTask* task) {
    __gorget_run_task_inline_sync(task);
}

static void __gorget_executor_resubmit(GorgetTask* task) {
    task->done = 0;
    __gorget_executor_submit(task);
}

static void __gorget_fiber_waker_wake(GorgetWaker* w) { (void)w; }
static void __gorget_executor_shutdown(void) { }

typedef struct {
    pthread_mutex_t mtx;
    pthread_cond_t cond;
    volatile int woken;
} __GorgetWorkerWakerCtx;

static void __gorget_worker_waker_wake(GorgetWaker* w) {
    __GorgetWorkerWakerCtx* ctx = (__GorgetWorkerWakerCtx*)w->data;
    ctx->woken = 1;
}

static int __gorget_select_yield(void) { sched_yield(); return 0; }

#define GORGET_SCHEDULER_SUBMIT(task) __gorget_executor_submit(task)
#define GORGET_SCHEDULER_WAIT(task_ptr) do { (void)(task_ptr); } while(0)
