
// ── Scheduler: Thread (1:1 OS thread per spawn) ──

static void __gorget_run_task_to_completion(GorgetTask* task) {
    if (task->poll != NULL) {
        while (1) {
            int status = task->poll(task);
            if (status == GORGET_POLL_READY) break;
            // Busy-wait for waker (condvar signal from child).
            pthread_mutex_lock(&task->mtx);
            while (task->scheduled == -1)
                pthread_cond_wait(&task->cond, &task->mtx);
            task->scheduled = 0;
            pthread_mutex_unlock(&task->mtx);
        }
    } else {
        task->run(task);
    }
    pthread_mutex_lock(&task->mtx);
    task->done = 1;
    pthread_cond_broadcast(&task->cond);
    GorgetWaker pw = task->parent_waker;
    pthread_mutex_unlock(&task->mtx);
    if (pw.wake) pw.wake(&pw);
}

static void* __gorget_thread_entry(void* arg) {
    GorgetTask* task = (GorgetTask*)arg;
    __gorget_run_task_to_completion(task);
    return NULL;
}

static void __gorget_executor_submit(GorgetTask* task) {
    pthread_t th;
    pthread_create(&th, NULL, __gorget_thread_entry, task);
    pthread_detach(th);
}

static void __gorget_executor_resubmit(GorgetTask* task) {
    pthread_mutex_lock(&task->mtx);
    task->done = 0;
    pthread_mutex_unlock(&task->mtx);
    __gorget_executor_submit(task);
}

static void __gorget_fiber_waker_wake(GorgetWaker* w) {
    GorgetTask* task = (GorgetTask*)w->data;
    pthread_mutex_lock(&task->mtx);
    if (task->scheduled == -1) {
        task->scheduled = 0;
        pthread_cond_signal(&task->cond);
    } else if (task->scheduled == 0) {
        task->scheduled = 1;
    }
    pthread_mutex_unlock(&task->mtx);
}

static void __gorget_executor_shutdown(void) { /* no-op for thread backend */ }

// Stub WorkerWakerCtx for reactor compatibility.
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

static int __gorget_select_yield(void) { sched_yield(); return 0; }

#define GORGET_SCHEDULER_SUBMIT(task) __gorget_executor_submit(task)
#define GORGET_SCHEDULER_WAIT(task_ptr) do { \
    pthread_mutex_lock(&(task_ptr)->mtx); \
    while (!(task_ptr)->done) \
        pthread_cond_wait(&(task_ptr)->cond, &(task_ptr)->mtx); \
    pthread_mutex_unlock(&(task_ptr)->mtx); \
} while(0)
