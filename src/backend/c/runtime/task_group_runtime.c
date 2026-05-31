
// ── TaskGroup ──
// Stores a dynamic array of GorgetTask* pointers for all spawned child tasks.
// The first field of every __SpawnCtx_fn is GorgetTask base, so a void* cast works.
// join() waits via condvar on each task's done flag; drop calls join + frees memory.
typedef struct gorget_task_group_t {
    GorgetTask** tasks;
    void**       task_ctxs;   // raw __SpawnCtx_fn* pointers for freeing via __drop
    void (**drops)(void*);    // per-task __spawn_drop_fn pointers
    int          count;
    int          cap;
} gorget_task_group_t;
typedef gorget_task_group_t* TaskGroup;

static inline gorget_task_group_t* gorget_task_group_new(void) {
    gorget_task_group_t* g = (gorget_task_group_t*)GORGET_CALLOC(1, sizeof(gorget_task_group_t));
    return g;
}

// Record a spawned task's GorgetTask* so join() can wait on it.
// task_ctx is __SpawnCtx_fn* whose first field is GorgetTask base.
static inline void gorget_task_group_submit_raw(gorget_task_group_t* g,
                                                void* task_ctx,
                                                void (*drop_fn)(void*)) {
    if (g->count == g->cap) {
        int new_cap = g->cap ? g->cap * 2 : 4;
        GorgetTask** tbuf = (GorgetTask**)GORGET_ALLOC((size_t)new_cap * sizeof(GorgetTask*));
        void**       cbuf = (void**)GORGET_ALLOC((size_t)new_cap * sizeof(void*));
        void (**dbuf)(void*) = (void (**)(void*))GORGET_ALLOC((size_t)new_cap * sizeof(void(*)(void*)));
        if (g->tasks) {
            memcpy(tbuf, g->tasks, (size_t)g->count * sizeof(GorgetTask*));
            memcpy(cbuf, g->task_ctxs, (size_t)g->count * sizeof(void*));
            memcpy(dbuf, g->drops, (size_t)g->count * sizeof(void(*)(void*)));
            GORGET_FREE(g->tasks,    (size_t)g->cap * sizeof(GorgetTask*));
            GORGET_FREE(g->task_ctxs,(size_t)g->cap * sizeof(void*));
            GORGET_FREE(g->drops,    (size_t)g->cap * sizeof(void(*)(void*)));
        }
        g->tasks     = tbuf;
        g->task_ctxs = cbuf;
        g->drops     = dbuf;
        g->cap       = new_cap;
    }
    g->tasks[g->count]     = (GorgetTask*)task_ctx;
    g->task_ctxs[g->count] = task_ctx;
    g->drops[g->count]     = drop_fn;
    g->count++;
}

// Extracts __task void* and __drop from any Task__T struct, records them.
// Nulls out __task so the Task temporary's drop is a no-op (TaskGroup owns cleanup).
#define gorget_task_group_submit(g, task) do { \
    gorget_task_group_submit_raw((g), (task).__task, (task).__drop); \
    (task).__task = NULL; \
} while(0)

// Blocking join — waits for all submitted tasks to finish, then frees each.
// Uses work-stealing: while waiting for a task to complete, tries to dequeue
// and run other tasks from the executor queue. This prevents thread-pool
// starvation when tasks block waiting on child tasks (nested spawn pattern).
static inline void gorget_task_group_join(gorget_task_group_t* g) {
    for (int i = 0; i < g->count; i++) {
        GORGET_SCHEDULER_WAIT(g->tasks[i]);
        g->drops[i](g->task_ctxs[i]);
    }
    g->count = 0;  // reset; group may be reused after join()
}

// Destructor (called by RAII drop): join all tasks, free arrays, free group.
static inline void gorget_task_group_free(gorget_task_group_t** gp) {
    if (!gp || !*gp) return;
    gorget_task_group_t* g = *gp;
    gorget_task_group_join(g);
    if (g->tasks)     GORGET_FREE(g->tasks,     (size_t)g->cap * sizeof(GorgetTask*));
    if (g->task_ctxs) GORGET_FREE(g->task_ctxs, (size_t)g->cap * sizeof(void*));
    if (g->drops)     GORGET_FREE(g->drops,      (size_t)g->cap * sizeof(void(*)(void*)));
    GORGET_FREE(g, sizeof(gorget_task_group_t));
    *gp = NULL;
}
