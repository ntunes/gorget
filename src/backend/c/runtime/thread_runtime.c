
// ── std.thread runtime ──────────────────────────────────────
#include <pthread.h>

static inline int64_t gorget_current_thread_id(void) {
    return (int64_t)(uintptr_t)pthread_self();
}
