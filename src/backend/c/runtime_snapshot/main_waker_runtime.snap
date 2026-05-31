
// ── Main Thread Waker ──
static pthread_mutex_t __gorget_main_mtx = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t __gorget_main_cond = PTHREAD_COND_INITIALIZER;
static volatile int __gorget_main_woken = 1;

static void __gorget_main_wake(GorgetWaker* w) {
    (void)w;
    pthread_mutex_lock(&__gorget_main_mtx);
    __gorget_main_woken = 1;
    pthread_cond_signal(&__gorget_main_cond);
    pthread_mutex_unlock(&__gorget_main_mtx);
}
static GorgetWaker __gorget_main_waker = { __gorget_main_wake, NULL };
