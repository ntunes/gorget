
// ── Async Runtime ──
#define GORGET_POLL_READY 0
#define GORGET_POLL_PENDING 1

#ifndef GORGET_WAKER_DEFINED
#define GORGET_WAKER_DEFINED
typedef struct GorgetWaker {
    void (*wake)(struct GorgetWaker*);
    void* data;
} GorgetWaker;
#endif

static void __gorget_noop_wake(GorgetWaker* w) { (void)w; }
static GorgetWaker __gorget_noop_waker = { __gorget_noop_wake, NULL };
