
// ── Channel (bounded MPMC) ──
#ifndef GORGET_PTHREAD_INCLUDED
#define GORGET_PTHREAD_INCLUDED
#include <pthread.h>
#endif
typedef struct GorgetChannel {
    void*           buf;
    size_t          elem_size;
    size_t          capacity;
    size_t          head;
    size_t          tail;
    size_t          count;
    pthread_mutex_t mtx;
    pthread_cond_t  not_full;
    pthread_cond_t  not_empty;
    volatile int    closed;
    GorgetWaker*    send_waiters;
    size_t          send_waiter_count;
    size_t          send_waiter_cap;
    GorgetWaker*    recv_waiters;
    size_t          recv_waiter_count;
    size_t          recv_waiter_cap;
    GorgetAllocator* alloc;
    volatile int64_t refcount;  // Atomic reference count for RAII (auto-close+free)
} GorgetChannel;

static GorgetChannel* gorget_channel_new(size_t capacity, size_t elem_size) {
    GorgetAllocator* a = __gorget_current_alloc;
    GorgetChannel* ch = (GorgetChannel*)GORGET_CALLOC(1, sizeof(GorgetChannel));
    ch->elem_size = elem_size;
    ch->capacity = capacity;
    ch->alloc = a;
    ch->refcount = 1;
    if (capacity == 0) {
        // Rendezvous channel: single-element transfer slot, no ring buffer
        ch->buf = a->alloc(a->ctx, elem_size);
    } else {
        ch->buf = a->alloc(a->ctx, capacity * elem_size);
    }
    pthread_mutex_init(&ch->mtx, NULL);
    pthread_cond_init(&ch->not_full, NULL);
    pthread_cond_init(&ch->not_empty, NULL);
    return ch;
}

static void gorget_channel_send(GorgetChannel* ch, const void* data) {
    pthread_mutex_lock(&ch->mtx);
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mtx);
        fprintf(stderr, "gorget: panic: send on closed channel\n");
        exit(1);
    }

    if (ch->capacity == 0) {
        // Rendezvous send: wait until transfer slot is free, deposit, then
        // block until the receiver has consumed (count drops back to 0).
        while (ch->count == 1 && !ch->closed)
            pthread_cond_wait(&ch->not_full, &ch->mtx);
        if (ch->closed) {
            pthread_mutex_unlock(&ch->mtx);
            fprintf(stderr, "gorget: panic: send on closed channel\n");
            exit(1);
        }
        memcpy(ch->buf, data, ch->elem_size);
        ch->count = 1;
        // Wake async recv waiter or signal sync condvar
        if (ch->recv_waiter_count > 0) {
            GorgetWaker w = ch->recv_waiters[0];
            memmove(ch->recv_waiters, ch->recv_waiters + 1, (--ch->recv_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
            // Re-acquire lock to wait for the receiver to consume the slot
            pthread_mutex_lock(&ch->mtx);
        } else {
            pthread_cond_signal(&ch->not_empty);
            // Keep lock held for the wait below
        }
        // Block until receiver has consumed (count == 0 signals ack)
        while (ch->count == 1 && !ch->closed)
            pthread_cond_wait(&ch->not_full, &ch->mtx);
        pthread_mutex_unlock(&ch->mtx);
        return;
    }

    // Buffered send
    while (ch->count == ch->capacity && !ch->closed)
        pthread_cond_wait(&ch->not_full, &ch->mtx);
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mtx);
        fprintf(stderr, "gorget: panic: send on closed channel\n");
        exit(1);
    }
    memcpy((char*)ch->buf + ch->tail * ch->elem_size, data, ch->elem_size);
    ch->tail = (ch->tail + 1) % ch->capacity;
    ch->count++;
    // Wake one async recv waiter if any, else signal condvar for sync waiters
    if (ch->recv_waiter_count > 0) {
        GorgetWaker w = ch->recv_waiters[0];
        memmove(ch->recv_waiters, ch->recv_waiters + 1, (--ch->recv_waiter_count) * sizeof(GorgetWaker));
        pthread_mutex_unlock(&ch->mtx);
        w.wake(&w);
    } else {
        pthread_cond_signal(&ch->not_empty);
        pthread_mutex_unlock(&ch->mtx);
    }
}

static void gorget_channel_recv(GorgetChannel* ch, void* out) {
    pthread_mutex_lock(&ch->mtx);

    if (ch->capacity == 0) {
        // Rendezvous recv: wait for sender to deposit, consume, signal ack.
        while (ch->count == 0 && !ch->closed)
            pthread_cond_wait(&ch->not_empty, &ch->mtx);
        if (ch->count == 0 && ch->closed) {
            pthread_mutex_unlock(&ch->mtx);
            fprintf(stderr, "gorget: panic: recv on closed empty channel\n");
            exit(1);
        }
        memcpy(out, ch->buf, ch->elem_size);
        ch->count = 0;
        // Ack the sender (wake async send waiter or signal condvar)
        if (ch->send_waiter_count > 0) {
            GorgetWaker w = ch->send_waiters[0];
            memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
        } else {
            pthread_cond_signal(&ch->not_full);
            pthread_mutex_unlock(&ch->mtx);
        }
        return;
    }

    // Buffered recv
    while (ch->count == 0 && !ch->closed)
        pthread_cond_wait(&ch->not_empty, &ch->mtx);
    if (ch->count == 0 && ch->closed) {
        pthread_mutex_unlock(&ch->mtx);
        fprintf(stderr, "gorget: panic: recv on closed empty channel\n");
        exit(1);
    }
    memcpy(out, (char*)ch->buf + ch->head * ch->elem_size, ch->elem_size);
    ch->head = (ch->head + 1) % ch->capacity;
    ch->count--;
    // Wake one async send waiter if any, else signal condvar for sync waiters
    if (ch->send_waiter_count > 0) {
        GorgetWaker w = ch->send_waiters[0];
        memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
        pthread_mutex_unlock(&ch->mtx);
        w.wake(&w);
    } else {
        pthread_cond_signal(&ch->not_full);
        pthread_mutex_unlock(&ch->mtx);
    }
}

// Poll-based channel send for async contexts. Returns 1 if sent, 0 if would block (waker registered).
static int gorget_channel_poll_send(GorgetChannel* ch, const void* data, GorgetWaker* waker) {
    pthread_mutex_lock(&ch->mtx);
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mtx);
        fprintf(stderr, "gorget: panic: send on closed channel\n");
        exit(1);
    }

    if (ch->capacity == 0) {
        // Rendezvous poll_send: deposit when slot is free; return pending if full.
        // Note: true rendezvous ack (wait for count==0) requires two-phase state
        // machine support — not yet generated by codegen. For now, treat deposit
        // as completion (consistent with sync send, which the producer uses).
        if (ch->count == 0) {
            memcpy(ch->buf, data, ch->elem_size);
            ch->count = 1;
            if (ch->recv_waiter_count > 0) {
                GorgetWaker w = ch->recv_waiters[0];
                memmove(ch->recv_waiters, ch->recv_waiters + 1, (--ch->recv_waiter_count) * sizeof(GorgetWaker));
                pthread_mutex_unlock(&ch->mtx);
                w.wake(&w);
            } else {
                pthread_cond_signal(&ch->not_empty);
                pthread_mutex_unlock(&ch->mtx);
            }
            return 1;
        }
        // Slot occupied — register waker and return pending
        if (waker) {
            if (ch->send_waiter_count == ch->send_waiter_cap) {
                size_t old_cap = ch->send_waiter_cap;
                ch->send_waiter_cap = old_cap ? old_cap * 2 : 4;
                ch->send_waiters = (GorgetWaker*)ch->alloc->realloc(ch->alloc->ctx, ch->send_waiters, old_cap * sizeof(GorgetWaker), ch->send_waiter_cap * sizeof(GorgetWaker));
            }
            ch->send_waiters[ch->send_waiter_count++] = *waker;
        }
        pthread_mutex_unlock(&ch->mtx);
        return 0;
    }

    // Buffered poll_send
    if (ch->count < ch->capacity) {
        memcpy((char*)ch->buf + ch->tail * ch->elem_size, data, ch->elem_size);
        ch->tail = (ch->tail + 1) % ch->capacity;
        ch->count++;
        if (ch->recv_waiter_count > 0) {
            GorgetWaker w = ch->recv_waiters[0];
            memmove(ch->recv_waiters, ch->recv_waiters + 1, (--ch->recv_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
        } else {
            pthread_cond_signal(&ch->not_empty);
            pthread_mutex_unlock(&ch->mtx);
        }
        return 1;
    }
    // Channel full — register waker (if non-NULL) and return pending
    if (waker) {
        if (ch->send_waiter_count == ch->send_waiter_cap) {
            size_t old_cap = ch->send_waiter_cap;
            ch->send_waiter_cap = old_cap ? old_cap * 2 : 4;
            ch->send_waiters = (GorgetWaker*)ch->alloc->realloc(ch->alloc->ctx, ch->send_waiters, old_cap * sizeof(GorgetWaker), ch->send_waiter_cap * sizeof(GorgetWaker));
        }
        ch->send_waiters[ch->send_waiter_count++] = *waker;
    }
    pthread_mutex_unlock(&ch->mtx);
    return 0;
}

// Poll-based channel recv for async contexts. Returns 1 if received, 0 if would block (waker registered).
static int gorget_channel_poll_recv(GorgetChannel* ch, void* out, GorgetWaker* waker) {
    pthread_mutex_lock(&ch->mtx);

    if (ch->capacity == 0) {
        // Rendezvous poll_recv: consume from slot if data present, else register waker.
        if (ch->count == 1) {
            memcpy(out, ch->buf, ch->elem_size);
            ch->count = 0;
            // Ack the sender (wake async send waiter or signal condvar)
            if (ch->send_waiter_count > 0) {
                GorgetWaker w = ch->send_waiters[0];
                memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
                pthread_mutex_unlock(&ch->mtx);
                w.wake(&w);
            } else {
                pthread_cond_signal(&ch->not_full);
                pthread_mutex_unlock(&ch->mtx);
            }
            return 1;
        }
        if (ch->closed) {
            pthread_mutex_unlock(&ch->mtx);
            fprintf(stderr, "gorget: panic: recv on closed empty channel\n");
            exit(1);
        }
        // No data — register waker and return pending
        if (waker) {
            if (ch->recv_waiter_count == ch->recv_waiter_cap) {
                size_t old_cap = ch->recv_waiter_cap;
                ch->recv_waiter_cap = old_cap ? old_cap * 2 : 4;
                ch->recv_waiters = (GorgetWaker*)ch->alloc->realloc(ch->alloc->ctx, ch->recv_waiters, old_cap * sizeof(GorgetWaker), ch->recv_waiter_cap * sizeof(GorgetWaker));
            }
            ch->recv_waiters[ch->recv_waiter_count++] = *waker;
        }
        pthread_mutex_unlock(&ch->mtx);
        return 0;
    }

    // Buffered poll_recv
    if (ch->count > 0) {
        memcpy(out, (char*)ch->buf + ch->head * ch->elem_size, ch->elem_size);
        ch->head = (ch->head + 1) % ch->capacity;
        ch->count--;
        if (ch->send_waiter_count > 0) {
            GorgetWaker w = ch->send_waiters[0];
            memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
        } else {
            pthread_cond_signal(&ch->not_full);
            pthread_mutex_unlock(&ch->mtx);
        }
        return 1;
    }
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mtx);
        fprintf(stderr, "gorget: panic: recv on closed empty channel\n");
        exit(1);
    }
    // Channel empty — register waker (if non-NULL) and return pending
    if (waker) {
        if (ch->recv_waiter_count == ch->recv_waiter_cap) {
            size_t old_cap = ch->recv_waiter_cap;
            ch->recv_waiter_cap = old_cap ? old_cap * 2 : 4;
            ch->recv_waiters = (GorgetWaker*)ch->alloc->realloc(ch->alloc->ctx, ch->recv_waiters, old_cap * sizeof(GorgetWaker), ch->recv_waiter_cap * sizeof(GorgetWaker));
        }
        ch->recv_waiters[ch->recv_waiter_count++] = *waker;
    }
    pthread_mutex_unlock(&ch->mtx);
    return 0;
}

static int64_t gorget_channel_len(GorgetChannel* ch) {
    pthread_mutex_lock(&ch->mtx);
    int64_t n = (int64_t)ch->count;
    pthread_mutex_unlock(&ch->mtx);
    return n;
}

static int64_t gorget_channel_capacity(GorgetChannel* ch) {
    return (int64_t)ch->capacity;
}

static bool gorget_channel_is_closed(GorgetChannel* ch) {
    return ch->closed != 0;
}

// Blocking recv with timeout (ms). Returns 1 if received, 0 on timeout.
static int gorget_channel_recv_timeout(GorgetChannel* ch, void* out, int64_t timeout_ms) {
    pthread_mutex_lock(&ch->mtx);
    if (ch->count > 0 || (ch->capacity == 0 && ch->count == 1)) goto have_data;

    {
        struct timespec deadline;
        clock_gettime(CLOCK_REALTIME, &deadline);
        deadline.tv_sec  += timeout_ms / 1000;
        deadline.tv_nsec += (timeout_ms % 1000) * 1000000L;
        if (deadline.tv_nsec >= 1000000000L) {
            deadline.tv_sec++;
            deadline.tv_nsec -= 1000000000L;
        }
        while (ch->count == 0 && !ch->closed) {
            int rc = pthread_cond_timedwait(&ch->not_empty, &ch->mtx, &deadline);
            if (rc != 0) { /* ETIMEDOUT */
                pthread_mutex_unlock(&ch->mtx);
                return 0;
            }
        }
        if (ch->count == 0) {
            pthread_mutex_unlock(&ch->mtx);
            return 0; /* closed + empty */
        }
    }

have_data:
    if (ch->capacity == 0) {
        memcpy(out, ch->buf, ch->elem_size);
        ch->count = 0;
        if (ch->send_waiter_count > 0) {
            GorgetWaker w = ch->send_waiters[0];
            memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
        } else {
            pthread_cond_signal(&ch->not_full);
            pthread_mutex_unlock(&ch->mtx);
        }
    } else {
        memcpy(out, (char*)ch->buf + ch->head * ch->elem_size, ch->elem_size);
        ch->head = (ch->head + 1) % ch->capacity;
        ch->count--;
        if (ch->send_waiter_count > 0) {
            GorgetWaker w = ch->send_waiters[0];
            memmove(ch->send_waiters, ch->send_waiters + 1, (--ch->send_waiter_count) * sizeof(GorgetWaker));
            pthread_mutex_unlock(&ch->mtx);
            w.wake(&w);
        } else {
            pthread_cond_signal(&ch->not_full);
            pthread_mutex_unlock(&ch->mtx);
        }
    }
    return 1;
}

static void gorget_channel_close(GorgetChannel* ch) {
    pthread_mutex_lock(&ch->mtx);
    ch->closed = 1;
    // Wake all async waiters
    for (size_t i = 0; i < ch->send_waiter_count; i++) {
        ch->send_waiters[i].wake(&ch->send_waiters[i]);
    }
    ch->send_waiter_count = 0;
    for (size_t i = 0; i < ch->recv_waiter_count; i++) {
        ch->recv_waiters[i].wake(&ch->recv_waiters[i]);
    }
    ch->recv_waiter_count = 0;
    pthread_cond_broadcast(&ch->not_full);
    pthread_cond_broadcast(&ch->not_empty);
    pthread_mutex_unlock(&ch->mtx);
}

static void gorget_channel_free(GorgetChannel* ch) {
    if (!ch) return;
    GorgetAllocator* a = ch->alloc;
    pthread_mutex_destroy(&ch->mtx);
    pthread_cond_destroy(&ch->not_full);
    pthread_cond_destroy(&ch->not_empty);
    // Rendezvous channels allocate a single elem_size slot; buffered use capacity * elem_size
    if (ch->capacity == 0) {
        a->dealloc(a->ctx, ch->buf, ch->elem_size);
    } else {
        a->dealloc(a->ctx, ch->buf, ch->capacity * ch->elem_size);
    }
    a->dealloc(a->ctx, ch->send_waiters, ch->send_waiter_cap * sizeof(GorgetWaker));
    a->dealloc(a->ctx, ch->recv_waiters, ch->recv_waiter_cap * sizeof(GorgetWaker));
    a->dealloc(a->ctx, ch, sizeof(GorgetChannel));
}

// Retain: atomically increment the channel's reference count.
static GorgetChannel* gorget_channel_retain(GorgetChannel* ch) {
    __atomic_add_fetch(&ch->refcount, 1, __ATOMIC_RELAXED);
    return ch;
}

// Release: atomically decrement the channel's reference count.
// When the last reference is dropped, auto-close (if not already) and free.
static void gorget_channel_release(GorgetChannel* ch) {
    if (!ch) return;
    if (__atomic_sub_fetch(&ch->refcount, 1, __ATOMIC_ACQ_REL) == 0) {
        if (!ch->closed) gorget_channel_close(ch);
        gorget_channel_free(ch);
    }
}
