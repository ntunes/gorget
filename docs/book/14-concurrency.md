# Concurrency

Concurrency is where most languages reveal their seams. Shared mutable state leads to data
races; too much message-passing leads to deadlocks; fine-grained locks lead to priority
inversion. Gorget's approach is to start from two clear, composable primitives — tasks with
channels and shared variables — and let the compiler enforce the invariants that distinguish
safe concurrent code from code that merely appears to work.

This chapter covers how those primitives work, when each is the right tool, and how they
fit together in practice. It assumes you're comfortable with `async` functions and `spawn`
from *[Chapter 13: Tasks and Async/Await](13-async.md)*.

---

## Two Primitives, One Principle

Every concurrent Gorget program is built from two mechanisms:

- **Channels** — for *communication*: transferring values between tasks with ordering guarantees.
- **Shared variables** — for *state*: persisting mutable values that multiple tasks need to read or write.

The principle that unites them is straightforward:

> **Use channels to coordinate. Use shared variables to store.**

This is not a rule to memorize but a consequence of what each primitive actually guarantees.
A channel gives you ordering — values arrive in the sequence they were sent, and a send
blocks until the receiver is ready (or until the buffer has space). A shared variable gives
you a current value — any task that holds the lock sees the latest write. Neither primitive
does the other's job well, and mixing them up is the source of most concurrency bugs.

---

## Channels

A channel is a typed conduit between two tasks. You create one with a buffer size; capacity
zero means every send blocks until a receiver is waiting (rendezvous), while a positive
capacity means the sender can deposit up to that many values before blocking.

```gorget
from std.channel import Channel

async void producer(Channel[int] out):
    int i = 1
    while i <= 5:
        out.send(i)
        i += 1

async void main():
    Channel[int] ch = Channel[int](0)   # unbuffered: strict rendezvous
    Task[void] t = spawn producer(ch)
    int i = 0
    while i < 5:
        int val = ch.recv()
        print(val)
        i += 1
    t.await()
    ch.close()
```

The key property of a channel: **the receiver cannot see a value before the sender sends it**.
This ordering guarantee is what makes channels the right tool for signalling, for pipelines,
and for any situation where the *sequence* of events matters.

### Pipelines

The natural use of multiple channels is a pipeline, where each stage is a separate task and
channels connect them. Values flow left to right; backpressure is automatic — a stage that
processes slowly will cause its upstream sender to block.

```gorget
async void transform(Channel[int] inp, Channel[int] out, int count):
    int i = 0
    while i < count:
        int val = inp.recv()
        out.send(val * val)     # square each value
        i += 1

async void main():
    Channel[int] source = Channel[int](8)
    Channel[int] result = Channel[int](8)
    int count = 10

    Task[void] gen = spawn generate(source, count)     # produces 1..10
    Task[void] xfm = spawn transform(source, result, count)

    int i = 0
    while i < count:
        print(result.recv())
        i += 1

    gen.await()
    xfm.await()
    source.close()
    result.close()
```

### Selecting over Multiple Channels

When a task needs to respond to whichever of several channels produces a value first, use
`select`. It waits on all listed channels simultaneously and takes the first branch that
can proceed:

```gorget
async void main():
    Channel[int] urgent   = Channel[int](1)
    Channel[String] messages = Channel[String](4)

    # ... spawn senders elsewhere ...

    while True:
        select:
            case int code = urgent.recv():
                print(f"urgent: {code}")
                break
            case String msg = messages.recv():
                print(msg)
```

`select` is the idiomatic way to multiplex. It does not spin — it yields until a channel is
ready, so it is as efficient as a single `recv`.

---

## Shared Variables

A shared variable is mutable state that is safe to access from multiple concurrent tasks.
You declare one with the `shared` keyword:

```gorget
shared int counter = 0
shared Vector[String] log_entries = Vector[String]()
```

The compiler wraps the value in a `Shared[Mutex[T]]` internally. You never touch the lock
directly; instead, you access the value through a `with` block, which acquires the lock
and binds the current value to a local name:

```gorget
async void increment(int &counter):
    counter += 1

async void main():
    shared int x = 0
    Task[void] t = spawn increment(&x)
    t.await()
    with x:
        print(x)   # guaranteed fresh: 1
```

The `with x:` block acquires the lock, makes the current value of `x` available by that
name, then releases the lock when the block exits. There is no way to access a shared
variable without going through this mechanism — the compiler enforces it.

> **Two forms of `with`.** You may recognize `with` from *[Chapter 11: Ownership](11-ownership.md)*,
> where `with File.open(path) as f:` manages a scoped resource. The `as` keyword is the
> disambiguator: `with expr as name:` creates a new binding from an expression and drops it
> at block exit (resource management), while `with name:` references an existing `shared`
> variable, acquires its lock, and auto-refreshes the value across yield points.

### What `with` Guarantees — and What It Does Not

Here is the thing that trips people up: **`with` provides freshness, not atomicity**.

Gorget's async model is stackless — tasks run cooperatively, and a task yields control at
explicit suspension points: `await`, `sleep`, blocking I/O. At each such point, the runtime
releases all held locks, runs other tasks, then reacquires the locks on resumption.

This is the right default for async code. Holding locks across `await` would serialize all
concurrent tasks through every shared variable they touch — effectively defeating
concurrency. But it means the lock is not held for the entire duration of a `with` block if
that block contains a yield point.

```gorget
with counter:
    int snapshot = counter       # read while lock is held
    sleep_ms(100)                   # lock released here; another task may write
                                 # lock reacquired; counter is refreshed
    counter = snapshot + 1       # PROBLEM: snapshot is stale
```

After the `sleep`, `counter` is refreshed to the current value — but `snapshot` was captured
before the sleep. Writing `counter = snapshot + 1` silently discards any writes that
occurred during the sleep. This is a **lost update**, and the compiler will warn about it
(see *What the Compiler Catches*, below).

The deeper point: within a single uninterrupted synchronous region — between any two yield
points — a `with` block provides genuine mutual exclusion. Across yield points, the
exclusion is released and must not be relied upon for atomicity of multi-step operations.

---

## Coordination Patterns

With this foundation, five patterns cover the vast majority of concurrent Gorget programs.

### Pattern 1 — Notify (channel signals, shared carries the value)

The most common pattern: one task updates shared state and notifies another via a channel.
The channel provides ordering (the receiver knows the write has happened); `with` provides
the fresh value.

```gorget
async void producer(int &counter, Channel[void] ready):
    counter += 1
    ready.send(())              # signal: write is done

async void consumer(int &counter, Channel[void] ready):
    ready.recv()                # wait for the signal
    with counter:
        print(counter)          # guaranteed to see 1; no stale-condition warning

async void main():
    shared int x = 0
    Channel[void] ready = Channel[void](1)
    Task[void] p = spawn producer(&x, ready)
    Task[void] c = spawn consumer(&x, ready)
    p.await()
    c.await()
```

Do not use shared state to signal readiness (a flag that one task polls in a loop). That
is both wasteful and racy. A channel is the right signal mechanism.

### Pattern 2 — Actor (channel for all access, no shared state)

When a piece of state has complex invariants that must hold across multiple fields —
a bank account, a game world, a connection pool — expose it through channels rather
than shared variables. One task owns the state and handles requests; callers never touch
the data directly.

```gorget
async void account_actor(Channel[int] ops, Channel[int] out):
    int balance = 0
    while True:
        int op = ops.recv()
        if op == 0:             # sentinel: query balance
            out.send(balance)
            break
        balance += op           # positive = deposit, negative = withdrawal
        if balance < 0:
            balance = 0         # business rule: floor at zero

async void main():
    Channel[int] ops = Channel[int](8)
    Channel[int] out = Channel[int](1)
    Task[void] actor = spawn account_actor(ops, out)

    ops.send(100)               # deposit 100
    ops.send(-30)               # withdraw 30
    ops.send(0)                 # query (and terminate)

    int balance = out.recv()
    print(balance)              # 70
    actor.await()
```

The actor model guarantees that all mutations are sequential — they run inside a single
task, so no lock is needed. This is the right pattern when correctness depends on
multi-step invariants: the actor processes one operation at a time, in order, with full
access to all its internal state.

### Pattern 3 — Work Queue (channel distributes, shared accumulates)

A buffered channel distributes work items to a pool of workers. Workers do their
computation independently — without touching shared state — and write only a final result
when done.

```gorget
async void worker(Channel[int] jobs, int &total, Channel[void] done):
    while True:
        int n = jobs.recv()
        if n < 0: break         # sentinel: no more work
        int result = expensive_compute(n)   # no shared access here
        total += result
    done.send(())

async void main():
    shared int sum = 0
    Channel[int]  jobs = Channel[int](32)
    Channel[void] done = Channel[void](2)

    spawn worker(jobs, &sum, done)
    spawn worker(jobs, &sum, done)

    for i in 1..11:
        jobs.send(i)
    jobs.send(-1)               # terminate worker 1
    jobs.send(-1)               # terminate worker 2

    done.recv()
    done.recv()

    with sum:
        print(sum)
```

> **Note:** `total += result` is a read-modify-write on a shared variable. Two workers
> doing this simultaneously is a race — each reads the current value, adds its result,
> and writes back, potentially overwriting the other's write. The safe fix is either to
> protect the write with a `with` block, or to have each worker accumulate locally and
> merge at the end. The latter is usually faster.

### Pattern 4 — Semaphore (buffered channel as a token pool)

A buffered channel pre-filled with N tokens acts as a counting semaphore: receiving a
token acquires it, sending one back releases it. At most N tasks can be inside the critical
section simultaneously — without any shared variable.

```gorget
async void limited(Channel[void] sem, int id):
    sem.recv()                  # acquire: blocks if pool is empty
    sleep_ms(100)                  # critical section
    print(f"running: {id}")
    sem.send(())                # release

async void main():
    Channel[void] sem = Channel[void](3)    # at most 3 concurrent
    sem.send(())
    sem.send(())
    sem.send(())                            # pre-fill

    Vector[Task[void]] tasks = Vector[Task[void]]()
    for i in 0..10:
        tasks.push(spawn limited(sem, i))
    for t in tasks:
        t.await()
```

This pattern is useful for rate-limiting: connections to a database, concurrent HTTP
requests to an upstream, file handles. The channel's buffer capacity *is* the limit.

### Pattern 5 — Snapshot Config (single writer via channel, many readers via `with`)

Configuration that is read frequently but updated rarely is a natural fit for the
combination: one updater task receives new config through a channel and writes to the
shared variable; all other tasks read via `with`.

```gorget
struct Config:
    int timeout_ms
    String upstream_host

async void config_manager(Config &cfg, Channel[Config] updates):
    while True:
        Config new_cfg = updates.recv()
        cfg = new_cfg           # single writer — no write race

async void handler(Config &cfg, int request_id):
    with cfg:
        String host = cfg.upstream_host
        int ms   = cfg.timeout_ms
    # use host and ms outside the lock — no yield while holding it
    print(f"request {request_id} → {host} ({ms}ms)")

async void main():
    shared Config cfg = Config(timeout_ms=5000, upstream_host="localhost")
    Channel[Config] updates = Channel[Config](1)

    spawn config_manager(&cfg, updates)

    # Handlers read concurrently; updater writes exclusively.
    for i in 0..5:
        spawn handler(&cfg, i)

    updates.send(Config(timeout_ms=1000, upstream_host="prod.example.com"))
```

The `with cfg:` block in the handler reads both fields while the lock is held, then
releases it before doing any async work. This is the correct structure: **hold the lock
only long enough to copy what you need, then work with the copies**.

---

## What the Compiler Catches

Gorget's borrow checker actively warns about the most common ways these patterns go wrong.
§3.4–§3.8 are warnings — the program compiles — but each points to a real race
condition. §3.9 is specified as an error; §3.10's captures materialise at the
escape under D34. See their status notes for what the compiler enforces today.

### §3.4 — Stale condition

A local variable is read from shared state, a yield point occurs, and then the local is
used in a branch condition. The local may not reflect the current value of the shared
variable.

```gorget
int snapshot = x        # derived from shared x
t.await()               # x may change during this await
if snapshot > 0:        # warning: snapshot may be stale
    ...
```

Fix: re-read `x` after the await, or use `with x:` — the `with` binding is auto-refreshed
after every yield point, so it never triggers this warning.

### §3.5 — Check-then-act

A `with`-tracked binding is used in a branch condition, and the branch body contains a
yield point. The condition held when it was evaluated, but it may not hold by the time the
body executes — another task may have changed the shared variable while the lock was
released at the yield.

```gorget
with x:
    if x > 0:           # condition holds now
        sleep_ms(100)       # lock released — another task may set x to 0
                         # lock reacquired, x refreshed — but we're already inside the branch
        x -= 1           # warning: condition may no longer hold
```

Fix: move the yield before the branch, or re-check the condition after the yield.

### §3.6 — Stale write-back

A value computed from shared state before a yield is written back to shared state after
the yield. The write discards any changes that occurred during the yield.

```gorget
int val = x             # derived from shared x
sleep_ms(100)              # x may change; val is now stale
x = val + 1             # warning: lost update — val is stale
```

This applies to both plain assignment (`x = val`) and compound assignment (`x += val`).
Fix: re-read `x` after the yield and recompute.

### §3.7 — Iterator invalidation

A yield inside a `for` loop over a `with`-tracked shared collection releases the lock.
Another task may add or remove elements between iterations, invalidating the iterator's
position.

```gorget
with items:
    for item in items:
        sleep(1)        # warning: items may change between iterations
        print(item)
```

Fix: snapshot the collection into a local before iterating, or ensure no other task can
modify it during the loop.

### §3.8 — Spawn with tracked binding

A `with`-tracked binding is passed to a `spawn` call. The spawned task runs outside the
`with` block's lock scope — it receives a snapshot of the value, not live access through
the lock.

```gorget
with x:
    spawn other(x)      # warning: spawned task receives x's current value,
                        # not a live reference through the with lock
```

Fix: pass the underlying shared reference (`&x` declared `shared`) directly to the spawn,
outside the `with` block, so the spawned task manages its own lock acquisition.

### §3.9 — Borrowed reference crossing `spawn`

A `spawn` argument is a plain borrow (not wrapped in a `shared T` container). The child
task could outlive the borrow's owner, leaving a dangling reference.

```gorget
void child(Config &cfg):    # takes a borrow it writes through
    cfg.retries = cfg.retries + 1

void parent():
    Config cfg = Config.default()
    spawn child(&cfg)       # ERROR: borrowed reference crossing spawn boundary
```

Fix: wrap the value in `shared Config`, pass by value (if Copy), move ownership with
`!cfg`, or use a channel to communicate.

> **Status against the current compiler.** The rule above is the specification.
> `E_SpawnWithBorrowedRef` exists but no ordinary borrow shape has been observed
> to trip it — including the example above, which the compiler accepts today.

### §3.10 — Closure captures a local

A closure passed to `spawn` escapes into the child task, so its captures must be
materialised at that boundary — the closure may outlive the local it read. Same risk as §3.9.

```gorget
void parent():
    String msg = "hello"
    spawn ((): print(msg))()  # the closure captures `msg` from the enclosing scope
```

Capturing a `shared` directly is a separate rule and **is rejected today**
(`E_SpawnClosureCaptureShared`): the point of `shared` is that access goes
through its lock, so pass it as a direct `spawn` argument — `spawn worker(n)` —
rather than closing over the variable.

A closure that **assigns** to a captured local is rejected too
(`E_SpawnClosureCaptureMutable`) — a mutating capture holds a pointer into the
parent's stack frame, which the child task may outlive. Under D34 that check
becomes unnecessary rather than smarter: materialising the capture at the escape
turns the pointer into an owned value, and the mutation lands on the closure's
own state. A read-only capture is always safe, whatever its type.

> **Status against the current compiler.** The READ-ONLY shape above compiles —
> no check fires on it. A capture the compiler infers as mutating — the closure
> **assigns** the captured name — is rejected today with
> `E_SpawnClosureCaptureMutable`, and that is what `spawn unchecked` opts out
> of. Mutation through a **method call** (`spawn ((): s.push('!'))()`) is not
> detected: it is accepted and the mutation is silently lost, the same
> capture-mode inference gap §9.1's status note records. D34 would retire that rejection in favour of materialising at the escape,
> but it is not implemented. The `shared`-capture rejection is untouched by D34
> either way — it is lock discipline, not escape safety — and stands.

### Escape Hatch — `spawn unchecked`

When you know the child task's lifetime is bounded by the parent (e.g., you await the
task before the capture goes out of scope, or you manage the synchronization manually),
you can opt out of the spawn-capture safety check with `spawn unchecked`:

```gorget
async void main():
    String msg = "hello"
    Task[void] t = spawn unchecked ((): print(msg))()
    t.await()    # await before msg goes out of scope
    print("done")
```

`unchecked` works with `spawn blocking` too: `spawn blocking unchecked fn()` or
`spawn unchecked blocking fn()`. The keyword is grep-able so audits can find every
opt-out in a codebase.

This is an escape hatch, not a shortcut — prefer `shared T` or a channel over
`unchecked`. Reach for it only when the safety check rejects a pattern you've proven
safe by another means.

---

## Choosing the Right Primitive

When you reach for a concurrency primitive, ask two questions:

**Is this about transferring a value, or holding a value?**

If one task produces something and another consumes it — and especially if the sequence
matters — that is a transfer. Use a channel. If multiple tasks need to read or write the
same evolving value over time, that is persistent state. Use a shared variable.

**Does ordering matter?**

If task B must see task A's result *and know that A has finished*, a channel provides that
guarantee explicitly. Shared state does not order events — it only stores the latest write.
If you find yourself using a shared variable to signal readiness (`while flag == 0: sleep(10)`),
that is a polling loop around a synchronization problem that a channel would solve cleanly.

| Situation | Right primitive |
|-----------|-----------------|
| One task produces, one consumes | Channel |
| Multiple writers, no ordering needed | Shared variable |
| Signal that an event occurred | Channel (send `()`) |
| Persistent counter / config / cache | Shared variable |
| Rate limiting (N concurrent) | Buffered channel as semaphore |
| Complex invariants across multiple fields | Actor (channel only, no shared) |
| One writer, many readers | Shared variable + channel for updates |

---

## Looking Ahead

The patterns in this chapter are composable: a notification pattern can feed into an actor;
a work queue can use a semaphore to throttle; a config snapshot can drive a pipeline.
Gorget does not force one model on you — it gives you the primitives, enforces the safety
rules, and trusts you to combine them for your problem.

Two capabilities are planned that will make common patterns even more ergonomic:

- **`await_change(x)`** — a built-in that blocks until a shared variable's value changes,
  replacing the channel-as-notifier pattern for simple cases.
- **`actor` functions** — syntactic sugar that generates the channel infrastructure for
  Pattern 2 automatically, reducing the boilerplate of the request/response plumbing.

Both are deferred until the core language stabilizes. In the meantime, the patterns above
give you everything you need to write correct, efficient concurrent programs today.
