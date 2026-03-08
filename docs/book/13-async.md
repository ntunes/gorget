# Tasks and Async/Await

Most programs need to do more than one thing at a time. A web server handles many
connections. A build tool compiles files in parallel. A game updates physics while
rendering frames. Gorget's concurrency model is built on three ideas: async functions
that can pause, tasks that run concurrently, and a set of rules the compiler enforces
to keep everything safe.

This chapter covers the mechanics. The next chapter — *[Concurrency](14-concurrency.md)* —
covers coordination patterns built on top of these primitives.

---

## Async Functions

An async function can suspend its execution and resume later. You mark it with `async`:

```gorget
async int fetch_count():
    int result = slow_database_query()
    return result
```

From the caller's perspective, the function returns a value like any other. Under the
hood, the compiler transforms it into a state machine that can pause at specific points
and yield control back to the runtime, letting other work proceed.

An async function looks exactly like a regular function, with one exception: it can
contain `await` expressions — points where execution might pause.

You can call async functions from other async functions. The entry point to your
concurrent program is typically an `async void main()`:

```gorget
async void main():
    int count = fetch_count()
    print("got {count}")
```

Wait — that call to `fetch_count()` looks synchronous. Where is the `await`? In Gorget,
calling an async function from another async function is a direct call — it runs to
completion before the caller continues. The function *can* suspend internally (at its
own `await` points), but the caller blocks until the result is ready.

To run things concurrently, you need tasks.

---

## Spawning Tasks

`spawn` launches an async function as an independent task. The current function continues
immediately — it does not wait for the spawned task to finish.

```gorget
async void download(str url):
    # ... fetch data from url ...
    print("done: {url}")

async void main():
    Task[void] t1 = spawn download("https://example.com/a")
    Task[void] t2 = spawn download("https://example.com/b")
    print("both started")
    t1.await()
    t2.await()
    print("both finished")
```

This prints `both started` first, then the two `done:` messages in whatever order the
downloads complete, then `both finished`. The two downloads run concurrently — neither
blocks the other.

`spawn` returns a `Task[T]`, where `T` is the async function's return type. You hold
onto this handle and `await` it later to get the result.

### What You Can Spawn

`spawn` accepts direct function calls and closures:

```gorget
spawn worker(42)                     # direct call — OK
spawn ((): print("hello"))()        # closure — OK

int x = 10
spawn ((): print(x))()             # closure capturing Copy type — OK
```

`spawn` does **not** accept method calls or indirect calls:

```gorget
spawn obj.method()                  # ERROR — method calls not supported
spawn get_fn()(x)                   # ERROR — indirect call
```

The restriction exists because the compiler needs to verify at the call site that all
arguments and captures are safe to send to another thread. Method calls and indirect
calls obscure this analysis.

### Spawn Is Not a Suspension Point

`spawn` returns immediately. The current function continues executing the very next
line. This matters for the borrow checker:

```gorget
async void example(str s):
    auto task = spawn some_fn()
    print(s)     # fine: spawn doesn't suspend, s is still live
```

Compare this with `await`, which *does* suspend — the distinction is important for
understanding which values remain valid (covered below).

---

## Awaiting Results

`.await()` is a suspension point: the current task pauses until the awaited operation
completes, then resumes with the result.

Gorget supports two equivalent syntaxes:

```gorget
# Prefix — makes the suspension point stand out
int result = await add_async(3, 4)

# Postfix — chains naturally with method calls
String body = http.get(url).await().text()
```

Both forms are identical in meaning. Use whichever reads better in context. You cannot
combine them — `await expr.await()` is a compile error.

### Getting a Value Back

When a spawned task returns a value, `.await()` gives you that value:

```gorget
async int compute(int x):
    return x * x

async void main():
    Task[int] t = spawn compute(7)
    # ... do other work ...
    int result = t.await()
    print(result)    # 49
```

For `Task[void]`, `.await()` simply waits for the task to finish.

### Multiple Tasks

You can spawn many tasks and await them all:

```gorget
async void main():
    Vector[Task[int]] tasks = Vector[Task[int]]()
    for i in 0..10:
        tasks.push(spawn compute(i))

    int total = 0
    for t in tasks:
        total += t.await()
    print(total)
```

Tasks run concurrently while you await them one by one. The total time is limited by the
slowest task, not the sum of all task times.

---

## Suspension-Point Safety

An `.await()` is a point where execution pauses. When it resumes, the task may be on a
different thread. This creates a problem: what if a local variable was borrowed before
the `await`, and the borrow is used after? The original variable's stack frame might be
gone.

Gorget's compiler prevents this class of bugs entirely. The rule is:

> **References to local variables may not live across an `.await()` point.**

The compiler checks every `.await()` and verifies that no borrowed reference from a local
variable is used afterward. Here's what that means in practice.

### What Can Cross an Await

**Owned types** — `String`, structs, enums, collections — own their data. The data moves
into the suspended state and comes back when the task resumes. Always safe.

```gorget
async void example():
    String name = String.from("Alice")
    some_task().await()
    print(name)     # fine: String owns its data
```

**Copy types** — `int`, `float`, `bool`, `char` — are trivially duplicated. No pointers
involved.

```gorget
async void example():
    int x = 42
    some_task().await()
    print(x)        # fine: int is Copy
```

**Static string literals** — point to program-global storage that never goes away.

```gorget
async void example():
    str msg = "hello"
    some_task().await()
    print(msg)      # fine: "hello" lives in static memory
```

**Parameters** — when you directly await an async function, the caller is blocked. Its
stack frame (and all its parameters) stays alive for the entire duration of the call.

```gorget
async void process(str name):
    some_task().await()
    print(name)     # fine: caller is blocked, name is live

async void main():
    str s = "world"
    process(s)      # direct call — main is blocked until process returns
```

### What Cannot Cross an Await

**`str` borrowed from a local `String`** — the `str` is a view into the `String`'s buffer.
If the task suspends, the `String` might not be at the same memory address on resume.

```gorget
async void broken():
    String owned = String.from("hello")
    str s = owned.as_str()          # s borrows from owned
    some_task().await()
    print(s)                        # ERROR: s borrows from local

async void fixed():
    String owned = String.from("hello")
    some_task().await()
    print(owned)                    # OK: use the owned String directly
```

**References to local variables** — same reasoning. A `&T` into a local doesn't survive
suspension.

The fix is always the same: use owned data, or restructure the code so the borrow doesn't
span the `await`.

### Spawn Is Stricter Than Await

`spawn` launches a task that runs independently — it may outlive the calling function.
This means even parameter borrows are not safe (unlike `await`, where the caller blocks):

```gorget
async void worker(str name):
    print(name)

void launch(str name):
    auto t = spawn worker(name)     # ERROR: name may not outlive launch()
```

The fix: pass owned data. If the spawned function needs a string, give it a `String`
instead of a `str`.

Closures follow the same rule — they can only capture owned or Copy types:

```gorget
int x = 42
spawn ((): print(x))()             # OK: x is Copy

str name = get_name()
spawn ((): print(name))()          # ERROR: name has borrowed origin
```

---

## The Task Lifecycle

A `Task[T]` represents a running concurrent computation. Its lifecycle is simple:

1. **Created** by `spawn` — the task starts running immediately.
2. **Running** — the runtime schedules it alongside other tasks.
3. **Completed** — the function returns. The result is stored in the task.
4. **Awaited** — the caller retrieves the result with `.await()`.

If you drop a `Task[T]` without awaiting it, Gorget **joins** the task — it blocks until
the task completes, then discards the result. This prevents fire-and-forget tasks from
being silently orphaned:

```gorget
async void main():
    auto t = spawn long_running()
    # t goes out of scope here — the runtime waits for it to finish
```

This is the safe default. If you truly want fire-and-forget, store the task somewhere
that outlives the current scope.

### Tasks and Named Scopes

Named scopes interact with task lifetimes: all tasks created inside a named scope are
joined before the scope exits.

```gorget
scope workers:
    Task[void] t1 = spawn process(data)
    Task[void] t2 = spawn process(data)
# t2 dropped, t1 dropped — both joined here, before outer code continues
```

This guarantees that borrowed data passed to the tasks remains valid for the tasks'
entire duration.

---

## Schedulers

Gorget's runtime includes a scheduler that decides how spawned tasks are executed. You
don't interact with the scheduler directly, but you can choose which one to use.

### Pool (Default)

```gorget
directive scheduler=pool
```

An M:N thread pool with work-stealing. Multiple tasks are multiplexed across a smaller
number of OS threads. This is the right choice for I/O-heavy programs with many
concurrent tasks — HTTP servers, crawlers, chat systems.

### Thread

```gorget
directive scheduler=thread
```

One OS thread per `spawn`. Simple and predictable. Good for CPU-bound work where each
task does heavy computation, for FFI code that uses thread-local state, or when debugging
concurrency issues (each task has its own stack trace).

### Inline

```gorget
directive scheduler=inline
```

Synchronous execution on the caller's thread. `spawn` runs the function immediately and
returns a completed task. This is useful for testing — your concurrent code runs
deterministically, making assertions reliable — and for platforms without threading
support (like WASM).

### Single

```gorget
directive scheduler=single
```

All tasks run cooperatively on a single thread, yielding at suspension points. Low
overhead, no thread synchronization. Suitable for embedded systems, scripting, and
programs where concurrency means interleaving, not parallelism.

### Choosing a Scheduler

You set the scheduler once — either in your source file with a `directive` or on the
command line:

```
gg run --scheduler=thread myprogram.gg
```

The CLI flag overrides the source directive. Most programs should use the default (`pool`)
unless you have a specific reason not to.

| Workload | Recommended scheduler |
|----------|----------------------|
| I/O-heavy, many connections | `pool` (default) |
| CPU-bound computation | `thread` |
| Tests, deterministic replay | `inline` |
| Embedded, scripting, WASM | `single` |

---

## Putting It Together

Here's a complete example that ties the concepts together — a parallel computation
that spawns workers, collects results, and handles suspension safely:

```gorget
async int fib(int n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

async void main():
    # Spawn tasks to compute fibonacci numbers in parallel
    Vector[Task[int]] tasks = Vector[Task[int]]()
    Vector[int] inputs = Vector[int](30, 31, 32, 33, 34)

    for n in inputs:
        tasks.push(spawn fib(n))

    # Collect results — each await blocks only until that specific task completes
    int i = 0
    for t in tasks:
        int result = t.await()
        print("fib({inputs.get(i)}) = {result}")
        i += 1
```

Five Fibonacci computations run concurrently. With the `pool` scheduler, they spread
across CPU cores. With `inline`, they run sequentially — same result, just slower. The
code is identical either way.

---

## Summary

| Concept | What it does |
|---------|-------------|
| `async` | Marks a function as suspendable |
| `spawn expr` | Launches a task, returns `Task[T]`, does not block |
| `expr.await()` | Suspends until the task completes, returns its result |
| `await expr` | Same as above, prefix form |
| `Task[T]` | Handle to a running task; joined on drop |
| Suspension safety | No local borrows across `await`; no borrows into `spawn` |
| Schedulers | `pool` (default), `thread`, `inline`, `single` |

The next chapter — *[Concurrency](14-concurrency.md)* — builds on these primitives to
show how tasks communicate and coordinate: channels for transferring values, shared
variables for persistent state, and the patterns that make concurrent programs correct.
