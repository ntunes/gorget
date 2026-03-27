# Testing

Gorget has built-in testing — no framework to choose, no dependencies to add.
Write tests in the same file as your code, run them with `gg test`.

---

## Test Blocks

A test is a named block that runs assertions:

```gorget
test "addition works":
    assert 1 + 1 == 2

test "string equality":
    auto s = "hello"
    assert s == "hello"
```

Run with:

```bash
gg test myfile.gg
```

Output:

```
Running 2 tests...
  test: addition works ... PASS (0ms)
  test: string equality ... PASS (0ms)

2 passed, 0 failed (0ms)
```

Each test runs independently. A failing assertion doesn't stop other tests from
running.

---

## Assertions

```gorget
assert true
assert 2 + 2 == 4
assert x > 0, "x must be positive"
assert items.len() == 3, "expected 3 items, got {items.len()}"
```

The optional second argument is an error message. Without it, the compiler
generates a message showing the failed expression.

Assertions run in all builds by default — debug and release. If a condition is
worth checking, it's worth checking in production. To strip them:

```bash
gg build app.gg --strip-asserts
```

Or per-file:

```gorget
directive strip-asserts
```

---

## Test Resources

Tests can use scoped resources with `with` blocks:

```gorget
test "reads file":
    with File.open("data.txt") as f:
        auto content = f.read_all().unwrap()
        assert content == "expected"

test "multiple resources":
    with Resource("a") as a:
        with Resource("b") as b:
            assert a.name == "a"
            assert b.name == "b"
```

Resources implementing `Drop` are cleaned up on both success and failure paths.

---

## Suite Setup and Teardown

Run code before or after all tests in a file:

```gorget
suite setup:
    print("initializing test database")

suite teardown:
    print("cleaning up test database")

test "query works":
    assert true

test "insert works":
    assert true
```

`suite setup` runs once before all tests. `suite teardown` runs once after all
tests complete. At most one of each per file.

---

## Expecting Panics

Use `@should_panic` for tests that are supposed to fail:

```gorget
@should_panic
test "out of bounds panics":
    auto v = [1, 2, 3]
    auto x = v[10]       # panics

@should_panic("index out of bounds")
test "panic message matches":
    auto v = [1, 2, 3]
    auto x = v[10]
```

The test passes if it panics. With a string argument, the panic message must
contain that substring.

---

## Tag Filtering

Organize tests with tags:

```gorget
@tag("smoke")
test "quick check":
    assert true

@tag("slow")
test "integration test":
    # long-running test
    assert true

@tag("smoke")
@tag("core")
test "important check":
    assert true
```

Run only tagged tests:

```bash
gg test app.gg --tag smoke              # only @tag("smoke") tests
gg test app.gg --exclude-tag slow       # skip @tag("slow") tests
```

---

## Name Filtering

Run tests matching a substring:

```bash
gg test app.gg --filter "query"         # tests with "query" in the name
```

---

## HTML Reports

Generate a visual test report:

```bash
gg test app.gg --report html
```

This produces `app.report.html` with detailed results, timing, and trace
information.

---

## Skipping Tests

Use `@skip` to temporarily disable a test:

```gorget
@skip
test "not ready yet":
    assert false

@skip("blocked on #42")
test "needs fix":
    assert false
```

Skipped tests appear in output but don't execute:

```
  test: not ready yet ... SKIP
  test: needs fix ... SKIP (blocked on #42)
```

---

## Timeouts

Set a per-test timeout with `@timeout`:

```gorget
@timeout("500")
test "must be fast":
    auto result = compute()
    assert result > 0

@timeout("5000")
test "allowed to be slow":
    auto data = load_large_dataset()
    assert data.len() > 0
```

If the test exceeds its timeout, it's interrupted and reported as a failure:

```
  test: must be fast ... FAIL: timed out after 500ms
```

Set a global timeout from the CLI:

```bash
gg test app.gg --timeout 5s       # 5 seconds
gg test app.gg --timeout 500ms    # 500 milliseconds
gg test app.gg --timeout 5000     # bare number = milliseconds
```

Per-test `@timeout` overrides the global default.

---

## Parallel Execution

Run tests across multiple worker processes:

```bash
gg test app.gg --parallel 4
```

Each worker runs a subset of the tests. Results are merged at the end.

---

## Re-run Strategies

After a test run, results are saved to `.gorget/<stem>.test-results.json`.
Use these results to focus subsequent runs:

```bash
gg test app.gg --failed-only     # only re-run previously failed tests
gg test app.gg --failed-first    # run failed tests first, then the rest
```

`--failed-only` is useful for quick iteration on broken tests.
`--failed-first` gives fast feedback while still running the full suite.

---

## Snapshots

The `snapshot` statement captures a value during a test run for later comparison:

```gorget
test "arithmetic":
    int x = 2 + 3
    snapshot "result" x
    snapshot "doubled" x * 2

test "config":
    String greeting = "hello world"
    snapshot "greeting" greeting
```

Snapshots are pure capture — they don't assert anything. Comparison happens
through the CLI:

```bash
gg test app.gg --snapshot save "v1"          # run tests, save captures as "v1"
gg test app.gg --snapshot save "v2"          # run again, save as "v2"
gg test app.gg --snapshot diff "v1" "v2"     # compare two versions
gg test app.gg --snapshot list               # list saved versions
gg test app.gg --snapshot show "v1"          # print a version's contents
gg test app.gg --snapshot delete "v1"        # remove a version
```

**Rules:**

- `snapshot` is only valid inside `test` blocks.
- The expression must be a primitive (`int`, `float`, `String`, `bool`) or implement
  `Serializable`.
- Snapshots are stored in `.gorget/snapshots/<file_stem>/<version>.json`.

**Diff output** shows field-level changes:

```
Snapshot diff: v1 vs v2

  test "arithmetic":
    "result": 5 -> 7
    "doubled": 10 -> 14

1 test(s) changed, 1 test(s) unchanged
```

`--snapshot diff` exits 0 if identical, 1 if different — suitable for CI.

---

## Benchmarks

Use `bench` blocks to measure performance:

```gorget
bench "fibonacci":
    auto result = fib(30)
```

Run with:

```bash
gg test app.gg --bench
```

The runner automatically warms up (3 iterations), then calibrates iteration
count (starting at 100, doubling until >= 1 second), and reports per-iteration
timing.

---

## Tracing

Enable detailed execution tracing:

```bash
gg test app.gg --trace
```

Or per-file:

```gorget
directive trace
```

This writes `app.trace.jsonl` with function entries, exits, variable assignments,
and loop iterations. Useful for debugging test failures.

Generate a report from a trace:

```bash
gg report app.trace.jsonl
```

---

## Output Capture

By default, `print()` output from test bodies is captured and only shown for
failing tests. Passing tests produce no output noise:

```
Running 3 tests...
  test: setup works ... PASS (0ms)
  test: validation fails ... FAIL: assertion failed: left == right
  left:  1
  right: 2 (0ms)
    --- captured output ---
debug: checking value
debug: value was 42
    ---
  test: cleanup works ... PASS (0ms)
```

To see all output (including from passing tests), use `--nocapture`:

```bash
gg test app.gg --nocapture
```

Suite setup and teardown output is never captured — it prints directly.

---

## Test Discovery

Pass a directory instead of a file to discover and run all test files recursively:

```bash
gg test .                            # all test files under cwd
gg test tests/                       # all test files under tests/
gg test src/ --tag smoke             # with flags — forwarded to each file
```

Discovery rules:
- Finds `.gg` files containing `test "` blocks (or `bench "` with `--bench`)
- Recurses into subdirectories, skipping hidden directories (`.gorget/`, `.git/`, etc.)
- Files are run in alphabetical order for deterministic results
- Files where no tests match the current filters are silently skipped

Output groups results by file, then prints an aggregate summary:

```
--- math.gg ---
  test: addition ... PASS (0ms)
  test: division ... PASS (0ms)
--- strings.gg ---
  test: concat ... PASS (0ms)

3 passed, 0 failed (2 file(s))
```

All flags work in directory mode: `--filter`, `--tag`, `--exclude-tag`, `--timeout`,
`--parallel`, `--failed-only`, `--failed-first`, `--bench`, `--report html`.

---

## Summary

| Feature | Syntax | Example |
|---------|--------|---------|
| Test block | `test "name": body` | `test "add": assert 1+1==2` |
| Assertion | `assert expr [, msg]` | `assert x > 0, "positive"` |
| Resources | `with expr as x:` in test body | Scoped cleanup |
| Setup | `suite setup: body` | Runs before all tests |
| Teardown | `suite teardown: body` | Runs after all tests |
| Expect panic | `@should_panic` | Test must panic to pass |
| Skip | `@skip` / `@skip("reason")` | Disable a test |
| Timeout | `@timeout("ms")` / `--timeout` | Interrupt slow tests |
| Tags | `@tag("name")` | Categorize tests |
| Filter | `--filter "pattern"` | Run matching tests |
| Capture | default on / `--nocapture` | Show output only on failure |
| Discovery | `gg test <dir>` | Run all test files in directory |
| Parallel | `--parallel N` | Multi-process execution |
| Re-run | `--failed-only` / `--failed-first` | Focus on failures |
| Snapshot | `snapshot "name" expr` | Capture values for diffing |
| Benchmark | `bench "name": body` | Measure performance |
| HTML report | `--report html` | Visual test report |
