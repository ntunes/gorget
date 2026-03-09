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

Tests can declare scoped resources with `with`:

```gorget
test "reads file" with File.open("data.txt") as f:
    auto content = f.read_all().unwrap()
    assert content == "expected"

test "multiple resources" with Resource("a") as a, Resource("b") as b:
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

## Summary

| Feature | Syntax | Example |
|---------|--------|---------|
| Test block | `test "name": body` | `test "add": assert 1+1==2` |
| Assertion | `assert expr [, msg]` | `assert x > 0, "positive"` |
| Resources | `test "name" with expr as x:` | Scoped cleanup |
| Setup | `suite setup: body` | Runs before all tests |
| Teardown | `suite teardown: body` | Runs after all tests |
| Expect panic | `@should_panic` | Test must panic to pass |
| Tags | `@tag("name")` | Categorize tests |
| Filter | `--filter "pattern"` | Run matching tests |
| HTML report | `--report html` | Visual test report |
