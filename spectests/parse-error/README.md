# `spectests/parse-error/` — parse-rejection fixtures

`mode: parse-error` fixtures: source a conforming implementation MUST reject at
the parser. The `expect:` block carries the parser diagnostic.

Expectation provenance (RFC §4): `parse-error/` expectations derive from the
**shared production parser** (trusted-declared — an inversion in name only, since
`ggdef` shares that exact parser; stated for honesty).

**Empty in phase 0.**
