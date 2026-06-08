---
name: 'R Test Practices'
description: 'Unit testing conventions for R files'
applyTo: '**/*.R'
---

# Test Practices

- Add tests for new logic immediately.
- Name tests: `tests/testthat/test_<source>.R`
- Use `mockery` for external deps.
- If a test fails: inspect, patch code or tests, and rerun the relevant
  test command.
