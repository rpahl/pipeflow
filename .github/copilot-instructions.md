# Copilot Instructions

## Goal
Maintain passing CI (lint, unit tests, R CMD check).
Automatically fix failing tests and re-run until green.

## Stack
R (≥4.2.2), Shiny, renv, testthat (edition 3), lintr, GitHub Actions.

## Code and Naming Conventions
- Use native pipe |>, line length ≤80 and indent of 4 spaces.
- Ideally, the body of a function/module fits within 50 lines and
  should rarely exceed 100 lines. If it does, consider refactoring.
- Document all functions with roxygen2 headers. The header should end
  with an @noRd tag unless they are @export ed.
- Maintain file naming consistent with existing structure.

## Test Practices
- Add tests for new logic immediately.
- Name tests: tests/testthat/test_<source>.R
- Individual files can be tested via devtools::test(filter = <source>.R).
- Use mockery for external deps.
- If a test fails: inspect, patch code or tests, and
  re-run devtools::test(filter = <source>.R).

#### Full check (= “green CI pipeline”)

When working on an issue, *before* creating a Pull Request,
you MUST follow these steps:

1) `lintr::lint_package()` — fix all lints.
2) `devtools::test()` — fix failing tests.
3) `rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')` — fix all errors/warnings.

Target in step 3: **0 errors / 0 warnings** (notes preferably 0).

If you are unable to fix all issues within **45 minutes**,
commit your latest changes, then stop, and do *not* open a PR.

## Commit Messages
Conventional commits (feat:, fix:, test:, refactor:, docs:).
