# Copilot Instructions

## Code and Naming Conventions
- Use native pipe |>, line length ≤80 and indent of 4 spaces.
- Document all functions with roxygen2 headers. The header should end
  with an @noRd tag unless they are @export ed.
- Maintain consistent mod_/utils_/app_ file naming.

## R Development Workflow running locally
- REQUIRED: Use the R version specified by renv.lock.
- REQUIRED: Run all R commands from repository root (${workspaceFolder}).
- REQUIRED: Source .Rprofile in every R session before running package code
  (source(".Rprofile")).
- NEVER use PATH R or PATH Rscript if their version differs from renv.lock.
- NEVER continue test/check execution if R version preflight fails.

Before tests/checks, run this preflight:
- Confirm active R version matches renv.lock (full version, major.minor.patch).
- Confirm .Rprofile has been sourced from repository root.
- Confirm .libPaths() points to project renv library and sandbox.

Command pattern for this repository:
- Use explicit R executable when needed, for example:
  <R_BINARY> -q -e "source('.Rprofile'); devtools::test()"
- For a single test file:
  <R_BINARY> -q -e "source('.Rprofile'); devtools::test_active_file('<file path>')"

If a package is missing:
- Run renv::restore(prompt = FALSE), then rerun the same command.
- Do not use global/system libraries for this repository.

## Test Practices
- Add tests for new logic immediately.
- Name tests: tests/testthat/test_<source>.R
- Use mockery for external deps.
- If a test fails: inspect, patch code or tests, and rerun the relevant
  test command.


## Working on an issue at github

### Goal
Maintain passing CI (lint, unit tests, R CMD check).
Automatically fix failing tests and re-run until green.
Escape condition for iterative fixes:
- Stop after 3 consecutive failed fix attempts for the same failure signature,
  or 6 total fix attempts in one session.
- Stop immediately if the only available changes are hacky workarounds
  (for example: disabling tests, weakening assertions, or masking errors).
- When stopping, provide a concise summary of attempts, current blockers,
  and 1-3 recommended next steps instead of continuing to iterate.

### Full check (= “green CI pipeline”)

When working on an issue, *before* creating a Pull Request,
you MUST follow these steps:

1) `lintr::lint_package()` — fix all lints.
2) `devtools::test()` — fix failing tests.
3) `rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')` — fix all errors/warnings.

Target in step 3: **0 errors / 0 warnings** (notes preferably 0).

If you are unable to fix all issues within **45 minutes**,
commit your latest changes, provide a concise summary of attempts and blockers,
then stop, and do *not* open a PR.

### Commit Messages
Conventional commits (feat:, fix:, test:, refactor:, docs:).
