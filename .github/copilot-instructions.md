# Copilot Instructions

## Code and Naming Conventions
- Use native pipe |>, line length ≤80 and indent of 2 or 4 spaces.
- Document all functions with roxygen2 headers. The header should end
  with an @noRd tag unless they are @export ed.
- Maintain consistent mod_/utils_/app_ file naming.

## R Development Workflow running locally
- NEVER run bare `R` or `Rscript` commands without having first setup the
  environment as follows:
  - ALWAYS run commands with working directory set to repository root (`${workspaceFolder}`).
  - ALWAYS source the .Rprofile file to activate the renv environment (`source(".Rprofile")`).
- Before tests/checks, run this preflight:
- Use the R version specified by renv.lock.
- Start R from the repository root so .Rprofile sources renv/activate.R.
- Confirm .libPaths() points to the project renv library and sandbox.
- For all tests use devtools::test().
- For a single test file under tests/testthat use
  devtools::test_active_file(<file path>).
- If a package is missing, run renv::restore(prompt = FALSE), then rerun.
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

### Full check (= “green CI pipeline”)

When working on an issue, *before* creating a Pull Request,
you MUST follow these steps:

1) `lintr::lint_package()` — fix all lints.
2) `devtools::test()` — fix failing tests.
3) `rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')` — fix all errors/warnings.

Target in step 3: **0 errors / 0 warnings** (notes preferably 0).

If you are unable to fix all issues within **45 minutes**,
commit your latest changes, then stop, and do *not* open a PR.

### Commit Messages
Conventional commits (feat:, fix:, test:, refactor:, docs:).
