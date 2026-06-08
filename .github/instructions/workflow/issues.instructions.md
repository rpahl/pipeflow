---
name: 'Issue Workflow & CI Pipeline'
description: 'Guidelines for working on GitHub issues and CI checks'
applyTo: '**/*'
---

# Working on an Issue

## Goal

Maintain passing CI (lint, unit tests, R CMD check).
Automatically fix failing tests and re-run until green.

### Escape Condition

- Stop after 3 consecutive failed fix attempts for the same failure signature,
  or 6 total fix attempts in one session.
- Stop immediately if the only available changes are hacky workarounds
  (for example: disabling tests, weakening assertions, or masking errors).
- When stopping, provide a concise summary of attempts, current blockers,
  and 1-3 recommended next steps instead of continuing to iterate.

## Full Check ("green CI pipeline")

When working on an issue, *before* creating a Pull Request,
you MUST follow these steps:

1. `lintr::lint_package()` — fix all lints.
2. `devtools::test()` — fix failing tests.
3. `rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')` — fix all
   errors/warnings.

Target in step 3: **0 errors / 0 warnings** (notes preferably 0).

If you are unable to fix all issues within **45 minutes**,
commit your latest changes, provide a concise summary of attempts and blockers,
then stop, and do *not* open a PR.
