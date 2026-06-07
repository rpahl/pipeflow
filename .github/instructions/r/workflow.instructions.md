---
name: 'R Development Workflow'
description: 'R dev workflow, preflight checks, and command patterns'
applyTo: '**/*.R'
---

# R Development Workflow

## Prerequisites

- REQUIRED: Use the R version specified by `renv.lock`.
- REQUIRED: Run all R commands from repository root (`${workspaceFolder}`).
- REQUIRED: Source `.Rprofile` in every R session before running package code
  (`source(".Rprofile")`).
- NEVER use PATH R or PATH Rscript if their version differs from `renv.lock`.
- NEVER continue test/check execution if R version preflight fails.

## Preflight Before Tests/Checks

- Confirm active R version matches `renv.lock` (full version, `major.minor.patch`).
- Confirm `.Rprofile` has been sourced from repository root.
- Confirm `.libPaths()` points to project renv library and sandbox.

## Command Patterns

- Use explicit R executable when needed, for example:

  ```r
  <R_BINARY> -q -e "source('.Rprofile'); devtools::test()"
  ```

- For a single test file:

  ```r
  <R_BINARY> -q -e "source('.Rprofile'); devtools::test_active_file('<file path>')"
  ```

## Missing Packages

- Run `renv::restore(prompt = FALSE)`, then rerun the same command.
- Do not use global/system libraries for this repository.
