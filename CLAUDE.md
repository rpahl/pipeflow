# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

`pipeflow` is an R package (CRAN) for building interactive data analysis
pipelines. Users add R functions as steps with `pip_add()`, and the
dependencies between steps live in a C++ DAG (Rcpp).

More detailed agent instructions are in `.github/instructions/**` (also
referenced by `kilo.json`). The important parts are summarized below.

## Environment

- Dependencies are pinned with `renv` (`renv.lock`, which requires
  R 4.5.3). `renv/` and `.Rprofile` are gitignored, so a fresh worktree may
  not have them. The R on PATH may be a different version. Check
  `R --version` before running tests or checks, and don't run them on a
  mismatched R.
- Run R from the repo root. If `.Rprofile` exists, call `source(".Rprofile")`
  first so the renv library is active. If packages are missing, run
  `renv::restore(prompt = FALSE)`. Don't install into global or system
  libraries.

## Commands

```sh
Rscript -e "lintr::lint_package()"                 # lint (CI fails on any lint)
Rscript -e "devtools::test()"                      # all tests
Rscript -e "devtools::test(filter = 'pipeflow')"   # one file: tests/testthat/test_pipeflow.R
Rscript -e "devtools::test_active_file('tests/testthat/test_utils.R')"
Rscript -e "devtools::document()"                  # regenerate man/ + NAMESPACE from roxygen
Rscript -e "Rcpp::compileAttributes()"             # after changing // [[Rcpp::export]] in src/
Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual', error_on='warning')"
Rscript -e "devtools::build_readme()"              # README.md is generated from README.Rmd
```

Before opening a PR, run the full check in this order: lint, then tests,
then `rcmdcheck`. The target is 0 errors and 0 warnings, with 0 notes if
possible. CI (`.github/workflows/ci.yaml`) runs lint, then R CMD check on
Ubuntu, Windows and macOS, then covr. If the same failure is still there
after 3 fix attempts (or after 6 attempts in total), stop and summarize
instead of weakening tests or masking errors.

## Architecture

**Pipeline object.** A `pipeflow` is a small S3 list:
`list(name, view, pipenv)`. All state lives in `pipenv`, an environment that
`pip_new()` builds and that is shared by reference:

- `data` is a `data.table` with one row per step. Its columns are `step`,
  `fun`, `params`, `out`, `state`, `tags`, `locked`, `exec`, `time`,
  `depends`, `unbound` and `nodeId` (see `.empty_pipeline()` in
  `R/pipeflow.R`). `depends`, `unbound` and `nodeId` are derived columns
  (`.derived_cols`) and can't be assigned directly.
- `.dag` is an external pointer to the C++ `Dag` (`src/dag.cpp`).
  `.steps_to_nodes` is a hash env that maps step names to DAG node ids.
- `.run_state` (`ready`/`restart`/`running`/`halted`/`failed`/`continued`),
  `.last_run` and the restart counters hold the run state.

Because `pipenv` is a reference, functions mostly mutate it in place (often
with `data.table::set` or `:=`) and return `x` invisibly. `pip_clone()` or
`p[]` makes a real copy, and that copy includes the DAG via `dag_clone()`.

**Views.** A view is a `pipeflow` whose `view` field holds row indices.
Views come from `pip_view()` or `p[i]` and share the same `pipenv`.
Structural operations call `.assert_pip()` and reject views. Read and run
operations call `.assert_pip_or_view()`. Running a view also runs the
upstream steps it depends on, then marks unprocessed downstream steps as
`outdated`. `p[i, view = FALSE]` builds a standalone, compacted pipeline
(`.pip_compact()` in `R/generic.pipeflow.R`).

**Step dependencies.** Step functions must give every argument a default
value. If a default is a formula, it refers to another step: `x = ~step1`
by name, or `x = ~-1` by relative position. `.extract_depends()` and
`formula_deps()` resolve these references into `depends` and DAG edges. The
pipeline is checked when steps are added, not when it runs. A reference
wrapped in `try()` (`x = ~try(step1)`, also `~try(-1)`) marks an argument
that may receive a failed input in a continuing run (see Execution). The
marker lives in the step's `params`, read via `formula_try_args()`; rename
and `rbind()` rebuild reference formulas and must keep the `try()` wrapper.

**C++ DAG (`src/dag.cpp`).** Nodes are stored by a running id. A separate
`nodes_order` vector stores the topological order, so inserting a step only
edits that vector. Removed nodes are tombstoned (`alive = false`) and later
cleaned up by `dag_tidy_up`/`dag_rebuild`. R calls it through the generated
`R/RcppExports.R` and `src/RcppExports.cpp`. Don't edit those two files by
hand. `tests/testthat/test_RcppExports.R` tests the DAG directly.

**Execution.** `pip_run()` loops over rows in order. It skips steps that are
`done` (unless `force`) and steps that are locked, then calls `.pip_run_row()`
and `.pip_execute_step_call()`. Step states are `new`, `done`, `outdated` and
`failed` (`.step_states` in `R/pipeflow-package.R`). Each step's `exec` mode
controls split/map/reduce:

- `split` marks the output as `pipeflow_partitioned` (a named list).
- `auto` maps the step over the partition keys when an input is
  partitioned.
- `reduce` combines a partitioned input in a single call.
- `plain` forbids partitioned inputs.

With `pip_run(on_error = "continue")`, a failing step gets state `failed`
(its condition is stored as `out`) and the run goes on. Steps with a failed
input are skipped (they stay `new` or become `outdated`), unless that
argument is a `~try()` reference: then the step runs and receives a
`pipeflow_failure` condition. A run with failures ends in run state
`continued` and signals a `pipeflow_run_failed` warning.

**Self-modifying pipelines.** Inside a step body, `.self` refers to the full
pipeline (it is injected via `.wrap_self()` and by `.pip_run_row()`). A step
can call `.pip_restart()` or `.pip_halt()`. On restart, `pip_run()` calls
itself recursively and hands off the remaining work. After each step,
`.pip_run_row()` re-reads `pipenv$data` by step name, because the step may
have changed the pipeline.

**Code layout.**
- `R/pipeflow.R` holds the exported `pip_*` API, with internal `.pip_*`
  helpers at the top.
- `R/generic.pipeflow.R` holds the S3 methods: `[`, `[[`, `$`, their
  assignment forms, `rbind` (pipeline binding), `length`, `dim` and `str`.
- `R/print.pipeflow.R` holds the print method.
- `R/log.R` holds the default logger, `pipeflow_lgr(level, msg)`.
- The package imports all of `data.table` and re-exports some of its
  operators (`%like%`, `%between%`, …) for `[` filters.

## Conventions

- Use the native pipe `|>`, a 4-space indent and lines of at most 80
  characters (`.lintr`, `air.toml`). Local variables are camelCase. Internal
  helpers start with `.`, and exported functions start with `pip_`.
- Every function has a roxygen header. Unexported functions end the header
  with `@noRd`. `NAMESPACE` and `man/` are generated by roxygen, so don't
  edit them by hand.
- Most source files `R/<x>.R` have tests in `tests/testthat/test_<x>.R`
  (the DAG is tested in `test_RcppExports.R`; testthat edition 3, run in
  parallel). Use `mockery` for external
  dependencies. Helpers live in `tests/testthat/helper_*.R`.
- Vignettes (`vignettes/v0*.Rmd`, plus `vignettes/articles/` for
  pkgdown-only articles) use descriptive chunk labels and a `knitr-setup`
  chunk with `comment = "#"`. Their YAML follows the template in
  `.github/instructions/r/rmarkdown.instructions.md`.
- Commit messages use conventional commits: `feat:`, `fix:`, `test:`,
  `refactor:`, `docs:`. Note user-facing changes in `NEWS.md`.
