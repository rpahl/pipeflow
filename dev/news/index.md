# Changelog

## pipeflow 0.5.0.9001

### Documentation

- New hexagon logo with the package name, used in the README and as
  pkgdown logo and favicons. The source is `logo/logo.svg`.
- New two-page cheat sheet (`cheatsheet/pipeflow-cheatsheet.pdf`), shown
  in the README between “Installation” and “Usage”.
- Shorter README introduction and “Why pipeflow?” list.
- The pkgdown site is centered again, and its home page shows the logo
  left of the package name.

## pipeflow 0.5.0

CRAN release: 2026-10-13

### Breaking changes

- `v[["view"]]` returns the names of the covered steps instead of row
  indices.
- `.self` is only made available in step functions that refer to it,
  i.e. if the name `.self` appears in the function body.
- [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  by default now keeps the tags and the execution mode of the replaced
  step.

### New features

- `pip_run(on_error = "continue")` continues the run of unaffected steps
  after a step fails with an error.
- allow mitigation of inputs from failed steps via `~try()` references
  (`x = ~try(step)`), e.g. by creating fallback results for failed
  steps.
- [`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
  is a generic for unwrapping parameter values, enabling users to set
  pipeline parameters that consist of non-trivial types or classes.
- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  gains `unknown = c("warn", "ignore", "error")` for parameters that no
  step defines, and `force = TRUE` to outdate steps even if the values
  are identical.
- [`pip_run_state()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run_state.md)
  returns the run state of a pipeline or view.
- The default logger
  [`pipeflow_lgr()`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow_lgr.md)
  is exported, so custom loggers can build on it.
- Pipelines survive [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
  and [`readRDS()`](https://rdrr.io/r/base/readRDS.html), and can be
  sent to other R processes (see
  [`?pip_new`](https://rpahl.github.io/pipeflow/dev/reference/pip_new.md)).

### Changes

- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  no longer outdates steps if the values are identical to the stored
  ones.
- Errors from
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  name the step (`step 'fit': ...`) and have class `pipeflow_step_error`
  with the fields `step` and `parent`. A failing partition key gives a
  `pipeflow_key_error` with `key` and `parent`.
- Views keep the name of their pipeline (before: `"<name> view"`).
- Views keep track of their steps instead of row numbers, so they stay
  valid when steps are added, removed, renamed or replaced.
- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  also re-runs a `"done"` step if one of its inputs was executed in the
  same run.
- A run stopped by `halt()` ends in run state `"halted"` (before:
  `"ready"`).
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  warn if `params` contain names that are also arguments of `fun`.
- [`print()`](https://rdrr.io/r/base/print.html) now shows conditions,
  matrices, arrays, tables, functions and environments in the `out`
  column in short form, e.g. `<error/simpleError>`.

### Bug fixes

- Steps that never ran stay `"new"`, and a run only outdates the steps
  downstream of steps it executed.
- `x[[step, j]]` on a view read the wrong row.
- Copies made by
  [`pip_clone()`](https://rpahl.github.io/pipeflow/dev/reference/pip_clone.md),
  `p[]` or `p[i, view = FALSE]` shared `.self` with the original.
- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  no longer warns about parameters defined only in locked steps.
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  reject primitive functions such as `sum` with a clear error, and
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  calls `.self` a reserved *parameter* name.
- A step that references the same step more than once no longer warns
  about an existing edge.

### Documentation

- New vignettes “Handling failures” and “Annotated parameter values”.
- “Get started”, “Nested pipelines”, “Self-modifying pipelines” and
  “Combine pipelines” cover identical values, `unknown = "ignore"`, when
  `.self` is available, and saving pipelines.
- The benchmark vignette was revised.
- The package website is built by a GitHub Actions workflow on release
  tags.

## pipeflow 0.4.0

CRAN release: 2026-09-27

### New features

- Pipeline functions can be called as methods on the pipeline object,
  e.g. `p$add(...)`, `p$run(...)`, `p$set_params(...)`, …
- The extract and replace operators gained new capabilities:
  - two-index extraction (`p[[step, "out"]]`),
  - negative row indices in `p[...]` and `p[i, j] <- value`,
  - boolean filters in `p[...]` (data.table-style expressions),
  - cross-pipeline assignment to copy steps between pipelines,
  - step removal via `p[[step, "step"]] <- NULL`,
  - time stamps via `p[[step, "time"]] <- value`,
  - setting or replacing views via `p[["view"]] <- ...`.
- [`pip_collect()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
  (formerly
  [`pip_collect_out()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md))
  supports grouping via a `by` argument, `as.table = TRUE` to return a
  compact table, and a `simplify` argument to control flattening of
  single-step groups.
- [`pip_data()`](https://rpahl.github.io/pipeflow/dev/reference/pip_data.md)
  provides direct access to the underlying step table.
- [`pip_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md)
  (formerly
  [`pip_get_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md))
  returns visNetwork-compatible graph data. The old function names
  remain available as deprecated aliases.
- The derived columns `depends`, `unbound` and `nodeId` are read-only
  and can’t be assigned directly.
- [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  gains a `join = "union"` argument to combine filters as a logical OR.
- `.self` is now automatically available inside every step function (and
  is a protected parameter name), enabling pipelines to modify
  themselves at runtime, including `.self$restart()` and `.self$halt()`.

### Changes

- The columns of the internal step table were reordered and `nodeId` is
  now shown in the full print view (`cols = "all"`).
- The dependencies on `lgr` and `jsonlite` were removed.

### Bug fixes

- [`pip_rename()`](https://rpahl.github.io/pipeflow/dev/reference/pip_rename.md)
  no longer corrupts the parameters of single-step pipelines.
- Running a view now resolves upstream dependencies correctly.

### Documentation

- New vignettes: “Working with pipeline views” and “Nested pipelines”.
- Help pages of related functions were consolidated (extract/replace
  operators, `dim`/`length`, `lock`/`unlock`, `tag`/`untag`,
  `set_params`/`get_params`).
- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  documents the runtime control flow (`restart()`, `halt()`), and
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  gained an example for the `after` argument.

## pipeflow 0.3.0

CRAN release: 2026-06-15

### New features

- **C++ DAG engine** (`src/dag.cpp`): All graph operations (node/edge
  add/remove, topological ordering, reachability queries) run at C++
  speed via Rcpp, eliminating R object overhead for dependency
  resolution.
- **New `pip_*` API**: Functional API replacing the R6 `Pipeline` class
  —
  [`pip_new()`](https://rpahl.github.io/pipeflow/dev/reference/pip_new.md),
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md),
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md),
  [`pip_clone()`](https://rpahl.github.io/pipeflow/dev/reference/pip_clone.md),
  `pip_bind()`,
  [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md),
  [`pip_tag()`](https://rpahl.github.io/pipeflow/dev/reference/pip_tag.md)/[`pip_untag()`](https://rpahl.github.io/pipeflow/dev/reference/pip_tag.md),
  etc.
- **Execution modes** (`auto`/`split`/`reduce`/`plain`): Native support
  for map-reduce style workflows where steps split output into named
  partitions and downstream steps auto-map over them.
- **Dependency validation at definition time**:
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md),
  and
  [`pip_remove()`](https://rpahl.github.io/pipeflow/dev/reference/pip_remove.md)
  fail fast on broken references.
- New pkgdown-only article *pipeflow vs targets*
  (`vignettes/articles/v07-vs-targets.Rmd`) with an 18-row feature
  comparison table and benchmarks.

### Breaking changes

- Legacy `pipe_*` functions and `Pipeline` R6 class are deprecated
  (preserved as aliases in `R/aliases.R`, `R/pipelineR6.R`).
- [`pip_collect_out()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
  no longer accepts `grouped` or `by` parameters. It returns a flat
  named list of step outputs. Use
  [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  with tags and manual list composition for grouped output.
- The `group` column has been removed from the pipeline data.table.
  Steps previously using `group = "..."` in
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  should use `tags = "..."`.

### Documentation

- Comprehensive revision of all vignettes to match the new `pip_*` API.
- Revised `vignettes/v04-collect-output.Rmd` to demonstrate tag-based
  grouping via
  [`pip_view()`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  composition.
- Updated README with a “Why use {pipeflow}” feature list, a short usage
  example, and a revised vs-targets summary.

### Internal

- Extensive test suite covering C++ DAG, `pip_*` API, aliases, execution
  modes, and recursive runs.
- MIT license, updated CI workflows, dependabot config, kilo.json,
  lintr/styler config.

## pipeflow 0.2.3

CRAN release: 2025-07-26

- Add Depends R \>= 4.2.0 to DESCRIPTION
- Fix issue caused by update of lgr package
  ([\#571](https://github.com/rpahl/pipeflow/issues/571)e8260)
- Fix badge links in README

## pipeflow 0.2.2

CRAN release: 2024-12-22

- Add News and this Changelog
- Add unit tests and detailed documentation for [alias
  functions](https://rpahl.github.io/pipeflow/reference/index.html#alias-functions)
  ([\#24](https://github.com/rpahl/pipeflow/issues/24))
- Link to other packges via [my R
  universe](https://rpahl.r-universe.dev/packages)
  ([\#25](https://github.com/rpahl/pipeflow/issues/25))

## pipeflow 0.2.1

CRAN release: 2024-12-05

- CRAN release
