# pipeflow 0.5.0

## Breaking changes

- `v[["view"]]` returns the names of the covered steps instead of row
  indices.
- `.self` is only made available in step functions that refer to it, i.e.
  if the name `.self` appears in the function body.
- `pip_replace()` by default now keeps the tags and the execution mode of
  the replaced step.

## New features

- `pip_run(on_error = "continue")` continues the run of unaffected steps
  after a step to failed with an error.
- `~try()` references (`x = ~try(step)`) allows to mitigate inputs from
  failed steps to e.g. create a temporary fallback result for failed steps.
- `pip_param_value()` is a generic for unwrapping parameter values, which
  pipeline parameters to be constructed as non-trivial types or classes.
- `pip_set_params()` gains `unknown = c("warn", "ignore", "error")` for
  parameters that no step defines, and `force = TRUE` to outdate steps even
  if the values are identical.
- `pip_run_state()` returns the run state of a pipeline or view.
- The default logger `pipeflow_lgr()` is exported, so custom loggers can
  build on it.
- Pipelines survive `saveRDS()` and `readRDS()`, and can be sent to other R
  processes (see `?pip_new`).

## Changes

- `pip_set_params()` no longer outdates steps if the values are identical to
  the stored ones.
- Errors from `pip_run()` name the step (`step 'fit': ...`) and have class
  `pipeflow_step_error` with the fields `step` and `parent`. A failing
  partition key gives a `pipeflow_key_error` with `key` and `parent`.
- Views keep the name of their pipeline (before: `"<name> view"`).
- Views keep track of their steps instead of row numbers, so they stay valid
  when steps are added, removed, renamed or replaced.
- `pip_run()` also re-runs a `"done"` step if one of its inputs was executed
  in the same run.
- A run stopped by `halt()` ends in run state `"halted"` (before: `"ready"`).
- `pip_add()` and `pip_replace()` warn if `params` contain names that are
  also arguments of `fun`.
- `print()` now shows conditions, matrices, arrays, tables, functions and
  environments in the `out` column in short form, e.g. `<error/simpleError>`.

## Bug fixes

- Steps that never ran stay `"new"`, and a run only outdates the steps
  downstream of steps it executed.
- `x[[step, j]]` on a view read the wrong row.
- Copies made by `pip_clone()`, `p[]` or `p[i, view = FALSE]` shared `.self`
  with the original.
- `pip_set_params()` no longer warns about parameters defined only in
  locked steps.
- `pip_add()` and `pip_replace()` reject primitive functions such as `sum`
  with a clear error, and `pip_add()` calls `.self` a reserved *parameter*
  name.
- A step that references the same step more than once no longer warns about
  an existing edge.

## Documentation

- New vignettes "Handling failures" and "Annotated parameter values".
- "Get started", "Nested pipelines", "Self-modifying pipelines" and
  "Combine pipelines" cover identical values, `unknown = "ignore"`, when
  `.self` is available, and saving pipelines.
- The benchmark vignette was revised.
- The package website is built by a GitHub Actions workflow on release tags.


# pipeflow 0.4.0

## New features

- Pipeline functions can be called as methods on the pipeline object,
  e.g. `p$add(...)`, `p$run(...)`, `p$set_params(...)`, ...
- The extract and replace operators gained new capabilities:
  - two-index extraction (`p[[step, "out"]]`),
  - negative row indices in `p[...]` and `p[i, j] <- value`,
  - boolean filters in `p[...]` (data.table-style expressions),
  - cross-pipeline assignment to copy steps between pipelines,
  - step removal via `p[[step, "step"]] <- NULL`,
  - time stamps via `p[[step, "time"]] <- value`,
  - setting or replacing views via `p[["view"]] <- ...`.
- `pip_collect()` (formerly `pip_collect_out()`) supports grouping via a
  `by` argument, `as.table = TRUE` to return a compact table, and a
  `simplify` argument to control flattening of single-step groups.
- `pip_data()` provides direct access to the underlying step table.
- `pip_graph()` (formerly `pip_get_graph()`) returns visNetwork-compatible
  graph data. The old function names remain available as deprecated aliases.
- The derived columns `depends`, `unbound` and `nodeId` are read-only and
  can't be assigned directly.
- `pip_view()` gains a `join = "union"` argument to combine filters as a
  logical OR.
- `.self` is now automatically available inside every step function (and is
  a protected parameter name), enabling pipelines to modify themselves at
  runtime, including `.self$restart()` and `.self$halt()`.

## Changes

- The columns of the internal step table were reordered and `nodeId` is now
  shown in the full print view (`cols = "all"`).
- The dependencies on `lgr` and `jsonlite` were removed.

## Bug fixes

- `pip_rename()` no longer corrupts the parameters of single-step pipelines.
- Running a view now resolves upstream dependencies correctly.

## Documentation

- New vignettes: "Working with pipeline views" and "Nested pipelines".
- Help pages of related functions were consolidated (extract/replace
  operators, `dim`/`length`, `lock`/`unlock`, `tag`/`untag`,
  `set_params`/`get_params`).
- `pip_run()` documents the runtime control flow (`restart()`, `halt()`), and
  `pip_add()` gained an example for the `after` argument.



# pipeflow 0.3.0

## New features

- **C++ DAG engine** (`src/dag.cpp`): All graph operations (node/edge
  add/remove, topological ordering, reachability queries) run at C++
  speed via Rcpp, eliminating R object overhead for dependency resolution.
- **New `pip_*` API**: Functional API replacing the R6 `Pipeline` class —
  `pip_new()`, `pip_add()`, `pip_run()`, `pip_replace()`, `pip_clone()`,
  `pip_bind()`, `pip_view()`, `pip_tag()`/`pip_untag()`, etc.
- **Execution modes** (`auto`/`split`/`reduce`/`plain`): Native support
  for map-reduce style workflows where steps split output into named
  partitions and downstream steps auto-map over them.
- **Dependency validation at definition time**: `pip_add()`,
  `pip_replace()`, and `pip_remove()` fail fast on broken references.
- New pkgdown-only article *pipeflow vs targets*
  (`vignettes/articles/v07-vs-targets.Rmd`) with an 18-row feature
  comparison table and benchmarks.

## Breaking changes

- Legacy `pipe_*` functions and `Pipeline` R6 class are deprecated
  (preserved as aliases in `R/aliases.R`, `R/pipelineR6.R`).
- `pip_collect_out()` no longer accepts `grouped` or `by` parameters.
  It returns a flat named list of step outputs. Use `pip_view()` with
  tags and manual list composition for grouped output.
- The `group` column has been removed from the pipeline data.table.
  Steps previously using `group = "..."` in `pip_add()` should use
  `tags = "..."`.

## Documentation

- Comprehensive revision of all vignettes to match the new `pip_*` API.
- Revised `vignettes/v04-collect-output.Rmd` to demonstrate tag-based
  grouping via `pip_view()` composition.
- Updated README with a "Why use {pipeflow}" feature list, a short usage
  example, and a revised vs-targets summary.

## Internal

- Extensive test suite covering C++ DAG, `pip_*` API, aliases, execution
  modes, and recursive runs.
- MIT license, updated CI workflows, dependabot config, kilo.json,
  lintr/styler config.

# pipeflow 0.2.3

- Add Depends R >= 4.2.0 to DESCRIPTION
- Fix issue caused by update of lgr package (#571e8260)
- Fix badge links in README

# pipeflow 0.2.2

- Add News and this Changelog
- Add unit tests and detailed documentation for [alias functions](https://rpahl.github.io/pipeflow/reference/index.html#alias-functions) (#24)
- Link to other packges via [my R universe](https://rpahl.r-universe.dev/packages) (#25)

# pipeflow 0.2.1

- CRAN release
