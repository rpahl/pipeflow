# Changelog

## pipeflow 0.6.0

### Breaking changes

- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  now signal an error for `params` whose names are no arguments of the
  step function, unless the function has a `...` argument that takes
  them.
- The argument `after` of
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  moved to the end, after `exec`, so that
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  share the order `x, step, fun, tags, params, exec`. A call that gave
  `after` by position must name it now.
- `params` of
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  override the defaults of `fun` again, as before 0.4.0 (in 0.5.0 the
  defaults won and
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  warned). They must be a list with unique names, and the params of a
  step list the arguments of `fun` first, followed by the other params.
- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  on a view sets the values in every step of the pipeline that has the
  param, also outside the view, so that a param keeps one value.
  `scope = "view"` gives the old behavior. An option named `scope` in
  `...` no longer reaches the methods of `pip_param_value<-()`.
- `.self` is released when a step returns, so that step outputs no
  longer keep the pipeline alive. A function returned by a step that
  refers to `.self` fails with `object '.self' not found` when it is
  called after the run.
- A step that changes its own function, params or exec mode while it
  runs (via `.self`), or that a later step changes during the run, now
  ends the run as `"outdated"` instead of `"done"` and runs again in the
  next run, as do the steps that ran with its output.
- In a run with `on_error = "continue"`, a step mapped over partitioned
  inputs fails per partition key: its output keeps the results of the
  keys that succeeded, and each failed key holds a `pipeflow_failure`
  with the field `key`. The step gets state `"failed"`. Mapped steps
  downstream are not run for the failed keys, unless they take the input
  via `~try()`. The `pipeflow_run_failed` warning has a new field
  `keys`.
- The deprecated aliases `pip_collect_out()` and `pip_get_graph()` were
  removed. Use
  [`pip_collect()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
  and
  [`pip_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md).
- pipeflow no longer re-exports the data.table operators `%like%`,
  `%ilike%`, `%flike%`, `%plike%`, `%chin%`, `%between%`, `%inrange%`
  and `%notin%`. They still work in the filters of `[`,
  e.g. `p[tags %like% "model"]`, without attaching data.table. To use
  them elsewhere, call [`library(data.table)`](https://r-datatable.com).

### New features

- Pipelines as functions:
  [`as.function()`](https://rdrr.io/r/base/as.function.html) turns a
  pipeline into a function whose arguments are the unbound params of the
  pipeline, so that a pipeline can be a step of another pipeline, e.g.
  `pip_add(outer, "step", as.function(inner), params = list(df = ~split))`.
  The exec modes map it over partitions, and the function keeps one
  inner pipeline per outer step and partition key, so that a changed
  param only reruns the affected inner steps. `out` can be a function
  that is called with a light copy of the run inner pipeline,
  `on_error = "continue"` continues the inner run, `lgr` sets the logger
  of the inner run, and `cache = FALSE` switches the kept pipelines off.
  New `pip_inner(x, step)` returns the inner pipelines kept by such a
  step, e.g. to debug an inner run. See
  [`?as.function.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)
  and the “Nested pipelines” vignette.
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  takes a pipeline or view as `step` and copies its steps, with their
  functions, params, tags and exec modes, but without their upstream
  steps: references are resolved by step name in the target pipeline,
  e.g. `pip_add(p, q[c("fit", "plot")])`. If a step can’t be added, the
  pipeline is left unchanged.
- `pip_clone(out = )` copies a pipeline with the outputs of the named
  steps only, e.g. to send the results of a run back from a parallel
  worker.
- [`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md)
  is a generic for annotating the output of a step when the step
  returns, e.g. to stamp a result with the step it came from and the
  time of the run.
  [`` `pip_param_value<-`() ``](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
  is the write side of
  [`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md):
  [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  uses it to keep the annotations of a stored object when it is given a
  plain value, and passes `...` on to its methods. It determines the
  values for all affected steps before it changes anything, and reports
  all failures at once with an error of class
  `pipeflow_set_params_error`. See the vignette “Annotated step
  outputs”.
- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  can bind params to steps: a one-sided formula as value,
  e.g. `pip_set_params(p, list(model = ~fit))`, binds the param in all
  selected, unlocked steps that have it unbound, as a formula does in
  the defaults of a step function and in `params` of
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md).
  (A formula was set as a value before; to do so now, give it as a
  string and turn it into a formula in the step.)
- `params` of
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  can give values to parameters of `fun` that have no default, so that
  existing functions can be steps without a wrapper,
  e.g. `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.
- [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  gets the argument `keep_params`. With `TRUE`, the new step keeps the
  current values of the unbound params that are arguments of the new
  function, unless `params` give them or the new function binds them.
- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  has a new argument `map`, a function like
  [`lapply()`](https://rdrr.io/r/base/lapply.html) that runs the keys of
  a step mapped over partitions (`exec = "auto"`), e.g. in parallel with
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) or
  `future.apply::future_lapply`.
- A run can be cancelled from the `progress` callback of
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md):
  if it returns `FALSE`, the run is halted before the next step.
- A mapped step that failed for some keys is only called for the failed
  keys in the next run, unless its parameters or inputs changed or the
  run is forced. This does not hold for a copy made by
  [`pip_clone()`](https://rpahl.github.io/pipeflow/dev/reference/pip_clone.md),
  which runs all keys again (known issue,
  [\#172](https://github.com/rpahl/pipeflow/issues/172)).
- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  warns (class `pipeflow_outdated_before`) if a step changes itself or a
  done step before it via `.self`, as these steps then run again in the
  next run.
- The step table has new columns `time_start` (the start of the step’s
  last run, also set if the run fails) and `duration` (how long the last
  run took, as a `difftime`). Both are `NA` for a step that has not run
  since it was added, replaced or reset, and pipelines saved with an
  earlier version get the columns when read.
- The log of
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  names the pipeline and ends with a summary of the whole run, including
  restarts, e.g. `Finished in 0.251 s (3 done, 1 skipped, 0 failed)`.
  The footer of [`print()`](https://rdrr.io/r/base/print.html) shows the
  duration of the last run and the number of steps per state.
- The option `pipeflow.lgr` sets the default logger of
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md),
  e.g. `options(pipeflow.lgr = function(level, msg) NULL)` switches
  logging off.
- `pip_get_params(x, simplify = FALSE)` returns the values of all steps,
  grouped by param name and named by step, e.g. to find params whose
  values differ between steps.
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  and [`rbind()`](https://rdrr.io/r/base/cbind.html) warn (class
  `pipeflow_param_conflict`) if an unbound param of a new step has
  another value than the same param in other steps. Turn it off with
  `options(pipeflow.warn_param_conflicts = FALSE)`.
- `[` warns if the selector is a column name that is also a variable of
  the caller: with `step <- "load"`, `p[step]` selects by the `step`
  column.
- [`pip_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md):
  the nodes have a `title` column (shown by visNetwork on hover), the
  edges a `dashes` column (a `~try()` reference is dashed), and
  `format = "mermaid"` returns the graph as a mermaid flowchart, which
  renders e.g. on GitHub and in Quarto.

### Bug fixes

- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  tracks the steps of a run by node id, so a step that changes its own
  pipeline via `.self` (or a `progress` callback) no longer makes the
  run run the wrong step, fail with “subscript out of bounds”
  ([\#157](https://github.com/rpahl/pipeflow/issues/157)) or run the
  dependents of a renamed failed step with the failure as input
  ([\#160](https://github.com/rpahl/pipeflow/issues/160)). A step that
  was removed during the run is skipped, and a step that renames itself
  keeps its output.
- [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
  skips a step whose input is still `"new"`, e.g. a step added via
  `.self` during the run, instead of running it with a `NULL` input.
- A step that fails in a run with `on_error = "stop"` stores its error
  as its output, as with `"continue"`, instead of keeping the output of
  an earlier attempt.
- A step function is called by the step name with its arguments as
  symbols, e.g. `fit(x = x)`, so errors and `pipeflow_failure`s no
  longer carry the inputs of the step in their call (megabytes of data),
  and an output that keeps the frame of the step call no longer keeps
  the pipeline.
- A key of a partitioned output whose value is `NULL` is kept, and a
  step mapped over it gets the argument.
- [`pip_lock()`](https://rpahl.github.io/pipeflow/dev/reference/pip_lock.md)
  on a step that is not done marks its downstream steps as outdated
  ([\#147](https://github.com/rpahl/pipeflow/issues/147)).
- [`pip_reset()`](https://rpahl.github.io/pipeflow/dev/reference/pip_reset.md)
  marks the downstream steps of the reset steps as `"outdated"`, and on
  a view it keeps the time of the last run and the run state of the
  pipeline.
- Setting `exec` or `out` of a step with `p[[step, "exec"]] <-` or
  `p[[step, "out"]] <-` (also via `p[i, j] <-` and views) marks the
  affected steps as `"outdated"`, so the next run uses the new value.
  See
  [`?Extract.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/Extract.pipeflow.md).
- An assignment to several steps, `p[i, j] <- value` or
  `p[i, ] <- q[i, ]`, changes all selected steps or none.
- Copying steps with `p[i] <- q[j]` keeps the params given via `params`
  and resolves relative references such as `~-1` by name, also onto
  themselves.
- [`rbind()`](https://rdrr.io/r/base/cbind.html) no longer drops
  arguments with a `NULL` value from the step functions of the second
  pipeline, and no longer fails if it references a step with a
  non-syntactic name.
- Steps with non-syntactic names, e.g. `"source data"`, can be
  referenced with backticks, `` x = ~`source data` ``.
- [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  and
  [`pip_rename()`](https://rpahl.github.io/pipeflow/dev/reference/pip_rename.md)
  reject step names that start with `-`, which a reference `~-1` would
  take as a relative reference.
- [`pip_rename()`](https://rpahl.github.io/pipeflow/dev/reference/pip_rename.md)
  also renames the step in the failures of failed keys if the step name
  has a name attribute, e.g. if it is taken from `p[["step"]]`.
- `p[i, view = FALSE]` no longer warns that an edge already exists if a
  step refers to the same step with several arguments, and
  `v[, view = FALSE]` returns a standalone pipeline instead of the view.
- [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  on a pipeline or view without steps no longer says that all selected
  steps are locked.

### Minor improvements

- The error messages of
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
  [`pip_remove()`](https://rpahl.github.io/pipeflow/dev/reference/pip_remove.md),
  [`pip_rename()`](https://rpahl.github.io/pipeflow/dev/reference/pip_rename.md)
  and
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md)
  for step name, function and params are the same, e.g.
  `pip_remove(p, "")` says that the step must be a non-empty string.
- Adding or replacing a step that references a step that comes after the
  insertion point reports “cannot reference steps that come after …”,
  and the error for a view given to
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
  [`pip_clone()`](https://rpahl.github.io/pipeflow/dev/reference/pip_clone.md)
  or [`rbind()`](https://rdrr.io/r/base/cbind.html) points at
  `x[, view = FALSE]`.
- The error of a mapped step whose partitioned inputs have different
  keys names the arguments that differ and the keys that are missing,
  extra or in a different order.
- The warning of
  [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  about unknown params no longer shows the call.
- Internal helpers were renamed and documented consistently, and a
  `%||%` helper is used for NULL defaults.

### Documentation

- New articles “Emulating targets features” (recipes for file tracking,
  rebuilds after helper changes, a `tar_meta()`-like table, reproducible
  random numbers, saved results, runs in a fresh R process and outdated
  steps) and “Comparison and benchmarks” (formerly “pipeflow vs
  targets”), and the vignettes “Annotated step outputs” and “Nested
  pipelines”.
- [`?as.function.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)
  shows the error messages of inner steps,
  [`?pip_add`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
  explains how outputs that keep an environment can keep the whole
  pipeline, and
  [`?pip_view`](https://rpahl.github.io/pipeflow/dev/reference/pip_view.md)
  says that running a view also runs the upstream steps of its steps.
- New hexagon logo, two-page cheat sheet (shown in the README), shorter
  README introduction, and a centered pkgdown site with the logo.

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
  (formerly `pip_collect_out()`) supports grouping via a `by` argument,
  `as.table = TRUE` to return a compact table, and a `simplify` argument
  to control flattening of single-step groups.
- [`pip_data()`](https://rpahl.github.io/pipeflow/dev/reference/pip_data.md)
  provides direct access to the underlying step table.
- [`pip_graph()`](https://rpahl.github.io/pipeflow/dev/reference/pip_graph.md)
  (formerly `pip_get_graph()`) returns visNetwork-compatible graph data.
  The old function names remain available as deprecated aliases.
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
- `pip_collect_out()` no longer accepts `grouped` or `by` parameters. It
  returns a flat named list of step outputs. Use
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
