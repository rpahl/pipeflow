# pipeflow 0.5.0.9028

## Breaking changes

- `pip_add()` and `pip_replace()` now signal an error for `params` whose
  names are no arguments of the step function, unless the function has a
  `...` argument that takes them. Before, they were stored and shown by
  `pip_get_params()`, and the run failed with "unused argument".
- The argument `after` of `pip_add()` moved to the end, after `exec`, so
  that `pip_add()` and `pip_replace()` share the order
  `x, step, fun, tags, params, exec`. A call that gave `after` by position
  must name it now.

## New features

- `pip_replace()` gets the argument `keep_params`. With `TRUE`, the new step
  keeps the current values of the unbound params that are arguments of the
  new function (e.g. values set with `pip_set_params()`, annotated values
  included), unless `params` give them or the new function binds them to a
  step. Bound params are not kept, so replacing a step still unbinds them.

# pipeflow 0.5.0.9027

## Breaking changes

- `pip_set_params()` on a view now sets the values in every step of the
  pipeline that has the param, also outside the view, so that a param keeps
  one value. The view still selects which params can be set. The new
  argument `scope = "view"` gives the old behaviour of setting the values in
  the steps of the view only. Bindings (`list(x = ~step)`) are unchanged and
  only apply to the steps of the view. An option named `scope` in `...` no
  longer reaches the methods of `pip_param_value<-()`.

## New features

- `pip_add()`, `pip_replace()` and `rbind()` warn if an unbound param of a
  new step has another value than the same param in the other steps, since
  `pip_get_params()` shows one value per name and `pip_set_params()` sets
  it in all steps. The warning has class `pipeflow_param_conflict` and can
  be turned off with `options(pipeflow.warn_param_conflicts = FALSE)`.
- `pip_get_params(x, simplify = FALSE)` returns the values of all steps,
  grouped by param name and named by step, e.g. to find the params whose
  values differ between steps.

# pipeflow 0.5.0.9026

## Bug fixes

- A step that fails in a run with `on_error = "stop"` (the default) now
  stores its error as its output, as in a run with `on_error = "continue"`.
  Before, it kept the output of an earlier attempt, which could come from
  other params, so the output of a `"failed"` step could not be told apart
  from a stale one.

# pipeflow 0.5.0.9025

## New features

- `pip_run()` has a new argument `map`, a function like `lapply()` that runs
  the keys of a step mapped over partitions (`exec = "auto"`), for example
  in parallel with `parallel::mclapply` or `future.apply::future_lapply`.
  Steps that refer to `.self` or whose function was made by
  `as.function()` (with `cache = TRUE`, the default) still run their keys
  one after another.

# pipeflow 0.5.0.9024

## New features

- The nodes of `pip_graph()` have a `title` column, which visNetwork shows
  when hovering over a step: its state, exec mode, tags, lock, the start
  and duration of its last run, the failed keys of a mapped step, its
  parameters and the steps it depends on.
- The edges of `pip_graph()` have a `dashes` column, so that visNetwork
  draws a `~try()` reference as a dashed edge.
- `pip_graph(format = "mermaid")` returns the graph as the text of a
  mermaid flowchart, which renders e.g. on GitHub and in Quarto. It has the
  colors and dashed edges of the visNetwork graph, a hexagon for `auto` and
  `plain` steps and a trapezoid for `split` (widening) and `reduce`
  (narrowing) steps.

# pipeflow 0.5.0.9023

## New features

- The step table has a new column `duration`: how long the step's last run
  took, as a `difftime` in seconds. Unlike `time - time_start`, it is also
  set if the run failed (`time` keeps the end of the last successful run).
  It is `NA` for a step that has not run since it was added, replaced or
  reset. Set it with `p[[step, "duration"]] <- value` (`NA` allowed).
  Pipelines saved with an earlier version get the column (all `NA`) when
  read.
- The footer of `print()` shows the duration of the last run, and how many
  of the printed steps are in each state, e.g.
  `<ready> last run: 2026-10-25 14:03:12 (0.251 s)` and
  `steps: 3 done, 1 failed, 2 new`. The `duration` column (e.g. with
  `cols = "all"`) is printed in short form, e.g. `0.251 s`.

# pipeflow 0.5.0.9022

## Bug fixes

- `pip_reset()` on a view that covers only some steps now keeps the time of
  the last run and the run state of the pipeline. Before, resetting a
  single step showed `last run: never`.

## Documentation

- New article "Emulating targets features" with recipes for file tracking,
  rebuilds after helper changes, a `tar_meta()`-like table, reproducible
  random numbers, saved results, runs in a fresh R process and a list of
  outdated steps, and the limits of each. The article "pipeflow vs
  targets" is now called "Comparison and benchmarks" and links to the
  recipes.

# pipeflow 0.5.0.9021

## New features

- A run can be cancelled from the `progress` callback of `pip_run()`: if it
  returns `FALSE`, the run is halted before the next step, as by
  `.self$halt()`. This lets a Shiny app stop a run between steps.

# pipeflow 0.5.0.9020

## Bug fixes

- `pip_rename()` now also renames the step in the failures of failed keys
  if the step name has a name attribute, e.g. if it is taken from
  `p[["step"]]`. Before, the failures in the outputs of the step and its
  dependents kept the old name, and renaming a step to its own name this
  way failed with "already exists".
- `pip_reset()` now marks the downstream steps of the reset steps as
  `"outdated"`, as they ran with the cleared outputs. Locked steps keep
  their state. Before, the downstream steps stayed `"done"`, and if a reset
  step was then locked, the next run kept their results.
- `p[i, view = FALSE]` no longer warns that an edge already exists if a
  step refers to the same step with several arguments.

# pipeflow 0.5.0.9019

## Breaking changes

- The deprecated aliases `pip_collect_out()` and `pip_get_graph()` were
  removed. Use `pip_collect()` and `pip_graph()`.
- pipeflow no longer re-exports the data.table operators `%like%`,
  `%ilike%`, `%flike%`, `%plike%`, `%chin%`, `%between%`, `%inrange%` and
  `%notin%`. They still work in the filters of `[`, e.g.
  `p[tags %like% "model"]`, without attaching data.table. To use them
  elsewhere, call `library(data.table)`.

# pipeflow 0.5.0.9018

## Changes

- The error of a mapped step whose partitioned inputs don't have the same
  keys names the arguments that differ and lists the keys that are missing,
  extra or in a different order, e.g. `partitioned arguments must share
  identical keys: argument 'r' differs from 'l' (missing 'b'; extra 'd')`.

## Documentation

- `?as.function.pipeflow` has a new section 'Errors of inner steps' with
  examples of the error messages: they name the outer step, the partition
  key and the failed inner step, e.g.
  `step 'by_batch': key 'D': step 'fit': negative`. The vignette on error
  handling shows how to get the outer step, the key and the inner step from
  the error.

# pipeflow 0.5.0.9017

## Bug fixes

- An assignment to several steps, `p[i, j] <- value`, and a copy of steps,
  `p[i, ] <- q[i, ]`, now change all selected steps or none: if a step
  fails (e.g. because a new step name already exists), the steps assigned
  before are restored. Before, they kept their changes.

# pipeflow 0.5.0.9016

## Breaking changes

- A step that changes its own function, params or exec mode while it runs
  (via `.self`) now ends the run as `"outdated"` instead of `"done"`, as
  its output comes from the old definition, and runs again in the next run.
  The same holds for a step that a later step changes during the run. The
  steps that ran with the output of such a step are marked `"outdated"`
  too. Before, all of them stayed `"done"` with stale results, also after
  `.self$restart(force = FALSE)`. A step that sets its own params to fixed
  values settles after one more run, while a step that sets new values in
  every run now runs again in every run.

## Bug fixes

- Changing the execution mode of a step with `p[[step, "exec"]] <- mode`
  (also via `p[i, "exec"] <-` and views) now marks the step and its
  downstream steps as `"outdated"`, so that the next run uses the new mode.
  Before, a step that had run kept its state and its output from the old
  mode. A step that failed for some keys and got a new mode is run again
  for all keys.
- Setting the output of a step with `p[[step, "out"]] <- value` (also via
  `p[i, "out"] <-`, views and `p[i, ] <- q[j, ]`) now marks its downstream
  steps as `"outdated"`, so that the next run computes them from the new
  output. Before, they stayed `"done"` with the results of the old output.
  The step itself keeps its state. `?Extract.pipeflow` lists which
  assignments outdate steps.

# pipeflow 0.5.0.9015

## Breaking changes

- In a run with `on_error = "continue"`, a step mapped over partitioned
  inputs now fails per partition key: its output keeps the results of the
  keys that succeeded, and each failed key holds a `pipeflow_failure` with
  the field `key` (before, the whole step failed and its output was the
  `pipeflow_key_error`). The step gets state `"failed"`. Mapped steps
  downstream are not run for the failed keys, unless they take the input
  via `~try()`; other steps get the output with the failed keys via
  `~try()` and are skipped otherwise. The `pipeflow_run_failed` warning
  has a new field `keys` with the failed keys per step.

## New features

- When a mapped step failed for some keys, the next run only calls it for
  the failed keys and keeps the results of the other keys, unless its
  parameters or inputs changed or the run is forced.
- The log line at the end of `pip_run()` is shorter and counts the steps
  that were done, skipped (already done, locked or with a failed input) and
  failed in the whole run, including restarts, e.g.
  `Finished in 0.251 s (3 done, 1 skipped, 0 failed)`. The pipeline is
  named in the log line at the start of the run.

# pipeflow 0.5.0.9014

## New features

- The step table has a new column `time_start`: the start of the step's last
  run, set each time the step is run, also if the run fails. It is `NA` for
  a step that has not run since it was added, replaced or reset. `time`
  keeps its meaning (end of the last successful run), so
  `time - time_start` is the duration of a successful run, and
  `time_start >= t0` tells which steps a run started at `t0` attempted.
  Set it with `p[[step, "time_start"]] <- value` (`NA` allowed). Pipelines
  saved with an earlier version get the column (all `NA`) when read.
- The log line at the end of `pip_run()` shows the total time of the run,
  including restarts, e.g. `Finished run of pipeflow 'demo' in 0.251 s`.

# pipeflow 0.5.0.9013

## Breaking changes

- `rbind()` of pipelines lost the argument `bind` (added in 0.5.0.9008).
  Bind the params after binding the pipelines with `pip_set_params()`, using
  a view to select the steps, e.g.
  `pip_set_params(ab[-seq_along(a)], list(data = ~prep))`.

# pipeflow 0.5.0.9012

## Breaking changes

- `as.function()` lost the argument `bind`. Bind the inputs of the function
  with `params` of `pip_add()` instead, as for any other step function, e.g.
  `pip_add(outer, "step", f, params = list(input_df = ~split))`.
- The argument `cache` of `as.function()` is now `TRUE` (the default) or
  `FALSE` instead of the number of inner pipelines to keep.

## New features

- `out` of `as.function()` can be a function. It is called with a light copy
  of the run inner pipeline, and its result is the result of the function,
  e.g. `out = \(p) pip_collect(p, by = "tags")`.
- New `pip_inner(x, step)` returns the inner pipelines that the function of
  a step, made by `as.function()`, has kept: one per partition key for a
  mapped step. Use it e.g. to debug an inner run.

## Changes

- A function made by `as.function()` keeps one inner pipeline per outer step
  and partition key, so a later call only reruns the inner steps whose
  params or inputs changed, for any number of partitions. This also works
  for inputs bound via `params` of `pip_add()`.

## Documentation

- The "Nested pipelines" vignette shows a function as `out` and
  `pip_inner()`, and its "Full control" example forwards the step's params
  with `pip_set_params(p, list(...))`.

# pipeflow 0.5.0.9011

## Documentation

- The cheat sheet shows "Ways to connect steps" again on page 2.

# pipeflow 0.5.0.9010

## Breaking changes

- `pip_set_params()` binds a param to a step if its value is a one-sided
  formula, e.g. `pip_set_params(p, list(model = ~fit))`, as a formula does
  in the defaults of a step function and in `params` of `pip_add()`. Before,
  the formula was set as value. To set a formula as value, give it as a
  string and turn it into a formula in the step, e.g. with `as.formula()`.
  Because of this, `as.function()` no longer turns such a value into a
  reference to a step of the outer pipeline.

## New features

- `pip_set_params()` can bind params to steps. The param is bound in all
  selected, unlocked steps that have it unbound, which must come after the
  step it is bound to; a view selects the steps to bind. A bound param is no
  longer unbound, and setting the same binding again does nothing.

## Changes

- The warning of `pip_set_params()` about unknown params no longer shows
  the call.

# pipeflow 0.5.0.9009

## Breaking changes

- `params` of `pip_add()` and `pip_replace()` now override the defaults of
  `fun`, as they did before 0.4.0. Before, the defaults of `fun` won and
  `pip_add()` warned that the values in `params` were dropped. A formula in
  `params` makes the parameter a reference to another step, also if its
  default is a value.

## New features

- `params` can give a value to parameters of `fun` that have no default, so
  that existing functions can be steps without a wrapper, e.g.
  `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.

## Changes

- `params` of `pip_add()` and `pip_replace()` must be a list with unique
  names.
- The params of a step list the arguments of `fun` first, in their order,
  followed by the other params (before: the given `params` came first).
- `pip_add()` and `pip_replace()` point to `as.function()` if `fun` is a
  pipeline.
- Replacing a step with one that references a step which comes after it
  now reports "cannot reference steps that come after the replaced step",
  matching `pip_add()`, instead of "cannot reference unknown steps".

## Bug fixes

- Copying steps between pipelines with `p[i] <- q[j]` dropped the params
  that were given via `params` (e.g. extra params passed via `...`), and
  took references from the defaults of the function, ignoring a renamed
  upstream step.
- Copying steps with `p[i] <- q[j]`, also onto themselves, resolved
  relative references such as `~-1` again, so that after inserting or
  removing a step, the copied step depended on another step or failed. A
  copied step now depends on the same steps as the source step, by name.
- `rbind()` dropped the arguments with a `NULL` value from the step
  functions of the second pipeline, so that these steps failed with
  "unused argument".

# pipeflow 0.5.0.9008

## New features

- `rbind()` of pipelines gained the argument `bind`, a named list of
  one-sided formulas as in `as.function()`, e.g.
  `rbind(a, b, bind = list(data = ~prep))`. It binds unbound params of the
  second pipeline to steps of the first one, so that the pipelines are
  connected without editing the second one first.

## Bug fixes

- Steps with non-syntactic names, e.g. `"source data"`, can be referenced
  with backticks, ``x = ~`source data` ``. Before, `pip_add()` reported them
  as unknown steps.
- `rbind()` no longer fails if the second pipeline references a step with a
  non-syntactic name, e.g. after `pip_rename()`.

# pipeflow 0.5.0.9007

## New features

- `as.function()` gained the argument `on_error`. Set to `"continue"`, the
  inner pipeline is run as `pip_run(on_error = "continue")` runs it: inner
  steps that do not depend on a failed step still run, and a `~try(step)`
  reference inside the inner pipeline receives the failure. The function
  still fails if one of the steps named in `out` failed or was not run
  because an input failed.
- `as.function()` gained the argument `lgr`, the logger of the inner run.
  The inner pipeline is still run silently by default (`lgr = NULL`), but
  its steps can now be logged, e.g. `as.function(x, lgr = pipeflow_lgr)`.

# pipeflow 0.5.0.9006

## New features

- `as.function()` turns a pipeline into a function whose arguments are the
  unbound params of the pipeline, so that a pipeline can be a step of
  another pipeline, e.g.
  `pip_add(outer, "step", as.function(inner), params = list(df = ~split))`.
  The params of the inner pipeline become params of the outer step, the exec
  modes map it over partitions, and the function keeps the inner pipelines it
  has run, so that a changed param only reruns the affected inner steps. See
  `?as.function.pipeflow` and the "Nested pipelines" vignette.

# pipeflow 0.5.0.9005

## Changes

- Adding a step that references a step which exists but comes after the
  insertion point now reports "cannot reference steps that come after the
  insertion point" instead of "cannot reference unknown steps".
- The error raised when `pip_add()`, `pip_clone()` or `rbind()` is given a
  view now points at `x[, view = FALSE]`.

## Bug fixes

- `v[, view = FALSE]` returned the view instead of a standalone pipeline of
  the covered steps, silently ignoring `view = FALSE`.

# pipeflow 0.5.0.9004

## New features

- `` `pip_param_value<-`() `` is the write side of `pip_param_value()`: a
  generic that turns a plain value into the value stored in a step, so
  that `pip_set_params()` keeps the annotations of a stored object when it
  is given a plain value.
- `pip_set_params()` gained `...`, which is passed on to the methods of
  `` `pip_param_value<-`() ``. It now determines the value to store for
  every affected step before it changes anything: if a method fails,
  nothing is set and an error of class `pipeflow_set_params_error`
  reports all failures at once.

## Documentation

- `pip_get_params()` notes that a view excludes locked steps,
  `pip_get_params(p[!locked])`.

# pipeflow 0.5.0.9003

## New features

- `pip_output_value()` is a generic for annotating the output of a step
  when the step returns, for example to stamp a result with the step it
  came from and the time of the run. It is applied once to the output,
  and once per partition for a partitioned output, which gets its key in
  `ctx$key`. See the new vignette "Annotated step outputs".

# pipeflow 0.5.0.9002

## Breaking changes

- `.self` is released when a step returns, so that step outputs no longer
  keep the pipeline alive. A step that returns a function which refers to
  `.self` now fails with `object '.self' not found` when that function is
  called after the run.

# pipeflow 0.5.0.9001

## Documentation

- New hexagon logo with the package name, used in the README and as
  pkgdown logo and favicons. The source is `logo/logo.svg`.
- New two-page cheat sheet (`cheatsheet/pipeflow-cheatsheet.pdf`), shown
  in the README between "Installation" and "Usage".
- Shorter README introduction and "Why pipeflow?" list.
- The pkgdown site is centered again, and its home page shows the logo
  left of the package name.

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
  after a step fails with an error.
- allow mitigation of inputs from failed steps via `~try()` references
  (`x = ~try(step)`), e.g. by creating fallback results for failed steps.
- `pip_param_value()` is a generic for unwrapping parameter values, enabling
  users to set pipeline parameters that consist of non-trivial types or classes.
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
