<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# pipeflow 0.4.0.9008

- Same as previous version.


# pipeflow 0.4.0.9007

- With `pip_run(on_error = "continue")`, a `reduce` step that takes the output
  of a failed split pipeline via `~try()` now runs and receives the failure
  object. Before, it failed with "reduce mode requires at least one
  partitioned input", which hid the actual cause.


# pipeflow 0.4.0.9006

- `pip_add()` and `pip_replace()` now warn if `params` contain names that
  are also arguments of `fun`. As documented, the defaults of `fun` take
  precedence over `params`, so such values were silently dropped before.
- `pip_set_params()` gains `unknown = c("warn", "ignore", "error")` to
  control what happens with parameters that are not defined in the target.
  The default `"warn"` keeps the current behaviour; `"error"` fails before
  anything is changed.
- Behaviour change: `pip_set_params()` (and `p[[step, "params"]] <- value`)
  no longer outdates a step if the values set on it are `identical()` to
  the stored ones. Only steps whose values change, and the steps downstream
  of them, are outdated. Before, every step a value was set on was
  outdated, so apps that set all inputs on every change re-ran unchanged
  steps. `pip_run(force = TRUE)` still re-runs all steps. The new argument
  `force = TRUE` of `pip_set_params()` outdates the steps even if the values
  are identical, e.g. to refresh steps whose inputs are environments or R6
  objects that were modified in place.


# pipeflow 0.4.0.9005

- Behaviour change: if a step fails, the error from `pip_run()` now names
  the step (`step 'fit': object 'x' not found`). It is of class
  `pipeflow_step_error`, with the fields `step` and `parent` (the original
  condition, which keeps its class). If a partition key fails, the original
  condition is wrapped in a `pipeflow_key_error` with the fields `key` and
  `parent`; with `on_error = "continue"`, it is the output of the failed
  step. Before, only the message of the original error was signalled.


# pipeflow 0.4.0.9004

- `x[[step, j]]` on a view now returns the value of `step`. Before, it read
  the wrong row (or failed) when the view did not start at the first step.
- The steps of a copy made by `pip_clone()`, `p[]` or `p[i, view = FALSE]`
  get their own `.self`. Before, they shared it with the steps of the
  original, so once the copy ran, a running step of the original saw the
  copy as `.self`.
- `pip_set_params()` no longer warns about parameters that are defined only
  in locked steps. It now also warns about undefined parameters when all
  selected steps are locked. Locked steps still keep their parameters.
- `pip_add()` and `pip_replace()` now reject primitive functions such as
  `sum` with a clear error. Before, they failed with "use of NULL
  environment is defunct".
- `pip_add()` now says that `.self` is a reserved *parameter* name (not a
  step name) if `fun` declares it, as `pip_replace()` does.
- Behaviour change: a run stopped by `halt()` now ends with run state
  `"halted"`, as shown when printing the pipeline. Before, it was always
  overwritten with `"ready"` (or `"continued"`). The next `pip_run()`
  continues as before.


# pipeflow 0.4.0.9003

- Same as previous version.


# pipeflow 0.4.0.9002

- Same as previous version.


# pipeflow 0.4.0.9001

- Revise benchmark vignette
- decrease font size in vignettes
- Steps that have never run now stay `"new"` instead of becoming
  `"outdated"` when `pip_set_params()` or `pip_replace()` changes them or a
  step upstream of them, or when a run (of a view, or an aborted run) does
  not reach them.
- At the end of a run, `pip_run()` now marks only the steps downstream of
  steps it actually *executed* as `"outdated"`. Before, steps it skipped
  because they were `"done"` counted too, so running a view outdated done
  steps outside the view although none of their inputs had changed.
- `pip_run()` gains `on_error = c("stop", "continue")`. With `"continue"`,
  a failing step gets state `"failed"` with its error condition stored as
  its output, and the run goes on with all steps that do not depend on a
  failed step. If any step failed, the run state is set to `"continued"` and
  a warning of class `pipeflow_run_failed` lists the failed and the skipped
  steps.
- A step reference wrapped in `try()`, e.g. `x = ~try(other_step)`, marks
  an argument that may receive a failed input in a continuing run. Such an
  argument gets a condition of class `pipeflow_failure` instead of the step
  output.
- `pip_run()` now also re-runs a `"done"` step if one of its inputs was
  executed in the same run.


# pipeflow 0.4.0.9000

- Same as previous version.


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
