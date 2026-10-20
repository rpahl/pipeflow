# Run a pipeline

Executes all pending steps in order. On repeated runs, steps that are
already `"done"` are skipped and only steps that are still `"new"` or
were marked `"outdated"` (because one of their dependencies changed) are
executed. Use `force = TRUE` to re-execute every step.

## Usage

``` r
pip_run(
  x,
  lgr = pipeflow_lgr,
  force = FALSE,
  progress = NULL,
  on_error = c("stop", "continue")
)
```

## Arguments

- x:

  A pipeflow pip or view

- lgr:

  A logging function with the arguments `level` (`"info"`, `"warn"` or
  `"error"`) and `msg`, which is called as
  `lgr(level = level, msg = msg)`. The default is
  [`pipeflow_lgr()`](https://rpahl.github.io/pipeflow/dev/reference/pipeflow_lgr.md),
  which can also be used to build custom loggers. To suppress logging,
  you can set `lgr = NULL`.

- force:

  Logical indicating if all steps should be forced to run, regardless of
  whether they are outdated or not.

- progress:

  Optional callback of the form `function(value, detail)` called before
  each step.

- on_error:

  What to do when a step fails: `"stop"` (the default) aborts the run,
  `"continue"` goes on with all steps that do not depend on a failed
  step (see section 'Continuing after errors' below).

## Value

The updated pipeline or view, invisibly.

## Details

### Step states

A step can take the following states:

- new: the step has been added but not yet executed, or was reset via
  [`pip_reset()`](https://rpahl.github.io/pipeflow/dev/reference/pip_reset.md).

- outdated: a step can get outdated in the following scenarios:

  - one of the step's parameters were changed (e.g. via
    [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md))

  - a step it depends on was re-executed or replaced

  - a step it depends on was executed, but the run did not reach the
    step itself, either on purpose (see section 'Running views' below)
    or because the run was aborted early due to a failed step

- done: the step was executed successfully with its current inputs

- failed: the step raised an error during its last execution

Only steps that have run before can become outdated: a step in state
`"new"` stays `"new"` until it is executed. A "done" step is skipped
unless `force = TRUE` was set. For all other states, the step will be
re-executed in the next `pip_run()`.

### Running views

When `x` is a view, the requested rows are run together with their
upstream dependencies, so the steps covered by the view are brought up
to date even if their inputs come from steps outside the view. The rest
of the pipeline is not executed; downstream steps of executed steps are
marked `"outdated"`. Steps whose inputs were not re-executed (for
example, because they were already `"done"`) keep their state.

### Runtime errors

If a step fails with an error, the failing step's state is set to
`"failed"`, the run is aborted (no further steps are executed), and the
pipeline run state is set to `"failed"`. Steps downstream of executed
steps that were not reached are marked `"outdated"`, so a subsequent run
retries them.

The error is signalled as a condition of class
`c("pipeflow_step_error", "error", "condition")`. Its message is the
message of the original error, prefixed with `"step '<step>': "`. Its
field `step` names the failed step and its field `parent` holds the
original error condition with its class, which can be caught via
`tryCatch(pip_run(p), pipeflow_step_error = function(e) e$parent)`. If
the error belongs to a single partition key (see `exec` in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md))
— a step mapped over partitioned inputs, or
[`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md)
failing for one key of a partitioned output — `parent` is a condition of
class `c("pipeflow_key_error", "error", "condition")` instead. Its
message is prefixed with `"key '<key>': "`, its field `key` names the
failed partition key and its field `parent` holds the original error
condition.

### Continuing after errors

With `on_error = "continue"`, a failing step gets state `"failed"`, its
error condition is stored as its output, and the run goes on. For a step
whose error belongs to a single partition key, the stored condition is
the `pipeflow_key_error` described above. An input of a step is *failed*
if its step failed or was not run because a step upstream of it failed
in this run.

- A step with a failed input is not run, and counts as failed for its
  own dependents, unless the argument receiving that input refers to its
  step via [`try()`](https://rdrr.io/r/base/try.html), e.g.
  `x = ~try(other_step)` (see
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)).

- A step whose failed inputs all use
  [`try()`](https://rdrr.io/r/base/try.html) references is run, and each
  of these arguments receives a condition of class
  `c("pipeflow_failure", "error", "condition")` with the fields
  `message`, `step` (the step of the input), `failed_step` (the step
  that actually failed) and `parent` (the original error condition).

- Locked steps are the exception: they are never run, so a failed input
  does not affect them. They keep their state and output, and their
  dependents receive that output as usual.

Steps that ran (including those that received failure objects) are
`"done"`. Steps that were not run are treated like steps not reached by
an aborted run: a `"done"` step becomes `"outdated"`, a `"new"` one
stays `"new"`, so the next run retries them together with the failed
steps. Once a failed step succeeds, the steps downstream of it,
including those that received failure objects, are run again.

If any step failed, the pipeline run state is set to `"continued"` and,
once all states and outputs are stored, a warning of class
`c("pipeflow_run_failed", "warning", "condition")` is signalled. Its
field `failed` is a named list with the original conditions of the
failed steps and its field `skipped` names the steps that were not run.

### Runtime control flow via restart and halt

A running pipeline can be interrupted via the `restart()` and `halt()`
virtual methods. They are intended for advanced, self-modifying
pipelines and are most often called from within a step function via
`.self` (see section 'The pipeline as `.self`' in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)).

- `.self$restart(force = TRUE, times = 1L)`: aborts the current run
  after the current step has finished and restarts it from the first
  step. The default parameters are `force = TRUE` and `times = 1L`, that
  is, the above call is the same as just invoking .self\$restart(). To
  skip steps that are already in state `"done"`, set `force = FALSE`.
  The `times` parameter limits the number of consecutive restarts within
  a single `pip_run()` call. If a view is being run, a restart covers
  the view steps together with their upstream dependencies.

- `p$halt()`: aborts the current run after the current step has
  finished. This is a *controlled halt* and deliberately distinct from
  [`base::stop()`](https://rdrr.io/r/base/stop.html): no error is raised
  and the pipeline is not marked as `"failed"`. The run simply ends with
  run state `"halted"` (see
  [`pip_run_state()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run_state.md))
  and a subsequent `pip_run()` will continue where the run left off.

In both cases steps downstream of executed steps that have not been
executed until the restart or halt happens are marked as `"outdated"`.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://rpahl.github.io/pipeflow/dev/articles/v06-self-modify-pipeline.md)
for an advanced example of dynamic pipelines.

## Examples

``` r
p <- pip_new() |>
pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("prep", \(x = ~load, weight = 1) x * weight) |>
  pip_add("square", \(x = ~prep) x^2) |>
  pip_add("total", \(x = ~square) sum(x))

pip_run(p)
#> info [2026-10-20 18:51:07.345 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:07.345 UTC]: Step 1/4 load
#> info [2026-10-20 18:51:07.346 UTC]: Step 2/4 prep
#> info [2026-10-20 18:51:07.347 UTC]: Step 3/4 square
#> info [2026-10-20 18:51:07.349 UTC]: Step 4/4 total
#> info [2026-10-20 18:51:07.350 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state   out
#> 1:   load        n          done 1,2,3
#> 2:   prep x,weight    load  done 1,2,3
#> 3: square        x    prep  done 1,4,9
#> 4:  total        x  square  done    14
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:07

pip_set_params(p, list(weight = 2))
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends    state   out
#> 1:   load        n             done 1,2,3
#> 2:   prep x,weight    load outdated 1,2,3
#> 3: square        x    prep outdated 1,4,9
#> 4:  total        x  square outdated    14
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:07

# Already-done steps are skipped on a second run
pip_run(p) # first step skipped
#> info [2026-10-20 18:51:07.361 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:07.362 UTC]: Step 1/4 load - skipping done step
#> info [2026-10-20 18:51:07.362 UTC]: Step 2/4 prep
#> info [2026-10-20 18:51:07.363 UTC]: Step 3/4 square
#> info [2026-10-20 18:51:07.364 UTC]: Step 4/4 total
#> info [2026-10-20 18:51:07.365 UTC]: Finished run of pipeflow 'pipe'

# lgr = NULL suppresses log output
pip_run(p, lgr = NULL)

# force = TRUE re-executes every step regardless of state
pip_run(p, force = TRUE)
#> info [2026-10-20 18:51:07.367 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:07.367 UTC]: Step 1/4 load
#> info [2026-10-20 18:51:07.368 UTC]: Step 2/4 prep
#> info [2026-10-20 18:51:07.369 UTC]: Step 3/4 square
#> info [2026-10-20 18:51:07.370 UTC]: Step 4/4 total
#> info [2026-10-20 18:51:07.371 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state      out
#> 1:   load        n          done    1,2,3
#> 2:   prep x,weight    load  done    2,4,6
#> 3: square        x    prep  done  4,16,36
#> 4:  total        x  square  done       56
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:07

# Run only a subset of steps via a view;
# upstream dependencies are automatically included
v <- pip_view(p, step = "total")
pip_run(v)
#> info [2026-10-20 18:51:07.375 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:07.375 UTC]: Step 1/4 [upstream] load - skipping done step
#> info [2026-10-20 18:51:07.375 UTC]: Step 2/4 [upstream] prep - skipping done step
#> info [2026-10-20 18:51:07.376 UTC]: Step 3/4 [upstream] square - skipping done step
#> info [2026-10-20 18:51:07.376 UTC]: Step 4/4 [view] total - skipping done step
#> info [2026-10-20 18:51:07.376 UTC]: Finished run of pipeflow 'pipe'

# Halt or restart pipeline at runtime (for advanced usage)
p <- pip_new("restart") |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("check", \(x = ~load) {
    if (length(x) > 10L) .self$halt()
  }) |>
  pip_add("model", \(x = ~load) {
    if (length(x) == 3L) {
      .self$set_params(list(n = 5))
      .self$restart()
    }
    x * 2
  })

pip_run(p)
#> info [2026-10-20 18:51:07.378 UTC]: Starting run of pipeflow 'restart'
#> info [2026-10-20 18:51:07.378 UTC]: Step 1/3 load
#> info [2026-10-20 18:51:07.379 UTC]: Step 2/3 check
#> info [2026-10-20 18:51:07.380 UTC]: Step 3/3 model
#> info [2026-10-20 18:51:07.382 UTC]: Restarting pipeline execution.
#> info [2026-10-20 18:51:07.383 UTC]: Restarting run of pipeflow 'restart'
#> info [2026-10-20 18:51:07.383 UTC]: Step 1/3 load
#> info [2026-10-20 18:51:07.384 UTC]: Step 2/3 check
#> info [2026-10-20 18:51:07.385 UTC]: Step 3/3 model
#> info [2026-10-20 18:51:07.386 UTC]: Finished run of pipeflow 'restart'
p
#> <pipeflow> restart (3 steps)
#> ----------------------------
#>     step params depends state            out
#> 1:  load      n          done      1,2,3,4,5
#> 2: check      x    load  done         [NULL]
#> 3: model      x    load  done  2, 4, 6, 8,10
#> ----------------------------
#> <ready> last run: 2026-10-20 18:51:07

pip_set_params(p, list(n = 15)) # now halt() in 'check' step is triggered
pip_run(p)
#> info [2026-10-20 18:51:07.390 UTC]: Starting run of pipeflow 'restart'
#> info [2026-10-20 18:51:07.390 UTC]: Step 1/3 load
#> info [2026-10-20 18:51:07.391 UTC]: Step 2/3 check
#> info [2026-10-20 18:51:07.392 UTC]: Aborting pipeline execution on manual halt.
#> info [2026-10-20 18:51:07.392 UTC]: Finished run of pipeflow 'restart'

# Continue after errors and pass failures to the steps that allow them
p <- pip_new("continue") |>
  pip_add("data", \(n = 5) seq_len(n)) |>
  pip_add("check", \(x = ~data) stop("check failed")) |>
  pip_add("count", \(x = ~check) length(x)) |>
  pip_add("total", \(x = ~data) sum(x)) |>
  pip_add("report", \(total = ~total, n = ~try(count)) {
    if (inherits(n, "pipeflow_failure")) conditionMessage(n) else n
  })

withCallingHandlers(
  pip_run(p, lgr = NULL, on_error = "continue"),
  pipeflow_run_failed = \(w) {
    message("failed: ", toString(names(w$failed)))
    invokeRestart("muffleWarning")
  }
)
#> failed: check
p
#> <pipeflow> continue (5 steps)
#> -----------------------------
#>      step  params     depends  state
#> 1:   data       n               done
#> 2:  check       x        data failed
#> 3:  count       x       check    new
#> 4:  total       x        data   done
#> 5: report total,n total,count   done
#>                                            out
#> 1:                                   1,2,3,4,5
#> 2:                         <error/simpleError>
#> 3:                                      [NULL]
#> 4:                                          15
#> 5: step 'count' was not run: step 'check' f...
#> -----------------------------
#> <continued> last run: 2026-10-20 18:51:07
p[["report", "out"]]
#> [1] "step 'count' was not run: step 'check' failed: check failed"
```
