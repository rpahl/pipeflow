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

### Time stamps

The step table has two time stamps per step:

- `time_start`: the start of the step's last run. It is set each time
  the step is run, also if the run fails. It is `NA` for a step that has
  not run since it was added, replaced (see
  [`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md))
  or reset (see
  [`pip_reset()`](https://rpahl.github.io/pipeflow/dev/reference/pip_reset.md)).
  A skipped step (because it is `"done"`, locked or has a failed input)
  keeps its value.

- `time`: the time the step was added or replaced, or the end of its
  last successful run. A failed run does not change it. It is the time
  stamp that
  [`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md)
  gets in `ctx`.

With `t0 <- Sys.time()` taken before `pip_run()`, a step was run in that
run if `time_start >= t0`. Its last run succeeded if its state is
`"done"` and `time >= time_start`, and then `time - time_start` is the
duration of that run.

### Running views

When `x` is a view, the requested rows are run together with their
upstream dependencies, so the steps covered by the view are brought up
to date even if their inputs come from steps outside the view. The rest
of the pipeline is not executed; downstream steps of executed steps are
marked `"outdated"`. Steps whose inputs were not re-executed (for
example, because they were already `"done"`) keep their state.

### Runtime errors

By default, a failing step stops the run: the step gets state
`"failed"`, the pipeline run state becomes `"failed"`, and steps after
it that were not reached are marked `"outdated"`, so the next run
retries them.

The error message names the step, and for a step mapped over partitions
(see `exec` in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md))
also the partition key, e.g. `"step 'fit': key 'b': negative"`. The
error has class `pipeflow_step_error` and keeps the original error in
its field `parent`:
`tryCatch(pip_run(p), pipeflow_step_error = function(e) e$parent)`.

### Continuing after errors

With `on_error = "continue"`, the run does not stop at a failed step:

- The failed step gets state `"failed"`, and its error is stored as its
  output.

- Steps that depend on a failed step are not run, and neither are their
  own dependents. All other steps are run.

- To run a step despite a failed input, refer to that input via
  [`try()`](https://rdrr.io/r/base/try.html), e.g. `x = ~try(fit)` (see
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)).
  The argument then receives a failure object (an error condition) whose
  message tells which step failed, e.g. `"step 'fit' failed: negative"`.

- Locked steps are never run, so a failed input does not affect them.

A step mapped over partitions fails only for the keys whose call failed:

- Its output keeps the results of the other keys and holds a failure
  object for each failed key, e.g.
  `"step 'fit' failed: key 'b': negative"`. The step gets state
  `"failed"`, and printing the pipeline shows its output as e.g.
  `<partitioned: 1/3 failed>`.

- A mapped step after it is not run for the failed keys, whose output is
  then a failure object too
  (`"step 'show' was not run: step 'fit' failed: key 'b': negative"`),
  unless it takes the input via
  [`try()`](https://rdrr.io/r/base/try.html).

- A step that is not mapped (e.g. `exec = "reduce"`) is not run, unless
  it takes the input via [`try()`](https://rdrr.io/r/base/try.html).
  Then it receives the results of all keys, including the failure
  objects.

At the end of the run, the pipeline run state is `"continued"`, and a
warning of class `pipeflow_run_failed` lists the failed steps (with the
number of failed keys) and the steps that were not run.

The next run retries the failed steps and the steps that were not run;
once a failed step succeeds, the steps after it, including those that
received failure objects, are run again. A mapped step is only run again
for its failed keys, keeping the results of the other keys, unless
something changed that affects all keys: its parameters or an input step
(which marks it `"outdated"`), an input step that is run again in the
same run, or `force = TRUE`. See
[`vignette("v05c-error-handling", package = "pipeflow")`](https://rpahl.github.io/pipeflow/dev/articles/v05c-error-handling.md)
for examples.

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
#> info [2026-10-25 08:12:13.387 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-25 08:12:13.387 UTC]: Step 1/4 load
#> info [2026-10-25 08:12:13.388 UTC]: Step 2/4 prep
#> info [2026-10-25 08:12:13.390 UTC]: Step 3/4 square
#> info [2026-10-25 08:12:13.391 UTC]: Step 4/4 total
#> info [2026-10-25 08:12:13.392 UTC]: Finished in 0.005 s (4 done, 0 skipped, 0 failed)
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state   out
#> 1:   load        n          done 1,2,3
#> 2:   prep x,weight    load  done 1,2,3
#> 3: square        x    prep  done 1,4,9
#> 4:  total        x  square  done    14
#> -------------------------
#> <ready> last run: 2026-10-25 08:12:13

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
#> <ready> last run: 2026-10-25 08:12:13

# Already-done steps are skipped on a second run
pip_run(p) # first step skipped
#> info [2026-10-25 08:12:13.399 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-25 08:12:13.399 UTC]: Step 1/4 load - skipping done step
#> info [2026-10-25 08:12:13.399 UTC]: Step 2/4 prep
#> info [2026-10-25 08:12:13.400 UTC]: Step 3/4 square
#> info [2026-10-25 08:12:13.401 UTC]: Step 4/4 total
#> info [2026-10-25 08:12:13.403 UTC]: Finished in 0.004 s (3 done, 1 skipped, 0 failed)

# lgr = NULL suppresses log output
pip_run(p, lgr = NULL)

# force = TRUE re-executes every step regardless of state
pip_run(p, force = TRUE)
#> info [2026-10-25 08:12:13.405 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-25 08:12:13.405 UTC]: Step 1/4 load
#> info [2026-10-25 08:12:13.406 UTC]: Step 2/4 prep
#> info [2026-10-25 08:12:13.407 UTC]: Step 3/4 square
#> info [2026-10-25 08:12:13.408 UTC]: Step 4/4 total
#> info [2026-10-25 08:12:13.409 UTC]: Finished in 0.004 s (4 done, 0 skipped, 0 failed)
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state      out
#> 1:   load        n          done    1,2,3
#> 2:   prep x,weight    load  done    2,4,6
#> 3: square        x    prep  done  4,16,36
#> 4:  total        x  square  done       56
#> -------------------------
#> <ready> last run: 2026-10-25 08:12:13

# Run only a subset of steps via a view;
# upstream dependencies are automatically included
v <- pip_view(p, step = "total")
pip_run(v)
#> info [2026-10-25 08:12:13.414 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-25 08:12:13.415 UTC]: Step 1/4 [upstream] load - skipping done step
#> info [2026-10-25 08:12:13.415 UTC]: Step 2/4 [upstream] prep - skipping done step
#> info [2026-10-25 08:12:13.415 UTC]: Step 3/4 [upstream] square - skipping done step
#> info [2026-10-25 08:12:13.415 UTC]: Step 4/4 [view] total - skipping done step
#> info [2026-10-25 08:12:13.415 UTC]: Finished in 0.001 s (0 done, 4 skipped, 0 failed)

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
#> info [2026-10-25 08:12:13.418 UTC]: Starting run of pipeflow 'restart'
#> info [2026-10-25 08:12:13.418 UTC]: Step 1/3 load
#> info [2026-10-25 08:12:13.419 UTC]: Step 2/3 check
#> info [2026-10-25 08:12:13.420 UTC]: Step 3/3 model
#> info [2026-10-25 08:12:13.427 UTC]: Restarting pipeline execution.
#> info [2026-10-25 08:12:13.428 UTC]: Restarting run of pipeflow 'restart'
#> info [2026-10-25 08:12:13.428 UTC]: Step 1/3 load
#> info [2026-10-25 08:12:13.429 UTC]: Step 2/3 check
#> info [2026-10-25 08:12:13.430 UTC]: Step 3/3 model
#> info [2026-10-25 08:12:13.431 UTC]: Finished in 0.013 s (3 done, 0 skipped, 0 failed)
p
#> <pipeflow> restart (3 steps)
#> ----------------------------
#>     step params depends state            out
#> 1:  load      n          done      1,2,3,4,5
#> 2: check      x    load  done         [NULL]
#> 3: model      x    load  done  2, 4, 6, 8,10
#> ----------------------------
#> <ready> last run: 2026-10-25 08:12:13

pip_set_params(p, list(n = 15)) # now halt() in 'check' step is triggered
pip_run(p)
#> info [2026-10-25 08:12:13.436 UTC]: Starting run of pipeflow 'restart'
#> info [2026-10-25 08:12:13.436 UTC]: Step 1/3 load
#> info [2026-10-25 08:12:13.436 UTC]: Step 2/3 check
#> info [2026-10-25 08:12:13.437 UTC]: Aborting pipeline execution on manual halt.
#> info [2026-10-25 08:12:13.438 UTC]: Finished in 0.002 s (2 done, 1 skipped, 0 failed)

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
#> <continued> last run: 2026-10-25 08:12:13
p[["report", "out"]]
#> [1] "step 'count' was not run: step 'check' failed: check failed"
```
