# Run a pipeline

Executes all pending steps in order. On repeated runs, steps that are
already `"done"` are skipped and only steps that are still `"new"` or
were marked `"outdated"` (because one of their dependencies changed) are
executed. Use `force = TRUE` to re-execute every step.

## Usage

``` r
pip_run(x, lgr = pipeflow_lgr, force = FALSE, progress = NULL)
```

## Arguments

- x:

  A pipeflow pip or view

- lgr:

  A logging function of the form `function(level, msg, ...)`. To
  suppress logging, you can set `lgr = NULL`.

- force:

  Logical indicating if all steps should be forced to run, regardless of
  whether they are outdated or not.

- progress:

  Optional callback of the form `function(value, detail)` called before
  each step.

## Value

The updated pipeline or view, invisibly.

## Details

### Step states

A step can take the following states:

- new: the step has been added but not yet executed, or was reset via
  [`pip_reset()`](https://github.com/rpahl/pipeflow/reference/pip_reset.md).

- outdated: a step can get outdated in the following scenarios:

  - one of the step's parameters were changed (e.g. via
    [`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md))

  - a step it depends on was re-executed or replaced

  - the last run did not reach it either on purpose (see section
    'Running views below) or because a run was aborted early due to
    failed step

- done: the step was executed successfully with its current inputs

- failed: the step raised an error during its last execution

A "done" step is skipped unless `force = TRUE` was set. For all other
states, the step will be re-executed in the next `pip_run()`.

### Running views

When `x` is a view, the requested rows are run together with their
upstream dependencies, so the steps covered by the view are brought up
to date even if their inputs come from steps outside the view. The rest
of the pipeline is not executed; downstream steps that were not
processed are marked `"outdated"`.

### Runtime errors

If a step fails with an error, the failing step's state is set to
`"failed"`, the run is aborted (no further steps are executed), and the
pipeline run state is set to `"failed"`. Steps that have not been
executed are marked `"outdated"`, so a subsequent run retries them.

### Runtime control flow via restart and halt

A running pipeline can be interrupted via the `restart()` and `halt()`
virtual methods. They are intended for advanced, self-modifying
pipelines and are most often called from within a step function via the
`.self` argument.

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
  and the pipeline is not marked as `"failed"`. The run simply ends, the
  steps that have not been executed are marked `"outdated"`, and a
  subsequent `pip_run()` will continue where the run left off.

In both cases steps that have not been executed until the restart or
halt happens are marked as `"outdated"`.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
for an advanced example of dynamic pipelines.

## Examples

``` r
p <- pip_new() |>
pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("prep", \(x = ~load, weight = 1) x * weight) |>
  pip_add("square", \(x = ~prep) x^2) |>
  pip_add("total", \(x = ~square) sum(x))

pip_run(p)
#> info [2026-09-27 17:53:08.417 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:08.417 UTC]: Step 1/4 load
#> info [2026-09-27 17:53:08.417 UTC]: Step 2/4 prep
#> info [2026-09-27 17:53:08.418 UTC]: Step 3/4 square
#> info [2026-09-27 17:53:08.419 UTC]: Step 4/4 total
#> info [2026-09-27 17:53:08.420 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state   out
#> 1:   load        n          done 1,2,3
#> 2:   prep x,weight    load  done 1,2,3
#> 3: square        x    prep  done 1,4,9
#> 4:  total        x  square  done    14
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:08

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
#> <ready> last run: 2026-09-27 19:53:08

# Already-done steps are skipped on a second run
pip_run(p) # first step skipped
#> info [2026-09-27 17:53:08.426 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:08.426 UTC]: Step 1/4 load - skipping done step
#> info [2026-09-27 17:53:08.426 UTC]: Step 2/4 prep
#> info [2026-09-27 17:53:08.427 UTC]: Step 3/4 square
#> info [2026-09-27 17:53:08.428 UTC]: Step 4/4 total
#> info [2026-09-27 17:53:08.429 UTC]: Finished run of pipeflow 'pipe'

# lgr = NULL suppresses log output
pip_run(p, lgr = NULL)

# force = TRUE re-executes every step regardless of state
pip_run(p, force = TRUE)
#> info [2026-09-27 17:53:08.430 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:08.430 UTC]: Step 1/4 load
#> info [2026-09-27 17:53:08.430 UTC]: Step 2/4 prep
#> info [2026-09-27 17:53:08.431 UTC]: Step 3/4 square
#> info [2026-09-27 17:53:08.432 UTC]: Step 4/4 total
#> info [2026-09-27 17:53:08.433 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (4 steps)
#> -------------------------
#>      step   params depends state      out
#> 1:   load        n          done    1,2,3
#> 2:   prep x,weight    load  done    2,4,6
#> 3: square        x    prep  done  4,16,36
#> 4:  total        x  square  done       56
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:08

# Run only a subset of steps via a view;
# upstream dependencies are automatically included
v <- pip_view(p, step = "total")
pip_run(v)
#> info [2026-09-27 17:53:08.437 UTC]: Starting run of pipeflow 'pipe view'
#> info [2026-09-27 17:53:08.437 UTC]: Step 1/4 [upstream] load - skipping done step
#> info [2026-09-27 17:53:08.438 UTC]: Step 2/4 [upstream] prep - skipping done step
#> info [2026-09-27 17:53:08.438 UTC]: Step 3/4 [upstream] square - skipping done step
#> info [2026-09-27 17:53:08.438 UTC]: Step 4/4 [view] total - skipping done step
#> info [2026-09-27 17:53:08.438 UTC]: Finished run of pipeflow 'pipe view'

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
#> info [2026-09-27 17:53:08.440 UTC]: Starting run of pipeflow 'restart'
#> info [2026-09-27 17:53:08.440 UTC]: Step 1/3 load
#> info [2026-09-27 17:53:08.441 UTC]: Step 2/3 check
#> info [2026-09-27 17:53:08.442 UTC]: Step 3/3 model
#> info [2026-09-27 17:53:08.448 UTC]: Restarting pipeline execution.
#> info [2026-09-27 17:53:08.448 UTC]: Restarting run of pipeflow 'restart'
#> info [2026-09-27 17:53:08.448 UTC]: Step 1/3 load
#> info [2026-09-27 17:53:08.448 UTC]: Step 2/3 check
#> info [2026-09-27 17:53:08.449 UTC]: Step 3/3 model
#> info [2026-09-27 17:53:08.450 UTC]: Finished run of pipeflow 'restart'
p
#> <pipeflow> restart (3 steps)
#> ----------------------------
#>     step params depends state            out
#> 1:  load      n          done      1,2,3,4,5
#> 2: check      x    load  done         [NULL]
#> 3: model      x    load  done  2, 4, 6, 8,10
#> ----------------------------
#> <ready> last run: 2026-09-27 19:53:08

pip_set_params(p, list(n = 15)) # now halt() in 'check' step is triggered
pip_run(p)
#> info [2026-09-27 17:53:08.454 UTC]: Starting run of pipeflow 'restart'
#> info [2026-09-27 17:53:08.454 UTC]: Step 1/3 load
#> info [2026-09-27 17:53:08.454 UTC]: Step 2/3 check
#> info [2026-09-27 17:53:08.455 UTC]: Aborting pipeline execution on manual halt.
#> info [2026-09-27 17:53:08.455 UTC]: Finished run of pipeflow 'restart'
```
