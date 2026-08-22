# Restart a pipeline run

Requests a restart of the current
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
execution. When called from within a step function (via the `.self`
argument), the pipeline run is aborted and restarted from the first
step. If a view was run, the view run is restarted together with its
upstream dependencies.

## Usage

``` r
pip_restart(x, force = TRUE, times = 1L)
```

## Arguments

- x:

  A pipeflow pip or view.

- force:

  Logical indicating if all steps should be forced to run on the
  restarted run. If `FALSE`, steps that are already in state `"done"`
  are skipped.

- times:

  Maximum number of restarts to request. Once the pipeline has been
  restarted `times` times, further calls of `pip_restart()` are ignored
  until the next run.

## Value

The updated pipeline or view, invisibly.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
for an advanced example of dynamic pipelines.

## Examples

``` r
p <- pip_new("restart") |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("model", \(x = ~load) {
    if (length(x) == 3L) {
      pip_restart(.self)
    }
    x * 2
  })

pip_run(p)
#> info [2026-08-22 17:08:02.976 UTC]: Starting run of pipeflow_pip 'restart'
#> info [2026-08-22 17:08:02.976 UTC]: Step 1/2 load
#> info [2026-08-22 17:08:02.976 UTC]: Step 2/2 model
#> info [2026-08-22 17:08:02.977 UTC]: Restarting pipeline execution.
#> info [2026-08-22 17:08:02.977 UTC]: Restarting run of pipeflow_pip 'restart'
#> info [2026-08-22 17:08:02.977 UTC]: Step 1/2 load
#> info [2026-08-22 17:08:02.978 UTC]: Step 2/2 model
#> info [2026-08-22 17:08:02.979 UTC]: Finished run of pipeflow_pip 'restart'
p
#> <pipeflow_pip> restart (2 steps)
#> --------------------------------
#>     step depends   out state
#> 1:  load         1,2,3  done
#> 2: model    load 2,4,6  done
```
