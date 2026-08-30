# Stop a pipeline run

Aborts the current
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
execution. When called from within a step function (via the `.self`
argument), the pipeline run is stopped after the current step. Steps
that were not executed are marked as `"outdated"`. If a view was run,
only the steps covered by the view are affected.

## Usage

``` r
pip_stop(x)
```

## Arguments

- x:

  A pipeflow pip or view.

## Value

The updated pipeline or view, invisibly.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
for an advanced example of dynamic pipelines.

## Examples

``` r
p <- pip_new("stop") |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("model", \(x = ~load) {
    if (length(x) == 3L) {
      pip_stop(.self)
    }
    x * 2
  }) |>
  pip_add("report", \(x = ~model) paste("result:", x))

pip_run(p)
#> info [2026-08-22 17:08:03.499 UTC]: Starting run of pipeflow 'stop'
#> info [2026-08-22 17:08:03.500 UTC]: Step 1/3 load
#> info [2026-08-22 17:08:03.500 UTC]: Step 2/3 model
#> info [2026-08-22 17:08:03.501 UTC]: Aborting pipeline execution on manual stop.
#> info [2026-08-22 17:08:03.501 UTC]: Finished run of pipeflow 'stop'
p
#> <pipeflow> stop (3 steps)
#> -----------------------------
#>      step depends    out    state
#> 1:   load          1,2,3     done
#> 2:  model    load  2,4,6     done
#> 3: report   model [NULL] outdated
```
