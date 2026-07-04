# Collect step outputs

Returns the outputs of all pipeline steps as a flat named list keyed by
step name. Use
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
to narrow the selection before collecting, and compose calls if grouped
output is needed.

## Usage

``` r
pip_collect_out(x)
```

## Arguments

- x:

  A pipeflow pip or view.

## Value

A named list of outputs, one element per step.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x, tags = "io") |>
  pip_add("clean", \(x = ~load) x + 1, tags = "io") |>
  pip_add("model", \(x = ~clean) x * 2, tags = "model")
pip_run(p)
#> info [2026-06-20 19:19:05.402 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-20 19:19:05.402 UTC]: Step 1/3 load
#> info [2026-06-20 19:19:05.402 UTC]: Step 2/3 clean
#> info [2026-06-20 19:19:05.404 UTC]: Step 3/3 model
#> info [2026-06-20 19:19:05.405 UTC]: Finished run of pipeflow_pip 'pipe'

# Flat named list with one entry per step
pip_collect_out(p)
#> $load
#> [1] 1
#> 
#> $clean
#> [1] 2
#> 
#> $model
#> [1] 4
#> 

# Combine with pip_view to collect output for specific tags
grouped <- list(
  io = pip_view(p, tags = "io") |> pip_collect_out(),
  model = pip_view(p, tags = "model") |> pip_collect_out()
)
grouped
#> $io
#> $io$load
#> [1] 1
#> 
#> $io$clean
#> [1] 2
#> 
#> 
#> $model
#> $model$model
#> [1] 4
#> 
#> 
```
