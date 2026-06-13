# Get independent parameters

Returns the current default values of all tunable (non-dependency)
parameters across the pipeline. These are the parameters that can be
updated via
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md).
Parameters wired to another step's output via `~step_name` are excluded.

## Usage

``` r
pip_get_params(x)
```

## Arguments

- x:

  A pipeflow pip or view

## Value

Named list of tunable parameter values. If the same parameter name
appears in multiple steps, the first occurrence in pipeline order is
returned.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 100, seed = 42) seq_len(n)) |>
  pip_add("model", \(x = ~load, lambda = 0.1) x * lambda)

# ~load is a dependency — only non-dependency params are returned
pip_get_params(p) # list(n = 100, seed = 42, lambda = 0.1)
#> $n
#> [1] 100
#> 
#> $seed
#> [1] 42
#> 
#> $lambda
#> [1] 0.1
#> 

# Useful as a guide for pip_set_params()
pip_set_params(p, params = list(n = 20, lambda = 0.5))
pip_run(p) |> pip_collect_out()
#> info [2026-06-13 17:22:27.689 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-13 17:22:27.690 UTC]: Step 1/2 load
#> info [2026-06-13 17:22:27.690 UTC]: Step 2/2 model
#> info [2026-06-13 17:22:27.692 UTC]: Finished run of pipeflow_pip 'pipe'
#> $load
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#> 
#> $model
#>  [1]  0.5  1.0  1.5  2.0  2.5  3.0  3.5  4.0  4.5  5.0  5.5  6.0  6.5  7.0  7.5
#> [16]  8.0  8.5  9.0  9.5 10.0
#> 
```
