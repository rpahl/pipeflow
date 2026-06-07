# Collect step outputs

Returns the outputs of all pipeline steps as a named list, optionally
grouped by step group label.

## Usage

``` r
pip_collect_out(x, grouped = TRUE)
```

## Arguments

- x:

  A pipeflow pip or view

- grouped:

  Logical indicating if the output should be grouped by step groups

## Value

A named list of outputs. If `grouped = TRUE`, groups with more than one
step are returned as nested named lists.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x, group = "io") |>
  pip_add("clean", \(x = ~load) x + 1, group = "io") |>
  pip_add("model", \(x = ~clean) x * 2, group = "model")
pip_run(p)
#> info [2026-06-07 15:34:07.239 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-07 15:34:07.239 UTC]: Step 1/3 load
#> info [2026-06-07 15:34:07.240 UTC]: Step 2/3 clean
#> info [2026-06-07 15:34:07.241 UTC]: Step 3/3 model
#> info [2026-06-07 15:34:07.242 UTC]: Finished run of pipeflow_pip 'pipe'

# grouped = TRUE (default): steps sharing a group become a nested list
pip_collect_out(p)
#> $io
#> $io$load
#> [1] 1
#> 
#> $io$clean
#> [1] 2
#> 
#> 
#> $model
#> [1] 4
#> 

# grouped = FALSE: flat named list with one entry per step
pip_collect_out(p, grouped = FALSE)
#> $load
#> [1] 1
#> 
#> $clean
#> [1] 2
#> 
#> $model
#> [1] 4
#> 

# Combine with pip_view to collect output from a specific group
pip_view(p, filter = list(group = "io")) |> pip_collect_out()
#> $io
#> $io$load
#> [1] 1
#> 
#> $io$clean
#> [1] 2
#> 
#> 
pip_view(p, filter = list(group = "model")) |> pip_collect_out()
#> $model
#> [1] 4
#> 
```
