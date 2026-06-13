# Set independent parameters

Updates the default values of tunable parameters across the pipeline.
Affected steps and their downstream dependents are automatically marked
as outdated.

## Usage

``` r
pip_set_params(p, params = list())
```

## Arguments

- p:

  A pipeflow pip or view

- params:

  Named list of parameters to set.

## Value

The updated pipeline or view, invisibly.

## Details

Parameters of locked steps are never changed and their state remains
unchanged.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 10) seq_len(n)) |>
  pip_add("scale", \(x = ~load, factor = 0.5) x * factor)

# See all adjustable parameters before running
pip_get_params(p) # list(n = 10, factor = 0.5)
#> $n
#> [1] 10
#> 
#> $factor
#> [1] 0.5
#> 

# Updating params marks affected steps (and their dependents) outdated
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>     step depends    out    state
#> 1:  load         [NULL] outdated
#> 2: scale    load [NULL] outdated

pip_run(p)
#> info [2026-06-13 17:22:31.066 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-13 17:22:31.066 UTC]: Step 1/2 load
#> info [2026-06-13 17:22:31.067 UTC]: Step 2/2 scale
#> info [2026-06-13 17:22:31.068 UTC]: Finished run of pipeflow_pip 'pipe'
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>     step depends            out state
#> 1:  load              1,2,3,4,5  done
#> 2: scale    load  2, 4, 6, 8,10  done
```
