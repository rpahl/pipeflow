# Get or set unbound parameters

`pip_get_params()` returns the current default values of all unbound
parameters of a pipeline or view, that is, parameters wired to another
step's output via `~step_name` are excluded. `pip_set_params()` updates
these parameters for the whole pipeline or or view and marks the
affected steps and their downstream dependents as outdated.

## Usage

``` r
pip_get_params(x)

pip_set_params(x, params = list())
```

## Arguments

- x:

  A pipeflow pip or view.

- params:

  Named list of parameters to set (only used by `pip_set_params()`).

## Value

For `pip_get_params()`, a named list of unbound parameter values; if the
same parameter name appears in multiple steps, the first occurrence in
pipeline order is returned. For `pip_set_params()`, the updated pipeline
or view, invisibly.

## Note

Parameters of locked steps are never changed and their state remains
unchanged.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 10) seq_len(n)) |>
  pip_add("scale", \(x = ~load, factor = 0.5) x * factor)

# See all unbound/adjustable parameters before running
pip_get_params(p) # list(n = 10, factor = 0.5)
#> $n
#> [1] 10
#> 
#> $factor
#> [1] 0.5
#> 
(pip_run(p))
#> info [2026-09-27 17:53:08.720 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:08.720 UTC]: Step 1/2 load
#> info [2026-09-27 17:53:08.720 UTC]: Step 2/2 scale
#> info [2026-09-27 17:53:08.721 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state                             out
#> 1:  load        n          done             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load  done 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:08

# Updating params marks affected steps (and their dependents) outdated
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends    state                             out
#> 1:  load        n         outdated             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load outdated 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:08
(pip_run(p))
#> info [2026-09-27 17:53:08.726 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:08.726 UTC]: Step 1/2 load
#> info [2026-09-27 17:53:08.726 UTC]: Step 2/2 scale
#> info [2026-09-27 17:53:08.727 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:08

# Setting a parameter that is not defined in the pipeline yields a warning
# \donttest{
pip_set_params(p, params = list(nope = 1))
#> Warning: Trying to set parameters not defined in the target: nope
# }
```
