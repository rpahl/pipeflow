# Get or set unbound parameters

`pip_get_params()` returns the current default values of all unbound
parameters of a pipeline or view (parameters wired to another step's
output via `~step_name` are excluded), including those of locked steps.
`pip_set_params()` updates these parameters for the whole pipeline or
view. Steps whose values change are marked as outdated, together with
their downstream dependents (steps that have never run stay `"new"`).
Values that are [`identical()`](https://rdrr.io/r/base/identical.html)
to the stored ones are skipped and don't outdate any step, unless
`force = TRUE`.

## Usage

``` r
pip_get_params(x)

pip_set_params(
  x,
  params = list(),
  unknown = c("warn", "ignore", "error"),
  force = FALSE
)
```

## Arguments

- x:

  A pipeflow pip or view.

- params:

  Named list of parameters to set

- unknown:

  How to handle `params` that are not defined in any of the selected
  steps, including locked ones: `"warn"` (default) signals a warning and
  sets the remaining parameters, `"ignore"` does so silently, and
  `"error"` fails before anything is changed.

- force:

  Logical. If `TRUE`, every step that has one of the `params` is
  outdated (together with its downstream steps), even if the values are
  [`identical()`](https://rdrr.io/r/base/identical.html) to the stored
  ones. This refreshes steps whose inputs are environments or reference
  objects that were modified in place.

## Value

For `pip_get_params()`, a named list of unbound parameter values; if the
same parameter name appears in multiple steps, the first occurrence in
pipeline order is returned. For `pip_set_params()`, the updated pipeline
or view, invisibly.

## Note

Parameters of locked steps are never changed and their state remains
unchanged. `pip_get_params()` still returns them, so if a locked step
comes first among the steps that share a parameter name,
`pip_get_params()` keeps returning the locked step's value after the
parameter was set.

An environment (or a reference object such as an R6 object) that was
modified in place is identical to the stored one, so setting it again
doesn't outdate the step unless `force = TRUE`.

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
#> info [2026-10-15 10:41:14.418 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-15 10:41:14.418 UTC]: Step 1/2 load
#> info [2026-10-15 10:41:14.419 UTC]: Step 2/2 scale
#> info [2026-10-15 10:41:14.420 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state                             out
#> 1:  load        n          done             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load  done 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-10-15 10:41:14

# Updating params marks affected steps (and their dependents) outdated
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends    state                             out
#> 1:  load        n         outdated             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load outdated 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-10-15 10:41:14
(pip_run(p))
#> info [2026-10-15 10:41:14.426 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-15 10:41:14.426 UTC]: Step 1/2 load
#> info [2026-10-15 10:41:14.426 UTC]: Step 2/2 scale
#> info [2026-10-15 10:41:14.427 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-15 10:41:14

# Setting the same values again doesn't outdate any step
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-15 10:41:14

# Use force = TRUE to outdate the steps anyway
pip_set_params(p, params = list(n = 5, factor = 2.0), force = TRUE)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends    state            out
#> 1:  load        n         outdated      1,2,3,4,5
#> 2: scale x,factor    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-15 10:41:14

# Setting a parameter that is not defined in the pipeline yields a warning
# \donttest{
pip_set_params(p, params = list(nope = 1))
#> Warning: Trying to set parameters not defined in the target: nope
# }

# ... which can be silenced or turned into an error
pip_set_params(p, params = list(nope = 1), unknown = "ignore")
try(pip_set_params(p, params = list(nope = 1), unknown = "error"))
#> Error : Trying to set parameters not defined in the target: nope
```
