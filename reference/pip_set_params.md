# Get or set unbound parameters

`pip_get_params()` returns the current default values of all unbound
parameters of a pipeline or view (i.e., parameters bound to another step
via `~step_name` are excluded). `pip_set_params()` updates these
parameters in all steps that have them, except for locked steps. Steps
whose values change are marked as outdated, together with their
downstream dependents. Values that are
[`identical()`](https://rdrr.io/r/base/identical.html) to the stored
ones are skipped and don't outdate any step, unless `force = TRUE`.

## Usage

``` r
pip_get_params(x, simplify = TRUE)

pip_set_params(
  x,
  params = list(),
  unknown = c("warn", "ignore", "error"),
  force = FALSE,
  scope = c("pipeline", "view"),
  ...
)
```

## Arguments

- x:

  A pipeflow pip or view.

- simplify:

  If `TRUE` `pip_get_params()` returns one value per parameter name,
  otherwise the values of all steps, grouped by name.

- params:

  A named list of parameters to set. One-sided formulas bind parameters
  to a step, see Binding parameters to steps.

- unknown:

  How to handle undefined `params`:

  - `"warn"` (default) signals a warning and sets the remaining
    parameters,

  - `"ignore"` does so silently,

  - `"error"` fails before anything is changed

- force:

  Logical. If `TRUE`, every step affected by a parameter update is
  outdated even if the values were
  [`identical()`](https://rdrr.io/r/base/identical.html) to the stored
  ones.

- scope:

  Defines the scope of steps for the parameter update if `x` is a view:

  - `"pipeline"` (default) sets them in all steps of the pipeline that
    have the param

  - `"view"` only in the steps of the view (except bindings, see Views
    and scope)

- ...:

  Passed on to the methods of `` `pip_param_value<-`() ``

## Value

For `pip_get_params()`, a named list of unbound parameter values; if the
same parameter name appears in multiple steps, the first occurrence in
pipeline order is returned. With `simplify = FALSE`, a named list with
an element per parameter name, which is a list of the values named by
step, e.g. `list(x = list(s1 = 1, s2 = 2), y = list(s1 = 3))`. For
`pip_set_params()`, the updated pipeline or view, invisibly.

## Views and scope

On a view, a parameter name is updated if it appears in one of the
view's steps, otherwise it is considered unknown (see `unknown`). With
`scope = "pipeline"`, the value is spread to every unbound step of the
whole pipeline, so that all names get the same value, as
`pip_get_params()` assumes. With `scope = "view"`, the update is limited
to the steps of the view.

Bindings ignore `scope`: a one-sided formula binds a param only in the
steps of the view. For example, in
`pip_set_params(v, list(alpha = 5, data = ~load))`, `alpha` is set in
the whole pipeline while `data` is only bound in the steps of `v`.

## Binding parameters to steps

A one-sided formula binds a parameter to the output of a step, e.g.
`pip_set_params(p, list(model = ~fit))`, as in
[`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md).

- The parameter is bound in *all selected steps*, except for *locked
  steps*. To bind it only in certain steps, select them with a view.

- The bound steps *must come after* the step that the formula refers to,
  otherwise an error is signalled and nothing is changed.

- A *relative reference* such as `~-1` is not allowed, but `~try(fit)`
  works as in
  [`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md).

A bound parameter is no longer unbound: `pip_get_params()` doesn't
return it, and `pip_set_params()` can't change it any more. To unbind or
rebind it, replace the step with
[`pip_replace()`](https://rpahl.github.io/pipeflow/reference/pip_replace.md).
A one-sided formula therfore can't be set as value. Give it as a string
instead and turn it into a formula in the step, e.g. with
[`stats::as.formula()`](https://rdrr.io/r/stats/formula.html).

## Locked steps

Parameters of locked steps are never changed and their state remains
unchanged. `pip_get_params()` still returns them, so if a locked step
comes first among the steps that share a parameter name,
`pip_get_params()` keeps returning the locked step's value after the
parameter was set. To read the parameters of the unlocked steps only,
use a view that excludes the locked ones, `pip_get_params(p[!locked])`.

An environment (or a reference object such as an R6 object) that was
modified in place is identical to the stored one, so setting it again
doesn't outdate the step unless `force = TRUE`.

## Storing annotated values

A new value does not simply replace the stored one. For a step whose
param currently holds `x`, the pipeline basically does
`pip_param_value(x) <- new_value` and stores the result. For plain
values, this is just `new_value`. For an annotated object (e.g. a value
with units or labels), a method for its class can instead merge
`new_value` into `x`, so that the annotations survive (see
[`pip_param_value()`](https://rpahl.github.io/pipeflow/reference/pip_param_value.md)).
Here, the merging follows these rules:

- *All or nothing*: the merged values of all affected steps are computed
  before anything is changed. If a method fails, nothing is set and an
  error of class `pipeflow_set_params_error` reports every failure at
  once.

- *Comparison*: the merged value is what is compared with the stored
  one, so setting a plain value that an annotated object already holds
  doesn't outdate the step.

## See also

[`pip_param_value()`](https://rpahl.github.io/pipeflow/reference/pip_param_value.md)
for storing and passing on annotated parameter values.

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
#> info [2026-11-15 12:47:03.937 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-11-15 12:47:03.937 UTC]: Step 1/2 load
#> info [2026-11-15 12:47:03.938 UTC]: Step 2/2 scale
#> info [2026-11-15 12:47:03.939 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state                             out
#> 1:  load        n          done             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load  done 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:03 (0.002 s)
#> steps: 2 done

# Updating params marks affected steps (and their dependents) outdated
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends    state                             out
#> 1:  load        n         outdated             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load outdated 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:03 (0.002 s)
#> steps: 2 outdated
(pip_run(p))
#> info [2026-11-15 12:47:03.945 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-11-15 12:47:03.945 UTC]: Step 1/2 load
#> info [2026-11-15 12:47:03.946 UTC]: Step 2/2 scale
#> info [2026-11-15 12:47:03.947 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:03 (0.002 s)
#> steps: 2 done

# Setting the same values again doesn't outdate any step
pip_set_params(p, params = list(n = 5, factor = 2.0))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:03 (0.002 s)
#> steps: 2 done

# Use force = TRUE to outdate the steps anyway
pip_set_params(p, params = list(n = 5, factor = 2.0), force = TRUE)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends    state            out
#> 1:  load        n         outdated      1,2,3,4,5
#> 2: scale x,factor    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:03 (0.002 s)
#> steps: 2 outdated

# Setting a parameter that is not defined in the pipeline yields a warning
# \donttest{
pip_set_params(p, params = list(nope = 1))
#> Warning: Trying to set parameters not defined in the target: nope
# }
# ... which can be silenced or turned into an error
pip_set_params(p, params = list(nope = 1), unknown = "ignore")
try(pip_set_params(p, params = list(nope = 1), unknown = "error"))
#> Error : Trying to set parameters not defined in the target: nope

# Show the values of all steps with simplify = FALSE
d <- suppressWarnings(
  pip_new() |>
    pip_add("s1", \(x = 1, y = 2) x + y) |>
    pip_add("s2", \(x = 3) x)
)
pip_get_params(d, simplify = FALSE)
#> $x
#> $x$s1
#> [1] 1
#> 
#> $x$s2
#> [1] 3
#> 
#> 
#> $y
#> $y$s1
#> [1] 2
#> 
#> 

# pip_get_params() includes locked steps; a view can exclude them
pip_lock(p["load"])
pip_get_params(p) # list(n = 5, factor = 2.0)
#> $n
#> [1] 5
#> 
#> $factor
#> [1] 2
#> 
pip_get_params(p[!locked]) # list(factor = 2.0)
#> $factor
#> [1] 2
#> 

# On a view, a value is set in the whole pipeline unless scope = "view"
r <- pip_new() |>
  pip_add("fit", \(alpha = 1) alpha) |>
  pip_add("plot", \(x = ~fit, alpha = 1) c(x, alpha), tags = "rep")
pip_set_params(pip_view(r, tags = "rep"), list(alpha = 5))
r[["params"]] # alpha = 5 in both steps
#> $fit
#> $fit$alpha
#> [1] 5
#> 
#> 
#> $plot
#> $plot$x
#> ~fit
#> <environment: 0x55b76ad0b818>
#> 
#> $plot$alpha
#> [1] 5
#> 
#> 
pip_set_params(pip_view(r, tags = "rep"), list(alpha = 2), scope = "view")
r[["params"]] # alpha = 5 in "fit", 2 in "plot"
#> $fit
#> $fit$alpha
#> [1] 5
#> 
#> 
#> $plot
#> $plot$x
#> ~fit
#> <environment: 0x55b76ad0b818>
#> 
#> $plot$alpha
#> [1] 2
#> 
#> 

# A formula binds a parameter to the output of a step
q <- pip_new() |>
  pip_add("load", \(n = 5) seq_len(n)) |>
  pip_add("fit_mean", \(x = ~load) mean(x)) |>
  pip_add("fit_max", \(x = ~load) max(x)) |>
  pip_add("report", \(model = NA) paste("result:", model)) |>
  pip_add("report_mean", \(model = NA) paste("mean:", model))

# A view selects the steps to bind, here only "report_mean"
pip_set_params(q["report_mean"], params = list(model = ~fit_mean))

# Without a view, the param is bound in all steps that have it unbound
pip_set_params(q, params = list(model = ~fit_max))
q
#> <pipeflow> pipe (5 steps)
#> -------------------------
#>           step params  depends state
#> 1:        load      n            new
#> 2:    fit_mean      x     load   new
#> 3:     fit_max      x     load   new
#> 4:      report  model  fit_max   new
#> 5: report_mean  model fit_mean   new
#> -------------------------
#> <ready> last run: never
#> steps: 5 new
pip_run(q)
#> info [2026-11-15 12:47:03.979 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-11-15 12:47:03.980 UTC]: Step 1/5 load
#> info [2026-11-15 12:47:03.981 UTC]: Step 2/5 fit_mean
#> info [2026-11-15 12:47:03.981 UTC]: Step 3/5 fit_max
#> info [2026-11-15 12:47:03.982 UTC]: Step 4/5 report
#> info [2026-11-15 12:47:03.983 UTC]: Step 5/5 report_mean
#> info [2026-11-15 12:47:03.983 UTC]: Finished in 0.004 s (5 done, 0 skipped, 0 failed)
q[["report", "out"]]
#> [1] "result: 5"
q[["report_mean", "out"]]
#> [1] "mean: 3"
```
