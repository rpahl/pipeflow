# Get or set unbound parameters

`pip_get_params()` returns the current default values of all unbound
parameters of a pipeline or view (parameters wired to another step's
output via `~step_name` are excluded). `pip_set_params()` updates these
parameters for the whole pipeline or view except for locked steps. A
one-sided formula binds a parameter to the output of a step instead (see
below). Steps whose values change are marked as outdated, together with
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
  force = FALSE,
  ...
)
```

## Arguments

- x:

  A pipeflow pip or view.

- params:

  Named list of parameters to set. A one-sided formula binds the
  parameter to a step, see Binding parameters to steps.

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

- ...:

  Passed on to the methods of `` `pip_param_value<-`() ``, which
  `pip_set_params()` uses to determine the value to store.

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
parameter was set. To read the parameters of the unlocked steps only,
use a view that excludes the locked ones, `pip_get_params(p[!locked])`.

An environment (or a reference object such as an R6 object) that was
modified in place is identical to the stored one, so setting it again
doesn't outdate the step unless `force = TRUE`.

## Storing annotated values

The value that is stored for a step is the result of
`` `pip_param_value<-`() `` applied to the value the step currently
holds, which by default is the given value. A method of that generic can
merge the given value into an annotated object instead, so that its
annotations survive (see
[`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)).
The merged values of all affected steps are computed before anything is
changed: if a method fails, nothing is set and an error of class
`pipeflow_set_params_error` reports every failure at once. The merged
value is also what is compared with the stored one, so setting a plain
value that an annotated object already holds doesn't outdate the step.

## Binding parameters to steps

A one-sided formula binds a parameter to the output of a step, e.g.
`pip_set_params(p, list(model = ~fit))`, as it does in the defaults of a
step function or in `params` of
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md).
The parameter is bound in all selected steps that have it unbound,
except for locked steps. These steps must come after the step that the
formula refers to, otherwise an error is signalled and nothing is
changed. If you want to bind parameters only for certain steps, use a
view to select the desired steps. A relative reference such as `~-1` is
not allowed, but `~try(fit)` works as in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md).

A bound parameter is no longer unbound: `pip_get_params()` doesn't
return it, and `pip_set_params()` can't change it any more. Setting the
same binding again does nothing, and any other value is taken as an
unknown parameter (see `unknown`). To unbind or rebind it, replace the
step with
[`pip_replace()`](https://rpahl.github.io/pipeflow/dev/reference/pip_replace.md).
Therefore, a one-sided formula can't be set as value. Give it as a
string instead and turn it into a formula in the step, e.g. with
[`stats::as.formula()`](https://rdrr.io/r/stats/formula.html).

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
#> info [2026-10-26 16:50:51.056 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-26 16:50:51.056 UTC]: Step 1/2 load
#> info [2026-10-26 16:50:51.057 UTC]: Step 2/2 scale
#> info [2026-10-26 16:50:51.058 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state                             out
#> 1:  load        n          done             1,2,3,4,5,6,...[10]
#> 2: scale x,factor    load  done 0.5,1.0,1.5,2.0,2.5,3.0,...[10]
#> -------------------------
#> <ready> last run: 2026-10-26 16:50:51 (0.002 s)
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
#> <ready> last run: 2026-10-26 16:50:51 (0.002 s)
#> steps: 2 outdated
(pip_run(p))
#> info [2026-10-26 16:50:51.064 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-26 16:50:51.064 UTC]: Step 1/2 load
#> info [2026-10-26 16:50:51.065 UTC]: Step 2/2 scale
#> info [2026-10-26 16:50:51.065 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>     step   params depends state            out
#> 1:  load        n          done      1,2,3,4,5
#> 2: scale x,factor    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-26 16:50:51 (0.002 s)
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
#> <ready> last run: 2026-10-26 16:50:51 (0.002 s)
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
#> <ready> last run: 2026-10-26 16:50:51 (0.002 s)
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
#> info [2026-10-26 16:50:51.087 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-26 16:50:51.087 UTC]: Step 1/5 load
#> info [2026-10-26 16:50:51.088 UTC]: Step 2/5 fit_mean
#> info [2026-10-26 16:50:51.089 UTC]: Step 3/5 fit_max
#> info [2026-10-26 16:50:51.090 UTC]: Step 4/5 report
#> info [2026-10-26 16:50:51.090 UTC]: Step 5/5 report_mean
#> info [2026-10-26 16:50:51.091 UTC]: Finished in 0.004 s (5 done, 0 skipped, 0 failed)
q[["report", "out"]]
#> [1] "result: 5"
q[["report_mean", "out"]]
#> [1] "mean: 3"
```
