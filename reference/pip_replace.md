# Replace a step

Replaces a step's function while keeping it in the same position in the
pipeline. Downstream steps that have run before are automatically marked
as outdated and will re-run on the next
[`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_replace(
  x,
  step,
  fun,
  tags = NULL,
  params = list(),
  exec = NULL,
  keep_params = FALSE
)
```

## Arguments

- x:

  A pipeflow pipeline or view object.

- step:

  Name of the step to replace.

- fun:

  Function to execute for the step. Each function parameter needs a
  value: a default value in `fun` or an entry in `params`. Default
  values that are simple constants are resolved immediately. Default
  values that are formulas like `~other_step` are treated as
  dependencies to those steps and resolved to the respective output
  values at runtime once the step is executed. A reference wrapped in
  [`try()`](https://rdrr.io/r/base/try.html), like `~try(other_step)`,
  accepts a failed input when the pipeline was run with
  `on_error = "continue"` (see section 'Continuing after errors' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md)).

- tags:

  Optional character vector of tags belonging to the step. If `NULL`
  (the default), the current tags of the step are kept. Use
  `character(0)` to remove all tags.

- params:

  Optional named list of parameter values. They override the defaults of
  `fun` and can be used to give a value to parameters of `fun` that have
  no default. As in `fun`, a formula like `~other_step` refers to the
  output of another step. Names that are no parameters of `fun` are an
  error, unless `fun` has a `...` argument, which then takes them. Use
  cases for `params` are:

  1.  Set param values programmatically, e.g. when adding steps in a
      loop or at runtime.

  2.  Use existing functions whose parameters have no defaults, e.g.
      `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.

  3.  Provide extra param values to "mark" dependencies that are defined
      in pipelines nested in a step, which ensures that it is
      re-executed when one of the nested param values change. See
      [`vignette("v05b-nested-pipeline", package = "pipeflow")`](https://rpahl.github.io/pipeflow/articles/v05b-nested-pipeline.md).

- exec:

  Execution mode for the step, see
  [`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md).
  If `NULL` (the default), the current execution mode of the step is
  kept.

- keep_params:

  Logical. If `TRUE`, the new step keeps the current values of the
  unbound params of the old step that are also parameters of the new
  function, unless `params` give them or their default in the new
  function is a step reference (`~step`). The current values include the
  defaults of the old function, so a changed default of a kept param has
  no effect. Bound params are never kept, so replacing a step still
  unbinds them, and neither are params that went into the `...` of the
  old function: give them in `params` again. If `FALSE` (the default),
  all params are taken from the new function and `params`.

## Value

The updated pipeline, invisibly.

## Details

The step keeps its tags and execution mode by default, while output,
state and lock status are always reset, like for a freshly added step.
The parameters are taken from the new function and `params`, unless
`keep_params = TRUE`.

- `tags = NULL` and `exec = NULL` mean *keep the current value*. To
  change them, pass explicit values.

- `tags = character(0)` *removes all tags* and `exec = "auto"` *resets*
  the execution mode to its default.

As in
[`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md), a
warning is signalled if a param of the new function has another value
than the same param in other steps (see Params with the same name in
[`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md)).
So if you set a param with
[`pip_set_params()`](https://rpahl.github.io/pipeflow/reference/pip_set_params.md)
and replace the step, the new function's default causes this warning,
unless `keep_params = TRUE` keeps the value that was set.

## Examples

``` r
p <- pip_new() |>
    pip_add("load", \(n = 5) seq_len(n)) |>
    pip_add("double", \(x = ~load) x * 2)
pip_run(p)
#> info [2026-11-15 12:47:02.593 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-11-15 12:47:02.593 UTC]: Step 1/2 load
#> info [2026-11-15 12:47:02.594 UTC]: Step 2/2 double
#> info [2026-11-15 12:47:02.595 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state            out
#> 1:   load      n          done      1,2,3,4,5
#> 2: double      x    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:02 (0.002 s)
#> steps: 2 done

# Replace "load" — downstream steps are automatically marked "outdated"
pip_replace(p, "load", \(n = 3) seq_len(n))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends    state            out
#> 1:   load      n              new         [NULL]
#> 2: double      x    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:02 (0.002 s)
#> steps: 1 outdated, 1 new

# Re-run to bring everything up to date
pip_run(p)
#> info [2026-11-15 12:47:02.601 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-11-15 12:47:02.601 UTC]: Step 1/2 load
#> info [2026-11-15 12:47:02.602 UTC]: Step 2/2 double
#> info [2026-11-15 12:47:02.603 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state   out
#> 1:   load      n          done 1,2,3
#> 2: double      x    load  done 2,4,6
#> -------------------------
#> <ready> last run: 2026-11-15 12:47:02 (0.002 s)
#> steps: 2 done

# If a view is passed, the step must be part of the view
v <- pip_view(p, step = "double")
pip_replace(v, "double", \(x = ~load) x * 3)
p[["state"]]                                # "double" is "new" again
#>   load double 
#> "done"  "new" 

try(pip_replace(v, "load", \(n = 2) seq_len(n)))
#> Error : step 'load' is not part of the view

# Tags and execution mode are kept unless set explicitly
pip_tag(p, "io")
pip_replace(p, "load", \(n = 4) seq_len(n))
p[, "tags"]
#>      tags
#>    <list>
#> 1:     io
#> 2:     io
pip_replace(p, "load", \(n = 4) seq_len(n), tags = character(0))
p[, "tags"]
#>      tags
#>    <list>
#> 1:       
#> 2:     io

# Keep the params set before
pip_set_params(p, list(n = 10))
pip_replace(p, "load", \(n = 4) rev(seq_len(n)), keep_params = TRUE)
p[["load", "params"]]                       # n = 10, not 4
#> $n
#> [1] 10
#> 
```
