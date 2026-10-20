# Replace a step

Replaces a step's function while keeping it in the same position in the
pipeline. Downstream steps that have run before are automatically marked
as outdated and will re-run on the next
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md).

## Usage

``` r
pip_replace(x, step, fun, tags = NULL, params = list(), exec = NULL)
```

## Arguments

- x:

  A pipeflow pipeline or view object.

- step:

  Unique step name.

- fun:

  Function to execute for the step. Each function parameter needs a
  value: a default value in `fun` or an entry in `params`. Default
  values that are simple constants are resolved immediately. Default
  values that are formulas like `~other_step` are treated as
  dependencies to those steps and resolved to the respective output
  values at runtime once the step is executed. A reference wrapped in
  [`try()`](https://rdrr.io/r/base/try.html), like `~try(other_step)`,
  accepts a failed input when the pipeline is run with
  `on_error = "continue"`: instead of the output of the referenced step,
  the argument then receives a condition of class `pipeflow_failure`
  (see section 'Continuing after errors' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)).
  The function can refer to the running pipeline as `.self` (see section
  'The pipeline as `.self`' in
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)).

- tags:

  Optional character vector of tags belonging to the step. If `NULL`
  (the default), the current tags of the step are kept. Use
  `character(0)` to remove all tags.

- params:

  Optional named list of parameter values. They override the defaults of
  `fun` and give a value to parameters of `fun` that have no default. As
  in the defaults of `fun`, a formula like `~other_step` refers to the
  output of another step. Names that are no parameters of `fun` add
  params to the step, which are passed on to `fun` as well, so `fun`
  needs a `...` argument for them. Use cases for `params` are:

  1.  Set param values programmatically, e.g. when adding steps in a
      loop or at runtime.

  2.  Use existing functions whose parameters have no defaults, e.g.
      `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.

  3.  Provide extra param values to "mark" dependencies that are defined
      in pipelines nested in a step, which ensures that it is
      re-executed when one of the nested param values change. See the
      [nested pipelines
      vignette](https://rpahl.github.io/pipeflow/dev/doc/v05b-nested-pipeline.md).

- exec:

  Execution mode for the step, see
  [`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md).
  If `NULL` (the default), the current execution mode of the step is
  kept.

## Value

The updated pipeline, invisibly.

## Details

By default, the step keeps its tags and execution mode: `tags = NULL`
and `exec = NULL` mean "keep the current value". To change them, pass
explicit values, where `tags = character(0)` removes all tags and
`exec = "auto"` resets the execution mode to its default. Parameters,
output, state and lock status are always reset, like for a freshly added
step.

## Examples

``` r
p <- pip_new() |>
    pip_add("load", \(n = 5) seq_len(n)) |>
    pip_add("double", \(x = ~load) x * 2)
pip_run(p)
#> info [2026-10-20 18:51:06.889 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:06.889 UTC]: Step 1/2 load
#> info [2026-10-20 18:51:06.890 UTC]: Step 2/2 double
#> info [2026-10-20 18:51:06.891 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state            out
#> 1:   load      n          done      1,2,3,4,5
#> 2: double      x    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:06

# Replace "load" — downstream steps are automatically marked "outdated"
pip_replace(p, "load", \(n = 3) seq_len(n))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends    state            out
#> 1:   load      n              new         [NULL]
#> 2: double      x    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:06

# Re-run to bring everything up to date
pip_run(p)
#> info [2026-10-20 18:51:06.897 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-20 18:51:06.898 UTC]: Step 1/2 load
#> info [2026-10-20 18:51:06.898 UTC]: Step 2/2 double
#> info [2026-10-20 18:51:06.899 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state   out
#> 1:   load      n          done 1,2,3
#> 2: double      x    load  done 2,4,6
#> -------------------------
#> <ready> last run: 2026-10-20 18:51:06

# If a view is passed, the step must be part of the view
v <- pip_view(p, step = "double")
pip_replace(v, "double", \(x = ~load) x * 3)
p[["state"]]                                # "double" is "new" again
#>   load double 
#> "done"  "new" 

try(pip_replace(v, "load", \(n = 2) seq_len(n)))
#> Error in pip_replace(v, "load", function(n = 2) seq_len(n)) : 
#>   step 'load' is not part of the view

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
```
