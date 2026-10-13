# Replace a step

Replaces a step's function while keeping it in the same position in the
pipeline. Downstream steps that have run before are automatically marked
as outdated and will re-run on the next
[`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md).

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

  Function to execute for the step. Each function parameter must have a
  default value. Default values that are simple constants are resolved
  immediately. Default values that are formulas like `~other_step` are
  treated as dependencies to those steps and resolved to the respective
  output values at runtime once the step is executed. A reference
  wrapped in [`try()`](https://rdrr.io/r/base/try.html), like
  `~try(other_step)`, accepts a failed input when the pipeline is run
  with `on_error = "continue"`: instead of the output of the referenced
  step, the argument then receives a condition of class
  `pipeflow_failure` (see section 'Continuing after errors' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md)).
  The function can refer to the running pipeline as `.self` (see section
  'The pipeline as `.self`' in
  [`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md)).

- tags:

  Optional character vector of tags belonging to the step. If `NULL`
  (the default), the current tags of the step are kept. Use
  `character(0)` to remove all tags.

- params:

  Optional named list of parameter values, which will be merged with the
  defaults of `fun` (if overlapping names, the default values in `fun`
  take precedence). There are two use cases for `params`:

  1.  Provide param values programmatically when adding steps at runtime

  2.  Provide extra param values to "mark" dependencies that are defined
      in pipelines nested in a step, which ensures that the step (and
      with that the pipeline in the step) is re-executed when one of the
      respective param values change.

- exec:

  Execution mode for the step, see
  [`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md).
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
#> info [2026-10-13 15:27:08.072 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-13 15:27:08.072 UTC]: Step 1/2 load
#> info [2026-10-13 15:27:08.072 UTC]: Step 2/2 double
#> info [2026-10-13 15:27:08.074 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state            out
#> 1:   load      n          done      1,2,3,4,5
#> 2: double      x    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-13 15:27:08

# Replace "load" — downstream steps are automatically marked "outdated"
pip_replace(p, "load", \(n = 3) seq_len(n))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends    state            out
#> 1:   load      n              new         [NULL]
#> 2: double      x    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-10-13 15:27:08

# Re-run to bring everything up to date
pip_run(p)
#> info [2026-10-13 15:27:08.080 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-13 15:27:08.080 UTC]: Step 1/2 load
#> info [2026-10-13 15:27:08.080 UTC]: Step 2/2 double
#> info [2026-10-13 15:27:08.081 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state   out
#> 1:   load      n          done 1,2,3
#> 2: double      x    load  done 2,4,6
#> -------------------------
#> <ready> last run: 2026-10-13 15:27:08

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
