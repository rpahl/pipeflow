# Replace a step

Replaces a step's function while keeping it in the same position in the
pipeline. Downstream steps are automatically marked as outdated and will
re-run on the next
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_replace(x, step, fun, tags = character(0), params = list(), exec = "auto")
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
  output values at runtime once the step is executed.

- tags:

  Optional character vector of tags belonging to the step. Can also be
  adjusted later using `[pip_tag()]`.

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

  Execution mode for this step. One of "auto", "split", "reduce" or
  "plain". Using execution mode `exec = split`, the output of the step
  is marked as partitioned output. In this mode, any step that depends
  on the split step (directly or indirectly) will have its output
  automatically mapped partition-wise during step execution. The
  `reduce` mode expects partitioned input and passes it through without
  mapping, while `plain` mode only accepts non-partitioned input and
  always intends to execute a single call. In summary:

  - auto: map if partitioned input appears, otherwise single call

  - split: single call, then mark output as partitioned

  - reduce: single call, but only valid with partitioned input

  - plain: single call, only valid with non-partitioned input

## Value

The updated pipeline, invisibly.

## Examples

``` r
p <- pip_new() |>
    pip_add("load", \(n = 5) seq_len(n)) |>
    pip_add("double", \(x = ~load) x * 2)
pip_run(p)
#> info [2026-09-27 15:19:38.986 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 15:19:38.986 UTC]: Step 1/2 load
#> info [2026-09-27 15:19:38.987 UTC]: Step 2/2 double
#> info [2026-09-27 15:19:38.988 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state            out
#> 1:   load      n          done      1,2,3,4,5
#> 2: double      x    load  done  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-09-27 17:19:38

# Replace "load" — downstream steps are automatically marked "outdated"
pip_replace(p, "load", \(n = 3) seq_len(n))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends    state            out
#> 1:   load      n              new         [NULL]
#> 2: double      x    load outdated  2, 4, 6, 8,10
#> -------------------------
#> <ready> last run: 2026-09-27 17:19:38

# Re-run to bring everything up to date
pip_run(p)
#> info [2026-09-27 15:19:38.996 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 15:19:38.997 UTC]: Step 1/2 load
#> info [2026-09-27 15:19:38.997 UTC]: Step 2/2 double
#> info [2026-09-27 15:19:38.998 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state   out
#> 1:   load      n          done 1,2,3
#> 2: double      x    load  done 2,4,6
#> -------------------------
#> <ready> last run: 2026-09-27 17:19:38

# If a view is passed, the step must be part of the view
v <- pip_view(p, step = "double")
pip_replace(v, "double", \(x = ~load) x * 3)
p[["state"]]                                # "double" is "new" again
#>   load double 
#> "done"  "new" 

try(pip_replace(v, "load", \(n = 2) seq_len(n)))
#> Error in pip_replace(v, "load", function(n = 2) seq_len(n)) : 
#>   step 'load' is not part of the view
```
