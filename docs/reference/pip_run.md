# Run a pipeline

Executes all pending steps in order. Steps already in state `"done"` are
skipped unless `force = TRUE`.

## Usage

``` r
pip_run(x, lgr = pipeflow_lgr, force = FALSE, progress = NULL)
```

## Arguments

- x:

  A pipeflow pip or view

- lgr:

  A logging function of the form `function(level, msg, ...)`. To
  suppress logging, you can set `lgr = NULL`.

- force:

  Logical indicating if all steps should be forced to run, regardless of
  whether they are outdated or not.

- progress:

  Optional callback of the form `function(value, detail)` called before
  each step.

## Value

The updated pipeline or view, invisibly.

## Details

When `x` is a view, requested rows are run together with required
upstream dependencies. If a step fails, the pipeline run state is set to
`"failed"` and the error is re-thrown.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
for an advanced example of dynamic pipelines.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2) |>
  pip_add("total", \(x = ~square) sum(x))

pip_run(p)
#> info [2026-08-22 17:08:03.134 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-08-22 17:08:03.134 UTC]: Step 1/3 load
#> info [2026-08-22 17:08:03.135 UTC]: Step 2/3 square
#> info [2026-08-22 17:08:03.136 UTC]: Step 3/3 total
#> info [2026-08-22 17:08:03.136 UTC]: Finished run of pipeflow 'pipe'
p
#> <pipeflow> pipe (3 steps)
#> -----------------------------
#>      step depends   out state
#> 1:   load         1,2,3  done
#> 2: square    load 1,4,9  done
#> 3:  total  square    14  done

# Already-done steps are skipped on a second run
pip_run(p) # all steps skipped
#> info [2026-08-22 17:08:03.139 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-08-22 17:08:03.139 UTC]: Step 1/3 load - skipping done step
#> info [2026-08-22 17:08:03.139 UTC]: Step 2/3 square - skipping done step
#> info [2026-08-22 17:08:03.139 UTC]: Step 3/3 total - skipping done step
#> info [2026-08-22 17:08:03.139 UTC]: Finished run of pipeflow 'pipe'

# lgr = NULL suppresses log output
pip_run(p, lgr = NULL)

# force = TRUE re-executes every step regardless of state
pip_run(p, force = TRUE)
#> info [2026-08-22 17:08:03.140 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-08-22 17:08:03.141 UTC]: Step 1/3 load
#> info [2026-08-22 17:08:03.141 UTC]: Step 2/3 square
#> info [2026-08-22 17:08:03.142 UTC]: Step 3/3 total
#> info [2026-08-22 17:08:03.143 UTC]: Finished run of pipeflow 'pipe'

# Run only a subset of steps via a view;
# upstream dependencies are automatically included
v <- pip_view(p, step = "total")
pip_run(v)
#> info [2026-08-22 17:08:03.145 UTC]: Starting run of pipeflow_view 'pipe view'
#> info [2026-08-22 17:08:03.145 UTC]: Step 1/3 [upstream] load - skipping done step
#> info [2026-08-22 17:08:03.145 UTC]: Step 2/3 [upstream] square - skipping done step
#> info [2026-08-22 17:08:03.145 UTC]: Step 3/3 [view] total - skipping done step
#> info [2026-08-22 17:08:03.145 UTC]: Finished run of pipeflow_view 'pipe view'
```
