# Run a pipeline

Executes all pending steps in order. Steps already in state `"done"` are
skipped unless `force = TRUE`.

## Usage

``` r
pip_run(
  x,
  lgr = pipeflow_lgr,
  force = FALSE,
  progress = NULL,
  recursive = FALSE
)
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

- recursive:

  If `TRUE` and a step returns a pipeline object, the current run is
  aborted and continues from the returned pipeline. Useful for dynamic
  or self-modifying pipelines.

## Value

The updated pipeline or view, invisibly.

## Details

When `x` is a view, requested rows are run together with required
upstream dependencies.

## See also

[`vignette("v06-self-modify-pipeline", package = "pipeflow")`](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
for an advanced example of recursive/dynamic pipelines.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2) |>
  pip_add("total", \(x = ~square) sum(x))

pip_run(p)
#> info [2026-06-13 15:08:36.937 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-13 15:08:36.937 UTC]: Step 1/3 load
#> info [2026-06-13 15:08:36.938 UTC]: Step 2/3 square
#> info [2026-06-13 15:08:36.942 UTC]: Step 3/3 total
#> info [2026-06-13 15:08:36.944 UTC]: Finished run of pipeflow_pip 'pipe'
p
#> <pipeflow_pip> pipe (3 steps)
#> -----------------------------
#>      step depends   out state
#> 1:   load         1,2,3  done
#> 2: square    load 1,4,9  done
#> 3:  total  square    14  done

# Already-done steps are skipped on a second run
pip_run(p) # all steps skipped
#> info [2026-06-13 15:08:36.949 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-13 15:08:36.949 UTC]: Step 1/3 load - skipping done step
#> info [2026-06-13 15:08:36.949 UTC]: Step 2/3 square - skipping done step
#> info [2026-06-13 15:08:36.950 UTC]: Step 3/3 total - skipping done step
#> info [2026-06-13 15:08:36.950 UTC]: Finished run of pipeflow_pip 'pipe'

# lgr = NULL suppresses log output
pip_run(p, lgr = NULL)

# force = TRUE re-executes every step regardless of state
pip_run(p, force = TRUE)
#> info [2026-06-13 15:08:36.952 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-13 15:08:36.953 UTC]: Step 1/3 load
#> info [2026-06-13 15:08:36.953 UTC]: Step 2/3 square
#> info [2026-06-13 15:08:36.954 UTC]: Step 3/3 total
#> info [2026-06-13 15:08:36.956 UTC]: Finished run of pipeflow_pip 'pipe'

# Run only a subset of steps via a view;
# upstream dependencies are automatically included
v <- pip_view(p, i = "total")
pip_run(v)
#> info [2026-06-13 15:08:36.958 UTC]: Start run of pipeflow_view 'pipe view'
#> info [2026-06-13 15:08:36.958 UTC]: Step 1/2 [upstream] square - skipping done step
#> info [2026-06-13 15:08:36.959 UTC]: Step 2/2 [view] total - skipping done step
#> info [2026-06-13 15:08:36.959 UTC]: Finished run of pipeflow_view 'pipe view'
```
