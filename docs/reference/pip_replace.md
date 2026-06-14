# Replace a step

Replaces a step's function while keeping it in the same position in the
pipeline. Downstream steps are automatically marked as outdated and will
re-run on the next
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_replace(x, step, fun, tags = character(0))
```

## Arguments

- x:

  A pipeflow pipeline object.

- step:

  Step name.

- fun:

  Function to execute for the step.

- tags:

  Optional character vector of tags belonging to the step. Can also be
  adjusted later using `[pip_tag()]`.

## Value

The updated pipeline, invisibly.

## Examples

``` r
p <- pip_new() |>
    pip_add("load", \(n = 5) seq_len(n)) |>
    pip_add("double", \(x = ~load) x * 2)
pip_run(p)
#> info [2026-06-14 13:40:16.251 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-14 13:40:16.252 UTC]: Step 1/2 load
#> info [2026-06-14 13:40:16.252 UTC]: Step 2/2 double
#> info [2026-06-14 13:40:16.254 UTC]: Finished run of pipeflow_pip 'pipe'
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>      step depends            out state
#> 1:   load              1,2,3,4,5  done
#> 2: double    load  2, 4, 6, 8,10  done

# Replace "load" — downstream steps are automatically marked "outdated"
pip_replace(p, "load", \(n = 3) seq_len(n))
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>      step depends            out    state
#> 1:   load                 [NULL]      new
#> 2: double    load  2, 4, 6, 8,10 outdated

# Re-run to bring everything up to date
pip_run(p)
#> info [2026-06-14 13:40:16.266 UTC]: Start run of pipeflow_pip 'pipe'
#> info [2026-06-14 13:40:16.266 UTC]: Step 1/2 load
#> info [2026-06-14 13:40:16.267 UTC]: Step 2/2 double
#> info [2026-06-14 13:40:16.269 UTC]: Finished run of pipeflow_pip 'pipe'
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>      step depends   out state
#> 1:   load         1,2,3  done
#> 2: double    load 2,4,6  done
```
