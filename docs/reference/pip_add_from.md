# Copy a step from another pipeline

Copies one step from pipeline `y` into pipeline `x`, preserving its
function, parameters, group, tags, and dependency links.

## Usage

``` r
pip_add_from(x, y, step)
```

## Arguments

- x:

  Target pipeflow pipeline object.

- y:

  Source pipeflow pipeline object.

- step:

  Step name to copy from `y`.

## Value

The updated target pipeline, invisibly.

## Examples

``` r
# Build a source pipeline with reusable steps
src <- pip_new("source") |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2)

# Copy steps into a new pipeline one at a time.
# The dependency of "square" on "load" is re-established automatically.
dst <- pip_new("target")
pip_add_from(dst, src, "load")
pip_add_from(dst, src, "square")
pip_run(dst)
#> info [2026-06-07 15:34:06.508 UTC]: Start run of pipeflow_pip 'target'
#> info [2026-06-07 15:34:06.508 UTC]: Step 1/2 load
#> info [2026-06-07 15:34:06.509 UTC]: Step 2/2 square
#> info [2026-06-07 15:34:06.510 UTC]: Finished run of pipeflow_pip 'target'
pip_collect_out(dst)
#> $load
#> [1] 1 2 3
#> 
#> $square
#> [1] 1 4 9
#> 
```
