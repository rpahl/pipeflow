# Clone a pipeline

Creates an independent copy of the pipeline. Changes to the cloned
pipeline do not affect the original pipeline, and vice versa.

## Usage

``` r
pip_clone(x, name = NULL)
```

## Arguments

- x:

  A pipeflow pipeline object.

- name:

  Optional name for the cloned pipeline. If `NULL`, the original name is
  used.

## Value

A cloned pipeflow pipeline object.

## Examples

``` r
p <- pip_new("original") |>
  pip_add("s1", \(x = 1) x) |>
  pip_add("s2", \(x = ~s1) x + 1)

# Clone produces a fully independent copy
cp <- pip_clone(p, name = "copy")
pip_add(cp, "s3", \(x = ~s2) x * 10)

# As a result, the clone has the new step ...
cp
#> <pipeflow_pip> copy (3 steps)
#> -----------------------------
#>    step depends    out state
#> 1:   s1         [NULL]   new
#> 2:   s2      s1 [NULL]   new
#> 3:   s3      s2 [NULL]   new

# ... while the original is left unchanged
p
#> <pipeflow_pip> original (2 steps)
#> ---------------------------------
#>    step depends    out state
#> 1:   s1         [NULL]   new
#> 2:   s2      s1 [NULL]   new
```
