# Unlock selected steps

Unlocks all selected steps in the pipeline unless `p` is a view, in
which case only steps covered by the view are unlocked.

## Usage

``` r
pip_unlock(p)
```

## Arguments

- p:

  A pipeflow pip or view.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x * 2)

# Lock all steps, then unlock to restore normal execution
pip_lock(p)
p[["pipeline"]][["locked"]] # TRUE, TRUE
#> [1] TRUE TRUE

pip_unlock(p)
p[["pipeline"]][["locked"]] # FALSE, FALSE
#> [1] FALSE FALSE
```
