# Check whether a step exists

Check whether a step exists

## Usage

``` r
pip_has_step(x, step)
```

## Arguments

- x:

  A pipeflow pip

- step:

  A step name

## Value

Logical indicating if the step exists

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)

pip_has_step(p, "load") # TRUE
#> [1] TRUE
pip_has_step(p, "fit") # TRUE
#> [1] TRUE
pip_has_step(p, "predict") # FALSE — step not yet added
#> [1] FALSE
```
