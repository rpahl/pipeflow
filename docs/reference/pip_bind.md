# Bind pipelines

Bind two pipelines together by concatenating their steps. If both
pipelines have steps with the same name, the step names of the second
pipeline will be automatically adapted to avoid name clashes.

## Usage

``` r
pip_bind(x, y)
```

## Arguments

- x:

  A pipeflow pipeline object.

- y:

  A pipeflow pipeline object.

## Value

A new pipeflow pipeline object representing the bound pipelines.

## Examples

``` r
a <- pip_new("a") |>
  pip_add("prep", \(x = 1) x * 2) |>
  pip_add("fit", \(x = ~prep) x + 10)

# "prep" exists in both pipelines; the one from b gets a numeric suffix
b <- pip_new("b") |> pip_add("prep", \(x = 5) x * 3)

ab <- pip_bind(a, b)
ab[["step"]] # "prep", "fit", "prep2" (step name conflict auto-resolved)
#> [1] "prep"  "fit"   "prep2"
ab
#> <pipeflow_pip> a-b (3 steps)
#> ----------------------------
#>     step group depends    out state
#> 1:  prep  prep         [NULL]   new
#> 2:   fit   fit    prep [NULL]   new
#> 3: prep2  prep         [NULL]   new
```
