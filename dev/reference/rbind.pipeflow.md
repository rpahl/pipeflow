# Bind pipelines

Binds two or more pipelines together by concatenating their steps. If
the pipelines have steps with the same name, the step names of later
pipelines are automatically adapted to avoid name clashes. A single
pipeline is returned unchanged.

## Usage

``` r
# S3 method for class 'pipeflow'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  Two or more pipeflow pipeline objects.

- deparse.level:

  Not used, for compatibility with the generic
  [`rbind()`](https://rdrr.io/r/base/cbind.html).

## Value

A new pipeflow pipeline object representing the bound pipelines.

## Examples

``` r
a <- pip_new("a") |>
  pip_add("prep", \(x = 1) x * 2) |>
  pip_add("fit", \(x = ~prep) x + 10)

# "prep" exists in both pipelines; the one from b gets a numeric suffix
b <- pip_new("b") |> pip_add("prep", \(x = 5) x * 3)

ab <- rbind(a, b)
ab[["step"]] # "prep", "fit", "prep2" (step name conflict auto-resolved)
#>    prep     fit   prep2 
#>  "prep"   "fit" "prep2" 
ab
#> <pipeflow> a-b (3 steps)
#> ------------------------
#>     step params depends state
#> 1:  prep      x           new
#> 2:   fit      x    prep   new
#> 3: prep2      x           new
#> ------------------------
#> <ready> last run: never

# Any number of pipelines can be combined
abc <- rbind(a, b, b)
abc
#> <pipeflow> a-b-b (4 steps)
#> --------------------------
#>     step params depends state
#> 1:  prep      x           new
#> 2:   fit      x    prep   new
#> 3: prep2      x           new
#> 4: prep3      x           new
#> --------------------------
#> <ready> last run: never
```
