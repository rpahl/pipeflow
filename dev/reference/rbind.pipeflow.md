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

## Details

To feed steps of the second pipeline with the output of steps of the
first one, bind their params with
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
afterwards, e.g. `pip_set_params(ab, list(data = ~prep))`. This binds
the param in all steps that have it unbound, so to bind only the steps
of the second pipeline, select them with a view, e.g.
`ab[-seq_along(a)]`. Locked steps are not bound: unlock them before and
lock them again afterwards, e.g. with
[`pip_unlock()`](https://rpahl.github.io/pipeflow/dev/reference/pip_lock.md)
and
[`pip_lock()`](https://rpahl.github.io/pipeflow/dev/reference/pip_lock.md)
on the same view.

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

# Feed the param `data` of the second pipeline with the output of "prep"
d <- pip_new("d") |> pip_add("scale", \(data = 1, k = 3) data * k)
ad <- rbind(a, d)
pip_set_params(ad[-seq_along(a)], list(data = ~prep))
ad[["scale", "depends"]]
#>   data 
#> "prep" 
pip_get_params(ad) # `data` is no longer a param
#> $x
#> [1] 1
#> 
#> $k
#> [1] 3
#> 
pip_run(ad)
#> info [2026-10-25 08:12:15.617 UTC]: Starting run of pipeflow 'a-d'
#> info [2026-10-25 08:12:15.617 UTC]: Step 1/3 prep
#> info [2026-10-25 08:12:15.618 UTC]: Step 2/3 fit
#> info [2026-10-25 08:12:15.619 UTC]: Step 3/3 scale
#> info [2026-10-25 08:12:15.620 UTC]: Finished in 0.003 s (3 done, 0 skipped, 0 failed)
ad[["scale", "out"]]
#> [1] 6
```
