# Bind pipelines

Binds two or more pipelines together by concatenating their steps. If
the pipelines have steps with the same name, the step names of later
pipelines are automatically adapted to avoid name clashes. A single
pipeline is returned unchanged.

## Usage

``` r
# S3 method for class 'pipeflow'
rbind(..., deparse.level = 1, bind = list())
```

## Arguments

- ...:

  Two or more pipeflow pipeline objects.

- deparse.level:

  Not used, for compatibility with the generic
  [`rbind()`](https://rdrr.io/r/base/cbind.html).

- bind:

  A named list of one-sided formulas, e.g. `list(data = ~prep)`, that
  binds the unbound params of the second pipeline to steps of the first
  pipeline. If several steps of the second pipeline have a param of that
  name, all of them are bound. The steps are referred to by name
  (`~try(step)` is possible as well, see
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)),
  not by relative position. Only possible if exactly two pipelines are
  bound; to bind more, chain the calls, e.g.
  `rbind(rbind(a, b, bind = ...), c, bind = ...)`.

## Value

A new pipeflow pipeline object representing the bound pipelines.

## Details

To feed steps of the second pipeline with the output of steps of the
first one, use the `bind` argument. It binds unbound params of the
second pipeline, which then become references to steps of the first one,
as if they had been written as `param = ~step` in the step functions.
The bound params are no longer params of the result (see
[`pip_get_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)),
and bound steps that have been run before are marked `"outdated"`,
together with their downstream steps.

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
ad <- rbind(a, d, bind = list(data = ~prep))
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
#> info [2026-10-20 18:51:09.417 UTC]: Starting run of pipeflow 'a-d'
#> info [2026-10-20 18:51:09.417 UTC]: Step 1/3 prep
#> info [2026-10-20 18:51:09.418 UTC]: Step 2/3 fit
#> info [2026-10-20 18:51:09.419 UTC]: Step 3/3 scale
#> info [2026-10-20 18:51:09.420 UTC]: Finished run of pipeflow 'a-d'
ad[["scale", "out"]]
#> [1] 6
```
