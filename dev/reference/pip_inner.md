# Inner pipelines of a pipeline function step

Returns the inner pipelines that the function of `step`, made by
[`as.function.pipeflow()`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md),
has kept, e.g. to debug an inner run or to look at the steps behind the
returned output.

## Usage

``` r
pip_inner(x, step)
```

## Arguments

- x:

  A pipeflow pipeline object, the "outer" pipeline.

- step:

  Name of a step of `x` whose function was made by
  [`as.function.pipeflow()`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md).

## Value

For a step that is mapped over partitions, a list of pipelines named by
partition key, otherwise the pipeline of the last call of the step. The
pipelines are copies that share the values of the kept ones, so don't
modify the values in place. An empty list is returned if the function
has kept nothing (yet), e.g. before the first run or with
`cache = FALSE`.

## See also

[`as.function.pipeflow()`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)

## Examples

``` r
inner <- pip_new("inner") |>
  pip_add("data", \(input_df = NULL) input_df) |>
  pip_add("fit", \(data = ~data) lm(mpg ~ wt, data = data))

outer <- pip_new("outer") |>
  pip_add("df", \(df = mtcars) df) |>
  pip_add("by_cyl", \(df = ~df) split(df, df$cyl), exec = "split") |>
  pip_add(
    "fits",
    as.function(inner, out = "fit"),
    params = list(input_df = ~by_cyl)
  )
pip_run(outer, lgr = NULL)

fits <- pip_inner(outer, "fits")
names(fits)
#> [1] "4" "6" "8"
fits[["6"]]
#> <pipeflow> inner (2 steps)
#> --------------------------
#>    step   params depends state                out
#> 1: data input_df          done <data.frame[7x11]>
#> 2:  fit     data    data  done           <lm[12]>
#> --------------------------
#> <ready> last run: 2026-10-25 08:12:10
```
