# Turn a pipeline into a function

Returns a function that runs a copy of the pipeline `x` and returns the
output of selected steps. The function arguments are the unbound
parameters of `x`, with their current values as defaults. As any other
function, it can be added to an "outer" pipeline with
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
in which case the unbound parameters of `x` become parameters of that
pipeline. To bind parameters of `x` to output of steps of the outer
pipeline, pass them as references in `params` of
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
e.g.
`pip_add(outer, "inner", f_inner, params = list(df = ~outer_step))`.

## Usage

``` r
# S3 method for class 'pipeflow'
as.function(
  x,
  out = NULL,
  cache = TRUE,
  on_error = c("stop", "continue"),
  lgr = NULL,
  ...
)
```

## Arguments

- x:

  A pipeflow pipeline (not a view; use `x[i, view = FALSE]` to turn a
  part of a pipeline into a function).

- out:

  The result of the function. `NULL` (the default) returns the outputs
  of all steps as a named list, see
  [`pip_collect()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md);
  a step name returns the output of that step; several step names return
  a named list of their outputs. A function with at least one argument
  is called with (a light copy of) the run inner pipeline and returns
  the result, see Details.

- cache:

  Whether to keep the run copies of `x` for reuse, `TRUE` (the default)
  or `FALSE`. The function keeps one copy per outer step and partition,
  see Details.

- on_error:

  What to do when an inner step fails: `"stop"` (the default) ends the
  inner run at that step, `"continue"` runs the inner steps that do not
  depend on it, see Details and
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md).

- lgr:

  The logger of the inner run, see
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md).
  `NULL` (the default) runs the inner pipeline silently. To activate
  logging, set e.g. `lgr = pipeflow_lgr`.

- ...:

  Not used.

## Value

A function with the unbound params of `x` as arguments.

## Details

By default, the inner pipeline is run silently, but warnings and
messages of its steps are still passed on to the caller. A failing inner
step results in an error of the caller which in turn makes the outer
step fail. Param values of the outer pipeline reach the function
unwrapped (see
[`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)).
The arguments of the function are values, also if they are formulas:
unlike in
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md),
a formula argument does not bind a param of `x` to a step.

### Choosing the output

With step names in `out`, the function returns their outputs. If `out`
is a function, it is called with the run inner pipeline, and its result
is the result of the function, e.g.
`out = \(p) pip_collect(p, by = "tags")` groups the outputs by tags. The
pipeline that `out` gets is a light copy of the kept one: it has its own
steps, but the values (params and outputs) are the same objects.
Returning it, or a view of it, therefore keeps no extra copy of the
data, and later calls of the function don't change it. Since the values
are shared, don't modify them in place (e.g. with `:=` on a `data.table`
output).

### Continuing after errors of inner steps

With `on_error = "continue"` the inner pipeline is run like
`pip_run(on_error = "continue")`, that is, a failing inner step gets
state `"failed"`, the inner steps that do not depend on it still run,
and an inner step that refers to it via `~try(step)` runs and receives
the failure.

The function still fails if one of the steps named in `out` failed or
was not run because an input failed. Since with `out = NULL` or a
function `out` *all* steps are requested, you need to name the relevant
steps in `out` to profit from a continuing inner run.

### Pipeline functions with partitioned inputs

If the step with the pipeline function receives partitioned inputs (from
an earlier step with `exec = "split"`), the function is called once per
partition caching one inner pipeline of `x` per step and partition key.
A call updates its parameters and inputs in the respective pipeline, so
that (as usual in pipelines) only outdated inner steps are run again.
This works for inputs bound via `params` of
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
as well as for those bound in the step function.

- A partition caches one pipeline for each key, which hold their inputs
  and outputs, so they need memory.

- A step without partitioned inputs keeps a single pipeline.

- Pure calls of the pipeline function (outside of a step) always share
  one pipeline, e.g. `lapply(subsets, f_pip)` don't cache anything.

- Do *not* use the same pipeline function in two different pipelines -
  this avoids name clash of keys, which would prevent proper caching
  (not wrong but slower).

- Cached pipelines are stored in the function. If you save the function
  or send it to another process, they are copied, including their data
  and output, so new runs work independently from the original. A
  function that has not been called yet, or one made with
  `cache = FALSE`, has no pipelines and therefore is small to copy.

- In case of an error, the error message includes the key of the
  partition and the name of the failed inner step.

### Limits

- Params that several steps of `x` share by name become one argument,
  with the default of the first step as in
  [`pip_get_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md).
  Every call sets this value in all of these steps, so a step whose own
  default differs from it runs with the value of the argument.

- Inputs are compared as they are. A change made in place to an
  environment or another reference object (e.g. R6) is not seen: as for
  [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  of a pipeline, which needs `force = TRUE`, the steps stay up to date.
  A forced run of the outer pipeline (`pip_run(force = TRUE)`) does not
  force the inner pipelines either, but you can force them by setting
  `cache = FALSE`.

## Note

The conversion is done by taking a snapshot of `x`. Later changes of `x`
therefore don't have an impact on the function. Also all steps start as
new, so the pipeline function result does not depend on whether `x` has
been run before, except for locked steps, which keep their outputs.

## See also

[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md),
[`pip_inner()`](https://rpahl.github.io/pipeflow/dev/reference/pip_inner.md)
to look at the kept inner pipelines

## Examples

``` r
inner <- pip_new("inner") |>
  pip_add("input", \(input_df = NULL) input_df) |>
  pip_add("fit", \(data = ~input, degree = 1) {
    lm(mpg ~ poly(wt, degree, raw = TRUE), data = data)
  }) |>
  pip_add("coefs", \(fit = ~fit, digits = 2) round(coef(fit), digits))

# As a plain function: the unbound params are its arguments
f_inner <- as.function(inner, out = "coefs")
f_inner(input_df = mtcars, digits = 1)
#>                  (Intercept) poly(wt, degree, raw = TRUE) 
#>                         37.3                         -5.3 

# As a step: one inner pipeline
single <- pip_new("single") |>
  pip_add("df", \(df = mtcars) df) |>
  pip_add("coefs", f_inner, params = list(input_df = ~df))

pip_run(single, lgr = NULL)
single[["coefs", "out"]]
#>                  (Intercept) poly(wt, degree, raw = TRUE) 
#>                        37.29                        -5.34 

# Only the inner steps after the changed param run again
pip_set_params(single, list(digits = 3))
pip_run(single, lgr = NULL)
single[["coefs", "out"]]
#>                  (Intercept) poly(wt, degree, raw = TRUE) 
#>                       37.285                       -5.344 

# With partioned inputs: one inner pipeline per cylinder number
outer <- pip_new("outer") |>
  pip_add("df", \(df = mtcars) df) |>
  pip_add("by_cyl", \(df = ~df) split(df, df$cyl), exec = "split") |>
  pip_add(
    "coefs",
    as.function(inner, out = "coefs"),
    params = list(input_df = ~by_cyl)
  ) |>
  pip_add("table", \(coefs = ~coefs) do.call(rbind, coefs), exec = "reduce")

pip_get_params(outer)
#> $df
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> $degree
#> [1] 1
#> 
#> $digits
#> [1] 2
#> 
pip_run(outer, lgr = NULL)
outer[["table", "out"]]
#>   (Intercept) poly(wt, degree, raw = TRUE)
#> 4       39.57                        -5.65
#> 6       28.41                        -2.78
#> 8       23.87                        -2.19

# Only the steps after the changed param run again, for each partition
pip_set_params(outer, list(digits = 3))
print(outer)
#> <pipeflow> outer (4 steps)
#> --------------------------
#>      step                 params depends    state                 out   exec
#> 1:     df                     df             done <data.frame[32x11]>   auto
#> 2: by_cyl                     df      df     done           <list[3]>  split
#> 3:  coefs input_df,degree,digits  by_cyl outdated           <list[3]>   auto
#> 4:  table                  coefs   coefs outdated       <matrix[3x2]> reduce
#> --------------------------
#> <ready> last run: 2026-10-25 08:12:08
pip_run(outer, lgr = NULL)
outer[["table", "out"]]
#>   (Intercept) poly(wt, degree, raw = TRUE)
#> 4      39.571                       -5.647
#> 6      28.409                       -2.780
#> 8      23.868                       -2.192

# Don't use the same function for the same step name in two pipelines:
# this breaks the caching (results are still correct, but it is slower)
a <- pip_new("a") |>
  pip_add("df", \(df = mtcars) df) |>
  pip_add("coefs", f_inner, params = list(input_df = ~df))
b <- pip_new("b") |>
  pip_add("df", \(df = mtcars[mtcars$am == 1, ]) df) |>
  pip_add("coefs", f_inner, params = list(input_df = ~df))

pip_run(a, lgr = NULL)
pip_run(b, lgr = NULL)

# The inner pipeline now holds the input of `b`, so all its steps run again
# for `a`, not only the ones after `digits`
pip_set_params(a, list(digits = 3))
pip_run(a, lgr = NULL)
a[["coefs", "out"]]
#>                  (Intercept) poly(wt, degree, raw = TRUE) 
#>                       37.285                       -5.344 

# Better: make one function per pipeline
pip_replace(
  a, "coefs", as.function(inner, out = "coefs"),
  params = list(input_df = ~df)
)

# A function as out: the outputs grouped by tags
tagged <- pip_new("tagged") |>
  pip_add("input", \(input_df = NULL) input_df) |>
  pip_add("n", \(data = ~input) nrow(data), tags = "summary") |>
  pip_add("mpg", \(data = ~input) mean(data$mpg), tags = "summary") |>
  pip_add("fit", \(data = ~input) lm(mpg ~ wt, data = data), tags = "model")

f_tagged <- as.function(tagged, out = \(p) pip_collect(p, by = "tags"))
res <- f_tagged(input_df = mtcars)
names(res)
#> [1] "summary" "model"  
res$summary
#> $n
#> [1] 32
#> 
#> $mpg
#> [1] 20.09062
#> 

# With on_error = "continue", the inner pipeline handles a failed inner
# step itself: `coefs` is run with the failure of `fit` and reports it.
robust <- pip_new("robust") |>
  pip_add("input", \(input_df = NULL) input_df) |>
  pip_add("fit", \(data = ~input) {
    if (nrow(data) < 3) stop("too few rows to fit")
    lm(mpg ~ wt, data = data)
  }) |>
  pip_add("coefs", \(fit = ~try(fit), digits = 2) {
    if (inherits(fit, "pipeflow_failure")) NULL else round(coef(fit), digits)
  })

f_robust <- as.function(robust, out = "coefs", on_error = "continue")
f_robust(input_df = mtcars)
#> (Intercept)          wt 
#>       37.29       -5.34 

# the continued inner run signals a warning instead of an error
suppressWarnings(f_robust(input_df = mtcars[1:2, ]))
#> NULL
```
