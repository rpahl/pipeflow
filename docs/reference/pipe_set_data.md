# Set data

Set data in first step of pipeline.

## Usage

``` r
pipe_set_data(pip, data)
```

## Arguments

- pip:

  `Pipeline` object

- data:

  initial data set.

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-13 19:22:22.085] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:22.087] Step 1/2 data
#> INFO  [2026-06-13 19:22:22.089] Step 2/2 add1
#> INFO  [2026-06-13 19:22:22.090] Finished execution of steps.
#> INFO  [2026-06-13 19:22:22.091] Done.
#> $add1
#> [1] 2
#> 

pipe_set_data(p, 3)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-13 19:22:22.095] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:22.096] Step 1/2 data
#> INFO  [2026-06-13 19:22:22.099] Step 2/2 add1
#> INFO  [2026-06-13 19:22:22.101] Finished execution of steps.
#> INFO  [2026-06-13 19:22:22.101] Done.
#> $add1
#> [1] 4
#> 
```
