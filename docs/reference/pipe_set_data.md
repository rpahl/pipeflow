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
#> INFO  [2026-06-07 17:34:04.175] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:04.177] Step 1/2 data
#> INFO  [2026-06-07 17:34:04.179] Step 2/2 add1
#> INFO  [2026-06-07 17:34:04.180] Finished execution of steps.
#> INFO  [2026-06-07 17:34:04.181] Done.
#> $add1
#> [1] 2
#> 

pipe_set_data(p, 3)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-07 17:34:04.185] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:04.186] Step 1/2 data
#> INFO  [2026-06-07 17:34:04.189] Step 2/2 add1
#> INFO  [2026-06-07 17:34:04.190] Finished execution of steps.
#> INFO  [2026-06-07 17:34:04.191] Done.
#> $add1
#> [1] 4
#> 
```
