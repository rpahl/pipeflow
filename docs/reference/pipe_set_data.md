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
#> INFO  [2026-06-20 21:19:01.399] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:19:01.400] Step 1/2 data
#> INFO  [2026-06-20 21:19:01.403] Step 2/2 add1
#> INFO  [2026-06-20 21:19:01.404] Finished execution of steps.
#> INFO  [2026-06-20 21:19:01.405] Done.
#> $add1
#> [1] 2
#> 

pipe_set_data(p, 3)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-20 21:19:01.410] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:19:01.411] Step 1/2 data
#> INFO  [2026-06-20 21:19:01.414] Step 2/2 add1
#> INFO  [2026-06-20 21:19:01.416] Finished execution of steps.
#> INFO  [2026-06-20 21:19:01.417] Done.
#> $add1
#> [1] 4
#> 
```
