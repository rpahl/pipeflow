# Get output of given step

Get output of given step

## Usage

``` r
pipe_get_out(pip, step)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

## Value

the output at the given step.

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## See also

[`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)
to collect output of multiple steps.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_run(p)
#> INFO  [2026-06-20 21:18:55.016] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:18:55.017] Step 1/3 data
#> INFO  [2026-06-20 21:18:55.024] Step 2/3 add1
#> INFO  [2026-06-20 21:18:55.028] Step 3/3 add2
#> INFO  [2026-06-20 21:18:55.029] Finished execution of steps.
#> INFO  [2026-06-20 21:18:55.030] Done.
pipe_get_out(p, "add1")
#> [1] 2 3
pipe_get_out(p, "add2")
#> [1] 3 5
```
