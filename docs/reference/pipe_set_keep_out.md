# Change output flag

Change the `keepOut` flag at a given pipeline step, which determines
whether the output of that step is collected when calling
[`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)`after the pipeline was run. See column`keepOut\`
when printing a pipeline to view the status.

## Usage

``` r
pipe_set_keep_out(pip, step, keepOut = TRUE)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

- keepOut:

  `logical` whether to keep output of step

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y, keepOut = TRUE)
pipe_add(p, "add2", \(x = ~data, y = 2) x + y)
pipe_add(p, "mult", \(x = ~add1, y = ~add2) x * y)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-13 19:22:23.323] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:23.324] Step 1/4 data
#> INFO  [2026-06-13 19:22:23.327] Step 2/4 add1
#> INFO  [2026-06-13 19:22:23.329] Step 3/4 add2
#> INFO  [2026-06-13 19:22:23.332] Step 4/4 mult
#> INFO  [2026-06-13 19:22:23.333] Finished execution of steps.
#> INFO  [2026-06-13 19:22:23.334] Done.
#> $add1
#> [1] 2
#> 

pipe_set_keep_out(p, "add1", keepOut = FALSE)
pipe_set_keep_out(p, "mult", keepOut = TRUE)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-13 19:22:23.338] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:23.339] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-13 19:22:23.340] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-13 19:22:23.341] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-13 19:22:23.344] Step 4/4 mult - skip 'done' step
#> INFO  [2026-06-13 19:22:23.345] Finished execution of steps.
#> INFO  [2026-06-13 19:22:23.345] Done.
#> $mult
#> [1] 6
#> 
```
