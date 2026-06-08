# Run specific step

Run given pipeline step possibly together with upstream and/or
downstream dependencies.

## Usage

``` r
pipe_run_step(
  pip,
  step,
  upstream = TRUE,
  downstream = FALSE,
  cleanUnkept = FALSE
)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

- upstream:

  `logical` if `TRUE`, run all dependent upstream steps first.

- downstream:

  `logical` if `TRUE`, run all depdendent downstream afterwards.

- cleanUnkept:

  `logical` if `TRUE` all output that was not marked to be kept is
  removed after the pipeline run. This option can be useful if temporary
  results require a lot of memory.

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
pipe_add(p, "add2", \(x = ~add1, z = 2) x + z)
pipe_add(p, "mult", \(x = ~add1, y = ~add2) x * y)
pipe_run_step(p, "add2")
#> INFO  [2026-06-07 17:34:03.883] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:03.884] Step 1/3 data (upstream)
#> INFO  [2026-06-07 17:34:03.887] Step 2/3 add1 (upstream)
#> INFO  [2026-06-07 17:34:03.890] Step 3/3 add2
#> INFO  [2026-06-07 17:34:03.892] Finished execution of steps.
#> INFO  [2026-06-07 17:34:03.893] Done.

pipe_run_step(p, "add2", downstream = TRUE)
#> INFO  [2026-06-07 17:34:03.895] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:03.896] Step 1/4 data (upstream)
#> INFO  [2026-06-07 17:34:03.898] Step 2/4 add1 (upstream)
#> INFO  [2026-06-07 17:34:03.901] Step 3/4 add2
#> INFO  [2026-06-07 17:34:03.904] Step 4/4 mult (downstream)
#> INFO  [2026-06-07 17:34:03.906] Finished execution of steps.
#> INFO  [2026-06-07 17:34:03.907] Done.

pipe_run_step(p, "mult", upstream = TRUE)
#> INFO  [2026-06-07 17:34:03.909] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:03.910] Step 1/4 data (upstream)
#> INFO  [2026-06-07 17:34:03.913] Step 2/4 add1 (upstream)
#> INFO  [2026-06-07 17:34:03.915] Step 3/4 add2 (upstream)
#> INFO  [2026-06-07 17:34:03.918] Step 4/4 mult
#> INFO  [2026-06-07 17:34:03.919] Finished execution of steps.
#> INFO  [2026-06-07 17:34:03.920] Done.
```
