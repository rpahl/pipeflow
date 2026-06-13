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
#> INFO  [2026-06-13 17:08:29.321] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:29.322] Step 1/3 data (upstream)
#> INFO  [2026-06-13 17:08:29.324] Step 2/3 add1 (upstream)
#> INFO  [2026-06-13 17:08:29.327] Step 3/3 add2
#> INFO  [2026-06-13 17:08:29.329] Finished execution of steps.
#> INFO  [2026-06-13 17:08:29.330] Done.

pipe_run_step(p, "add2", downstream = TRUE)
#> INFO  [2026-06-13 17:08:29.332] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:29.333] Step 1/4 data (upstream)
#> INFO  [2026-06-13 17:08:29.336] Step 2/4 add1 (upstream)
#> INFO  [2026-06-13 17:08:29.339] Step 3/4 add2
#> INFO  [2026-06-13 17:08:29.340] Step 4/4 mult (downstream)
#> INFO  [2026-06-13 17:08:29.343] Finished execution of steps.
#> INFO  [2026-06-13 17:08:29.344] Done.

pipe_run_step(p, "mult", upstream = TRUE)
#> INFO  [2026-06-13 17:08:29.346] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:29.347] Step 1/4 data (upstream)
#> INFO  [2026-06-13 17:08:29.350] Step 2/4 add1 (upstream)
#> INFO  [2026-06-13 17:08:29.352] Step 3/4 add2 (upstream)
#> INFO  [2026-06-13 17:08:29.354] Step 4/4 mult
#> INFO  [2026-06-13 17:08:29.355] Finished execution of steps.
#> INFO  [2026-06-13 17:08:29.356] Done.
```
