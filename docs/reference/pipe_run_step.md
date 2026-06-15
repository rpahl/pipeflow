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
#> INFO  [2026-06-15 12:50:38.311] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:38.313] Step 1/3 data (upstream)
#> INFO  [2026-06-15 12:50:38.319] Step 2/3 add1 (upstream)
#> INFO  [2026-06-15 12:50:38.323] Step 3/3 add2
#> INFO  [2026-06-15 12:50:38.325] Finished execution of steps.
#> INFO  [2026-06-15 12:50:38.325] Done.

pipe_run_step(p, "add2", downstream = TRUE)
#> INFO  [2026-06-15 12:50:38.327] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:38.329] Step 1/4 data (upstream)
#> INFO  [2026-06-15 12:50:38.331] Step 2/4 add1 (upstream)
#> INFO  [2026-06-15 12:50:38.335] Step 3/4 add2
#> INFO  [2026-06-15 12:50:38.338] Step 4/4 mult (downstream)
#> INFO  [2026-06-15 12:50:38.340] Finished execution of steps.
#> INFO  [2026-06-15 12:50:38.340] Done.

pipe_run_step(p, "mult", upstream = TRUE)
#> INFO  [2026-06-15 12:50:38.342] Start step run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:38.343] Step 1/4 data (upstream)
#> INFO  [2026-06-15 12:50:38.346] Step 2/4 add1 (upstream)
#> INFO  [2026-06-15 12:50:38.349] Step 3/4 add2 (upstream)
#> INFO  [2026-06-15 12:50:38.352] Step 4/4 mult
#> INFO  [2026-06-15 12:50:38.354] Finished execution of steps.
#> INFO  [2026-06-15 12:50:38.355] Done.
```
