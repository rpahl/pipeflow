# Run pipeline

Runs all new and/or outdated pipeline steps.

## Usage

``` r
pipe_run(
  pip,
  force = FALSE,
  recursive = TRUE,
  cleanUnkept = FALSE,
  progress = NULL,
  showLog = TRUE
)
```

## Arguments

- pip:

  `Pipeline` object

- force:

  `logical` if `TRUE` all steps are run regardless of whether they are
  outdated or not.

- recursive:

  `logical` if `TRUE` and a step returns a new pipeline, the run of the
  current pipeline is aborted and the new pipeline is run recursively.

- cleanUnkept:

  `logical` if `TRUE` all output that was not marked to be kept is
  removed after the pipeline run. This option can be useful if temporary
  results require a lot of memory.

- progress:

  `function` this parameter can be used to provide a custom progress
  function of the form `function(value, detail)`, which will show the
  progress of the pipeline run for each step, where `value` is the
  current step number and `detail` is the name of the step.

- showLog:

  `logical` should the steps be logged during the pipeline run?

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
instead.

## Examples

``` r
# Simple pipeline
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
pipe_add(p, "add2", \(x = ~add1, z = 2) x + z)
pipe_add(p, "final", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
p |>
  pipe_run() |>
  pipe_collect_out()
#> INFO  [2026-06-07 17:34:01.942] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:01.943] Step 1/4 data
#> INFO  [2026-06-07 17:34:01.946] Step 2/4 add1
#> INFO  [2026-06-07 17:34:01.949] Step 3/4 add2
#> INFO  [2026-06-07 17:34:01.952] Step 4/4 final
#> INFO  [2026-06-07 17:34:01.957] Finished execution of steps.
#> INFO  [2026-06-07 17:34:01.958] Done.
#> $final
#> [1] 8
#> 
pipe_set_params(p, list(z = 4))  # outdates steps add2 and final
p
#>      step   depends    out keepOut  group    state
#>    <char>    <list> <list>  <lgcl> <char>   <char>
#> 1:   data                1   FALSE   data     Done
#> 2:   add1      data      2   FALSE   add1     Done
#> 3:   add2      add1      4   FALSE   add2 Outdated
#> 4:  final add1,add2      8    TRUE  final Outdated

p |> pipe_run() |> pipe_collect_out()
#> INFO  [2026-06-07 17:34:01.967] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:01.969] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-07 17:34:01.970] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-07 17:34:01.971] Step 3/4 add2
#> INFO  [2026-06-07 17:34:01.973] Step 4/4 final
#> INFO  [2026-06-07 17:34:01.975] Finished execution of steps.
#> INFO  [2026-06-07 17:34:01.975] Done.
#> $final
#> [1] 12
#> 

pipe_run(p, cleanUnkept = TRUE)
#> INFO  [2026-06-07 17:34:01.977] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:01.978] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-07 17:34:01.979] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-07 17:34:01.980] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-07 17:34:01.982] Step 4/4 final - skip 'done' step
#> INFO  [2026-06-07 17:34:01.982] Finished execution of steps.
#> INFO  [2026-06-07 17:34:01.983] Clean temporary results.
#> INFO  [2026-06-07 17:34:01.983] Done.
p
#>      step   depends    out keepOut  group    state
#>    <char>    <list> <list>  <lgcl> <char>   <char>
#> 1:   data           [NULL]   FALSE   data Outdated
#> 2:   add1      data [NULL]   FALSE   add1 Outdated
#> 3:   add2      add1 [NULL]   FALSE   add2 Outdated
#> 4:  final add1,add2     12    TRUE  final     Done

# Recursive pipeline (for advanced users)
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
pipe_add(p, "new_pipe", \(x = ~add1) {
    p2 <- pipe_new("new_pipe", data = x)
    pipe_add(p2, "add1", \(x = ~data) x + 1)
    pipe_add(p2, "add2", \(x = ~add1) x + 2, keepOut = TRUE)
  }
)
p |> pipe_run() |> pipe_collect_out()
#> INFO  [2026-06-07 17:34:01.991] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:01.993] Step 1/3 data
#> INFO  [2026-06-07 17:34:01.995] Step 2/3 add1
#> INFO  [2026-06-07 17:34:01.997] Step 3/3 new_pipe
#> INFO  [2026-06-07 17:34:02.002] Abort pipeline execution and restart on new.
#> INFO  [2026-06-07 17:34:02.003] Start run of 'new_pipe' pipeline:
#> INFO  [2026-06-07 17:34:02.004] Step 1/3 data
#> INFO  [2026-06-07 17:34:02.006] Step 2/3 add1
#> INFO  [2026-06-07 17:34:02.009] Step 3/3 add2
#> INFO  [2026-06-07 17:34:02.010] Finished execution of steps.
#> INFO  [2026-06-07 17:34:02.010] Done.
#> $add2
#> [1] 5
#> 

# Run pipeline with progress bar
p <- pipe_new("pipe", data = 1)
pipe_add(p, "first step", \() Sys.sleep(0.5))
pipe_add(p, "second step", \() Sys.sleep(0.5))
pipe_add(p, "last step", \() Sys.sleep(0.5))
pb <- txtProgressBar(min = 1, max = pipe_length(p), style = 3)
fprogress <- function(value, detail) {
   setTxtProgressBar(pb, value)
}
pipe_run(p, progress = fprogress, showLog = FALSE)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
```
