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
#> INFO  [2026-06-13 19:22:18.959] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:18.961] Step 1/4 data
#> INFO  [2026-06-13 19:22:18.964] Step 2/4 add1
#> INFO  [2026-06-13 19:22:18.967] Step 3/4 add2
#> INFO  [2026-06-13 19:22:18.969] Step 4/4 final
#> INFO  [2026-06-13 19:22:18.970] Finished execution of steps.
#> INFO  [2026-06-13 19:22:18.971] Done.
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
#> INFO  [2026-06-13 19:22:18.981] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:18.982] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-13 19:22:18.983] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-13 19:22:18.984] Step 3/4 add2
#> INFO  [2026-06-13 19:22:18.986] Step 4/4 final
#> INFO  [2026-06-13 19:22:18.988] Finished execution of steps.
#> INFO  [2026-06-13 19:22:18.988] Done.
#> $final
#> [1] 12
#> 

pipe_run(p, cleanUnkept = TRUE)
#> INFO  [2026-06-13 19:22:18.990] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:18.991] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-13 19:22:18.992] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-13 19:22:18.993] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-13 19:22:18.994] Step 4/4 final - skip 'done' step
#> INFO  [2026-06-13 19:22:18.995] Finished execution of steps.
#> INFO  [2026-06-13 19:22:18.996] Clean temporary results.
#> INFO  [2026-06-13 19:22:18.996] Done.
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
#> INFO  [2026-06-13 19:22:19.005] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:19.006] Step 1/3 data
#> INFO  [2026-06-13 19:22:19.009] Step 2/3 add1
#> INFO  [2026-06-13 19:22:19.011] Step 3/3 new_pipe
#> INFO  [2026-06-13 19:22:19.016] Abort pipeline execution and restart on new.
#> INFO  [2026-06-13 19:22:19.017] Start run of 'new_pipe' pipeline:
#> INFO  [2026-06-13 19:22:19.018] Step 1/3 data
#> INFO  [2026-06-13 19:22:19.020] Step 2/3 add1
#> INFO  [2026-06-13 19:22:19.022] Step 3/3 add2
#> INFO  [2026-06-13 19:22:19.024] Finished execution of steps.
#> INFO  [2026-06-13 19:22:19.024] Done.
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
