# Replace pipeline step

Replaces an existing pipeline step.

## Usage

``` r
pipe_replace_step(
  pip,
  step,
  fun,
  params = list(),
  description = "",
  group = step,
  keepOut = FALSE
)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` the name of the step. Each step name must be unique.

- fun:

  `function` or name of the function to be applied at the step. Both
  existing and anonymous/lambda functions can be used. All function
  parameters must have default values. If a parameter is missing a
  default value in the function signature, alternatively, it can be set
  via the `params` argument (see Examples section with
  [`mean()`](https://rdrr.io/r/base/mean.html) function).

- params:

  `list` list of parameters to set or overwrite parameters of the passed
  function.

- description:

  `string` optional description of the step

- group:

  `string` output collected after pipeline execution (see function
  [`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md))
  is grouped by the defined group names. By default, this is the name of
  the step, which comes in handy when the pipeline is copy-appended
  multiple times to keep the results of the same function/step grouped
  at one place.

- keepOut:

  `logical` if `FALSE` (default) the output of the step is not collected
  when calling
  [`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)
  after the pipeline run. This option is used to only keep the results
  that matter and skip intermediate results that are not needed. See
  also function
  [`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)
  for more details.

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_replace()`](https://github.com/rpahl/pipeflow/reference/pip_replace.md)
instead.

## See also

[`pipe_add()`](https://github.com/rpahl/pipeflow/reference/pipe_add.md)

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 1) x + y)
pipe_add(p, "add2", \(x = ~data, y = 2) x + y)
pipe_add(p, "mult", \(x = 1, y = 2) x * y, keepOut = TRUE)
pipe_run(p) |> pipe_collect_out()
#> INFO  [2026-06-14 15:40:00.765] Start run of 'pipe' pipeline:
#> INFO  [2026-06-14 15:40:00.767] Step 1/4 data
#> INFO  [2026-06-14 15:40:00.771] Step 2/4 add1
#> INFO  [2026-06-14 15:40:00.774] Step 3/4 add2
#> INFO  [2026-06-14 15:40:00.777] Step 4/4 mult
#> INFO  [2026-06-14 15:40:00.779] Finished execution of steps.
#> INFO  [2026-06-14 15:40:00.780] Done.
#> $mult
#> [1] 2
#> 
pipe_replace_step(p, "mult", \(x = ~add1, y = ~add2) x * y, keepOut = TRUE)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_run(p) |> pipe_collect_out()
#> INFO  [2026-06-14 15:40:00.786] Start run of 'pipe' pipeline:
#> INFO  [2026-06-14 15:40:00.788] Step 1/4 data - skip 'done' step
#> INFO  [2026-06-14 15:40:00.790] Step 2/4 add1 - skip 'done' step
#> INFO  [2026-06-14 15:40:00.791] Step 3/4 add2 - skip 'done' step
#> INFO  [2026-06-14 15:40:00.793] Step 4/4 mult
#> INFO  [2026-06-14 15:40:00.794] Finished execution of steps.
#> INFO  [2026-06-14 15:40:00.795] Done.
#> $mult
#> [1] 6
#> 
try(pipe_replace_step(p, "foo", \(x = 1) x)) # step 'foo' does not exist
#> Error : step 'foo' does not exist
```
