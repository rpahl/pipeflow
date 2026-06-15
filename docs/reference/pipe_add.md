# Add pipeline step

A pipeline consists of a series of steps, which usually are added one by
one. Each step is made up of a function computing something once the
pipeline is run. This function can be an existing R function (e.g.
[`mean()`](https://rdrr.io/r/base/mean.html)) or an anonymous/lambda
function specifically defined for the pipeline. One useful feature is
that function parameters can refer to results of earlier pipeline steps
using the syntax `x = ~earlier_step_name` - see the Examples for more
details.

## Usage

``` r
pipe_add(
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

  `string` output collected after pipeline execution (see
  [`pipe_collect_out()`](https://github.com/rpahl/pipeflow/reference/pipe_collect_out.md)
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
[`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
instead.

## Examples

``` r
# Add steps with lambda functions
p <- pipe_new("myPipe", data = 1)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_add(p, "s1", \(x = ~data) 2 * x) # use input data
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_add(p, "s2", \(x = ~data, y = ~s1) x * y)
try(pipe_add(p, "s2", \(z = 3) 3)) # error: step 's2' exists already
#> Error : step 's2' already exists
try(pipe_add(p, "s3", \(z = ~foo) 3)) # dependency 'foo' not found
#> Error : step 's3': dependency 'foo' not found
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     s1    data [NULL]   FALSE     s1    New
#> 3:     s2 data,s1 [NULL]   FALSE     s2    New

# Add step with existing function
p <- pipe_new("myPipe", data = c(1, 2, NA, 3, 4))
try(pipe_add(p, "calc_mean", mean)) # default value for x is missing
#> Error : 'x' parameter(s) must have default values
pipe_add(p, "calc_mean", mean, params = list(x = ~data, na.rm = TRUE))
p |>
  pipe_run() |>
  pipe_get_out("calc_mean")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> INFO  [2026-06-15 12:50:24.260] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-15 12:50:24.262] Step 1/2 data
#> INFO  [2026-06-15 12:50:24.264] Step 2/2 calc_mean
#> INFO  [2026-06-15 12:50:24.266] Finished execution of steps.
#> INFO  [2026-06-15 12:50:24.266] Done.
#> [1] 2.5

# Step description
p <- pipe_new("myPipe", data = 1:10)
pipe_add(p, "s1", \(x = ~data) 2 * x, description = "multiply by 2")
print(p, verbose = TRUE) # print all columns including description
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   data <function[1]> function <list[0]>         [NULL]   FALSE   data
#> 2:     s1 <function[1]> function <list[1]>    data [NULL]   FALSE     s1
#>      description                time  state
#>           <char>              <POSc> <char>
#> 1:               2026-06-15 12:50:24    New
#> 2: multiply by 2 2026-06-15 12:50:24    New


# Group output
p <- pipe_new("myPipe", data = data.frame(x = 1:2, y = 3:4))
pipe_add(p, "prep_x", \(data = ~data) data$x, group = "prep")
pipe_add(p, "prep_y", \(data = ~data) (data$y)^2, group = "prep")
pipe_add(p, "sum", \(x = ~prep_x, y = ~prep_y) x + y)
p |>
  pipe_run() |>
  pipe_collect_out(all = TRUE)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> INFO  [2026-06-15 12:50:24.283] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-15 12:50:24.284] Step 1/4 data
#> INFO  [2026-06-15 12:50:24.287] Step 2/4 prep_x
#> INFO  [2026-06-15 12:50:24.290] Step 3/4 prep_y
#> INFO  [2026-06-15 12:50:24.292] Step 4/4 sum
#> INFO  [2026-06-15 12:50:24.294] Finished execution of steps.
#> INFO  [2026-06-15 12:50:24.294] Done.
#> $data
#>   x y
#> 1 1 3
#> 2 2 4
#> 
#> $prep
#> $prep$prep_x
#> [1] 1 2
#> 
#> $prep$prep_y
#> [1]  9 16
#> 
#> 
#> $sum
#> [1] 10 18
#> 
```
