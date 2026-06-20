# Get step information

Get step information

## Usage

``` r
pipe_get_step(pip, step)

pipe_get_step_names(pip)

pipe_get_step_number(pip, step)

pipe_has_step(pip, step)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

## Value

- `pipe_get_step`: `data.table` row containing the step

- `pipe_get_step_names`: `character` vector of step names

- `pipe_get_step_number`: the step number in the pipeline

- `pipe_get_step_number`: whether step exists

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

Deprecated. Legacy API. Use
[`pip_has_step()`](https://github.com/rpahl/pipeflow/reference/pip_has_step.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = 2, z = ~add1) x + y + z)
pipe_run(p)
#> INFO  [2026-06-20 21:19:11.447] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:19:11.448] Step 1/3 data
#> INFO  [2026-06-20 21:19:11.464] Step 2/3 add1
#> INFO  [2026-06-20 21:19:11.468] Step 3/3 add2
#> INFO  [2026-06-20 21:19:11.470] Finished execution of steps.
#> INFO  [2026-06-20 21:19:11.470] Done.

# pipe_get_step_names
pipe_get_step_names(p)
#> [1] "data" "add1" "add2"

# get_step_number
pipe_get_step_number(p, "add1")
#> [1] 2
pipe_get_step_number(p, "add2")
#> [1] 3

# pipe_has_step
pipe_has_step(p, "add1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> [1] TRUE
pipe_has_step(p, "foo")
#> [1] FALSE

# pipe_get_step
add1 <- pipe_get_step(p, "add1")
add1
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   add1 <function[1]> function <list[2]>    data    2,3   FALSE   add1
#>    description                time  state
#>         <char>              <POSc> <char>
#> 1:             2026-06-20 21:19:11   Done

add1[["params"]]
#> [[1]]
#> [[1]]$data
#> ~data
#> <environment: 0x0000022e84b51928>
#> 
#> [[1]]$x
#> [1] 1
#> 
#> 

add1[["fun"]]
#> [[1]]
#> function (data = ~data, x = 1) 
#> x + data
#> <environment: 0x0000022e8473cf28>
#> 

try(p$get_step("foo")) # error: step 'foo' does not exist
#> Error : step 'foo' does not exist
```
