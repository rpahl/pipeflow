# Get step information

Get step information

Check if pipeline has given step

## Usage

``` r
pipe_get_step(pip, step)

pipe_get_step_names(pip, ...)

pipe_get_step_number(pip, ...)

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

`logical` whether step exists

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = 2, z = ~add1) x + y + z)
pipe_run(p)
#> INFO  [2024-12-15 17:56:12.206] Start run of 'pipe' pipeline:
#> INFO  [2024-12-15 17:56:12.207] Step 1/3 data
#> INFO  [2024-12-15 17:56:12.210] Step 2/3 add1
#> INFO  [2024-12-15 17:56:12.227] Step 3/3 add2
#> INFO  [2024-12-15 17:56:12.229] Finished execution of steps.
#> INFO  [2024-12-15 17:56:12.229] Done.

# pipe_get_step_names
pipe_get_step_names(p)
#> [1] "data" "add1" "add2"

# get_step_number
pipe_get_step_number(p, "add1")
#> [1] 2
pipe_get_step_number(p, "add2")
#> [1] 3

# pipe_get_step
add1 <- pipe_get_step(p, "add1")
add1
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   add1 <function[1]> function <list[2]>    data    2,3   FALSE   add1
#>    description                time  state
#>         <char>              <POSc> <char>
#> 1:             2024-12-15 17:56:12   Done

add1[["params"]]
#> [[1]]
#> [[1]]$data
#> ~data
#> <environment: 0x000001e08b103108>
#> 
#> [[1]]$x
#> [1] 1
#> 
#> 

add1[["fun"]]
#> [[1]]
#> function (data = ~data, x = 1) 
#> x + data
#> <environment: 0x000001e08e88fc68>
#> 

try(p$get_step("foo")) # error: step 'foo' does not exist
#> Error : step 'foo' does not exist
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(y = 1) y)
pipe_has_step(p, "f2")
#> [1] TRUE
pipe_has_step(p, "foo")
#> [1] FALSE
```
