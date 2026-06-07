# Get pipeline parameters

Retrieves all unbound function parameters defined in the pipeline where
'unbound' refers to parameters that are not pointing to other steps.

## Usage

``` r
pipe_get_params(pip, ignoreHidden = TRUE)

pipe_get_params_at_step(pip, step, ignoreHidden = TRUE)

pipe_get_params_unique(pip, ignoreHidden = TRUE)

pipe_get_params_unique_json(pip, ignoreHidden = TRUE)
```

## Arguments

- pip:

  `Pipeline` object

- ignoreHidden:

  `logical` if TRUE, hidden parameters (i.e. all paramater names
  starting with a dot) are ignored and thus not returned.

- step:

  `string` name of step

## Value

- `pipe_get_params`: list of parameters, sorted and named by step -
  steps with no parameters are filtered out

- `pipe_get_params_at_step`: list of parameters at given step

- `pipe_get_params_unique`: list of parameters where each parameter is
  only listed once. The values of the parameters will be the values of
  the first step where the parameters were defined, respectively.

- `get_params_unique_json`: flat unnamed json list of unique parameters

## Examples

``` r
# pipe_get_params
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = 2, .z = 3) x + y + .z)
pipe_add(p, "add3", \() 1 + 2)
pipe_get_params(p, ) |> str()
#> List of 2
#>  $ add1:List of 1
#>   ..$ x: num 1
#>  $ add2:List of 2
#>   ..$ x: num 1
#>   ..$ y: num 2
pipe_get_params(p, ignoreHidden = FALSE) |> str()
#> List of 2
#>  $ add1:List of 1
#>   ..$ x: num 1
#>  $ add2:List of 3
#>   ..$ x : num 1
#>   ..$ y : num 2
#>   ..$ .z: num 3

# pipe_get_params_at_step
pipe_get_params_at_step(p, "add2")
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
pipe_get_params_at_step(p, "add2", ignoreHidden = FALSE)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
#> $.z
#> [1] 3
#> 
pipe_get_params_at_step(p, "add3")
#> list()

# pipe_get_params_unique
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = 2, .z = 3) x + y + .z)
pipe_add(p, "mult1", \(x = 4, y = 5, .z = 6, b = ~add2) x * y * b)
pipe_get_params_unique(p)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
pipe_get_params_unique(p, ignoreHidden = FALSE)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
#> $.z
#> [1] 3
#> 

# get_params_unique_json
pipe_get_params_unique_json(p)
#> [
#>   {
#>     "name": "x",
#>     "value": 1
#>   },
#>   {
#>     "name": "y",
#>     "value": 2
#>   }
#> ] 
pipe_get_params_unique_json(p, ignoreHidden = FALSE)
#> [
#>   {
#>     "name": "x",
#>     "value": 1
#>   },
#>   {
#>     "name": "y",
#>     "value": 2
#>   },
#>   {
#>     "name": ".z",
#>     "value": 3
#>   }
#> ] 
```
