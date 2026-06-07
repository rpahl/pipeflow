# Set parameters at step

Set unbound function parameters defined at given pipeline step where
'unbound' means parameters that are not linked to other steps. If one or
more parameters don't exist, an error is given.

## Usage

``` r
pipe_set_params_at_step(pip, step, params)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` the name of the step

- params:

  `list` of parameters to be set

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 2, z = 3) x + y)
pipe_set_params_at_step(p, step = "add1", params = list(y = 5, z = 6))
pipe_get_params(p)
#> $add1
#> $add1$y
#> [1] 5
#> 
#> $add1$z
#> [1] 6
#> 
#> 

try(
  pipe_set_params_at_step(p, step = "add1", params = list(foo = 3))
)
#> Error : Unable to set parameter(s) foo at step add1 - candidates are y, z
```
