# Set pipeline parameters

Set unbound function parameters defined in the pipeline where 'unbound'
means parameters that are not linked to other steps. Trying to set
parameters that don't exist in the pipeline is ignored, by default, with
a warning.

## Usage

``` r
pipe_set_params(pip, params, warnUndefined = TRUE)
```

## Arguments

- pip:

  `Pipeline` object

- params:

  `list` of parameters to be set

- warnUndefined:

  `logical` whether to give a warning when trying to set a parameter
  that is not defined in the pipeline.

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = ~data, y = 2) x + y)
pipe_add(p, "add2", \(x = ~data, y = 3) x + y)
pipe_add(p, "mult", \(x = 4, z = 5) x * z)
pipe_get_params(p)
#> $add1
#> $add1$y
#> [1] 2
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 4
#> 
#> $mult$z
#> [1] 5
#> 
#> 
pipe_set_params(p, params = list(x = 3, y = 3))
pipe_get_params(p)
#> $add1
#> $add1$y
#> [1] 3
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 3
#> 
#> $mult$z
#> [1] 5
#> 
#> 
pipe_set_params(p, params = list(x = 5, z = 3))
pipe_get_params(p)
#> $add1
#> $add1$y
#> [1] 3
#> 
#> 
#> $add2
#> $add2$y
#> [1] 3
#> 
#> 
#> $mult
#> $mult$x
#> [1] 5
#> 
#> $mult$z
#> [1] 3
#> 
#> 

suppressWarnings(
  pipe_set_params(p, list(foo = 3)) # gives warning as 'foo' is undefined
)
pipe_set_params(p, list(foo = 3), warnUndefined = FALSE)
```
