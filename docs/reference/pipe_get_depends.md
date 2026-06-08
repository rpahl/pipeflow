# Get step dependencies

Get step dependencies

## Usage

``` r
pipe_get_depends(pip)

pipe_get_depends_down(pip, step, recursive = TRUE)

pipe_get_depends_up(pip, step, recursive = TRUE)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

- recursive:

  `logical` if `TRUE`, dependencies of dependencies are also returned.

## Value

- `pipe_get_depends`: named list of dependencies for each step

- `pipe_get_depends_down`: list of downstream dependencies

- `pipe_get_depends_up`: list of downstream dependencies

## Methods

- `pipe_get_depends`: get all dependencies for all steps defined in the
  pipeline

- `pipe_get_depends_down`: get all downstream dependencies of a given
  step, by default descending recursively.

- `pipe_get_depends_up`: get all upstream dependencies of a given step,
  by default descending recursively.

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

## Examples

``` r
# pipe_get_depends
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_get_depends(p)
#> $data
#> character(0)
#> 
#> $add1
#>      x 
#> "data" 
#> 
#> $add2
#>      x      y 
#> "data" "add1" 
#> 

# pipe_get_depends_down
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
pipe_get_depends_down(p, "add1")
#> [1] "add2"  "mult3" "mult4"
pipe_get_depends_down(p, "add1", recursive = FALSE)
#> [1] "add2"  "mult3"

# pipe_get_depends_up
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
pipe_get_depends_up(p, "mult4")
#> [1] "data" "add1" "add2"
pipe_get_depends_up(p, "mult4", recursive = FALSE)
#> [1] "add2"
```
