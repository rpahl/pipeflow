# Get data

Get the data set for the pipeline

## Usage

``` r
pipe_get_data(pip)
```

## Arguments

- pip:

  `Pipeline` object

## Value

the output defined in the `data` step, which by default is the first
step of the pipeline

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_get_data(p)
#> [1] 1 2
pipe_set_data(p, 3:4)
pipe_get_data(p)
#> [1] 3 4
```
