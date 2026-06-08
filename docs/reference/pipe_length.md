# Length of the pipeline

Length of the pipeline

## Usage

``` r
pipe_length(pip)
```

## Arguments

- pip:

  `Pipeline` object

## Value

`numeric` length of pipeline, that is, the total number of steps

## Lifecycle

Deprecated. Legacy API. Use
[`length()`](https://rdrr.io/r/base/length.html) instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(y = 1) y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
pipe_length(p)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> [1] 3
```
