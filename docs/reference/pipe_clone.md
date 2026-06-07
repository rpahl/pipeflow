# Clone pipeline

Creates a copy of a pipeline object.

## Usage

``` r
pipe_clone(pip, deep = FALSE)
```

## Arguments

- pip:

  `Pipeline` object

- deep:

  `logical` whether to perform a deep copy

## Value

returns the copied `Pipeline` object

## Lifecycle

Deprecated. Legacy API. Use
[`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md)
instead.

## Examples

``` r
p1 <- pipe_new("pipe")
pipe_add(p1, "step1", \(x = 1) x)
p2 <- pipe_clone(p1)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_add(p2, "step2", \(y = 1) y)
p1
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:  step1         [NULL]   FALSE  step1    New
p2
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:  step1         [NULL]   FALSE  step1    New
#> 3:  step2         [NULL]   FALSE  step2    New
```
