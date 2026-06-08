# Remove certain step from the pipeline.

Can be used to remove any given step. If other steps depend on the step
to be removed, an error is given and the removal is blocked, unless
`recursive` was set to `TRUE`.

## Usage

``` r
pipe_remove_step(pip, step, recursive = FALSE)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` the name of the step to be removed

- recursive:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_remove()`](https://github.com/rpahl/pipeflow/reference/pip_remove.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
pipe_add(p, "mult1", \(x = 1, y = ~add2) x * y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
#> 4:  mult1    add2 [NULL]   FALSE  mult1    New

pipe_remove_step(p, "mult1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New

try(pipe_remove_step(p, "add1"))
#> Error : cannot remove step 'add1' because the following steps depend on it: 'add2'
pipe_remove_step(p, "add1", recursive = TRUE)
#> Removing step 'add1' and its downstream dependencies: 'add2'
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
```
