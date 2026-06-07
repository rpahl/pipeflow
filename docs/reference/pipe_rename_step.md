# Rename step

Safely rename a step in the pipeline. If new step name would result in a
name clash, an error is given.

## Usage

``` r
pipe_rename_step(pip, from, to)
```

## Arguments

- pip:

  `Pipeline` object

- from:

  `string` the name of the step to be renamed.

- to:

  `string` the new name of the step.

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_rename()`](https://github.com/rpahl/pipeflow/reference/pip_rename.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New

try(pipe_rename_step(p, from = "add1", to = "add2"))
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#> Error : step 'add2' already exists

pipe_rename_step(p, from = "add1", to = "first_add")
p
#>         step   depends    out keepOut  group  state
#>       <char>    <list> <list>  <lgcl> <char> <char>
#> 1:      data           [NULL]   FALSE   data    New
#> 2: first_add      data [NULL]   FALSE   add1    New
#> 3:      add2 first_add [NULL]   FALSE   add2    New
```
