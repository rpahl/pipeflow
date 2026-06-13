# Print the pipeline as a table

Print the pipeline as a table

## Usage

``` r
pipe_print(pip, verbose = FALSE)
```

## Arguments

- pip:

  `Pipeline` object

- verbose:

  `logical` if `TRUE`, print all columns of the pipeline, otherwise only
  the most relevant columns are displayed.

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`print()`](https://rdrr.io/r/base/print.html) instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
p$add("f1", \(x = 1) x)
p$add("f2", \(y = 1) y)
pipe_print(p)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
pipe_print(p, verbose = TRUE)
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   data <function[1]> function <list[0]>         [NULL]   FALSE   data
#> 2:     f1 <function[1]> function <list[1]>         [NULL]   FALSE     f1
#> 3:     f2 <function[1]> function <list[1]>         [NULL]   FALSE     f2
#>    description                time  state
#>         <char>              <POSc> <char>
#> 1:             2026-06-13 19:22:16    New
#> 2:             2026-06-13 19:22:16    New
#> 3:             2026-06-13 19:22:16    New

# Also works with standard print function
print(p)
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
print(p, verbose = TRUE)
#>      step           fun funcName    params depends    out keepOut  group
#>    <char>        <list>   <char>    <list>  <list> <list>  <lgcl> <char>
#> 1:   data <function[1]> function <list[0]>         [NULL]   FALSE   data
#> 2:     f1 <function[1]> function <list[1]>         [NULL]   FALSE     f1
#> 3:     f2 <function[1]> function <list[1]>         [NULL]   FALSE     f2
#>    description                time  state
#>         <char>              <POSc> <char>
#> 1:             2026-06-13 19:22:16    New
#> 2:             2026-06-13 19:22:16    New
#> 3:             2026-06-13 19:22:16    New
```
