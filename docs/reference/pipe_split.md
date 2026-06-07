# Split-up pipeline

Splits pipeline into its independent parts. This can be useful, for
example, to split-up the pipeline in order to run each part in parallel.

## Usage

``` r
pipe_split(pip)
```

## Arguments

- pip:

  `Pipeline` object

## Value

list of `Pipeline` objects

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
# Example for two independent calculation paths
p <- pipe_new("pipe", data = 1)
pipe_add(p, "f1", \(x = ~data) x)
pipe_add(p, "f2", \(x = 1) x)
pipe_add(p, "f3", \(x = ~f1) x)
pipe_add(p, "f4", \(x = ~f2) x)
pipe_split(p)
#> [[1]]
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1    data [NULL]   FALSE     f1    New
#> 3:     f3      f1 [NULL]   FALSE     f3    New
#> 
#> [[2]]
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:     f2         [NULL]   FALSE     f2    New
#> 2:     f4      f2 [NULL]   FALSE     f4    New
#> 

# Example of split by three data sets
dataList <- list(a = 1, b = 2, c = 3)
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1, keepOut = TRUE)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipes <- pipe_set_data_split(p, dataList) |> pipe_split()
pipes
#> [[1]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE      a      New
#> 2: add1.a        data.a [NULL]    TRUE      a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE      a Outdated
#> 
#> [[2]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.b               [NULL]   FALSE      b      New
#> 2: add1.b        data.b [NULL]    TRUE      b Outdated
#> 3: mult.b data.b,add1.b [NULL]    TRUE      b Outdated
#> 
#> [[3]]
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.c               [NULL]   FALSE      c      New
#> 2: add1.c        data.c [NULL]    TRUE      c Outdated
#> 3: mult.c data.c,add1.c [NULL]    TRUE      c Outdated
#> 
```
