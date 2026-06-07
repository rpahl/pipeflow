# Discard steps from the pipeline

Discard all steps that match a given `pattern`.

## Usage

``` r
pipe_discard_steps(pip, pattern, recursive = FALSE, fixed = TRUE, ...)
```

## Arguments

- pip:

  `Pipeline` object

- pattern:

  `string` containing a regular expression (or character string for
  `fixed = TRUE`) to be matched.

- recursive:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

- fixed:

  `logical` If `TRUE`, `pattern` is a string to be matched as is.
  Overrides all conflicting arguments.

- ...:

  further arguments passed to
  [`grep()`](https://rdrr.io/r/base/grep.html).

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~add1) x + 2)
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
#> 4:  mult3    add1 [NULL]   FALSE  mult3    New
#> 5:  mult4    add2 [NULL]   FALSE  mult4    New

pipe_discard_steps(p, "mult")
#> step 'mult4' was removed
#> step 'mult3' was removed
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New

# Re-add steps
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:   add1    data [NULL]   FALSE   add1    New
#> 3:   add2    add1 [NULL]   FALSE   add2    New
#> 4:  mult3    add1 [NULL]   FALSE  mult3    New
#> 5:  mult4    add2 [NULL]   FALSE  mult4    New

# Discarding 'add1' does not work ...
try(pipe_discard_steps(p, "add1"))
#> Error : cannot remove step 'add1' because the following steps depend on it: 'add2', 'mult3'

# ... unless we enforce to remove its downstream dependencies as well
pipe_discard_steps(p, "add1", recursive = TRUE)
#> Removing step 'add1' and its downstream dependencies: 'add2', 'mult3', 'mult4'
#> step 'add1' was removed
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New

# Trying to discard non-existent steps is just ignored
pipe_discard_steps(p, "non-existent")
```
