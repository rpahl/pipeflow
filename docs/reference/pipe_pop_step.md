# Pop steps from the pipeline

Use this function to drop steps from the end of the pipeline.

## Usage

``` r
pipe_pop_step(pip)

pipe_pop_steps_after(pip, step)

pipe_pop_steps_from(pip, step)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step

## Value

`string` the name of the step that was removed

## Methods

- `pipe_pop_step`: drop last step from the pipeline

- `pipe_pop_steps_after`: drop all steps after given steps

- `pipe_pop_steps_from`: drop all steps from and including given steps

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
# pipe_pop_step
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(y = 1) y)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
pipe_pop_step(p)
#> [1] "f2"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New

# pipe_pop_steps_after
pipe_add(p, "f2", \(y = 1) y)
pipe_add(p, "f3", \(z = 1) z)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
#> 4:     f3         [NULL]   FALSE     f3    New
pipe_pop_steps_after(p, "f1")
#> [1] "f2" "f3"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New

# pipe_pop_steps_from
pipe_add(p, "f2", \(y = 1) y)
pipe_add(p, "f3", \(z = 1) z)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
#> 4:     f3         [NULL]   FALSE     f3    New
pipe_pop_steps_from(p, "f1")
#> [1] "f1" "f2" "f3"
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
```
