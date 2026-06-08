# Insert step

Insert step

## Usage

``` r
pipe_insert_after(pip, afterStep, step, ...)

pipe_insert_before(pip, beforeStep, step, ...)
```

## Arguments

- pip:

  `Pipeline` object

- afterStep:

  `string` name of step after which to insert

- step:

  `string` name of step to insert

- ...:

  further arguments passed to
  [`pipe_add()`](https://github.com/rpahl/pipeflow/reference/pipe_add.md)

- beforeStep:

  `string` name of step before which to insert

## Value

returns the `Pipeline` object invisibly

## Methods

- `pipe_insert_after`: insert step after a certain step of the pipeline

- `pipe_insert_before`: insert step before a certain step of the
  pipeline

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
# pipe_insert_after
p <- pipe_new("pipe", data = 1)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(x = ~f1) x)
pipe_insert_after(p, "f1", step = "after_f1", \(x = ~f1) x)
p
#>        step depends    out keepOut    group  state
#>      <char>  <list> <list>  <lgcl>   <char> <char>
#> 1:     data         [NULL]   FALSE     data    New
#> 2:       f1         [NULL]   FALSE       f1    New
#> 3: after_f1      f1 [NULL]   FALSE after_f1    New
#> 4:       f2      f1 [NULL]   FALSE       f2    New

# insert_before
pipe_insert_before(p, "f2", step = "before_f2", \(x = ~f1) 2 * x)
p
#>         step depends    out keepOut     group  state
#>       <char>  <list> <list>  <lgcl>    <char> <char>
#> 1:      data         [NULL]   FALSE      data    New
#> 2:        f1         [NULL]   FALSE        f1    New
#> 3:  after_f1      f1 [NULL]   FALSE  after_f1    New
#> 4: before_f2      f1 [NULL]   FALSE before_f2    New
#> 5:        f2      f1 [NULL]   FALSE        f2    New
```
