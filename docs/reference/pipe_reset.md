# Reset pipeline

Resets the pipeline to the state before it was run. This means that all
output is removed and the state of all steps is reset to 'New'.

## Usage

``` r
pipe_reset(pip)
```

## Arguments

- pip:

  `Pipeline` object

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(y = 1) y)
pipe_run(p, )
#> INFO  [2026-06-13 17:08:26.886] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:26.888] Step 1/3 data
#> INFO  [2026-06-13 17:08:26.893] Step 2/3 f1
#> INFO  [2026-06-13 17:08:26.897] Step 3/3 f2
#> INFO  [2026-06-13 17:08:26.902] Finished execution of steps.
#> INFO  [2026-06-13 17:08:26.904] Done.
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data            1,2   FALSE   data   Done
#> 2:     f1              1   FALSE     f1   Done
#> 3:     f2              1   FALSE     f2   Done

pipe_reset(p)
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:     f1         [NULL]   FALSE     f1    New
#> 3:     f2         [NULL]   FALSE     f2    New
```
