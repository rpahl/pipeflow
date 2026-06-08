# Append string to all step names

Appends string to all step names and takes care of updating step
dependencies accordingly.

## Usage

``` r
pipe_append_to_step_names(pip, postfix, sep = ".")
```

## Arguments

- pip:

  `Pipeline` object

- postfix:

  `string` to be appended to each step name.

- sep:

  `string` separator between step name and postfix.

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
p <- pipe_new("pipe")
pipe_add(p, "step1", \(x = 1) x)
pipe_add(p, "step2", \(y = 1) y)
pipe_append_to_step_names(p, "new")
p
#>         step depends    out keepOut  group  state
#>       <char>  <list> <list>  <lgcl> <char> <char>
#> 1:  data.new         [NULL]   FALSE   data    New
#> 2: step1.new         [NULL]   FALSE  step1    New
#> 3: step2.new         [NULL]   FALSE  step2    New
pipe_append_to_step_names(p, "foo", sep = "__")
p
#>              step depends    out keepOut  group  state
#>            <char>  <list> <list>  <lgcl> <char> <char>
#> 1:  data.new__foo         [NULL]   FALSE   data    New
#> 2: step1.new__foo         [NULL]   FALSE  step1    New
#> 3: step2.new__foo         [NULL]   FALSE  step2    New
```
