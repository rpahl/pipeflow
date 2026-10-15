# Unwrap a parameter value before a step function is called

`pip_param_value()` is an extension point for packages that store
annotated values (e.g. values with units or labels, or parameter objects
that carry UI metadata) as step parameters. When a step runs, the
generic is applied once to each unbound argument of the step, that is,
to the values that come from `params` or from the defaults of the step
function, and its result is what the step function receives. The stored
parameters are not changed.

## Usage

``` r
pip_param_value(x, ...)
```

## Arguments

- x:

  The parameter value as stored in the step.

- ...:

  Currently unused, reserved for future extensions. Methods should
  accept it.

## Value

The value the step function receives for the argument.

## Details

The default method returns `x` unchanged. Methods are meant for your own
classes, not for base classes such as `numeric` or `character`. Note
that this is a hook that unwraps a stored value, not an accessor: use
[`pip_get_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
to read parameters from a pipeline.

The generic is *not* applied to the outputs of other steps (arguments
defined as `~step`), to partitions or to failure objects. An error in a
method is an error of the step and is handled like any other step error,
including `pip_run(on_error = "continue")`.

## See also

[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md),
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)

## Examples

``` r
# A value that carries annotations
my_number <- structure(
    list(value = 1.23, label = "Number", descr = "I am an example"),
    class = "foo"
)

# Step functions then simply see the plain value
pip_param_value.foo <- function(x, ...) x$value
registerS3method("pip_param_value", "foo", pip_param_value.foo)

p <- pip_new() |>
    pip_add("double", \(x = 1) x * 2)
pip_set_params(p, list(x = my_number))
pip_run(p, lgr = NULL)
pip_collect(p)
#> $double
#> [1] 2.46
#> 
```
