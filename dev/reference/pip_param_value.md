# Annotated parameter values

`pip_param_value()` is an extension point for handling non-trivial
parameter values (e.g. values with units or labels, or parameter objects
that carry UI metadata) as step parameters. It can be thought of as a
hook that allows you to preprocess or unwrap parameter values before
they are passed to the step function. When a step runs, the generic is
applied to each unbound argument of the step, that is, to the values
that come from `params` or from the defaults of the step function, and
its result is what the step function receives. The stored parameters are
not changed, and a method should not change them either: it is called
with the stored value, so a method that modifies a reference-backed `x`
such as an environment or an R6 object in place changes the step's
parameter on every run.

## Usage

``` r
pip_param_value(x, ...)

pip_param_value(x, ...) <- value
```

## Arguments

- x:

  The parameter value as stored in the step.

- ...:

  Currently unused, reserved for future extensions. Methods should
  accept it.

- value:

  The plain value to set into the stored value `x`.

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

`` `pip_param_value<-`() `` is the write side of the same extension
point. It turns a plain value into the value that is stored in the step,
so that
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
can keep the annotations of a stored object when it is given a plain
value. Dispatch is on `x`, the value as it is currently stored, and
`value` is the requested value. The default method just returns `value`.
A method decides what a plain `value` and what an object of its own
class means, for example whether to merge it into the stored object or
to use it as is.

Like `pip_param_value()`, this is a hook, not an accessor: `x` is a
stored value, and the parameters of a pipeline are changed with
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md).
The two sides belong together, so that applying `pip_param_value()` to
the result of the setter gives back the plain value that was set:

    pip_param_value(`pip_param_value<-`(x, value = v))   # v

[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
computes the stored value of every affected step *before* it changes any
parameters in the pipeline. If one or more methods fail, an error of
class `pipeflow_set_params_error` reports all failures. This two-phase
approach ensures that either all parameter changes are applied
successfully or, in case of an error, the pipeline stays unchanged. For
that to hold, a method should not modify `x` in place as this would
change pipeline parameters directly. A reference-backed `x` such as an
environment or an R6 object therefore should be copied first.

## See also

[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md),
[`pip_output_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_output_value.md),
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

# The write side keeps the annotations when a plain value is set
`pip_param_value<-.foo` <- function(x, ..., value) {
    if (inherits(value, "foo")) {
        return(value)
    }
    x$value <- value
    x
}
registerS3method("pip_param_value<-", "foo", `pip_param_value<-.foo`)

pip_set_params(p, list(x = 42))
pip_get_params(p) # still a "foo" object, now holding 42
#> $x
#> $value
#> [1] 42
#> 
#> $label
#> [1] "Number"
#> 
#> $descr
#> [1] "I am an example"
#> 
#> attr(,"class")
#> [1] "foo"
#> 
pip_run(p, lgr = NULL)
pip_collect(p)
#> $double
#> [1] 84
#> 
```
