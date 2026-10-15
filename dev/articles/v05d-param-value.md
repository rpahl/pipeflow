# Annotated parameter values

Parameters of a pipeline are usually plain values such as numbers or
strings. Some packages, however, store *annotated* values as parameters:
numbers with a unit, values with a label and a description, or parameter
objects that carry metadata for a user interface. The step functions
usually need the plain value, though.

Without further help, every step function of such a pipeline would have
to unwrap its arguments itself, or be wrapped in a function that does
so. The generic
[`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
is a single hook for this. You write one method for your class, and the
pipeline applies it whenever a step is run.

### An annotated value

Let’s use a simple class that holds a number together with its unit.

``` r

library(pipeflow)

quantity <- function(value, unit) {
    structure(list(value = value, unit = unit), class = "quantity")
}

print.quantity <- function(x, ...) {
    cat("<quantity>", x$value, x$unit, "\n")
    invisible(x)
}

quantity(120, "km")
# <quantity> 120 km
```

We use it as the parameters of a step that computes a speed.

``` r

p <- pip_new("speed") |>
    pip_add("speed", \(distance = 1, time = 1) distance / time)

pip_set_params(
    p,
    list(distance = quantity(120, "km"), time = quantity(2, "h"))
)
```

Running the pipeline fails, because the step function receives the
`quantity` objects, not the numbers:

``` r

tryCatch(
    pip_run(p, lgr = NULL),
    error = \(e) conditionMessage(e)
)
# [1] "step 'speed': non-numeric argument to binary operator"
```

### Write and register a method

[`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
is an S3 generic, which has a default method that returns the value
unchanged. For our class we add a method that returns the plain value.

``` r

pip_param_value.quantity <- function(x, ...) x$value
```

Inside a script, register the method with
[`registerS3method()`](https://rdrr.io/r/base/ns-internal.html):

``` r

registerS3method("pip_param_value", "quantity", pip_param_value.quantity)
```

In a package, register it in the `NAMESPACE` file instead, either
directly with `S3method(pipeflow::pip_param_value, quantity)` or, if you
use roxygen, by tagging the method with
`@exportS3Method pipeflow::pip_param_value`. Methods are meant for your
own classes, not for base classes such as `numeric` or `character`.

Now the pipeline runs:

``` r

pip_run(p, lgr = NULL)
p[["speed", "out"]]
# [1] 60
```

The step function received the plain numbers, while the stored
parameters still hold the annotated values.

``` r

pip_get_params(p)
# $distance
# <quantity> 120 km 
# 
# $time
# <quantity> 2 h
```

### Where the method is applied

The generic is applied once to each *unbound* argument of a step right
before the step function is called. This includes extra parameters
passed on via `...`:

``` r

p2 <- pip_new("defaults") |>
    pip_add(
        "describe",
        \(x = quantity(3, "m"), ...) {
            c(x = class(x), dots = sapply(list(...), class))
        },
        params = list(extra = quantity(1, "s"))
    )

pip_run(p2, lgr = NULL)
p2[["describe", "out"]]
#          x dots.extra 
#  "numeric"  "numeric"
```

The generic is *not* applied to

- the outputs of other steps, that is, arguments defined as `~step`,
- the partitions of a split step, and
- failure objects (see `pip_run(on_error = "continue")`).

Step outputs, for example, are passed on as they are:

``` r

p3 <- pip_new("outputs") |>
    pip_add("make", \(v = 3) quantity(v, "m")) |>
    pip_add("inspect", \(q = ~make) class(q))

pip_run(p3, lgr = NULL)
p3[["inspect", "out"]]
# [1] "quantity"
```

The `quantity` that `make` returns reaches `inspect` untouched.

### Errors in a method

A method runs as part of the step call. An error in a method is
therefore an error of the step, handled like any other step error as
described in the vignette on [handling
failures](https://rpahl.github.io/pipeflow/dev/articles/v05c-error-handling.md).
Let’s add a class that checks its unit.

``` r

checked <- function(value, unit) {
    structure(list(value = value, unit = unit), class = "checked")
}

pip_param_value.checked <- function(x, ...) {
    if (!x$unit %in% c("m", "km")) {
        stop("unknown unit '", x$unit, "'")
    }
    x$value
}
registerS3method("pip_param_value", "checked", pip_param_value.checked)

p4 <- pip_new("errors") |>
    pip_add("double", \(x = 1) x * 2) |>
    pip_add("increment", \(y = ~double) y + 1)

pip_set_params(p4, list(x = checked(1, "ft")))
```

By default, the run stops and the message tells which step failed:

``` r

tryCatch(
    pip_run(p4, lgr = NULL),
    error = \(e) conditionMessage(e)
)
# [1] "step 'double': unknown unit 'ft'"
```

With `on_error = "continue"`, the step is marked as failed and the run
goes on with the steps that don’t depend on it. The run also signals a
`pipeflow_run_failed` warning, which we suppress here.

``` r

suppressWarnings(
    pip_run(p4, lgr = NULL, on_error = "continue")
)

pip_data(p4)[, c("step", "state")]
#         step  state
#       <char> <char>
# 1:    double failed
# 2: increment    new
```

As usual, the condition of the failed step is stored as its output.

``` r

conditionMessage(p4[["double", "out"]])
# [1] "unknown unit 'ft'"
```

### Summary

- `pip_param_value(x)` unwraps a stored parameter value right before the
  step function is called, and it does not change the stored parameters.
- Define one method per annotated class and register it as an S3 method.
- It applies to unbound arguments only (`params`, defaults, and `...`),
  not to step outputs, partitions or failure objects.
- An error in a method is a step error, so `on_error` applies.
