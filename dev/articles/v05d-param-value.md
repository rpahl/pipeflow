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

### Setting a plain value

So far the annotated values were set as whole objects. A user interface,
however, usually hands back a plain value: the number the user typed,
not the `quantity` object around it. Setting it with
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
would replace the stored object and lose its unit.

``` r

p5 <- pip_new("speed") |>
    pip_add("speed", \(distance = 1) distance)

pip_set_params(p5, list(distance = quantity(120, "km")))
pip_set_params(p5, list(distance = 150))

pip_get_params(p5)
# $distance
# [1] 150
```

The write side of the same extension point fixes this.
`pip_param_value<-()` turns a plain value into the value that is stored,
and it dispatches on the value that the step currently holds.

``` r

`pip_param_value<-.quantity` <- function(x, ..., value) {
    if (inherits(value, "quantity")) {
        return(value)
    }
    x$value <- value
    x
}

registerS3method(
    "pip_param_value<-",
    "quantity",
    `pip_param_value<-.quantity`
)
```

Now a plain value is merged into the stored object, and the unit
survives.

``` r

p6 <- pip_new("speed") |>
    pip_add("speed", \(distance = 1) distance)

pip_set_params(p6, list(distance = quantity(120, "km")))
pip_set_params(p6, list(distance = 150))

pip_get_params(p6)
# $distance
# <quantity> 150 km
```

The step function still receives the plain value, because the two sides
belong together.

``` r

pip_run(p6, lgr = NULL)
p6[["speed", "out"]]
# [1] 150
```

The default method replaces the stored value, which is what pipelines
without any method do. A method decides for itself what a plain value
and what an object of its own class means; the method above uses an
object as it is and merges anything else.

Because the merged value is what
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
compares with the stored one, setting a plain value that the stored
object already holds doesn’t outdate the step.

``` r

pip_set_params(p6, list(distance = 150))
p6
# <pipeflow> speed (1 step)
# -------------------------
#     step   params depends state out
# 1: speed distance          done 150
# -------------------------
# <ready> last run: 2026-10-25 08:13:21
```

#### Errors while setting

[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
computes the value to store for every affected step before it changes
anything. If a method fails, nothing is set and the error reports all
failures at once.

``` r

`pip_param_value<-.checked` <- function(x, ..., value) {
    if (!is.numeric(value)) {
        stop("value must be numeric")
    }
    x$value <- value
    x
}
registerS3method(
    "pip_param_value<-",
    "checked",
    `pip_param_value<-.checked`
)

p7 <- pip_new("checked") |>
    pip_add("a", \(x = 1) x) |>
    pip_add("b", \(y = 2) y)
pip_set_params(p7, list(x = checked(1, "m"), y = checked(2, "m")))

err <- tryCatch(
    pip_set_params(p7, list(x = "no", y = "no")),
    error = \(e) e
)
cat(conditionMessage(err))
# Cannot set 2 parameters: nothing was set.
#   step 'a', parameter 'x': value must be numeric
#   step 'b', parameter 'y': value must be numeric
```

The condition has class `pipeflow_set_params_error` and carries a table
of the failures, which a user interface can show next to the respective
inputs.

``` r

class(err)
# [1] "pipeflow_set_params_error" "error"                     "condition"
err$errors
#   step name               message
# 1    a    x value must be numeric
# 2    b    y value must be numeric
```

Nothing was set:

``` r

pip_get_params(p7)
# $x
# $value
# [1] 1
# 
# $unit
# [1] "m"
# 
# attr(,"class")
# [1] "checked"
# 
# $y
# $value
# [1] 2
# 
# $unit
# [1] "m"
# 
# attr(,"class")
# [1] "checked"
```

### Summary

- `pip_param_value(x)` unwraps a stored parameter value right before the
  step function is called, and it does not change the stored parameters.
- Define one method per annotated class and register it as an S3 method.
- It applies to unbound arguments only (`params`, defaults, and `...`),
  not to step outputs, partitions or failure objects.
- An error in a method is a step error, so `on_error` applies.
- [`` `pip_param_value<-`() ``](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)
  is the write side: it turns a plain value into the value that is
  stored, so that annotations survive a
  [`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
  call that passes a plain value.
- Either method returns a value; neither should modify its `x` in place.
