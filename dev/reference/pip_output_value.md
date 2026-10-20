# Annotated step outputs

`pip_output_value()` is an extension point for annotating the outputs of
a pipeline, for example to stamp a result with the step it came from or
the time it was computed. It can be thought of as a hook that allows you
to postprocess or annotate the output of a step before it is stored. The
generic is applied to the output of a step right after the step function
returns, and its result is what is stored as the step's output.

## Usage

``` r
pip_output_value(x, ctx = list(), ...)
```

## Arguments

- x:

  The output value of a step, or one of its partitions if the output is
  partitioned.

- ctx:

  Named list describing the context of the value:

  - `step`: the name of the step, as it was before the step ran.

  - `key`: the partition key, or `NULL` if the output is not
    partitioned.

  - `time`: the time stamp of the step's run, which is the same for all
    partition keys and is also stored as the step's `time`.

- ...:

  Currently unused, reserved for future extensions. Methods should
  accept it.

## Value

The value that is stored as the step's output.

## Details

The default method returns `x` unchanged. Methods are meant for your own
classes, not for base classes such as `numeric` or `character`.

The generic is applied once to the output as a whole, so a method sees
the whole object a step returned. The one exception is a partitioned
output (a step with `exec = "split"`, or a step that was mapped over a
partitioned input): there the generic is applied once per partition, and
each partition gets its key in `ctx$key`.

If you want to annotate an output element-wise instead of the whole
object, just write a method that walks over each element. For example,
for a plain list, a method could look something like this:

    pip_output_value.list <- function(x, ctx = list(), ...) {
        x[] <- lapply(x, FUN = pip_output_value, ctx = ctx)
        x
    }

The generic is *not* applied to the outputs of failed steps (see
`pip_run(on_error = "continue")`), which remain the condition of the
failure, nor to the inputs a step receives from other steps: it acts on
what a step returns, once. An error in a method is an error of the step
and is handled like any other step error; for a partitioned output it
names the partition key the method failed for.

## See also

[`pip_param_value()`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md),
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)

## Examples

``` r
# Stamp every output with the step it came from
stamped <- function(x) structure(x, class = "stamped")

pip_output_value.stamped <- function(x, ctx = list(), ...) {
    attr(x, "step") <- ctx[["step"]]
    x
}
registerS3method("pip_output_value", "stamped", pip_output_value.stamped)

p <- pip_new() |>
    pip_add("make", \(n = 3) stamped(seq_len(n)))
pip_run(p, lgr = NULL)
attr(p[["make", "out"]], "step")
#> [1] "make"
```
