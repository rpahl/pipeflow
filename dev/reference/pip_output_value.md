# Annotated step outputs

`pip_output_value()` is a hook to annotate or postprocess the output of
a step, for example to stamp a result with the step it came from or the
time it was computed. It is applied to the output right after the step
function returns, and its result is what is stored as the step's output.
The default method returns `x` unchanged. Methods are meant for your own
classes, not for base classes such as `numeric`.

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

The generic is applied once to the whole output, except for a
partitioned output (see `exec` in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)):
there it is applied once per partition, with the key in `ctx$key`. To
annotate an output element-wise, write a method that walks over the
elements, e.g. for a plain list:

    pip_output_value.list <- function(x, ctx = list(), ...) {
        x[] <- lapply(x, FUN = pip_output_value, ctx = ctx)
        x
    }

It is *not* applied to the outputs of failed steps, which remain the
failure condition, nor to the inputs a step receives from other steps.
An error in a method is a step error and, for a partitioned output,
names the partition key.

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
