# Create new pipeline

A new pipeline is always initialized with one 'data' step, which
basically is a function returning the data.

## Usage

``` r
pipe_new(name, data = NULL, logger = NULL)
```

## Arguments

- name:

  the name of the Pipeline

- data:

  optional data used at the start of the pipeline. The data also can be
  set later using the
  [`pipe_set_data()`](https://github.com/rpahl/pipeflow/reference/pipe_set_data.md)
  function.

- logger:

  custom logger to be used for logging. If no logger is provided, the
  default logger is used, which should be sufficient for most use cases.
  If you do want to use your own custom log function, you need to
  provide a function that obeys the following form:

  `function(level, msg, ...) { your custom logging code here }`

  The `level` argument is a string and will be one of `info`, `warn`, or
  `error`. The `msg` argument is a string containing the message to be
  logged. The `...` argument is a list of named parameters, which can be
  used to add additional information to the log message. Currently, this
  is only used to add the context in case of a step giving a warning or
  error.

  Note that with the default logger, the log layout can be altered any
  time via
  [`set_log_layout()`](https://github.com/rpahl/pipeflow/reference/set_log_layout.md).

## Value

returns the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
instead.

## Examples

``` r
data <- data.frame(x = 1:2, y = 3:4)
p <- pipe_new("myPipe", data = data)
p |>
  pipe_run() |>
  pipe_get_out("data")
#> INFO  [2026-06-20 21:18:56.660] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-20 21:18:56.662] Step 1/1 data
#> INFO  [2026-06-20 21:18:56.663] Finished execution of steps.
#> INFO  [2026-06-20 21:18:56.664] Done.
#>   x y
#> 1 1 3
#> 2 2 4

# Setting data later
p <- pipe_new("myPipe")
pipe_get_data(p)
#> NULL

p <- pipe_set_data(p, data)
pipe_get_data(p)
#>   x y
#> 1 1 3
#> 2 2 4
p |>
  pipe_run() |>
  pipe_get_out("data")
#> INFO  [2026-06-20 21:18:56.673] Start run of 'myPipe' pipeline:
#> INFO  [2026-06-20 21:18:56.676] Step 1/1 data
#> INFO  [2026-06-20 21:18:56.677] Finished execution of steps.
#> INFO  [2026-06-20 21:18:56.678] Done.
#>   x y
#> 1 1 3
#> 2 2 4

# Initialize with custom logger
my_logger <- function(level, msg, ...) {
  cat(level, msg, "\n")
}
p <- pipe_new("myPipe", data = data, logger = my_logger)
p |>
  pipe_run() |>
  pipe_get_out("data")
#> info Start run of 'myPipe' pipeline: 
#> info Step 1/1 data 
#> info Finished execution of steps. 
#> info Done. 
#>   x y
#> 1 1 3
#> 2 2 4
```
