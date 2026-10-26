# Default logger of pipeflow

The logging function that
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
uses by default. It writes one line per message to the console in the
form `<level> [<UTC time stamp>]: <msg>`, for example
`info [2026-01-01 12:34:56.789 UTC]: Step 1/2 load`.

## Usage

``` r
pipeflow_lgr(level, msg)
```

## Arguments

- level:

  The log level, one of `"info"`, `"warn"` or `"error"`.

- msg:

  The message, a single string.

## Value

`NULL`, invisibly.

## Details

It is exported so that custom loggers can build on it, for example to
add a prefix, or to drop some levels and pass the rest on to the default
logger. A logger is any function with the arguments `level` and `msg`.
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
calls it as `lgr(level = level, msg = msg)`.

## See also

[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
for how loggers are used and how to switch logging off with
`lgr = NULL`.

## Examples

``` r
pipeflow_lgr("info", "Hello")
#> info [2026-10-26 16:24:39.522 UTC]: Hello

p <- pip_new("demo") |>
    pip_add("load", \(n = 3) seq_len(n)) |>
    pip_add("double", \(x = ~load) x * 2)

# Add a prefix to every message
prefixed <- function(level, msg) {
    pipeflow_lgr(level, paste("[demo]", msg))
}
pip_run(p, lgr = prefixed)
#> info [2026-10-26 16:24:39.526 UTC]: [demo] Starting run of pipeflow 'demo'
#> info [2026-10-26 16:24:39.526 UTC]: [demo] Step 1/2 load
#> info [2026-10-26 16:24:39.527 UTC]: [demo] Step 2/2 double
#> info [2026-10-26 16:24:39.527 UTC]: [demo] Finished in 0.002 s (2 done, 0 skipped, 0 failed)

# Drop the "info" messages and pass the rest on
quiet <- function(level, msg) {
    if (level != "info") {
        pipeflow_lgr(level, msg)
    }
}
pip_run(p, lgr = quiet, force = TRUE)
```
