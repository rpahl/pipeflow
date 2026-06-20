# Set pipeflow log layout

This function provides an easy way to set the basic log layout of the
pipeline logging. For a fine-grained control of the logger, which you
can retrieve via `lgr::get_logger("pipeflow")`, see e.g. the
[logger_config](https://s-fleck.github.io/lgr/reference/logger_config.html)
function from the
[lgr](https://s-fleck.github.io/lgr/reference/lgr-package.html) package.

## Usage

``` r
set_log_layout(layout = c("text", "json"))
```

## Arguments

- layout:

  Layout name, which at this point can be either 'text' or 'json'.

## Value

invisibly returns a `Logger` object

## Examples

``` r
p <- Pipeline$new("pipe", data = 1:2)
p$add("add1", \(data = ~data, x = 1) x + data)
p$run()
#> INFO  [2026-06-20 21:19:11.095] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:19:11.096] Step 1/2 data
#> INFO  [2026-06-20 21:19:11.099] Step 2/2 add1
#> INFO  [2026-06-20 21:19:11.100] Finished execution of steps.
#> INFO  [2026-06-20 21:19:11.101] Done.

lg <- set_log_layout("json")
print(lg)
#> <Logger> [info] pipeflow
#> 
#> appenders:
#>   [[1]]: <AppenderConsole> [all] -> console

p$run()
#> {"application":"pipeflow","level":"info","time":"2026-06-20 19:19:11.122 UTC","message":"Start run of 'pipe' pipeline:"}
#> {"application":"pipeflow","level":"info","time":"2026-06-20 19:19:11.123 UTC","message":"Step 1/2 data - skip 'done' step"}
#> {"application":"pipeflow","level":"info","time":"2026-06-20 19:19:11.130 UTC","message":"Step 2/2 add1 - skip 'done' step"}
#> {"application":"pipeflow","level":"info","time":"2026-06-20 19:19:11.131 UTC","message":"Finished execution of steps."}
#> {"application":"pipeflow","level":"info","time":"2026-06-20 19:19:11.131 UTC","message":"Done."}

set_log_layout("text")
p$run()
#> INFO  [2026-06-20 21:19:11.139] Start run of 'pipe' pipeline:
#> INFO  [2026-06-20 21:19:11.141] Step 1/2 data - skip 'done' step
#> INFO  [2026-06-20 21:19:11.142] Step 2/2 add1 - skip 'done' step
#> INFO  [2026-06-20 21:19:11.143] Finished execution of steps.
#> INFO  [2026-06-20 21:19:11.144] Done.
```
