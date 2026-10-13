# Get the run state of a pipeline

Returns the state of the last (or current) run of a pipeline. Views
share the run state of their pipeline.

## Usage

``` r
pip_run_state(x)
```

## Arguments

- x:

  A pipeflow pip or view.

## Value

A single character string.

## Details

The run state is one of

- `"ready"`: the pipeline is new, was reset, or its last run finished
  without failures.

- `"running"`: a run is in progress (seen from within a step).

- `"restart"`: a step requested a restart, which
  [`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md)
  carries out right after the step.

- `"halted"`: a step halted the run (see section 'Runtime control flow
  via restart and halt' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md)).

- `"failed"`: the last run stopped on an error (`on_error = "stop"`).

- `"continued"`: the last run had failed steps but went on
  (`on_error = "continue"`).

[`pip_reset()`](https://rpahl.github.io/pipeflow/reference/pip_reset.md)
sets the run state back to `"ready"`.

## See also

[`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md),
[`pip_reset()`](https://rpahl.github.io/pipeflow/reference/pip_reset.md)

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("fail", \(x = ~load) stop("boom"))

pip_run_state(p) # "ready"
#> [1] "ready"
try(pip_run(p, lgr = NULL))
#> Error : step 'fail': boom
pip_run_state(p) # "failed"
#> [1] "failed"

suppressWarnings(pip_run(p, on_error = "continue", lgr = NULL))
pip_run_state(p) # "continued"
#> [1] "continued"

pip_reset(p)
pip_run_state(p) # "ready"
#> [1] "ready"
```
