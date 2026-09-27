# Create a pipeline

Creates a new, empty pipeline. Add steps with
[`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
and execute them with
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_new(name = "pipe")
```

## Arguments

- name:

  The name of the pipeline used for display and logging.

## Value

A pipeflow pipeline object.

## Examples

``` r
p <- pip_new("demo") |>
    pip_add("numbers", \(n = 5) seq_len(n)) |>
    pip_add("squared", \(x = ~numbers) x^2) |>
    pip_add("total",   \(x = ~squared) sum(x))
p
#> <pipeflow> demo (3 steps)
#> -------------------------
#>       step params depends state
#> 1: numbers      n           new
#> 2: squared      x numbers   new
#> 3:   total      x squared   new
#> -------------------------
#> <ready> last run: never
str(p)
#> List of 3
#>  $ name  : chr "demo"
#>  $ view  : NULL
#>  $ pipenv:<environment: 0x559204ac61a0> 
p[["name"]]  # "demo"
#> [1] "demo"
p[["view"]]  # initially NULL
#> NULL

# Inner pipeline environment (for advanced usage)
ls(p[["pipenv"]])                # shows "data"
#> [1] "data"
ls(p[["pipenv"]], all = TRUE)    # also shows hidden variables
#> [1] ".dag"            ".last_run"       ".restart_count"  ".restart_force" 
#> [5] ".run_state"      ".steps_to_nodes" "data"           
```
