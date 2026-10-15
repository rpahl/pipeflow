# Create a pipeline

Creates a new, empty pipeline. Add steps with
[`pip_add()`](https://rpahl.github.io/pipeflow/reference/pip_add.md) and
execute them with
[`pip_run()`](https://rpahl.github.io/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_new(name = "pipe")
```

## Arguments

- name:

  The name of the pipeline used for display and logging.

## Value

A pipeflow pipeline object.

## Saving pipelines

A pipeline can be saved with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and read back with
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) (or serialized in
other ways, e.g. to send it to another R process), with its steps,
parameters, outputs and states. The dependency graph of the steps is
kept outside of R and is not saved, but it is rebuilt from the steps the
first time it is needed. Views that are saved in the same object as
their pipeline, as in `saveRDS(list(p, v), file)`, are read as views of
the read pipeline. A view that is saved on its own is read with its own
copy of the pipeline.

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
#>  $ pipenv:<environment: 0x556c5ba07988> 
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

# Save the pipeline and read it back
file <- tempfile(fileext = ".rds")
saveRDS(p, file)
p2 <- readRDS(file)
pip_run(p2)
#> info [2026-10-15 11:21:05.518 UTC]: Starting run of pipeflow 'demo'
#> info [2026-10-15 11:21:05.518 UTC]: Step 1/3 numbers
#> info [2026-10-15 11:21:05.518 UTC]: Step 2/3 squared
#> info [2026-10-15 11:21:05.520 UTC]: Step 3/3 total
#> info [2026-10-15 11:21:05.520 UTC]: Finished run of pipeflow 'demo'
p2
#> <pipeflow> demo (3 steps)
#> -------------------------
#>       step params depends state            out
#> 1: numbers      n          done      1,2,3,4,5
#> 2: squared      x numbers  done  1, 4, 9,16,25
#> 3:   total      x squared  done             55
#> -------------------------
#> <ready> last run: 2026-10-15 11:21:05
unlink(file)
```
