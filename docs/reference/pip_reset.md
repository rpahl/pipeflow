# Reset a pipeline to its initial state

Resets all unlocked steps of a pipeline (or a subset of steps defined by
a view) to state `"new"` and clears their outputs, so a subsequent
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
re-executes the cleaned steps from scratch. Any pending restart counters
are cleared, but parameters, tags, and locked flags are left unchanged.

## Usage

``` r
pip_reset(x)
```

## Arguments

- x:

  A pipeflow pip or view. If a view is given, only the steps covered by
  the view are reset.

## Value

The updated pipeline or view, invisibly.

## Details

Locked steps are skipped: their state and output are preserved. If all
selected steps are locked, a warning is issued and nothing is changed.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2)

pip_run(p)
#> info [2026-09-27 15:19:39.196 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 15:19:39.196 UTC]: Step 1/2 load
#> info [2026-09-27 15:19:39.197 UTC]: Step 2/2 square
#> info [2026-09-27 15:19:39.198 UTC]: Finished run of pipeflow 'pipe'
p[["state"]] # "done", "done"
#>   load square 
#> "done" "done" 

# Locked steps keep their state and output when resetting
pip_lock(pip_view(p, step = "square"))
pip_reset(p)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state    out locked
#> 1:   load      n           new [NULL]  FALSE
#> 2: square      x    load  done  1,4,9   TRUE
#> -------------------------
#> <ready> last run: never
p[["state"]] # "new", "done"
#>   load square 
#>  "new" "done" 
p[["out"]]   # NULL, (x^2 result)
#> $load
#> NULL
#> 
#> $square
#> [1] 1 4 9
#> 

pip_unlock(p)
pip_reset(p)
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>      step params depends state
#> 1:   load      n           new
#> 2: square      x    load   new
#> -------------------------
#> <ready> last run: never
p[["state"]] # "new", "new"
#>   load square 
#>  "new"  "new" 
p[["out"]]   # NULL, NULL
#> $load
#> NULL
#> 
#> $square
#> NULL
#> 
```
