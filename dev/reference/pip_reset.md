# Reset a pipeline to its initial state

Resets all unlocked steps of a pipeline (or a subset of steps defined by
a view) to state `"new"` and clears their outputs, so a subsequent
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)
re-executes the cleaned steps from scratch. Their downstream steps that
have run are marked `"outdated"`, as they ran with the cleared outputs.
Any pending restart counters are cleared, but parameters, tags, and
locked flags are left unchanged. If all steps of the pipeline are
selected, the run state is set to `"ready"` and the time of the last run
is cleared. A view that covers only some steps leaves both alone, as
they describe the whole pipeline.

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

Locked steps are skipped: their state and output are preserved, also if
they are downstream of a reset step. If all selected steps are locked, a
message is shown and nothing is changed.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 3) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2)

pip_run(p)
#> info [2026-10-26 16:24:37.566 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-26 16:24:37.566 UTC]: Step 1/2 load
#> info [2026-10-26 16:24:37.567 UTC]: Step 2/2 square
#> info [2026-10-26 16:24:37.568 UTC]: Finished in 0.002 s (2 done, 0 skipped, 0 failed)
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
#> steps: 1 done, 1 new
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
#> steps: 2 new
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

# Resetting a step outdates its downstream steps
pip_run(p)
#> info [2026-10-26 16:24:37.578 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-10-26 16:24:37.578 UTC]: Step 1/2 load
#> info [2026-10-26 16:24:37.579 UTC]: Step 2/2 square
#> info [2026-10-26 16:24:37.579 UTC]: Finished in 0.001 s (2 done, 0 skipped, 0 failed)
pip_reset(pip_view(p, step = "load"))
p[["state"]] # "new", "outdated"
#>       load     square 
#>      "new" "outdated" 
```
