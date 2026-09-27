# Lock or unlock steps

Locks or unlocks all steps of a pipeline, or a subset of steps defined
by a view. Locked steps are skipped during
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
and are protected against modification from
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md),
[`pip_tag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md) or
[`pip_untag()`](https://github.com/rpahl/pipeflow/reference/pip_tag.md).
Calling `pip_unlock()` removes the lock again.

## Usage

``` r
pip_lock(x)

pip_unlock(x)
```

## Arguments

- x:

  A pipeflow pip or view.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("x", \(x = 1) x) |>
  pip_add("y", \(y = 2) y) |>
  pip_add("sum", \(x = 1, y = 2) x + y)
(pip_run(p))
#> info [2026-09-27 17:53:07.208 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:07.208 UTC]: Step 1/3 x
#> info [2026-09-27 17:53:07.209 UTC]: Step 2/3 y
#> info [2026-09-27 17:53:07.209 UTC]: Step 3/3 sum
#> info [2026-09-27 17:53:07.209 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>    step params depends state out
#> 1:    x      x          done   1
#> 2:    y      y          done   2
#> 3:  sum    x,y          done   3
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:07
p[["sum", "out"]] # 3
#> [1] 3

# Lock "sum" step via a view so it cannot be overwritten
pip_set_params(p, params = list(x = 10, y = 20))
p[["sum", "params"]] # x = 10, y = 20
#> $x
#> [1] 10
#> 
#> $y
#> [1] 20
#> 
pip_lock(p["sum", ])
(pip_run(p))
#> info [2026-09-27 17:53:07.214 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:07.215 UTC]: Step 1/3 x
#> info [2026-09-27 17:53:07.215 UTC]: Step 2/3 y
#> info [2026-09-27 17:53:07.215 UTC]: Step 3/3 sum - skipping locked step
#> info [2026-09-27 17:53:07.215 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>    step params depends    state out locked
#> 1:    x      x             done  10  FALSE
#> 2:    y      y             done  20  FALSE
#> 3:  sum    x,y         outdated   3   TRUE
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:07
p[["sum", "out"]] # still 3
#> [1] 3

# Note that locking also prevents any parameter updates
pip_set_params(p, params = list(x = 100, y = 200))
p[["x", "params"]] # x = 100
#> $x
#> [1] 100
#> 
p[["y", "params"]] # y = 200
#> $y
#> [1] 200
#> 
p[["sum", "params"]] # still x = 10, y = 20
#> $x
#> [1] 10
#> 
#> $y
#> [1] 20
#> 

# Unlock everything to allow updates again
pip_unlock(p)
pip_set_params(p, params = list(x = 100, y = 200))
(pip_run(p))
#> info [2026-09-27 17:53:07.223 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 17:53:07.223 UTC]: Step 1/3 x
#> info [2026-09-27 17:53:07.224 UTC]: Step 2/3 y
#> info [2026-09-27 17:53:07.224 UTC]: Step 3/3 sum
#> info [2026-09-27 17:53:07.224 UTC]: Finished run of pipeflow 'pipe'
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>    step params depends state out
#> 1:    x      x          done 100
#> 2:    y      y          done 200
#> 3:  sum    x,y          done 300
#> -------------------------
#> <ready> last run: 2026-09-27 19:53:07
```
