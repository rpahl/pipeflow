# Lock steps

Locking a step means that both its parameters and its output (given it
has output) are locked such that neither setting new pipeline parameters
nor future pipeline runs can change the current parameter and output
content. To unlock a locked step, use `pipe_unlock_step()`.

## Usage

``` r
pipe_lock_step(pip, step)

pipe_unlock_step(pip, step)
```

## Arguments

- pip:

  `Pipeline` object

- step:

  `string` name of step to lock or unlock

## Value

the `Pipeline` object invisibly

## Lifecycle

Deprecated. Legacy API. Use
[`pip_lock()`](https://github.com/rpahl/pipeflow/reference/pip_lock.md)
instead.

Deprecated. Legacy API. Use
[`pip_unlock()`](https://github.com/rpahl/pipeflow/reference/pip_unlock.md)
instead.

## Examples

``` r
# pipe_lock_step
p <- pipe_new("pipe", data = 1)
pipe_add(p, "add1", \(x = 1, data = ~data) x + data)
pipe_add(p, "add2", \(x = 1, data = ~data) x + data)
pipe_run(p)
#> INFO  [2026-06-14 15:39:56.788] Start run of 'pipe' pipeline:
#> INFO  [2026-06-14 15:39:56.789] Step 1/3 data
#> INFO  [2026-06-14 15:39:56.791] Step 2/3 add1
#> INFO  [2026-06-14 15:39:56.793] Step 3/3 add2
#> INFO  [2026-06-14 15:39:56.795] Finished execution of steps.
#> INFO  [2026-06-14 15:39:56.796] Done.
pipe_get_out(p, "add1")
#> [1] 2
pipe_get_out(p, "add2")
#> [1] 2
pipe_lock_step(p, "add1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.

pipe_set_data(p, 3)
pipe_set_params(p, list(x = 3))
#> skipping setting parameters x at locked step 'add1'
pipe_run(p)
#> INFO  [2026-06-14 15:39:56.807] Start run of 'pipe' pipeline:
#> INFO  [2026-06-14 15:39:56.809] Step 1/3 data
#> INFO  [2026-06-14 15:39:56.811] Step 2/3 add1 - skip 'locked' step
#> INFO  [2026-06-14 15:39:56.813] Step 3/3 add2
#> INFO  [2026-06-14 15:39:56.814] Finished execution of steps.
#> INFO  [2026-06-14 15:39:56.815] Done.
pipe_get_out(p, "add1")
#> [1] 2
pipe_get_out(p, "add2")
#> [1] 6

# pipe_unlock_step
pipe_unlock_step(p, "add1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_set_params(p, list(x = 3))
pipe_run(p)
#> INFO  [2026-06-14 15:39:56.832] Start run of 'pipe' pipeline:
#> INFO  [2026-06-14 15:39:56.835] Step 1/3 data - skip 'done' step
#> INFO  [2026-06-14 15:39:56.839] Step 2/3 add1
#> INFO  [2026-06-14 15:39:56.843] Step 3/3 add2
#> INFO  [2026-06-14 15:39:56.846] Finished execution of steps.
#> INFO  [2026-06-14 15:39:56.847] Done.
pipe_get_out(p, "add1")
#> [1] 6
```
