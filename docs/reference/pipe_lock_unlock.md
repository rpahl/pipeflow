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
#> INFO  [2026-06-15 12:50:32.743] Start run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:32.745] Step 1/3 data
#> INFO  [2026-06-15 12:50:32.749] Step 2/3 add1
#> INFO  [2026-06-15 12:50:32.752] Step 3/3 add2
#> INFO  [2026-06-15 12:50:32.753] Finished execution of steps.
#> INFO  [2026-06-15 12:50:32.754] Done.
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
#> INFO  [2026-06-15 12:50:32.768] Start run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:32.770] Step 1/3 data
#> INFO  [2026-06-15 12:50:32.774] Step 2/3 add1 - skip 'locked' step
#> INFO  [2026-06-15 12:50:32.775] Step 3/3 add2
#> INFO  [2026-06-15 12:50:32.777] Finished execution of steps.
#> INFO  [2026-06-15 12:50:32.778] Done.
pipe_get_out(p, "add1")
#> [1] 2
pipe_get_out(p, "add2")
#> [1] 6

# pipe_unlock_step
pipe_unlock_step(p, "add1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_set_params(p, list(x = 3))
pipe_run(p)
#> INFO  [2026-06-15 12:50:32.788] Start run of 'pipe' pipeline:
#> INFO  [2026-06-15 12:50:32.789] Step 1/3 data - skip 'done' step
#> INFO  [2026-06-15 12:50:32.790] Step 2/3 add1
#> INFO  [2026-06-15 12:50:32.792] Step 3/3 add2
#> INFO  [2026-06-15 12:50:32.794] Finished execution of steps.
#> INFO  [2026-06-15 12:50:32.795] Done.
pipe_get_out(p, "add1")
#> [1] 6
```
