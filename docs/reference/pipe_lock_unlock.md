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
#> INFO  [2026-06-13 19:22:14.924] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:14.926] Step 1/3 data
#> INFO  [2026-06-13 19:22:14.929] Step 2/3 add1
#> INFO  [2026-06-13 19:22:14.931] Step 3/3 add2
#> INFO  [2026-06-13 19:22:14.933] Finished execution of steps.
#> INFO  [2026-06-13 19:22:14.934] Done.
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
#> INFO  [2026-06-13 19:22:14.943] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:14.945] Step 1/3 data
#> INFO  [2026-06-13 19:22:14.948] Step 2/3 add1 - skip 'locked' step
#> INFO  [2026-06-13 19:22:14.949] Step 3/3 add2
#> INFO  [2026-06-13 19:22:14.950] Finished execution of steps.
#> INFO  [2026-06-13 19:22:14.951] Done.
pipe_get_out(p, "add1")
#> [1] 2
pipe_get_out(p, "add2")
#> [1] 6

# pipe_unlock_step
pipe_unlock_step(p, "add1")
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
pipe_set_params(p, list(x = 3))
pipe_run(p)
#> INFO  [2026-06-13 19:22:14.961] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:14.963] Step 1/3 data - skip 'done' step
#> INFO  [2026-06-13 19:22:14.964] Step 2/3 add1
#> INFO  [2026-06-13 19:22:14.966] Step 3/3 add2
#> INFO  [2026-06-13 19:22:14.967] Finished execution of steps.
#> INFO  [2026-06-13 19:22:14.967] Done.
pipe_get_out(p, "add1")
#> [1] 6
```
