# Clone a pipeline

Creates an independent copy of the pipeline. Changes to the cloned
pipeline do not affect the original pipeline, and vice versa.

## Usage

``` r
pip_clone(x, name = NULL, out = NULL)
```

## Arguments

- x:

  A pipeflow pipeline object.

- name:

  Optional name for the cloned pipeline. If `NULL`, the original name is
  used.

- out:

  `NULL` (all outputs) or a character vector of the steps whose outputs
  the copy keeps (`character(0)`: none), see "Copies without some
  outputs".

## Value

A cloned pipeflow pipeline object.

## Copies without some outputs

With `out`, the copy keeps the outputs of the named steps only, e.g. to
send the results of a run back from another R process (a parallel
worker) without the data of the steps in between. Everything else is
copied: the steps, their params, times and failures, and the states
except for these changes:

- A step that was `"done"` and loses its output becomes `"outdated"`, so
  the copy never passes on a missing output: running it runs these steps
  again, and with them the steps after them, also those that kept their
  output.

- Failed steps keep their output (the failure, or for a mapped step the
  outputs and failures of its keys), which tells why the steps after
  them are not done.

- Locked steps keep their output, as a run passes it on.

The copy still holds the step functions, with their environments: a
function defined inside another function keeps that function's frame and
the data in it (see "Outputs that keep an environment" in
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)).

## Examples

``` r
p <- pip_new("original") |>
  pip_add("s1", \(x = 1) x) |>
  pip_add("s2", \(x = ~s1) x + 1)

# Clone produces a fully independent copy
cp <- pip_clone(p, name = "copy")
pip_add(cp, "s3", \(x = ~s2) x * 10)

# As a result, the clone has the new step ...
cp
#> <pipeflow> copy (3 steps)
#> -------------------------
#>    step params depends state
#> 1:   s1      x           new
#> 2:   s2      x      s1   new
#> 3:   s3      x      s2   new
#> -------------------------
#> <ready> last run: never
#> steps: 3 new

# ... while the original is left unchanged
p
#> <pipeflow> original (2 steps)
#> -----------------------------
#>    step params depends state
#> 1:   s1      x           new
#> 2:   s2      x      s1   new
#> -----------------------------
#> <ready> last run: never
#> steps: 2 new

# A copy with the output of the last step only, e.g. to return from a worker
p <- pip_new("worker") |>
  pip_add("data", \(n = 1e5) rnorm(n)) |>
  pip_add("summary", \(x = ~data) summary(x))
pip_run(p)
#> info [2026-11-15 20:37:43.229 UTC]: Starting run of pipeflow 'worker'
#> info [2026-11-15 20:37:43.229 UTC]: Step 1/2 data
#> info [2026-11-15 20:37:43.234 UTC]: Step 2/2 summary
#> info [2026-11-15 20:37:43.239 UTC]: Finished in 0.010 s (2 done, 0 skipped, 0 failed)
result <- pip_clone(p, out = "summary")
result[["summary", "out"]]
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -4.702157 -0.676390 -0.005350 -0.003002  0.672128  4.110795 
is.null(result[["data", "out"]])
#> [1] TRUE

# The step without output is outdated, so a run computes it again
result
#> <pipeflow> worker (2 steps)
#> ---------------------------
#>       step params depends    state
#> 1:    data      n         outdated
#> 2: summary      x    data     done
#>                                                                              out
#> 1:                                                                        [NULL]
#> 2: -4.702156920,-0.676390385,-0.005349902,-0.003001808, 0.672128248, 4.110795227
#> ---------------------------
#> <ready> last run: never
#> steps: 1 done, 1 outdated
```
