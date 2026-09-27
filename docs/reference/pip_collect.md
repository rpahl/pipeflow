# Collect step outputs

Returns the outputs of the steps in a pipeline or view. With
`by = "step"` (the default) the result is a named list of step outputs.
With any other column, the outputs are grouped by the values of that
column (typically `"tags"`).

## Usage

``` r
pip_collect(x, by = "step", as.table = FALSE, simplify = TRUE)

pip_collect_out(x, by = "step", as.table = FALSE, simplify = TRUE)
```

## Arguments

- x:

  A pipeflow pip or view.

- by:

  Single step-table column name to group by.

- as.table:

  If TRUE, return a `data.table` instead of a named list.

- simplify:

  If TRUE (default), if the list of collected outputs contains exactly
  one step per group, the result is flattened by one level, otherwise it
  is returned as a grouped list.

## Value

A named list of outputs

## Details

The result always takes one of two shapes:

- **Flat**: a named list whose elements are the step outputs directly,
  e.g. `list(s1 = 1, s2 = 2)`.

- **Grouped**: a named list whose elements are themselves named lists of
  step outputs, one per group, e.g.
  `list(io = list(s1 = 1, s2 = 2), model = list(s3 = 4))`.

With `simplify = TRUE` (the default) the result is *flat* whenever every
group contains exactly one step, which, for example, is always the case
for `by = "step"` as step names are unique. If any group contains more
than one step (or `simplify = FALSE`) the result is *grouped*. For list
columns such as `tags`, a step with several entries contributes its
output to every corresponding group, and steps without an entry (e.g.
untagged steps) are omitted. You can use
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
to further narrow the selection before collecting.

## Lifecycle

Deprecated

`pip_collect_out()` is a legacy alias for `pip_collect()`. It raises a
deprecation warning and will be removed in a future release.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x, tags = "io") |>
  pip_add("clean", \(x = ~load) x + 1, tags = "io") |>
  pip_add("model", \(x = ~clean) x * 2, tags = "model")
pip_run(p)
#> info [2026-09-27 15:19:37.551 UTC]: Starting run of pipeflow 'pipe'
#> info [2026-09-27 15:19:37.551 UTC]: Step 1/3 load
#> info [2026-09-27 15:19:37.552 UTC]: Step 2/3 clean
#> info [2026-09-27 15:19:37.553 UTC]: Step 3/3 model
#> info [2026-09-27 15:19:37.554 UTC]: Finished run of pipeflow 'pipe'

# By default, a flat named list with one entry per step
pip_collect(p)
#> $load
#> [1] 1
#> 
#> $clean
#> [1] 2
#> 
#> $model
#> [1] 4
#> 

# The same output as a data.table
pip_collect(p, as.table = TRUE)
#>      step    out
#>    <char> <list>
#> 1:   load      1
#> 2:  clean      2
#> 3:  model      4

# Group the outputs by tag ...
pip_collect(p, by = "tags")
#> $io
#> $io$load
#> [1] 1
#> 
#> $io$clean
#> [1] 2
#> 
#> 
#> $model
#> $model$model
#> [1] 4
#> 
#> 

# ... which is equivalent to
list(
  io = pip_view(p, tags = "io") |> pip_collect(),
  model = pip_view(p, tags = "model") |> pip_collect()
)
#> $io
#> $io$load
#> [1] 1
#> 
#> $io$clean
#> [1] 2
#> 
#> 
#> $model
#> $model$model
#> [1] 4
#> 
#> 

# Grouped table output
pip_collect(p, by = "tags", as.table = TRUE)
#>      tags       out
#>    <char>    <list>
#> 1:     io <list[2]>
#> 2:  model <list[1]>

# Keep single-step groups nested
pip_collect(p, simplify = FALSE)
#> $load
#> $load$load
#> [1] 1
#> 
#> 
#> $clean
#> $clean$clean
#> [1] 2
#> 
#> 
#> $model
#> $model$model
#> [1] 4
#> 
#> 

# Collect output from a view
v <- p[step %in% c("clean", "model"), ]
pip_collect(v)
#> $clean
#> [1] 2
#> 
#> $model
#> [1] 4
#> 
pip_collect(v, as.table = TRUE)
#>      step    out
#>    <char> <list>
#> 1:  clean      2
#> 2:  model      4
```
