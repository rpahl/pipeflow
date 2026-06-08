# Lock selected steps against updates

Locks all selected steps in the pipeline unless `p` is a view, in which
case only steps covered by the view are locked.

## Usage

``` r
pip_lock(p)
```

## Arguments

- p:

  A pipeflow pip or view.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 10) x) |>
  pip_add("fit", \(x = ~load) x * 2)
pip_run(p, lgr = NULL)

# Lock only "load" via a view so it won't be re-executed or overwritten
pip_lock(pip_view(p, i = "load"))
p[["pipeline"]][["locked"]] # TRUE, FALSE
#> [1]  TRUE FALSE

# Locked steps are silently skipped during pip_run()
pip_run(p, lgr = NULL, force = TRUE)
p[["pipeline"]][["out"]][[1]] # still 10 — locked, not re-executed
#> [1] 10

pip_unlock(p)
p[["pipeline"]][["locked"]] # FALSE, FALSE
#> [1] FALSE FALSE
```
