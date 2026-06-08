# Rename a step

Renames the selected step and updates dependency references in
downstream steps.

## Usage

``` r
pip_rename(x, from, to)
```

## Arguments

- x:

  A pipeflow pip

- from:

  Existing step name

- to:

  New step name

## Value

The updated pipeline, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("s1", \(x = 1) x) |>
  pip_add("s2", \(x = ~s1) x + 1)           # "s2" depends on "s1"

# Downstream dependency references are updated automatically
pip_rename(p, from = "s1", to = "load_data")
p
#> <pipeflow_pip> pipe (2 steps)
#> -----------------------------
#>         step group   depends    out state
#> 1: load_data    s1           [NULL]   new
#> 2:        s2    s2 load_data [NULL]   new

#' # Trying to rename to an existing step name raises an error:
try(pip_rename(p, "load_data", to = "s2"))  # step 's2' already exists!
#> Error in pip_rename(p, "load_data", to = "s2") : step 's2' already exists
```
