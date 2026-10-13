# Rename a step

Renames the selected step and updates dependency references in
downstream steps.

## Usage

``` r
pip_rename(x, from, to)
```

## Arguments

- x:

  A pipeflow pip or view

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
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>         step params   depends state
#> 1: load_data      x             new
#> 2:        s2      x load_data   new
#> -------------------------
#> <ready> last run: never

# Trying to rename to an existing step name raises an error:
try(pip_rename(p, "load_data", to = "s2"))  # step 's2' already exists!
#> Error in pip_rename(p, "load_data", to = "s2") : step 's2' already exists

# If a view is passed, the step must be part of the view
v <- pip_view(p, step = c("load_data", "s2"))
pip_rename(v, from = "load_data", to = "input")
p[["step"]]                                 # "input", "s2"
#>   input      s2 
#> "input"    "s2" 
v2 <- pip_view(p, step = "s2")
try(pip_rename(v2, from = "input", to = "data"))
#> Error in pip_rename(v2, from = "input", to = "data") : 
#>   step 'input' is not part of the view
```
