# Remove a step

If other steps depend on the step to be removed, an error is given and
the removal is blocked, unless `force` was set to `TRUE`. In force mode,
the selected step and all downstream dependent steps are removed
together.

## Usage

``` r
pip_remove(x, step, force = FALSE)
```

## Arguments

- x:

  A pipeflow pip

- step:

  `string` the name of the step to be removed.

- force:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

## Value

The updated pipeline, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("transform", \(x = ~load) x * 2) |>
  pip_add("model", \(x = ~transform) x + 10)

# Removing a leaf step (nothing depends on it) works directly
pip_remove(p, "model")
p                        # "load", "transform"
#> <pipeflow> pipe (2 steps)
#> -----------------------------
#>         step depends    out state
#> 1:      load         [NULL]   new
#> 2: transform    load [NULL]   new

# Trying to remove a step that others depend on raises an error:
# pip_remove(p, "load")  # Error!

# force = TRUE removes the step and all its downstream dependents
pip_remove(p, "load", force = TRUE)
#> Removing step 'load' and its downstream dependencies: 'transform'
p                        # pipeline is now empty
#> <pipeflow> pipe (0 steps)
#> -----------------------------
#> Empty data.table (0 rows and 4 cols): step,depends,out,state
```
