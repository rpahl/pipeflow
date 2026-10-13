# Remove a step

Removes a pipeline step by its name. If other steps depend on it, an
error is given and the removal of the step is blocked, unless `force`
was set to `TRUE`, which will remove the selected step together with all
its downstream dependent steps.

## Usage

``` r
pip_remove(x, step, force = FALSE)
```

## Arguments

- x:

  A pipeflow pip or view

- step:

  `string` the name of the step to be removed.

- force:

  `logical` if `TRUE` the step is removed together with all its
  downstream dependencies.

## Value

The updated pipeline or view, invisibly.

## Note

If called on a view, the step to be removed must be part of the view.
The view, as well as any other view on the same pipeline, stays valid:
the removed steps just drop out of it.

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
#> -------------------------
#>         step params depends state
#> 1:      load      x           new
#> 2: transform      x    load   new
#> -------------------------
#> <ready> last run: never

# Trying to remove a step that others depend on raises an error:
# pip_remove(p, "load")  # Error!

# If a view is passed, the step must be part of the view.
v <- pip_view(p, step = "transform")
try(pip_remove(v, "load"))  # Error: "load" is not part of the view
#> Error in pip_remove(v, "load") : step 'load' is not part of the view

# Views stay valid, the removed steps just drop out of them
v <- pip_view(p, step = c("load", "transform"))
pip_remove(v, "transform")
v[["step"]]              # "load"
#>   load 
#> "load" 
p                        # "load"
#> <pipeflow> pipe (1 step)
#> ------------------------
#>    step params depends state
#> 1: load      x           new
#> ------------------------
#> <ready> last run: never

# force = TRUE removes the step and all its downstream dependents
pip_remove(p, "load", force = TRUE)
p                        # pipeline is now empty
#> <pipeflow> pipe (0 steps)
#> -------------------------
#> Empty pipeline
#> -------------------------
#> <ready> last run: never
```
