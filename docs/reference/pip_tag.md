# Add tags to selected steps

Adds tags to existing tags for all steps in the pipeline unless `p` is a
view, in which case tags are only added for steps covered by the view.
Locked steps are skipped and not updated.

## Usage

``` r
pip_tag(p, tags = character())
```

## Arguments

- p:

  A pipeflow pip or view.

- tags:

  Character vector of tags to add for each selected step.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)

# Tag every step in the pipeline at once
pip_tag(p, tags = c("daily", "core"))
p[["pipeline"]][["tags"]] # both steps have c("daily", "core")
#> [[1]]
#> [1] "daily" "core" 
#> 
#> [[2]]
#> [1] "daily" "core" 
#> 

# Add an extra tag to only one step via a view
v <- pip_view(p, i = "fit")
pip_tag(v, tags = "model")
p[["pipeline"]][["tags"]] # "fit" also has "model"
#> [[1]]
#> [1] "daily" "core" 
#> 
#> [[2]]
#> [1] "daily" "core"  "model"
#> 
```
