# Remove tags from selected steps

Removes tags from existing tags for all steps in the pipeline unless `p`
is a view, in which case tags are only removed for steps covered by the
view. Locked steps are skipped and not updated.

## Usage

``` r
pip_untag(p, tags = character())
```

## Arguments

- p:

  A pipeflow pip or view.

- tags:

  Character vector of tags to remove for each selected step.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x, tags = c("daily", "core")) |>
  pip_add("fit", \(x = ~load) x + 1, tags = c("daily", "model"))

# Remove "daily" from all steps
pip_untag(p, tags = "daily")
# "load" retains "core"; "fit" retains "model"
p[["pipeline"]][["tags"]]
#> [[1]]
#> [1] "core"
#> 
#> [[2]]
#> [1] "model"
#> 
```
