# Add or remove tags

Adds tags to, or removes tags from, all steps of a pipeline or a subset
of steps defined by a view. Tagged steps can later be selected via
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md).

## Usage

``` r
pip_tag(x, tags = character())

pip_untag(x, tags = character())
```

## Arguments

- x:

  A pipeflow pip or view.

- tags:

  Character vector of tags to add to or remove from each selected step.

## Value

The updated pipeline or view, invisibly.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)

# Tag every step in the pipeline at once
pip_tag(p, c("daily", "core"))
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>    step params depends state       tags
#> 1: load      x           new daily,core
#> 2:  fit      x    load   new daily,core
#> -------------------------
#> <ready> last run: never
p[, "tags"] # both steps have c("daily", "core")
#>          tags
#>        <list>
#> 1: daily,core
#> 2: daily,core

# Add an extra tag to only one step via a view
p[step == "fit"] |> pip_tag("model")
p
#> <pipeflow> pipe (2 steps)
#> -------------------------
#>    step params depends state             tags
#> 1: load      x           new       daily,core
#> 2:  fit      x    load   new daily,core,model
#> -------------------------
#> <ready> last run: never
p[, "tags"] # "fit" also has "model"
#>                tags
#>              <list>
#> 1:       daily,core
#> 2: daily,core,model

# Remove "daily" from all steps
pip_untag(p, "daily")
p[, "tags"]
#>          tags
#>        <list>
#> 1:       core
#> 2: core,model
```
