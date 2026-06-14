# Create a pipeline view

Creates a filtered view showing only a selected subset of steps. A view
references the underlying pipeline without copying it, so operations
like
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
and
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
applied to a view affect only the selected steps.

## Usage

``` r
pip_view(
  x,
  i = integer(),
  filter = list(),
  tags = character(),
  fixed = TRUE,
  ...
)
```

## Arguments

- x:

  A pipeflow pipeline or view.

- i:

  Optional row indices or step names to keep.

- filter:

  A named list of filters to apply. Each element can be a character
  vector specifying the values to keep for the corresponding property
  or, if `fixed` is FALSE, a regular expression. See examples for usage.

- tags:

  Tag filter (character). Keeps steps with any matching tag.

- fixed:

  If TRUE, values in `filter` are treated as fixed strings, otherwise
  they are treated as regular expressions.

- ...:

  further args passed to `grepl` (only in effect when `fixed` is
  `FALSE`).

## Value

A `pipeflow_view` object.

## Examples

``` r

p <- pip_new()
pip_add(p, "load_raw", \(x = 1) x,
  tags = c("io", "core", "daily")
)
pip_add(p, "fit_model", \(x = 2) x + 1,
  tags = c("model")
)
pip_add(p, "eval_model", \(x = ~fit_model) x,
  tags = c("model", "daily", "report")
)

# Filter by a fixed column value (one or more states)
pip_view(p, filter = list(state = "new"))
#> <pipeflow_view> pipe view (3 of 3 steps)
#> ----------------------------------------
#>        step   depends    out state               tags
#>    load_raw           [NULL]   new      io,core,daily
#>   fit_model           [NULL]   new              model
#>  eval_model fit_model [NULL]   new model,daily,report

# Combine filters: step pattern AND state
pip_view(p, filter = list(step = "model", state = "new"))
#> <pipeflow_view> pipe view (0 of 3 steps)
#> ----------------------------------------

# Filter by tag — keeps steps that have *any* of the given tags
pip_view(p, tags = "daily")
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step   depends    out state               tags
#>    load_raw           [NULL]   new      io,core,daily
#>  eval_model fit_model [NULL]   new model,daily,report

# Combine explicit step selection with a filter (intersection)
pip_view(p,
  i      = c("load_raw", "fit_model"),
  filter = list(state = "new")
)
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>       step depends    out state          tags
#>   load_raw         [NULL]   new io,core,daily
#>  fit_model         [NULL]   new         model

# Select by integer row indices
pip_view(p, i = c(1L, 2L), filter = list(state = "new"))
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>       step depends    out state          tags
#>   load_raw         [NULL]   new io,core,daily
#>  fit_model         [NULL]   new         model

# Use a regex pattern to match step names
pip_view(p, filter = list(step = "_model$"), fixed = FALSE)
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step   depends    out state               tags
#>   fit_model           [NULL]   new              model
#>  eval_model fit_model [NULL]   new model,daily,report

# Views are composable: create a view-of-view for progressive narrowing
v1 <- pip_view(p, tags = "daily")
print(v1) # load_raw, eval_model
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step   depends    out state               tags
#>    load_raw           [NULL]   new      io,core,daily
#>  eval_model fit_model [NULL]   new model,daily,report
v2 <- pip_view(v1, tags = "report")
print(v2) # eval_model only
#> <pipeflow_view> pipe view view (1 of 3 steps)
#> ---------------------------------------------
#>        step   depends    out state               tags
#>  eval_model fit_model [NULL]   new model,daily,report
```
