# Create a pipeline view

Creates a filtered view showing only a selected subset of steps. A view
references the underlying pipeline without copying it, so operations
like
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md)
and
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
applied to a view work directly on the underlying pipeline, but are
restricted to the the steps defined by the view.

## Usage

``` r
pip_view(x, ..., join = c("intersect", "union"), fixed = TRUE)
```

## Arguments

- x:

  A pipeflow pipeline or view.

- ...:

  Named filters, which can be one or more of `step`, `params`, `state`,
  `tags`, `exec`, and `depends`. Each filter value is a character vector
  of values to keep, or - if `fixed` is `FALSE` - a regular expression.
  The `params` filter matches against the actual parameter names of each
  step.

- join:

  How individual filters are combined:

  - `"intersect"` (the default) keeps steps that match *all* filters,

  - `"union"` keeps steps that match *any* filter. Within a single
    filter, multiple values are always treated as alternatives (OR).

- fixed:

  If TRUE, values in `...` are treated as fixed strings, otherwise they
  are treated as regular expressions.

## Value

A `pipeflow_view` object.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(a = 1) a, tags = c("io", "core", "daily")) |>
  pip_add("fit", \(b = 2) b + 1, tags = c("model")) |>
  pip_add("eval_fit", \(fit = ~fit) fit,
    tags = c("model", "daily", "report")
  )
p
#> <pipeflow> pipe (3 steps)
#> -------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2:      fit      b           new              model
#> 3: eval_fit    fit     fit   new model,daily,report
#> -------------------------
#> <ready> last run: never

# Filter by one or more column values
pip_view(p, state = "new")
#> <pipeflow_view> pipe view (3 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2:      fit      b           new              model
#> 3: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never
pip_view(p, step = c("load", "fit"))
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>    step params depends state          tags
#> 1: load      a           new io,core,daily
#> 2:  fit      b           new         model
#> ----------------------------------------
#> <ready> last run: never

# Filter by tag — keeps steps that have *any* of the given tags
pip_view(p, tags = "daily")
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never

# Combine filters: step pattern AND state (logical AND)
pip_view(p, step = "fit", state = "new")
#> <pipeflow_view> pipe view (1 of 3 steps)
#> ----------------------------------------
#>    step params depends state  tags
#> 1:  fit      b           new model
#> ----------------------------------------
#> <ready> last run: never

# Combine filters as a union (step OR state)
pip_view(p, step = "load", tags = "report", join = "union")
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never

# Use a regex pattern
pip_view(p, step = "fit$", fixed = FALSE)
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:      fit      b           new              model
#> 2: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never

# Filter by parameter names — steps with any of the given parameters
pip_view(p, params = c("a", "fit"))
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never

# Views are composable: create a view-of-view for progressive narrowing
v1 <- pip_view(p, tags = "daily")
print(v1) # load, eval_fit
#> <pipeflow_view> pipe view (2 of 3 steps)
#> ----------------------------------------
#>        step params depends state               tags
#> 1:     load      a           new      io,core,daily
#> 2: eval_fit    fit     fit   new model,daily,report
#> ----------------------------------------
#> <ready> last run: never
v2 <- pip_view(v1, tags = "report")
print(v2) # eval_fit only
#> <pipeflow_view> pipe view view (1 of 3 steps)
#> ---------------------------------------------
#>        step params depends state               tags
#> 1: eval_fit    fit     fit   new model,daily,report
#> ---------------------------------------------
#> <ready> last run: never
```
