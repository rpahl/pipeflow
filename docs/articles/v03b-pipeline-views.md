# Pipeline views

Pipelines can get long, and often you want to focus on a subset of
steps: a topic, a stage, or the steps that produce the outputs you care
about. *Views* are {pipeflow}’s way of working on a subset of steps
without copying anything. A view references the underlying pipeline, so
every operation applied to a view (running it, updating parameters,
tagging, locking, …) writes through to the original pipeline, restricted
to the steps covered by the view.

This vignette shows how to create and combine views, how to select steps
with the `[` operator, and how to run only part of a pipeline.

## Setup

Again, we use a very simplified example of a pipeline.

``` r
library(pipeflow)

pip <- pip_new("my-pip") |>
    pip_add("load", \(n = 5) seq_len(n), tags = c("io", "daily")) |>
    pip_add("clean", \(x = ~load) x * 2, tags = c("io", "core")) |>
    pip_add("fit", \(x = ~clean) sum(x), tags = c("model", "core")) |>
    pip_add("report", \(x = ~fit) paste("result:", x), tags = "report")
```

As you see above, besides `step` and `fun`,
[`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
also allows to set tags. We will use these tags as meta information to
filter certain steps by topic and/or output type. Let’s do a first run
before we move on.

``` r
(pip_run(pip, lgr = NULL))
# <pipeflow> my-pip (4 steps)
# ---------------------------
#      step params depends state            out       tags
# 1:   load      n          done      1,2,3,4,5   io,daily
# 2:  clean      x    load  done  2, 4, 6, 8,10    io,core
# 3:    fit      x   clean  done             30 model,core
# 4: report      x     fit  done     result: 30     report
# ---------------------------
# <ready> last run: 2026-09-27 17:19:55
```

## Creating views

[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
returns a *view* of the pipeline that contains only the steps matching
the given filters:

``` r
pip_view(pip, tags = "core")
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1: clean      x    load  done  2, 4, 6, 8,10    io,core
# 2:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

Filters can be combined. By default, steps must match *all* filters
(logical AND), while the values within a single filter are treated as
alternatives (OR):

``` r
pip_view(pip, tags = "core", state = "done")
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1: clean      x    load  done  2, 4, 6, 8,10    io,core
# 2:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55

pip_view(pip, step = c("clean", "fit"))
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1: clean      x    load  done  2, 4, 6, 8,10    io,core
# 2:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

`join = "union"` keeps steps that match *any* filter:

``` r
pip_view(pip, tags = "report", step = "clean", join = "union")
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#      step params depends state            out    tags
# 1:  clean      x    load  done  2, 4, 6, 8,10 io,core
# 2: report      x     fit  done     result: 30  report
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

With `fixed = FALSE`, filter values are interpreted as regular
expressions:

``` r
pip_view(pip, step = "^f", fixed = FALSE)
# <pipeflow_view> my-pip view (1 of 4 steps)
# ------------------------------------------
#    step params depends state out       tags
# 1:  fit      x   clean  done  30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

The available filters are `step`, `params`, `state`, `exec`, `tags`, and
`depends`. For example, to find all steps that depend on `load` and are
still `new`:

``` r
pip_reset(pip) # reset to initial state

pip_view(pip, depends = "load", state = "new")
# <pipeflow_view> my-pip view (1 of 4 steps)
# ------------------------------------------
#     step params depends state    tags
# 1: clean      x    load   new io,core
# ------------------------------------------
# <ready> last run: never
```

## Selecting steps with `[`

The extract operator `[` provides a data.table-like way of selecting
steps. It returns a view by default:

``` r
pip_run(pip, lgr = NULL)
pip[c("load", "fit")]
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#    step params depends state       out       tags
# 1: load      n          done 1,2,3,4,5   io,daily
# 2:  fit      x   clean  done        30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55

pip[1:3]
# <pipeflow_view> my-pip view (3 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1:  load      n          done      1,2,3,4,5   io,daily
# 2: clean      x    load  done  2, 4, 6, 8,10    io,core
# 3:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

Boolean filters are evaluated in the context of the step table, so the
same columns as in
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
are available as variables:

``` r
pip[tags %like% "core"]
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1: clean      x    load  done  2, 4, 6, 8,10    io,core
# 2:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55

pip[step %in% c("clean", "fit") & state == "done"]
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out       tags
# 1: clean      x    load  done  2, 4, 6, 8,10    io,core
# 2:   fit      x   clean  done             30 model,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

Negative indices select all steps except the excluded ones:

``` r
pip[-2]
# <pipeflow_view> my-pip view (3 of 4 steps)
# ------------------------------------------
#      step params depends state        out       tags
# 1:   load      n          done  1,2,3,4,5   io,daily
# 2:    fit      x   clean  done         30 model,core
# 3: report      x     fit  done result: 30     report
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

`p[]` returns a copy of the pipeline, and using two indices (`p[i, j]`)
extracts a the given rows and columns as a `data.table`:

``` r
pip2 <- pip[]

pip[, c("step", "tags")]
#      step       tags
#    <char>     <list>
# 1:   load   io,daily
# 2:  clean    io,core
# 3:    fit model,core
# 4: report     report

pip[c("load", "fit"), "out"]
#          out
#       <list>
# 1: 1,2,3,4,5
# 2:        30
```

While `[` returns a view by default, `view = FALSE` builds a new,
self-contained pipeline containing the selected steps together with all
their upstream dependencies:

``` r
pip[c("fit", "report"), view = FALSE]
# pulled in 2 upstream dependencies
# <pipeflow> my-pip (4 steps)
# ---------------------------
#      step params depends state            out       tags
# 1:   load      n          done      1,2,3,4,5   io,daily
# 2:  clean      x    load  done  2, 4, 6, 8,10    io,core
# 3:    fit      x   clean  done             30 model,core
# 4: report      x     fit  done     result: 30     report
# ---------------------------
# <ready> last run: never
```

The printed message tells you how many steps were pulled in as upstream
dependencies.

## Composing views

Views can be nested: applying
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
(or `[`) to a view narrows the view further.

``` r
v1 <- pip_view(pip, tags = "io") # load, clean
v1
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out     tags
# 1:  load      n          done      1,2,3,4,5 io,daily
# 2: clean      x    load  done  2, 4, 6, 8,10  io,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55

v2 <- v1 |> pip_view(tags = "core") # clean only
v2
# <pipeflow_view> my-pip view view (1 of 4 steps)
# -----------------------------------------------
#     step params depends state            out    tags
# 1: clean      x    load  done  2, 4, 6, 8,10 io,core
# -----------------------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

## The `view` meta field

Under the hood, a view is defined by a vector of row indices covered by
the view (`NULL` for a “no view”). This vector is stored in the `view`
meta field of the pipeline object, so in principle you can also
manipulate the view directly by assigning to the meta field[¹](#fn1).

``` r
pip[["view"]] # NULL — not a view
# NULL

w <- pip
w[["view"]] <- c("load", "clean")
w
# <pipeflow_view> my-pip view (2 of 4 steps)
# ------------------------------------------
#     step params depends state            out     tags
# 1:  load      n          done      1,2,3,4,5 io,daily
# 2: clean      x    load  done  2, 4, 6, 8,10  io,core
# ------------------------------------------
# <ready> last run: 2026-09-27 17:19:55

w[["view"]] <- NULL # back to the full pipeline
w
# <pipeflow> my-pip view (4 steps)
# --------------------------------
#      step params depends state            out       tags
# 1:   load      n          done      1,2,3,4,5   io,daily
# 2:  clean      x    load  done  2, 4, 6, 8,10    io,core
# 3:    fit      x   clean  done             30 model,core
# 4: report      x     fit  done     result: 30     report
# --------------------------------
# <ready> last run: 2026-09-27 17:19:55
```

## Running views

Running a view executes the covered steps together with any upstream
dependencies that are not up to date. The run log marks steps that
belong to the view as `[view]` and steps that were pulled in as
dependencies as `[upstream]`:

``` r
pip_reset(pip)
pip_run(pip_view(pip, step = "report"))
# info [2026-09-27 15:19:56.187 UTC]: Starting run of pipeflow 'my-pip view'
# info [2026-09-27 15:19:56.187 UTC]: Step 1/4 [upstream] load
# info [2026-09-27 15:19:56.187 UTC]: Step 2/4 [upstream] clean
# info [2026-09-27 15:19:56.188 UTC]: Step 3/4 [upstream] fit
# info [2026-09-27 15:19:56.189 UTC]: Step 4/4 [view] report
# info [2026-09-27 15:19:56.192 UTC]: Finished run of pipeflow 'my-pip view'
```

Afterwards, the original pipeline is up to date for the covered steps.

Pipeline views are not only useful to inspect or run certain parts of
the pipeline but also to filter and collect the final output of your
analysis run. For more details on this see the next vignette [Collect
and group
output](https://github.com/rpahl/pipeflow/articles/v04-collect-output.md).

------------------------------------------------------------------------

1.  Direct manipulation of the view usually is not needed and probably
    mostly useful for debugging.
