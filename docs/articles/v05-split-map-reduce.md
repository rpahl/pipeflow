# Split, map, and reduce

### Motivation

A common scenario is to split a data set into subsets and then apply the
same analysis to each part. In context of pipelines, this means that we
would like to apply the same pipeline multiple times to each data
subset. In addition, we may then want to combine parts of the individual
output. As we will see, {pipeflow} provides a built-in function to
handle this scenario.

### Define pipeline

Let’s first define our pipeline, which, to keep matters simple, just
fits a linear model and outputs the model coefficients.

``` r

library(pipeflow)

pip <- pip_new("my-pipeline") |>
    pip_add(
        "data",
        function(data = NULL) data
    ) |>
    pip_add(
        "fit",
        function(
            data = ~data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add(
        "coefs",
        function(fit = ~fit) {
            coefficients(fit)
        }
    )
```

So our pipeline looks like this:

``` r

pip
# <pipeflow_pip> my-pipeline (3 steps)
# ------------------------------------
#     step depends    out state
# 1:  data         [NULL]   new
# 2:   fit    data [NULL]   new
# 3: coefs     fit [NULL]   new
```

Or graphically:

We use the `iris` data set as our working example.

``` r

head(iris)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
```

First, we apply the pipeline to the whole data set.

``` r

pip |> pip_set_params(list(
    data = iris,
    xVar = "Sepal.Length",
    yVar = "Sepal.Width"
))

pip_run(pip)
# info [2026-06-13 17:23:30.778 UTC]: Start run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.778 UTC]: Step 1/3 data
# info [2026-06-13 17:23:30.780 UTC]: Step 2/3 fit
# info [2026-06-13 17:23:30.784 UTC]: Step 3/3 coefs
# info [2026-06-13 17:23:30.785 UTC]: Finished run of pipeflow_pip 'my-pipeline'
```

``` r

pip[["coefs", "out"]]
#  (Intercept) Sepal.Length 
#    3.4189468   -0.0618848
```

### Split data

Next, we want to apply the pipeline to each species separately. One way
to do this would be to use R’s `split` function. We can split it by the
`Species` column and then run the pipeline for each subset. For example:

``` r

run_pipeline_helper <- function(data) {
    pip |> pip_set_params(list(data = data))
    pip_run(pip)
    pip[["coefs", "out"]]
}

results <- lapply(split(iris, iris$Species), FUN = run_pipeline_helper)
# info [2026-06-13 17:23:30.904 UTC]: Start run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.905 UTC]: Step 1/3 data
# info [2026-06-13 17:23:30.906 UTC]: Step 2/3 fit
# info [2026-06-13 17:23:30.909 UTC]: Step 3/3 coefs
# info [2026-06-13 17:23:30.911 UTC]: Finished run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.915 UTC]: Start run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.915 UTC]: Step 1/3 data
# info [2026-06-13 17:23:30.915 UTC]: Step 2/3 fit
# info [2026-06-13 17:23:30.917 UTC]: Step 3/3 coefs
# info [2026-06-13 17:23:30.919 UTC]: Finished run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.922 UTC]: Start run of pipeflow_pip 'my-pipeline'
# info [2026-06-13 17:23:30.922 UTC]: Step 1/3 data
# info [2026-06-13 17:23:30.923 UTC]: Step 2/3 fit
# info [2026-06-13 17:23:30.925 UTC]: Step 3/3 coefs
# info [2026-06-13 17:23:30.926 UTC]: Finished run of pipeflow_pip 'my-pipeline'
```

``` r

results
# $setosa
#  (Intercept) Sepal.Length 
#   -0.5694327    0.7985283 
# 
# $versicolor
#  (Intercept) Sepal.Length 
#    0.8721460    0.3197193 
# 
# $virginica
#  (Intercept) Sepal.Length 
#    1.4463054    0.2318905
```

Unfortunately, with this approach we had to create additional code that
had to be run outside the pipeline framework. In addition, the run log
quickly can become redundant and confusing, as it now contains multiple
runs of the same pipeline. Since splitting data sets (or more generally
mapping function calls to different subsets of data) is such a common
scenario, {pipeflow} also provides a built-in mechanism to handle this
case.

Since version 0.4.0, for each step, it is possible to set the so-called
execution mode, which by default is `exec = "auto"`. To model the above
scenario, we add a new step to our pipeline that splits the data set and
set its execution mode to `split`.

``` r

pip <- pip_new("my-split-pip") |>
    pip_add(
        "data",
        function(data = NULL) data
    ) |>
    pip_add(
        "split_data",
        function(
            data = ~data,
            byVar = "by"
        ) {
            split(data, f = data[[byVar]])
        },
        exec = "split" # <-- set execution mode to "split"
    ) |>
    pip_add(
        "fit",
        function(
            data = ~split_data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add(
        "coefs",
        function(fit = ~fit) {
            coefficients(fit)
        }
    )
```

``` r

pip
# <pipeflow_pip> my-split-pip (4 steps)
# -------------------------------------
#          step    depends    out state  exec
# 1:       data            [NULL]   new  auto
# 2: split_data       data [NULL]   new split
# 3:        fit split_data [NULL]   new  auto
# 4:      coefs        fit [NULL]   new  auto
```

First of all, we see that the pipeline now is printed with an additional
column `exec` marking the `split` execution mode for the `split_data`
step. This also can be inspected in the graph:

``` r

library(visNetwork)
do.call(visNetwork, args = pip_get_graph(pip))
```

Now what does this execution mode actually do? It basically tells the
pipeline that for all steps that depend on the `split_data` step
(directly or indirectly), the results coming from the split step should
be treated as lists of results, which should be iterated over.

In our particular example, this means that the `fit` step will be
executed for each data subset coming from the `split_data` step and
likewise the `coefs` step will be executed for each fitted model coming
from the `fit` step.

Let’s see this in action by running the pipeline.

``` r

pip |> pip_set_params(list(
    data = iris,
    xVar = "Sepal.Length",
    yVar = "Sepal.Width",
    byVar = "Species"
))

pip_run(pip)
# info [2026-06-13 17:23:31.323 UTC]: Start run of pipeflow_pip 'my-split-pip'
# info [2026-06-13 17:23:31.323 UTC]: Step 1/4 data
# info [2026-06-13 17:23:31.324 UTC]: Step 2/4 split_data
# info [2026-06-13 17:23:31.325 UTC]: Step 3/4 fit
# info [2026-06-13 17:23:31.329 UTC]: Step 4/4 coefs
# info [2026-06-13 17:23:31.330 UTC]: Finished run of pipeflow_pip 'my-split-pip'
```

Looking at the pipeline overview, we see that the `out`puts following
the `split_data` steps are now all lists of results.

``` r

pip
# <pipeflow_pip> my-split-pip (4 steps)
# -------------------------------------
#          step    depends                 out state  exec
# 1:       data            <data.frame[150x5]>  done  auto
# 2: split_data       data           <list[3]>  done split
# 3:        fit split_data           <list[3]>  done  auto
# 4:      coefs        fit           <list[3]>  done  auto
```

Inspecting in particular the output of the `coefs` step, we see that it
is now a list of coefficient tables, one for each species.

``` r

pip[["coefs", "out"]]
# $setosa
#  (Intercept) Sepal.Length 
#   -0.5694327    0.7985283 
# 
# $versicolor
#  (Intercept) Sepal.Length 
#    0.8721460    0.3197193 
# 
# $virginica
#  (Intercept) Sepal.Length 
#    1.4463054    0.2318905 
# 
# attr(,"class")
# [1] "list"                 "pipeflow_partitioned"
```

This matches the output[^1] we obtained earlier with the helper function
but was obtained without the need having to write all this extra code
around the pipeline.

### Recombine output

While the above approach looks nice already, we are only half way there,
because often we will want to recombine the output of all the different
subsets in some way. For example, we may want to show the resulting
coefficients of the linear models in one summary table.

This is where the `reduce` execution mode comes into play. Let’s for
this matter extend our pipeline by one step at the end.

``` r

pip |> pip_add(
    "combine_coefs",
    function(coefs = ~coefs) {
        do.call(rbind, coefs)
    },
    exec = "reduce" # <-- set execution mode to "reduce"
)
```

``` r

pip
# <pipeflow_pip> my-split-pip (5 steps)
# -------------------------------------
#             step    depends                 out state   exec
# 1:          data            <data.frame[150x5]>  done   auto
# 2:    split_data       data           <list[3]>  done  split
# 3:           fit split_data           <list[3]>  done   auto
# 4:         coefs        fit           <list[3]>  done   auto
# 5: combine_coefs      coefs              [NULL]   new reduce
```

Again, we see that the new step is marked with the execution mode
(`reduce`) in the overview. Graphically, this mode is represented by a
circle.

``` r

do.call(visNetwork, args = pip_get_graph(pip))
```

If we now run the pipeline, we see that the output of the
`combine_coefs` step is a combined table of coefficients.

``` r

pip_run(pip)
# info [2026-06-13 17:23:31.723 UTC]: Start run of pipeflow_pip 'my-split-pip'
# info [2026-06-13 17:23:31.723 UTC]: Step 1/5 data - skipping done step
# info [2026-06-13 17:23:31.723 UTC]: Step 2/5 split_data - skipping done step
# info [2026-06-13 17:23:31.723 UTC]: Step 3/5 fit - skipping done step
# info [2026-06-13 17:23:31.723 UTC]: Step 4/5 coefs - skipping done step
# info [2026-06-13 17:23:31.724 UTC]: Step 5/5 combine_coefs
# info [2026-06-13 17:23:31.725 UTC]: Finished run of pipeflow_pip 'my-split-pip'

pip[["combine_coefs", "out"]]
#            (Intercept) Sepal.Length
# setosa      -0.5694327    0.7985283
# versicolor   0.8721460    0.3197193
# virginica    1.4463054    0.2318905
```

There you go :-)

[^1]: Technically, the output is slightly different, because the
    returned list has an additional class attribute
    “pipeflow_partitioned”.
