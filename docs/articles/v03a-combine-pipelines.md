# Combine pipelines

The possibility to combine pipelines basically allows to modularize the
pipeline creation process. This is especially useful when you have a set
of pipelines that are used in different contexts and you want to avoid
code duplication[¹](#fn1).

### Two pipelines

Let’s define one pipeline that is used for data preprocessing and one
that does the modelling[²](#fn2).

``` r
library(pipeflow)

pip1 <- pip_new("preprocess") |>
    pip_add("data", \(x = 1:5) x) |>
    pip_add("prep", \(x = ~data) x + 1) |>
    pip_add("standardize", \(x = ~prep, scale = 2) x * scale)
```

``` r
pip2 <- pip_new("model") |>
    pip_add("data", \(x = 1:5) x) |>
    pip_add("fit", \(x = ~data, k = 2, b = 0) x * k + b) |>
    pip_add("predict", \(x = ~fit) paste0("pred: ", x))
```

### Combined pipeline

Next we combine the two pipelines using
[`rbind()`](https://rdrr.io/r/base/cbind.html).

``` r
pip <- rbind(pip1, pip2)

pip
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params depends state
# 1:        data       x           new
# 2:        prep       x    data   new
# 3: standardize x,scale    prep   new
# 4:       data2       x           new
# 5:         fit   x,k,b   data2   new
# 6:     predict       x     fit   new
# -------------------------------------
# <ready> last run: never
```

Note that the `data` step of the second pipeline has been renamed to
`data2` in both the `step` and the `depends` columns (see line 4 above).
That is, when “rbinding” pipelines, {pipeflow} automatically ensures
that all step names stay unique by renaming any duplicates accordingly.

As is also visible from the graphical representation of the pipeline,

``` r
library(visNetwork)
do.call(visNetwork, args = pip_graph(pip)) |>
    visHierarchicalLayout(direction = "LR")
```

the two pipelines are not yet connected. To make sense of the combined
pipeline, we want to use the output of the `standardize` step as the
input of the `data2` step, which we can do by applying the `replace`
function, which was introduced in the previous vignette [modify the
pipeline](https://github.com/rpahl/pipeflow/articles/v02-modify-pipeline.md),
as follows:

``` r
pip |> pip_replace("data2", \(x = ~standardize) x)

pip
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends    state
# 1:        data       x                  new
# 2:        prep       x        data      new
# 3: standardize x,scale        prep      new
# 4:       data2       x standardize      new
# 5:         fit   x,k,b       data2 outdated
# 6:     predict       x         fit outdated
# -------------------------------------
# <ready> last run: never
```

The `data2` step points to the output of the `standardize` step, so that
both pipelines are now connected.

#### Relative indexing

Since the name of the re-routed step might not always be known[³](#fn3),
the {pipeflow} package also provides a relative position indexing
mechanism, which allows to rewrite the above command using a number
(instead of the step name `standardize`) while having the same effect as
above.

``` r
pip |> pip_replace("data2", \(x = ~ -1) x)

pip
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends    state
# 1:        data       x                  new
# 2:        prep       x        data      new
# 3: standardize x,scale        prep      new
# 4:       data2       x standardize      new
# 5:         fit   x,k,b       data2 outdated
# 6:     predict       x         fit outdated
# -------------------------------------
# <ready> last run: never
```

The relative indexing mechanism allows to refer to steps positioned
above the current step. The index `~-1` can be interpreted as “go one
step back”, `~-2` as “go two steps back”, and so on.

### Combined pipeline results

Let’s now run the combined pipeline and inspect the results of the final
step.

``` r
pip_run(pip)
# info [2026-09-27 17:53:23.096 UTC]: Starting run of pipeflow 'preprocess-model'
# info [2026-09-27 17:53:23.096 UTC]: Step 1/6 data
# info [2026-09-27 17:53:23.097 UTC]: Step 2/6 prep
# info [2026-09-27 17:53:23.098 UTC]: Step 3/6 standardize
# info [2026-09-27 17:53:23.099 UTC]: Step 4/6 data2
# info [2026-09-27 17:53:23.100 UTC]: Step 5/6 fit
# info [2026-09-27 17:53:23.101 UTC]: Step 6/6 predict
# info [2026-09-27 17:53:23.102 UTC]: Finished run of pipeflow 'preprocess-model'
```

``` r
pip[["predict", "out"]]
# [1] "pred: 8"  "pred: 12" "pred: 16" "pred: 20" "pred: 24"
```

As we can see, the outputs of the preprocessing pipeline flow into the
modelling pipeline. We can now go ahead and for example change the
multiplier of the `fit` step and rerun the pipeline.

``` r
pip_set_params(pip, params = list(k = 3))
```

``` r
pip_run(pip)
# info [2026-09-27 17:53:23.328 UTC]: Starting run of pipeflow 'preprocess-model'
# info [2026-09-27 17:53:23.328 UTC]: Step 1/6 data - skipping done step
# info [2026-09-27 17:53:23.328 UTC]: Step 2/6 prep - skipping done step
# info [2026-09-27 17:53:23.328 UTC]: Step 3/6 standardize - skipping done step
# info [2026-09-27 17:53:23.328 UTC]: Step 4/6 data2 - skipping done step
# info [2026-09-27 17:53:23.328 UTC]: Step 5/6 fit
# info [2026-09-27 17:53:23.329 UTC]: Step 6/6 predict
# info [2026-09-27 17:53:23.330 UTC]: Finished run of pipeflow 'preprocess-model'
```

``` r
pip[["predict", "out"]]
# [1] "pred: 12" "pred: 18" "pred: 24" "pred: 30" "pred: 36"
```

Whether you combine them or not, in practice pipelines can get long
quickly. The next vignette shows how you can easily focus on certain
parts of your entire analysis workflow by [Pipeline
views](https://github.com/rpahl/pipeflow/articles/v03b-pipeline-views.md).

------------------------------------------------------------------------

1.  Note that code duplication is not bad per se and can even be
    preferable, since it reduces entanglement and improves local
    readability, or as Sandi Metz put it, “duplication is far cheaper
    than the wrong abstraction”.

2.  The step functions in these pipelines are minimal to keep the focus
    on the combine functionality.

3.  A typical example would be appending several pipelines in a
    programmatic context.
