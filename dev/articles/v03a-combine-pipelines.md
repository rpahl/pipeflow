# Combine pipelines

The possibility to combine pipelines basically allows to modularize the
pipeline creation process. This is especially useful when you have a set
of pipelines that are used in different contexts and you want to avoid
code duplication[^1].

### Two pipelines

Let’s define one pipeline that is used for data preprocessing and one
that does the modelling[^2].

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
pipeline](https://rpahl.github.io/pipeflow/dev/articles/v02-modify-pipeline.md),
as follows:

``` r

pip |> pip_replace("data2", \(x = ~standardize) x)

pip
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends state
# 1:        data       x               new
# 2:        prep       x        data   new
# 3: standardize x,scale        prep   new
# 4:       data2       x standardize   new
# 5:         fit   x,k,b       data2   new
# 6:     predict       x         fit   new
# -------------------------------------
# <ready> last run: never
```

The `data2` step points to the output of the `standardize` step, so that
both pipelines are now connected.

#### Relative indexing

Since the name of the re-routed step might not always be known[^3], the
{pipeflow} package also provides a relative position indexing mechanism,
which allows to rewrite the above command using a number (instead of the
step name `standardize`) while having the same effect as above.

``` r

pip |> pip_replace("data2", \(x = ~ -1) x)

pip
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends state
# 1:        data       x               new
# 2:        prep       x        data   new
# 3: standardize x,scale        prep   new
# 4:       data2       x standardize   new
# 5:         fit   x,k,b       data2   new
# 6:     predict       x         fit   new
# -------------------------------------
# <ready> last run: never
```

The relative indexing mechanism allows to refer to steps positioned
above the current step. The index `~-1` can be interpreted as “go one
step back”, `~-2` as “go two steps back”, and so on.

#### Binding params

Instead of replacing the step, params can also be bound after combining,
with
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md):
a one-sided formula binds the param to that step in all selected steps
that have it unbound. Here the `data` step of the first pipeline has an
unbound param `x` as well, but it comes before the `standardize` step,
so it can’t be bound to it. Therefore, we select the `data2` step with a
view:

``` r

pip_bound <- rbind(pip1, pip2)
pip_set_params(pip_bound["data2"], list(x = ~standardize))

pip_bound
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends state
# 1:        data       x               new
# 2:        prep       x        data   new
# 3: standardize x,scale        prep   new
# 4:       data2       x standardize   new
# 5:         fit   x,k,b       data2   new
# 6:     predict       x         fit   new
# -------------------------------------
# <ready> last run: never
```

The param `x` of the `data2` step now refers to the `standardize` step,
as if it had been defined as `\(x = ~standardize) x`. Without the view,
[`pip_set_params()`](https://rpahl.github.io/pipeflow/dev/reference/pip_set_params.md)
would signal an error and leave the pipeline unchanged. The steps are
bound by name, not by relative position.

If the names of the steps to bind are not known, a view can also select
all steps of the second pipeline by position, e.g.
`pip_bound[-seq_along(pip1)]`. Steps where `x` already refers to another
step, like `fit` and `predict`, are left as they are.

### Combined pipeline results

Let’s now run the combined pipeline and inspect the results of the final
step.

``` r

pip_run(pip)
# info [2026-10-25 08:12:39.749 UTC]: Starting run of pipeflow 'preprocess-model'
# info [2026-10-25 08:12:39.750 UTC]: Step 1/6 data
# info [2026-10-25 08:12:39.751 UTC]: Step 2/6 prep
# info [2026-10-25 08:12:39.753 UTC]: Step 3/6 standardize
# info [2026-10-25 08:12:39.754 UTC]: Step 4/6 data2
# info [2026-10-25 08:12:39.755 UTC]: Step 5/6 fit
# info [2026-10-25 08:12:39.756 UTC]: Step 6/6 predict
# info [2026-10-25 08:12:39.757 UTC]: Finished in 0.008 s (6 done, 0 skipped, 0 failed)
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
# info [2026-10-25 08:12:40.020 UTC]: Starting run of pipeflow 'preprocess-model'
# info [2026-10-25 08:12:40.020 UTC]: Step 1/6 data - skipping done step
# info [2026-10-25 08:12:40.020 UTC]: Step 2/6 prep - skipping done step
# info [2026-10-25 08:12:40.021 UTC]: Step 3/6 standardize - skipping done step
# info [2026-10-25 08:12:40.021 UTC]: Step 4/6 data2 - skipping done step
# info [2026-10-25 08:12:40.021 UTC]: Step 5/6 fit
# info [2026-10-25 08:12:40.024 UTC]: Step 6/6 predict
# info [2026-10-25 08:12:40.026 UTC]: Finished in 0.006 s (2 done, 4 skipped, 0 failed)
```

``` r

pip[["predict", "out"]]
# [1] "pred: 12" "pred: 18" "pred: 24" "pred: 30" "pred: 36"
```

### Saving and restoring pipelines

A pipeline, like the combined one above, can be built once and reused
later, also in another R session. To this end, it can be saved to a file
with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and read back
with [`readRDS()`](https://rdrr.io/r/base/readRDS.html)[^4].

``` r

file <- tempfile(fileext = ".rds")
saveRDS(pip, file)

pip_restored <- readRDS(file)
pip_restored
# <pipeflow> preprocess-model (6 steps)
# -------------------------------------
#           step  params     depends state                                          out
# 1:        data       x              done                                    1,2,3,4,5
# 2:        prep       x        data  done                                    2,3,4,5,6
# 3: standardize x,scale        prep  done                                4, 6, 8,10,12
# 4:       data2       x standardize  done                                4, 6, 8,10,12
# 5:         fit   x,k,b       data2  done                               12,18,24,30,36
# 6:     predict       x         fit  done pred: 12,pred: 18,pred: 24,pred: 30,pred: 36
# -------------------------------------
# <ready> last run: 2026-10-25 08:12:40
```

The restored pipeline has all steps, parameters, outputs and states of
the saved one. That is, it can be modified and run right away, and only
the steps that need an update are run again:

``` r

pip_set_params(pip_restored, params = list(k = 4))
pip_run(pip_restored)
# info [2026-10-25 08:12:40.222 UTC]: Starting run of pipeflow 'preprocess-model'
# info [2026-10-25 08:12:40.223 UTC]: Step 1/6 data - skipping done step
# info [2026-10-25 08:12:40.223 UTC]: Step 2/6 prep - skipping done step
# info [2026-10-25 08:12:40.223 UTC]: Step 3/6 standardize - skipping done step
# info [2026-10-25 08:12:40.223 UTC]: Step 4/6 data2 - skipping done step
# info [2026-10-25 08:12:40.223 UTC]: Step 5/6 fit
# info [2026-10-25 08:12:40.225 UTC]: Step 6/6 predict
# info [2026-10-25 08:12:40.226 UTC]: Finished in 0.004 s (2 done, 4 skipped, 0 failed)

pip_restored[["predict", "out"]]
# [1] "pred: 16" "pred: 24" "pred: 32" "pred: 40" "pred: 48"
```

Both the saved and the restored pipelines are independent of each other,
so the original `pip` still uses the multiplier `k = 3`:

``` r

pip_get_params(pip)$k
# [1] 3
```

For more details see also the section “Saving pipelines” in the help of
[`pip_new()`](https://rpahl.github.io/pipeflow/dev/reference/pip_new.md).

Whether you combine them or not, in practice pipelines can get long
quickly. The next vignette shows how you can easily focus on certain
parts of your entire analysis workflow by [Pipeline
views](https://rpahl.github.io/pipeflow/dev/articles/v03b-pipeline-views.md).

[^1]: Note that code duplication is not bad per se and can even be
    preferable, since it reduces entanglement and improves local
    readability, or as Sandi Metz put it, “duplication is far cheaper
    than the wrong abstraction”.

[^2]: The step functions in these pipelines are minimal to keep the
    focus on the combine functionality.

[^3]: A typical example would be appending several pipelines in a
    programmatic context.

[^4]: Other ways to serialize R objects work as well, for example to
    send a pipeline to another R process.
