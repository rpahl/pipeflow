# Nested pipelines

A pipeline step can contain another pipeline: instead of computing a
result directly, the step builds an inner pipeline and returns it. This
allows to reuse a standard analysis pipeline inside a larger workflow.

### Inner pipeline

We start with the same pipeline as in the previous [“Split, map, and
reduce”](https://github.com/rpahl/pipeflow/articles/v05a-split-map-reduce.md)
vignette, fitting a linear model and returning its coefficients. This
will serve as our inner pipeline.

``` r
library(pipeflow)

inner <- pip_new("coefficients") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add(
        "fit",
        \(data = ~data, xVar = "x", yVar = "y") {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add("coefs", \(fit = ~fit) coefficients(fit))

inner
# <pipeflow> coefficients (3 steps)
# ---------------------------------
#     step         params depends state
# 1:  data           data           new
# 2:   fit data,xVar,yVar    data   new
# 3: coefs            fit     fit   new
# ---------------------------------
# <ready> last run: never
```

### Outer pipeline

The outer pipeline splits the data into subsets and derives the model
coefficients by running the inner pipeline for each split.

``` r
# Helper to run inner pipeline
run_inner_pip <- function(pip, name, data) {
    pip$name <- sprintf("coefs for *%s*", name)

    # Set data subset for inner pipeline and run it
    pip_set_params(pip, list(data = data)) |> pip_run()

    pip[["coefs", "out"]]
}

outer <- pip_new("full analysis") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add(
        "split_data", \(data = ~data, byVar = "by") {
            split(data, f = data[[byVar]])
        }
    ) |>
    pip_add(
        "inner_run",
        \(dataList = ~split_data, xVar = "x", yVar = "y") {
            p <- pip_clone(inner)

            # Forward parameters to inner
            pip_set_params(p, list(xVar = xVar, yVar = yVar))

            Map(
                f = run_inner_pip,
                name = names(dataList),
                data = dataList,
                MoreArgs = list(pip = p)
            )
        }
    ) |>
    pip_add(
        "combine",
        \(coefs = ~inner_run) as.data.frame(do.call(rbind, coefs))
    )

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#          step             params    depends state
# 1:       data               data              new
# 2: split_data         data,byVar       data   new
# 3:  inner_run dataList,xVar,yVar split_data   new
# 4:    combine              coefs  inner_run   new
# ----------------------------------
# <ready> last run: never
```

Note the `inner_run` step: its parameters `xVar` and `yVar` are
forwarded to the inner pipeline.

### Run nested pipeline

Let’s now set the analysis parameters and run the full pipeline:

``` r
outer |>
    pip_set_params(
        list(
            data = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()
# info [2026-09-27 17:53:34.988 UTC]: Starting run of pipeflow 'full analysis'
# info [2026-09-27 17:53:34.988 UTC]: Step 1/4 data
# info [2026-09-27 17:53:34.990 UTC]: Step 2/4 split_data
# info [2026-09-27 17:53:34.993 UTC]: Step 3/4 inner_run
# info [2026-09-27 17:53:35.001 UTC]: Starting run of pipeflow 'coefs for *setosa*'
# info [2026-09-27 17:53:35.001 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.002 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.006 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.007 UTC]: Finished run of pipeflow 'coefs for *setosa*'
# info [2026-09-27 17:53:35.024 UTC]: Starting run of pipeflow 'coefs for *versicolor*'
# info [2026-09-27 17:53:35.025 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.025 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.027 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.028 UTC]: Finished run of pipeflow 'coefs for *versicolor*'
# info [2026-09-27 17:53:35.030 UTC]: Starting run of pipeflow 'coefs for *virginica*'
# info [2026-09-27 17:53:35.030 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.030 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.033 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.034 UTC]: Finished run of pipeflow 'coefs for *virginica*'
# info [2026-09-27 17:53:35.035 UTC]: Step 4/4 combine
# info [2026-09-27 17:53:35.036 UTC]: Finished run of pipeflow 'full analysis'
```

The output of the `inner_run` step is a list of coefficient vectors, one
for each species,

``` r
outer[["inner_run", "out"]]
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

and the `combine` step returns the expected combined table.

``` r
outer[["combine", "out"]]
#            (Intercept) Sepal.Length
# setosa      -0.5694327    0.7985283
# versicolor   0.8721460    0.3197193
# virginica    1.4463054    0.2318905
```

Now suppose we want to change one of the model settings, say use
`Petal.Length` instead of `Sepal.Length` as the predictor.

``` r
pip_set_params(outer, params = list(xVar = "Petal.Length"))

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#          step             params    depends    state                 out
# 1:       data               data                done <data.frame[150x5]>
# 2: split_data         data,byVar       data     done           <list[3]>
# 3:  inner_run dataList,xVar,yVar split_data outdated           <list[3]>
# 4:    combine              coefs  inner_run outdated   <data.frame[3x2]>
# ----------------------------------
# <ready> last run: 2026-09-27 19:53:35
```

Since the `xVar` parameter is part of the `inner_run` step’s function
arguments, the `inner_run` step’s state (and its downstream
dependencies) correctly has now been marked as “outdated”.

### Forward inner pipeline parameters programmatically

While forwarding the parameters *manually* to the inner pipeline is
straight-forward in our toy example, trying to *manually* synchronize
real-world parameter sets between the outer and inner pipeline quickly
becomes a unfeasible and a source for bugs that are hard to detect.

For this reason, in practice, the following pattern should be used,
which basically just forwards the combined set of *all* existing
parameters. To do this, we replace the `inner_run` step as follows:

``` r
outer |> pip_replace(
    "inner_run",
    \(dataList = ~split_data, ...) {
        p <- pip_clone(inner)

        # Forward all parameters (from outer and inner)
        all_params <- .self$get_params()
        pip_set_params(p, all_params)

        Map(
            f = run_inner_pip,
            name = names(dataList),
            data = dataList,
            MoreArgs = list(pip = p)
        )
    },
    params = pip_get_params(inner) # <--- default parameters of inner pipeline
)

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#          step                  params    depends    state                 out
# 1:       data                    data                done <data.frame[150x5]>
# 2: split_data              data,byVar       data     done           <list[3]>
# 3:  inner_run data,xVar,yVar,dataList split_data      new              [NULL]
# 4:    combine                   coefs  inner_run outdated   <data.frame[3x2]>
# ----------------------------------
# <ready> last run: 2026-09-27 19:53:35
```

In this version, the `inner_run` step no longer declares `xVar` and
`yVar` as its own arguments. Instead, its parameters are seeded with the
inner pipeline’s parameters via `params = pip_get_params(inner)`, and
the step forwards the combined parameter set at run time. Three aspects
are worth spelling out:

- **`.self`** refers to the pipeline that is currently being run and is
  available inside *every* step function without having to declare it.
  It exposes the usual pipeline interface, so `.self$get_params()`
  returns the current unbound parameters of the outer pipeline, and
  things like `.self$name` or `.self$run()` work as expected. For more
  examples on this mechanism see the [self-modifying
  pipelines](https://github.com/rpahl/pipeflow/articles/v06-self-modify-pipeline.md)
  vignette.
- **`params = pip_get_params(inner)`** registers the inner pipeline’s
  unbound parameters (`data`, `xVar`, `yVar`) as parameters of the
  `inner_run` step. They therefore show up in the step’s `params`
  column, become part of the outer pipeline’s parameter set (and can be
  updated with `pip_set_params(outer, ...)`), and are passed to the step
  function when it runs.
- **Precedence over function arguments**: when `params` overlaps with
  the arguments of the step function, the function’s default values take
  precedence. This is why the replacement function above declares only
  `dataList` and `...` — had it declared e.g. `xVar = "x"`, that default
  would win over the value coming from `params`, and the forwarding
  would silently be ignored. As another example,
  `pip_add("s", \(x = 99, ...) x, params = list(x = 1))` stores
  `x = 99`, not `x = 1`.

Let’s re-run the full pipeline.

``` r
outer |>
    pip_set_params(
        list(
            data = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()
# info [2026-09-27 17:53:35.347 UTC]: Starting run of pipeflow 'full analysis'
# info [2026-09-27 17:53:35.347 UTC]: Step 1/4 data
# info [2026-09-27 17:53:35.347 UTC]: Step 2/4 split_data
# info [2026-09-27 17:53:35.349 UTC]: Step 3/4 inner_run
# warn [2026-09-27 17:53:35.351 UTC]: Trying to set parameters not defined in the target: byVar
# Warning in pip_set_params(p, all_params): Trying to set parameters not defined in the target: byVar
# info [2026-09-27 17:53:35.355 UTC]: Starting run of pipeflow 'coefs for *setosa*'
# info [2026-09-27 17:53:35.355 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.355 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.357 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.359 UTC]: Finished run of pipeflow 'coefs for *setosa*'
# info [2026-09-27 17:53:35.361 UTC]: Starting run of pipeflow 'coefs for *versicolor*'
# info [2026-09-27 17:53:35.361 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.361 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.363 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.364 UTC]: Finished run of pipeflow 'coefs for *versicolor*'
# info [2026-09-27 17:53:35.366 UTC]: Starting run of pipeflow 'coefs for *virginica*'
# info [2026-09-27 17:53:35.366 UTC]: Step 1/3 data
# info [2026-09-27 17:53:35.367 UTC]: Step 2/3 fit
# info [2026-09-27 17:53:35.368 UTC]: Step 3/3 coefs
# info [2026-09-27 17:53:35.369 UTC]: Finished run of pipeflow 'coefs for *virginica*'
# info [2026-09-27 17:53:35.370 UTC]: Step 4/4 combine
# info [2026-09-27 17:53:35.371 UTC]: Finished run of pipeflow 'full analysis'
```

Note that the warning in the log above is expected and harmless.
Basically, `.self$get_params()` returns the outer pipeline’s *entire*
parameter set, which includes `byVar` (from the `split_data` step) while
the inner pipeline only defines `data`, `xVar`, and `yVar`. Since
[`pip_set_params()`](https://github.com/rpahl/pipeflow/reference/pip_set_params.md)
reports any parameters that are not defined in the target and simply
leaves them unset, the inner pipeline still receives all parameters it
knows and the result is unaffected.

If you need to omit the warning (e.g. in production code), just suppress
it:

``` r
suppressWarnings(pip_set_params(p, all_params))
```

Alternatively, you could first restrict the forwarded parameters to
those the inner pipeline actually knows. That has the same effect but
adds code, and since forwarding the full parameter set is the whole
point of this pattern, there is no need for it.

Again, changing one of the inner parameters will correctly outdate the
`inner_run` step plus downstream dependencies.

``` r
pip_set_params(outer, params = list(xVar = "Petal.Length"))

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#          step                  params    depends    state                 out
# 1:       data                    data                done <data.frame[150x5]>
# 2: split_data              data,byVar       data     done           <list[3]>
# 3:  inner_run data,xVar,yVar,dataList split_data outdated           <list[3]>
# 4:    combine                   coefs  inner_run outdated   <data.frame[3x2]>
# ----------------------------------
# <ready> last run: 2026-09-27 19:53:35
```

With the above pattern, you can now change both the inner and outer
pipeline, adding and/or removing any steps or parameters, without having
to worry about parameter synchronization.

### Built-in exec modes vs. nested pipelines

Both this and the [previous
vignette](https://github.com/rpahl/pipeflow/articles/v05a-split-map-reduce.md)
solve the same “split, apply, and combine” problem. The built-in
execution modes (`exec = "split"`/`"reduce"`) are the recommended
default whenever they fit while nested pipelines can be considered the
more general tool. As a rule of thumb:

- **Code**: the built-in modes are declarative and need no extra code,
  while the nested pattern requires cloning, mapping, and forwarding
  parameters.
- **Map shape**: the built-in modes handle a single split point in the
  pipeline, while nested pipelines can map over arbitrary inputs (e.g.
  cross-validation folds, several data sets, or per-group settings) and
  multiple levels (nested splits).
- **Reuse**: with nested pipelines the inner pipeline is a standalone,
  reusable artifact, whereas with the built-in modes the steps live in a
  single pipeline.
- **Run log and state**: the built-in modes produce one unified run over
  all partitions, while nested pipelines are run separately.
- **Parallelism**: the built-in modes are handled by {pipeflow}, whereas
  with nested pipelines you control the loop and can parallelize it
  yourself.
