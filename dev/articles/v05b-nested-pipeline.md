# Nested pipelines

A pipeline can be a step of another pipeline. This allows to reuse a
standard analysis pipeline inside a larger workflow. The key is
[`as.function()`](https://rdrr.io/r/base/as.function.html), which turns
a pipeline into a function that can be used in pipeline steps.

### Inner pipeline

We start with the same pipeline as in the previous [“Split, map, and
reduce”](https://rpahl.github.io/pipeflow/dev/articles/v05a-split-map-reduce.md)
vignette, fitting a linear model and returning its coefficients. This
will serve as our inner pipeline.

``` r

library(pipeflow)

inner <- pip_new("coefficients") |>
    pip_add("input", \(input_df = NULL) input_df) |>
    pip_add("fit",
        \(data = ~input, xVar = "x", yVar = "y") {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add("coefs",
        \(fit = ~fit, digits = 3) {
            round(coefficients(fit), digits)
        }
    )

inner
# <pipeflow> coefficients (3 steps)
# ---------------------------------
#     step         params depends state
# 1: input       input_df           new
# 2:   fit data,xVar,yVar   input   new
# 3: coefs     fit,digits     fit   new
# ---------------------------------
# <ready> last run: never
# steps: 3 new
```

### A pipeline as a function

[`as.function()`](https://rdrr.io/r/base/as.function.html) returns a
function whose arguments are the unbound parameters of the pipeline,
with the current values as defaults, and whose result is the output of
the step(s) defined by `out`.

``` r

f_inner <- as.function(inner, out = "coefs")

formals(f_inner) |> str()
# Dotted pair list of 4
#  $ input_df: NULL
#  $ xVar    : chr "x"
#  $ yVar    : chr "y"
#  $ digits  : num 3
```

As you see, the function version of the inner pipeline has the same
default params as the pipeline itself.

Under the hood, the function runs a copy of the inner pipeline. Let’s
see this now:

``` r

params <- list(input_df = iris, xVar = "Sepal.Length", yVar = "Sepal.Width")
pip_set_params(inner, params) |> pip_run()
# info [2026-10-25 19:30:42.846 UTC]: Starting run of pipeflow 'coefficients'
# info [2026-10-25 19:30:42.847 UTC]: Step 1/3 input
# info [2026-10-25 19:30:42.848 UTC]: Step 2/3 fit
# info [2026-10-25 19:30:42.851 UTC]: Step 3/3 coefs
# info [2026-10-25 19:30:42.852 UTC]: Finished in 0.006 s (3 done, 0 skipped, 0 failed)

inner[["coefs", "out"]]
#  (Intercept) Sepal.Length 
#        3.419       -0.062

do.call(f_inner, params)
#  (Intercept) Sepal.Length 
#        3.419       -0.062
```

Note that since `f_inner` gets its own copy of the inner pipeline, later
changes to the inner pipeline will *not* change the function.

### Outer pipeline

As any other function, the function can be added as a step of another
pipeline. We start with the simplest case, where the outer pipeline
provides the data and the inner pipeline analyses it as a whole.

``` r

outer_all <- pip_new("analysis") |>
    pip_add("df", \(df = NULL) df) |>
    pip_add(
        "inner_run",
        as.function(inner, out = "coefs"),  # coefs is returned from inner run
        params = list(input_df = ~df)       # df is passed to the inner pipeline
    )

outer_all
# <pipeflow> analysis (2 steps)
# -----------------------------
#         step                    params depends state
# 1:        df                        df           new
# 2: inner_run input_df,xVar,yVar,digits      df   new
# -----------------------------
# <ready> last run: never
# steps: 2 new
```

As for any other step function, `params` of
[`pip_add()`](https://rpahl.github.io/pipeflow/dev/reference/pip_add.md)
wires the function into the outer pipeline. `list(input_df = ~df)`
reads: the argument `input_df` of the function is *bound* to the output
of the step `df` from the outer pipeline, that is, the step `inner_run`
now depends on `df`.

The remaining arguments of the function, `xVar`, `yVar` and `digits`,
are not bound. They stay unbound parameters, and as such are parameters
of the outer pipeline, without having to forward them:

``` r

pip_get_params(outer_all) |> str()
# List of 4
#  $ df    : NULL
#  $ xVar  : chr "Sepal.Length"
#  $ yVar  : chr "Sepal.Width"
#  $ digits: num 3
```

The `df` listed here belongs to the outer step `df`.

Let’s set the analysis parameters and run the outer pipeline:

``` r

outer_all |>
    pip_set_params(
        list(df = iris, xVar = "Sepal.Length", yVar = "Sepal.Width")
    ) |>
    pip_run()
# info [2026-10-25 19:30:42.964 UTC]: Starting run of pipeflow 'analysis'
# info [2026-10-25 19:30:42.970 UTC]: Step 1/2 df
# info [2026-10-25 19:30:42.971 UTC]: Step 2/2 inner_run
# info [2026-10-25 19:30:42.984 UTC]: Finished in 0.020 s (2 done, 0 skipped, 0 failed)

outer_all[["inner_run", "out"]]
#  (Intercept) Sepal.Length 
#        3.419       -0.062
```

The inner pipeline was run as a single step and returned the same
coefficients as above where we called the function directly.

#### Returning something computed

Instead of step names, `out` can also be a function. It receives the
inner pipeline after the run and returns whatever you need from it,
e.g. a value computed from several steps:

``` r

f_summary <- as.function(
    inner,
    out = \(p) list(
        r2 = summary(p[["fit", "out"]])$r.squared,
        coefs = p[["coefs", "out"]]
    )
)

f_summary(input_df = iris, xVar = "Sepal.Length", yVar = "Sepal.Width")
# $r2
# [1] 0.01382265
# 
# $coefs
#  (Intercept) Sepal.Length 
#        3.419       -0.062
```

To simply return the outputs of some steps, use
`out = c("fit", "coefs")`. Functions like
[`pip_collect()`](https://rpahl.github.io/pipeflow/dev/reference/pip_collect.md)
are useful in `out` when you want more, e.g. to collect the outputs by
tags (see
[`?as.function.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)).

### Outer pipeline with split data

The real strength shows when the inner pipeline is used multiple times
in a “split-data” scenario. Here, the outer pipeline splits the data
into subsets (see also the [previous
vignette](https://rpahl.github.io/pipeflow/dev/articles/v05a-split-map-reduce.md)),
and `input_df` is bound to the output of the split step. Then the
function is called once per subset.

``` r

outer <- pip_new("full analysis") |>
    pip_add("df", \(df = NULL) df) |>
    pip_add(
        "split_df",
        \(df = ~df, byVar = "by") split(df, f = df[[byVar]]),
        exec = "split"
    ) |>
    pip_add(
        "inner_run",
        as.function(inner, out = "coefs"),
        params = list(input_df = ~split_df)    # bind to split data
    ) |>
    pip_add(
        "combine",
        \(coefs = ~inner_run) as.data.frame(do.call(rbind, coefs)),
        exec = "reduce"
    )

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#         step                    params   depends state   exec
# 1:        df                        df             new   auto
# 2:  split_df                  df,byVar        df   new  split
# 3: inner_run input_df,xVar,yVar,digits  split_df   new   auto
# 4:   combine                     coefs inner_run   new reduce
# ----------------------------------
# <ready> last run: never
# steps: 4 new
```

The split output is partitioned, so the function is called once per
subset, and its output is partitioned as well. The `combine` step is a
`reduce` step and receives all partitions. As before, the unbound
parameters of the inner pipeline are parameters of the outer pipeline,
now next to `byVar` of the split step:

``` r

pip_get_params(outer) |> str()
# List of 5
#  $ df    : NULL
#  $ byVar : chr "by"
#  $ xVar  : chr "Sepal.Length"
#  $ yVar  : chr "Sepal.Width"
#  $ digits: num 3
```

Let’s set the analysis parameters and run the full pipeline:

``` r

outer |>
    pip_set_params(
        list(
            df = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()
# info [2026-10-25 19:30:43.137 UTC]: Starting run of pipeflow 'full analysis'
# info [2026-10-25 19:30:43.137 UTC]: Step 1/4 df
# info [2026-10-25 19:30:43.138 UTC]: Step 2/4 split_df
# info [2026-10-25 19:30:43.139 UTC]: Step 3/4 inner_run
# info [2026-10-25 19:30:43.153 UTC]: Step 4/4 combine
# info [2026-10-25 19:30:43.154 UTC]: Finished in 0.017 s (4 done, 0 skipped, 0 failed)
```

The output of the `inner_run` step is a partitioned list of coefficient
vectors, one for each species,

``` r

outer[["inner_run", "out"]]
# $setosa
#  (Intercept) Sepal.Length 
#       -0.569        0.799 
# 
# $versicolor
#  (Intercept) Sepal.Length 
#        0.872        0.320 
# 
# $virginica
#  (Intercept) Sepal.Length 
#        1.446        0.232 
# 
# attr(,"class")
# [1] "list"                 "pipeflow_partitioned"
```

and the `combine` step returns the expected combined table.

``` r

outer[["combine", "out"]]
#            (Intercept) Sepal.Length
# setosa          -0.569        0.799
# versicolor       0.872        0.320
# virginica        1.446        0.232
```

Now suppose we want to change one of the model settings, say use
`Petal.Length` instead of `Sepal.Length` as the predictor.

``` r

pip_set_params(outer, params = list(xVar = "Petal.Length"))

outer
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#         step                    params   depends    state                 out   exec
# 1:        df                        df               done <data.frame[150x5]>   auto
# 2:  split_df                  df,byVar        df     done           <list[3]>  split
# 3: inner_run input_df,xVar,yVar,digits  split_df outdated           <list[3]>   auto
# 4:   combine                     coefs inner_run outdated   <data.frame[3x2]> reduce
# ----------------------------------
# <ready> last run: 2026-10-25 19:30:43 (0.017 s)
# steps: 2 done, 2 outdated
```

Since `xVar` is a parameter of the `inner_run` step, this step and its
downstream steps have been marked as “outdated”.

### Caching of inner pipelines

A pipeline function by default caches the inner pipelines it has run:
one for each step and subset. In a step that is mapped over subsets,
each subset gets an inner pipeline of its own. A later call sets all
parameters and inputs in the pipeline of its subset, so that exactly the
steps that are outdated by the change are run again.

To see this at work, we use a minimal toy example. The inner pipeline
squares numbers and scales the result. The counters tell us how often
each step has actually run.

``` r

runs <- new.env()
runs$square <- runs$scale <- 0L
count <- function(step) runs[[step]] <- runs[[step]] + 1L
n_runs <- function() unlist(mget(c("square", "scale"), envir = runs))

toy_inner <- pip_new("toy inner") |>
    pip_add("input", \(x = NULL) x) |>
    pip_add("square", \(x = ~input) {
        count("square")
        x^2
    }) |>
    pip_add("scale", \(sq = ~square, factor = 1) {
        count("scale")
        sq * factor
    })
```

The outer pipeline splits a list of two number vectors, `a` and `b`, and
runs the inner pipeline on each of them.

``` r

toy_outer <- pip_new("toy outer") |>
    pip_add(
        "numbers",
        \(values = list(a = 1:3, b = 4:6)) values,
        exec = "split"
    ) |>
    pip_add(
        "inner_run",
        as.function(toy_inner, out = "scale"),
        params = list(x = ~numbers)
    )

pip_run(toy_outer, lgr = NULL)
n_runs()
# square  scale 
#      2      2
```

Each step ran twice, once in the inner pipeline of `a` and once in the
one of `b`. Now we change `factor`, which only the last step uses. Only
this step is outdated in the inner pipelines, so only this step runs
again:

``` r

pip_set_params(toy_outer, list(factor = 10))
pip_run(toy_outer, lgr = NULL)
n_runs()
# square  scale 
#      2      4
```

Next we change the input data and give `a` other numbers while `b` stays
the same, so that we expect that the inner pipeline is run *once* for
`a`.

``` r

pip_set_params(toy_outer, list(values = list(a = 1:4, b = 4:6)))
pip_run(toy_outer, lgr = NULL)
n_runs()
# square  scale 
#      3      5

toy_outer[["inner_run", "out"]]
# $a
# [1]  10  40  90 160
# 
# $b
# [1] 160 250 360
# 
# attr(,"class")
# [1] "list"                 "pipeflow_partitioned"
```

To look at the kept inner pipelines, e.g. while debugging, use
[`pip_inner()`](https://rpahl.github.io/pipeflow/dev/reference/pip_inner.md).
For a mapped step, it returns one pipeline per partition key:

``` r

inner_pips <- pip_inner(toy_outer, "inner_run")
names(inner_pips)
# [1] "a" "b"

inner_pips[["a"]]
# <pipeflow> toy inner (3 steps)
# ------------------------------
#      step    params depends state             out
# 1:  input         x          done         1,2,3,4
# 2: square         x   input  done      1, 4, 9,16
# 3:  scale sq,factor  square  done  10, 40, 90,160
# ------------------------------
# <ready> last run: 2026-10-25 19:30:43 (0.003 s)
# steps: 3 done
```

A few things to know:

- Each call has an overhead of a few milliseconds for setting the
  parameters and running the pipeline, so reuse pays when the inner
  steps are expensive.
- Inputs are compared as they are, so a change made in place to an
  environment or an R6 object is not seen (as in a plain pipeline). Use
  `cache = FALSE` for such inputs.
- Warnings and messages of the inner steps reach the outer run. If an
  inner step fails, the outer step fails, unless the inner pipeline
  handles the failure itself (see [“Handling
  failures”](https://rpahl.github.io/pipeflow/dev/articles/v05c-error-handling.md)).
- The inner pipelines are run silently. To follow their steps while
  debugging, set `as.function(lgr = pipeflow_lgr)` (or any other
  logger).

See the “Pipeline functions with partitioned inputs” section of
[`?as.function.pipeflow`](https://rpahl.github.io/pipeflow/dev/reference/as.function.pipeflow.md)
for how the inner pipelines are kept and reused.

### Full control: running the inner pipeline yourself

[`as.function()`](https://rdrr.io/r/base/as.function.html) shares the
parameters of the inner pipeline between all subsets. If you need more
control, e.g. different settings per group or mapping over something
else than the output of a split step, you can always “manually” clone
and run the inner pipeline in a step function yourself.

``` r

# Helper to run inner pipeline
run_inner_pip <- function(pip, name, df) {
    pip$name <- sprintf("coefs for *%s*", name)

    # Set data subset for inner pipeline and run it
    pip_set_params(pip, list(input_df = df)) |> pip_run(lgr = NULL)

    pip[["coefs", "out"]]
}

manual <- pip_new("full analysis") |>
    pip_add("df", \(df = NULL) df) |>
    pip_add(
        "split_df",
        \(df = ~df, byVar = "by") split(df, f = df[[byVar]])
    ) |>
    pip_add(
        "inner_run",
        \(dfList = ~split_df, ...) {
            p <- pip_clone(inner)

            # Forward the parameters of the step to the inner pipeline
            pip_set_params(p, list(...))

            Map(
                f = run_inner_pip,
                name = names(dfList),
                df = dfList,
                MoreArgs = list(pip = p)
            )
        },
        params = pip_get_params(inner) # <--- default parameters of inner
    ) |>
    pip_add(
        "combine",
        \(coefs = ~inner_run) as.data.frame(do.call(rbind, coefs))
    )

manual
# <pipeflow> full analysis (4 steps)
# ----------------------------------
#         step                           params   depends state
# 1:        df                               df             new
# 2:  split_df                         df,byVar        df   new
# 3: inner_run dfList,input_df,xVar,yVar,digits  split_df   new
# 4:   combine                            coefs inner_run   new
# ----------------------------------
# <ready> last run: never
# steps: 4 new
```

In this version, the `inner_run` step does not declare the parameters of
the inner pipeline as arguments. Instead, its parameters are seeded with
the inner pipeline’s parameters via `params = pip_get_params(inner)`,
and the step forwards them to the inner pipeline at run time:

- **`params = pip_get_params(inner)`** registers the inner pipeline’s
  unbound parameters (`input_df`, `xVar`, `yVar`, `digits`) as
  parameters of the `inner_run` step. They show up in the step’s
  `params` column, become part of the outer pipeline’s parameter set,
  and changing them outdates the step.
- **`...`** *takes the forwarded parameters*: since the `inner_run` step
  only declares `dfList` as an argument, the registered parameters reach
  the step function via `...`. It carries exactly the parameters of this
  step, with their current values (unwrapped, see
  [`?pip_param_value`](https://rpahl.github.io/pipeflow/dev/reference/pip_param_value.md)),
  so `pip_set_params(p, list(...))` sets them in the inner pipeline,
  without any unknown parameters to ignore. Where the names overlap, the
  values in `params` would override the function defaults: had the inner
  pipeline an unbound parameter `dfList`, its value would replace the
  reference `~split_df` of the `inner_run` step, something to be aware
  of.

Let us now run the manual pipeline to see it in action.

``` r

manual |>
    pip_set_params(
        list(
            df = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()
# info [2026-10-25 19:30:43.649 UTC]: Starting run of pipeflow 'full analysis'
# info [2026-10-25 19:30:43.649 UTC]: Step 1/4 df
# info [2026-10-25 19:30:43.650 UTC]: Step 2/4 split_df
# info [2026-10-25 19:30:43.651 UTC]: Step 3/4 inner_run
# info [2026-10-25 19:30:43.666 UTC]: Step 4/4 combine
# info [2026-10-25 19:30:43.667 UTC]: Finished in 0.018 s (4 done, 0 skipped, 0 failed)

manual[["combine", "out"]]
#            (Intercept) Sepal.Length
# setosa          -0.569        0.799
# versicolor       0.872        0.320
# virginica        1.446        0.232
```

This version runs each subset from scratch every time the step is
outdated. With
[`as.function()`](https://rdrr.io/r/base/as.function.html), the same
result is obtained with less code and with caching.

### Built-in exec modes vs. nested pipelines

Both this and the [previous
vignette](https://rpahl.github.io/pipeflow/dev/articles/v05a-split-map-reduce.md)
solve the same “split, apply, and combine” problem. The built-in
execution modes (`exec = "split"`/`"reduce"`) are the recommended
default whenever the work fits into the steps of a single pipeline,
while nested pipelines are the more general tool. As a rule of thumb:

- **Code**: the built-in modes are declarative and need no extra code.
  [`as.function()`](https://rdrr.io/r/base/as.function.html) needs one
  step that binds its inputs. Running the inner pipeline yourself
  requires cloning, mapping, and forwarding parameters.
- **Map shape**: the built-in modes handle a single split point in the
  pipeline, while nested pipelines can be mapped over the partitions of
  a split step *and* can contain their own splits (nested splits).
  Running the inner pipeline yourself can map over arbitrary inputs
  (e.g. cross-validation folds or several data sets).
- **Run log and state**: the built-in modes produce one unified run over
  all partitions, while a nested pipeline is run as one step of the
  outer pipeline, with its own log silenced by default.
- **Parallelism**: the built-in modes are handled by {pipeflow}, whereas
  with a manual loop you control it and can parallelize it yourself.
