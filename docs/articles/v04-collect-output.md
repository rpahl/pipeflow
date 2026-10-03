# Collect and group output

{pipeflow} manages all functions and parameter dependencies for you,
thereby enabling you to create a lot of pipeline steps without losing
track[¹](#fn1). You therefore can (and should) basically follow the
principle *“one step, one task”*, which on the other hand means that
most steps will be helpers and only a few of them contain the final
output we are interested in.

This vignette shows how to conveniently collect and group those final
outputs.

### Setup

Again, to keep the focus on the displayed functionality, the step
functions are kept very basic.

``` r
library(pipeflow)

pip <- pip_new("my-pip") |>
    pip_add("data", \(x = 1:5) x) |>
    pip_add("prep", \(x = ~data) x * 2, tags = "data") |>
    pip_add("data_summary", \(x = ~prep) range(x),
        tags = c("data", "summary")
    ) |>
    pip_add("model_fit", \(x = ~prep, k = 2) x * k, tags = c("model", "fit")) |>
    pip_add("model_summary", \(x = ~model_fit) sum(x),
        tags = c("model", "summary")
    )
```

As introduced in the [previous
vignette](https://github.com/rpahl/pipeflow/articles/v03b-pipeline-views.md),
we use tags to label steps, specifically, `"data"`/`"model"` to
distinguish the topic, and `"summary"`/`"fit"` for the output type.
Let’s briefly run the pipeline and see what’s in the `out` column.

``` r
(pip_run(pip, lgr = NULL))
# <pipeflow> my-pip (5 steps)
# ---------------------------
#             step params   depends state            out          tags
# 1:          data      x            done      1,2,3,4,5              
# 2:          prep      x      data  done  2, 4, 6, 8,10          data
# 3:  data_summary      x      prep  done           2,10  data,summary
# 4:     model_fit    x,k      prep  done  4, 8,12,16,20     model,fit
# 5: model_summary      x model_fit  done             60 model,summary
# ---------------------------
# <ready> last run: 2026-10-02 20:37:30
```

### Flat output collection

To collect output from a pipeline we use
[`pip_collect()`](https://github.com/rpahl/pipeflow/reference/pip_collect.md),
which by default returns all step outputs as a flat named list.

``` r
pip_collect(pip)
# $data
# [1] 1 2 3 4 5
# 
# $prep
# [1]  2  4  6  8 10
# 
# $data_summary
# [1]  2 10
# 
# $model_fit
# [1]  4  8 12 16 20
# 
# $model_summary
# [1] 60
```

### Filtered output using tags

To collect only the output of steps with a specific tag, we filter the
pipeline with
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md)
and then call
[`pip_collect()`](https://github.com/rpahl/pipeflow/reference/pip_collect.md)
on the resulting view. For example, to collect just the summaries or
model output, we filter by their respective tags:

``` r
pip_view(pip, tags = "summary") |> # or pip[tags %like% "summary"]
    pip_collect()
# $data_summary
# [1]  2 10
# 
# $model_summary
# [1] 60

pip_view(pip, tags = "model") |>
    pip_collect()
# $model_fit
# [1]  4  8 12 16 20
# 
# $model_summary
# [1] 60
```

### Grouped output

Often the output often different groups will be further combined. Let’s
add some more tags to represent section titles of a statistical report.

``` r
pip[step %in% c("data", "prep")] |> pip_tag("Introduction")
pip[step == "model_fit"] |> pip_tag("Model")
pip[step %like% "summary"] |> pip_tag("Summary")

pip
# <pipeflow> my-pip (5 steps)
# ---------------------------
#             step params   depends state            out                  tags
# 1:          data      x            done      1,2,3,4,5          Introduction
# 2:          prep      x      data  done  2, 4, 6, 8,10     data,Introduction
# 3:  data_summary      x      prep  done           2,10  data,summary,Summary
# 4:     model_fit    x,k      prep  done  4, 8,12,16,20       model,fit,Model
# 5: model_summary      x model_fit  done             60 model,summary,Summary
# ---------------------------
# <ready> last run: 2026-10-02 20:37:30
```

Naturally, we then would group the output as follows:

``` r
report <- list(
    Introduction = pip_view(pip, tags = "Introduction") |> pip_collect(),
    Model = pip_view(pip, tags = "Model") |> pip_collect(),
    Summary = pip_view(pip, tags = "Summary") |> pip_collect()
)

str(report)
# List of 3
#  $ Introduction:List of 2
#   ..$ data: int [1:5] 1 2 3 4 5
#   ..$ prep: num [1:5] 2 4 6 8 10
#  $ Model       :List of 1
#   ..$ model_fit: num [1:5] 4 8 12 16 20
#  $ Summary     :List of 2
#   ..$ data_summary : num [1:2] 2 10
#   ..$ model_summary: num 60
```

As this use case is so common, since version `0.4.0` the `pip_collect`
function natively supports grouping via a `by` parameter, which allows
to simplify the above call as follows:

``` r
byTags <- pip_collect(pip, by = "tags")
report2 <- byTags[c("Introduction", "Model", "Summary")]

str(report2)
# List of 3
#  $ Introduction:List of 2
#   ..$ data: int [1:5] 1 2 3 4 5
#   ..$ prep: num [1:5] 2 4 6 8 10
#  $ Model       :List of 1
#   ..$ model_fit: num [1:5] 4 8 12 16 20
#  $ Summary     :List of 2
#   ..$ data_summary : num [1:2] 2 10
#   ..$ model_summary: num 60
```

You now also can return the collected results as a compact table …

``` r
pip_collect(pip, by = "tags", as.table = TRUE)
#            tags       out
#          <char>    <list>
# 1: Introduction <list[2]>
# 2:         data <list[2]>
# 3:      summary <list[2]>
# 4:      Summary <list[2]>
# 5:        model <list[2]>
# 6:          fit <list[1]>
# 7:        Model <list[1]>
```

… and of course `by` can be based on other variables, for example, by
all the dependencies.

``` r
pip_collect(pip, by = "depends", as.table = TRUE)
#      depends       out
#       <char>    <list>
# 1:      data <list[1]>
# 2:      prep <list[2]>
# 3: model_fit <list[1]>
```

For more details see
[`?pip_collect`](https://github.com/rpahl/pipeflow/reference/pip_collect.md).

------------------------------------------------------------------------

1.  The linear/sequential structure of a pipeline design also helps!
