# Collecting and filtering output

Generally speaking, one should keep pipeline steps as simple as
possible, basically following the principle *“one step, one task”*.
Splitting up analysis steps into multiple functions naturally can be
hard to manage, but since {pipeflow} manages all function and parameter
dependencies for you, this is not a problem. Following this principle,
usually a lot of pipeline steps will carry intermediate results and only
a few steps will contain the final output we are interested in. This
vignette shows how to conveniently tag, collect, and possibly group
those final outputs.

### Group and tag definitions

The {pipeflow} package is developed with the goal in mind to provide a
fast, lightweight, and flexible framework that does not force users into
a specific workflow. However, two features for managing pipeline output
seemed to appear so frequently in practice that they found their way as
core features into the package:

- groups to define related output steps, and
- tags to mark steps and enable to filter them for output collection.

Both **group** and **tag** properties can be already set during the
pipeline creation. Let’s illustrate this with an example.

``` r

library(pipeflow)

pip <- pip_new("my-pip") |>
    pip_add(
        "data",
        function(data = airquality) data
    ) |>
    pip_add("data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5 / 9)
        },
        group = "Data", # <- set 'Data' group
        tags = "data" # <- set 'data' tag
    ) |>
    pip_add(
        "data_summary",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            format(summary(data[, c(xVar, yVar)])) |>
                as.data.frame(row.names = NA)
        },
        group = "Data", # <- set 'Data' group
        tags = c("data", "summary") # <- set 'data' and 'summary' tags
    ) |>
    pip_add(
        "data_plot",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            require(ggplot2, quietly = TRUE)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[[yVar]])) +
                labs(title = "Data")
        },
        group = "Data", # <- set 'Data' group
        tags = c("data", "plot") # <- set 'data' and 'plot' tags
    ) |>
    pip_add(
        "model_fit",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        },
        group = "Model", # <- set 'Model' group
        tags = c("model", "fit") # <- set 'model' and 'fit' tags
    ) |>
    pip_add(
        "model_summary",
        function(fit = ~model_fit) {
            summary(fit) |>
                coefficients() |>
                as.data.frame()
        },
        group = "Model", # <- set 'Model' group
        tags = c("model", "summary") # <- set 'model' and 'summary' tags
    ) |>
    pip_add(
        "model_plot",
        function(
            model = ~model_fit,
            data_plot = ~data_plot,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            coeffs <- coefficients(model)
            data_plot +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = "Linear model fit")
        },
        group = "Model", # <- set 'Model' group
        tags = c("model", "plot") # <- set 'model' and 'plot' tags
    )
```

Looking at the pipeline, we can see that in contrast to the examples of
the previous vignettes, two new columns `group` and `tags` have been
added to the overview.

``` r

pip
# <pipeflow_pip> my-pip (7 steps)
# -------------------------------
#             step group             depends    out state          tags
# 1:          data  data                     [NULL]   new              
# 2:     data_prep  Data                data [NULL]   new          data
# 3:  data_summary  Data           data_prep [NULL]   new  data,summary
# 4:     data_plot  Data           data_prep [NULL]   new     data,plot
# 5:     model_fit Model           data_prep [NULL]   new     model,fit
# 6: model_summary Model           model_fit [NULL]   new model,summary
# 7:    model_plot Model model_fit,data_plot [NULL]   new    model,plot
```

Before showing how to make use of the defined groups and tags, let’s run
the pipeline and inspect the output inidividually as we did in the
previous vignettes.

``` r

pip_run(pip)
# info [2026-06-07 15:34:49.001 UTC]: Start run of pipeflow_pip 'my-pip'
# info [2026-06-07 15:34:49.001 UTC]: Step 1/7 data
# info [2026-06-07 15:34:49.003 UTC]: Step 2/7 data_prep
# info [2026-06-07 15:34:49.006 UTC]: Step 3/7 data_summary
# info [2026-06-07 15:34:49.009 UTC]: Step 4/7 data_plot
# info [2026-06-07 15:34:49.417 UTC]: Step 5/7 model_fit
# info [2026-06-07 15:34:49.420 UTC]: Step 6/7 model_summary
# info [2026-06-07 15:34:49.426 UTC]: Step 7/7 model_plot
# info [2026-06-07 15:34:49.431 UTC]: Finished run of pipeflow_pip 'my-pip'

pip
# <pipeflow_pip> my-pip (7 steps)
# -------------------------------
#             step group             depends                 out state          tags
# 1:          data  data                     <data.frame[153x6]>  done              
# 2:     data_prep  Data                data <data.frame[153x7]>  done          data
# 3:  data_summary  Data           data_prep   <data.frame[7x2]>  done  data,summary
# 4:     data_plot  Data           data_prep   <ggplot2::ggplot>  done     data,plot
# 5:     model_fit Model           data_prep            <lm[13]>  done     model,fit
# 6: model_summary Model           model_fit   <data.frame[2x4]>  done model,summary
# 7:    model_plot Model model_fit,data_plot   <ggplot2::ggplot>  done    model,plot
```

``` r

pip[["data_plot", "out"]]
```

![](v04-collect-output_files/figure-html/unnamed-chunk-3-1.png)

``` r

pip[["model_plot", "out"]]
```

![](v04-collect-output_files/figure-html/unnamed-chunk-4-1.png)

### Grouped output

The default {pipeflow} way of gobbling up all output is to call
[`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md)
on the pipeline, which returns a list with all the output.

``` r

out <- pip_collect_out(pip)

str(out, max.level = 1)
# List of 3
#  $ data :'data.frame':    153 obs. of  6 variables:
#  $ Data :List of 3
#  $ Model:List of 3
```

But, as we can see, the output has been grouped into sublists defined by
the `group` properties set earlier. The `Data` and `Model` sublists
contain the output of the respective steps.

``` r

names(out[["Data"]])
# [1] "data_prep"    "data_summary" "data_plot"

names(out[["Model"]])
# [1] "model_fit"     "model_summary" "model_plot"
```

To ignore the defined groups and collect all output step-wise into a
single flat list, we set `grouped = FALSE` when calling
[`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md).

``` r

out <- pip_collect_out(pip, grouped = FALSE)

names(out)
# [1] "data"          "data_prep"     "data_summary"  "data_plot"     "model_fit"     "model_summary"
# [7] "model_plot"
```

### Filtered output using tags

To collect only the output of steps with a specific tag, we use
[`pip_view()`](https://github.com/rpahl/pipeflow/reference/pip_view.md),
which is {pipeflow}’s general-purpose function for filtering pipelines
and then call
[`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md)
on the filtered pipeline. To collect only the plots, for example, we can
filter by the `plot` tag.

``` r

pip_view(pip, tags = "plot")
# <pipeflow_view> my-pip view (2 of 7 steps)
# ------------------------------------------
#        step group             depends               out state       tags
#   data_plot  Data           data_prep <ggplot2::ggplot>  done  data,plot
#  model_plot Model model_fit,data_plot <ggplot2::ggplot>  done model,plot
```

``` r

pip |>
    pip_view(tags = "plot") |>
    pip_collect_out() |>
    gridExtra::grid.arrange(grobs = _, nrow = 2)
```

![](v04-collect-output_files/figure-html/unnamed-chunk-9-1.png)

Next, we collect all summaries.

``` r

summaries <- pip |>
    pip_view(tags = "summary") |>
    pip_collect_out()
```

``` r

summaries[["Data"]]
#       Temp.Celsius            Ozone
# 1 Min.   :13.33    Min.   :  1.00  
# 2 1st Qu.:22.22    1st Qu.: 18.00  
# 3 Median :26.11    Median : 31.50  
# 4 Mean   :25.49    Mean   : 42.13  
# 5 3rd Qu.:29.44    3rd Qu.: 63.25  
# 6 Max.   :36.11    Max.   :168.00  
# 7 NA               NA's   :37
```

``` r

summaries[["Model"]]
#                Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  -69.276985 10.9182367 -6.345071 4.648829e-09
# Temp.Celsius   4.371666  0.4196373 10.417724 2.931897e-18
```

### More on views

To make the example a bit more interesting, we first update some
parameters.

``` r

pip_set_params(pip, params = list(xVar = "Solar.R", yVar = "Wind"))

pip
# <pipeflow_pip> my-pip (7 steps)
# -------------------------------
#             step group             depends                 out    state          tags
# 1:          data  data                     <data.frame[153x6]>     done              
# 2:     data_prep  Data                data <data.frame[153x7]>     done          data
# 3:  data_summary  Data           data_prep   <data.frame[7x2]> outdated  data,summary
# 4:     data_plot  Data           data_prep   <ggplot2::ggplot> outdated     data,plot
# 5:     model_fit Model           data_prep            <lm[13]> outdated     model,fit
# 6: model_summary Model           model_fit   <data.frame[2x4]> outdated model,summary
# 7:    model_plot Model model_fit,data_plot   <ggplot2::ggplot> outdated    model,plot
```

{pipeflow} views provide a variety of filtering options. In the previous
section, the filtering was done based on tags, but you can also filter
based on other properties, for example, all steps that depend on the
`model_fit` step:

``` r

pip |> pip_view(filter = list(depends = "model_fit", state = "outdated"))
# <pipeflow_view> my-pip view (2 of 7 steps)
# ------------------------------------------
#           step group             depends               out    state          tags
#  model_summary Model           model_fit <data.frame[2x4]> outdated model,summary
#     model_plot Model model_fit,data_plot <ggplot2::ggplot> outdated    model,plot
```

or using regex-based filtering, for example, to filter all outdated
steps starting with `data`:

``` r

pip |>
    pip_view(filter = list(step = "^data", state = "outdated"), fixed = FALSE)
# <pipeflow_view> my-pip view (2 of 7 steps)
# ------------------------------------------
#          step group   depends               out    state         tags
#  data_summary  Data data_prep <data.frame[7x2]> outdated data,summary
#     data_plot  Data data_prep <ggplot2::ggplot> outdated    data,plot
```

Views can also be chained together:

``` r

v <- pip |> pip_view(filter = list(state = "outdated"))
v
# <pipeflow_view> my-pip view (5 of 7 steps)
# ------------------------------------------
#           step group             depends               out    state          tags
#   data_summary  Data           data_prep <data.frame[7x2]> outdated  data,summary
#      data_plot  Data           data_prep <ggplot2::ggplot> outdated     data,plot
#      model_fit Model           data_prep          <lm[13]> outdated     model,fit
#  model_summary Model           model_fit <data.frame[2x4]> outdated model,summary
#     model_plot Model model_fit,data_plot <ggplot2::ggplot> outdated    model,plot

v2 <- v |> pip_view(tags = "plot")
v2
# <pipeflow_view> my-pip view view (2 of 7 steps)
# -----------------------------------------------
#        step group             depends               out    state       tags
#   data_plot  Data           data_prep <ggplot2::ggplot> outdated  data,plot
#  model_plot Model model_fit,data_plot <ggplot2::ggplot> outdated model,plot
```

Last but not least, views can be run as pipelines themselves, which
allows to conveniently re-run only the filtered steps, while {pipeflow}
ensures that any upstream dependencies are run first if needed.

``` r

v2 |> pip_run()
# info [2026-06-07 15:34:51.223 UTC]: Start run of pipeflow_view 'my-pip view view'
# info [2026-06-07 15:34:51.223 UTC]: Step 1/4 [upstream] data_prep - skipping done step
# info [2026-06-07 15:34:51.223 UTC]: Step 2/4 [view] data_plot
# info [2026-06-07 15:34:51.233 UTC]: Step 3/4 [upstream] model_fit
# info [2026-06-07 15:34:51.237 UTC]: Step 4/4 [view] model_plot
# info [2026-06-07 15:34:51.244 UTC]: Finished run of pipeflow_view 'my-pip view view'
```

Having a closer look at the run log, you’ll see which steps were re-run
as part of the `[view]` and which were re-run as `[upstream]`
dependencies. Since all views work by reference on the given pipeline,
the original pipeline is now up-to-date for the filtered steps.

``` r

pip
# <pipeflow_pip> my-pip (7 steps)
# -------------------------------
#             step group             depends                 out    state          tags
# 1:          data  data                     <data.frame[153x6]>     done              
# 2:     data_prep  Data                data <data.frame[153x7]>     done          data
# 3:  data_summary  Data           data_prep   <data.frame[7x2]> outdated  data,summary
# 4:     data_plot  Data           data_prep   <ggplot2::ggplot>     done     data,plot
# 5:     model_fit Model           data_prep            <lm[13]>     done     model,fit
# 6: model_summary Model           model_fit   <data.frame[2x4]> outdated model,summary
# 7:    model_plot Model model_fit,data_plot   <ggplot2::ggplot>     done    model,plot
```
