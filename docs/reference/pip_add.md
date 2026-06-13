# Add a step

Adds a named step to the pipeline. Each step is a function whose
parameters either hold constant defaults or reference the output of a
prior step using formula notation (`~step_name`). Dependencies are
validated when the step is added.

## Usage

``` r
pip_add(x, step, fun, tags = character(0), after = length(x), exec = "auto")
```

## Arguments

- x:

  A pipeflow pipeline object.

- step:

  Unique step name.

- fun:

  Function to execute for the step. Each function parameter must have a
  default value. Default values that are simple constants are resolved
  immediately. Default values that are formulas like `~other_step` are
  treated as dependencies to those steps and resolved to the respective
  output values at runtime once the step is executed.

- tags:

  Optional character vector of tags belonging to the step. Can also be
  adjusted later using `[pip_tag()]`.

- after:

  Optional position after which the new step should be inserted
  (defaults to last position). Can be a step name or an integer index.
  If set to 0, the new step will be inserted at the beginning of the
  pipeline.

- exec:

  Execution mode for this step. One of "auto", "split", "reduce" or
  "plain". Using execution mode `exec = split`, the output of the step
  is marked as partitioned output. In this mode, any step that depends
  on the split step (directly or indirectly) will have its output
  automatically mapped partition-wise during step execution. The
  `reduce` mode expects partitioned input and passes it through without
  mapping, while `plain` mode only accepts non-partitioned input and
  always intends to execute a single call. In summary:

  - auto: map if partitioned input appears, otherwise single call

  - split: single call, then mark output as partitioned

  - reduce: single call, but only valid with partitioned input

  - plain: single call, only valid with non-partitioned input

## Value

The updated pipeline, invisibly.

## Details

If `after` was specified, the new step will be inserted after the given
step or position. Be aware that in contrast to adding a step at the end,
inserting a step in the middle is a rather expensive operation as it
requires re-wiring parts of the internal pipeline structure, especially
if the new step is inserted at an early position.

## Examples

``` r
# --- Tags, and view filtering ---
p <- pip_new("analysis") |>
  pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
  pip_add("clean", \(x = ~load) x * 2, tags = c("io", "process")) |>
  pip_add("fit", \(x = ~clean) sum(x), tags = c("model", "core", "daily")) |>
  pip_add("report", \(x = ~fit) paste("result:", x), tags = "report")

pip_run(p)
#> info [2026-06-13 15:08:32.092 UTC]: Start run of pipeflow_pip 'analysis'
#> info [2026-06-13 15:08:32.092 UTC]: Step 1/4 load
#> info [2026-06-13 15:08:32.093 UTC]: Step 2/4 clean
#> info [2026-06-13 15:08:32.096 UTC]: Step 3/4 fit
#> info [2026-06-13 15:08:32.098 UTC]: Step 4/4 report
#> info [2026-06-13 15:08:32.099 UTC]: Finished run of pipeflow_pip 'analysis'
p
#> <pipeflow_pip> analysis (4 steps)
#> ---------------------------------
#>      step depends            out state             tags
#> 1:   load              1,2,3,4,5  done           io,raw
#> 2:  clean    load  2, 4, 6, 8,10  done       io,process
#> 3:    fit   clean             30  done model,core,daily
#> 4: report     fit     result: 30  done           report

# Filter by tag using pip_view — keeps steps with any matching tag
pip_view(p, tags = "daily")
#> <pipeflow_view> analysis view (1 of 4 steps)
#> --------------------------------------------
#>  step depends out state             tags
#>   fit   clean  30  done model,core,daily
pip_view(p, tags = "core")
#> <pipeflow_view> analysis view (1 of 4 steps)
#> --------------------------------------------
#>  step depends out state             tags
#>   fit   clean  30  done model,core,daily
pip_view(p, tags = c("raw", "report"))
#> <pipeflow_view> analysis view (2 of 4 steps)
#> --------------------------------------------
#>    step depends        out state   tags
#>    load          1,2,3,4,5  done io,raw
#>  report     fit result: 30  done report

# --- Split / reduce execution modes ---
q <- pip_new("split-demo") |>
  pip_add("data", \(x = iris) x) |>
  pip_add("split", \(x = ~data) split(x, x$Species),
    exec = "split"
  ) |>
  pip_add("stats", \(x = ~split) summary(x)) |>
  pip_add("combine", \(x = ~stats) do.call(rbind, x),
    exec = "reduce"
  )

pip_run(q)
#> info [2026-06-13 15:08:32.115 UTC]: Start run of pipeflow_pip 'split-demo'
#> info [2026-06-13 15:08:32.115 UTC]: Step 1/4 data
#> info [2026-06-13 15:08:32.116 UTC]: Step 2/4 split
#> info [2026-06-13 15:08:32.118 UTC]: Step 3/4 stats
#> info [2026-06-13 15:08:32.123 UTC]: Step 4/4 combine
#> info [2026-06-13 15:08:32.125 UTC]: Finished run of pipeflow_pip 'split-demo'
q[["stats", "out"]]   # partitioned list — one summary per species
#> $setosa
#>   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#>  Min.   :4.300   Min.   :2.300   Min.   :1.000   Min.   :0.100  
#>  1st Qu.:4.800   1st Qu.:3.200   1st Qu.:1.400   1st Qu.:0.200  
#>  Median :5.000   Median :3.400   Median :1.500   Median :0.200  
#>  Mean   :5.006   Mean   :3.428   Mean   :1.462   Mean   :0.246  
#>  3rd Qu.:5.200   3rd Qu.:3.675   3rd Qu.:1.575   3rd Qu.:0.300  
#>  Max.   :5.800   Max.   :4.400   Max.   :1.900   Max.   :0.600  
#>        Species  
#>  setosa    :50  
#>  versicolor: 0  
#>  virginica : 0  
#>                 
#>                 
#>                 
#> 
#> $versicolor
#>   Sepal.Length    Sepal.Width     Petal.Length   Petal.Width          Species  
#>  Min.   :4.900   Min.   :2.000   Min.   :3.00   Min.   :1.000   setosa    : 0  
#>  1st Qu.:5.600   1st Qu.:2.525   1st Qu.:4.00   1st Qu.:1.200   versicolor:50  
#>  Median :5.900   Median :2.800   Median :4.35   Median :1.300   virginica : 0  
#>  Mean   :5.936   Mean   :2.770   Mean   :4.26   Mean   :1.326                  
#>  3rd Qu.:6.300   3rd Qu.:3.000   3rd Qu.:4.60   3rd Qu.:1.500                  
#>  Max.   :7.000   Max.   :3.400   Max.   :5.10   Max.   :1.800                  
#> 
#> $virginica
#>   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#>  Min.   :4.900   Min.   :2.200   Min.   :4.500   Min.   :1.400  
#>  1st Qu.:6.225   1st Qu.:2.800   1st Qu.:5.100   1st Qu.:1.800  
#>  Median :6.500   Median :3.000   Median :5.550   Median :2.000  
#>  Mean   :6.588   Mean   :2.974   Mean   :5.552   Mean   :2.026  
#>  3rd Qu.:6.900   3rd Qu.:3.175   3rd Qu.:5.875   3rd Qu.:2.300  
#>  Max.   :7.900   Max.   :3.800   Max.   :6.900   Max.   :2.500  
#>        Species  
#>  setosa    : 0  
#>  versicolor: 0  
#>  virginica :50  
#>                 
#>                 
#>                 
#> 
#> attr(,"class")
#> [1] "list"                 "pipeflow_partitioned"
q[["combine", "out"]] # combined table
#>   Sepal.Length      Sepal.Width       Petal.Length      Petal.Width     
#>  "Min.   :4.300  " "Min.   :2.300  " "Min.   :1.000  " "Min.   :0.100  "
#>  "1st Qu.:4.800  " "1st Qu.:3.200  " "1st Qu.:1.400  " "1st Qu.:0.200  "
#>  "Median :5.000  " "Median :3.400  " "Median :1.500  " "Median :0.200  "
#>  "Mean   :5.006  " "Mean   :3.428  " "Mean   :1.462  " "Mean   :0.246  "
#>  "3rd Qu.:5.200  " "3rd Qu.:3.675  " "3rd Qu.:1.575  " "3rd Qu.:0.300  "
#>  "Max.   :5.800  " "Max.   :4.400  " "Max.   :1.900  " "Max.   :0.600  "
#>  "Min.   :4.900  " "Min.   :2.000  " "Min.   :3.00  "  "Min.   :1.000  "
#>  "1st Qu.:5.600  " "1st Qu.:2.525  " "1st Qu.:4.00  "  "1st Qu.:1.200  "
#>  "Median :5.900  " "Median :2.800  " "Median :4.35  "  "Median :1.300  "
#>  "Mean   :5.936  " "Mean   :2.770  " "Mean   :4.26  "  "Mean   :1.326  "
#>  "3rd Qu.:6.300  " "3rd Qu.:3.000  " "3rd Qu.:4.60  "  "3rd Qu.:1.500  "
#>  "Max.   :7.000  " "Max.   :3.400  " "Max.   :5.10  "  "Max.   :1.800  "
#>  "Min.   :4.900  " "Min.   :2.200  " "Min.   :4.500  " "Min.   :1.400  "
#>  "1st Qu.:6.225  " "1st Qu.:2.800  " "1st Qu.:5.100  " "1st Qu.:1.800  "
#>  "Median :6.500  " "Median :3.000  " "Median :5.550  " "Median :2.000  "
#>  "Mean   :6.588  " "Mean   :2.974  " "Mean   :5.552  " "Mean   :2.026  "
#>  "3rd Qu.:6.900  " "3rd Qu.:3.175  " "3rd Qu.:5.875  " "3rd Qu.:2.300  "
#>  "Max.   :7.900  " "Max.   :3.800  " "Max.   :6.900  " "Max.   :2.500  "
#>        Species    
#>  "setosa    :50  "
#>  "versicolor: 0  "
#>  "virginica : 0  "
#>  NA               
#>  NA               
#>  NA               
#>  "setosa    : 0  "
#>  "versicolor:50  "
#>  "virginica : 0  "
#>  NA               
#>  NA               
#>  NA               
#>  "setosa    : 0  "
#>  "versicolor: 0  "
#>  "virginica :50  "
#>  NA               
#>  NA               
#>  NA               
```
