# Add a step

Adds a named step to the pipeline. Each step is a function whose
parameters either hold constant defaults or reference the output of a
prior step using formula notation (`~step_name`). Dependencies are
validated when the step is added.

## Usage

``` r
pip_add(
  x,
  step,
  fun,
  tags = character(0),
  after = length(x),
  params = list(),
  exec = "auto"
)
```

## Arguments

- x:

  A pipeflow pipeline object.

- step:

  Unique step name.

- fun:

  Function to execute for the step. Each function parameter needs a
  value: a default value in `fun` or an entry in `params`. Default
  values that are simple constants are resolved immediately. Default
  values that are formulas like `~other_step` are treated as
  dependencies to those steps and resolved to the respective output
  values at runtime once the step is executed. A reference wrapped in
  [`try()`](https://rdrr.io/r/base/try.html), like `~try(other_step)`,
  accepts a failed input when the pipeline is run with
  `on_error = "continue"`: instead of the output of the referenced step,
  the argument then receives a condition of class `pipeflow_failure`
  (see section 'Continuing after errors' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)).
  The function can refer to the running pipeline as `.self` (see section
  'The pipeline as `.self`' in `pip_add()`).

- tags:

  Optional character vector of tags belonging to the step. Can also be
  adjusted later using `[pip_tag()]`.

- after:

  Optional position after which the new step should be inserted
  (defaults to last position). Can be a step name or an integer index.
  If set to 0, the new step will be inserted at the beginning of the
  pipeline.

- params:

  Optional named list of parameter values. They override the defaults of
  `fun` and give a value to parameters of `fun` that have no default. As
  in the defaults of `fun`, a formula like `~other_step` refers to the
  output of another step. Names that are no parameters of `fun` add
  params to the step, which are passed on to `fun` as well, so `fun`
  needs a `...` argument for them. Use cases for `params` are:

  1.  Set param values programmatically, e.g. when adding steps in a
      loop or at runtime.

  2.  Use existing functions whose parameters have no defaults, e.g.
      `pip_add(p, "avg", mean, params = list(x = ~data, na.rm = TRUE))`.

  3.  Provide extra param values to "mark" dependencies that are defined
      in pipelines nested in a step, which ensures that it is
      re-executed when one of the nested param values change. See the
      [nested pipelines
      vignette](https://rpahl.github.io/pipeflow/dev/doc/v05b-nested-pipeline.md).

- exec:

  Execution mode for this step. One of "auto", "split", "reduce" or
  "plain". Using execution mode `exec = split`, the output of the step
  is marked as partitioned output. In this mode, any step that depends
  on the split step (directly or indirectly) will have its output
  automatically mapped partition-wise during step execution. The
  `reduce` mode expects partitioned input and passes it through without
  mapping, while `plain` mode only accepts non-partitioned input and
  always intends to execute a single call. In a run with
  `on_error = "continue"`, a mapped step that fails for some partition
  keys keeps the outputs of the other keys (see section 'Continuing
  after errors' in
  [`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)).
  If a split step fails, a `reduce` step also accepts the resulting
  failure object when it takes it via `~try()`. In summary:

  - auto: map if partitioned input appears, otherwise single call.
    Several partitioned inputs must have the same keys in the same
    order.

  - split: single call, then mark output as partitioned

  - reduce: single call, but only valid with partitioned input or, via
    `~try()`, a failure object

  - plain: single call, only valid with non-partitioned input

## Value

The updated pipeline, invisibly.

## Details

If `after` was specified, the new step will be inserted after the given
step or position. Be aware that in contrast to adding a step at the end,
inserting a step in the middle is a rather expensive operation as it
requires re-wiring parts of the internal pipeline structure, especially
if the new step is inserted at an early position.

### The pipeline as `.self`

A step function can refer to the pipeline that is being run as `.self`
without declaring it as an argument, for example to set parameters or
add steps at runtime, or to call `.self$restart()` or `.self$halt()`
(see
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)).
`.self` is always the full pipeline, also when a view is run. pipeflow
provides it each time the step runs, but only if the body of the step
function contains the name `.self` (also within functions defined in the
body). Indirect access, such as `get(".self")`, is not detected.

A step that changes its own function, params or exec mode via `.self`,
ends the run as `"outdated"`. The same is true for other steps changed
via `.self`. Lastly, `.self` is only valid while the step runs and
released once the step was returned. For this reason, a step that
returns a function referring to `.self` fails with
`object '.self' not found` when called later.

## Examples

``` r
# --- Tags, and view filtering ---
p <- pip_new("analysis") |>
  pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
  pip_add("clean", \(x = ~load) x * 2, tags = c("io", "process")) |>
  pip_add("fit", \(x = ~clean) sum(x), tags = c("model", "core", "daily")) |>
  pip_add("report", \(x = ~fit) paste("result:", x), tags = "report")

pip_run(p)
#> info [2026-10-25 19:30:01.612 UTC]: Starting run of pipeflow 'analysis'
#> info [2026-10-25 19:30:01.612 UTC]: Step 1/4 load
#> info [2026-10-25 19:30:01.613 UTC]: Step 2/4 clean
#> info [2026-10-25 19:30:01.614 UTC]: Step 3/4 fit
#> info [2026-10-25 19:30:01.615 UTC]: Step 4/4 report
#> info [2026-10-25 19:30:01.615 UTC]: Finished in 0.003 s (4 done, 0 skipped, 0 failed)
p
#> <pipeflow> analysis (4 steps)
#> -----------------------------
#>      step params depends state            out             tags
#> 1:   load      n          done      1,2,3,4,5           io,raw
#> 2:  clean      x    load  done  2, 4, 6, 8,10       io,process
#> 3:    fit      x   clean  done             30 model,core,daily
#> 4: report      x     fit  done     result: 30           report
#> -----------------------------
#> <ready> last run: 2026-10-25 19:30:01 (0.003 s)
#> steps: 4 done

# Filter by tag using pip_view — keeps steps with any matching tag
pip_view(p, tags = "daily")
#> <pipeflow_view> analysis (1 of 4 steps)
#> ---------------------------------------
#>    step params depends state out             tags
#> 1:  fit      x   clean  done  30 model,core,daily
#> ---------------------------------------
#> <ready> last run: 2026-10-25 19:30:01 (0.003 s)
#> steps: 1 done
pip_view(p, tags = "core")
#> <pipeflow_view> analysis (1 of 4 steps)
#> ---------------------------------------
#>    step params depends state out             tags
#> 1:  fit      x   clean  done  30 model,core,daily
#> ---------------------------------------
#> <ready> last run: 2026-10-25 19:30:01 (0.003 s)
#> steps: 1 done
pip_view(p, tags = c("raw", "report"))
#> <pipeflow_view> analysis (2 of 4 steps)
#> ---------------------------------------
#>      step params depends state        out   tags
#> 1:   load      n          done  1,2,3,4,5 io,raw
#> 2: report      x     fit  done result: 30 report
#> ---------------------------------------
#> <ready> last run: 2026-10-25 19:30:01 (0.003 s)
#> steps: 2 done

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
#> info [2026-10-25 19:30:01.623 UTC]: Starting run of pipeflow 'split-demo'
#> info [2026-10-25 19:30:01.623 UTC]: Step 1/4 data
#> info [2026-10-25 19:30:01.623 UTC]: Step 2/4 split
#> info [2026-10-25 19:30:01.624 UTC]: Step 3/4 stats
#> info [2026-10-25 19:30:01.628 UTC]: Step 4/4 combine
#> info [2026-10-25 19:30:01.628 UTC]: Finished in 0.006 s (4 done, 0 skipped, 0 failed)
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

# --- Insert a step at a specific position with 'after' ---
p2 <- pip_new("insert-demo") |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)
p2
#> <pipeflow> insert-demo (2 steps)
#> --------------------------------
#>    step params depends state
#> 1: load      x           new
#> 2:  fit      x    load   new
#> --------------------------------
#> <ready> last run: never
#> steps: 2 new
pip_add(p2, "clean", \(x = ~load) x * 2, after = "load")
p2  # "load", "clean", "fit" — inserted after "load"
#> <pipeflow> insert-demo (3 steps)
#> --------------------------------
#>     step params depends state
#> 1:  load      x           new
#> 2: clean      x    load   new
#> 3:   fit      x    load   new
#> --------------------------------
#> <ready> last run: never
#> steps: 3 new

# after = 0 inserts the new step at the very beginning
pip_add(p2, "preload", \(x = 1) x, after = 0)
p2
#> <pipeflow> insert-demo (4 steps)
#> --------------------------------
#>       step params depends state
#> 1: preload      x           new
#> 2:    load      x           new
#> 3:   clean      x    load   new
#> 4:     fit      x    load   new
#> --------------------------------
#> <ready> last run: never
#> steps: 4 new

# --- Provide parameter values with 'params' ---
# 'params' override the defaults of the function ...
p3 <- pip_new("params-demo") |>
  pip_add("data", \(x = c(1, NA, 3)) x) |>
  pip_add("scale", \(x = ~data, factor = 1) x * factor,
    params = list(factor = 10)
  ) |>
  # ... and give values to parameters without default, e.g. of mean()
  pip_add("avg", mean, params = list(x = ~scale, na.rm = TRUE))
pip_run(p3)
#> info [2026-10-25 19:30:01.642 UTC]: Starting run of pipeflow 'params-demo'
#> info [2026-10-25 19:30:01.642 UTC]: Step 1/3 data
#> info [2026-10-25 19:30:01.643 UTC]: Step 2/3 scale
#> info [2026-10-25 19:30:01.643 UTC]: Step 3/3 avg
#> info [2026-10-25 19:30:01.644 UTC]: Finished in 0.002 s (3 done, 0 skipped, 0 failed)
p3
#> <pipeflow> params-demo (3 steps)
#> --------------------------------
#>     step   params depends state      out
#> 1:  data        x          done  1,NA, 3
#> 2: scale x,factor    data  done 10,NA,30
#> 3:   avg  x,na.rm   scale  done       20
#> --------------------------------
#> <ready> last run: 2026-10-25 19:30:01 (0.002 s)
#> steps: 3 done
p3[["avg", "out"]] # 20
#> [1] 20

# Extra params are passed via `...`
p4 <- pip_new("dots-demo") |>
  pip_add("load", \(x = 1, ...) c(x, ...), params = list(size = 42))
pip_run(p4)
#> info [2026-10-25 19:30:01.647 UTC]: Starting run of pipeflow 'dots-demo'
#> info [2026-10-25 19:30:01.647 UTC]: Step 1/1 load
#> info [2026-10-25 19:30:01.648 UTC]: Finished in 0.001 s (1 done, 0 skipped, 0 failed)
p4[["load", "out"]] # c(1, size = 42)
#>      size 
#>    1   42 
```
