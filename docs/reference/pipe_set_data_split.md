# Split-multiply pipeline by list of data sets

This function can be used to apply the pipeline repeatedly to various
data sets. For this, the pipeline split-copies itself by the list of
given data sets. Each sub-pipeline will have one of the data sets set as
input data. The step names of the sub-pipelines will be the original
step names plus the name of the data set.

## Usage

``` r
pipe_set_data_split(
  pip,
  dataList,
  toStep = character(),
  groupBySplit = TRUE,
  sep = "."
)
```

## Arguments

- pip:

  `Pipeline` object

- dataList:

  `list` of data sets

- toStep:

  `string` step name marking optional subset of the pipeline, to which
  the data split should be applied to.

- groupBySplit:

  `logical` whether to set step groups according to data split.

- sep:

  `string` separator to be used between step name and data set name when
  creating the new step names.

## Value

new combined `Pipeline` with each sub-pipeline having set one of the
data sets.

## Lifecycle

Deprecated. Legacy API. Use
[`pip_new()`](https://github.com/rpahl/pipeflow/reference/pip_new.md)
and related pip\_\* functions.

## Examples

``` r
# Split by three data sets
dataList <- list(a = 1, b = 2, c = 3)
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1, keepOut = TRUE)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipe_set_data_split(p, dataList)
p
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE      a      New
#> 2: add1.a        data.a [NULL]    TRUE      a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE      a Outdated
#> 4: data.b               [NULL]   FALSE      b      New
#> 5: add1.b        data.b [NULL]    TRUE      b Outdated
#> 6: mult.b data.b,add1.b [NULL]    TRUE      b Outdated
#> 7: data.c               [NULL]   FALSE      c      New
#> 8: add1.c        data.c [NULL]    TRUE      c Outdated
#> 9: mult.c data.c,add1.c [NULL]    TRUE      c Outdated

p |>
  pipe_run() |>
  pipe_collect_out() |>
  str()
#> INFO  [2026-06-13 19:22:22.685] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:22.687] Step 1/9 data.a
#> INFO  [2026-06-13 19:22:22.689] Step 2/9 add1.a
#> INFO  [2026-06-13 19:22:22.692] Step 3/9 mult.a
#> INFO  [2026-06-13 19:22:22.694] Step 4/9 data.b
#> INFO  [2026-06-13 19:22:22.696] Step 5/9 add1.b
#> INFO  [2026-06-13 19:22:22.699] Step 6/9 mult.b
#> INFO  [2026-06-13 19:22:22.701] Step 7/9 data.c
#> INFO  [2026-06-13 19:22:22.703] Step 8/9 add1.c
#> INFO  [2026-06-13 19:22:22.706] Step 9/9 mult.c
#> INFO  [2026-06-13 19:22:22.707] Finished execution of steps.
#> INFO  [2026-06-13 19:22:22.708] Done.
#> List of 3
#>  $ a:List of 2
#>   ..$ add1.a: num 2
#>   ..$ mult.a: num 2
#>  $ b:List of 2
#>   ..$ add1.b: num 3
#>   ..$ mult.b: num 6
#>  $ c:List of 2
#>   ..$ add1.c: num 4
#>   ..$ mult.c: num 12

# Don't group output by split
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1, keepOut = TRUE)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipe_set_data_split(p, dataList, groupBySplit = FALSE)
p
#>      step       depends    out keepOut  group    state
#>    <char>        <list> <list>  <lgcl> <char>   <char>
#> 1: data.a               [NULL]   FALSE data.a      New
#> 2: add1.a        data.a [NULL]    TRUE add1.a Outdated
#> 3: mult.a data.a,add1.a [NULL]    TRUE mult.a Outdated
#> 4: data.b               [NULL]   FALSE data.b      New
#> 5: add1.b        data.b [NULL]    TRUE add1.b Outdated
#> 6: mult.b data.b,add1.b [NULL]    TRUE mult.b Outdated
#> 7: data.c               [NULL]   FALSE data.c      New
#> 8: add1.c        data.c [NULL]    TRUE add1.c Outdated
#> 9: mult.c data.c,add1.c [NULL]    TRUE mult.c Outdated

p |>
  pipe_run() |>
  pipe_collect_out() |>
  str()
#> INFO  [2026-06-13 19:22:22.742] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:22.743] Step 1/9 data.a
#> INFO  [2026-06-13 19:22:22.746] Step 2/9 add1.a
#> INFO  [2026-06-13 19:22:22.749] Step 3/9 mult.a
#> INFO  [2026-06-13 19:22:22.751] Step 4/9 data.b
#> INFO  [2026-06-13 19:22:22.754] Step 5/9 add1.b
#> INFO  [2026-06-13 19:22:22.756] Step 6/9 mult.b
#> INFO  [2026-06-13 19:22:22.762] Step 7/9 data.c
#> INFO  [2026-06-13 19:22:22.765] Step 8/9 add1.c
#> INFO  [2026-06-13 19:22:22.767] Step 9/9 mult.c
#> INFO  [2026-06-13 19:22:22.769] Finished execution of steps.
#> INFO  [2026-06-13 19:22:22.769] Done.
#> List of 6
#>  $ add1.a: num 2
#>  $ mult.a: num 2
#>  $ add1.b: num 3
#>  $ mult.b: num 6
#>  $ add1.c: num 4
#>  $ mult.c: num 12

# Split up to certain step
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y)
pipe_add(p, "average_result", \(x = ~mult) mean(unlist(x)), keepOut = TRUE)
p
#>              step   depends    out keepOut          group  state
#>            <char>    <list> <list>  <lgcl>         <char> <char>
#> 1:           data           [NULL]   FALSE           data    New
#> 2:           add1      data [NULL]   FALSE           add1    New
#> 3:           mult data,add1 [NULL]   FALSE           mult    New
#> 4: average_result      mult [NULL]    TRUE average_result    New
pipe_get_depends(p)[["average_result"]]
#>      x 
#> "mult" 

pipe_set_data_split(p, dataList, toStep = "mult")
p
#>               step       depends    out keepOut          group    state
#>             <char>        <list> <list>  <lgcl>         <char>   <char>
#>  1:         data.a               [NULL]   FALSE              a      New
#>  2:         add1.a        data.a [NULL]   FALSE              a Outdated
#>  3:         mult.a data.a,add1.a [NULL]   FALSE              a Outdated
#>  4:         data.b               [NULL]   FALSE              b      New
#>  5:         add1.b        data.b [NULL]   FALSE              b Outdated
#>  6:         mult.b data.b,add1.b [NULL]   FALSE              b Outdated
#>  7:         data.c               [NULL]   FALSE              c      New
#>  8:         add1.c        data.c [NULL]   FALSE              c Outdated
#>  9:         mult.c data.c,add1.c [NULL]   FALSE              c Outdated
#> 10: average_result     <list[1]> [NULL]    TRUE average_result      New
pipe_get_depends(p)[["average_result"]]
#> $x
#> [1] "mult.a" "mult.b" "mult.c"
#> 

p |>
  pipe_run() |>
  pipe_collect_out() |>
  str()
#> INFO  [2026-06-13 19:22:22.811] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 19:22:22.812] Step 1/10 data.a
#> INFO  [2026-06-13 19:22:22.816] Step 2/10 add1.a
#> INFO  [2026-06-13 19:22:22.818] Step 3/10 mult.a
#> INFO  [2026-06-13 19:22:22.820] Step 4/10 data.b
#> INFO  [2026-06-13 19:22:22.824] Step 5/10 add1.b
#> INFO  [2026-06-13 19:22:22.826] Step 6/10 mult.b
#> INFO  [2026-06-13 19:22:22.829] Step 7/10 data.c
#> INFO  [2026-06-13 19:22:22.832] Step 8/10 add1.c
#> INFO  [2026-06-13 19:22:22.835] Step 9/10 mult.c
#> INFO  [2026-06-13 19:22:22.837] Step 10/10 average_result
#> INFO  [2026-06-13 19:22:22.838] Finished execution of steps.
#> INFO  [2026-06-13 19:22:22.839] Done.
#> List of 1
#>  $ average_result: num 6.67
```
