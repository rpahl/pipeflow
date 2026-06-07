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
#> INFO  [2026-06-07 17:34:04.476] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:04.477] Step 1/9 data.a
#> INFO  [2026-06-07 17:34:04.480] Step 2/9 add1.a
#> INFO  [2026-06-07 17:34:04.482] Step 3/9 mult.a
#> INFO  [2026-06-07 17:34:04.484] Step 4/9 data.b
#> INFO  [2026-06-07 17:34:04.487] Step 5/9 add1.b
#> INFO  [2026-06-07 17:34:04.489] Step 6/9 mult.b
#> INFO  [2026-06-07 17:34:04.491] Step 7/9 data.c
#> INFO  [2026-06-07 17:34:04.493] Step 8/9 add1.c
#> INFO  [2026-06-07 17:34:04.495] Step 9/9 mult.c
#> INFO  [2026-06-07 17:34:04.496] Finished execution of steps.
#> INFO  [2026-06-07 17:34:04.497] Done.
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
#> INFO  [2026-06-07 17:34:04.529] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:04.530] Step 1/9 data.a
#> INFO  [2026-06-07 17:34:04.532] Step 2/9 add1.a
#> INFO  [2026-06-07 17:34:04.534] Step 3/9 mult.a
#> INFO  [2026-06-07 17:34:04.536] Step 4/9 data.b
#> INFO  [2026-06-07 17:34:04.539] Step 5/9 add1.b
#> INFO  [2026-06-07 17:34:04.541] Step 6/9 mult.b
#> INFO  [2026-06-07 17:34:04.543] Step 7/9 data.c
#> INFO  [2026-06-07 17:34:04.545] Step 8/9 add1.c
#> INFO  [2026-06-07 17:34:04.548] Step 9/9 mult.c
#> INFO  [2026-06-07 17:34:04.549] Finished execution of steps.
#> INFO  [2026-06-07 17:34:04.550] Done.
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
#> INFO  [2026-06-07 17:34:04.586] Start run of 'pipe' pipeline:
#> INFO  [2026-06-07 17:34:04.587] Step 1/10 data.a
#> INFO  [2026-06-07 17:34:04.590] Step 2/10 add1.a
#> INFO  [2026-06-07 17:34:04.596] Step 3/10 mult.a
#> INFO  [2026-06-07 17:34:04.598] Step 4/10 data.b
#> INFO  [2026-06-07 17:34:04.601] Step 5/10 add1.b
#> INFO  [2026-06-07 17:34:04.603] Step 6/10 mult.b
#> INFO  [2026-06-07 17:34:04.606] Step 7/10 data.c
#> INFO  [2026-06-07 17:34:04.608] Step 8/10 add1.c
#> INFO  [2026-06-07 17:34:04.611] Step 9/10 mult.c
#> INFO  [2026-06-07 17:34:04.613] Step 10/10 average_result
#> INFO  [2026-06-07 17:34:04.614] Finished execution of steps.
#> INFO  [2026-06-07 17:34:04.615] Done.
#> List of 1
#>  $ average_result: num 6.67
```
