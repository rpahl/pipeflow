# Collect output from entire pipeline

Collects output afer pipeline run, by default, from all steps for which
`keepOut` was set to `TRUE` when steps were added (see
[`pipe_add()`](https://github.com/rpahl/pipeflow/reference/pipe_add.md)).
The output is grouped by the group names (see `group` parameter in
[`pipe_add()`](https://github.com/rpahl/pipeflow/reference/pipe_add.md)),
which by default are set identical to the step names.

## Usage

``` r
pipe_collect_out(pip, groupBy = "group", all = FALSE)
```

## Arguments

- pip:

  `Pipeline` object

- groupBy:

  `string` column of pipeline by which to group the output.

- all:

  `logical` if `TRUE` all output is collected regardless of the
  `keepOut` flag. This can be useful for debugging.

## Value

`list` containing the output, named after the groups, which, by default,
are the steps.

## Lifecycle

Deprecated. Legacy API. Use
[`pip_collect_out()`](https://github.com/rpahl/pipeflow/reference/pip_collect_out.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "step1", \(x = ~data) x + 2)
pipe_add(p, "step2", \(x = ~step1) x + 2, keepOut = TRUE)
pipe_run(p)
#> INFO  [2026-06-13 17:08:19.250] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:19.252] Step 1/3 data
#> INFO  [2026-06-13 17:08:19.256] Step 2/3 step1
#> INFO  [2026-06-13 17:08:19.259] Step 3/3 step2
#> INFO  [2026-06-13 17:08:19.261] Finished execution of steps.
#> INFO  [2026-06-13 17:08:19.262] Done.
pipe_collect_out(p)
#> $step2
#> [1] 5 6
#> 
pipe_collect_out(p, all = TRUE) |> str()
#> List of 3
#>  $ data : int [1:2] 1 2
#>  $ step1: num [1:2] 3 4
#>  $ step2: num [1:2] 5 6

# Grouped output
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "step1", \(x = ~data) x + 2, group = "add")
pipe_add(p, "step2", \(x = ~step1, y = 2) x + y, group = "add")
pipe_add(p, "step3", \(x = ~data) x * 3, group = "mult")
pipe_add(p, "step4", \(x = ~data, y = 2) x * y, group = "mult")
p
#>      step depends    out keepOut  group  state
#>    <char>  <list> <list>  <lgcl> <char> <char>
#> 1:   data         [NULL]   FALSE   data    New
#> 2:  step1    data [NULL]   FALSE    add    New
#> 3:  step2   step1 [NULL]   FALSE    add    New
#> 4:  step3    data [NULL]   FALSE   mult    New
#> 5:  step4    data [NULL]   FALSE   mult    New

pipe_run(p)
#> INFO  [2026-06-13 17:08:19.287] Start run of 'pipe' pipeline:
#> INFO  [2026-06-13 17:08:19.289] Step 1/5 data
#> INFO  [2026-06-13 17:08:19.292] Step 2/5 step1
#> INFO  [2026-06-13 17:08:19.294] Step 3/5 step2
#> INFO  [2026-06-13 17:08:19.296] Step 4/5 step3
#> INFO  [2026-06-13 17:08:19.298] Step 5/5 step4
#> INFO  [2026-06-13 17:08:19.299] Finished execution of steps.
#> INFO  [2026-06-13 17:08:19.299] Done.
pipe_collect_out(p, all = TRUE) |> str()
#> List of 3
#>  $ data: int [1:2] 1 2
#>  $ add :List of 2
#>   ..$ step1: num [1:2] 3 4
#>   ..$ step2: num [1:2] 5 6
#>  $ mult:List of 2
#>   ..$ step3: num [1:2] 3 6
#>   ..$ step4: num [1:2] 2 4

# Grouped by state
pipe_set_params(p, list(y = 5))
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
p
#>      step depends    out keepOut  group    state
#>    <char>  <list> <list>  <lgcl> <char>   <char>
#> 1:   data            1,2   FALSE   data     Done
#> 2:  step1    data    3,4   FALSE    add     Done
#> 3:  step2   step1    5,6   FALSE    add Outdated
#> 4:  step3    data    3,6   FALSE   mult     Done
#> 5:  step4    data    2,4   FALSE   mult Outdated

pipe_collect_out(p, groupBy = "state", all = TRUE) |> str()
#> List of 2
#>  $ Done    :List of 3
#>   ..$ data : int [1:2] 1 2
#>   ..$ step1: num [1:2] 3 4
#>   ..$ step3: num [1:2] 3 6
#>  $ Outdated:List of 2
#>   ..$ step2: num [1:2] 5 6
#>   ..$ step4: num [1:2] 2 4
```
