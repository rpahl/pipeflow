# Create a pipeline

Creates a new, empty pipeline. Add steps with
[`pip_add()`](https://github.com/rpahl/pipeflow/reference/pip_add.md)
and execute them with
[`pip_run()`](https://github.com/rpahl/pipeflow/reference/pip_run.md).

## Usage

``` r
pip_new(name = "pipe")
```

## Arguments

- name:

  Single name used for printing and for derived view names.

## Value

A pipeflow pipeline object.

## Examples

``` r
# Create a named pipeline
p <- pip_new("my_analysis")
p[["name"]] # "my_analysis"
#> [1] "my_analysis"

# Build a simple pipeline and run it
pip_add(p, "load", \(n = 5) seq_len(n))
pip_add(p, "double", \(x = ~load) x * 2) # x depends on load's output
p
#> <pipeflow_pip> my_analysis (2 steps)
#> ------------------------------------
#>      step depends    out state
#> 1:   load         [NULL]   new
#> 2: double    load [NULL]   new
pip_run(p)
#> info [2026-06-15 10:50:44.809 UTC]: Start run of pipeflow_pip 'my_analysis'
#> info [2026-06-15 10:50:44.809 UTC]: Step 1/2 load
#> info [2026-06-15 10:50:44.810 UTC]: Step 2/2 double
#> info [2026-06-15 10:50:44.811 UTC]: Finished run of pipeflow_pip 'my_analysis'
p[["out"]] # list of outputs, one per step
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> [[2]]
#> [1]  2  4  6  8 10
#> 
```
