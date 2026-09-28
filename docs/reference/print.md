# Print pipeflow objects

Print pipeflow objects

## Usage

``` r
# S3 method for class 'pipeflow'
str(object, ...)

# S3 method for class 'pipeflow'
print(
  x,
  rows = integer(),
  cols = getOption("pipeflow.print.cols", default = "core"),
  topn = getOption("pipeflow.print.topn", default = 5),
  nrows = getOption("pipeflow.print.nrows", default = 50),
  row.names = getOption("pipeflow.print.rownames", default = TRUE),
  class = getOption("pipeflow.print.class", default = FALSE),
  header = TRUE,
  ...
)
```

## Arguments

- object:

  A pipeflow pipeline or view, for
  [`utils::str()`](https://rdrr.io/r/utils/str.html).

- ...:

  Other arguments passed to `print.data.table`

- x:

  A pipeflow pipeline or view.

- rows:

  Row indices to be printed. If empty, all rows are printed.

- cols:

  The columns to be printed. Can be either one of `core` or `all` to
  print the core or all columns, respectively, or an explicit character
  vector of columns to be printed. The `params` column lists the names
  of the step's parameters.

- topn:

  The number of rows to be printed from the beginning and end of tables
  with more than `nrows` rows.

- nrows:

  The number of rows printed before truncation is enforced.

- row.names:

  If TRUE, row indices will be printed alongside x.

- class:

  If TRUE, the resulting output will include above each column its
  storage class (or a self-evident abbreviation thereof).

- header:

  If TRUE, a header with the pipeline name and number of steps, and a
  footer with the run state and the time of the last run, will be
  printed.

## Value

Invisibly returns `x`.

## Examples

``` r
p <- pip_new("demo") |>
  pip_add("load", \(n = 5) seq_len(n), tags = c("io", "raw")) |>
  pip_add("square", \(x = ~load) x^2, tags = "compute") |>
  pip_add("total", \(x = ~square) sum(x), tags = "compute")

print(p) # core columns: step, params, depends, state, tags
#> <pipeflow> demo (3 steps)
#> -------------------------
#>      step params depends state    tags
#> 1:   load      n           new  io,raw
#> 2: square      x    load   new compute
#> 3:  total      x  square   new compute
#> -------------------------
#> <ready> last run: never
print(p, cols = "all") # all step-table columns
#> <pipeflow> demo (3 steps)
#> -------------------------
#>      step           fun params    out state    tags locked exec
#> 1:   load <function[1]>      n [NULL]   new  io,raw  FALSE auto
#> 2: square <function[1]>      x [NULL]   new compute  FALSE auto
#> 3:  total <function[1]>      x [NULL]   new compute  FALSE auto
#>                   time depends unbound nodeId
#> 1: 2026-09-27 19:53:09               n      0
#> 2: 2026-09-27 19:53:09    load              1
#> 3: 2026-09-27 19:53:09  square              2
#> -------------------------
#> <ready> last run: never
print(p, rows = 2:3) # print only steps 2 and 3
#> <pipeflow> demo (3 steps)
#> -------------------------
#>      step params depends state    tags
#> 1: square      x    load   new compute
#> 2:  total      x  square   new compute
#> -------------------------
#> <ready> last run: never

v <- pip_view(p, tags = "compute")
print(v)
#> <pipeflow_view> demo view (2 of 3 steps)
#> ----------------------------------------
#>      step params depends state    tags
#> 1: square      x    load   new compute
#> 2:  total      x  square   new compute
#> ----------------------------------------
#> <ready> last run: never
```
