# Print pipeflow objects

Print pipeflow objects

## Usage

``` r
# S3 method for class 'pipeflow_pip'
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

# S3 method for class 'pipeflow_view'
print(x, header = TRUE, ...)
```

## Arguments

- x:

  A pipeflow pipeline or view.

- rows:

  Row indices to be printed. If empty, all rows are printed.

- cols:

  The columns to be printed. Can be either one of `core` or `all` to
  print the core or all columns, respectively, or an explicit character
  vector of columns to be printed.

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

  If TRUE, a header with the pipeline name and number of steps will be
  printed.

- ...:

  Other arguments passed to `print.data.table`

## Value

Invisibly returns `x`.

## Examples

``` r
p <- pip_new("demo") |>
  pip_add("load", \(n = 5) seq_len(n), group = "io", tags = "raw") |>
  pip_add("square", \(x = ~load) x^2, group = "compute") |>
  pip_add("total", \(x = ~square) sum(x), group = "compute")

print(p) # core columns: step, group, depends, tags, out, state
#> <pipeflow_pip> demo (3 steps)
#> -----------------------------
#>      step   group depends    out state tags
#> 1:   load      io         [NULL]   new  raw
#> 2: square compute    load [NULL]   new     
#> 3:  total compute  square [NULL]   new     
print(p, cols = "all") # all non-hidden columns
#> <pipeflow_pip> demo (3 steps)
#> -----------------------------
#>      step   group           fun    params     signature depends    out state
#> 1:   load      io <function[1]> <list[1]>       (n = 5)         [NULL]   new
#> 2: square compute <function[1]> <list[1]>   (x = ~load)    load [NULL]   new
#> 3:  total compute <function[1]> <list[1]> (x = ~square)  square [NULL]   new
#>    tags                time locked exec
#> 1:  raw 2026-06-07 17:34:11  FALSE auto
#> 2:      2026-06-07 17:34:11  FALSE auto
#> 3:      2026-06-07 17:34:11  FALSE auto
print(p, rows = 2:3) # print only steps 2 and 3
#> <pipeflow_pip> demo (3 steps)
#> -----------------------------
#>      step   group depends    out state tags
#> 1: square compute    load [NULL]   new     
#> 2:  total compute  square [NULL]   new     
p <- pip_new() |>
  pip_add("s1", \(x = 1) x, group = "io") |>
  pip_add("s2", \(x = ~s1) x + 1, group = "model")

# A view header shows how many steps are selected out of the total
v <- pip_view(p, filter = list(group = "model"))
print(v) # "<pipeflow_view> pipe view (1 of 2 steps)"
#> <pipeflow_view> pipe view (1 of 2 steps)
#> ----------------------------------------
#>  step group depends    out state
#>    s2 model      s1 [NULL]   new
```
