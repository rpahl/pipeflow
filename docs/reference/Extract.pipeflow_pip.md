# Extract or subset a pipeline

Returns a new pipeline containing selected steps and all required
upstream dependencies.

Extracts values from a pipeline using one or two indices. With a single
string name, named fields such as `"pipeline"` or `"name"` are returned
first; anything else returns the matching step-table column. With two
indices (`row`, `column`), a single cell is extracted.

## Usage

``` r
# S3 method for class 'pipeflow_pip'
x[i, ...]

# S3 method for class 'pipeflow_pip'
x[[i, j, ...]]
```

## Arguments

- x:

  A pipeflow pipeline object.

- i:

  integer (row indices) or character vector (step names) of steps to
  select

- ...:

  not used

- j:

  column names to select

## Value

A new pipeflow pipeline object.

Extracted value(s), depending on `i` and `j`.

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 5) seq_len(n)) |>
  pip_add("square", \(x = ~load) x^2) |>
  pip_add("total", \(x = ~square) sum(x))

# Select by step name — upstream deps are pulled in automatically.
# Selecting only "total" still includes "load" and "square".
sub <- p["total"]
sub[["pipeline"]][["step"]] # "load", "square", "total"
#> [1] "load"   "square" "total" 

# Select a subset of steps by name vector
p[c("load", "square")][["pipeline"]][["step"]] # "load", "square"
#> [1] "load"   "square"

# Select by integer row index
p[1:2][["pipeline"]][["step"]] # "load", "square"
#> [1] "load"   "square"
p <- pip_new() |>
  pip_add("load", \(x = 1) x) |>
  pip_add("fit", \(x = ~load) x + 1)

# Access internal objects by name
p[["pipeline"]] # the full step table
#>      step           fun    params   signature depends    out  state   tags
#>    <char>        <list>    <list>      <char>  <list> <list> <char> <list>
#> 1:   load <function[1]> <list[1]>     (x = 1)         [NULL]    new       
#> 2:    fit <function[1]> <list[1]> (x = ~load)    load [NULL]    new       
#>                   time locked   exec .nodeId .indeps
#>                 <POSc> <lgcl> <char>   <int>  <list>
#> 1: 2026-06-20 21:18:41  FALSE   auto       0       x
#> 2: 2026-06-20 21:18:41  FALSE   auto       1        
p[["name"]] # "pipe"
#> [1] "pipe"

# Shorthand column access (equivalent to p[["pipeline"]][["step"]])
p[["step"]]
#> [1] "load" "fit" 

# Two-index form: p[[row, column]] extracts a single cell
p[["fit", "depends"]] # "load"
#>      x 
#> "load" 
p[[2, "state"]] # state of the second step
#> [1] "new"
```
