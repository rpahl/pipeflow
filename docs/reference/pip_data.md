# Access the underlying step table

This is a convenience wrapper for accessing the internal `data.table`,
which normally is reachable via `p[["pipenv"]][["data"]]`.

## Usage

``` r
pip_data(x)
```

## Arguments

- x:

  A pipeflow pipeline or view.

## Value

The underlying step table as a `data.table`.

## Details

The internal `data.table` is holding the pipeline steps, one row per
step. Unless you know what you are doing, this table should not be
modified directly, as this can corrupt the pipeline including any views
that are derived from it. If you want to experiment, consider cloning
the pipeline first with
[`pip_clone()`](https://github.com/rpahl/pipeflow/reference/pip_clone.md).

## Examples

``` r
p <- pip_new() |>
  pip_add("load", \(n = 5) seq_len(n)) |>
  pip_add("model", \(x = ~load) sum(x))

pip_data(p)
#>      step           fun    params    out  state   tags locked   exec
#>    <char>        <list>    <list> <list> <char> <list> <lgcl> <char>
#> 1:   load <function[1]> <list[1]> [NULL]    new         FALSE   auto
#> 2:  model <function[1]> <list[1]> [NULL]    new         FALSE   auto
#>                   time depends unbound nodeId
#>                 <POSc>  <list>  <list>  <int>
#> 1: 2026-09-27 19:53:06               n      0
#> 2: 2026-09-27 19:53:06    load              1
```
