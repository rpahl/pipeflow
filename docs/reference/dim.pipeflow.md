# Dimensions of a pipeflow pipeline or view

Treats a pipeline as a table of steps:
[`length()`](https://rdrr.io/r/base/length.html) and
[`nrow()`](https://rdrr.io/r/base/nrow.html) return the number of steps,
[`ncol()`](https://rdrr.io/r/base/nrow.html) returns the number of
columns of the underlying step table, and
[`dim()`](https://rdrr.io/r/base/dim.html) returns both. Views report
only the steps covered by the view.

## Usage

``` r
# S3 method for class 'pipeflow'
length(x)

# S3 method for class 'pipeflow'
dim(x)
```

## Arguments

- x:

  A pipeflow pipeline or view

## Value

- [`length()`](https://rdrr.io/r/base/length.html),
  [`nrow()`](https://rdrr.io/r/base/nrow.html): the number of steps

- [`ncol()`](https://rdrr.io/r/base/nrow.html): the number of columns of
  the step table

- [`dim()`](https://rdrr.io/r/base/dim.html): vector with the number of
  steps and columns.

## See also

[`base::dim()`](https://rdrr.io/r/base/dim.html),
[`base::nrow()`](https://rdrr.io/r/base/nrow.html),
[`base::length()`](https://rdrr.io/r/base/length.html)

## Examples

``` r
p <- pip_new() |>
  pip_add("s1", \(x = 1) x) |>
  pip_add("s2", \(x = ~s1) x + 1) |>
  pip_add("s3", \(x = ~s2) x * 2)

dim(p)
#> [1]  3 12
nrow(p)
#> [1] 3
ncol(p)
#> [1] 12
length(p)
#> [1] 3
length(p) == nrow(p) # TRUE
#> [1] TRUE

# A view reports only the number of selected (visible) steps
v <- pip_view(p, step = c("s2", "s3"))
length(v) # 2
#> [1] 2
nrow(v) # 2
#> [1] 2
```
