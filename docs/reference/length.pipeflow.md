# Length of a pipeflow pipeline or view

Length of a pipeflow pipeline or view

## Usage

``` r
# S3 method for class 'pipeflow_pip'
length(x)

# S3 method for class 'pipeflow_view'
length(x)
```

## Arguments

- x:

  A pipeflow pipeline or view

## Value

Number of steps as an integer.

## Examples

``` r
p <- pip_new() |>
  pip_add("s1", \(x = 1) x) |>
  pip_add("s2", \(x = ~s1) x + 1) |>
  pip_add("s3", \(x = ~s2) x * 2)
length(p) # 3 — total steps in the pipeline
#> [1] 3

# A view reports only the number of selected (visible) steps
v <- pip_view(p, i = c("s2", "s3"))
length(v) # 2
#> [1] 2
```
