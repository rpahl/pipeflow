# Append two pipelines

When appending, `pipeflow` takes care of potential name clashes with
respect to step names and dependencies, that is, if needed, it will
automatically adapt step names and dependencies to make sure they are
unique in the merged pipeline.

## Usage

``` r
pipe_append(pip, p, outAsIn = FALSE, tryAutofixNames = TRUE, sep = ".")
```

## Arguments

- pip:

  `Pipeline` object to be appended to.

- p:

  `Pipeline` object to be appended.

- outAsIn:

  `logical` if `TRUE`, output of first pipeline is used as input for the
  second pipeline.

- tryAutofixNames:

  `logical` if `TRUE`, name clashes are tried to be automatically
  resolved by appending the 2nd pipeline's name. Only set to `FALSE`, if
  you know what you are doing.

- sep:

  `string` separator used when auto-resolving step names

## Value

returns new combined `Pipeline` object.

## Lifecycle

Deprecated. Legacy API. Use
[`pip_bind()`](https://github.com/rpahl/pipeflow/reference/pip_bind.md)
instead.

## Examples

``` r
# Append pipeline
p1 <- pipe_new("pipe1")
pipe_add(p1, "step1", \(x = 1) x)
p2 <- pipe_new("pipe2")
pipe_add(p2, "step2", \(y = 1) y)
p1 |> pipe_append(p2)
#> Warning: The legacy 'pipe_*' API is deprecated and will be removed in a future release. Please migrate to the new 'pip_*' API.
#>          step depends    out keepOut  group  state
#>        <char>  <list> <list>  <lgcl> <char> <char>
#> 1:       data         [NULL]   FALSE   data    New
#> 2:      step1         [NULL]   FALSE  step1    New
#> 3: data.pipe2         [NULL]   FALSE   data    New
#> 4:      step2         [NULL]   FALSE  step2    New

# Append pipeline with potential name clashes
p3 <- pipe_new("pipe3")
pipe_add(p3, "step1", \(z = 1) z)
p1 |>
  pipe_append(p2) |>
  pipe_append(p3)
#>           step depends    out keepOut  group  state
#>         <char>  <list> <list>  <lgcl> <char> <char>
#> 1:        data         [NULL]   FALSE   data    New
#> 2:       step1         [NULL]   FALSE  step1    New
#> 3:  data.pipe2         [NULL]   FALSE   data    New
#> 4:       step2         [NULL]   FALSE  step2    New
#> 5:  data.pipe3         [NULL]   FALSE   data    New
#> 6: step1.pipe3         [NULL]   FALSE  step1    New

# Use output of first pipeline as input for second pipeline
p1 <- pipe_new("pipe1", data = 8)
p2 <- pipe_new("pipe2")
pipe_add(p1, "square", \(x = ~data) x^2)
pipe_add(p2, "log2", \(x = ~data) log2(x))

p12 <- p1 |> pipe_append(p2, outAsIn = TRUE)
p12 |>
  pipe_run() |>
  pipe_get_out("log2")
#> INFO  [2026-06-20 21:18:52.054] Start run of 'pipe1.pipe2' pipeline:
#> INFO  [2026-06-20 21:18:52.055] Step 1/4 data
#> INFO  [2026-06-20 21:18:52.058] Step 2/4 square
#> INFO  [2026-06-20 21:18:52.060] Step 3/4 data.pipe2
#> INFO  [2026-06-20 21:18:52.063] Step 4/4 log2
#> INFO  [2026-06-20 21:18:52.064] Finished execution of steps.
#> INFO  [2026-06-20 21:18:52.065] Done.
#> [1] 6
p12
#>          step    depends    out keepOut      group  state
#>        <char>     <list> <list>  <lgcl>     <char> <char>
#> 1:       data                 8   FALSE       data   Done
#> 2:     square       data     64   FALSE     square   Done
#> 3: data.pipe2     square     64   FALSE data.pipe2   Done
#> 4:       log2 data.pipe2      6   FALSE       log2   Done

# Custom name separator for adapted step names
p1 |> pipe_append(p2, sep = "___")
#>            step      depends    out keepOut  group  state
#>          <char>       <list> <list>  <lgcl> <char> <char>
#> 1:         data              [NULL]   FALSE   data    New
#> 2:       square         data [NULL]   FALSE square    New
#> 3: data___pipe2              [NULL]   FALSE   data    New
#> 4:         log2 data___pipe2 [NULL]   FALSE   log2    New
```
