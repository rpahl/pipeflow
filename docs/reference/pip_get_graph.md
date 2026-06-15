# Build pipeline graph data

Builds graph data (nodes and edges) describing the pipeline's step
structure, suitable for visualisation with
[`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html).

## Usage

``` r
pip_get_graph(x, include_upstream = FALSE)
```

## Arguments

- x:

  A pipeflow pip or view.

- include_upstream:

  Logical. Only relevant for views. If `TRUE`, add all upstream
  dependencies of selected steps.

## Value

A named list with two `data.frame`s: `nodes` and `edges`.

## Details

Node shapes reflect execution mode:

- `auto`/`plain`: `hexagon`

- `reduce`: `dot`

- `split`: `star`

## Examples

``` r
p <- pip_new()
pip_add(p, "load", \(x = 1) x, tags = "io")
pip_add(p, "clean", \(x = ~load) x + 1, tags = "io")
pip_add(p, "fit", \(x = ~clean) x * 2, tags = "model")

graph <- pip_get_graph(p)
graph$nodes # data.frame: id, label, shape, color
#>   id label   shape     color
#> 1  0  load hexagon #47b8ffff
#> 2  1 clean hexagon #47b8ffff
#> 3  2   fit hexagon #47b8ffff
graph$edges # data.frame: from, to, arrows
#>   from to arrows
#> 1    0  1     to
#> 2    1  2     to

# For a view, include_upstream = TRUE adds upstream deps to the graph
v <- pip_view(p, i = "fit")
pip_get_graph(v, include_upstream = TRUE)
#> $nodes
#>   id label   shape     color
#> 1  0  load hexagon #47b8ffff
#> 2  1 clean hexagon #47b8ffff
#> 3  2   fit hexagon #47b8ffff
#> 
#> $edges
#>   from to arrows
#> 1    0  1     to
#> 2    1  2     to
#> 

if (require("visNetwork", quietly = TRUE)) {
  do.call(what = visNetwork::visNetwork, args = graph)
}

{"x":{"nodes":{"id":[0,1,2],"label":["load","clean","fit"],"shape":["hexagon","hexagon","hexagon"],"color":["#47b8ffff","#47b8ffff","#47b8ffff"]},"edges":{"from":[0,1],"to":[1,2],"arrows":["to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}
```
