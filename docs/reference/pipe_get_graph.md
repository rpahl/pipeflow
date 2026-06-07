# Pipeline graph

Get the pipeline as a graph with nodes and edges.

## Usage

``` r
pipe_get_graph(pip, groups = NULL)
```

## Arguments

- pip:

  `Pipeline` object

- groups:

  `character` if not `NULL`, only steps belonging to the given groups
  are considered.

## Value

list with two data frames, one for nodes and one for edges ready to be
used with the
[`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html)
function of the
[visNetwork](https://rdrr.io/pkg/visNetwork/man/visNetwork.html)
package.

## Lifecycle

Deprecated. Legacy API. Use
[`pip_get_graph()`](https://github.com/rpahl/pipeflow/reference/pip_get_graph.md)
instead.

## Examples

``` r
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(data = ~data, x = 1) x + data)
pipe_add(p, "add2", \(x = 1, y = ~add1) x + y)
pipe_add(p, "mult1", \(x = ~add1, y = ~add2) x * y)
graph <- pipe_get_graph(p)
graph
#> $nodes
#>   id label group    shape     color   title
#> 1  1  data  data database lightblue <p></p>
#> 2  2  add1  add1      box lightblue <p></p>
#> 3  3  add2  add2      box lightblue <p></p>
#> 4  4 mult1 mult1      box lightblue <p></p>
#> 
#> $edges
#>         from to arrows
#> add1       1  2     to
#> add2       2  3     to
#> mult1.x    2  4     to
#> mult1.y    3  4     to
#> 

if (require("visNetwork", quietly = TRUE)) {
  do.call(visNetwork, args = graph)
}

{"x":{"nodes":{"id":[1,2,3,4],"label":["data","add1","add2","mult1"],"group":["data","add1","add2","mult1"],"shape":["database","box","box","box"],"color":["lightblue","lightblue","lightblue","lightblue"],"title":["<p><\/p>","<p><\/p>","<p><\/p>","<p><\/p>"]},"edges":{"from":[1,2,2,3],"to":[2,3,4,4],"arrows":["to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":["data","add1","add2","mult1"],"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}
```
