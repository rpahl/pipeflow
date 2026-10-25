# Build pipeline graph data

Builds graph data (nodes and edges) describing the pipeline's step
structure, suitable for visualisation with
[`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html),
or the graph as a [mermaid](https://mermaid.js.org) flowchart.

## Usage

``` r
pip_graph(x, include_upstream = FALSE, format = c("visNetwork", "mermaid"))
```

## Arguments

- x:

  A pipeflow pip or view.

- include_upstream:

  Logical. Only relevant for views. If `TRUE`, add all upstream
  dependencies of selected steps.

- format:

  The output format, either `"visNetwork"` or `"mermaid"`.

## Value

For `format = "visNetwork"`, a named list with two `data.frame`s:
`nodes` (columns `id`, `label`, `shape`, `color` and `title`) and
`edges` (columns `from`, `to`, `arrows` and `dashes`). For
`format = "mermaid"`, a single string with the mermaid flowchart.

## Details

Node shapes reflect execution mode:

- `auto`/`plain`: `hexagon`

- `reduce`: `dot`

- `split`: `star`

Node colors reflect the step state (`new`, `done`, `outdated` or
`failed`). Hovering over a node shows its `title`: the state, the exec
mode, the tags, whether it is locked, the start of its last run (and its
duration if it succeeded), the failed keys of a mapped step, its
parameters and the steps it depends on.

An edge is dashed if the step refers to the other step only with
`~try()`, i.e. if the step also runs when the other step failed (see
[`pip_run()`](https://rpahl.github.io/pipeflow/dev/reference/pip_run.md)).

### Mermaid

With `format = "mermaid"`, the graph is returned as the text of a
mermaid flowchart. It renders without extra packages in a code block
marked `mermaid`, e.g. on GitHub or in a Quarto document. It has the
same colors and dashed edges as the visNetwork graph, but no hover text.
The shapes are a hexagon for `auto`/`plain`, a trapezoid that widens for
`split` and one that narrows for `reduce`.

## Examples

``` r
p <- pip_new()
pip_add(p, "load", \(x = 1) x, tags = "io")
pip_add(p, "clean", \(x = ~load) x + 1, tags = "io")
pip_add(p, "fit", \(x = ~clean) x * 2, tags = "model")
pip_add(p, "report", \(fit = ~try(fit)) class(fit))

graph <- pip_graph(p)
graph$nodes # data.frame: id, label, shape, color, title
#>   id  label   shape     color
#> 1  0   load hexagon #47b8ffff
#> 2  1  clean hexagon #47b8ffff
#> 3  2    fit hexagon #47b8ffff
#> 4  3 report hexagon #47b8ffff
#>                                                                     title
#> 1    <b>load</b><br>state: new<br>exec: auto<br>tags: io<br>params: x = 1
#> 2   <b>clean</b><br>state: new<br>exec: auto<br>tags: io<br>depends: load
#> 3 <b>fit</b><br>state: new<br>exec: auto<br>tags: model<br>depends: clean
#> 4          <b>report</b><br>state: new<br>exec: auto<br>depends: try(fit)
graph$edges # data.frame: from, to, arrows, dashes
#>   from to arrows dashes
#> 1    0  1     to  FALSE
#> 2    1  2     to  FALSE
#> 3    2  3     to   TRUE

# For a view, include_upstream = TRUE adds upstream deps to the graph
v <- pip_view(p, step = "fit")
pip_graph(v, include_upstream = TRUE)
#> $nodes
#>   id label   shape     color
#> 1  0  load hexagon #47b8ffff
#> 2  1 clean hexagon #47b8ffff
#> 3  2   fit hexagon #47b8ffff
#>                                                                     title
#> 1    <b>load</b><br>state: new<br>exec: auto<br>tags: io<br>params: x = 1
#> 2   <b>clean</b><br>state: new<br>exec: auto<br>tags: io<br>depends: load
#> 3 <b>fit</b><br>state: new<br>exec: auto<br>tags: model<br>depends: clean
#> 
#> $edges
#>   from to arrows dashes
#> 1    0  1     to  FALSE
#> 2    1  2     to  FALSE
#> 

if (require("visNetwork", quietly = TRUE)) {
  do.call(what = visNetwork::visNetwork, args = graph)
}

{"x":{"nodes":{"id":[0,1,2,3],"label":["load","clean","fit","report"],"shape":["hexagon","hexagon","hexagon","hexagon"],"color":["#47b8ffff","#47b8ffff","#47b8ffff","#47b8ffff"],"title":["<b>load<\/b><br>state: new<br>exec: auto<br>tags: io<br>params: x = 1","<b>clean<\/b><br>state: new<br>exec: auto<br>tags: io<br>depends: load","<b>fit<\/b><br>state: new<br>exec: auto<br>tags: model<br>depends: clean","<b>report<\/b><br>state: new<br>exec: auto<br>depends: try(fit)"]},"edges":{"from":[0,1,2],"to":[1,2,3],"arrows":["to","to","to"],"dashes":[false,false,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}
# The graph as a mermaid flowchart
cat(pip_graph(p, format = "mermaid"))
#> flowchart LR
#>     n0{{"load"}}
#>     n1{{"clean"}}
#>     n2{{"fit"}}
#>     n3{{"report"}}
#>     n0 --> n1
#>     n1 --> n2
#>     n2 -.-> n3
#>     classDef new fill:#47b8ff,color:#000
#>     class n0,n1,n2,n3 new
```
